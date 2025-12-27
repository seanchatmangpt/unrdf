/**
 * @fileoverview Dimension Computer - Compute D_t from package universes
 *
 * D_t represents the representable dimensions per package and system-wide.
 * It measures log2(quads possible under constraints) per partition.
 *
 * **Core Formula**:
 * D_t = log2(|Q_admissible|) = log2(|S| * |P| * |O| * |G|) under constraints
 *
 * Where:
 * - S = set of admissible subjects
 * - P = set of admissible predicates
 * - O = set of admissible objects
 * - G = set of admissible graphs
 *
 * **Properties**:
 * - Higher D_t = more expressive capacity
 * - D_t drop = constraints have reduced expressiveness
 * - D_t stability = healthy governance
 *
 * @module measurement/dimension-computer
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

/**
 * Namespace constraint schema
 */
const NamespaceConstraintSchema = z.object({
  namespace: z.string(),
  allowed: z.boolean(),
  maxTerms: z.number().optional(),
  termPattern: z.string().optional()
});

/**
 * Partition dimension schema
 */
const PartitionDimensionSchema = z.object({
  partitionName: z.string(),
  subjects: z.number(),
  predicates: z.number(),
  objects: z.number(),
  graphs: z.number(),
  dimension: z.number(),
  constraintFactor: z.number(),
  effectiveDimension: z.number(),
  timestamp: z.string().datetime()
});

/**
 * System dimension schema
 */
const SystemDimensionSchema = z.object({
  epoch: z.string(),
  systemDimension: z.number(),
  partitionDimensions: z.array(PartitionDimensionSchema),
  totalQuads: z.number(),
  possibleQuads: z.number(),
  utilizationRatio: z.number(),
  dimensionHash: z.string(),
  computedAt: z.string().datetime()
});

/**
 * Historical dimension record
 */
const DimensionHistorySchema = z.object({
  epoch: z.string(),
  systemDimension: z.number(),
  deltaFromPrevious: z.number().nullable(),
  percentChange: z.number().nullable(),
  trend: z.enum(['stable', 'increasing', 'decreasing', 'volatile']),
  timestamp: z.string().datetime()
});

/**
 * Dimension Computer - Computes representable dimensions for universe
 *
 * @class DimensionComputer
 * @example
 * const computer = new DimensionComputer();
 * const dimension = await computer.computeSystemDimension(universe);
 * console.log(`System dimension: ${dimension.systemDimension} bits`);
 */
export class DimensionComputer {
  /**
   * Create a new Dimension Computer
   * @param {Object} [config] - Configuration options
   * @param {Object} [config.constraints] - Namespace constraints
   * @param {number} [config.historySize=1000] - Historical records to keep
   * @param {number} [config.samplingRate=1.0] - Sampling rate for large universes
   */
  constructor(config = {}) {
    this.config = {
      constraints: config.constraints || {},
      historySize: config.historySize || 1000,
      samplingRate: config.samplingRate || 1.0,
      ...config
    };

    /** @type {Array<Object>} */
    this.history = [];

    /** @type {Map<string, Set<string>>} Cached term sets per partition */
    this._termCache = new Map();

    /** @type {Map<string, number>} Cached constraint factors */
    this._constraintFactors = new Map();
  }

  /**
   * Compute dimension for a single partition
   *
   * D_partition = log2(|S| * |P| * |O|) * constraintFactor
   *
   * @param {Object} partition - Partition object with store
   * @param {Object} [options] - Computation options
   * @returns {Promise<Object>} Partition dimension result
   */
  async computePartitionDimension(partition, options = {}) {
    const startTime = Date.now();

    // Extract unique terms from partition
    const terms = await this._extractTerms(partition, options);

    // Count unique terms
    const subjects = terms.subjects.size;
    const predicates = terms.predicates.size;
    const objects = terms.objects.size;
    const graphs = terms.graphs.size || 1; // Default graph if none

    // Raw dimension (unconstrained)
    // D = log2(S * P * O * G)
    const rawDimension = this._safeLog2(subjects * predicates * objects * graphs);

    // Compute constraint factor based on namespace restrictions
    const constraintFactor = await this._computeConstraintFactor(
      partition,
      terms
    );

    // Effective dimension (after constraints)
    const effectiveDimension = rawDimension * constraintFactor;

    const result = {
      partitionName: partition.name || 'unknown',
      subjects,
      predicates,
      objects,
      graphs,
      dimension: rawDimension,
      constraintFactor,
      effectiveDimension,
      computeTimeMs: Date.now() - startTime,
      timestamp: new Date().toISOString()
    };

    // Validate result
    PartitionDimensionSchema.parse(result);

    return result;
  }

  /**
   * Compute system-wide dimension from all partitions
   *
   * D_system = sum(D_partition) weighted by partition priority
   *
   * @param {Object} universe - Universe with partitions
   * @param {Object} [options] - Computation options
   * @returns {Promise<Object>} System dimension result
   */
  async computeSystemDimension(universe, options = {}) {
    const startTime = Date.now();
    const epoch = this._generateEpoch();

    const partitions = universe.getAllPartitions
      ? universe.getAllPartitions()
      : Object.values(universe.partitions || {});

    // Compute dimension for each partition
    const partitionDimensions = [];
    let totalQuads = 0;
    let totalPossibleQuads = 0;

    for (const partition of partitions) {
      const dim = await this.computePartitionDimension(partition, options);
      partitionDimensions.push(dim);

      // Track actual vs possible quads
      const actualQuads = partition.size || 0;
      totalQuads += actualQuads;

      // Possible quads = 2^dimension
      const possibleQuads = Math.pow(2, dim.effectiveDimension);
      totalPossibleQuads += possibleQuads;
    }

    // System dimension is sum of effective dimensions (log-additive)
    const systemDimension = partitionDimensions.reduce(
      (sum, d) => sum + d.effectiveDimension,
      0
    );

    // Utilization ratio = actual / possible
    const utilizationRatio = totalPossibleQuads > 0
      ? totalQuads / totalPossibleQuads
      : 0;

    // Compute deterministic hash
    const dimensionHash = await this._computeDimensionHash({
      epoch,
      systemDimension,
      partitionDimensions
    });

    const result = {
      epoch,
      systemDimension,
      partitionDimensions,
      totalQuads,
      possibleQuads: totalPossibleQuads,
      utilizationRatio,
      dimensionHash,
      computeTimeMs: Date.now() - startTime,
      computedAt: new Date().toISOString()
    };

    // Store in history
    this._recordHistory(result);

    return result;
  }

  /**
   * Compute dimension change from baseline
   *
   * @param {Object} currentDimension - Current system dimension
   * @param {Object} baselineDimension - Baseline to compare against
   * @returns {Object} Dimension delta analysis
   */
  computeDimensionDelta(currentDimension, baselineDimension) {
    const delta = currentDimension.systemDimension - baselineDimension.systemDimension;
    const percentChange = baselineDimension.systemDimension > 0
      ? (delta / baselineDimension.systemDimension) * 100
      : 0;

    // Per-partition deltas
    const partitionDeltas = [];
    for (const current of currentDimension.partitionDimensions) {
      const baseline = baselineDimension.partitionDimensions.find(
        p => p.partitionName === current.partitionName
      );

      if (baseline) {
        partitionDeltas.push({
          partitionName: current.partitionName,
          currentDimension: current.effectiveDimension,
          baselineDimension: baseline.effectiveDimension,
          delta: current.effectiveDimension - baseline.effectiveDimension,
          percentChange: baseline.effectiveDimension > 0
            ? ((current.effectiveDimension - baseline.effectiveDimension) / baseline.effectiveDimension) * 100
            : 0
        });
      }
    }

    // Determine trend
    let trend = 'stable';
    if (percentChange > 5) trend = 'increasing';
    else if (percentChange < -5) trend = 'decreasing';
    else if (Math.abs(percentChange) > 15) trend = 'volatile';

    return {
      currentEpoch: currentDimension.epoch,
      baselineEpoch: baselineDimension.epoch,
      systemDelta: delta,
      systemPercentChange: percentChange,
      partitionDeltas,
      trend,
      isHealthy: Math.abs(percentChange) < 20,
      analyzedAt: new Date().toISOString()
    };
  }

  /**
   * Get dimension trend over time
   *
   * @param {number} [windowSize=10] - Number of recent measurements to analyze
   * @returns {Object} Trend analysis
   */
  getDimensionTrend(windowSize = 10) {
    if (this.history.length < 2) {
      return {
        trend: 'insufficient_data',
        dataPoints: this.history.length,
        message: 'Need at least 2 measurements for trend analysis'
      };
    }

    const recentHistory = this.history.slice(-windowSize);
    const dimensions = recentHistory.map(h => h.systemDimension);

    // Calculate statistics
    const mean = dimensions.reduce((a, b) => a + b, 0) / dimensions.length;
    const variance = dimensions.reduce((sum, d) => sum + Math.pow(d - mean, 2), 0) / dimensions.length;
    const stdDev = Math.sqrt(variance);
    const coefficientOfVariation = mean > 0 ? (stdDev / mean) * 100 : 0;

    // Linear regression for trend direction
    const n = dimensions.length;
    const xMean = (n - 1) / 2;
    const yMean = mean;

    let numerator = 0;
    let denominator = 0;
    for (let i = 0; i < n; i++) {
      numerator += (i - xMean) * (dimensions[i] - yMean);
      denominator += (i - xMean) * (i - xMean);
    }

    const slope = denominator !== 0 ? numerator / denominator : 0;
    const slopePercent = mean > 0 ? (slope / mean) * 100 : 0;

    // Determine trend
    let trend;
    if (Math.abs(slopePercent) < 1 && coefficientOfVariation < 5) {
      trend = 'stable';
    } else if (slopePercent > 2) {
      trend = 'increasing';
    } else if (slopePercent < -2) {
      trend = 'decreasing';
    } else if (coefficientOfVariation > 15) {
      trend = 'volatile';
    } else {
      trend = 'stable';
    }

    return {
      trend,
      windowSize: recentHistory.length,
      mean,
      stdDev,
      coefficientOfVariation,
      slope,
      slopePercent,
      min: Math.min(...dimensions),
      max: Math.max(...dimensions),
      latest: dimensions[dimensions.length - 1],
      first: dimensions[0],
      overallChange: dimensions[dimensions.length - 1] - dimensions[0],
      history: recentHistory.map(h => ({
        epoch: h.epoch,
        dimension: h.systemDimension,
        timestamp: h.timestamp
      }))
    };
  }

  /**
   * Estimate dimension from constraints without full computation
   *
   * Useful for capacity planning before adding new constraints
   *
   * @param {Object} constraints - Proposed constraint configuration
   * @param {Object} currentDimension - Current system dimension
   * @returns {Object} Estimated dimension impact
   */
  estimateDimensionImpact(constraints, currentDimension) {
    // Start with current dimension
    let estimatedDimension = currentDimension.systemDimension;
    const impacts = [];

    // Analyze each constraint type
    if (constraints.namespaceRestrictions) {
      for (const [ns, restriction] of Object.entries(constraints.namespaceRestrictions)) {
        const impact = this._estimateNamespaceImpact(ns, restriction, currentDimension);
        impacts.push(impact);
        estimatedDimension -= impact.dimensionReduction;
      }
    }

    if (constraints.predicateWhitelist) {
      const predicateImpact = this._estimatePredicateWhitelistImpact(
        constraints.predicateWhitelist,
        currentDimension
      );
      impacts.push(predicateImpact);
      estimatedDimension -= predicateImpact.dimensionReduction;
    }

    if (constraints.maxQuadsPerPartition) {
      const quadLimitImpact = this._estimateQuadLimitImpact(
        constraints.maxQuadsPerPartition,
        currentDimension
      );
      impacts.push(quadLimitImpact);
      estimatedDimension -= quadLimitImpact.dimensionReduction;
    }

    const dimensionChange = estimatedDimension - currentDimension.systemDimension;
    const percentChange = (dimensionChange / currentDimension.systemDimension) * 100;

    return {
      currentDimension: currentDimension.systemDimension,
      estimatedDimension: Math.max(0, estimatedDimension),
      dimensionChange,
      percentChange,
      impacts,
      severity: this._categorizeSeverity(percentChange),
      recommendation: this._generateRecommendation(percentChange, impacts)
    };
  }

  /**
   * Compute dimension capacity utilization
   *
   * @param {Object} dimension - Current dimension measurement
   * @returns {Object} Capacity analysis
   */
  computeCapacityUtilization(dimension) {
    const totalCapacity = Math.pow(2, dimension.systemDimension);
    const usedCapacity = dimension.totalQuads;
    const utilizationPercent = totalCapacity > 0
      ? (usedCapacity / totalCapacity) * 100
      : 0;

    // Per-partition utilization
    const partitionUtilization = dimension.partitionDimensions.map(p => {
      const capacity = Math.pow(2, p.effectiveDimension);
      const used = p.quadCount || 0;
      return {
        partitionName: p.partitionName,
        capacity,
        used,
        utilizationPercent: capacity > 0 ? (used / capacity) * 100 : 0,
        bitsRemaining: p.effectiveDimension - this._safeLog2(used + 1)
      };
    });

    return {
      totalCapacity,
      usedCapacity,
      utilizationPercent,
      bitsUsed: this._safeLog2(usedCapacity + 1),
      bitsAvailable: dimension.systemDimension - this._safeLog2(usedCapacity + 1),
      partitionUtilization,
      headroom: 100 - utilizationPercent,
      status: this._categorizeCapacityStatus(utilizationPercent)
    };
  }

  /**
   * Extract unique terms from partition
   * @private
   */
  async _extractTerms(partition, options = {}) {
    const subjects = new Set();
    const predicates = new Set();
    const objects = new Set();
    const graphs = new Set();

    // Check cache first
    const cacheKey = `${partition.name || 'anon'}_${partition.size || 0}`;
    if (this._termCache.has(cacheKey) && !options.noCache) {
      return this._termCache.get(cacheKey);
    }

    // Get quads from partition store
    const store = partition.store;
    if (!store) {
      return { subjects, predicates, objects, graphs };
    }

    // Match all quads
    const quads = store.match ? store.match(null, null, null, null) : [];

    // Apply sampling for large datasets
    let sampleCount = 0;
    const sampleEvery = Math.ceil(1 / this.config.samplingRate);

    for (const quad of quads) {
      sampleCount++;

      // Skip based on sampling rate
      if (this.config.samplingRate < 1 && sampleCount % sampleEvery !== 0) {
        continue;
      }

      if (quad.subject) subjects.add(quad.subject.value);
      if (quad.predicate) predicates.add(quad.predicate.value);
      if (quad.object) objects.add(quad.object.value);
      if (quad.graph) graphs.add(quad.graph.value);
    }

    // Scale up if sampled
    const scaleFactor = 1 / this.config.samplingRate;
    if (this.config.samplingRate < 1) {
      // Estimate total unique terms based on sample
      const result = {
        subjects: this._scaleSet(subjects, scaleFactor),
        predicates: this._scaleSet(predicates, scaleFactor),
        objects: this._scaleSet(objects, scaleFactor),
        graphs: this._scaleSet(graphs, scaleFactor),
        sampled: true,
        sampleRate: this.config.samplingRate
      };
      this._termCache.set(cacheKey, result);
      return result;
    }

    const result = { subjects, predicates, objects, graphs };
    this._termCache.set(cacheKey, result);
    return result;
  }

  /**
   * Compute constraint factor for partition
   * @private
   */
  async _computeConstraintFactor(partition, terms) {
    let factor = 1.0;

    // Get partition-specific constraints
    const constraints = this.config.constraints[partition.name] || {};

    // Namespace restrictions reduce dimension
    if (constraints.allowedNamespaces) {
      const allowedPrefixes = new Set(constraints.allowedNamespaces);
      let restrictedTerms = 0;
      let totalTerms = 0;

      for (const term of terms.subjects) {
        totalTerms++;
        if (!this._termMatchesNamespaces(term, allowedPrefixes)) {
          restrictedTerms++;
        }
      }

      if (totalTerms > 0) {
        factor *= 1 - (restrictedTerms / totalTerms);
      }
    }

    // Predicate whitelist
    if (constraints.allowedPredicates) {
      const allowedPreds = new Set(constraints.allowedPredicates);
      const matchingPreds = [...terms.predicates].filter(p => allowedPreds.has(p));
      if (terms.predicates.size > 0) {
        factor *= matchingPreds.length / terms.predicates.size;
      }
    }

    // Read-only partition has reduced effective dimension
    if (partition.readOnly) {
      factor *= 0.1; // 90% reduction for read-only
    }

    // Cached constraint factors
    if (constraints.maxTerms) {
      const currentTerms = terms.subjects.size + terms.predicates.size + terms.objects.size;
      if (currentTerms > constraints.maxTerms) {
        factor *= constraints.maxTerms / currentTerms;
      }
    }

    return Math.max(0, Math.min(1, factor));
  }

  /**
   * Safe log2 computation
   * @private
   */
  _safeLog2(n) {
    if (n <= 0) return 0;
    return Math.log2(n);
  }

  /**
   * Scale a set based on sampling
   * @private
   */
  _scaleSet(set, factor) {
    // Create synthetic set with estimated size
    const estimatedSize = Math.ceil(set.size * factor);
    const scaledSet = new Set(set);
    // @ts-ignore - adding size property for estimation
    scaledSet.estimatedSize = estimatedSize;
    return scaledSet;
  }

  /**
   * Check if term matches allowed namespaces
   * @private
   */
  _termMatchesNamespaces(term, allowedPrefixes) {
    for (const prefix of allowedPrefixes) {
      if (term.startsWith(prefix)) return true;
    }
    return false;
  }

  /**
   * Generate epoch string
   * @private
   */
  _generateEpoch() {
    const now = new Date();
    const year = now.getUTCFullYear();
    const month = String(now.getUTCMonth() + 1).padStart(2, '0');
    const day = String(now.getUTCDate()).padStart(2, '0');
    const hour = String(now.getUTCHours()).padStart(2, '0');
    const minute = String(now.getUTCMinutes()).padStart(2, '0');
    const ms = String(now.getUTCMilliseconds()).padStart(3, '0');

    return `tau_${year}_${month}_${day}_${hour}${minute}_${ms}`;
  }

  /**
   * Compute deterministic hash for dimension result
   * @private
   */
  async _computeDimensionHash(data) {
    const canonical = JSON.stringify(data, Object.keys(data).sort());
    return blake3(canonical);
  }

  /**
   * Record measurement in history
   * @private
   */
  _recordHistory(dimension) {
    const record = {
      epoch: dimension.epoch,
      systemDimension: dimension.systemDimension,
      totalQuads: dimension.totalQuads,
      timestamp: dimension.computedAt
    };

    // Calculate delta from previous
    if (this.history.length > 0) {
      const previous = this.history[this.history.length - 1];
      record.deltaFromPrevious = dimension.systemDimension - previous.systemDimension;
      record.percentChange = previous.systemDimension > 0
        ? (record.deltaFromPrevious / previous.systemDimension) * 100
        : 0;
    }

    this.history.push(record);

    // Trim history if needed
    while (this.history.length > this.config.historySize) {
      this.history.shift();
    }
  }

  /**
   * Estimate namespace restriction impact
   * @private
   */
  _estimateNamespaceImpact(namespace, restriction, currentDimension) {
    // Estimate what fraction of terms use this namespace
    const estimatedFraction = 0.1; // Conservative estimate
    const reduction = restriction.blocked
      ? currentDimension.systemDimension * estimatedFraction
      : 0;

    return {
      type: 'namespace_restriction',
      namespace,
      restriction,
      estimatedFraction,
      dimensionReduction: reduction
    };
  }

  /**
   * Estimate predicate whitelist impact
   * @private
   */
  _estimatePredicateWhitelistImpact(whitelist, currentDimension) {
    // Estimate current predicates
    const avgPredicatesPerPartition = 50;
    const whitelistedCount = whitelist.length;
    const reductionRatio = Math.max(0, 1 - (whitelistedCount / avgPredicatesPerPartition));

    return {
      type: 'predicate_whitelist',
      whitelistSize: whitelistedCount,
      estimatedReductionRatio: reductionRatio,
      dimensionReduction: currentDimension.systemDimension * reductionRatio * 0.3
    };
  }

  /**
   * Estimate quad limit impact
   * @private
   */
  _estimateQuadLimitImpact(maxQuads, currentDimension) {
    const currentQuads = currentDimension.totalQuads;
    if (currentQuads <= maxQuads) {
      return {
        type: 'quad_limit',
        maxQuads,
        currentQuads,
        dimensionReduction: 0,
        message: 'Limit not constraining'
      };
    }

    const reductionBits = this._safeLog2(currentQuads) - this._safeLog2(maxQuads);
    return {
      type: 'quad_limit',
      maxQuads,
      currentQuads,
      dimensionReduction: reductionBits,
      message: 'Limit would reduce capacity'
    };
  }

  /**
   * Categorize severity of dimension change
   * @private
   */
  _categorizeSeverity(percentChange) {
    const abs = Math.abs(percentChange);
    if (abs < 5) return 'low';
    if (abs < 15) return 'medium';
    if (abs < 30) return 'high';
    return 'critical';
  }

  /**
   * Generate recommendation based on impact
   * @private
   */
  _generateRecommendation(percentChange, impacts) {
    if (percentChange > -5) {
      return 'Constraints have minimal impact. Safe to proceed.';
    }
    if (percentChange > -15) {
      return 'Moderate dimension reduction. Review necessity of each constraint.';
    }
    if (percentChange > -30) {
      return 'Significant dimension reduction. Consider phased rollout.';
    }
    return 'Critical dimension reduction. Strongly recommend reconsidering constraints.';
  }

  /**
   * Categorize capacity status
   * @private
   */
  _categorizeCapacityStatus(utilizationPercent) {
    if (utilizationPercent < 25) return 'underutilized';
    if (utilizationPercent < 50) return 'healthy';
    if (utilizationPercent < 75) return 'moderate';
    if (utilizationPercent < 90) return 'high';
    return 'critical';
  }

  /**
   * Clear term cache
   */
  clearCache() {
    this._termCache.clear();
    this._constraintFactors.clear();
  }

  /**
   * Clear history
   */
  clearHistory() {
    this.history = [];
  }

  /**
   * Export dimension data for external analysis
   * @returns {Object} Exportable dimension data
   */
  exportData() {
    return {
      history: [...this.history],
      config: { ...this.config },
      cacheSize: this._termCache.size,
      exportedAt: new Date().toISOString()
    };
  }

  /**
   * Import dimension history
   * @param {Array} historyData - Historical dimension records
   */
  importHistory(historyData) {
    for (const record of historyData) {
      if (record.epoch && typeof record.systemDimension === 'number') {
        this.history.push(record);
      }
    }
    // Trim if needed
    while (this.history.length > this.config.historySize) {
      this.history.shift();
    }
  }
}

/**
 * Create a dimension computer with default configuration
 * @param {Object} [config] - Configuration options
 * @returns {DimensionComputer} New dimension computer
 */
export function createDimensionComputer(config = {}) {
  return new DimensionComputer(config);
}

/**
 * Quick dimension computation for a universe
 * @param {Object} universe - Universe to measure
 * @param {Object} [options] - Computation options
 * @returns {Promise<Object>} Dimension result
 */
export async function computeDimension(universe, options = {}) {
  const computer = new DimensionComputer(options);
  return computer.computeSystemDimension(universe, options);
}

/**
 * Check if dimension is within healthy bounds
 * @param {Object} dimension - Dimension measurement
 * @param {Object} [thresholds] - Custom thresholds
 * @returns {Object} Health check result
 */
export function checkDimensionHealth(dimension, thresholds = {}) {
  const defaults = {
    minDimension: 10,
    maxDimensionDrop: 20,
    maxUtilization: 90,
    minUtilization: 1
  };

  const t = { ...defaults, ...thresholds };
  const issues = [];

  if (dimension.systemDimension < t.minDimension) {
    issues.push({
      type: 'low_dimension',
      message: `System dimension ${dimension.systemDimension.toFixed(2)} below minimum ${t.minDimension}`,
      severity: 'warning'
    });
  }

  const utilizationPercent = dimension.utilizationRatio * 100;
  if (utilizationPercent > t.maxUtilization) {
    issues.push({
      type: 'high_utilization',
      message: `Utilization ${utilizationPercent.toFixed(2)}% exceeds maximum ${t.maxUtilization}%`,
      severity: 'critical'
    });
  }

  if (utilizationPercent < t.minUtilization && dimension.totalQuads > 0) {
    issues.push({
      type: 'low_utilization',
      message: `Utilization ${utilizationPercent.toFixed(2)}% below minimum ${t.minUtilization}%`,
      severity: 'info'
    });
  }

  return {
    healthy: issues.filter(i => i.severity === 'critical').length === 0,
    issues,
    checkedAt: new Date().toISOString()
  };
}

export default DimensionComputer;
