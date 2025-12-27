/**
 * @fileoverview Correlation Computer - Compute TC (Total Correlation) from admission sequences
 *
 * TC measures the coupling between packages/partitions based on their admission sequences.
 * High TC = packages are tightly coupled (changes in one require changes in others)
 * Low TC = packages are modular and independent
 *
 * **Core Formula**:
 * TC = sum(H(X_i)) - H(X_1, X_2, ..., X_n)
 *
 * Where:
 * - H(X_i) = entropy of admission sequence for package i
 * - H(X_1,...,X_n) = joint entropy of all admission sequences
 *
 * **Interpretation**:
 * - TC = 0: Packages are completely independent
 * - TC > 0: Packages share mutual information (coupling)
 * - High TC: Changes tend to cascade across packages
 *
 * @module measurement/correlation-computer
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

/**
 * Admission event schema
 */
const AdmissionEventSchema = z.object({
  epoch: z.string(),
  partition: z.string(),
  decision: z.enum(['allow', 'deny', 'ALLOW', 'DENY']),
  timestamp: z.string().datetime(),
  capsuleId: z.string().optional(),
  quadCount: z.number().optional()
});

/**
 * Correlation result schema
 */
const CorrelationResultSchema = z.object({
  totalCorrelation: z.number(),
  normalizedTC: z.number(),
  partitionEntropies: z.record(z.number()),
  jointEntropy: z.number(),
  pairwiseMI: z.array(z.object({
    partition1: z.string(),
    partition2: z.string(),
    mutualInformation: z.number(),
    normalizedMI: z.number()
  })),
  epoch: z.string(),
  windowSize: z.number(),
  computedAt: z.string().datetime()
});

/**
 * Correlation Computer - Measures coupling between partitions
 *
 * @class CorrelationComputer
 * @example
 * const computer = new CorrelationComputer();
 * computer.recordAdmission({ partition: 'A', decision: 'allow', ... });
 * const tc = computer.computeTotalCorrelation();
 */
export class CorrelationComputer {
  /**
   * Create a new Correlation Computer
   * @param {Object} [config] - Configuration options
   * @param {number} [config.windowSize=1000] - Sliding window size for computation
   * @param {number} [config.binCount=10] - Number of bins for discretization
   * @param {number} [config.historySize=10000] - Maximum admission events to keep
   */
  constructor(config = {}) {
    this.config = {
      windowSize: config.windowSize || 1000,
      binCount: config.binCount || 10,
      historySize: config.historySize || 10000,
      ...config
    };

    /** @type {Array<Object>} Admission event history */
    this.admissionHistory = [];

    /** @type {Map<string, Array<Object>>} Per-partition admission sequences */
    this.partitionSequences = new Map();

    /** @type {Array<Object>} TC computation history */
    this.tcHistory = [];

    /** @type {Map<string, Map<string, number>>} Cached pairwise MI */
    this._miCache = new Map();
  }

  /**
   * Record an admission event
   *
   * @param {Object} event - Admission event
   * @param {string} event.epoch - Epoch identifier
   * @param {string} event.partition - Partition name
   * @param {'allow'|'deny'} event.decision - Admission decision
   * @param {string} event.timestamp - ISO timestamp
   * @param {string} [event.capsuleId] - Delta capsule ID
   * @param {number} [event.quadCount] - Number of quads in capsule
   */
  recordAdmission(event) {
    const validated = AdmissionEventSchema.parse(event);

    // Add to global history
    this.admissionHistory.push(validated);

    // Add to partition-specific sequence
    if (!this.partitionSequences.has(validated.partition)) {
      this.partitionSequences.set(validated.partition, []);
    }
    this.partitionSequences.get(validated.partition).push(validated);

    // Trim histories if needed
    this._trimHistories();

    // Invalidate MI cache
    this._miCache.clear();
  }

  /**
   * Record multiple admission events
   *
   * @param {Array<Object>} events - Array of admission events
   */
  recordAdmissions(events) {
    for (const event of events) {
      this.recordAdmission(event);
    }
  }

  /**
   * Compute Total Correlation across all partitions
   *
   * TC = sum(H(X_i)) - H(X_1, ..., X_n)
   *
   * @param {Object} [options] - Computation options
   * @param {number} [options.windowSize] - Override window size
   * @returns {Object} Total correlation result
   */
  computeTotalCorrelation(options = {}) {
    const windowSize = options.windowSize || this.config.windowSize;
    const epoch = this._generateEpoch();

    // Get partition names with data
    const partitions = Array.from(this.partitionSequences.keys());

    if (partitions.length < 2) {
      return {
        totalCorrelation: 0,
        normalizedTC: 0,
        partitionEntropies: {},
        jointEntropy: 0,
        pairwiseMI: [],
        epoch,
        windowSize,
        message: 'Need at least 2 partitions for correlation',
        computedAt: new Date().toISOString()
      };
    }

    // Build time-aligned sequences
    const alignedSequences = this._buildAlignedSequences(partitions, windowSize);

    if (alignedSequences.timePoints.length < 10) {
      return {
        totalCorrelation: 0,
        normalizedTC: 0,
        partitionEntropies: {},
        jointEntropy: 0,
        pairwiseMI: [],
        epoch,
        windowSize,
        message: 'Insufficient aligned data points',
        computedAt: new Date().toISOString()
      };
    }

    // Compute individual entropies
    const partitionEntropies = {};
    let sumEntropies = 0;

    for (const partition of partitions) {
      const sequence = alignedSequences.sequences.get(partition);
      const entropy = this._computeEntropy(sequence);
      partitionEntropies[partition] = entropy;
      sumEntropies += entropy;
    }

    // Compute joint entropy
    const jointEntropy = this._computeJointEntropy(alignedSequences.sequences, partitions);

    // TC = sum(H(X_i)) - H(X_1, ..., X_n)
    const totalCorrelation = Math.max(0, sumEntropies - jointEntropy);

    // Normalized TC (0-1 range)
    const maxTC = sumEntropies - Math.max(...Object.values(partitionEntropies));
    const normalizedTC = maxTC > 0 ? totalCorrelation / maxTC : 0;

    // Compute pairwise mutual information
    const pairwiseMI = this._computePairwiseMI(alignedSequences.sequences, partitions);

    const result = {
      totalCorrelation,
      normalizedTC,
      partitionEntropies,
      jointEntropy,
      pairwiseMI,
      epoch,
      windowSize: alignedSequences.timePoints.length,
      computedAt: new Date().toISOString()
    };

    // Store in history
    this._recordTCHistory(result);

    return result;
  }

  /**
   * Compute pairwise Mutual Information between two partitions
   *
   * MI(X,Y) = H(X) + H(Y) - H(X,Y)
   *
   * @param {string} partition1 - First partition name
   * @param {string} partition2 - Second partition name
   * @param {Object} [options] - Computation options
   * @returns {Object} Mutual information result
   */
  computeMutualInformation(partition1, partition2, options = {}) {
    const windowSize = options.windowSize || this.config.windowSize;

    // Check cache
    const cacheKey = `${partition1}_${partition2}`;
    if (this._miCache.has(partition1) && this._miCache.get(partition1).has(partition2)) {
      return this._miCache.get(partition1).get(partition2);
    }

    // Build aligned sequences for these two partitions
    const alignedSequences = this._buildAlignedSequences(
      [partition1, partition2],
      windowSize
    );

    if (alignedSequences.timePoints.length < 10) {
      return {
        partition1,
        partition2,
        mutualInformation: 0,
        normalizedMI: 0,
        message: 'Insufficient aligned data'
      };
    }

    const seq1 = alignedSequences.sequences.get(partition1);
    const seq2 = alignedSequences.sequences.get(partition2);

    // Compute entropies
    const h1 = this._computeEntropy(seq1);
    const h2 = this._computeEntropy(seq2);
    const jointH = this._computeJointEntropy(
      new Map([[partition1, seq1], [partition2, seq2]]),
      [partition1, partition2]
    );

    // MI = H(X) + H(Y) - H(X,Y)
    const mutualInformation = Math.max(0, h1 + h2 - jointH);

    // Normalized MI (0-1 range) using min entropy
    const minEntropy = Math.min(h1, h2);
    const normalizedMI = minEntropy > 0 ? mutualInformation / minEntropy : 0;

    const result = {
      partition1,
      partition2,
      mutualInformation,
      normalizedMI,
      entropy1: h1,
      entropy2: h2,
      jointEntropy: jointH,
      dataPoints: alignedSequences.timePoints.length
    };

    // Cache result
    if (!this._miCache.has(partition1)) {
      this._miCache.set(partition1, new Map());
    }
    this._miCache.get(partition1).set(partition2, result);

    return result;
  }

  /**
   * Detect coupling anomalies (sudden TC spikes or drops)
   *
   * @param {Object} [options] - Detection options
   * @param {number} [options.threshold=2] - Standard deviation threshold
   * @returns {Object} Anomaly detection result
   */
  detectCouplingAnomalies(options = {}) {
    const threshold = options.threshold || 2;

    if (this.tcHistory.length < 5) {
      return {
        anomalyDetected: false,
        message: 'Insufficient history for anomaly detection',
        historySize: this.tcHistory.length
      };
    }

    // Get recent TC values
    const recentTC = this.tcHistory.slice(-20).map(h => h.totalCorrelation);

    // Compute statistics
    const mean = recentTC.reduce((a, b) => a + b, 0) / recentTC.length;
    const variance = recentTC.reduce((sum, tc) => sum + Math.pow(tc - mean, 2), 0) / recentTC.length;
    const stdDev = Math.sqrt(variance);

    // Check latest value
    const latest = recentTC[recentTC.length - 1];
    const zScore = stdDev > 0 ? (latest - mean) / stdDev : 0;

    const anomalyDetected = Math.abs(zScore) > threshold;

    // Identify which partition pairs are most correlated
    const highCorrelationPairs = [];
    if (this.tcHistory.length > 0) {
      const latestResult = this.tcHistory[this.tcHistory.length - 1];
      if (latestResult.pairwiseMI) {
        for (const pair of latestResult.pairwiseMI) {
          if (pair.normalizedMI > 0.5) {
            highCorrelationPairs.push({
              partitions: [pair.partition1, pair.partition2],
              correlation: pair.normalizedMI
            });
          }
        }
      }
    }

    return {
      anomalyDetected,
      latestTC: latest,
      mean,
      stdDev,
      zScore,
      threshold,
      anomalyType: zScore > threshold ? 'spike' : (zScore < -threshold ? 'drop' : 'none'),
      highCorrelationPairs,
      recommendation: anomalyDetected
        ? 'Investigate recent changes - coupling has changed significantly'
        : 'Coupling is within normal bounds',
      checkedAt: new Date().toISOString()
    };
  }

  /**
   * Get correlation trend over time
   *
   * @param {number} [windowSize=10] - Number of measurements to analyze
   * @returns {Object} Trend analysis
   */
  getCorrelationTrend(windowSize = 10) {
    if (this.tcHistory.length < 2) {
      return {
        trend: 'insufficient_data',
        dataPoints: this.tcHistory.length
      };
    }

    const recent = this.tcHistory.slice(-windowSize);
    const tcValues = recent.map(h => h.totalCorrelation);

    // Calculate statistics
    const mean = tcValues.reduce((a, b) => a + b, 0) / tcValues.length;
    const variance = tcValues.reduce((sum, tc) => sum + Math.pow(tc - mean, 2), 0) / tcValues.length;
    const stdDev = Math.sqrt(variance);

    // Linear regression for trend
    const n = tcValues.length;
    const xMean = (n - 1) / 2;

    let numerator = 0;
    let denominator = 0;
    for (let i = 0; i < n; i++) {
      numerator += (i - xMean) * (tcValues[i] - mean);
      denominator += (i - xMean) * (i - xMean);
    }

    const slope = denominator !== 0 ? numerator / denominator : 0;

    // Determine trend
    let trend;
    if (Math.abs(slope) < 0.01 && stdDev < mean * 0.1) {
      trend = 'stable';
    } else if (slope > 0.05) {
      trend = 'increasing';
    } else if (slope < -0.05) {
      trend = 'decreasing';
    } else if (stdDev > mean * 0.3) {
      trend = 'volatile';
    } else {
      trend = 'stable';
    }

    return {
      trend,
      windowSize: recent.length,
      mean,
      stdDev,
      slope,
      min: Math.min(...tcValues),
      max: Math.max(...tcValues),
      latest: tcValues[tcValues.length - 1],
      first: tcValues[0],
      interpretation: this._interpretTrend(trend, mean)
    };
  }

  /**
   * Get partition dependency graph
   *
   * @param {number} [miThreshold=0.1] - Minimum MI to include edge
   * @returns {Object} Dependency graph
   */
  getPartitionDependencyGraph(miThreshold = 0.1) {
    const partitions = Array.from(this.partitionSequences.keys());
    const nodes = partitions.map(p => ({
      id: p,
      eventCount: this.partitionSequences.get(p).length
    }));

    const edges = [];
    const tc = this.computeTotalCorrelation();

    if (tc.pairwiseMI) {
      for (const pair of tc.pairwiseMI) {
        if (pair.mutualInformation >= miThreshold) {
          edges.push({
            source: pair.partition1,
            target: pair.partition2,
            weight: pair.mutualInformation,
            normalizedWeight: pair.normalizedMI
          });
        }
      }
    }

    // Compute graph metrics
    const avgDegree = edges.length > 0
      ? (edges.length * 2) / nodes.length
      : 0;

    const density = nodes.length > 1
      ? (edges.length * 2) / (nodes.length * (nodes.length - 1))
      : 0;

    return {
      nodes,
      edges,
      metrics: {
        nodeCount: nodes.length,
        edgeCount: edges.length,
        averageDegree: avgDegree,
        density,
        totalCorrelation: tc.totalCorrelation
      },
      generatedAt: new Date().toISOString()
    };
  }

  /**
   * Build time-aligned sequences for partitions
   * @private
   */
  _buildAlignedSequences(partitions, windowSize) {
    // Find common time window
    const allEvents = [];
    for (const partition of partitions) {
      const seq = this.partitionSequences.get(partition) || [];
      for (const event of seq) {
        allEvents.push({
          partition,
          timestamp: new Date(event.timestamp).getTime(),
          decision: event.decision.toLowerCase() === 'allow' ? 1 : 0,
          quadCount: event.quadCount || 0
        });
      }
    }

    // Sort by timestamp
    allEvents.sort((a, b) => a.timestamp - b.timestamp);

    // Take last windowSize events
    const windowEvents = allEvents.slice(-windowSize);

    // Create time buckets (group by minute)
    const buckets = new Map();
    for (const event of windowEvents) {
      const bucketKey = Math.floor(event.timestamp / 60000);
      if (!buckets.has(bucketKey)) {
        buckets.set(bucketKey, new Map());
      }
      const bucket = buckets.get(bucketKey);

      if (!bucket.has(event.partition)) {
        bucket.set(event.partition, []);
      }
      bucket.get(event.partition).push(event);
    }

    // Build aligned sequences
    const timePoints = Array.from(buckets.keys()).sort((a, b) => a - b);
    const sequences = new Map();

    for (const partition of partitions) {
      sequences.set(partition, []);
    }

    for (const timePoint of timePoints) {
      const bucket = buckets.get(timePoint);

      for (const partition of partitions) {
        const events = bucket.get(partition) || [];
        // Aggregate events in bucket
        if (events.length === 0) {
          sequences.get(partition).push(0); // No activity
        } else {
          const avgDecision = events.reduce((sum, e) => sum + e.decision, 0) / events.length;
          sequences.get(partition).push(avgDecision);
        }
      }
    }

    return { sequences, timePoints };
  }

  /**
   * Compute entropy of a sequence
   * @private
   */
  _computeEntropy(sequence) {
    if (!sequence || sequence.length === 0) return 0;

    // Discretize into bins
    const bins = new Map();
    const binSize = 1 / this.config.binCount;

    for (const value of sequence) {
      const binIndex = Math.min(
        Math.floor(value / binSize),
        this.config.binCount - 1
      );
      bins.set(binIndex, (bins.get(binIndex) || 0) + 1);
    }

    // Compute probabilities and entropy
    const n = sequence.length;
    let entropy = 0;

    for (const count of bins.values()) {
      const p = count / n;
      if (p > 0) {
        entropy -= p * Math.log2(p);
      }
    }

    return entropy;
  }

  /**
   * Compute joint entropy of multiple sequences
   * @private
   */
  _computeJointEntropy(sequences, partitions) {
    if (partitions.length === 0) return 0;

    const n = sequences.get(partitions[0]).length;
    if (n === 0) return 0;

    // Create joint states
    const jointCounts = new Map();
    const binSize = 1 / this.config.binCount;

    for (let i = 0; i < n; i++) {
      // Build joint state key
      const stateKey = partitions.map(p => {
        const value = sequences.get(p)[i];
        const binIndex = Math.min(
          Math.floor(value / binSize),
          this.config.binCount - 1
        );
        return binIndex;
      }).join('_');

      jointCounts.set(stateKey, (jointCounts.get(stateKey) || 0) + 1);
    }

    // Compute joint entropy
    let entropy = 0;
    for (const count of jointCounts.values()) {
      const p = count / n;
      if (p > 0) {
        entropy -= p * Math.log2(p);
      }
    }

    return entropy;
  }

  /**
   * Compute pairwise MI for all partition pairs
   * @private
   */
  _computePairwiseMI(sequences, partitions) {
    const results = [];

    for (let i = 0; i < partitions.length; i++) {
      for (let j = i + 1; j < partitions.length; j++) {
        const p1 = partitions[i];
        const p2 = partitions[j];

        const seq1 = sequences.get(p1);
        const seq2 = sequences.get(p2);

        const h1 = this._computeEntropy(seq1);
        const h2 = this._computeEntropy(seq2);
        const jointH = this._computeJointEntropy(
          new Map([[p1, seq1], [p2, seq2]]),
          [p1, p2]
        );

        const mi = Math.max(0, h1 + h2 - jointH);
        const normalizedMI = Math.min(h1, h2) > 0 ? mi / Math.min(h1, h2) : 0;

        results.push({
          partition1: p1,
          partition2: p2,
          mutualInformation: mi,
          normalizedMI
        });
      }
    }

    // Sort by MI descending
    results.sort((a, b) => b.mutualInformation - a.mutualInformation);

    return results;
  }

  /**
   * Generate epoch string
   * @private
   */
  _generateEpoch() {
    const now = new Date();
    return `tau_${now.toISOString().replace(/[-:T.Z]/g, '_').slice(0, 19)}`;
  }

  /**
   * Trim histories to configured limits
   * @private
   */
  _trimHistories() {
    // Trim global history
    while (this.admissionHistory.length > this.config.historySize) {
      const removed = this.admissionHistory.shift();
      // Also remove from partition sequence
      const seq = this.partitionSequences.get(removed.partition);
      if (seq && seq.length > 0 && seq[0].epoch === removed.epoch) {
        seq.shift();
      }
    }

    // Trim TC history
    while (this.tcHistory.length > 100) {
      this.tcHistory.shift();
    }
  }

  /**
   * Record TC result in history
   * @private
   */
  _recordTCHistory(result) {
    this.tcHistory.push({
      epoch: result.epoch,
      totalCorrelation: result.totalCorrelation,
      normalizedTC: result.normalizedTC,
      partitionCount: Object.keys(result.partitionEntropies).length,
      pairwiseMI: result.pairwiseMI,
      timestamp: result.computedAt
    });
  }

  /**
   * Interpret correlation trend
   * @private
   */
  _interpretTrend(trend, meanTC) {
    const interpretations = {
      stable: meanTC < 0.3
        ? 'Partitions are loosely coupled and stable - good modularity'
        : 'Partitions have consistent coupling - monitor for changes',
      increasing: 'Coupling is increasing - changes are becoming more interdependent',
      decreasing: 'Coupling is decreasing - partitions are becoming more modular',
      volatile: 'Coupling is unstable - investigate inconsistent change patterns'
    };
    return interpretations[trend] || 'Unknown trend';
  }

  /**
   * Clear all data
   */
  clear() {
    this.admissionHistory = [];
    this.partitionSequences.clear();
    this.tcHistory = [];
    this._miCache.clear();
  }

  /**
   * Export data for persistence
   * @returns {Object} Exportable data
   */
  exportData() {
    return {
      admissionHistory: [...this.admissionHistory],
      tcHistory: [...this.tcHistory],
      config: { ...this.config },
      exportedAt: new Date().toISOString()
    };
  }

  /**
   * Import historical data
   * @param {Object} data - Previously exported data
   */
  importData(data) {
    if (data.admissionHistory) {
      for (const event of data.admissionHistory) {
        this.recordAdmission(event);
      }
    }
    if (data.tcHistory) {
      this.tcHistory = [...data.tcHistory];
    }
  }

  /**
   * Get partition statistics
   * @returns {Object} Per-partition statistics
   */
  getPartitionStats() {
    const stats = {};

    for (const [partition, sequence] of this.partitionSequences) {
      const decisions = sequence.map(e => e.decision.toLowerCase());
      const allowCount = decisions.filter(d => d === 'allow').length;
      const denyCount = decisions.filter(d => d === 'deny').length;

      stats[partition] = {
        totalEvents: sequence.length,
        allowCount,
        denyCount,
        allowRate: sequence.length > 0 ? allowCount / sequence.length : 0,
        entropy: this._computeEntropy(
          sequence.map(e => e.decision.toLowerCase() === 'allow' ? 1 : 0)
        ),
        firstEvent: sequence.length > 0 ? sequence[0].timestamp : null,
        lastEvent: sequence.length > 0 ? sequence[sequence.length - 1].timestamp : null
      };
    }

    return stats;
  }
}

/**
 * Create a correlation computer with default configuration
 * @param {Object} [config] - Configuration options
 * @returns {CorrelationComputer} New correlation computer
 */
export function createCorrelationComputer(config = {}) {
  return new CorrelationComputer(config);
}

/**
 * Quick TC computation from admission events
 * @param {Array<Object>} events - Admission events
 * @param {Object} [options] - Computation options
 * @returns {Object} Total correlation result
 */
export function computeTotalCorrelation(events, options = {}) {
  const computer = new CorrelationComputer(options);
  computer.recordAdmissions(events);
  return computer.computeTotalCorrelation(options);
}

/**
 * Check if correlation is healthy
 * @param {Object} tcResult - Total correlation result
 * @param {Object} [thresholds] - Custom thresholds
 * @returns {Object} Health check result
 */
export function checkCorrelationHealth(tcResult, thresholds = {}) {
  const defaults = {
    maxTC: 0.7,
    maxNormalizedTC: 0.8,
    minPartitions: 2
  };

  const t = { ...defaults, ...thresholds };
  const issues = [];

  if (tcResult.normalizedTC > t.maxNormalizedTC) {
    issues.push({
      type: 'high_coupling',
      message: `Normalized TC ${tcResult.normalizedTC.toFixed(3)} exceeds threshold ${t.maxNormalizedTC}`,
      severity: 'warning'
    });
  }

  const partitionCount = Object.keys(tcResult.partitionEntropies).length;
  if (partitionCount < t.minPartitions) {
    issues.push({
      type: 'insufficient_partitions',
      message: `Only ${partitionCount} partitions - TC may be unreliable`,
      severity: 'info'
    });
  }

  // Check for highly coupled pairs
  if (tcResult.pairwiseMI) {
    const highlyCoupled = tcResult.pairwiseMI.filter(p => p.normalizedMI > 0.8);
    if (highlyCoupled.length > 0) {
      issues.push({
        type: 'partition_pair_coupling',
        message: `${highlyCoupled.length} partition pair(s) are highly coupled (MI > 0.8)`,
        pairs: highlyCoupled.map(p => `${p.partition1}-${p.partition2}`),
        severity: 'warning'
      });
    }
  }

  return {
    healthy: issues.filter(i => i.severity === 'critical').length === 0,
    issues,
    totalCorrelation: tcResult.totalCorrelation,
    normalizedTC: tcResult.normalizedTC,
    checkedAt: new Date().toISOString()
  };
}

export default CorrelationComputer;
