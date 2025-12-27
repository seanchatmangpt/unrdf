/**
 * @fileoverview Transfer Entropy Computer - Compute TE from causal paths
 *
 * TE measures directed information flow between partitions.
 * Unlike mutual information, TE is asymmetric and captures causality.
 *
 * **Core Formula**:
 * TE(X -> Y) = I(X(t-1); Y(t) | Y(t-1))
 *            = H(Y(t) | Y(t-1)) - H(Y(t) | Y(t-1), X(t-1))
 *
 * Where:
 * - X(t-1) = past state of source partition
 * - Y(t) = current state of target partition
 * - Y(t-1) = past state of target partition
 *
 * **Interpretation**:
 * - TE(X -> Y) > 0: X causally influences Y
 * - TE(X -> Y) = 0: X does not influence Y beyond Y's own history
 * - Asymmetric: TE(X -> Y) != TE(Y -> X) generally
 *
 * @module measurement/transfer-entropy-computer
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

/**
 * State event schema
 */
const StateEventSchema = z.object({
  epoch: z.string(),
  partition: z.string(),
  state: z.union([z.number(), z.string()]),
  timestamp: z.string().datetime(),
  metadata: z.record(z.any()).optional()
});

/**
 * Transfer entropy result schema
 */
const TransferEntropyResultSchema = z.object({
  source: z.string(),
  target: z.string(),
  transferEntropy: z.number(),
  normalizedTE: z.number(),
  significance: z.number(),
  lag: z.number(),
  sampleSize: z.number(),
  computedAt: z.string().datetime()
});

/**
 * Causal graph schema
 */
const CausalGraphSchema = z.object({
  nodes: z.array(z.object({
    id: z.string(),
    inDegree: z.number(),
    outDegree: z.number(),
    netInfluence: z.number()
  })),
  edges: z.array(z.object({
    source: z.string(),
    target: z.string(),
    te: z.number(),
    normalizedTE: z.number(),
    lag: z.number()
  })),
  epoch: z.string(),
  computedAt: z.string().datetime()
});

/**
 * Transfer Entropy Computer - Measures causal influence between partitions
 *
 * @class TransferEntropyComputer
 * @example
 * const computer = new TransferEntropyComputer();
 * computer.recordState({ partition: 'A', state: 1, ... });
 * const te = computer.computeTransferEntropy('A', 'B');
 */
export class TransferEntropyComputer {
  /**
   * Create a new Transfer Entropy Computer
   * @param {Object} [config] - Configuration options
   * @param {number} [config.lag=1] - Time lag for causality (default 1 epoch)
   * @param {number} [config.historyLength=3] - History length for conditioning
   * @param {number} [config.binCount=10] - Bins for state discretization
   * @param {number} [config.maxHistorySize=10000] - Maximum events to keep
   * @param {number} [config.significanceThreshold=0.05] - Significance threshold
   */
  constructor(config = {}) {
    this.config = {
      lag: config.lag || 1,
      historyLength: config.historyLength || 3,
      binCount: config.binCount || 10,
      maxHistorySize: config.maxHistorySize || 10000,
      significanceThreshold: config.significanceThreshold || 0.05,
      ...config
    };

    /** @type {Map<string, Array<Object>>} State history per partition */
    this.stateHistory = new Map();

    /** @type {Array<Object>} TE computation history */
    this.teHistory = [];

    /** @type {Map<string, Object>} Cached TE results */
    this._teCache = new Map();

    /** @type {number} Last cache invalidation time */
    this._lastCacheTime = 0;
  }

  /**
   * Record a state observation for a partition
   *
   * @param {Object} event - State event
   * @param {string} event.epoch - Epoch identifier
   * @param {string} event.partition - Partition name
   * @param {number|string} event.state - Current state value
   * @param {string} event.timestamp - ISO timestamp
   * @param {Object} [event.metadata] - Additional metadata
   */
  recordState(event) {
    const validated = StateEventSchema.parse(event);

    if (!this.stateHistory.has(validated.partition)) {
      this.stateHistory.set(validated.partition, []);
    }

    const history = this.stateHistory.get(validated.partition);
    history.push({
      ...validated,
      numericState: this._toNumericState(validated.state),
      recordedAt: Date.now()
    });

    // Trim if needed
    while (history.length > this.config.maxHistorySize) {
      history.shift();
    }

    // Invalidate cache
    this._invalidateCache();
  }

  /**
   * Record multiple state events
   *
   * @param {Array<Object>} events - Array of state events
   */
  recordStates(events) {
    for (const event of events) {
      this.recordState(event);
    }
  }

  /**
   * Compute Transfer Entropy from source to target partition
   *
   * TE(X -> Y) = H(Y(t) | Y(t-1)) - H(Y(t) | Y(t-1), X(t-1))
   *
   * @param {string} source - Source partition name
   * @param {string} target - Target partition name
   * @param {Object} [options] - Computation options
   * @param {number} [options.lag] - Override lag value
   * @returns {Object} Transfer entropy result
   */
  computeTransferEntropy(source, target, options = {}) {
    const lag = options.lag || this.config.lag;
    const cacheKey = `${source}_${target}_${lag}`;

    // Check cache
    if (this._teCache.has(cacheKey)) {
      return this._teCache.get(cacheKey);
    }

    // Get aligned time series
    const aligned = this._alignTimeSeries(source, target, lag);

    if (aligned.length < 20) {
      return {
        source,
        target,
        transferEntropy: 0,
        normalizedTE: 0,
        significance: 1.0,
        lag,
        sampleSize: aligned.length,
        message: 'Insufficient data for TE computation',
        computedAt: new Date().toISOString()
      };
    }

    // Build state vectors
    const { sourceHistory, targetHistory, targetCurrent } = this._buildStateVectors(
      aligned, lag
    );

    // Compute conditional entropies
    // H(Y(t) | Y(t-1)) - entropy of target given its own past
    const hYgivenY = this._computeConditionalEntropy(targetCurrent, targetHistory);

    // H(Y(t) | Y(t-1), X(t-1)) - entropy of target given its past and source past
    const hYgivenYX = this._computeConditionalEntropyJoint(
      targetCurrent, targetHistory, sourceHistory
    );

    // TE = H(Y(t)|Y(t-1)) - H(Y(t)|Y(t-1),X(t-1))
    const transferEntropy = Math.max(0, hYgivenY - hYgivenYX);

    // Normalized TE (0-1 range)
    const normalizedTE = hYgivenY > 0 ? transferEntropy / hYgivenY : 0;

    // Compute significance via shuffling
    const significance = this._computeSignificance(
      sourceHistory, targetHistory, targetCurrent, transferEntropy
    );

    const result = {
      source,
      target,
      transferEntropy,
      normalizedTE,
      significance,
      lag,
      sampleSize: aligned.length,
      hYgivenY,
      hYgivenYX,
      isSignificant: significance < this.config.significanceThreshold,
      computedAt: new Date().toISOString()
    };

    // Cache result
    this._teCache.set(cacheKey, result);

    // Record in history
    this._recordTEHistory(result);

    return result;
  }

  /**
   * Compute bidirectional Transfer Entropy (both directions)
   *
   * @param {string} partition1 - First partition
   * @param {string} partition2 - Second partition
   * @param {Object} [options] - Computation options
   * @returns {Object} Bidirectional TE result
   */
  computeBidirectionalTE(partition1, partition2, options = {}) {
    const te12 = this.computeTransferEntropy(partition1, partition2, options);
    const te21 = this.computeTransferEntropy(partition2, partition1, options);

    // Net information flow
    const netFlow = te12.transferEntropy - te21.transferEntropy;
    const dominantDirection = netFlow > 0 ? `${partition1} -> ${partition2}` : `${partition2} -> ${partition1}`;

    // Asymmetry ratio
    const maxTE = Math.max(te12.transferEntropy, te21.transferEntropy);
    const asymmetryRatio = maxTE > 0 ? Math.abs(netFlow) / maxTE : 0;

    return {
      forward: te12,
      backward: te21,
      netFlow,
      netFlowDirection: dominantDirection,
      asymmetryRatio,
      isBidirectional: te12.isSignificant && te21.isSignificant,
      isAsymmetric: asymmetryRatio > 0.5,
      interpretation: this._interpretBidirectionalTE(te12, te21, netFlow),
      computedAt: new Date().toISOString()
    };
  }

  /**
   * Compute full causal graph for all partitions
   *
   * @param {Object} [options] - Computation options
   * @param {number} [options.teThreshold=0.01] - Minimum TE to include edge
   * @returns {Object} Causal graph
   */
  computeCausalGraph(options = {}) {
    const teThreshold = options.teThreshold || 0.01;
    const partitions = Array.from(this.stateHistory.keys());
    const epoch = this._generateEpoch();

    if (partitions.length < 2) {
      return {
        nodes: partitions.map(p => ({ id: p, inDegree: 0, outDegree: 0, netInfluence: 0 })),
        edges: [],
        epoch,
        message: 'Need at least 2 partitions for causal graph',
        computedAt: new Date().toISOString()
      };
    }

    // Compute TE for all pairs
    const edges = [];
    const nodeInfluence = new Map();

    for (const p of partitions) {
      nodeInfluence.set(p, { inDegree: 0, outDegree: 0, inTE: 0, outTE: 0 });
    }

    for (const source of partitions) {
      for (const target of partitions) {
        if (source === target) continue;

        const te = this.computeTransferEntropy(source, target, options);

        if (te.transferEntropy >= teThreshold && te.isSignificant) {
          edges.push({
            source,
            target,
            te: te.transferEntropy,
            normalizedTE: te.normalizedTE,
            lag: te.lag,
            significance: te.significance
          });

          // Update node influence
          const sourceInf = nodeInfluence.get(source);
          const targetInf = nodeInfluence.get(target);

          sourceInf.outDegree++;
          sourceInf.outTE += te.transferEntropy;
          targetInf.inDegree++;
          targetInf.inTE += te.transferEntropy;
        }
      }
    }

    // Build nodes with influence metrics
    const nodes = partitions.map(p => {
      const inf = nodeInfluence.get(p);
      return {
        id: p,
        inDegree: inf.inDegree,
        outDegree: inf.outDegree,
        totalInTE: inf.inTE,
        totalOutTE: inf.outTE,
        netInfluence: inf.outTE - inf.inTE,
        role: this._classifyNodeRole(inf)
      };
    });

    // Sort edges by TE
    edges.sort((a, b) => b.te - a.te);

    // Compute graph metrics
    const totalTE = edges.reduce((sum, e) => sum + e.te, 0);
    const avgTE = edges.length > 0 ? totalTE / edges.length : 0;
    const density = partitions.length > 1
      ? edges.length / (partitions.length * (partitions.length - 1))
      : 0;

    return {
      nodes,
      edges,
      metrics: {
        nodeCount: nodes.length,
        edgeCount: edges.length,
        totalTransferEntropy: totalTE,
        averageTE: avgTE,
        graphDensity: density,
        maxTE: edges.length > 0 ? Math.max(...edges.map(e => e.te)) : 0
      },
      epoch,
      computedAt: new Date().toISOString()
    };
  }

  /**
   * Detect causal anomalies (sudden changes in information flow)
   *
   * @param {Object} [options] - Detection options
   * @returns {Object} Anomaly detection result
   */
  detectCausalAnomalies(options = {}) {
    const threshold = options.threshold || 2.0;

    if (this.teHistory.length < 10) {
      return {
        anomalyDetected: false,
        message: 'Insufficient history for anomaly detection',
        historySize: this.teHistory.length
      };
    }

    // Group by source-target pair
    const pairHistory = new Map();
    for (const record of this.teHistory) {
      const key = `${record.source}_${record.target}`;
      if (!pairHistory.has(key)) {
        pairHistory.set(key, []);
      }
      pairHistory.get(key).push(record.transferEntropy);
    }

    const anomalies = [];

    for (const [pair, values] of pairHistory) {
      if (values.length < 5) continue;

      const recent = values.slice(-10);
      const mean = recent.reduce((a, b) => a + b, 0) / recent.length;
      const variance = recent.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / recent.length;
      const stdDev = Math.sqrt(variance);

      const latest = recent[recent.length - 1];
      const zScore = stdDev > 0 ? (latest - mean) / stdDev : 0;

      if (Math.abs(zScore) > threshold) {
        const [source, target] = pair.split('_');
        anomalies.push({
          source,
          target,
          type: zScore > 0 ? 'spike' : 'drop',
          latestTE: latest,
          mean,
          stdDev,
          zScore,
          severity: Math.abs(zScore) > 3 ? 'critical' : 'warning'
        });
      }
    }

    // Sort by severity
    anomalies.sort((a, b) => Math.abs(b.zScore) - Math.abs(a.zScore));

    return {
      anomalyDetected: anomalies.length > 0,
      anomalies,
      totalPairsAnalyzed: pairHistory.size,
      recommendation: anomalies.length > 0
        ? 'Investigate causal relationships that have significantly changed'
        : 'Information flow patterns are within normal bounds',
      checkedAt: new Date().toISOString()
    };
  }

  /**
   * Find optimal lag for TE computation
   *
   * @param {string} source - Source partition
   * @param {string} target - Target partition
   * @param {Object} [options] - Search options
   * @param {number} [options.maxLag=10] - Maximum lag to test
   * @returns {Object} Optimal lag result
   */
  findOptimalLag(source, target, options = {}) {
    const maxLag = options.maxLag || 10;
    const results = [];

    for (let lag = 1; lag <= maxLag; lag++) {
      const te = this.computeTransferEntropy(source, target, { lag });
      results.push({
        lag,
        te: te.transferEntropy,
        normalizedTE: te.normalizedTE,
        significance: te.significance,
        isSignificant: te.isSignificant
      });
    }

    // Find lag with maximum significant TE
    const significantResults = results.filter(r => r.isSignificant);
    const optimalResult = significantResults.length > 0
      ? significantResults.reduce((max, r) => r.te > max.te ? r : max)
      : results.reduce((max, r) => r.te > max.te ? r : max);

    return {
      source,
      target,
      optimalLag: optimalResult.lag,
      optimalTE: optimalResult.te,
      allResults: results,
      recommendation: significantResults.length > 0
        ? `Use lag ${optimalResult.lag} for maximum causal signal`
        : 'No significant causal relationship detected at any lag',
      computedAt: new Date().toISOString()
    };
  }

  /**
   * Get TE trend over time for a pair
   *
   * @param {string} source - Source partition
   * @param {string} target - Target partition
   * @param {number} [windowSize=10] - Window size for trend
   * @returns {Object} Trend analysis
   */
  getTETrend(source, target, windowSize = 10) {
    const pairKey = `${source}_${target}`;
    const pairRecords = this.teHistory.filter(
      r => r.source === source && r.target === target
    );

    if (pairRecords.length < 2) {
      return {
        trend: 'insufficient_data',
        source,
        target,
        dataPoints: pairRecords.length
      };
    }

    const recent = pairRecords.slice(-windowSize);
    const teValues = recent.map(r => r.transferEntropy);

    // Statistics
    const mean = teValues.reduce((a, b) => a + b, 0) / teValues.length;
    const variance = teValues.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / teValues.length;
    const stdDev = Math.sqrt(variance);

    // Trend via linear regression
    const n = teValues.length;
    const xMean = (n - 1) / 2;
    let numerator = 0;
    let denominator = 0;

    for (let i = 0; i < n; i++) {
      numerator += (i - xMean) * (teValues[i] - mean);
      denominator += (i - xMean) * (i - xMean);
    }

    const slope = denominator !== 0 ? numerator / denominator : 0;

    let trend;
    if (Math.abs(slope) < 0.001 && stdDev < mean * 0.1) {
      trend = 'stable';
    } else if (slope > 0.01) {
      trend = 'increasing';
    } else if (slope < -0.01) {
      trend = 'decreasing';
    } else if (stdDev > mean * 0.3) {
      trend = 'volatile';
    } else {
      trend = 'stable';
    }

    return {
      trend,
      source,
      target,
      windowSize: recent.length,
      mean,
      stdDev,
      slope,
      min: Math.min(...teValues),
      max: Math.max(...teValues),
      latest: teValues[teValues.length - 1],
      interpretation: this._interpretTETrend(trend, mean, slope)
    };
  }

  /**
   * Align time series for two partitions
   * @private
   */
  _alignTimeSeries(source, target, lag) {
    const sourceHist = this.stateHistory.get(source) || [];
    const targetHist = this.stateHistory.get(target) || [];

    if (sourceHist.length === 0 || targetHist.length === 0) {
      return [];
    }

    // Build timestamp-indexed maps
    const sourceByTime = new Map();
    for (const s of sourceHist) {
      const timeKey = Math.floor(new Date(s.timestamp).getTime() / 60000);
      sourceByTime.set(timeKey, s);
    }

    const targetByTime = new Map();
    for (const t of targetHist) {
      const timeKey = Math.floor(new Date(t.timestamp).getTime() / 60000);
      targetByTime.set(timeKey, t);
    }

    // Find aligned time points
    const aligned = [];
    const sortedTimes = Array.from(targetByTime.keys()).sort((a, b) => a - b);

    for (const t of sortedTimes) {
      const laggedTime = t - lag;

      if (sourceByTime.has(laggedTime) && targetByTime.has(t)) {
        // Also need target history
        const hasTargetHistory = Array.from({ length: this.config.historyLength })
          .every((_, i) => targetByTime.has(t - i - 1));

        if (hasTargetHistory) {
          aligned.push({
            time: t,
            sourceState: sourceByTime.get(laggedTime).numericState,
            targetState: targetByTime.get(t).numericState,
            targetHistory: Array.from({ length: this.config.historyLength })
              .map((_, i) => targetByTime.get(t - i - 1).numericState)
          });
        }
      }
    }

    return aligned;
  }

  /**
   * Build state vectors for TE computation
   * @private
   */
  _buildStateVectors(aligned, lag) {
    const sourceHistory = [];
    const targetHistory = [];
    const targetCurrent = [];

    for (const point of aligned) {
      sourceHistory.push(point.sourceState);
      targetHistory.push(point.targetHistory);
      targetCurrent.push(point.targetState);
    }

    return { sourceHistory, targetHistory, targetCurrent };
  }

  /**
   * Compute conditional entropy H(X|Y)
   * @private
   */
  _computeConditionalEntropy(x, y) {
    const n = x.length;
    if (n === 0) return 0;

    // Discretize
    const binSize = 1 / this.config.binCount;

    // Joint counts P(X, Y)
    const jointCounts = new Map();
    // Marginal counts P(Y)
    const yCounts = new Map();

    for (let i = 0; i < n; i++) {
      const xBin = Math.min(Math.floor(x[i] * this.config.binCount), this.config.binCount - 1);
      const yBin = this._vectorToBin(y[i]);

      const jointKey = `${xBin}_${yBin}`;
      jointCounts.set(jointKey, (jointCounts.get(jointKey) || 0) + 1);
      yCounts.set(yBin, (yCounts.get(yBin) || 0) + 1);
    }

    // H(X|Y) = H(X,Y) - H(Y)
    let jointEntropy = 0;
    for (const count of jointCounts.values()) {
      const p = count / n;
      if (p > 0) jointEntropy -= p * Math.log2(p);
    }

    let yEntropy = 0;
    for (const count of yCounts.values()) {
      const p = count / n;
      if (p > 0) yEntropy -= p * Math.log2(p);
    }

    return jointEntropy - yEntropy;
  }

  /**
   * Compute conditional entropy H(X|Y,Z)
   * @private
   */
  _computeConditionalEntropyJoint(x, y, z) {
    const n = x.length;
    if (n === 0) return 0;

    // Joint counts P(X, Y, Z)
    const jointCounts = new Map();
    // Marginal counts P(Y, Z)
    const yzCounts = new Map();

    for (let i = 0; i < n; i++) {
      const xBin = Math.min(Math.floor(x[i] * this.config.binCount), this.config.binCount - 1);
      const yBin = this._vectorToBin(y[i]);
      const zBin = Math.min(Math.floor(z[i] * this.config.binCount), this.config.binCount - 1);

      const jointKey = `${xBin}_${yBin}_${zBin}`;
      jointCounts.set(jointKey, (jointCounts.get(jointKey) || 0) + 1);

      const yzKey = `${yBin}_${zBin}`;
      yzCounts.set(yzKey, (yzCounts.get(yzKey) || 0) + 1);
    }

    // H(X|Y,Z) = H(X,Y,Z) - H(Y,Z)
    let jointEntropy = 0;
    for (const count of jointCounts.values()) {
      const p = count / n;
      if (p > 0) jointEntropy -= p * Math.log2(p);
    }

    let yzEntropy = 0;
    for (const count of yzCounts.values()) {
      const p = count / n;
      if (p > 0) yzEntropy -= p * Math.log2(p);
    }

    return jointEntropy - yzEntropy;
  }

  /**
   * Convert vector to bin key
   * @private
   */
  _vectorToBin(vec) {
    if (!Array.isArray(vec)) {
      return Math.min(Math.floor(vec * this.config.binCount), this.config.binCount - 1);
    }
    return vec.map(v =>
      Math.min(Math.floor(v * this.config.binCount), this.config.binCount - 1)
    ).join('_');
  }

  /**
   * Compute significance via shuffle test
   * @private
   */
  _computeSignificance(sourceHistory, targetHistory, targetCurrent, observedTE) {
    const numShuffles = 100;
    let countHigher = 0;

    for (let s = 0; s < numShuffles; s++) {
      // Shuffle source history
      const shuffled = [...sourceHistory].sort(() => Math.random() - 0.5);

      // Compute TE with shuffled source
      const hYgivenY = this._computeConditionalEntropy(targetCurrent, targetHistory);
      const hYgivenYX = this._computeConditionalEntropyJoint(
        targetCurrent, targetHistory, shuffled
      );
      const shuffledTE = Math.max(0, hYgivenY - hYgivenYX);

      if (shuffledTE >= observedTE) {
        countHigher++;
      }
    }

    return countHigher / numShuffles;
  }

  /**
   * Convert state to numeric value
   * @private
   */
  _toNumericState(state) {
    if (typeof state === 'number') return state;
    if (typeof state === 'string') {
      // Hash string to number
      let hash = 0;
      for (let i = 0; i < state.length; i++) {
        hash = ((hash << 5) - hash) + state.charCodeAt(i);
        hash |= 0;
      }
      return Math.abs(hash) / 2147483647; // Normalize to 0-1
    }
    return 0;
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
   * Invalidate TE cache
   * @private
   */
  _invalidateCache() {
    this._teCache.clear();
  }

  /**
   * Record TE result in history
   * @private
   */
  _recordTEHistory(result) {
    this.teHistory.push({
      source: result.source,
      target: result.target,
      transferEntropy: result.transferEntropy,
      normalizedTE: result.normalizedTE,
      significance: result.significance,
      timestamp: result.computedAt
    });

    // Trim history
    while (this.teHistory.length > 1000) {
      this.teHistory.shift();
    }
  }

  /**
   * Classify node role in causal graph
   * @private
   */
  _classifyNodeRole(influence) {
    if (influence.outDegree > influence.inDegree * 2) return 'driver';
    if (influence.inDegree > influence.outDegree * 2) return 'receiver';
    if (influence.outDegree > 0 && influence.inDegree > 0) return 'mediator';
    if (influence.outDegree === 0 && influence.inDegree === 0) return 'isolated';
    return 'balanced';
  }

  /**
   * Interpret bidirectional TE
   * @private
   */
  _interpretBidirectionalTE(te12, te21, netFlow) {
    if (!te12.isSignificant && !te21.isSignificant) {
      return 'No significant causal relationship in either direction';
    }
    if (te12.isSignificant && !te21.isSignificant) {
      return `Unidirectional causality: ${te12.source} drives ${te12.target}`;
    }
    if (!te12.isSignificant && te21.isSignificant) {
      return `Unidirectional causality: ${te21.source} drives ${te21.target}`;
    }
    if (Math.abs(netFlow) < 0.1 * Math.max(te12.transferEntropy, te21.transferEntropy)) {
      return 'Symmetric bidirectional causality - mutual influence';
    }
    return `Asymmetric bidirectional causality - dominant flow: ${netFlow > 0 ? te12.source : te21.source}`;
  }

  /**
   * Interpret TE trend
   * @private
   */
  _interpretTETrend(trend, mean, slope) {
    const interpretations = {
      stable: mean > 0.1
        ? 'Consistent causal influence'
        : 'Consistently weak/no causal influence',
      increasing: 'Causal influence is strengthening - changes in source increasingly affect target',
      decreasing: 'Causal influence is weakening - partitions becoming more independent',
      volatile: 'Unstable causal relationship - investigate intermittent dependencies'
    };
    return interpretations[trend] || 'Unknown trend';
  }

  /**
   * Clear all data
   */
  clear() {
    this.stateHistory.clear();
    this.teHistory = [];
    this._teCache.clear();
  }

  /**
   * Export data for persistence
   * @returns {Object} Exportable data
   */
  exportData() {
    const stateHistoryObj = {};
    for (const [k, v] of this.stateHistory) {
      stateHistoryObj[k] = v;
    }

    return {
      stateHistory: stateHistoryObj,
      teHistory: [...this.teHistory],
      config: { ...this.config },
      exportedAt: new Date().toISOString()
    };
  }

  /**
   * Import historical data
   * @param {Object} data - Previously exported data
   */
  importData(data) {
    if (data.stateHistory) {
      for (const [partition, events] of Object.entries(data.stateHistory)) {
        for (const event of events) {
          this.recordState({
            ...event,
            partition
          });
        }
      }
    }
    if (data.teHistory) {
      this.teHistory = [...data.teHistory];
    }
  }
}

/**
 * Create a transfer entropy computer with default configuration
 * @param {Object} [config] - Configuration options
 * @returns {TransferEntropyComputer} New TE computer
 */
export function createTransferEntropyComputer(config = {}) {
  return new TransferEntropyComputer(config);
}

/**
 * Quick TE computation from state events
 * @param {Array<Object>} events - State events
 * @param {string} source - Source partition
 * @param {string} target - Target partition
 * @param {Object} [options] - Computation options
 * @returns {Object} Transfer entropy result
 */
export function computeTransferEntropy(events, source, target, options = {}) {
  const computer = new TransferEntropyComputer(options);
  computer.recordStates(events);
  return computer.computeTransferEntropy(source, target, options);
}

/**
 * Check if TE indicates healthy causality
 * @param {Object} teResult - Transfer entropy result
 * @param {Object} [thresholds] - Custom thresholds
 * @returns {Object} Health check result
 */
export function checkTEHealth(teResult, thresholds = {}) {
  const defaults = {
    maxTE: 0.9,
    minSignificance: 0.05,
    minSampleSize: 50
  };

  const t = { ...defaults, ...thresholds };
  const issues = [];

  if (teResult.normalizedTE > t.maxTE) {
    issues.push({
      type: 'high_causal_influence',
      message: `Normalized TE ${teResult.normalizedTE.toFixed(3)} exceeds threshold`,
      severity: 'warning'
    });
  }

  if (teResult.sampleSize < t.minSampleSize) {
    issues.push({
      type: 'small_sample',
      message: `Sample size ${teResult.sampleSize} may be insufficient for reliable TE`,
      severity: 'info'
    });
  }

  if (teResult.significance > t.minSignificance && teResult.transferEntropy > 0.1) {
    issues.push({
      type: 'insignificant_te',
      message: 'TE is not statistically significant despite non-zero value',
      severity: 'info'
    });
  }

  return {
    healthy: issues.filter(i => i.severity === 'critical').length === 0,
    issues,
    transferEntropy: teResult.transferEntropy,
    isSignificant: teResult.isSignificant,
    checkedAt: new Date().toISOString()
  };
}

export default TransferEntropyComputer;
