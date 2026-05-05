/**
 * Drift Detector - Detect and measure semantic drift between systems
 * @module agent-7/drift-detector
 */

import { getMismatches, getLedgerStats } from './mismatch-ledger.mjs';

/**
 * Diff type severity weights (0-1 scale)
 * Higher values indicate more severe drift
 * @type {Object<string, number>}
 */
const SEVERITY_WEIGHTS = {
  TYPE_MISMATCH: 1.0, // Different types = critical
  KEY_MISMATCH: 0.9, // Missing/extra fields = very severe
  NULL_MISMATCH: 0.7, // Null vs value = severe
  ARRAY_LENGTH_MISMATCH: 0.8, // Different array sizes = severe
  ARRAY_ELEMENT_MISMATCH: 0.6, // Element differences = moderate
  VALUE_MISMATCH: 0.5, // Value differences = moderate
  UNKNOWN: 0.5, // Unknown diff type = moderate
};

/**
 * Drift thresholds
 * @type {Object}
 */
const DRIFT_THRESHOLDS = {
  MINIMAL: 10, // 0-10: Minimal drift
  LOW: 25, // 10-25: Low drift
  MODERATE: 50, // 25-50: Moderate drift
  HIGH: 75, // 50-75: High drift
  CRITICAL: 100, // 75-100: Critical drift
};

/**
 * Detect drift for an operation
 * @param {string} operation - Operation identifier
 * @param {Object} options - Detection options
 * @param {number} [options.lookbackMs] - Time window to analyze (default: 1 hour)
 * @param {number} [options.minSamples] - Minimum samples required (default: 10)
 * @returns {Object} Drift detection result
 */
export function detectDrift(operation, options = {}) {
  const lookbackMs = options.lookbackMs || 60 * 60 * 1000; // 1 hour default
  const minSamples = options.minSamples || 10;

  const since = Date.now() - lookbackMs;

  // Get mismatches for this operation in the time window
  const mismatches = getMismatches({ operation, since });

  if (mismatches.length < minSamples) {
    return {
      operation,
      hasDrift: false,
      score: 0,
      severity: 'NONE',
      reason: `Insufficient samples (${mismatches.length}/${minSamples})`,
      samples: mismatches.length,
      timestamp: Date.now(),
    };
  }

  // Calculate drift score
  const score = calculateDriftScore(mismatches);
  const severity = getDriftSeverity(score);

  // Analyze drift patterns
  const patterns = analyzeDriftPatterns(mismatches);

  return {
    operation,
    hasDrift: score > DRIFT_THRESHOLDS.MINIMAL,
    score,
    severity,
    samples: mismatches.length,
    patterns,
    recommendation: getRecommendation(score, patterns),
    timestamp: Date.now(),
  };
}

/**
 * Calculate drift score from mismatches (0-100 scale)
 * @param {Array<Object>} mismatches - Array of mismatch entries
 * @returns {number} Drift score (0-100)
 */
export function calculateDriftScore(mismatches) {
  if (!Array.isArray(mismatches) || mismatches.length === 0) {
    return 0;
  }

  let totalWeight = 0;
  let maxPossibleWeight = 0;

  for (const mismatch of mismatches) {
    const diffType = mismatch.diff?.type || 'UNKNOWN';
    const severity = SEVERITY_WEIGHTS[diffType] || SEVERITY_WEIGHTS.UNKNOWN;

    totalWeight += severity;
    maxPossibleWeight += 1.0; // Max severity
  }

  // Normalize to 0-100 scale
  const normalizedScore = (totalWeight / maxPossibleWeight) * 100;

  return Math.round(normalizedScore * 10) / 10; // Round to 1 decimal
}

/**
 * Get drift severity level
 * @param {number} score - Drift score (0-100)
 * @returns {string} Severity level
 */
function getDriftSeverity(score) {
  if (score >= DRIFT_THRESHOLDS.CRITICAL) return 'CRITICAL';
  if (score >= DRIFT_THRESHOLDS.HIGH) return 'HIGH';
  if (score >= DRIFT_THRESHOLDS.MODERATE) return 'MODERATE';
  if (score >= DRIFT_THRESHOLDS.LOW) return 'LOW';
  if (score >= DRIFT_THRESHOLDS.MINIMAL) return 'MINIMAL';
  return 'NONE';
}

/**
 * Analyze drift patterns in mismatches
 * @param {Array<Object>} mismatches - Array of mismatch entries
 * @returns {Object} Pattern analysis
 */
function analyzeDriftPatterns(mismatches) {
  const patterns = {
    byDiffType: {},
    affectedFields: {},
    timeDistribution: {
      first: null,
      last: null,
      trend: 'STABLE', // STABLE, INCREASING, DECREASING
    },
  };

  if (mismatches.length === 0) {
    return patterns;
  }

  // Count by diff type
  for (const mismatch of mismatches) {
    const diffType = mismatch.diff?.type || 'UNKNOWN';
    patterns.byDiffType[diffType] = (patterns.byDiffType[diffType] || 0) + 1;

    // Extract affected fields
    if (mismatch.diff?.fields) {
      for (const field of mismatch.diff.fields) {
        patterns.affectedFields[field] = (patterns.affectedFields[field] || 0) + 1;
      }
    }
  }

  // Time distribution
  const timestamps = mismatches.map((m) => m.timestamp).sort((a, b) => a - b);
  patterns.timeDistribution.first = timestamps[0];
  patterns.timeDistribution.last = timestamps[timestamps.length - 1];

  // Detect trend (simple linear regression)
  if (timestamps.length >= 5) {
    patterns.timeDistribution.trend = detectTrend(timestamps);
  }

  return patterns;
}

/**
 * Detect trend in mismatch timestamps
 * @param {Array<number>} timestamps - Sorted timestamps
 * @returns {string} Trend direction
 */
function detectTrend(timestamps) {
  // Simple approach: compare first half vs second half
  const midpoint = Math.floor(timestamps.length / 2);
  const firstHalf = timestamps.slice(0, midpoint);
  const secondHalf = timestamps.slice(midpoint);

  // Calculate time spans
  const firstSpan = firstHalf[firstHalf.length - 1] - firstHalf[0];
  const secondSpan = secondHalf[secondHalf.length - 1] - secondHalf[0];

  // Calculate rates (mismatches per ms)
  const firstRate = firstHalf.length / (firstSpan || 1);
  const secondRate = secondHalf.length / (secondSpan || 1);

  const rateChange = (secondRate - firstRate) / (firstRate || 1);

  if (rateChange > 0.2) return 'INCREASING';
  if (rateChange < -0.2) return 'DECREASING';
  return 'STABLE';
}

/**
 * Get recommendation based on drift analysis
 * @param {number} score - Drift score
 * @param {Object} patterns - Drift patterns
 * @returns {string} Recommendation
 */
function getRecommendation(score, patterns) {
  if (score < DRIFT_THRESHOLDS.MINIMAL) {
    return 'No action required. Drift is minimal.';
  }

  if (score < DRIFT_THRESHOLDS.LOW) {
    return 'Monitor drift. Consider investigating if trend increases.';
  }

  if (score < DRIFT_THRESHOLDS.MODERATE) {
    return 'Investigate root cause. Review affected fields and diff types.';
  }

  if (score < DRIFT_THRESHOLDS.HIGH) {
    return 'Urgent investigation required. Consider pausing migration.';
  }

  return 'CRITICAL: Halt migration. Systems are significantly diverged.';
}

/**
 * Check if drift is acceptable given a threshold
 * @param {number} score - Drift score (0-100)
 * @param {number} threshold - Acceptable threshold (0-100)
 * @returns {boolean} True if drift is acceptable
 */
export function isAcceptableDrift(score, threshold) {
  if (typeof score !== 'number' || score < 0 || score > 100) {
    throw new Error('Score must be a number between 0 and 100');
  }

  if (typeof threshold !== 'number' || threshold < 0 || threshold > 100) {
    throw new Error('Threshold must be a number between 0 and 100');
  }

  return score <= threshold;
}

/**
 * Get drift report for all operations
 * @param {Object} options - Report options
 * @param {number} [options.lookbackMs] - Time window to analyze
 * @param {number} [options.minSamples] - Minimum samples required
 * @returns {Object} Comprehensive drift report
 */
export function getDriftReport(options = {}) {
  const stats = getLedgerStats();
  const operations = Object.keys(stats.byOperation);

  const operationDrift = {};
  let totalScore = 0;
  let operationsWithDrift = 0;

  for (const operation of operations) {
    const drift = detectDrift(operation, options);
    operationDrift[operation] = drift;

    totalScore += drift.score;
    if (drift.hasDrift) {
      operationsWithDrift++;
    }
  }

  const avgScore = operations.length > 0 ? totalScore / operations.length : 0;

  return {
    summary: {
      totalOperations: operations.length,
      operationsWithDrift,
      averageScore: Math.round(avgScore * 10) / 10,
      overallSeverity: getDriftSeverity(avgScore),
    },
    operations: operationDrift,
    timestamp: Date.now(),
  };
}

/**
 * Get drift thresholds
 * @returns {Object} Drift thresholds
 */
export function getThresholds() {
  return { ...DRIFT_THRESHOLDS };
}

/**
 * Get severity weights
 * @returns {Object} Severity weights
 */
export function getSeverityWeights() {
  return { ...SEVERITY_WEIGHTS };
}

/**
 * Analyze specific diff type impact
 * @param {string} diffType - Diff type to analyze
 * @param {Object} options - Analysis options
 * @returns {Object} Diff type analysis
 */
export function analyzeDiffType(diffType, options = {}) {
  const lookbackMs = options.lookbackMs || 60 * 60 * 1000;
  const since = Date.now() - lookbackMs;

  const allMismatches = getMismatches({ since, diffType });

  const operationCounts = {};
  for (const mismatch of allMismatches) {
    operationCounts[mismatch.operation] = (operationCounts[mismatch.operation] || 0) + 1;
  }

  return {
    diffType,
    totalOccurrences: allMismatches.length,
    affectedOperations: Object.keys(operationCounts).length,
    severity: SEVERITY_WEIGHTS[diffType] || SEVERITY_WEIGHTS.UNKNOWN,
    byOperation: operationCounts,
    timestamp: Date.now(),
  };
}
