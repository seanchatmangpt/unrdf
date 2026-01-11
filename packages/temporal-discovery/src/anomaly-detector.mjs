/**
 * @file Statistical anomaly detection
 * @module @unrdf/temporal-discovery/anomaly-detector
 * @description Z-Score and MAD-based anomaly detection for time series
 */

import {
  AnomalyDetectionOptionsSchema,
  AnomalySchema,
  TimeSeriesSchema,
} from './schemas.mjs';

/**
 * Calculate mean of array
 * @param {number[]} values - Array of numbers
 * @returns {number} Mean value
 */
function mean(values) {
  if (values.length === 0) return 0;
  return values.reduce((sum, val) => sum + val, 0) / values.length;
}

/**
 * Calculate standard deviation
 * @param {number[]} values - Array of numbers
 * @param {number} [meanValue] - Pre-calculated mean
 * @returns {number} Standard deviation
 */
function standardDeviation(values, meanValue) {
  if (values.length === 0) return 0;
  const avg = meanValue !== undefined ? meanValue : mean(values);
  const squareDiffs = values.map((value) => Math.pow(value - avg, 2));
  return Math.sqrt(mean(squareDiffs));
}

/**
 * Calculate median absolute deviation (MAD)
 * @param {number[]} values - Array of numbers
 * @returns {number} MAD value
 */
function medianAbsoluteDeviation(values) {
  if (values.length === 0) return 0;

  const sorted = [...values].sort((a, b) => a - b);
  const median = sorted[Math.floor(sorted.length / 2)];

  const absoluteDeviations = values.map((v) => Math.abs(v - median));
  const sortedDeviations = absoluteDeviations.sort((a, b) => a - b);

  return sortedDeviations[Math.floor(sortedDeviations.length / 2)];
}

/**
 * Calculate interquartile range (IQR)
 * @param {number[]} values - Array of numbers
 * @returns {{q1: number, q3: number, iqr: number}} IQR statistics
 */
function interquartileRange(values) {
  if (values.length === 0) {
    return { q1: 0, q3: 0, iqr: 0 };
  }

  const sorted = [...values].sort((a, b) => a - b);
  const q1Index = Math.floor(sorted.length * 0.25);
  const q3Index = Math.floor(sorted.length * 0.75);

  const q1 = sorted[q1Index];
  const q3 = sorted[q3Index];
  const iqr = q3 - q1;

  return { q1, q3, iqr };
}

/**
 * Classify anomaly severity based on z-score
 * @param {number} zScore - Z-score value
 * @returns {'low'|'medium'|'high'|'critical'} Severity level
 */
function classifySeverity(zScore) {
  const absZ = Math.abs(zScore);
  if (absZ >= 5) return 'critical';
  if (absZ >= 4) return 'high';
  if (absZ >= 3) return 'medium';
  return 'low';
}

/**
 * Detect anomalies using Z-Score method
 * @param {Object} timeSeries - Time series data
 * @param {Object} options - Detection options
 * @param {number} [options.threshold=3.0] - Z-score threshold
 * @param {number} [options.windowSize=100] - Sliding window size
 * @param {'zscore'|'mad'|'iqr'} [options.method='zscore'] - Detection method
 * @returns {Object[]} Detected anomalies
 * @throws {Error} If time series is invalid
 * @example
 * const series = {
 *   name: 'temperature',
 *   data: [
 *     { timestamp: 1000, value: 20.5 },
 *     { timestamp: 2000, value: 100.0 },  // anomaly
 *   ]
 * };
 * const anomalies = detectAnomalies(series, { threshold: 3.0 });
 */
export function detectAnomalies(timeSeries, options = {}) {
  const validatedSeries = TimeSeriesSchema.parse(timeSeries);
  const validatedOptions = AnomalyDetectionOptionsSchema.parse(options);

  const { threshold, windowSize, method } = validatedOptions;
  const { data } = validatedSeries;

  const anomalies = [];

  for (let i = 0; i < data.length; i++) {
    const effectiveWindowSize = Math.min(windowSize, data.length - 1);
    const windowStart = Math.max(0, i - effectiveWindowSize);
    const windowEnd = i;

    const window = data.slice(windowStart, windowEnd);
    const windowValues = window.map((d) => d.value);

    if (windowValues.length < 3) {
      continue;
    }

    const currentValue = data[i].value;
    let zScore = 0;
    let expectedValue = currentValue;

    if (method === 'zscore') {
      const windowMean = mean(windowValues);
      const windowStd = standardDeviation(windowValues, windowMean);

      if (windowStd > 0) {
        zScore = (currentValue - windowMean) / windowStd;
        expectedValue = windowMean;
      } else if (Math.abs(currentValue - windowMean) > 1e-10) {
        zScore = threshold + 5;
        expectedValue = windowMean;
      }
    } else if (method === 'mad') {
      const windowMedian = [...windowValues].sort((a, b) => a - b)[
        Math.floor(windowValues.length / 2)
      ];
      const mad = medianAbsoluteDeviation(windowValues);

      if (mad > 0) {
        zScore = (0.6745 * (currentValue - windowMedian)) / mad;
        expectedValue = windowMedian;
      } else {
        const windowMean = mean(windowValues);
        const windowStd = standardDeviation(windowValues, windowMean);
        if (windowStd > 0) {
          zScore = (currentValue - windowMean) / windowStd;
          expectedValue = windowMean;
        }
      }
    } else if (method === 'iqr') {
      const { q1, q3, iqr } = interquartileRange(windowValues);

      const lowerBound = q1 - 1.5 * iqr;
      const upperBound = q3 + 1.5 * iqr;

      if (currentValue < lowerBound || currentValue > upperBound) {
        zScore = currentValue < lowerBound ? -threshold - 1 : threshold + 1;
        expectedValue = (q1 + q3) / 2;
      }
    }

    if (Math.abs(zScore) >= threshold) {
      anomalies.push(
        AnomalySchema.parse({
          timestamp: data[i].timestamp,
          value: currentValue,
          zScore,
          severity: classifySeverity(zScore),
          expectedValue,
        })
      );
    }
  }

  return anomalies;
}

/**
 * Batch anomaly detection for multiple time series
 * @param {Object[]} timeSeriesList - Array of time series
 * @param {Object} options - Detection options
 * @returns {Map<string, Object[]>} Map of series name to anomalies
 * @example
 * const results = batchDetectAnomalies([series1, series2], { threshold: 2.5 });
 */
export function batchDetectAnomalies(timeSeriesList, options = {}) {
  const results = new Map();

  for (const series of timeSeriesList) {
    const anomalies = detectAnomalies(series, options);
    results.set(series.name, anomalies);
  }

  return results;
}

/**
 * Filter anomalies by severity
 * @param {Object[]} anomalies - Array of anomalies
 * @param {string[]} severities - Severity levels to include
 * @returns {Object[]} Filtered anomalies
 * @example
 * const critical = filterBySeverity(anomalies, ['critical', 'high']);
 */
export function filterBySeverity(anomalies, severities) {
  return anomalies.filter((anomaly) => severities.includes(anomaly.severity));
}
