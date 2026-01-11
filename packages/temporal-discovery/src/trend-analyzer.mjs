/**
 * @file Trend analysis with moving averages
 * @module @unrdf/temporal-discovery/trend-analyzer
 * @description Trend detection and analysis for temporal data
 */

import {
  TrendAnalysisOptionsSchema,
  TrendSchema,
  TimeSeriesSchema,
} from './schemas.mjs';


/**
 * Calculate exponential moving average
 * @param {number[]} values - Array of values
 * @param {number} smoothingFactor - Smoothing factor (0-1)
 * @returns {number[]} Exponential moving averages
 */
function exponentialMovingAverage(values, smoothingFactor) {
  if (values.length === 0) return [];

  const result = [values[0]];

  for (let i = 1; i < values.length; i++) {
    const ema =
      smoothingFactor * values[i] + (1 - smoothingFactor) * result[i - 1];
    result.push(ema);
  }

  return result;
}

/**
 * Calculate linear regression slope
 * @param {number[]} x - X values (typically indices or timestamps)
 * @param {number[]} y - Y values
 * @returns {{slope: number, intercept: number, r2: number}} Regression statistics
 */
function linearRegression(x, y) {
  const n = x.length;

  if (n === 0) {
    return { slope: 0, intercept: 0, r2: 0 };
  }

  const sumX = x.reduce((a, b) => a + b, 0);
  const sumY = y.reduce((a, b) => a + b, 0);
  const sumXY = x.reduce((sum, xi, i) => sum + xi * y[i], 0);
  const sumX2 = x.reduce((sum, xi) => sum + xi * xi, 0);

  const slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX);
  const intercept = (sumY - slope * sumX) / n;

  const yMean = sumY / n;
  const ssTotal = y.reduce((sum, yi) => sum + Math.pow(yi - yMean, 2), 0);
  const ssResidual = y.reduce(
    (sum, yi, i) => sum + Math.pow(yi - (slope * x[i] + intercept), 2),
    0
  );
  const r2 = ssTotal > 0 ? 1 - ssResidual / ssTotal : 0;

  return { slope, intercept, r2 };
}

/**
 * Calculate volatility (coefficient of variation)
 * @param {number[]} values - Array of values
 * @returns {number} Volatility measure
 */
function calculateVolatility(values) {
  if (values.length === 0) return 0;

  const mean = values.reduce((a, b) => a + b, 0) / values.length;
  if (mean === 0) return 0;

  const variance =
    values.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0) /
    values.length;
  const stdDev = Math.sqrt(variance);

  return stdDev / Math.abs(mean);
}

/**
 * Classify trend direction
 * @param {number} slope - Regression slope
 * @param {number} volatility - Volatility measure
 * @param {number} volatilityThreshold - Threshold for volatile classification
 * @returns {'increasing'|'decreasing'|'stable'|'volatile'} Trend direction
 */
function classifyTrend(slope, volatility, volatilityThreshold) {
  if (volatility > volatilityThreshold) {
    return 'volatile';
  }

  const slopeThreshold = 0.01;
  if (Math.abs(slope) < slopeThreshold) {
    return 'stable';
  }

  return slope > 0 ? 'increasing' : 'decreasing';
}

/**
 * Analyze trends in time series data
 * @param {Object} timeSeries - Time series data
 * @param {Object} options - Analysis options
 * @param {number} [options.windowSize=10] - Window size for moving average
 * @param {number} [options.smoothingFactor=0.3] - Smoothing factor for EMA
 * @param {number} [options.volatilityThreshold=0.5] - Threshold for volatile trends
 * @returns {Object[]} Detected trends
 * @throws {Error} If time series is invalid
 * @example
 * const series = {
 *   name: 'sales',
 *   data: [
 *     { timestamp: 1000, value: 100 },
 *     { timestamp: 2000, value: 110 },
 *     { timestamp: 3000, value: 120 },
 *   ]
 * };
 * const trends = analyzeTrends(series, { windowSize: 5 });
 */
export function analyzeTrends(timeSeries, options = {}) {
  const validatedSeries = TimeSeriesSchema.parse(timeSeries);
  const validatedOptions = TrendAnalysisOptionsSchema.parse(options);

  const { windowSize, smoothingFactor, volatilityThreshold } = validatedOptions;
  const { data } = validatedSeries;

  if (data.length < windowSize) {
    return [];
  }

  const trends = [];

  const values = data.map((d) => d.value);
  const timestamps = data.map((d) => d.timestamp);

  const smoothedValues = exponentialMovingAverage(values, smoothingFactor);

  for (let i = windowSize; i <= data.length; i += Math.floor(windowSize / 2)) {
    const start = Math.max(0, i - windowSize);
    const end = i;

    const windowValues = smoothedValues.slice(start, end);
    const windowTimestamps = timestamps.slice(start, end);
    const windowIndices = Array.from({ length: end - start }, (_, idx) => idx);

    const { slope, r2 } = linearRegression(windowIndices, windowValues);
    const volatility = calculateVolatility(windowValues);

    const direction = classifyTrend(slope, volatility, volatilityThreshold);

    trends.push(
      TrendSchema.parse({
        direction,
        slope,
        strength: r2,
        startTimestamp: windowTimestamps[0],
        endTimestamp: windowTimestamps[windowTimestamps.length - 1],
        points: windowValues.length,
      })
    );
  }

  return trends;
}

/**
 * Get smoothed time series using exponential moving average
 * @param {Object} timeSeries - Time series data
 * @param {number} smoothingFactor - Smoothing factor (0-1)
 * @returns {Object} Smoothed time series
 * @example
 * const smoothed = getSmoothedSeries(series, 0.3);
 */
export function getSmoothedSeries(timeSeries, smoothingFactor = 0.3) {
  const validatedSeries = TimeSeriesSchema.parse(timeSeries);
  const values = validatedSeries.data.map((d) => d.value);
  const smoothedValues = exponentialMovingAverage(values, smoothingFactor);

  return {
    name: `${validatedSeries.name}_smoothed`,
    data: validatedSeries.data.map((d, i) => ({
      timestamp: d.timestamp,
      value: smoothedValues[i],
      metadata: d.metadata,
    })),
    unit: validatedSeries.unit,
  };
}

/**
 * Find strongest trend in time series
 * @param {Object} timeSeries - Time series data
 * @param {Object} options - Analysis options
 * @returns {Object|null} Strongest trend or null
 * @example
 * const strongest = findStrongestTrend(series);
 */
export function findStrongestTrend(timeSeries, options = {}) {
  const trends = analyzeTrends(timeSeries, options);

  if (trends.length === 0) {
    return null;
  }

  return trends.reduce((strongest, current) =>
    current.strength > strongest.strength ? current : strongest
  );
}
