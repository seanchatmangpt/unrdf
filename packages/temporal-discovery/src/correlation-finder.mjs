/**
 * @file Temporal correlation analysis
 * @module @unrdf/temporal-discovery/correlation-finder
 * @description Pearson correlation and lagged correlation analysis
 */

import {
  CorrelationOptionsSchema,
  CorrelationSchema,
  TimeSeriesSchema,
} from './schemas.mjs';

/**
 * Calculate Pearson correlation coefficient
 * @param {number[]} x - First series
 * @param {number[]} y - Second series
 * @returns {number} Correlation coefficient (-1 to 1)
 */
function pearsonCorrelation(x, y) {
  const n = x.length;

  if (n === 0 || n !== y.length) {
    return 0;
  }

  const sumX = x.reduce((a, b) => a + b, 0);
  const sumY = y.reduce((a, b) => a + b, 0);
  const sumXY = x.reduce((sum, xi, i) => sum + xi * y[i], 0);
  const sumX2 = x.reduce((sum, xi) => sum + xi * xi, 0);
  const sumY2 = y.reduce((sum, yi) => sum + yi * yi, 0);

  const numerator = n * sumXY - sumX * sumY;
  const denominator = Math.sqrt(
    (n * sumX2 - sumX * sumX) * (n * sumY2 - sumY * sumY)
  );

  if (denominator === 0) {
    return 0;
  }

  return numerator / denominator;
}

/**
 * Calculate Spearman rank correlation
 * @param {number[]} x - First series
 * @param {number[]} y - Second series
 * @returns {number} Rank correlation coefficient
 */
function spearmanCorrelation(x, y) {
  if (x.length !== y.length || x.length === 0) {
    return 0;
  }

  const rankX = getRanks(x);
  const rankY = getRanks(y);

  return pearsonCorrelation(rankX, rankY);
}

/**
 * Get ranks of array values
 * @param {number[]} values - Array of values
 * @returns {number[]} Ranks
 */
function getRanks(values) {
  const indexed = values.map((val, idx) => ({ val, idx }));
  indexed.sort((a, b) => a.val - b.val);

  const ranks = new Array(values.length);
  for (let i = 0; i < indexed.length; i++) {
    ranks[indexed[i].idx] = i + 1;
  }

  return ranks;
}

/**
 * Classify correlation strength
 * @param {number} coefficient - Correlation coefficient
 * @returns {'very_weak'|'weak'|'moderate'|'strong'|'very_strong'} Strength
 */
function classifyStrength(coefficient) {
  const abs = Math.abs(coefficient);

  if (abs >= 0.8) return 'very_strong';
  if (abs >= 0.6) return 'strong';
  if (abs >= 0.4) return 'moderate';
  if (abs >= 0.2) return 'weak';
  return 'very_weak';
}

/**
 * Determine correlation direction
 * @param {number} coefficient - Correlation coefficient
 * @returns {'positive'|'negative'|'none'} Direction
 */
function classifyDirection(coefficient) {
  if (Math.abs(coefficient) < 0.1) return 'none';
  return coefficient > 0 ? 'positive' : 'negative';
}

/**
 * Align two time series by timestamp
 * @param {Object} series1 - First time series
 * @param {Object} series2 - Second time series
 * @returns {{values1: number[], values2: number[], timestamps: number[]}} Aligned data
 */
function alignTimeSeries(series1, series2) {
  const map1 = new Map(series1.data.map((d) => [d.timestamp, d.value]));
  const map2 = new Map(series2.data.map((d) => [d.timestamp, d.value]));

  const commonTimestamps = Array.from(map1.keys()).filter((t) => map2.has(t));
  commonTimestamps.sort((a, b) => a - b);

  return {
    values1: commonTimestamps.map((t) => map1.get(t)),
    values2: commonTimestamps.map((t) => map2.get(t)),
    timestamps: commonTimestamps,
  };
}

/**
 * Find correlation between two time series
 * @param {Object} series1 - First time series
 * @param {Object} series2 - Second time series
 * @param {Object} options - Correlation options
 * @param {'pearson'|'spearman'|'kendall'} [options.method='pearson'] - Correlation method
 * @param {number} [options.minOverlap=10] - Minimum overlapping points
 * @param {number} [options.lagMax=0] - Maximum lag to test
 * @returns {Object|null} Correlation result or null if insufficient overlap
 * @throws {Error} If time series are invalid
 * @example
 * const series1 = { name: 'temperature', data: [...] };
 * const series2 = { name: 'sales', data: [...] };
 * const correlation = findCorrelation(series1, series2);
 */
export function findCorrelation(series1, series2, options = {}) {
  const validatedSeries1 = TimeSeriesSchema.parse(series1);
  const validatedSeries2 = TimeSeriesSchema.parse(series2);
  const validatedOptions = CorrelationOptionsSchema.parse(options);

  const { method, minOverlap, lagMax } = validatedOptions;

  const { values1, values2 } = alignTimeSeries(
    validatedSeries1,
    validatedSeries2
  );

  if (values1.length < minOverlap) {
    return null;
  }

  let bestCoefficient = 0;

  for (let lag = -lagMax; lag <= lagMax; lag++) {
    let x, y;

    if (lag >= 0) {
      x = values1.slice(0, values1.length - lag || values1.length);
      y = values2.slice(lag);
    } else {
      x = values1.slice(-lag);
      y = values2.slice(0, values2.length + lag || values2.length);
    }

    const minLength = Math.min(x.length, y.length);
    x = x.slice(0, minLength);
    y = y.slice(0, minLength);

    if (x.length < minOverlap) {
      continue;
    }

    let coefficient;
    if (method === 'pearson') {
      coefficient = pearsonCorrelation(x, y);
    } else if (method === 'spearman') {
      coefficient = spearmanCorrelation(x, y);
    } else {
      coefficient = pearsonCorrelation(x, y);
    }

    if (Math.abs(coefficient) > Math.abs(bestCoefficient)) {
      bestCoefficient = coefficient;
    }
  }

  return CorrelationSchema.parse({
    series1: validatedSeries1.name,
    series2: validatedSeries2.name,
    coefficient: bestCoefficient,
    strength: classifyStrength(bestCoefficient),
    direction: classifyDirection(bestCoefficient),
  });
}

/**
 * Find correlations for multiple time series (correlation matrix)
 * @param {Object[]} timeSeriesList - Array of time series
 * @param {Object} options - Correlation options
 * @returns {Object[]} Array of correlation results
 * @example
 * const correlations = findMultipleCorrelations([series1, series2, series3]);
 */
export function findMultipleCorrelations(timeSeriesList, options = {}) {
  const correlations = [];

  for (let i = 0; i < timeSeriesList.length; i++) {
    for (let j = i + 1; j < timeSeriesList.length; j++) {
      const correlation = findCorrelation(
        timeSeriesList[i],
        timeSeriesList[j],
        options
      );

      if (correlation !== null) {
        correlations.push(correlation);
      }
    }
  }

  correlations.sort(
    (a, b) => Math.abs(b.coefficient) - Math.abs(a.coefficient)
  );

  return correlations;
}

/**
 * Filter correlations by strength
 * @param {Object[]} correlations - Array of correlations
 * @param {string[]} strengths - Strength levels to include
 * @returns {Object[]} Filtered correlations
 * @example
 * const strong = filterByStrength(correlations, ['strong', 'very_strong']);
 */
export function filterByStrength(correlations, strengths) {
  return correlations.filter((corr) => strengths.includes(corr.strength));
}
