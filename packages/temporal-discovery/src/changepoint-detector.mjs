/**
 * @file Changepoint detection using PELT algorithm
 * @module @unrdf/temporal-discovery/changepoint-detector
 * @description PELT (Pruned Exact Linear Time) changepoint detection
 */

import {
  ChangepointDetectionOptionsSchema,
  ChangepointSchema,
  TimeSeriesSchema,
} from './schemas.mjs';

/**
 * Calculate cost for a segment (sum of squared errors)
 * @param {number[]} values - Array of values
 * @param {number} start - Start index (inclusive)
 * @param {number} end - End index (exclusive)
 * @returns {number} Cost value
 */
function segmentCost(values, start, end) {
  if (start >= end) return 0;

  const segment = values.slice(start, end);
  const mean = segment.reduce((a, b) => a + b, 0) / segment.length;

  return segment.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0);
}

/**
 * PELT algorithm for changepoint detection
 * @param {number[]} data - Time series data
 * @param {number} penalty - Penalty for adding changepoint (larger = fewer changepoints)
 * @param {number} minSegmentLength - Minimum segment length
 * @returns {number[]} Changepoint indices
 */
function pelt(data, penalty, minSegmentLength) {
  const n = data.length;
  const F = new Array(n + 1).fill(Infinity);
  const cp = new Array(n + 1).fill(0);
  const candidates = new Set([0]);

  F[0] = -penalty;

  for (let t = minSegmentLength; t <= n; t++) {
    const candidatesList = Array.from(candidates);
    let bestCost = Infinity;
    let bestTau = 0;

    for (const tau of candidatesList) {
      if (t - tau < minSegmentLength) continue;

      const cost = F[tau] + segmentCost(data, tau, t) + penalty;

      if (cost < bestCost) {
        bestCost = cost;
        bestTau = tau;
      }
    }

    F[t] = bestCost;
    cp[t] = bestTau;

    const pruneThreshold = F[t];
    const newCandidates = new Set([t]);

    for (const tau of candidatesList) {
      if (F[tau] + segmentCost(data, tau, t) <= pruneThreshold) {
        newCandidates.add(tau);
      }
    }

    candidates.clear();
    for (const c of newCandidates) {
      candidates.add(c);
    }
  }

  const changepoints = [];
  let current = n;

  while (cp[current] > 0) {
    changepoints.unshift(cp[current]);
    current = cp[current];
  }

  return changepoints;
}

/**
 * Binary segmentation algorithm for changepoint detection
 * @param {number[]} data - Time series data
 * @param {number} penalty - Penalty parameter
 * @param {number} minSegmentLength - Minimum segment length
 * @param {number} maxChangepoints - Maximum changepoints to find
 * @returns {number[]} Changepoint indices
 */
function binarySegmentation(data, penalty, minSegmentLength, maxChangepoints) {
  const changepoints = [];
  const segments = [[0, data.length]];

  while (segments.length > 0 && changepoints.length < maxChangepoints) {
    let bestSegmentIdx = -1;
    let bestSplit = -1;
    let bestImprovement = 0;

    for (let i = 0; i < segments.length; i++) {
      const [start, end] = segments[i];

      if (end - start < 2 * minSegmentLength) continue;

      const fullCost = segmentCost(data, start, end);

      for (
        let split = start + minSegmentLength;
        split < end - minSegmentLength;
        split++
      ) {
        const leftCost = segmentCost(data, start, split);
        const rightCost = segmentCost(data, split, end);
        const splitCost = leftCost + rightCost + penalty;

        const improvement = fullCost - splitCost;

        if (improvement > bestImprovement) {
          bestImprovement = improvement;
          bestSegmentIdx = i;
          bestSplit = split;
        }
      }
    }

    if (bestSegmentIdx === -1) break;

    const [start, end] = segments[bestSegmentIdx];
    segments.splice(bestSegmentIdx, 1);

    segments.push([start, bestSplit]);
    segments.push([bestSplit, end]);
    changepoints.push(bestSplit);
  }

  return changepoints.sort((a, b) => a - b);
}

/**
 * Calculate segment mean
 * @param {number[]} values - Array of values
 * @param {number} start - Start index
 * @param {number} end - End index
 * @returns {number} Mean value
 */
function segmentMean(values, start, end) {
  const segment = values.slice(start, end);
  return segment.reduce((a, b) => a + b, 0) / segment.length;
}

/**
 * Detect changepoints in time series
 * @param {Object} timeSeries - Time series data
 * @param {Object} options - Detection options
 * @param {number} [options.penalty=3.0] - Penalty for adding changepoints
 * @param {number} [options.minSegmentLength=5] - Minimum segment length
 * @param {number} [options.maxChangepoints=10] - Maximum changepoints
 * @param {'pelt'|'binary_segmentation'|'bottom_up'} [options.method='pelt'] - Detection method
 * @returns {Object[]} Detected changepoints
 * @throws {Error} If time series is invalid
 * @example
 * const series = {
 *   name: 'metric',
 *   data: [
 *     { timestamp: 1000, value: 10 },
 *     { timestamp: 2000, value: 10 },
 *     { timestamp: 3000, value: 20 },  // changepoint
 *     { timestamp: 4000, value: 20 },
 *   ]
 * };
 * const changepoints = detectChangepoints(series);
 */
export function detectChangepoints(timeSeries, options = {}) {
  const validatedSeries = TimeSeriesSchema.parse(timeSeries);
  const validatedOptions = ChangepointDetectionOptionsSchema.parse(options);

  const { penalty, minSegmentLength, maxChangepoints, method } =
    validatedOptions;
  const { data } = validatedSeries;

  const values = data.map((d) => d.value);

  let changepointIndices;

  if (method === 'pelt') {
    changepointIndices = pelt(values, penalty, minSegmentLength);
  } else if (method === 'binary_segmentation') {
    changepointIndices = binarySegmentation(
      values,
      penalty,
      minSegmentLength,
      maxChangepoints
    );
  } else {
    changepointIndices = pelt(values, penalty, minSegmentLength);
  }

  if (changepointIndices.length > maxChangepoints) {
    changepointIndices = changepointIndices.slice(0, maxChangepoints);
  }

  const changepoints = changepointIndices.map((index) => {
    const beforeMean =
      index > 0 ? segmentMean(values, 0, index) : values[index];
    const afterMean =
      index < values.length
        ? segmentMean(values, index, values.length)
        : values[index];

    const magnitude = Math.abs(afterMean - beforeMean);
    const cost = segmentCost(values, 0, index);

    return ChangepointSchema.parse({
      timestamp: data[index].timestamp,
      index,
      cost,
      beforeMean,
      afterMean,
      magnitude,
    });
  });

  changepoints.sort((a, b) => b.magnitude - a.magnitude);

  return changepoints;
}

/**
 * Find most significant changepoint
 * @param {Object} timeSeries - Time series data
 * @param {Object} options - Detection options
 * @returns {Object|null} Most significant changepoint or null
 * @example
 * const mostSignificant = findMostSignificantChangepoint(series);
 */
export function findMostSignificantChangepoint(timeSeries, options = {}) {
  const changepoints = detectChangepoints(timeSeries, options);

  if (changepoints.length === 0) {
    return null;
  }

  return changepoints[0];
}

/**
 * Segment time series at changepoints
 * @param {Object} timeSeries - Time series data
 * @param {Object[]} changepoints - Detected changepoints
 * @returns {Object[]} Array of segmented time series
 * @example
 * const segments = segmentTimeSeries(series, changepoints);
 */
export function segmentTimeSeries(timeSeries, changepoints) {
  const validatedSeries = TimeSeriesSchema.parse(timeSeries);

  if (changepoints.length === 0) {
    return [validatedSeries];
  }

  const segments = [];
  const sortedCps = [...changepoints].sort((a, b) => a.index - b.index);

  let start = 0;
  for (const cp of sortedCps) {
    if (cp.index > start) {
      segments.push({
        name: `${validatedSeries.name}_segment_${segments.length}`,
        data: validatedSeries.data.slice(start, cp.index),
        unit: validatedSeries.unit,
      });
    }
    start = cp.index;
  }

  if (start < validatedSeries.data.length) {
    segments.push({
      name: `${validatedSeries.name}_segment_${segments.length}`,
      data: validatedSeries.data.slice(start),
      unit: validatedSeries.unit,
    });
  }

  return segments;
}
