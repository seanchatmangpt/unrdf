/**
 * @file Changepoint detection example
 * @description Demonstrates PELT algorithm for changepoint detection
 */

import {
  detectChangepoints,
  findMostSignificantChangepoint,
} from '../src/changepoint-detector.mjs';

console.log('=== Changepoint Detection Example ===\n');

const segment1 = Array.from({ length: 30 }, (_, i) => ({
  timestamp: 1000 + i * 3600000,
  value: 50 + (Math.random() - 0.5) * 5,
}));

const segment2 = Array.from({ length: 25 }, (_, i) => ({
  timestamp: 1000 + (30 + i) * 3600000,
  value: 75 + (Math.random() - 0.5) * 5,
}));

const segment3 = Array.from({ length: 20 }, (_, i) => ({
  timestamp: 1000 + (55 + i) * 3600000,
  value: 40 + (Math.random() - 0.5) * 5,
}));

const segment4 = Array.from({ length: 15 }, (_, i) => ({
  timestamp: 1000 + (75 + i) * 3600000,
  value: 90 + (Math.random() - 0.5) * 5,
}));

const series = {
  name: 'process_metric',
  data: [...segment1, ...segment2, ...segment3, ...segment4],
  unit: 'units',
};

console.log(`Detecting changepoints in ${series.data.length} data points...\n`);

const changepoints = detectChangepoints(series, {
  penalty: 3.0,
  minSegmentLength: 10,
  maxChangepoints: 5,
  method: 'pelt',
});

console.log(`Found ${changepoints.length} changepoints:\n`);

changepoints.forEach((cp, i) => {
  const time = new Date(cp.timestamp).toISOString();

  console.log(`Changepoint ${i + 1}:`);
  console.log(`  Time: ${time}`);
  console.log(`  Index: ${cp.index}`);
  console.log(`  Before Mean: ${cp.beforeMean.toFixed(2)}`);
  console.log(`  After Mean: ${cp.afterMean.toFixed(2)}`);
  console.log(`  Magnitude: ${cp.magnitude.toFixed(2)}`);
  console.log(`  Cost: ${cp.cost.toFixed(2)}`);
  console.log();
});

console.log('=== Most Significant Changepoint ===\n');
const mostSignificant = findMostSignificantChangepoint(series);

if (mostSignificant) {
  const time = new Date(mostSignificant.timestamp).toISOString();
  console.log(`Time: ${time}`);
  console.log(`Magnitude: ${mostSignificant.magnitude.toFixed(2)}`);
  console.log(`Change: ${mostSignificant.beforeMean.toFixed(2)} → ${mostSignificant.afterMean.toFixed(2)}`);
  console.log(`Percent Change: ${(((mostSignificant.afterMean - mostSignificant.beforeMean) / mostSignificant.beforeMean) * 100).toFixed(1)}%`);
}
