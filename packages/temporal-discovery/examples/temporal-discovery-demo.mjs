/**
 * @file Complete temporal discovery demonstration
 * @description Demonstrates full discovery engine with all algorithms
 */

import { createDiscoveryEngine } from '../src/temporal-discovery-engine.mjs';

console.log('=== Temporal Discovery Engine Demo ===\n');

const generateSyntheticSeries = (name, length, pattern) => {
  const data = [];

  for (let i = 0; i < length; i++) {
    let value;

    switch (pattern) {
      case 'growth':
        value = 100 + i * 3 + (Math.random() - 0.5) * 10;
        break;
      case 'decline':
        value = 200 - i * 2 + (Math.random() - 0.5) * 10;
        break;
      case 'seasonal':
        value = 150 + Math.sin(i / 10) * 30 + (Math.random() - 0.5) * 5;
        break;
      case 'step':
        value = i < length / 2 ? 100 : 150;
        value += (Math.random() - 0.5) * 5;
        break;
      default:
        value = 100 + (Math.random() - 0.5) * 20;
    }

    if (i % 20 === 0 && Math.random() > 0.7) {
      value += (Math.random() - 0.5) * 100;
    }

    data.push({
      timestamp: 1000 + i * 3600000,
      value,
    });
  }

  return { name, data, unit: 'units' };
};

const series1 = generateSyntheticSeries('revenue', 100, 'growth');
const series2 = generateSyntheticSeries('costs', 100, 'decline');
const series3 = generateSyntheticSeries('traffic', 100, 'seasonal');
const series4 = generateSyntheticSeries('conversions', 100, 'step');

const engine = createDiscoveryEngine({
  enableAnomalyDetection: true,
  enableTrendAnalysis: true,
  enableCorrelationAnalysis: true,
  enableChangepointDetection: true,
  anomalyDetection: {
    threshold: 3.0,
    windowSize: 20,
  },
  trendAnalysis: {
    windowSize: 15,
    smoothingFactor: 0.3,
  },
  changepoint: {
    penalty: 3.0,
    minSegmentLength: 10,
  },
});

console.log('Running comprehensive discovery on 4 time series...\n');

const startTime = performance.now();
const results = engine.discover([series1, series2, series3, series4]);
const endTime = performance.now();

console.log(`Discovery completed in ${(endTime - startTime).toFixed(2)}ms\n`);

console.log('=== Summary ===\n');
const summary = engine.getSummary(results);

console.log(`Execution Time: ${summary.executionTimeMs.toFixed(2)}ms`);
console.log(`Algorithms Run: ${summary.algorithmsRun.join(', ')}`);
console.log();

console.log('=== Anomalies ===');
console.log(`Total: ${summary.totalAnomalies}`);
console.log(`Critical: ${summary.criticalAnomalies}`);
console.log();

console.log('=== Trends ===');
console.log(`Total: ${summary.totalTrends}`);
console.log(`Increasing: ${summary.increasingTrends}`);
console.log(`Decreasing: ${summary.decreasingTrends}`);
console.log();

console.log('=== Correlations ===');
console.log(`Total: ${summary.totalCorrelations}`);
console.log(`Strong: ${summary.strongCorrelations}`);
console.log();

console.log('=== Changepoints ===');
console.log(`Total: ${summary.totalChangepoints}`);
console.log();

if (results.anomalies && results.anomalies.length > 0) {
  console.log('=== Top 5 Critical Anomalies ===\n');

  const critical = results.anomalies
    .filter((a) => a.severity === 'critical' || a.severity === 'high')
    .slice(0, 5);

  critical.forEach((anomaly, i) => {
    console.log(`${i + 1}. Value: ${anomaly.value.toFixed(2)}, Z-Score: ${anomaly.zScore.toFixed(2)}, Severity: ${anomaly.severity}`);
  });
  console.log();
}

if (results.trends && results.trends.length > 0) {
  console.log('=== Top 3 Strongest Trends ===\n');

  const topTrends = [...results.trends]
    .sort((a, b) => b.strength - a.strength)
    .slice(0, 3);

  topTrends.forEach((trend, i) => {
    console.log(
      `${i + 1}. ${trend.direction.toUpperCase()}: Strength ${(trend.strength * 100).toFixed(1)}%, Slope ${trend.slope.toFixed(4)}`
    );
  });
  console.log();
}

if (results.correlations && results.correlations.length > 0) {
  console.log('=== Top 3 Correlations ===\n');

  results.correlations.slice(0, 3).forEach((corr, i) => {
    console.log(
      `${i + 1}. ${corr.series1} ↔ ${corr.series2}: ${corr.coefficient.toFixed(3)} (${corr.strength})`
    );
  });
  console.log();
}

if (results.changepoints && results.changepoints.length > 0) {
  console.log('=== Top 3 Changepoints ===\n');

  results.changepoints.slice(0, 3).forEach((cp, i) => {
    console.log(
      `${i + 1}. Magnitude ${cp.magnitude.toFixed(2)}: ${cp.beforeMean.toFixed(2)} → ${cp.afterMean.toFixed(2)}`
    );
  });
  console.log();
}

console.log('=== Performance Metrics ===\n');
console.log(`Total Data Points: ${results.metadata.dataPointsProcessed}`);
console.log(`Processing Rate: ${(results.metadata.dataPointsProcessed / results.metadata.executionTimeMs * 1000).toFixed(0)} points/sec`);
console.log(`Average Time per Point: ${(results.metadata.executionTimeMs / results.metadata.dataPointsProcessed).toFixed(4)}ms`);
