/**
 * @file Anomaly detection example
 * @description Demonstrates Z-Score and MAD anomaly detection
 */

import { detectAnomalies } from '../src/anomaly-detector.mjs';

console.log('=== Anomaly Detection Example ===\n');

const normalData = Array.from({ length: 50 }, (_, i) => ({
  timestamp: 1000 + i * 1000,
  value: 20 + Math.sin(i / 5) * 5 + (Math.random() - 0.5) * 2,
}));

const anomalyIndices = [15, 30, 45];
anomalyIndices.forEach((idx) => {
  normalData[idx].value += Math.random() > 0.5 ? 50 : -50;
});

const series = {
  name: 'system_metric',
  data: normalData,
  unit: 'milliseconds',
};

console.log(`Analyzing ${series.data.length} data points for anomalies...\n`);

const anomalies = detectAnomalies(series, {
  threshold: 3.0,
  windowSize: 20,
  method: 'zscore',
});

console.log(`Found ${anomalies.length} anomalies:\n`);

anomalies.forEach((anomaly, i) => {
  const time = new Date(anomaly.timestamp).toISOString();
  console.log(`Anomaly ${i + 1}:`);
  console.log(`  Time: ${time}`);
  console.log(`  Value: ${anomaly.value.toFixed(2)}`);
  console.log(`  Expected: ${anomaly.expectedValue?.toFixed(2) || 'N/A'}`);
  console.log(`  Z-Score: ${anomaly.zScore.toFixed(2)}`);
  console.log(`  Severity: ${anomaly.severity}`);
  console.log();
});

console.log('=== Severity Distribution ===\n');
const severityCounts = {
  low: anomalies.filter((a) => a.severity === 'low').length,
  medium: anomalies.filter((a) => a.severity === 'medium').length,
  high: anomalies.filter((a) => a.severity === 'high').length,
  critical: anomalies.filter((a) => a.severity === 'critical').length,
};

Object.entries(severityCounts).forEach(([severity, count]) => {
  if (count > 0) {
    console.log(`${severity.toUpperCase()}: ${count}`);
  }
});
