/**
 * @file Trend analysis example
 * @description Demonstrates trend detection and moving averages
 */

import { analyzeTrends, findStrongestTrend } from '../src/trend-analyzer.mjs';

console.log('=== Trend Analysis Example ===\n');

const growthPhase = Array.from({ length: 30 }, (_, i) => ({
  timestamp: 1000 + i * 86400000,
  value: 100 + i * 5 + (Math.random() - 0.5) * 10,
}));

const plateauPhase = Array.from({ length: 20 }, (_, i) => ({
  timestamp: 1000 + (30 + i) * 86400000,
  value: 250 + (Math.random() - 0.5) * 15,
}));

const declinePhase = Array.from({ length: 20 }, (_, i) => ({
  timestamp: 1000 + (50 + i) * 86400000,
  value: 250 - i * 8 + (Math.random() - 0.5) * 10,
}));

const series = {
  name: 'revenue',
  data: [...growthPhase, ...plateauPhase, ...declinePhase],
  unit: 'USD',
};

console.log(`Analyzing trends in ${series.data.length} data points...\n`);

const trends = analyzeTrends(series, {
  windowSize: 15,
  smoothingFactor: 0.3,
  volatilityThreshold: 0.3,
});

console.log(`Found ${trends.length} trends:\n`);

trends.forEach((trend, i) => {
  const start = new Date(trend.startTimestamp).toISOString().split('T')[0];
  const end = new Date(trend.endTimestamp).toISOString().split('T')[0];

  console.log(`Trend ${i + 1}:`);
  console.log(`  Direction: ${trend.direction.toUpperCase()}`);
  console.log(`  Slope: ${trend.slope.toFixed(4)}`);
  console.log(`  Strength (R²): ${(trend.strength * 100).toFixed(1)}%`);
  console.log(`  Period: ${start} to ${end}`);
  console.log(`  Data Points: ${trend.points}`);
  console.log();
});

console.log('=== Strongest Trend ===\n');
const strongest = findStrongestTrend(series);

if (strongest) {
  console.log(`Direction: ${strongest.direction.toUpperCase()}`);
  console.log(`Strength: ${(strongest.strength * 100).toFixed(1)}%`);
  console.log(`Slope: ${strongest.slope.toFixed(4)}`);
}

console.log('\n=== Trend Distribution ===\n');
const distribution = {
  increasing: trends.filter((t) => t.direction === 'increasing').length,
  decreasing: trends.filter((t) => t.direction === 'decreasing').length,
  stable: trends.filter((t) => t.direction === 'stable').length,
  volatile: trends.filter((t) => t.direction === 'volatile').length,
};

Object.entries(distribution).forEach(([direction, count]) => {
  console.log(`${direction.toUpperCase()}: ${count}`);
});
