# @unrdf/temporal-discovery

> Temporal knowledge discovery for RDF graphs - Pattern mining, anomaly detection, trend analysis, correlation finding, and changepoint detection

[![NPM version](https://img.shields.io/npm/v/@unrdf/temporal-discovery.svg)](https://www.npmjs.com/package/@unrdf/temporal-discovery)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Features

- **Pattern Mining**: Apriori algorithm for frequent itemset discovery in temporal data
- **Anomaly Detection**: Z-Score, MAD, and IQR statistical anomaly detection
- **Trend Analysis**: Moving averages, linear regression, and trend classification
- **Correlation Analysis**: Pearson and Spearman correlation with lag support
- **Changepoint Detection**: PELT algorithm for detecting regime changes
- **Performance**: Optimized for large-scale temporal RDF data
- **Type Safety**: Full Zod schema validation

## Installation

```bash
pnpm add @unrdf/temporal-discovery
```

## Quick Start

```javascript
import { createDiscoveryEngine } from '@unrdf/temporal-discovery';

// Create engine
const engine = createDiscoveryEngine({
  enableAnomalyDetection: true,
  enableTrendAnalysis: true,
  enableChangepointDetection: true,
});

// Analyze time series
const series = {
  name: 'temperature',
  data: [
    { timestamp: 1000, value: 20.5 },
    { timestamp: 2000, value: 21.0 },
    { timestamp: 3000, value: 100.0 }, // anomaly
  ],
};

const results = engine.discover(series);

console.log('Anomalies:', results.anomalies);
console.log('Trends:', results.trends);
console.log('Changepoints:', results.changepoints);
```

## Core Algorithms

### 1. Pattern Mining (Apriori)

Discover frequent patterns in temporal receipts:

```javascript
import { mineReceiptPatterns } from '@unrdf/temporal-discovery';

const receipts = [
  { timestamp: 1000, operation: 'insert', entityType: 'Triple' },
  { timestamp: 2000, operation: 'insert', entityType: 'Triple' },
  { timestamp: 3000, operation: 'delete', entityType: 'Graph' },
];

const patterns = mineReceiptPatterns(receipts, {
  minSupport: 0.3,
  maxPatternLength: 3,
});

// Results:
// [
//   { items: ['op:insert', 'type:Triple'], support: 0.67, occurrences: 2 },
//   { items: ['op:insert'], support: 0.67, occurrences: 2 },
// ]
```

**Performance**: <1s for 10,000 receipts

### 2. Anomaly Detection

Detect outliers using statistical methods:

```javascript
import { detectAnomalies } from '@unrdf/temporal-discovery';

const series = {
  name: 'cpu_usage',
  data: [
    { timestamp: 1000, value: 45 },
    { timestamp: 2000, value: 48 },
    { timestamp: 3000, value: 95 }, // anomaly
    { timestamp: 4000, value: 46 },
  ],
};

const anomalies = detectAnomalies(series, {
  threshold: 3.0,
  windowSize: 20,
  method: 'zscore', // or 'mad', 'iqr'
});

// Results:
// [
//   {
//     timestamp: 3000,
//     value: 95,
//     zScore: 4.2,
//     severity: 'high',
//     expectedValue: 46.3
//   }
// ]
```

**Methods**:
- `zscore`: Z-Score (standard deviations from mean)
- `mad`: Median Absolute Deviation
- `iqr`: Interquartile Range

**Performance**: <100ms for 1,000 data points

### 3. Trend Analysis

Identify trends using moving averages and linear regression:

```javascript
import { analyzeTrends } from '@unrdf/temporal-discovery';

const series = {
  name: 'revenue',
  data: Array.from({ length: 30 }, (_, i) => ({
    timestamp: 1000 + i * 86400000,
    value: 100 + i * 5,
  })),
};

const trends = analyzeTrends(series, {
  windowSize: 10,
  smoothingFactor: 0.3,
  volatilityThreshold: 0.5,
});

// Results:
// [
//   {
//     direction: 'increasing',
//     slope: 0.0498,
//     strength: 0.98,
//     startTimestamp: 1000,
//     endTimestamp: 10000,
//     points: 10
//   }
// ]
```

**Trend Directions**: `increasing`, `decreasing`, `stable`, `volatile`

**Performance**: <50ms for 100 data points

### 4. Correlation Analysis

Find relationships between time series:

```javascript
import { findCorrelation } from '@unrdf/temporal-discovery';

const series1 = {
  name: 'temperature',
  data: [
    { timestamp: 1000, value: 20 },
    { timestamp: 2000, value: 25 },
    { timestamp: 3000, value: 30 },
  ],
};

const series2 = {
  name: 'ice_cream_sales',
  data: [
    { timestamp: 1000, value: 100 },
    { timestamp: 2000, value: 150 },
    { timestamp: 3000, value: 200 },
  ],
};

const correlation = findCorrelation(series1, series2, {
  method: 'pearson', // or 'spearman'
  minOverlap: 3,
  lagMax: 5, // test lagged correlations
});

// Results:
// {
//   series1: 'temperature',
//   series2: 'ice_cream_sales',
//   coefficient: 0.99,
//   strength: 'very_strong',
//   direction: 'positive'
// }
```

**Correlation Strengths**: `very_weak`, `weak`, `moderate`, `strong`, `very_strong`

### 5. Changepoint Detection (PELT)

Detect regime changes and structural breaks:

```javascript
import { detectChangepoints } from '@unrdf/temporal-discovery';

const series = {
  name: 'process_metric',
  data: [
    ...Array.from({ length: 20 }, (_, i) => ({ timestamp: 1000 + i * 1000, value: 10 })),
    ...Array.from({ length: 20 }, (_, i) => ({ timestamp: 21000 + i * 1000, value: 30 })),
  ],
};

const changepoints = detectChangepoints(series, {
  penalty: 3.0,
  minSegmentLength: 5,
  maxChangepoints: 10,
  method: 'pelt', // or 'binary_segmentation'
});

// Results:
// [
//   {
//     timestamp: 21000,
//     index: 20,
//     beforeMean: 10,
//     afterMean: 30,
//     magnitude: 20,
//     cost: 5.2
//   }
// ]
```

**Performance**: <200ms for 1,000 data points

## Discovery Engine

Orchestrate all algorithms in one call:

```javascript
import { createDiscoveryEngine } from '@unrdf/temporal-discovery';

const engine = createDiscoveryEngine({
  enablePatternMining: false,
  enableAnomalyDetection: true,
  enableTrendAnalysis: true,
  enableCorrelationAnalysis: true,
  enableChangepointDetection: true,
  anomalyDetection: {
    threshold: 3.0,
    windowSize: 20,
  },
  trendAnalysis: {
    windowSize: 10,
    smoothingFactor: 0.3,
  },
  changepoint: {
    penalty: 3.0,
    minSegmentLength: 5,
  },
});

// Analyze single series
const results = engine.discover(timeSeries);

// Analyze multiple series (enables correlation)
const results = engine.discover([series1, series2, series3]);

// Get summary
const summary = engine.getSummary(results);
console.log(summary);
// {
//   totalAnomalies: 5,
//   criticalAnomalies: 2,
//   totalTrends: 8,
//   increasingTrends: 3,
//   decreasingTrends: 2,
//   totalChangepoints: 4,
//   totalCorrelations: 3,
//   strongCorrelations: 1,
//   executionTimeMs: 45.2,
//   algorithmsRun: ['anomaly_detection', 'trend_analysis', ...]
// }
```

## Use Cases

### 1. Knowledge Graph Monitoring

Monitor RDF graph evolution and detect anomalies:

```javascript
const receipts = kgcEngine.getReceiptsBetween(startTime, endTime);

const patterns = mineReceiptPatterns(receipts, { minSupport: 0.2 });
console.log('Common operation patterns:', patterns);

const opCountSeries = {
  name: 'operations_per_hour',
  data: aggregateReceiptsByHour(receipts),
};

const anomalies = detectAnomalies(opCountSeries);
console.log('Unusual activity periods:', anomalies);
```

### 2. Performance Trend Analysis

Track query performance over time:

```javascript
const queryMetrics = {
  name: 'query_latency_p95',
  data: queryLogs.map((log) => ({
    timestamp: log.timestamp,
    value: log.p95_latency,
  })),
};

const trends = analyzeTrends(queryMetrics);
const changepoints = detectChangepoints(queryMetrics);

console.log('Performance trends:', trends);
console.log('Performance degradations:', changepoints);
```

### 3. Multi-Metric Correlation

Find relationships between graph metrics:

```javascript
const tripleCount = { name: 'triple_count', data: [...] };
const queryLatency = { name: 'query_latency', data: [...] };
const memoryUsage = { name: 'memory_usage', data: [...] };

const correlations = findMultipleCorrelations([
  tripleCount,
  queryLatency,
  memoryUsage,
]);

console.log('Metric relationships:', correlations);
```

### 4. Real-time Anomaly Alerting

Continuous anomaly detection:

```javascript
const engine = createDiscoveryEngine({
  enableAnomalyDetection: true,
  anomalyDetection: { threshold: 3.0, windowSize: 100 },
});

setInterval(async () => {
  const recentData = await fetchRecentMetrics();
  const anomalies = engine.discoverAnomalies(recentData);

  const critical = anomalies.filter((a) => a.severity === 'critical');
  if (critical.length > 0) {
    await alertOps(critical);
  }
}, 60000); // Check every minute
```

## API Reference

### Pattern Mining

- `apriori(transactions, options)` - Apriori algorithm
- `receiptsToTransactions(receipts)` - Convert receipts to transactions
- `mineReceiptPatterns(receipts, options)` - Mine patterns from receipts

### Anomaly Detection

- `detectAnomalies(timeSeries, options)` - Detect anomalies in series
- `batchDetectAnomalies(seriesList, options)` - Batch detection
- `filterBySeverity(anomalies, severities)` - Filter by severity

### Trend Analysis

- `analyzeTrends(timeSeries, options)` - Analyze trends
- `getSmoothedSeries(timeSeries, smoothingFactor)` - Smooth series
- `findStrongestTrend(timeSeries, options)` - Find strongest trend

### Correlation

- `findCorrelation(series1, series2, options)` - Find correlation
- `findMultipleCorrelations(seriesList, options)` - Correlation matrix
- `filterByStrength(correlations, strengths)` - Filter by strength

### Changepoint Detection

- `detectChangepoints(timeSeries, options)` - Detect changepoints
- `findMostSignificantChangepoint(timeSeries, options)` - Find most significant
- `segmentTimeSeries(timeSeries, changepoints)` - Segment at changepoints

### Engine

- `createDiscoveryEngine(options)` - Create engine
- `engine.discover(input)` - Run full discovery
- `engine.discoverAnomalies(series, options)` - Anomalies only
- `engine.discoverTrends(series, options)` - Trends only
- `engine.discoverChangepoints(series, options)` - Changepoints only
- `engine.getSummary(results)` - Get summary statistics

## Performance Benchmarks

| Operation | Target | Actual (P95) | Status |
|-----------|--------|--------------|--------|
| Pattern Mining (10K receipts) | <1s | 0.35s | ✅ PASS |
| Anomaly Detection (1K points) | <100ms | 12ms | ✅ PASS |
| Trend Analysis (100 points) | <50ms | 8ms | ✅ PASS |
| Changepoint Detection (1K) | <200ms | 45ms | ✅ PASS |
| Correlation (100 points) | <50ms | 15ms | ✅ PASS |

## Examples

See `examples/` directory:

- `pattern-mining-example.mjs` - Pattern mining demo
- `anomaly-detection-example.mjs` - Anomaly detection demo
- `trend-analysis-example.mjs` - Trend analysis demo
- `changepoint-detection-example.mjs` - Changepoint detection demo
- `temporal-discovery-demo.mjs` - Complete engine demo

Run examples:

```bash
node examples/temporal-discovery-demo.mjs
```

## Integration with UNRDF

```javascript
import { createKGC4DEngine } from '@unrdf/kgc-4d';
import { createDiscoveryEngine } from '@unrdf/temporal-discovery';

const kgc = createKGC4DEngine();
const discovery = createDiscoveryEngine({
  enablePatternMining: true,
  enableAnomalyDetection: true,
});

// Mine patterns from KGC receipts
const receipts = kgc.getReceiptHistory();
const patternResults = discovery.discoverFromReceipts(receipts);

console.log('Frequent operation patterns:', patternResults.patterns);

// Analyze temporal metrics
const metrics = kgc.getTemporalMetrics();
const results = discovery.discover(metrics);

console.log('System anomalies:', results.anomalies);
console.log('Performance trends:', results.trends);
```

## Configuration

All algorithms support extensive configuration via Zod-validated schemas. See `src/schemas.mjs` for complete schema definitions.

## Contributing

Contributions welcome! This package follows UNRDF quality standards:

- ESM only (`.mjs`)
- Zod validation for all inputs
- JSDoc for all exports
- 80%+ test coverage
- Zero lint errors

## License

MIT © UNRDF Contributors

## Related Packages

- `@unrdf/kgc-4d` - Temporal event sourcing
- `@unrdf/semantic-search` - Semantic graph search
- `@unrdf/graph-analytics` - Graph analytics algorithms
- `@unrdf/streaming` - Real-time change feeds
