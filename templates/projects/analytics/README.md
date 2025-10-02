# UNRDF Analytics Project

An analytics-focused UNRDF project template for knowledge graph analysis, metrics collection, and insight generation.

## Features

- Knowledge graph analytics and metrics
- Time-series analysis and trend detection
- Anomaly detection with Knowledge Hooks
- Real-time monitoring and alerting
- Analytics dashboards and reporting

## Project Structure

```
.
├── src/
│   ├── index.mjs                    # Main entry point
│   ├── analytics/
│   │   ├── run-analysis.mjs         # Analysis orchestration
│   │   ├── metrics-collector.mjs    # Metrics collection
│   │   ├── dashboard.mjs            # Analytics dashboard
│   │   └── analyzers/               # Analysis modules
│   │       ├── graph-stats.mjs
│   │       ├── trend-detector.mjs
│   │       └── anomaly-detector.mjs
│   └── hooks/                       # Analytics hooks
│       ├── threshold-monitor.mjs
│       ├── pattern-detector.mjs
│       └── aggregator.mjs
├── queries/                         # SPARQL analytics queries
│   ├── metrics/
│   │   ├── node-degree.rq
│   │   ├── centrality.rq
│   │   └── clustering.rq
│   └── insights/
│       ├── trends.rq
│       └── anomalies.rq
├── data/                            # Sample datasets
│   └── sample-metrics.ttl
├── test/
│   └── analytics/
│       ├── metrics.test.mjs
│       └── analyzers.test.mjs
└── unrdf.config.mjs
```

## Getting Started

1. Install dependencies:
   ```bash
   npm install
   ```

2. Run analysis:
   ```bash
   npm run analyze
   ```

3. Collect metrics:
   ```bash
   npm run metrics
   ```

4. View dashboard:
   ```bash
   npm run dashboard
   ```

5. Run tests:
   ```bash
   npm test
   ```

## Analytics Features

### Graph Metrics
- Node count and degree distribution
- Centrality measures (betweenness, closeness, PageRank)
- Clustering coefficient
- Graph density and connectivity

### Trend Detection
- Time-series analysis
- Moving averages and smoothing
- Seasonality detection
- Forecast generation

### Anomaly Detection
- Statistical outlier detection
- Pattern-based anomaly identification
- Threshold monitoring
- Real-time alerting

### Real-time Monitoring
- Knowledge Hooks for live data streams
- Windowed aggregations
- Rate limiting and spike detection
- Automated alerts

## Sample Analyses

### 1. Network Analysis
```sparql
# Find most connected nodes
SELECT ?node (COUNT(?connection) AS ?degree)
WHERE {
  ?node ?property ?connection .
}
GROUP BY ?node
ORDER BY DESC(?degree)
LIMIT 10
```

### 2. Trend Analysis
```sparql
# Analyze growth over time
SELECT ?month (COUNT(?entity) AS ?count)
WHERE {
  ?entity ex:createdAt ?timestamp .
  BIND(SUBSTR(STR(?timestamp), 1, 7) AS ?month)
}
GROUP BY ?month
ORDER BY ?month
```

### 3. Anomaly Detection
Uses Knowledge Hooks to detect:
- Values exceeding thresholds
- Unusual patterns in relationships
- Rapid changes in metrics
- Missing expected data

## Dashboard

The analytics dashboard provides:
- Real-time metrics visualization
- Historical trend charts
- Anomaly alerts
- Custom analytics reports

Access via `npm run dashboard`

## Next Steps

1. Load your data into `data/`
2. Create custom analytics queries in `queries/`
3. Implement domain-specific analyzers in `src/analytics/analyzers/`
4. Configure threshold hooks for alerting
5. Build custom dashboards

## Documentation

- [UNRDF Analytics Guide](https://github.com/unrdf/unrdf/blob/main/docs/analytics.md)
- [Knowledge Hooks for Analytics](https://github.com/unrdf/unrdf/blob/main/docs/analytics-hooks.md)
- [SPARQL Analytics Patterns](https://www.w3.org/TR/sparql11-query/)

## License

MIT
