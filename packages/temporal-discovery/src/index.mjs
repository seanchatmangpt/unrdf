/**
 * @file Temporal Knowledge Discovery for RDF Graphs
 * @module @unrdf/temporal-discovery
 * @description Pattern mining, anomaly detection, trend analysis, and changepoint detection
 */

export {
  TemporalDiscoveryEngine,
  createDiscoveryEngine,
} from './temporal-discovery-engine.mjs';

export {
  apriori,
  receiptsToTransactions,
  mineReceiptPatterns,
} from './pattern-miner.mjs';

export {
  detectAnomalies,
  batchDetectAnomalies,
  filterBySeverity,
} from './anomaly-detector.mjs';

export {
  analyzeTrends,
  getSmoothedSeries,
  findStrongestTrend,
} from './trend-analyzer.mjs';

export {
  findCorrelation,
  findMultipleCorrelations,
  filterByStrength,
} from './correlation-finder.mjs';

export {
  detectChangepoints,
  findMostSignificantChangepoint,
  segmentTimeSeries,
} from './changepoint-detector.mjs';

export * from './schemas.mjs';
