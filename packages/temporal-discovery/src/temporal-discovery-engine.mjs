/**
 * @file Temporal discovery engine orchestrating all algorithms
 * @module @unrdf/temporal-discovery/engine
 * @description Main engine for temporal knowledge discovery
 */

import { DiscoveryOptionsSchema, DiscoveryResultsSchema } from './schemas.mjs';
import { mineReceiptPatterns } from './pattern-miner.mjs';
import { detectAnomalies, batchDetectAnomalies } from './anomaly-detector.mjs';
import { analyzeTrends } from './trend-analyzer.mjs';
import { findMultipleCorrelations } from './correlation-finder.mjs';
import { detectChangepoints } from './changepoint-detector.mjs';

/**
 * Temporal Discovery Engine
 * Orchestrates pattern mining, anomaly detection, trend analysis,
 * correlation finding, and changepoint detection
 */
export class TemporalDiscoveryEngine {
  /**
   * Create a new temporal discovery engine
   * @param {Object} options - Engine options
   * @example
   * const engine = new TemporalDiscoveryEngine({
   *   enablePatternMining: true,
   *   enableAnomalyDetection: true
   * });
   */
  constructor(options = {}) {
    this.options = DiscoveryOptionsSchema.parse(options);
  }

  /**
   * Run full discovery analysis on time series data
   * @param {Object|Object[]} input - Time series or array of time series
   * @returns {Object} Discovery results
   * @throws {Error} If input is invalid
   * @example
   * const results = engine.discover(timeSeries);
   * console.log(results.anomalies, results.trends);
   */
  discover(input) {
    const startTime = performance.now();
    const results = {
      patterns: undefined,
      anomalies: undefined,
      trends: undefined,
      correlations: undefined,
      changepoints: undefined,
      metadata: {
        executionTimeMs: 0,
        dataPointsProcessed: 0,
        algorithmsRun: [],
      },
    };

    const timeSeriesList = Array.isArray(input) ? input : [input];
    let totalDataPoints = 0;

    for (const series of timeSeriesList) {
      if (series.data) {
        totalDataPoints += series.data.length;
      }
    }

    if (this.options.enableAnomalyDetection) {
      results.metadata.algorithmsRun.push('anomaly_detection');

      if (timeSeriesList.length === 1) {
        results.anomalies = detectAnomalies(
          timeSeriesList[0],
          this.options.anomalyDetection
        );
      } else {
        const anomalyMap = batchDetectAnomalies(
          timeSeriesList,
          this.options.anomalyDetection
        );
        results.anomalies = [];
        for (const anomalies of anomalyMap.values()) {
          results.anomalies.push(...anomalies);
        }
      }
    }

    if (this.options.enableTrendAnalysis) {
      results.metadata.algorithmsRun.push('trend_analysis');
      results.trends = [];

      for (const series of timeSeriesList) {
        const trends = analyzeTrends(series, this.options.trendAnalysis);
        results.trends.push(...trends);
      }
    }

    if (this.options.enableCorrelationAnalysis && timeSeriesList.length > 1) {
      results.metadata.algorithmsRun.push('correlation_analysis');
      results.correlations = findMultipleCorrelations(
        timeSeriesList,
        this.options.correlation
      );
    }

    if (this.options.enableChangepointDetection) {
      results.metadata.algorithmsRun.push('changepoint_detection');
      results.changepoints = [];

      for (const series of timeSeriesList) {
        const changepoints = detectChangepoints(
          series,
          this.options.changepoint
        );
        results.changepoints.push(...changepoints);
      }
    }

    const endTime = performance.now();
    results.metadata.executionTimeMs = endTime - startTime;
    results.metadata.dataPointsProcessed = totalDataPoints;

    return DiscoveryResultsSchema.parse(results);
  }

  /**
   * Run discovery on KGC receipts
   * @param {Object[]} receipts - Array of KGC receipts
   * @returns {Object} Discovery results including patterns
   * @example
   * const results = engine.discoverFromReceipts(receipts);
   */
  discoverFromReceipts(receipts) {
    const startTime = performance.now();
    const results = {
      patterns: undefined,
      metadata: {
        executionTimeMs: 0,
        dataPointsProcessed: receipts.length,
        algorithmsRun: [],
      },
    };

    if (this.options.enablePatternMining) {
      results.metadata.algorithmsRun.push('pattern_mining');
      results.patterns = mineReceiptPatterns(
        receipts,
        this.options.patternMining
      );
    }

    const endTime = performance.now();
    results.metadata.executionTimeMs = endTime - startTime;

    return results;
  }

  /**
   * Discover anomalies in time series
   * @param {Object} timeSeries - Time series data
   * @param {Object} options - Anomaly detection options
   * @returns {Object[]} Detected anomalies
   * @example
   * const anomalies = engine.discoverAnomalies(series);
   */
  discoverAnomalies(timeSeries, options = {}) {
    const opts = { ...this.options.anomalyDetection, ...options };
    return detectAnomalies(timeSeries, opts);
  }

  /**
   * Discover trends in time series
   * @param {Object} timeSeries - Time series data
   * @param {Object} options - Trend analysis options
   * @returns {Object[]} Detected trends
   * @example
   * const trends = engine.discoverTrends(series);
   */
  discoverTrends(timeSeries, options = {}) {
    const opts = { ...this.options.trendAnalysis, ...options };
    return analyzeTrends(timeSeries, opts);
  }

  /**
   * Discover changepoints in time series
   * @param {Object} timeSeries - Time series data
   * @param {Object} options - Changepoint detection options
   * @returns {Object[]} Detected changepoints
   * @example
   * const changepoints = engine.discoverChangepoints(series);
   */
  discoverChangepoints(timeSeries, options = {}) {
    const opts = { ...this.options.changepoint, ...options };
    return detectChangepoints(timeSeries, opts);
  }

  /**
   * Get summary statistics for discovery results
   * @param {Object} results - Discovery results
   * @returns {Object} Summary statistics
   * @example
   * const summary = engine.getSummary(results);
   */
  getSummary(results) {
    return {
      totalAnomalies: results.anomalies?.length || 0,
      criticalAnomalies:
        results.anomalies?.filter((a) => a.severity === 'critical').length || 0,
      totalTrends: results.trends?.length || 0,
      increasingTrends:
        results.trends?.filter((t) => t.direction === 'increasing').length || 0,
      decreasingTrends:
        results.trends?.filter((t) => t.direction === 'decreasing').length || 0,
      totalChangepoints: results.changepoints?.length || 0,
      totalCorrelations: results.correlations?.length || 0,
      strongCorrelations:
        results.correlations?.filter(
          (c) => c.strength === 'strong' || c.strength === 'very_strong'
        ).length || 0,
      totalPatterns: results.patterns?.length || 0,
      executionTimeMs: results.metadata?.executionTimeMs || 0,
      algorithmsRun: results.metadata?.algorithmsRun || [],
    };
  }
}

/**
 * Create a new temporal discovery engine
 * @param {Object} options - Engine options
 * @returns {TemporalDiscoveryEngine} Engine instance
 * @example
 * const engine = createDiscoveryEngine({ enableAnomalyDetection: true });
 */
export function createDiscoveryEngine(options = {}) {
  return new TemporalDiscoveryEngine(options);
}
