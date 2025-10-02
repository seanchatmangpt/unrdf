/**
 * @file Dark Matter 80/20 Critical Path Identification
 * @module dark-matter/critical-path
 *
 * @description
 * Implements the 80/20 algorithm to identify the top 20% slowest queries
 * that account for 80% of performance impact.
 */

import { z } from 'zod';

/**
 * Schema for query execution log
 */
const QueryExecutionLogSchema = z.object({
  queryId: z.string(),
  query: z.string(),
  executionTime: z.number(),
  timestamp: z.number(),
  metadata: z.object({}).passthrough().optional()
});

/**
 * Schema for critical path result
 */
const CriticalPathResultSchema = z.object({
  criticalQueries: z.array(z.object({
    queryId: z.string(),
    query: z.string(),
    executionTime: z.number(),
    occurrences: z.number(),
    totalTime: z.number(),
    percentageOfTotal: z.number(),
    rank: z.number()
  })),
  metrics: z.object({
    totalQueries: z.number(),
    criticalQueryCount: z.number(),
    criticalQueryPercentage: z.number(),
    totalExecutionTime: z.number(),
    criticalExecutionTime: z.number(),
    impactRatio: z.number(),
    avgExecutionTime: z.number(),
    p50: z.number(),
    p90: z.number(),
    p99: z.number()
  }),
  timestamp: z.number()
});

/**
 * Critical Path Identifier for Dark Matter 80/20
 */
export class CriticalPathIdentifier {
  /**
   * Create a new critical path identifier
   * @param {Object} [config] - Configuration
   */
  constructor(config = {}) {
    this.config = {
      targetImpactRatio: config.targetImpactRatio || 0.8, // 80% of impact
      targetQueryRatio: config.targetQueryRatio || 0.2, // from 20% of queries
      minSampleSize: config.minSampleSize || 10,
      windowSize: config.windowSize || 1000, // Keep last 1000 queries
      ...config
    };

    this.executionLogs = [];
    this.cache = new Map();
  }

  /**
   * Log a query execution
   * @param {string} queryId - Query identifier
   * @param {string} query - Query string
   * @param {number} executionTime - Execution time in ms
   * @param {Object} [metadata] - Optional metadata
   */
  logExecution(queryId, query, executionTime, metadata = {}) {
    const log = {
      queryId,
      query,
      executionTime,
      timestamp: Date.now(),
      metadata
    };

    this.executionLogs.push(QueryExecutionLogSchema.parse(log));

    // Keep window size
    if (this.executionLogs.length > this.config.windowSize) {
      this.executionLogs.shift();
    }

    // Invalidate cache
    this.cache.clear();
  }

  /**
   * Analyze and identify critical path
   * @returns {Object} Critical path analysis
   */
  identify() {
    if (this.executionLogs.length < this.config.minSampleSize) {
      throw new Error(
        `Insufficient data: ${this.executionLogs.length} queries logged, ` +
        `minimum ${this.config.minSampleSize} required`
      );
    }

    // Check cache
    const cacheKey = 'critical-path';
    if (this.cache.has(cacheKey)) {
      return this.cache.get(cacheKey);
    }

    // Group queries by queryId and calculate statistics
    const queryStats = this._aggregateQueryStats();

    // Sort by total execution time (descending)
    const sortedQueries = Array.from(queryStats.values())
      .sort((a, b) => b.totalTime - a.totalTime);

    // Calculate total execution time
    const totalExecutionTime = sortedQueries.reduce((sum, q) => sum + q.totalTime, 0);

    // Find critical queries (top 20% that account for 80% of time)
    const criticalQueries = this._findCriticalQueries(
      sortedQueries,
      totalExecutionTime
    );

    // Calculate metrics
    const metrics = this._calculateMetrics(
      sortedQueries,
      criticalQueries,
      totalExecutionTime
    );

    const result = {
      criticalQueries: criticalQueries.map((q, index) => ({
        ...q,
        rank: index + 1
      })),
      metrics,
      timestamp: Date.now()
    };

    const validated = CriticalPathResultSchema.parse(result);

    // Cache result
    this.cache.set(cacheKey, validated);

    return validated;
  }

  /**
   * Aggregate query statistics
   * @returns {Map} Query statistics
   * @private
   */
  _aggregateQueryStats() {
    const stats = new Map();

    for (const log of this.executionLogs) {
      if (!stats.has(log.queryId)) {
        stats.set(log.queryId, {
          queryId: log.queryId,
          query: log.query,
          executionTime: 0,
          occurrences: 0,
          totalTime: 0,
          percentageOfTotal: 0
        });
      }

      const stat = stats.get(log.queryId);
      stat.occurrences++;
      stat.totalTime += log.executionTime;
      stat.executionTime = stat.totalTime / stat.occurrences; // Average
    }

    return stats;
  }

  /**
   * Find critical queries using 80/20 algorithm
   * @param {Array} sortedQueries - Queries sorted by total time
   * @param {number} totalExecutionTime - Total execution time
   * @returns {Array} Critical queries
   * @private
   */
  _findCriticalQueries(sortedQueries, totalExecutionTime) {
    const critical = [];
    let cumulativeTime = 0;
    let cumulativePercentage = 0;

    // Find queries that contribute to target impact ratio (80%)
    for (const query of sortedQueries) {
      cumulativeTime += query.totalTime;
      cumulativePercentage = cumulativeTime / totalExecutionTime;

      query.percentageOfTotal = (query.totalTime / totalExecutionTime) * 100;
      critical.push(query);

      // Stop when we reach the target impact ratio
      if (cumulativePercentage >= this.config.targetImpactRatio) {
        break;
      }
    }

    // Ensure we don't exceed target query ratio (20%)
    const maxCriticalQueries = Math.ceil(
      sortedQueries.length * this.config.targetQueryRatio
    );

    // Return either the queries that hit 80% impact or top 20%, whichever is smaller
    return critical.slice(0, Math.min(critical.length, maxCriticalQueries));
  }

  /**
   * Calculate performance metrics
   * @param {Array} allQueries - All queries
   * @param {Array} criticalQueries - Critical queries
   * @param {number} totalExecutionTime - Total execution time
   * @returns {Object} Metrics
   * @private
   */
  _calculateMetrics(allQueries, criticalQueries, totalExecutionTime) {
    const criticalExecutionTime = criticalQueries.reduce(
      (sum, q) => sum + q.totalTime,
      0
    );

    // Calculate percentiles
    const executionTimes = this.executionLogs
      .map(log => log.executionTime)
      .sort((a, b) => a - b);

    const p50 = this._percentile(executionTimes, 0.50);
    const p90 = this._percentile(executionTimes, 0.90);
    const p99 = this._percentile(executionTimes, 0.99);

    return {
      totalQueries: allQueries.length,
      criticalQueryCount: criticalQueries.length,
      criticalQueryPercentage: (criticalQueries.length / allQueries.length) * 100,
      totalExecutionTime,
      criticalExecutionTime,
      impactRatio: criticalExecutionTime / totalExecutionTime,
      avgExecutionTime: totalExecutionTime / this.executionLogs.length,
      p50,
      p90,
      p99
    };
  }

  /**
   * Calculate percentile
   * @param {Array} sortedValues - Sorted array of values
   * @param {number} percentile - Percentile (0-1)
   * @returns {number} Percentile value
   * @private
   */
  _percentile(sortedValues, percentile) {
    if (sortedValues.length === 0) return 0;

    const index = Math.ceil(sortedValues.length * percentile) - 1;
    return sortedValues[Math.max(0, index)];
  }

  /**
   * Get report in markdown format
   * @returns {string} Markdown report
   */
  getReport() {
    const analysis = this.identify();

    let report = '# Dark Matter 80/20 Critical Path Report\n\n';

    // Summary
    report += '## Summary\n\n';
    report += `- **Total Queries Analyzed**: ${analysis.metrics.totalQueries}\n`;
    report += `- **Critical Queries (Top ${(this.config.targetQueryRatio * 100).toFixed(0)}%)**: ${analysis.metrics.criticalQueryCount}\n`;
    report += `- **Impact Ratio**: ${(analysis.metrics.impactRatio * 100).toFixed(1)}% of total execution time\n`;
    report += `- **Average Execution Time**: ${analysis.metrics.avgExecutionTime.toFixed(2)}ms\n`;
    report += `- **P50 Latency**: ${analysis.metrics.p50.toFixed(2)}ms\n`;
    report += `- **P90 Latency**: ${analysis.metrics.p90.toFixed(2)}ms\n`;
    report += `- **P99 Latency**: ${analysis.metrics.p99.toFixed(2)}ms\n\n`;

    // Critical queries
    report += '## Critical Queries\n\n';
    report += '| Rank | Query ID | Occurrences | Avg Time | Total Time | % of Total |\n';
    report += '|------|----------|-------------|----------|------------|------------|\n';

    for (const query of analysis.criticalQueries) {
      report += `| ${query.rank} | ${query.queryId} | ${query.occurrences} | ${query.executionTime.toFixed(2)}ms | ${query.totalTime.toFixed(2)}ms | ${query.percentageOfTotal.toFixed(1)}% |\n`;
    }

    report += '\n## Recommendations\n\n';
    report += 'Focus optimization efforts on the queries listed above. These represent the critical path:\n\n';
    report += `- Optimizing the top ${analysis.metrics.criticalQueryCount} queries will improve ${(analysis.metrics.impactRatio * 100).toFixed(1)}% of query performance\n`;
    report += '- Consider adding indexes, rewriting queries, or caching results for critical queries\n';
    report += '- Monitor P99 latency to catch performance regressions\n';

    return report;
  }

  /**
   * Get execution logs
   * @param {Object} [filter] - Optional filter
   * @returns {Array} Execution logs
   */
  getLogs(filter = {}) {
    let logs = [...this.executionLogs];

    if (filter.queryId) {
      logs = logs.filter(log => log.queryId === filter.queryId);
    }

    if (filter.minExecutionTime) {
      logs = logs.filter(log => log.executionTime >= filter.minExecutionTime);
    }

    if (filter.startTime && filter.endTime) {
      logs = logs.filter(
        log => log.timestamp >= filter.startTime && log.timestamp <= filter.endTime
      );
    }

    return logs;
  }

  /**
   * Clear all logs
   */
  clearLogs() {
    this.executionLogs = [];
    this.cache.clear();
  }

  /**
   * Export logs to JSON
   * @returns {string} JSON string
   */
  exportLogs() {
    return JSON.stringify({
      logs: this.executionLogs,
      config: this.config,
      timestamp: Date.now()
    }, null, 2);
  }

  /**
   * Import logs from JSON
   * @param {string} json - JSON string
   */
  importLogs(json) {
    const data = JSON.parse(json);
    this.executionLogs = data.logs.map(log =>
      QueryExecutionLogSchema.parse(log)
    );
    this.cache.clear();
  }
}

/**
 * Create a critical path identifier instance
 * @param {Object} [config] - Configuration
 * @returns {CriticalPathIdentifier} Critical path identifier
 */
export function createCriticalPathIdentifier(config = {}) {
  return new CriticalPathIdentifier(config);
}

export default CriticalPathIdentifier;
