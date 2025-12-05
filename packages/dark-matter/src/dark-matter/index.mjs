/**
 * @file Dark Matter 80/20 Query Optimization - Main Export
 * @module dark-matter
 *
 * @description
 * Main entry point for Dark Matter 80/20 query optimization system.
 * Provides integrated query analysis, critical path identification,
 * and query optimization following the 80/20 principle.
 */

import { QueryAnalyzer, createQueryAnalyzer } from './query-analyzer.mjs';
import { CriticalPathIdentifier, createCriticalPathIdentifier } from './critical-path.mjs';
import { DarkMatterOptimizer, createDarkMatterOptimizer } from './optimizer.mjs';

/**
 * Integrated Dark Matter query optimization system
 */
export class DarkMatterQuerySystem {
  /**
   * Create a new Dark Matter query system
   * @param {Object} [config] - Configuration
   */
  constructor(config = {}) {
    this.analyzer = createQueryAnalyzer(config.analyzer);
    this.criticalPath = createCriticalPathIdentifier(config.criticalPath);
    this.optimizer = createDarkMatterOptimizer(config.optimizer);

    this.config = {
      enableAutoOptimization: config.enableAutoOptimization !== false,
      complexityThreshold: config.complexityThreshold || 100,
      ...config,
    };
  }

  /**
   * Analyze a query
   * @param {string} query - SPARQL query
   * @param {string} [queryId] - Optional query identifier
   * @returns {Object} Analysis result
   */
  analyze(query, queryId = null) {
    return this.analyzer.analyze(query, queryId);
  }

  /**
   * Log query execution for critical path analysis
   * @param {string} queryId - Query identifier
   * @param {string} query - SPARQL query
   * @param {number} executionTime - Execution time in ms
   * @param {Object} [metadata] - Optional metadata
   */
  logExecution(queryId, query, executionTime, metadata = {}) {
    this.criticalPath.logExecution(queryId, query, executionTime, metadata);
  }

  /**
   * Identify critical queries
   * @returns {Object} Critical path analysis
   */
  identifyCriticalQueries() {
    return this.criticalPath.identify();
  }

  /**
   * Optimize a query
   * @param {string} query - SPARQL query
   * @param {Object} [analysis] - Optional pre-computed analysis
   * @returns {Object} Optimization result
   */
  optimize(query, analysis = null) {
    // Analyze first if not provided
    if (!analysis) {
      analysis = this.analyzer.analyze(query);
    }

    // Only optimize if above complexity threshold
    if (analysis.complexity.score < this.config.complexityThreshold) {
      return {
        original: query,
        optimized: query,
        rules: [],
        estimatedImprovement: {
          before: analysis.complexity.score,
          after: analysis.complexity.score,
          percentageGain: 0,
        },
        timestamp: Date.now(),
        skipped: true,
        reason: 'Query complexity below threshold',
      };
    }

    return this.optimizer.optimize(query, analysis);
  }

  /**
   * Analyze and optimize a query in one step
   * @param {string} query - SPARQL query
   * @param {string} [queryId] - Optional query identifier
   * @returns {Object} Combined analysis and optimization
   */
  analyzeAndOptimize(query, queryId = null) {
    const analysis = this.analyze(query, queryId);
    const optimization = this.optimize(query, analysis);

    return {
      analysis,
      optimization,
      shouldOptimize: !optimization.skipped,
    };
  }

  /**
   * Process query execution: analyze, log, and optionally optimize
   * @param {string} query - SPARQL query
   * @param {number} executionTime - Execution time in ms
   * @param {string} [queryId] - Optional query identifier
   * @returns {Object} Processing result
   */
  processExecution(query, executionTime, queryId = null) {
    const analysis = this.analyze(query, queryId);
    const id = queryId || analysis.queryId;

    // Log execution
    this.logExecution(id, query, executionTime, {
      complexity: analysis.complexity.score,
      expensiveOps: analysis.expensiveOperations.length,
    });

    // Auto-optimize if enabled and above threshold
    let optimization = null;
    if (this.config.enableAutoOptimization) {
      optimization = this.optimize(query, analysis);
    }

    return {
      queryId: id,
      analysis,
      optimization,
      logged: true,
    };
  }

  /**
   * Get comprehensive statistics
   * @returns {Object} Statistics
   */
  getStats() {
    let criticalPathMetrics = null;

    try {
      criticalPathMetrics = this.criticalPath.identify().metrics;
    } catch (error) {
      // Not enough data yet for critical path analysis
      criticalPathMetrics = {
        error: error.message,
        totalQueries: 0,
        criticalQueryCount: 0,
        criticalQueryPercentage: 0,
        totalExecutionTime: 0,
        criticalExecutionTime: 0,
        impactRatio: 0,
        avgExecutionTime: 0,
        p50: 0,
        p90: 0,
        p99: 0,
      };
    }

    return {
      analyzer: this.analyzer.getStats(),
      criticalPath: criticalPathMetrics,
      optimizer: this.optimizer.getStats(),
    };
  }

  /**
   * Generate full report
   * @returns {string} Markdown report
   */
  getReport() {
    let report = '# Dark Matter 80/20 Query Optimization Report\n\n';

    // Analyzer stats
    const analyzerStats = this.analyzer.getStats();
    report += '## Query Analysis\n\n';
    report += `- **Total Queries Analyzed**: ${analyzerStats.totalAnalyzed}\n`;
    report += `- **Complex Queries**: ${analyzerStats.complexQueries}\n`;
    report += `- **Simple Queries**: ${analyzerStats.simpleQueries}\n`;
    report += `- **Complexity Ratio**: ${(analyzerStats.complexQueryRatio * 100).toFixed(1)}%\n`;
    report += `- **Average Complexity**: ${analyzerStats.avgComplexity.toFixed(2)}\n\n`;

    // Critical path
    try {
      const criticalPathReport = this.criticalPath.getReport();
      report += criticalPathReport + '\n\n';
    } catch (error) {
      report += '## Critical Path Analysis\n\n';
      report += `*Insufficient data for analysis: ${error.message}*\n\n`;
    }

    // Optimizer stats
    const optimizerStats = this.optimizer.getStats();
    report += '## Optimization Statistics\n\n';
    report += `- **Total Optimizations**: ${optimizerStats.totalOptimizations}\n`;
    report += '- **Rules Applied**:\n';

    for (const [rule, count] of Object.entries(optimizerStats.rulesApplied)) {
      report += `  - ${rule}: ${count}\n`;
    }

    return report;
  }

  /**
   * Clear all data
   */
  clear() {
    this.analyzer.resetStats();
    this.criticalPath.clearLogs();
    this.optimizer.resetStats();
  }
}

/**
 * Create a Dark Matter query system
 * @param {Object} [config] - Configuration
 * @returns {DarkMatterQuerySystem} Query system
 */
export function createDarkMatterQuerySystem(config = {}) {
  return new DarkMatterQuerySystem(config);
}

// Re-export individual components
export {
  QueryAnalyzer,
  createQueryAnalyzer,
  CriticalPathIdentifier,
  createCriticalPathIdentifier,
  DarkMatterOptimizer,
  createDarkMatterOptimizer,
};

export default DarkMatterQuerySystem;
