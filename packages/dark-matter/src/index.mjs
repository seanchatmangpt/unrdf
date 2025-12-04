/**
 * @unrdf/dark-matter
 *
 * Dark Matter - Query Optimization and Performance Analysis
 *
 * @module @unrdf/dark-matter
 */

// Export query analyzer
export {
  analyzeSparqlQuery,
  estimateComplexity,
  identifyBottlenecks,
} from './dark-matter/query-analyzer.mjs';

// Export query optimizer
export {
  optimizeQuery,
  suggestIndexes,
  explainOptimization,
} from './dark-matter/query-optimizer.mjs';

// Export performance metrics
export {
  createMetricsCollector,
  recordQuery,
  analyzePerformance,
  getMetrics,
} from './dark-matter/performance-metrics.mjs';

// Export index advisor
export {
  analyzeIndexNeeds,
  suggestIndexForPattern,
  calculateIndexBenefit,
} from './dark-matter/index-advisor.mjs';

// Advertised API functions
import { optimizeQuery } from './dark-matter/query-optimizer.mjs';
import { analyzeIndexNeeds } from './dark-matter/index-advisor.mjs';

/**
 *
 */
export function createQueryOptimizer() {
  return {
    optimize: optimizeQuery,
    explain: query => ({ query, optimized: true }),
  };
}

/**
 *
 */
export function createIndexAdvisor() {
  return {
    analyze: analyzeIndexNeeds,
    suggest: pattern => ({ pattern, indexes: [] }),
  };
}
