/**
 * @file SPARQL Query Execution Plan Visualization and Explanation
 * @module @unrdf/core/viz/query-explainer
 * @description Pure function SPARQL query explainer with performance stats
 */

import { z } from 'zod';

/**
 * Query explanation options schema
 */
const ExplanationOptionsSchema = z.object({
  includeStats: z.boolean().optional(),
  includeOptimizations: z.boolean().optional(),
  format: z.enum(['text', 'json', 'tree']).optional(),
}).optional();

/**
 * Parse SPARQL query into components
 * @param {string} query - SPARQL query string
 * @returns {Object} Parsed query components
 */
function parseQuery(query) {
  const trimmed = query.trim();

  const queryType = /^\s*SELECT/i.test(trimmed) ? 'SELECT'
    : /^\s*CONSTRUCT/i.test(trimmed) ? 'CONSTRUCT'
    : /^\s*ASK/i.test(trimmed) ? 'ASK'
    : /^\s*DESCRIBE/i.test(trimmed) ? 'DESCRIBE'
    : 'UNKNOWN';

  const prefixes = [];
  const prefixRegex = /PREFIX\s+(\w+):\s*<([^>]+)>/gi;
  let match;
  while ((match = prefixRegex.exec(trimmed)) !== null) {
    prefixes.push({ prefix: match[1], uri: match[2] });
  }

  const whereMatch = trimmed.match(/WHERE\s*\{([^}]+)\}/i);
  const whereClause = whereMatch ? whereMatch[1].trim() : '';

  const patterns = whereClause
    .split('.')
    .map(p => p.trim())
    .filter(p => p.length > 0);

  const filters = [];
  const filterRegex = /FILTER\s*\(([^)]+)\)/gi;
  while ((match = filterRegex.exec(trimmed)) !== null) {
    filters.push(match[1].trim());
  }

  const optionalMatch = trimmed.match(/OPTIONAL\s*\{([^}]+)\}/gi);
  const optionals = optionalMatch ? optionalMatch.map(o => o.trim()) : [];

  const limitMatch = trimmed.match(/LIMIT\s+(\d+)/i);
  const limit = limitMatch ? parseInt(limitMatch[1], 10) : null;

  const offsetMatch = trimmed.match(/OFFSET\s+(\d+)/i);
  const offset = offsetMatch ? parseInt(offsetMatch[1], 10) : null;

  return {
    queryType,
    prefixes,
    patterns,
    filters,
    optionals,
    limit,
    offset,
  };
}

/**
 * Generate execution plan for SPARQL query
 * @param {string} query - SPARQL query string
 * @param {Object} [options] - Explanation options
 * @returns {Object} Execution plan with steps
 *
 * @example
 * const plan = explainQuery('SELECT * WHERE { ?s ?p ?o }');
 * console.log(plan.steps);
 */
export function explainQuery(query, options = {}) {
  if (typeof query !== 'string' || query.trim().length === 0) {
    throw new TypeError('query must be a non-empty string');
  }

  const opts = ExplanationOptionsSchema.parse(options);
  const parsed = parseQuery(query);

  const steps = [];
  let stepCounter = 1;

  if (parsed.prefixes.length > 0) {
    steps.push({
      step: stepCounter++,
      operation: 'RESOLVE_PREFIXES',
      description: `Resolve ${parsed.prefixes.length} namespace prefix(es)`,
      cost: parsed.prefixes.length,
    });
  }

  if (parsed.patterns.length > 0) {
    for (let i = 0; i < parsed.patterns.length; i++) {
      steps.push({
        step: stepCounter++,
        operation: 'TRIPLE_PATTERN',
        description: `Match triple pattern: ${parsed.patterns[i].substring(0, 50)}`,
        cost: 10 + i * 5,
      });
    }
  }

  if (parsed.filters.length > 0) {
    for (const filter of parsed.filters) {
      steps.push({
        step: stepCounter++,
        operation: 'FILTER',
        description: `Apply filter: ${filter.substring(0, 50)}`,
        cost: 5,
      });
    }
  }

  if (parsed.optionals.length > 0) {
    steps.push({
      step: stepCounter++,
      operation: 'OPTIONAL',
      description: `Process ${parsed.optionals.length} optional pattern(s)`,
      cost: parsed.optionals.length * 8,
    });
  }

  if (parsed.limit !== null) {
    steps.push({
      step: stepCounter++,
      operation: 'LIMIT',
      description: `Limit results to ${parsed.limit}`,
      cost: 1,
    });
  }

  if (parsed.offset !== null) {
    steps.push({
      step: stepCounter++,
      operation: 'OFFSET',
      description: `Skip first ${parsed.offset} result(s)`,
      cost: 1,
    });
  }

  const totalCost = steps.reduce((sum, step) => sum + step.cost, 0);

  return {
    queryType: parsed.queryType,
    steps,
    estimatedCost: totalCost,
    optimizations: opts?.includeOptimizations ? generateOptimizations(parsed) : [],
  };
}

/**
 * Generate optimization suggestions
 * @param {Object} parsed - Parsed query components
 * @returns {Array} Array of optimization suggestions
 */
function generateOptimizations(parsed) {
  const suggestions = [];

  if (parsed.patterns.length > 3) {
    suggestions.push({
      type: 'PATTERN_ORDER',
      severity: 'medium',
      message: 'Consider reordering patterns to place most selective first',
    });
  }

  if (parsed.filters.length > 0 && parsed.optionals.length > 0) {
    suggestions.push({
      type: 'FILTER_PLACEMENT',
      severity: 'low',
      message: 'Move filters before OPTIONAL clauses when possible',
    });
  }

  if (!parsed.limit && parsed.patterns.length > 2) {
    suggestions.push({
      type: 'MISSING_LIMIT',
      severity: 'high',
      message: 'Add LIMIT clause to prevent unbounded result sets',
    });
  }

  return suggestions;
}

/**
 * Format execution plan as tree
 * @param {Object} plan - Execution plan
 * @returns {string} Tree-formatted plan
 *
 * @example
 * const tree = formatPlanAsTree(plan);
 * console.log(tree);
 */
export function formatPlanAsTree(plan) {
  if (!plan || !Array.isArray(plan.steps)) {
    throw new TypeError('plan must have steps array');
  }

  let tree = `Query Type: ${plan.queryType}\n`;
  tree += `Estimated Cost: ${plan.estimatedCost}\n\n`;
  tree += 'Execution Steps:\n';

  for (let i = 0; i < plan.steps.length; i++) {
    const step = plan.steps[i];
    const isLast = i === plan.steps.length - 1;
    const prefix = isLast ? '└──' : '├──';

    tree += `${prefix} Step ${step.step}: ${step.operation}\n`;
    tree += `${isLast ? '   ' : '│  '} ${step.description} (cost: ${step.cost})\n`;
  }

  if (plan.optimizations && plan.optimizations.length > 0) {
    tree += '\nOptimization Suggestions:\n';
    for (const opt of plan.optimizations) {
      tree += `  [${opt.severity.toUpperCase()}] ${opt.message}\n`;
    }
  }

  return tree;
}

/**
 * Track query execution statistics
 * @param {Function} queryFn - Query execution function
 * @param {string} query - SPARQL query string
 * @returns {Promise<Object>} Execution results with stats
 *
 * @example
 * const stats = await trackQueryStats(async () => executeQuery(store, query), query);
 * console.log(`Execution time: ${stats.executionTime}ms`);
 */
export async function trackQueryStats(queryFn, query) {
  if (typeof queryFn !== 'function') {
    throw new TypeError('queryFn must be a function');
  }

  const startTime = performance.now();
  const startMemory = process.memoryUsage().heapUsed;

  let result;
  let error = null;

  try {
    result = await queryFn();
  } catch (err) {
    error = err;
  }

  const endTime = performance.now();
  const endMemory = process.memoryUsage().heapUsed;

  return {
    query,
    executionTime: endTime - startTime,
    memoryDelta: endMemory - startMemory,
    resultCount: Array.isArray(result) ? result.length : result ? 1 : 0,
    success: error === null,
    error: error ? error.message : null,
    result: error ? null : result,
  };
}

/**
 * Compare query performance
 * @param {Array<Object>} stats - Array of query statistics
 * @returns {Object} Performance comparison
 *
 * @example
 * const comparison = compareQueryPerformance([stats1, stats2, stats3]);
 * console.log(comparison.fastest);
 */
export function compareQueryPerformance(stats) {
  if (!Array.isArray(stats) || stats.length === 0) {
    throw new TypeError('stats must be a non-empty array');
  }

  const sorted = [...stats].sort((a, b) => a.executionTime - b.executionTime);

  return {
    fastest: sorted[0],
    slowest: sorted[sorted.length - 1],
    average: stats.reduce((sum, s) => sum + s.executionTime, 0) / stats.length,
    median: sorted[Math.floor(sorted.length / 2)].executionTime,
    totalTime: stats.reduce((sum, s) => sum + s.executionTime, 0),
  };
}
