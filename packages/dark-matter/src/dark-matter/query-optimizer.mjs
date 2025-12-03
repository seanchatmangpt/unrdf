/**
 * @file SPARQL Query Optimizer - Reorder patterns for efficiency
 * @module @unrdf/dark-matter/query-optimizer
 */

import { z } from 'zod';
import { analyzeSparqlQuery } from './query-analyzer.mjs';

/**
 * @typedef {import('n3').Store} Store
 */

/**
 * Optimization result schema
 */
const OptimizationResultSchema = z.object({
  originalQuery: z.string(),
  optimizedQuery: z.string(),
  changes: z.array(
    z.object({
      type: z.string(),
      description: z.string(),
    })
  ),
  estimatedSpeedup: z.number(),
});

/**
 * Optimize SPARQL query by reordering patterns
 * @param {string} query - Original SPARQL query
 * @param {Store} [store] - Optional store for statistics
 * @returns {Object} Optimization result
 *
 * @throws {TypeError} If query is not a string
 *
 * @example
 * const result = optimizeQuery(`
 *   SELECT ?name WHERE {
 *     ?person foaf:knows ?friend .
 *     ?person foaf:name ?name .
 *   }
 * `);
 *
 * console.log('Optimized:', result.optimizedQuery);
 * console.log('Speedup:', result.estimatedSpeedup);
 */
export function optimizeQuery(query, _store = null) {
  if (typeof query !== 'string') {
    throw new TypeError('optimizeQuery: query must be a string');
  }

  const analysis = analyzeSparqlQuery(query);
  const changes = [];

  // Strategy: Move most selective patterns first (filter early)
  const optimizedPatterns = optimizePatternOrder(analysis.patterns, changes);

  // Rebuild query with optimized pattern order
  const optimizedQuery = rebuildQuery(query, analysis, optimizedPatterns);

  // Estimate speedup
  const estimatedSpeedup = calculateSpeedup(analysis.patterns, optimizedPatterns);

  const result = {
    originalQuery: query,
    optimizedQuery,
    changes,
    estimatedSpeedup,
  };

  return OptimizationResultSchema.parse(result);
}

/**
 * Optimize pattern execution order
 * @param {Array} patterns - Original patterns
 * @param {Array} changes - Changes accumulator
 * @returns {Array} Optimized patterns
 */
function optimizePatternOrder(patterns, changes) {
  if (patterns.length <= 1) {
    return patterns;
  }

  // Sort by selectivity (descending) - execute most selective first
  const sorted = [...patterns].sort((a, b) => b.selectivity - a.selectivity);

  // Check if order changed
  let orderChanged = false;
  for (let i = 0; i < patterns.length; i++) {
    if (patterns[i] !== sorted[i]) {
      orderChanged = true;
      break;
    }
  }

  if (orderChanged) {
    changes.push({
      type: 'pattern_reorder',
      description: 'Reordered patterns by selectivity (most selective first)',
    });
  }

  return sorted;
}

/**
 * Rebuild query with optimized patterns
 * @param {string} originalQuery - Original query
 * @param {Object} analysis - Query analysis
 * @param {Array} optimizedPatterns - Optimized patterns
 * @returns {string} Rebuilt query
 */
function rebuildQuery(originalQuery, analysis, optimizedPatterns) {
  // Extract query parts
  const whereMatch = originalQuery.match(/(.*WHERE\s*\{)([^}]+)(\}.*)/is);

  if (!whereMatch) {
    return originalQuery; // Can't optimize without WHERE clause
  }

  const [, before, , after] = whereMatch;

  // Rebuild WHERE clause with optimized pattern order
  const patternStrings = optimizedPatterns.map(p => `${p.subject} ${p.predicate} ${p.object}`);
  const newWhereClause = patternStrings.join(' .\n    ');

  return `${before}\n    ${newWhereClause}\n  ${after}`;
}

/**
 * Calculate estimated speedup from optimization
 * @param {Array} original - Original patterns
 * @param {Array} optimized - Optimized patterns
 * @returns {number} Estimated speedup multiplier
 */
function calculateSpeedup(original, optimized) {
  // Speedup estimate based on selectivity improvement
  // Early filtering reduces intermediate result size

  let originalCost = 0;
  let optimizedCost = 0;
  let intermediateSize = 1000; // Base estimate

  // Original order cost
  for (const pattern of original) {
    originalCost += intermediateSize;
    intermediateSize *= 1 - pattern.selectivity; // Results after this pattern
  }

  // Reset for optimized
  intermediateSize = 1000;

  // Optimized order cost
  for (const pattern of optimized) {
    optimizedCost += intermediateSize;
    intermediateSize *= 1 - pattern.selectivity;
  }

  const speedup = originalCost > 0 ? originalCost / Math.max(optimizedCost, 1) : 1.0;
  return Math.round(speedup * 100) / 100;
}

/**
 * Suggest indexes for store
 * @param {Store} store - RDF store
 * @param {string} query - SPARQL query
 * @returns {Array<Object>} Index suggestions
 *
 * @throws {TypeError} If store or query is invalid
 *
 * @example
 * const suggestions = suggestIndexes(store, query);
 * suggestions.forEach(s => console.log(s.type, s.priority));
 */
export function suggestIndexes(store, query) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('suggestIndexes: store must be a valid Store instance');
  }

  if (typeof query !== 'string') {
    throw new TypeError('suggestIndexes: query must be a string');
  }

  const analysis = analyzeSparqlQuery(query);
  const suggestions = [];

  // Track unique predicates to avoid duplicates
  const uniquePredicates = new Set();

  // Analyze patterns for index opportunities
  for (const pattern of analysis.patterns) {
    // Predicate-specific indexes
    const cleanPredicate = pattern.predicate.trim();
    if (!cleanPredicate.startsWith('?') && cleanPredicate.length > 0) {
      if (!uniquePredicates.has(cleanPredicate)) {
        uniquePredicates.add(cleanPredicate);
        suggestions.push({
          type: 'predicate_index',
          predicate: cleanPredicate,
          priority: 'high',
          reason: 'Specific predicate queries benefit from indexes',
        });
      }
    }

    // Subject-predicate composite indexes for bound subjects
    if (!pattern.subject.startsWith('?') && !cleanPredicate.startsWith('?')) {
      suggestions.push({
        type: 'subject_predicate_index',
        subject: pattern.subject,
        predicate: cleanPredicate,
        priority: 'medium',
        reason: 'Bound subject+predicate can use composite index',
      });
    }
  }

  // Join-based suggestions
  for (const join of analysis.joins) {
    if (join.patterns.length > 2) {
      suggestions.push({
        type: 'join_index',
        variable: join.variable,
        priority: 'medium',
        reason: `Variable ${join.variable} joins ${join.patterns.length} patterns`,
      });
    }
  }

  return suggestions;
}

/**
 * Explain optimization changes
 * @param {string} original - Original query
 * @param {string} optimized - Optimized query
 * @returns {Object} Explanation
 *
 * @example
 * const explanation = explainOptimization(original, optimized);
 * console.log(explanation.summary);
 */
export function explainOptimization(original, optimized) {
  if (typeof original !== 'string' || typeof optimized !== 'string') {
    throw new TypeError('explainOptimization: queries must be strings');
  }

  const originalAnalysis = analyzeSparqlQuery(original);
  const optimizedAnalysis = analyzeSparqlQuery(optimized);

  const explanation = {
    summary: 'Query optimization analysis',
    originalComplexity: originalAnalysis.patterns.length,
    optimizedComplexity: optimizedAnalysis.patterns.length,
    changes: [],
  };

  // Compare pattern order
  let patternOrderChanged = false;
  for (
    let i = 0;
    i < Math.min(originalAnalysis.patterns.length, optimizedAnalysis.patterns.length);
    i++
  ) {
    if (
      JSON.stringify(originalAnalysis.patterns[i]) !== JSON.stringify(optimizedAnalysis.patterns[i])
    ) {
      patternOrderChanged = true;
      break;
    }
  }

  if (patternOrderChanged) {
    explanation.changes.push({
      type: 'pattern_order',
      description: 'Patterns reordered for better selectivity',
      impact: 'Reduces intermediate result set size',
    });
  }

  return explanation;
}
