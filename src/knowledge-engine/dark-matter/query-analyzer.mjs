/**
 * @file Dark Matter 80/20 Query Analyzer
 * @module dark-matter/query-analyzer
 *
 * @description
 * Analyzes SPARQL queries to extract patterns, calculate complexity scores,
 * and identify expensive operations for 80/20 optimization.
 */

import { analyzeSPARQLQuery, extractVariables } from '../../utils/sparql-utils.mjs';
import { z } from 'zod';

/**
 * Schema for query analysis result
 */
const QueryAnalysisSchema = z.object({
  queryId: z.string(),
  query: z.string(),
  type: z.enum(['SELECT', 'ASK', 'CONSTRUCT', 'DESCRIBE', 'UNKNOWN']),
  patterns: z.array(
    z.object({
      type: z.string(),
      subject: z.string().optional(),
      predicate: z.string().optional(),
      object: z.string().optional(),
      complexity: z.number(),
    })
  ),
  filters: z.array(z.string()),
  joins: z.array(
    z.object({
      type: z.string(),
      variables: z.array(z.string()),
      estimatedCost: z.number(),
    })
  ),
  aggregations: z.array(z.string()),
  complexity: z.object({
    score: z.number(),
    patternCount: z.number(),
    filterCount: z.number(),
    joinCount: z.number(),
    aggregationCount: z.number(),
    variableCount: z.number(),
    estimatedRows: z.number(),
  }),
  expensiveOperations: z.array(
    z.object({
      type: z.string(),
      cost: z.number(),
      reason: z.string(),
    })
  ),
  timestamp: z.number(),
});

/**
 * Query Analyzer for Dark Matter 80/20 optimization
 */
export class QueryAnalyzer {
  /**
   * Create a new query analyzer
   * @param {Object} [config] - Configuration
   */
  constructor(config = {}) {
    this.config = {
      complexityThreshold: config.complexityThreshold || 100,
      expensiveOperationThreshold: config.expensiveOperationThreshold || 50,
      joinCostMultiplier: config.joinCostMultiplier || 10,
      filterCostMultiplier: config.filterCostMultiplier || 5,
      aggregationCostMultiplier: config.aggregationCostMultiplier || 8,
      ...config,
    };

    this.stats = {
      totalAnalyzed: 0,
      complexQueries: 0,
      simpleQueries: 0,
      avgComplexity: 0,
    };
  }

  /**
   * Analyze a SPARQL query
   * @param {string} query - SPARQL query string
   * @param {string} [queryId] - Optional query identifier
   * @param {Object} [metadata] - Optional metadata
   * @returns {Object} Query analysis result
   */
  analyze(query, queryId = null, metadata = {}) {
    const analysis = analyzeSPARQLQuery(query);
    const patterns = this._extractPatterns(query, analysis);
    const filters = this._extractFilters(query, analysis);
    const joins = this._identifyJoins(query, analysis);
    const aggregations = this._extractAggregations(query);

    // Calculate complexity score
    const complexity = this._calculateComplexity(patterns, filters, joins, aggregations, analysis);

    // Identify expensive operations
    const expensiveOperations = this._identifyExpensiveOperations(
      patterns,
      filters,
      joins,
      aggregations,
      complexity
    );

    const result = {
      queryId: queryId || `query-${Date.now()}`,
      query,
      type: analysis.type,
      patterns,
      filters,
      joins,
      aggregations,
      complexity,
      expensiveOperations,
      timestamp: Date.now(),
      metadata,
    };

    // Update stats
    this.stats.totalAnalyzed++;
    if (complexity.score >= this.config.complexityThreshold) {
      this.stats.complexQueries++;
    } else {
      this.stats.simpleQueries++;
    }

    // Update average complexity
    this.stats.avgComplexity =
      (this.stats.avgComplexity * (this.stats.totalAnalyzed - 1) + complexity.score) /
      this.stats.totalAnalyzed;

    return QueryAnalysisSchema.parse(result);
  }

  /**
   * Extract triple patterns from query
   * @param {string} query - SPARQL query
   * @param {Object} analysis - Basic analysis
   * @returns {Array} Triple patterns
   * @private
   */
  _extractPatterns(query, _analysis) {
    const patterns = [];

    // Extract WHERE clause
    const whereMatch = query.match(/WHERE\s*\{([^}]+)\}/is);
    if (!whereMatch) return patterns;

    const whereClause = whereMatch[1];

    // Simple pattern extraction (s p o .)
    const triplePattern = /(\??\w+|<[^>]+>)\s+(\??\w+|<[^>]+>)\s+(\??\w+|<[^>]+>|"[^"]*")\s*\./g;
    let match;

    while ((match = triplePattern.exec(whereClause)) !== null) {
      const [, subject, predicate, object] = match;

      // Calculate pattern complexity
      let complexity = 1;

      // Variable in subject position = join likely
      if (subject.startsWith('?')) complexity += 5;

      // Variable in predicate position = very expensive
      if (predicate.startsWith('?')) complexity += 10;

      // Variable in object position = filter likely
      if (object.startsWith('?')) complexity += 3;

      patterns.push({
        type: 'triple',
        subject,
        predicate,
        object,
        complexity,
      });
    }

    return patterns;
  }

  /**
   * Extract FILTER clauses
   * @param {string} query - SPARQL query
   * @param {Object} analysis - Basic analysis
   * @returns {Array} Filters
   * @private
   */
  _extractFilters(query, _analysis) {
    const filters = [];
    const filterPattern = /FILTER\s*\(([^)]+)\)/gi;
    let match;

    while ((match = filterPattern.exec(query)) !== null) {
      filters.push(match[1].trim());
    }

    return filters;
  }

  /**
   * Identify JOIN operations
   * @param {string} query - SPARQL query
   * @param {Object} analysis - Basic analysis
   * @returns {Array} Joins
   * @private
   */
  _identifyJoins(query, _analysis) {
    const joins = [];
    const variables = extractVariables(query);

    // Simple heuristic: if a variable appears multiple times, it's a join
    const variableCounts = new Map();

    for (const variable of variables) {
      const regex = new RegExp(`\\?${variable}`, 'g');
      const matches = query.match(regex);
      variableCounts.set(variable, matches ? matches.length : 0);
    }

    // Identify joins based on shared variables
    for (const [variable, count] of variableCounts.entries()) {
      if (count >= 2) {
        // Estimate join cost based on number of occurrences
        const estimatedCost = count * this.config.joinCostMultiplier;

        joins.push({
          type: 'variable-join',
          variables: [variable],
          estimatedCost,
        });
      }
    }

    // Detect OPTIONAL joins (left outer joins)
    if (query.includes('OPTIONAL')) {
      joins.push({
        type: 'optional-join',
        variables: [],
        estimatedCost: 20, // OPTIONAL is typically expensive
      });
    }

    // Detect UNION (union joins)
    if (query.includes('UNION')) {
      joins.push({
        type: 'union',
        variables: [],
        estimatedCost: 30, // UNION is very expensive
      });
    }

    return joins;
  }

  /**
   * Extract aggregation operations
   * @param {string} query - SPARQL query
   * @returns {Array} Aggregations
   * @private
   */
  _extractAggregations(query) {
    const aggregations = [];
    const aggPattern = /(COUNT|SUM|AVG|MIN|MAX|GROUP_CONCAT|SAMPLE)\s*\(/gi;
    let match;

    while ((match = aggPattern.exec(query)) !== null) {
      aggregations.push(match[1].toUpperCase());
    }

    return aggregations;
  }

  /**
   * Calculate query complexity score
   * @param {Array} patterns - Triple patterns
   * @param {Array} filters - Filters
   * @param {Array} joins - Joins
   * @param {Array} aggregations - Aggregations
   * @param {Object} analysis - Basic analysis
   * @returns {Object} Complexity metrics
   * @private
   */
  _calculateComplexity(patterns, filters, joins, aggregations, analysis) {
    let score = 0;

    // Base cost from patterns
    const patternCost = patterns.reduce((sum, p) => sum + p.complexity, 0);
    score += patternCost;

    // Filter cost
    const filterCost = filters.length * this.config.filterCostMultiplier;
    score += filterCost;

    // Join cost
    const joinCost = joins.reduce((sum, j) => sum + j.estimatedCost, 0);
    score += joinCost;

    // Aggregation cost
    const aggregationCost = aggregations.length * this.config.aggregationCostMultiplier;
    score += aggregationCost;

    // Complexity modifiers
    if (analysis.hasGroupBy) score *= 1.5;
    if (analysis.hasOrderBy) score *= 1.2;
    if (analysis.hasDistinct) score *= 1.3;

    // Estimate result rows (used for optimization decisions)
    let estimatedRows = 100; // Base estimate

    if (joins.length > 0) {
      estimatedRows *= Math.pow(10, joins.length); // Cartesian product estimation
    }

    if (filters.length > 0) {
      estimatedRows /= filters.length * 2; // Filters reduce results
    }

    return {
      score: Math.round(score),
      patternCount: patterns.length,
      filterCount: filters.length,
      joinCount: joins.length,
      aggregationCount: aggregations.length,
      variableCount: analysis.variables.length,
      estimatedRows: Math.round(estimatedRows),
    };
  }

  /**
   * Identify expensive operations
   * @param {Array} patterns - Triple patterns
   * @param {Array} filters - Filters
   * @param {Array} joins - Joins
   * @param {Array} aggregations - Aggregations
   * @param {Object} complexity - Complexity metrics
   * @returns {Array} Expensive operations
   * @private
   */
  _identifyExpensiveOperations(patterns, filters, joins, aggregations, complexity) {
    const expensive = [];

    // Check for expensive patterns
    for (const pattern of patterns) {
      if (pattern.complexity >= this.config.expensiveOperationThreshold / 5) {
        expensive.push({
          type: 'pattern',
          cost: pattern.complexity,
          reason: `High complexity pattern: ${pattern.subject} ${pattern.predicate} ${pattern.object}`,
        });
      }

      // Variable predicates are very expensive
      if (pattern.predicate.startsWith('?')) {
        expensive.push({
          type: 'variable-predicate',
          cost: 100,
          reason: `Variable predicate ${pattern.predicate} requires full graph scan`,
        });
      }
    }

    // Check for expensive joins
    for (const join of joins) {
      if (join.estimatedCost >= this.config.expensiveOperationThreshold) {
        expensive.push({
          type: 'join',
          cost: join.estimatedCost,
          reason: `${join.type} with cost ${join.estimatedCost}`,
        });
      }
    }

    // UNION is always expensive
    if (joins.some(j => j.type === 'union')) {
      expensive.push({
        type: 'union',
        cost: 50,
        reason: 'UNION requires multiple query executions',
      });
    }

    // Aggregations without GROUP BY are expensive on large datasets
    if (aggregations.length > 0 && complexity.estimatedRows > 1000) {
      expensive.push({
        type: 'aggregation',
        cost: aggregations.length * this.config.aggregationCostMultiplier,
        reason: `${aggregations.length} aggregation(s) on ~${complexity.estimatedRows} rows`,
      });
    }

    // Unfiltered queries with high estimated rows
    if (filters.length === 0 && complexity.estimatedRows > 10000) {
      expensive.push({
        type: 'unfiltered',
        cost: 75,
        reason: `No filters with estimated ${complexity.estimatedRows} rows`,
      });
    }

    return expensive.sort((a, b) => b.cost - a.cost);
  }

  /**
   * Get analyzer statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      complexQueryRatio:
        this.stats.totalAnalyzed > 0 ? this.stats.complexQueries / this.stats.totalAnalyzed : 0,
    };
  }

  /**
   * Reset statistics
   */
  resetStats() {
    this.stats = {
      totalAnalyzed: 0,
      complexQueries: 0,
      simpleQueries: 0,
      avgComplexity: 0,
    };
  }
}

/**
 * Create a query analyzer instance
 * @param {Object} [config] - Configuration
 * @returns {QueryAnalyzer} Query analyzer
 */
export function createQueryAnalyzer(config = {}) {
  return new QueryAnalyzer(config);
}

export default QueryAnalyzer;
