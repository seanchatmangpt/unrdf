/**
 * @file Dark Matter 80/20 Query Optimizer
 * @module dark-matter/optimizer
 *
 * @description
 * Implements query rewrite rules to optimize SPARQL queries based on
 * 80/20 principles: focus on the most impactful optimizations.
 */

import { z } from 'zod';

/**
 * Schema for optimization result
 */
const OptimizationResultSchema = z.object({
  original: z.string(),
  optimized: z.string(),
  rules: z.array(
    z.object({
      name: z.string(),
      applied: z.boolean(),
      impact: z.enum(['high', 'medium', 'low']),
      description: z.string(),
    })
  ),
  estimatedImprovement: z.object({
    before: z.number(),
    after: z.number(),
    percentageGain: z.number(),
  }),
  timestamp: z.number(),
});

/**
 * Query Optimizer for Dark Matter 80/20
 */
export class DarkMatterOptimizer {
  /**
   * Create a new optimizer
   * @param {Object} [config] - Configuration
   */
  constructor(config = {}) {
    this.config = {
      enableFilterPushdown: config.enableFilterPushdown !== false,
      enableJoinReordering: config.enableJoinReordering !== false,
      enableOptionalOptimization: config.enableOptionalOptimization !== false,
      enableUnionOptimization: config.enableUnionOptimization !== false,
      aggressiveOptimization: config.aggressiveOptimization || false,
      ...config,
    };

    this.rules = this._initializeRules();
    this.stats = {
      totalOptimizations: 0,
      rulesApplied: new Map(),
    };
  }

  /**
   * Initialize optimization rules
   * @returns {Array} Optimization rules
   * @private
   */
  _initializeRules() {
    return [
      {
        name: 'filter-pushdown',
        enabled: this.config.enableFilterPushdown,
        impact: 'high',
        description: 'Push filters down to reduce intermediate results',
        apply: query => this._applyFilterPushdown(query),
      },
      {
        name: 'join-reordering',
        enabled: this.config.enableJoinReordering,
        impact: 'high',
        description: 'Reorder joins to minimize intermediate results',
        apply: query => this._applyJoinReordering(query),
      },
      {
        name: 'optional-to-filter',
        enabled: this.config.enableOptionalOptimization,
        impact: 'medium',
        description: 'Replace OPTIONAL with FILTER EXISTS when possible',
        apply: query => this._applyOptionalOptimization(query),
      },
      {
        name: 'union-optimization',
        enabled: this.config.enableUnionOptimization,
        impact: 'medium',
        description: 'Optimize UNION clauses',
        apply: query => this._applyUnionOptimization(query),
      },
      {
        name: 'limit-early',
        enabled: true,
        impact: 'high',
        description: 'Add LIMIT early for queries without aggregation',
        apply: query => this._applyEarlyLimit(query),
      },
      {
        name: 'index-hints',
        enabled: true,
        impact: 'medium',
        description: 'Add index hints for common patterns',
        apply: query => this._applyIndexHints(query),
      },
    ];
  }

  /**
   * Optimize a SPARQL query
   * @param {string} query - SPARQL query
   * @param {Object} [analysis] - Optional query analysis
   * @returns {Object} Optimization result
   */
  optimize(query, _analysis = null) {
    let optimized = query;
    const appliedRules = [];
    let costBefore = this._estimateCost(query);

    // Apply each enabled rule
    for (const rule of this.rules) {
      if (!rule.enabled) continue;

      try {
        const result = rule.apply(optimized);

        if (result.modified) {
          optimized = result.query;

          appliedRules.push({
            name: rule.name,
            applied: true,
            impact: rule.impact,
            description: rule.description,
          });

          // Update stats
          const count = this.stats.rulesApplied.get(rule.name) || 0;
          this.stats.rulesApplied.set(rule.name, count + 1);
        } else {
          appliedRules.push({
            name: rule.name,
            applied: false,
            impact: rule.impact,
            description: rule.description,
          });
        }
      } catch (error) {
        console.warn(`Rule ${rule.name} failed:`, error.message);
      }
    }

    const costAfter = this._estimateCost(optimized);
    const percentageGain = costBefore > 0 ? ((costBefore - costAfter) / costBefore) * 100 : 0;

    this.stats.totalOptimizations++;

    return OptimizationResultSchema.parse({
      original: query,
      optimized,
      rules: appliedRules,
      estimatedImprovement: {
        before: costBefore,
        after: costAfter,
        percentageGain,
      },
      timestamp: Date.now(),
    });
  }

  /**
   * Apply filter pushdown optimization
   * @param {string} query - Query
   * @returns {Object} Result
   * @private
   */
  _applyFilterPushdown(query) {
    // Extract FILTER clauses
    const filterPattern = /FILTER\s*\(([^)]+)\)/gi;
    const filters = [];
    let match;

    while ((match = filterPattern.exec(query)) !== null) {
      filters.push(match[0]);
    }

    if (filters.length === 0) {
      return { query, modified: false };
    }

    // Simple heuristic: Move filters closer to the patterns they reference
    // In production, this would use a proper query parser
    let optimized = query;
    let modified = false;

    // For now, just ensure filters appear before OPTIONAL clauses
    if (query.includes('OPTIONAL') && filters.length > 0) {
      // Remove all filters
      for (const filter of filters) {
        optimized = optimized.replace(filter, '');
      }

      // Insert filters before OPTIONAL
      optimized = optimized.replace(/OPTIONAL/, `${filters.join('\n  ')}\n  OPTIONAL`);
      modified = true;
    }

    return { query: optimized, modified };
  }

  /**
   * Apply join reordering optimization
   * @param {string} query - Query
   * @returns {Object} Result
   * @private
   */
  _applyJoinReordering(query) {
    // Extract triple patterns
    const whereMatch = query.match(/WHERE\s*\{([^}]+)\}/is);
    if (!whereMatch) {
      return { query, modified: false };
    }

    const whereClause = whereMatch[1];
    const triplePattern = /(\??\w+|<[^>]+>)\s+(\??\w+|<[^>]+>)\s+(\??\w+|<[^>]+>|"[^"]*")\s*\./g;
    const triples = [];
    let match;

    while ((match = triplePattern.exec(whereClause)) !== null) {
      const [fullMatch, subject, predicate, object] = match;

      // Calculate selectivity score (lower = more selective)
      let selectivity = 100;

      // Concrete subject is very selective
      if (!subject.startsWith('?')) selectivity -= 40;

      // Concrete predicate is selective
      if (!predicate.startsWith('?')) selectivity -= 30;

      // Concrete object is selective
      if (!object.startsWith('?')) selectivity -= 20;

      triples.push({
        pattern: fullMatch,
        subject,
        predicate,
        object,
        selectivity,
      });
    }

    if (triples.length <= 1) {
      return { query, modified: false };
    }

    // Sort by selectivity (most selective first)
    const sorted = [...triples].sort((a, b) => a.selectivity - b.selectivity);

    // Check if order changed
    const orderChanged = !sorted.every((t, i) => t.pattern === triples[i].pattern);

    if (!orderChanged) {
      return { query, modified: false };
    }

    // Rebuild WHERE clause with optimized order
    let optimizedWhere = whereClause;

    // Remove all triples
    for (const triple of triples) {
      optimizedWhere = optimizedWhere.replace(triple.pattern, '');
    }

    // Add back in optimized order
    const newTriples = sorted.map(t => `  ${t.pattern}`).join('\n');
    optimizedWhere = newTriples + '\n' + optimizedWhere;

    const optimized = query.replace(whereClause, optimizedWhere);

    return { query: optimized, modified: true };
  }

  /**
   * Apply OPTIONAL optimization
   * @param {string} query - Query
   * @returns {Object} Result
   * @private
   */
  _applyOptionalOptimization(query) {
    if (!query.includes('OPTIONAL')) {
      return { query, modified: false };
    }

    // Simple optimization: If OPTIONAL is used but result is always filtered,
    // replace with FILTER EXISTS
    // This is a conservative optimization that would need query analysis

    return { query, modified: false };
  }

  /**
   * Apply UNION optimization
   * @param {string} query - Query
   * @returns {Object} Result
   * @private
   */
  _applyUnionOptimization(query) {
    if (!query.includes('UNION')) {
      return { query, modified: false };
    }

    // In some cases, UNION can be replaced with property paths or VALUES
    // This is a placeholder for more sophisticated optimization

    return { query, modified: false };
  }

  /**
   * Apply early LIMIT optimization
   * @param {string} query - Query
   * @returns {Object} Result
   * @private
   */
  _applyEarlyLimit(query) {
    // If query has no LIMIT and no aggregation, add a reasonable default
    if (
      query.includes('LIMIT') ||
      query.includes('COUNT') ||
      query.includes('SUM') ||
      query.includes('AVG')
    ) {
      return { query, modified: false };
    }

    // Add LIMIT 1000 as a safety measure
    const optimized = query.trim() + '\nLIMIT 1000';

    return { query: optimized, modified: true };
  }

  /**
   * Apply index hints
   * @param {string} query - Query
   * @returns {Object} Result
   * @private
   */
  _applyIndexHints(query) {
    // Add comments with index hints for query engines that support them
    // This is database-specific and would need customization

    return { query, modified: false };
  }

  /**
   * Estimate query cost
   * @param {string} query - Query
   * @returns {number} Estimated cost
   * @private
   */
  _estimateCost(query) {
    let cost = 10; // Base cost

    // Count patterns
    const patterns = (query.match(/\?[a-zA-Z0-9]+\s+/g) || []).length;
    cost += patterns * 5;

    // FILTER increases cost
    const filters = (query.match(/FILTER/gi) || []).length;
    cost += filters * 10;

    // OPTIONAL increases cost significantly
    const optionals = (query.match(/OPTIONAL/gi) || []).length;
    cost += optionals * 20;

    // UNION is very expensive
    const unions = (query.match(/UNION/gi) || []).length;
    cost += unions * 30;

    // Aggregations are expensive
    const aggregations = (query.match(/(COUNT|SUM|AVG|MIN|MAX)/gi) || []).length;
    cost += aggregations * 15;

    // ORDER BY adds cost
    if (query.includes('ORDER BY')) cost += 10;

    // DISTINCT adds cost
    if (query.includes('DISTINCT')) cost += 8;

    return cost;
  }

  /**
   * Get optimizer statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      totalOptimizations: this.stats.totalOptimizations,
      rulesApplied: Object.fromEntries(this.stats.rulesApplied),
    };
  }

  /**
   * Reset statistics
   */
  resetStats() {
    this.stats = {
      totalOptimizations: 0,
      rulesApplied: new Map(),
    };
  }
}

/**
 * Create an optimizer instance
 * @param {Object} [config] - Configuration
 * @returns {DarkMatterOptimizer} Optimizer
 */
export function createDarkMatterOptimizer(config = {}) {
  return new DarkMatterOptimizer(config);
}

export default DarkMatterOptimizer;
