/**
 * SPARQL Query Cost Estimator
 *
 * Estimates computational cost of SPARQL queries to prevent:
 * - Resource exhaustion attacks
 * - Accidental expensive queries
 * - System overload
 *
 * Rejects queries exceeding 1000 cost points and provides optimization suggestions.
 *
 * @module sidecar/utils/query-cost-estimator
 */

import { trace, context, SpanStatusCode } from '@opentelemetry/api';
import { metrics } from './otel-metrics.mjs';

const tracer = trace.getTracer('sidecar-query-cost');

// Cost configuration
const COST_CONFIG = {
  maxCost: 1000,

  // Base costs for operations
  baseCosts: {
    triple: 1,
    filter: 5,
    optional: 3,
    union: 10,
    join: 8,
    bind: 2,
    service: 20,
    subquery: 15,
    property_path: 10,
  },

  // Multipliers
  multipliers: {
    unboundSubject: 5,     // ?s ?p ?o
    unboundPredicate: 10,  // s ?p o
    regex: 3,
    aggregation: 5,
    orderBy: 2,
    distinct: 3,
    limit: 0.5,            // LIMIT reduces cost
  },

  // Complexity penalties
  penalties: {
    cartesianProduct: 100,
    nestedOptional: 20,
    complexPropertyPath: 50,
  },
};

/**
 * Parse SPARQL query and extract patterns
 * (Simplified parser - in production use a proper SPARQL parser)
 */
function parseQueryPatterns(query) {
  const patterns = {
    triples: 0,
    filters: 0,
    optionals: 0,
    unions: 0,
    subqueries: 0,
    services: 0,
    propertyPaths: 0,
    binds: 0,
    unboundSubjects: 0,
    unboundPredicates: 0,
    hasRegex: false,
    hasAggregation: false,
    hasOrderBy: false,
    hasDistinct: false,
    hasLimit: false,
    limitValue: null,
  };

  const queryUpper = query.toUpperCase();
  const queryLower = query.toLowerCase();

  // Count basic patterns
  patterns.triples = (query.match(/\?[\w]+\s+[^\s]+\s+[^\s.]+\s*[.;]/g) || []).length;
  patterns.filters = (queryUpper.match(/FILTER/g) || []).length;
  patterns.optionals = (queryUpper.match(/OPTIONAL/g) || []).length;
  patterns.unions = (queryUpper.match(/UNION/g) || []).length;
  patterns.subqueries = (query.match(/\{[^}]*SELECT/gi) || []).length;
  patterns.services = (queryUpper.match(/SERVICE/g) || []).length;
  patterns.binds = (queryUpper.match(/BIND/g) || []).length;

  // Property paths
  patterns.propertyPaths = (query.match(/[/^*+?|]/g) || []).length;

  // Unbound patterns (simplified detection)
  const unboundSubjectMatches = query.match(/\?\w+\s+\?\w+\s+\?\w+/g) || [];
  patterns.unboundSubjects = unboundSubjectMatches.length;

  const unboundPredicateMatches = query.match(/<?[^?]\w*>?\s+\?\w+\s+/g) || [];
  patterns.unboundPredicates = unboundPredicateMatches.length;

  // Special features
  patterns.hasRegex = /REGEX\s*\(/i.test(query);
  patterns.hasAggregation = /(COUNT|SUM|AVG|MIN|MAX|GROUP_CONCAT)\s*\(/i.test(query);
  patterns.hasOrderBy = /ORDER\s+BY/i.test(query);
  patterns.hasDistinct = /SELECT\s+DISTINCT/i.test(query);
  patterns.hasLimit = /LIMIT\s+(\d+)/i.test(query);

  if (patterns.hasLimit) {
    const limitMatch = query.match(/LIMIT\s+(\d+)/i);
    patterns.limitValue = limitMatch ? parseInt(limitMatch[1], 10) : null;
  }

  return patterns;
}

/**
 * Estimate query cost based on patterns
 */
export function estimateQueryCost(query) {
  const span = tracer.startSpan('query-cost-estimation');

  try {
    const patterns = parseQueryPatterns(query);
    let cost = 0;

    // Base costs
    cost += patterns.triples * COST_CONFIG.baseCosts.triple;
    cost += patterns.filters * COST_CONFIG.baseCosts.filter;
    cost += patterns.optionals * COST_CONFIG.baseCosts.optional;
    cost += patterns.unions * COST_CONFIG.baseCosts.union;
    cost += patterns.subqueries * COST_CONFIG.baseCosts.subquery;
    cost += patterns.services * COST_CONFIG.baseCosts.service;
    cost += patterns.binds * COST_CONFIG.baseCosts.bind;
    cost += patterns.propertyPaths * COST_CONFIG.baseCosts.property_path;

    // Multipliers for expensive patterns
    if (patterns.unboundSubjects > 0) {
      cost *= COST_CONFIG.multipliers.unboundSubject;
    }
    if (patterns.unboundPredicates > 0) {
      cost *= COST_CONFIG.multipliers.unboundPredicate;
    }
    if (patterns.hasRegex) {
      cost *= COST_CONFIG.multipliers.regex;
    }
    if (patterns.hasAggregation) {
      cost *= COST_CONFIG.multipliers.aggregation;
    }
    if (patterns.hasOrderBy) {
      cost *= COST_CONFIG.multipliers.orderBy;
    }
    if (patterns.hasDistinct) {
      cost *= COST_CONFIG.multipliers.distinct;
    }

    // LIMIT reduces cost
    if (patterns.hasLimit && patterns.limitValue) {
      const limitFactor = Math.min(patterns.limitValue / 100, 1.0);
      cost *= COST_CONFIG.multipliers.limit + (limitFactor * 0.5);
    }

    // Penalties for complex patterns
    if (patterns.unboundSubjects > 2 && patterns.unboundPredicates > 2) {
      cost += COST_CONFIG.penalties.cartesianProduct;
    }
    if (patterns.optionals > 3) {
      cost += COST_CONFIG.penalties.nestedOptional;
    }
    if (patterns.propertyPaths > 5) {
      cost += COST_CONFIG.penalties.complexPropertyPath;
    }

    const finalCost = Math.ceil(cost);
    const exceeded = finalCost > COST_CONFIG.maxCost;

    span.setAttributes({
      'query.cost': finalCost,
      'query.max_cost': COST_CONFIG.maxCost,
      'query.exceeded': exceeded,
      'query.triple_count': patterns.triples,
      'query.has_unbound': patterns.unboundSubjects > 0 || patterns.unboundPredicates > 0,
    });

    // Generate optimization suggestions
    const suggestions = generateOptimizationSuggestions(patterns, finalCost);

    span.setStatus({ code: SpanStatusCode.OK });

    return {
      cost: finalCost,
      maxCost: COST_CONFIG.maxCost,
      exceeded,
      patterns,
      suggestions,
    };

  } catch (err) {
    span.recordException(err);
    span.setStatus({ code: SpanStatusCode.ERROR, message: err.message });

    // On error, assume high cost for safety
    return {
      cost: COST_CONFIG.maxCost + 1,
      maxCost: COST_CONFIG.maxCost,
      exceeded: true,
      error: err.message,
      suggestions: ['Query parsing failed - please verify syntax'],
    };
  } finally {
    span.end();
  }
}

/**
 * Generate optimization suggestions
 */
function generateOptimizationSuggestions(patterns, cost) {
  const suggestions = [];

  // Unbound patterns
  if (patterns.unboundSubjects > 2) {
    suggestions.push('Reduce unbound triple patterns (?s ?p ?o) - bind at least one variable');
  }
  if (patterns.unboundPredicates > 2) {
    suggestions.push('Avoid queries with unbound predicates - specify properties when possible');
  }

  // Add LIMIT if missing
  if (!patterns.hasLimit && cost > 500) {
    suggestions.push('Add LIMIT clause to reduce result set size');
  }

  // Reduce OPTIONAL usage
  if (patterns.optionals > 3) {
    suggestions.push('Consider restructuring query to reduce OPTIONAL clauses');
  }

  // Simplify property paths
  if (patterns.propertyPaths > 5) {
    suggestions.push('Simplify property path expressions or split into multiple queries');
  }

  // Regex optimization
  if (patterns.hasRegex) {
    suggestions.push('Ensure REGEX filters are applied after other filters to reduce search space');
  }

  // Subquery optimization
  if (patterns.subqueries > 2) {
    suggestions.push('Consider flattening subqueries or using VALUES instead');
  }

  // SERVICE calls
  if (patterns.services > 0) {
    suggestions.push('Minimize SERVICE calls and cache results when possible');
  }

  // Union optimization
  if (patterns.unions > 3) {
    suggestions.push('Consider splitting UNION queries into separate requests');
  }

  if (suggestions.length === 0 && cost < COST_CONFIG.maxCost) {
    suggestions.push('Query is well-optimized');
  }

  return suggestions;
}

/**
 * Validate query cost and return rejection if too expensive
 */
export function validateQueryCost(query) {
  const estimation = estimateQueryCost(query);

  // Record metrics
  if (metrics?.queryPerformanceHistogram) {
    metrics.queryPerformanceHistogram.record(estimation.cost, {
      exceeded: estimation.exceeded,
    });
  }

  if (estimation.exceeded) {
    if (metrics?.queryRejectedCounter) {
      metrics.queryRejectedCounter.add(1, { reason: 'cost_exceeded' });
    }

    return {
      allowed: false,
      cost: estimation.cost,
      maxCost: estimation.maxCost,
      message: `Query cost (${estimation.cost}) exceeds maximum allowed (${estimation.maxCost})`,
      suggestions: estimation.suggestions,
    };
  }

  return {
    allowed: true,
    cost: estimation.cost,
    maxCost: estimation.maxCost,
    suggestions: estimation.suggestions,
  };
}

/**
 * Query cost middleware for SPARQL endpoints
 */
export function queryCostMiddleware(req, res, next) {
  const span = tracer.startSpan('query-cost-check');

  try {
    // Extract query from request
    const query = req.body?.query || req.query?.query;

    if (!query) {
      return next(); // No query to validate
    }

    const validation = validateQueryCost(query);

    // Add cost information to response headers
    res.setHeader('X-Query-Cost', validation.cost);
    res.setHeader('X-Query-Max-Cost', validation.maxCost);

    if (!validation.allowed) {
      span.setAttributes({
        'query.cost': validation.cost,
        'query.rejected': true,
      });
      span.setStatus({ code: SpanStatusCode.OK, message: 'Query rejected - cost exceeded' });

      return res.status(400).json({
        error: 'Query Too Complex',
        message: validation.message,
        cost: validation.cost,
        maxCost: validation.maxCost,
        suggestions: validation.suggestions,
      });
    }

    span.setAttributes({
      'query.cost': validation.cost,
      'query.allowed': true,
    });
    span.setStatus({ code: SpanStatusCode.OK });

    // Attach cost info to request for logging
    req.queryCost = validation;

    next();

  } catch (err) {
    console.error('[QueryCost] Error validating query:', err);
    span.recordException(err);
    span.setStatus({ code: SpanStatusCode.ERROR, message: err.message });

    // On error, allow query to proceed (fail open)
    next();
  } finally {
    span.end();
  }
}

export default queryCostMiddleware;
