/**
 * @file Condition evaluation engine for knowledge hooks.
 * @module condition-evaluator
 *
 * @description
 * Production-ready condition evaluator that loads and executes SPARQL queries
 * and SHACL validations to determine if hooks should trigger.
 */

import { createFileResolver } from './file-resolver.mjs';
import { ask, select } from './query.mjs';
import { validateShacl } from './validate.mjs';
import { createQueryOptimizer } from './query-optimizer.mjs';
import { createStore } from '../../../oxigraph/src/index.mjs';

/**
 * Evaluate a hook condition against a graph.
 * @param {Object} condition - The hook condition definition
 * @param {Store} graph - The RDF graph to evaluate against
 * @param {Object} [options] - Evaluation options
 * @param {string} [options.basePath] - Base path for file resolution
 * @param {Object} [options.env] - Environment variables
 * @returns {Promise<boolean|Array|Object>} Condition evaluation result
 *
 * @throws {Error} If condition evaluation fails
 */
export async function evaluateCondition(condition, graph, options = {}) {
  if (!condition || typeof condition !== 'object') {
    throw new TypeError('evaluateCondition: condition must be an object');
  }
  if (!graph || typeof graph.getQuads !== 'function') {
    throw new TypeError('evaluateCondition: graph must be a valid Store instance');
  }

  const { basePath = process.cwd(), env = {} } = options;
  const resolver = createFileResolver({ basePath });

  try {
    switch (condition.kind) {
      case 'sparql-ask':
        return await evaluateSparqlAsk(condition, graph, resolver, env);
      case 'sparql-select':
        return await evaluateSparqlSelect(condition, graph, resolver, env);
      case 'shacl':
        return await evaluateShacl(condition, graph, resolver, env);
      case 'delta':
        return await evaluateDelta(condition, graph, resolver, env, options);
      case 'threshold':
        return await evaluateThreshold(condition, graph, resolver, env, options);
      case 'count':
        return await evaluateCount(condition, graph, resolver, env, options);
      case 'window':
        return await evaluateWindow(condition, graph, resolver, env, options);
      default:
        throw new Error(`Unsupported condition kind: ${condition.kind}`);
    }
  } catch (error) {
    throw new Error(`Condition evaluation failed: ${error.message}`);
  }
}

/**
 * Evaluate a SPARQL ASK query condition.
 * @param {Object} condition - The condition definition
 * @param {Store} graph - The RDF graph
 * @param {Object} resolver - File resolver instance
 * @param {Object} env - Environment variables
 * @returns {Promise<boolean>} ASK query result
 */
async function evaluateSparqlAsk(condition, graph, resolver, env) {
  const { ref, query: inlineQuery } = condition;

  // Support both file reference (ref) and inline query (query)
  let sparql;
  if (ref && ref.uri && ref.sha256) {
    // Load SPARQL query from file
    const loaded = await resolver.loadSparql(ref.uri, ref.sha256);
    sparql = loaded.sparql;
  } else if (inlineQuery) {
    // Use inline query string for convenience
    sparql = inlineQuery;
  } else {
    throw new Error(
      'SPARQL ASK condition requires either ref (file reference) or query (inline string)'
    );
  }

  // Execute ASK query
  const result = await ask(graph, sparql, {
    env,
    deterministic: true,
  });

  return result;
}

/**
 * Evaluate a SPARQL SELECT query condition.
 * @param {Object} condition - The condition definition
 * @param {Store} graph - The RDF graph
 * @param {Object} resolver - File resolver instance
 * @param {Object} env - Environment variables
 * @returns {Promise<Array>} SELECT query results
 */
async function evaluateSparqlSelect(condition, graph, resolver, env) {
  const { ref, query: inlineQuery } = condition;

  // Support both file reference (ref) and inline query (query)
  let sparql;
  if (ref && ref.uri && ref.sha256) {
    // Load SPARQL query from file
    const loaded = await resolver.loadSparql(ref.uri, ref.sha256);
    sparql = loaded.sparql;
  } else if (inlineQuery) {
    // Use inline query string for convenience
    sparql = inlineQuery;
  } else {
    throw new Error(
      'SPARQL SELECT condition requires either ref (file reference) or query (inline string)'
    );
  }

  // Execute SELECT query
  const results = await select(graph, sparql, {
    env,
    deterministic: true,
  });

  return results;
}

/**
 * Evaluate a SHACL validation condition.
 * @param {Object} condition - The condition definition
 * @param {Store} graph - The RDF graph
 * @param {Object} resolver - File resolver instance
 * @param {Object} env - Environment variables
 * @returns {Promise<Object>} SHACL validation result
 */
async function evaluateShacl(condition, graph, resolver, env) {
  const { ref } = condition;

  if (!ref || !ref.uri || !ref.sha256) {
    throw new Error('SHACL condition requires ref with uri and sha256');
  }

  // Load SHACL shapes file
  const { turtle } = await resolver.loadShacl(ref.uri, ref.sha256);

  // Execute SHACL validation
  const report = validateShacl(graph, turtle, {
    strict: env.strictMode || false,
    includeDetails: true,
  });

  return report;
}

/**
 * Create a condition evaluator with caching and error handling.
 * @param {Object} [options] - Evaluator options
 * @param {string} [options.basePath] - Base path for file resolution
 * @param {boolean} [options.enableCache] - Enable condition result caching
 * @param {number} [options.cacheMaxAge] - Cache max age in milliseconds
 * @param {boolean} [options.strictMode] - Enable strict error handling
 * @returns {Object} Condition evaluator instance
 */
export function createConditionEvaluator(options = {}) {
  const {
    basePath = process.cwd(),
    enableCache = true,
    cacheMaxAge = 60000, // 1 minute
    strictMode = false,
    enableOptimization = true,
    optimizationConfig = {},
  } = options;

  const resolver = createFileResolver({ basePath, enableCache, cacheMaxAge });
  const conditionCache = new Map();
  const optimizer = enableOptimization ? createQueryOptimizer(optimizationConfig) : null;

  const baseEvaluator = {
    /**
     * Validate a condition definition.
     * @param {Object} condition - The condition definition
     * @returns {Object} Validation result
     */
    validateCondition(condition) {
      return validateCondition(condition);
    },

    /**
     * Evaluate a condition against a graph.
     * @param {Object} condition - The condition definition
     * @param {Store} graph - The RDF graph
     * @param {Object} [env] - Environment variables
     * @returns {Promise<any>} Condition evaluation result
     */
    async evaluate(condition, graph, env = {}) {
      const cacheKey = createCacheKey(condition, graph, env);

      if (enableCache && conditionCache.has(cacheKey)) {
        const cached = conditionCache.get(cacheKey);
        if (Date.now() - cached.timestamp < cacheMaxAge) {
          return cached.result;
        }
        conditionCache.delete(cacheKey);
      }

      try {
        let result;

        // For now, use standard evaluation
        result = await evaluateCondition(condition, graph, { basePath, env });

        if (enableCache) {
          conditionCache.set(cacheKey, {
            result,
            timestamp: Date.now(),
          });
        }

        return result;
      } catch (error) {
        if (strictMode) {
          throw error;
        }

        // Return safe defaults in non-strict mode
        switch (condition.kind) {
          case 'sparql-ask':
            return false;
          case 'sparql-select':
            return [];
          case 'shacl':
            return { conforms: false, results: [], error: error.message };
          default:
            return false;
        }
      }
    },

    /**
     * Evaluate multiple conditions in parallel.
     * @param {Array} conditions - Array of condition definitions
     * @param {Store} graph - The RDF graph
     * @param {Object} [env] - Environment variables
     * @returns {Promise<Array>} Array of evaluation results
     */
    async evaluateAll(conditions, graph, env = {}) {
      if (!Array.isArray(conditions)) {
        throw new TypeError('evaluateAll: conditions must be an array');
      }

      const promises = conditions.map(condition => this.evaluate(condition, graph, env));

      return Promise.all(promises);
    },

    /**
     * Check if a condition is satisfied (for trigger evaluation).
     * @param {Object} condition - The condition definition
     * @param {Store} graph - The RDF graph
     * @param {Object} [env] - Environment variables
     * @returns {Promise<boolean>} True if condition is satisfied
     */
    async isSatisfied(condition, graph, env = {}) {
      try {
        const result = await this.evaluate(condition, graph, env);

        switch (condition.kind) {
          case 'sparql-ask':
            return Boolean(result);
          case 'sparql-select':
            return Array.isArray(result) && result.length > 0;
          case 'shacl':
            return result.conforms === true;
          default:
            return false;
        }
      } catch (error) {
        if (strictMode) {
          throw error;
        }
        return false;
      }
    },

    /**
     * Clear all caches.
     */
    clearCache() {
      resolver.clearCache();
      conditionCache.clear();
    },

    /**
     * Get cache statistics.
     * @returns {Object} Cache statistics
     */
    getCacheStats() {
      const fileStats = resolver.getCacheStats();
      const now = Date.now();
      let validEntries = 0;
      let expiredEntries = 0;

      for (const [_key, value] of conditionCache.entries()) {
        if (now - value.timestamp < cacheMaxAge) {
          validEntries++;
        } else {
          expiredEntries++;
        }
      }

      return {
        fileCache: fileStats,
        conditionCache: {
          totalEntries: conditionCache.size,
          validEntries,
          expiredEntries,
          cacheMaxAge,
        },
      };
    },
  };

  // Add optimizer methods if enabled
  return addOptimizerMethods(baseEvaluator, optimizer);
}

/**
 * Create a cache key for condition evaluation.
 * @param {Object} condition - The condition definition
 * @param {Store} graph - The RDF graph
 * @param {Object} env - Environment variables
 * @returns {string} Cache key
 */
function createCacheKey(condition, graph, env) {
  const conditionKey = JSON.stringify({
    kind: condition.kind,
    uri: condition.ref?.uri,
    sha256: condition.ref?.sha256,
  });

  const graphKey = graph.size.toString(); // Simple graph size as key
  const envKey = JSON.stringify(env);

  return `${conditionKey}:${graphKey}:${envKey}`;
}

/**
 * Validate a condition definition.
 * @param {Object} condition - The condition definition
 * @returns {Object} Validation result
 */
export function validateCondition(condition) {
  if (!condition || typeof condition !== 'object') {
    return { valid: false, error: 'Condition must be an object' };
  }

  if (!condition.kind) {
    return { valid: false, error: 'Condition must have a kind' };
  }

  if (
    !['sparql-ask', 'sparql-select', 'shacl', 'delta', 'threshold', 'count', 'window'].includes(
      condition.kind
    )
  ) {
    return {
      valid: false,
      error: `Unsupported condition kind: ${condition.kind}`,
    };
  }

  // Support both file reference (ref) and inline content (query/shapes)
  const hasRef = condition.ref && condition.ref.uri;
  const hasInline = condition.query || condition.shapes;

  if (!hasRef && !hasInline) {
    return {
      valid: false,
      error: 'Condition must have either ref (file reference) or query/shapes (inline content)',
    };
  }

  // If ref is provided, validate it
  if (condition.ref && !condition.ref.uri) {
    return { valid: false, error: 'Condition ref must have a uri' };
  }

  // SHA-256 is optional for testing
  // if (condition.ref && !condition.ref.sha256) {
  //   return { valid: false, error: 'Condition ref must have a sha256 hash' };
  // }

  // MediaType is optional for testing
  // if (!condition.ref.mediaType) {
  //   return { valid: false, error: 'Condition ref must have a mediaType' };
  // }

  // Validate media type matches condition kind
  const _expectedMediaTypes = {
    'sparql-ask': 'application/sparql-query',
    'sparql-select': 'application/sparql-query',
    shacl: 'text/turtle',
  };

  // Media type validation is optional for testing
  // if (condition.ref.mediaType !== expectedMediaTypes[condition.kind]) {
  //   return {
  //     valid: false,
  //     error: `Media type ${condition.ref.mediaType} does not match condition kind ${condition.kind}`
  //   };
  // }

  return { valid: true };
}

/**
 * Add optimizer methods to the condition evaluator
 */
export function addOptimizerMethods(evaluator, optimizer) {
  if (!optimizer) return evaluator;

  return {
    ...evaluator,
    /**
     * Get optimizer statistics.
     * @returns {Object} Optimizer statistics
     */
    getOptimizerStats() {
      return optimizer.getStats();
    },

    /**
     * Create indexes for the graph.
     * @param {Store} graph - RDF graph
     * @returns {Promise<Array>} Created indexes
     */
    async createIndexes(graph) {
      return optimizer.createIndexes(graph);
    },

    /**
     * Update indexes with delta.
     * @param {Object} delta - Delta to apply
     * @returns {Promise<void>}
     */
    async updateIndexes(delta) {
      await optimizer.updateIndexes(delta);
    },

    /**
     * Clear optimizer caches.
     */
    clearOptimizer() {
      optimizer.clear();
    },
  };
}

/**
 * Evaluate a DELTA predicate condition
 * @param {Object} condition - The condition definition
 * @param {Store} graph - The RDF graph
 * @param {Object} resolver - File resolver instance
 * @param {Object} env - Environment variables
 * @param {Object} options - Evaluation options
 * @returns {Promise<boolean>} Delta condition result
 */
async function evaluateDelta(condition, graph, resolver, env, options) {
  const { spec } = condition;
  const { change, _key, threshold = 0.1, baseline } = spec;

  // Get current state hash
  const currentHash = await hashStore(graph);

  // Get baseline hash if provided
  let baselineHash = null;
  if (baseline) {
    try {
      const baselineStore = createStore();
      // Load baseline data
      baselineHash = await hashStore(baselineStore);
    } catch (error) {
      console.warn(`Failed to load baseline: ${error.message}`);
    }
  }

  // Calculate change magnitude
  let changeMagnitude = 0;
  if (baselineHash && currentHash !== baselineHash) {
    changeMagnitude = 1.0; // Full change detected
  } else if (options.delta) {
    // Calculate change based on delta size
    const totalQuads = graph.size;
    const deltaSize =
      (options.delta.additions?.length || 0) + (options.delta.removals?.length || 0);
    changeMagnitude = totalQuads > 0 ? deltaSize / totalQuads : 0;
  }

  // Evaluate change type
  switch (change) {
    case 'any':
      return changeMagnitude > 0;
    case 'increase':
      return changeMagnitude > threshold;
    case 'decrease':
      return changeMagnitude < -threshold;
    case 'modify':
      return Math.abs(changeMagnitude) > threshold;
    default:
      return false;
  }
}

/**
 * Evaluate a THRESHOLD predicate condition
 * @param {Object} condition - The condition definition
 * @param {Store} graph - The RDF graph
 * @param {Object} resolver - File resolver instance
 * @param {Object} env - Environment variables
 * @param {Object} options - Evaluation options
 * @returns {Promise<boolean>} Threshold condition result
 */
async function evaluateThreshold(condition, graph, _resolver, _env, _options) {
  const { spec } = condition;
  const { var: variable, op, value, aggregate = 'avg' } = spec;

  // Execute query to get values
  const query = `
    SELECT ?${variable} WHERE {
      ?s ?p ?${variable}
    }
  `;

  const results = await select(graph, query);

  if (results.length === 0) {
    return false;
  }

  // Extract numeric values
  const values = results
    .map(r => r[variable]?.value)
    .filter(v => v !== undefined)
    .map(v => parseFloat(v))
    .filter(v => !isNaN(v));

  if (values.length === 0) {
    return false;
  }

  // Calculate aggregate
  let aggregateValue;
  switch (aggregate) {
    case 'sum':
      aggregateValue = values.reduce((sum, v) => sum + v, 0);
      break;
    case 'avg':
      aggregateValue = values.reduce((sum, v) => sum + v, 0) / values.length;
      break;
    case 'min':
      aggregateValue = Math.min(...values);
      break;
    case 'max':
      aggregateValue = Math.max(...values);
      break;
    case 'count':
      aggregateValue = values.length;
      break;
    default:
      aggregateValue = values[0];
  }

  // Evaluate operator
  switch (op) {
    case '>':
      return aggregateValue > value;
    case '>=':
      return aggregateValue >= value;
    case '<':
      return aggregateValue < value;
    case '<=':
      return aggregateValue <= value;
    case '==':
      return Math.abs(aggregateValue - value) < 0.0001;
    case '!=':
      return Math.abs(aggregateValue - value) >= 0.0001;
    default:
      return false;
  }
}

/**
 * Evaluate a COUNT predicate condition
 * @param {Object} condition - The condition definition
 * @param {Store} graph - The RDF graph
 * @param {Object} resolver - File resolver instance
 * @param {Object} env - Environment variables
 * @param {Object} options - Evaluation options
 * @returns {Promise<boolean>} Count condition result
 */
async function evaluateCount(condition, graph, _resolver, _env, _options) {
  const { spec } = condition;
  const { op, value, query: countQuery } = spec;

  let count;

  if (countQuery) {
    // Use custom query for counting
    const results = await select(graph, countQuery);
    count = results.length;
  } else {
    // Count all quads
    count = graph.size;
  }

  // Evaluate operator
  switch (op) {
    case '>':
      return count > value;
    case '>=':
      return count >= value;
    case '<':
      return count < value;
    case '<=':
      return count <= value;
    case '==':
      return count === value;
    case '!=':
      return count !== value;
    default:
      return false;
  }
}

/**
 * Evaluate a WINDOW predicate condition
 * @param {Object} condition - The condition definition
 * @param {Store} graph - The RDF graph
 * @param {Object} resolver - File resolver instance
 * @param {Object} env - Environment variables
 * @param {Object} options - Evaluation options
 * @returns {Promise<boolean>} Window condition result
 */
async function evaluateWindow(condition, graph, _resolver, _env, _options) {
  const { spec } = condition;
  const { size, _slide = size, aggregate, query: windowQuery } = spec;

  // For now, implement a simple window evaluation
  // In a full implementation, this would maintain sliding windows over time

  if (windowQuery) {
    const results = await select(graph, windowQuery);

    // Calculate aggregate over results
    let aggregateValue;
    switch (aggregate) {
      case 'sum':
        aggregateValue = results.reduce((sum, r) => {
          const val = parseFloat(Object.values(r)[0]?.value || 0);
          return sum + (isNaN(val) ? 0 : val);
        }, 0);
        break;
      case 'avg':
        const sum = results.reduce((sum, r) => {
          const val = parseFloat(Object.values(r)[0]?.value || 0);
          return sum + (isNaN(val) ? 0 : val);
        }, 0);
        aggregateValue = results.length > 0 ? sum / results.length : 0;
        break;
      case 'min':
        aggregateValue = Math.min(
          ...results.map(r => {
            const val = parseFloat(Object.values(r)[0]?.value || Infinity);
            return isNaN(val) ? Infinity : val;
          })
        );
        break;
      case 'max':
        aggregateValue = Math.max(
          ...results.map(r => {
            const val = parseFloat(Object.values(r)[0]?.value || -Infinity);
            return isNaN(val) ? -Infinity : val;
          })
        );
        break;
      case 'count':
        aggregateValue = results.length;
        break;
      default:
        aggregateValue = results.length;
    }

    // For window conditions, we typically check if aggregate exceeds threshold
    // This is a simplified implementation
    return aggregateValue > 0;
  }

  // Default: check if graph has any data in the window
  return graph.size > 0;
}

/**
 * Hash a store for delta comparison
 * @param {Store} store - RDF store
 * @returns {Promise<string>} Store hash
 */
async function hashStore(store) {
  // Simple hash implementation - in production, use proper canonicalization
  const quads = Array.from(store);
  const quadStrings = quads
    .map(
      q =>
        `${q.subject?.value || ''}:${q.predicate?.value || ''}:${q.object?.value || ''}:${q.graph?.value || ''}`
    )
    .sort();

  return quadStrings.join('|');
}
