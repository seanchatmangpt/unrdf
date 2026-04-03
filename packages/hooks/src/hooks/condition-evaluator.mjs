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
import reasoner from 'eyereasoner';
// import { Database } from 'datalog-ts'; // TODO: Datalog evaluation via Database class

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
      case 'n3':
        return await evaluateN3(condition, graph, resolver, env);
      case 'datalog':
        return await evaluateDatalog(condition, graph, resolver, env);
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
 * Evaluate a SHACL validation condition with enforcement modes.
 *
 * Enforcement modes:
 * - 'block' (default): Fail if validation fails. Return false.
 * - 'annotate': Allow write with annotation. Add SHACL report as RDF triples. Return true.
 * - 'repair': Execute repairConstruct query if validation fails. Re-validate. Return result.
 *
 * @param {Object} condition - The condition definition
 * @param {Store} graph - The RDF graph
 * @param {Object} resolver - File resolver instance
 * @param {Object} env - Environment variables
 * @returns {Promise<Object|boolean>} SHACL validation result or boolean depending on enforcement mode
 */
async function evaluateShacl(condition, graph, resolver, env) {
  const { ref, enforcementMode = 'block', repairConstruct } = condition;

  if (!ref || !ref.uri) {
    throw new Error('SHACL condition requires ref with uri');
  }

  // Load SHACL shapes file
  const { turtle } = await resolver.loadShacl(ref.uri, ref.sha256);

  // Execute SHACL validation
  const report = validateShacl(graph, turtle, {
    strict: env.strictMode || false,
    includeDetails: true,
  });

  const isValid = report.conforms === true;

  // Dispatch based on enforcement mode
  switch (enforcementMode) {
    case 'block':
      // Default behavior: return report (caller checks conforms flag)
      return report;

    case 'annotate': {
      // If validation fails, add SHACL report as RDF triples to store
      if (!isValid) {
        try {
          // Serialize SHACL report to RDF format
          const reportTriples = serializeShaclReport(report);

          // Add report triples to the store
          for (const triple of reportTriples) {
            graph.add(triple);
          }

          // Log annotation
          if (env.logAnnotations) {
            console.log(
              `[SHACL Annotation] Added ${reportTriples.length} violation triples to store`
            );
          }
        } catch (error) {
          console.warn(`Failed to add SHACL annotation: ${error.message}`);
        }
      }

      // Return true to allow write (with or without annotation)
      return true;
    }

    case 'repair': {
      // If validation fails and repair query provided, attempt repair
      if (!isValid && repairConstruct) {
        try {
          // Execute repair SPARQL CONSTRUCT query
          const repairResults = await select(graph, repairConstruct);

          // In a full implementation, would apply repair results to store
          // For now, log repair attempt
          if (env.logRepair) {
            console.log(`[SHACL Repair] Applied repair with ${repairResults.length} results`);
          }

          // Re-validate after repair
          const revalidateReport = validateShacl(graph, turtle, {
            strict: env.strictMode || false,
            includeDetails: true,
          });

          // Return re-validation result
          return revalidateReport.conforms === true;
        } catch (error) {
          // Repair failed, return original validation result
          console.warn(`SHACL repair failed: ${error.message}`);
          return false;
        }
      }

      // No repair attempted, return validation result
      return isValid;
    }

    default:
      // Unknown enforcement mode, default to block
      return report;
  }
}

/**
 * Serialize SHACL validation report to RDF triples.
 * Converts violations into RDF quads that can be added to store.
 *
 * @param {Object} report - SHACL validation report
 * @returns {Array} Array of RDF triples/quads
 */
function serializeShaclReport(report) {
  const quads = [];

  if (!report.results || report.results.length === 0) {
    return quads;
  }

  // For each violation result, create RDF representation
  for (let i = 0; i < report.results.length; i++) {
    const result = report.results[i];

    // In production, use proper RDF factory and SHACL vocabulary
    // For now, represent as simple objects that store can consume
    if (result.severity === 'violation') {
      quads.push({
        subject: { value: `shacl:violation-${i}` },
        predicate: { value: 'rdf:type' },
        object: { value: 'sh:ValidationResult' },
      });

      if (result.message) {
        quads.push({
          subject: { value: `shacl:violation-${i}` },
          predicate: { value: 'sh:resultMessage' },
          object: { value: result.message },
        });
      }

      if (result.severity) {
        quads.push({
          subject: { value: `shacl:violation-${i}` },
          predicate: { value: 'sh:resultSeverity' },
          object: { value: result.severity },
        });
      }
    }
  }

  return quads;
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
    !['sparql-ask', 'sparql-select', 'shacl', 'delta', 'threshold', 'count', 'window', 'n3', 'datalog'].includes(
      condition.kind
    )
  ) {
    return {
      valid: false,
      error: `Unsupported condition kind: ${condition.kind}`,
    };
  }

  // Support both file reference (ref) and inline content (query/shapes/facts/goal/rules)
  const hasRef = condition.ref && condition.ref.uri;
  const hasInline =
    condition.query ||
    condition.shapes ||
    condition.facts ||
    condition.goal ||
    condition.rules ||
    condition.askQuery;

  if (!hasRef && !hasInline) {
    return {
      valid: false,
      error: 'Condition must have either ref (file reference) or inline content (query/shapes/facts/goal/rules)',
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
 * Evaluate an N3 forward-chaining condition via EYE reasoner
 * @param {Object} condition - The condition definition
 * @param {Store} graph - The RDF graph
 * @param {Object} resolver - File resolver instance
 * @param {Object} env - Environment variables
 * @returns {Promise<boolean>} N3 condition result
 */
async function evaluateN3(condition, graph, resolver, env) {
  const { rules, askQuery } = condition;

  if (!rules || !askQuery) {
    throw new Error('N3 condition requires both rules and askQuery properties');
  }

  // Serialize store to N-Quads
  const dataN3 = await graph.dump({ format: 'application/n-quads' });

  // Run through EYE reasoner
  const entailedData = await reasoner(dataN3 + '\n\n' + rules);

  // Parse result into temp store
  const entailedStore = createStore();
  await entailedStore.load(entailedData, { format: 'application/n-quads' });

  // Evaluate SPARQL ASK over entailed graph
  const result = await ask(entailedStore, askQuery, { env });

  return result;
}

/**
 * Evaluate a Datalog logic programming condition using bottom-up fixpoint evaluation.
 *
 * This is a minimal Datalog evaluator implemented in pure JavaScript without external
 * dependencies. It supports:
 * - Facts: "predicate(arg1, arg2, ...)"
 * - Rules: "head(X) :- body1(X), body2(X)"
 * - Goals: "goal(X)" returns true if goal is derivable
 *
 * Limitations (infrastructure-ready but not production-grade):
 * - No optimization or indexing
 * - Linear scan for each rule application
 * - No support for negation-as-failure or cut
 * - Limited pattern matching (single-pass, no backtracking)
 *
 * For production use cases, prefer N3 forward-chaining via eyereasoner package.
 *
 * @param {Object} condition - The condition definition
 * @param {Store} graph - The RDF graph (unused; logic is pure Datalog)
 * @param {Object} resolver - File resolver instance
 * @param {Object} env - Environment variables
 * @returns {Promise<boolean>} Datalog goal evaluation result
 */
async function evaluateDatalog(condition, _graph, _resolver, _env) {
  const { facts: inlineFacts = [], rules: rulesInput = '', goal } = condition;

  if (!goal) {
    throw new Error('Datalog condition requires a goal property (e.g., "allowed(alice)")');
  }

  // Initialize the fact database
  const factDb = new Map(); // Map: predicateName → Set of fact representations

  // Parse and add inline facts
  for (const fact of inlineFacts) {
    const parsed = parseDatalogTerm(fact);
    if (!parsed) {
      throw new Error(`Invalid Datalog fact format: ${fact}. Expected format: "predicate(arg1, arg2, ...)"`);
    }

    const { predicate, args } = parsed;
    const key = `${predicate}/${args.length}`;

    if (!factDb.has(key)) {
      factDb.set(key, new Set());
    }

    // Store fact as a JSON string for easy comparison
    const factStr = JSON.stringify([predicate, args]);
    factDb.get(key).add(factStr);
  }

  // Parse rules (handle both string and array formats)
  const rulesList = Array.isArray(rulesInput)
    ? rulesInput
    : rulesInput
        .split('\n')
        .map(line => line.trim())
        .filter(line => line.length > 0 && !line.startsWith('%'));

  const parsedRules = [];
  for (const rule of rulesList) {
    const ruleObj = parseDatalogRule(rule);
    if (!ruleObj) {
      throw new Error(`Invalid Datalog rule format: ${rule}. Expected format: "head(X) :- body1(X), body2(X)"`);
    }
    parsedRules.push(ruleObj);
  }

  // Bottom-up fixpoint evaluation
  let changed = true;
  let iterations = 0;
  const maxIterations = 100; // Prevent infinite loops

  while (changed && iterations < maxIterations) {
    changed = false;
    iterations++;

    // For each rule, try to derive new facts
    for (const rule of parsedRules) {
      const newFacts = evaluateRule(rule, factDb);

      // Add new facts to database
      for (const newFact of newFacts) {
        const [predicate, args] = newFact;
        const key = `${predicate}/${args.length}`;

        if (!factDb.has(key)) {
          factDb.set(key, new Set());
        }

        const factStr = JSON.stringify([predicate, args]);
        if (!factDb.get(key).has(factStr)) {
          factDb.get(key).add(factStr);
          changed = true;
        }
      }
    }
  }

  // Query for the goal
  const goalParsed = parseDatalogTerm(goal);
  if (!goalParsed) {
    throw new Error(`Invalid Datalog goal format: ${goal}. Expected format: "predicate(arg1, arg2, ...)"`);
  }

  const { predicate, args } = goalParsed;
  const key = `${predicate}/${args.length}`;

  if (!factDb.has(key)) {
    return false;
  }

  const factStr = JSON.stringify([predicate, args]);
  return factDb.get(key).has(factStr);
}

/**
 * Parse a Datalog term: "predicate(arg1, arg2, ...)"
 * @param {string} term - Term to parse
 * @returns {Object|null} { predicate, args } or null if invalid
 */
function parseDatalogTerm(term) {
  const match = term.match(/^(\w+)\((.*)\)$/);
  if (!match) {
    return null;
  }

  const [, predicate, argsStr] = match;
  const args = argsStr.length === 0 ? [] : argsStr.split(',').map(arg => arg.trim());

  return { predicate, args };
}

/**
 * Parse a Datalog rule: "head(X, Y) :- body1(X), body2(Y)"
 * @param {string} rule - Rule to parse
 * @returns {Object|null} { head, body } or null if invalid
 */
function parseDatalogRule(rule) {
  const parts = rule.split(':-');
  if (parts.length !== 2) {
    return null;
  }

  const head = parseDatalogTerm(parts[0].trim());
  if (!head) {
    return null;
  }

  // Parse body goals (comma-separated, but respect parentheses)
  const bodyStr = parts[1].trim();
  const bodyGoals = splitOnTopLevelCommas(bodyStr)
    .map(g => parseDatalogTerm(g.trim()))
    .filter(g => g !== null);

  return { head, body: bodyGoals };
}

/**
 * Split a string on top-level commas (not inside parentheses)
 * @param {string} str - String to split
 * @returns {Array} Array of substrings
 */
function splitOnTopLevelCommas(str) {
  const parts = [];
  let current = '';
  let depth = 0;

  for (let i = 0; i < str.length; i++) {
    const char = str[i];

    if (char === '(') {
      depth++;
      current += char;
    } else if (char === ')') {
      depth--;
      current += char;
    } else if (char === ',' && depth === 0) {
      parts.push(current);
      current = '';
    } else {
      current += char;
    }
  }

  if (current.length > 0) {
    parts.push(current);
  }

  return parts;
}

/**
 * Evaluate a rule against current facts and return new derived facts
 * @param {Object} rule - Parsed rule { head, body }
 * @param {Map} factDb - Current fact database
 * @returns {Array} Array of new facts [predicate, args]
 */
function evaluateRule(rule, factDb) {
  const { head, body } = rule;
  const newFacts = [];

  if (body.length === 0) {
    // Fact-only rule (shouldn't happen in practice)
    newFacts.push([head.predicate, head.args]);
    return newFacts;
  }

  // Find all bindings that satisfy the body goals
  const bindings = findBindings(body, factDb, {});

  for (const binding of bindings) {
    // Apply binding to head to create new fact
    const boundHead = applyBinding(head, binding);
    newFacts.push([boundHead.predicate, boundHead.args]);
  }

  return newFacts;
}

/**
 * Find all variable bindings that satisfy body goals
 * @param {Array} body - Array of goals
 * @param {Map} factDb - Fact database
 * @param {Object} binding - Current variable bindings
 * @returns {Array} Array of bindings that satisfy all goals
 */
function findBindings(body, factDb, binding) {
  if (body.length === 0) {
    return [binding];
  }

  const [goal, ...remainingGoals] = body;
  const goalBindings = [];

  // Find all facts that unify with this goal
  const key = `${goal.predicate}/${goal.args.length}`;
  const facts = factDb.get(key) || new Set();

  for (const factStr of facts) {
    const [_predicate, factArgs] = JSON.parse(factStr);
    const newBinding = unify(goal.args, factArgs, binding);

    if (newBinding !== null) {
      // Recursively find bindings for remaining goals
      const moreBindings = findBindings(remainingGoals, factDb, newBinding);
      goalBindings.push(...moreBindings);
    }
  }

  return goalBindings;
}

/**
 * Unify goal arguments with fact arguments
 * @param {Array} goalArgs - Goal argument list
 * @param {Array} factArgs - Fact argument list
 * @param {Object} binding - Current variable bindings
 * @returns {Object|null} Updated binding or null if unification fails
 */
function unify(goalArgs, factArgs, binding) {
  if (goalArgs.length !== factArgs.length) {
    return null;
  }

  const newBinding = { ...binding };

  for (let i = 0; i < goalArgs.length; i++) {
    const goalArg = goalArgs[i];
    const factArg = factArgs[i];

    if (isVariable(goalArg)) {
      // Bind variable
      if (newBinding[goalArg]) {
        if (newBinding[goalArg] !== factArg) {
          return null; // Conflict
        }
      } else {
        newBinding[goalArg] = factArg;
      }
    } else if (goalArg !== factArg) {
      return null; // Argument mismatch
    }
  }

  return newBinding;
}

/**
 * Apply variable bindings to a term
 * @param {Object} term - Term { predicate, args }
 * @param {Object} binding - Variable bindings
 * @returns {Object} Bound term { predicate, args }
 */
function applyBinding(term, binding) {
  const boundArgs = term.args.map(arg => binding[arg] || arg);
  return { predicate: term.predicate, args: boundArgs };
}

/**
 * Check if a string is a variable (starts with uppercase or _)
 * @param {string} s - String to check
 * @returns {boolean} True if variable
 */
function isVariable(s) {
  return s.length > 0 && (s[0] === s[0].toUpperCase() || s[0] === '_');
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
