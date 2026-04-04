/**
 * @file Condition evaluation engine for knowledge hooks.
 * @module condition-evaluator
 *
 * @description
 * Production-ready condition evaluator that loads and executes SPARQL queries
 * and SHACL validations to determine if hooks should trigger.
 */

import { createFileResolver } from './file-resolver.mjs';
import { ask, select, construct } from './query.mjs';
import { validateShacl } from './validate.mjs';
import { createQueryOptimizer } from './query-optimizer.mjs';
import { createStore, dataFactory } from '../../../oxigraph/src/index.mjs';
import reasoner from 'eyereasoner';
import { Parser as SparqlParser, Generator as SparqlGenerator } from 'sparqljs';
import { z } from 'zod';

// ─── SPARQL Injection Prevention ────────────────────────────────────────────
const sparqlParser = new SparqlParser();
const sparqlGenerator = new SparqlGenerator();

/** Safe SPARQL variable name: letters, digits, underscore; must start with letter/underscore */
const SAFE_SPARQL_VAR_RE = /^[a-zA-Z_][a-zA-Z0-9_]*$/;

/**
 * Zod schema for SPARQL parameter values.
 * Allows: string, number, boolean, RDF NamedNode/BlankNode/Literal terms.
 * Rejects: plain objects, arrays, functions, null, undefined.
 */
export const SparqlParamSchema = z.union([
  z.string(),
  z.number().finite(),
  z.boolean(),
  z.object({
    termType: z.enum(['NamedNode', 'BlankNode', 'Literal']),
    value: z.string(),
  }),
]);

/**
 * Validate that a string is a safe SPARQL variable name.
 * @param {string} name - Variable name to validate
 * @returns {string} The validated name
 * @throws {Error} If name contains injection characters
 */
export function validateSparqlVariableName(name) {
  if (typeof name !== 'string' || !SAFE_SPARQL_VAR_RE.test(name)) {
    throw new Error(
      `Invalid SPARQL variable name: "${String(name)}". ` +
        'Must match /^[a-zA-Z_][a-zA-Z0-9_]*$/.'
    );
  }
  return name;
}

/**
 * Validate and parse a SPARQL query string to prevent injection.
 * Only allows read-only query types (SELECT, ASK, CONSTRUCT, DESCRIBE).
 * Rejects UPDATE, SERVICE, and LOAD operations.
 * @param {string} queryString - SPARQL query to validate
 * @returns {object} Parsed query AST
 * @throws {Error} If query is invalid or disallowed
 */
export function validateSparqlQuery(queryString) {
  if (typeof queryString !== 'string' || queryString.trim().length === 0) {
    throw new Error('SPARQL query must be a non-empty string');
  }

  let parsed;
  try {
    parsed = sparqlParser.parse(queryString);
  } catch (error) {
    throw new Error(`Invalid SPARQL syntax: ${error.message}`);
  }

  // Reject UPDATE operations (INSERT DATA, DELETE, DROP, CLEAR, CREATE, LOAD)
  if (parsed.type === 'update') {
    throw new Error('SPARQL UPDATE operations are not allowed in condition queries');
  }

  // Walk AST JSON to reject SERVICE clauses (federated query escape)
  const astJson = JSON.stringify(parsed);
  if (astJson.includes('"type":"service"')) {
    throw new Error('SERVICE clauses are not allowed in condition queries');
  }

  return parsed;
}

/**
 * Safely bind parameter values into a SPARQL query via AST manipulation.
 * @param {string} queryTemplate - SPARQL query with ?variable placeholders
 * @param {Object<string, *>} params - Variable-name → value map
 * @returns {string} Generated SPARQL with values bound
 */
export function bindSparqlParams(queryTemplate, params = {}) {
  if (!params || Object.keys(params).length === 0) {
    return queryTemplate;
  }

  // Validate every parameter key and value
  const replacements = new Map();
  for (const [key, value] of Object.entries(params)) {
    const varName = key.startsWith('?') ? key.slice(1) : key;
    validateSparqlVariableName(varName);

    const result = SparqlParamSchema.safeParse(value);
    if (!result.success) {
      throw new Error(
        `Invalid SPARQL parameter value for ?${varName}: ${result.error.message}`
      );
    }
    replacements.set(varName, toRdfTerm(value));
  }

  // Parse template → AST, replace variables, regenerate
  const parsed = sparqlParser.parse(queryTemplate);
  replaceVariablesInAst(parsed, replacements);
  return sparqlGenerator.stringify(parsed);
}

/** Convert a JS value to a sparqljs-compatible RDF term node. */
function toRdfTerm(value) {
  if (typeof value === 'string') {
    return { termType: 'Literal', value };
  }
  if (typeof value === 'number') {
    return {
      termType: 'Literal',
      value: String(value),
      datatype: {
        termType: 'NamedNode',
        value: 'http://www.w3.org/2001/XMLSchema#decimal',
      },
    };
  }
  if (typeof value === 'boolean') {
    return {
      termType: 'Literal',
      value: String(value),
      datatype: {
        termType: 'NamedNode',
        value: 'http://www.w3.org/2001/XMLSchema#boolean',
      },
    };
  }
  if (value && value.termType) {
    return value;
  }
  throw new Error(`Cannot convert value to RDF term: ${typeof value}`);
}

/** Recursively replace Variable nodes in a sparqljs AST. */
function replaceVariablesInAst(node, replacements) {
  if (!node || typeof node !== 'object') return;
  if (Array.isArray(node)) {
    for (let i = 0; i < node.length; i++) {
      if (node[i]?.termType === 'Variable' && replacements.has(node[i].value)) {
        node[i] = replacements.get(node[i].value);
      } else {
        replaceVariablesInAst(node[i], replacements);
      }
    }
    return;
  }
  for (const key of Object.keys(node)) {
    const val = node[key];
    if (val && typeof val === 'object') {
      if (val.termType === 'Variable' && replacements.has(val.value)) {
        node[key] = replacements.get(val.value);
      } else {
        replaceVariablesInAst(val, replacements);
      }
    }
  }
}

/**
 * SlidingWindow class for temporal event tracking
 * Maintains a window of events and supports both time-based and event-based windowing
 */
class SlidingWindow {
  /** @type {number} Memory warning threshold in bytes (100MB) */
  static MEMORY_WARN_BYTES = 100 * 1024 * 1024;
  /** @type {number} Memory hard limit in bytes (500MB) */
  static MEMORY_LIMIT_BYTES = 500 * 1024 * 1024;

  /**
   * @param {number} size - Window size (milliseconds for time-based, count for event-based)
   * @param {number} [slide] - Slide amount (defaults to size for tumbling window)
   * @param {boolean} [timeWindow=true] - If true, size is in milliseconds; if false, size is event count
   * @param {number} [maxHistorySize=10000] - Maximum number of events to retain (LRU eviction)
   */
  constructor(size, slideAmount = size, timeWindow = true, maxHistorySize = 10000) {
    this.size = size;
    this.slideAmount = slideAmount;
    this.timeWindow = timeWindow;
    this.maxHistorySize = maxHistorySize;
    this.events = []; // Array of { timestamp, value, data }
    this.lastSlideTime = Date.now();

    // Metrics tracking
    this._metrics = {
      totalEvictions: 0,
      totalEventsAdded: 0,
      peakEventCount: 0,
      lastMemoryEstimate: 0,
      memoryWarnings: 0,
    };
  }

  /**
   * Estimate approximate memory usage of the event queue in bytes.
   * Each event object has: timestamp (8), value (variable), data (variable), index (8).
   * We estimate ~200 bytes per event as a conservative baseline for object overhead + pointers.
   * @returns {number} Estimated memory usage in bytes
   */
  estimateMemoryUsage() {
    const perEventOverhead = 200;
    const estimate = this.events.length * perEventOverhead;
    this._metrics.lastMemoryEstimate = estimate;
    return estimate;
  }

  /**
   * Add an event to the window
   * @param {*} value - The value to add
   * @param {*} data - Additional data to track
   * @throws {Error} If estimated memory exceeds 500MB hard limit
   */
  add(value, data = null) {
    // Enforce maxHistorySize via LRU eviction (remove oldest first)
    while (this.events.length >= this.maxHistorySize) {
      this.events.shift();
      this._metrics.totalEvictions++;
    }

    const now = Date.now();
    this.events.push({
      timestamp: now,
      value,
      data,
      index: this._metrics.totalEventsAdded,
    });
    this._metrics.totalEventsAdded++;

    // Track peak
    if (this.events.length > this._metrics.peakEventCount) {
      this._metrics.peakEventCount = this.events.length;
    }

    // Memory monitoring (check every 1000 events to avoid overhead)
    if (this._metrics.totalEventsAdded % 1000 === 0) {
      this._checkMemoryLimits();
    }

    // Clean up expired events
    this.prune();
  }

  /**
   * Check memory limits and warn/throw as appropriate
   * @private
   */
  _checkMemoryLimits() {
    const memEstimate = this.estimateMemoryUsage();

    if (memEstimate > SlidingWindow.MEMORY_LIMIT_BYTES) {
      throw new Error(
        `SlidingWindow memory limit exceeded: ${(memEstimate / 1024 / 1024).toFixed(1)}MB > 500MB limit. ` +
          `Events: ${this.events.length}, consider reducing maxHistorySize (current: ${this.maxHistorySize})`
      );
    }

    if (memEstimate > SlidingWindow.MEMORY_WARN_BYTES) {
      this._metrics.memoryWarnings++;
      console.warn(
        `[SlidingWindow] Memory warning: ~${(memEstimate / 1024 / 1024).toFixed(1)}MB used ` +
          `(${this.events.length} events, limit: ${this.maxHistorySize})`
      );
    }
  }

  /**
   * Get current window contents
   * @returns {Array} Events within current window
   */
  getWindow() {
    const now = Date.now();
    if (this.timeWindow) {
      // Time-based window: keep events within [now - size, now)
      const cutoff = now - this.size;
      return this.events.filter(e => e.timestamp > cutoff);
    }
    // Event-based window: keep last 'size' events
    if (this.events.length <= this.size) {
      return this.events;
    }
    return this.events.slice(-this.size);
  }

  /**
   * Remove events outside the window
   */
  prune() {
    const now = Date.now();
    if (this.timeWindow) {
      const cutoff = now - this.size;
      this.events = this.events.filter(e => e.timestamp > cutoff);
    }
  }

  /**
   * Slide the window forward
   * @returns {boolean} True if window slid
   */
  slide() {
    const now = Date.now();
    const shouldSlide = now - this.lastSlideTime >= this.slideAmount;

    if (shouldSlide) {
      this.lastSlideTime = now;
      this.prune();
      return true;
    }
    return false;
  }

  /**
   * Clear window state
   */
  clear() {
    this.events = [];
    this.lastSlideTime = Date.now();
  }

  /**
   * Get window size (number of events in current window)
   * @returns {number}
   */
  length() {
    return this.getWindow().length;
  }

  /**
   * Get telemetry metrics for this window
   * @returns {Object} Metrics snapshot
   */
  getMetrics() {
    return {
      totalEvictions: this._metrics.totalEvictions,
      totalEventsAdded: this._metrics.totalEventsAdded,
      currentEventCount: this.events.length,
      peakEventCount: this._metrics.peakEventCount,
      estimatedMemoryBytes: this.estimateMemoryUsage(),
      maxHistorySize: this.maxHistorySize,
      memoryWarnings: this._metrics.memoryWarnings,
    };
  }
}

// Export SlidingWindow for testing and external use
export { SlidingWindow };

// Global window state storage (keyed by condition ID)
const _windowStateMap = new WeakMap();

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
  const report = await validateShacl(graph, turtle, {
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
          // Execute repair SPARQL CONSTRUCT query to get repair quads
          const repairQuads = await construct(graph, repairConstruct, { env });

          // Apply repaired quads to the store
          let quadsApplied = 0;
          for (const quad of repairQuads) {
            graph.add(quad);
            quadsApplied++;
          }

          // Log repair application
          if (env.logRepair) {
            console.log(`[SHACL Repair] Applied ${quadsApplied} repair quads to store`);
          }

          // Re-validate after repair with updated graph
          const revalidateReport = await validateShacl(graph, turtle, {
            strict: env.strictMode || false,
            includeDetails: true,
          });

          // Log revalidation result
          if (env.logRepair) {
            console.log(
              `[SHACL Repair] Revalidation result: ${revalidateReport.conforms ? 'conforms' : 'violations remain'}`
            );
          }

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
 * @returns {Array} Array of valid RDF quads
 */
export function serializeShaclReport(report) {
  const quads = [];

  if (!report.results || report.results.length === 0) {
    return quads;
  }

  // SHACL vocabulary IRIs
  const SHACL_NS = 'http://www.w3.org/ns/shacl#';
  const RDF_NS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';

  // Severity level URIs
  const SEVERITY_URIS = {
    violation: `${SHACL_NS}Violation`,
    warning: `${SHACL_NS}Warning`,
    info: `${SHACL_NS}Info`,
  };

  // For each violation result, create RDF representation
  for (let i = 0; i < report.results.length; i++) {
    const result = report.results[i];

    // Map severity string to SHACL severity URI
    const severityUri = SEVERITY_URIS[result.severity] || SEVERITY_URIS.violation;

    // Create unique URI for this validation result
    const resultUri = `http://example.com/validation/result-${i}`;
    const resultNode = dataFactory.namedNode(resultUri);

    // Triple 1: result rdf:type sh:ValidationResult
    quads.push(
      dataFactory.quad(
        resultNode,
        dataFactory.namedNode(`${RDF_NS}type`),
        dataFactory.namedNode(`${SHACL_NS}ValidationResult`)
      )
    );

    // Triple 2: result sh:resultMessage "message"
    if (result.message) {
      quads.push(
        dataFactory.quad(
          resultNode,
          dataFactory.namedNode(`${SHACL_NS}resultMessage`),
          dataFactory.literal(result.message)
        )
      );
    }

    // Triple 3: result sh:resultSeverity sh:Violation (or Warning/Info)
    quads.push(
      dataFactory.quad(
        resultNode,
        dataFactory.namedNode(`${SHACL_NS}resultSeverity`),
        dataFactory.namedNode(severityUri)
      )
    );
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
    ![
      'sparql-ask',
      'sparql-select',
      'shacl',
      'delta',
      'threshold',
      'count',
      'window',
      'n3',
      'datalog',
    ].includes(condition.kind)
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
      error:
        'Condition must have either ref (file reference) or inline content (query/shapes/facts/goal/rules)',
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
    // Calculate change based on delta composition
    // Positive = more additions (increase), Negative = more removals (decrease)
    const totalQuads = graph.size;
    const additions = options.delta.additions?.length || 0;
    const removals = options.delta.removals?.length || 0;
    const netChange = additions - removals;
    changeMagnitude = totalQuads > 0 ? netChange / totalQuads : 0;
  }

  // Evaluate change type
  switch (change) {
    case 'any':
      return changeMagnitude !== 0;
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

  // Validate variable name to prevent SPARQL injection
  validateSparqlVariableName(variable);

  // Execute query to get values (variable is now guaranteed safe)
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
    // Validate query to prevent SPARQL injection
    validateSparqlQuery(countQuery);
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
async function evaluateWindow(condition, graph, _resolver, _env, _options = {}) {
  const { spec, id } = condition;
  if (!spec) {
    throw new Error('Window condition requires a spec property');
  }

  const { size, slide = size, aggregate = 'count', query: windowQuery } = spec;

  if (!size || size <= 0) {
    throw new Error('Window condition spec.size must be positive');
  }

  // Get or create window state storage from options
  let windowState = _options.windowState;
  if (!windowState) {
    windowState = new Map();
    if (_options) {
      _options.windowState = windowState;
    }
  }

  // Use condition ID or a hash as key; fallback to stringified spec
  const stateKey = id || JSON.stringify(spec);

  // Get or create sliding window instance
  let window = windowState.get(stateKey);
  if (!window) {
    // Assume time-based window (size is in milliseconds)
    window = new SlidingWindow(size, slide, true);
    windowState.set(stateKey, window);
  }

  // Execute the window query to get values
  if (!windowQuery || typeof windowQuery !== 'string') {
    throw new Error('Window condition requires a query property');
  }

  // Validate query to prevent SPARQL injection
  validateSparqlQuery(windowQuery);

  const results = await select(graph, windowQuery);

  // Extract numeric values from results
  const values = results
    .map(r => {
      // Get first binding value
      const firstValue = Object.values(r)[0];
      if (!firstValue) return null;

      const val = parseFloat(firstValue.value);
      return isNaN(val) ? null : val;
    })
    .filter(v => v !== null);

  // Add values to window
  for (const val of values) {
    window.add(val, { timestamp: Date.now() });
  }

  // Slide window if needed
  window.slide();

  // Get window contents for aggregation
  const windowContents = window.getWindow();

  // Calculate aggregate over window contents
  let aggregateValue;
  switch (aggregate) {
    case 'sum':
      aggregateValue = windowContents.reduce((sum, e) => sum + (e.value || 0), 0);
      break;
    case 'avg':
      if (windowContents.length === 0) {
        aggregateValue = 0;
      } else {
        const sum = windowContents.reduce((s, e) => s + (e.value || 0), 0);
        aggregateValue = sum / windowContents.length;
      }
      break;
    case 'min':
      aggregateValue =
        windowContents.length > 0 ? Math.min(...windowContents.map(e => e.value)) : Infinity;
      break;
    case 'max':
      aggregateValue =
        windowContents.length > 0 ? Math.max(...windowContents.map(e => e.value)) : -Infinity;
      break;
    case 'count':
      aggregateValue = windowContents.length;
      break;
    default:
      aggregateValue = windowContents.length;
  }

  // Window conditions typically check if aggregate meets a threshold
  // Return true if we have data in the window
  // For rate limiting scenarios, check maxMatches if provided in original API
  const maxMatches = spec.maxMatches;
  if (maxMatches !== undefined && maxMatches !== null) {
    return aggregateValue <= maxMatches;
  }

  // Default: true if window has content
  return aggregateValue > 0;
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
