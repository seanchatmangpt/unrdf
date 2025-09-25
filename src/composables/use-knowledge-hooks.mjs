/**
 * @fileoverview useKnowledgeHooks - Reactive triggers for RDF graphs using existing composables
 * 
 * A composable-first implementation that leverages existing unrdf composables:
 * - useGraph for SPARQL queries
 * - useReasoner for OWL reasoning
 * - useDelta for change detection
 * - useZod for schema validation
 * - useCanon for canonicalization
 * 
 * @version 3.0.0
 * @author GitVan Team
 * @license MIT
 */

import { useStoreContext } from "../context/index.mjs";
import { useGraph } from "./use-graph.mjs";
import { useReasoner } from "./use-reasoner.mjs";
import { useDelta } from "./use-delta.mjs";
import { useZod } from "./use-zod.mjs";
import { useCanon } from "./use-canon.mjs";
import { EVENTS } from "../engines/event-bus.mjs";

/**
 * Define a knowledge hook using existing composables
 * @param {Object} config - Hook configuration
 * @param {string} config.id - Unique hook identifier
 * @param {string} config.query - SPARQL query to execute
 * @param {Array<Object>} [config.predicates] - Array of predicate conditions
 * @param {string} [config.combine='AND'] - How to combine predicates ('AND', 'OR')
 * @returns {Object} Hook definition
 * 
 * @example
 * // COUNT predicate using useGraph
 * const hook = defineHook({
 *   id: 'error-count',
 *   query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
 *   predicates: [
 *     { kind: 'COUNT', spec: { operator: '>', value: 5 } }
 *   ]
 * });
 * 
 * // THRESHOLD predicate using useGraph results
 * const hook = defineHook({
 *   id: 'latency-threshold',
 *   query: 'SELECT ?service ?latency WHERE { ?service ex:latency ?latency }',
 *   predicates: [
 *     { kind: 'THRESHOLD', spec: { variable: 'latency', operator: '>', value: 1000 } }
 *   ]
 * });
 * 
 * // OWL predicate using useReasoner
 * const hook = defineHook({
 *   id: 'inference-check',
 *   query: 'SELECT ?person WHERE { ?person rdf:type ex:Person }',
 *   predicates: [
 *     { kind: 'OWL', spec: { rules: '@prefix ex: <http://example.org/> . { ?x ex:hasParent ?y } => { ?x ex:isChildOf ?y } .' } }
 *   ]
 * });
 * 
 * // DELTA predicate using useDelta
 * const hook = defineHook({
 *   id: 'change-detection',
 *   query: 'SELECT ?s WHERE { ?s ex:modified ?date }',
 *   predicates: [
 *     { kind: 'DELTA', spec: { compareWith: newStore, operator: '>', value: 0 } }
 *   ]
 * });
 * 
 * // ZOD predicate using useZod
 * const hook = defineHook({
 *   id: 'schema-validation',
 *   query: 'SELECT ?person ?name ?age WHERE { ?person ex:name ?name ; ex:age ?age }',
 *   predicates: [
 *     { kind: 'ZOD', spec: { schema: z.object({ name: z.string(), age: z.number() }) } }
 *   ]
 * });
 * 
 * // ASK predicate using useGraph
 * const hook = defineHook({
 *   id: 'existence-check',
 *   query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
 *   predicates: [
 *     { kind: 'ASK', spec: { query: 'ASK WHERE { ?s ex:type ex:Error }', expected: true } }
 *   ]
 * });
 */
export function defineHook(config) {
  const {
    id,
    query,
    predicates = [],
    combine = 'AND'
  } = config;

  const hook = {
    id,
    query,
    predicates,
    combine,
    _validate() {
      if (!id || typeof id !== 'string') {
        throw new Error('Hook id must be a non-empty string');
      }
      if (!query || typeof query !== 'string') {
        throw new Error('Hook query must be a non-empty string');
      }
      if (!Array.isArray(predicates)) {
        throw new Error('Hook predicates must be an array');
      }
      if (!['AND', 'OR'].includes(combine)) {
        throw new Error('Hook combine must be "AND" or "OR"');
      }
      
      // Validate each predicate
      for (const predicate of predicates) {
        if (!predicate.kind || typeof predicate.kind !== 'string') {
          throw new Error('Predicate must have a kind');
        }
        if (!predicate.spec || typeof predicate.spec !== 'object') {
          throw new Error('Predicate must have a spec object');
        }
        
        // Validate predicate-specific requirements
        switch (predicate.kind) {
          case 'COUNT':
            if (!predicate.spec.operator || !['>', '<', '>=', '<=', '==', '!='].includes(predicate.spec.operator)) {
              throw new Error('COUNT predicate requires a valid operator');
            }
            if (typeof predicate.spec.value !== 'number') {
              throw new Error('COUNT predicate requires a numeric value');
            }
            break;
          case 'THRESHOLD':
            if (!predicate.spec.variable || typeof predicate.spec.variable !== 'string') {
              throw new Error('THRESHOLD predicate requires a variable');
            }
            if (!predicate.spec.operator || !['>', '<', '>=', '<=', '==', '!='].includes(predicate.spec.operator)) {
              throw new Error('THRESHOLD predicate requires a valid operator');
            }
            if (typeof predicate.spec.value !== 'number') {
              throw new Error('THRESHOLD predicate requires a numeric value');
            }
            break;
          case 'ASK':
            if (!predicate.spec.query || typeof predicate.spec.query !== 'string') {
              throw new Error('ASK predicate requires a query');
            }
            break;
          case 'OWL':
            if (!predicate.spec.rules || typeof predicate.spec.rules !== 'string') {
              throw new Error('OWL predicate requires rules');
            }
            break;
          case 'DELTA':
            if (!predicate.spec.compareWith) {
              throw new Error('DELTA predicate requires compareWith store');
            }
            if (!predicate.spec.operator || !['>', '<', '>=', '<=', '==', '!='].includes(predicate.spec.operator)) {
              throw new Error('DELTA predicate requires a valid operator');
            }
            if (typeof predicate.spec.value !== 'number') {
              throw new Error('DELTA predicate requires a numeric value');
            }
            break;
          case 'ZOD':
            if (!predicate.spec.schema) {
              throw new Error('ZOD predicate requires a schema');
            }
            break;
          default:
            throw new Error(`Unknown predicate kind: ${predicate.kind}`);
        }
      }
    }
  };

  // Validate immediately
  hook._validate();
  
  return hook;
}

/**
 * Evaluate a knowledge hook using existing composables
 * @param {Object} hook - Hook definition
 * @param {Object} [options] - Evaluation options
 * @returns {Promise<Object>} Evaluation result
 * 
 * @example
 * const result = await evaluateHook(hook);
 * if (result.fired) {
 *   console.log('Hook fired!', result.data);
 * }
 */
export async function evaluateHook(hook, options = {}) {
  // Validate hook
  hook._validate();
  
  // Get composables from context
  const storeContext = useStoreContext();
  const graph = useGraph();
  const reasoner = useReasoner();
  const delta = useDelta();
  const zod = useZod();
  const canon = useCanon();

  const startTime = Date.now();

  try {
    // Step 1: Execute SPARQL query using useGraph
    console.log('DEBUG: Executing query:', hook.query);
    const queryResults = await graph.select(hook.query);
    console.log('DEBUG: Query results:', queryResults);
    console.log('DEBUG: Query results length:', queryResults.length);
    
    // Step 2: Evaluate predicates using appropriate composables
    const predicateResults = [];
    let fired = hook.combine === 'AND';
    
    for (const predicate of hook.predicates) {
      const predicateResult = await evaluatePredicate(predicate, queryResults, {
        graph,
        reasoner,
        delta,
        zod,
        canon,
        storeContext
      });
      
      predicateResults.push(predicateResult);
      
      // Combine results based on operator
      if (hook.combine === 'AND') {
        fired = fired && predicateResult.fired;
      } else {
        fired = fired || predicateResult.fired;
      }
    }

    const duration = Date.now() - startTime;

    return {
    hookId: hook.id,
    fired,
    data: {
        queryResults,
        predicateResults,
        count: queryResults.length
      },
      duration,
      timestamp: new Date().toISOString()
    };
  } catch (error) {
    return {
      hookId: hook.id,
      fired: false,
      error: error.message,
      data: {
        queryResults: [],
        predicateResults: [],
        count: 0
      },
      duration: Date.now() - startTime,
      timestamp: new Date().toISOString()
    };
  }
}

/**
 * Evaluate a single predicate using the appropriate composable
 * @param {Object} predicate - Predicate definition
 * @param {Array} queryResults - SPARQL query results
 * @param {Object} composables - Available composables
 * @returns {Promise<Object>} Predicate result
 */
async function evaluatePredicate(predicate, queryResults, composables) {
  const { graph, reasoner, delta, zod, canon, storeContext } = composables;
  
  switch (predicate.kind) {
    case 'COUNT':
      return await evaluateCountPredicate(predicate, queryResults);
      
    case 'THRESHOLD':
      return await evaluateThresholdPredicate(predicate, queryResults);
      
    case 'ASK':
      return await evaluateAskPredicate(predicate, queryResults, graph);
      
    case 'OWL':
      return await evaluateOwlPredicate(predicate, queryResults, reasoner);
      
    case 'DELTA':
      return await evaluateDeltaPredicate(predicate, queryResults, delta);
      
    case 'ZOD':
      return await evaluateZodPredicate(predicate, queryResults, zod);
      
    default:
      return {
        kind: predicate.kind,
        fired: false,
        reason: `Unknown predicate kind: ${predicate.kind}`,
        duration: 0
      };
  }
}

/**
 * Evaluate COUNT predicate - count query results
 */
async function evaluateCountPredicate(predicate, queryResults) {
  const startTime = Date.now();
  const { operator, value } = predicate.spec;
  
  const count = queryResults.length;
  const fired = compareValues(count, operator, value);
    
    return {
    kind: 'COUNT',
      fired,
    reason: `COUNT ${count} ${operator} ${value} = ${fired}`,
    duration: Date.now() - startTime,
    data: { count }
  };
}

/**
 * Evaluate THRESHOLD predicate - extract numeric values from query results
 */
async function evaluateThresholdPredicate(predicate, queryResults) {
  const startTime = Date.now();
  const { variable, operator, value } = predicate.spec;
  
  // Extract values for the specified variable
  const values = queryResults
    .map(row => {
      const term = row[variable];
      if (term && typeof term === 'object' && term.termType === 'Literal') {
        return parseFloat(term.value);
      }
      return typeof term === 'string' ? parseFloat(term) : term;
    })
    .filter(val => !isNaN(val));
  
  if (values.length === 0) {
    return {
      kind: 'THRESHOLD',
      fired: false,
      reason: `No numeric values found for variable ${variable}`,
      duration: Date.now() - startTime
    };
  }
  
  // Use average for threshold comparison
  const averageValue = values.reduce((sum, val) => sum + val, 0) / values.length;
  const fired = compareValues(averageValue, operator, value);
  
  return {
    kind: 'THRESHOLD',
    fired,
    reason: `THRESHOLD ${variable} average (${averageValue.toFixed(2)}) ${operator} ${value} = ${fired}`,
    duration: Date.now() - startTime,
    data: { averageValue, values }
  };
}

/**
 * Evaluate ASK predicate using useGraph
 */
async function evaluateAskPredicate(predicate, queryResults, graph) {
  const startTime = Date.now();
  const { query, expected = true } = predicate.spec;
  
  try {
    const result = await graph.ask(query);
    const fired = result === expected;
    
    return {
      kind: 'ASK',
      fired,
      reason: `ASK query ${fired ? 'matched' : 'did not match'} expected result (${expected})`,
      duration: Date.now() - startTime,
      data: { result, expected }
    };
  } catch (error) {
    return {
      kind: 'ASK',
      fired: false,
      reason: `ASK predicate failed: ${error.message}`,
      duration: Date.now() - startTime
    };
  }
}

/**
 * Evaluate OWL predicate using useReasoner
 */
async function evaluateOwlPredicate(predicate, queryResults, reasoner) {
  const startTime = Date.now();
  const { rules } = predicate.spec;
  
  try {
    const result = await reasoner.infer(rules);
    
    return {
      kind: 'OWL',
      fired: result.success && result.newTriples > 0,
      reason: `OWL reasoning ${result.success ? 'succeeded' : 'failed'} (${result.newTriples} new triples)`,
      duration: Date.now() - startTime,
      data: { newTriples: result.newTriples, success: result.success }
    };
  } catch (error) {
    return {
      kind: 'OWL',
      fired: false,
      reason: `OWL predicate failed: ${error.message}`,
      duration: Date.now() - startTime
    };
  }
}

/**
 * Evaluate DELTA predicate using useDelta
 */
async function evaluateDeltaPredicate(predicate, queryResults, delta) {
  const startTime = Date.now();
  const { compareWith, operator, value } = predicate.spec;
  
  try {
    const changes = delta.compareWith(compareWith);
    const changeCount = changes.addedCount + changes.removedCount;
    const fired = compareValues(changeCount, operator, value);
    
    return {
      kind: 'DELTA',
      fired,
      reason: `DELTA changes (${changeCount}) ${operator} ${value} = ${fired}`,
      duration: Date.now() - startTime,
      data: { changeCount, changes }
    };
  } catch (error) {
    return {
      kind: 'DELTA',
      fired: false,
      reason: `DELTA predicate failed: ${error.message}`,
      duration: Date.now() - startTime
    };
  }
}

/**
 * Evaluate ZOD predicate using useZod
 */
async function evaluateZodPredicate(predicate, queryResults, zod) {
  const startTime = Date.now();
  const { schema } = predicate.spec;
  
  try {
    // Convert query results to plain objects for Zod validation
    const plainResults = queryResults.map(row => {
      const plainRow = {};
      for (const [key, term] of Object.entries(row)) {
        if (term && typeof term === 'object' && term.termType === 'Literal') {
          plainRow[key] = term.value;
        } else if (term && typeof term === 'object' && term.termType === 'NamedNode') {
          plainRow[key] = term.value;
        } else {
          plainRow[key] = term;
        }
      }
      return plainRow;
    });
    
    // Validate each result
    const validationResults = plainResults.map(result => zod.validate(result, schema));
    const allValid = validationResults.every(vr => vr.success);
    
    return {
      kind: 'ZOD',
      fired: allValid,
      reason: `ZOD validation ${allValid ? 'passed' : 'failed'} (${validationResults.filter(vr => !vr.success).length} failures)`,
      duration: Date.now() - startTime,
      data: { validationResults, allValid }
    };
  } catch (error) {
    return {
      kind: 'ZOD',
      fired: false,
      reason: `ZOD predicate failed: ${error.message}`,
      duration: Date.now() - startTime
    };
  }
}

/**
 * Compare two values using the specified operator
 * @param {number} a - First value
 * @param {string} operator - Comparison operator
 * @param {number} b - Second value
 * @returns {boolean} Comparison result
 */
function compareValues(a, operator, b) {
  switch (operator) {
    case '>': return a > b;
    case '<': return a < b;
    case '>=': return a >= b;
    case '<=': return a <= b;
    case '==': return a === b;
    case '!=': return a !== b;
    default: return false;
  }
}

/**
 * Create a knowledge hooks composable using existing composables
 * @param {Object} [options] - Options
 * @returns {Object} Knowledge hooks interface
 */
export function useKnowledgeHooks(options = {}) {
  const storeContext = useStoreContext();
  const engine = storeContext.engine;
  
  return {
    /**
     * The underlying RDF engine
     * @type {RdfEngine}
     */
    get engine() {
      return engine;
    },
    
    /**
     * Define a hook (legacy query-based system)
     * @param {Object} config - Hook configuration
     * @returns {Object} Hook definition
     */
    defineHook,
    
    /**
     * Evaluate a hook (legacy query-based system)
     * @param {Object} hook - Hook definition
     * @param {Object} options - Evaluation options
     * @returns {Promise<Object>} Evaluation result
     */
    evaluateHook,
    
    /**
     * Evaluate multiple hooks (legacy query-based system)
     * @param {Array<Object>} hooks - Hook definitions
     * @param {Object} options - Evaluation options
     * @returns {Promise<Array<Object>>} Evaluation results
     */
    async evaluateHooks(hooks, options = {}) {
      return Promise.all(hooks.map(hook => evaluateHook(hook, options)));
    },
    
    /**
     * Register an event-based hook
     * @param {string|Array<string>} events - Event name(s) to listen to
     * @param {Function} handler - Event handler function
     * @param {Object} [hookOptions] - Hook options
     * @returns {Object} Registration result with unregister function
     * 
     * @example
     * // Register hook for quad additions
     * const unregister = hooks.registerEventHook(
     *   EVENTS.AFTER_ADD_QUAD,
     *   async (payload) => {
     *     console.log('Quad added:', payload.quad.subject.value);
     *   },
     *   { id: 'quad-monitor' }
     * );
     * 
     * // Register hook for multiple events
     * const unregister2 = hooks.registerEventHook(
     *   [EVENTS.AFTER_ADD_QUAD, EVENTS.AFTER_REMOVE_QUAD],
     *   async (payload) => {
     *     console.log('Store modified:', payload.event);
     *   }
     * );
     */
    registerEventHook(events, handler, hookOptions = {}) {
      const eventList = Array.isArray(events) ? events : [events];
      const registrations = [];
      
      for (const event of eventList) {
        const registration = engine.on(event, handler, hookOptions);
        registrations.push(registration);
      }
      
      return {
        unregister: () => {
          registrations.forEach(reg => reg.unregister());
        }
      };
    },
    
    /**
     * Register a knowledge hook with event triggers
     * @param {Object} config - Hook configuration with events
     * @returns {Object} Registration result with unregister function
     * 
     * @example
     * const hook = defineHook({
     *   id: 'error-monitor',
     *   events: [EVENTS.AFTER_ADD_QUAD],
     *   query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
     *   predicates: [
     *     { kind: 'COUNT', spec: { operator: '>', value: 5 } }
     *   ],
     *   options: {
     *     callback: async (result, payload) => {
     *       console.log('Error count exceeded threshold!');
     *     }
     *   }
     * });
     * 
     * const unregister = hooks.registerKnowledgeHook(hook);
     */
    registerKnowledgeHook(hook) {
      const eventList = hook.events || [];
      const registrations = [];
      
      for (const event of eventList) {
        const handler = async (payload) => {
          try {
            const result = await evaluateHook(hook, payload);
            if (result.fired && hook.options?.callback) {
              await hook.options.callback(result, payload);
            }
          } catch (error) {
            console.error(`Knowledge hook ${hook.id} failed:`, error);
          }
        };
        
        const registration = engine.on(event, handler, {
          id: hook.id,
          ...hook.options
        });
        registrations.push(registration);
      }
      
      return {
        unregister: () => {
          registrations.forEach(reg => reg.unregister());
        }
      };
    },
    
    /**
     * Start batch mode for multiple operations
     * @param {Function} operations - Function containing operations
     * @returns {Promise<boolean>} True if all operations were allowed
     * 
     * @example
     * await hooks.batch(() => {
     *   store.addQuad(s1, p1, o1);
     *   store.addQuad(s2, p2, o2);
     *   store.addQuad(s3, p3, o3);
     *   // Only one batch event fired
     * });
     */
    async batch(operations) {
      engine.store.startBatch();
      try {
        operations();
      } finally {
        return await engine.store.endBatch();
      }
    },
    
    /**
     * Get event statistics
     * @returns {Object} Event bus statistics
     */
    getEventStats() {
      return engine.getEventStats();
    },
    
    /**
     * Enable or disable event emission
     * @param {boolean} enabled - Whether to enable events
     */
    setEventEnabled(enabled) {
      engine.setEventEnabled(enabled);
    },
    
    /**
     * Clear all hooks and reset statistics
     */
    clearHooks() {
      engine.clearHooks();
    },
    
    /**
     * Get statistics from evaluation results (legacy)
     * @param {Array<Object>} results - Evaluation results
     * @returns {Object} Statistics
     */
    getStats(results) {
      const total = results.length;
      const fired = results.filter(r => r.fired).length;
      const avgDuration = results.reduce((sum, r) => sum + r.duration, 0) / total;
      const errors = results.filter(r => r.error).length;
      
      return {
        total,
        fired,
        fireRate: total > 0 ? fired / total : 0,
        avgDuration,
        errors,
        errorRate: total > 0 ? errors / total : 0,
        predicates: results.reduce((acc, r) => {
          if (r.data && r.data.predicateResults) {
            r.data.predicateResults.forEach(p => {
            acc[p.kind] = (acc[p.kind] || 0) + 1;
          });
          }
          return acc;
        }, {})
      };
    }
  };
}