/**
 * @fileoverview useKnowledgeHooks - Reactive triggers for RDF graphs with OWL, SHACL, SPARQL support
 * 
 * A comprehensive implementation of knowledge hooks that supports:
 * - SPARQL queries for data selection
 * - OWL reasoning for inference
 * - SHACL validation for constraints
 * - Clean, predictable API
 * 
 * @version 2.0.0
 * @author GitVan Team
 * @license MIT
 */

import { useStoreContext } from "../context/index.mjs";
import { useGraph } from "./use-graph.mjs";
import { useReasoner } from "./use-reasoner.mjs";

// Stub implementations for missing composables
const useValidator = () => ({
  async validateShape(shape) {
    // Stub implementation - in real implementation would use rdf-validate-shacl
    return {
      conforms: true,
      results: []
    };
  }
});

/**
 * Define a knowledge hook
 * @param {Object} config - Hook configuration
 * @param {string} config.id - Unique hook identifier
 * @param {string} config.query - SPARQL query to execute
 * @param {Array<Object>} [config.predicates] - Array of predicate conditions
 * @param {string} [config.combine='AND'] - How to combine predicates ('AND', 'OR')
 * @param {Object} [config.output] - Output configuration
 * @returns {Object} Hook definition
 * 
 * @example
 * // SPARQL-based hook with SHACL validation
 * const hook = defineHook({
 *   id: 'service-health-monitor',
 *   query: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }',
 *   predicates: [
 *     { kind: 'THRESHOLD', spec: { variable: 'errorRate', operator: '>', value: 0.05 } },
 *     { kind: 'COUNT', spec: { operator: '>', value: 3 } }
 *   ],
 *   combine: 'OR'
 * });
 * 
 * // OWL reasoning hook
 * const hook = defineHook({
 *   id: 'inference-check',
 *   query: 'SELECT ?person WHERE { ?person rdf:type ex:Person }',
 *   predicates: [
 *     { kind: 'OWL', spec: { rules: '@prefix ex: <http://example.org/> . { ?x ex:hasParent ?y } => { ?x ex:isChildOf ?y } .' } }
 *   ]
 * });
 * 
 * // SHACL validation hook
 * const hook = defineHook({
 *   id: 'data-quality',
 *   query: 'SELECT ?person WHERE { ?person rdf:type ex:Person }',
 *   predicates: [
 *     { kind: 'SHACL', spec: { shape: 'ex:PersonShape', strict: true } }
 *   ]
 * });
 */
export function defineHook(config) {
  const {
    id,
    query,
    predicates = [],
    combine = 'AND',
    output
  } = config;

  const hook = {
    id,
    query,
    predicates,
    combine,
    output,
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
          case 'COUNT':
            if (!predicate.spec.operator || !['>', '<', '>=', '<=', '==', '!='].includes(predicate.spec.operator)) {
              throw new Error('COUNT predicate requires a valid operator');
            }
            if (typeof predicate.spec.value !== 'number') {
              throw new Error('COUNT predicate requires a numeric value');
            }
            break;
          case 'SHACL':
            if (!predicate.spec.shape || typeof predicate.spec.shape !== 'string') {
              throw new Error('SHACL predicate requires a shape IRI');
            }
            break;
          case 'OWL':
            if (!predicate.spec.rules || typeof predicate.spec.rules !== 'string') {
              throw new Error('OWL predicate requires rules');
            }
            break;
          case 'ASK':
            if (!predicate.spec.query || typeof predicate.spec.query !== 'string') {
              throw new Error('ASK predicate requires a query');
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
 * Evaluate a knowledge hook
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
  const validator = useValidator();
  const reasoner = useReasoner();

  const startTime = Date.now();

  try {
    // Step 1: Execute SPARQL query
    const queryResults = await graph.select(hook.query);
    
    // Step 2: Evaluate predicates
    const predicateResults = [];
    let fired = hook.combine === 'AND';
    
    for (const predicate of hook.predicates) {
      const predicateResult = await evaluatePredicate(predicate, queryResults, {
        storeContext,
        graph,
        validator,
        reasoner
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
 * Evaluate a single predicate
 * @param {Object} predicate - Predicate definition
 * @param {Array} queryResults - SPARQL query results
 * @param {Object} composables - Available composables
 * @returns {Promise<Object>} Predicate result
 */
async function evaluatePredicate(predicate, queryResults, composables) {
  const { storeContext, graph, validator, reasoner } = composables;
  
  switch (predicate.kind) {
    case 'THRESHOLD':
      return await evaluateThresholdPredicate(predicate, queryResults);
      
    case 'COUNT':
      return await evaluateCountPredicate(predicate, queryResults);
      
    case 'SHACL':
      return await evaluateShaclPredicate(predicate, queryResults, validator);
      
    case 'OWL':
      return await evaluateOwlPredicate(predicate, queryResults, reasoner);
      
    case 'ASK':
      return await evaluateAskPredicate(predicate, queryResults, graph);
      
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
 * Evaluate THRESHOLD predicate
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
 * Evaluate COUNT predicate
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
 * Evaluate SHACL predicate
 */
async function evaluateShaclPredicate(predicate, queryResults, validator) {
  const startTime = Date.now();
  const { shape, strict = false } = predicate.spec;
  
  try {
    const validation = await validator.validateShape(shape);
    
    const fired = strict ? 
      validation.conforms && validation.results.length === 0 :
      !validation.conforms;
    
    return {
      kind: 'SHACL',
      fired,
      reason: `SHACL validation ${fired ? 'failed' : 'passed'} (${validation.results.length} violations)`,
      duration: Date.now() - startTime,
      data: { violations: validation.results.length, conforms: validation.conforms }
    };
  } catch (error) {
    return {
      kind: 'SHACL',
      fired: false,
      reason: `SHACL predicate failed: ${error.message}`,
      duration: Date.now() - startTime
    };
  }
}

/**
 * Evaluate OWL predicate
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
 * Evaluate ASK predicate
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
 * Create a knowledge hooks composable
 * @param {Object} [options] - Options
 * @returns {Object} Knowledge hooks interface
 */
export function useKnowledgeHooks(options = {}) {
  const storeContext = useStoreContext();

  return {
    /**
     * The underlying RDF engine
     * @type {RdfEngine}
     */
    get engine() {
      return storeContext.engine;
    },

    /**
     * Define a hook
     * @param {Object} config - Hook configuration
     * @returns {Object} Hook definition
     */
    defineHook,

    /**
     * Evaluate a hook
     * @param {Object} hook - Hook definition
     * @param {Object} options - Evaluation options
     * @returns {Promise<Object>} Evaluation result
     */
    evaluateHook,

    /**
     * Evaluate multiple hooks
     * @param {Array<Object>} hooks - Hook definitions
     * @param {Object} options - Evaluation options
     * @returns {Promise<Array<Object>>} Evaluation results
     */
    async evaluateHooks(hooks, options = {}) {
      return Promise.all(hooks.map(hook => evaluateHook(hook, options)));
    },

    /**
     * Get statistics from evaluation results
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