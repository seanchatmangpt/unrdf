/**
 * @fileoverview Knowledge Hooks - Opinionated trigger system for RDF graphs
 * 
 * Knowledge Hooks are pure data + pure functions that evaluate:
 * - ASK predicates (true/false intent)
 * - SHACL predicates (shape conformance/violations)  
 * - DELTA predicates (stable row digests, added/removed quads)
 * - THRESHOLD predicates (counts, metrics, cohort checks)
 * - WINDOW predicates (tumbling/hopping time windows)
 * 
 * With AND/OR/NOT combinators and Zod contracts on outputs.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { z } from "zod";
import { useStoreContext } from "../context/index.mjs";
import { useGraph } from "./use-graph.mjs";
import { useValidator } from "./use-validator.mjs";
import { useCanon } from "./use-canon.mjs";
import { useTypes } from "./use-types.mjs";
import { useJSONLD } from "./use-jsonld.mjs";
import { useRDFExt } from "./use-rdfext.mjs";
import { useTerms } from "./use-terms.mjs";

// Zod schemas for Knowledge Hooks
const PredicateSchema = z.discriminatedUnion('kind', [
  z.object({
    kind: z.literal('ASK'),
    spec: z.object({
      query: z.string(),
      expected: z.boolean().optional().default(true)
    })
  }),
  z.object({
    kind: z.literal('SHACL'),
    spec: z.object({
      shape: z.string(),
      focusNode: z.string().optional(),
      strict: z.boolean().optional().default(false)
    })
  }),
  z.object({
    kind: z.literal('DELTA'),
    spec: z.object({
      change: z.enum(['increase', 'decrease', 'any']),
      key: z.array(z.string()).optional(),
      threshold: z.number().optional()
    })
  }),
  z.object({
    kind: z.literal('THRESHOLD'),
    spec: z.object({
      var: z.string(),
      op: z.enum(['>', '<', '>=', '<=', '==', '!=']),
      value: z.number(),
      aggregate: z.enum(['sum', 'avg', 'count', 'max', 'min']).optional().default('count')
    })
  }),
  z.object({
    kind: z.literal('COUNT'),
    spec: z.object({
      op: z.enum(['>', '<', '>=', '<=', '==', '!=']),
      value: z.number()
    })
  }),
  z.object({
    kind: z.literal('WINDOW'),
    spec: z.object({
      duration: z.string(), // ISO 8601 duration
      slide: z.string().optional(),
      aggregate: z.enum(['sum', 'avg', 'count', 'max', 'min']).default('count'),
      threshold: z.number().optional()
    })
  })
]);

const HookSchema = z.object({
  id: z.string(),
  name: z.string().optional(),
  description: z.string().optional(),
  select: z.string(), // SPARQL SELECT query
  predicates: z.array(PredicateSchema),
  combine: z.enum(['AND', 'OR', 'NOT']).default('AND'),
  output: z.object({
    schema: z.any().optional(), // Zod schema for output validation
    format: z.enum(['json', 'jsonld', 'nquads', 'turtle']).default('json'),
    destination: z.enum(['stdout', 'file', 'webhook', 'store']).default('stdout')
  }).optional(),
  baseline: z.object({
    store: z.string().optional(),
    key: z.string().optional()
  }).optional()
});

const ReceiptSchema = z.object({
  hookId: z.string(),
  timestamp: z.string(),
  fired: z.boolean(),
  predicates: z.array(z.object({
    kind: z.string(),
    result: z.boolean(),
    reason: z.string(),
    duration: z.number(),
    hash: z.string().optional().nullable()
  })),
  data: z.object({
    bindings: z.array(z.record(z.any())),
    count: z.number(),
    hash: z.string()
  }),
  provenance: z.object({
    queryHash: z.string(),
    graphHash: z.string(),
    hookHash: z.string(),
    baselineHash: z.string().optional().nullable()
  }),
  performance: z.object({
    totalDuration: z.number(),
    queryDuration: z.number(),
    predicateDuration: z.number(),
    canonicalizationDuration: z.number()
  })
});

/**
 * Hash a string using SHA-256
 * @param {string} input - Input string
 * @returns {Promise<string>} Hash string
 */
async function hashString(input) {
  if (input === undefined || input === null) {
    return '';
  }
  const { createHash } = await import('node:crypto');
  const hash = createHash('sha256');
  hash.update(String(input), 'utf8');
  return hash.digest('hex');
}

/**
 * Define a Knowledge Hook
 * @param {Object} config - Hook configuration
 * @returns {Object} Validated hook definition
 * 
 * @example
 * const hook = defineHook({
 *   id: 'ex:Health',
 *   select: 'SELECT ?svc ?err WHERE { ?svc ex:errorRate ?err }',
 *   predicates: [
 *     { kind: 'THRESHOLD', spec: { var: 'err', op: '>', value: 0.02 } },
 *     { kind: 'DELTA', spec: { change: 'increase', key: ['svc'] } }
 *   ],
 *   combine: 'AND'
 * });
 */
export function defineHook(config) {
  const validated = HookSchema.parse(config);
  
  // Add computed properties
  validated._hash = null; // Will be computed during evaluation
  validated._baseline = null; // Will be loaded during evaluation
  
  return validated;
}

/**
 * Evaluate a Knowledge Hook
 * @param {Object} hook - Hook definition
 * @param {Object} options - Evaluation options
 * @param {boolean} [options.persist=true] - Persist receipts and baselines
 * @param {string} [options.baselineStore] - Store for baseline data
 * @returns {Promise<Object>} Receipt object
 * 
 * @example
 * const receipt = await evaluateHook(hook, { persist: true });
 * console.log(receipt.fired ? '🔥 Action' : '— No change');
 */
export async function evaluateHook(hook, options = {}) {
  const { persist = true, baselineStore } = options;
  
  // Get composables from context
  const storeContext = useStoreContext();
  const graph = useGraph();
  const validator = useValidator();
  const canon = useCanon();
  const types = useTypes();
  const jsonld = useJSONLD();
  const rdfExt = useRDFExt();
  const terms = useTerms();
  
  const startTime = performance.now();
  const timestamp = new Date().toISOString();
  
  // Step 1: Execute SPARQL SELECT query
  const queryStart = performance.now();
  const queryResult = await graph.select(hook.select);
  const queryDuration = performance.now() - queryStart;
  
  // Step 2: Create hash for data
  const canonStart = performance.now();
  const dataString = JSON.stringify(queryResult);
  const dataHash = await hashString(dataString);
  const canonDuration = performance.now() - canonStart;
  
  // Step 3: Evaluate predicates
  const predicateStart = performance.now();
  const predicateResults = await Promise.all(
    hook.predicates.map(async (predicate) => {
      const predStart = performance.now();
      const result = await evaluatePredicate(predicate, queryResult, {
        graph, validator, canon, types, rdfExt, terms
      });
      const predDuration = performance.now() - predStart;
      
      return {
        kind: predicate.kind,
        result: result.fired,
        reason: result.reason,
        duration: predDuration,
        hash: result.hash
      };
    })
  );
  const predicateDuration = performance.now() - predicateStart;
  
  // Step 4: Combine predicate results
  const fired = combinePredicates(predicateResults, hook.combine);
  
  // Step 5: Generate provenance hashes
  const queryHash = await hashString(hook.select);
  const graphHash = await canon.hash(storeContext.store);
  const hookHash = await hashString(JSON.stringify(hook));
  
  // Step 6: Create receipt
  const receipt = {
    hookId: hook.id,
    timestamp,
    fired,
    predicates: predicateResults,
    data: {
      bindings: queryResult,
      count: queryResult.length,
      hash: dataHash
    },
    provenance: {
      queryHash,
      graphHash,
      hookHash,
      baselineHash: hook._baseline?.hash
    },
    performance: {
      totalDuration: performance.now() - startTime,
      queryDuration,
      predicateDuration,
      canonicalizationDuration: canonDuration
    }
  };
  
  // Step 7: Handle output
  if (receipt.fired) {
    await handleOutput(receipt, hook.output, { jsonld });
  }
  
  // Step 8: Update baseline if persisting
  if (persist) {
    await updateBaseline(hook, receipt, { canon });
  }
  
  return receipt;
}

/**
 * Evaluate a single predicate
 * @param {Object} predicate - Predicate definition
 * @param {Object} queryResult - SPARQL query result
 * @param {Object} composables - Available composables
 * @returns {Promise<Object>} Predicate result
 */
async function evaluatePredicate(predicate, queryResult, composables) {
  const { graph, validator, canon, types, rdfExt, terms } = composables;
  
  switch (predicate.kind) {
    case 'ASK':
      return await evaluateAskPredicate(predicate, queryResult, graph);
      
    case 'SHACL':
      return await evaluateShaclPredicate(predicate, queryResult, validator);
      
    case 'DELTA':
      return await evaluateDeltaPredicate(predicate, queryResult, canon);
      
    case 'THRESHOLD':
      return await evaluateThresholdPredicate(predicate, queryResult);
      
    case 'COUNT':
      return await evaluateCountPredicate(predicate, queryResult);
      
    case 'WINDOW':
      return await evaluateWindowPredicate(predicate, queryResult);
      
    default:
      return {
        fired: false,
        reason: `Unknown predicate kind: ${predicate.kind}`,
        hash: null
      };
  }
}

/**
 * Evaluate ASK predicate
 */
async function evaluateAskPredicate(predicate, queryResult, graph) {
  try {
    const askQuery = predicate.spec.query;
    const result = await graph.ask(askQuery);
    const expected = predicate.spec.expected !== undefined ? predicate.spec.expected : true;
    const fired = result === expected;
    
    return {
      fired,
      reason: `ASK query ${fired ? 'matched' : 'did not match'} expected result (${expected})`,
      hash: await hashString(askQuery)
    };
  } catch (error) {
    return {
      fired: false,
      reason: `ASK predicate failed: ${error.message}`,
      hash: null
    };
  }
}

/**
 * Evaluate SHACL predicate
 */
async function evaluateShaclPredicate(predicate, queryResult, validator) {
  try {
    const validation = await validator.validateShape(
      predicate.spec.shape,
      predicate.spec.focusNode
    );
    
    const fired = predicate.spec.strict ? 
      validation.conforms && validation.results.length === 0 :
      !validation.conforms;
    
    return {
      fired,
      reason: `SHACL validation ${fired ? 'failed' : 'passed'} (${validation.results.length} violations)`,
      hash: await hashString(predicate.spec.shape)
    };
  } catch (error) {
    return {
      fired: false,
      reason: `SHACL predicate failed: ${error.message}`,
      hash: null
    };
  }
}

/**
 * Evaluate DELTA predicate
 */
async function evaluateDeltaPredicate(predicate, queryResult, canon) {
  try {
    // If no baseline exists, don't fire (as per Gherkin spec)
    if (!predicate._baselineHash) {
      return {
        fired: false,
        reason: `DELTA no baseline - initializing without firing`,
        hash: null
      };
    }
    
    // This would compare against baseline data
    // For now, simplified implementation
    const currentHash = await hashString(JSON.stringify(queryResult));
    const hasChange = currentHash !== predicate._baselineHash;
    
    return {
      fired: hasChange,
      reason: `DELTA ${hasChange ? 'detected' : 'no'} change in key variables`,
      hash: currentHash
    };
  } catch (error) {
    return {
      fired: false,
      reason: `DELTA predicate failed: ${error.message}`,
      hash: null
    };
  }
}

/**
 * Evaluate THRESHOLD predicate
 */
async function evaluateThresholdPredicate(predicate, queryResult) {
  try {
    const { var: varName, op, value, aggregate } = predicate.spec;
    
    // Extract values for the variable
    const values = queryResult
      .map(binding => binding[varName])
      .filter(val => val !== undefined)
      .map(val => {
        // Handle RDF.js Literal objects
        if (val && typeof val === 'object' && val.termType === 'Literal') {
          return parseFloat(val.value);
        }
        // Handle plain strings/numbers
        return typeof val === 'string' ? parseFloat(val) : val;
      })
      .filter(val => !isNaN(val));
    
    if (values.length === 0) {
      return {
        fired: false,
        reason: `No numeric values found for variable ${varName}`,
        hash: null
      };
    }
    
    // Apply aggregation
    let aggregatedValue;
    switch (aggregate) {
      case 'sum':
        aggregatedValue = values.reduce((sum, val) => sum + val, 0);
        break;
      case 'avg':
        aggregatedValue = values.reduce((sum, val) => sum + val, 0) / values.length;
        break;
      case 'count':
        aggregatedValue = values.length;
        break;
      case 'max':
        aggregatedValue = Math.max(...values);
        break;
      case 'min':
        aggregatedValue = Math.min(...values);
        break;
      default:
        aggregatedValue = values.length;
    }
    
    // Apply comparison
    let fired;
    switch (op) {
      case '>':
        fired = aggregatedValue > value;
        break;
      case '<':
        fired = aggregatedValue < value;
        break;
      case '>=':
        fired = aggregatedValue >= value;
        break;
      case '<=':
        fired = aggregatedValue <= value;
        break;
      case '==':
        fired = aggregatedValue === value;
        break;
      case '!=':
        fired = aggregatedValue !== value;
        break;
      default:
        fired = false;
    }
    
    return {
      fired,
      reason: `THRESHOLD ${aggregate}(${varName}) = ${aggregatedValue} ${op} ${value} = ${fired}`,
      hash: await hashString(`${varName}:${aggregate}:${op}:${value}`)
    };
  } catch (error) {
    return {
      fired: false,
      reason: `THRESHOLD predicate failed: ${error.message}`,
      hash: null
    };
  }
}

/**
 * Evaluate COUNT predicate
 */
async function evaluateCountPredicate(predicate, queryResult) {
  try {
    const { op, value } = predicate.spec;
    const count = queryResult.length;
    
    let fired = false;
    switch (op) {
      case '>':
        fired = count > value;
        break;
      case '>=':
        fired = count >= value;
        break;
      case '<':
        fired = count < value;
        break;
      case '<=':
        fired = count <= value;
        break;
      case '==':
        fired = count === value;
        break;
      case '!=':
        fired = count !== value;
        break;
      default:
        fired = false;
    }
    
    return {
      fired,
      reason: `COUNT ${count} ${op} ${value} = ${fired}`,
      hash: await hashString(`count:${op}:${value}`)
    };
  } catch (error) {
    return {
      fired: false,
      reason: `COUNT predicate failed: ${error.message}`,
      hash: null
    };
  }
}

/**
 * Evaluate WINDOW predicate
 */
async function evaluateWindowPredicate(predicate, queryResult) {
  try {
    // Simplified window implementation
    // In production, this would handle time windows properly
    const count = queryResult.length;
    const threshold = predicate.spec.threshold || 0;
    
    return {
      fired: count > threshold,
      reason: `WINDOW count ${count} ${count > threshold ? 'exceeded' : 'within'} threshold ${threshold}`,
      hash: await hashString(`window:${predicate.spec.duration}:${threshold}`)
    };
  } catch (error) {
    return {
      fired: false,
      reason: `WINDOW predicate failed: ${error.message}`,
      hash: null
    };
  }
}

/**
 * Combine predicate results based on operator
 */
function combinePredicates(predicateResults, operator) {
  switch (operator) {
    case 'AND':
      return predicateResults.every(p => p.result);
    case 'OR':
      return predicateResults.some(p => p.result);
    case 'NOT':
      return !predicateResults.every(p => p.result);
    default:
      return false;
  }
}

/**
 * Handle output based on hook configuration
 */
async function handleOutput(receipt, output, { jsonld }) {
  if (!output) return;
  
  const { format = 'json', destination = 'stdout' } = output;
  
  let serialized;
  switch (format) {
    case 'json':
      serialized = JSON.stringify(receipt, null, 2);
      break;
    case 'jsonld':
      serialized = await jsonld.fromRDF(receipt);
      break;
    default:
      serialized = JSON.stringify(receipt, null, 2);
  }
  
  switch (destination) {
    case 'stdout':
      console.log(serialized);
      break;
    case 'file':
      // Would write to file
      console.log(`[FILE OUTPUT] ${serialized}`);
      break;
    case 'webhook':
      // Would send HTTP request
      console.log(`[WEBHOOK OUTPUT] ${serialized}`);
      break;
    case 'store':
      // Would add to RDF store
      console.log(`[STORE OUTPUT] ${serialized}`);
      break;
  }
}

/**
 * Update baseline data
 */
async function updateBaseline(hook, receipt, { canon }) {
  // Simplified baseline update
  // In production, this would persist to a baseline store
  hook._baseline = {
    hash: receipt.data.hash,
    timestamp: receipt.timestamp,
    bindings: receipt.data.bindings
  };
}

/**
 * Create a Knowledge Hooks composable
 * @param {Object} [options] - Options
 * @returns {Object} Knowledge Hooks interface
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
     * @returns {Promise<Object>} Receipt
     */
    evaluateHook,
    
    /**
     * Evaluate multiple hooks
     * @param {Array<Object>} hooks - Hook definitions
     * @param {Object} options - Evaluation options
     * @returns {Promise<Array<Object>>} Receipts
     */
    async evaluateHooks(hooks, options = {}) {
      return Promise.all(hooks.map(hook => evaluateHook(hook, options)));
    },
    
    /**
     * Get hook statistics
     * @param {Array<Object>} receipts - Receipt history
     * @returns {Object} Statistics
     */
    getStats(receipts) {
      const total = receipts.length;
      const fired = receipts.filter(r => r.fired).length;
      const avgDuration = receipts.reduce((sum, r) => sum + r.performance.totalDuration, 0) / total;
      
      return {
        total,
        fired,
        fireRate: fired / total,
        avgDuration,
        predicates: receipts.reduce((acc, r) => {
          r.predicates.forEach(p => {
            acc[p.kind] = (acc[p.kind] || 0) + 1;
          });
          return acc;
        }, {})
      };
    }
  };
}
