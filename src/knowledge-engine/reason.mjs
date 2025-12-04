/**
 * @file Reasoning support using N3 rules.
 * @module reason
 */

import { Parser, Writer } from '@unrdf/core/rdf/n3-justified-only';
import { Store } from 'n3'; // TODO: Replace with Oxigraph Store

// Dynamic import to avoid top-level await issues
let basicQuery;
const loadBasicQuery = async () => {
  if (!basicQuery) {
    const eyereasoner = await import('eyereasoner');
    basicQuery = eyereasoner.basicQuery;
  }
  return basicQuery;
};

/**
 * Run forward-chaining reasoning with N3 rules.
 * @param {Store} store - The store containing data to reason over
 * @param {Store|string} rules - The store or Turtle string containing N3 rules
 * @param {Object} [options] - Reasoning options
 * @param {boolean} [options.includeOriginal] - Include original data in result
 * @param {number} [options.maxIterations] - Maximum number of reasoning iterations
 * @param {boolean} [options.debug] - Enable debug output
 * @returns {Promise<Store>} Promise resolving to a new store containing original and inferred quads
 *
 * @throws {Error} If reasoning fails
 *
 * @example
 * const dataStore = new Store();
 * // ... add data quads to store
 *
 * const rulesTtl = `
 *   @prefix ex: <http://example.org/> .
 *   @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
 *
 *   { ?x ex:parent ?y } => { ?x rdfs:subClassOf ?y } .
 * `;
 *
 * const reasonedStore = await reason(dataStore, rulesTtl);
 * console.log('Original quads:', dataStore.size);
 * console.log('Reasoned quads:', reasonedStore.size);
 */
export async function reason(store, rules, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('reason: store must be a valid Store instance');
  }
  if (!rules) {
    throw new TypeError('reason: rules must be provided');
  }

  const { includeOriginal = true, _maxIterations = 100, debug = false } = options;

  try {
    // Get data quads from store
    const dataQuads = store.getQuads();

    // Convert rules to quads if needed
    let rulesQuads;
    if (typeof rules === 'string') {
      // Parse Turtle rules to quads
      const parser = new Parser();
      rulesQuads = parser.parse(rules);
    } else if (rules && typeof rules.getQuads === 'function') {
      rulesQuads = rules.getQuads();
    } else {
      throw new TypeError('reason: rules must be a Store or Turtle string');
    }

    if (debug) {
      console.log('Data quads:', dataQuads.length);
      console.log('Rules quads:', rulesQuads.length);
    }

    // Run reasoning using EYE reasoner
    // Load basicQuery dynamically to avoid top-level await issues
    const query = await loadBasicQuery();

    // Combine data and rules as EYE expects
    const combinedQuads = [...dataQuads, ...rulesQuads];
    const { result } = await query(combinedQuads, []);

    if (debug) {
      console.log('Result quads:', result.length);
    }

    // Create result store
    const _resultStore = new Store(result);

    // Combine original and inferred data if requested
    if (includeOriginal) {
      return new Store([...dataQuads, ...result]);
    } else {
      // Extract only inferred quads (not in original data)
      const originalQuadSet = new Set(dataQuads.map(q => q.toString()));
      const inferredQuads = result.filter(q => !originalQuadSet.has(q.toString()));
      return new Store(inferredQuads);
    }
  } catch (error) {
    throw new Error(`N3 reasoning failed: ${error.message}`);
  }
}

/**
 * Run reasoning with multiple rule sets.
 * @param {Store} store - The store containing data to reason over
 * @param {Array<Store|string>} rulesList - Array of stores or Turtle strings containing N3 rules
 * @param {Object} [options] - Reasoning options
 * @returns {Promise<Store>} Promise resolving to a new store with all reasoning results
 *
 * @throws {Error} If reasoning fails
 *
 * @example
 * const rulesList = [
 *   rdfsRulesTtl,
 *   owlRulesTtl,
 *   customRulesTtl
 * ];
 *
 * const reasonedStore = await reasonMultiple(store, rulesList);
 */
export async function reasonMultiple(store, rulesList, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('reasonMultiple: store must be a valid Store instance');
  }
  if (!Array.isArray(rulesList) || rulesList.length === 0) {
    throw new TypeError('reasonMultiple: rulesList must be a non-empty array');
  }

  try {
    let currentStore = store;

    for (let i = 0; i < rulesList.length; i++) {
      const rules = rulesList[i];
      if (options.debug) {
        console.log(`Applying rule set ${i + 1}/${rulesList.length}`);
      }

      currentStore = await reason(currentStore, rules, {
        ...options,
        includeOriginal: true, // Always include original data for subsequent rule applications
      });
    }

    return currentStore;
  } catch (error) {
    throw new Error(`Multiple N3 reasoning failed: ${error.message}`);
  }
}

/**
 * Extract only the newly inferred quads from reasoning.
 * @param {Store} originalStore - The original store before reasoning
 * @param {Store} reasonedStore - The store after reasoning
 * @returns {Store} Store containing only the newly inferred quads
 *
 * @example
 * const originalStore = new Store();
 * // ... add original quads
 *
 * const reasonedStore = await reason(originalStore, rules);
 * const inferredOnly = extractInferred(originalStore, reasonedStore);
 * console.log('Newly inferred quads:', inferredOnly.size);
 */
export function extractInferred(originalStore, reasonedStore) {
  if (!originalStore || typeof originalStore.getQuads !== 'function') {
    throw new TypeError('extractInferred: originalStore must be a valid Store instance');
  }
  if (!reasonedStore || typeof reasonedStore.getQuads !== 'function') {
    throw new TypeError('extractInferred: reasonedStore must be a valid Store instance');
  }

  const originalQuads = new Set(originalStore.getQuads().map(q => q.toString()));
  const reasonedQuads = reasonedStore.getQuads();

  const inferredQuads = reasonedQuads.filter(q => !originalQuads.has(q.toString()));

  return new Store(inferredQuads);
}

/**
 * Get reasoning statistics.
 * @param {Store} originalStore - The original store before reasoning
 * @param {Store} reasonedStore - The store after reasoning
 * @returns {Object} Reasoning statistics
 *
 * @example
 * const stats = getReasoningStats(originalStore, reasonedStore);
 * console.log(`Original quads: ${stats.originalCount}`);
 * console.log(`Inferred quads: ${stats.inferredCount}`);
 * console.log(`Total quads: ${stats.totalCount}`);
 * console.log(`Inference ratio: ${stats.inferenceRatio}`);
 */
export function getReasoningStats(originalStore, reasonedStore) {
  if (!originalStore || typeof originalStore.getQuads !== 'function') {
    throw new TypeError('getReasoningStats: originalStore must be a valid Store instance');
  }
  if (!reasonedStore || typeof reasonedStore.getQuads !== 'function') {
    throw new TypeError('getReasoningStats: reasonedStore must be a valid Store instance');
  }

  const originalCount = originalStore.size;
  const totalCount = reasonedStore.size;
  const inferredCount = totalCount - originalCount;
  const inferenceRatio = originalCount > 0 ? inferredCount / originalCount : 0;

  return {
    originalCount,
    inferredCount,
    totalCount,
    inferenceRatio,
    hasInferences: inferredCount > 0,
  };
}

/**
 * Validate N3 rules syntax.
 * @param {Store|string} rules - The store or Turtle string containing N3 rules
 * @returns {Object} Validation result
 *
 * @example
 * const validation = validateRules(rulesTtl);
 * if (!validation.valid) {
 *   console.log('Rule validation errors:', validation.errors);
 * }
 */
export function validateRules(rules) {
  if (!rules) {
    return { valid: false, errors: ['Rules must be provided'] };
  }

  try {
    let rulesTurtle;
    if (typeof rules === 'string') {
      rulesTurtle = rules;
    } else if (rules && typeof rules.getQuads === 'function') {
      const writer = new Writer({ format: 'Turtle' });
      writer.addQuads(rules.getQuads());
      rulesTurtle = writer.quadsToString(rules.getQuads());
    } else {
      return {
        valid: false,
        errors: ['Rules must be a Store or Turtle string'],
      };
    }

    // Basic syntax validation
    const parser = new Parser();
    parser.parse(rulesTurtle);

    return { valid: true, errors: [] };
  } catch (error) {
    return { valid: false, errors: [error.message] };
  }
}

/**
 * Create a reasoning session with persistent state.
 * @param {Store} initialStore - Initial store state
 * @param {Store|string} rules - N3 rules to apply
 * @param {Object} [options] - Session options
 * @returns {Object} Reasoning session object
 *
 * @example
 * const session = createReasoningSession(store, rules);
 *
 * // Add new data
 * session.addData(newQuads);
 *
 * // Apply reasoning
 * await session.reason();
 *
 * // Get current state
 * const currentState = session.getState();
 */
export function createReasoningSession(initialStore, rules, options = {}) {
  if (!initialStore || typeof initialStore.getQuads !== 'function') {
    throw new TypeError('createReasoningSession: initialStore must be a valid Store instance');
  }
  if (!rules) {
    throw new TypeError('createReasoningSession: rules must be provided');
  }

  let currentStore = new Store(initialStore.getQuads());
  const sessionRules = rules;

  return {
    /**
     * Add new data to the session.
     * @param {Array|Quad} quads - Quads to add
     */
    addData(quads) {
      if (Array.isArray(quads)) {
        currentStore.addQuads(quads);
      } else {
        currentStore.add(quads);
      }
    },

    /**
     * Remove data from the session.
     * @param {Array|Quad} quads - Quads to remove
     */
    removeData(quads) {
      if (Array.isArray(quads)) {
        currentStore.removeQuads(quads);
      } else {
        currentStore.delete(quads);
      }
    },

    /**
     * Apply reasoning to current state.
     * @param {Object} [reasonOptions] - Reasoning options
     * @returns {Promise<Store>} Updated store
     */
    async reason(reasonOptions = {}) {
      currentStore = await reason(currentStore, sessionRules, {
        ...options,
        ...reasonOptions,
      });
      return currentStore;
    },

    /**
     * Get current store state.
     * @returns {Store} Current store
     */
    getState() {
      return new Store(currentStore.getQuads());
    },

    /**
     * Reset to initial state.
     */
    reset() {
      currentStore = new Store(initialStore.getQuads());
    },

    /**
     * Get session statistics.
     * @returns {Object} Session statistics
     */
    getStats() {
      return getReasoningStats(initialStore, currentStore);
    },
  };
}
