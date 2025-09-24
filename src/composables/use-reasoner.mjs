/**
 * @fileoverview useReasoner composable - EYE/N3 reasoning operations
 * 
 * This composable provides reasoning capabilities using the EYE reasoner.
 * It enforces the "One Reasoner Rule" - EYE is the only reasoning engine.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { Store } from "n3";
import { useStoreContext } from "../context/index.mjs";
import { useGraph } from "./use-graph.mjs";

/**
 * Create a reasoner composable
 * 
 * @param {Object} [options] - Reasoner options
 * @param {number} [options.timeoutMs=30000] - Reasoning timeout
 * @param {Function} [options.onMetric] - Metrics callback
 * @returns {Object} Reasoner interface
 * 
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const reasoner = useReasoner();
 *   
 *   // Reason over the context store with rules
 *   const inferred = await reasoner.reason(null, rulesStore);
 *   
 *   // Reason with timeout
 *   const reasonerWithTimeout = useReasoner({ timeoutMs: 60000 });
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useReasoner(options = {}) {
  const {
    timeoutMs = 30_000,
    onMetric
  } = options;

  // Get the engine from context
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
     * Apply reasoning rules to a data store
     * @param {Store|Object} dataStore - Data store to reason over
     * @param {string|Store|Object} rulesInput - Rules as Turtle string or Store
     * @returns {Promise<Object>} New useGraph instance with inferred triples
     * 
     * @example
     * const inferred = await reasoner.reason(dataStore, rulesStore);
     * console.log(`Inferred ${inferred.size} new triples`);
     * 
     * // Use with Turtle rules
     * const rules = `
     *   @prefix ex: <http://example.org/> .
     *   { ?s ex:parent ?p } => { ?s ex:ancestor ?p } .
     * `;
     * const inferred = await reasoner.reason(dataStore, rules);
     */
    async reason(dataStore, rulesInput) {
      // Get the context store
      const storeContext = useStoreContext();
      const contextStore = storeContext.store;
      
      // Use provided data store or context store
      const dataStoreInstance = dataStore ? (dataStore.store || dataStore) : contextStore;
      const rulesStore = typeof rulesInput === "string" 
        ? engine.parseTurtle(rulesInput)
        : rulesInput.store || rulesInput;
      
      // Handle empty stores gracefully
      if (dataStoreInstance.size === 0 && rulesStore.size === 0) {
        // Return empty graph
        const emptyStore = new Store();
        return createTemporaryGraph(emptyStore, engine);
      }
      
      // Handle empty rules store
      if (rulesStore.size === 0) {
        // Return graph with original data
        return createTemporaryGraph(dataStoreInstance, engine);
      }
      
      // Handle empty data store
      if (dataStoreInstance.size === 0) {
        // Return empty graph
        const emptyStore = new Store();
        return createTemporaryGraph(emptyStore, engine);
      }
      
      const inferredStore = await engine.reason(dataStoreInstance, rulesStore);
      return createTemporaryGraph(inferredStore, engine);
    },

    /**
     * Apply multiple rule sets sequentially
     * @param {Store|Object} dataStore - Data store to reason over
     * @param {Array<string|Store|Object>} rulesInputs - Array of rule sets
     * @returns {Promise<Object>} New useGraph instance with all inferred triples
     * 
     * @example
     * const inferred = await reasoner.reasonSequentially(dataStore, [rules1, rules2, rules3]);
     */
    async reasonSequentially(dataStore, rulesInputs) {
      let currentStore = dataStore.store || dataStore;
      
      for (const rulesInput of rulesInputs) {
        const rulesStore = typeof rulesInput === "string" 
          ? engine.parseTurtle(rulesInput)
          : rulesInput.store || rulesInput;
        
        currentStore = await engine.reason(currentStore, rulesStore);
      }
      
      return useGraph(currentStore);
    },

    /**
     * Apply multiple rule sets in parallel and merge results
     * @param {Store|Object} dataStore - Data store to reason over
     * @param {Array<string|Store|Object>} rulesInputs - Array of rule sets
     * @returns {Promise<Object>} New useGraph instance with merged inferred triples
     * 
     * @example
     * const inferred = await reasoner.reasonParallel(dataStore, [rules1, rules2, rules3]);
     */
    async reasonParallel(dataStore, rulesInputs) {
      const dataStoreInstance = dataStore.store || dataStore;
      
      const promises = rulesInputs.map(rulesInput => {
        const rulesStore = typeof rulesInput === "string" 
          ? engine.parseTurtle(rulesInput)
          : rulesInput.store || rulesInput;
        
        return engine.reason(dataStoreInstance, rulesStore);
      });
      
      const results = await Promise.all(promises);
      const mergedStore = engine.union(...results);
      
      return useGraph(mergedStore);
    },

    /**
     * Get the difference between original and inferred triples
     * @param {Store|Object} originalStore - Original data store
     * @param {Store|Object} inferredStore - Store with inferred triples
     * @returns {Object} New useGraph instance with only new triples
     * 
     * @example
     * const inferred = await reasoner.reason(dataStore, rules);
     * const newTriples = reasoner.getNewTriples(dataStore, inferred);
     * console.log(`Found ${newTriples.size} new triples`);
     */
    getNewTriples(originalStore, inferredStore) {
      const original = originalStore.store || originalStore;
      const inferred = inferredStore.store || inferredStore;
      const newStore = engine.difference(inferred, original);
      return useGraph(newStore);
    },

    /**
     * Check if reasoning would produce new triples
     * @param {Store|Object} dataStore - Data store to reason over
     * @param {string|Store|Object} rulesInput - Rules to apply
     * @returns {Promise<boolean>} True if reasoning would produce new triples
     * 
     * @example
     * const wouldProduceNew = await reasoner.wouldProduceNewTriples(dataStore, rules);
     * if (wouldProduceNew) {
     *   console.log("Reasoning would add new knowledge");
     * }
     */
    async wouldProduceNewTriples(dataStore, rulesInput) {
      const inferred = await this.reason(dataStore, rulesInput);
      const newTriples = this.getNewTriples(dataStore, inferred);
      return newTriples.size > 0;
    },

    /**
     * Get statistics about reasoning results
     * @param {Store|Object} originalStore - Original data store
     * @param {Store|Object} inferredStore - Store with inferred triples
     * @returns {Object} Reasoning statistics
     * 
     * @example
     * const inferred = await reasoner.reason(dataStore, rules);
     * const stats = reasoner.getStats(dataStore, inferred);
     * console.log(`Original: ${stats.original.quads}, Inferred: ${stats.inferred.quads}, New: ${stats.new.quads}`);
     */
    getStats(originalStore, inferredStore) {
      const original = originalStore.store || originalStore;
      const inferred = inferredStore.store || inferredStore;
      const newStore = engine.difference(inferred, original);
      
      return {
        original: engine.getStats(original),
        inferred: engine.getStats(inferred),
        new: engine.getStats(newStore),
        growth: {
          absolute: inferred.size - original.size,
          percentage: original.size > 0 ? ((inferred.size - original.size) / original.size) * 100 : 0
        }
      };
    },

    /**
     * Create a reasoning pipeline with multiple steps
     * @param {Array<Object>} steps - Array of reasoning steps
     * @returns {Object} Pipeline interface
     * 
     * @example
     * const pipeline = reasoner.createPipeline([
     *   { name: "transitive", rules: transitiveRules },
     *   { name: "symmetric", rules: symmetricRules },
     *   { name: "inverse", rules: inverseRules }
     * ]);
     * 
     * const result = await pipeline.execute(dataStore);
     */
    createPipeline(steps) {
      return {
        steps,
        
        /**
         * Execute the reasoning pipeline
         * @param {Store|Object} dataStore - Data store to reason over
         * @returns {Promise<Object>} Final useGraph instance
         */
        async execute(dataStore) {
          let currentStore = dataStore.store || dataStore;
          const results = [];
          
          for (const step of this.steps) {
            const startTime = performance.now();
            const rulesStore = typeof step.rules === "string" 
              ? engine.parseTurtle(step.rules)
              : step.rules.store || step.rules;
            
            const beforeSize = currentStore.size;
            currentStore = await engine.reason(currentStore, rulesStore);
            const afterSize = currentStore.size;
            const duration = performance.now() - startTime;
            
            results.push({
              name: step.name,
              beforeSize,
              afterSize,
              newTriples: afterSize - beforeSize,
              duration
            });
          }
          
          return {
            result: useGraph(currentStore),
            steps: results
          };
        }
      };
    }
  };
}

/**
 * Create a temporary graph interface for a specific store
 * Used for operations that return new stores (reasoning results)
 * @param {Store} store - The store to wrap
 * @param {RdfEngine} engine - The RDF engine to use
 * @returns {Object} Graph interface
 * @private
 */
function createTemporaryGraph(store, engine) {
  return {
    get store() {
      return store;
    },
    
    get engine() {
      return engine;
    },
    
    async query(sparql, options) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useReasoner] SPARQL query must be a string");
      }
      try {
        return await engine.query(store, sparql, options);
      } catch (error) {
        throw new Error(`[useReasoner] Query failed: ${error.message}`);
      }
    },
    
    async select(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useReasoner] SPARQL query must be a string");
      }
      try {
        const res = await engine.query(store, sparql);
        if (res.type !== "select") {
          throw new Error("[useReasoner] Query is not a SELECT query");
        }
        return res.results;
      } catch (error) {
        throw new Error(`[useReasoner] SELECT query failed: ${error.message}`);
      }
    },
    
    async ask(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useReasoner] SPARQL query must be a string");
      }
      try {
        const res = await engine.query(store, sparql);
        if (res.type !== "ask") {
          throw new Error("[useReasoner] Query is not an ASK query");
        }
        return res.boolean;
      } catch (error) {
        throw new Error(`[useReasoner] ASK query failed: ${error.message}`);
      }
    },
    
    async construct(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useReasoner] SPARQL query must be a string");
      }
      try {
        const res = await engine.query(store, sparql);
        if (res.type !== "construct") {
          throw new Error("[useReasoner] Query is not a CONSTRUCT query");
        }
        return res.store;
      } catch (error) {
        throw new Error(`[useReasoner] CONSTRUCT query failed: ${error.message}`);
      }
    },
    
    async update(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useReasoner] SPARQL query must be a string");
      }
      try {
        const res = await engine.query(store, sparql);
        if (res.type !== "update") {
          throw new Error("[useReasoner] Query is not an UPDATE query");
        }
        return res;
      } catch (error) {
        throw new Error(`[useReasoner] UPDATE query failed: ${error.message}`);
      }
    },
    
    async validate(shapesInput) {
      try {
        return await engine.validateShacl(store, shapesInput);
      } catch (error) {
        throw new Error(`[useReasoner] Validation failed: ${error.message}`);
      }
    },
    
    async validateOrThrow(shapesInput) {
      try {
        return await engine.validateShaclOrThrow(store, shapesInput);
      } catch (error) {
        throw new Error(`[useReasoner] Validation failed: ${error.message}`);
      }
    },
    
    async serialize(options = {}) {
      if (options && typeof options !== 'object') {
        throw new TypeError("[useReasoner] serialize options must be an object");
      }
      
      const { format = "Turtle", prefixes } = options;
      
      try {
        if (format === "Turtle") {
          return await engine.serializeTurtle(store, { prefixes });
        }
        if (format === "N-Quads") {
          return await engine.serializeNQuads(store);
        }
        
        throw new Error(`[useReasoner] Unsupported serialization format: ${format}`);
      } catch (error) {
        throw new Error(`[useReasoner] Serialization failed: ${error.message}`);
      }
    },
    
    pointer() {
      try {
        return engine.getClownface(store);
      } catch (error) {
        throw new Error(`[useReasoner] Pointer creation failed: ${error.message}`);
      }
    },
    
    stats() {
      try {
        return engine.getStats(store);
      } catch (error) {
        throw new Error(`[useReasoner] Stats calculation failed: ${error.message}`);
      }
    },
    
    async isIsomorphic(otherGraph) {
      try {
        const otherStore = otherGraph.store || otherGraph;
        return await engine.isIsomorphic(store, otherStore);
      } catch (error) {
        throw new Error(`[useReasoner] Isomorphism check failed: ${error.message}`);
      }
    },
    
    union(...otherGraphs) {
      try {
        const otherStores = otherGraphs.map(g => g.store || g);
        const resultStore = engine.union(store, ...otherStores);
        return createTemporaryGraph(resultStore, engine);
      } catch (error) {
        throw new Error(`[useReasoner] Union operation failed: ${error.message}`);
      }
    },
    
    difference(otherGraph) {
      try {
        const otherStore = otherGraph.store || otherGraph;
        const resultStore = engine.difference(store, otherStore);
        return createTemporaryGraph(resultStore, engine);
      } catch (error) {
        throw new Error(`[useReasoner] Difference operation failed: ${error.message}`);
      }
    },
    
    intersection(otherGraph) {
      try {
        const otherStore = otherGraph.store || otherGraph;
        const resultStore = engine.intersection(store, otherStore);
        return createTemporaryGraph(resultStore, engine);
      } catch (error) {
        throw new Error(`[useReasoner] Intersection operation failed: ${error.message}`);
      }
    },
    
    skolemize(baseIRI) {
      try {
        const resultStore = engine.skolemize(store, baseIRI);
        return createTemporaryGraph(resultStore, engine);
      } catch (error) {
        throw new Error(`[useReasoner] Skolemization failed: ${error.message}`);
      }
    },
    
    async toJSONLD(options = {}) {
      try {
        return await engine.toJSONLD(store, options);
      } catch (error) {
        throw new Error(`[useReasoner] JSON-LD conversion failed: ${error.message}`);
      }
    },
    
    get size() {
      return store.size;
    }
  };
}