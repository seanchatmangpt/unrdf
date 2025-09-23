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

import { RdfEngine } from "../engines/RdfEngine.mjs";
import { useGraph } from "./useGraph.mjs";

// Create a single, shared instance of the engine for efficiency
const rdfEngine = new RdfEngine();

/**
 * Create a reasoner composable
 * 
 * @param {Object} [options] - Reasoner options
 * @param {number} [options.timeoutMs=30000] - Reasoning timeout
 * @param {Function} [options.onMetric] - Metrics callback
 * @returns {Object} Reasoner interface
 * 
 * @example
 * const reasoner = useReasoner();
 * 
 * // Reason over data with rules
 * const inferred = await reasoner.reason(dataStore, rulesStore);
 * 
 * // Reason with timeout
 * const reasoner = useReasoner({ timeoutMs: 60000 });
 */
export function useReasoner(options = {}) {
  const {
    timeoutMs = 30000,
    onMetric
  } = options;

  const engine = new RdfEngine({ 
    timeoutMs,
    onMetric 
  });

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
      const dataStoreInstance = dataStore.store || dataStore;
      const rulesStore = typeof rulesInput === "string" 
        ? engine.parseTurtle(rulesInput)
        : rulesInput.store || rulesInput;
      
      const inferredStore = await engine.reason(dataStoreInstance, rulesStore);
      return useGraph(inferredStore);
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
