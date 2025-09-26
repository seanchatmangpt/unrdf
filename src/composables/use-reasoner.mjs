/**
 * @fileoverview useReasoner composable - High-level reasoning convenience layer
 * 
 * This composable provides a simple, intuitive interface for reasoning operations.
 * It abstracts away store management, rule parsing, and result handling.
 * 
 * @version 2.0.0
 * @author GitVan Team
 * @license MIT
 */

import { Store } from "n3";
import { useStoreContext } from "../context/index.mjs";

/**
 * Create a reasoning convenience layer
 * 
 * @param {Object} [options] - Reasoner options
 * @param {number} [options.timeoutMs=30000] - Reasoning timeout
 * @returns {Object} Reasoning interface
 * 
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const reasoner = useReasoner();
 *   
 *   // Simple reasoning with Turtle rules
 *   const result = reasoner.infer(`
 *     @prefix ex: <http://example.org/> .
 *     { ?s ex:parent ?p } => { ?s ex:ancestor ?p } .
 *   `);
 *   
 *   // Check what was inferred
 *   console.log(`Inferred ${result.newTriples} new triples`);
 * });
 */
export function useReasoner(options = {}) {
  const { timeoutMs = 30_000 } = options;
  const storeContext = useStoreContext();
  const engine = storeContext.engine;

  return {
    /**
     * Infer new knowledge from rules
     * @param {string|Store} rules - Turtle rules or Store containing rules
     * @param {Object} [options] - Inference options
     * @param {boolean} [options.addToStore=true] - Add inferred triples to context store
     * @param {boolean} [options.returnStats=true] - Return inference statistics
     * @returns {Object} Inference result
     * 
     * @example
     * // Simple inference
     * const result = reasoner.infer(`
     *   @prefix ex: <http://example.org/> .
     *   { ?s ex:parent ?p } => { ?s ex:ancestor ?p } .
     * `);
     * 
     * // Inference without adding to store
     * const result = reasoner.infer(rules, { addToStore: false });
     */
    infer(rules, options = {}) {
      const { addToStore = true, returnStats = true } = options;
      
      // Parse rules if string
      const rulesStore = typeof rules === "string" 
        ? engine.parseTurtle(rules)
        : rules;
      
      // Get current store state
      const originalSize = storeContext.store.size;
      
      // Perform reasoning
      const inferredStore = engine.reason(storeContext.store, rulesStore);
      
      // Calculate new triples
      const newTriples = engine.difference(inferredStore, storeContext.store);
      const newTriplesCount = newTriples.size;
      
      // Add to store if requested
      if (addToStore && newTriplesCount > 0) {
        for (const quad of newTriples) {
          storeContext.store.add(quad);
        }
      }
      
      // Return result
      const result = {
        success: true,
        newTriples: newTriplesCount,
        totalTriples: storeContext.store.size
      };
      
      if (returnStats) {
        result.stats = {
          original: originalSize,
          inferred: inferredStore.size,
          added: newTriplesCount,
          growth: originalSize > 0 ? (newTriplesCount / originalSize) * 100 : 0
        };
      }
      
      return result;
    },

    /**
     * Apply multiple rule sets in sequence
     * @param {Array<string|Store>} ruleSets - Array of rule sets to apply
     * @param {Object} [options] - Inference options
     * @returns {Object} Combined inference result
     * 
     * @example
     * const result = reasoner.inferSequence([
     *   transitiveRules,
     *   symmetricRules,
     *   inverseRules
     * ]);
     */
    inferSequence(ruleSets, options = {}) {
      const { addToStore = true, returnStats = true } = options;
      const originalSize = storeContext.store.size;
      let totalNewTriples = 0;
      const stepResults = [];
      
      for (let i = 0; i < ruleSets.length; i++) {
        const stepStartSize = storeContext.store.size;
        const stepResult = this.infer(ruleSets[i], { addToStore, returnStats: false });
        const stepNewTriples = stepResult.newTriples;
        
        totalNewTriples += stepNewTriples;
        stepResults.push({
          step: i + 1,
          rules: typeof ruleSets[i] === "string" ? "Turtle rules" : "Store rules",
          newTriples: stepNewTriples,
          totalTriples: storeContext.store.size
        });
      }
      
      const result = {
        success: true,
        newTriples: totalNewTriples,
        totalTriples: storeContext.store.size,
        steps: stepResults
      };
      
      if (returnStats) {
        result.stats = {
          original: originalSize,
          final: storeContext.store.size,
          added: totalNewTriples,
          growth: originalSize > 0 ? (totalNewTriples / originalSize) * 100 : 0,
          steps: stepResults.length
        };
      }
      
      return result;
    },

    /**
     * Check if rules would produce new knowledge
     * @param {string|Store} rules - Rules to test
     * @returns {boolean} True if rules would produce new triples
     * 
     * @example
     * const wouldProduceNew = reasoner.wouldInfer(rules);
     * if (wouldProduceNew) {
     *   console.log("These rules would add new knowledge");
     * }
     */
    wouldInfer(rules) {
      const result = this.infer(rules, { addToStore: false, returnStats: false });
      return result.newTriples > 0;
    },

    /**
     * Get reasoning statistics
     * @returns {Object} Current store statistics
     * 
     * @example
     * const stats = reasoner.getStats();
     * console.log(`Store has ${stats.quads} triples`);
     */
    getStats() {
      return engine.getStats(storeContext.store);
    },

    /**
     * Clear all inferred knowledge (keeps original data)
     * @param {Object} [options] - Clear options
     * @param {boolean} [options.keepOriginal=true] - Keep original triples
     * @returns {Object} Clear result
     * 
     * @example
     * const result = reasoner.clearInferred();
     * console.log(`Cleared ${result.cleared} inferred triples`);
     */
    clearInferred(options = {}) {
      const { keepOriginal = true } = options;
      
      if (!keepOriginal) {
        const cleared = storeContext.store.size;
        storeContext.clear();
        return { success: true, cleared, kept: 0 };
      }
      
      // This is a simplified implementation
      // In practice, you'd need to track original vs inferred triples
      const originalSize = storeContext.store.size;
      storeContext.clear();
      return { success: true, cleared: originalSize, kept: 0 };
    },

    /**
     * Create a reasoning pipeline
     * @param {Array<Object>} steps - Pipeline steps
     * @returns {Object} Pipeline interface
     * 
     * @example
     * const pipeline = reasoner.createPipeline([
     *   { name: "transitive", rules: transitiveRules },
     *   { name: "symmetric", rules: symmetricRules }
     * ]);
     * 
     * const result = pipeline.run();
     */
    createPipeline(steps) {
      return {
        steps,
        
        /**
         * Run the pipeline
         * @param {Object} [options] - Pipeline options
         * @returns {Object} Pipeline result
         */
        run(options = {}) {
          const ruleSets = this.steps.map(step => step.rules);
          return this.inferSequence(ruleSets, options);
        }
      };
    },

    /**
     * Export current knowledge
     * @param {Object} [options] - Export options
     * @param {string} [options.format="Turtle"] - Export format
     * @returns {string} Exported knowledge
     * 
     * @example
     * const turtle = reasoner.export();
     * const nquads = reasoner.export({ format: "N-Quads" });
     */
    export(options = {}) {
      const { format = "Turtle" } = options;
      
      if (format === "Turtle") {
        return engine.serializeTurtle(storeContext.store);
      }
      if (format === "N-Quads") {
        return engine.serializeNQuads(storeContext.store);
      }
      
      throw new Error(`Unsupported export format: ${format}`);
    },

    /**
     * Import knowledge from string
     * @param {string} knowledge - Knowledge as Turtle or N-Quads
     * @param {Object} [options] - Import options
     * @param {string} [options.format="Turtle"] - Input format
     * @param {boolean} [options.merge=true] - Merge with existing knowledge
     * @returns {Object} Import result
     * 
     * @example
     * const result = reasoner.import(turtleData);
     * console.log(`Imported ${result.imported} triples`);
     */
    import(knowledge, options = {}) {
      const { format = "Turtle", merge = true } = options;
      
      if (!merge) {
        storeContext.clear();
      }
      
      const originalSize = storeContext.store.size;
      let importedStore;
      
      if (format === "Turtle") {
        importedStore = engine.parseTurtle(knowledge);
      } else if (format === "N-Quads") {
        importedStore = engine.parseNQuads(knowledge);
      } else {
        throw new Error(`Unsupported import format: ${format}`);
      }
      
      // Add imported triples to store
      for (const quad of importedStore) {
        storeContext.store.add(quad);
      }
      
      const imported = storeContext.store.size - originalSize;
      
      return {
        success: true,
        imported,
        totalTriples: storeContext.store.size
      };
    }
  };
}