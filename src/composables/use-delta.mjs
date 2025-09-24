/**
 * @fileoverview useDelta composable - graph diff and patch operations with context
 * 
 * This composable provides graph difference and patch capabilities.
 * It enables change tracking and incremental updates for RDF graphs.
 * Now uses unctx for global delta management.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { Store } from "n3";
import { useStoreContext } from "../context/index.mjs";

/**
 * Create a delta composable for graph operations
 * 
 * @param {Object} [options] - Delta options
 * @param {boolean} [options.deterministic=true] - Enable deterministic operations
 * @returns {Object} Delta interface
 * 
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const delta = useDelta();
 * 
 *   // Calculate difference between two stores
 *   const { added, removed } = delta.diff(store1, store2);
 * 
 *   // Apply changes to a base store
 *   const patched = delta.patch(baseStore, { added, removed });
 * 
 *   // Get statistics about changes
 *   const stats = delta.getStats({ added, removed });
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useDelta(options = {}) {
  // Get the engine from context
  const storeContext = useStoreContext();
  const engine = storeContext.engine;
  
  const { deterministic = true } = options;

  return {
    /**
     * Calculate the difference between two stores
     * @param {Store} storeA - The first store
     * @param {Store} storeB - The second store
     * @returns {Object} Difference object with added and removed quads
     */
    diff(storeA, storeB) {
      if (!storeA || typeof storeA.getQuads !== "function") {
        throw new Error("[useDelta] First store must be a valid N3.Store");
      }
      
      if (!storeB || typeof storeB.getQuads !== "function") {
        throw new Error("[useDelta] Second store must be a valid N3.Store");
      }

      const added = new Store();
      const removed = new Store();

      // Find quads in B but not in A (added)
      for (const quad of storeB) {
        if (!storeA.has(quad)) {
          added.add(quad);
        }
      }

      // Find quads in A but not in B (removed)
      for (const quad of storeA) {
        if (!storeB.has(quad)) {
          removed.add(quad);
        }
      }

      return {
        added: deterministic ? this._sortQuads(added) : added,
        removed: deterministic ? this._sortQuads(removed) : removed
      };
    },

    /**
     * Apply changes to a base store
     * @param {Store} baseStore - The base store to patch
     * @param {Object} changes - Changes to apply
     * @param {Store} changes.added - Quads to add
     * @param {Store} changes.removed - Quads to remove
     * @returns {Store} New store with changes applied
     */
    patch(baseStore, changes) {
      if (!baseStore || typeof baseStore.getQuads !== "function") {
        throw new Error("[useDelta] Base store must be a valid N3.Store");
      }

      if (!changes || typeof changes !== "object") {
        throw new Error("[useDelta] Changes must be an object");
      }

      const { added, removed } = changes;
      
      if (!added || !removed) {
        throw new Error("[useDelta] Changes must include added and removed stores");
      }

      // Create a new store from the base
      const result = new Store([...baseStore]);

      // Remove quads
      for (const quad of removed) {
        result.delete(quad);
      }

      // Add quads
      for (const quad of added) {
        result.add(quad);
      }

      return result;
    },

    /**
     * Get statistics about changes
     * @param {Object} changes - Changes object
     * @param {Store} changes.added - Added quads
     * @param {Store} changes.removed - Removed quads
     * @returns {Object} Statistics about the changes
     */
    getStats(changes) {
      if (!changes || typeof changes !== "object") {
        throw new Error("[useDelta] Changes must be an object");
      }

      const { added, removed } = changes;
      
      if (!added || !removed) {
        throw new Error("[useDelta] Changes must include added and removed stores");
      }

      return {
        added: {
          quads: added.size,
          subjects: new Set([...added].map(q => q.subject.value)).size,
          predicates: new Set([...added].map(q => q.predicate.value)).size,
          objects: new Set([...added].map(q => q.object.value)).size
        },
        removed: {
          quads: removed.size,
          subjects: new Set([...removed].map(q => q.subject.value)).size,
          predicates: new Set([...removed].map(q => q.predicate.value)).size,
          objects: new Set([...removed].map(q => q.object.value)).size
        },
        total: {
          quads: added.size + removed.size,
          net: added.size - removed.size
        }
      };
    },

    /**
     * Check if changes are empty
     * @param {Object} changes - Changes object
     * @returns {boolean} True if no changes
     */
    isEmpty(changes) {
      if (!changes || typeof changes !== "object") {
        return true;
      }

      const { added, removed } = changes;
      return (!added || added.size === 0) && (!removed || removed.size === 0);
    },

    /**
     * Merge multiple change sets
     * @param {...Object} changeSets - Change sets to merge
     * @returns {Object} Merged changes
     */
    merge(...changeSets) {
      const merged = {
        added: new Store(),
        removed: new Store()
      };

      for (const changes of changeSets) {
        if (!changes || typeof changes !== "object") {
          continue;
        }

        const { added, removed } = changes;
        
        if (added) {
          for (const quad of added) {
            merged.added.add(quad);
          }
        }
        
        if (removed) {
          for (const quad of removed) {
            merged.removed.add(quad);
          }
        }
      }

      return {
        added: deterministic ? this._sortQuads(merged.added) : merged.added,
        removed: deterministic ? this._sortQuads(merged.removed) : merged.removed
      };
    },

    /**
     * Invert changes (swap added and removed)
     * @param {Object} changes - Changes to invert
     * @returns {Object} Inverted changes
     */
    invert(changes) {
      if (!changes || typeof changes !== "object") {
        throw new Error("[useDelta] Changes must be an object");
      }

      const { added, removed } = changes;
      
      return {
        added: removed || new Store(),
        removed: added || new Store()
      };
    },

    /**
     * Sort quads deterministically
     * @param {Store} store - Store to sort
     * @returns {Store} Sorted store
     * @private
     */
    _sortQuads(store) {
      const quads = [...store];
      quads.sort((a, b) => {
        const aStr = `${a.subject.value}${a.predicate.value}${a.object.value}${a.graph.value}`;
        const bStr = `${b.subject.value}${b.predicate.value}${b.object.value}${b.graph.value}`;
        return aStr.localeCompare(bStr);
      });
      
      return new Store(quads);
    }
  };
}
