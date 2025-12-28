/**
 * @fileoverview useDelta composable - Simple graph diff and patch operations
 *
 * This composable provides a simple interface for graph difference and patch operations.
 * It abstracts away store management and provides intuitive methods for change tracking.
 *
 * @version 2.0.0
 * @author GitVan Team
 * @license MIT
 */

import { createStore } from '@unrdf/oxigraph';
import { useStoreContext } from '../context/index.mjs';

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
 *   // Compare two graphs
 *   const result = delta.compare(sourceData, targetData);
 *   console.log(`Added: ${result.added}, Removed: ${result.removed}`);
 *
 *   // Apply changes to current store
 *   delta.apply(result);
 * });
 */
export async function useDelta(options = {}) {
  const { deterministic = true } = options;
  const storeContext = useStoreContext();
  const engine = storeContext.engine;

  return {
    /**
     * Compare current context store with new data and return differences
     * @param {Store} newStore - New data store to compare against context store
     * @returns {Object} Comparison result
     *
     * @example
     * const newStore = engine.parseTurtle(newTurtleData);
     * const result = delta.compareWith(newStore);
     * console.log(`Changes: +${result.added} -${result.removed}`);
     */
    compareWith(newStore) {
      // Use current context store as source
      const contextStore = storeContext.store;

      // Calculate differences
      const added = await createStore();
      const removed = await createStore();

      // Find quads in new data but not in context (added)
      for (const quad of newStore) {
        if (!contextStore.has(quad)) {
          added.add(quad);
        }
      }

      // Find quads in context but not in new data (removed)
      for (const quad of contextStore) {
        if (!newStore.has(quad)) {
          removed.add(quad);
        }
      }

      return {
        added: deterministic ? this._sortQuads(added) : added,
        removed: deterministic ? this._sortQuads(removed) : removed,
        addedCount: added.size,
        removedCount: removed.size,
        unchangedCount: contextStore.size - removed.size,
        contextSize: contextStore.size,
        newDataSize: newStore.size,
      };
    },

    /**
     * Compare current context store with new data and apply changes
     * @param {Store} newStore - New data store to compare against context store
     * @param {Object} [options] - Options
     * @param {boolean} [options.dryRun=false] - Don't actually apply changes
     * @returns {Object} Apply result with comparison info
     *
     * @example
     * const newStore = engine.parseTurtle(newTurtleData);
     * const result = delta.syncWith(newStore);
     * console.log(`Synced: +${result.added} -${result.removed}`);
     */
    syncWith(newStore, options = {}) {
      const { dryRun = false } = options;

      // Get the differences
      const changes = this.compareWith(newStore);

      // Apply the changes
      const result = this.apply(changes, { dryRun });

      return {
        ...result,
        changes: changes,
      };
    },

    /**
     * Apply changes to the current store
     * @param {Object} changes - Changes to apply
     * @param {Store} changes.added - Quads to add
     * @param {Store} changes.removed - Quads to remove
     * @param {Object} [options] - Apply options
     * @param {boolean} [options.dryRun=false] - Don't actually apply changes
     * @returns {Object} Apply result
     *
     * @example
     * const result = delta.apply(changes);
     * console.log(`Applied: +${result.added} -${result.removed}`);
     */
    apply(changes, options = {}) {
      const { dryRun = false } = options;
      const { added, removed } = changes;

      const originalSize = storeContext.store.size;
      let addedCount = 0;
      let removedCount = 0;

      if (!dryRun) {
        // Remove quads
        for (const quad of removed) {
          if (storeContext.store.has(quad)) {
            storeContext.store.delete(quad);
            removedCount++;
          }
        }

        // Add quads
        for (const quad of added) {
          if (!storeContext.store.has(quad)) {
            storeContext.store.add(quad);
            addedCount++;
          }
        }
      } else {
        // Dry run - just count
        for (const quad of removed) {
          if (storeContext.store.has(quad)) {
            removedCount++;
          }
        }

        for (const quad of added) {
          if (!storeContext.store.has(quad)) {
            addedCount++;
          }
        }
      }

      return {
        success: true,
        added: addedCount,
        removed: removedCount,
        originalSize,
        finalSize: storeContext.store.size,
        dryRun,
      };
    },

    /**
     * Get statistics about changes
     * @param {Object} changes - Changes object
     * @returns {Object} Statistics about the changes
     *
     * @example
     * const stats = delta.getStats(changes);
     * console.log(`Net change: ${stats.netChange}`);
     */
    getStats(changes) {
      const { added, removed } = changes;

      const addedSubjects = new Set([...added].map(q => q.subject.value));
      const addedPredicates = new Set([...added].map(q => q.predicate.value));
      const addedObjects = new Set([...added].map(q => q.object.value));

      const removedSubjects = new Set([...removed].map(q => q.subject.value));
      const removedPredicates = new Set([...removed].map(q => q.predicate.value));
      const removedObjects = new Set([...removed].map(q => q.object.value));

      return {
        added: {
          quads: added.size,
          subjects: addedSubjects.size,
          predicates: addedPredicates.size,
          objects: addedObjects.size,
        },
        removed: {
          quads: removed.size,
          subjects: removedSubjects.size,
          predicates: removedPredicates.size,
          objects: removedObjects.size,
        },
        total: {
          quads: added.size + removed.size,
          netChange: added.size - removed.size,
        },
        coverage: {
          addedSubjects: Array.from(addedSubjects),
          removedSubjects: Array.from(removedSubjects),
          addedPredicates: Array.from(addedPredicates),
          removedPredicates: Array.from(removedPredicates),
        },
      };
    },

    /**
     * Check if changes are empty
     * @param {Object} changes - Changes object
     * @returns {boolean} True if no changes
     *
     * @example
     * if (delta.isEmpty(changes)) {
     *   console.log("No changes detected");
     * }
     */
    isEmpty(changes) {
      const { added, removed } = changes;
      return added.size === 0 && removed.size === 0;
    },

    /**
     * Merge multiple change sets
     * @param {...Object} changeSets - Change sets to merge
     * @returns {Object} Merged changes
     *
     * @example
     * const merged = delta.merge(changes1, changes2, changes3);
     */
    merge(...changeSets) {
      const merged = {
        added: await createStore(),
        removed: await createStore(),
      };

      for (const changes of changeSets) {
        if (!changes || typeof changes !== 'object') {
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
        removed: deterministic ? this._sortQuads(merged.removed) : merged.removed,
      };
    },

    /**
     * Invert changes (swap added and removed)
     * @param {Object} changes - Changes to invert
     * @returns {Object} Inverted changes
     *
     * @example
     * const inverted = delta.invert(changes);
     */
    invert(changes) {
      const { added, removed } = changes;

      return {
        added: removed || await createStore(),
        removed: added || await createStore(),
      };
    },

    /**
     * Create a patch from changes
     * @param {Object} changes - Changes object
     * @param {Object} [options] - Patch options
     * @param {string} [options.format="Turtle"] - Output format
     * @returns {Object} Patch object
     *
     * @example
     * const patch = delta.createPatch(changes);
     * console.log(`Patch: +${patch.addedCount} -${patch.removedCount}`);
     */
    createPatch(changes, options = {}) {
      const { format = 'Turtle' } = options;
      const { added, removed } = changes;

      let addedData = '';
      let removedData = '';

      if (format === 'Turtle') {
        addedData = engine.serializeTurtle(added);
        removedData = engine.serializeTurtle(removed);
      } else if (format === 'N-Quads') {
        addedData = engine.serializeNQuads(added);
        removedData = engine.serializeNQuads(removed);
      } else {
        throw new Error(`Unsupported format: ${format}`);
      }

      return {
        added: addedData,
        removed: removedData,
        addedCount: added.size,
        removedCount: removed.size,
        format,
        stats: this.getStats(changes),
      };
    },

    /**
     * Apply a patch to the current store
     * @param {Object} patch - Patch object
     * @param {string} patch.added - Added data
     * @param {string} patch.removed - Removed data
     * @param {Object} [options] - Apply options
     * @returns {Object} Apply result
     *
     * @example
     * const result = delta.applyPatch(patch);
     */
    applyPatch(patch, options = {}) {
      const { added, removed, format = 'Turtle' } = patch;

      // Parse patch data using the engine
      const addedStore =
        format === 'Turtle' ? engine.parseTurtle(added) : engine.parseNQuads(added);

      const removedStore =
        format === 'Turtle' ? engine.parseTurtle(removed) : engine.parseNQuads(removed);

      // Apply changes
      return this.apply({ added: addedStore, removed: removedStore }, options);
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
    },
  };
}
