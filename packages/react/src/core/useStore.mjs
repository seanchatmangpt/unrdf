/**
 * @fileoverview useStore - React hook for RDF store operations
 * @module react-hooks/core/useStore
 *
 * @description
 * Hook for managing RDF store operations including add, remove, and clear operations.
 * Provides reactive state management for RDF quads.
 *
 * @example
 * ```jsx
 * import { useStore } from 'unrdf/react-hooks';
 * import { DataFactory } from 'n3';
 *
 * const { quad } = DataFactory;
 *
 * function MyComponent() {
 *   const { store, addQuad, removeQuad, clear, size } = useStore();
 *
 *   const handleAdd = () => {
 *     addQuad(quad(
 *       namedNode('http://example.org/subject'),
 *       namedNode('http://example.org/predicate'),
 *       literal('value')
 *     ));
 *   };
 *
 *   return <div>Store size: {size}</div>;
 * }
 * ```
 */

import { useState, useCallback, useMemo } from 'react';
import { Store } from 'n3';

/**
 * Hook for RDF store management
 *
 * @param {Store} [initialStore] - Initial RDF store
 * @returns {Object} Store operations and state
 */
export function useStore(initialStore = null) {
  const [store, setStore] = useState(() => initialStore || new Store());
  const [version, setVersion] = useState(0);

  /**
   * Add a quad to the store
   */
  const addQuad = useCallback(
    (quad) => {
      try {
        store.addQuad(quad);
        setVersion((v) => v + 1);
        return true;
      } catch (error) {
        console.error('[useStore] Failed to add quad:', error);
        return false;
      }
    },
    [store]
  );

  /**
   * Add multiple quads to the store
   */
  const addQuads = useCallback(
    (quads) => {
      try {
        store.addQuads(quads);
        setVersion((v) => v + 1);
        return true;
      } catch (error) {
        console.error('[useStore] Failed to add quads:', error);
        return false;
      }
    },
    [store]
  );

  /**
   * Remove a quad from the store
   */
  const removeQuad = useCallback(
    (quad) => {
      try {
        store.removeQuad(quad);
        setVersion((v) => v + 1);
        return true;
      } catch (error) {
        console.error('[useStore] Failed to remove quad:', error);
        return false;
      }
    },
    [store]
  );

  /**
   * Remove multiple quads from the store
   */
  const removeQuads = useCallback(
    (quads) => {
      try {
        store.removeQuads(quads);
        setVersion((v) => v + 1);
        return true;
      } catch (error) {
        console.error('[useStore] Failed to remove quads:', error);
        return false;
      }
    },
    [store]
  );

  /**
   * Remove matching quads
   */
  const removeMatches = useCallback(
    (subject, predicate, object, graph) => {
      try {
        store.removeMatches(subject, predicate, object, graph);
        setVersion((v) => v + 1);
        return true;
      } catch (error) {
        console.error('[useStore] Failed to remove matches:', error);
        return false;
      }
    },
    [store]
  );

  /**
   * Clear all quads from the store
   */
  const clear = useCallback(() => {
    try {
      const newStore = new Store();
      setStore(newStore);
      setVersion(0);
      return true;
    } catch (error) {
      console.error('[useStore] Failed to clear store:', error);
      return false;
    }
  }, []);

  /**
   * Replace entire store
   */
  const replaceStore = useCallback((newStore) => {
    setStore(newStore);
    setVersion(0);
  }, []);

  /**
   * Get store size (memoized, updates with version)
   */
  const size = useMemo(() => store.size, [store, version]);

  /**
   * Check if store is empty
   */
  const isEmpty = useMemo(() => store.size === 0, [store, version]);

  /**
   * Get all quads (memoized)
   */
  const quads = useMemo(() => store.getQuads(), [store, version]);

  /**
   * Match quads by pattern
   */
  const match = useCallback(
    (subject, predicate, object, graph) => {
      return store.getQuads(subject, predicate, object, graph);
    },
    [store, version]
  );

  /**
   * Check if quad exists
   */
  const has = useCallback(
    (quad) => {
      return store.has(quad);
    },
    [store, version]
  );

  /**
   * Count matching quads
   */
  const countMatches = useCallback(
    (subject, predicate, object, graph) => {
      return store.countQuads(subject, predicate, object, graph);
    },
    [store, version]
  );

  return {
    store,
    addQuad,
    addQuads,
    removeQuad,
    removeQuads,
    removeMatches,
    clear,
    replaceStore,
    match,
    has,
    countMatches,
    size,
    isEmpty,
    quads,
    version, // Expose version for external tracking
  };
}
