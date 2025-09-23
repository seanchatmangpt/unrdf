/**
 * @fileoverview useStore composable - canonical N3.Store operations with context
 * 
 * This composable provides the foundation for all unrdf operations.
 * It enforces the "One Store Rule" - N3.Store is the only memory model.
 * Now uses unctx for global store management.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { useStoreContext } from "../context/index.mjs";

/**
 * Create or get the store composable from context
 * 
 * @returns {Object} Store composable interface
 * 
 * @example
 * // First call gets the store from context
 * const store = useStore();
 * store.add(quad(namedNode('ex:subject'), namedNode('ex:predicate'), literal('value')));
 * console.log(store.stats());
 * 
 * // Subsequent calls return the same store instance
 * const sameStore = useStore();
 * console.log(sameStore === store); // true
 * 
 * @throws {Error} If store context is not initialized
 */
export function useStore() {
  // Get the store from context
  return useStoreContext();
}
