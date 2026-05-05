/**
 * Reconcile delta with current ontology state
 *
 * Computes μ(O ⊔ Δ) by:
 * 1. Detecting conflicts between Δ and O
 * 2. Resolving conflicts using resolver strategy
 * 3. Applying all operations atomically
 * 4. Computing new state hash
 *
 * @param {Object} store - KnowledgeStore instance
 * @param {Object} delta - Validated delta to reconcile
 * @param {Function} [conflictResolver] - Conflict resolution strategy
 * @returns {Promise<Object>} Reconciliation result {applied, stateHash, conflicts, reason}
 *
 * @example
 * const result = await reconcile(store, delta, (conflict) => 'delta-wins');
 * if (result.applied) {
 *   console.log('New state hash:', result.stateHash);
 *   console.log('Conflicts resolved:', result.conflicts.length);
 * }
 */
export function reconcile(store: any, delta: any, conflictResolver?: Function): Promise<any>;
/**
 * Default conflict resolver
 *
 * Strategy: delta-wins (new value always replaces old value)
 *
 * @param {Object} _conflict - Conflict to resolve
 * @returns {string} Resolution strategy
 */
export function defaultConflictResolver(_conflict: any): string;
/**
 * Create current-wins conflict resolver
 *
 * Strategy: Keep existing value, reject delta change
 *
 * @returns {Function} Conflict resolver function
 *
 * @example
 * const resolver = currentWinsResolver();
 * const result = await reconcile(store, delta, resolver);
 */
export function currentWinsResolver(): Function;
/**
 * Create strict conflict resolver
 *
 * Strategy: Reject any conflicts
 *
 * @returns {Function} Conflict resolver function
 *
 * @example
 * const resolver = strictResolver();
 * const result = await reconcile(store, delta, resolver);
 */
export function strictResolver(): Function;
/**
 * Create custom conflict resolver
 *
 * Allows application-specific conflict resolution logic.
 *
 * @param {Function} strategy - Custom strategy function
 * @returns {Function} Conflict resolver function
 *
 * @example
 * const resolver = customResolver((conflict) => {
 *   // Custom logic based on predicate
 *   if (conflict.predicate.includes('timestamp')) {
 *     return 'delta-wins'; // Always use newer timestamp
 *   }
 *   return 'current-wins';
 * });
 */
export function customResolver(strategy: Function): Function;
