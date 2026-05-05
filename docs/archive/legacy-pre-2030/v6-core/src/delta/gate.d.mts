/**
 * Delta Gate - Admissibility enforcer
 *
 * Validates and applies deltas with policy enforcement.
 * Ensures only lawful deltas mutate ontology state.
 *
 * @class
 *
 * @example
 * import { DeltaGate } from '@unrdf/v6-core/delta';
 * const gate = new DeltaGate({ policies: myPolicies });
 * const receipt = await gate.proposeDelta(delta, store);
 * console.assert(receipt.applied, 'Delta was applied');
 */
export class DeltaGate {
    /**
     * @param {Object} [options] - Configuration options
     * @param {Object} [options.policies] - Policy enforcement rules
     * @param {boolean} [options.strict] - Strict mode (reject on any conflict)
     * @param {Function} [options.conflictResolver] - Custom conflict resolution
     */
    constructor(options?: {
        policies?: any;
        strict?: boolean;
        conflictResolver?: Function;
    });
    policies: any;
    strict: boolean;
    conflictResolver: Function;
    /**
     * Propose delta for application
     *
     * Steps:
     * 1. Validate delta schema
     * 2. Check admissibility policies
     * 3. If lawful → apply via applyDelta
     * 4. If unlawful → reject via rejectDelta
     *
     * @param {Object} delta - Delta to propose
     * @param {Object} store - KnowledgeStore instance
     * @returns {Promise<Object>} DeltaReceipt
     * @throws {Error} If validation fails
     *
     * @example
     * const receipt = await gate.proposeDelta(delta, store);
     * if (receipt.applied) {
     *   console.log('Delta applied:', receipt.stateHash);
     * } else {
     *   console.error('Delta rejected:', receipt.reason);
     * }
     */
    proposeDelta(delta: any, store: any): Promise<any>;
    /**
     * Apply delta atomically
     *
     * Performs reconciliation μ(O ⊔ Δ) and commits all operations.
     * Returns success receipt with state hash.
     *
     * @param {Object} delta - Validated delta
     * @param {Object} store - KnowledgeStore instance
     * @returns {Promise<Object>} Success DeltaReceipt
     *
     * @example
     * const receipt = await gate.applyDelta(delta, store);
     * console.assert(receipt.applied === true);
     * console.assert(receipt.stateHash, 'Has new state hash');
     */
    applyDelta(delta: any, store: any): Promise<any>;
    /**
     * Reject delta with denial receipt
     *
     * Returns denial receipt without mutating state.
     * Ensures every delta attempt produces a receipt.
     *
     * @param {Object} delta - Delta to reject
     * @param {string} reason - Rejection reason
     * @returns {Object} Denial DeltaReceipt
     *
     * @example
     * const receipt = gate.rejectDelta(delta, 'Policy violation');
     * console.assert(receipt.applied === false);
     * console.assert(receipt.reason, 'Has rejection reason');
     */
    rejectDelta(delta: any, reason: string): any;
    /**
     * Check admissibility policies
     *
     * Validates delta against configured policies.
     * Returns {lawful: boolean, reason?: string}.
     *
     * @param {Object} delta - Delta to check
     * @param {Object} store - KnowledgeStore instance
     * @returns {Promise<Object>} Admissibility result
     * @private
     */
    private _checkAdmissibility;
    /**
     * Default conflict resolver
     *
     * Strategy: delta-wins (new value replaces old value)
     *
     * @param {Object} _conflict - Conflict to resolve
     * @returns {string} Resolution strategy
     * @private
     */
    private _defaultConflictResolver;
    /**
     * Add policy
     *
     * Register a new policy for admissibility checks.
     *
     * @param {string} id - Policy identifier
     * @param {Function} policy - Policy function (async (delta, store) => {allowed: boolean, reason?: string})
     *
     * @example
     * gate.addPolicy('no-deletes', async (delta, store) => {
     *   const hasDeletes = delta.operations.some(op => op.op === 'delete');
     *   return {
     *     allowed: !hasDeletes,
     *     reason: hasDeletes ? 'Delete operations not allowed' : undefined
     *   };
     * });
     */
    addPolicy(id: string, policy: Function): void;
    /**
     * Remove policy
     *
     * Unregister a policy.
     *
     * @param {string} id - Policy identifier
     * @returns {boolean} Whether policy was removed
     */
    removePolicy(id: string): boolean;
    /**
     * List policies
     *
     * Get all registered policy IDs.
     *
     * @returns {string[]} Policy identifiers
     */
    listPolicies(): string[];
}
