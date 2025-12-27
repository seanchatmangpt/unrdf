/**
 * V6 Delta Gate - Admissibility Enforcement
 *
 * ΔGate enforces admissibility policies on proposed deltas.
 * Guards the boundary between proposed changes and applied changes.
 *
 * @module @unrdf/v6-core/delta/gate
 *
 * GUARD PATTERN:
 * - If delta is lawful → apply and return success receipt
 * - If delta is unlawful → reject and return denial receipt
 * - NO partial applications: all-or-none atomicity
 */

import { validateDelta, validateDeltaReceipt } from './schema.mjs';
import { reconcile } from './reconcile.mjs';

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
  constructor(options = {}) {
    this.policies = options.policies || {};
    this.strict = options.strict !== false; // Default true
    this.conflictResolver = options.conflictResolver || this._defaultConflictResolver;
  }

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
  async proposeDelta(delta, store) {
    try {
      // 1. Validate delta schema
      const validatedDelta = validateDelta(delta);

      // 2. Check admissibility
      const admissibilityCheck = await this._checkAdmissibility(validatedDelta, store);

      if (!admissibilityCheck.lawful) {
        return this.rejectDelta(validatedDelta, admissibilityCheck.reason);
      }

      // 3. Apply delta atomically
      return await this.applyDelta(validatedDelta, store);
    } catch (error) {
      // Validation or runtime error → rejection receipt
      return this.rejectDelta(delta, `Validation failed: ${error.message}`);
    }
  }

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
  async applyDelta(delta, store) {
    try {
      // Reconcile: μ(O ⊔ Δ)
      const reconcileResult = await reconcile(store, delta, this.conflictResolver);

      if (!reconcileResult.applied) {
        return this.rejectDelta(delta, reconcileResult.reason || 'Reconciliation failed');
      }

      // Generate receipt
      const receipt = {
        deltaId: delta.id,
        applied: true,
        timestamp_ns: BigInt(Date.now()) * 1_000_000n,
        stateHash: reconcileResult.stateHash,
        operationsApplied: delta.operations.length,
      };

      return validateDeltaReceipt(receipt);
    } catch (error) {
      return this.rejectDelta(delta, `Application failed: ${error.message}`);
    }
  }

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
  rejectDelta(delta, reason) {
    const receipt = {
      deltaId: delta.id || 'unknown',
      applied: false,
      timestamp_ns: BigInt(Date.now()) * 1_000_000n,
      reason,
      operationsApplied: 0,
    };

    return validateDeltaReceipt(receipt);
  }

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
  async _checkAdmissibility(delta, store) {
    // No admissibility config → lawful by default
    if (!delta.admissibility) {
      return { lawful: true };
    }

    const { policyId, constraints, preConditions } = delta.admissibility;

    // Check policy if specified
    if (policyId && this.policies[policyId]) {
      const policy = this.policies[policyId];
      const policyResult = await policy(delta, store);

      if (!policyResult.allowed) {
        return { lawful: false, reason: `Policy ${policyId} denied: ${policyResult.reason}` };
      }
    }

    // Check constraints
    if (constraints && constraints.length > 0) {
      for (const constraintId of constraints) {
        const constraint = this.policies[constraintId];
        if (!constraint) {
          return { lawful: false, reason: `Unknown constraint: ${constraintId}` };
        }

        const constraintResult = await constraint(delta, store);
        if (!constraintResult.allowed) {
          return { lawful: false, reason: `Constraint ${constraintId} violated: ${constraintResult.reason}` };
        }
      }
    }

    // Check pre-conditions
    if (preConditions && preConditions.length > 0) {
      for (const preConditionId of preConditions) {
        const preCondition = this.policies[preConditionId];
        if (!preCondition) {
          return { lawful: false, reason: `Unknown pre-condition: ${preConditionId}` };
        }

        const preConditionResult = await preCondition(delta, store);
        if (!preConditionResult.satisfied) {
          return { lawful: false, reason: `Pre-condition ${preConditionId} not satisfied: ${preConditionResult.reason}` };
        }
      }
    }

    return { lawful: true };
  }

  /**
   * Default conflict resolver
   *
   * Strategy: delta-wins (new value replaces old value)
   *
   * @param {Object} conflict - Conflict to resolve
   * @returns {string} Resolution strategy
   * @private
   */
  _defaultConflictResolver(conflict) {
    return 'delta-wins';
  }

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
  addPolicy(id, policy) {
    if (typeof policy !== 'function') {
      throw new TypeError('Policy must be a function');
    }
    this.policies[id] = policy;
  }

  /**
   * Remove policy
   *
   * Unregister a policy.
   *
   * @param {string} id - Policy identifier
   * @returns {boolean} Whether policy was removed
   */
  removePolicy(id) {
    if (this.policies[id]) {
      delete this.policies[id];
      return true;
    }
    return false;
  }

  /**
   * List policies
   *
   * Get all registered policy IDs.
   *
   * @returns {string[]} Policy identifiers
   */
  listPolicies() {
    return Object.keys(this.policies);
  }
}
