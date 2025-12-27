/**
 * @fileoverview Reconciliation Engine - Pure state transition function μ
 *
 * **Purpose**: Execute reconciliation (state transitions) in a pure, deterministic way
 * - reconcile(universe, state, observations) → {consequences, artifacts, errors}
 * - Invariant checking
 * - Minimality verification
 *
 * **Design**:
 * - μ (reconcile function) MUST be pure (no side effects)
 * - Deterministic: same inputs = same outputs
 * - All errors captured and returned (never thrown during reconciliation)
 *
 * @module narrative-state-chain/reconcile
 */

import { blake3 } from 'hash-wasm';

/**
 * Execute reconciliation function μ for a universe
 *
 * **Process**:
 * 1. Call universe.reconcile(state, observations)
 * 2. Check invariants on resulting state
 * 3. Return consequences, artifacts, errors
 *
 * @param {import('./types.mjs').Universe} universe - Universe definition
 * @param {Object} currentState - Current state before observations
 * @param {any[]} observations - New observations to reconcile
 * @returns {Promise<{consequences: any[], artifacts: Object, errors: string[]}>} Reconciliation result
 *
 * @example
 * const result = await reconcile(universe, { count: 5 }, [{ type: 'increment' }]);
 * // result: { consequences: [{ count: 6 }], artifacts: {}, errors: [] }
 */
export async function reconcile(universe, currentState, observations) {
  try {
    // Execute user-defined reconciliation function
    const result = await universe.reconcile(currentState, observations);

    // Ensure result has required shape
    const normalizedResult = {
      consequences: result.consequences || [],
      artifacts: result.artifacts || {},
      errors: result.errors || [],
    };

    // Check invariants on new state
    const newState = { ...currentState, ...normalizedResult.artifacts };
    const invariantViolations = await checkInvariants(universe, newState);

    if (invariantViolations.length > 0) {
      normalizedResult.errors.push(...invariantViolations);
    }

    return normalizedResult;
  } catch (error) {
    // Catch any errors from reconcile function
    return {
      consequences: [],
      artifacts: {},
      errors: [`Reconciliation error: ${error.message}`],
    };
  }
}

/**
 * Check all invariants for a universe state
 *
 * **Invariants**: Pure predicate functions that must always hold
 * - Example: balance >= 0
 * - Example: references are valid
 *
 * @param {import('./types.mjs').Universe} universe - Universe definition
 * @param {Object} state - State to check
 * @returns {Promise<string[]>} Array of violation messages (empty if all hold)
 *
 * @example
 * const violations = await checkInvariants(universe, { balance: -10 });
 * // violations: ['Invariant failed: balance must be non-negative']
 */
export async function checkInvariants(universe, state) {
  const violations = [];

  for (const invariant of universe.invariants) {
    try {
      const holds = await invariant.predicate(state);

      if (!holds) {
        violations.push(`Invariant failed: ${invariant.name} (${invariant.id})`);
      }
    } catch (error) {
      violations.push(`Invariant error: ${invariant.name} - ${error.message}`);
    }
  }

  return violations;
}

/**
 * Check if a delta is minimal (no redundant changes)
 *
 * **Minimality**: Delta should only contain values that differ from previous state
 *
 * @param {Object} delta - Proposed state change
 * @param {Object} previousState - Previous state
 * @returns {Promise<{minimal: boolean, proof: string}>} Minimality check result
 *
 * @example
 * const result = await checkMinimality({ a: 1, b: 2 }, { a: 1, c: 3 });
 * // result: { minimal: true, proof: 'hash...' }
 */
export async function checkMinimality(delta, previousState) {
  const redundantKeys = [];

  for (const key in delta) {
    if (previousState[key] === delta[key]) {
      redundantKeys.push(key);
    }
  }

  // Generate proof hash
  const canonical = JSON.stringify(
    {
      delta,
      previousState,
      redundantKeys,
    },
    Object.keys({ delta, previousState, redundantKeys }).sort()
  );

  const proof = await blake3(canonical);

  return {
    minimal: redundantKeys.length === 0,
    proof,
    redundantKeys: redundantKeys.length > 0 ? redundantKeys : undefined,
  };
}

/**
 * Compute state hash for comparison/verification
 *
 * @param {Object} state - State object
 * @returns {Promise<string>} BLAKE3 hash of state
 *
 * @example
 * const hash = await computeStateHash({ key: 'value', count: 42 });
 */
export async function computeStateHash(state) {
  // Canonical JSON serialization (sorted keys)
  const canonical = JSON.stringify(state, Object.keys(state).sort());
  return blake3(canonical);
}

/**
 * Merge multiple state objects (later overrides earlier)
 *
 * **Use case**: Combining consequences from multiple reconciliations
 *
 * @param {Object[]} states - Array of state objects
 * @returns {Object} Merged state
 *
 * @example
 * const merged = mergeStates([{ a: 1 }, { b: 2 }, { a: 3 }]);
 * // merged: { a: 3, b: 2 }
 */
export function mergeStates(states) {
  return states.reduce((acc, state) => ({ ...acc, ...state }), {});
}

/**
 * Validate reconciliation result shape
 *
 * @param {any} result - Result to validate
 * @returns {{valid: boolean, errors: string[]}} Validation result
 *
 * @example
 * const validation = validateReconciliationResult(result);
 * if (!validation.valid) {
 *   console.error('Invalid result:', validation.errors);
 * }
 */
export function validateReconciliationResult(result) {
  const errors = [];

  if (!result || typeof result !== 'object') {
    errors.push('Result must be an object');
    return { valid: false, errors };
  }

  if (!Array.isArray(result.consequences)) {
    errors.push('consequences must be an array');
  }

  if (!result.artifacts || typeof result.artifacts !== 'object') {
    errors.push('artifacts must be an object');
  }

  if (!Array.isArray(result.errors)) {
    errors.push('errors must be an array');
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

/**
 * Create a simple reconciliation function (identity)
 *
 * **Use case**: Testing or pass-through universes
 *
 * @returns {(state: Object, observations: any[]) => Promise<{consequences: any[], artifacts: Object, errors: string[]}>} Identity reconciliation function
 *
 * @example
 * const universe = await store.create({
 *   schema: 'http://example.org/schema#',
 *   reconcile: createIdentityReconcile(),
 *   metadata: { name: 'PassThrough' }
 * });
 */
export function createIdentityReconcile() {
  return async (state, observations) => ({
    consequences: observations,
    artifacts: {},
    errors: [],
  });
}
