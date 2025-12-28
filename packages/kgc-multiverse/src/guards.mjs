/**
 * KGC Multiverse - Poka-Yoke Guards
 * Implements state machine guards to make invalid operations IMPOSSIBLE
 *
 * @module @unrdf/kgc-multiverse/guards
 */

/**
 * Valid State Transitions Matrix
 * Defines all allowed state transitions
 */
const VALID_TRANSITIONS = {
  GENESIS: ['ACTIVE'],
  ACTIVE: ['FORKED', 'FROZEN'],
  FORKED: ['MERGED', 'FROZEN'],
  MERGED: ['ACTIVE', 'DISCARDED'],
  FROZEN: ['DISCARDED'],
  DISCARDED: [], // Terminal state
};

/**
 * Guard GR1-10: State Transition Validation
 * Prevents invalid state transitions
 *
 * @param {string} currentState - Current universe state
 * @param {string} targetState - Desired target state
 * @throws {Error} If transition is invalid
 *
 * @example
 * import { guardStateTransition } from './guards.mjs';
 * guardStateTransition('ACTIVE', 'FROZEN'); // OK
 * guardStateTransition('FROZEN', 'ACTIVE'); // Throws error
 */
export function guardStateTransition(currentState, targetState) {
  if (typeof currentState !== 'string') {
    throw new TypeError('guardStateTransition: currentState must be string');
  }
  if (typeof targetState !== 'string') {
    throw new TypeError('guardStateTransition: targetState must be string');
  }

  const allowed = VALID_TRANSITIONS[currentState];

  if (!allowed) {
    throw new Error(
      `guardStateTransition: Unknown state '${currentState}'`
    );
  }

  if (!allowed.includes(targetState)) {
    throw new Error(
      `guardStateTransition: Invalid transition ${currentState} → ${targetState} ` +
      `(allowed: ${allowed.length > 0 ? allowed.join(', ') : 'none - terminal state'})`
    );
  }
}

/**
 * Guard GR3: Cannot apply Φ (morphism) to FROZEN universe
 *
 * @param {string} state - Universe state
 * @throws {Error} If state is FROZEN
 *
 * @example
 * guardMorphismApplication('ACTIVE'); // OK
 * guardMorphismApplication('FROZEN'); // Throws
 */
export function guardMorphismApplication(state) {
  if (state === 'FROZEN') {
    throw new Error(
      'guardMorphismApplication: FROZEN universe is immutable ' +
      '(use reconstructState to create editable copy)'
    );
  }

  if (state === 'DISCARDED') {
    throw new Error(
      'guardMorphismApplication: Cannot apply morphism to DISCARDED universe'
    );
  }

  // Only ACTIVE, GENESIS, FORKED, MERGED are valid for morphism application
  const validStates = ['ACTIVE', 'GENESIS', 'FORKED', 'MERGED'];
  if (!validStates.includes(state)) {
    throw new Error(
      `guardMorphismApplication: Invalid state '${state}' for morphism ` +
      `(valid: ${validStates.join(', ')})`
    );
  }
}

/**
 * Guard GR2 & GR8: Merge preconditions check
 *
 * @param {Object} childUniverse - Forked child universe
 * @param {Array<Object>} conflicts - Detected conflicts
 * @param {Object|null} resolution - Conflict resolution strategy
 * @throws {Error} If merge preconditions not met
 *
 * @example
 * guardMergePreconditions(fork, [], null); // OK - no conflicts
 * guardMergePreconditions(fork, [conflict], null); // Throws - unresolved
 */
export function guardMergePreconditions(childUniverse, conflicts, resolution) {
  // Must be FORKED state
  if (childUniverse.state !== 'FORKED') {
    throw new Error(
      `guardMergePreconditions: Cannot merge ${childUniverse.state} universe ` +
      `(must be FORKED)`
    );
  }

  // Must have parent reference (GR8)
  if (!childUniverse.parent || typeof childUniverse.parent !== 'string') {
    throw new Error(
      'guardMergePreconditions: FORKED universe missing parent reference (corrupted state)'
    );
  }

  // Must have conflict resolution if conflicts exist (GR2)
  if (Array.isArray(conflicts) && conflicts.length > 0 && !resolution) {
    throw new Error(
      `guardMergePreconditions: Merge requires conflict resolution ` +
      `(found ${conflicts.length} conflict${conflicts.length > 1 ? 's' : ''})`
    );
  }
}

/**
 * Guard GR1, GR5, GR10: Freeze preconditions check
 *
 * @param {Object} universe - Universe to freeze
 * @throws {Error} If freeze not allowed
 *
 * @example
 * guardFreezePreconditions({ state: 'ACTIVE', eventCount: 5 }); // OK
 * guardFreezePreconditions({ state: 'FORKED' }); // Throws - GR1
 */
export function guardFreezePreconditions(universe) {
  // GR1: Cannot freeze while FORKED
  if (universe.state === 'FORKED') {
    throw new Error(
      'guardFreezePreconditions: Cannot freeze FORKED universe - ' +
      'must merge or discard first'
    );
  }

  // Cannot freeze DISCARDED
  if (universe.state === 'DISCARDED') {
    throw new Error(
      'guardFreezePreconditions: Cannot freeze DISCARDED universe (terminal state)'
    );
  }

  // GR10: Cannot freeze empty GENESIS
  if (universe.state === 'GENESIS' && universe.eventCount === 0) {
    throw new Error(
      'guardFreezePreconditions: Cannot freeze empty GENESIS (no events to snapshot)'
    );
  }

  // GR5: Warn on already FROZEN (idempotent but suspicious)
  if (universe.state === 'FROZEN') {
    console.warn(
      'guardFreezePreconditions: Universe already FROZEN - ' +
      'returning existing snapshot (idempotent)'
    );
  }
}

/**
 * Guard GR4: Delete safety check
 *
 * @param {Object} universe - Universe to delete
 * @param {Object} [options={}] - Delete options
 * @param {boolean} [options.force=false] - Force delete ACTIVE universe
 * @throws {Error} If delete is unsafe
 *
 * @example
 * guardDeleteSafety({ state: 'FROZEN' }); // OK
 * guardDeleteSafety({ state: 'DISCARDED' }); // Throws - GR4
 */
export function guardDeleteSafety(universe, options = {}) {
  // GR4: Cannot delete DISCARDED (idempotency check)
  if (universe.state === 'DISCARDED') {
    throw new Error(
      'guardDeleteSafety: Universe already DISCARDED (terminal state)'
    );
  }

  // Cannot delete ACTIVE without force flag
  if (universe.state === 'ACTIVE' && !options.force) {
    throw new Error(
      'guardDeleteSafety: Cannot delete ACTIVE universe without force=true ' +
      '(has live data)'
    );
  }

  // Warn on deleting FORKED (may have unmerged work)
  if (universe.state === 'FORKED') {
    console.warn(
      'guardDeleteSafety: Deleting FORKED universe - ' +
      'unmerged changes will be lost'
    );
  }
}

/**
 * Guard GR9: Cannot transition from MERGED to FORKED
 *
 * @param {string} state - Current state
 * @throws {Error} If trying to fork from MERGED
 *
 * @example
 * guardCanFork('ACTIVE'); // OK
 * guardCanFork('MERGED'); // Throws - GR9
 */
export function guardCanFork(state) {
  // GR9: Cannot fork from MERGED
  if (state === 'MERGED') {
    throw new Error(
      'guardCanFork: MERGED is transient - wait for ACTIVE or DISCARDED'
    );
  }

  // GR5: Cannot fork from FROZEN
  if (state === 'FROZEN') {
    throw new Error(
      'guardCanFork: Cannot fork FROZEN universe ' +
      '(reconstruct to ACTIVE first)'
    );
  }

  // Cannot fork from DISCARDED
  if (state === 'DISCARDED') {
    throw new Error(
      'guardCanFork: Cannot fork DISCARDED universe (terminal state)'
    );
  }
}

/**
 * Guard GR6: Cannot unfreeze snapshot
 *
 * @param {string} state - Current state
 * @throws {Error} If trying to unfreeze
 *
 * @example
 * guardNoUnfreeze('FROZEN'); // Throws
 */
export function guardNoUnfreeze(state) {
  if (state === 'FROZEN') {
    throw new Error(
      'guardNoUnfreeze: FROZEN is immutable ' +
      '(use reconstructState for editable copy)'
    );
  }
}

/**
 * Guard: Universe state must be one of allowed states
 *
 * @param {string} state - Current state
 * @param {Array<string>} allowedStates - List of allowed states
 * @throws {Error} If state not in allowed list
 *
 * @example
 * guardUniverseState('ACTIVE', ['ACTIVE', 'GENESIS']); // OK
 * guardUniverseState('FROZEN', ['ACTIVE']); // Throws
 */
export function guardUniverseState(state, allowedStates) {
  if (!allowedStates.includes(state)) {
    throw new Error(
      `guardUniverseState: Universe state '${state}' not allowed ` +
      `(expected: ${allowedStates.join(', ')})`
    );
  }
}

/**
 * Guard: Validate Q* identifier format
 *
 * @param {string} qid - Q* identifier
 * @throws {TypeError} If Q* ID format invalid
 *
 * @example
 * guardQStarID('Q*_0123456789abcdef'); // OK
 * guardQStarID('invalid'); // Throws
 */
export function guardQStarID(qid) {
  if (typeof qid !== 'string') {
    throw new TypeError('guardQStarID: Q* ID must be string');
  }

  if (!/^Q\*_[a-f0-9]{16}$/.test(qid)) {
    throw new TypeError(
      `guardQStarID: Invalid Q* ID format '${qid}' ` +
      `(expected Q*_<16 hex chars>)`
    );
  }
}

/**
 * Guard: Validate universe hash (BLAKE3, 64 hex chars)
 *
 * @param {string} hash - Universe hash
 * @throws {TypeError} If hash format invalid
 *
 * @example
 * guardUniverseHash('a'.repeat(64)); // OK
 * guardUniverseHash('invalid'); // Throws
 */
export function guardUniverseHash(hash) {
  if (typeof hash !== 'string') {
    throw new TypeError('guardUniverseHash: Hash must be string');
  }

  if (!/^[a-f0-9]{64}$/.test(hash)) {
    throw new TypeError(
      `guardUniverseHash: Invalid hash format (expected 64 hex chars, got ${hash.length})`
    );
  }
}

/**
 * Export all guards for testing
 */
export const allGuards = {
  guardStateTransition,
  guardMorphismApplication,
  guardMergePreconditions,
  guardFreezePreconditions,
  guardDeleteSafety,
  guardCanFork,
  guardNoUnfreeze,
  guardUniverseState,
  guardQStarID,
  guardUniverseHash,
};
