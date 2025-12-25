/**
 * YAWL Workflow Patterns - Pattern Registry
 *
 * Pattern definitions from Van der Aalst's workflow patterns.
 *
 * @module @unrdf/yawl/patterns-registry
 */

// =============================================================================
// Split and Join Types
// =============================================================================

/**
 * Split type enumeration for outgoing flows
 * @type {Readonly<{SEQUENCE: 'sequence', AND: 'and', XOR: 'xor', OR: 'or'}>}
 */
export const SPLIT_TYPE = Object.freeze({
  /** WP1: Single outgoing flow */
  SEQUENCE: 'sequence',
  /** WP2: All branches execute (AND-split) */
  AND: 'and',
  /** WP4: Exactly one branch based on condition (XOR-split) */
  XOR: 'xor',
  /** WP6: One or more branches based on conditions (OR-split) */
  OR: 'or',
});

/**
 * Join type enumeration for incoming flows
 * @type {Readonly<{SEQUENCE: 'sequence', AND: 'and', XOR: 'xor', OR: 'or'}>}
 */
export const JOIN_TYPE = Object.freeze({
  /** WP1: Single incoming flow */
  SEQUENCE: 'sequence',
  /** WP3: Wait for all branches (AND-join) */
  AND: 'and',
  /** WP5: First branch to complete (XOR-join) */
  XOR: 'xor',
  /** WP7: Sync all activated branches (structured sync merge) */
  OR: 'or',
});

// =============================================================================
// Pattern Registry
// =============================================================================

/**
 * @typedef {Object} PatternDefinition
 * @property {string} name - Human-readable pattern name
 * @property {string} id - Pattern identifier
 * @property {number} wpNumber - Van der Aalst WP number
 * @property {string} splitType - Required split type ('none' | 'and' | 'xor' | 'or')
 * @property {string} joinType - Required join type ('none' | 'and' | 'xor' | 'or')
 * @property {number} minBranches - Minimum number of branches required
 * @property {boolean} allowsCycles - Whether pattern allows cycles
 * @property {string} description - Pattern description
 */

/**
 * Pattern registry with complete metadata for all Van der Aalst patterns
 * @type {Readonly<Record<string, PatternDefinition>>}
 */
export const PATTERNS = Object.freeze({
  /** WP1: Sequence - Task A followed by Task B */
  SEQUENCE: {
    name: 'Sequence',
    id: 'WP1',
    wpNumber: 1,
    splitType: 'none',
    joinType: 'none',
    minBranches: 1,
    allowsCycles: false,
    description: 'An activity in a workflow process is enabled after the completion of a preceding activity in the same process.',
  },

  /** WP2: Parallel Split (AND-split) - All branches execute */
  PARALLEL_SPLIT: {
    name: 'ParallelSplit',
    id: 'WP2',
    wpNumber: 2,
    splitType: 'and',
    joinType: 'none',
    minBranches: 2,
    allowsCycles: false,
    description: 'The divergence of a branch into two or more parallel branches each of which execute concurrently.',
  },

  /** WP3: Synchronization (AND-join) - Wait for all branches */
  SYNCHRONIZATION: {
    name: 'Synchronization',
    id: 'WP3',
    wpNumber: 3,
    splitType: 'none',
    joinType: 'and',
    minBranches: 2,
    allowsCycles: false,
    description: 'The convergence of two or more branches into a single subsequent branch such that the thread of control is passed to the subsequent branch when all input branches have been enabled.',
  },

  /** WP4: Exclusive Choice (XOR-split) - Exactly one branch */
  EXCLUSIVE_CHOICE: {
    name: 'ExclusiveChoice',
    id: 'WP4',
    wpNumber: 4,
    splitType: 'xor',
    joinType: 'none',
    minBranches: 2,
    allowsCycles: false,
    description: 'The divergence of a branch into two or more branches such that when the incoming branch is enabled, the thread of control is immediately passed to precisely one of the outgoing branches.',
  },

  /** WP5: Simple Merge (XOR-join) - First to arrive proceeds */
  SIMPLE_MERGE: {
    name: 'SimpleMerge',
    id: 'WP5',
    wpNumber: 5,
    splitType: 'none',
    joinType: 'xor',
    minBranches: 2,
    allowsCycles: false,
    description: 'The convergence of two or more branches into a single subsequent branch. Each enablement of an incoming branch results in the thread of control being passed to the subsequent branch.',
  },

  /** WP6: Multi-Choice (OR-split) - One or more branches */
  MULTI_CHOICE: {
    name: 'MultiChoice',
    id: 'WP6',
    wpNumber: 6,
    splitType: 'or',
    joinType: 'none',
    minBranches: 2,
    allowsCycles: false,
    description: 'The divergence of a branch into two or more branches such that when the incoming branch is enabled, the thread of control is immediately passed to one or more of the outgoing branches based on the evaluation of distinct conditions.',
  },

  /** WP7: Structured Synchronizing Merge (OR-join) - Sync activated branches */
  STRUCTURED_SYNC_MERGE: {
    name: 'StructuredSyncMerge',
    id: 'WP7',
    wpNumber: 7,
    splitType: 'none',
    joinType: 'or',
    minBranches: 2,
    allowsCycles: false,
    description: 'The convergence of two or more branches which diverged earlier in the process into a single subsequent branch such that the thread of control is passed to the subsequent branch when all active incoming branches have been enabled.',
  },

  /** WP8: Multi-Merge - Tokens merge without synchronization */
  MULTI_MERGE: {
    name: 'MultiMerge',
    id: 'WP8',
    wpNumber: 8,
    splitType: 'none',
    joinType: 'xor',
    minBranches: 2,
    allowsCycles: false,
    description: 'The convergence of two or more branches into a single subsequent branch. Each enablement of an incoming branch results in the thread of control being passed to the subsequent branch.',
  },

  /** WP9: Structured Discriminator - First of N branches triggers downstream */
  STRUCTURED_DISCRIMINATOR: {
    name: 'StructuredDiscriminator',
    id: 'WP9',
    wpNumber: 9,
    splitType: 'none',
    joinType: 'xor',
    minBranches: 2,
    allowsCycles: false,
    description: 'The convergence of two or more branches into a single subsequent branch following a corresponding parallel split earlier in the process. Once the first branch fires, the subsequent branch is enabled and the remaining branches are withdrawn.',
  },

  /** WP10: Arbitrary Cycle - Loops in workflow */
  ARBITRARY_CYCLE: {
    name: 'ArbitraryCycle',
    id: 'WP10',
    wpNumber: 10,
    splitType: 'xor',
    joinType: 'xor',
    minBranches: 1,
    allowsCycles: true,
    description: 'The ability to represent cycles in a workflow that have more than one entry or exit point.',
  },

  /** WP11: Implicit Termination */
  IMPLICIT_TERMINATION: {
    name: 'ImplicitTermination',
    id: 'WP11',
    wpNumber: 11,
    splitType: 'none',
    joinType: 'none',
    minBranches: 0,
    allowsCycles: false,
    description: 'A given subprocess should terminate when there are no remaining work items that are able to be done either now or at any time in the future.',
  },

  /** WP16: Deferred Choice - External trigger determines branch */
  DEFERRED_CHOICE: {
    name: 'DeferredChoice',
    id: 'WP16',
    wpNumber: 16,
    splitType: 'deferred',
    joinType: 'none',
    minBranches: 2,
    allowsCycles: false,
    description: 'A point in the workflow process where one of several branches is chosen based on interaction with the operating environment.',
  },

  /** WP19: Cancel Task */
  CANCEL_TASK: {
    name: 'CancelTask',
    id: 'WP19',
    wpNumber: 19,
    splitType: 'none',
    joinType: 'none',
    minBranches: 1,
    allowsCycles: false,
    description: 'An enabled task is withdrawn prior to it commencing execution.',
  },

  /** WP20: Cancel Case */
  CANCEL_CASE: {
    name: 'CancelCase',
    id: 'WP20',
    wpNumber: 20,
    splitType: 'none',
    joinType: 'none',
    minBranches: 0,
    allowsCycles: false,
    description: 'A case (i.e. workflow instance) is removed completely.',
  },
});

/**
 * Get pattern by WP number
 * @param {number} wpNumber - Van der Aalst WP number (1-20)
 * @returns {PatternDefinition|undefined}
 */
export function getPatternByWPNumber(wpNumber) {
  return Object.values(PATTERNS).find(p => p.wpNumber === wpNumber);
}

/**
 * Get all patterns as an array
 * @returns {PatternDefinition[]}
 */
export function getAllPatterns() {
  return Object.values(PATTERNS);
}

/**
 * Check if a pattern is a split pattern
 * @param {string} patternName - Pattern name
 * @returns {boolean}
 */
export function isSplitPattern(patternName) {
  const pattern = PATTERNS[patternName];
  return pattern ? pattern.splitType !== 'none' : false;
}

/**
 * Check if a pattern is a join pattern
 * @param {string} patternName - Pattern name
 * @returns {boolean}
 */
export function isJoinPattern(patternName) {
  const pattern = PATTERNS[patternName];
  return pattern ? pattern.joinType !== 'none' : false;
}

/**
 * Get the appropriate split type constant for a pattern
 * @param {string} patternName - Pattern name
 * @returns {string|undefined}
 */
export function getSplitTypeForPattern(patternName) {
  const pattern = PATTERNS[patternName];
  if (!pattern || pattern.splitType === 'none') {
    return undefined;
  }
  return pattern.splitType;
}

/**
 * Get the appropriate join type constant for a pattern
 * @param {string} patternName - Pattern name
 * @returns {string|undefined}
 */
export function getJoinTypeForPattern(patternName) {
  const pattern = PATTERNS[patternName];
  if (!pattern || pattern.joinType === 'none') {
    return undefined;
  }
  return pattern.joinType;
}
