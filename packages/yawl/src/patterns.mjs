/**
 * YAWL Workflow Patterns - Van der Aalst's 20 Core Patterns
 *
 * Complete implementation of workflow patterns with:
 * - Pattern registry with metadata
 * - Pattern application and validation
 * - Comprehensive Zod schemas
 * - Cardinality and cycle validation
 *
 * Reference: W.M.P. van der Aalst et al., "Workflow Patterns"
 * @see https://www.workflowpatterns.com/
 *
 * @module @unrdf/yawl/patterns
 */

import { z } from 'zod';

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

// =============================================================================
// Zod Schemas
// =============================================================================

/**
 * Split type schema
 */
export const SplitTypeSchema = z.enum(['sequence', 'and', 'xor', 'or', 'deferred']);

/**
 * Join type schema
 */
export const JoinTypeSchema = z.enum(['sequence', 'and', 'xor', 'or']);

/**
 * Task definition schema with validation
 */
export const TaskDefSchema = z.object({
  /** Unique task identifier */
  id: z.string().min(1).max(100),
  /** Human-readable task name */
  name: z.string().min(1).max(200).optional(),
  /** Split type for outgoing flows */
  splitType: SplitTypeSchema.default('sequence'),
  /** Join type for incoming flows */
  joinType: JoinTypeSchema.default('sequence'),
  /** Condition function for XOR/OR splits */
  condition: z.function().optional(),
  /** Timeout in milliseconds */
  timeout: z.number().positive().optional(),
  /** Resource ID for allocation */
  resource: z.string().min(1).max(100).optional(),
  /** Role for resource allocation */
  role: z.string().min(1).max(100).optional(),
  /** Cancellation region identifier */
  cancellationRegion: z.string().min(1).max(100).optional(),
  /** Task priority (0-100) */
  priority: z.number().int().min(0).max(100).optional(),
});

/**
 * Flow definition schema with validation
 */
export const FlowDefSchema = z.object({
  /** Source task ID */
  from: z.string().min(1).max(100),
  /** Target task ID */
  to: z.string().min(1).max(100),
  /** Condition function for conditional flows */
  condition: z.function().optional(),
  /** Evaluation priority (higher = evaluated first) */
  priority: z.number().int().default(0),
  /** Whether this flow creates a cycle */
  isCycle: z.boolean().optional(),
  /** Whether this is a deferred choice flow */
  deferred: z.boolean().optional(),
}).refine(
  data => data.from !== data.to || data.isCycle === true,
  { message: 'Source and target must differ unless isCycle is true' }
);

/**
 * Pattern application context schema
 */
export const PatternContextSchema = z.object({
  /** Source task ID (for split patterns) */
  sourceId: z.string().min(1).optional(),
  /** Target task ID (for join patterns) */
  targetId: z.string().min(1).optional(),
  /** Target task IDs (for split patterns) */
  targetIds: z.array(z.string().min(1)).optional(),
  /** Source task IDs (for join patterns) */
  sourceIds: z.array(z.string().min(1)).optional(),
  /** Branch configurations */
  branches: z.array(z.object({
    taskId: z.string().min(1),
    condition: z.function().optional(),
    priority: z.number().int().optional(),
  })).optional(),
  /** Loop condition function */
  loopCondition: z.function().optional(),
  /** Workflow data context */
  data: z.record(z.unknown()).optional(),
});

/**
 * Pattern result schema
 */
export const PatternResultSchema = z.object({
  /** Whether pattern was applied successfully */
  success: z.boolean(),
  /** Pattern that was applied */
  pattern: z.string(),
  /** Task configuration for source */
  sourceTask: z.object({
    id: z.string(),
    splitType: z.string().optional(),
    joinType: z.string().optional(),
  }).optional(),
  /** Task configuration for target */
  targetTask: z.object({
    id: z.string(),
    splitType: z.string().optional(),
    joinType: z.string().optional(),
  }).optional(),
  /** Generated flows */
  flows: z.array(FlowDefSchema).default([]),
  /** Candidate task IDs for deferred choice */
  candidates: z.array(z.string()).optional(),
  /** Validation errors if any */
  errors: z.array(z.string()).default([]),
});

/**
 * Validation result type
 * @typedef {Object} PatternValidationResult
 * @property {boolean} valid - Whether validation passed
 * @property {string[]} errors - Validation error messages
 * @property {string[]} warnings - Validation warnings
 */

// =============================================================================
// Pattern Builders (WP1-WP7 + extras)
// =============================================================================

/**
 * WP1: Sequence - Task A followed by Task B
 * @param {string} fromId - Source task ID
 * @param {string} toId - Target task ID
 * @returns {Object} Flow definition
 */
export function sequence(fromId, toId) {
  return FlowDefSchema.parse({
    from: fromId,
    to: toId,
  });
}

/**
 * WP2: Parallel Split (AND-split) - Task spawns multiple parallel branches
 * @param {string} sourceId - Source task ID
 * @param {string[]} targetIds - Target task IDs (all execute)
 * @returns {Object} Pattern definition with flows
 * @throws {Error} If fewer than 2 target tasks
 */
export function parallelSplit(sourceId, targetIds) {
  if (!Array.isArray(targetIds) || targetIds.length < 2) {
    throw new Error('Parallel split requires at least 2 target tasks');
  }
  return {
    pattern: 'PARALLEL_SPLIT',
    sourceTask: { id: sourceId, splitType: SPLIT_TYPE.AND },
    flows: targetIds.map(toId => sequence(sourceId, toId)),
  };
}

/**
 * WP3: Synchronization (AND-join) - Wait for all branches before continuing
 * @param {string[]} sourceIds - Source task IDs (all must complete)
 * @param {string} targetId - Target task ID
 * @returns {Object} Pattern definition with flows
 * @throws {Error} If fewer than 2 source tasks
 */
export function synchronization(sourceIds, targetId) {
  if (!Array.isArray(sourceIds) || sourceIds.length < 2) {
    throw new Error('Synchronization requires at least 2 source tasks');
  }
  return {
    pattern: 'SYNCHRONIZATION',
    targetTask: { id: targetId, joinType: JOIN_TYPE.AND },
    flows: sourceIds.map(fromId => sequence(fromId, targetId)),
  };
}

/**
 * WP4: Exclusive Choice (XOR-split) - Route to exactly one branch based on condition
 * @param {string} sourceId - Source task ID
 * @param {Object[]} branches - Array of { taskId, condition, priority? } objects
 * @returns {Object} Pattern definition with conditional flows
 * @throws {Error} If fewer than 2 branches
 */
export function exclusiveChoice(sourceId, branches) {
  if (!Array.isArray(branches) || branches.length < 2) {
    throw new Error('Exclusive choice requires at least 2 branches');
  }
  return {
    pattern: 'EXCLUSIVE_CHOICE',
    sourceTask: { id: sourceId, splitType: SPLIT_TYPE.XOR },
    flows: branches.map((branch, index) => ({
      from: sourceId,
      to: branch.taskId,
      condition: branch.condition,
      priority: branch.priority ?? index,
    })),
  };
}

/**
 * WP5: Simple Merge (XOR-join) - Continue when any one branch completes
 * @param {string[]} sourceIds - Source task IDs (any one can trigger)
 * @param {string} targetId - Target task ID
 * @returns {Object} Pattern definition with flows
 * @throws {Error} If fewer than 2 source tasks
 */
export function simpleMerge(sourceIds, targetId) {
  if (!Array.isArray(sourceIds) || sourceIds.length < 2) {
    throw new Error('Simple merge requires at least 2 source tasks');
  }
  return {
    pattern: 'SIMPLE_MERGE',
    targetTask: { id: targetId, joinType: JOIN_TYPE.XOR },
    flows: sourceIds.map(fromId => sequence(fromId, targetId)),
  };
}

/**
 * WP6: Multi-Choice (OR-split) - Route to one or more branches based on conditions
 * @param {string} sourceId - Source task ID
 * @param {Object[]} branches - Array of { taskId, condition, priority? } objects
 * @returns {Object} Pattern definition with conditional flows
 * @throws {Error} If fewer than 2 branches
 */
export function multiChoice(sourceId, branches) {
  if (!Array.isArray(branches) || branches.length < 2) {
    throw new Error('Multi-choice requires at least 2 branches');
  }
  return {
    pattern: 'MULTI_CHOICE',
    sourceTask: { id: sourceId, splitType: SPLIT_TYPE.OR },
    flows: branches.map((branch, index) => ({
      from: sourceId,
      to: branch.taskId,
      condition: branch.condition,
      priority: branch.priority ?? index,
    })),
  };
}

/**
 * WP7: Structured Synchronizing Merge (OR-join) - Sync all activated branches
 * @param {string[]} sourceIds - Potential source task IDs
 * @param {string} targetId - Target task ID
 * @returns {Object} Pattern definition with flows
 * @throws {Error} If fewer than 2 source tasks
 */
export function structuredSyncMerge(sourceIds, targetId) {
  if (!Array.isArray(sourceIds) || sourceIds.length < 2) {
    throw new Error('Structured sync merge requires at least 2 source tasks');
  }
  return {
    pattern: 'STRUCTURED_SYNC_MERGE',
    targetTask: { id: targetId, joinType: JOIN_TYPE.OR },
    flows: sourceIds.map(fromId => sequence(fromId, targetId)),
  };
}

/**
 * WP10: Arbitrary Cycle - Allow loops in workflow
 * @param {string} entryId - Entry task ID
 * @param {string} loopBackId - Task to loop back to
 * @param {Function} [condition] - Continue loop condition
 * @returns {Object} Pattern definition with cycle
 */
export function arbitraryCycle(entryId, loopBackId, condition) {
  return {
    pattern: 'ARBITRARY_CYCLE',
    flows: [
      {
        from: entryId,
        to: loopBackId,
        condition,
        isCycle: true,
      },
    ],
  };
}

/**
 * WP16: Deferred Choice - External trigger determines branch
 * @param {string} sourceId - Source task ID
 * @param {string[]} candidateIds - Candidate task IDs (first to start wins)
 * @returns {Object} Pattern definition
 * @throws {Error} If fewer than 2 candidates
 */
export function deferredChoice(sourceId, candidateIds) {
  if (!Array.isArray(candidateIds) || candidateIds.length < 2) {
    throw new Error('Deferred choice requires at least 2 candidates');
  }
  return {
    pattern: 'DEFERRED_CHOICE',
    sourceTask: { id: sourceId, splitType: 'deferred' },
    candidates: candidateIds,
    flows: candidateIds.map(toId => ({
      from: sourceId,
      to: toId,
      deferred: true,
    })),
  };
}

// =============================================================================
// Pattern Application - Internal Helpers
// =============================================================================

/**
 * Create error result for pattern application
 * @param {string} patternName - Pattern name
 * @param {string[]} errors - Error messages
 * @returns {Object} Error result
 */
function createPatternErrorResult(patternName, errors) {
  return {
    success: false,
    pattern: patternName,
    flows: [],
    errors,
  };
}

/**
 * Create success result for pattern application
 * @param {Object} patternResult - Pattern result from builder
 * @returns {Object} Success result
 */
function createPatternSuccessResult(patternResult) {
  return {
    success: true,
    ...patternResult,
    errors: [],
  };
}

/**
 * Apply sequence pattern
 * @param {Object} context - Pattern context
 * @returns {Object} Pattern result
 */
function applySequencePattern(context) {
  return {
    success: true,
    pattern: 'Sequence',
    flows: [sequence(context.sourceId, context.targetId)],
    errors: [],
  };
}

/**
 * Apply pattern using builder function
 * @param {string} patternName - Pattern name
 * @param {Function} builder - Builder function
 * @param {any[]} args - Builder arguments
 * @returns {Object} Pattern result
 */
function applyWithBuilder(patternName, builder, args) {
  try {
    return createPatternSuccessResult(builder(...args));
  } catch (error) {
    return createPatternErrorResult(patternName, [error.message]);
  }
}

// =============================================================================
// Pattern Application
// =============================================================================

/**
 * Apply a pattern to a workflow context
 *
 * @param {string|PatternDefinition} pattern - Pattern name or definition
 * @param {Object} context - Pattern application context
 * @param {string} [context.sourceId] - Source task ID (for split patterns)
 * @param {string} [context.targetId] - Target task ID (for join patterns)
 * @param {string[]} [context.targetIds] - Target task IDs (for split patterns)
 * @param {string[]} [context.sourceIds] - Source task IDs (for join patterns)
 * @param {Object[]} [context.branches] - Branch configurations with conditions
 * @param {Function} [context.loopCondition] - Condition for cycles
 * @returns {Object} Pattern result with configuration and flows
 */
export function applyPattern(pattern, context) {
  // Resolve pattern definition
  const patternDef = typeof pattern === 'string' ? PATTERNS[pattern] : pattern;

  if (!patternDef) {
    return createPatternErrorResult(String(pattern), [`Unknown pattern: ${pattern}`]);
  }

  // Validate context
  const validationResult = validatePatternContext(patternDef, context);
  if (!validationResult.valid) {
    return createPatternErrorResult(patternDef.name, validationResult.errors);
  }

  // Apply pattern based on type
  switch (patternDef.name) {
    case 'Sequence':
      return applySequencePattern(context);

    case 'ParallelSplit':
      return applyWithBuilder('ParallelSplit', parallelSplit, [context.sourceId, context.targetIds]);

    case 'Synchronization':
      return applyWithBuilder('Synchronization', synchronization, [context.sourceIds, context.targetId]);

    case 'ExclusiveChoice':
      return applyWithBuilder('ExclusiveChoice', exclusiveChoice, [context.sourceId, context.branches]);

    case 'SimpleMerge':
      return applyWithBuilder('SimpleMerge', simpleMerge, [context.sourceIds, context.targetId]);

    case 'MultiChoice':
      return applyWithBuilder('MultiChoice', multiChoice, [context.sourceId, context.branches]);

    case 'StructuredSyncMerge':
      return applyWithBuilder('StructuredSyncMerge', structuredSyncMerge, [context.sourceIds, context.targetId]);

    case 'ArbitraryCycle':
      return applyWithBuilder('ArbitraryCycle', arbitraryCycle, [context.sourceId, context.targetId, context.loopCondition]);

    case 'DeferredChoice':
      return applyWithBuilder('DeferredChoice', deferredChoice, [context.sourceId, context.targetIds]);

    default:
      return createPatternErrorResult(patternDef.name, [`Pattern ${patternDef.name} not implemented`]);
  }
}

// =============================================================================
// Pattern Validation - Internal Helpers
// =============================================================================

/**
 * Validate split type requirements
 * @param {PatternDefinition} patternDef - Pattern definition
 * @param {Object} context - Pattern context
 * @returns {string[]} Error messages
 */
function validateSplitTypeRequirements(patternDef, context) {
  const errors = [];

  if (patternDef.splitType === 'none') {
    return errors;
  }

  if (!context.sourceId) {
    errors.push(`Pattern ${patternDef.name} requires sourceId`);
  }

  const branchCount = context.targetIds?.length || context.branches?.length || 0;
  if (branchCount < patternDef.minBranches) {
    errors.push(
      `Pattern ${patternDef.name} requires at least ${patternDef.minBranches} branches, got ${branchCount}`
    );
  }

  return errors;
}

/**
 * Validate join type requirements
 * @param {PatternDefinition} patternDef - Pattern definition
 * @param {Object} context - Pattern context
 * @returns {string[]} Error messages
 */
function validateJoinTypeRequirements(patternDef, context) {
  const errors = [];

  if (patternDef.joinType === 'none') {
    return errors;
  }

  if (!context.targetId) {
    errors.push(`Pattern ${patternDef.name} requires targetId`);
  }

  const sourceCount = context.sourceIds?.length || 0;
  if (sourceCount < patternDef.minBranches) {
    errors.push(
      `Pattern ${patternDef.name} requires at least ${patternDef.minBranches} source tasks, got ${sourceCount}`
    );
  }

  return errors;
}

/**
 * Validate sequence pattern requirements
 * @param {PatternDefinition} patternDef - Pattern definition
 * @param {Object} context - Pattern context
 * @returns {string[]} Error messages
 */
function validateSequenceRequirements(patternDef, context) {
  const errors = [];

  if (patternDef.name !== 'Sequence') {
    return errors;
  }

  if (!context.sourceId) {
    errors.push('Sequence pattern requires sourceId');
  }
  if (!context.targetId) {
    errors.push('Sequence pattern requires targetId');
  }

  return errors;
}

// =============================================================================
// Pattern Validation
// =============================================================================

/**
 * Validate pattern context against pattern requirements
 *
 * @param {PatternDefinition} patternDef - Pattern definition
 * @param {Object} context - Pattern application context
 * @returns {PatternValidationResult}
 */
export function validatePatternContext(patternDef, context) {
  const errors = [
    ...validateSplitTypeRequirements(patternDef, context),
    ...validateJoinTypeRequirements(patternDef, context),
    ...validateSequenceRequirements(patternDef, context),
  ];

  return {
    valid: errors.length === 0,
    errors,
    warnings: [],
  };
}

/**
 * Validate task definition
 * @param {Object} taskDef - Task definition to validate
 * @returns {Object} Validated task definition
 * @throws {z.ZodError} If validation fails
 */
export function validateTaskDef(taskDef) {
  return TaskDefSchema.parse(taskDef);
}

/**
 * Validate flow definition
 * @param {Object} flowDef - Flow definition to validate
 * @returns {Object} Validated flow definition
 * @throws {z.ZodError} If validation fails
 */
export function validateFlowDef(flowDef) {
  return FlowDefSchema.parse(flowDef);
}

/**
 * Get pattern-specific cardinality requirement
 * @param {string} patternName - Pattern name
 * @returns {number} Minimum required count
 */
function getPatternMinimumBranches(patternName) {
  const multibranchPatterns = [
    'PARALLEL_SPLIT',
    'SYNCHRONIZATION',
    'EXCLUSIVE_CHOICE',
    'SIMPLE_MERGE',
    'MULTI_CHOICE',
    'STRUCTURED_SYNC_MERGE',
    'DEFERRED_CHOICE',
  ];

  if (multibranchPatterns.includes(patternName)) {
    return 2;
  }

  if (patternName === 'SEQUENCE' || patternName === 'ARBITRARY_CYCLE') {
    const pattern = PATTERNS[patternName];
    return pattern?.minBranches > 0 ? 1 : 0;
  }

  return 0;
}

/**
 * Validate cardinality constraints for a pattern
 *
 * @param {string} patternName - Pattern name
 * @param {Object} config - Pattern configuration
 * @param {number} [config.branchCount] - Number of branches
 * @param {number} [config.sourceCount] - Number of source tasks
 * @returns {PatternValidationResult}
 */
export function validateCardinality(patternName, config) {
  const errors = [];
  const pattern = PATTERNS[patternName];

  if (!pattern) {
    errors.push(`Unknown pattern: ${patternName}`);
    return { valid: false, errors, warnings: [] };
  }

  const count = config.branchCount || config.sourceCount || 0;

  if (count < pattern.minBranches) {
    errors.push(
      `${patternName} requires at least ${pattern.minBranches} branches/sources, got ${count}`
    );
  }

  const minRequired = getPatternMinimumBranches(patternName);
  if (count < minRequired) {
    errors.push(`${patternName} requires at least ${minRequired} branches`);
  }

  return { valid: errors.length === 0, errors, warnings: [] };
}

/**
 * Validate split type compatibility
 * @param {PatternDefinition} pattern - Pattern definition
 * @param {string} splitType - Split type to validate
 * @returns {string[]} Warning messages
 */
function validateSplitTypeCompatibility(pattern, splitType) {
  const warnings = [];

  if (pattern.splitType === 'none') {
    return warnings;
  }

  if (splitType && splitType !== pattern.splitType && splitType !== 'sequence') {
    warnings.push(
      `Pattern ${pattern.name} expects split type '${pattern.splitType}', got '${splitType}'`
    );
  }

  return warnings;
}

/**
 * Validate join type compatibility
 * @param {PatternDefinition} pattern - Pattern definition
 * @param {string} joinType - Join type to validate
 * @returns {string[]} Warning messages
 */
function validateJoinTypeCompatibility(pattern, joinType) {
  const warnings = [];

  if (pattern.joinType === 'none') {
    return warnings;
  }

  if (joinType && joinType !== pattern.joinType && joinType !== 'sequence') {
    warnings.push(
      `Pattern ${pattern.name} expects join type '${pattern.joinType}', got '${joinType}'`
    );
  }

  return warnings;
}

/**
 * Detect common split/join mismatches
 * @param {string} splitType - Split type
 * @param {string} joinType - Join type
 * @returns {string[]} Warning messages
 */
function detectSplitJoinMismatches(splitType, joinType) {
  const warnings = [];

  if (splitType === 'and' && joinType === 'xor') {
    warnings.push('AND-split with XOR-join may lose tokens (use AND-join for synchronization)');
  }

  if (splitType === 'or' && joinType === 'and') {
    warnings.push('OR-split with AND-join may deadlock (use OR-join for structured sync merge)');
  }

  return warnings;
}

/**
 * Validate split/join type compatibility
 *
 * @param {Object} config - Configuration to validate
 * @param {string} config.splitType - Split type of source task
 * @param {string} config.joinType - Join type of target task
 * @param {string} config.patternName - Pattern being applied
 * @returns {PatternValidationResult}
 */
export function validateSplitJoinMatch(config) {
  const { splitType, joinType, patternName } = config;
  const pattern = PATTERNS[patternName];

  if (!pattern) {
    return {
      valid: false,
      errors: [`Unknown pattern: ${patternName}`],
      warnings: [],
    };
  }

  const warnings = [
    ...validateSplitTypeCompatibility(pattern, splitType),
    ...validateJoinTypeCompatibility(pattern, joinType),
    ...detectSplitJoinMismatches(splitType, joinType),
  ];

  return { valid: true, errors: [], warnings };
}

/**
 * Build adjacency graph from flows
 * @param {Array<{from: string, to: string, isCycle?: boolean}>} flows - Flow definitions
 * @param {boolean} allowMarkedCycles - Whether to exclude marked cycles
 * @returns {{graph: Map<string, string[]>, allowedCycles: string[][]}}
 */
function buildFlowGraph(flows, allowMarkedCycles) {
  const graph = new Map();
  const allowedCycles = [];

  for (const flow of flows) {
    if (flow.isCycle && allowMarkedCycles) {
      allowedCycles.push([flow.from, flow.to]);
      continue;
    }

    if (!graph.has(flow.from)) {
      graph.set(flow.from, []);
    }
    graph.get(flow.from).push(flow.to);
  }

  return { graph, allowedCycles };
}

/**
 * Perform DFS cycle detection
 * @param {Map<string, string[]>} graph - Adjacency graph
 * @returns {{hasCycle: boolean, cycleNodes: string[]}}
 */
function performCycleDFS(graph) {
  const visited = new Set();
  const recStack = new Set();
  const cycleNodes = [];

  /**
   * @param {string} node
   * @returns {boolean}
   */
  function hasCycleDFS(node) {
    if (recStack.has(node)) {
      cycleNodes.push(node);
      return true;
    }
    if (visited.has(node)) {
      return false;
    }

    visited.add(node);
    recStack.add(node);

    const neighbors = graph.get(node) || [];
    for (const neighbor of neighbors) {
      if (hasCycleDFS(neighbor)) {
        if (cycleNodes[0] !== node) {
          cycleNodes.push(node);
        }
        return true;
      }
    }

    recStack.delete(node);
    return false;
  }

  let hasCycle = false;
  for (const node of graph.keys()) {
    if (hasCycleDFS(node)) {
      hasCycle = true;
      break;
    }
  }

  return { hasCycle, cycleNodes: hasCycle ? cycleNodes.reverse() : [] };
}

/**
 * Detect cycles in a set of flows
 *
 * @param {Array<{from: string, to: string, isCycle?: boolean}>} flows - Flow definitions
 * @param {Object} [options] - Detection options
 * @param {boolean} [options.allowMarkedCycles=true] - Allow flows marked as cycles
 * @returns {{hasCycle: boolean, cycleNodes: string[], allowedCycles: string[][]}}
 */
export function detectCycles(flows, options = {}) {
  const { allowMarkedCycles = true } = options;

  const { graph, allowedCycles } = buildFlowGraph(flows, allowMarkedCycles);
  const { hasCycle, cycleNodes } = performCycleDFS(graph);

  return {
    hasCycle,
    cycleNodes,
    allowedCycles,
  };
}

/**
 * Validate that flows have no invalid cycles
 *
 * @param {Array<{from: string, to: string, isCycle?: boolean}>} flows - Flow definitions
 * @returns {PatternValidationResult}
 */
export function validateNoCycles(flows) {
  const errors = [];
  const warnings = [];

  const result = detectCycles(flows, { allowMarkedCycles: true });

  if (result.hasCycle) {
    errors.push(
      `Invalid cycle detected: ${result.cycleNodes.join(' -> ')}. ` +
      `Mark cycle flows with isCycle: true or restructure workflow.`
    );
  }

  if (result.allowedCycles.length > 0) {
    warnings.push(
      `${result.allowedCycles.length} explicit cycle(s) found (marked with isCycle: true)`
    );
  }

  return { valid: errors.length === 0, errors, warnings };
}

/**
 * Comprehensive pattern validation
 *
 * @param {Object} config - Validation configuration
 * @param {string} config.patternName - Pattern name
 * @param {Object} [config.sourceTask] - Source task definition
 * @param {Object} [config.targetTask] - Target task definition
 * @param {Array<{from: string, to: string}>} [config.flows] - Flow definitions
 * @param {number} [config.branchCount] - Number of branches
 * @returns {PatternValidationResult}
 */
export function validatePattern(config) {
  const allErrors = [];
  const allWarnings = [];

  // Cardinality validation
  const cardinalityResult = validateCardinality(config.patternName, {
    branchCount: config.branchCount,
    sourceCount: config.branchCount,
  });
  allErrors.push(...cardinalityResult.errors);
  allWarnings.push(...cardinalityResult.warnings);

  // Split/join match validation
  if (config.sourceTask || config.targetTask) {
    const matchResult = validateSplitJoinMatch({
      splitType: config.sourceTask?.splitType,
      joinType: config.targetTask?.joinType,
      patternName: config.patternName,
    });
    allErrors.push(...matchResult.errors);
    allWarnings.push(...matchResult.warnings);
  }

  // Cycle validation
  if (config.flows && config.flows.length > 0) {
    const pattern = PATTERNS[config.patternName];
    if (pattern && !pattern.allowsCycles) {
      const cycleResult = validateNoCycles(config.flows);
      allErrors.push(...cycleResult.errors);
      allWarnings.push(...cycleResult.warnings);
    }
  }

  return {
    valid: allErrors.length === 0,
    errors: allErrors,
    warnings: allWarnings,
  };
}

// =============================================================================
// Utility Functions
// =============================================================================

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

/**
 * Create a pattern builder function with validation
 *
 * @param {string} patternName - Pattern name
 * @returns {Function} Builder function
 */
export function createPatternBuilder(patternName) {
  const pattern = PATTERNS[patternName];

  if (!pattern) {
    throw new Error(`Unknown pattern: ${patternName}`);
  }

  /**
   * @param {Object} context - Pattern context
   * @returns {Object} Pattern result
   */
  return function patternBuilder(context) {
    return applyPattern(pattern, context);
  };
}
