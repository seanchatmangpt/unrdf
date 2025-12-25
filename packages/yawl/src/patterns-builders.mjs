/**
 * YAWL Workflow Patterns - Pattern Builders and Application
 *
 * Pattern builders and application logic for workflow patterns.
 *
 * @module @unrdf/yawl/patterns-builders
 */

import { z } from 'zod';
import { SPLIT_TYPE, JOIN_TYPE, PATTERNS } from './patterns-registry.mjs';

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
// Schema Validators
// =============================================================================

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
// Pattern Application
// =============================================================================

/**
 * Validate pattern context against pattern requirements
 *
 * @param {PatternDefinition} patternDef - Pattern definition
 * @param {Object} context - Pattern application context
 * @returns {PatternValidationResult}
 */
function validatePatternContext(patternDef, context) {
  const errors = [];
  const warnings = [];

  // Validate based on split type
  if (patternDef.splitType !== 'none') {
    if (!context.sourceId) {
      errors.push(`Pattern ${patternDef.name} requires sourceId`);
    }

    // Validate branch count for split patterns
    const branchCount = context.targetIds?.length || context.branches?.length || 0;
    if (branchCount < patternDef.minBranches) {
      errors.push(
        `Pattern ${patternDef.name} requires at least ${patternDef.minBranches} branches, got ${branchCount}`
      );
    }
  }

  // Validate based on join type
  if (patternDef.joinType !== 'none') {
    if (!context.targetId) {
      errors.push(`Pattern ${patternDef.name} requires targetId`);
    }

    // Validate source count for join patterns
    const sourceCount = context.sourceIds?.length || 0;
    if (sourceCount < patternDef.minBranches) {
      errors.push(
        `Pattern ${patternDef.name} requires at least ${patternDef.minBranches} source tasks, got ${sourceCount}`
      );
    }
  }

  // Validate sequence pattern
  if (patternDef.name === 'Sequence') {
    if (!context.sourceId) {
      errors.push('Sequence pattern requires sourceId');
    }
    if (!context.targetId) {
      errors.push('Sequence pattern requires targetId');
    }
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
  };
}

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
    return {
      success: false,
      pattern: String(pattern),
      flows: [],
      errors: [`Unknown pattern: ${pattern}`],
    };
  }

  // Validate context
  const validationResult = validatePatternContext(patternDef, context);
  if (!validationResult.valid) {
    return {
      success: false,
      pattern: patternDef.name,
      flows: [],
      errors: validationResult.errors,
    };
  }

  // Apply pattern based on type
  try {
    switch (patternDef.name) {
      case 'Sequence':
        return {
          success: true,
          pattern: 'Sequence',
          flows: [sequence(context.sourceId, context.targetId)],
          errors: [],
        };

      case 'ParallelSplit':
        return {
          success: true,
          ...parallelSplit(context.sourceId, context.targetIds),
          errors: [],
        };

      case 'Synchronization':
        return {
          success: true,
          ...synchronization(context.sourceIds, context.targetId),
          errors: [],
        };

      case 'ExclusiveChoice':
        return {
          success: true,
          ...exclusiveChoice(context.sourceId, context.branches),
          errors: [],
        };

      case 'SimpleMerge':
        return {
          success: true,
          ...simpleMerge(context.sourceIds, context.targetId),
          errors: [],
        };

      case 'MultiChoice':
        return {
          success: true,
          ...multiChoice(context.sourceId, context.branches),
          errors: [],
        };

      case 'StructuredSyncMerge':
        return {
          success: true,
          ...structuredSyncMerge(context.sourceIds, context.targetId),
          errors: [],
        };

      case 'ArbitraryCycle':
        return {
          success: true,
          ...arbitraryCycle(context.sourceId, context.targetId, context.loopCondition),
          errors: [],
        };

      case 'DeferredChoice':
        return {
          success: true,
          ...deferredChoice(context.sourceId, context.targetIds),
          errors: [],
        };

      default:
        return {
          success: false,
          pattern: patternDef.name,
          flows: [],
          errors: [`Pattern ${patternDef.name} not implemented`],
        };
    }
  } catch (error) {
    return {
      success: false,
      pattern: patternDef.name,
      flows: [],
      errors: [error.message],
    };
  }
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
