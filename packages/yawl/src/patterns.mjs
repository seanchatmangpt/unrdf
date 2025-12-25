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

// Registry
export {
  SPLIT_TYPE,
  JOIN_TYPE,
  PATTERNS,
  getPatternByWPNumber,
  getAllPatterns,
  isSplitPattern,
  isJoinPattern,
  getSplitTypeForPattern,
  getJoinTypeForPattern,
} from './patterns-registry.mjs';

// Builders and Application
export {
  SplitTypeSchema,
  JoinTypeSchema,
  TaskDefSchema,
  FlowDefSchema,
  PatternContextSchema,
  PatternResultSchema,
  validateTaskDef,
  validateFlowDef,
  sequence,
  parallelSplit,
  synchronization,
  exclusiveChoice,
  simpleMerge,
  multiChoice,
  structuredSyncMerge,
  arbitraryCycle,
  deferredChoice,
  applyPattern,
  createPatternBuilder,
} from './patterns-builders.mjs';

// Validation
export {
  validatePatternContext,
  validateCardinality,
  validateSplitJoinMatch,
  detectCycles,
  validateNoCycles,
  validatePattern,
} from './patterns-validation.mjs';
