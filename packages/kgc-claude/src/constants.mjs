/**
 * KGC-Claude Constants - Namespace URIs and predicates for Claude substrate
 * @module @unrdf/kgc-claude/constants
 */

/**
 * Named graphs for KGC-Claude substrate
 * @readonly
 * @enum {string}
 */
export const GRAPHS = Object.freeze({
  /** Current mutable universe state */
  UNIVERSE: 'http://kgc.io/Universe',
  /** Immutable append-only event log */
  EVENT_LOG: 'http://kgc.io/EventLog',
  /** System metadata and configuration */
  SYSTEM: 'http://kgc.io/System',
  /** Run capsules (Claude executions) */
  RUN_CAPSULES: 'http://kgc.io/RunCapsules',
  /** Async work items */
  WORK_ITEMS: 'http://kgc.io/WorkItems',
  /** Agent shards for concurrency */
  AGENT_SHARDS: 'http://kgc.io/AgentShards',
});

/**
 * KGC-Claude namespace
 * @constant {string}
 */
export const KGC_CLAUDE = 'http://kgc.io/claude/';

/**
 * Predicates for KGC-Claude substrate
 * @readonly
 * @enum {string}
 */
export const PREDICATES = Object.freeze({
  // Run capsule predicates
  RUN_ID: `${KGC_CLAUDE}runId`,
  RUN_TYPE: `${KGC_CLAUDE}runType`,
  RUN_STATUS: `${KGC_CLAUDE}runStatus`,
  TOOL_TRACE: `${KGC_CLAUDE}toolTrace`,
  ARTIFACTS: `${KGC_CLAUDE}artifacts`,
  DELTA_O: `${KGC_CLAUDE}deltaO`,
  DELTA_PI: `${KGC_CLAUDE}deltaPi`,
  DELTA_LAMBDA: `${KGC_CLAUDE}deltaLambda`,
  DELTA_Q: `${KGC_CLAUDE}deltaQ`,
  RUN_HASH: `${KGC_CLAUDE}runHash`,
  PARENT_RUN: `${KGC_CLAUDE}parentRun`,

  // Checkpoint predicates
  CHECKPOINT_ID: `${KGC_CLAUDE}checkpointId`,
  SNAPSHOT_HASH: `${KGC_CLAUDE}snapshotHash`,
  SNAPSHOT_REF: `${KGC_CLAUDE}snapshotRef`,
  CHECKPOINT_TIME: `${KGC_CLAUDE}checkpointTime`,

  // Autonomy guard predicates
  BUDGET_EPOCH: `${KGC_CLAUDE}budgetEpoch`,
  MAX_DELTA_SIZE: `${KGC_CLAUDE}maxDeltaSize`,
  MAX_TOOL_OPS: `${KGC_CLAUDE}maxToolOps`,
  MAX_FILES_TOUCHED: `${KGC_CLAUDE}maxFilesTouched`,
  CURRENT_USAGE: `${KGC_CLAUDE}currentUsage`,

  // Shard predicates
  SHARD_ID: `${KGC_CLAUDE}shardId`,
  AGENT_ID: `${KGC_CLAUDE}agentId`,
  SHARD_SCOPE: `${KGC_CLAUDE}shardScope`,
  MERGE_ORDER: `${KGC_CLAUDE}mergeOrder`,

  // Async workflow predicates
  WORK_ITEM_ID: `${KGC_CLAUDE}workItemId`,
  WORK_ITEM_STATUS: `${KGC_CLAUDE}workItemStatus`,
  WORK_ITEM_CONSTRAINTS: `${KGC_CLAUDE}workItemConstraints`,
  WORK_ITEM_BUDGET: `${KGC_CLAUDE}workItemBudget`,
  EXECUTOR_ID: `${KGC_CLAUDE}executorId`,

  // Common predicates
  T_NS: 'http://kgc.io/t_ns',
  VECTOR_CLOCK: 'http://kgc.io/vector_clock',
  RECEIPT_HASH: `${KGC_CLAUDE}receiptHash`,
  PREVIOUS_RECEIPT: `${KGC_CLAUDE}previousReceipt`,
});

/**
 * Run status values
 * @readonly
 * @enum {string}
 */
export const RUN_STATUS = Object.freeze({
  PENDING: 'pending',
  RUNNING: 'running',
  COMPLETED: 'completed',
  FAILED: 'failed',
  DENIED: 'denied',
  ROLLED_BACK: 'rolled_back',
});

/**
 * Work item status values
 * @readonly
 * @enum {string}
 */
export const WORK_ITEM_STATUS = Object.freeze({
  QUEUED: 'queued',
  ASSIGNED: 'assigned',
  EXECUTING: 'executing',
  COMPLETED: 'completed',
  FAILED: 'failed',
  CANCELLED: 'cancelled',
});

/**
 * Autonomy denial reasons
 * @readonly
 * @enum {string}
 */
export const DENIAL_REASONS = Object.freeze({
  BUDGET_EXCEEDED: 'budget_exceeded',
  DELTA_TOO_LARGE: 'delta_too_large',
  TOOLS_EXCEEDED: 'tools_exceeded',
  FILES_EXCEEDED: 'files_exceeded',
  INVARIANT_VIOLATED: 'invariant_violated',
  SHARD_CONFLICT: 'shard_conflict',
});
