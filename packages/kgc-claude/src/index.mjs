/**
 * @unrdf/kgc-claude - KGC-Claude Substrate
 *
 * Deterministic run objects, universal checkpoints, bounded autonomy,
 * and multi-agent concurrency for Claude integration.
 *
 * This package turns Claude into a replaceable actuator and moves
 * correctness, continuity, and scale into the substrate.
 *
 * ## Core Innovations
 *
 * 1. **Deterministic Run Objects** (Δ_run capsules)
 *    - Every Claude run is a first-class capsule
 *    - Normalized tool traces and artifacts
 *    - Admission via preserve(Q) ∧ Δ_run ∉ H
 *
 * 2. **Universal Checkpointing**
 *    - freeze: O_t → ⟨snapshot, receipt_t, hash(μ(O_t))⟩
 *    - Portable across CLI/IDE/MCP surfaces
 *    - Rollback independent of Claude features
 *
 * 3. **Bounded Autonomy**
 *    - Explicit budget/capacity per epoch: C_τ
 *    - Hard stops with denial receipts
 *    - Guards H enforce limits regardless of mode
 *
 * 4. **Multi-Agent Concurrency**
 *    - Shard deltas and merge deterministically
 *    - μ(O ⊔ Δ₁ ⊔ Δ₂) = μ(O ⊔ Δ₁) ⊔ μ(Δ₂)
 *    - Conflicts resolved by law (Λ), not negotiation
 *
 * 5. **Async Workflow Primitives**
 *    - WorkItem nodes in O
 *    - Completion as state transition
 *    - Receipts for every step
 *
 * 6. **Surface Projections**
 *    - UI = Π_ui(μ(O)), CLI = Π_cli(μ(O))
 *    - Single source of truth, multiple views
 *    - Missing features = missing projections
 *
 * @module @unrdf/kgc-claude
 */

// Constants
export { GRAPHS, PREDICATES, RUN_STATUS, WORK_ITEM_STATUS, DENIAL_REASONS, KGC_CLAUDE } from './constants.mjs';

// Run Capsules - Deterministic Δ_run objects
export {
  createRunCapsule,
  checkAdmission,
  persistRunCapsule,
  replayRunCapsule,
  RunCapsuleSchema,
} from './run-capsule.mjs';

// Checkpointing - Universal freeze/thaw
export {
  freeze,
  thaw,
  verifyCheckpoint,
  withCheckpoint,
  reconstructSession,
  calculateDrift,
  getCheckpointHistory,
  clearCheckpointHistory,
  CheckpointReceiptSchema,
} from './checkpoint.mjs';

// Autonomy Guards - Bounded autonomy
export {
  createAutonomyGuard,
  withGuard,
  createProductionGuard,
  createStrictGuard,
  BudgetSchema,
  UsageSchema,
  DenialReceiptSchema,
} from './autonomy-guard.mjs';

// Shard Merge - Multi-agent concurrency
export {
  createShard,
  addDelta,
  mergeDeltas,
  applyMergedDeltas,
  checkShardOverlap,
  createMultiAgentSession,
  getPendingDeltas,
  clearPendingDeltas,
  getShards,
  removeShard,
  clearShards,
  ShardScopeSchema,
  AgentShardSchema,
  MergeResultSchema,
} from './shard-merge.mjs';

// Async Workflow - Long-running work primitives
export {
  enqueueWorkItem,
  registerExecutor,
  unregisterExecutor,
  assignWorkItem,
  startExecution,
  reportProgress,
  completeWorkItem,
  failWorkItem,
  cancelWorkItem,
  getWorkItem,
  getWorkItemReceipts,
  getQueuedItems,
  getExecutingItems,
  persistWorkItem,
  clearWorkItems,
  clearExecutors,
  WorkItemSchema,
  WorkItemConstraintSchema,
  WorkItemBudgetSchema,
  ExecutionReceiptSchema,
} from './async-workflow.mjs';

// Projections - Surface parity
export {
  project,
  registerProjection,
  registerTransform,
  calculateProjectionDrift,
  getProjections,
  getProjectionsForSurface,
  clearProjections,
  ProjectionConfigSchema,
  ProjectionResultSchema,
} from './projection.mjs';

// PolicyBridge - Integration with @unrdf/hooks
export {
  loadPolicyPack,
  evaluateHookCondition,
  applyPolicy,
  isDenialReceipt,
  createPolicyBridge,
  HOOK_TO_WORKITEM_LIFECYCLE,
  WORKITEM_TO_HOOK_TRIGGERS,
} from './PolicyBridge.mjs';

// ============================================================================
// KGC-SWARM 10-Agent Calculus Implementation
// ============================================================================

// SwarmOrchestrator - 10-agent coordination (𝔄 ≔ {α₁,…,α₁₀})
export {
  SwarmOrchestrator,
  createSwarm10,
  AgentConfigSchema as SwarmAgentConfigSchema,
  BudgetSchema as SwarmBudgetSchema,
  SwarmConfigSchema,
  ProbeSchema,
  ObservationSchema,
  SwarmReceiptSchema,
} from './swarm-orchestrator.mjs';

// PokaYokeGuard - Deny-by-construction (Σ_guard, H set)
export {
  PokaYokeGuard,
  ForbiddenOps,
  createGuard as createPokaYokeGuard,
  withGuard as withPokaYoke,
  GuardConfigSchema,
  ViolationReceiptSchema,
  CheckResultSchema,
} from './poka-yoke-guards.mjs';

// ObservableIO - Observable-only I/O (O_vm ⊔ O_bb)
export {
  ObservableIO,
  createObservableIO,
  withObservable,
  TraceEntrySchema,
  VMObservationSchema,
  ControlledInputSchema,
} from './observable-io.mjs';

// InfoScheduler - Information-theoretic scheduling (Λ ordering)
export {
  InfoScheduler,
  createScheduler,
  ProbePrioritySchema,
  SchedulerStateSchema,
} from './info-scheduler.mjs';

// DriftDetector - Discovery/Convergence (Δ detection)
export {
  DriftDetector,
  createDriftDetector,
  ArtifactSnapshotSchema,
  DriftMeasurementSchema,
  ConvergenceStateSchema,
} from './drift-detector.mjs';

// BudgetEnforcer - Resource constraint enforcement (B)
export {
  BudgetEnforcer,
  createBudget,
  withBudget,
  BudgetSchema as EnforcerBudgetSchema,
  BudgetUsageSchema,
  BudgetCheckSchema,
  BudgetReceiptSchema,
} from './budget-enforcer.mjs';

// AgentHarness - Per-agent lifecycle management
export {
  AgentHarness,
  createAgent,
  createAgents,
  AgentConfigSchema as AgentHarnessConfigSchema,
  AgentStateSchema,
  AgentResultSchema,
} from './agent-harness.mjs';

// ReceiptCompositor - Merkle-based receipt aggregation
export {
  ReceiptCompositor,
  createCompositor,
  ReceiptEntrySchema,
  MerkleNodeSchema,
  CompositeReceiptSchema,
  ReceiptChainSchema,
} from './receipt-compositor.mjs';

/**
 * Create a complete KGC-Claude substrate instance
 *
 * @param {Object} store - KGCStore instance
 * @param {Object} gitBackbone - GitBackbone instance
 * @param {Object} [options]
 * @param {Object} [options.budget] - Autonomy budget config
 * @returns {KGCClaudeSubstrate}
 *
 * @example
 * import { KGCStore, GitBackbone } from '@unrdf/kgc-4d';
 * import { createSubstrate } from '@unrdf/kgc-claude';
 *
 * const store = new KGCStore();
 * const git = new GitBackbone('/path/to/repo');
 * const substrate = createSubstrate(store, git);
 *
 * // Create a guarded run
 * const run = substrate.createRun();
 * run.addToolCall({ name: 'Read', input: { file: 'foo.txt' } });
 *
 * // Check autonomy before sealing
 * const check = await substrate.guard.check(run.getMetrics());
 * if (check.allowed) {
 *   const capsule = await run.seal();
 *   await substrate.persist(capsule);
 * }
 */
export function createSubstrate(store, gitBackbone, options = {}) {
  const {
    createAutonomyGuard: createGuard,
    createProductionGuard,
  } = require('./autonomy-guard.mjs');

  const { freeze, thaw, withCheckpoint } = require('./checkpoint.mjs');
  const { createRunCapsule, persistRunCapsule } = require('./run-capsule.mjs');
  const { createMultiAgentSession } = require('./shard-merge.mjs');

  const guard = options.budget ? createGuard(options.budget) : createProductionGuard();

  return {
    store,
    gitBackbone,
    guard,

    /**
     * Create a new run capsule builder
     * @param {Object} [options]
     * @returns {RunCapsuleBuilder}
     */
    createRun(runOptions = {}) {
      return createRunCapsule(runOptions);
    },

    /**
     * Persist a run capsule
     * @param {RunCapsule} capsule
     * @returns {Promise<{ receipt: Object }>}
     */
    async persist(capsule) {
      return persistRunCapsule(store, capsule);
    },

    /**
     * Create a checkpoint
     * @param {Object} [options]
     * @returns {Promise<CheckpointReceipt>}
     */
    async checkpoint(checkpointOptions = {}) {
      return freeze(store, gitBackbone, checkpointOptions);
    },

    /**
     * Restore from checkpoint
     * @param {string} checkpointId
     * @returns {Promise<Object>}
     */
    async restore(checkpointId) {
      return thaw(store, gitBackbone, checkpointId);
    },

    /**
     * Execute with checkpoint protection
     * @param {Function} operation
     * @returns {Promise<{ result: any, checkpoint: CheckpointReceipt }>}
     */
    async withCheckpoint(operation) {
      return withCheckpoint(store, gitBackbone, operation);
    },

    /**
     * Create a multi-agent session
     * @param {Object[]} agents
     * @returns {{ shards: AgentShard[], merge: () => Promise<MergeResult> }}
     */
    createMultiAgentSession(agents) {
      return createMultiAgentSession(agents);
    },
  };
}

/**
 * @typedef {ReturnType<typeof createSubstrate>} KGCClaudeSubstrate
 */
