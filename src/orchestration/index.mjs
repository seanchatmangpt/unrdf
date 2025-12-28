/**
 * @fileoverview Orchestration Module - Multi-package atomic change coordination
 *
 * **Purpose**: Coordinate changes across multiple packages atomically:
 * - Dependency resolution and topological ordering
 * - Stage-based execution (admission, test, build, validate, integrate, commit)
 * - Automatic rollback on failure
 * - Comprehensive receipt aggregation for audit trail
 *
 * **Usage**:
 * ```javascript
 * import { executeWorkflow, analyzeWorkflowImpact } from '@unrdf/orchestration';
 *
 * // Analyze impact before executing
 * const impact = analyzeWorkflowImpact({
 *   changedPackages: ['@unrdf/core'],
 *   packages: { ... }
 * });
 *
 * // Execute atomic multi-package workflow
 * const result = await executeWorkflow({
 *   changedPackages: ['@unrdf/core'],
 *   packages: { ... }
 * });
 *
 * if (result.decision === 'allow') {
 *   console.log('Changes applied atomically');
 * }
 * ```
 *
 * @module orchestration
 */

// Dependency Resolution
export {
  DependencyResolver,
  createDependencyResolver,
  PackageDependencySchema,
  DependencyGraphSchema,
  ResolutionResultSchema
} from './dependency-resolver.mjs';

// Import factory functions for internal use
import { createDependencyResolver as _createDependencyResolver } from './dependency-resolver.mjs';
import { createStageExecutor as _createStageExecutor } from './stage-executor.mjs';
import { createRollbackManager as _createRollbackManager } from './rollback-manager.mjs';
import { createReceiptAggregator as _createReceiptAggregator } from './receipt-aggregator.mjs';
import { createWorkflowOrchestrator as _createWorkflowOrchestrator } from './workflow-orchestrator.mjs';

// Re-export for convenience
export const createResolver = _createDependencyResolver;

// Stage Execution
export {
  StageExecutor,
  createStageExecutor,
  StageType,
  StageStatus,
  StageResultSchema,
  StageConfigSchema
} from './stage-executor.mjs';

// Rollback Management
export {
  RollbackManager,
  createRollbackManager,
  withRollback,
  CheckpointSchema,
  TransactionSchema,
  RollbackResultSchema
} from './rollback-manager.mjs';

// Receipt Aggregation
export {
  ReceiptAggregator,
  createReceiptAggregator,
  PackageReceiptSchema,
  StageReceiptSchema,
  WorkflowReceiptSchema
} from './receipt-aggregator.mjs';

// Workflow Orchestration
export {
  WorkflowOrchestrator,
  createWorkflowOrchestrator,
  executeWorkflow,
  analyzeWorkflowImpact,
  WorkflowStatus,
  WorkflowConfigSchema,
  WorkflowResultSchema
} from './workflow-orchestrator.mjs';

// Parallel Orchestration - Worker Pool
export {
  WorkerPool,
  createWorkerPool,
  WorkerStatus,
  WorkerSchema,
  PoolConfigSchema
} from './worker-pool.mjs';

// Parallel Orchestration - Task Queue
export {
  TaskQueue,
  createTaskQueue,
  TaskPriority,
  TaskStatus,
  TaskSchema,
  QueueConfigSchema
} from './task-queue.mjs';

// Parallel Orchestration - Agent Router
export {
  AgentRouter,
  createAgentRouter,
  AgentRole,
  RoutingStrategy,
  AgentRegistrationSchema
} from './agent-router.mjs';

// Parallel Orchestration - Circuit Breaker
export {
  CircuitBreaker,
  createCircuitBreaker,
  withCircuitBreaker,
  CircuitState,
  CircuitBreakerConfigSchema
} from './circuit-breaker.mjs';

// Parallel Orchestration - Main Orchestrator
export {
  ParallelOrchestrator,
  createParallelOrchestrator,
  ObservableUpdateSchema,
  ParallelOrchestratorConfigSchema
} from './parallel-orchestrator.mjs';

/**
 * Default workflow stages
 */
export const DEFAULT_STAGES = [
  'dependency_analysis',
  'admission',
  'testing',
  'building',
  'validation',
  'integration',
  'commit'
];

/**
 * Create a complete orchestration pipeline
 *
 * @param {Object} [options] - Pipeline options
 * @returns {Object} Configured pipeline components
 *
 * @example
 * const pipeline = createOrchestrationPipeline({
 *   admissionEngine: myEngine,
 *   testRunner: myTestRunner
 * });
 *
 * const result = await pipeline.orchestrator.execute(config);
 */
export function createOrchestrationPipeline(options = {}) {
  const resolver = _createDependencyResolver(options.resolverOptions);
  const executor = _createStageExecutor(options.executorOptions);
  const rollbackManager = _createRollbackManager(options.rollbackOptions);
  const aggregator = _createReceiptAggregator();
  const orchestrator = _createWorkflowOrchestrator(options);

  return {
    resolver,
    executor,
    rollbackManager,
    aggregator,
    orchestrator,
    /**
     * Execute workflow using pipeline components
     * @param {Object} config - Workflow config
     * @returns {Promise<Object>} Result
     */
    execute: (config) => orchestrator.execute(config),
    /**
     * Analyze impact without executing
     * @param {Object} config - Workflow config
     * @returns {Object} Impact analysis
     */
    analyze: (config) => orchestrator.analyzeImpact(config)
  };
}

/**
 * Version information
 */
export const VERSION = '1.0.0';

/**
 * Module metadata
 */
export const MODULE_INFO = {
  name: '@unrdf/orchestration',
  version: VERSION,
  description: 'Multi-package atomic change orchestration with parallel agent execution',
  author: 'UNRDF Contributors',
  license: 'MIT'
};

// Import factory for parallel orchestrator
import { createParallelOrchestrator as _createParallelOrchestrator } from './parallel-orchestrator.mjs';

/**
 * Create a parallel orchestration system
 *
 * @param {Object} [config] - Configuration
 * @returns {ParallelOrchestrator} Parallel orchestrator
 *
 * @example
 * import { createParallelOrchestrationSystem, AgentRole } from '@unrdf/orchestration';
 *
 * const system = createParallelOrchestrationSystem({
 *   workerPool: { minWorkers: 2, maxWorkers: 10 },
 *   taskQueue: { maxSize: 1000 }
 * });
 *
 * await system.start();
 *
 * system.registerAgent({
 *   agentId: 'observer-1',
 *   role: AgentRole.OBSERVER,
 *   capabilities: ['observe', 'compress']
 * });
 *
 * const observable = system.submit({
 *   type: 'observe-task',
 *   capabilities: ['observe'],
 *   payload: { data: [...] }
 * });
 *
 * observable.on('complete', (result) => {
 *   console.log('Complete:', result);
 * });
 */
export function createParallelOrchestrationSystem(config = {}) {
  return _createParallelOrchestrator(config);
}
