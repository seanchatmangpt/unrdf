/**
 * @fileoverview Workflow Orchestrator - Main coordination engine for multi-package changes
 *
 * **Purpose**: Orchestrate atomic multi-package changes through:
 * 1. Dependency analysis - Detect affected packages and build execution order
 * 2. Admission - GOS gate check for all packages
 * 3. Testing - Run tests for all packages
 * 4. Building - Build artifacts for all packages
 * 5. Validation - Production readiness checks
 * 6. Integration - Cross-package integration tests
 * 7. Commit - Atomic write to universe O
 *
 * **Properties**:
 * - Atomic: All packages succeed or all rollback
 * - Ordered: Execute in topological dependency order
 * - Auditable: Full receipt trail for all operations
 * - Recoverable: Rollback on any failure
 *
 * @module orchestration/workflow-orchestrator
 */

import { z } from 'zod';
import { DependencyResolver } from './dependency-resolver.mjs';
import { StageExecutor, StageType, StageStatus } from './stage-executor.mjs';
import { RollbackManager, withRollback } from './rollback-manager.mjs';
import { ReceiptAggregator } from './receipt-aggregator.mjs';

/**
 * Workflow status enum
 */
export const WorkflowStatus = {
  PENDING: 'pending',
  RUNNING: 'running',
  COMPLETED: 'completed',
  FAILED: 'failed',
  ROLLED_BACK: 'rolled_back'
};

/**
 * Package schema for workflow config
 */
export const PackageSchema = z.object({
  dependencies: z.array(z.string()).default([]),
  devDependencies: z.array(z.string()).default([]),
  peerDependencies: z.array(z.string()).default([]),
  path: z.string().optional(),
  version: z.string().optional()
});

/**
 * Workflow configuration schema
 */
export const WorkflowConfigSchema = z.object({
  changedPackages: z.array(z.string()).min(1),
  packages: z.record(z.string(), PackageSchema),
  options: z.object({
    parallel: z.boolean().default(false),
    dryRun: z.boolean().default(false),
    skipStages: z.array(z.string()).default([]),
    timeouts: z.record(z.string(), z.number()).default({}),
    customExecutors: z.record(z.string(), z.function()).default({})
  }).optional()
});

/**
 * Workflow result schema
 */
export const WorkflowResultSchema = z.object({
  workflowId: z.string().uuid(),
  status: z.enum(['pending', 'running', 'completed', 'failed', 'rolled_back']),
  decision: z.enum(['allow', 'deny']),
  changedPackages: z.array(z.string()),
  affectedPackages: z.array(z.string()),
  executionOrder: z.array(z.string()),
  stages: z.array(z.object({
    name: z.string(),
    status: z.string(),
    duration: z.number().optional(),
    packages: z.array(z.object({
      name: z.string(),
      status: z.string(),
      duration: z.number().optional()
    }))
  })),
  receipt: z.any(),
  rollbackResult: z.any().optional(),
  startTime: z.string().datetime(),
  endTime: z.string().datetime(),
  duration: z.number()
});

/**
 * Workflow Orchestrator - Coordinates multi-package atomic changes
 *
 * @class WorkflowOrchestrator
 *
 * @example
 * const orchestrator = new WorkflowOrchestrator();
 *
 * const result = await orchestrator.execute({
 *   changedPackages: ['@unrdf/core'],
 *   packages: {
 *     '@unrdf/core': { dependencies: [] },
 *     '@unrdf/utils': { dependencies: ['@unrdf/core'] },
 *     '@unrdf/cli': { dependencies: ['@unrdf/core', '@unrdf/utils'] }
 *   }
 * });
 *
 * if (result.decision === 'allow') {
 *   console.log('All changes applied atomically');
 * } else {
 *   console.log('Changes rolled back:', result.rollbackResult);
 * }
 */
export class WorkflowOrchestrator {
  /**
   * Create a new workflow orchestrator
   *
   * @param {Object} [options] - Orchestrator options
   * @param {Object} [options.admissionEngine] - Admission engine for GOS checks
   * @param {Object} [options.testRunner] - Test runner
   * @param {Object} [options.builder] - Build system
   * @param {Object} [options.validator] - Validator
   * @param {Object} [options.integrationRunner] - Integration test runner
   * @param {Object} [options.universe] - Universe for commits
   * @param {Object} [options.receiptGenerator] - Receipt generator
   */
  constructor(options = {}) {
    /** @type {Object} Admission engine */
    this.admissionEngine = options.admissionEngine;

    /** @type {Object} Test runner */
    this.testRunner = options.testRunner;

    /** @type {Object} Builder */
    this.builder = options.builder;

    /** @type {Object} Validator */
    this.validator = options.validator;

    /** @type {Object} Integration runner */
    this.integrationRunner = options.integrationRunner;

    /** @type {Object} Universe */
    this.universe = options.universe;

    /** @type {Object} Receipt generator */
    this.receiptGenerator = options.receiptGenerator;

    /** @type {Map<string, Object>} Active workflows */
    this.workflows = new Map();

    /** @type {Array} Completed workflow history */
    this.history = [];
  }

  /**
   * Execute a multi-package workflow
   *
   * @param {Object} config - Workflow configuration
   * @returns {Promise<Object>} Workflow result
   */
  async execute(config) {
    const startTime = new Date();
    const workflowId = crypto.randomUUID();

    // Validate configuration
    const validated = WorkflowConfigSchema.parse(config);
    const options = validated.options || {};

    // Initialize components
    const resolver = new DependencyResolver();
    const executor = StageExecutor.createDefault();
    const rollbackManager = new RollbackManager();
    const aggregator = new ReceiptAggregator(workflowId);

    // Register workflow
    const workflow = {
      id: workflowId,
      status: WorkflowStatus.RUNNING,
      config: validated,
      startTime: startTime.toISOString()
    };
    this.workflows.set(workflowId, workflow);

    try {
      // Phase 1: Dependency Analysis
      resolver.addPackages(validated.packages);
      const resolution = resolver.resolve(validated.changedPackages);

      if (!resolution.success) {
        throw new Error(`Dependency resolution failed: ${resolution.error}`);
      }

      const { order: executionOrder, closure: affectedPackages } = resolution;

      // Begin transaction
      const txId = rollbackManager.beginTransaction(affectedPackages);

      // Phase 2-7: Execute stages for each package
      const stageResults = [];

      // Determine stages to run
      const stagesToRun = [
        StageType.ADMISSION,
        StageType.TESTING,
        StageType.BUILDING,
        StageType.VALIDATION,
        StageType.INTEGRATION,
        StageType.COMMIT
      ].filter(s => !options.skipStages?.includes(s));

      // Execute each stage
      for (const stageType of stagesToRun) {
        const stageStartTime = Date.now();
        const packageResults = [];
        let stageSuccess = true;

        if (stageType === StageType.INTEGRATION || stageType === StageType.COMMIT) {
          // These stages run once for all packages
          const context = this._buildContext(stageType, null, {
            packages: executionOrder,
            resolver,
            rollbackManager,
            changes: validated.changedPackages.map(p => ({
              partition: p,
              quads: []
            }))
          });

          // Checkpoint before stage
          await rollbackManager.checkpoint('workflow', stageType, {
            packages: executionOrder,
            stage: stageType
          });

          const result = await executor.executeStage(stageType, context);

          // Add to aggregator
          await aggregator.addPackageReceipt(
            stageType,
            'workflow',
            result.success ? 'allow' : 'deny',
            { duration: result.duration, output: result.output }
          );

          packageResults.push({
            name: 'workflow',
            status: result.status,
            duration: result.duration
          });

          if (!result.success) {
            stageSuccess = false;
          }
        } else {
          // These stages run per-package
          for (const pkg of executionOrder) {
            const context = this._buildContext(stageType, pkg, {
              resolver,
              rollbackManager
            });

            // Checkpoint before each package
            await rollbackManager.checkpoint(pkg, stageType, {
              package: pkg,
              stage: stageType
            });

            // Record operation
            rollbackManager.recordOperation(stageType, pkg, stageType, {
              reversible: true
            });

            const result = await executor.executeStage(stageType, context);

            // Add to aggregator
            await aggregator.addPackageReceipt(
              stageType,
              pkg,
              result.success ? 'allow' : 'deny',
              { duration: result.duration, output: result.output }
            );

            packageResults.push({
              name: pkg,
              status: result.status,
              duration: result.duration
            });

            if (!result.success) {
              stageSuccess = false;
              break; // Fail fast
            }
          }
        }

        // Finalize stage receipt
        await aggregator.finalizeStage(stageType);

        stageResults.push({
          name: stageType,
          status: stageSuccess ? StageStatus.COMPLETED : StageStatus.FAILED,
          duration: Date.now() - stageStartTime,
          packages: packageResults
        });

        if (!stageSuccess) {
          // Rollback on failure
          const rollbackResult = await rollbackManager.rollback(txId);

          // Add rollback trail to aggregator
          for (const entry of rollbackResult.restoredStates || []) {
            aggregator.addRollbackTrail(
              entry.stage,
              entry.checkpoint,
              entry.state
            );
          }

          // Finalize workflow receipt (denied)
          const receipt = await aggregator.finalizeWorkflow({
            changedPackages: validated.changedPackages,
            affectedPackages,
            executionOrder
          });

          const endTime = new Date();
          const result = {
            workflowId,
            status: WorkflowStatus.ROLLED_BACK,
            decision: 'deny',
            changedPackages: validated.changedPackages,
            affectedPackages,
            executionOrder,
            stages: stageResults,
            receipt,
            rollbackResult,
            startTime: startTime.toISOString(),
            endTime: endTime.toISOString(),
            duration: endTime - startTime
          };

          // Update workflow
          workflow.status = WorkflowStatus.ROLLED_BACK;
          workflow.result = result;
          this.history.push(workflow);
          this.workflows.delete(workflowId);

          return result;
        }
      }

      // All stages passed - commit transaction
      rollbackManager.commit(txId);

      // Finalize workflow receipt (allowed)
      const receipt = await aggregator.finalizeWorkflow({
        changedPackages: validated.changedPackages,
        affectedPackages,
        executionOrder
      });

      const endTime = new Date();
      const result = {
        workflowId,
        status: WorkflowStatus.COMPLETED,
        decision: 'allow',
        changedPackages: validated.changedPackages,
        affectedPackages,
        executionOrder,
        stages: stageResults,
        receipt,
        startTime: startTime.toISOString(),
        endTime: endTime.toISOString(),
        duration: endTime - startTime
      };

      // Update workflow
      workflow.status = WorkflowStatus.COMPLETED;
      workflow.result = result;
      this.history.push(workflow);
      this.workflows.delete(workflowId);

      return result;

    } catch (error) {
      // Handle unexpected errors
      const endTime = new Date();

      const result = {
        workflowId,
        status: WorkflowStatus.FAILED,
        decision: 'deny',
        changedPackages: validated.changedPackages,
        affectedPackages: [],
        executionOrder: [],
        stages: [],
        error: error.message,
        startTime: startTime.toISOString(),
        endTime: endTime.toISOString(),
        duration: endTime - startTime
      };

      workflow.status = WorkflowStatus.FAILED;
      workflow.result = result;
      workflow.error = error.message;
      this.history.push(workflow);
      this.workflows.delete(workflowId);

      return result;
    }
  }

  /**
   * Build context for a stage execution
   *
   * @param {string} stageType - Stage type
   * @param {string|null} packageName - Package name (null for workflow-level stages)
   * @param {Object} extras - Additional context
   * @returns {Object} Execution context
   * @private
   */
  _buildContext(stageType, packageName, extras = {}) {
    const base = {
      packageName,
      engine: this.admissionEngine,
      testRunner: this.testRunner,
      builder: this.builder,
      validator: this.validator,
      integrationRunner: this.integrationRunner,
      universe: this.universe,
      receiptGenerator: this.receiptGenerator,
      ...extras
    };

    return base;
  }

  /**
   * Execute a dry-run workflow (no actual changes)
   *
   * @param {Object} config - Workflow configuration
   * @returns {Promise<Object>} Simulated workflow result
   */
  async dryRun(config) {
    return this.execute({
      ...config,
      options: {
        ...config.options,
        dryRun: true
      }
    });
  }

  /**
   * Get workflow status
   *
   * @param {string} workflowId - Workflow ID
   * @returns {Object|undefined} Workflow status
   */
  getWorkflow(workflowId) {
    return this.workflows.get(workflowId) ||
           this.history.find(w => w.id === workflowId);
  }

  /**
   * Get active workflows
   *
   * @returns {Object[]} Active workflows
   */
  getActiveWorkflows() {
    return Array.from(this.workflows.values());
  }

  /**
   * Get workflow history
   *
   * @param {number} [limit=10] - Number of entries
   * @returns {Object[]} Workflow history
   */
  getHistory(limit = 10) {
    return this.history.slice(-limit);
  }

  /**
   * Get orchestrator statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    const completed = this.history.filter(w => w.status === WorkflowStatus.COMPLETED);
    const failed = this.history.filter(w => w.status === WorkflowStatus.FAILED);
    const rolledBack = this.history.filter(w => w.status === WorkflowStatus.ROLLED_BACK);

    return {
      activeWorkflows: this.workflows.size,
      totalCompleted: completed.length,
      totalFailed: failed.length,
      totalRolledBack: rolledBack.length,
      successRate: this.history.length > 0
        ? ((completed.length / this.history.length) * 100).toFixed(2) + '%'
        : 'N/A',
      averageDuration: this.history.length > 0
        ? Math.round(this.history.reduce((sum, w) => sum + (w.result?.duration || 0), 0) / this.history.length)
        : 0
    };
  }

  /**
   * Analyze impact of proposed changes
   *
   * @param {Object} config - Workflow configuration
   * @returns {Object} Impact analysis
   */
  analyzeImpact(config) {
    const validated = WorkflowConfigSchema.parse(config);

    const resolver = new DependencyResolver();
    resolver.addPackages(validated.packages);

    const resolution = resolver.resolve(validated.changedPackages);

    if (!resolution.success) {
      return {
        success: false,
        error: resolution.error
      };
    }

    return {
      success: true,
      changedPackages: validated.changedPackages,
      affectedPackages: resolution.closure,
      executionOrder: resolution.order,
      executionLevels: resolution.levels,
      impactAnalysis: resolution.impactAnalysis,
      recommendation: this._generateRecommendation(resolution)
    };
  }

  /**
   * Generate recommendation based on impact analysis
   *
   * @param {Object} resolution - Dependency resolution result
   * @returns {Object} Recommendation
   * @private
   */
  _generateRecommendation(resolution) {
    const affectedCount = resolution.closure.length;
    const levels = resolution.levels.length;

    let riskLevel = 'low';
    let recommendation = 'Proceed with workflow';

    if (affectedCount > 10) {
      riskLevel = 'high';
      recommendation = 'Consider splitting into smaller changes';
    } else if (affectedCount > 5) {
      riskLevel = 'medium';
      recommendation = 'Run integration tests thoroughly';
    }

    // Check for critical packages
    const criticalImpact = Object.values(resolution.impactAnalysis || {})
      .some(a => a.impactLevel === 'critical');

    if (criticalImpact) {
      riskLevel = 'critical';
      recommendation = 'This change affects critical packages. Require additional review.';
    }

    return {
      riskLevel,
      recommendation,
      estimatedDuration: `${levels * 2}-${levels * 5} minutes`,
      parallelizableStages: ['testing', 'building', 'validation']
    };
  }

  /**
   * Create orchestrator with default configuration
   *
   * @param {Object} [options] - Options
   * @returns {WorkflowOrchestrator}
   */
  static createDefault(options = {}) {
    return new WorkflowOrchestrator(options);
  }
}

/**
 * Create a workflow orchestrator
 *
 * @param {Object} [options] - Orchestrator options
 * @returns {WorkflowOrchestrator}
 */
export function createWorkflowOrchestrator(options = {}) {
  return new WorkflowOrchestrator(options);
}

/**
 * Execute a multi-package workflow
 *
 * Convenience function for one-off workflow execution.
 *
 * @param {Object} config - Workflow configuration
 * @param {Object} [options] - Orchestrator options
 * @returns {Promise<Object>} Workflow result
 */
export async function executeWorkflow(config, options = {}) {
  const orchestrator = new WorkflowOrchestrator(options);
  return orchestrator.execute(config);
}

/**
 * Analyze impact of proposed changes
 *
 * Convenience function for impact analysis.
 *
 * @param {Object} config - Workflow configuration
 * @returns {Object} Impact analysis
 */
export function analyzeWorkflowImpact(config) {
  const orchestrator = new WorkflowOrchestrator();
  return orchestrator.analyzeImpact(config);
}
