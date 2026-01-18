/**
 * @file Worklet Runner - Core worklet execution engine
 * @module @unrdf/yawl/worklets/runner
 *
 * @description
 * Implements Java YAWL YExletRunner functionality:
 * - Dynamic worklet selection via RDR (Ripple Down Rules)
 * - Sub-workflow invocation with parent context passing
 * - Completion handling (success/failure/compensation)
 * - Receipt chain linking parent ↔ worklet
 *
 * Based on: org.yawlfoundation.yawl.worklet.YExletRunner
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';
import { createReceipt } from '../receipt-core.mjs';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Worklet execution state
 */
export const WorkletStateSchema = z.enum([
  'pending',      // Worklet selected, not yet started
  'executing',    // Sub-workflow running
  'completed',    // Successfully finished
  'failed',       // Execution failed
  'compensating', // Running compensation logic
  'compensated',  // Compensation complete
]);

/**
 * Worklet execution result
 */
export const WorkletResultSchema = z.object({
  success: z.boolean(),
  action: z.enum(['resume', 'cancel', 'retry', 'compensate']),
  outputData: z.record(z.string(), z.any()).optional(),
  error: z.string().optional(),
  compensationActions: z.array(z.string()).optional(),
});

/**
 * Worklet execution context (passed to worklet)
 */
export const WorkletExecutionContextSchema = z.object({
  workletId: z.string().uuid(),
  parentWorkItemId: z.string().uuid(),
  parentCaseId: z.string().uuid(),
  parentTaskId: z.string().min(1),
  exceptionType: z.string().min(1),
  exceptionData: z.record(z.string(), z.any()),
  caseData: z.record(z.string(), z.any()),
  timestamp: z.coerce.date(),
});

/**
 * Worklet execution instance
 */
export const WorkletExecutionSchema = z.object({
  id: z.string().uuid(),
  workletId: z.string().min(1),
  parentWorkItemId: z.string().uuid(),
  parentCaseId: z.string().uuid(),
  parentTaskId: z.string().min(1),
  exceptionType: z.string().min(1),
  state: WorkletStateSchema,
  startedAt: z.coerce.date(),
  completedAt: z.coerce.date().optional(),
  result: WorkletResultSchema.optional(),
  receiptIds: z.array(z.string()),
  subCaseId: z.string().uuid().optional(),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * @typedef {z.infer<typeof WorkletExecutionSchema>} WorkletExecution
 * @typedef {z.infer<typeof WorkletExecutionContextSchema>} WorkletExecutionContext
 * @typedef {z.infer<typeof WorkletResultSchema>} WorkletResult
 */

// ============================================================================
// WORKLET RUNNER
// ============================================================================

/**
 * Worklet Runner - Executes exception handlers as sub-workflows
 *
 * Implements the worklet pattern from Java YAWL:
 * 1. Exception occurs in parent workflow
 * 2. Select appropriate worklet handler via RDR
 * 3. Create sub-workflow instance with parent context
 * 4. Execute worklet
 * 5. On completion: resume/cancel/retry parent based on result
 *
 * @example
 * const runner = new WorkletRunner({ registry, engine });
 *
 * const execution = await runner.launchWorklet(
 *   'timeout',
 *   { workItemId, caseId, taskId },
 *   { amount: 5000 }
 * );
 *
 * // Worklet executes asynchronously
 * await execution.promise;
 *
 * // Check result
 * if (execution.result.action === 'resume') {
 *   // Resume parent work item
 * }
 */
export class WorkletRunner {
  /**
   * Create worklet runner
   * @param {Object} config - Runner configuration
   * @param {Object} config.registry - Worklet registry for handler selection
   * @param {Object} config.engine - YAWL engine for sub-workflow execution
   * @param {Object} [config.receiptLogger] - Receipt logger for audit trail
   * @param {Function} [config.onWorkletStart] - Callback when worklet starts
   * @param {Function} [config.onWorkletComplete] - Callback when worklet completes
   */
  constructor(config) {
    this.registry = config.registry;
    this.engine = config.engine;
    this.receiptLogger = config.receiptLogger || null;
    this.onWorkletStart = config.onWorkletStart || null;
    this.onWorkletComplete = config.onWorkletComplete || null;

    /** @type {Map<string, WorkletExecution>} */
    this.executions = new Map();

    /** @type {Map<string, WorkletExecution[]>} */
    this.executionsByParent = new Map();
  }

  /**
   * Launch worklet for exception handling
   *
   * @param {string} exceptionType - Exception type (timeout, constraint_violation, etc.)
   * @param {Object} parentContext - Parent workflow context
   * @param {string} parentContext.workItemId - Parent work item ID
   * @param {string} parentContext.caseId - Parent case ID
   * @param {string} parentContext.taskId - Parent task ID
   * @param {Object} exceptionData - Exception-specific data
   * @param {Object} [caseData={}] - Parent case data to pass to worklet
   * @returns {Promise<WorkletExecution>} Worklet execution instance
   * @throws {Error} If no handler found or worklet fails to start
   *
   * @example
   * const execution = await runner.launchWorklet(
   *   'timeout',
   *   { workItemId: 'wi-1', caseId: 'case-1', taskId: 'approve' },
   *   { durationMs: 120000, timeoutMs: 60000 },
   *   { amount: 10000, approver: 'manager-1' }
   * );
   */
  async launchWorklet(exceptionType, parentContext, exceptionData, caseData = {}) {
    // 1. Select worklet handler via RDR
    const handler = this.registry.selectHandler(exceptionType, { ...caseData, ...exceptionData });

    if (!handler) {
      throw new Error(`No worklet handler found for exception type: ${exceptionType}`);
    }

    // 2. Create worklet execution instance
    const execution = {
      id: randomUUID(),
      workletId: handler.id,
      parentWorkItemId: parentContext.workItemId,
      parentCaseId: parentContext.caseId,
      parentTaskId: parentContext.taskId,
      exceptionType,
      state: 'pending',
      startedAt: new Date(),
      receiptIds: [],
      metadata: {
        handlerPriority: handler.priority,
        handlerCondition: handler.condition,
      },
    };

    WorkletExecutionSchema.parse(execution);

    // 3. Store execution
    this.executions.set(execution.id, execution);

    if (!this.executionsByParent.has(parentContext.workItemId)) {
      this.executionsByParent.set(parentContext.workItemId, []);
    }
    this.executionsByParent.get(parentContext.workItemId).push(execution);

    // 4. Log worklet launch receipt
    if (this.receiptLogger) {
      const receipt = this.receiptLogger.log('WORKLET_LAUNCHED', {
        executionId: execution.id,
        workletId: handler.id,
        parentWorkItemId: parentContext.workItemId,
        parentCaseId: parentContext.caseId,
        exceptionType,
        timestamp: execution.startedAt,
      });
      execution.receiptIds.push(receipt.id);
    }

    // 5. Create worklet context
    const context = {
      workletId: execution.id,
      parentWorkItemId: parentContext.workItemId,
      parentCaseId: parentContext.caseId,
      parentTaskId: parentContext.taskId,
      exceptionType,
      exceptionData,
      caseData,
      timestamp: execution.startedAt,
    };

    WorkletExecutionContextSchema.parse(context);

    // 6. Execute worklet asynchronously
    execution.state = 'executing';

    if (this.onWorkletStart) {
      try {
        await this.onWorkletStart(execution, context);
      } catch {
        // Ignore callback errors
      }
    }

    // Execute worklet workflow
    this._executeWorklet(execution, handler, context)
      .catch(error => {
        execution.state = 'failed';
        execution.completedAt = new Date();
        execution.result = {
          success: false,
          action: 'cancel',
          error: error.message,
        };

        this._logWorkletCompletion(execution);
      });

    return execution;
  }

  /**
   * Execute worklet workflow (internal)
   * @private
   */
  async _executeWorklet(execution, handler, context) {
    try {
      // Execute handler workflow/function
      let result;

      if (typeof handler.workflow === 'function') {
        // Handler is a function
        result = await handler.workflow(context);
      } else if (typeof handler.workflow === 'object') {
        // Handler is a workflow specification - create sub-case
        const subCase = await this.engine.launchCase(
          handler.workflow.id || handler.id,
          {
            ...context.caseData,
            _workletContext: context,
          }
        );

        execution.subCaseId = subCase.id;

        // Wait for sub-case completion
        result = await this._waitForCaseCompletion(subCase.id);
      } else {
        throw new Error(`Invalid worklet handler type for ${handler.id}`);
      }

      // Parse result
      const parsedResult = WorkletResultSchema.parse(result);

      execution.state = 'completed';
      execution.completedAt = new Date();
      execution.result = parsedResult;

      this._logWorkletCompletion(execution);

      // Handle compensation if needed
      if (parsedResult.action === 'compensate' && parsedResult.compensationActions) {
        await this._executeCompensation(execution, parsedResult.compensationActions);
      }

      return parsedResult;
    } catch (error) {
      execution.state = 'failed';
      execution.completedAt = new Date();
      execution.result = {
        success: false,
        action: 'cancel',
        error: error.message,
      };

      this._logWorkletCompletion(execution);
      throw error;
    }
  }

  /**
   * Wait for case completion (polling)
   * @private
   */
  async _waitForCaseCompletion(caseId) {
    const maxWait = 300000; // 5 minutes
    const pollInterval = 1000; // 1 second
    const startTime = Date.now();

    while (Date.now() - startTime < maxWait) {
      const caseInstance = await this.engine.getCase(caseId);

      if (caseInstance.status === 'completed') {
        return {
          success: true,
          action: 'resume',
          outputData: caseInstance.data,
        };
      }

      if (caseInstance.status === 'failed' || caseInstance.status === 'cancelled') {
        return {
          success: false,
          action: 'cancel',
          error: `Sub-case ${caseInstance.status}`,
        };
      }

      await new Promise(resolve => setTimeout(resolve, pollInterval));
    }

    throw new Error(`Worklet timeout: sub-case ${caseId} did not complete within ${maxWait}ms`);
  }

  /**
   * Execute compensation actions
   * @private
   */
  async _executeCompensation(execution, compensationActions) {
    execution.state = 'compensating';

    if (this.receiptLogger) {
      const receipt = this.receiptLogger.log('WORKLET_COMPENSATING', {
        executionId: execution.id,
        compensationActions,
        timestamp: new Date(),
      });
      execution.receiptIds.push(receipt.id);
    }

    // Execute compensation actions (placeholder - actual implementation via compensation handler)
    // In production, this would invoke the CompensationHandler
    for (const action of compensationActions) {
      // Execute each compensation action
      // TODO: Integrate with CompensationHandler
    }

    execution.state = 'compensated';

    if (this.receiptLogger) {
      const receipt = this.receiptLogger.log('WORKLET_COMPENSATED', {
        executionId: execution.id,
        timestamp: new Date(),
      });
      execution.receiptIds.push(receipt.id);
    }
  }

  /**
   * Log worklet completion
   * @private
   */
  _logWorkletCompletion(execution) {
    if (this.receiptLogger) {
      const receipt = this.receiptLogger.log('WORKLET_COMPLETED', {
        executionId: execution.id,
        workletId: execution.workletId,
        parentWorkItemId: execution.parentWorkItemId,
        state: execution.state,
        result: execution.result,
        timestamp: execution.completedAt || new Date(),
      });
      execution.receiptIds.push(receipt.id);
    }

    if (this.onWorkletComplete) {
      try {
        this.onWorkletComplete(execution);
      } catch {
        // Ignore callback errors
      }
    }
  }

  /**
   * Get worklet execution by ID
   * @param {string} executionId - Execution ID
   * @returns {WorkletExecution|null} Execution or null if not found
   */
  getExecution(executionId) {
    return this.executions.get(executionId) || null;
  }

  /**
   * Get worklet executions for parent work item
   * @param {string} workItemId - Parent work item ID
   * @returns {WorkletExecution[]} Array of executions
   */
  getExecutionsForWorkItem(workItemId) {
    return this.executionsByParent.get(workItemId) || [];
  }

  /**
   * Get all active worklet executions
   * @returns {WorkletExecution[]} Array of active executions
   */
  getActiveExecutions() {
    return Array.from(this.executions.values())
      .filter(e => e.state === 'executing' || e.state === 'compensating');
  }

  /**
   * Cancel worklet execution
   * @param {string} executionId - Execution ID
   * @returns {boolean} True if cancelled, false if not found or already complete
   */
  async cancelExecution(executionId) {
    const execution = this.executions.get(executionId);

    if (!execution || execution.state === 'completed' || execution.state === 'failed') {
      return false;
    }

    execution.state = 'failed';
    execution.completedAt = new Date();
    execution.result = {
      success: false,
      action: 'cancel',
      error: 'Worklet cancelled by user',
    };

    // Cancel sub-case if exists
    if (execution.subCaseId && this.engine.cancelCase) {
      await this.engine.cancelCase(execution.subCaseId);
    }

    this._logWorkletCompletion(execution);

    return true;
  }
}

/**
 * Create worklet runner
 * @param {Object} config - Runner configuration
 * @returns {WorkletRunner} Worklet runner instance
 */
export function createWorkletRunner(config) {
  return new WorkletRunner(config);
}
