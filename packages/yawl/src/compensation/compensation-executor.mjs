/**
 * @file Compensation Executor - Execute Undo Operations
 * @module yawl/compensation/executor
 *
 * @description
 * Executes compensation workflows to undo completed work.
 * Handles retry logic, invariant checking, and receipt logging.
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Schema for compensation execution context
 */
export const CompensationContextSchema = z.object({
  /** Execution ID */
  id: z.string().uuid(),
  /** Task being compensated */
  taskId: z.string().min(1),
  /** Compensation spec ID */
  compensationSpecId: z.string().uuid(),
  /** Original task output (input to compensation) */
  originalData: z.any(),
  /** Compensation status */
  status: z.enum(['pending', 'executing', 'completed', 'failed', 'retrying']),
  /** Attempt number */
  attemptNumber: z.number().int().positive().default(1),
  /** Maximum attempts */
  maxAttempts: z.number().int().positive().default(3),
  /** Started at timestamp */
  startedAt: z.coerce.date(),
  /** Completed at timestamp */
  completedAt: z.coerce.date().optional(),
  /** Error if failed */
  error: z.any().optional(),
  /** Compensation output */
  output: z.any().optional(),
});

/**
 * Schema for invariant check
 */
export const InvariantCheckSchema = z.object({
  /** Invariant name */
  name: z.string().min(1),
  /** Check function */
  check: z.function().returns(z.promise(z.boolean())),
  /** Error message if failed */
  errorMessage: z.string().optional(),
});

/**
 * @typedef {z.infer<typeof CompensationContextSchema>} CompensationContext
 * @typedef {z.infer<typeof InvariantCheckSchema>} InvariantCheck
 */

// ============================================================================
// COMPENSATION EXECUTOR
// ============================================================================

/**
 * Executes compensation workflows with retry and verification
 */
export class CompensationExecutor {
  /**
   * @param {Object} config
   * @param {Object} config.compensationRegistry - Compensation registry
   * @param {Object} [config.receiptLogger] - Receipt logger for audit trail
   * @param {Function} [config.onCompensationStart] - Callback when compensation starts
   * @param {Function} [config.onCompensationComplete] - Callback when compensation completes
   * @param {Function} [config.onCompensationFailed] - Callback when compensation fails
   */
  constructor(config) {
    if (!config.compensationRegistry) {
      throw new Error('CompensationExecutor requires compensationRegistry');
    }

    this.compensationRegistry = config.compensationRegistry;
    this.receiptLogger = config.receiptLogger;
    this.onCompensationStart = config.onCompensationStart;
    this.onCompensationComplete = config.onCompensationComplete;
    this.onCompensationFailed = config.onCompensationFailed;

    /** @type {Map<string, CompensationContext>} */
    this.executions = new Map();

    /** @type {Map<string, InvariantCheck[]>} Task ID → Invariants */
    this.invariants = new Map();
  }

  /**
   * Execute compensation for a task
   * @param {string} taskId - Task to compensate
   * @param {*} originalData - Original task output
   * @param {Object} [options] - Execution options
   * @returns {Promise<CompensationContext>} Compensation execution result
   *
   * @example
   * await executor.executeCompensation('book-hotel', {
   *   bookingId: 'booking-123',
   *   amount: 200
   * });
   */
  async executeCompensation(taskId, originalData, options = {}) {
    const spec = this.compensationRegistry.getCompensation(taskId);
    if (!spec) {
      throw new Error(`No compensation registered for task: ${taskId}`);
    }

    const context = {
      id: randomUUID(),
      taskId,
      compensationSpecId: spec.id,
      originalData,
      status: 'pending',
      attemptNumber: 1,
      maxAttempts: spec.retryPolicy?.maxAttempts || 3,
      startedAt: new Date(),
    };

    const validated = CompensationContextSchema.parse(context);
    this.executions.set(validated.id, validated);

    if (this.onCompensationStart) {
      this.onCompensationStart({ context: validated, spec });
    }

    if (this.receiptLogger) {
      this.receiptLogger.log('COMPENSATION_STARTED', {
        executionId: validated.id,
        taskId,
        compensationSpecId: spec.id,
        startedAt: validated.startedAt.toISOString(),
      });
    }

    try {
      // Execute compensation with retry
      await this._executeWithRetry(validated, spec);

      // Verify invariants
      await this._verifyInvariants(taskId, validated);

      validated.status = 'completed';
      validated.completedAt = new Date();

      if (this.onCompensationComplete) {
        this.onCompensationComplete({ context: validated, spec });
      }

      if (this.receiptLogger) {
        this.receiptLogger.log('COMPENSATION_COMPLETED', {
          executionId: validated.id,
          taskId,
          completedAt: validated.completedAt.toISOString(),
          attempts: validated.attemptNumber,
        });
      }

      return validated;

    } catch (error) {
      validated.status = 'failed';
      validated.error = error;
      validated.completedAt = new Date();

      if (this.onCompensationFailed) {
        this.onCompensationFailed({ context: validated, spec, error });
      }

      if (this.receiptLogger) {
        this.receiptLogger.log('COMPENSATION_FAILED', {
          executionId: validated.id,
          taskId,
          error: error.message,
          attempts: validated.attemptNumber,
          failedAt: validated.completedAt.toISOString(),
        });
      }

      throw error;
    }
  }

  /**
   * Execute compensation workflow with retry logic
   * @param {CompensationContext} context - Execution context
   * @param {Object} spec - Compensation spec
   * @private
   */
  async _executeWithRetry(context, spec) {
    const backoffMs = spec.retryPolicy?.backoffMs || 1000;

    while (context.attemptNumber <= context.maxAttempts) {
      try {
        context.status = context.attemptNumber === 1 ? 'executing' : 'retrying';

        // Map input from original data
        const compensationInput = this._mapInput(context.originalData, spec.inputMapping);

        // Execute compensation workflow
        context.output = await this._executeWorkflow(spec, compensationInput);

        return;

      } catch (error) {
        if (context.attemptNumber >= context.maxAttempts) {
          throw error;
        }

        context.attemptNumber++;

        // Exponential backoff
        const delay = backoffMs * Math.pow(2, context.attemptNumber - 2);
        await new Promise(resolve => setTimeout(resolve, delay));
      }
    }
  }

  /**
   * Map original data to compensation input
   * @param {*} originalData - Original task output
   * @param {Object} mapping - Input mapping
   * @returns {*} Mapped input
   * @private
   */
  _mapInput(originalData, mapping = {}) {
    if (!mapping || Object.keys(mapping).length === 0) {
      return originalData;
    }

    const mapped = {};
    for (const [targetKey, sourceKey] of Object.entries(mapping)) {
      if (originalData && typeof originalData === 'object' && sourceKey in originalData) {
        mapped[targetKey] = originalData[sourceKey];
      }
    }
    return mapped;
  }

  /**
   * Execute compensation workflow tasks
   * @param {Object} spec - Compensation spec
   * @param {*} input - Workflow input
   * @returns {Promise<*>} Workflow output
   * @private
   */
  async _executeWorkflow(spec, input) {
    let output = input;

    for (const task of spec.workflowSpec.tasks) {
      if (task.execute) {
        output = await task.execute({ input: output });
      }
    }

    return output;
  }

  /**
   * Verify invariants after compensation
   * @param {string} taskId - Task ID
   * @param {CompensationContext} context - Execution context
   * @private
   */
  async _verifyInvariants(taskId, context) {
    const checks = this.invariants.get(taskId) || [];

    for (const invariant of checks) {
      const result = await invariant.check(context.output);
      if (!result) {
        const message = invariant.errorMessage || `Invariant check failed: ${invariant.name}`;
        throw new Error(message);
      }
    }
  }

  /**
   * Register an invariant check for a task
   * @param {string} taskId - Task ID
   * @param {string} name - Invariant name
   * @param {Function} check - Check function
   * @param {string} [errorMessage] - Error message if check fails
   *
   * @example
   * executor.registerInvariant('book-hotel', 'booking-cancelled', async (output) => {
   *   const booking = await getBooking(output.bookingId);
   *   return booking.status === 'cancelled';
   * }, 'Hotel booking was not cancelled');
   */
  registerInvariant(taskId, name, check, errorMessage) {
    const invariant = InvariantCheckSchema.parse({
      name,
      check,
      errorMessage,
    });

    if (!this.invariants.has(taskId)) {
      this.invariants.set(taskId, []);
    }

    this.invariants.get(taskId).push(invariant);
  }

  /**
   * Get compensation execution by ID
   * @param {string} executionId - Execution ID
   * @returns {CompensationContext|null} Execution context or null
   */
  getExecution(executionId) {
    return this.executions.get(executionId) || null;
  }

  /**
   * Get all executions for a task
   * @param {string} taskId - Task ID
   * @returns {CompensationContext[]} Executions
   */
  getExecutionsForTask(taskId) {
    return Array.from(this.executions.values())
      .filter(ctx => ctx.taskId === taskId);
  }

  /**
   * Get execution statistics
   * @returns {Object} Statistics
   */
  getStats() {
    const executions = Array.from(this.executions.values());
    return {
      total: executions.length,
      completed: executions.filter(e => e.status === 'completed').length,
      failed: executions.filter(e => e.status === 'failed').length,
      inProgress: executions.filter(e => e.status === 'executing' || e.status === 'retrying').length,
      avgAttempts: executions.length > 0
        ? executions.reduce((sum, e) => sum + e.attemptNumber, 0) / executions.length
        : 0,
    };
  }

  /**
   * Clear all executions
   */
  clear() {
    this.executions.clear();
  }
}

/**
 * Create a new compensation executor
 * @param {Object} config - Configuration
 * @returns {CompensationExecutor} New executor instance
 */
export function createCompensationExecutor(config) {
  return new CompensationExecutor(config);
}
