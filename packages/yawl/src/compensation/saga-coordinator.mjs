/**
 * @file Saga Coordinator - Transaction Coordination
 * @module yawl/compensation/saga-coordinator
 *
 * @description
 * Coordinates saga execution with forward and backward phases.
 * Implements the saga pattern for distributed transactions.
 *
 * Forward phase: Execute tasks sequentially
 * Backward phase: Compensate in reverse order on failure
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Schema for saga task
 */
export const SagaTaskSchema = z.object({
  /** Task ID */
  taskId: z.string().min(1),
  /** Task execution function */
  execute: z.function(),
  /** Compensation function (optional if registered) */
  compensate: z.function().optional(),
  /** Task input data */
  input: z.any().optional(),
  /** Task output (populated after execution) */
  output: z.any().optional(),
  /** Execution status */
  status: z.enum(['pending', 'executing', 'completed', 'failed', 'compensating', 'compensated']).default('pending'),
  /** Started at timestamp */
  startedAt: z.coerce.date().optional(),
  /** Completed at timestamp */
  completedAt: z.coerce.date().optional(),
  /** Error if failed */
  error: z.any().optional(),
});

/**
 * Schema for saga execution
 */
export const SagaExecutionSchema = z.object({
  /** Saga ID */
  id: z.string().uuid(),
  /** Saga name */
  name: z.string().min(1).optional(),
  /** Tasks in execution order */
  tasks: z.array(SagaTaskSchema),
  /** Current phase */
  phase: z.enum(['forward', 'backward', 'completed', 'failed']),
  /** Started at timestamp */
  startedAt: z.coerce.date(),
  /** Completed at timestamp */
  completedAt: z.coerce.date().optional(),
  /** Success flag */
  success: z.boolean().optional(),
  /** Error if failed */
  error: z.any().optional(),
});

/**
 * @typedef {z.infer<typeof SagaTaskSchema>} SagaTask
 * @typedef {z.infer<typeof SagaExecutionSchema>} SagaExecution
 */

// ============================================================================
// SAGA COORDINATOR
// ============================================================================

/**
 * Coordinates saga pattern execution with compensation
 */
export class SagaCoordinator {
  /**
   * @param {Object} config
   * @param {Object} [config.compensationRegistry] - Compensation registry
   * @param {Function} [config.onTaskCompleted] - Callback when task completes
   * @param {Function} [config.onCompensation] - Callback when compensation starts
   * @param {Function} [config.onSagaCompleted] - Callback when saga completes
   */
  constructor(config = {}) {
    this.compensationRegistry = config.compensationRegistry;
    this.onTaskCompleted = config.onTaskCompleted;
    this.onCompensation = config.onCompensation;
    this.onSagaCompleted = config.onSagaCompleted;

    /** @type {Map<string, SagaExecution>} */
    this.executions = new Map();
  }

  /**
   * Start a new saga execution
   * @param {SagaTask[]} tasks - Tasks to execute
   * @param {Object} [options] - Execution options
   * @returns {Promise<SagaExecution>} Saga execution result
   *
   * @example
   * const result = await coordinator.startSaga([
   *   {
   *     taskId: 'book-hotel',
   *     execute: async () => await bookHotel(),
   *   },
   *   {
   *     taskId: 'book-flight',
   *     execute: async () => await bookFlight(),
   *   }
   * ]);
   */
  async startSaga(tasks, options = {}) {
    const saga = {
      id: randomUUID(),
      name: options.name,
      tasks: tasks.map(t => ({ ...t, status: 'pending' })),
      phase: 'forward',
      startedAt: new Date(),
    };

    const validated = SagaExecutionSchema.parse(saga);
    this.executions.set(validated.id, validated);

    try {
      // Forward phase: execute tasks sequentially
      await this._executeForwardPhase(validated);

      validated.phase = 'completed';
      validated.completedAt = new Date();
      validated.success = true;

      if (this.onSagaCompleted) {
        this.onSagaCompleted({ saga: validated, success: true });
      }

      return validated;

    } catch (error) {
      // Backward phase: compensate completed tasks
      validated.phase = 'backward';
      validated.error = error;

      await this._executeBackwardPhase(validated);

      validated.phase = 'failed';
      validated.completedAt = new Date();
      validated.success = false;

      if (this.onSagaCompleted) {
        this.onSagaCompleted({ saga: validated, success: false, error });
      }

      throw error;
    }
  }

  /**
   * Execute forward phase (task execution)
   * @param {SagaExecution} saga - Saga execution
   * @private
   */
  async _executeForwardPhase(saga) {
    for (const task of saga.tasks) {
      task.status = 'executing';
      task.startedAt = new Date();

      try {
        task.output = await task.execute(task.input);
        task.status = 'completed';
        task.completedAt = new Date();

        if (this.onTaskCompleted) {
          this.onTaskCompleted({ task, saga });
        }

      } catch (error) {
        task.status = 'failed';
        task.error = error;
        throw error;
      }
    }
  }

  /**
   * Execute backward phase (compensation)
   * @param {SagaExecution} saga - Saga execution
   * @private
   */
  async _executeBackwardPhase(saga) {
    // Get completed tasks in reverse order
    const completedTasks = saga.tasks
      .filter(t => t.status === 'completed')
      .reverse();

    for (const task of completedTasks) {
      await this._compensateTask(task, saga);
    }
  }

  /**
   * Compensate a single task
   * @param {SagaTask} task - Task to compensate
   * @param {SagaExecution} saga - Parent saga
   * @private
   */
  async _compensateTask(task, saga) {
    task.status = 'compensating';

    if (this.onCompensation) {
      this.onCompensation({ task, saga });
    }

    try {
      // Use inline compensate function if provided
      if (task.compensate) {
        await task.compensate(task.output);
      }
      // Otherwise lookup from registry
      else if (this.compensationRegistry) {
        const spec = this.compensationRegistry.getCompensation(task.taskId);
        if (spec) {
          // Execute compensation workflow
          const compensationInput = this._mapCompensationInput(task.output, spec.inputMapping);
          await this._executeCompensationWorkflow(spec, compensationInput);
        }
      }

      task.status = 'compensated';

    } catch (error) {
      // Compensation failed - log but continue compensating other tasks
      task.status = 'failed';
      task.error = error;
      console.error(`Compensation failed for task ${task.taskId}:`, error);
    }
  }

  /**
   * Map task output to compensation input
   * @param {*} output - Task output
   * @param {Object} mapping - Input mapping
   * @returns {*} Mapped input
   * @private
   */
  _mapCompensationInput(output, mapping = {}) {
    if (!mapping || Object.keys(mapping).length === 0) {
      return output;
    }

    const mapped = {};
    for (const [targetKey, sourceKey] of Object.entries(mapping)) {
      if (output && typeof output === 'object' && sourceKey in output) {
        mapped[targetKey] = output[sourceKey];
      }
    }
    return mapped;
  }

  /**
   * Execute compensation workflow
   * @param {Object} spec - Compensation spec
   * @param {*} input - Compensation input
   * @private
   */
  async _executeCompensationWorkflow(spec, input) {
    // Execute compensation tasks sequentially
    for (const task of spec.workflowSpec.tasks) {
      if (task.execute) {
        await task.execute({ input });
      }
    }
  }

  /**
   * Compensate specific completed tasks
   * @param {string[]} completedTaskIds - Task IDs to compensate
   * @returns {Promise<Object>} Compensation result
   *
   * @example
   * // Compensate specific tasks after partial failure
   * await coordinator.compensate(['book-hotel', 'book-flight']);
   */
  async compensate(completedTaskIds) {
    const result = {
      compensated: [],
      failed: [],
    };

    // Compensate in reverse order of completion
    const tasksToCompensate = completedTaskIds.reverse();

    for (const taskId of tasksToCompensate) {
      try {
        const spec = this.compensationRegistry?.getCompensation(taskId);
        if (spec) {
          await this._executeCompensationWorkflow(spec, {});
          result.compensated.push(taskId);
        }
      } catch (error) {
        result.failed.push({ taskId, error });
      }
    }

    return result;
  }

  /**
   * Get saga execution by ID
   * @param {string} sagaId - Saga ID
   * @returns {SagaExecution|null} Saga execution or null
   */
  getSaga(sagaId) {
    return this.executions.get(sagaId) || null;
  }

  /**
   * Get all saga executions
   * @returns {SagaExecution[]} All executions
   */
  getAllSagas() {
    return Array.from(this.executions.values());
  }

  /**
   * Get saga statistics
   * @returns {Object} Statistics
   */
  getStats() {
    const sagas = this.getAllSagas();
    return {
      total: sagas.length,
      completed: sagas.filter(s => s.phase === 'completed').length,
      failed: sagas.filter(s => s.phase === 'failed').length,
      inProgress: sagas.filter(s => s.phase === 'forward' || s.phase === 'backward').length,
    };
  }

  /**
   * Clear all saga executions
   */
  clear() {
    this.executions.clear();
  }
}

/**
 * Create a new saga coordinator
 * @param {Object} config - Configuration
 * @returns {SagaCoordinator} New coordinator instance
 */
export function createSagaCoordinator(config = {}) {
  return new SagaCoordinator(config);
}
