/**
 * @fileoverview Saga Pattern - Distributed Transaction Orchestration
 * Multi-step workflows with compensation for failure recovery
 */

import { z } from 'zod';
import { generateReceipt } from './receipt.mjs';

/**
 * Saga step schema
 */
export const SagaStepSchema = z.object({
  id: z.string(),
  name: z.string(),
  execute: z.function().args(z.any()).returns(z.promise(z.any())),
  compensate: z.function().args(z.any()).returns(z.promise(z.any())),
  retryable: z.boolean().default(true),
  maxRetries: z.number().nonnegative().default(3),
});

/**
 * Saga configuration schema
 */
export const SagaConfigSchema = z.object({
  id: z.string(),
  name: z.string(),
  steps: z.array(SagaStepSchema),
  parallel: z.boolean().default(false),
  continueOnError: z.boolean().default(false),
});

/**
 * @typedef {z.infer<typeof SagaStepSchema>} SagaStep
 * @typedef {z.infer<typeof SagaConfigSchema>} SagaConfig
 */

/**
 * Saga execution state
 * @typedef {{
 *   sagaId: string,
 *   status: 'pending' | 'running' | 'completed' | 'compensating' | 'compensated' | 'failed',
 *   currentStep: number,
 *   completedSteps: Array<{stepId: string, result: any}>,
 *   compensatedSteps: Array<{stepId: string, result: any}>,
 *   error?: Error,
 *   startTime: number,
 *   endTime?: number,
 * }} SagaState
 */

/**
 * Saga orchestrator for distributed transactions
 * Implements saga pattern with compensation
 */
export class SagaOrchestrator {
  /**
   * @param {SagaConfig} config - Saga configuration
   */
  constructor(config) {
    this.config = SagaConfigSchema.parse(config);
    /** @type {Map<string, SagaState>} */
    this.executions = new Map();
  }

  /**
   * Execute saga workflow
   * @param {Record<string, any>} initialContext - Initial context for execution
   * @returns {Promise<{success: boolean, result?: any, error?: Error, state: SagaState}>} Execution result
   */
  async execute(initialContext = {}) {
    const executionId = `${this.config.id}-${Date.now()}`;

    /** @type {SagaState} */
    const state = {
      sagaId: executionId,
      status: 'running',
      currentStep: 0,
      completedSteps: [],
      compensatedSteps: [],
      startTime: Date.now(),
    };

    this.executions.set(executionId, state);

    try {
      let context = { ...initialContext };

      if (this.config.parallel) {
        // Execute all steps in parallel
        context = await this._executeParallel(state, context);
      } else {
        // Execute steps sequentially
        context = await this._executeSequential(state, context);
      }

      state.status = 'completed';
      state.endTime = Date.now();

      return {
        success: true,
        result: context,
        state,
      };
    } catch (error) {
      state.error = error;
      state.status = 'compensating';

      try {
        await this._compensate(state);
        state.status = 'compensated';
      } catch (compensationError) {
        state.status = 'failed';
        state.error = compensationError;
      }

      state.endTime = Date.now();

      return {
        success: false,
        error: state.error,
        state,
      };
    }
  }

  /**
   * Execute steps sequentially
   * @param {SagaState} state - Saga state
   * @param {Record<string, any>} context - Execution context
   * @returns {Promise<Record<string, any>>} Updated context
   * @private
   */
  async _executeSequential(state, context) {
    for (let i = 0; i < this.config.steps.length; i++) {
      const step = this.config.steps[i];
      state.currentStep = i;

      const result = await this._executeStepWithRetry(step, context);

      state.completedSteps.push({
        stepId: step.id,
        result,
      });

      // Merge result into context
      context = { ...context, [step.name]: result };
    }

    return context;
  }

  /**
   * Execute steps in parallel
   * @param {SagaState} state - Saga state
   * @param {Record<string, any>} context - Execution context
   * @returns {Promise<Record<string, any>>} Updated context
   * @private
   */
  async _executeParallel(state, context) {
    const results = await Promise.all(
      this.config.steps.map(async (step, i) => {
        const result = await this._executeStepWithRetry(step, context);
        state.completedSteps.push({
          stepId: step.id,
          result,
        });
        return { name: step.name, result };
      })
    );

    // Merge all results into context
    for (const { name, result } of results) {
      context[name] = result;
    }

    return context;
  }

  /**
   * Execute step with retry logic
   * @param {SagaStep} step - Step to execute
   * @param {Record<string, any>} context - Execution context
   * @returns {Promise<any>} Step result
   * @private
   */
  async _executeStepWithRetry(step, context) {
    let lastError;
    const maxAttempts = step.retryable ? step.maxRetries + 1 : 1;

    for (let attempt = 0; attempt < maxAttempts; attempt++) {
      try {
        return await step.execute(context);
      } catch (error) {
        lastError = error;

        if (!step.retryable || attempt === maxAttempts - 1) {
          throw error;
        }

        // Exponential backoff
        const delay = Math.min(1000 * Math.pow(2, attempt), 10000);
        await new Promise(resolve => setTimeout(resolve, delay));
      }
    }

    throw lastError;
  }

  /**
   * Compensate completed steps in reverse order
   * @param {SagaState} state - Saga state
   * @returns {Promise<void>}
   * @private
   */
  async _compensate(state) {
    // Compensate in reverse order
    const stepsToCompensate = [...state.completedSteps].reverse();

    for (const { stepId } of stepsToCompensate) {
      const step = this.config.steps.find(s => s.id === stepId);
      if (!step) continue;

      try {
        const result = await step.compensate(state);
        state.compensatedSteps.push({
          stepId,
          result,
        });
      } catch (error) {
        // Compensation failure is critical
        throw new Error(
          `Failed to compensate step '${stepId}': ${error.message}`,
          { cause: error }
        );
      }
    }
  }

  /**
   * Get saga execution state
   * @param {string} executionId - Execution ID
   * @returns {SagaState | undefined} Saga state
   */
  getState(executionId) {
    return this.executions.get(executionId);
  }

  /**
   * Get all execution states
   * @returns {Array<SagaState>} All states
   */
  getAllStates() {
    return Array.from(this.executions.values());
  }

  /**
   * Generate receipt for saga execution
   * @param {string} executionId - Execution ID
   * @returns {Promise<import('./receipt.mjs').Receipt>} Receipt
   */
  async generateReceipt(executionId) {
    const state = this.executions.get(executionId);
    if (!state) {
      throw new Error(`Saga execution '${executionId}' not found`);
    }

    return generateReceipt(
      `saga:${this.config.name}`,
      { sagaId: this.config.id, executionId },
      {
        status: state.status,
        completedSteps: state.completedSteps.length,
        compensatedSteps: state.compensatedSteps.length,
        duration: (state.endTime || Date.now()) - state.startTime,
        success: state.status === 'completed',
      }
    );
  }
}

/**
 * Create a simple saga step
 * @param {string} id - Step ID
 * @param {string} name - Step name
 * @param {(ctx: any) => Promise<any>} executeFn - Execute function
 * @param {(ctx: any) => Promise<any>} compensateFn - Compensate function
 * @param {object} [options] - Step options
 * @returns {SagaStep} Saga step
 */
export function createSagaStep(id, name, executeFn, compensateFn, options = {}) {
  return SagaStepSchema.parse({
    id,
    name,
    execute: executeFn,
    compensate: compensateFn,
    ...options,
  });
}

/**
 * Create saga builder for fluent API
 * @param {string} id - Saga ID
 * @param {string} name - Saga name
 * @returns {object} Saga builder
 */
export function createSagaBuilder(id, name) {
  /** @type {SagaStep[]} */
  const steps = [];
  let parallel = false;
  let continueOnError = false;

  return {
    /**
     * Add step to saga
     * @param {SagaStep} step - Step to add
     * @returns {object} Builder (for chaining)
     */
    step(step) {
      steps.push(step);
      return this;
    },

    /**
     * Enable parallel execution
     * @returns {object} Builder (for chaining)
     */
    inParallel() {
      parallel = true;
      return this;
    },

    /**
     * Continue on error
     * @returns {object} Builder (for chaining)
     */
    continueOnError() {
      continueOnError = true;
      return this;
    },

    /**
     * Build saga orchestrator
     * @returns {SagaOrchestrator} Saga orchestrator
     */
    build() {
      return new SagaOrchestrator({
        id,
        name,
        steps,
        parallel,
        continueOnError,
      });
    },
  };
}
