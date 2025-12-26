/**
 * @file Saga Pattern - Long-running transactions with compensation
 * @module @unrdf/yawl-durable/saga
 *
 * Implements the Saga pattern using YAWL cancellation regions.
 * When a saga fails, all completed activities are compensated in reverse order.
 */

import { DurableWorkflowEngine } from './engine.mjs';

// =============================================================================
// Saga Execution
// =============================================================================

/**
 * Execute a saga workflow with automatic compensation on failure
 *
 * A saga is a long-running transaction composed of multiple activities.
 * Each activity can have a compensation handler. If any activity fails,
 * all previously completed activities are compensated in reverse order.
 *
 * This uses YAWL's cancellation regions - each activity is in its own region
 * so we can selectively cancel/compensate activities.
 *
 * @param {DurableWorkflowEngine} engine - Workflow engine
 * @param {string} workflowId - Saga workflow ID
 * @param {Object} input - Saga input
 * @param {Object} [options] - Execution options
 * @returns {Promise<Object>} Saga result or compensation result
 *
 * @example
 * const result = await executeSaga(engine, 'booking-saga', {
 *   flightId: 'FL123',
 *   hotelId: 'HTL456',
 *   carId: 'CAR789',
 * });
 */
export async function executeSaga(engine, workflowId, input, options = {}) {
  const execution = await engine.startWorkflow(workflowId, input, options);
  const executionId = execution.executionId;

  const workflowDef = engine.workflows.get(workflowId);
  if (!workflowDef) {
    throw new Error(`Workflow ${workflowId} not defined`);
  }

  const completedActivities = [];
  let sagaFailed = false;
  let failureError = null;

  try {
    // Execute activities in sequence
    for (const activityConfig of workflowDef.config.activities) {
      const activityId = activityConfig.id;

      try {
        const result = await engine.executeActivity(
          executionId,
          activityId,
          completedActivities.length > 0
            ? completedActivities[completedActivities.length - 1].output
            : input
        );

        completedActivities.push({
          activityId,
          output: result,
          compensate: activityConfig.compensate,
        });

        console.log(`✅ Saga activity ${activityId} completed`);
      } catch (error) {
        console.error(`❌ Saga activity ${activityId} failed:`, error.message);
        sagaFailed = true;
        failureError = error;
        break;
      }
    }

    if (!sagaFailed) {
      // Saga completed successfully
      return {
        success: true,
        executionId,
        completedActivities: completedActivities.map(a => a.activityId),
        output: completedActivities.length > 0
          ? completedActivities[completedActivities.length - 1].output
          : null,
      };
    }
  } catch (error) {
    sagaFailed = true;
    failureError = error;
  }

  // Saga failed - execute compensation in reverse order
  console.log('🔄 Saga failed, executing compensation...');

  const compensationResults = await compensateSaga(
    engine,
    executionId,
    completedActivities.reverse()
  );

  return {
    success: false,
    executionId,
    error: failureError?.message || 'Saga failed',
    compensated: compensationResults.compensated,
    compensationErrors: compensationResults.errors,
  };
}

/**
 * Compensate completed saga activities in reverse order
 *
 * @param {DurableWorkflowEngine} engine - Workflow engine
 * @param {string} executionId - Execution ID
 * @param {Array} completedActivities - Activities to compensate (reversed)
 * @returns {Promise<Object>} Compensation results
 */
export async function compensateSaga(engine, executionId, completedActivities) {
  const compensated = [];
  const errors = [];

  for (const activity of completedActivities) {
    if (!activity.compensate) {
      console.warn(`Activity ${activity.activityId} has no compensation handler`);
      continue;
    }

    try {
      await activity.compensate(activity.output, {
        executionId,
        activityId: activity.activityId,
      });

      compensated.push(activity.activityId);
      console.log(`✅ Compensated ${activity.activityId}`);
    } catch (error) {
      errors.push({
        activityId: activity.activityId,
        error: error.message,
      });
      console.error(`❌ Compensation failed for ${activity.activityId}:`, error);
    }
  }

  return { compensated, errors };
}

/**
 * Create a saga workflow definition
 *
 * Helper to create a workflow with saga semantics (sequential execution,
 * compensation on failure).
 *
 * @param {Object} config - Saga configuration
 * @returns {Object} Workflow configuration
 *
 * @example
 * const sagaConfig = createSagaWorkflow({
 *   id: 'booking-saga',
 *   name: 'Travel Booking Saga',
 *   steps: [
 *     {
 *       id: 'bookFlight',
 *       handler: async (input) => {...},
 *       compensate: async (output) => {...},
 *     },
 *     {
 *       id: 'bookHotel',
 *       handler: async (input) => {...},
 *       compensate: async (output) => {...},
 *     },
 *   ],
 * });
 */
export function createSagaWorkflow(config) {
  const activities = config.steps.map((step, index) => ({
    id: step.id,
    name: step.name || step.id,
    handler: step.handler,
    compensate: step.compensate,
    timeout: step.timeout || 30000,
    retryPolicy: step.retryPolicy || {
      maxAttempts: 3,
      initialInterval: 1000,
      backoffCoefficient: 2,
    },
  }));

  // Create sequential flow
  const flow = [];
  for (let i = 0; i < activities.length - 1; i++) {
    flow.push({
      from: activities[i].id,
      to: activities[i + 1].id,
    });
  }

  return {
    id: config.id,
    name: config.name,
    activities,
    flow,
    version: config.version || '1.0.0',
  };
}
