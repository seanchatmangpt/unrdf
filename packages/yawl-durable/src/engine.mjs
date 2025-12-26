/**
 * @file Durable Workflow Engine - Temporal.io-inspired execution using YAWL
 * @module @unrdf/yawl-durable/engine
 *
 * Core innovation: Use YAWL's cryptographic receipt chain as event history
 * for deterministic replay, enabling durable execution semantics.
 */

import { createWorkflow, YawlCase } from '@unrdf/yawl';
import { now, toISO } from '@unrdf/kgc-4d';
import { z } from 'zod';
import { DurableActivity } from './activity.mjs';
import { replayFromReceipts } from './replay.mjs';

// =============================================================================
// Schemas
// =============================================================================

const ActivityConfigSchema = z.object({
  id: z.string(),
  name: z.string(),
  handler: z.function(),
  timeout: z.number().optional().default(30000), // 30s default
  retryPolicy: z.object({
    maxAttempts: z.number().optional().default(3),
    initialInterval: z.number().optional().default(1000),
    backoffCoefficient: z.number().optional().default(2),
    maximumInterval: z.number().optional().default(60000),
  }).optional(),
  compensate: z.function().optional(), // Compensation handler for sagas
});

const WorkflowConfigSchema = z.object({
  id: z.string(),
  name: z.string(),
  activities: z.array(ActivityConfigSchema),
  flow: z.array(z.object({
    from: z.string(),
    to: z.string(),
    condition: z.function().optional(),
  })),
  version: z.string().optional().default('1.0.0'),
});

// =============================================================================
// DurableWorkflowEngine
// =============================================================================

/**
 * Durable Workflow Engine implementing Temporal.io patterns using YAWL
 *
 * Key features:
 * - Deterministic replay from receipt chain
 * - Saga pattern with compensation
 * - Activity timeouts and retries
 * - Workflow versioning via KGC-4D
 *
 * @example
 * const engine = new DurableWorkflowEngine();
 * const workflowId = await engine.defineWorkflow({
 *   id: 'booking-saga',
 *   name: 'Travel Booking Saga',
 *   activities: [
 *     { id: 'bookFlight', handler: async (input) => {...}, compensate: async () => {...} },
 *     { id: 'bookHotel', handler: async (input) => {...}, compensate: async () => {...} },
 *   ],
 *   flow: [
 *     { from: 'bookFlight', to: 'bookHotel' },
 *   ],
 * });
 *
 * const execution = await engine.startWorkflow('booking-saga', { userId: '123' });
 */
export class DurableWorkflowEngine {
  constructor() {
    /** @type {Map<string, Object>} Workflow definitions */
    this.workflows = new Map();

    /** @type {Map<string, YawlCase>} Active workflow executions */
    this.executions = new Map();

    /** @type {Map<string, Array>} Receipt history for deterministic replay */
    this.receiptStore = new Map();

    /** @type {Map<string, DurableActivity>} Activity registry */
    this.activities = new Map();
  }

  /**
   * Define a durable workflow
   *
   * @param {Object} config - Workflow configuration
   * @returns {Promise<string>} Workflow ID
   */
  async defineWorkflow(config) {
    const validated = WorkflowConfigSchema.parse(config);

    // Register activities
    for (const activityConfig of validated.activities) {
      const activity = new DurableActivity(activityConfig);
      this.activities.set(activityConfig.id, activity);
    }

    // Create YAWL workflow
    const yawlWorkflow = createWorkflow({
      id: validated.id,
      name: validated.name,
      version: validated.version,
      tasks: validated.activities.map(a => ({
        id: a.id,
        name: a.name,
        timeout: a.timeout,
        // Each activity gets its own cancellation region for saga compensation
        cancellationRegion: `${validated.id}:${a.id}`,
      })),
      controlFlow: validated.flow.map(f => ({
        from: f.from,
        to: f.to,
        condition: f.condition ? f.condition : undefined,
      })),
    });

    this.workflows.set(validated.id, {
      config: validated,
      yawlWorkflow,
    });

    return validated.id;
  }

  /**
   * Start a workflow execution
   *
   * Creates a new YAWL case and begins execution. All state transitions
   * are captured in cryptographic receipts for deterministic replay.
   *
   * @param {string} workflowId - Workflow definition ID
   * @param {Object} input - Initial workflow input
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution handle
   */
  async startWorkflow(workflowId, input = {}, options = {}) {
    const workflowDef = this.workflows.get(workflowId);
    if (!workflowDef) {
      throw new Error(`Workflow ${workflowId} not defined`);
    }

    // Create YAWL case (workflow instance)
    const caseObj = new YawlCase({
      id: options.executionId || `${workflowId}-${Date.now()}`,
      workflow: workflowDef.yawlWorkflow,
      data: input,
    });

    const executionId = caseObj.id;
    this.executions.set(executionId, caseObj);
    this.receiptStore.set(executionId, []);

    // Start the case (enables first task)
    const { receipt } = await caseObj.start();
    this._storeReceipt(executionId, receipt);

    return {
      executionId,
      workflowId,
      status: 'RUNNING',
      startedAt: toISO(now()),
      input,
    };
  }

  /**
   * Execute an activity with retry and timeout logic
   *
   * This is the core of durable execution - activities are executed with:
   * - Automatic retries on failure
   * - Timeout enforcement
   * - Receipt generation for deterministic replay
   *
   * @param {string} executionId - Workflow execution ID
   * @param {string} activityId - Activity ID
   * @param {Object} input - Activity input
   * @returns {Promise<Object>} Activity result
   */
  async executeActivity(executionId, activityId, input) {
    const caseObj = this.executions.get(executionId);
    if (!caseObj) {
      throw new Error(`Execution ${executionId} not found`);
    }

    const activity = this.activities.get(activityId);
    if (!activity) {
      throw new Error(`Activity ${activityId} not registered`);
    }

    // Find work item for this activity
    const workItems = Array.from(caseObj.workItems.values());
    const workItem = workItems.find(wi => wi.name === activity.config.name);

    if (!workItem) {
      throw new Error(`No work item found for activity ${activityId}`);
    }

    // Start the task
    const { receipt: startReceipt } = await caseObj.startTask(workItem.id);
    this._storeReceipt(executionId, startReceipt);

    // Execute activity with retry logic
    const result = await activity.execute(input, {
      executionId,
      activityId,
      workItemId: workItem.id,
    });

    // Complete the task
    const { receipt: completeReceipt, downstreamEnabled } = await caseObj.completeTask(
      workItem.id,
      result,
    );
    this._storeReceipt(executionId, completeReceipt);

    // Auto-execute downstream activities
    for (const downstream of downstreamEnabled) {
      const downstreamActivity = this.activities.get(downstream.taskId);
      if (downstreamActivity) {
        // Execute asynchronously (fire and forget for this example)
        setImmediate(() => {
          this.executeActivity(executionId, downstream.taskId, result).catch(err => {
            console.error(`Downstream activity ${downstream.taskId} failed:`, err);
          });
        });
      }
    }

    return result;
  }

  /**
   * Replay workflow from receipt history
   *
   * This enables deterministic replay - reconstruct exact workflow state
   * from the cryptographic receipt chain without re-executing activities.
   *
   * @param {string} executionId - Workflow execution ID
   * @returns {Promise<Object>} Replayed workflow state
   */
  async replay(executionId) {
    const receipts = this.receiptStore.get(executionId);
    if (!receipts || receipts.length === 0) {
      throw new Error(`No receipts found for execution ${executionId}`);
    }

    const state = await replayFromReceipts(receipts);

    return {
      executionId,
      state,
      receiptCount: receipts.length,
      lastReceiptHash: receipts[receipts.length - 1].receiptHash,
    };
  }

  /**
   * Get workflow execution status
   *
   * @param {string} executionId - Workflow execution ID
   * @returns {Object} Execution status
   */
  getExecutionStatus(executionId) {
    const caseObj = this.executions.get(executionId);
    if (!caseObj) {
      throw new Error(`Execution ${executionId} not found`);
    }

    const receipts = this.receiptStore.get(executionId) || [];

    return {
      executionId,
      status: caseObj.status,
      receiptCount: receipts.length,
      completedTasks: Array.from(caseObj.completedTasks),
      activeTasks: Array.from(caseObj.workItems.keys()).filter(id => {
        const wi = caseObj.workItems.get(id);
        return wi.status === 'ENABLED' || wi.status === 'ACTIVE';
      }),
      data: caseObj.data,
    };
  }

  /**
   * Get receipt history for an execution
   *
   * @param {string} executionId - Workflow execution ID
   * @returns {Array} Receipt chain
   */
  getReceiptHistory(executionId) {
    return this.receiptStore.get(executionId) || [];
  }

  /**
   * Verify receipt chain integrity
   *
   * @param {string} executionId - Workflow execution ID
   * @returns {Promise<Object>} Verification result
   */
  async verifyReceiptChain(executionId) {
    const receipts = this.receiptStore.get(executionId);
    if (!receipts || receipts.length === 0) {
      return { valid: false, error: 'No receipts found' };
    }

    // Verify each receipt chains to the next
    for (let i = 1; i < receipts.length; i++) {
      const prev = receipts[i - 1];
      const curr = receipts[i];

      if (curr.previousReceiptHash !== prev.receiptHash) {
        return {
          valid: false,
          error: `Chain broken at receipt ${i}: expected ${prev.receiptHash}, got ${curr.previousReceiptHash}`,
        };
      }
    }

    return {
      valid: true,
      receiptCount: receipts.length,
      genesisHash: receipts[0].receiptHash,
      latestHash: receipts[receipts.length - 1].receiptHash,
    };
  }

  /**
   * Store receipt in history
   * @private
   */
  _storeReceipt(executionId, receipt) {
    const receipts = this.receiptStore.get(executionId);
    if (receipts) {
      receipts.push(receipt);
    }
  }
}
