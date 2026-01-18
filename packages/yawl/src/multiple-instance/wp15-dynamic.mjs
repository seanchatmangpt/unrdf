/**
 * @file WP15 - Multiple Instances without A Priori Runtime Knowledge
 * @module @unrdf/yawl/multiple-instance/wp15-dynamic
 *
 * @description
 * Implements YAWL Workflow Pattern 15: Multiple Instances without A Priori Runtime Knowledge.
 *
 * "The ability to create multiple instances of a task in a single case dynamically
 * during execution. The number of instances is not known when the first instance starts,
 * and new instances can be created while other instances are running. Synchronization
 * occurs when a 'no more instances' signal is received."
 *
 * This is THE HARDEST multiple instance pattern due to:
 * - Unknown instance count at start
 * - Dynamic addition during execution
 * - Race conditions between addition and completion
 * - Complex synchronization barriers
 *
 * Reference: van der Aalst, W.M.P., et al. (2003). Workflow Patterns.
 */

import { randomUUID } from 'crypto';
import { z } from 'zod';
import { now, toISO } from '@unrdf/kgc-4d';
import { DynamicBarrier } from './dynamic-barrier.mjs';
import { ProofChain } from '../receipt-proofchain.mjs';
import { generateReceipt } from '../receipt-core.mjs';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Task definition schema for MI instances
 */
export const MITaskSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  handler: z.function(),
});

/**
 * MI Instance schema
 */
export const MIInstanceSchema = z.object({
  id: z.string().uuid(),
  taskId: z.string().min(1),
  inputData: z.any(),
  status: z.enum(['pending', 'running', 'completed', 'failed', 'cancelled']),
  result: z.any().optional(),
  error: z.any().optional(),
  createdAt: z.coerce.date(),
  startedAt: z.coerce.date().optional(),
  completedAt: z.coerce.date().optional(),
});

/**
 * @typedef {z.infer<typeof MIInstanceSchema>} MIInstance
 */

// ============================================================================
// DYNAMIC MI CONTROLLER
// ============================================================================

/**
 * Dynamic Multiple Instance Controller for WP15
 *
 * Manages dynamically created task instances with receipts and synchronization.
 *
 * @example
 * // Create controller
 * const task = {
 *   id: 'process-document',
 *   name: 'Process Document',
 *   handler: async (data) => {
 *     // Process document...
 *     return { processed: true, doc: data.documentId };
 *   }
 * };
 *
 * const controller = new DynamicMIController(task, { caseId: 'case-123' });
 *
 * // Start with initial instances
 * await controller.addInstance({ documentId: 1 });
 * await controller.addInstance({ documentId: 2 });
 *
 * // Later, dynamically add more during execution
 * await controller.addInstance({ documentId: 3 });
 * await controller.addInstance({ documentId: 4 });
 *
 * // Signal no more instances
 * controller.signalComplete();
 *
 * // Wait for all to complete
 * const results = await controller.wait();
 * console.log(`Processed ${results.length} documents`);
 */
export class DynamicMIController {
  /**
   * Create a new dynamic MI controller
   *
   * @param {Object} task - Task definition
   * @param {string} task.id - Task ID
   * @param {string} task.name - Task name
   * @param {Function} task.handler - Async task handler function
   * @param {Object} [options] - Controller options
   * @param {string} [options.caseId] - Case ID for receipts
   * @param {boolean} [options.receipts=true] - Enable receipt generation
   * @param {string} [options.nodeId='controller'] - Node ID for receipts
   */
  constructor(task, options = {}) {
    // Validate task
    const validated = MITaskSchema.parse(task);

    /** @type {string} */
    this.taskId = validated.id;

    /** @type {string} */
    this.taskName = validated.name;

    /** @type {Function} */
    this.handler = validated.handler;

    /** @type {string} */
    this.caseId = options.caseId ?? randomUUID();

    /** @type {boolean} */
    this.receiptsEnabled = options.receipts !== false;

    /** @type {string} */
    this.nodeId = options.nodeId ?? 'controller';

    /** @type {Map<string, MIInstance>} */
    this.instances = new Map();

    /** @type {DynamicBarrier} */
    this.barrier = new DynamicBarrier();

    /** @type {ProofChain|null} */
    this.receiptChain = this.receiptsEnabled ? new ProofChain(this.nodeId) : null;

    /** @type {boolean} */
    this.signaled = false;

    /** @type {boolean} */
    this.cancelled = false;

    /** @type {Date} */
    this.createdAt = new Date();

    /** @type {Date|null} */
    this.signaledAt = null;

    /** @type {Date|null} */
    this.completedAt = null;
  }

  /**
   * Add a new instance dynamically
   *
   * @param {any} inputData - Input data for the instance
   * @param {Object} [options] - Instance options
   * @param {string} [options.instanceId] - Override instance ID
   * @returns {Promise<string>} Instance ID
   * @throws {Error} If controller already signaled as complete
   * @throws {Error} If controller is cancelled
   */
  async addInstance(inputData, options = {}) {
    if (this.signaled) {
      throw new Error('Cannot add instance: controller has been signaled as complete');
    }

    if (this.cancelled) {
      throw new Error('Cannot add instance: controller is cancelled');
    }

    const instanceId = options.instanceId ?? randomUUID();

    // Create instance
    const instance = {
      id: instanceId,
      taskId: this.taskId,
      inputData,
      status: 'pending',
      createdAt: new Date(),
    };

    this.instances.set(instanceId, instance);

    // Add to barrier
    this.barrier.addExpected(instanceId);

    // Generate receipt for instance creation
    if (this.receiptsEnabled) {
      const receipt = await generateReceipt(
        {
          eventType: 'WORK_ITEM_CREATED',
          caseId: this.caseId,
          taskId: this.taskId,
          payload: {
            decision: 'INSTANCE_CREATED',
            justification: {
              reasoning: `Dynamic MI instance ${instanceId} created for ${this.taskName}`,
            },
            context: {
              instanceId,
              totalInstances: this.instances.size,
              inputData,
            },
          },
        },
        this.receiptChain.getLatest()
      );

      await this.receiptChain.append(receipt);
    }

    // Start execution immediately
    this._executeInstance(instanceId).catch((error) => {
      console.error(`Instance ${instanceId} execution error:`, error);
    });

    return instanceId;
  }

  /**
   * Execute a single instance
   *
   * @param {string} instanceId - Instance ID
   * @returns {Promise<void>}
   * @private
   */
  async _executeInstance(instanceId) {
    const instance = this.instances.get(instanceId);
    if (!instance) {
      throw new Error(`Instance ${instanceId} not found`);
    }

    try {
      // Mark as running
      instance.status = 'running';
      instance.startedAt = new Date();
      this.barrier.markRunning(instanceId);

      // Execute handler
      const result = await this.handler(instance.inputData);

      // Mark as completed
      instance.status = 'completed';
      instance.result = result;
      instance.completedAt = new Date();
      await this.barrier.markComplete(instanceId, result);

      // Generate completion receipt
      if (this.receiptsEnabled) {
        const receipt = await generateReceipt(
          {
            eventType: 'TASK_COMPLETED',
            caseId: this.caseId,
            taskId: this.taskId,
            payload: {
              decision: 'INSTANCE_COMPLETED',
              justification: {
                reasoning: `Instance ${instanceId} completed successfully`,
              },
              context: {
                instanceId,
                result,
                duration: instance.completedAt - instance.startedAt,
              },
            },
          },
          this.receiptChain.getLatest()
        );

        await this.receiptChain.append(receipt);
      }
    } catch (error) {
      // Mark as failed
      instance.status = 'failed';
      instance.error = error;
      instance.completedAt = new Date();
      await this.barrier.markFailed(instanceId, error);

      // Generate failure receipt
      if (this.receiptsEnabled) {
        const receipt = await generateReceipt(
          {
            eventType: 'TASK_FAILED',
            caseId: this.caseId,
            taskId: this.taskId,
            payload: {
              decision: 'INSTANCE_FAILED',
              justification: {
                reasoning: `Instance ${instanceId} failed: ${error.message}`,
              },
              context: {
                instanceId,
                error: error.message,
                stack: error.stack,
              },
            },
          },
          this.receiptChain.getLatest()
        );

        await this.receiptChain.append(receipt);
      }
    }
  }

  /**
   * Signal that no more instances will be added
   * Does NOT wait for instances to complete - use wait() for that
   *
   * @returns {Promise<void>}
   */
  async signalComplete() {
    if (this.signaled) {
      return; // Idempotent
    }

    this.signaled = true;
    this.signaledAt = new Date();

    // Signal barrier
    this.barrier.signalNoMoreInstances();

    // Generate signal receipt
    if (this.receiptsEnabled) {
      const receipt = await generateReceipt(
        {
          eventType: 'CONTROL_FLOW_EVALUATED',
          caseId: this.caseId,
          taskId: this.taskId,
          payload: {
            decision: 'NO_MORE_INSTANCES',
            justification: {
              reasoning: `All instances created, total: ${this.instances.size}`,
            },
            context: {
              totalInstances: this.instances.size,
              status: this.barrier.getStatus(),
            },
          },
        },
        this.receiptChain.getLatest()
      );

      await this.receiptChain.append(receipt);
    }
  }

  /**
   * Wait for all instances to complete
   * MUST call signalComplete() first
   *
   * @returns {Promise<Array<any>>} Array of instance results (including failures)
   * @throws {Error} If not signaled yet
   */
  async wait() {
    if (!this.signaled) {
      throw new Error('Must call signalComplete() before wait()');
    }

    await this.barrier.wait();

    this.completedAt = new Date();

    // Collect results
    const results = [];
    for (const instance of this.instances.values()) {
      if (instance.status === 'completed') {
        results.push(instance.result);
      } else if (instance.status === 'failed') {
        results.push({ error: instance.error, instanceId: instance.id });
      }
    }

    return results;
  }

  /**
   * Cancel all instances
   *
   * @returns {Promise<void>}
   */
  async cancel() {
    if (this.cancelled) {
      return; // Idempotent
    }

    this.cancelled = true;

    // Cancel barrier
    await this.barrier.cancel();

    // Mark all pending/running instances as cancelled
    for (const instance of this.instances.values()) {
      if (instance.status === 'pending' || instance.status === 'running') {
        instance.status = 'cancelled';
        instance.completedAt = new Date();
      }
    }

    // Generate cancellation receipt
    if (this.receiptsEnabled) {
      const receipt = await generateReceipt(
        {
          eventType: 'TASK_CANCELLED',
          caseId: this.caseId,
          taskId: this.taskId,
          payload: {
            decision: 'ALL_INSTANCES_CANCELLED',
            justification: {
              reasoning: 'Controller cancelled, all instances cancelled',
            },
            context: {
              totalInstances: this.instances.size,
              cancelledCount: Array.from(this.instances.values()).filter(
                i => i.status === 'cancelled'
              ).length,
            },
          },
        },
        this.receiptChain.getLatest()
      );

      await this.receiptChain.append(receipt);
    }
  }

  /**
   * Get current status
   * @returns {Object}
   */
  getStatus() {
    const barrierStatus = this.barrier.getStatus();

    return {
      caseId: this.caseId,
      taskId: this.taskId,
      taskName: this.taskName,
      totalInstances: this.instances.size,
      signaled: this.signaled,
      cancelled: this.cancelled,
      completed: this.completedAt !== null,
      createdAt: this.createdAt,
      signaledAt: this.signaledAt,
      completedAt: this.completedAt,
      barrier: barrierStatus,
      receipts: this.receiptChain?.length ?? 0,
    };
  }

  /**
   * Get all instances
   * @returns {MIInstance[]}
   */
  getInstances() {
    return Array.from(this.instances.values());
  }

  /**
   * Get instance by ID
   * @param {string} instanceId
   * @returns {MIInstance|undefined}
   */
  getInstance(instanceId) {
    return this.instances.get(instanceId);
  }

  /**
   * Get completed instances
   * @returns {MIInstance[]}
   */
  getCompletedInstances() {
    return Array.from(this.instances.values()).filter(i => i.status === 'completed');
  }

  /**
   * Get failed instances
   * @returns {MIInstance[]}
   */
  getFailedInstances() {
    return Array.from(this.instances.values()).filter(i => i.status === 'failed');
  }

  /**
   * Check if any instance failed
   * @returns {boolean}
   */
  hasFailed() {
    return this.getFailedInstances().length > 0;
  }

  /**
   * Get receipt chain
   * @returns {ProofChain|null}
   */
  getReceiptChain() {
    return this.receiptChain;
  }

  /**
   * Export audit trail
   * @returns {Promise<Object>}
   */
  async exportAuditTrail() {
    const receiptAudit = this.receiptChain
      ? await this.receiptChain.exportAuditTrail()
      : null;

    return {
      controller: this.getStatus(),
      instances: this.getInstances().map(i => ({
        ...i,
        createdAt: i.createdAt.toISOString(),
        startedAt: i.startedAt?.toISOString(),
        completedAt: i.completedAt?.toISOString(),
      })),
      receipts: receiptAudit,
      exportedAt: toISO(now()),
    };
  }
}

/**
 * Create a dynamic MI controller
 *
 * @param {Object} task - Task definition
 * @param {Object} [options] - Controller options
 * @returns {DynamicMIController}
 */
export function createDynamicMIController(task, options) {
  return new DynamicMIController(task, options);
}
