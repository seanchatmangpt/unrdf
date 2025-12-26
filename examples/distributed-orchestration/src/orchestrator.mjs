/**
 * @file Distributed Workflow Orchestrator
 * @description Production-grade orchestrator using YAWL + Federation + Streaming
 * @module distributed-orchestration/orchestrator
 */

import { createWorkflow, WorkflowEngine } from '@unrdf/yawl';
import { FederationCoordinator } from '@unrdf/federation';
import { createChangeFeed } from '@unrdf/streaming';
import { createStore } from '@unrdf/oxigraph';
import { trace, context } from '@opentelemetry/api';

const tracer = trace.getTracer('distributed-orchestration');

/**
 * Distributed Workflow Orchestrator
 *
 * Features:
 * - Multi-node workflow execution
 * - Automatic failover and recovery
 * - Real-time progress monitoring
 * - OpenTelemetry instrumentation
 *
 * @class
 */
export class DistributedOrchestrator {
  /**
   * @param {object} config - Configuration
   * @param {number} config.port - Server port
   * @param {string[]} config.peers - Initial peer list
   * @param {number} config.maxWorkers - Maximum worker nodes
   * @param {number} config.taskTimeout - Task timeout in ms
   */
  constructor(config = {}) {
    this.config = {
      port: 8080,
      peers: [],
      maxWorkers: 10,
      taskTimeout: 30000,
      ...config,
    };

    this.store = createStore();
    this.engine = new WorkflowEngine({ store: this.store });
    this.coordinator = null;
    this.changeFeed = null;
    this.workers = new Map();
    this.taskQueue = [];
    this.activeWorkflows = new Map();
  }

  /**
   * Initialize the orchestrator
   *
   * @returns {Promise<void>}
   */
  async initialize() {
    return tracer.startActiveSpan('orchestrator.initialize', async (span) => {
      try {
        // Initialize federation coordinator
        this.coordinator = new FederationCoordinator({
          port: this.config.port,
          peers: this.config.peers,
          store: this.store,
        });

        await this.coordinator.start();
        span.setAttribute('peers.count', this.config.peers.length);

        // Initialize change feed for real-time updates
        this.changeFeed = await createChangeFeed({
          store: this.store,
          batchSize: 100,
          throttleMs: 100,
        });

        // Subscribe to workflow state changes
        this.changeFeed.subscribe((changes) => {
          this._handleWorkflowChanges(changes);
        });

        // Start worker health monitoring
        this._startHealthMonitoring();

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Submit a workflow for distributed execution
   *
   * @param {object} workflowDef - Workflow definition
   * @param {object} input - Initial input data
   * @returns {Promise<string>} Workflow instance ID
   */
  async submitWorkflow(workflowDef, input = {}) {
    return tracer.startActiveSpan('orchestrator.submitWorkflow', async (span) => {
      try {
        // Create workflow instance
        const workflow = createWorkflow(workflowDef);
        const instance = await this.engine.createCase(workflow, input);

        span.setAttribute('workflow.id', instance.id);
        span.setAttribute('workflow.tasks', workflowDef.tasks?.length || 0);

        // Register with active workflows
        this.activeWorkflows.set(instance.id, {
          workflow,
          instance,
          startTime: Date.now(),
          status: 'running',
        });

        // Start execution
        await this._executeWorkflow(instance.id);

        span.setStatus({ code: 1 });
        return instance.id;
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Execute workflow by distributing tasks to workers
   *
   * @private
   * @param {string} workflowId - Workflow instance ID
   * @returns {Promise<void>}
   */
  async _executeWorkflow(workflowId) {
    const workflowData = this.activeWorkflows.get(workflowId);
    if (!workflowData) {
      throw new Error(`Workflow ${workflowId} not found`);
    }

    const { instance } = workflowData;

    // Get enabled tasks
    const enabledTasks = await this.engine.getEnabledTasks(instance.id);

    // Distribute tasks to workers
    for (const task of enabledTasks) {
      await this._assignTask(workflowId, task);
    }
  }

  /**
   * Assign task to available worker
   *
   * @private
   * @param {string} workflowId - Workflow instance ID
   * @param {object} task - Task to assign
   * @returns {Promise<void>}
   */
  async _assignTask(workflowId, task) {
    return tracer.startActiveSpan('orchestrator.assignTask', async (span) => {
      try {
        span.setAttribute('task.id', task.id);
        span.setAttribute('workflow.id', workflowId);

        // Find available worker
        const worker = await this._findAvailableWorker();

        if (!worker) {
          // Queue task if no workers available
          this.taskQueue.push({ workflowId, task, queuedAt: Date.now() });
          span.setAttribute('task.queued', true);
          return;
        }

        // Assign task to worker
        const assignment = {
          workflowId,
          taskId: task.id,
          assignedAt: Date.now(),
          timeout: this.config.taskTimeout,
        };

        worker.activeTasks.set(task.id, assignment);

        // Send task to worker (via federation)
        await this.coordinator.sendMessage(worker.nodeId, {
          type: 'task_assignment',
          workflowId,
          task,
          timeout: this.config.taskTimeout,
        });

        span.setAttribute('worker.id', worker.nodeId);
        span.setStatus({ code: 1 });
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Find available worker node
   *
   * @private
   * @returns {Promise<object|null>} Worker node or null
   */
  async _findAvailableWorker() {
    // Get healthy workers sorted by load
    const availableWorkers = Array.from(this.workers.values())
      .filter((w) => w.status === 'healthy' && w.activeTasks.size < w.capacity)
      .sort((a, b) => a.activeTasks.size - b.activeTasks.size);

    return availableWorkers[0] || null;
  }

  /**
   * Handle task completion from worker
   *
   * @param {string} workflowId - Workflow instance ID
   * @param {string} taskId - Task ID
   * @param {object} result - Task result
   * @returns {Promise<void>}
   */
  async handleTaskCompletion(workflowId, taskId, result) {
    return tracer.startActiveSpan('orchestrator.handleTaskCompletion', async (span) => {
      try {
        span.setAttribute('workflow.id', workflowId);
        span.setAttribute('task.id', taskId);

        // Complete task in workflow engine
        await this.engine.completeTask(workflowId, taskId, result);

        // Remove from worker's active tasks
        for (const worker of this.workers.values()) {
          if (worker.activeTasks.has(taskId)) {
            worker.activeTasks.delete(taskId);
            break;
          }
        }

        // Check if workflow is complete
        const workflowData = this.activeWorkflows.get(workflowId);
        if (workflowData) {
          const status = await this.engine.getStatus(workflowId);
          if (status.state === 'completed') {
            workflowData.status = 'completed';
            workflowData.completedAt = Date.now();
            span.setAttribute('workflow.completed', true);
          }
        }

        // Process queued tasks
        await this._processTaskQueue();

        span.setStatus({ code: 1 });
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Process queued tasks
   *
   * @private
   * @returns {Promise<void>}
   */
  async _processTaskQueue() {
    while (this.taskQueue.length > 0) {
      const worker = await this._findAvailableWorker();
      if (!worker) break;

      const { workflowId, task } = this.taskQueue.shift();
      await this._assignTask(workflowId, task);
    }
  }

  /**
   * Register worker node
   *
   * @param {string} nodeId - Node ID
   * @param {object} capabilities - Worker capabilities
   * @returns {Promise<void>}
   */
  async registerWorker(nodeId, capabilities = {}) {
    return tracer.startActiveSpan('orchestrator.registerWorker', async (span) => {
      try {
        span.setAttribute('worker.id', nodeId);
        span.setAttribute('worker.capacity', capabilities.capacity || 5);

        this.workers.set(nodeId, {
          nodeId,
          capacity: capabilities.capacity || 5,
          activeTasks: new Map(),
          status: 'healthy',
          lastHeartbeat: Date.now(),
          capabilities,
        });

        span.setStatus({ code: 1 });
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Handle worker heartbeat
   *
   * @param {string} nodeId - Node ID
   * @returns {void}
   */
  handleHeartbeat(nodeId) {
    const worker = this.workers.get(nodeId);
    if (worker) {
      worker.lastHeartbeat = Date.now();
      worker.status = 'healthy';
    }
  }

  /**
   * Start health monitoring for workers
   *
   * @private
   * @returns {void}
   */
  _startHealthMonitoring() {
    setInterval(() => {
      const now = Date.now();
      const timeout = 30000; // 30 seconds

      for (const [nodeId, worker] of this.workers.entries()) {
        if (now - worker.lastHeartbeat > timeout) {
          worker.status = 'unhealthy';

          // Reassign tasks from unhealthy worker
          this._reassignWorkerTasks(nodeId);
        }
      }
    }, 10000); // Check every 10 seconds
  }

  /**
   * Reassign tasks from failed worker
   *
   * @private
   * @param {string} nodeId - Failed worker node ID
   * @returns {Promise<void>}
   */
  async _reassignWorkerTasks(nodeId) {
    return tracer.startActiveSpan('orchestrator.reassignTasks', async (span) => {
      try {
        const worker = this.workers.get(nodeId);
        if (!worker) return;

        span.setAttribute('worker.id', nodeId);
        span.setAttribute('tasks.count', worker.activeTasks.size);

        // Get all active tasks from failed worker
        const tasksToReassign = Array.from(worker.activeTasks.values());

        // Clear worker's tasks
        worker.activeTasks.clear();

        // Requeue tasks
        for (const assignment of tasksToReassign) {
          const workflowData = this.activeWorkflows.get(assignment.workflowId);
          if (workflowData) {
            const task = await this.engine.getTask(
              assignment.workflowId,
              assignment.taskId
            );
            this.taskQueue.push({
              workflowId: assignment.workflowId,
              task,
              queuedAt: Date.now(),
            });
          }
        }

        // Process requeued tasks
        await this._processTaskQueue();

        span.setStatus({ code: 1 });
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Handle workflow state changes
   *
   * @private
   * @param {object[]} changes - Change events
   * @returns {void}
   */
  _handleWorkflowChanges(changes) {
    // Broadcast workflow state changes to monitoring dashboards
    for (const change of changes) {
      if (this.coordinator) {
        this.coordinator.broadcast({
          type: 'workflow_change',
          change,
          timestamp: Date.now(),
        });
      }
    }
  }

  /**
   * Get orchestrator statistics
   *
   * @returns {object} Statistics
   */
  getStats() {
    const activeWorkflowCount = Array.from(this.activeWorkflows.values()).filter(
      (w) => w.status === 'running'
    ).length;

    const completedWorkflowCount = Array.from(this.activeWorkflows.values()).filter(
      (w) => w.status === 'completed'
    ).length;

    const healthyWorkerCount = Array.from(this.workers.values()).filter(
      (w) => w.status === 'healthy'
    ).length;

    const totalTasks = Array.from(this.workers.values()).reduce(
      (sum, w) => sum + w.activeTasks.size,
      0
    );

    return {
      workflows: {
        active: activeWorkflowCount,
        completed: completedWorkflowCount,
        total: this.activeWorkflows.size,
      },
      workers: {
        healthy: healthyWorkerCount,
        total: this.workers.size,
      },
      tasks: {
        active: totalTasks,
        queued: this.taskQueue.length,
      },
    };
  }

  /**
   * Shutdown orchestrator gracefully
   *
   * @returns {Promise<void>}
   */
  async shutdown() {
    return tracer.startActiveSpan('orchestrator.shutdown', async (span) => {
      try {
        // Stop accepting new workflows
        this.activeWorkflows.clear();

        // Stop change feed
        if (this.changeFeed) {
          this.changeFeed.unsubscribe();
        }

        // Stop coordinator
        if (this.coordinator) {
          await this.coordinator.stop();
        }

        span.setStatus({ code: 1 });
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }
}

export default DistributedOrchestrator;
