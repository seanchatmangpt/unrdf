/**
 * @file Distributed Worker Node
 * @description Worker node for distributed workflow execution
 * @module distributed-orchestration/worker-node
 */

import { FederationClient } from '@unrdf/federation';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('worker-node');

/**
 * Worker Node for Distributed Execution
 *
 * Features:
 * - Task execution with timeout handling
 * - Heartbeat monitoring
 * - Result reporting
 * - Error recovery
 *
 * @class
 */
export class WorkerNode {
  /**
   * @param {object} config - Configuration
   * @param {string} config.nodeId - Unique node identifier
   * @param {string} config.orchestratorUrl - Orchestrator URL
   * @param {number} config.capacity - Max concurrent tasks
   * @param {number} config.heartbeatInterval - Heartbeat interval in ms
   */
  constructor(config = {}) {
    this.config = {
      nodeId: `worker-${Math.random().toString(36).substr(2, 9)}`,
      orchestratorUrl: 'http://localhost:8080',
      capacity: 5,
      heartbeatInterval: 5000,
      ...config,
    };

    this.client = null;
    this.activeTasks = new Map();
    this.capabilities = {
      capacity: this.config.capacity,
      types: ['compute', 'io', 'transform'],
    };
    this.running = false;
    this.heartbeatTimer = null;
  }

  /**
   * Start the worker node
   *
   * @returns {Promise<void>}
   */
  async start() {
    return tracer.startActiveSpan('worker.start', async (span) => {
      try {
        span.setAttribute('worker.id', this.config.nodeId);

        // Connect to orchestrator
        this.client = new FederationClient({
          serverUrl: this.config.orchestratorUrl,
        });

        await this.client.connect();

        // Register with orchestrator
        await this.client.send({
          type: 'worker_registration',
          nodeId: this.config.nodeId,
          capabilities: this.capabilities,
        });

        // Set up message handlers
        this.client.on('message', (msg) => this._handleMessage(msg));

        // Start heartbeat
        this.running = true;
        this._startHeartbeat();

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Handle incoming messages from orchestrator
   *
   * @private
   * @param {object} message - Message from orchestrator
   * @returns {Promise<void>}
   */
  async _handleMessage(message) {
    switch (message.type) {
      case 'task_assignment':
        await this._executeTask(message);
        break;
      case 'task_cancellation':
        await this._cancelTask(message.taskId);
        break;
      case 'shutdown':
        await this.shutdown();
        break;
      default:
        console.warn(`Unknown message type: ${message.type}`);
    }
  }

  /**
   * Execute assigned task
   *
   * @private
   * @param {object} assignment - Task assignment
   * @returns {Promise<void>}
   */
  async _executeTask(assignment) {
    return tracer.startActiveSpan('worker.executeTask', async (span) => {
      try {
        const { workflowId, task, timeout } = assignment;

        span.setAttribute('workflow.id', workflowId);
        span.setAttribute('task.id', task.id);

        // Check capacity
        if (this.activeTasks.size >= this.config.capacity) {
          throw new Error('Worker at capacity');
        }

        // Create task execution context
        const taskContext = {
          workflowId,
          task,
          startTime: Date.now(),
          timeout,
        };

        this.activeTasks.set(task.id, taskContext);

        // Execute task with timeout
        const result = await this._executeWithTimeout(task, timeout);

        // Report completion
        await this.client.send({
          type: 'task_completion',
          workflowId,
          taskId: task.id,
          result,
          duration: Date.now() - taskContext.startTime,
        });

        // Remove from active tasks
        this.activeTasks.delete(task.id);

        span.setAttribute('task.duration', Date.now() - taskContext.startTime);
        span.setStatus({ code: 1 });
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });

        // Report failure
        await this.client.send({
          type: 'task_failure',
          workflowId: assignment.workflowId,
          taskId: assignment.task.id,
          error: error.message,
        });

        // Remove from active tasks
        this.activeTasks.delete(assignment.task.id);
      } finally {
        span.end();
      }
    });
  }

  /**
   * Execute task with timeout
   *
   * @private
   * @param {object} task - Task to execute
   * @param {number} timeout - Timeout in ms
   * @returns {Promise<object>} Task result
   */
  async _executeWithTimeout(task, timeout) {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        reject(new Error(`Task ${task.id} timed out after ${timeout}ms`));
      }, timeout);

      this._performTask(task)
        .then((result) => {
          clearTimeout(timer);
          resolve(result);
        })
        .catch((error) => {
          clearTimeout(timer);
          reject(error);
        });
    });
  }

  /**
   * Perform the actual task execution
   *
   * @private
   * @param {object} task - Task definition
   * @returns {Promise<object>} Task result
   */
  async _performTask(task) {
    return tracer.startActiveSpan('worker.performTask', async (span) => {
      try {
        span.setAttribute('task.id', task.id);
        span.setAttribute('task.type', task.type || 'default');

        // Simulate task execution
        // In production, this would call actual task handlers
        const startTime = Date.now();

        let result;

        switch (task.type) {
          case 'compute':
            result = await this._computeTask(task);
            break;
          case 'io':
            result = await this._ioTask(task);
            break;
          case 'transform':
            result = await this._transformTask(task);
            break;
          default:
            result = await this._defaultTask(task);
        }

        span.setAttribute('task.execution_time', Date.now() - startTime);
        span.setStatus({ code: 1 });

        return result;
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Execute compute task
   *
   * @private
   * @param {object} task - Task definition
   * @returns {Promise<object>} Result
   */
  async _computeTask(task) {
    // Simulate computational work
    await new Promise((resolve) => setTimeout(resolve, 100));

    return {
      type: 'compute',
      input: task.input,
      output: { computed: true, value: Math.random() },
    };
  }

  /**
   * Execute I/O task
   *
   * @private
   * @param {object} task - Task definition
   * @returns {Promise<object>} Result
   */
  async _ioTask(task) {
    // Simulate I/O operation
    await new Promise((resolve) => setTimeout(resolve, 50));

    return {
      type: 'io',
      input: task.input,
      output: { read: true, data: 'sample-data' },
    };
  }

  /**
   * Execute transform task
   *
   * @private
   * @param {object} task - Task definition
   * @returns {Promise<object>} Result
   */
  async _transformTask(task) {
    // Simulate data transformation
    await new Promise((resolve) => setTimeout(resolve, 75));

    return {
      type: 'transform',
      input: task.input,
      output: { transformed: true, result: task.input?.value * 2 },
    };
  }

  /**
   * Execute default task
   *
   * @private
   * @param {object} task - Task definition
   * @returns {Promise<object>} Result
   */
  async _defaultTask(task) {
    // Default task execution
    await new Promise((resolve) => setTimeout(resolve, 100));

    return {
      type: 'default',
      input: task.input,
      output: { success: true },
    };
  }

  /**
   * Cancel running task
   *
   * @private
   * @param {string} taskId - Task ID to cancel
   * @returns {Promise<void>}
   */
  async _cancelTask(taskId) {
    return tracer.startActiveSpan('worker.cancelTask', async (span) => {
      try {
        span.setAttribute('task.id', taskId);

        const taskContext = this.activeTasks.get(taskId);
        if (taskContext) {
          // Mark as cancelled
          taskContext.cancelled = true;

          // Remove from active tasks
          this.activeTasks.delete(taskId);

          // Report cancellation
          await this.client.send({
            type: 'task_cancelled',
            workflowId: taskContext.workflowId,
            taskId,
          });

          span.setStatus({ code: 1 });
        }
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Start sending heartbeats
   *
   * @private
   * @returns {void}
   */
  _startHeartbeat() {
    this.heartbeatTimer = setInterval(async () => {
      if (!this.running) return;

      try {
        await this.client.send({
          type: 'heartbeat',
          nodeId: this.config.nodeId,
          activeTasks: this.activeTasks.size,
          capacity: this.config.capacity,
          timestamp: Date.now(),
        });
      } catch (error) {
        console.error('Heartbeat failed:', error);
      }
    }, this.config.heartbeatInterval);
  }

  /**
   * Get worker statistics
   *
   * @returns {object} Statistics
   */
  getStats() {
    return {
      nodeId: this.config.nodeId,
      capacity: this.config.capacity,
      activeTasks: this.activeTasks.size,
      available: this.config.capacity - this.activeTasks.size,
      running: this.running,
      uptime: this.startTime ? Date.now() - this.startTime : 0,
    };
  }

  /**
   * Shutdown worker gracefully
   *
   * @returns {Promise<void>}
   */
  async shutdown() {
    return tracer.startActiveSpan('worker.shutdown', async (span) => {
      try {
        span.setAttribute('worker.id', this.config.nodeId);

        // Stop accepting new tasks
        this.running = false;

        // Clear heartbeat
        if (this.heartbeatTimer) {
          clearInterval(this.heartbeatTimer);
        }

        // Wait for active tasks to complete (with timeout)
        const shutdownTimeout = 30000; // 30 seconds
        const startTime = Date.now();

        while (this.activeTasks.size > 0 && Date.now() - startTime < shutdownTimeout) {
          await new Promise((resolve) => setTimeout(resolve, 1000));
        }

        // Force cancel remaining tasks
        if (this.activeTasks.size > 0) {
          console.warn(
            `Forcing shutdown with ${this.activeTasks.size} active tasks`
          );
          this.activeTasks.clear();
        }

        // Disconnect from orchestrator
        if (this.client) {
          await this.client.disconnect();
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

export default WorkerNode;
