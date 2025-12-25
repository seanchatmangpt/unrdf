/**
 * @file Real-time Monitoring Dashboard
 * @description WebSocket-based monitoring dashboard for distributed workflows
 * @module distributed-orchestration/monitoring-dashboard
 */

import { createChangeFeed } from '@unrdf/streaming';
import { trace } from '@opentelemetry/api';
import { WebSocketServer } from 'ws';

const tracer = trace.getTracer('monitoring-dashboard');

/**
 * Real-time Monitoring Dashboard
 *
 * Features:
 * - Real-time workflow progress updates
 * - Worker health monitoring
 * - Task queue visualization
 * - Performance metrics
 *
 * @class
 */
export class MonitoringDashboard {
  /**
   * @param {object} config - Configuration
   * @param {object} config.orchestrator - Orchestrator instance
   * @param {number} config.port - WebSocket server port
   * @param {number} config.updateInterval - Metrics update interval in ms
   */
  constructor(config = {}) {
    this.config = {
      port: 3000,
      updateInterval: 1000,
      ...config,
    };

    this.orchestrator = config.orchestrator;
    this.wss = null;
    this.clients = new Set();
    this.changeFeed = null;
    this.metricsTimer = null;
    this.metrics = {
      workflows: [],
      workers: [],
      tasks: {
        active: 0,
        queued: 0,
        completed: 0,
      },
      performance: {
        avgTaskDuration: 0,
        throughput: 0,
      },
    };
  }

  /**
   * Start the monitoring dashboard
   *
   * @returns {Promise<void>}
   */
  async start() {
    return tracer.startActiveSpan('dashboard.start', async (span) => {
      try {
        span.setAttribute('dashboard.port', this.config.port);

        // Create WebSocket server
        this.wss = new WebSocketServer({ port: this.config.port });

        // Handle client connections
        this.wss.on('connection', (ws) => {
          this._handleClientConnection(ws);
        });

        // Subscribe to workflow changes
        if (this.orchestrator?.changeFeed) {
          this.changeFeed = this.orchestrator.changeFeed;
          this.changeFeed.subscribe((changes) => {
            this._handleWorkflowChanges(changes);
          });
        }

        // Start metrics collection
        this._startMetricsCollection();

        console.log(
          `Monitoring dashboard started on ws://localhost:${this.config.port}`
        );

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
   * Handle new client connection
   *
   * @private
   * @param {WebSocket} ws - WebSocket connection
   * @returns {void}
   */
  _handleClientConnection(ws) {
    return tracer.startActiveSpan('dashboard.clientConnect', (span) => {
      try {
        // Add to clients
        this.clients.add(ws);
        span.setAttribute('clients.count', this.clients.size);

        // Send current state
        this._sendInitialState(ws);

        // Handle messages from client
        ws.on('message', (data) => {
          try {
            const message = JSON.parse(data.toString());
            this._handleClientMessage(ws, message);
          } catch (error) {
            console.error('Invalid message from client:', error);
          }
        });

        // Handle disconnection
        ws.on('close', () => {
          this.clients.delete(ws);
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
   * Send initial state to new client
   *
   * @private
   * @param {WebSocket} ws - WebSocket connection
   * @returns {void}
   */
  _sendInitialState(ws) {
    const state = {
      type: 'initial_state',
      metrics: this.metrics,
      orchestratorStats: this.orchestrator?.getStats(),
      timestamp: Date.now(),
    };

    ws.send(JSON.stringify(state));
  }

  /**
   * Handle message from client
   *
   * @private
   * @param {WebSocket} ws - WebSocket connection
   * @param {object} message - Client message
   * @returns {void}
   */
  _handleClientMessage(ws, message) {
    switch (message.type) {
      case 'get_workflow_details':
        this._sendWorkflowDetails(ws, message.workflowId);
        break;
      case 'get_worker_details':
        this._sendWorkerDetails(ws, message.nodeId);
        break;
      case 'pause_workflow':
        this._pauseWorkflow(message.workflowId);
        break;
      case 'resume_workflow':
        this._resumeWorkflow(message.workflowId);
        break;
      default:
        console.warn(`Unknown message type: ${message.type}`);
    }
  }

  /**
   * Handle workflow state changes
   *
   * @private
   * @param {object[]} changes - Change events
   * @returns {void}
   */
  _handleWorkflowChanges(changes) {
    // Broadcast changes to all connected clients
    const update = {
      type: 'workflow_update',
      changes,
      timestamp: Date.now(),
    };

    this.broadcast(update);

    // Update metrics
    this._updateWorkflowMetrics(changes);
  }

  /**
   * Update workflow metrics
   *
   * @private
   * @param {object[]} changes - Change events
   * @returns {void}
   */
  _updateWorkflowMetrics(changes) {
    for (const change of changes) {
      // Track task completions
      if (change.type === 'task_completed') {
        this.metrics.tasks.completed++;

        // Update average task duration
        if (change.duration) {
          const prevAvg = this.metrics.performance.avgTaskDuration;
          const count = this.metrics.tasks.completed;
          this.metrics.performance.avgTaskDuration =
            (prevAvg * (count - 1) + change.duration) / count;
        }
      }
    }
  }

  /**
   * Start collecting metrics
   *
   * @private
   * @returns {void}
   */
  _startMetricsCollection() {
    this.metricsTimer = setInterval(() => {
      this._collectMetrics();
    }, this.config.updateInterval);
  }

  /**
   * Collect current metrics
   *
   * @private
   * @returns {void}
   */
  _collectMetrics() {
    if (!this.orchestrator) return;

    const stats = this.orchestrator.getStats();

    // Update metrics
    this.metrics.tasks.active = stats.tasks.active;
    this.metrics.tasks.queued = stats.tasks.queued;

    // Calculate throughput (tasks per second)
    const now = Date.now();
    if (this.lastMetricsTime) {
      const elapsed = (now - this.lastMetricsTime) / 1000;
      const tasksCompleted =
        this.metrics.tasks.completed - (this.lastCompletedCount || 0);
      this.metrics.performance.throughput = tasksCompleted / elapsed;
    }

    this.lastMetricsTime = now;
    this.lastCompletedCount = this.metrics.tasks.completed;

    // Collect workflow data
    this.metrics.workflows = Array.from(
      this.orchestrator.activeWorkflows.values()
    ).map((w) => ({
      id: w.instance.id,
      status: w.status,
      startTime: w.startTime,
      duration: w.completedAt ? w.completedAt - w.startTime : Date.now() - w.startTime,
    }));

    // Collect worker data
    this.metrics.workers = Array.from(this.orchestrator.workers.values()).map(
      (w) => ({
        nodeId: w.nodeId,
        status: w.status,
        capacity: w.capacity,
        activeTasks: w.activeTasks.size,
        utilization: (w.activeTasks.size / w.capacity) * 100,
      })
    );

    // Broadcast metrics update
    this.broadcast({
      type: 'metrics_update',
      metrics: this.metrics,
      timestamp: now,
    });
  }

  /**
   * Send workflow details to client
   *
   * @private
   * @param {WebSocket} ws - WebSocket connection
   * @param {string} workflowId - Workflow ID
   * @returns {void}
   */
  _sendWorkflowDetails(ws, workflowId) {
    const workflowData = this.orchestrator?.activeWorkflows.get(workflowId);

    if (!workflowData) {
      ws.send(
        JSON.stringify({
          type: 'error',
          message: `Workflow ${workflowId} not found`,
        })
      );
      return;
    }

    const details = {
      type: 'workflow_details',
      workflowId,
      data: workflowData,
      timestamp: Date.now(),
    };

    ws.send(JSON.stringify(details));
  }

  /**
   * Send worker details to client
   *
   * @private
   * @param {WebSocket} ws - WebSocket connection
   * @param {string} nodeId - Worker node ID
   * @returns {void}
   */
  _sendWorkerDetails(ws, nodeId) {
    const worker = this.orchestrator?.workers.get(nodeId);

    if (!worker) {
      ws.send(
        JSON.stringify({
          type: 'error',
          message: `Worker ${nodeId} not found`,
        })
      );
      return;
    }

    const details = {
      type: 'worker_details',
      nodeId,
      data: worker,
      timestamp: Date.now(),
    };

    ws.send(JSON.stringify(details));
  }

  /**
   * Pause workflow execution
   *
   * @private
   * @param {string} workflowId - Workflow ID
   * @returns {void}
   */
  _pauseWorkflow(workflowId) {
    // Implementation would pause workflow in orchestrator
    console.log(`Pausing workflow: ${workflowId}`);
  }

  /**
   * Resume workflow execution
   *
   * @private
   * @param {string} workflowId - Workflow ID
   * @returns {void}
   */
  _resumeWorkflow(workflowId) {
    // Implementation would resume workflow in orchestrator
    console.log(`Resuming workflow: ${workflowId}`);
  }

  /**
   * Broadcast message to all connected clients
   *
   * @param {object} message - Message to broadcast
   * @returns {void}
   */
  broadcast(message) {
    const data = JSON.stringify(message);

    for (const client of this.clients) {
      if (client.readyState === 1) {
        // OPEN
        client.send(data);
      }
    }
  }

  /**
   * Get dashboard statistics
   *
   * @returns {object} Statistics
   */
  getStats() {
    return {
      clients: this.clients.size,
      metrics: this.metrics,
      uptime: Date.now() - (this.startTime || Date.now()),
    };
  }

  /**
   * Shutdown dashboard
   *
   * @returns {Promise<void>}
   */
  async shutdown() {
    return tracer.startActiveSpan('dashboard.shutdown', async (span) => {
      try {
        // Stop metrics collection
        if (this.metricsTimer) {
          clearInterval(this.metricsTimer);
        }

        // Unsubscribe from changes
        if (this.changeFeed) {
          this.changeFeed.unsubscribe();
        }

        // Close all client connections
        for (const client of this.clients) {
          client.close();
        }
        this.clients.clear();

        // Close WebSocket server
        if (this.wss) {
          await new Promise((resolve) => {
            this.wss.close(resolve);
          });
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

export default MonitoringDashboard;
