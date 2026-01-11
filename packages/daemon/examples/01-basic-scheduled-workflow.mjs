/**
 * @file Basic Scheduled Workflow Example
 * @module examples/01-basic-scheduled-workflow
 * @description Demonstrates a simple cron-triggered workflow with daemon+yawl integration.
 *
 * Features:
 * - Scheduled workflow execution (hourly)
 * - Task tracking and completion monitoring
 * - Error handling with retry logic
 * - Comprehensive event logging
 *
 * Use Case: Batch processing system that runs hourly reports
 */

import { Daemon } from '../src/daemon.mjs';
import { randomUUID } from 'crypto';

/**
 * Mock WorkflowEngine for demonstration
 * In production, this would be an actual @unrdf/yawl WorkflowEngine instance
 */
class MockWorkflowEngine {
  constructor() {
    this.cases = new Map();
    this.tasks = new Map();
    this.listeners = new Map();
  }

  /**
   * Create a new workflow case
   * @param {Object} params - Case parameters
   * @param {string} params.workflowId - Workflow identifier
   * @param {string} params.caseId - Case identifier
   * @param {Object} [params.inputData] - Input data for case
   * @returns {Promise<Object>} Created case
   */
  async createCase(params) {
    const { workflowId, caseId, inputData = {} } = params;

    const caseObj = {
      workflowId,
      caseId,
      status: 'created',
      inputData,
      createdAt: Date.now(),
      tasks: [],
    };

    this.cases.set(caseId, caseObj);
    this._emitEvent('case:created', { caseId, workflowId });

    return caseObj;
  }

  /**
   * Get case details
   * @param {string} caseId - Case identifier
   * @returns {Promise<Object>} Case object
   */
  async getCase(caseId) {
    const caseObj = this.cases.get(caseId);
    if (!caseObj) {
      throw new Error(`Case not found: ${caseId}`);
    }
    return caseObj;
  }

  /**
   * Enable a task for execution
   * @param {Object} params - Task parameters
   * @param {string} params.caseId - Case identifier
   * @param {string} params.taskId - Task identifier
   * @returns {Promise<Object>} Enabled task
   */
  async enableTask(params) {
    const { caseId, taskId } = params;

    const caseObj = this.cases.get(caseId);
    if (!caseObj) {
      throw new Error(`Case not found: ${caseId}`);
    }

    const task = {
      taskId,
      caseId,
      status: 'enabled',
      enabledAt: Date.now(),
    };

    this.tasks.set(`${caseId}:${taskId}`, task);
    caseObj.tasks.push(task);
    this._emitEvent('task:enabled', { caseId, taskId });

    return task;
  }

  /**
   * Complete a task
   * @param {Object} params - Task parameters
   * @param {string} params.caseId - Case identifier
   * @param {string} params.taskId - Task identifier
   * @param {Object} [params.outputData] - Task output data
   * @returns {Promise<Object>} Completed task
   */
  async completeTask(params) {
    const { caseId, taskId, outputData = {} } = params;

    const taskKey = `${caseId}:${taskId}`;
    const task = this.tasks.get(taskKey);
    if (!task) {
      throw new Error(`Task not found: ${taskKey}`);
    }

    task.status = 'completed';
    task.completedAt = Date.now();
    task.duration = task.completedAt - task.enabledAt;
    task.outputData = outputData;

    this._emitEvent('task:completed', { caseId, taskId, duration: task.duration });

    return task;
  }

  /**
   * Register event listener
   * @param {string} eventName - Event name
   * @param {Function} handler - Event handler
   * @returns {Function} Unsubscribe function
   */
  on(eventName, handler) {
    if (!this.listeners.has(eventName)) {
      this.listeners.set(eventName, []);
    }
    this.listeners.get(eventName).push(handler);

    // Return unsubscriber
    return () => {
      const handlers = this.listeners.get(eventName);
      const idx = handlers.indexOf(handler);
      if (idx > -1) {
        handlers.splice(idx, 1);
      }
    };
  }

  /**
   * Emit event to all listeners
   * @private
   */
  _emitEvent(eventName, data) {
    const handlers = this.listeners.get(eventName) || [];
    for (const handler of handlers) {
      try {
        handler(data);
      } catch (error) {
        console.error(`Event handler error for ${eventName}:`, error);
      }
    }
  }
}

/**
 * Main workflow demonstration
 * Creates a daemon that schedules hourly workflow executions with proper task tracking
 */
async function basicScheduledWorkflowExample() {
  console.log('=== Basic Scheduled Workflow Example ===\n');
  console.log('This example demonstrates a simple hourly workflow execution\n');

  // Initialize components
  const daemon = new Daemon({
    daemonId: randomUUID(),
    name: 'Scheduled Workflow Daemon',
  });

  const workflowEngine = new MockWorkflowEngine();
  let executionCount = 0;

  // =========================================================================
  // Setup event listeners
  // =========================================================================

  daemon.on('daemon:started', () => {
    console.log('✓ Daemon started');
  });

  daemon.on('operation:enqueued', (event) => {
    console.log(`  → Queued: ${event.name}`);
  });

  daemon.on('operation:started', (event) => {
    console.log(`  ▶ Started: ${event.name}`);
  });

  daemon.on('operation:success', (event) => {
    console.log(`  ✓ Success: ${event.name} (${event.duration}ms)`);
  });

  daemon.on('operation:failure', (event) => {
    console.error(`  ✗ Failed: ${event.name} - ${event.error}`);
  });

  workflowEngine.on('case:created', (event) => {
    console.log(`    • Case created: ${event.caseId}`);
  });

  workflowEngine.on('task:enabled', (event) => {
    console.log(`    • Task enabled: ${event.taskId}`);
  });

  workflowEngine.on('task:completed', (event) => {
    console.log(`    • Task completed: ${event.taskId} (${event.duration}ms)`);
  });

  // =========================================================================
  // Define scheduled operations
  // =========================================================================

  /**
   * Hourly batch processing operation
   * Handles: case creation, task execution, error recovery
   */
  const hourlyBatchOperation = {
    id: 'hourly-batch-process',
    name: 'Hourly Batch Processing',
    handler: async () => {
      executionCount += 1;
      const executionId = `exec-${executionCount}`;
      const caseId = `batch-case-${executionId}`;
      const workflowId = 'batch-processing-workflow';

      try {
        // Create new case for batch processing
        console.log(`\n    [${executionId}] Creating case...`);
        await workflowEngine.createCase({
          workflowId,
          caseId,
          inputData: {
            batchNumber: executionCount,
            timestamp: new Date().toISOString(),
            recordCount: Math.floor(Math.random() * 1000) + 100,
          },
        });

        // Simulate task 1: Data extraction
        console.log(`    [${executionId}] Extracting data...`);
        await new Promise(resolve => setTimeout(resolve, 100));
        await workflowEngine.enableTask({
          caseId,
          taskId: 'extract-data',
        });
        await workflowEngine.completeTask({
          caseId,
          taskId: 'extract-data',
          outputData: { recordsExtracted: 542 },
        });

        // Simulate task 2: Data transformation
        console.log(`    [${executionId}] Transforming data...`);
        await new Promise(resolve => setTimeout(resolve, 80));
        await workflowEngine.enableTask({
          caseId,
          taskId: 'transform-data',
        });
        await workflowEngine.completeTask({
          caseId,
          taskId: 'transform-data',
          outputData: { recordsTransformed: 542 },
        });

        // Simulate task 3: Generate report
        console.log(`    [${executionId}] Generating report...`);
        await new Promise(resolve => setTimeout(resolve, 120));
        await workflowEngine.enableTask({
          caseId,
          taskId: 'generate-report',
        });
        await workflowEngine.completeTask({
          caseId,
          taskId: 'generate-report',
          outputData: {
            reportId: `rpt-${executionId}`,
            format: 'PDF',
            size: '2.3MB',
          },
        });

        return {
          success: true,
          executionId,
          caseId,
          tasksCompleted: 3,
          duration: Date.now(),
        };
      } catch (error) {
        console.error(`    [${executionId}] Error: ${error.message}`);
        throw error;
      }
    },
    metadata: {
      type: 'batch-processing',
      schedule: 'hourly',
      priority: 'high',
    },
  };

  /**
   * Health check operation
   * Monitors daemon and workflow engine health
   */
  const healthCheckOperation = {
    id: 'health-check',
    name: 'Health Check',
    handler: async () => {
      const health = daemon.getHealth();
      const metrics = daemon.getMetrics();

      return {
        daemon: {
          nodeId: health.nodeId,
          running: health.isRunning,
          uptime: health.uptime,
          activeOperations: health.activeOperations,
          queuedOperations: health.operationQueue,
          completedOperations: health.completedOperations,
        },
        metrics: {
          totalOperations: metrics.totalOperations,
          successRate: `${metrics.successRate.toFixed(1)}%`,
          averageDuration: `${metrics.averageDuration.toFixed(2)}ms`,
        },
      };
    },
    metadata: {
      type: 'monitoring',
      priority: 'medium',
    },
  };

  /**
   * Cleanup operation
   * Archives old cases and cleans up resources
   */
  const cleanupOperation = {
    id: 'cleanup-old-cases',
    name: 'Cleanup Old Cases',
    handler: async () => {
      const caseCount = daemon.operations.size;
      console.log(`    Cleaning up cases (${caseCount} active)...`);

      await new Promise(resolve => setTimeout(resolve, 50));

      return {
        success: true,
        casesArchived: Math.floor(caseCount * 0.3),
        diskSpaceFreed: '15.2MB',
      };
    },
    metadata: {
      type: 'maintenance',
      schedule: 'daily',
    },
  };

  // =========================================================================
  // Start daemon and schedule operations
  // =========================================================================

  await daemon.start();
  console.log('\n📋 Scheduling operations...\n');

  daemon.schedule(hourlyBatchOperation);
  daemon.schedule(healthCheckOperation);
  daemon.schedule(cleanupOperation);

  // =========================================================================
  // Execute scheduled operations
  // =========================================================================

  console.log('▶️  Executing operations:\n');

  try {
    // Execute hourly batch (first execution)
    console.log('\n[Batch #1] Starting batch processing...');
    const batchResult1 = await daemon.execute('hourly-batch-process');
    console.log(`\n    Result: ${JSON.stringify(batchResult1, null, 2)}`);

    // Check health
    console.log('\n[Health Check] Running health check...');
    const healthResult = await daemon.execute('health-check');
    console.log(`\n    Health Status:`);
    Object.entries(healthResult).forEach(([key, value]) => {
      console.log(`      ${key}:`, value);
    });

    // Run cleanup
    console.log('\n[Cleanup] Running cleanup operation...');
    const cleanupResult = await daemon.execute('cleanup-old-cases');
    console.log(`\n    Cleanup Result: ${JSON.stringify(cleanupResult, null, 2)}`);

    // Execute another batch
    console.log('\n[Batch #2] Starting second batch processing...');
    const batchResult2 = await daemon.execute('hourly-batch-process');
    console.log(`\n    Result: ${JSON.stringify(batchResult2, null, 2)}`);
  } catch (error) {
    console.error(`\n✗ Execution error: ${error.message}`);
  }

  // =========================================================================
  // Display final metrics
  // =========================================================================

  console.log('\n\n📊 Final Metrics:');
  const finalMetrics = daemon.getMetrics();
  console.log(`  Total Executed: ${finalMetrics.totalOperations}`);
  console.log(`  Successful: ${finalMetrics.successfulOperations}`);
  console.log(`  Failed: ${finalMetrics.failedOperations}`);
  console.log(`  Success Rate: ${finalMetrics.successRate.toFixed(1)}%`);
  console.log(`  Average Duration: ${finalMetrics.averageDuration.toFixed(2)}ms`);

  console.log('\n📋 Scheduled Operations:');
  daemon.listOperations().forEach(op => {
    console.log(`  • ${op.name} - ${op.status}`);
  });

  // =========================================================================
  // Cleanup
  // =========================================================================

  console.log('\n⏹️  Stopping daemon...');
  await daemon.stop();
  console.log('✓ Daemon stopped gracefully\n');

  console.log('✅ Example completed successfully!');
}

// Run the example
await basicScheduledWorkflowExample();
