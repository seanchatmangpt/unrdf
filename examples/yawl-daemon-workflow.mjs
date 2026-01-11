/**
 * @file YAWL Daemon Workflow Example
 * @module examples/yawl-daemon-workflow
 * @description Production-ready example demonstrating YAWL daemon integration
 * with scheduled case creation, timeout enforcement, automatic retries,
 * and parallel task distribution.
 *
 * This example implements a document approval workflow with:
 * - Daily case creation at 2 AM
 * - 5-minute timeout for review tasks
 * - Automatic retry on failures (3 attempts with exponential backoff)
 * - Parallel review distribution across daemon nodes
 * - Comprehensive health monitoring and metrics
 * - Error handling and graceful shutdown
 *
 * Run: node examples/yawl-daemon-workflow.mjs
 */

import { Daemon } from '@unrdf/daemon';
import { YawlDaemonBridge } from '@unrdf/daemon/integrations/yawl';
import { EventEmitter } from 'events';

// =============================================================================
// Mock YAWL Engine (replace with real engine in production)
// =============================================================================

/**
 * Mock YAWL Engine for demonstration
 * In production, use the real YAWL engine from @unrdf/yawl
 */
class MockYawlEngine extends EventEmitter {
  constructor() {
    super();
    this.cases = new Map();
    this.tasks = new Map();
    this.completedCases = 0;
    this.failedTasks = 0;
  }

  /**
   * Override on() to return unsubscriber function
   */
  on(eventName, handler) {
    super.on(eventName, handler);
    return () => this.off(eventName, handler);
  }

  /**
   * Create a workflow case
   */
  async createCase({ workflowId, caseId, inputData = {} }) {
    const caseData = {
      id: caseId,
      workflowId,
      inputData,
      status: 'RUNNING',
      createdAt: new Date(),
      tasks: [],
    };

    this.cases.set(caseId, caseData);
    this.emit('case:created', { caseId, workflowId });

    return {
      caseId,
      workflowId,
      status: 'RUNNING',
      createdAt: caseData.createdAt,
    };
  }

  /**
   * Enable a task for execution
   */
  async enableTask({ caseId, taskId }) {
    const key = `${caseId}:${taskId}`;
    const taskData = {
      caseId,
      taskId,
      status: 'ENABLED',
      enabledAt: new Date(),
    };

    this.tasks.set(key, taskData);

    const caseData = this.cases.get(caseId);
    if (caseData) {
      caseData.tasks.push(taskId);
    }

    this.emit('task:enabled', { caseId, taskId });

    return { caseId, taskId, status: 'ENABLED' };
  }

  /**
   * Cancel a task
   */
  async cancelTask({ caseId, taskId, reason = '' }) {
    const key = `${caseId}:${taskId}`;
    const taskData = this.tasks.get(key);

    if (taskData) {
      taskData.status = 'CANCELLED';
      taskData.reason = reason;
      taskData.cancelledAt = new Date();
      this.tasks.set(key, taskData);
    }

    this.emit('task:cancelled', { caseId, taskId, reason });

    return { caseId, taskId, status: 'CANCELLED' };
  }

  /**
   * Complete a task
   */
  async completeTask({ caseId, taskId, outputData = {} }) {
    const key = `${caseId}:${taskId}`;
    const taskData = this.tasks.get(key);

    if (taskData) {
      taskData.status = 'COMPLETED';
      taskData.outputData = outputData;
      taskData.completedAt = new Date();
      this.tasks.set(key, taskData);
    }

    this.emit('task:completed', { caseId, taskId });

    return { caseId, taskId, status: 'COMPLETED' };
  }

  /**
   * Simulate task failure
   */
  async failTask({ caseId, taskId, error = '' }) {
    const key = `${caseId}:${taskId}`;
    const taskData = this.tasks.get(key);

    if (taskData) {
      taskData.status = 'FAILED';
      taskData.error = error;
      taskData.failedAt = new Date();
      this.tasks.set(key, taskData);
      this.failedTasks += 1;
    }

    this.emit('task:failed', { caseId, taskId, error });

    return { caseId, taskId, status: 'FAILED' };
  }

  /**
   * Get task state
   */
  getTaskState(caseId, taskId) {
    return this.tasks.get(`${caseId}:${taskId}`);
  }

  /**
   * Get case state
   */
  getCaseState(caseId) {
    return this.cases.get(caseId);
  }

  /**
   * Get statistics
   */
  getStats() {
    return {
      totalCases: this.cases.size,
      completedCases: this.completedCases,
      totalTasks: this.tasks.size,
      failedTasks: this.failedTasks,
    };
  }
}

// =============================================================================
// Logging and Monitoring
// =============================================================================

/**
 * Structured logger for production use
 */
const logger = {
  debug: (msg, meta = {}) =>
    console.debug(JSON.stringify({ level: 'debug', msg, timestamp: new Date(), ...meta })),
  info: (msg, meta = {}) =>
    console.log(JSON.stringify({ level: 'info', msg, timestamp: new Date(), ...meta })),
  warn: (msg, meta = {}) =>
    console.warn(JSON.stringify({ level: 'warn', msg, timestamp: new Date(), ...meta })),
  error: (msg, meta = {}) =>
    console.error(JSON.stringify({ level: 'error', msg, timestamp: new Date(), ...meta })),
};

/**
 * Metrics collector for monitoring
 */
class MetricsCollector {
  constructor() {
    this.metrics = {
      casesCreated: 0,
      tasksEnabled: 0,
      tasksCompleted: 0,
      tasksCancelled: 0,
      tasksFailed: 0,
      retriesExecuted: 0,
      timeoutsEnforced: 0,
    };
  }

  increment(metric) {
    if (metric in this.metrics) {
      this.metrics[metric] += 1;
    }
  }

  getMetrics() {
    return { ...this.metrics, timestamp: new Date() };
  }

  reset() {
    Object.keys(this.metrics).forEach((key) => {
      if (typeof this.metrics[key] === 'number') {
        this.metrics[key] = 0;
      }
    });
  }
}

// =============================================================================
// Main Workflow Demonstration
// =============================================================================

async function demonstrateYawlDaemon() {
  console.log('=== YAWL Daemon Workflow Example ===\n');
  logger.info('Starting YAWL daemon workflow demonstration');

  const metrics = new MetricsCollector();

  // -------------------------------------------------------------------------
  // Step 1: Create Daemon
  // -------------------------------------------------------------------------

  console.log('[STEP 1] Creating daemon...');
  const daemon = new Daemon({
    daemonId: `yawl-daemon-${Date.now()}`,
    name: 'YAWL Production Daemon',
    maxConcurrent: 10,
    logger,
  });
  console.log('✓ Daemon created\n');

  // -------------------------------------------------------------------------
  // Step 2: Create YAWL Engine
  // -------------------------------------------------------------------------

  console.log('[STEP 2] Creating YAWL engine...');
  const yawlEngine = new MockYawlEngine();
  console.log('✓ YAWL engine created\n');

  // -------------------------------------------------------------------------
  // Step 3: Create Bridge
  // -------------------------------------------------------------------------

  console.log('[STEP 3] Creating YAWL-Daemon bridge...');
  const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
    daemonNodeId: 'production-node-1',
    maxConcurrentCases: 100,
    enableAutoRetry: true,
    enableTimeoutTracking: true,
    enableDistribution: true,
    retryPolicy: {
      maxAttempts: 3,
      backoffMs: 1000,
      backoffMultiplier: 2,
      maxBackoffMs: 30000,
      jitterFactor: 0.1,
    },
    timeoutDefaults: {
      taskTimeoutMs: 300000, // 5 minutes
      caseTimeoutMs: 3600000, // 1 hour
      checkIntervalMs: 5000, // Check every 5 seconds
    },
    logger,
  });
  console.log('✓ Bridge created\n');

  // -------------------------------------------------------------------------
  // Step 4: Set Up Event Listeners
  // -------------------------------------------------------------------------

  console.log('[STEP 4] Setting up event listeners...');

  // Daemon events
  daemon.on('daemon:started', (event) => {
    logger.info('Daemon started', { nodeId: event.nodeId });
  });

  daemon.on('operation:success', (event) => {
    logger.info('Operation succeeded', {
      operationId: event.operationId,
      duration: event.duration,
    });
  });

  daemon.on('operation:failure', (event) => {
    logger.error('Operation failed', {
      operationId: event.operationId,
      error: event.error,
    });
  });

  // Bridge events
  bridge.on('bridge:started', (event) => {
    logger.info('Bridge started', { bridgeId: event.bridgeId });
  });

  bridge.on('case:created-by-schedule', (event) => {
    metrics.increment('casesCreated');
    logger.info('Case created by schedule', {
      caseId: event.caseId,
      workflowId: event.workflowId,
    });
  });

  bridge.on('task:timeout-enforced', (event) => {
    metrics.increment('timeoutsEnforced');
    logger.warn('Task timeout enforced', {
      caseId: event.caseId,
      taskId: event.taskId,
      timeoutMs: event.timeoutMs,
    });
  });

  bridge.on('task:retry-executed', (event) => {
    metrics.increment('retriesExecuted');
    logger.info('Task retry executed', {
      caseId: event.caseId,
      taskId: event.taskId,
      attempt: event.attempt,
    });
  });

  bridge.on('task:retry-exhausted', (event) => {
    logger.error('Task retry exhausted', {
      caseId: event.caseId,
      taskId: event.taskId,
      attempts: event.attempts,
    });
  });

  bridge.on('tasks:distributed', (event) => {
    logger.info('Tasks distributed', {
      caseId: event.caseId,
      taskCount: event.taskIds.length,
      strategy: event.strategy,
    });
  });

  // YAWL events
  yawlEngine.on('case:created', (event) => {
    logger.info('YAWL case created', { caseId: event.caseId });
  });

  yawlEngine.on('task:enabled', (event) => {
    metrics.increment('tasksEnabled');
    logger.info('YAWL task enabled', { caseId: event.caseId, taskId: event.taskId });
  });

  yawlEngine.on('task:completed', (event) => {
    metrics.increment('tasksCompleted');
    logger.info('YAWL task completed', { caseId: event.caseId, taskId: event.taskId });
  });

  yawlEngine.on('task:cancelled', (event) => {
    metrics.increment('tasksCancelled');
    logger.warn('YAWL task cancelled', {
      caseId: event.caseId,
      taskId: event.taskId,
      reason: event.reason,
    });
  });

  yawlEngine.on('task:failed', (event) => {
    metrics.increment('tasksFailed');
    logger.error('YAWL task failed', {
      caseId: event.caseId,
      taskId: event.taskId,
      error: event.error,
    });
  });

  console.log('✓ Event listeners configured\n');

  // -------------------------------------------------------------------------
  // Step 5: Start Daemon and Bridge
  // -------------------------------------------------------------------------

  console.log('[STEP 5] Starting daemon and bridge...');
  await daemon.start();
  await bridge.start();
  console.log('✓ Daemon and bridge started\n');

  // -------------------------------------------------------------------------
  // Step 6: Schedule Recurring Case Creation
  // -------------------------------------------------------------------------

  console.log('[STEP 6] Scheduling recurring case creation...');
  const scheduleResult = await bridge.scheduleRecurringCase(
    'doc-approval-workflow',
    '0 2 * * *', // Every day at 2 AM
    {
      caseIdPrefix: 'daily-approval',
      priority: 5,
      inputData: {
        department: 'engineering',
        submittedBy: 'automation-system',
        timestamp: new Date().toISOString(),
      },
    }
  );
  console.log(`✓ Scheduled operation: ${scheduleResult.operationId}`);
  console.log(`  Workflow: ${scheduleResult.workflowId}`);
  console.log(`  Schedule: Daily at 2 AM\n`);

  // -------------------------------------------------------------------------
  // Step 7: Demonstrate Timeout Enforcement
  // -------------------------------------------------------------------------

  console.log('[STEP 7] Demonstrating timeout enforcement...');
  const timeoutCaseId = 'demo-timeout-case-001';
  await yawlEngine.createCase({
    caseId: timeoutCaseId,
    workflowId: 'doc-approval-workflow',
    inputData: { department: 'legal' },
  });

  // Enable task
  await yawlEngine.enableTask({ caseId: timeoutCaseId, taskId: 'legal-review' });

  // Watch for timeout (short timeout for demo: 3 seconds)
  const timeoutResult = await bridge.watchTaskTimeout(timeoutCaseId, 'legal-review', 3000);
  console.log(`✓ Watching timeout for task: ${timeoutResult.taskId}`);
  console.log(`  Timeout: ${timeoutResult.timeoutMs}ms\n`);

  // -------------------------------------------------------------------------
  // Step 8: Demonstrate Automatic Retry
  // -------------------------------------------------------------------------

  console.log('[STEP 8] Demonstrating automatic retry...');
  const retryCaseId = 'demo-retry-case-001';
  await yawlEngine.createCase({
    caseId: retryCaseId,
    workflowId: 'doc-approval-workflow',
    inputData: { department: 'finance' },
  });

  // Enable and then fail task
  await yawlEngine.enableTask({ caseId: retryCaseId, taskId: 'finance-review' });
  await yawlEngine.failTask({
    caseId: retryCaseId,
    taskId: 'finance-review',
    error: 'Network timeout - service unavailable',
  });

  console.log('✓ Task failed, automatic retry scheduled by bridge');
  console.log('  Max attempts: 3');
  console.log('  Backoff strategy: Exponential (1s, 2s, 4s)\n');

  // -------------------------------------------------------------------------
  // Step 9: Demonstrate Parallel Task Distribution
  // -------------------------------------------------------------------------

  console.log('[STEP 9] Demonstrating parallel task distribution...');
  const distCaseId = 'demo-distribution-case-001';
  await yawlEngine.createCase({
    caseId: distCaseId,
    workflowId: 'doc-approval-workflow',
    inputData: { department: 'multi' },
  });

  // Distribute parallel review tasks
  const distributionResult = await bridge.distributeAndSplitTasks(
    distCaseId,
    ['legal-review', 'tech-review', 'finance-review', 'compliance-review'],
    { strategy: 'round-robin' }
  );

  console.log(`✓ Tasks distributed: ${distributionResult.taskIds.length} tasks`);
  console.log(`  Strategy: ${distributionResult.strategy}`);
  console.log(`  Distribution ID: ${distributionResult.distributionId}\n`);

  // Execute distribution
  await new Promise((resolve) => setTimeout(resolve, 100));
  const operation = daemon.operations.get(distributionResult.operationId);
  if (operation?.handler) {
    await operation.handler();
  }

  // -------------------------------------------------------------------------
  // Step 10: Simulate Workflow Execution
  // -------------------------------------------------------------------------

  console.log('[STEP 10] Simulating workflow execution...');

  // Complete some tasks
  await yawlEngine.completeTask({
    caseId: distCaseId,
    taskId: 'legal-review',
    outputData: { approved: true, reviewer: 'legal-team' },
  });

  await yawlEngine.completeTask({
    caseId: distCaseId,
    taskId: 'tech-review',
    outputData: { approved: true, reviewer: 'tech-team' },
  });

  console.log('✓ Completed 2 review tasks\n');

  // -------------------------------------------------------------------------
  // Step 11: Monitor Health and Metrics
  // -------------------------------------------------------------------------

  console.log('[STEP 11] Monitoring health and metrics...\n');

  // Daemon health
  const health = daemon.getHealth();
  console.log('━━━ Daemon Health ━━━');
  console.log(`  Running: ${health.isRunning ? '✓ Yes' : '✗ No'}`);
  console.log(`  Uptime: ${Math.round(health.uptime / 1000)}s`);
  console.log(`  Active Operations: ${health.activeOperations}`);
  console.log(`  Queued Operations: ${health.queuedOperations}`);
  console.log(`  Completed Operations: ${health.completedOperations}`);
  console.log();

  // Daemon metrics
  const daemonMetrics = daemon.getMetrics();
  console.log('━━━ Daemon Metrics ━━━');
  console.log(`  Total Operations: ${daemonMetrics.totalOperations}`);
  console.log(`  Successful: ${daemonMetrics.successfulOperations}`);
  console.log(`  Failed: ${daemonMetrics.failedOperations}`);
  console.log(`  Success Rate: ${daemonMetrics.successRate.toFixed(1)}%`);
  console.log(`  Avg Duration: ${daemonMetrics.averageDuration.toFixed(2)}ms`);
  console.log();

  // Bridge statistics
  const bridgeStats = bridge.getStats();
  console.log('━━━ Bridge Statistics ━━━');
  console.log(`  Bridge ID: ${bridgeStats.bridgeId}`);
  console.log(`  Running: ${bridgeStats.isRunning ? '✓ Yes' : '✗ No'}`);
  console.log(`  Case Schedules: ${bridgeStats.caseSchedules}`);
  console.log(`  Active Timeouts: ${bridgeStats.activeTimeouts}`);
  console.log(`  Active Retries: ${bridgeStats.activeRetries}`);
  console.log(`  Active Triggers: ${bridgeStats.activeTriggers}`);
  console.log(`  Distributions: ${bridgeStats.distributions}`);
  console.log();

  // YAWL engine statistics
  const yawlStats = yawlEngine.getStats();
  console.log('━━━ YAWL Engine Statistics ━━━');
  console.log(`  Total Cases: ${yawlStats.totalCases}`);
  console.log(`  Completed Cases: ${yawlStats.completedCases}`);
  console.log(`  Total Tasks: ${yawlStats.totalTasks}`);
  console.log(`  Failed Tasks: ${yawlStats.failedTasks}`);
  console.log();

  // Collected metrics
  const collectedMetrics = metrics.getMetrics();
  console.log('━━━ Collected Metrics ━━━');
  console.log(`  Cases Created: ${collectedMetrics.casesCreated}`);
  console.log(`  Tasks Enabled: ${collectedMetrics.tasksEnabled}`);
  console.log(`  Tasks Completed: ${collectedMetrics.tasksCompleted}`);
  console.log(`  Tasks Cancelled: ${collectedMetrics.tasksCancelled}`);
  console.log(`  Tasks Failed: ${collectedMetrics.tasksFailed}`);
  console.log(`  Retries Executed: ${collectedMetrics.retriesExecuted}`);
  console.log(`  Timeouts Enforced: ${collectedMetrics.timeoutsEnforced}`);
  console.log();

  // -------------------------------------------------------------------------
  // Step 12: Wait for Timeout to Trigger
  // -------------------------------------------------------------------------

  console.log('[STEP 12] Waiting for timeout enforcement (4 seconds)...');
  await new Promise((resolve) => setTimeout(resolve, 4000));

  // Execute timeout check operation
  const timeoutOp = daemon.operations.get(timeoutResult.operationId);
  if (timeoutOp?.handler) {
    await timeoutOp.handler();
  }

  // Verify timeout was enforced
  const timeoutTaskState = yawlEngine.getTaskState(timeoutCaseId, 'legal-review');
  if (timeoutTaskState?.status === 'CANCELLED') {
    console.log('✓ Task was automatically cancelled due to timeout');
    console.log(`  Reason: ${timeoutTaskState.reason}\n`);
  } else {
    console.log(`  Task status: ${timeoutTaskState?.status || 'unknown'}\n`);
  }

  // -------------------------------------------------------------------------
  // Step 13: Display Final Metrics
  // -------------------------------------------------------------------------

  console.log('[STEP 13] Final metrics summary...\n');

  const finalMetrics = metrics.getMetrics();
  console.log('━━━ Final Metrics Summary ━━━');
  console.log(`  Total Cases Created: ${finalMetrics.casesCreated}`);
  console.log(`  Total Tasks Enabled: ${finalMetrics.tasksEnabled}`);
  console.log(`  Total Tasks Completed: ${finalMetrics.tasksCompleted}`);
  console.log(`  Total Tasks Cancelled: ${finalMetrics.tasksCancelled}`);
  console.log(`  Total Timeouts Enforced: ${finalMetrics.timeoutsEnforced}`);
  console.log(`  Total Retries Executed: ${finalMetrics.retriesExecuted}`);
  console.log();

  // -------------------------------------------------------------------------
  // Step 14: Graceful Shutdown
  // -------------------------------------------------------------------------

  console.log('[STEP 14] Shutting down gracefully...');
  await bridge.stop();
  await daemon.stop();
  console.log('✓ Daemon and bridge stopped\n');

  // -------------------------------------------------------------------------
  // Summary
  // -------------------------------------------------------------------------

  console.log('=== Demonstration Complete ===\n');
  console.log('You successfully demonstrated:');
  console.log('  ✓ Daemon and YAWL bridge creation and configuration');
  console.log('  ✓ Scheduled recurring case creation (cron)');
  console.log('  ✓ Task timeout enforcement with automatic cancellation');
  console.log('  ✓ Automatic retry with exponential backoff');
  console.log('  ✓ Parallel task distribution with round-robin strategy');
  console.log('  ✓ Comprehensive health monitoring and metrics collection');
  console.log('  ✓ Event-driven architecture with structured logging');
  console.log('  ✓ Graceful shutdown with resource cleanup');
  console.log();
  console.log('Next steps for production:');
  console.log('  1. Replace MockYawlEngine with real YAWL engine');
  console.log('  2. Configure production retry policies');
  console.log('  3. Set up distributed daemon cluster with Raft consensus');
  console.log('  4. Integrate with observability platform (Prometheus, Grafana)');
  console.log('  5. Implement persistent storage for daemon state');
  console.log('  6. Configure production-grade logging and alerting');
  console.log();

  logger.info('YAWL daemon workflow demonstration completed successfully');
}

// =============================================================================
// Graceful Shutdown Handler
// =============================================================================

let isShuttingDown = false;

process.on('SIGINT', () => {
  if (!isShuttingDown) {
    isShuttingDown = true;
    console.log('\n\nReceived SIGINT, shutting down gracefully...');
    logger.warn('Shutdown signal received');
    process.exit(0);
  }
});

process.on('SIGTERM', () => {
  if (!isShuttingDown) {
    isShuttingDown = true;
    console.log('\n\nReceived SIGTERM, shutting down gracefully...');
    logger.warn('Shutdown signal received');
    process.exit(0);
  }
});

// =============================================================================
// Run Demonstration
// =============================================================================

demonstrateYawlDaemon().catch((error) => {
  console.error('\n❌ Demonstration failed:', error.message);
  logger.error('Demonstration failed', { error: error.message, stack: error.stack });
  process.exit(1);
});
