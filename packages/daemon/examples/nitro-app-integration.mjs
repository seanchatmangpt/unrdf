/**
 * @file Complete Nitro + Daemon Integration Example
 * @module examples/nitro-app-integration
 * @description Full-featured example demonstrating:
 * - Task registration and scheduling
 * - Payload handling and transformation
 * - Error handling and recovery
 * - Event monitoring and health checks
 * - API integration for manual execution
 * - Metrics collection and reporting
 */

import { Daemon } from '../src/daemon.mjs';
import { createNitroTaskExecutor } from '../src/integrations/nitro-tasks.mjs';

// ============================================================================
// UUID Generator Utility
// ============================================================================

/**
 * Generate UUID v4
 * @returns {string} UUID
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

// ============================================================================
// Task Definitions
// ============================================================================

/**
 * Task 1: Database Migration
 * Performs schema migrations with dry-run support
 */
const dbMigrateOpId = generateUUID();

function registerDatabaseMigrateTask(daemon) {
  daemon.schedule({
    id: dbMigrateOpId,
    name: 'database:migrate',
    handler: async (context = {}) => {
      const { dryRun = true, verbose = false } = context;

      if (verbose) {
        console.log('[DB Migration] Starting with dryRun:', dryRun);
      }

      // Simulate migration operations
      const migrations = [
        { name: '001_create_users_table', duration: 50 },
        { name: '002_add_email_index', duration: 30 },
        { name: '003_create_sessions_table', duration: 40 },
      ];

      const results = [];

      for (const migration of migrations) {
        if (verbose) {
          console.log(`[DB Migration] Running: ${migration.name}`);
        }

        // Simulate async operation
        await new Promise((r) => setTimeout(r, migration.duration));

        results.push({
          name: migration.name,
          status: dryRun ? 'simulated' : 'applied',
          duration: migration.duration,
        });
      }

      const totalDuration = results.reduce((sum, r) => sum + r.duration, 0);

      return {
        success: true,
        dryRun,
        migrationsRun: results.length,
        migrations: results,
        totalDuration,
        timestamp: new Date(),
      };
    },
    metadata: {
      category: 'database',
      critical: true,
    },
  });
}

/**
 * Task 2: Cache Warming
 * Pre-loads frequently accessed data into cache
 */
const cacheWarmupOpId = generateUUID();

function registerCacheWarmupTask(daemon) {
  daemon.schedule({
    id: cacheWarmupOpId,
    name: 'cache:warmup',
    handler: async (context = {}) => {
      const { datasets = ['users', 'products', 'settings'], maxAge = 3600 } = context;

      console.log('[Cache Warmup] Warming datasets:', datasets);

      const results = await Promise.all(
        datasets.map(async (dataset) => {
          await new Promise((r) => setTimeout(r, Math.random() * 100 + 50));
          return {
            dataset,
            itemsLoaded: Math.floor(Math.random() * 1000) + 100,
            cacheSize: Math.floor(Math.random() * 10) + 1,
            maxAge,
          };
        })
      );

      const totalItems = results.reduce((sum, r) => sum + r.itemsLoaded, 0);
      const totalSize = results.reduce((sum, r) => sum + r.cacheSize, 0);

      return {
        success: true,
        datasetsWarmed: results.length,
        results,
        totalItemsLoaded: totalItems,
        totalCacheSizeGB: totalSize,
        timestamp: new Date(),
      };
    },
    metadata: {
      category: 'cache',
      priority: 'high',
    },
  });
}

/**
 * Task 3: Email Digest Sending
 * Sends daily digest emails to active users
 */
const emailSendOpId = generateUUID();

function registerEmailSendTask(daemon) {
  daemon.schedule({
    id: emailSendOpId,
    name: 'email:send-digest',
    handler: async (context = {}) => {
      const { digestType = 'daily', batchSize = 100 } = context;

      console.log(`[Email Send] Sending ${digestType} digest to users`);

      // Simulate fetching users
      const userCount = Math.floor(Math.random() * 500) + 100;
      const batchCount = Math.ceil(userCount / batchSize);

      const results = [];
      for (let i = 0; i < batchCount; i++) {
        await new Promise((r) => setTimeout(r, 50)); // Simulate sending
        const sent = Math.min(batchSize, userCount - i * batchSize);
        results.push({
          batch: i + 1,
          sent,
          failed: Math.floor(sent * 0.02), // 2% failure rate
          duration: Math.random() * 100 + 50,
        });
      }

      const totalSent = results.reduce((sum, r) => sum + r.sent, 0);
      const totalFailed = results.reduce((sum, r) => sum + r.failed, 0);

      return {
        success: true,
        digestType,
        batchesProcessed: results.length,
        totalEmailsSent: totalSent,
        totalEmailsFailed: totalFailed,
        successRate: totalSent / (totalSent + totalFailed),
        results,
        timestamp: new Date(),
      };
    },
    metadata: {
      category: 'email',
      priority: 'normal',
    },
  });
}

// ============================================================================
// Application Setup
// ============================================================================

/**
 * Initialize the complete application with daemon and Nitro integration
 */
async function setupApplication() {
  console.log('\n🚀 Initializing Nitro + Daemon Integration Example\n');

  // Create daemon instance
  const daemon = new Daemon({
    daemonId: generateUUID(),
    name: 'nitro-app-daemon',
    port: 8000,
    logLevel: 'info',
    concurrency: 5,
  });

  // Register all task operations
  registerDatabaseMigrateTask(daemon);
  registerCacheWarmupTask(daemon);
  registerEmailSendTask(daemon);

  // Create Nitro executor
  const executor = createNitroTaskExecutor(daemon, {
    autoStart: true,
    timeout: 60000,
    maxRetries: 3,
    enableEventRelay: true,
    enableMetrics: true,
  });

  // =========================================================================
  // Event Listeners Setup
  // =========================================================================

  executor.on('executor:started', (event) => {
    console.log(`✓ Executor started: ${event.executorId}`);
  });

  executor.on('task:registered', (event) => {
    console.log(`✓ Task registered: ${event.nitroTaskId}`);
  });

  executor.on('task:started', (event) => {
    console.log(`▶ Task started: ${event.nitroTaskId}`);
  });

  executor.on('task:succeeded', (event) => {
    console.log(`✓ Task succeeded: ${event.nitroTaskId} (${event.duration}ms)`);
  });

  executor.on('task:failed', (event) => {
    console.error(`✗ Task failed: ${event.nitroTaskId}`);
    console.error(`  Error: ${event.error}`);
  });

  // =========================================================================
  // Task Registration
  // =========================================================================

  // Register database migration task
  executor.registerOperationAsTask(dbMigrateOpId, 'db:migrate', {
    description: 'Run database migrations',
    cronExpression: '0 2 * * *', // 2 AM daily
    priority: 'high',
    tags: ['critical', 'database'],
  });

  // Register cache warmup task
  executor.registerOperationAsTask(cacheWarmupOpId, 'cache:warmup', {
    description: 'Warm application caches',
    cronExpression: '0 */6 * * *', // Every 6 hours
    priority: 'high',
    tags: ['performance', 'cache'],
  });

  // Register email send task
  executor.registerOperationAsTask(emailSendOpId, 'email:send-digest', {
    description: 'Send daily digest emails',
    cronExpression: '0 9 * * *', // 9 AM daily
    priority: 'normal',
    tags: ['email', 'communication'],
  });

  console.log('\n📋 Registered Tasks:\n');
  const tasks = executor.listTasks();
  tasks.forEach((task) => {
    console.log(`  • ${task.nitroTaskId}`);
    console.log(`    Type: ${task.operationType}`);
    console.log(`    Cron: ${task.cronExpression || 'manual'}`);
    console.log(`    Priority: ${task.priority}`);
    console.log('');
  });

  return { daemon, executor };
}

// ============================================================================
// API Route Simulation
// ============================================================================

/**
 * Simulate API route for manual task execution
 */
async function handleTaskExecutionAPI(executor, req) {
  const { taskId } = req.query;
  const payload = req.body || {};

  console.log(`\n🔧 API: Execute Task - ${taskId}`);
  console.log(`   Payload:`, JSON.stringify(payload, null, 2));

  const validation = executor.validateTask(taskId);

  if (!validation.valid) {
    console.log(`   ✗ Validation failed: ${validation.reason}`);
    return {
      success: false,
      error: validation.reason,
      statusCode: 400,
    };
  }

  try {
    const result = await executor.runTask(taskId, payload);
    console.log(`   ✓ Task completed successfully`);
    console.log(`   Result:`, JSON.stringify(result, null, 2));
    return {
      success: true,
      result,
      statusCode: 200,
    };
  } catch (error) {
    console.error(`   ✗ Task execution failed:`, error.message);
    return {
      success: false,
      error: error.message,
      statusCode: 500,
    };
  }
}

// ============================================================================
// Health Check API
// ============================================================================

/**
 * Simulate health check endpoint
 */
function handleHealthCheckAPI(executor) {
  const status = executor.getStatus();
  const metrics = executor.getMetrics();

  console.log('\n🏥 Health Check:');
  console.log(`   Status: ${status.running ? 'HEALTHY' : 'UNHEALTHY'}`);
  console.log(`   Running: ${status.running}`);
  console.log(`   Registered Tasks: ${status.registeredTasks}`);
  console.log(`   Tasks Executed: ${metrics.tasksExecuted}`);
  console.log(`   Success Rate: ${((metrics.tasksSucceeded / metrics.tasksExecuted) * 100).toFixed(1)}%`);
  console.log(`   Avg Duration: ${metrics.averageDuration.toFixed(0)}ms`);

  return {
    healthy: status.running && metrics.tasksFailed < metrics.tasksSucceeded * 0.1,
    status,
    metrics,
  };
}

// ============================================================================
// Scheduled Task Simulation
// ============================================================================

/**
 * Simulate scheduled task execution (cron-like)
 */
async function simulateScheduledExecution(executor) {
  console.log('\n⏰ Simulating Scheduled Executions:\n');

  const tasks = executor.listTasks();

  for (const task of tasks) {
    const taskId = executor.daemonToNitroMap.get(task.daemonOperationId);

    console.log(`\n🕐 Executing scheduled task: ${taskId}`);

    // Prepare payload based on task type
    let payload = {};
    if (taskId.includes('db:migrate')) {
      payload = { dryRun: false, verbose: true };
    } else if (taskId.includes('cache:warmup')) {
      payload = { datasets: ['users', 'products'], maxAge: 3600 };
    } else if (taskId.includes('email:send')) {
      payload = { digestType: 'daily', batchSize: 50 };
    }

    try {
      const result = await executor.runTask(taskId, payload);
      console.log(`✓ Task completed:`, JSON.stringify(result, null, 2));
    } catch (error) {
      console.error(`✗ Task failed:`, error.message);
    }
  }
}

// ============================================================================
// Metrics Reporting
// ============================================================================

/**
 * Generate comprehensive metrics report
 */
function generateMetricsReport(executor) {
  const metrics = executor.getMetrics();
  const status = executor.getStatus();

  console.log('\n📊 Comprehensive Metrics Report:\n');

  console.log('Execution Statistics:');
  console.log(`  Tasks Executed:      ${metrics.tasksExecuted}`);
  console.log(`  Tasks Succeeded:     ${metrics.tasksSucceeded}`);
  console.log(`  Tasks Failed:        ${metrics.tasksFailed}`);
  console.log(`  Success Rate:        ${((metrics.tasksSucceeded / metrics.tasksExecuted) * 100).toFixed(1)}%`);

  console.log('\nPerformance:');
  console.log(`  Total Duration:      ${metrics.totalDuration}ms`);
  console.log(`  Avg Duration:        ${metrics.averageDuration.toFixed(2)}ms`);
  console.log(`  Registered Tasks:    ${metrics.registeredTasks}`);

  console.log('\nDaemon Health:');
  const health = metrics.daemonHealth;
  console.log(`  Running:             ${health.isRunning}`);
  console.log(`  Uptime:              ${(health.uptime / 1000).toFixed(1)}s`);
  console.log(`  Active Operations:   ${health.activeOperations}`);
  console.log(`  Queued Operations:   ${health.queuedOperations}`);

  console.log('\nExecution History:');
  const history = executor.getExecutionHistory();
  history.slice(-5).forEach((entry) => {
    console.log(
      `  ${entry.timestamp.toISOString()} | ${entry.taskId} | ${entry.success ? '✓' : '✗'}`
    );
  });
}

// ============================================================================
// Error Recovery Demonstration
// ============================================================================

/**
 * Demonstrate error handling and recovery strategies
 */
async function demonstrateErrorRecovery(executor) {
  console.log('\n🛡️ Error Recovery Strategies:\n');

  const daemon = executor.daemon;

  // Simulate a failing task
  const failingOpId = generateUUID();

  daemon.schedule({
    id: failingOpId,
    name: 'failing:task',
    handler: async () => {
      throw new Error('Intentional task failure for demonstration');
    },
  });

  executor.registerOperationAsTask(failingOpId, 'demo:failing-task', {
    description: 'Demonstrates failure handling',
    priority: 'normal',
    tags: ['demo', 'error-handling'],
  });

  const taskId = executor.daemonToNitroMap.get(failingOpId);

  // Set up error listener
  executor.on('task:failed', (event) => {
    if (event.nitroTaskId === taskId) {
      console.log(`\n🔄 Implementing recovery strategy for ${taskId}:`);
      console.log(`   Original Error: ${event.error}`);
      console.log(`   Duration: ${event.duration}ms`);
      console.log(`   → Recommended Action: Manual investigation required`);
      console.log(`   → Alert: Sent to ops team`);
    }
  });

  // Attempt execution
  try {
    console.log(`\n▶ Executing failing task: ${taskId}`);
    await executor.runTask(taskId);
  } catch (error) {
    console.log(`✗ Task failed as expected: ${error.message}`);
  }
}

// ============================================================================
// Main Execution
// ============================================================================

/**
 * Run the complete example
 */
async function main() {
  let { daemon, executor } = await setupApplication();

  // Simulate various operations
  await simulateScheduledExecution(executor);

  // Generate metrics report
  generateMetricsReport(executor);

  // Simulate API calls
  console.log('\n🌐 Simulating API Calls:\n');

  // Execute database migration via API
  const dbResult = await handleTaskExecutionAPI(executor, {
    query: {
      taskId: executor.daemonToNitroMap.get(dbMigrateOpId),
    },
    body: { dryRun: false, verbose: true },
  });

  // Check health
  const healthResult = handleHealthCheckAPI(executor);

  // Demonstrate error recovery
  await demonstrateErrorRecovery(executor);

  // Final metrics
  console.log('\n\n📈 Final Metrics:\n');
  generateMetricsReport(executor);

  // Graceful shutdown
  console.log('\n\n🛑 Shutting down...\n');
  await executor.stop();
  console.log('✓ Application shut down successfully\n');
}

// Run example
main().catch(console.error);

export { setupApplication, generateMetricsReport, handleTaskExecutionAPI, handleHealthCheckAPI };
