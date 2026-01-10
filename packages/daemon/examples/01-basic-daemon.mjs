/**
 * @file Basic Daemon Example
 * @module examples/01-basic-daemon
 * @description Demonstrates fundamental daemon usage:
 * - Creating a daemon instance
 * - Scheduling operations
 * - Executing operations
 * - Listening to events
 * - Checking health and metrics
 */

import { Daemon } from '../src/daemon.mjs';

/**
 * Example: Basic daemon with multiple operations
 */
async function basicDaemonExample() {
  console.log('=== Basic Daemon Example ===\n');

  // Create daemon instance
  const daemon = new Daemon({
    id: 'basic-example-daemon',
    logger: console,
  });

  // Define operation 1: Fetch user data
  const fetchUsers = {
    id: 'fetch-users',
    name: 'Fetch Active Users',
    handler: async () => {
      // Simulate database query
      await new Promise(resolve => setTimeout(resolve, 200));
      return {
        status: 'success',
        count: 42,
        timestamp: new Date().toISOString(),
      };
    },
    metadata: {
      dataSource: 'users_db',
      cache: 'redis',
    },
  };

  // Define operation 2: Generate report
  const generateReport = {
    id: 'generate-report',
    name: 'Generate Daily Report',
    handler: async () => {
      // Simulate report generation
      await new Promise(resolve => setTimeout(resolve, 300));
      return {
        status: 'generated',
        format: 'PDF',
        size: '2.5MB',
        timestamp: new Date().toISOString(),
      };
    },
    metadata: {
      format: 'PDF',
      schedule: 'daily_2am',
    },
  };

  // Define operation 3: Cleanup cache
  const cleanupCache = {
    id: 'cleanup-cache',
    name: 'Clean Cache',
    handler: async () => {
      // Simulate cache cleanup
      await new Promise(resolve => setTimeout(resolve, 100));
      return {
        status: 'cleaned',
        itemsRemoved: 156,
        bytesFreed: 5242880, // 5MB
        timestamp: new Date().toISOString(),
      };
    },
    metadata: {
      priority: 'low',
      retention: '24h',
    },
  };

  // Setup event listeners
  daemon.on('daemon:started', (event) => {
    console.log(`📍 Daemon started at ${event.timestamp.toISOString()}`);
  });

  daemon.on('operation:enqueued', (event) => {
    console.log(`📋 Queued: ${event.name}`);
  });

  daemon.on('operation:started', (event) => {
    console.log(`▶️  Started: ${event.name}`);
  });

  daemon.on('operation:success', (event) => {
    console.log(`✓ Success: ${event.name} (${event.duration}ms)`);
  });

  daemon.on('operation:failure', (event) => {
    console.error(`✗ Failed: ${event.name} - ${event.error}`);
  });

  daemon.on('daemon:stopped', () => {
    console.log('🛑 Daemon stopped');
  });

  // Start daemon
  await daemon.start();
  console.log('');

  // Schedule operations
  daemon.schedule(fetchUsers);
  daemon.schedule(generateReport);
  daemon.schedule(cleanupCache);

  // List scheduled operations
  console.log('\n📋 Scheduled Operations:');
  const scheduled = daemon.listOperations();
  scheduled.forEach(op => {
    console.log(`  • ${op.name} (${op.status})`);
  });

  // Execute operations
  console.log('\n▶️  Executing operations...\n');

  try {
    const result1 = await daemon.execute('fetch-users');
    console.log(`  Result: ${JSON.stringify(result1, null, 2)}`);
  } catch (e) {
    console.error(`  Error: ${e.message}`);
  }

  console.log('');

  try {
    const result2 = await daemon.execute('generate-report');
    console.log(`  Result: ${JSON.stringify(result2, null, 2)}`);
  } catch (e) {
    console.error(`  Error: ${e.message}`);
  }

  console.log('');

  try {
    const result3 = await daemon.execute('cleanup-cache');
    console.log(`  Result: ${JSON.stringify(result3, null, 2)}`);
  } catch (e) {
    console.error(`  Error: ${e.message}`);
  }

  // Show health status
  console.log('\n📊 Daemon Health:');
  const health = daemon.getHealth();
  console.log(`  Running: ${health.isRunning ? 'Yes' : 'No'}`);
  console.log(`  Uptime: ${Math.round(health.uptime / 1000)}s`);
  console.log(`  Active Operations: ${health.activeOperations}`);
  console.log(`  Completed Operations: ${health.completedOperations}`);

  // Show metrics
  console.log('\n📈 Metrics:');
  const metrics = daemon.getMetrics();
  console.log(`  Total Executed: ${metrics.totalOperations}`);
  console.log(`  Successful: ${metrics.successfulOperations}`);
  console.log(`  Failed: ${metrics.failedOperations}`);
  console.log(`  Success Rate: ${metrics.successRate.toFixed(1)}%`);
  console.log(`  Avg Duration: ${metrics.averageDuration.toFixed(2)}ms`);
  console.log(`  Total Duration: ${metrics.totalDuration.toFixed(0)}ms`);

  // Cleanup
  console.log('\n⏹️  Stopping daemon...');
  await daemon.stop();
  console.log('✓ Daemon stopped gracefully\n');
}

// Run example
await basicDaemonExample();
