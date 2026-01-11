/**
 * @file Basic Daemon Usage Example
 * @description Demonstrates fundamental daemon setup and operation scheduling
 *
 * This example shows:
 * 1. Creating a daemon instance
 * 2. Registering operations
 * 3. Starting and stopping the daemon
 * 4. Checking health and metrics
 */

import { Daemon } from '../src/index.mjs';

async function main() {
  // Create a daemon instance
  const daemon = new Daemon({
    daemonId: 'example-daemon',
    name: 'Example Daemon',
    port: 8080,
    concurrency: 5,
  });

  // Subscribe to daemon events
  daemon.on('daemon:started', (event) => {
    console.log('Daemon started:', event);
  });

  daemon.on('operation:success', (event) => {
    console.log('Operation succeeded:', event);
  });

  daemon.on('operation:failure', (event) => {
    console.log('Operation failed:', event);
  });

  // Start the daemon
  await daemon.start();

  // Schedule some operations
  daemon.schedule({
    id: 'task-1',
    name: 'Example Task 1',
    handler: async () => {
      console.log('Executing task 1...');
      return { status: 'completed' };
    },
    metadata: { category: 'example' },
  });

  daemon.schedule({
    id: 'task-2',
    name: 'Example Task 2',
    handler: async () => {
      console.log('Executing task 2...');
      await new Promise(resolve => setTimeout(resolve, 100));
      return { status: 'completed', duration: 100 };
    },
    metadata: { category: 'example' },
  });

  // List scheduled operations
  console.log('Scheduled operations:', daemon.listOperations());

  // Execute an operation
  try {
    const result = await daemon.execute('task-1');
    console.log('Task 1 result:', result);
  } catch (error) {
    console.error('Task 1 failed:', error.message);
  }

  // Get health status
  console.log('Daemon health:', daemon.getHealth());

  // Get metrics
  console.log('Daemon metrics:', daemon.getMetrics());

  // Clean up
  await daemon.stop();
}

main().catch(console.error);
