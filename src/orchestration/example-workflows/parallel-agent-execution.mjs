/**
 * @fileoverview Example: Parallel Agent Execution
 *
 * Demonstrates advanced parallel orchestration with 10+ concurrent agents.
 */

import {
  createParallelOrchestrator,
  AgentRole,
  TaskPriority
} from '../index.mjs';

/**
 * Example: KGC-SWARM Parallel Execution
 *
 * This example demonstrates:
 * - 10 specialized agents (α₁-α₁₀)
 * - Priority-based task scheduling
 * - Real-time streaming observables
 * - Circuit breaker failure handling
 * - Agent health monitoring
 */
export async function runParallelAgentExample() {
  console.log('=== KGC-SWARM Parallel Agent Execution Example ===\n');

  // Create orchestrator
  const orchestrator = createParallelOrchestrator({
    workerPool: {
      minWorkers: 3,
      maxWorkers: 10,
      assignmentStrategy: 'least-busy'
    },
    taskQueue: {
      maxSize: 1000,
      defaultPriority: TaskPriority.NORMAL
    },
    circuitBreaker: {
      failureThreshold: 5,
      resetTimeout: 30000
    },
    streaming: {
      enableDeltaCompression: true,
      snapshotInterval: 10,
      backpressureThreshold: 100
    },
    checkpointing: {
      enabled: true,
      interval: 5000
    }
  });

  // Start orchestrator
  await orchestrator.start();
  console.log('Orchestrator started\n');

  // Register 10 specialized agents (KGC-SWARM roles)
  const agents = [
    {
      agentId: 'observer-1',
      role: AgentRole.OBSERVER,
      capabilities: ['observe', 'collect', 'monitor'],
      maxConcurrent: 3
    },
    {
      agentId: 'compressor-1',
      role: AgentRole.COMPRESSOR,
      capabilities: ['compress', 'encode', 'optimize'],
      maxConcurrent: 2
    },
    {
      agentId: 'validator-1',
      role: AgentRole.VALIDATOR,
      capabilities: ['validate', 'verify', 'check'],
      maxConcurrent: 2
    },
    {
      agentId: 'orchestrator-1',
      role: AgentRole.ORCHESTRATOR,
      capabilities: ['orchestrate', 'coordinate', 'manage'],
      maxConcurrent: 1
    },
    {
      agentId: 'analyzer-1',
      role: AgentRole.ANALYZER,
      capabilities: ['analyze', 'inspect', 'evaluate'],
      maxConcurrent: 2
    },
    {
      agentId: 'optimizer-1',
      role: AgentRole.OPTIMIZER,
      capabilities: ['optimize', 'improve', 'enhance'],
      maxConcurrent: 2
    },
    {
      agentId: 'monitor-1',
      role: AgentRole.MONITOR,
      capabilities: ['monitor', 'track', 'observe'],
      maxConcurrent: 3
    },
    {
      agentId: 'aggregator-1',
      role: AgentRole.AGGREGATOR,
      capabilities: ['aggregate', 'combine', 'merge'],
      maxConcurrent: 2
    },
    {
      agentId: 'executor-1',
      role: AgentRole.EXECUTOR,
      capabilities: ['execute', 'run', 'process'],
      maxConcurrent: 3
    },
    {
      agentId: 'coordinator-1',
      role: AgentRole.COORDINATOR,
      capabilities: ['coordinate', 'sync', 'align'],
      maxConcurrent: 1
    }
  ];

  for (const agent of agents) {
    orchestrator.registerAgent(agent);
    console.log(`Registered: ${agent.agentId} (${agent.role})`);
  }
  console.log('');

  // Event listeners
  orchestrator.on('task:submitted', ({ taskId, type }) => {
    console.log(`[SUBMIT] Task ${taskId} (${type})`);
  });

  orchestrator.on('task:completed', ({ taskId, duration }) => {
    console.log(`[COMPLETE] Task ${taskId} (${duration}ms)`);
  });

  orchestrator.on('task:failed', ({ taskId, error }) => {
    console.log(`[FAILED] Task ${taskId}: ${error}`);
  });

  orchestrator.on('agent:circuit-state', ({ agentId, from, to }) => {
    console.log(`[CIRCUIT] Agent ${agentId}: ${from} → ${to}`);
  });

  // Submit tasks with different priorities
  const tasks = [
    // Critical: System monitoring
    {
      type: 'system-monitor',
      priority: TaskPriority.CRITICAL,
      capabilities: ['monitor', 'observe'],
      payload: { system: 'production', interval: 1000 }
    },

    // High: Data validation
    {
      type: 'data-validation',
      priority: TaskPriority.HIGH,
      capabilities: ['validate', 'verify'],
      payload: { dataset: 'critical-dataset', rules: ['rule1', 'rule2'] }
    },

    // Normal: Data compression
    {
      type: 'data-compression',
      priority: TaskPriority.NORMAL,
      capabilities: ['compress', 'optimize'],
      payload: { data: new Array(1000).fill('data'), algorithm: 'lz4' }
    },

    // Normal: Analysis
    {
      type: 'data-analysis',
      priority: TaskPriority.NORMAL,
      capabilities: ['analyze', 'evaluate'],
      payload: { metrics: ['cpu', 'memory', 'disk'], period: '1h' }
    },

    // Low: Background optimization
    {
      type: 'background-optimize',
      priority: TaskPriority.LOW,
      capabilities: ['optimize', 'improve'],
      payload: { target: 'cache', strategy: 'eviction' }
    }
  ];

  console.log('Submitting tasks...\n');

  const observables = [];

  for (const task of tasks) {
    const observable = orchestrator.submit(task);

    // Subscribe to streaming updates
    observable.on('start', ({ taskId, agentId, workerId }) => {
      console.log(`[START] Task ${taskId} on agent ${agentId} (worker ${workerId})`);
    });

    observable.on('delta', ({ data, delta, metadata }) => {
      console.log(`[UPDATE] Task ${observable.taskId} - Step ${data.step}/${data.total} (${data.progress.toFixed(0)}%)`);
      if (metadata.sequence % 3 === 0) {
        console.log(`  Delta: ${JSON.stringify(delta)}`);
      }
    });

    observable.on('snapshot', ({ data, metadata }) => {
      console.log(`[SNAPSHOT] Task ${observable.taskId} at sequence ${metadata.sequence}`);
    });

    observable.on('complete', (result) => {
      console.log(`[DONE] Task ${observable.taskId}: ${result.result}`);
      console.log(`  Duration: ${result.duration}ms, Updates: ${result.updateCount}`);
    });

    observable.on('error', ({ error }) => {
      console.error(`[ERROR] Task ${observable.taskId}: ${error}`);
    });

    observables.push(observable);
  }

  // Wait for all tasks to complete
  await Promise.all(
    observables.map(observable =>
      new Promise((resolve, reject) => {
        observable.on('complete', resolve);
        observable.on('error', reject);
        observable.on('cancelled', resolve);
      })
    )
  );

  // Show final statistics
  console.log('\n=== Final Statistics ===\n');
  const stats = orchestrator.getStats();

  console.log('Orchestrator:');
  console.log(`  Uptime: ${stats.uptime}ms`);
  console.log(`  Total Submitted: ${stats.totalSubmitted}`);
  console.log(`  Total Completed: ${stats.totalCompleted}`);
  console.log(`  Total Failed: ${stats.totalFailed}`);
  console.log(`  Success Rate: ${stats.successRate}`);
  console.log(`  Checkpoints: ${stats.checkpoints}`);

  console.log('\nWorker Pool:');
  console.log(`  Current Workers: ${stats.workerPool.currentWorkers}`);
  console.log(`  Peak Workers: ${stats.workerPool.peakWorkers}`);
  console.log(`  Workers Created: ${stats.workerPool.workersCreated}`);
  console.log(`  Workers Retired: ${stats.workerPool.workersRetired}`);
  console.log(`  Success Rate: ${stats.workerPool.successRate}`);

  console.log('\nTask Queue:');
  console.log(`  Total Enqueued: ${stats.taskQueue.totalEnqueued}`);
  console.log(`  Total Dequeued: ${stats.taskQueue.totalDequeued}`);
  console.log(`  Total Retried: ${stats.taskQueue.totalRetried}`);
  console.log(`  Success Rate: ${stats.taskQueue.successRate}`);

  console.log('\nAgent Router:');
  console.log(`  Total Agents: ${stats.agentRouter.totalAgents}`);
  console.log(`  Total Routings: ${stats.agentRouter.totalRoutings}`);
  console.log(`  Routing Success Rate: ${stats.agentRouter.routingSuccessRate}`);

  // Shutdown
  console.log('\nShutting down orchestrator...');
  await orchestrator.shutdown({ graceful: true, timeout: 5000 });
  console.log('Shutdown complete\n');
}

// Run example if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runParallelAgentExample()
    .then(() => {
      console.log('Example completed successfully');
      process.exit(0);
    })
    .catch((error) => {
      console.error('Example failed:', error);
      process.exit(1);
    });
}
