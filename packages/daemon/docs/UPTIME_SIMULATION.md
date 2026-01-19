# Uptime Simulation Guide

> **Comprehensive Guide for Simulating and Validating System Uptime**

## Overview

The @unrdf/daemon uptime simulation system provides tools for testing system resilience, measuring availability, and validating recovery mechanisms under various conditions. This guide covers architecture, configuration, and practical usage patterns.

## Table of Contents

1. [Architecture](#architecture)
2. [Quick Start](#quick-start)
3. [Configuration Options](#configuration-options)
4. [API Reference](#api-reference)
5. [Examples](#examples)
6. [Monitoring & Metrics](#monitoring--metrics)
7. [Best Practices](#best-practices)

---

## Architecture

### Component Overview

```
                    ┌─────────────────────────────────────────────┐
                    │           Uptime Simulation Layer           │
                    ├─────────────────────────────────────────────┤
                    │  ┌───────────┐  ┌───────────┐  ┌─────────┐ │
                    │  │  Health   │  │  Metrics  │  │  Event  │ │
                    │  │  Monitor  │  │ Collector │  │ Emitter │ │
                    │  └───────────┘  └───────────┘  └─────────┘ │
                    ├─────────────────────────────────────────────┤
                    │  ┌───────────────────────────────────────┐  │
                    │  │         Daemon Core Engine            │  │
                    │  │  - Operation Scheduler                │  │
                    │  │  - Execution Engine                   │  │
                    │  │  - LRU Cache                          │  │
                    │  └───────────────────────────────────────┘  │
                    ├─────────────────────────────────────────────┤
                    │  ┌───────────┐  ┌───────────┐  ┌─────────┐ │
                    │  │   Raft    │  │Distributed│  │  Error  │ │
                    │  │ Consensus │  │   Tasks   │  │Recovery │ │
                    │  └───────────┘  └───────────┘  └─────────┘ │
                    └─────────────────────────────────────────────┘
```

### Key Components

| Component | Responsibility | Key Methods |
|-----------|---------------|-------------|
| Daemon | Core operation management | `start()`, `stop()`, `execute()` |
| Health Monitor | System health tracking | `getHealth()` |
| Metrics Collector | Performance metrics | `getMetrics()` |
| Event Emitter | Real-time notifications | `on()`, `emit()` |
| LRU Cache | Completed operations cache | `set()`, `get()`, `has()` |

### Uptime Calculation

```javascript
// Uptime is calculated from daemon start time
const uptime = Date.now() - daemon.startTime;

// Health check includes uptime
const health = daemon.getHealth();
// {
//   uptime: 86400000,  // 24 hours in ms
//   isRunning: true,
//   activeOperations: 5,
//   ...
// }
```

---

## Quick Start

### Basic Uptime Simulation

```javascript
import { Daemon } from '@unrdf/daemon';

// Create daemon instance
const daemon = new Daemon({
  daemonId: 'uptime-test-001',
  name: 'Uptime Simulation Daemon',
  concurrency: 10,
});

// Start daemon (begins uptime tracking)
await daemon.start();
console.log(`Daemon started at: ${new Date(daemon.startTime)}`);

// Simulate workload
for (let i = 0; i < 100; i++) {
  daemon.schedule({
    id: `op-${i}`,
    name: `Operation ${i}`,
    handler: async () => {
      await new Promise(resolve => setTimeout(resolve, Math.random() * 100));
      return { success: true, timestamp: Date.now() };
    },
  });
}

// Execute operations
for (let i = 0; i < 100; i++) {
  try {
    await daemon.execute(`op-${i}`);
  } catch (error) {
    console.error(`Operation ${i} failed:`, error.message);
  }
}

// Check uptime and health
const health = daemon.getHealth();
console.log(`Uptime: ${health.uptime}ms`);
console.log(`Success rate: ${daemon.getMetrics().successRate}%`);

// Stop daemon
await daemon.stop();
```

### Continuous Uptime Monitoring

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({ daemonId: 'continuous-monitor' });
await daemon.start();

// Set up health monitoring interval
const monitoringInterval = setInterval(() => {
  const health = daemon.getHealth();

  console.log({
    timestamp: new Date().toISOString(),
    uptime: `${(health.uptime / 1000 / 60).toFixed(2)} minutes`,
    isRunning: health.isRunning,
    activeOps: health.activeOperations,
    queuedOps: health.queuedOperations,
    completedOps: health.completedOperations,
  });

  // Alert if daemon is not running
  if (!health.isRunning) {
    console.error('ALERT: Daemon is not running!');
  }
}, 5000); // Every 5 seconds

// Graceful shutdown
process.on('SIGTERM', async () => {
  clearInterval(monitoringInterval);
  await daemon.stop();
  process.exit(0);
});
```

---

## Configuration Options

### Daemon Configuration Schema

```javascript
const daemon = new Daemon({
  // Core identification
  daemonId: 'string',           // Unique daemon identifier (UUID recommended)
  name: 'string',               // Human-readable name

  // Cluster configuration
  nodeId: 'string',             // Node identifier (auto-generated if not provided)
  clusterId: 'string',          // Cluster identifier (default: 'default-cluster')

  // Performance tuning
  concurrency: 10,              // Maximum concurrent operations (default: 10)

  // Cache configuration
  maxCacheSize: 1000,           // Max completed operations in LRU cache

  // Logging
  logger: console,              // Logger instance (default: console)
});
```

### Uptime Simulation Configuration

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `daemonId` | string | required | Unique identifier for the daemon |
| `name` | string | daemonId | Human-readable daemon name |
| `nodeId` | string | auto | Node identifier in cluster |
| `clusterId` | string | 'default-cluster' | Cluster identifier |
| `concurrency` | number | 10 | Max concurrent operations |
| `logger` | object | console | Logger instance |

### Environment Variables

```bash
# Configure daemon via environment
export DAEMON_ID=uptime-daemon-001
export DAEMON_CLUSTER_ID=production-cluster
export DAEMON_CONCURRENCY=50
export DAEMON_LOG_LEVEL=info
```

---

## API Reference

### Daemon Class

#### Constructor

```javascript
/**
 * Creates a new daemon instance
 * @param {Object} config - Daemon configuration
 * @param {string} config.daemonId - Unique daemon identifier
 * @param {string} [config.name] - Human-readable name
 * @param {number} [config.concurrency=10] - Max concurrent operations
 * @param {Object} [config.logger=console] - Logger instance
 */
const daemon = new Daemon(config);
```

#### Lifecycle Methods

```javascript
/**
 * Start the daemon and initialize uptime tracking
 * @returns {Promise<void>}
 */
await daemon.start();

/**
 * Gracefully stop the daemon
 * @returns {Promise<void>}
 */
await daemon.stop();
```

#### Operation Methods

```javascript
/**
 * Schedule an operation for execution
 * @param {Object} operation - Operation configuration
 * @param {string} operation.id - Unique operation identifier
 * @param {Function} operation.handler - Async handler function
 * @param {string} [operation.name] - Operation name
 * @param {Object} [operation.metadata] - Additional metadata
 */
daemon.schedule(operation);

/**
 * Execute a scheduled operation
 * @param {string} operationId - Operation identifier
 * @returns {Promise<*>} Operation result
 */
const result = await daemon.execute(operationId);

/**
 * Remove a scheduled operation
 * @param {string} operationId - Operation identifier
 * @returns {boolean} Whether operation was removed
 */
const removed = daemon.unschedule(operationId);

/**
 * List all scheduled operations
 * @returns {Array<Object>} Array of operation summaries
 */
const operations = daemon.listOperations();
```

#### Health & Metrics Methods

```javascript
/**
 * Get daemon health status
 * @returns {Object} Health information
 */
const health = daemon.getHealth();
// {
//   nodeId: 'node-123',
//   clusterId: 'default-cluster',
//   isRunning: true,
//   isLeader: false,
//   uptime: 86400000,  // ms since start
//   activeOperations: 5,
//   queuedOperations: 10,
//   completedOperations: 1000,
//   timestamp: Date
// }

/**
 * Get performance metrics
 * @returns {Object} Metrics information
 */
const metrics = daemon.getMetrics();
// {
//   nodeId: 'node-123',
//   totalOperations: 1005,
//   successfulOperations: 1000,
//   failedOperations: 5,
//   averageDuration: 45.5,
//   totalDuration: 45727,
//   successRate: 99.5,
//   timestamp: Date
// }
```

### Events

```javascript
// Daemon lifecycle events
daemon.on('daemon:started', ({ nodeId, timestamp }) => {});
daemon.on('daemon:stopped', ({ nodeId, timestamp }) => {});

// Operation lifecycle events
daemon.on('operation:enqueued', ({ operationId, name, timestamp }) => {});
daemon.on('operation:started', ({ operationId, name, timestamp }) => {});
daemon.on('operation:success', ({ operationId, name, duration, timestamp }) => {});
daemon.on('operation:failure', ({ operationId, name, error, duration, timestamp }) => {});
```

---

## Examples

### Example 1: Long-Running Uptime Simulation

```javascript
/**
 * @file Long-running uptime simulation with periodic health checks
 * @description Simulates 24-hour operation with various workload patterns
 */
import { Daemon } from '@unrdf/daemon';

async function runUptimeSimulation() {
  const daemon = new Daemon({
    daemonId: '24h-uptime-test',
    name: '24-Hour Uptime Simulation',
    concurrency: 20,
  });

  // Track metrics over time
  const metricsHistory = [];

  // Event listeners for monitoring
  daemon.on('operation:success', ({ operationId, duration }) => {
    console.log(`[SUCCESS] ${operationId} completed in ${duration}ms`);
  });

  daemon.on('operation:failure', ({ operationId, error }) => {
    console.error(`[FAILURE] ${operationId}: ${error}`);
  });

  await daemon.start();
  console.log('Uptime simulation started');

  // Periodic metrics collection (every minute)
  const metricsCollector = setInterval(() => {
    const health = daemon.getHealth();
    const metrics = daemon.getMetrics();

    metricsHistory.push({
      timestamp: Date.now(),
      uptime: health.uptime,
      successRate: metrics.successRate,
      totalOps: metrics.totalOperations,
      avgDuration: metrics.averageDuration,
    });

    console.log(`[METRICS] Uptime: ${(health.uptime / 1000 / 60).toFixed(1)}m, ` +
                `Success: ${metrics.successRate.toFixed(2)}%, ` +
                `Ops: ${metrics.totalOperations}`);
  }, 60000);

  // Simulate varied workload
  const workloadPatterns = [
    { rate: 10, duration: 300000 },  // 5 min: 10 ops/sec
    { rate: 50, duration: 300000 },  // 5 min: 50 ops/sec
    { rate: 5, duration: 300000 },   // 5 min: 5 ops/sec
    { rate: 100, duration: 300000 }, // 5 min: 100 ops/sec
  ];

  let opCounter = 0;

  for (const pattern of workloadPatterns) {
    const endTime = Date.now() + pattern.duration;
    const interval = 1000 / pattern.rate;

    console.log(`Starting workload: ${pattern.rate} ops/sec for ${pattern.duration / 1000}s`);

    while (Date.now() < endTime) {
      const opId = `op-${opCounter++}`;

      daemon.schedule({
        id: opId,
        handler: async () => {
          // Simulate varying operation duration
          const duration = Math.random() * 50 + 10;
          await new Promise(resolve => setTimeout(resolve, duration));
          return { success: true };
        },
      });

      daemon.execute(opId).catch(() => {});
      await new Promise(resolve => setTimeout(resolve, interval));
    }
  }

  clearInterval(metricsCollector);

  // Final report
  const finalHealth = daemon.getHealth();
  const finalMetrics = daemon.getMetrics();

  console.log('\n=== UPTIME SIMULATION REPORT ===');
  console.log(`Total Uptime: ${(finalHealth.uptime / 1000 / 60).toFixed(2)} minutes`);
  console.log(`Total Operations: ${finalMetrics.totalOperations}`);
  console.log(`Success Rate: ${finalMetrics.successRate.toFixed(2)}%`);
  console.log(`Average Duration: ${finalMetrics.averageDuration.toFixed(2)}ms`);
  console.log('================================\n');

  await daemon.stop();
  return metricsHistory;
}

// Run simulation
runUptimeSimulation().then(history => {
  console.log(`Collected ${history.length} metrics samples`);
});
```

### Example 2: Distributed Cluster Uptime

```javascript
/**
 * @file Distributed cluster uptime simulation
 * @description Simulates multi-node cluster with leader election
 */
import { Daemon } from '@unrdf/daemon';
import { integrateRaftNode, distributeWork } from '@unrdf/daemon';
import { EventEmitter } from 'events';

async function runClusterSimulation(nodeCount = 3) {
  const nodes = [];
  const mockRaftNodes = [];

  // Create daemon nodes
  for (let i = 0; i < nodeCount; i++) {
    const daemon = new Daemon({
      daemonId: `cluster-node-${i}`,
      name: `Cluster Node ${i}`,
      nodeId: `node-${i}`,
      clusterId: 'uptime-cluster',
      concurrency: 10,
    });

    // Create mock Raft node
    const raftNode = new EventEmitter();
    integrateRaftNode(daemon, raftNode);

    nodes.push(daemon);
    mockRaftNodes.push(raftNode);

    await daemon.start();
    console.log(`Node ${i} started`);
  }

  // Elect initial leader (node 0)
  mockRaftNodes[0].emit('leader_elected', { leaderId: nodes[0].id });
  console.log('Node 0 elected as leader');

  // Monitor cluster health
  const clusterMonitor = setInterval(() => {
    const clusterHealth = nodes.map((node, i) => ({
      nodeId: node.nodeId,
      isLeader: node.isLeader,
      isRunning: node.isRunning,
      uptime: node.getHealth().uptime,
      ops: node.getMetrics().totalOperations,
    }));

    console.log('[CLUSTER HEALTH]', JSON.stringify(clusterHealth, null, 2));
  }, 10000);

  // Simulate leader failover after 30 seconds
  setTimeout(() => {
    console.log('Simulating leader failover...');
    mockRaftNodes[0].emit('leader_lost');
    mockRaftNodes[1].emit('leader_elected', { leaderId: nodes[1].id });
    console.log('Node 1 is now leader');
  }, 30000);

  // Run for 2 minutes
  await new Promise(resolve => setTimeout(resolve, 120000));

  clearInterval(clusterMonitor);

  // Shutdown cluster
  for (const node of nodes) {
    await node.stop();
  }

  console.log('Cluster simulation complete');
}

runClusterSimulation(3);
```

### Example 3: Stress Test with Recovery

```javascript
/**
 * @file Stress test with error recovery
 * @description Tests daemon resilience under high load with failures
 */
import { Daemon } from '@unrdf/daemon';

async function runStressTest() {
  const daemon = new Daemon({
    daemonId: 'stress-test',
    name: 'Stress Test Daemon',
    concurrency: 50,
  });

  let successCount = 0;
  let failureCount = 0;

  daemon.on('operation:success', () => successCount++);
  daemon.on('operation:failure', () => failureCount++);

  await daemon.start();

  // Schedule 1000 operations with 10% failure rate
  const totalOps = 1000;
  const failureRate = 0.1;

  for (let i = 0; i < totalOps; i++) {
    daemon.schedule({
      id: `stress-op-${i}`,
      handler: async () => {
        if (Math.random() < failureRate) {
          throw new Error('Simulated failure');
        }
        await new Promise(resolve => setTimeout(resolve, Math.random() * 20));
        return { success: true };
      },
    });
  }

  // Execute all operations concurrently
  const startTime = Date.now();

  await Promise.all(
    Array.from({ length: totalOps }, (_, i) =>
      daemon.execute(`stress-op-${i}`).catch(() => {})
    )
  );

  const duration = Date.now() - startTime;
  const health = daemon.getHealth();
  const metrics = daemon.getMetrics();

  console.log('\n=== STRESS TEST RESULTS ===');
  console.log(`Duration: ${duration}ms`);
  console.log(`Throughput: ${(totalOps / duration * 1000).toFixed(2)} ops/sec`);
  console.log(`Success: ${successCount} (${(successCount / totalOps * 100).toFixed(1)}%)`);
  console.log(`Failures: ${failureCount} (${(failureCount / totalOps * 100).toFixed(1)}%)`);
  console.log(`Daemon still running: ${health.isRunning}`);
  console.log(`Uptime: ${health.uptime}ms`);
  console.log('===========================\n');

  // Verify daemon is still healthy after stress
  if (!health.isRunning) {
    throw new Error('Daemon crashed during stress test!');
  }

  await daemon.stop();
}

runStressTest();
```

---

## Monitoring & Metrics

### Key Uptime Metrics

| Metric | Description | Target |
|--------|-------------|--------|
| `uptime` | Time since daemon start (ms) | Continuous |
| `isRunning` | Daemon running state | `true` |
| `successRate` | Percentage of successful operations | >99% |
| `averageDuration` | Mean operation duration (ms) | <50ms |
| `activeOperations` | Currently executing operations | <maxConcurrent |

### Health Check Implementation

```javascript
/**
 * Production-ready health check endpoint
 */
function createHealthEndpoint(daemon) {
  return async (req, res) => {
    const health = daemon.getHealth();
    const metrics = daemon.getMetrics();

    const status = {
      status: health.isRunning ? 'healthy' : 'unhealthy',
      uptime: health.uptime,
      uptimeHuman: formatUptime(health.uptime),
      metrics: {
        totalOperations: metrics.totalOperations,
        successRate: metrics.successRate,
        averageDuration: metrics.averageDuration,
      },
      capacity: {
        active: health.activeOperations,
        queued: health.queuedOperations,
        cached: health.completedOperations,
      },
      timestamp: new Date().toISOString(),
    };

    res.status(health.isRunning ? 200 : 503).json(status);
  };
}

function formatUptime(ms) {
  const seconds = Math.floor(ms / 1000);
  const minutes = Math.floor(seconds / 60);
  const hours = Math.floor(minutes / 60);
  const days = Math.floor(hours / 24);

  if (days > 0) return `${days}d ${hours % 24}h`;
  if (hours > 0) return `${hours}h ${minutes % 60}m`;
  if (minutes > 0) return `${minutes}m ${seconds % 60}s`;
  return `${seconds}s`;
}
```

### Prometheus Metrics Export

```javascript
/**
 * Export metrics in Prometheus format
 */
function exportPrometheusMetrics(daemon) {
  const health = daemon.getHealth();
  const metrics = daemon.getMetrics();

  return `
# HELP daemon_uptime_seconds Daemon uptime in seconds
# TYPE daemon_uptime_seconds gauge
daemon_uptime_seconds{node="${health.nodeId}"} ${health.uptime / 1000}

# HELP daemon_operations_total Total operations processed
# TYPE daemon_operations_total counter
daemon_operations_total{node="${health.nodeId}",status="success"} ${metrics.successfulOperations}
daemon_operations_total{node="${health.nodeId}",status="failure"} ${metrics.failedOperations}

# HELP daemon_success_rate Operation success rate
# TYPE daemon_success_rate gauge
daemon_success_rate{node="${health.nodeId}"} ${metrics.successRate}

# HELP daemon_operation_duration_ms Average operation duration in ms
# TYPE daemon_operation_duration_ms gauge
daemon_operation_duration_ms{node="${health.nodeId}"} ${metrics.averageDuration}

# HELP daemon_active_operations Currently active operations
# TYPE daemon_active_operations gauge
daemon_active_operations{node="${health.nodeId}"} ${health.activeOperations}

# HELP daemon_queued_operations Operations waiting in queue
# TYPE daemon_queued_operations gauge
daemon_queued_operations{node="${health.nodeId}"} ${health.queuedOperations}
`.trim();
}
```

---

## Best Practices

### 1. Always Monitor Uptime Continuously

```javascript
// Set up continuous health monitoring
const HEALTH_CHECK_INTERVAL = 5000; // 5 seconds

setInterval(() => {
  const health = daemon.getHealth();

  if (!health.isRunning) {
    logger.error('Daemon not running!');
    alerting.critical('Daemon down');
  }

  if (health.activeOperations > daemon.config.maxConcurrent * 0.8) {
    logger.warn('Approaching capacity limit');
    alerting.warning('High load');
  }
}, HEALTH_CHECK_INTERVAL);
```

### 2. Implement Graceful Shutdown

```javascript
async function gracefulShutdown(daemon) {
  console.log('Initiating graceful shutdown...');

  // Stop accepting new operations
  daemon.isRunning = false;

  // Wait for active operations to complete (with timeout)
  const shutdownTimeout = 30000; // 30 seconds
  const startTime = Date.now();

  while (daemon.activeCount > 0 && (Date.now() - startTime) < shutdownTimeout) {
    console.log(`Waiting for ${daemon.activeCount} operations to complete...`);
    await new Promise(resolve => setTimeout(resolve, 1000));
  }

  if (daemon.activeCount > 0) {
    console.warn(`Force stopping with ${daemon.activeCount} active operations`);
  }

  await daemon.stop();
  console.log('Shutdown complete');
}

process.on('SIGTERM', () => gracefulShutdown(daemon));
process.on('SIGINT', () => gracefulShutdown(daemon));
```

### 3. Use Event-Driven Monitoring

```javascript
// Track all operation outcomes
const operationTracker = {
  started: new Map(),
  completed: [],
};

daemon.on('operation:started', ({ operationId, timestamp }) => {
  operationTracker.started.set(operationId, timestamp);
});

daemon.on('operation:success', ({ operationId, duration }) => {
  operationTracker.completed.push({
    id: operationId,
    status: 'success',
    duration,
    timestamp: Date.now(),
  });
  operationTracker.started.delete(operationId);
});

daemon.on('operation:failure', ({ operationId, error, duration }) => {
  operationTracker.completed.push({
    id: operationId,
    status: 'failure',
    error,
    duration,
    timestamp: Date.now(),
  });
  operationTracker.started.delete(operationId);
});
```

### 4. Set Appropriate Timeouts

```javascript
// Wrap operations with timeout
function withTimeout(handler, timeoutMs = 5000) {
  return async (...args) => {
    const timeoutPromise = new Promise((_, reject) =>
      setTimeout(() => reject(new Error(`Operation timed out after ${timeoutMs}ms`)), timeoutMs)
    );

    return Promise.race([handler(...args), timeoutPromise]);
  };
}

daemon.schedule({
  id: 'time-sensitive-op',
  handler: withTimeout(async () => {
    // Operation logic
    return { success: true };
  }, 5000),
});
```

---

## References

- [BENCHMARKING_GUIDE.md](./BENCHMARKING_GUIDE.md) - Performance benchmarking
- [CHAOS_ENGINEERING.md](./CHAOS_ENGINEERING.md) - Chaos engineering patterns
- [performance-tuning.md](./performance-tuning.md) - Performance optimization
- [error-path-scenarios.md](./error-path-scenarios.md) - Error handling scenarios

---

**Document Version**: 1.0.0
**Last Updated**: 2026-01-18
**Target**: @unrdf/daemon v6.0.0+
