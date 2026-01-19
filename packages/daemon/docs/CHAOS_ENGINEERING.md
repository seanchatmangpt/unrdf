# Chaos Engineering Guide

> **Systematic Approach to Testing System Resilience Through Controlled Failure Injection**

## Overview

Chaos engineering is the discipline of experimenting on a distributed system to build confidence in its capability to withstand turbulent conditions in production. This guide covers chaos engineering principles, available chaos types, safety guidelines, and recovery procedures for the @unrdf/daemon system.

## Table of Contents

1. [Principles](#principles)
2. [Available Chaos Types](#available-chaos-types)
3. [Running Chaos Experiments](#running-chaos-experiments)
4. [Safety Guidelines](#safety-guidelines)
5. [Recovery Procedures](#recovery-procedures)
6. [Experiment Catalog](#experiment-catalog)
7. [Advanced Patterns](#advanced-patterns)

---

## Principles

### The Chaos Engineering Manifesto

1. **Build a Hypothesis Around Steady State Behavior**
   - Define what "normal" looks like for your system
   - Identify key metrics: uptime, latency, success rate

2. **Vary Real-World Events**
   - Inject failures that could happen in production
   - Test network issues, resource exhaustion, dependencies

3. **Run Experiments in Production**
   - Start in staging, graduate to production
   - Use canary deployments and feature flags

4. **Automate Experiments to Run Continuously**
   - Integrate with CI/CD pipelines
   - Schedule regular chaos experiments

5. **Minimize Blast Radius**
   - Start small, expand carefully
   - Have kill switches ready

### Daemon-Specific Principles

For the @unrdf/daemon system, we focus on:

| Principle | Implementation |
|-----------|---------------|
| Operation Isolation | Failures in one operation should not cascade |
| Graceful Degradation | System continues serving under partial failure |
| Fast Recovery | Return to steady state within SLO targets |
| Observable Failures | All failures are logged and tracked |
| Deterministic Recovery | Same failure always results in same recovery |

---

## Available Chaos Types

### 1. Operation Failures

Inject failures into operation handlers.

```javascript
/**
 * Chaos Type: Handler Exception
 * Simulates synchronous and asynchronous handler failures
 */
const chaosHandlers = {
  // Synchronous failure
  syncError: () => {
    throw new Error('Chaos: Synchronous handler failure');
  },

  // Asynchronous failure
  asyncError: async () => {
    await new Promise(resolve => setTimeout(resolve, 10));
    throw new Error('Chaos: Asynchronous handler failure');
  },

  // Random failure (configurable rate)
  randomFailure: (failureRate = 0.1) => async () => {
    if (Math.random() < failureRate) {
      throw new Error('Chaos: Random failure');
    }
    return { success: true };
  },

  // Conditional failure
  conditionalFailure: (condition) => async () => {
    if (condition()) {
      throw new Error('Chaos: Conditional failure triggered');
    }
    return { success: true };
  },
};
```

### 2. Timeout Failures

Simulate operations that exceed timeout thresholds.

```javascript
/**
 * Chaos Type: Operation Timeout
 * Simulates slow or hanging operations
 */
const timeoutChaos = {
  // Never resolves
  neverResolve: () => new Promise(() => {}),

  // Delayed response
  slowResponse: (delayMs) => async () => {
    await new Promise(resolve => setTimeout(resolve, delayMs));
    return { delayed: true };
  },

  // Variable latency
  jitteryResponse: (minMs, maxMs) => async () => {
    const delay = Math.random() * (maxMs - minMs) + minMs;
    await new Promise(resolve => setTimeout(resolve, delay));
    return { delay };
  },

  // Exponentially increasing latency
  degradingResponse: () => {
    let delay = 10;
    return async () => {
      delay *= 1.5;
      await new Promise(resolve => setTimeout(resolve, delay));
      return { delay };
    };
  },
};
```

### 3. Resource Exhaustion

Simulate memory pressure and queue saturation.

```javascript
/**
 * Chaos Type: Resource Exhaustion
 * Simulates resource constraints
 */
const resourceChaos = {
  // Large metadata (memory pressure)
  memoryPressure: (sizeKb) => ({
    id: `chaos-mem-${Date.now()}`,
    metadata: {
      largeData: 'x'.repeat(sizeKb * 1024),
    },
    handler: async () => ({ success: true }),
  }),

  // Queue flooding
  queueFlood: (daemon, count) => {
    for (let i = 0; i < count; i++) {
      daemon.schedule({
        id: `flood-op-${i}`,
        handler: async () => {
          await new Promise(resolve => setTimeout(resolve, 100));
          return { flooded: true };
        },
      });
    }
  },

  // Concurrent execution pressure
  concurrencyPressure: (daemon, count) => {
    const operations = [];
    for (let i = 0; i < count; i++) {
      daemon.schedule({
        id: `concurrent-${i}`,
        handler: async () => {
          await new Promise(resolve => setTimeout(resolve, 1000));
          return { concurrent: true };
        },
      });
      operations.push(`concurrent-${i}`);
    }
    return Promise.all(operations.map(id => daemon.execute(id).catch(() => {})));
  },
};
```

### 4. Network Partitions (Simulated)

Simulate distributed system partitions.

```javascript
/**
 * Chaos Type: Network Partition
 * Simulates cluster communication failures
 */
const networkChaos = {
  // Leader isolation
  isolateLeader: (raftNode) => {
    raftNode.emit('leader_lost');
  },

  // Split brain (multiple leaders)
  splitBrain: (raftNodes) => {
    // Node 0 thinks it's leader
    raftNodes[0].emit('leader_elected', { leaderId: 'node-0' });
    // Node 1 also thinks it's leader (conflict)
    raftNodes[1].emit('leader_elected', { leaderId: 'node-1' });
  },

  // Delayed leader election
  delayedElection: async (raftNode, delayMs) => {
    raftNode.emit('leader_lost');
    await new Promise(resolve => setTimeout(resolve, delayMs));
    raftNode.emit('leader_elected', { leaderId: 'recovered-leader' });
  },

  // Flapping leadership
  flappingLeadership: async (raftNode, iterations, intervalMs) => {
    for (let i = 0; i < iterations; i++) {
      raftNode.emit('leader_elected', { leaderId: `leader-${i % 3}` });
      await new Promise(resolve => setTimeout(resolve, intervalMs));
    }
  },
};
```

### 5. Data Corruption (Simulated)

Simulate corrupted state and invalid data.

```javascript
/**
 * Chaos Type: Data Corruption
 * Simulates state corruption scenarios
 */
const corruptionChaos = {
  // Corrupt operation state
  corruptOperationState: (daemon, operationId) => {
    const op = daemon.operations.get(operationId);
    if (op) {
      op.status = 'INVALID_STATE';
      op.handler = null;
    }
  },

  // Corrupt cache entry
  corruptCacheEntry: (daemon, operationId) => {
    const entry = daemon.completedOperations.get(operationId);
    if (entry) {
      entry.status = null;
      entry.result = undefined;
      entry.duration = -1;
    }
  },

  // Invalid operation data
  invalidOperationData: () => ({
    id: null,  // Invalid: null ID
    handler: 'not-a-function',  // Invalid: string instead of function
  }),
};
```

---

## Running Chaos Experiments

### Experiment Structure

```javascript
/**
 * @file Chaos Experiment Template
 * @description Standard structure for chaos experiments
 */
import { Daemon } from '@unrdf/daemon';
import { describe, it, expect, beforeEach, afterEach } from 'vitest';

describe('Chaos Experiment: [Name]', () => {
  let daemon;

  // 1. SETUP: Define steady state
  beforeEach(async () => {
    daemon = new Daemon({
      daemonId: 'chaos-experiment',
      name: 'Chaos Test Daemon',
      concurrency: 10,
    });
    await daemon.start();
  });

  // 2. CLEANUP: Ensure recovery
  afterEach(async () => {
    await daemon.stop();
  });

  // 3. HYPOTHESIS: Define expected behavior
  it('should [expected behavior] when [chaos condition]', async () => {
    // ARRANGE: Set up steady state
    const successHandler = async () => ({ success: true });
    daemon.schedule({ id: 'baseline-op', handler: successHandler });
    await daemon.execute('baseline-op');

    // Verify steady state
    expect(daemon.isRunning).toBe(true);

    // ACT: Inject chaos
    const chaosHandler = () => { throw new Error('Chaos failure'); };
    daemon.schedule({ id: 'chaos-op', handler: chaosHandler });

    try {
      await daemon.execute('chaos-op');
    } catch (e) {
      // Expected failure
    }

    // ASSERT: System maintains steady state
    expect(daemon.isRunning).toBe(true);

    // Verify recovery: new operations should work
    daemon.schedule({ id: 'recovery-op', handler: successHandler });
    const result = await daemon.execute('recovery-op');
    expect(result.success).toBe(true);
  });
});
```

### Running Chaos Tests

```bash
# Run all chaos-related tests
pnpm -C packages/daemon test -- --grep "Chaos"

# Run error recovery tests (includes chaos scenarios)
pnpm -C packages/daemon test e2e-error-recovery

# Run with verbose output
pnpm -C packages/daemon test e2e-error-recovery -- --reporter=verbose

# Run specific chaos scenario
pnpm -C packages/daemon test -- --grep "cascading failure"
```

### Chaos Experiment Runner

```javascript
/**
 * @file Chaos Experiment Runner
 * @description Automated chaos experiment execution
 */
import { Daemon } from '@unrdf/daemon';

class ChaosRunner {
  constructor(daemon) {
    this.daemon = daemon;
    this.results = [];
  }

  /**
   * Run a chaos experiment
   * @param {string} name - Experiment name
   * @param {Function} chaosInjector - Function that injects chaos
   * @param {Function} steadyStateValidator - Function that validates system health
   * @param {Object} options - Experiment options
   */
  async runExperiment(name, chaosInjector, steadyStateValidator, options = {}) {
    const {
      duration = 10000,
      cooldown = 5000,
      repetitions = 3,
    } = options;

    console.log(`Starting chaos experiment: ${name}`);

    const experimentResults = [];

    for (let i = 0; i < repetitions; i++) {
      // Verify steady state before chaos
      const preChaosHealth = await steadyStateValidator(this.daemon);
      if (!preChaosHealth.healthy) {
        throw new Error(`Pre-chaos health check failed: ${preChaosHealth.reason}`);
      }

      // Inject chaos
      const chaosStart = Date.now();
      console.log(`  [${i + 1}/${repetitions}] Injecting chaos...`);
      await chaosInjector(this.daemon);

      // Wait for chaos duration
      await new Promise(resolve => setTimeout(resolve, duration));
      const chaosDuration = Date.now() - chaosStart;

      // Allow recovery
      console.log(`  [${i + 1}/${repetitions}] Cooling down...`);
      await new Promise(resolve => setTimeout(resolve, cooldown));

      // Verify steady state after chaos
      const postChaosHealth = await steadyStateValidator(this.daemon);
      const recoveryTime = Date.now() - chaosStart - duration;

      experimentResults.push({
        iteration: i + 1,
        preChaosHealthy: preChaosHealth.healthy,
        postChaosHealthy: postChaosHealth.healthy,
        chaosDuration,
        recoveryTime,
        recovered: postChaosHealth.healthy,
      });

      console.log(`  [${i + 1}/${repetitions}] ${postChaosHealth.healthy ? 'RECOVERED' : 'FAILED'}`);
    }

    const result = {
      name,
      timestamp: new Date().toISOString(),
      repetitions,
      successRate: experimentResults.filter(r => r.recovered).length / repetitions * 100,
      avgRecoveryTime: experimentResults.reduce((sum, r) => sum + r.recoveryTime, 0) / repetitions,
      details: experimentResults,
    };

    this.results.push(result);
    console.log(`Experiment complete: ${result.successRate}% success rate`);

    return result;
  }

  /**
   * Generate experiment report
   */
  getReport() {
    return {
      totalExperiments: this.results.length,
      overallSuccessRate: this.results.reduce((sum, r) => sum + r.successRate, 0) / this.results.length,
      experiments: this.results,
    };
  }
}

// Usage example
async function runChaosExperiments() {
  const daemon = new Daemon({ daemonId: 'chaos-runner' });
  await daemon.start();

  const runner = new ChaosRunner(daemon);

  // Define steady state validator
  const validateSteadyState = async (d) => {
    const health = d.getHealth();
    return {
      healthy: health.isRunning && health.activeOperations === 0,
      reason: !health.isRunning ? 'Daemon not running' :
              health.activeOperations > 0 ? 'Operations still active' : null,
    };
  };

  // Run experiments
  await runner.runExperiment(
    'handler-failure',
    async (d) => {
      d.schedule({
        id: 'chaos-op',
        handler: () => { throw new Error('Chaos'); },
      });
      try { await d.execute('chaos-op'); } catch (e) {}
    },
    validateSteadyState,
    { duration: 1000, cooldown: 1000, repetitions: 5 }
  );

  console.log('\n=== CHAOS REPORT ===');
  console.log(JSON.stringify(runner.getReport(), null, 2));

  await daemon.stop();
}
```

---

## Safety Guidelines

### Pre-Experiment Checklist

- [ ] **Isolated Environment**: Run in staging, not production (initially)
- [ ] **Monitoring Enabled**: Ensure all metrics and logs are being collected
- [ ] **Kill Switch Ready**: Have immediate abort mechanism
- [ ] **Rollback Plan**: Know how to restore previous state
- [ ] **Stakeholder Notification**: Inform relevant teams
- [ ] **Time Boxed**: Set maximum experiment duration
- [ ] **Blast Radius Defined**: Know what could be affected

### Blast Radius Management

```javascript
/**
 * Blast Radius Levels
 */
const BLAST_RADIUS = {
  // Level 1: Single operation
  OPERATION: {
    description: 'Affects single operation only',
    risk: 'LOW',
    approval: 'Self',
    example: 'One operation handler throws exception',
  },

  // Level 2: Single daemon instance
  INSTANCE: {
    description: 'Affects single daemon instance',
    risk: 'MEDIUM',
    approval: 'Team lead',
    example: 'Daemon memory exhaustion',
  },

  // Level 3: Cluster node
  NODE: {
    description: 'Affects entire node in cluster',
    risk: 'HIGH',
    approval: 'Engineering manager',
    example: 'Node crash simulation',
  },

  // Level 4: Cluster-wide
  CLUSTER: {
    description: 'Affects entire cluster',
    risk: 'CRITICAL',
    approval: 'VP Engineering',
    example: 'Network partition between all nodes',
  },
};
```

### Safety Controls

```javascript
/**
 * Chaos Safety Controls
 */
class ChaosSafetyGuard {
  constructor(options = {}) {
    this.maxDuration = options.maxDuration || 60000;  // 1 minute
    this.healthCheckInterval = options.healthCheckInterval || 1000;
    this.autoAbortThreshold = options.autoAbortThreshold || 0.5;  // 50% failure rate
    this.isActive = false;
    this.abortController = null;
  }

  /**
   * Start chaos with safety guards
   */
  async withSafety(daemon, chaosExperiment) {
    this.isActive = true;
    this.abortController = new AbortController();

    const startTime = Date.now();
    let failureCount = 0;
    let totalChecks = 0;

    // Health monitoring
    const monitor = setInterval(async () => {
      totalChecks++;
      const health = daemon.getHealth();

      if (!health.isRunning) {
        failureCount++;
      }

      // Auto-abort conditions
      const elapsed = Date.now() - startTime;
      const failureRate = failureCount / totalChecks;

      if (elapsed > this.maxDuration) {
        console.warn('SAFETY: Max duration exceeded, aborting chaos');
        this.abort();
      }

      if (failureRate > this.autoAbortThreshold && totalChecks > 10) {
        console.warn(`SAFETY: Failure rate ${(failureRate * 100).toFixed(1)}% exceeded threshold, aborting`);
        this.abort();
      }
    }, this.healthCheckInterval);

    try {
      await Promise.race([
        chaosExperiment(),
        new Promise((_, reject) => {
          this.abortController.signal.addEventListener('abort', () => {
            reject(new Error('Chaos experiment aborted by safety guard'));
          });
        }),
      ]);
    } finally {
      clearInterval(monitor);
      this.isActive = false;
    }
  }

  /**
   * Abort active chaos experiment
   */
  abort() {
    if (this.abortController) {
      this.abortController.abort();
    }
  }
}

// Usage
const guard = new ChaosSafetyGuard({ maxDuration: 30000 });

await guard.withSafety(daemon, async () => {
  // Your chaos experiment here
  await injectChaos();
});
```

### Emergency Stop Procedure

```javascript
/**
 * Emergency Stop for Chaos Experiments
 */
async function emergencyStop(daemon, reason) {
  console.error(`EMERGENCY STOP: ${reason}`);

  // 1. Stop accepting new operations
  daemon.isRunning = false;

  // 2. Log current state
  const health = daemon.getHealth();
  const metrics = daemon.getMetrics();
  console.error('State at emergency stop:', { health, metrics });

  // 3. Wait for active operations (with timeout)
  const timeout = 10000;
  const start = Date.now();
  while (daemon.activeCount > 0 && (Date.now() - start) < timeout) {
    await new Promise(resolve => setTimeout(resolve, 100));
  }

  // 4. Force stop
  await daemon.stop();

  // 5. Notify
  console.error('Emergency stop complete');

  return {
    reason,
    stoppedAt: new Date().toISOString(),
    activeOperationsAtStop: daemon.activeCount,
    finalHealth: health,
    finalMetrics: metrics,
  };
}
```

---

## Recovery Procedures

### Automatic Recovery Verification

```javascript
/**
 * Recovery Verification Protocol
 */
async function verifyRecovery(daemon, options = {}) {
  const {
    timeout = 30000,
    checkInterval = 1000,
    requiredSuccessOperations = 5,
  } = options;

  const startTime = Date.now();
  const verificationResults = [];

  // Step 1: Verify daemon is running
  if (!daemon.isRunning) {
    return {
      recovered: false,
      reason: 'Daemon not running',
      details: verificationResults,
    };
  }
  verificationResults.push({ check: 'daemon_running', passed: true });

  // Step 2: Verify health endpoint responds
  try {
    const health = daemon.getHealth();
    verificationResults.push({
      check: 'health_endpoint',
      passed: true,
      health,
    });
  } catch (e) {
    return {
      recovered: false,
      reason: 'Health endpoint failed',
      details: verificationResults,
    };
  }

  // Step 3: Verify new operations can be scheduled and executed
  const testOperations = [];
  for (let i = 0; i < requiredSuccessOperations; i++) {
    const opId = `recovery-verify-${Date.now()}-${i}`;
    daemon.schedule({
      id: opId,
      handler: async () => ({ verified: true, index: i }),
    });
    testOperations.push(opId);
  }

  let successCount = 0;
  for (const opId of testOperations) {
    try {
      const result = await daemon.execute(opId);
      if (result.verified) {
        successCount++;
      }
    } catch (e) {
      // Operation failed
    }
  }

  verificationResults.push({
    check: 'operation_execution',
    passed: successCount === requiredSuccessOperations,
    successCount,
    required: requiredSuccessOperations,
  });

  // Step 4: Verify metrics are accurate
  const metrics = daemon.getMetrics();
  verificationResults.push({
    check: 'metrics_available',
    passed: metrics.totalOperations >= requiredSuccessOperations,
    metrics,
  });

  const allPassed = verificationResults.every(r => r.passed);

  return {
    recovered: allPassed,
    duration: Date.now() - startTime,
    details: verificationResults,
  };
}
```

### Manual Recovery Steps

```
RECOVERY RUNBOOK: Daemon Chaos Recovery

1. ASSESS
   - Check daemon.getHealth().isRunning
   - Check daemon.getMetrics().failedOperations
   - Check activeOperations count

2. ISOLATE
   - Stop accepting new operations if needed
   - Let active operations drain (with timeout)

3. RECOVER
   - If daemon crashed: Restart daemon instance
   - If state corrupted: Clear and reinitialize
   - If cluster partitioned: Trigger leader re-election

4. VERIFY
   - Run verifyRecovery() function
   - Execute test operations
   - Check metrics accuracy

5. RESUME
   - Re-enable operation acceptance
   - Monitor closely for 15 minutes
   - Document incident
```

### Recovery Time Objectives (RTO)

| Chaos Type | Target RTO | Max RTO | Escalation |
|------------|------------|---------|------------|
| Operation failure | 0s | 0s | Automatic |
| Handler exception | 0s | 0s | Automatic |
| Timeout | 5s | 30s | On-call |
| Memory pressure | 30s | 60s | On-call |
| Node crash | 60s | 300s | Incident |
| Network partition | 120s | 600s | Incident |
| Cluster failure | 300s | 1800s | P1 Incident |

---

## Experiment Catalog

### Experiment 1: Cascading Failure Prevention

```javascript
describe('Chaos: Cascading Failure Prevention', () => {
  it('should isolate failures preventing cascade', async () => {
    const daemon = new Daemon({ daemonId: 'cascade-test' });
    await daemon.start();

    // Schedule mix of failing and succeeding operations
    const sequence = [
      { id: 'op-1', fails: true },
      { id: 'op-2', fails: false },
      { id: 'op-3', fails: true },
      { id: 'op-4', fails: false },
      { id: 'op-5', fails: false },
    ];

    sequence.forEach(({ id, fails }) => {
      daemon.schedule({
        id,
        handler: fails
          ? async () => { throw new Error(`${id} failed`); }
          : async () => ({ success: true }),
      });
    });

    const results = [];
    for (const { id } of sequence) {
      try {
        const result = await daemon.execute(id);
        results.push({ id, succeeded: true, result });
      } catch (e) {
        results.push({ id, succeeded: false, error: e.message });
      }
    }

    // Verify: Failures don't prevent subsequent successes
    expect(results[0].succeeded).toBe(false);
    expect(results[1].succeeded).toBe(true);  // Should succeed despite previous failure
    expect(results[2].succeeded).toBe(false);
    expect(results[3].succeeded).toBe(true);  // Should succeed despite previous failure
    expect(results[4].succeeded).toBe(true);

    expect(daemon.isRunning).toBe(true);
    await daemon.stop();
  });
});
```

### Experiment 2: Memory Pressure Resilience

```javascript
describe('Chaos: Memory Pressure Resilience', () => {
  it('should handle memory pressure gracefully', async () => {
    const daemon = new Daemon({ daemonId: 'memory-chaos' });
    await daemon.start();

    // Schedule operations with large metadata
    const OPERATION_COUNT = 100;
    const METADATA_SIZE_KB = 10;

    for (let i = 0; i < OPERATION_COUNT; i++) {
      daemon.schedule({
        id: `mem-op-${i}`,
        metadata: {
          largeData: 'x'.repeat(METADATA_SIZE_KB * 1024),
        },
        handler: async () => ({ success: true }),
      });
    }

    // Execute subset
    for (let i = 0; i < 20; i++) {
      await daemon.execute(`mem-op-${i}`);
    }

    // Verify daemon is still responsive
    const health = daemon.getHealth();
    expect(health.isRunning).toBe(true);
    expect(health.queuedOperations).toBeGreaterThan(0);

    // New operations should still work
    daemon.schedule({
      id: 'post-pressure-op',
      handler: async () => ({ afterPressure: true }),
    });
    const result = await daemon.execute('post-pressure-op');
    expect(result.afterPressure).toBe(true);

    await daemon.stop();
  });
});
```

### Experiment 3: Concurrent Error Isolation

```javascript
describe('Chaos: Concurrent Error Isolation', () => {
  it('should isolate concurrent errors without affecting concurrent successes', async () => {
    const daemon = new Daemon({ daemonId: 'concurrent-chaos', concurrency: 20 });
    await daemon.start();

    // Schedule 20 operations: 10 will fail, 10 will succeed
    for (let i = 0; i < 20; i++) {
      const shouldFail = i % 2 === 0;
      daemon.schedule({
        id: `concurrent-${i}`,
        handler: shouldFail
          ? async () => { throw new Error('Concurrent chaos'); }
          : async () => ({ success: true, index: i }),
      });
    }

    // Execute all concurrently
    const results = await Promise.all(
      Array.from({ length: 20 }, (_, i) =>
        daemon.execute(`concurrent-${i}`)
          .then(result => ({ index: i, succeeded: true, result }))
          .catch(error => ({ index: i, succeeded: false, error: error.message }))
      )
    );

    const successes = results.filter(r => r.succeeded);
    const failures = results.filter(r => !r.succeeded);

    // Verify mix of results
    expect(successes.length).toBe(10);
    expect(failures.length).toBe(10);

    // Verify daemon health
    expect(daemon.isRunning).toBe(true);

    const metrics = daemon.getMetrics();
    expect(metrics.successfulOperations).toBe(10);
    expect(metrics.failedOperations).toBe(10);

    await daemon.stop();
  });
});
```

### Experiment 4: Leader Failover

```javascript
describe('Chaos: Leader Failover', () => {
  it('should handle leader failover gracefully', async () => {
    const { EventEmitter } = await import('events');
    const { integrateRaftNode } = await import('@unrdf/daemon');

    const daemon = new Daemon({ daemonId: 'failover-test', nodeId: 'node-1' });
    const mockRaft = new EventEmitter();
    integrateRaftNode(daemon, mockRaft);

    await daemon.start();

    // Initial leader election
    mockRaft.emit('leader_elected', { leaderId: 'node-1' });
    expect(daemon.isLeader).toBe(true);

    // Schedule operation as leader
    daemon.schedule({
      id: 'leader-op',
      handler: async () => ({ leaderOp: true }),
    });
    const result = await daemon.execute('leader-op');
    expect(result.leaderOp).toBe(true);

    // CHAOS: Lose leadership
    mockRaft.emit('leader_lost');
    expect(daemon.isLeader).toBe(false);

    // Daemon should still function (just not as leader)
    daemon.schedule({
      id: 'follower-op',
      handler: async () => ({ followerOp: true }),
    });
    const followerResult = await daemon.execute('follower-op');
    expect(followerResult.followerOp).toBe(true);

    // RECOVERY: New leader elected
    mockRaft.emit('leader_elected', { leaderId: 'node-2' });
    expect(daemon.isLeader).toBe(false);
    expect(daemon.currentLeaderId).toBe('node-2');

    expect(daemon.isRunning).toBe(true);
    await daemon.stop();
  });
});
```

---

## Advanced Patterns

### Game Day Simulation

```javascript
/**
 * Game Day: Coordinated Chaos Simulation
 * Run multiple chaos experiments in sequence with increasing severity
 */
async function runGameDay(daemon) {
  const gameDay = {
    name: 'Q1 2026 Game Day',
    startTime: Date.now(),
    experiments: [],
  };

  console.log('=== GAME DAY STARTED ===\n');

  // Phase 1: Operation failures (5 min)
  console.log('Phase 1: Operation Failures');
  for (let i = 0; i < 10; i++) {
    daemon.schedule({
      id: `gameday-fail-${i}`,
      handler: () => { throw new Error('Game Day failure'); },
    });
    try { await daemon.execute(`gameday-fail-${i}`); } catch (e) {}
  }
  gameDay.experiments.push({ phase: 1, type: 'operation_failures', health: daemon.getHealth() });

  // Phase 2: Memory pressure (5 min)
  console.log('Phase 2: Memory Pressure');
  for (let i = 0; i < 50; i++) {
    daemon.schedule({
      id: `gameday-mem-${i}`,
      metadata: { data: 'x'.repeat(50000) },
      handler: async () => ({ success: true }),
    });
    await daemon.execute(`gameday-mem-${i}`);
  }
  gameDay.experiments.push({ phase: 2, type: 'memory_pressure', health: daemon.getHealth() });

  // Phase 3: Concurrent load (5 min)
  console.log('Phase 3: Concurrent Load');
  const concurrentOps = [];
  for (let i = 0; i < 100; i++) {
    daemon.schedule({
      id: `gameday-concurrent-${i}`,
      handler: async () => {
        await new Promise(r => setTimeout(r, Math.random() * 1000));
        return { success: true };
      },
    });
    concurrentOps.push(daemon.execute(`gameday-concurrent-${i}`).catch(() => {}));
  }
  await Promise.all(concurrentOps);
  gameDay.experiments.push({ phase: 3, type: 'concurrent_load', health: daemon.getHealth() });

  // Final assessment
  gameDay.endTime = Date.now();
  gameDay.duration = gameDay.endTime - gameDay.startTime;
  gameDay.finalHealth = daemon.getHealth();
  gameDay.finalMetrics = daemon.getMetrics();
  gameDay.passed = daemon.isRunning && gameDay.finalMetrics.successRate > 50;

  console.log('\n=== GAME DAY COMPLETE ===');
  console.log(`Duration: ${(gameDay.duration / 1000 / 60).toFixed(2)} minutes`);
  console.log(`Result: ${gameDay.passed ? 'PASSED' : 'FAILED'}`);
  console.log(`Final Success Rate: ${gameDay.finalMetrics.successRate.toFixed(2)}%`);

  return gameDay;
}
```

### Chaos Monkey Integration

```javascript
/**
 * Chaos Monkey: Random Continuous Chaos
 * Continuously inject random failures at random intervals
 */
class ChaosMonkey {
  constructor(daemon, options = {}) {
    this.daemon = daemon;
    this.enabled = false;
    this.interval = options.interval || 5000;  // 5 seconds
    this.failureRate = options.failureRate || 0.1;  // 10%
    this.timer = null;
  }

  start() {
    this.enabled = true;
    this.timer = setInterval(() => this.maybeInjectChaos(), this.interval);
    console.log('Chaos Monkey started');
  }

  stop() {
    this.enabled = false;
    if (this.timer) {
      clearInterval(this.timer);
      this.timer = null;
    }
    console.log('Chaos Monkey stopped');
  }

  async maybeInjectChaos() {
    if (!this.enabled || Math.random() > this.failureRate) {
      return;
    }

    const chaosTypes = ['handler_error', 'slow_operation', 'large_payload'];
    const chaosType = chaosTypes[Math.floor(Math.random() * chaosTypes.length)];

    console.log(`Chaos Monkey: Injecting ${chaosType}`);

    switch (chaosType) {
      case 'handler_error':
        this.daemon.schedule({
          id: `monkey-${Date.now()}`,
          handler: () => { throw new Error('Chaos Monkey strike!'); },
        });
        try { await this.daemon.execute(`monkey-${Date.now()}`); } catch (e) {}
        break;

      case 'slow_operation':
        this.daemon.schedule({
          id: `monkey-slow-${Date.now()}`,
          handler: async () => {
            await new Promise(r => setTimeout(r, 5000));
            return { slow: true };
          },
        });
        break;

      case 'large_payload':
        this.daemon.schedule({
          id: `monkey-large-${Date.now()}`,
          metadata: { data: 'x'.repeat(100000) },
          handler: async () => ({ large: true }),
        });
        break;
    }
  }
}

// Usage
const monkey = new ChaosMonkey(daemon, { interval: 10000, failureRate: 0.2 });
monkey.start();
// ... run your tests ...
monkey.stop();
```

---

## References

- [UPTIME_SIMULATION.md](./UPTIME_SIMULATION.md) - Uptime testing
- [BENCHMARKING_GUIDE.md](./BENCHMARKING_GUIDE.md) - Performance benchmarks
- [error-path-scenarios.md](./error-path-scenarios.md) - Error handling scenarios
- [error-path-validation.md](./error-path-validation.md) - Error validation

---

**Document Version**: 1.0.0
**Last Updated**: 2026-01-18
**Target**: @unrdf/daemon v6.0.0+
