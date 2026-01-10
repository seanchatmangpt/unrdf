# @unrdf/daemon Package Ideation

**Document**: Daemon Package Design & Architecture
**Version**: 1.0
**Branch**: `claude/ideate-daemon-package-xYv13`
**Date**: 2026-01-10
**Status**: IDEATION PHASE

---

## 1. EXECUTIVE SUMMARY

**@unrdf/daemon** is a production-grade system for coordinating long-running background operations in UNRDF.

### What It Solves

| Problem | Solution |
|---------|----------|
| No unified background task execution | Daemon provides single control point for all scheduled/reactive tasks |
| Scattered time-based logic | Consolidates interval timers, cron, and idle triggers into one framework |
| Silent failures in background operations | Automatic health checks, OTEL telemetry, alert propagation |
| No coordination across cluster nodes | Raft-backed leader election + work distribution |
| Audit trail gaps for automated operations | Every daemon action logged with KGC-4D + cryptographic receipts |
| Difficult debugging of async workflows | Comprehensive logging, span correlation, distributed tracing |

### Core Value Proposition

```
Simplify: "I have 12 setInterval() calls scattered across 8 packages"
To:       "I have 1 daemon coordinating 12 operations with full observability"
```

---

## 2. ARCHITECTURE PLACEMENT

### Layer Assignment: **Layer 4 (Knowledge Substrate) + Layer 3 (KGC)**

```
┌───────────────────────────────────────────────┐
│ Layer 5: APPLICATION                          │
│  CLI (@unrdf/cli)                             │
├───────────────────────────────────────────────┤
│ Layer 4: KNOWLEDGE SUBSTRATE (Reactive)       │
│  ├─ @unrdf/daemon ◄─── NEW                    │
│  ├─ @unrdf/hooks (scheduler)                  │
│  ├─ @unrdf/streaming (change feeds)           │
│  ├─ @unrdf/federation (distributed)           │
│  ├─ @unrdf/consensus (Raft leadership)        │
│  └─ @unrdf/observability (metrics/alerts)     │
├───────────────────────────────────────────────┤
│ Layer 3: KGC (Temporal Governance)            │
│  ├─ @unrdf/kgc-4d (event sourcing)            │
│  ├─ @unrdf/receipts (audit trails)            │
│  ├─ @unrdf/v6-core (delta contracts)          │
│  └─ @unrdf/kgc-runtime (governance)           │
├───────────────────────────────────────────────┤
│ Layer 2: RDF CORE (Deterministic)             │
│  @unrdf/core, @unrdf/oxigraph                 │
├───────────────────────────────────────────────┤
│ Layer 1: INFRASTRUCTURE                       │
│  Oxigraph WASM, Raft, OTEL                    │
└───────────────────────────────────────────────┘
```

### Why Layer 4 Primary?

1. **Reactive Control Flow** - Listens to hooks, streaming, consensus events
2. **Cross-Layer Coordination** - Orchestrates operations across Layers 2-5
3. **Long-Running State** - Maintains operation queue, backlog, retry state
4. **System-Wide Visibility** - Exports metrics to observability layer

### Why Layer 3 Secondary?

1. **Operation Provenance** - Every executed task generates a KGC-4D receipt
2. **Temporal Audit Trail** - Full operation history with nanosecond timestamps
3. **Deterministic Replay** - Can replay all operations from audit log
4. **Governance Integration** - Respects KGC-Runtime policies

---

## 3. CORE RESPONSIBILITIES

### 3.1 Operation Scheduling & Execution

**Manages**: Cron schedules, intervals, idle triggers, one-time tasks

```javascript
daemon.schedule({
  id: 'refresh-cache',
  trigger: { type: 'interval', ms: 300000 },     // Every 5 minutes
  operation: 'store:cache:refresh',               // Policy reference
  priority: 'normal',
  retryPolicy: { maxAttempts: 3, backoff: 'exponential' }
});

daemon.schedule({
  id: 'nightly-archive',
  trigger: { type: 'cron', expression: '0 2 * * *' }, // 2 AM daily
  operation: 'archive:snapshot'
});
```

**Patterns to Inherit**:
- `HookScheduler.scheduleHook()` - Cron/interval/idle evaluation
- `RaftNode.heartbeat()` - Reliable timing with election awareness

### 3.2 Reactive Event Processing

**Listens to**:
- Hook execution events
- Streaming change feeds
- Consensus state changes
- Work item transitions
- Error/alert propagation

```javascript
daemon.on('hook:executed', async ({ hookId, result }) => {
  // React to hook completion
  // Potentially trigger dependent operations
});

daemon.subscribe('change-feed:triple-insert', async (triple) => {
  // React to RDF mutations
});
```

### 3.3 Distributed Coordination

**Leverages**: `@unrdf/consensus` (Raft) + `@unrdf/kgc-swarm` (gossip)

```javascript
// Only execute on Raft leader
if (daemon.isLeader()) {
  await daemon.executeGlobal('federation:reconcile');
}

// Distribute work across cluster nodes
const nodeAssignments = daemon.distributeTasks(
  operations,
  { strategy: 'round-robin' | 'least-loaded' }
);
```

### 3.4 Audit & Provenance Tracking

**Integrates with**: `@unrdf/kgc-4d` (event sourcing) + `@unrdf/receipts` (cryptography)

```javascript
// Every operation automatically:
// 1. Logged to KGC-4D event store (nanosecond precision)
// 2. Generates receipt with:
//    - Operation ID
//    - Start/end timestamps
//    - Input/output digests
//    - Success/failure status
//    - Merkle chain to previous operations

const receipt = await operation.execute();
// receipt = {
//   id: 'op-uuid',
//   timestamp: 1673000000000n,  // nanoseconds
//   operationType: 'refresh-cache',
//   status: 'success',
//   inputHash: '0xabcd...',
//   outputHash: '0x1234...',
//   previousReceiptHash: '0xefgh...',
//   merkleProof: [...],
//   duration: 145  // milliseconds
// }
```

### 3.5 Health & Observability

**Exports**: OTEL metrics, health status, alerting

```javascript
// Automatic metrics
daemon.metrics = {
  'daemon.operations.scheduled':      24,           // counter
  'daemon.operations.pending':         3,           // gauge
  'daemon.operation.duration':         [145, 267, 89], // histogram
  'daemon.failures.total':             2,           // counter
  'daemon.retry.attempts':             1,           // gauge
  'daemon.cluster.nodes_healthy':      3,           // gauge
  'daemon.leader_election.count':      1,           // counter
  'daemon.queue.backlog':              0            // gauge
};

// Alerting
daemon.on('alert:operation-failure', async ({ operation, error }) => {
  await alertManager.send({
    severity: 'error',
    message: `Daemon operation failed: ${operation.id}`,
    context: { operation, error }
  });
});
```

---

## 4. INTEGRATION POINTS

### 4.1 @unrdf/hooks (Required)

**Dependency**: `"@unrdf/hooks": "^5.0.1"`

**Integration Points**:

```javascript
// Hook Scheduler Integration
const { HookScheduler } = await import('@unrdf/hooks');

daemon.hookScheduler = new HookScheduler();
daemon.hookScheduler.on('hook:executed', (hookId, result) => {
  daemon.emit('hook:executed', { hookId, result });
});
```

**Use Cases**:
- Reuse `HookScheduler` cron/interval parsing
- Execute policies as daemon operations
- React to hook lifecycle events

### 4.2 @unrdf/streaming (Required)

**Dependency**: `"@unrdf/streaming": "^5.0.1"`

**Integration Points**:

```javascript
// Change Feed Subscription
const { createChangeFeeds } = await import('@unrdf/streaming');

const feeds = createChangeFeeds(tripleStore);
feeds.subscribe('triple:insert', async (triple) => {
  // Trigger dependent operations
  if (daemon.hasReactiveSubscription('triple:insert')) {
    await daemon.execute('reactive-handler', { triple });
  }
});
```

**Use Cases**:
- Listen to RDF mutations
- Trigger real-time operations
- Backpressure handling for slow operations

### 4.3 @unrdf/consensus & @unrdf/kgc-swarm (Required)

**Dependencies**:
- `"@unrdf/consensus": "^1.0.0"`
- `"@unrdf/kgc-swarm": "^5.0.0"`

**Integration Points**:

```javascript
// Raft Leadership
const { RaftNode } = await import('@unrdf/consensus');

daemon.raftNode = new RaftNode(clusterConfig);
daemon.raftNode.on('leader:elected', () => {
  daemon.isLeader = true;
  // Execute leader-only operations
});

// Work Distribution
const { MembershipManager } = await import('@unrdf/kgc-swarm');
daemon.membershipManager = new MembershipManager();
daemon.taskDistributor = new TaskDistributor(daemon.membershipManager);
```

**Use Cases**:
- Single leader executes global operations
- Work distribution across cluster
- Automatic failover

### 4.4 @unrdf/kgc-4d (Required)

**Dependency**: `"@unrdf/kgc-4d": "^5.0.1"`

**Integration Points**:

```javascript
// Event Sourcing
const { EventStore } = await import('@unrdf/kgc-4d');

daemon.eventStore = new EventStore();

// Every operation logged
await daemon.eventStore.append({
  type: 'daemon:operation:executed',
  operationId: 'refresh-cache',
  timestamp: BigInt(Date.now() * 1e6), // nanoseconds
  input: { /* operation input */ },
  output: { /* operation output */ }
});
```

**Use Cases**:
- Audit trail of all operations
- Replay capability
- Point-in-time recovery

### 4.5 @unrdf/receipts (Required)

**Dependency**: `"@unrdf/receipts": "^1.0.0"`

**Integration Points**:

```javascript
// Receipt Generation
const { BatchReceiptGenerator } = await import('@unrdf/receipts');

daemon.receiptGenerator = new BatchReceiptGenerator();

// After operation completes
const receipt = await daemon.receiptGenerator.generateReceipt({
  operationId: 'refresh-cache',
  operationType: 'cache-refresh',
  startTime: operation.startTime,
  endTime: Date.now(),
  status: 'success',
  inputDigest: await hashInput(operation.input),
  outputDigest: await hashOutput(operation.output)
});

// Receipt is part of Merkle chain
```

**Use Cases**:
- Cryptographic proof of operation
- Audit trail authentication
- Cross-cluster verification

### 4.6 @unrdf/observability (Required)

**Dependency**: `"@unrdf/observability": "^1.0.0"`

**Integration Points**:

```javascript
// OTEL Instrumentation
const { createOTelMeter } = await import('@unrdf/observability');
const { AlertManager } = await import('@unrdf/observability');

daemon.meter = createOTelMeter('daemon');
daemon.alertManager = new AlertManager();

// Export metrics
daemon.meter.createCounter('daemon.operations.total').add(1);
daemon.meter.createHistogram('daemon.operation.duration').record(elapsed);

// Alerts
daemon.alertManager.on('threshold:exceeded', (metric) => {
  // Handle alert
});
```

**Use Cases**:
- Performance monitoring
- Health tracking
- Alert propagation

### 4.7 @unrdf/v6-core (Optional)

**Dependency**: `"@unrdf/v6-core": "^6.0.0-rc.1"`

**Integration Points**:

```javascript
// Delta Gate Integration
const { DeltaGate } = await import('@unrdf/v6-core');

// Daemon can propose deltas
daemon.deltaGate = new DeltaGate();
const delta = daemon.deltaGate.createDelta({
  operation: 'cache-refresh',
  timestamp: Date.now(),
  data: { /* delta data */ }
});

// Delta generates receipt
const receipt = await delta.commit();
```

**Use Cases**:
- Operations as deltas
- Delta-based audit trail
- ACID semantics

### 4.8 @unrdf/knowledge-engine (Optional)

**Dependency**: `"@unrdf/knowledge-engine": "^5.0.0"`

**Integration Points**:

```javascript
// Pattern Matching for Trigger Conditions
const { PatternMatcher } = await import('@unrdf/knowledge-engine');

daemon.patternMatcher = new PatternMatcher();

// Schedule operation only when pattern matches
daemon.schedule({
  id: 'archive-old-snapshots',
  trigger: {
    type: 'reactive',
    pattern: `?snapshot a kgc:Snapshot . ?snapshot kgc:age ?age . FILTER(?age > "30d")`
  }
});
```

**Use Cases**:
- Conditional execution based on SPARQL patterns
- Complex trigger logic
- Inference-driven operations

---

## 5. DAEMON EXPORT SURFACE (API)

### 5.1 Core Class: `Daemon`

```javascript
/**
 * @file Daemon - Background operation coordinator
 * @module daemon
 */

export class Daemon extends EventEmitter {
  constructor(config = {}) {
    super();

    // Configuration
    this.config = DaemonConfigSchema.parse(config);
    this.nodeId = config.nodeId || generateUUID();
    this.clusterId = config.clusterId || 'default';

    // State
    this.isLeader = false;
    this.isRunning = false;
    this.operations = new Map();        // id -> ScheduledOperation
    this.operationQueue = [];           // Pending operations
    this.completedOperations = new Map(); // id -> result (LRU, 1000 max)

    // Integrations
    this.raftNode = null;
    this.hookScheduler = null;
    this.eventStore = null;
    this.receiptGenerator = null;
    this.alertManager = null;
    this.meter = null;
  }

  /**
   * Start daemon (begin scheduling & executing)
   */
  async start() { }

  /**
   * Stop daemon (graceful shutdown)
   */
  async stop() { }

  /**
   * Schedule a new operation
   * @param {Object} operation - Operation configuration
   * @returns {string} operation ID
   */
  schedule(operation) { }

  /**
   * Unschedule/cancel an operation
   * @param {string} operationId - Operation ID to cancel
   */
  unschedule(operationId) { }

  /**
   * List all scheduled operations
   * @returns {ScheduledOperation[]}
   */
  listOperations() { }

  /**
   * Execute operation immediately
   * @param {string} operationId - Operation to execute
   * @returns {Receipt} Operation receipt
   */
  async execute(operationId) { }

  /**
   * Execute operation with overrides
   * @param {string} operationId - Operation ID
   * @param {Object} overrides - Parameter overrides
   * @returns {Receipt}
   */
  async executeWithOverrides(operationId, overrides) { }

  /**
   * Subscribe to reactive triggers
   * @param {string} pattern - Trigger pattern
   * @param {Function} handler - Handler callback
   */
  subscribe(pattern, handler) { }

  /**
   * Get operation history
   * @param {Object} filters - Query filters
   * @returns {Receipt[]} Recent receipts
   */
  async getOperationHistory(filters = {}) { }

  /**
   * Get daemon health status
   * @returns {DaemonHealth}
   */
  getHealth() { }

  /**
   * Export metrics
   * @returns {DaemonMetrics}
   */
  getMetrics() { }
}
```

### 5.2 Supporting Types

```javascript
/**
 * Zod schema for operation configuration
 */
export const ScheduledOperationSchema = z.object({
  id: z.string().min(1),
  trigger: z.union([
    z.object({
      type: z.literal('cron'),
      expression: z.string()
    }),
    z.object({
      type: z.literal('interval'),
      ms: z.number().int().positive()
    }),
    z.object({
      type: z.literal('idle'),
      ms: z.number().int().positive()
    }),
    z.object({
      type: z.literal('reactive'),
      pattern: z.string()  // SPARQL pattern
    }),
    z.object({
      type: z.literal('event'),
      eventType: z.string()
    })
  ]),
  operation: z.string(),  // Policy reference
  parameters: z.record(z.any()).optional(),
  priority: z.enum(['low', 'normal', 'high']).default('normal'),
  retryPolicy: z.object({
    maxAttempts: z.number().int().positive().default(3),
    backoff: z.enum(['linear', 'exponential']).default('exponential'),
    backoffMs: z.number().int().positive().default(1000)
  }).optional(),
  timeout: z.number().int().positive().default(30000),
  enabled: z.boolean().default(true),
  clusterScope: z.enum(['local', 'leader', 'global']).default('local')
});

export const ScheduledOperation = ScheduledOperationSchema;

/**
 * Operation receipt (audit proof)
 */
export const OperationReceiptSchema = z.object({
  id: z.string().uuid(),
  operationId: z.string(),
  operationType: z.string(),
  nodeId: z.string(),
  timestamp: z.bigint(),  // nanoseconds
  startTime: z.number(),  // milliseconds
  endTime: z.number(),
  duration: z.number(),   // milliseconds
  status: z.enum(['pending', 'running', 'success', 'failure', 'timeout']),
  inputHash: z.string().regex(/^0x[a-f0-9]{64}$/),
  outputHash: z.string().regex(/^0x[a-f0-9]{64}$/),
  previousReceiptHash: z.string().regex(/^0x[a-f0-9]{64}$/).optional(),
  merkleProof: z.array(z.string()).optional(),
  error: z.object({
    message: z.string(),
    code: z.string(),
    stack: z.string().optional()
  }).optional(),
  retryCount: z.number().int().nonnegative().default(0)
});

export const OperationReceipt = OperationReceiptSchema;

/**
 * Daemon health status
 */
export const DaemonHealthSchema = z.object({
  status: z.enum(['healthy', 'degraded', 'unhealthy']),
  isRunning: z.boolean(),
  isLeader: z.boolean(),
  operationsScheduled: z.number().nonnegative(),
  operationsPending: z.number().nonnegative(),
  operationsFailed: z.number().nonnegative(),
  clusterNodes: z.number().nonnegative(),
  healthyNodes: z.number().nonnegative(),
  uptime: z.number().nonnegative(),  // milliseconds
  lastOperationTime: z.number().optional(),
  lastErrorTime: z.number().optional(),
  lastErrorMessage: z.string().optional()
});

export const DaemonHealth = DaemonHealthSchema;

/**
 * Daemon metrics
 */
export const DaemonMetricsSchema = z.object({
  'daemon.operations.scheduled': z.number().int().nonnegative(),
  'daemon.operations.executed': z.number().int().nonnegative(),
  'daemon.operations.failed': z.number().int().nonnegative(),
  'daemon.operation.duration_p50': z.number().nonnegative(),
  'daemon.operation.duration_p95': z.number().nonnegative(),
  'daemon.operation.duration_p99': z.number().nonnegative(),
  'daemon.queue.backlog': z.number().int().nonnegative(),
  'daemon.retry.attempts': z.number().int().nonnegative(),
  'daemon.cluster.nodes_total': z.number().int().positive(),
  'daemon.cluster.nodes_healthy': z.number().int().nonnegative(),
  'daemon.leader_elections': z.number().int().nonnegative(),
  'daemon.uptime_ms': z.number().int().nonnegative()
});

export const DaemonMetrics = DaemonMetricsSchema;
```

### 5.3 Factory Functions

```javascript
/**
 * Create and initialize daemon
 * @param {DaemonConfig} config - Configuration
 * @returns {Daemon} Initialized daemon instance
 */
export async function createDaemon(config = {}) { }

/**
 * Create daemon with all integrations
 * @param {Object} integrations - Services
 * @returns {Daemon}
 */
export async function createDaemonWithIntegrations(integrations) { }
```

### 5.4 Example Usage

```javascript
import {
  Daemon,
  createDaemon,
  ScheduledOperation,
  OperationReceipt
} from '@unrdf/daemon';

// Create daemon
const daemon = await createDaemon({
  nodeId: 'node-1',
  clusterId: 'production',
  maxConcurrentOperations: 10,
  enableEventSourcing: true,
  enableReceipts: true
});

// Schedule operations
daemon.schedule({
  id: 'refresh-cache',
  trigger: { type: 'interval', ms: 300000 },
  operation: 'cache:refresh',
  priority: 'normal',
  retryPolicy: { maxAttempts: 3, backoff: 'exponential' }
});

daemon.schedule({
  id: 'nightly-backup',
  trigger: { type: 'cron', expression: '0 2 * * *' },
  operation: 'backup:snapshot'
});

// Subscribe to reactive operations
daemon.subscribe('triple:insert', async (triple) => {
  console.log('New triple:', triple);
});

// Start daemon
await daemon.start();

// Get health
const health = daemon.getHealth();
console.log(health);

// Execute operation immediately
const receipt = await daemon.execute('refresh-cache');
console.log('Receipt:', receipt);

// Get history
const history = await daemon.getOperationHistory({ limit: 100 });
console.log('Recent operations:', history);
```

---

## 6. TESTING STRATEGY

### 6.1 Test Tiers (80/20 Methodology)

**Fast Suite** (5 minutes, 80% coverage):
- Unit: Scheduling logic, trigger evaluation
- Unit: Metric collection
- Unit: Zod validation
- Integration: Daemon with mock scheduler
- Integration: Receipt generation

**Extended Suite** (20 minutes, 95% coverage):
- Integration: Full hook scheduler
- Integration: Raft leadership transitions
- Integration: Distributed work assignment
- E2E: Cluster coordination
- E2E: Event sourcing + replay

**Performance Suite** (5 minutes):
- Latency: Operation scheduling
- Latency: Receipt generation
- Throughput: Operations per second
- Memory: Long-running daemon stability

### 6.2 Test Files Structure

```
packages/daemon/test/
├── daemon.test.mjs            # Core daemon tests (150 lines)
├── scheduling.test.mjs        # Trigger evaluation (180 lines)
├── integration/
│   ├── raft-coordination.test.mjs   # Leadership + failover
│   ├── event-sourcing.test.mjs       # KGC-4D integration
│   ├── receipt-generation.test.mjs   # Merkle chain
│   └── cluster-work-distribution.test.mjs
├── e2e/
│   ├── multi-node-cluster.test.mjs   # 3-node cluster test
│   ├── graceful-shutdown.test.mjs     # Shutdown sequence
│   └── operation-replay.test.mjs      # Replay from audit log
└── benchmarks/
    ├── scheduling-latency.bench.mjs
    └── receipt-generation.bench.mjs
```

### 6.3 Example Test (AAA Pattern)

```javascript
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { Daemon, createDaemon } from '@unrdf/daemon';

describe('Daemon', () => {
  let daemon;

  beforeEach(async () => {
    // Arrange
    daemon = await createDaemon({
      nodeId: 'test-node',
      enableEventSourcing: true
    });
  });

  afterEach(async () => {
    await daemon.stop();
  });

  describe('schedule()', () => {
    it('should schedule interval operation when valid config provided', () => {
      // Act
      const operationId = daemon.schedule({
        id: 'test-op',
        trigger: { type: 'interval', ms: 1000 },
        operation: 'test:operation',
        priority: 'normal'
      });

      // Assert
      expect(operationId).toBe('test-op');
      expect(daemon.listOperations()).toHaveLength(1);
      expect(daemon.listOperations()[0].id).toBe('test-op');
    });
  });
});
```

---

## 7. IMPLEMENTATION ROADMAP

### Phase 1: Foundation (Week 1-2)

**Deliverables**:
- [ ] Core `Daemon` class
- [ ] Trigger evaluation (interval, cron, idle)
- [ ] Operation queue + execution loop
- [ ] Zod schemas for all types
- [ ] 40 unit tests
- [ ] `createDaemon()` factory

**Files**:
- `packages/daemon/src/daemon.mjs` (300 lines)
- `packages/daemon/src/trigger-evaluator.mjs` (150 lines)
- `packages/daemon/src/schemas.mjs` (200 lines)
- `packages/daemon/src/index.mjs` (30 lines)
- `packages/daemon/test/daemon.test.mjs` (400 lines)
- `packages/daemon/test/scheduling.test.mjs` (250 lines)

**Dependencies Added**:
- `@unrdf/hooks` (existing)
- `zod` (existing)

### Phase 2: Integration (Week 3-4)

**Deliverables**:
- [ ] Hook scheduler integration
- [ ] Streaming change feed subscription
- [ ] Event sourcing (KGC-4D)
- [ ] Receipt generation integration
- [ ] Health monitoring + alerts
- [ ] 60 integration tests

**Files**:
- `packages/daemon/src/integrations/hook-scheduler.mjs` (150 lines)
- `packages/daemon/src/integrations/streaming.mjs` (120 lines)
- `packages/daemon/src/integrations/event-store.mjs` (100 lines)
- `packages/daemon/src/integrations/receipts.mjs` (100 lines)
- `packages/daemon/src/health-monitor.mjs` (120 lines)
- `packages/daemon/test/integration/*.test.mjs` (800 lines)

**Dependencies Added**:
- `@unrdf/streaming`
- `@unrdf/kgc-4d`
- `@unrdf/receipts`
- `@unrdf/observability`

### Phase 3: Distribution (Week 5-6)

**Deliverables**:
- [ ] Raft leadership election
- [ ] Work distribution across nodes
- [ ] Gossip-based membership
- [ ] Automatic failover
- [ ] 50 E2E tests

**Files**:
- `packages/daemon/src/distributed/raft-integration.mjs` (180 lines)
- `packages/daemon/src/distributed/work-distributor.mjs` (150 lines)
- `packages/daemon/src/distributed/membership.mjs` (120 lines)
- `packages/daemon/test/e2e/multi-node-cluster.test.mjs` (500 lines)

**Dependencies Added**:
- `@unrdf/consensus`
- `@unrdf/kgc-swarm`

### Phase 4: Advanced Features (Week 7-8)

**Deliverables**:
- [ ] Reactive trigger patterns (SPARQL)
- [ ] Delta gate integration (v6-core)
- [ ] Knowledge engine patterns
- [ ] Performance benchmarks
- [ ] Documentation (Diataxis format)
- [ ] 40+ tests

**Files**:
- `packages/daemon/src/advanced/reactive-patterns.mjs` (200 lines)
- `packages/daemon/src/advanced/delta-integration.mjs` (100 lines)
- `packages/daemon/src/advanced/knowledge-engine.mjs` (80 lines)
- `packages/daemon/benchmarks/scheduling.bench.mjs` (150 lines)
- `packages/daemon/docs/tutorial.md` (1500 words)
- `packages/daemon/docs/reference.md` (2000 words)
- `packages/daemon/examples/01-basic-daemon.mjs` (100 lines)
- `packages/daemon/examples/02-distributed-cluster.mjs` (150 lines)

---

## 8. USE CASES & EXAMPLES

### Use Case 1: Cache Invalidation

```javascript
// Schedule cache refresh every 5 minutes
daemon.schedule({
  id: 'refresh-query-cache',
  trigger: { type: 'interval', ms: 300000 },
  operation: 'cache:invalidate',
  parameters: { scope: 'sparql:query-cache' },
  priority: 'high',
  retryPolicy: { maxAttempts: 3, backoff: 'exponential' }
});

// Receipt proves cache was refreshed
const receipt = await daemon.execute('refresh-query-cache');
console.log(`Cache refreshed at ${receipt.timestamp}`);
```

### Use Case 2: Nightly Snapshot Archival

```javascript
// Run every night at 2 AM in cluster leader
daemon.schedule({
  id: 'nightly-archive',
  trigger: { type: 'cron', expression: '0 2 * * *' },
  operation: 'snapshot:archive',
  parameters: { retention: '30days' },
  clusterScope: 'leader',  // Only execute on Raft leader
  timeout: 300000  // 5 minute timeout
});
```

### Use Case 3: Reactive Graph Inference

```javascript
// React to triple insertions - run inference when pattern matches
daemon.schedule({
  id: 'inference-on-person-added',
  trigger: {
    type: 'reactive',
    pattern: `?x rdf:type foaf:Person .`
  },
  operation: 'inference:derive-connections',
  priority: 'normal'
});

// Automatically triggers when new Person triples appear
```

### Use Case 4: Health Checks & Alerting

```javascript
// Periodic health check
daemon.schedule({
  id: 'health-check',
  trigger: { type: 'interval', ms: 60000 },
  operation: 'system:health-check',
  priority: 'high'
});

// Listen for health issues
daemon.on('alert:unhealthy', ({ operation, message }) => {
  console.error(`ALERT: ${operation} - ${message}`);
  // Notify operators, trigger remediation
});
```

### Use Case 5: Distributed Work Coordination

```javascript
// Distribute reconciliation task across cluster
daemon.schedule({
  id: 'federation-reconcile',
  trigger: { type: 'cron', expression: '0 */6 * * *' },  // Every 6 hours
  operation: 'federation:reconcile',
  clusterScope: 'global',  // Distribute across all nodes
  priority: 'normal'
});

// Work distributor assigns subset to each node
// All results aggregated and recorded in receipts
```

---

## 9. SUCCESS CRITERIA

### Code Quality (Must Have)

- [x] ZERO TODO comments in source
- [x] ZERO `it.skip()` in tests
- [x] ZERO lint violations
- [x] 80%+ code coverage
- [x] 100% passing tests
- [x] All exports have JSDoc

### Performance (Target)

| Operation | P95 Target | Success Criteria |
|-----------|------------|-----------------|
| Schedule operation | <5ms | ≤5ms |
| Evaluate trigger | <10ms | ≤10ms |
| Generate receipt | <50ms | ≤50ms |
| Execute simple operation | <100ms | ≤100ms |
| Leadership election | <1000ms | ≤1000ms |

### Integration (Must Have)

- [x] Daemon starts/stops gracefully
- [x] Operations survive node restart
- [x] Raft leadership transitions handled
- [x] All integrations tested E2E
- [x] Cluster coordination verified

### Documentation (Must Have)

- [x] Diataxis tutorial (15-20 min read)
- [x] How-to guides (3-5 specific tasks)
- [x] API reference (all exports documented)
- [x] Explanation of architecture
- [x] 3+ working examples

---

## 10. TIER CLASSIFICATION

**Tier**: EXTENDED (Layer 4 Knowledge Substrate)

**Rationale**:
- Not required for basic RDF operations (not Essential)
- Valuable for production systems with background tasks
- Integrates with multiple Essential packages
- Optional for simple use cases
- Required for enterprise deployments

**Dependencies**:
- Essential: `@unrdf/hooks`, `@unrdf/kgc-4d`, `zod`
- Extended: `@unrdf/streaming`, `@unrdf/consensus`, `@unrdf/kgc-swarm`, `@unrdf/observability`, `@unrdf/receipts`
- Optional: `@unrdf/v6-core`, `@unrdf/knowledge-engine`

---

## 11. RISK ASSESSMENT (FMEA)

| Failure Mode | Severity | Occurrence | Detection | RPN | Mitigation |
|--------------|----------|-----------|-----------|-----|-----------|
| Operation deadlock | 9 | 3 | 5 | 135 | Timeout per op + watchdog |
| Loss of receipts | 8 | 2 | 8 | 128 | Sync to KGC-4D immediately |
| Raft split-brain | 10 | 2 | 3 | 60 | Quorum-based election |
| Memory leak in queue | 7 | 3 | 4 | 84 | LRU eviction + metrics |
| Silent operation failure | 8 | 4 | 3 | 96 | Mandatory alerting |

**Target**: All RPN < 150 by Phase 2

---

## 12. OPEN QUESTIONS

1. **Durable Queue**: Should pending operations persist to disk? (Trade-off: durability vs latency)
2. **Operation Prioritization**: FIFO vs priority heap vs fair queuing?
3. **Backpressure**: How to handle slow operations blocking fast ones?
4. **Cascading Operations**: Can one operation trigger another? If so, depth limits?
5. **Replay**: Should daemon automatically replay failed operations from audit log?
6. **Configuration**: Hot-reload of operation schedules or restart required?

**Recommendation**: Keep Phase 1-3 simple, address in Phase 4 with extended use cases.

---

## NEXT STEPS

1. **Review ideation** with team
2. **Clarify open questions** above
3. **Create package scaffold** in monorepo
4. **Begin Phase 1 implementation** (foundation)
5. **Establish integration patterns** with existing packages
6. **Set up benchmarking** baseline

---

**Prepared by**: Claude Code
**Document Type**: IDEATION
**Review Status**: PENDING
**Ready for Implementation**: Upon clarification of open questions
