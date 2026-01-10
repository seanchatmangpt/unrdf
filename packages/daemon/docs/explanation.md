# Explanation: Understanding @unrdf/daemon

Conceptual deep-dive into daemon architecture, design decisions, and integration patterns.

## What is a Daemon?

A **daemon** is a background service that:
1. **Runs continuously** in the background
2. **Schedules work** based on triggers (time, events, conditions)
3. **Executes operations** at appropriate times
4. **Reports status** through events and metrics
5. **Handles failures** with retries and graceful degradation

Think of it as a **programmable scheduler** that connects events to actions.

### Real-World Analogy

```
Traditional approach:
┌──────────────────┐
│ Main Application │ ← Must handle all tasks
├──────────────────┤
│ Process data     │
│ Send emails      │
│ Clean cache      │
│ Generate reports │
└──────────────────┘

Daemon approach:
┌──────────────────┐      ┌─────────────┐
│ Main Application │ ───→ │ Daemon      │
│                  │      │             │
│ (Fast, focused)  │      │ Process data│
│                  │      │ Send emails │
│                  │      │ Clean cache │
│                  │      │ Gen reports │
└──────────────────┘      └─────────────┘
```

---

## Architecture

### 5-Layer Design

```
┌─────────────────────────────────────────┐
│ Application Layer                       │
│ (Your code scheduling operations)       │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│ Daemon Core                             │
│ • Event handling                        │
│ • Operation queuing                     │
│ • Lifecycle management                  │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│ Trigger Evaluation                      │
│ • Cron expressions                      │
│ • Interval calculations                 │
│ • Event matching                        │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│ Integrations Layer                      │
│ • Distributed (Raft)                    │
│ • Event sourcing (KGC-4D)               │
│ • Streaming (Change feeds)              │
│ • Hooks (Policy execution)              │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│ Storage & Runtime                       │
│ • Operation cache (LRU)                 │
│ • Receipt generation                    │
│ • Logger integration                    │
└─────────────────────────────────────────┘
```

---

## Core Concepts

### 1. Operations

An **operation** is a named, repeatable unit of work.

```javascript
{
  id: 'backup-database',           // Unique identifier
  name: 'Daily Database Backup',   // Human-readable name
  handler: async () => { ... },    // The work to do
  metadata: { /* context */ }       // Custom data
}
```

**Key Characteristics:**
- **Idempotent** - Can be run multiple times safely
- **Stateless** - No shared state between runs
- **Async-first** - Must return a Promise
- **Isolated** - Failures don't affect daemon

### 2. Triggers

A **trigger** determines *when* an operation should execute.

**Trigger Types:**

| Type | Use Case | Example |
|------|----------|---------|
| `interval` | Periodic work | Every 5 minutes |
| `cron` | Time-based schedules | Daily at 2 AM |
| `idle` | Background cleanup | When daemon idle >1 hour |
| `reactive` | Data-driven | When entity created |
| `event` | Custom events | When event emitted |

**Decision Tree:**
```
Do I know exact times?
  → Yes, use CRON (daily 2 AM)
  → No, proceed...

Is it every N milliseconds?
  → Yes, use INTERVAL (every 5000ms)
  → No, proceed...

Should it run when something happens?
  → Yes, use REACTIVE or EVENT
  → No, proceed...

Should it run when daemon is idle?
  → Yes, use IDLE
```

### 3. Events

**Events** are the communication mechanism between daemon and your application.

```
Schedule Operation → enqueued event
Start Execution    → started event
Success Result     → success event
Failure Error      → failure event
```

**Event Flow:**

```javascript
daemon.on('operation:enqueued', (event) => {
  // Operation queued, not yet running
});

daemon.on('operation:started', (event) => {
  // Operation execution started now
});

// ... operation runs ...

daemon.on('operation:success', (event) => {
  // Operation completed with result
  // Duration is measured
});

// OR

daemon.on('operation:failure', (event) => {
  // Operation failed with error
  // Duration is measured
  // Re-run retry logic
});
```

---

## Design Decisions

### Why EventEmitter Instead of Promises?

```javascript
// ❌ Promise-based (limited)
const result = await daemon.execute('op-id');
// Only know about this specific execution

// ✅ EventEmitter-based (flexible)
daemon.on('operation:success', (event) => {
  // Notified of ALL successes
  // Can trigger other operations
  // Can aggregate metrics
  // Can log/monitor easily
});
```

**Benefits:**
- Loosely coupled (daemon doesn't know what listens)
- Multi-listener support (multiple handlers)
- Easy event chaining (trigger operations from results)
- Natural fit for background processing

### Why LRU Cache for Completed Operations?

```javascript
// Bounded memory usage
const cache = new LRUCache(1000);
// Keeps 1000 most recent completions
// Automatically evicts oldest when full

// Trade-off:
// - Get fast access to recent execution history
// - Lose old history (acceptable for metrics)
```

**Alternatives Considered:**
- Persistent database → Too heavyweight for background ops
- Unbounded array → Memory leak risk
- No history → Can't debug failures

**Result:** LRU cache provides "just enough" history.

### Why Zod Schemas?

```javascript
// At runtime, validate all inputs/outputs
const config = DaemonConfigSchema.parse(userInput);
// Throws immediately if invalid

// Benefits:
// - Catch bugs at boundaries
// - Type information for IDEs
// - Self-documenting schemas
// - Security (validate untrusted input)
```

---

## Clustering & Distribution

### Single-Node Daemon

```
┌─────────────────────────────┐
│ Node 1: Daemon              │
│ • Schedule ops              │
│ • Execute immediately       │
│ • Track state               │
└─────────────────────────────┘
```

**Use When:**
- ✓ Single machine deployment
- ✓ Non-critical background tasks
- ✓ Development/testing

### Clustered Daemon with Raft

```
┌─────────────────────────────────────────┐
│ Cluster: "production-cluster"           │
├──────────────┬──────────────┬───────────┤
│ Node 1       │ Node 2       │ Node 3    │
│ (LEADER)     │ (Follower)   │ (Follower)│
├──────────────┼──────────────┼───────────┤
│ • Execute    │ • Standby    │ • Standby │
│   global ops │ • Replicate  │ • Replicate│
│ • Replicate  │   leader ops │   state   │
│   state to   │ • Monitor    │ • Elect   │
│   followers  │   leader     │   new     │
└──────────────┴──────────────┴───────────┘
         ↓                          ↓
      Raft Consensus           Automatic
      (leader election)        Failover
```

**Use When:**
- ✓ High availability required
- ✓ No single point of failure
- ✓ Multiple machines available
- ✓ Leader election for global ops

### Operation Scope in Clusters

```
┌──────────────────────────────────────┐
│ Operation Scope Configuration        │
├──────────────────────────────────────┤
│ 'local'  → Run on current node      │
│ 'leader' → Run only on cluster leader
│ 'global' → Distribute across cluster │
└──────────────────────────────────────┘
```

**Example Distribution Strategies:**
- **Round-Robin** - Distribute evenly across nodes
- **Least-Loaded** - Send to node with fewer active ops
- **Hash-Based** - Same op always goes to same node

---

## Integration Patterns

### With KGC-4D (Event Sourcing)

```javascript
// Every operation execution generates a receipt
operation executed → receipt created → merkle hash → chain stored

// Benefits:
// - Full audit trail
// - Cryptographic proof of execution
// - Time-travel queries (replay history)
// - Compliance friendly (immutable records)
```

### With Hooks (Policy Execution)

```javascript
// Daemon triggers policy execution
trigger fired → operation handler called → hook evaluated → action taken

// Use case:
// Schedule a data processing job
// Job completion triggers cleanup hook
// Cleanup hook validates retention policy
```

### With Streaming (Change Feeds)

```javascript
// Operations can react to data changes
data changed → stream event → trigger evaluation → operation queues

// Use case:
// When important entity is created
// Automatically send notification
// Create audit log entry
// Update search index
```

---

## Performance Characteristics

### Latency

```
Operation Execution Latency: P50-P99
├─ Schedule: <1ms   (add to queue)
├─ Queue wait: varies (depends on load)
├─ Execute: depends on handler (1ms-minutes)
└─ Event emit: <1ms (publish event)

Total: Dominated by handler execution
```

### Throughput

```
Max Concurrent: configurable (default 5-10)
Example: 10 concurrent × 10 ops/sec = 100 ops/sec

Factors:
• Handler duration
• Number of daemons
• Network (for distributed)
• CPU cores
```

### Memory

```
Per Daemon Instance: ~50-100 KB
Per Scheduled Operation: ~5-10 KB
Per Completed Operation (cache): ~1-2 KB

LRU Cache (1000 entries):
├─ Keep 1000 most recent
├─ Auto-evict oldest
└─ Bounded memory growth
```

---

## When to Use Daemon

### ✅ Perfect For:
- Periodic data processing (ETL, reports)
- Scheduled maintenance (cleanup, optimization)
- Event-driven workflows (email on signup)
- Background notifications (reminders, alerts)
- Distributed coordination (leader-based tasks)
- Audit logging and compliance

### ⚠️ Consider Alternatives:
- **Worker Queue** - If >1000 items/sec needed
- **Cron Jobs** - If no clustering needed
- **Webhooks** - If external event triggering
- **In-Process** - If <100ms latency critical

---

## Best Practices

### 1. Keep Handlers Pure and Fast

```javascript
// ✅ GOOD - Fast, focused, pure
{
  id: 'send-email',
  handler: async () => {
    const users = await db.query('SELECT * FROM inactive_users');
    await emailService.send(users);
    return { sent: users.length };
  }
}

// ❌ BAD - Slow, has side effects
{
  id: 'do-everything',
  handler: async () => {
    // Queries db
    // Updates cache
    // Calls 3 APIs
    // Writes log files
    // Modifies global state
    // Takes 5+ minutes
  }
}
```

### 2. Implement Idempotency

```javascript
// ✅ GOOD - Can run multiple times safely
{
  id: 'generate-report',
  handler: async () => {
    // Check if already generated today
    const exists = await db.findOne({
      date: today,
      type: 'daily-report'
    });
    if (exists) return { status: 'skipped' };

    // Generate and store
    const report = await generateReport();
    await db.insert({ ...report, date: today });
    return { status: 'generated' };
  }
}
```

### 3. Monitor Health Continuously

```javascript
// Set up health checks
setInterval(() => {
  const health = daemon.getHealth();
  const metrics = daemon.getMetrics();

  if (!health.isRunning) {
    sendAlert('Daemon stopped');
  }
  if (metrics.failureRate > 0.1) {
    sendAlert('High failure rate');
  }
}, 30000);
```

### 4. Use Proper Logging

```javascript
// ✅ GOOD - Structured logging
daemon.on('operation:failure', (event) => {
  logger.error('operation_failed', {
    operationId: event.operationId,
    error: event.error,
    duration: event.duration,
    timestamp: event.timestamp,
  });
});

// ❌ BAD - Unstructured
daemon.on('operation:failure', (event) => {
  console.log('Error: ' + event.error);
});
```

---

## Architecture Decision Records

### ADR-1: EventEmitter vs Promise-Based API

**Decision:** Use EventEmitter pattern

**Rationale:**
- Background processing is inherently async and event-driven
- Single-operation promises insufficient for monitoring multiple operations
- Enables event chaining (success triggers next operation)
- Natural fit for background task systems

**Trade-offs:**
- More verbose setup code
- Requires listener management
- Better flexibility and extensibility

### ADR-2: LRU Cache vs Persistent Storage

**Decision:** LRU in-memory cache for recent completions

**Rationale:**
- Bounded memory usage (no unbounded growth)
- Fast metrics retrieval (sub-millisecond)
- Sufficient for health monitoring and debugging
- Avoid database complexity for background metadata

**Trade-offs:**
- Lost history on restart (acceptable)
- No long-term audit trail (use KGC-4D for that)
- Limited query capabilities (but metrics sufficient)

### ADR-3: Zod Runtime Validation

**Decision:** Validate all operations with Zod schemas

**Rationale:**
- Catch configuration errors at runtime
- Type hints for IDE autocomplete
- Self-documenting API contracts
- Security boundary protection

**Trade-offs:**
- Small performance overhead (<1ms per validation)
- Requires schema maintenance
- Trade CPU for data integrity

---

## Conclusion

@unrdf/daemon provides a clean, event-driven abstraction for background task management. By separating concerns (scheduling, execution, monitoring), it enables:

- **Scalability** - Add nodes to cluster without changing code
- **Reliability** - Automatic failover and retry mechanisms
- **Observability** - Rich event stream and metrics
- **Maintainability** - Clear operation definitions and contracts
