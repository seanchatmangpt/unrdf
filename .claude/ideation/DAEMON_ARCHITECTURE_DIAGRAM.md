# @unrdf/daemon - Architecture Diagram

## System Architecture

```
┌────────────────────────────────────────────────────────────────────────┐
│                          DAEMON OPERATIONS                              │
├────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐                 │
│  │ INTERVAL     │  │ CRON         │  │ IDLE         │                 │
│  │ (every 5m)   │  │ (0 2 * * *)  │  │ (30s no ops) │                 │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘                 │
│         │                 │                 │                          │
│         └─────────────────┼─────────────────┘                          │
│                           │                                             │
│                    ┌──────▼──────┐                                      │
│                    │ TRIGGER     │                                      │
│                    │ EVALUATOR   │                                      │
│                    └──────┬──────┘                                      │
│                           │                                             │
│              ┌────────────┼────────────┐                                │
│              │            │            │                               │
│         ┌────▼────┐  ┌────▼────┐  ┌───▼────┐                          │
│         │ QUEUE   │  │ EXECUTE  │  │ RETRY  │                          │
│         │ MANAGER │  │ LOOP     │  │ LOGIC  │                          │
│         └────┬────┘  └────┬────┘  └───┬────┘                          │
│              │            │           │                                │
│              └────────────┼───────────┘                                │
│                           │                                             │
│                    ┌──────▼──────┐                                      │
│                    │ OPERATION   │                                      │
│                    │ EXECUTION   │                                      │
│                    └──────┬──────┘                                      │
│                           │                                             │
└───────────────────────────┼───────────────────────────────────────────┘
                            │
                            │ (Emit event)
                            │
        ┌───────────────────┼───────────────────┐
        │                   │                   │
        │                   │                   │


┌───────────────────────────────────────────────────────────────────────────┐
│                      INTEGRATION LAYER                                    │
├───────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐       │
│  │  @unrdf/hooks    │  │ @unrdf/streaming │  │ @unrdf/consensus │       │
│  │  (HookScheduler) │  │ (ChangeFeed)     │  │ (RaftNode)       │       │
│  └─────────────┬────┘  └────────────┬─────┘  └────────────┬─────┘       │
│                │                    │                     │              │
│                └────────┬───────────┴───────────┬─────────┘              │
│                         │                       │                        │
│  ┌──────────────────┐  ┌─┴──────────────────┐  │                        │
│  │ @unrdf/kgc-4d    │  │    DAEMON          │  │                        │
│  │ (EventStore)     │  │ Orchestrator       │  │                        │
│  └────────────┬─────┘  └────────┬───────────┘  │                        │
│               │                 │              │                        │
│               │       ┌─────────┴──────────────┘                        │
│               │       │                                                 │
│  ┌────────────▼──────┐  ┌──────────────────┐  ┌──────────────────┐     │
│  │ @unrdf/receipts   │  │ @unrdf/kgc-swarm │  │ @unrdf/observ.   │     │
│  │ (ReceiptGen)      │  │ (TaskDistrib)    │  │ (AlertManager)   │     │
│  └───────────────────┘  └──────────────────┘  └──────────────────┘     │
│                                                                            │
└───────────────────────────────────────────────────────────────────────────┘
                                    ▲
                                    │
                                    │


┌────────────────────────────────────────────────────────────────────────┐
│                    AUDIT & OBSERVABILITY LAYER                         │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  Event Store              Receipts                 Metrics             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────┐            │
│  │ timestamp    │  │ receiptId    │  │ daemon.ops.total │            │
│  │ operationId  │  │ opHash       │  │ daemon.errors    │            │
│  │ status       │  │ merkleProof  │  │ daemon.latency   │            │
│  │ input/output │  │ signature    │  │ daemon.queue     │            │
│  └──────────────┘  └──────────────┘  └──────────────────┘            │
│                                                                         │
│  ◄─── Permanent audit trail ────►  ◄─── Observable metrics ───►       │
│                                                                         │
└────────────────────────────────────────────────────────────────────────┘
                                    ▲
                                    │
                          (Logged to KGC-4D)
                          (Receipts signed)
                          (OTEL exported)
```

---

## Layered Integration

```
┌─────────────────────────────────────────────────────────────────────┐
│ Layer 5: APPLICATION                                               │
│  ├─ @unrdf/cli              ◄─── Can invoke daemon operations      │
│  ├─ REST APIs               ◄─── Can trigger manual execution      │
│  └─ React/Vue components    ◄─── Can display daemon health        │
├─────────────────────────────────────────────────────────────────────┤
│ Layer 4: KNOWLEDGE SUBSTRATE (Reactive) ◄─── PRIMARY LAYER        │
│  ├─ @unrdf/daemon           ◄─── NEW (you are here)              │
│  ├─ @unrdf/hooks            ◄─── Uses scheduler                   │
│  ├─ @unrdf/streaming        ◄─── Reactive subscriptions           │
│  ├─ @unrdf/federation       ◄─── Distributed operations           │
│  ├─ @unrdf/consensus        ◄─── Leadership coordination          │
│  └─ @unrdf/observability    ◄─── Monitoring/alerting              │
├─────────────────────────────────────────────────────────────────────┤
│ Layer 3: KGC (Temporal Governance) ◄─── SECONDARY LAYER           │
│  ├─ @unrdf/kgc-4d           ◄─── Event sourcing (audit)           │
│  ├─ @unrdf/receipts         ◄─── Cryptographic proof              │
│  ├─ @unrdf/v6-core          ◄─── Delta contracts                  │
│  └─ @unrdf/kgc-runtime      ◄─── Policy enforcement               │
├─────────────────────────────────────────────────────────────────────┤
│ Layer 2: RDF CORE (Deterministic)                                  │
│  ├─ @unrdf/core             ◄─── Query execution                  │
│  └─ @unrdf/v6-core          ◄─── SPARQL via ΔGate                 │
├─────────────────────────────────────────────────────────────────────┤
│ Layer 1: INFRASTRUCTURE                                            │
│  ├─ Oxigraph WASM           ◄─── SPARQL engine                    │
│  ├─ Raft consensus          ◄─── Cluster coordination             │
│  └─ OTEL                    ◄─── Observability framework           │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Execution Flow: Single Operation

```
┌─ START ──────────────────────────────────────────────────────────┐
│                                                                   │
│  time: T0                                                         │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │ Trigger Fires (interval, cron, event, reactive)         │   │
│  └──────────────┬───────────────────────────────────────────┘   │
│                 │                                                │
│  time: T0 + 0ms                                                 │
│  ┌──────────────▼───────────────────────────────────────────┐   │
│  │ 1. Operation enqueued (FIFO or priority)                │   │
│  │    - Validate against schema                            │   │
│  │    - Emit 'operation:enqueued' event                    │   │
│  │    - Update metrics: queue.backlog++                    │   │
│  └──────────────┬───────────────────────────────────────────┘   │
│                 │                                                │
│  time: T0 + 5ms                                                 │
│  ┌──────────────▼───────────────────────────────────────────┐   │
│  │ 2. Check cluster scope                                  │   │
│  │    - local: execute on this node                        │   │
│  │    - leader: execute only if isLeader = true            │   │
│  │    - global: distribute to all nodes                    │   │
│  └──────────────┬───────────────────────────────────────────┘   │
│                 │                                                │
│  time: T0 + 10ms                                                │
│  ┌──────────────▼───────────────────────────────────────────┐   │
│  │ 3. Begin execution                                      │   │
│  │    - Start timer (respect timeout)                      │   │
│  │    - Emit 'operation:started' event                     │   │
│  │    - Log to KGC-4D event store (async)                 │   │
│  │    - Create OTEL span                                   │   │
│  └──────────────┬───────────────────────────────────────────┘   │
│                 │                                                │
│  time: T0 + 15ms                                                │
│  ┌──────────────▼───────────────────────────────────────────┐   │
│  │ 4. Execute operation (hook/policy)                      │   │
│  │    - Call registered handler                            │   │
│  │    - Compute input hash (Merkle)                        │   │
│  │    - Catch exceptions                                   │   │
│  └──────────────┬───────────────────────────────────────────┘   │
│                 │                                                │
│                 │ (Success path)                                 │
│                 │                                                │
│  time: T0 + 150ms                                               │
│  ┌──────────────▼───────────────────────────────────────────┐   │
│  │ 5a. SUCCESS: Generate receipt                           │   │
│  │    - Compute output hash                                │   │
│  │    - Create receipt object                              │   │
│  │    - Chain to previous receipt (Merkle)                │   │
│  │    - Sign receipt (crypto)                              │   │
│  │    - Store in receiptGenerator                          │   │
│  │    - Update metrics: operations.success++               │   │
│  └──────────────┬───────────────────────────────────────────┘   │
│                 │                                                │
│  time: T0 + 200ms                                               │
│  ┌──────────────▼───────────────────────────────────────────┐   │
│  │ 6a. EMIT SUCCESS EVENT                                  │   │
│  │    - 'operation:success' (receipt)                      │   │
│  │    - Alert manager: check for threshold alerts          │   │
│  │    - Streaming subscribers notified                     │   │
│  │    - Hook subscribers triggered (cascading)             │   │
│  └──────────────┬───────────────────────────────────────────┘   │
│                 │                                                │
│  time: T0 + 210ms                                               │
│  ┌──────────────▼───────────────────────────────────────────┐   │
│  │ 7a. FINALIZE SUCCESS                                    │   │
│  │    - Remove from pending queue                          │   │
│  │    - Add to completedOperations (LRU, 1000 max)        │   │
│  │    - Emit 'operation:complete' (final state)            │   │
│  │    - OTEL span ended                                    │   │
│  └──────────────┬───────────────────────────────────────────┘   │
│                 │                                                │
│                 └─────────────────────────────┐                 │
│                                               │                 │
│                                    ┌──────────▼─────┐           │
│                                    │ Receipt: ✓     │           │
│                                    │ Duration: 150ms│           │
│                                    │ Status: success│           │
│                                    └────────────────┘           │
│                                                                   │
└─── OR (if FAILURE) ────────────────────────────────────────────┐
│                                                                   │
│                      (Exception caught at step 4)                │
│                                                                   │
│  time: T0 + 160ms                                               │
│  ┌──────────────▼───────────────────────────────────────────┐   │
│  │ 5b. FAILURE: Check retry policy                         │   │
│  │    - retryCount < maxAttempts?                          │   │
│  │    - YES: re-enqueue with backoff                       │   │
│  │    - NO: generate failure receipt                       │   │
│  │    - Update metrics: operations.failed++                │   │
│  └──────────────┬───────────────────────────────────────────┘   │
│                 │                                                │
│  time: T0 + 165ms                                               │
│  ┌──────────────▼───────────────────────────────────────────┐   │
│  │ 6b. EMIT FAILURE ALERT                                  │   │
│  │    - AlertManager: trigger alert                        │   │
│  │    - Send to observability system                       │   │
│  │    - Operators notified                                 │   │
│  │    - Update health status (degraded/unhealthy)          │   │
│  └──────────────┬───────────────────────────────────────────┘   │
│                 │                                                │
│  time: T0 + 170ms                                               │
│  ┌──────────────▼───────────────────────────────────────────┐   │
│  │ 7b. FINALIZE FAILURE                                    │   │
│  │    - OR re-schedule if retry                            │   │
│  │    - Record error in receipt                            │   │
│  │    - OTEL span ended with error                         │   │
│  └──────────────┬───────────────────────────────────────────┘   │
│                 │                                                │
│                 └─────────────────────────────┐                 │
│                                               │                 │
│                                    ┌──────────▼──────┐          │
│                                    │ Receipt: ✗      │          │
│                                    │ Error: timeout  │          │
│                                    │ Retry: pending  │          │
│                                    └─────────────────┘          │
│                                                                   │
└───────────────────────────────────────────────────────────────────┘
```

---

## Data Flow: Multi-Node Cluster

```
CLUSTER: 3 nodes (leader + 2 followers)

Node 1 (LEADER)              Node 2 (FOLLOWER)       Node 3 (FOLLOWER)
┌──────────────────┐         ┌──────────────────┐    ┌──────────────────┐
│ DAEMON           │         │ DAEMON           │    │ DAEMON           │
├──────────────────┤         ├──────────────────┤    ├──────────────────┤
│ isLeader: true   │         │ isLeader: false  │    │ isLeader: false  │
│ Status: RUNNING  │         │ Status: STANDBY  │    │ Status: STANDBY  │
│                  │         │                  │    │                  │
│ ┌────────────┐   │         │ ┌────────────┐   │    │ ┌────────────┐   │
│ │ scheduler  │   │         │ │ scheduler  │   │    │ │ scheduler  │   │
│ │ (active)   │   │         │ │ (inactive) │   │    │ │ (inactive) │   │
│ └─────┬──────┘   │         │ └─────┬──────┘   │    │ └─────┬──────┘   │
│       │          │         │       │          │    │       │          │
│       ▼          │         │       ▼          │    │       ▼          │
│ ┌────────────┐   │         │ ┌────────────┐   │    │ ┌────────────┐   │
│ │ raft node  │   │         │ │ raft node  │   │    │ │ raft node  │   │
│ │ (leader)   │   │         │ │ (follower) │   │    │ │ (follower) │   │
│ └─────┬──────┘   │         │ └─────┬──────┘   │    │ └─────┬──────┘   │
│       │          │         │       │          │    │       │          │
└───────┼──────────┘         └───────┼──────────┘    └───────┼──────────┘
        │                            │                      │
        │ HEARTBEAT (every 150ms)    │                      │
        ├───────────────────────────►├─ APPEND ENTRY ─────►│
        │                            │                      │
        │ (ACK)                      │ (ACK)                │
        │◄──────────────────────────┤◄─────────────────────┤
        │                            │                      │
        │ DISTRIBUTE GLOBAL WORK     │                      │
        │                            │                      │
        │ (operations with           │                      │
        │  clusterScope: 'global')   │                      │
        │                            │                      │
        │ ┌───────────────────────┐  │                      │
        │ │ distribute([ops])      │  │                      │
        │ │ strategy: round-robin  │  │                      │
        │ └───────┬───────────────┘  │                      │
        │         │                   │                      │
        │         ├─ ops[0] ─────────►│ (execute locally)    │
        │         │                   │                      │
        │         ├─ ops[1] ────────────────────────────────►│
        │         │                   │                      │
        │         └─ ops[2] ─────────►│ (execute locally)    │
        │                            │                      │
        │                            ▼ (EXECUTE)            │ (EXECUTE)
        │                       ┌─────────────┐            ┌────────────┐
        │                       │ run op[0]   │            │ run op[1]  │
        │                       └─────┬───────┘            └──────┬─────┘
        │                             │                         │
        │                        (RESULT)                   (RESULT)
        │                             │                         │
        │◄────────── COLLECT RESULTS ─┴─────────────────────────┤
        │
        │ (AGGREGATE)
        │
        ▼
   ┌────────────────────┐
   │ Global receipt     │
   │ - from 3 nodes     │
   │ - consensus: yes   │
   │ - signed by leader │
   │ - broadcast to all │
   └────────────────────┘
```

---

## State Machine: Daemon Lifecycle

```
┌────────┐
│        │
│  INIT  │
│        │
└───┬────┘
    │ daemon.start()
    │
    ▼
┌──────────┐
│          │
│ STARTING │  - Initialize integrations
│          │  - Connect to Raft cluster
│          │  - Load scheduled operations
│          │  - Emit 'daemon:starting' event
└───┬──────┘
    │
    ▼
┌─────────────────┐
│                 │
│   RUNNING       │  - Scheduler active
│   (if leader)   │  - Processing operations
│                 │  - Emitting metrics
└───┬─────────────┘
    │        │
    │        └────► ┌──────────────────┐
    │               │   STANDBY        │
    │               │  (if follower)   │
    │               └────────┬─────────┘
    │                        │
    │                        │ leader:elected
    │                        ▼
    │               ┌──────────────────┐
    │               │   RUNNING        │
    │               │  (promoted)      │
    │               └────────┬─────────┘
    │                        │
    │     ┌──────────────────┘
    │     │
    │ daemon.stop()
    │ or leadership lost
    │
    ▼
┌──────────────┐
│              │
│  STOPPING    │  - Halt new operations
│              │  - Wait for in-flight ops (graceful)
│              │  - Flush receipts to storage
│              │  - Emit 'daemon:stopping' event
└───┬──────────┘
    │
    ▼
┌──────────┐
│          │
│ STOPPED  │  - All operations halted
│          │  - Connections closed
│          │  - Final metrics exported
└──────────┘
```

---

## Integration Points: Before & After

### BEFORE (@unrdf/daemon)

```
Application
├─ Cache refresh?    → setInterval() in module A
├─ Nightly backup?   → cron job outside app
├─ Health check?     → setInterval() in module B
├─ Work distribution? → manual across nodes
├─ Retry logic?      → ad-hoc try-catch
├─ Monitoring?       → scattered logging
└─ Audit trail?      → incomplete, scattered
```

### AFTER (@unrdf/daemon)

```
Application
└─ daemon.schedule({...})
   ├─ Trigger evaluation ──► HookScheduler
   ├─ Execution ─────────► Hook policies
   ├─ Change feeds ──────► Streaming subscriptions
   ├─ Distributed work ──► Raft leader election
   ├─ Retry logic ───────► Built-in, configurable
   ├─ Monitoring ────────► OTEL metrics + alerts
   └─ Audit trail ───────► KGC-4D + receipts

Result: Single control point for ALL background operations
```

---

## Success Metrics

```
BEFORE DAEMON:
├─ 12 setInterval() calls scattered
├─ 3 different retry mechanisms
├─ Silent failures (no alerting)
├─ No distributed coordination
├─ Manual node failover
└─ Incomplete audit trail

AFTER DAEMON:
├─ 1 unified orchestrator
├─ Consistent retry policy
├─ Automatic alerts + health checks
├─ Automatic Raft-based leadership
├─ Transparent multi-node failover
├─ Complete cryptographic audit trail
├─ OTEL metrics on all operations
└─ P95 operation latency: <100ms
```
