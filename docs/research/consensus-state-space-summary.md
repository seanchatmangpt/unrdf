# Consensus State Space Explosion - Quick Reference

**TL;DR**: Distributed consensus protocols face exponential state space growth. UNRDF implements both Raft (crash fault tolerant) and PBFT (Byzantine fault tolerant) with practical optimizations reducing state space by factors of 10^300+.

---

## State Space Growth Rates

| Nodes (N) | Raft States | PBFT States | Raft Messages/Op | PBFT Messages/Op |
|-----------|-------------|-------------|------------------|------------------|
| 3         | 27          | N/A         | 4                | N/A              |
| 4         | 81          | 625         | 6                | 28               |
| 5         | 243         | 3,125       | 8                | 40               |
| 7         | 2,187       | 78,125      | 12               | 91               |
| 10        | 59,049      | 9,765,625   | 18               | 190              |

**Growth**: Raft = O(3^N), PBFT = O(5^N)

---

## Key Formulas

### Raft State Space
```
Basic:     S = 3^N (node states: Follower, Candidate, Leader)
With log:  S = 3^N × 3^L (L log entries)
Complete:  S = 3^N × 3^L × T × (N+1) × L × L × 4^N × Bell(N)

Example (N=5, L=10, T=10):
  S ≈ 2.29 × 10^13 states (with optimizations)
  S ≈ 4.73 × 10^59 states (without optimizations)
```

### PBFT State Space
```
Basic:     S = 5^N (phases: Idle, Pre-Prepare, Prepare, Commit, Committed)
Complete:  S = 5^N × V × S × (2^N)^(S/C) × (2^N)^(S/C) × 8^f × 4^N

Example (N=7, f=2, V=10, S=10):
  S ≈ 1.14 × 10^32 states (with checkpoints)
  S ≈ astronomically large (without checkpoints)
```

### Message Complexity
```
Raft:  M = 2(N-1) per operation
PBFT:  M = 2N² - N per operation

Example (N=7):
  Raft:  12 messages
  PBFT:  91 messages (7.6× overhead)
```

---

## Fault Tolerance Boundaries

### Raft (Crash Failures)
```
Quorum = ⌊N/2⌋ + 1
Max failures = N - Quorum

N=3: 1 failure  (33%)
N=5: 2 failures (40%)
N=7: 3 failures (43%)

Asymptotic: ~50% crash tolerance
```

### PBFT (Byzantine Failures)
```
N = 3f + 1
Quorum = 2f + 1
Max Byzantine failures = f

f=1: N=4,  25% tolerance
f=2: N=7,  29% tolerance
f=3: N=10, 30% tolerance

Asymptotic: ~33% Byzantine tolerance
```

---

## Performance Characteristics

### Latency (from benchmarks)

| Cluster | Raft P95 | PBFT P95 | Raft Throughput | PBFT Throughput |
|---------|----------|----------|-----------------|-----------------|
| N=3     | 2.7ms    | N/A      | 450 ops/sec     | N/A             |
| N=4     | 3.5ms    | 8.0ms    | 350 ops/sec     | 180 ops/sec     |
| N=5     | 4.1ms    | 10.5ms   | 280 ops/sec     | 140 ops/sec     |
| N=7     | 7.9ms    | 18.0ms   | 180 ops/sec     | 80 ops/sec      |

**Trade-off**: Raft is 2-3× faster but only handles crash failures

### Recovery Time

| Scenario                  | Raft         | PBFT          |
|---------------------------|--------------|---------------|
| Single leader failure     | 200-400ms    | 5000-5100ms   |
| Split vote / view change  | 1200ms       | 5100ms        |
| Byzantine cascade         | N/A          | 10,200ms (f=2)|

**Raft recovers 12-25× faster** due to simpler protocol

---

## Network Partition Tolerance

### Raft (N=5 cluster)
```
[3, 2]:      Majority (3) continues, minority (2) waits
[2, 2, 1]:   No majority → UNAVAILABLE
[1, 1, 1, 1, 1]: Complete isolation → UNAVAILABLE

Recovery: Automatic once partition heals
Safety: GUARANTEED (no split-brain)
```

### PBFT (N=7, f=2 cluster)
```
[5, 2]:    Quorum (2f+1=5) in majority → AVAILABLE (if honest)
[4, 3]:    No quorum → UNAVAILABLE
[2, 2, 2, 1]: UNAVAILABLE

Recovery: Requires view change + network healing
Safety: GUARANTEED if ≤f Byzantine nodes
```

---

## Critical Scenarios

### Top 5 Complex Raft Scenarios

1. **Cascading Leader Failures**: 15+ state transitions, 200+ messages (N=5)
2. **Log Divergence**: 32 AppendEntries RPCs to reconcile (5 nodes, 5 entries)
3. **Split Vote Storm**: 1200ms to stability (4× election timeout)
4. **Quorum Loss During Membership Change**: Requires removing failed node
5. **Network Partition Healing**: Election storm as partitions merge

### Top 5 Complex PBFT Scenarios

1. **Byzantine Equivocation**: 121 messages, 2× latency, requires view change
2. **View Change Storm**: 165 messages, 3 view changes before honest primary
3. **Checkpoint Divergence**: Memory growth until 2f+1 honest checkpoints
4. **Pre-Prepare Timeout Cascade**: Sequences 10× slower during Byzantine attack
5. **Quorum Formation Failure**: View change loop if f+1 Byzantine nodes

---

## Optimization Impact

### Log Compaction (Raft)
```
Without: 3^1000 states (L=1000 log entries)
With:    3^10 states   (L=10 after snapshot)

Reduction: 10^467 factor
Implementation: Snapshot every 1000 entries
```

### Checkpointing (PBFT)
```
Without: (2^N)^1000 prepare/commit states (N=7, S=1000)
With:    (2^N)^100                        (checkpoint every 100)

Reduction: 10^300+ factor
Implementation: Checkpoint every 100 sequences
```

### Term/View Limits
```
Without: Unbounded term/view growth
With:    Reset at T=100 via distributed agreement

Prevents: Term overflow, view number explosion
Safety: Maintained through coordinated reset
```

---

## Recommended Cluster Sizes

### Development
```
Raft:  N=3 (fast, minimal resources, 1 failure tolerance)
PBFT:  N=4 (f=1, single Byzantine node)
```

### Production
```
Raft:  N=5 (optimal balance: 2 failures, 243 states, 4.1ms P95)
PBFT:  N=7 (f=2, two Byzantine nodes, 91 messages/op)
```

### High Security
```
Raft:  N=7 (3 failures, 2,187 states, 7.9ms P95)
PBFT:  N=10 (f=3, three Byzantine nodes, 190 messages/op)
```

### Avoid
```
N=2:  No fault tolerance
N>9:  Diminishing returns, message overhead dominates
```

---

## Health Monitoring

### Raft Critical Metrics
```
✓ Leader elections/hour:     <1 (stable leadership)
✓ Log replication lag:        <10 entries
✓ Term progression rate:      <5 terms/hour
✓ Split vote rate:            <10% of elections
✓ Follower heartbeat timeout: <1% of intervals
```

### PBFT Critical Metrics
```
✓ View changes/hour:          <1 (stable primary)
✓ Prepare timeout rate:       <1% of requests
✓ Commit latency P99:         <50ms (N=7)
✓ Checkpoint lag:             <100 sequences
✓ Message signature failures: 0% (authentication working)
```

---

## Implementation Details (UNRDF)

### File Sizes
```
Raft core:              713 lines  (packages/kgc-swarm/src/consensus/raft.mjs)
Raft coordinator:       777 lines  (packages/consensus/src/raft/raft-coordinator.mjs)
PBFT core:              731 lines  (packages/kgc-swarm/src/consensus/byzantine.mjs)
State machine:          458 lines  (packages/consensus/src/state/distributed-state-machine.mjs)
Cluster manager:        458 lines  (packages/consensus/src/membership/cluster-manager.mjs)

Total consensus code:   3,152 lines (production code: 2,421 lines)
```

### Default Configuration
```javascript
// Raft
electionTimeoutMin: 150,      // ms
electionTimeoutMax: 300,      // ms
heartbeatInterval: 50,        // ms
snapshotThreshold: 1000,      // log entries

// PBFT
viewChangeTimeout: 5000,      // ms
checkpointInterval: 100,      // sequences
f: 2,                         // Byzantine failures (N=7)
quorum: 5,                    // 2f+1 for N=7

// Cluster
healthCheckInterval: 5000,    // ms
maxFailedHealthChecks: 3,     // before marking unhealthy
autoDiscovery: false,         // disabled by default
```

---

## Quick Decision Matrix

| Requirement              | Choose Raft | Choose PBFT |
|--------------------------|-------------|-------------|
| Trusted nodes            | ✓           |             |
| Byzantine adversaries    |             | ✓           |
| Low latency critical     | ✓           |             |
| High throughput critical | ✓           |             |
| Small cluster (N≤5)      | ✓           |             |
| Large cluster (N>5)      |             | ✓           |
| Crash failures only      | ✓           |             |
| Malicious nodes possible |             | ✓           |
| Fast recovery needed     | ✓           |             |
| Cryptographic proofs     |             | ✓           |

**Rule of Thumb**: Use Raft unless you need Byzantine fault tolerance

---

## State Space Reduction Checklist

- [ ] Enable log compaction (Raft)
- [ ] Configure checkpoint interval (PBFT)
- [ ] Set snapshot threshold (Raft: 500-1000 entries)
- [ ] Implement term/view reset protocol
- [ ] Enable garbage collection of committed entries
- [ ] Monitor state machine size (target: <100,000 entries)
- [ ] Configure max log size alerts
- [ ] Test partition scenarios in staging
- [ ] Validate quorum math (N=3f+1 for PBFT)
- [ ] Benchmark with realistic failure rates

---

## References

**Full Analysis**: `/home/user/unrdf/docs/research/consensus-state-space-analysis.md` (1031 lines)

**Key Papers**:
- Raft: "In Search of an Understandable Consensus Algorithm" (Ongaro & Ousterhout, 2014)
- PBFT: "Practical Byzantine Fault Tolerance" (Castro & Liskov, 1999)

**Test Coverage**:
- Raft: 351 lines (packages/kgc-swarm/test/consensus/raft.test.mjs)
- PBFT: 380 lines (packages/kgc-swarm/test/consensus/byzantine.test.mjs)

**Benchmarks**:
- Raft Replication: packages/daemon/benchmarks/04-raft-replication.bench.mjs

---

**Last Updated**: 2026-01-11
