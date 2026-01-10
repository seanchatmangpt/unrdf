# Service Level Objectives (SLOs) - @unrdf/daemon

Production-grade SLO definitions for @unrdf/daemon operations, derived from 6 core JTBD (Jobs To Be Done) scenarios.

**Version**: 1.0.0
**Last Updated**: 2026-01-10
**Measurement Period**: Monthly (30-day rolling window)

---

## JTBD 1: Receipt Generation Latency

**Goal**: Generate operation receipts with cryptographic proofs in <100ms to support audit compliance and fast result confirmation.

### Key Metrics

| Metric | Unit | Target | Threshold Range |
|--------|------|--------|-----------------|
| **P50 Receipt Latency** | milliseconds | 5 | 1-10 ms |
| **P95 Receipt Latency** | milliseconds | 15 | 10-25 ms |
| **P99 Receipt Latency** | milliseconds | 45 | 35-60 ms |
| **P99.9 Receipt Latency** | milliseconds | 100 | 80-120 ms |
| **Receipt Generation Success Rate** | percent | 99.95 | 99.9-100% |
| **Receipt Hash Collision Rate** | per million | 0 | 0-0.1 |

### SLO Statement

**The daemon SHALL generate operation receipts with cryptographic Merkle-tree proofs in <100ms for 99.95% of operations, with P99 latency not exceeding 45ms.**

### Why This Matters

- **Compliance**: Audit trails must be created synchronously without blocking operations
- **User Experience**: Fast feedback on operation success/failure enables responsive UIs
- **Data Integrity**: Receipt generation failure = potential loss of audit trail
- **Operational Trust**: Users need cryptographic proof of operation execution for non-repudiation
- **Business Impact if Missed**:
  - Compliance violations → regulatory penalties (up to 10% revenue)
  - Audit failures → loss of trust from auditors
  - Slow receipt generation → user-visible latency → reduced adoption

### Measurement Method

**Test Scenario JTBD-1: Receipt Generation Latency**

```javascript
// From test harness: daemon.test.mjs + e2e scenario
1. Create daemon with receiptConfig: { generateMerkle: true }
2. Schedule 10,000 operations with receipt generation enabled
3. For each operation:
   - Record: timestamp_submitted, timestamp_receipt_generated
   - Calculate: latency = timestamp_receipt_generated - timestamp_submitted
   - Verify: receipt.merkleHash is valid and unique
4. Collect percentiles: P50, P95, P99, P99.9
5. Calculate: success_rate = (valid_receipts / total_operations) * 100
6. Calculate: collision_rate = (duplicate_hashes / total_operations) * 1_000_000
```

**Measurement Frequency**: Every operation (real-time)
**Aggregation Window**: 30-day rolling
**Data Source**: Operation completion events + receipt validation logs

### Alert Threshold

| Condition | Severity | Action |
|-----------|----------|--------|
| P99 latency > 60ms | CRITICAL | Page on-call immediately |
| P95 latency > 30ms | WARNING | Investigate within 1 hour |
| Success rate < 99.9% | CRITICAL | Stop deployments, investigate root cause |
| Collision rate > 0.1/million | CRITICAL | Security review + forensics |
| P50 latency drift > 50% month-over-month | WARNING | Performance regression analysis |

### Performance Targets by Operation Type

| Operation Type | P50 | P95 | P99 | Notes |
|---|---|---|---|---|
| **Create** (insert new entity) | 3ms | 12ms | 40ms | Simple receipt |
| **Update** (modify existing) | 5ms | 18ms | 45ms | Chain-linked receipt |
| **Delete** (remove entity) | 4ms | 15ms | 42ms | Tombstone receipt |
| **Batch** (10+ operations) | 8ms | 22ms | 55ms | Aggregated receipt |

---

## JTBD 2: Verification Latency

**Goal**: Verify operation receipts and proofs against blockchain/ledger in <5min (300,000ms) to enable fraud detection and compliance validation.

### Key Metrics

| Metric | Unit | Target | Threshold Range |
|--------|------|--------|-----------------|
| **P50 Verification Latency** | seconds | 5 | 1-10 sec |
| **P95 Verification Latency** | seconds | 45 | 30-60 sec |
| **P99 Verification Latency** | seconds | 120 | 90-150 sec |
| **P99.9 Verification Latency** | seconds | 280 | 250-300 sec |
| **Verification Success Rate** | percent | 99.9 | 99.8-100% |
| **Ledger Sync Time** | seconds | 30 | 15-45 sec |
| **False Negative Rate** | percent | 0.001 | 0-0.01% |
| **False Positive Rate** | percent | 0.001 | 0-0.01% |

### SLO Statement

**The daemon SHALL verify operation receipts against the authoritative ledger in <300 seconds (5 minutes) with 99.9% success rate, ensuring cryptographic proof validity with <0.001% false positive/negative rates.**

### Why This Matters

- **Security**: Verify proofs haven't been tampered with or forged
- **Compliance**: Regulatory requirement for immutable audit trails
- **Fraud Detection**: Identify unauthorized or double-spent operations
- **System Integrity**: Cross-validate across distributed nodes
- **Business Impact if Missed**:
  - Undetected fraud → financial loss (up to $5M per incident)
  - Failed compliance audit → license suspension
  - Loss of ledger sync → node divergence → consensus failure
  - False positives → operational overhead, alert fatigue

### Measurement Method

**Test Scenario JTBD-2: Verification Latency**

```javascript
// From test harness: verification-integration.test.mjs
1. Setup: 3-node Raft cluster with ledger consensus
2. Create 1,000 operations with receipts across cluster
3. Trigger verification on each receipt:
   - Submit proof to ledger node
   - Measure: time until ledger responds with validation
   - Verify: response matches canonical ledger state
   - Check: cryptographic signature is valid
4. Collect metrics:
   - latency percentiles (P50, P95, P99, P99.9)
   - success_rate = (verified / total)
   - false_positive_rate, false_negative_rate
   - ledger_sync_time (time from local receipt to leader acknowledgment)
5. Repeat under network conditions: 0ms latency, 10ms, 100ms, 500ms jitter
```

**Measurement Frequency**: Per verification request
**Aggregation Window**: 30-day rolling
**Data Source**: Ledger verification event logs + consensus metrics

### Alert Threshold

| Condition | Severity | Action |
|-----------|----------|--------|
| P99 verification latency > 150s | CRITICAL | Check ledger health + network |
| Verification success rate < 99.5% | CRITICAL | Stop accepting new operations |
| Ledger sync time > 60s | WARNING | Investigate leader health |
| False positive rate > 0.01% | CRITICAL | Audit verification logic |
| False negative rate > 0.01% | CRITICAL | Manual forensic review + rollback |

### Verification Scenarios

| Scenario | Load | Expected Latency | Notes |
|---|---|---|---|
| **Single operation** | 1 op | 2-5s | Immediate ledger response |
| **Batch** (10 ops) | 10 ops | 8-15s | Aggregated verification |
| **High throughput** | 100 ops/sec | 30-60s | Queue + batching overhead |
| **Network jitter** (100ms) | 100 ops/sec | 60-120s | Consensus re-tries |
| **Leader failover** | 100 ops/sec | 180-280s | Raft re-election + state transfer |

---

## JTBD 3: Failover Recovery Time

**Goal**: Recover from node/leader failures in <5 minutes (300,000ms) to maintain service availability in clustered deployments.

### Key Metrics

| Metric | Unit | Target | Threshold Range |
|--------|------|--------|-----------------|
| **Failure Detection Time** | seconds | 5 | 1-10 sec |
| **Leader Election Time** | seconds | 10 | 5-20 sec |
| **State Synchronization Time** | seconds | 30 | 15-60 sec |
| **Full Failover Recovery Time** | seconds | 45 | 30-60 sec |
| **Failover Success Rate** | percent | 99.99 | 99.95-100% |
| **Data Loss Rate** | bytes | 0 | 0-0 |
| **Operation Continuity Rate** | percent | 99.95 | 99.9-100% |

### SLO Statement

**The daemon cluster SHALL detect leader failures in <10 seconds, elect a new leader in <10 seconds, and resume operation within 45 seconds total failover time, with 99.99% success rate and ZERO data loss, maintaining operation continuity for 99.95% of ongoing operations.**

### Why This Matters

- **Availability**: System continues despite hardware/network failures
- **Data Safety**: No data loss during failover transitions
- **User Impact**: Minimal interruption to ongoing operations
- **Operational Confidence**: Operators can deploy without fear of extended downtime
- **Business Impact if Missed**:
  - 5-minute outage × 10 incidents/month = 50 minutes downtime → SLA breach
  - Undetected node failure → split-brain condition → inconsistent state
  - Slow failover → batch job timeout → lost work
  - Data loss → compliance violation + customer trust loss

### Measurement Method

**Test Scenario JTBD-3: Failover Recovery Time**

```javascript
// From test harness: failover-integration.test.mjs
1. Setup: 3-node Raft cluster, all healthy, running operations
2. Baseline: Monitor metrics on leader before failure
3. Inject failure: Kill leader process or network partition
4. Measure detection phase:
   - timestamp_failure_injected
   - Record when followers detect leader is unresponsive
   - detection_time = timestamp_detected - timestamp_failure_injected
5. Measure election phase:
   - Record when new leader is elected
   - election_time = timestamp_elected - timestamp_failure_injected
6. Measure sync phase:
   - Record when followers sync state from new leader
   - sync_time = timestamp_synced - timestamp_elected
7. Total recovery time = detection_time + election_time + sync_time
8. Verify:
   - New leader is different from failed leader
   - All in-flight operations are tracked (P99 < 1KB lost)
   - Queue is consistent across nodes
   - No duplicate execution of operations
9. Repeat test 50 times with random failure timings
```

**Measurement Frequency**: Per failover event
**Aggregation Window**: Per month (rolling 30 days)
**Data Source**: Raft consensus logs + health check events

### Alert Threshold

| Condition | Severity | Action |
|-----------|----------|--------|
| Failure detection > 15s | WARNING | Tune heartbeat config |
| Leader election > 20s | WARNING | Check network stability |
| Full recovery > 60s | CRITICAL | Investigate root cause |
| Failover success rate < 99.95% | CRITICAL | Audit Raft implementation |
| Data loss > 0 | CRITICAL | Immediate incident + forensics |
| Multiple failovers in 1 hour | CRITICAL | Check infrastructure health |

### Failover Scenarios

| Scenario | Failure Type | Expected Recovery | Notes |
|---|---|---|---|
| **Leader crash** | Process kill | 30-45s | Clean shutdown + election |
| **Network partition** | Link cut | 40-60s | Detection delay + timeout |
| **Slow disk I/O** | Disk contention | 60-90s | State sync delayed |
| **Cascading failure** | Multiple nodes | 120-180s | Quorum loss risk |
| **Split brain** | Network asymmetry | 180-300s | Manual intervention likely |

---

## JTBD 4: Diagnostic Latency

**Goal**: Generate diagnostic reports (health, metrics, logs) in <5 minutes (300,000ms) to enable rapid troubleshooting and operational decision-making.

### Key Metrics

| Metric | Unit | Target | Threshold Range |
|--------|------|--------|-----------------|
| **Health Check Response Time** | milliseconds | 50 | 10-100 ms |
| **Metrics Query Latency** | seconds | 3 | 1-5 sec |
| **Full Diagnostic Report Time** | seconds | 30 | 15-60 sec |
| **Log Aggregation Time** | seconds | 45 | 30-90 sec |
| **Diagnostic Completeness** | percent | 100 | 99-100% |
| **Diagnostic Accuracy** | percent | 99.9 | 99.5-100% |

### SLO Statement

**The daemon SHALL provide health checks within 50ms, generate full diagnostic reports within 30 seconds, and aggregate cluster logs within 45 seconds, with 100% data completeness and 99.9% accuracy for root cause identification.**

### Why This Matters

- **Operational Speed**: Quick diagnosis → fast fixes → reduced MTTR (Mean Time To Resolution)
- **Problem Identification**: Rapid metrics collection identifies root cause before cascade fails
- **Preventive Action**: Diagnostics enable proactive scaling/tuning before failures
- **Compliance Audit**: Auditors need rapid evidence of system state
- **Business Impact if Missed**:
  - Slow diagnostics → slow MTTR → extended outage → SLA breach
  - Incomplete diagnostics → wrong fixes → repeating outages
  - Inaccurate metrics → misleading trends → poor capacity planning
  - Missing logs → can't prove compliance → audit failure

### Measurement Method

**Test Scenario JTBD-4: Diagnostic Latency**

```javascript
// From test harness: diagnostics-integration.test.mjs
1. Setup: Running 3-node cluster with 100 operations/sec load
2. At steady state, trigger diagnostics request:
   - Request: Full health report
   - Measure: timestamp_requested to timestamp_completed
3. Health check phase:
   - Query each node's health endpoint
   - Measure: max latency across 3 nodes
4. Metrics phase:
   - Aggregate metrics from last 30 days
   - Calculate: throughput, latency percentiles, success rates
   - Measure: time to compute all metrics
5. Log aggregation phase:
   - Fetch logs from all 3 nodes
   - Filter: last 1000 operations or last 5 minutes
   - Deduplicate + sort by timestamp
   - Measure: time to deliver consolidated view
6. Verify:
   - All 3 nodes represented in report
   - All metrics calculated without sampling
   - No logs missing or duplicated
   - Timestamps are monotonically increasing
7. Compare against baseline:
   - Establish P50/P95/P99 distribution
   - Alert on drift > 50%
```

**Measurement Frequency**: Per diagnostic request
**Aggregation Window**: 30-day rolling
**Data Source**: Health check endpoints + metrics APIs + log streaming

### Alert Threshold

| Condition | Severity | Action |
|-----------|----------|--------|
| Health check > 200ms | WARNING | Check node load + network |
| Metrics query > 10s | WARNING | Profile query performance |
| Diagnostic report > 60s | CRITICAL | Investigate slowness |
| Completeness < 99% | CRITICAL | Check for node failure |
| Accuracy < 99% | CRITICAL | Audit metrics calculation |

### Diagnostic Report Contents

| Section | Latency Contribution | Content |
|---|---|---|
| **Health** | 50ms | Node status, uptime, active ops |
| **Metrics** | 3s | Throughput, latency, success rate |
| **Resource Usage** | 2s | CPU, memory, network, disk |
| **Recent Operations** | 5s | Last 100 completions + failures |
| **Cluster State** | 2s | Leader, followers, consensus status |
| **Error Summary** | 3s | Top 10 error types, frequencies |
| **Log Snapshot** | 15s | Deduplicated logs, last 5 minutes |
| **Total** | ~30s | Full comprehensive view |

---

## JTBD 5: Throughput Under Concurrency

**Goal**: Process 33+ jobs/second with 1000 concurrent operations to support high-volume batch workloads without degradation.

### Key Metrics

| Metric | Unit | Target | Threshold Range |
|--------|------|--------|-----------------|
| **Sustained Throughput** | ops/second | 33 | 25-40 ops/sec |
| **P99 Queue Wait Time** | milliseconds | 500 | 100-1000 ms |
| **Concurrent Operation Limit** | operations | 1000 | 800-1200 |
| **Throughput Stability** | percent variation | <5 | <10% |
| **Queue Depth @ 100% Load** | operations | 500 | 300-800 |
| **Operation Failure Rate @ Load** | percent | <0.5 | <1% |

### SLO Statement

**The daemon SHALL sustain 33+ operations per second while managing 1000 concurrent operations, with P99 queue wait time <500ms, queue depth <500 ops, and failure rate <0.5%, maintaining consistent throughput (variation <5%) under sustained load.**

### Why This Matters

- **Scale**: Support batch processing (data migration, report generation, analytics)
- **Efficiency**: Process jobs without manual intervention or per-job scaling
- **Stability**: Consistent performance under load enables SLA commitments
- **Cost Optimization**: Batch jobs → lower infrastructure costs than real-time
- **Business Impact if Missed**:
  - Throughput < 33 ops/sec → batch jobs take hours instead of minutes
  - High queue wait → stale data → poor decision quality
  - Frequent failures under load → unreliable batch processes
  - Throughput variation → unpredictable runtimes → job scheduling chaos

### Measurement Method

**Test Scenario JTBD-5: Throughput Under Concurrency**

```javascript
// From test harness: throughput-load.test.mjs
1. Setup: Single-node daemon, concurrency: 100 (tuned for 1000 total)
2. Load phase (60 seconds):
   - Submit 2000 operations total (33-40 ops/sec for 60s)
   - Each operation: 100ms handler delay (simulating real work)
   - Track: enqueue timestamp, start timestamp, complete timestamp
3. For each time window (5-second buckets):
   - Count: operations_completed in that 5-second window
   - Calculate: throughput_ops_per_sec = operations_completed / 5
   - Record: queue_depth (pending - completed)
   - Record: wait_time = start_timestamp - enqueue_timestamp
4. Calculate metrics:
   - sustained_throughput = avg(throughput_ops_per_sec)
   - p99_queue_wait = percentile(all wait_times, 99)
   - max_queue_depth = max(queue_depth across all windows)
   - failure_count = count(failed operations)
   - variation = stdev(throughput_ops_per_sec) / mean(throughput_ops_per_sec)
5. Under different concurrency levels:
   - Test with concurrency: 10, 50, 100 (testing cascade)
6. Scaling test (optional):
   - 3-node cluster, repeat test
   - Expected: throughput scales linearly
```

**Measurement Frequency**: Per load test run
**Aggregation Window**: Monthly baseline
**Data Source**: Operation completion events + queue depth metrics

### Alert Threshold

| Condition | Severity | Action |
|-----------|----------|--------|
| Throughput < 25 ops/sec | CRITICAL | Investigate bottleneck |
| P99 queue wait > 1 second | WARNING | Check handler performance |
| Queue depth > 800 | WARNING | Scale up concurrency |
| Failure rate > 1% | CRITICAL | Debug handler errors |
| Throughput variation > 10% | WARNING | Profile for jitter |

### Load Test Profiles

| Profile | Concurrency | Duration | Expected Result |
|---|---|---|---|
| **Light** | 10 ops | 60s | 10 ops/sec baseline |
| **Normal** | 100 ops | 60s | 33+ ops/sec target |
| **Heavy** | 500 ops | 60s | 40+ ops/sec plateau |
| **Extreme** | 1000 ops | 60s | 40 ops/sec (queue saturated) |
| **Stress** | 1000 ops | 5m | Stability under sustained load |
| **Recovery** | 1000→10 ops | 3m | Return to baseline after drain |

---

## JTBD 6: Deployment Time

**Goal**: Complete full daemon deployment (build, test, release, rollout) in <1 hour (3600 seconds) to enable rapid iteration and low MTTR.

### Key Metrics

| Metric | Unit | Target | Threshold Range |
|--------|------|--------|-----------------|
| **Build Time** | seconds | 30 | 20-45 sec |
| **Test Suite Execution** | seconds | 60 | 45-90 sec |
| **Artifact Publishing** | seconds | 30 | 20-45 sec |
| **Release Notes Generation** | seconds | 15 | 10-30 sec |
| **Rollout Time** | seconds | 120 | 60-180 sec |
| **Smoke Test Execution** | seconds | 30 | 20-45 sec |
| **Full Deployment Time** | seconds | 285 | 240-360 sec (4-6 minutes) |
| **Deployment Success Rate** | percent | 99.5 | 99-100% |
| **Rollback Time** | seconds | 60 | 30-120 sec |

### SLO Statement

**The daemon deployment pipeline SHALL complete end-to-end (build → test → publish → rollout → smoke test) in <6 minutes (360 seconds), with 99.5% success rate and rollback capability within 2 minutes, enabling safe rapid iteration.**

### Why This Matters

- **Agility**: Fast iteration → rapid feature releases → competitive advantage
- **Security**: Security patches deployed in minutes, not hours
- **Reliability**: Frequent deployments → smaller changes → lower risk
- **MTTR**: Quick rollbacks enable aggressive change cadence
- **Developer Velocity**: Developers see changes in production quickly → morale
- **Business Impact if Missed**:
  - 1-hour deployment → 3-4 deployments/day max → slow feature velocity
  - Failed deployment every 100 deploys × 5 min avg fix → 8+ hours/month downtime
  - 30-minute rollback → extended customer impact → SLA breach
  - Manual deployment steps → error-prone → production incidents

### Measurement Method

**Test Scenario JTBD-6: Deployment Time**

```javascript
// From test harness: deployment-automation.test.mjs
1. Setup: Clean checkout, no pre-built artifacts, fresh environment
2. Measure build phase:
   - Start: git clone + npm install
   - End: dist/ artifacts ready
   - Time: build_time = timestamp_end - timestamp_start
3. Measure test phase:
   - Start: run pnpm test
   - End: test report generated, all tests passed
   - Time: test_time = timestamp_end - timestamp_start
   - Verify: coverage >= 80%, 0 failing tests
4. Measure artifact phase:
   - Start: npm pack + npm publish
   - End: artifact available in registry
   - Time: artifact_time = timestamp_end - timestamp_start
5. Measure release notes:
   - Start: parse commits since last tag
   - End: release notes file created
   - Time: notes_time = timestamp_end - timestamp_start
6. Measure rollout phase:
   - Start: deploy container to staging
   - End: container healthy + passed readiness checks
   - Time: rollout_time = timestamp_end - timestamp_start
7. Measure smoke test:
   - Start: send synthetic requests
   - End: all checks passed
   - Time: smoke_time = timestamp_end - timestamp_start
8. Calculate total:
   - total_time = build + test + artifact + notes + rollout + smoke
9. Test rollback:
   - Measure time to revert to previous version
   - Time: rollback_time = timestamp_current - timestamp_before_rollback
10. Repeat 50 times to establish distribution (P50, P95, P99)
```

**Measurement Frequency**: Per deployment
**Aggregation Window**: Monthly rolling average
**Data Source**: CI/CD pipeline logs + deployment tracking

### Alert Threshold

| Condition | Severity | Action |
|-----------|----------|--------|
| Total deployment time > 480s | WARNING | Investigate bottleneck |
| Total deployment time > 600s | CRITICAL | Block further deployments |
| Test execution > 120s | WARNING | Profile test suite |
| Rollout time > 240s | WARNING | Check infra provisioning |
| Smoke test failures > 0 | CRITICAL | Investigate deployment |
| Success rate < 99% | WARNING | Review deployment safety |
| Rollback time > 120s | CRITICAL | Improve rollback procedure |

### Deployment Pipeline Stages

| Stage | Target Time | Critical Path | Notes |
|---|---|---|---|
| **Checkout + Install** | 20s | YES | Dominated by npm install |
| **Build** | 15s | YES | TypeScript/Babel compilation |
| **Unit Tests** | 45s | YES | 100+ test cases |
| **Integration Tests** | 20s | NO | Optional, can parallelize |
| **Lint + Quality** | 10s | NO | ESLint, coverage check |
| **Artifact Build** | 10s | YES | Docker/npm package |
| **Publish** | 15s | YES | Registry upload |
| **Staging Deploy** | 60s | YES | Container orchestration |
| **Smoke Tests** | 25s | YES | Synthetic monitoring |
| **Prod Deploy** | 45s | YES | Rolling update |
| **Total** | 265s | - | 4.4 minutes average |

---

## SLO Summary Table

| JTBD | Primary Metric | Target | Unit | Alert If | Business Impact |
|-----|---|---|---|---|---|
| **1: Receipt Gen** | P99 latency | <45 | ms | >60ms | Audit trail delay, compliance risk |
| **1: Receipt Gen** | Success rate | 99.95% | % | <99.9% | Lost receipts, audibility risk |
| **2: Verification** | P99 latency | <120 | sec | >150s | Fraud undetected, sync delay |
| **2: Verification** | False positive/neg | <0.001% | % | >0.01% | Undetected tampering |
| **3: Failover** | Recovery time | <45 | sec | >60s | Extended downtime, SLA breach |
| **3: Failover** | Data loss | 0 | bytes | >0 | Inconsistent state, customer data loss |
| **4: Diagnostic** | Health check | <50 | ms | >100ms | Slow troubleshooting, high MTTR |
| **4: Diagnostic** | Full report | <30 | sec | >60s | Slow diagnosis, delayed fixes |
| **5: Throughput** | Sustained | ≥33 | ops/sec | <25 | Batch jobs run slow |
| **5: Throughput** | P99 queue wait | <500 | ms | >1000ms | High latency variance, poor UX |
| **5: Throughput** | Failure rate | <0.5% | % | >1% | Unreliable batch processing |
| **6: Deployment** | Total time | <360 | sec | >480s | Slow iteration velocity, high MTTR |
| **6: Deployment** | Success rate | 99.5% | % | <99% | Frequent deployment failures |
| **6: Deployment** | Rollback time | <120 | sec | >180s | Extended incident response |

---

## Constraint Analysis: Most Difficult SLOs

### Tier 1: Hardest to Meet (Tightest Constraints)

#### 1️⃣ **JTBD 5: Throughput @ 1000 Concurrent Operations**
- **Why Hard**: CPU cores are the fundamental limit. At 1000 concurrent ops with 100ms handler time, need 10+ cores just for handlers alone.
- **Constraint**: Physical hardware bottleneck. Can't parallelize beyond core count.
- **Risk Level**: HIGH - Easily broken by infrastructure changes
- **Typical Failure Mode**: Someone reduces concurrency config, throughput collapses
- **Mitigation**:
  - Multi-node horizontal scaling (add more daemon nodes)
  - Fast handler implementations (<100ms)
  - Load distribution across cluster
- **Dependency**: Performance of underlying handler code (not daemon's fault)

#### 2️⃣ **JTBD 3: Zero-Loss Failover in <45 seconds**
- **Why Hard**: Raft consensus has inherent timing delays (heartbeat + election timeout)
- **Constraint**: Network latency compounds detection time. Can't detect faster than heartbeat frequency.
- **Risk Level**: HIGH - Network conditions are unpredictable
- **Typical Failure Mode**: High-latency network (cloud regions) → detection slow → failover breaches SLO
- **Mitigation**:
  - Tune Raft parameters (heartbeat interval, election timeout)
  - Dedicated low-latency network for cluster communication
  - Geographic co-location of cluster nodes
- **Trade-off**: Tighter detection = more false positives = cluster churn

#### 3️⃣ **JTBD 2: Verification with <0.001% False Positive/Negative**
- **Why Hard**: Cryptographic validation is probabilistic. Depends on underlying crypto algorithm quality.
- **Constraint**: Can't do better than crypto library's error rates.
- **Risk Level**: MEDIUM-HIGH - Rare but catastrophic (fraud undetected)
- **Typical Failure Mode**: Hash collision (astronomically rare), or logic bug in verification
- **Mitigation**:
  - Use battle-tested crypto (SHA-256, not custom)
  - Extensive test coverage for edge cases
  - Independent security audit
- **Dependency**: Cryptographic library correctness

### Tier 2: Medium Difficulty (Important but Achievable)

#### 4️⃣ **JTBD 6: 4-6 Minute Deployments**
- **Why Hard**: npm install dominates (40-60% of time). Parallelization has limits.
- **Constraint**: Serialized build steps create critical path. Test suite runtime is long.
- **Risk Level**: MEDIUM - Can improve with caching and parallel test execution
- **Typical Failure Mode**: Cold npm install (fresh CI runner), or test suite regression
- **Mitigation**:
  - npm cache optimization (persistent runner cache)
  - Parallel test execution (Vitest forks)
  - Incremental builds (TypeScript incremental)
  - Docker layer caching
- **Improvement Opportunity**: ~30-40% speed improvement possible

#### 5️⃣ **JTBD 1: <100ms Receipt Generation with 99.95% Success**
- **Why Hard**: Cryptographic operations are CPU-bound. High volume (33+ ops/sec) = high crypto load.
- **Constraint**: CPU throughput for hash operations.
- **Risk Level**: MEDIUM - Usually achievable on modern hardware, but at scale requires optimization
- **Typical Failure Mode**: Crypto operation slow (algorithm choice), or too many ops/sec for node
- **Mitigation**:
  - Optimize Merkle tree construction (use efficient data structures)
  - Consider batch hashing (amortize crypto cost)
  - Hardware crypto acceleration (if available)
  - Incremental hash updates
- **Improvement Opportunity**: ~2-3x improvement with careful implementation

### Tier 3: Easier (Well-Understood, Good Baseline)

#### 6️⃣ **JTBD 4: Diagnostic Reports in <30 seconds**
- **Why Easier**: Diagnostics are reads, not writes. Can be cached/optimized easily.
- **Constraint**: Network round-trip time to nodes.
- **Risk Level**: LOW - Usually meets SLO with simple optimization
- **Typical Failure Mode**: One slow node, or query aggregation inefficiency
- **Mitigation**:
  - Parallel node queries (don't wait for slowest)
  - Metrics caching (TTL: 1-5 seconds)
  - Async log aggregation (don't block on full logs)
- **Improvement Opportunity**: ~50% speed improvement possible (parallel aggregation)

---

## Risk Assessment Matrix

| SLO | Impact (1-10) | Likelihood (1-10) | RPN | Mitigation Priority |
|---|---|---|---|---|
| Receipt gen | 8 | 3 | 24 | MEDIUM |
| Verification | 10 | 2 | 20 | MEDIUM |
| Failover | 10 | 3 | 30 | HIGH |
| Diagnostics | 5 | 4 | 20 | LOW |
| Throughput | 8 | 5 | 40 | HIGH |
| Deployment | 6 | 4 | 24 | MEDIUM |

**RPN Interpretation:**
- **RPN > 30**: Address immediately (failover, throughput)
- **RPN 20-30**: Address in next sprint (receipt gen, deployment)
- **RPN < 20**: Monitor but lower priority (diagnostics, verification)

---

## Implementation Roadmap

### Phase 1 (Month 1): Baseline & Monitoring
- [ ] Implement SLO instrumentation (all 6 JTBDs)
- [ ] Create monitoring dashboards (Grafana/Datadog)
- [ ] Establish baseline metrics (run benchmarks 10 times)
- [ ] Document current vs target SLO gaps
- **Target**: Understand where we stand

### Phase 2 (Month 2): Critical Path Fixes
- [ ] Optimize JTBD 5 (throughput) - highest impact
- [ ] Optimize JTBD 3 (failover) - highest risk
- [ ] Tune Raft parameters for faster election
- **Target**: Hit the two hardest SLOs

### Phase 3 (Month 3): Remaining SLOs
- [ ] Optimize JTBD 1 (receipt generation)
- [ ] Improve JTBD 6 (deployment pipeline)
- [ ] Add redundancy for JTBD 2 (verification)
- [ ] Validate JTBD 4 (diagnostics already likely met)
- **Target**: 95%+ SLO compliance across all 6

### Phase 4 (Month 4): Hardening
- [ ] Extend baseline measurements (monthly trend)
- [ ] Chaos engineering tests (random failures)
- [ ] Network condition testing (jitter, partition)
- [ ] Load test regressions (catch performance drift)
- **Target**: Sustained compliance + confidence in SLOs

---

## Quarterly Review Process

**Every Quarter**:
1. **Compliance Report**: Measure actual vs target for all 6 JTBDs
2. **Gap Analysis**: Identify which SLOs are under-performing
3. **Root Cause Analysis**: Why did we miss? (infrastructure, code, config)
4. **Action Items**: Specific improvements for next quarter
5. **Alert Review**: Tune alert thresholds based on actual distribution

**Example Quarterly Report**:
```
JTBD-1 Receipt Generation:  ✅ 99.97% compliant (target 99.95%)
JTBD-2 Verification:        ✅ 99.91% compliant (target 99.9%)
JTBD-3 Failover:            ⚠️  98.5% compliant (target 99.99%) - Needs work
JTBD-4 Diagnostics:         ✅ 99.8% compliant (target 100%)
JTBD-5 Throughput:          ⚠️  28 ops/sec avg (target 33) - Under-provisioned
JTBD-6 Deployment:          ✅ 99.8% compliant (target 99.5%)

→ Action: Scale failover testing, increase daemon concurrency
```

---

## Conclusion

These SLOs define the performance contract for @unrdf/daemon in production. Key takeaways:

1. **Throughput (JTBD-5)** and **Failover (JTBD-3)** are the tightest constraints
2. **Zero-loss failover** requires careful Raft tuning and network isolation
3. **33+ ops/sec throughput** requires CPU provisioning and load distribution
4. **Sub-100ms receipts** achievable with optimized crypto and batching
5. **SLO monitoring** is essential - can't optimize what you don't measure

Success means users experience fast, reliable, auditable background operations that scale transparently across clusters.

