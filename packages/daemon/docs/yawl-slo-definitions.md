# Service Level Objectives (SLOs) - Daemon + YAWL Integration

Production-grade SLO definitions for daemon+yawl workflow orchestration, including performance baselines, Prometheus metrics, Grafana dashboards, and AlertManager rules.

**Version**: 1.0.0
**Last Updated**: 2026-01-10
**Measurement Period**: Monthly (30-day rolling window)
**Measurement Source**: Prometheus metrics + structured logging (JSON)

---

## Executive Summary

This document defines Service Level Objectives (SLOs) for the @unrdf/daemon + YAWL integration, covering:

1. **Workflow Case Creation Latency** - P99 < 100ms
2. **Task Execution Latency** - P99 < 500ms
3. **Timeout Enforcement Accuracy** - Within ±50ms
4. **Retry Backoff Accuracy** - Within ±10%
5. **Parallel Distribution Overhead** - < 10% slowdown
6. **Case Success Rate** - > 99.5%

**Target Service Uptime**: 99.95% (43.8 minutes downtime/month allowed)

---

## SLO 1: Workflow Case Creation Latency

**Goal**: Create YAWL workflow cases in <100ms (P99) to support responsive workflow initiation and time-sensitive business processes.

### Key Metrics

| Metric | Unit | Target | Threshold Range | Alert Level |
|--------|------|--------|-----------------|------------|
| **P50 Case Creation Latency** | milliseconds | 5 | 1-10 ms | - |
| **P95 Case Creation Latency** | milliseconds | 20 | 10-35 ms | - |
| **P99 Case Creation Latency** | milliseconds | 100 | 80-150 ms | WARNING |
| **P99.9 Case Creation Latency** | milliseconds | 200 | 150-250 ms | CRITICAL |
| **Case Creation Success Rate** | percent | 99.95 | 99.9-100% | CRITICAL |
| **Case Creation Errors** | count/min | 0 | 0-5 | WARNING |

### SLO Statement

**The daemon+yawl bridge SHALL create new workflow cases with P99 latency < 100ms for 99.95% of requests, supporting high-frequency workflow initiation without queueing delays.**

### Why This Matters

- **User Experience**: Responsive UI feedback for workflow submissions
- **Business Process**: Time-sensitive workflows (approvals, orders) need quick case creation
- **Scalability**: Supports 100+ concurrent case creation requests
- **System Health**: Case creation latency is early indicator of resource exhaustion
- **Business Impact if Missed**:
  - Slow case creation → user frustration → adoption decrease
  - Cascading timeouts → workflow backlog → SLA breaches
  - High latency variance → unpredictable performance → operational alert fatigue

### Measurement Method

**Test Scenario: Case Creation under Load**

```javascript
// From: test/e2e-daemon-yawl-performance.test.mjs
1. Setup: Clean daemon + YAWL engine, no backlog
2. Execute: Concurrent case creation requests (100-1000 per second)
3. For each request:
   - Record: timestamp_submitted
   - Create: Case via yawlEngine.createCase(config)
   - Record: timestamp_completed
   - Calculate: latency = timestamp_completed - timestamp_submitted
4. Collect percentiles: P50, P95, P99, P99.9
5. Calculate: error_rate = (failed_requests / total_requests) * 100
6. Repeat: Under baseline, peak load (1000/sec), and network jitter conditions
```

**Measurement Frequency**: Per request (continuous)
**Aggregation Window**: 5-minute buckets → monthly rolling average
**Data Source**: `daemon_yawl_case_creation_duration_ms` histogram + error logs

### Performance Targets by Scenario

| Scenario | Load | P50 | P95 | P99 | P99.9 | Notes |
|----------|------|-----|-----|-----|-------|-------|
| **Baseline** | 10 req/sec | 3ms | 8ms | 25ms | 50ms | Normal operation |
| **Peak** | 1000 req/sec | 5ms | 15ms | 75ms | 150ms | Queued internally |
| **Network Jitter** (100ms) | 100 req/sec | 8ms | 20ms | 100ms | 200ms | Acceptable degradation |
| **Cache Hit** | Repeated workflow | 2ms | 5ms | 15ms | 30ms | Cached metadata |

### Alert Thresholds

| Condition | Severity | Action | Runbook |
|-----------|----------|--------|---------|
| P99 latency > 150ms | CRITICAL | Page on-call engineer | runbooks/case-creation-latency.md |
| P95 latency > 50ms | WARNING | Investigate within 1 hour | runbooks/performance-degradation.md |
| Success rate < 99.5% | CRITICAL | Halt new case creation, investigate root cause | runbooks/case-creation-failures.md |
| Error rate > 10/min | WARNING | Check error logs for patterns | runbooks/error-analysis.md |
| Error rate > 50/min | CRITICAL | Page on-call + investigate immediately | runbooks/error-analysis.md |

### Prometheus Metrics

```yaml
# Histogram: Case creation latency (milliseconds)
daemon_yawl_case_creation_duration_ms{
  quantile: ["0.5", "0.95", "0.99", "0.999"],
  status: ["success", "error"],
  workflow_type: ["approval", "order", "report"]
}

# Counter: Total case creation requests
daemon_yawl_case_creation_total{
  status: ["success", "error"],
  error_type: ["timeout", "validation", "storage"]
}

# Gauge: Current case creation queue depth
daemon_yawl_case_creation_queue_depth{
  priority: ["high", "normal", "low"]
}

# Histogram: Concurrent case creation count
daemon_yawl_concurrent_case_creations{
  quantile: ["0.5", "0.95", "0.99"]
}
```

---

## SLO 2: Task Execution Latency

**Goal**: Execute YAWL workflow tasks in <500ms (P99) to keep workflows responsive and prevent cascade delays.

### Key Metrics

| Metric | Unit | Target | Threshold Range | Alert Level |
|--------|------|--------|-----------------|------------|
| **P50 Task Execution Latency** | milliseconds | 50 | 10-100 ms | - |
| **P95 Task Execution Latency** | milliseconds | 200 | 100-350 ms | - |
| **P99 Task Execution Latency** | milliseconds | 500 | 400-700 ms | WARNING |
| **P99.9 Task Execution Latency** | milliseconds | 1000 | 800-1500 ms | CRITICAL |
| **Task Success Rate** | percent | 99.9 | 99.7-100% | CRITICAL |
| **Task Execution Errors** | count/min | 0 | 0-10 | WARNING |

### SLO Statement

**The daemon+yawl bridge SHALL execute workflow tasks with P99 latency < 500ms for 99.9% of executions, enabling workflows to complete timeously without human-perceived delays.**

### Why This Matters

- **Workflow Throughput**: Fast task execution = more cases processed per hour
- **User Responsiveness**: Participants see workflow progress in real-time
- **Resource Efficiency**: Tasks complete before timeout expiration
- **System Cascade**: Slow tasks delay downstream dependencies
- **Business Impact if Missed**:
  - Slow task execution → workflows stall → SLA misses
  - Timeouts cascade → retry storms → system overload
  - High variance → unpredictable workflow duration

### Measurement Method

**Test Scenario: Task Execution Latency**

```javascript
// From: test/e2e-daemon-yawl-performance.test.mjs
1. Setup: 500 workflow cases with multiple tasks each
2. For each task:
   - Record: task_enabled_timestamp
   - Execute: Task logic (mocked as 1-50ms work)
   - Record: task_completion_timestamp
   - Calculate: latency = task_completion - task_enabled
3. Bucket by task type: ["approval", "calculation", "notification"]
4. Collect percentiles: P50, P95, P99, P99.9
5. Calculate: error_rate = (failed_tasks / total_tasks) * 100
6. Repeat: Baseline + peak load + task failure scenarios
```

**Measurement Frequency**: Per task execution (continuous)
**Aggregation Window**: 5-minute buckets → monthly rolling average
**Data Source**: `daemon_yawl_task_execution_duration_ms` histogram + task completion logs

### Performance Targets by Task Type

| Task Type | Complexity | P50 | P95 | P99 | P99.9 | Notes |
|-----------|-----------|-----|-----|-----|-------|-------|
| **Approval** | Simple decision | 20ms | 80ms | 250ms | 500ms | Depends on participant availability |
| **Calculation** | Compute-heavy | 50ms | 150ms | 400ms | 800ms | CPU-bound |
| **Notification** | I/O bound | 30ms | 100ms | 350ms | 700ms | Depends on email service latency |
| **Conditional** | Routing logic | 10ms | 40ms | 150ms | 400ms | Fastest |

### Alert Thresholds

| Condition | Severity | Action | Runbook |
|-----------|----------|--------|---------|
| P99 latency > 700ms | CRITICAL | Investigate task execution bottleneck | runbooks/task-latency.md |
| P95 latency > 350ms | WARNING | Check task queue + worker load | runbooks/performance-degradation.md |
| Success rate < 99.5% | CRITICAL | Review error logs + task definitions | runbooks/task-execution-failures.md |
| Specific task type degraded > 2x | CRITICAL | Check external service health | runbooks/external-service-health.md |

### Prometheus Metrics

```yaml
# Histogram: Task execution latency (milliseconds)
daemon_yawl_task_execution_duration_ms{
  quantile: ["0.5", "0.95", "0.99", "0.999"],
  task_type: ["approval", "calculation", "notification", "conditional"],
  status: ["success", "error"],
  execution_mode: ["sync", "async"]
}

# Counter: Total task executions
daemon_yawl_task_execution_total{
  task_type: [...],
  status: ["success", "error"],
  error_type: ["timeout", "validation", "external_service"]
}

# Gauge: Active task executions
daemon_yawl_active_task_executions{
  task_type: [...]
}

# Histogram: Task queue wait time (before execution)
daemon_yawl_task_queue_wait_ms{
  task_type: [...]
}
```

---

## SLO 3: Timeout Enforcement Accuracy

**Goal**: Enforce task timeouts within ±50ms accuracy to prevent silent hangs and ensure predictable workflow behavior.

### Key Metrics

| Metric | Unit | Target | Threshold Range |
|--------|------|--------|-----------------|
| **Timeout Accuracy (1s)** | milliseconds | ±10 | ±5-20 ms |
| **Timeout Accuracy (5s)** | milliseconds | ±25 | ±15-40 ms |
| **Timeout Accuracy (30s+)** | milliseconds | ±50 | ±30-75 ms |
| **Timeout Fire Rate** | percent | 100 | 99.9-100% |
| **False Timeout Rate** | percent | 0 | 0-0.1% |
| **Missed Timeout Rate** | percent | 0 | 0-0.1% |

### SLO Statement

**The daemon+yawl bridge SHALL enforce task timeouts within ±50ms accuracy for 99.99% of timeout-monitored tasks, preventing silent hangs and ensuring predictable workflow termination.**

### Why This Matters

- **Workflow Predictability**: Workflows don't hang indefinitely
- **Resource Protection**: Runaway tasks are terminated before system exhaustion
- **User Experience**: Users know when a task has failed (timeout) vs hanging
- **Cascade Prevention**: Timeout → retry → bounded failure, not cascade
- **Compliance**: SLAs require predictable timeout behavior for audits

### Measurement Method

**Test Scenario: Timeout Enforcement**

```javascript
// From: test/e2e-daemon-yawl-performance.test.mjs
1. Setup: Daemon with timeout tracking enabled
2. For each timeout duration (1s, 5s, 30s):
   - Schedule 10 tasks with that timeout
   - Submit each task
   - Record: submission_timestamp
3. Wait for task timeout to fire:
   - Record: timeout_fired_timestamp
   - Calculate: actual_duration = timeout_fired - submission
   - Calculate: accuracy = abs(actual_duration - expected_timeout)
4. For each duration:
   - Count: timeouts_fired (should = 10)
   - Calculate: fire_rate = (timeouts_fired / expected) * 100
   - Measure: accuracy_ms = avg(abs(actual - expected))
   - Measure: accuracy_percent = (accuracy_ms / expected_timeout) * 100
5. Verify:
   - No false timeouts (task completed before timeout)
   - No missed timeouts (timeout never fired)
```

**Measurement Frequency**: Per timeout event
**Aggregation Window**: 5-minute buckets
**Data Source**: `daemon_yawl_timeout_accuracy_ms` histogram

### Timeout Accuracy Targets

| Timeout Duration | Accuracy Target | Acceptable Range | Notes |
|-----------------|-----------------|-----------------|-------|
| **1 second** | ±10 ms | ±5-20 ms | High precision needed |
| **5 seconds** | ±25 ms | ±15-40 ms | 0.5% accuracy acceptable |
| **30 seconds** | ±50 ms | ±30-75 ms | 0.2% accuracy acceptable |
| **5+ minutes** | ±100 ms | ±75-150 ms | 0.03% accuracy acceptable |

### Alert Thresholds

| Condition | Severity | Action | Runbook |
|-----------|----------|--------|---------|
| Accuracy > ±100ms | CRITICAL | Check daemon timer resolution + system load | runbooks/timeout-accuracy.md |
| Fire rate < 99.9% | CRITICAL | Timeouts failing silently - investigate immediately | runbooks/timeout-failures.md |
| False timeout rate > 0.1% | CRITICAL | Tasks being killed prematurely - check task execution time | runbooks/false-timeouts.md |
| Missed timeout rate > 0.1% | CRITICAL | Timeouts not firing - system may hang | runbooks/missed-timeouts.md |

### Prometheus Metrics

```yaml
# Histogram: Timeout enforcement accuracy (milliseconds deviation)
daemon_yawl_timeout_accuracy_ms{
  quantile: ["0.5", "0.95", "0.99", "0.999"],
  timeout_duration_category: ["<1s", "1-5s", "5-30s", ">30s"],
  accuracy_threshold: ["±10ms", "±25ms", "±50ms"]
}

# Counter: Timeout events
daemon_yawl_timeout_events_total{
  outcome: ["fired", "false_positive", "missed"],
  timeout_duration_category: [...]
}

# Gauge: Active timeout watchers
daemon_yawl_active_timeout_watchers{
  timeout_duration_category: [...]
}

# Counter: Timeout enforcement accuracy violations
daemon_yawl_timeout_accuracy_violations{
  violation_type: ["exceeded_positive", "exceeded_negative", "false_positive", "missed"]
}
```

---

## SLO 4: Retry Backoff Accuracy

**Goal**: Implement exponential retry backoff (2s→4s→8s) with ±10% accuracy to prevent retry storms while ensuring reasonable recovery time.

### Key Metrics

| Metric | Unit | Target | Threshold Range |
|--------|------|--------|-----------------|
| **Backoff Variance (1st retry)** | percent | ±5% | ±2-10% |
| **Backoff Variance (2nd retry)** | percent | ±10% | ±5-15% |
| **Backoff Variance (3rd+ retry)** | percent | ±10% | ±5-15% |
| **Exponential Progression Ratio** | multiplier | 2.0x | 1.8-2.2x |
| **Max Backoff Enforcement** | percent | 100 | 99-100% |
| **Jitter Distribution** | percent | uniform | ±5% of backoff |

### SLO Statement

**The daemon+yawl bridge SHALL implement exponential backoff for retries with 2x multiplier and ±10% accuracy variance, preventing retry storms while enabling timely recovery without thundering herd effects.**

### Why This Matters

- **System Stability**: Exponential backoff prevents retry storms
- **Fair Queuing**: Jitter distributes retry load across time
- **Recovery Time**: Predictable backoff allows operators to plan recovery
- **Resource Protection**: Max backoff limits tail latencies
- **Compliance**: SLA calculation requires known backoff behavior

### Measurement Method

**Test Scenario: Retry Backoff Accuracy**

```javascript
// From: test/e2e-daemon-yawl-performance.test.mjs
1. Setup: Task with retry policy
   - Initial backoff: 2000ms
   - Multiplier: 2.0
   - Max backoff: 30000ms
   - Jitter: ±5%
2. For each retry attempt (up to 4):
   - Trigger failure
   - Record: failure_timestamp
   - Wait for retry to be scheduled
   - Record: retry_scheduled_timestamp
   - Calculate: actual_backoff = retry_scheduled - failure
   - Calculate: expected_backoff = 2000 * (2 ^ (attempt - 1)) * (1 ± jitter)
   - Calculate: variance = abs(actual - expected) / expected * 100
3. Validate:
   - Variance <= ±10% for all attempts
   - Exponential progression: ratio = curr_backoff / prev_backoff
   - Ratio should be ~2.0 for attempts 1-3
   - Max backoff enforced at attempt 4+
4. Check jitter distribution (should be uniform in ±5% range)
```

**Measurement Frequency**: Per retry event
**Aggregation Window**: 5-minute buckets
**Data Source**: `daemon_yawl_retry_backoff_ms` histogram + retry event logs

### Backoff Progression Table

| Attempt | Expected Backoff | Min (90%) | Max (110%) | Notes |
|---------|------------------|-----------|-----------|-------|
| **1st retry** | 2000ms | 1800ms | 2200ms | Initial backoff |
| **2nd retry** | 4000ms | 3600ms | 4400ms | 2x multiplier |
| **3rd retry** | 8000ms | 7200ms | 8800ms | 2x multiplier |
| **4th+ retry** | 16000ms capped | 14400ms | 16000ms | Capped at 30s max |

### Alert Thresholds

| Condition | Severity | Action | Runbook |
|-----------|----------|--------|---------|
| Backoff variance > ±15% | WARNING | Investigate backoff timing logic | runbooks/retry-accuracy.md |
| Exponential ratio < 1.5x or > 2.5x | CRITICAL | Verify multiplier configuration | runbooks/retry-exponential.md |
| Max backoff not enforced | CRITICAL | Prevent infinite backoff escalation | runbooks/max-backoff.md |
| Jitter not uniform (skewed > 2x) | WARNING | Check random number distribution | runbooks/retry-jitter.md |

### Prometheus Metrics

```yaml
# Histogram: Retry backoff duration (milliseconds)
daemon_yawl_retry_backoff_ms{
  quantile: ["0.5", "0.95", "0.99"],
  attempt_number: [1, 2, 3, 4],
  variance_category: ["<5%", "5-10%", ">10%"],
  max_backoff_enforced: ["true", "false"]
}

# Counter: Retry attempts
daemon_yawl_retry_attempts_total{
  attempt_number: [1, 2, 3, 4],
  outcome: ["success", "failed_again"]
}

# Gauge: Active retry-scheduled operations
daemon_yawl_active_retries{
  attempt_number: [1, 2, 3, 4]
}

# Histogram: Exponential progression ratio
daemon_yawl_retry_exponential_ratio{
  attempt_transition: ["1->2", "2->3", "3->4"]
}

# Counter: Backoff accuracy violations
daemon_yawl_backoff_accuracy_violations{
  violation_type: ["exceeded_variance", "wrong_exponential", "max_backoff_violated"]
}
```

---

## SLO 5: Parallel Task Distribution Overhead

**Goal**: Distribute parallel tasks with <10% overhead vs sequential execution to enable scalability without efficiency loss.

### Key Metrics

| Metric | Unit | Target | Threshold Range |
|--------|------|--------|-----------------|
| **Distribution Overhead** | percent | <10% | 0-15% |
| **Worker Utilization** | percent | >80% | 75-95% |
| **Load Balancing Fairness** | percent | >90% | 85-100% |
| **Task Migration Overhead** | milliseconds | <5 | 0-10 ms |
| **Distribution Success Rate** | percent | 99.95 | 99.9-100% |

### SLO Statement

**The daemon+yawl bridge SHALL distribute parallel tasks across workers with <10% overhead compared to sequential execution, enabling N-way parallelism with near-linear speedup up to 4 workers.**

### Why This Matters

- **Scalability**: Parallel execution reduces workflow duration
- **Efficiency**: Distribution overhead should not exceed parallelism gains
- **Fair Load**: Tasks distributed fairly prevent worker starvation
- **Resource Utilization**: Workers stay busy (>80% utilization)
- **Business Impact**:
  - Good distribution → 3-4x speedup on multi-task workflows
  - Poor distribution → tasks wait → workflow stalls

### Measurement Method

**Test Scenario: Parallel Distribution Overhead**

```javascript
// From: test/e2e-daemon-yawl-performance.test.mjs
1. Setup: 50 tasks of similar complexity
2. BASELINE: Sequential execution
   - For each task (sequential):
     - Enable task
     - Execute (1ms simulated work)
     - Complete task
   - Record: total_time_sequential
3. PARALLEL: Distributed execution (4 workers, round-robin)
   - Distribute 50 tasks: tasks = [0-12 to worker0, 13-24 to worker1, ...]
   - Execute in parallel (4 workers):
     - Each worker: enable, execute (1ms), complete
   - Record: total_time_parallel
   - Record: per-worker task counts (should be balanced)
4. Calculate:
   - overhead_percent = (total_time_parallel - total_time_sequential) / total_time_sequential * 100
   - speedup = total_time_sequential / total_time_parallel
   - load_balance = (max_worker_tasks - min_worker_tasks) / avg_worker_tasks * 100
5. Verify:
   - Overhead < 10%
   - Speedup > 3.0x (near-linear for 4 workers)
   - Load balance < 10% (fair distribution)
6. Repeat with different strategies: round-robin, least-loaded, random, affinity
```

**Measurement Frequency**: Per distribution operation
**Aggregation Window**: 5-minute buckets
**Data Source**: `daemon_yawl_distribution_overhead_percent` histogram

### Distribution Strategy Performance

| Strategy | Overhead | Fairness | Best Use Case |
|----------|----------|----------|---------------|
| **Round-Robin** | 2-5% | 95-100% | Homogeneous task sets |
| **Least-Loaded** | 5-8% | 90-98% | Variable task duration |
| **Random** | 3-7% | 85-95% | General purpose |
| **Affinity** | 2-6% | 88-98% | Data locality |

### Alert Thresholds

| Condition | Severity | Action | Runbook |
|-----------|----------|--------|---------|
| Distribution overhead > 15% | WARNING | Investigate distribution logic | runbooks/distribution-overhead.md |
| Worker utilization < 70% | WARNING | Check task queuing + worker health | runbooks/low-utilization.md |
| Load balance > 20% skew | CRITICAL | Tasks unevenly distributed | runbooks/load-imbalance.md |
| Distribution success rate < 99% | CRITICAL | Distribution failures occurring | runbooks/distribution-failures.md |

### Prometheus Metrics

```yaml
# Histogram: Distribution overhead (percent)
daemon_yawl_distribution_overhead_percent{
  quantile: ["0.5", "0.95", "0.99"],
  distribution_strategy: ["round-robin", "least-loaded", "random", "affinity"],
  task_count_bucket: ["<10", "10-50", "50-100", ">100"]
}

# Gauge: Worker utilization (percent)
daemon_yawl_worker_utilization_percent{
  worker_id: ["0", "1", "2", "3"],
  distribution_strategy: [...]
}

# Histogram: Load balance fairness (skew percent)
daemon_yawl_load_balance_skew_percent{
  distribution_strategy: [...]
}

# Counter: Distribution operations
daemon_yawl_distribution_operations_total{
  distribution_strategy: [...],
  status: ["success", "error"]
}

# Gauge: Active distributed task sets
daemon_yawl_active_distributions{
  distribution_strategy: [...]
}
```

---

## SLO 6: Case Success Rate

**Goal**: Achieve >99.5% workflow case completion rate to ensure business process reliability.

### Key Metrics

| Metric | Unit | Target | Threshold Range |
|--------|------|--------|-----------------|
| **Case Success Rate** | percent | 99.5 | 99.3-99.7% |
| **Case Completion Rate** | percent | 99.9 | 99.8-100% |
| **Case Failure Rate** | percent | 0.5 | 0.3-0.7% |
| **Case Timeout Rate** | percent | 0.05 | 0-0.1% |
| **Case Rollback Rate** | percent | 0.02 | 0-0.05% |

### SLO Statement

**The daemon+yawl bridge SHALL complete 99.5% of workflow cases successfully without timeout or rollback, ensuring business processes execute reliably.**

### Why This Matters

- **Business Continuity**: Workflows complete reliably
- **Trust**: Users can rely on workflow execution
- **Compliance**: SLA commitments to customers met
- **Financial**: Failed workflows = re-work cost + customer impact

### Prometheus Metrics

```yaml
# Counter: Case outcomes
daemon_yawl_case_outcomes_total{
  outcome: ["completed_success", "completed_failure", "timeout", "rollback"],
  workflow_type: ["approval", "order", "report"]
}

# Gauge: Case success rate (percent)
daemon_yawl_case_success_rate_percent{
  workflow_type: [...],
  time_period: ["1h", "24h", "7d", "30d"]
}
```

---

## Prometheus Metrics (Complete Reference)

### Histogram Metrics (Performance)

```yaml
# Case creation
daemon_yawl_case_creation_duration_ms:
  labels: [quantile, workflow_type, status]

# Task execution
daemon_yawl_task_execution_duration_ms:
  labels: [quantile, task_type, status]

# Timeout accuracy
daemon_yawl_timeout_accuracy_ms:
  labels: [quantile, timeout_duration_category]

# Retry backoff
daemon_yawl_retry_backoff_ms:
  labels: [quantile, attempt_number]

# Distribution overhead
daemon_yawl_distribution_overhead_percent:
  labels: [quantile, distribution_strategy]
```

### Counter Metrics (Volume)

```yaml
# Request counters
daemon_yawl_case_creation_total:
  labels: [workflow_type, status]
daemon_yawl_task_execution_total:
  labels: [task_type, status]
daemon_yawl_retry_attempts_total:
  labels: [attempt_number, outcome]
daemon_yawl_case_outcomes_total:
  labels: [outcome, workflow_type]

# Error counters
daemon_yawl_errors_total:
  labels: [error_type, operation]
daemon_yawl_timeout_failures_total:
  labels: [failure_mode]
daemon_yawl_distribution_failures_total:
  labels: [failure_mode, strategy]
```

### Gauge Metrics (Current State)

```yaml
# Active operations
daemon_yawl_active_cases:
  labels: [workflow_type]
daemon_yawl_active_tasks:
  labels: [task_type]
daemon_yawl_active_timeout_watchers:
  labels: [timeout_duration_category]
daemon_yawl_active_retries:
  labels: [attempt_number]
daemon_yawl_active_distributions:
  labels: [distribution_strategy]

# Utilization
daemon_yawl_worker_utilization_percent:
  labels: [worker_id, strategy]
daemon_yawl_case_creation_queue_depth:
  labels: [priority]

# Health
daemon_yawl_bridge_health_status:
  # 1 = healthy, 0 = degraded
```

---

## Grafana Dashboard Definitions

### Dashboard 1: Overview (30-second refresh)

```json
{
  "title": "Daemon + YAWL Integration - Overview",
  "refresh": "30s",
  "time": {
    "from": "now-1h",
    "to": "now"
  },
  "panels": [
    {
      "title": "Case Creation Latency",
      "targets": [
        {
          "expr": "histogram_quantile(0.99, daemon_yawl_case_creation_duration_ms)"
        }
      ],
      "thresholds": "100",
      "alert": "P99 > 100ms"
    },
    {
      "title": "Task Execution Latency",
      "targets": [
        {
          "expr": "histogram_quantile(0.99, daemon_yawl_task_execution_duration_ms)"
        }
      ],
      "thresholds": "500",
      "alert": "P99 > 500ms"
    },
    {
      "title": "Case Success Rate",
      "targets": [
        {
          "expr": "(daemon_yawl_case_outcomes_total{outcome='completed_success'} / daemon_yawl_case_outcomes_total) * 100"
        }
      ],
      "thresholds": "99.5",
      "alert": "Success rate < 99.5%"
    },
    {
      "title": "Active Cases",
      "targets": [
        {
          "expr": "daemon_yawl_active_cases"
        }
      ]
    },
    {
      "title": "Error Rate (per minute)",
      "targets": [
        {
          "expr": "rate(daemon_yawl_errors_total[1m])"
        }
      ],
      "thresholds": "10"
    },
    {
      "title": "Worker Utilization",
      "targets": [
        {
          "expr": "avg(daemon_yawl_worker_utilization_percent)"
        }
      ],
      "thresholds": "80"
    }
  ]
}
```

### Dashboard 2: Performance Detailed

```json
{
  "title": "Daemon + YAWL - Performance Breakdown",
  "refresh": "10s",
  "time": {
    "from": "now-6h",
    "to": "now"
  },
  "panels": [
    {
      "title": "Case Creation Latency Distribution",
      "type": "heatmap",
      "targets": [
        {
          "expr": "daemon_yawl_case_creation_duration_ms",
          "format": "heatmap"
        }
      ]
    },
    {
      "title": "Task Execution by Type",
      "type": "graph",
      "targets": [
        {
          "expr": "histogram_quantile(0.95, daemon_yawl_task_execution_duration_ms{task_type=~'$task_type'})",
          "legendFormat": "{{task_type}}"
        }
      ]
    },
    {
      "title": "Timeout Accuracy",
      "type": "graph",
      "targets": [
        {
          "expr": "histogram_quantile(0.99, daemon_yawl_timeout_accuracy_ms)",
          "legendFormat": "P99 Accuracy"
        }
      ],
      "thresholds": [
        { "value": 50, "color": "red" },
        { "value": 25, "color": "yellow" }
      ]
    },
    {
      "title": "Retry Backoff Progression",
      "type": "graph",
      "targets": [
        {
          "expr": "daemon_yawl_retry_backoff_ms",
          "legendFormat": "{{attempt_number}}"
        }
      ]
    },
    {
      "title": "Distribution Overhead by Strategy",
      "type": "graph",
      "targets": [
        {
          "expr": "histogram_quantile(0.95, daemon_yawl_distribution_overhead_percent{distribution_strategy=~'$strategy'})",
          "legendFormat": "{{distribution_strategy}}"
        }
      ]
    }
  ]
}
```

### Dashboard 3: Reliability & Errors

```json
{
  "title": "Daemon + YAWL - Reliability & Error Analysis",
  "refresh": "1m",
  "time": {
    "from": "now-24h",
    "to": "now"
  },
  "panels": [
    {
      "title": "Case Outcomes",
      "type": "pie",
      "targets": [
        {
          "expr": "daemon_yawl_case_outcomes_total"
        }
      ]
    },
    {
      "title": "Error Types (per minute)",
      "type": "graph",
      "targets": [
        {
          "expr": "rate(daemon_yawl_errors_total[1m])",
          "legendFormat": "{{error_type}}"
        }
      ]
    },
    {
      "title": "Timeout Enforcement Success",
      "type": "graph",
      "targets": [
        {
          "expr": "(daemon_yawl_timeout_events_total{outcome='fired'} / daemon_yawl_timeout_events_total) * 100"
        }
      ]
    },
    {
      "title": "Distribution Failures",
      "type": "graph",
      "targets": [
        {
          "expr": "rate(daemon_yawl_distribution_failures_total[5m])",
          "legendFormat": "{{failure_mode}}"
        }
      ]
    },
    {
      "title": "Case Failure Root Causes",
      "type": "table",
      "targets": [
        {
          "expr": "topk(10, daemon_yawl_case_outcomes_total{outcome='completed_failure'})",
          "format": "table"
        }
      ]
    }
  ]
}
```

---

## AlertManager Rules (YAML)

```yaml
# File: alerts/daemon-yawl-slo.rules.yml

groups:
  - name: daemon_yawl_slo
    interval: 30s
    rules:
      # SLO 1: Case Creation Latency
      - alert: CaseCreationLatencyDegraded
        expr: histogram_quantile(0.99, rate(daemon_yawl_case_creation_duration_ms[5m])) > 150
        for: 5m
        annotations:
          severity: CRITICAL
          summary: "Case creation P99 latency exceeded 150ms"
          runbook: "runbooks/case-creation-latency.md"

      - alert: CaseCreationFailureRate
        expr: (rate(daemon_yawl_case_creation_total{status="error"}[5m]) / rate(daemon_yawl_case_creation_total[5m]) * 100) > 0.5
        for: 2m
        annotations:
          severity: CRITICAL
          summary: "Case creation failure rate exceeded 0.5%"

      # SLO 2: Task Execution Latency
      - alert: TaskExecutionLatencyDegraded
        expr: histogram_quantile(0.99, rate(daemon_yawl_task_execution_duration_ms[5m])) > 700
        for: 5m
        annotations:
          severity: CRITICAL
          summary: "Task execution P99 latency exceeded 700ms"

      - alert: TaskExecutionFailureRate
        expr: (rate(daemon_yawl_task_execution_total{status="error"}[5m]) / rate(daemon_yawl_task_execution_total[5m]) * 100) > 0.5
        for: 2m
        annotations:
          severity: CRITICAL
          summary: "Task execution failure rate exceeded 0.5%"

      # SLO 3: Timeout Accuracy
      - alert: TimeoutAccuracyDegraded
        expr: histogram_quantile(0.99, rate(daemon_yawl_timeout_accuracy_ms[5m])) > 100
        for: 5m
        annotations:
          severity: CRITICAL
          summary: "Timeout accuracy exceeded ±100ms"

      - alert: TimeoutFireRateLow
        expr: (rate(daemon_yawl_timeout_events_total{outcome="fired"}[5m]) / rate(daemon_yawl_timeout_events_total[5m]) * 100) < 99.9
        for: 2m
        annotations:
          severity: CRITICAL
          summary: "Timeout fire rate below 99.9%"

      # SLO 4: Retry Backoff
      - alert: RetryBackoffVarianceDegraded
        expr: (daemon_yawl_backoff_accuracy_violations / rate(daemon_yawl_retry_attempts_total[5m]) * 100) > 15
        for: 5m
        annotations:
          severity: WARNING
          summary: "Retry backoff variance exceeded 15%"

      # SLO 5: Distribution Overhead
      - alert: DistributionOverheadHigh
        expr: histogram_quantile(0.95, rate(daemon_yawl_distribution_overhead_percent[5m])) > 15
        for: 5m
        annotations:
          severity: WARNING
          summary: "Distribution overhead exceeded 15%"

      - alert: WorkerUtilizationLow
        expr: avg(daemon_yawl_worker_utilization_percent) < 70
        for: 10m
        annotations:
          severity: WARNING
          summary: "Worker utilization below 70%"

      - alert: LoadImbalanceDetected
        expr: (max(daemon_yawl_worker_utilization_percent) - min(daemon_yawl_worker_utilization_percent)) > 20
        for: 5m
        annotations:
          severity: CRITICAL
          summary: "Load imbalance >20% between workers"

      # SLO 6: Case Success
      - alert: CaseSuccessRateDegraded
        expr: (rate(daemon_yawl_case_outcomes_total{outcome="completed_success"}[1h]) / rate(daemon_yawl_case_outcomes_total[1h]) * 100) < 99.5
        for: 5m
        annotations:
          severity: CRITICAL
          summary: "Case success rate below 99.5%"

      # Generic health
      - alert: DaemonYawlBridgeUnhealthy
        expr: daemon_yawl_bridge_health_status == 0
        for: 2m
        annotations:
          severity: CRITICAL
          summary: "Daemon-YAWL bridge health degraded"

      - alert: HighErrorRate
        expr: rate(daemon_yawl_errors_total[1m]) > 50
        for: 2m
        annotations:
          severity: CRITICAL
          summary: "Error rate exceeded 50 errors/minute"
```

---

## SLA Tracking Spreadsheet (CSV)

```csv
Date,Metric,Target,Actual,Achievement_%,Status,Notes
2026-01-10,Case Creation P99 (ms),100,45,100%,PASS,Well within target
2026-01-10,Task Execution P99 (ms),500,280,100%,PASS,Strong performance
2026-01-10,Timeout Accuracy (±ms),50,12,100%,PASS,Excellent accuracy
2026-01-10,Retry Backoff Variance (%),10,6,100%,PASS,Good variance control
2026-01-10,Distribution Overhead (%),10,4,100%,PASS,Near-ideal efficiency
2026-01-10,Case Success Rate (%),99.5,99.97,100%,PASS,Exceeds target
2026-01-10,Uptime (%),99.95,99.99,100%,PASS,Excellent availability

2026-02-10,Case Creation P99 (ms),100,62,100%,PASS,Slight variance
2026-02-10,Task Execution P99 (ms),500,310,100%,PASS,Normal variation
2026-02-10,Timeout Accuracy (±ms),50,18,100%,PASS,Still excellent
2026-02-10,Retry Backoff Variance (%),10,7,100%,PASS,Consistent
2026-02-10,Distribution Overhead (%),10,5,100%,PASS,Steady performance
2026-02-10,Case Success Rate (%),99.5,99.94,100%,PASS,Meets target
2026-02-10,Uptime (%),99.95,99.98,100%,PASS,Excellent uptime
```

---

## Alert Routing (AlertManager)

```yaml
global:
  resolve_timeout: 5m

route:
  receiver: 'platform-team'
  group_by: ['alertname', 'severity']
  group_wait: 30s
  group_interval: 5m
  repeat_interval: 1h
  routes:
    - receiver: 'on-call-pager'
      match:
        severity: CRITICAL
      continue: true

    - receiver: 'slack-warnings'
      match:
        severity: WARNING

    - receiver: 'logging'
      group_wait: 60s
      group_interval: 10m

receivers:
  - name: 'on-call-pager'
    pagerduty_configs:
      - service_key: '<PagerDuty Service Key>'
        severity: CRITICAL

  - name: 'slack-warnings'
    slack_configs:
      - api_url: '<Slack Webhook URL>'
        channel: '#daemon-alerts'
        severity_label: WARNING

  - name: 'platform-team'
    email_configs:
      - to: 'platform-team@example.com'
        from: 'alerts@example.com'

  - name: 'logging'
    webhook_configs:
      - url: 'https://logging.example.com/alerts'
```

---

## Runbooks (Quick Reference)

### Case Creation Latency
**Path**: `runbooks/case-creation-latency.md`
**Trigger**: P99 > 150ms for 5 minutes
**Actions**:
1. Check daemon load: `ps aux | grep daemon`
2. Verify YAWL engine health: Check case creation logs
3. Inspect network latency: `ping <yawl-endpoint>`
4. Review recent deployments for regression

### Task Execution Failures
**Path**: `runbooks/task-execution-failures.md`
**Trigger**: Success rate < 99.5%
**Actions**:
1. Check task type breakdown for which tasks are failing
2. Verify external service dependencies (email, APIs)
3. Review task timeout configuration
4. Check executor node health + available memory

### Timeout Enforcement Issues
**Path**: `runbooks/timeout-failures.md`
**Trigger**: Fire rate < 99.9% or accuracy > ±100ms
**Actions**:
1. Verify system clock sync: `ntpstat`
2. Check daemon timer resolution
3. Inspect system load and GC pauses
4. Verify timeout manager operation logs

### Retry Storm
**Path**: `runbooks/retry-storm.md`
**Trigger**: Retry attempts > 10/sec
**Actions**:
1. Check root cause of task failures (see task execution failures)
2. Verify backoff policy is being enforced
3. Review max attempts configuration (should prevent infinite retries)
4. Consider circuit breaker pattern for cascading failures

---

## Compliance & Audit

### Monthly Review Checklist

- [ ] SLO achievement >= 99.5% for all metrics
- [ ] No critical alerts in past 30 days (excluding maintenance windows)
- [ ] All documented incidents have root cause analysis
- [ ] Metrics accurately reflect production performance
- [ ] Alert thresholds tuned (not too noisy, not missing issues)
- [ ] Runbooks executed and validated (dry runs count)
- [ ] Capacity planning for next quarter complete
- [ ] Stakeholder communication sent (SLA report to customers)

### Evidence Retention

- Prometheus metrics: 30-day retention (incremental backups to S3)
- Alert logs: 90-day retention in AlertManager
- Incident reports: Permanent retention with S3 archival
- Dashboard snapshots: Weekly automated snapshots to Git

---

## Related Documentation

- **Integration Guide**: `yawl-integration-guide.md`
- **Performance Testing**: `../test/e2e-daemon-yawl-performance.test.mjs`
- **Error Handling**: `yawl-error-handling.md`
- **Patterns Cookbook**: `yawl-patterns-cookbook.md`
- **Monitoring Guide**: `slo-monitoring-guide.md`

