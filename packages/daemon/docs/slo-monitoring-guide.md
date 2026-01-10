# SLO Monitoring Guide: Dashboard & Metrics

Complete technical specification for monitoring @unrdf/daemon SLOs in production.

---

## Monitoring Architecture

```
┌─────────────────────────────────────────────────┐
│ @unrdf/daemon Application                       │
│ • Emit operation:success, operation:failure      │
│ • Health checks via getHealth(), getMetrics()   │
└────────────┬────────────────────────────────────┘
             │
┌────────────▼────────────────────────────────────┐
│ OpenTelemetry Instrumentation Layer             │
│ • Span collection (operation latency)            │
│ • Metric recording (throughput, queue depth)    │
│ • Log emission (structured JSON)                │
└────────────┬────────────────────────────────────┘
             │
┌────────────▼────────────────────────────────────┐
│ Metrics Export (Prometheus)                     │
│ • gauge: daemon_operations_active               │
│ • histogram: operation_latency_ms               │
│ • counter: operations_total                     │
└────────────┬────────────────────────────────────┘
             │
┌────────────▼────────────────────────────────────┐
│ Monitoring Stack                                │
│ ├─ Prometheus (time-series DB)                 │
│ ├─ Grafana (dashboards)                        │
│ └─ AlertManager (alerting)                     │
└─────────────────────────────────────────────────┘
```

---

## Metrics Specification

### JTBD-1: Receipt Generation Latency

#### Prometheus Metrics
```
# Histogram: operation_latency_ms (all operations)
operation_latency_ms_bucket{le="5", method="create"} 1234
operation_latency_ms_bucket{le="15", method="create"} 5678
operation_latency_ms_bucket{le="45", method="create"} 9012
operation_latency_ms_bucket{le="100", method="create"} 10000
operation_latency_ms_count{method="create"} 10000
operation_latency_ms_sum{method="create"} 125000

# Gauge: receipt generation metrics
receipt_generation_latency_p50{node="node-1"} 5.2
receipt_generation_latency_p95{node="node-1"} 14.8
receipt_generation_latency_p99{node="node-1"} 42.1
receipt_generation_latency_p99_9{node="node-1"} 98.3

# Counter: receipt success rate
receipts_generated_total{status="success", node="node-1"} 100000
receipts_generated_total{status="failure", node="node-1"} 5
receipts_generated_total{status="collision", node="node-1"} 0
```

#### Grafana Dashboard Panels

**Panel 1: Receipt Latency Percentiles (Last 24h)**
```sql
-- Query 1: P50 latency
histogram_quantile(0.50, rate(operation_latency_ms_bucket{method=~"create|update|delete"}[5m]))

-- Query 2: P95 latency
histogram_quantile(0.95, rate(operation_latency_ms_bucket{method=~"create|update|delete"}[5m]))

-- Query 3: P99 latency
histogram_quantile(0.99, rate(operation_latency_ms_bucket{method=~"create|update|delete"}[5m]))

-- Query 4: P99.9 latency
histogram_quantile(0.999, rate(operation_latency_ms_bucket{method=~"create|update|delete"}[5m]))
```

**Panel 2: Receipt Success Rate (Last 24h)**
```sql
(
  receipts_generated_total{status="success"}
  /
  (receipts_generated_total{status="success"} + receipts_generated_total{status="failure"})
) * 100
```

**Panel 3: Hash Collision Rate (Monthly)**
```sql
(receipts_generated_total{status="collision"} / receipts_generated_total{status="success"}) * 1000000
```

**Panel 4: Latency Heatmap (Last 7 days)**
```sql
rate(operation_latency_ms_bucket[5m])
-- Use Grafana heatmap visualization
```

#### Alert Rules

```yaml
groups:
- name: jtbd1_receipt_generation
  rules:
  - alert: ReceiptLatencyP99High
    expr: histogram_quantile(0.99, rate(operation_latency_ms_bucket[5m])) > 60
    for: 5m
    annotations:
      severity: critical
      summary: "Receipt generation P99 latency > 60ms"

  - alert: ReceiptSuccessRateLow
    expr: (receipts_generated_total{status="success"} / receipts_generated_total) < 0.999
    for: 5m
    annotations:
      severity: critical
      summary: "Receipt success rate < 99.9%"

  - alert: ReceiptHashCollision
    expr: receipts_generated_total{status="collision"} > 0
    for: 1m
    annotations:
      severity: critical
      summary: "Hash collision detected"
```

---

### JTBD-2: Verification Latency

#### Prometheus Metrics
```
# Histogram: verification_latency_seconds
verification_latency_seconds_bucket{le="5"} 1200
verification_latency_seconds_bucket{le="45"} 4500
verification_latency_seconds_bucket{le="120"} 9500
verification_latency_seconds_bucket{le="300"} 10000
verification_latency_seconds_count 10000
verification_latency_seconds_sum 125000

# Gauge: verification accuracy
verification_success_rate{node="node-1"} 0.999
verification_false_positive_rate{node="node-1"} 0.00001
verification_false_negative_rate{node="node-1"} 0.00001
ledger_sync_time_seconds{node="node-1"} 28.5

# Counter: verification events
verifications_total{status="pass", node="node-1"} 99900
verifications_total{status="fail", node="node-1"} 100
verifications_total{status="false_positive", node="node-1"} 0
verifications_total{status="false_negative", node="node-1"} 0
```

#### Grafana Dashboard Panels

**Panel 1: Verification Latency (All Percentiles)**
```sql
histogram_quantile(0.50, rate(verification_latency_seconds_bucket[5m])) as p50,
histogram_quantile(0.95, rate(verification_latency_seconds_bucket[5m])) as p95,
histogram_quantile(0.99, rate(verification_latency_seconds_bucket[5m])) as p99,
histogram_quantile(0.999, rate(verification_latency_seconds_bucket[5m])) as p99_9
```

**Panel 2: Verification Success Rate (24h)**
```sql
(verifications_total{status="pass"} / verifications_total) * 100
```

**Panel 3: False Positive/Negative Trend**
```sql
rate(verifications_total{status=~"false_positive|false_negative"}[1h])
```

**Panel 4: Ledger Sync Time Distribution**
```sql
histogram_quantile(0.95, rate(ledger_sync_time_seconds_bucket[5m]))
```

#### Alert Rules

```yaml
groups:
- name: jtbd2_verification
  rules:
  - alert: VerificationLatencyP99High
    expr: histogram_quantile(0.99, rate(verification_latency_seconds_bucket[5m])) > 150
    for: 5m
    annotations:
      severity: critical
      summary: "Verification P99 latency > 150s"

  - alert: VerificationSuccessRateLow
    expr: (verifications_total{status="pass"} / verifications_total) < 0.995
    for: 5m
    annotations:
      severity: critical
      summary: "Verification success rate < 99.5%"

  - alert: FalsePositiveDetected
    expr: verifications_total{status="false_positive"} > 0
    for: 1m
    annotations:
      severity: critical
      summary: "False positive verification detected"

  - alert: LedgerSyncDelayed
    expr: ledger_sync_time_seconds > 60
    for: 5m
    annotations:
      severity: warning
      summary: "Ledger sync time > 60s"
```

---

### JTBD-3: Failover Recovery Time

#### Prometheus Metrics
```
# Gauge: failover timing breakdown
failover_detection_time_seconds{node="node-1"} 8.2
failover_election_time_seconds{node="node-1"} 11.5
failover_state_sync_time_seconds{node="node-1"} 28.3
failover_total_recovery_time_seconds{node="node-1"} 48.0

# Counter: failover events
failover_events_total{outcome="success", node="node-1"} 15
failover_events_total{outcome="failure", node="node-1"} 0
failover_events_total{outcome="data_loss", node="node-1"} 0

# Gauge: cluster consensus state
raft_leader_elected_seconds{cluster="prod"} 0 (0 = leader exists)
raft_follower_behind_entries{node="node-2"} 0
raft_last_heartbeat_seconds{from_leader="node-1", to_node="node-2"} 0.15

# Gauge: operation continuity
operations_lost_during_failover{failover_id="123"} 0
operations_reexecuted{failover_id="123"} 3
```

#### Grafana Dashboard Panels

**Panel 1: Failover Recovery Time Breakdown (Last failover)**
```
Bar chart showing:
- Detection time (blue)
- Election time (green)
- Sync time (orange)
- Total time (red line at 45s target)
```

**Panel 2: Failover Success Rate (Monthly)**
```sql
(failover_events_total{outcome="success"} /
 (failover_events_total{outcome="success"} + failover_events_total{outcome="failure"})) * 100
```

**Panel 3: Data Loss Events (24h)**
```sql
sum(rate(failover_events_total{outcome="data_loss"}[1d]))
-- Alert if > 0
```

**Panel 4: Cluster Health (Real-time)**
```sql
-- For each node:
raft_follower_behind_entries{node="node-x"}
raft_last_heartbeat_seconds{to_node="node-x"}
```

#### Alert Rules

```yaml
groups:
- name: jtbd3_failover
  rules:
  - alert: FailoverRecoveryTooSlow
    expr: failover_total_recovery_time_seconds > 60
    for: 1m
    annotations:
      severity: critical
      summary: "Failover recovery > 60s"

  - alert: FailoverFailed
    expr: failover_events_total{outcome="failure"} > 0
    for: 1m
    annotations:
      severity: critical
      summary: "Failover failed"

  - alert: DataLostDuringFailover
    expr: failover_events_total{outcome="data_loss"} > 0
    for: 1m
    annotations:
      severity: critical
      summary: "Data loss during failover"

  - alert: FollowerBehindLogEntries
    expr: raft_follower_behind_entries > 100
    for: 2m
    annotations:
      severity: warning
      summary: "Follower behind on log entries"

  - alert: NoLeaderElected
    expr: raft_leader_elected_seconds > 30
    for: 5m
    annotations:
      severity: critical
      summary: "Leader election taking > 30s"
```

---

### JTBD-4: Diagnostic Latency

#### Prometheus Metrics
```
# Histogram: diagnostic operation latencies
health_check_latency_ms_bucket{le="50"} 8500
health_check_latency_ms_bucket{le="100"} 9800
health_check_latency_ms_count 10000

metrics_query_latency_seconds_bucket{le="3"} 95
metrics_query_latency_seconds_bucket{le="5"} 100
metrics_query_latency_seconds_count 100

diagnostic_report_latency_seconds_bucket{le="30"} 98
diagnostic_report_latency_seconds_bucket{le="60"} 100
diagnostic_report_latency_seconds_count 100

log_aggregation_latency_seconds_bucket{le="45"} 96
log_aggregation_latency_seconds_bucket{le="90"} 100
log_aggregation_latency_seconds_count 100

# Gauge: diagnostic completeness
diagnostic_completeness_percent{report_id="xyz"} 100
diagnostic_accuracy_percent{report_id="xyz"} 99.8

# Counter: diagnostic events
diagnostics_generated_total{status="complete"} 1000
diagnostics_generated_total{status="incomplete"} 5
diagnostics_generated_total{status="timeout"} 2
```

#### Grafana Dashboard Panels

**Panel 1: Diagnostic Operation Latencies (Last 24h)**
```
Four line charts:
1. Health check response time (ms)
2. Metrics query latency (sec)
3. Full diagnostic report (sec)
4. Log aggregation (sec)
```

**Panel 2: Diagnostic Completeness (Last 24h)**
```sql
(diagnostics_generated_total{status="complete"} / diagnostics_generated_total) * 100
```

**Panel 3: Diagnostic Accuracy Trend (Last 30 days)**
```sql
avg_over_time(diagnostic_accuracy_percent[1d])
```

**Panel 4: Component Health Status (Real-time)**
```
Table showing:
- Component name
- Last check time
- Health status
- Response time
- Missing fields (if any)
```

#### Alert Rules

```yaml
groups:
- name: jtbd4_diagnostics
  rules:
  - alert: HealthCheckSlow
    expr: histogram_quantile(0.95, rate(health_check_latency_ms_bucket[5m])) > 100
    for: 5m
    annotations:
      severity: warning
      summary: "Health check response > 100ms"

  - alert: DiagnosticReportTimeout
    expr: histogram_quantile(0.95, rate(diagnostic_report_latency_seconds_bucket[5m])) > 60
    for: 5m
    annotations:
      severity: critical
      summary: "Diagnostic report generation > 60s"

  - alert: DiagnosticIncomplete
    expr: (diagnostics_generated_total{status="incomplete"} / diagnostics_generated_total) > 0.01
    for: 5m
    annotations:
      severity: critical
      summary: "Diagnostic completeness < 99%"

  - alert: DiagnosticAccuracyDegraded
    expr: diagnostic_accuracy_percent < 99
    for: 10m
    annotations:
      severity: warning
      summary: "Diagnostic accuracy < 99%"
```

---

### JTBD-5: Throughput Under Concurrency

#### Prometheus Metrics
```
# Gauge: current throughput
operations_per_second{time_window="5m"} 32.8
operations_per_second{time_window="1h"} 31.5

# Gauge: queue metrics
queue_depth{node="node-1"} 245
max_queue_depth_24h{node="node-1"} 680
queue_wait_time_ms_p50{node="node-1"} 120
queue_wait_time_ms_p95{node="node-1"} 480
queue_wait_time_ms_p99{node="node-1"} 750

# Gauge: concurrent operations
active_operations_count{node="node-1"} 85
max_concurrent_capacity{node="node-1"} 100

# Counter: operation outcomes under load
operations_completed_total{status="success"} 150000
operations_completed_total{status="failure"} 500
operations_completed_total{status="timeout"} 10
operations_completed_total{status="queue_drop"} 0

# Gauge: throughput stability
throughput_variation_percent_24h{node="node-1"} 4.2
throughput_variation_percent_7d{node="node-1"} 6.8
```

#### Grafana Dashboard Panels

**Panel 1: Real-time Throughput (Last 24h)**
```sql
-- Show throughput over time with target line
operations_per_second{time_window="5m"}
-- Overlay horizontal line at y=33
```

**Panel 2: Queue Depth & Wait Time (Real-time)**
```
Left Y-axis: queue_depth (absolute)
Right Y-axis: queue_wait_time_ms_p95 (percentile)
```

**Panel 3: Operation Failure Rate Under Load**
```sql
(
  (operations_completed_total{status="failure"} +
   operations_completed_total{status="timeout"}) /
  operations_completed_total
) * 100
```

**Panel 4: Throughput Stability Index (Daily)**
```sql
avg_over_time(throughput_variation_percent_24h[1d])
-- Alert if > 10%
```

**Panel 5: Concurrency Usage Pattern (Last 7 days)**
```
Heatmap showing:
- X-axis: time
- Y-axis: active operations count
- Color: intensity
```

#### Alert Rules

```yaml
groups:
- name: jtbd5_throughput
  rules:
  - alert: ThroughputBelowTarget
    expr: operations_per_second{time_window="5m"} < 25
    for: 10m
    annotations:
      severity: critical
      summary: "Throughput < 25 ops/sec (target 33)"

  - alert: QueueWaitTimeHigh
    expr: histogram_quantile(0.99, rate(queue_wait_time_ms_bucket[5m])) > 1000
    for: 5m
    annotations:
      severity: warning
      summary: "P99 queue wait time > 1s"

  - alert: QueueDepthHigh
    expr: queue_depth > 800
    for: 5m
    annotations:
      severity: warning
      summary: "Queue depth > 800 (saturated)"

  - alert: OperationFailureRateHigh
    expr: (
      (operations_completed_total{status="failure"} +
       operations_completed_total{status="timeout"}) /
      operations_completed_total
    ) > 0.01
    for: 5m
    annotations:
      severity: critical
      summary: "Operation failure rate > 1%"

  - alert: ThroughputVariabilityHigh
    expr: throughput_variation_percent_24h > 10
    for: 5m
    annotations:
      severity: warning
      summary: "Throughput variation > 10%"
```

---

### JTBD-6: Deployment Time

#### Prometheus Metrics
```
# Histogram: deployment phase durations
deploy_build_time_seconds_bucket{le="30"} 28
deploy_build_time_seconds_bucket{le="45"} 30
deploy_build_time_seconds_count 30

deploy_test_time_seconds_bucket{le="60"} 28
deploy_test_time_seconds_bucket{le="90"} 30
deploy_test_time_seconds_count 30

deploy_artifact_time_seconds_bucket{le="30"} 30
deploy_artifact_time_seconds_bucket{le="45"} 30
deploy_artifact_time_seconds_count 30

deploy_rollout_time_seconds_bucket{le="120"} 28
deploy_rollout_time_seconds_bucket{le="180"} 30
deploy_rollout_time_seconds_count 30

deploy_total_time_seconds_bucket{le="360"} 28
deploy_total_time_seconds_bucket{le="480"} 30
deploy_total_time_seconds_count 30

deploy_rollback_time_seconds_bucket{le="120"} 28
deploy_rollback_time_seconds_bucket{le="180"} 30
deploy_rollback_time_seconds_count 30

# Counter: deployment outcomes
deployments_total{status="success"} 150
deployments_total{status="failure"} 1
deployments_total{status="rollback"} 2

# Gauge: test coverage
deploy_test_coverage_percent{deployment_id="v1.2.3"} 85.2
deploy_test_pass_rate_percent{deployment_id="v1.2.3"} 100
```

#### Grafana Dashboard Panels

**Panel 1: Deployment Time Breakdown (Last 30 deployments)**
```
Stacked bar chart:
- Build time (blue)
- Test time (green)
- Artifact time (yellow)
- Rollout time (orange)
- Total (red line at 360s)
```

**Panel 2: Deployment Success Rate (Monthly)**
```sql
(deployments_total{status="success"} /
 (deployments_total{status="success"} + deployments_total{status="failure"})) * 100
```

**Panel 3: Phase Duration Trend (Last 30 days)**
```
Four line charts:
1. Build time P95
2. Test time P95
3. Rollout time P95
4. Total time P95 (with 360s target line)
```

**Panel 4: Rollback Time Distribution**
```sql
histogram_quantile(0.95, rate(deploy_rollback_time_seconds_bucket[1d]))
```

**Panel 5: Test Coverage Trend**
```sql
avg_over_time(deploy_test_coverage_percent[1d])
```

#### Alert Rules

```yaml
groups:
- name: jtbd6_deployment
  rules:
  - alert: DeploymentSlow
    expr: histogram_quantile(0.95, rate(deploy_total_time_seconds_bucket[1d])) > 480
    for: 5m
    annotations:
      severity: warning
      summary: "Deployment taking > 8 minutes"

  - alert: BuildTimeRegression
    expr: histogram_quantile(0.95, rate(deploy_build_time_seconds_bucket[1d])) > 45
    for: 5m
    annotations:
      severity: warning
      summary: "Build time > 45s (check npm install)"

  - alert: TestExecutionSlow
    expr: histogram_quantile(0.95, rate(deploy_test_time_seconds_bucket[1d])) > 90
    for: 5m
    annotations:
      severity: warning
      summary: "Test execution > 90s"

  - alert: DeploymentFailed
    expr: deployments_total{status="failure"} > 0 or deployments_total{status="rollback"} > 0
    for: 1m
    annotations:
      severity: critical
      summary: "Deployment failure or rollback"

  - alert: RollbackSlow
    expr: histogram_quantile(0.95, rate(deploy_rollback_time_seconds_bucket[1d])) > 180
    for: 5m
    annotations:
      severity: critical
      summary: "Rollback taking > 3 minutes"

  - alert: TestCoverageLow
    expr: deploy_test_coverage_percent < 80
    for: 5m
    annotations:
      severity: warning
      summary: "Test coverage < 80%"
```

---

## Dashboard Layout

### Main SLO Dashboard (Executive View)

```
┌─────────────────────────────────────────────────────────┐
│ @unrdf/daemon SLO Status (Last 30 Days)                │
├─────────────────────────────────────────────────────────┤
│                                                          │
│  JTBD-1: Receipt Gen  ✅ 99.97%  JTBD-4: Diagnostic ✅ 99.8%
│  JTBD-2: Verification ✅ 99.91%  JTBD-5: Throughput  ⚠️ 85%
│  JTBD-3: Failover     ⚠️ 98.5%   JTBD-6: Deployment ✅ 95%
│                                                          │
├─────────────────────────────────────────────────────────┤
│  Overall SLO Compliance: 93.7%  Target: 95%  Gap: -1.3% │
└─────────────────────────────────────────────────────────┘
```

### Operational Dashboard (DevOps View)

```
Left Column:
├─ Real-time Throughput (ops/sec)
├─ Queue Depth
└─ Active Operations

Middle Column:
├─ Latency Percentiles (P50/P95/P99)
├─ Error Rate %
└─ Success Rate %

Right Column:
├─ Cluster Health (Leader/Followers)
├─ Resource Usage (CPU/Memory)
└─ Recent Alerts
```

### Troubleshooting Dashboard (SRE View)

```
Row 1 (Quick Diagnosis):
├─ Which JTBD is breaching? (Top alert)
├─ Root cause (resource bottleneck?)
└─ Recommended action

Row 2 (Details):
├─ 30-day trend for failing JTBD
├─ Recent deployments/config changes
└─ Cluster state at failure time

Row 3 (Recovery):
├─ Failover status (if applicable)
├─ Rollback available (yes/no)
└─ Estimated MTTR
```

---

## Implementation Checklist

- [ ] Export metrics from daemon (OpenTelemetry instrumentation)
- [ ] Configure Prometheus scraping (15s interval)
- [ ] Create all 6 dashboard pages (Grafana)
- [ ] Implement alert rules (AlertManager)
- [ ] Test alerts (send test notifications)
- [ ] Configure on-call escalation
- [ ] Document runbooks for each alert
- [ ] Set up SLO compliance reporting (monthly)
- [ ] Train team on dashboard interpretation
- [ ] Publish dashboard URLs to wiki/docs

---

## Troubleshooting Guide

### "JTBD-5 Throughput below 25 ops/sec"
**Check in order**:
1. `queue_depth` - if > 800, system is saturated
2. `active_operations_count` - if maxed out, increase concurrency
3. `operations_per_second` trend - is it declining? (resource leak?)
4. Node CPU - if > 80%, need more instances

### "JTBD-3 Failover recovery > 60s"
**Check in order**:
1. `failover_detection_time_seconds` - if > 15s, network is slow
2. `raft_last_heartbeat_seconds` - if > 300ms, network jitter
3. Check if nodes are in same datacenter (if not, use aggressive config)
4. Review Raft parameters (heartbeat, election timeout)

### "JTBD-1 Receipt generation failures"
**Check in order**:
1. `receipts_generated_total{status="failure"}` - what's the error?
2. `operation_latency_ms_bucket` - is P99 > 100ms? (may need batching)
3. Disk I/O metrics - is disk write slow?
4. Review recent code changes (crypto algorithm change?)

---

## SLA Reporting Template

**Monthly SLO Compliance Report**

```
Period: [Month] [Year]
Reporting Date: [Date]

┌─────────────────────────────────────────────────────────┐
│ Overall Compliance: [XX.X%]  Target: [95%]  Status: ✅/⚠️/❌
└─────────────────────────────────────────────────────────┘

JTBD-1: Receipt Generation
├─ Compliance: [XX.X%]  Target: 99.95%  Status: [✅/⚠️/❌]
├─ Issues: [List any breaches]
├─ Root Causes: [Analysis]
└─ Actions: [Improvements]

JTBD-2: Verification
├─ Compliance: [XX.X%]  Target: 99.9%   Status: [✅/⚠️/❌]
├─ Issues: [...]
├─ Root Causes: [...]
└─ Actions: [...]

[... repeat for JTBD 3-6 ...]

Next Steps:
├─ [ ] Implement mitigation for breached SLO
├─ [ ] Increase resources (if needed)
├─ [ ] Code optimization (if needed)
└─ [ ] Post-mortem (if critical)
```

