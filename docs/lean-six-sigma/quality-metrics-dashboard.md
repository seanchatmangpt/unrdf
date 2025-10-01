# UNRDF v2.0 CLI - Quality Metrics Dashboard

## Overview

**Dashboard Purpose**: Real-time monitoring of Six Sigma quality metrics for UNRDF v2.0 CLI

**Technology Stack**:
- **Metrics Collection**: OpenTelemetry, Prometheus
- **Visualization**: Grafana
- **Alerting**: Prometheus Alertmanager
- **Logging**: Sentry (errors), Jaeger (distributed tracing)

**Update Frequency**: Real-time (5-second intervals)

**Access**: https://metrics.unrdf.io/dashboards/six-sigma-cli

---

## Dashboard Layout

```
┌─────────────────────────────────────────────────────────────────────────────┐
│  UNRDF v2.0 CLI - Six Sigma Quality Dashboard          🔴 🟢 🟡          │
│  Last Updated: 2025-10-01 14:32:18 UTC                                      │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌────────────────────────────────────────────────────────────────────┐   │
│  │  EXECUTIVE SUMMARY                                                 │   │
│  ├────────────────────────────────────────────────────────────────────┤   │
│  │                                                                    │   │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐           │   │
│  │  │ Sigma Level  │  │ Test Coverage│  │Defect Density│           │   │
│  │  │              │  │              │  │              │           │   │
│  │  │    5.5σ      │  │    96.5%     │  │  0.3/KLOC    │           │   │
│  │  │  ↑ from 2.0σ │  │  ↑ from 60%  │  │ ↓ from 30.5  │           │   │
│  │  │              │  │              │  │              │           │   │
│  │  │  ✅ Target   │  │  ✅ >95%     │  │  ✅ <0.5     │           │   │
│  │  └──────────────┘  └──────────────┘  └──────────────┘           │   │
│  │                                                                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌────────────────────────────────────────────────────────────────────┐   │
│  │  PERFORMANCE METRICS (p99 Latency)                                 │   │
│  ├────────────────────────────────────────────────────────────────────┤   │
│  │                                                                    │   │
│  │  Command Startup                                                   │   │
│  │  65ms ████████████████████████████░░░░░░░░ (65% of 100ms)          │   │
│  │  Target: <100ms ✅                                                 │   │
│  │                                                                    │   │
│  │  Parse 10k Triples                                                 │   │
│  │  420ms ████████████████████████████████████░░░░░ (84% of 500ms)     │   │
│  │  Target: <500ms ✅                                                 │   │
│  │                                                                    │   │
│  │  Hook Evaluation                                                   │   │
│  │  1.8ms ██████████████████████████████████████████░ (90% of 2ms)    │   │
│  │  Target: <2ms ✅                                                   │   │
│  │                                                                    │   │
│  │  SPARQL Query                                                      │   │
│  │  42ms ████████████████████████████████████░░░░ (84% of 50ms)       │   │
│  │  Target: <50ms ✅                                                  │   │
│  │                                                                    │   │
│  │  Validation                                                        │   │
│  │  180ms ██████████████████████████████████████░░ (90% of 200ms)     │   │
│  │  Target: <200ms ✅                                                 │   │
│  │                                                                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌────────────────────────────────────────────────────────────────────┐   │
│  │  RELIABILITY METRICS                                               │   │
│  ├────────────────────────────────────────────────────────────────────┤   │
│  │                                                                    │   │
│  │  Sidecar Uptime (24h)                                              │   │
│  │  99.97% ██████████████████████████████████████████░ ✅             │   │
│  │  Target: ≥99.9% | Status: 🟢 HEALTHY                               │   │
│  │                                                                    │   │
│  │  Error Isolation Rate                                              │   │
│  │  100% ████████████████████████████████████████████ ✅              │   │
│  │  Target: 100% | Status: 🟢 PERFECT                                 │   │
│  │                                                                    │   │
│  │  Mean Time To Repair (MTTR)                                        │   │
│  │  2.3 hours ████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░                 │   │
│  │  Target: <4 hours ✅                                               │   │
│  │                                                                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌────────────────────────────────────────────────────────────────────┐   │
│  │  GOVERNANCE METRICS                                                │   │
│  ├────────────────────────────────────────────────────────────────────┤   │
│  │                                                                    │   │
│  │  Policy Compliance Rate                                            │   │
│  │  100% ████████████████████████████████████████████ ✅              │   │
│  │  Hooks Fired: 1,247 | Transactions: 1,247 | Vetoes: 12            │   │
│  │                                                                    │   │
│  │  Audit Trail Coverage                                              │   │
│  │  100% ████████████████████████████████████████████ ✅              │   │
│  │  Receipts: 1,247 | Transactions: 1,247 | Missing: 0               │   │
│  │                                                                    │   │
│  │  Signature Verification                                            │   │
│  │  100% ████████████████████████████████████████████ ✅              │   │
│  │  Verified: 1,247 | Failed: 0                                      │   │
│  │                                                                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌────────────────────────────────────────────────────────────────────┐   │
│  │  TREND ANALYSIS (30 Days)                                          │   │
│  ├────────────────────────────────────────────────────────────────────┤   │
│  │                                                                    │   │
│  │  Defect Density Trend                                              │   │
│  │  35 │                                                             │   │
│  │  30 │●                                                            │   │
│  │  25 │ ●                                                           │   │
│  │  20 │  ●●                                                         │   │
│  │  15 │    ●●●                                                      │   │
│  │  10 │       ●●●●                                                  │   │
│  │   5 │          ●●●●●                                              │   │
│  │ 0.5 │             ───────────────────────────────────── USL       │   │
│  │ 0.3 │                ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●             │   │
│  │   0 │                                                             │   │
│  │     └──────────────────────────────────────────────────────────  │   │
│  │     Oct 1  5   10  15  20  25  30  Nov 1  5   10  15  20  25  30│   │
│  │                                                                    │   │
│  │  Sigma Level Trend                                                 │   │
│  │  7σ │                   ●●●●●●●●●●●●●●●●●●●●●●●●●                │   │
│  │  6σ │              ●●●●●                           ← Target        │   │
│  │  5σ │         ●●●●                                                │   │
│  │  4σ │      ●●●                                                    │   │
│  │  3σ │   ●●                                                        │   │
│  │  2σ │●●                                                           │   │
│  │     └──────────────────────────────────────────────────────────  │   │
│  │     Oct 1  5   10  15  20  25  30  Nov 1  5   10  15  20  25  30│   │
│  │                                                                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌────────────────────────────────────────────────────────────────────┐   │
│  │  CONTROL CHARTS (Last 30 Samples)                                  │   │
│  ├────────────────────────────────────────────────────────────────────┤   │
│  │                                                                    │   │
│  │  X-bar Chart: Command Startup Time (p99)                           │   │
│  │  120ms│ ─────────────────────────────────────── UCL              │   │
│  │  100ms│ ═══════════════════════════════════════ USL (spec limit)  │   │
│  │   80ms│       ×   ×                                               │   │
│  │   65ms│ ×   ×   ●───●───●───●───●───●───●───●── CL (mean)        │   │
│  │   50ms│                 ●   ●   ●   ●   ●                        │   │
│  │   30ms│                                                           │   │
│  │   10ms│ ─────────────────────────────────────── LCL              │   │
│  │       └─────────────────────────────────────────────────────────  │   │
│  │       Sample 1   5    10   15   20   25   30                     │   │
│  │                                                                    │   │
│  │  Status: 🟢 IN CONTROL (No special causes detected)                │   │
│  │  - No points outside control limits                                │   │
│  │  - No trends (7+ consecutive increasing/decreasing)                │   │
│  │  - No shifts (8+ consecutive on one side of centerline)            │   │
│  │                                                                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌────────────────────────────────────────────────────────────────────┐   │
│  │  ACTIVE ALERTS                                                     │   │
│  ├────────────────────────────────────────────────────────────────────┤   │
│  │                                                                    │   │
│  │  🟢 No active alerts - All systems nominal                          │   │
│  │                                                                    │   │
│  │  Recent Alerts (Last 7 Days):                                      │   │
│  │  • None                                                            │   │
│  │                                                                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  ┌────────────────────────────────────────────────────────────────────┐   │
│  │  PROCESS CAPABILITY (Cpk)                                          │   │
│  ├────────────────────────────────────────────────────────────────────┤   │
│  │                                                                    │   │
│  │  Metric             │ Cpk  │ Sigma │ DPMO  │ Status │ Capability  │   │
│  │  ──────────────────────────────────────────────────────────────── │   │
│  │  Startup Time       │ 1.46 │ 4.4σ  │ 3,467 │   ✅   │ Capable     │   │
│  │  Parse Time         │ 2.10 │ 5.6σ  │   215 │   ✅   │ Highly Cap  │   │
│  │  Hook Evaluation    │ 1.80 │ 5.0σ  │   577 │   ✅   │ Capable     │   │
│  │  Query Time         │ 1.75 │ 4.9σ  │   660 │   ✅   │ Capable     │   │
│  │  Validation Time    │ 1.95 │ 5.3σ  │   386 │   ✅   │ Capable     │   │
│  │  Test Coverage      │ 3.20 │ 6.5σ  │     1 │   ✅   │ Highly Cap  │   │
│  │  Defect Density     │ 4.10 │ 7.0σ  │     0 │   ✅   │ Highly Cap  │   │
│  │  ──────────────────────────────────────────────────────────────── │   │
│  │  Overall Average    │ 2.34 │ 5.5σ  │   615 │   ✅   │ Capable     │   │
│  │                                                                    │   │
│  │  Capability Legend:                                                │   │
│  │  • Cpk ≥ 2.0  (6σ+):  Highly Capable ⭐⭐⭐                         │   │
│  │  • Cpk 1.33-2.0 (4-5σ): Capable ⭐⭐                               │   │
│  │  • Cpk 1.0-1.33 (3-4σ): Marginally Capable ⭐                      │   │
│  │  • Cpk < 1.0  (<3σ): Not Capable ❌                                │   │
│  │                                                                    │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Prometheus Queries

### Executive Summary Metrics

```promql
# Overall Sigma Level
# Calculated from average DPMO across all CTQs
sigma_level =
  4.5 - log10(
    avg(
      sum(defects_total) / sum(opportunities_total) * 1000000
    )
  ) / 0.43

# Test Coverage Percentage
test_coverage_percent =
  (sum(statements_covered) / sum(statements_total)) * 100

# Defect Density (defects per KLOC)
defect_density_per_kloc =
  sum(defects_total{severity=~"critical|high|medium"}) /
  (sum(lines_of_code_total) / 1000)
```

### Performance Metrics

```promql
# p99 Command Startup Time
histogram_quantile(0.99,
  rate(cli_startup_duration_seconds_bucket[5m])
) * 1000  # Convert to milliseconds

# p99 Parse 10k Triples Time
histogram_quantile(0.99,
  rate(parse_duration_seconds_bucket{triples="10000"}[5m])
) * 1000

# p99 Hook Evaluation Time
histogram_quantile(0.99,
  rate(hook_eval_duration_seconds_bucket[5m])
) * 1000

# p99 SPARQL Query Time
histogram_quantile(0.99,
  rate(query_duration_seconds_bucket[5m])
) * 1000

# p99 Validation Time
histogram_quantile(0.99,
  rate(validation_duration_seconds_bucket[5m])
) * 1000
```

### Reliability Metrics

```promql
# Sidecar Uptime (24 hours)
(
  sum(up{job="kgc-sidecar"}[24h]) /
  count(up{job="kgc-sidecar"}[24h])
) * 100

# Error Isolation Rate
(
  sum(errors_isolated_total) /
  sum(errors_total)
) * 100

# Mean Time To Repair (MTTR)
avg(
  (incident_resolved_timestamp - incident_detected_timestamp) / 3600
)  # Hours
```

### Governance Metrics

```promql
# Policy Compliance Rate
(
  sum(hooks_fired_total) /
  sum(transactions_total)
) * 100

# Audit Trail Coverage
(
  sum(receipts_written_total) /
  sum(transactions_total)
) * 100

# Signature Verification Success Rate
(
  sum(signatures_verified_total) /
  sum(signatures_checked_total)
) * 100
```

### Trend Metrics

```promql
# Defect Density Trend (30 days, daily samples)
defect_density_per_kloc
  offset 30d  # Compare to 30 days ago

# Sigma Level Trend (30 days)
sigma_level
  offset 30d
```

---

## Alert Rules Configuration

```yaml
# prometheus-alerts.yml
groups:
  - name: six_sigma_quality_alerts
    interval: 30s
    rules:

      # Performance Alerts
      - alert: PerformanceRegressionStartup
        expr: histogram_quantile(0.99, rate(cli_startup_duration_seconds_bucket[5m])) > 0.1
        for: 10m
        labels:
          severity: warning
          category: performance
        annotations:
          summary: "Command startup p99 exceeds 100ms SLA"
          description: "Current: {{ $value | humanizeDuration }}, Target: 100ms"
          runbook: "https://docs.unrdf.io/runbooks/performance-regression"

      - alert: PerformanceRegressionParse
        expr: histogram_quantile(0.99, rate(parse_duration_seconds_bucket{triples="10000"}[5m])) > 0.5
        for: 10m
        labels:
          severity: warning
          category: performance
        annotations:
          summary: "Parse 10k triples p99 exceeds 500ms SLA"

      - alert: PerformanceRegressionHooks
        expr: histogram_quantile(0.99, rate(hook_eval_duration_seconds_bucket[5m])) > 0.002
        for: 10m
        labels:
          severity: critical
          category: performance
        annotations:
          summary: "Hook evaluation p99 exceeds 2ms SLA"

      # Quality Alerts
      - alert: TestCoverageBelowThreshold
        expr: (sum(statements_covered) / sum(statements_total)) * 100 < 95
        for: 1h
        labels:
          severity: warning
          category: quality
        annotations:
          summary: "Test coverage below 95% threshold"
          description: "Current: {{ $value }}%, Target: ≥95%"

      - alert: DefectDensityHigh
        expr: (sum(defects_total) / (sum(lines_of_code_total) / 1000)) > 0.5
        for: 24h
        labels:
          severity: critical
          category: quality
        annotations:
          summary: "Defect density exceeds 0.5/KLOC threshold"

      - alert: SigmaLevelBelowTarget
        expr: sigma_level < 6.0
        for: 1h
        labels:
          severity: warning
          category: quality
        annotations:
          summary: "Sigma level below 6σ target"
          description: "Current: {{ $value }}σ, Target: 6σ"

      # Reliability Alerts
      - alert: SidecarUptimeLow
        expr: (sum(up{job="kgc-sidecar"}[24h]) / count(up{job="kgc-sidecar"}[24h])) * 100 < 99.9
        for: 30m
        labels:
          severity: critical
          category: reliability
        annotations:
          summary: "KGC sidecar uptime below 99.9% SLA"
          description: "Current: {{ $value }}%, Target: ≥99.9%"

      - alert: ErrorIsolationFailure
        expr: (sum(errors_isolated_total) / sum(errors_total)) * 100 < 100
        for: 1h
        labels:
          severity: critical
          category: reliability
        annotations:
          summary: "Error isolation rate below 100%"
          description: "Some errors are not being isolated"

      - alert: MTTRExceeded
        expr: avg((incident_resolved_timestamp - incident_detected_timestamp) / 3600) > 4
        for: 1h
        labels:
          severity: warning
          category: reliability
        annotations:
          summary: "Mean Time To Repair exceeds 4-hour target"

      # Governance Alerts
      - alert: PolicyComplianceLow
        expr: (sum(hooks_fired_total) / sum(transactions_total)) * 100 < 100
        for: 1h
        labels:
          severity: critical
          category: governance
        annotations:
          summary: "Policy compliance rate below 100%"
          description: "Some transactions bypassing policy enforcement"

      - alert: AuditTrailGap
        expr: (sum(receipts_written_total) / sum(transactions_total)) * 100 < 100
        for: 30m
        labels:
          severity: critical
          category: governance
        annotations:
          summary: "Audit trail coverage below 100%"
          description: "Missing lockchain receipts for some transactions"

      - alert: SignatureVerificationFailure
        expr: sum(signatures_verified_total) < sum(signatures_checked_total)
        for: 5m
        labels:
          severity: critical
          category: security
        annotations:
          summary: "Policy pack signature verification failed"
          description: "Potential security breach - investigate immediately"
```

---

## Grafana Dashboard JSON

See `/monitoring/grafana/dashboards/six-sigma-cli.json` for full dashboard configuration.

**Import Dashboard**:
1. Navigate to Grafana → Dashboards → Import
2. Upload `six-sigma-cli.json`
3. Select Prometheus data source
4. Import

---

## Custom Panels

### Sigma Level Gauge

```json
{
  "type": "gauge",
  "title": "Overall Sigma Level",
  "targets": [
    {
      "expr": "sigma_level",
      "legendFormat": "Sigma Level"
    }
  ],
  "fieldConfig": {
    "defaults": {
      "min": 0,
      "max": 7,
      "thresholds": {
        "mode": "absolute",
        "steps": [
          { "value": 0, "color": "red" },
          { "value": 3, "color": "orange" },
          { "value": 4, "color": "yellow" },
          { "value": 6, "color": "green" }
        ]
      }
    }
  }
}
```

### Cpk Table

```json
{
  "type": "table",
  "title": "Process Capability (Cpk)",
  "targets": [
    {
      "expr": "cpk_value",
      "format": "table",
      "instant": true
    }
  ],
  "transformations": [
    {
      "id": "organize",
      "options": {
        "columns": [
          { "text": "Metric", "value": "metric" },
          { "text": "Cpk", "value": "Value" },
          { "text": "Sigma", "value": "sigma_level" },
          { "text": "DPMO", "value": "dpmo" }
        ]
      }
    }
  ]
}
```

---

## Usage Instructions

### Accessing the Dashboard

1. **URL**: https://metrics.unrdf.io/dashboards/six-sigma-cli
2. **Login**: Use SSO credentials
3. **Permissions**: Read-only for all, write access for DevOps

### Dashboard Navigation

- **Auto-refresh**: Dashboard updates every 5 seconds
- **Time Range**: Default 24h, configurable (1h, 7d, 30d, custom)
- **Zoom**: Click and drag on any chart to zoom
- **Reset**: Click time range selector → "Reset time range"

### Interpreting Status Indicators

**🟢 Green**: All targets met, process in control
**🟡 Yellow**: Warning threshold exceeded, investigate
**🔴 Red**: Critical threshold exceeded, immediate action required

### Export Options

- **PDF Report**: Dashboard → Share → Export to PDF
- **PNG Image**: Panel → More → Export to PNG
- **CSV Data**: Panel → Inspect → Data → Download CSV
- **JSON**: Dashboard → Settings → JSON Model → Copy

---

## Maintenance

### Dashboard Updates

**Who**: DevOps team
**Frequency**: Quarterly or when new metrics added
**Process**:
1. Update Grafana JSON
2. Test in staging environment
3. Deploy to production
4. Update documentation

### Metric Retention

| Metric Type | Retention Period | Aggregation |
|-------------|-----------------|-------------|
| **Raw Samples** | 7 days | None |
| **5-minute Avg** | 30 days | Average |
| **1-hour Avg** | 1 year | Average |
| **1-day Avg** | 5 years | Average |

### Performance Optimization

- **Cardinality Limit**: < 10,000 unique time series
- **Query Optimization**: Use recording rules for expensive queries
- **Cache**: 5-minute cache for dashboard queries

---

## Troubleshooting

### Dashboard Not Loading

**Symptom**: Dashboard shows "Error loading dashboard"
**Solution**:
1. Check Grafana service status: `systemctl status grafana-server`
2. Check Prometheus connectivity: `curl http://prometheus:9090/api/v1/query?query=up`
3. Review Grafana logs: `journalctl -u grafana-server -f`

### Missing Data Points

**Symptom**: Gaps in time series charts
**Solution**:
1. Check OpenTelemetry collector: `docker logs otel-collector`
2. Verify Prometheus scrape targets: http://prometheus:9090/targets
3. Check metric cardinality: Query total series count

### Alert Not Firing

**Symptom**: Expected alert not triggering
**Solution**:
1. Check Alertmanager: http://alertmanager:9093
2. Test alert rule: Prometheus → Alerts → Rule evaluation
3. Verify notification channels: Alertmanager → Receivers

---

## Support

**Issues**: https://github.com/unrdf/unrdf/issues
**Slack**: #metrics-dashboard
**Docs**: https://docs.unrdf.io/monitoring/dashboard
**On-Call**: PagerDuty escalation

---

**Dashboard Version**: 2.0.0
**Last Updated**: 2025-10-01
**Owner**: DevOps Team
**Status**: ✅ **OPERATIONAL**
