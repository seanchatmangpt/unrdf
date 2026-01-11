# UNRDF v6 Monitoring Guide

## Overview

This guide explains how to monitor UNRDF v6 in production using Prometheus, Grafana, and AlertManager.

## Monitoring Architecture

```
┌─────────────────┐
│  UNRDF v6 App   │──► Metrics (:9090/metrics)
│  (Node.js)      │
└─────────────────┘
        │
        ▼
┌─────────────────┐
│   Prometheus    │──► Scrapes metrics every 15s
│   (:9091)       │──► Evaluates alert rules
└─────────────────┘
        │
        ├──► ┌──────────────┐
        │    │  Grafana     │──► Dashboards & Visualization
        │    │  (:3001)     │
        │    └──────────────┘
        │
        └──► ┌──────────────┐
             │ AlertManager │──► Routes alerts (Slack, PagerDuty)
             │  (:9093)     │
             └──────────────┘
```

## Key Metrics

### Application Metrics

#### Request Metrics
```promql
# Request rate (requests per second)
rate(unrdf_api_requests_total[5m])

# Request duration (P50, P95, P99)
histogram_quantile(0.95, rate(unrdf_api_duration_seconds_bucket[5m]))

# Error rate
rate(unrdf_errors_total[5m])

# Success rate
rate(unrdf_api_requests_total{status="success"}[5m]) /
rate(unrdf_api_requests_total[5m])
```

#### KGC Metrics
```promql
# Receipt creation rate
rate(unrdf_receipt_created_total[5m])

# Receipt creation duration
histogram_quantile(0.95, rate(unrdf_receipt_creation_duration_seconds_bucket[5m]))

# Delta validation duration
histogram_quantile(0.95, rate(unrdf_delta_validation_duration_seconds_bucket[5m]))

# Receipt verification failures
rate(unrdf_receipt_verification_failures_total[5m])
```

#### SPARQL Metrics
```promql
# Query rate
rate(unrdf_sparql_queries_total[5m])

# Query duration
histogram_quantile(0.95, rate(unrdf_sparql_duration_seconds_bucket[5m]))

# Query timeouts
rate(unrdf_sparql_timeouts_total[5m])

# Query complexity
avg(unrdf_sparql_complexity)
```

#### Hook Metrics
```promql
# Hook execution rate
rate(unrdf_hook_executions_total[5m])

# Hook execution duration
histogram_quantile(0.95, rate(unrdf_hook_duration_seconds_bucket[5m]))

# Hook failures
rate(unrdf_hook_failures_total[5m])

# Hook timeouts
rate(unrdf_hook_timeouts_total[5m])
```

### System Metrics

#### Resource Usage
```promql
# Memory usage (bytes)
process_resident_memory_bytes{job="unrdf-app"}

# Memory usage (percentage)
process_resident_memory_bytes{job="unrdf-app"} /
node_memory_MemTotal_bytes * 100

# CPU usage (percentage)
rate(process_cpu_seconds_total{job="unrdf-app"}[5m]) * 100

# Disk usage
(node_filesystem_size_bytes - node_filesystem_avail_bytes) /
node_filesystem_size_bytes * 100
```

#### Node.js Metrics
```promql
# Heap size
nodejs_heap_size_total_bytes

# Heap used
nodejs_heap_size_used_bytes

# Heap usage percentage
nodejs_heap_size_used_bytes / nodejs_heap_size_total_bytes * 100

# Garbage collection duration
rate(nodejs_gc_duration_seconds_sum[5m])

# Event loop lag
nodejs_eventloop_lag_seconds
```

## Grafana Dashboards

### UNRDF Overview Dashboard

Access: http://grafana.example.com:3001/d/unrdf-overview

**Panels:**
1. **Application Status** - Current up/down status
2. **Request Rate** - Requests per second over time
3. **API Latency** - P50, P95, P99 latency
4. **Error Rate** - Errors per second
5. **Memory Usage** - Memory consumption over time
6. **CPU Usage** - CPU utilization over time
7. **Receipt Creation Rate** - KGC receipts created per second
8. **SPARQL Query Performance** - Query latency distribution

### Creating Custom Dashboards

```json
{
  "dashboard": {
    "title": "Custom UNRDF Dashboard",
    "panels": [
      {
        "title": "Error Rate",
        "targets": [
          {
            "expr": "rate(unrdf_errors_total[5m])",
            "legendFormat": "Errors/sec"
          }
        ],
        "type": "graph"
      }
    ]
  }
}
```

Import via:
```bash
curl -X POST http://grafana.example.com:3001/api/dashboards/db \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $GRAFANA_API_KEY" \
  -d @dashboard.json
```

## Alerts

### Critical Alerts (P0)

#### ApplicationDown
```yaml
alert: ApplicationDown
expr: up{job="unrdf-app"} == 0
for: 1m
severity: critical
```
**Response:** Execute rollback immediately

#### HighErrorRate
```yaml
alert: HighErrorRate
expr: rate(unrdf_errors_total[5m]) > 0.05
for: 5m
severity: critical
```
**Response:** Investigate error patterns, consider rollback

#### ReceiptGenerationFailure
```yaml
alert: ReceiptGenerationFailure
expr: rate(unrdf_receipt_failures_total[5m]) > 0.01
for: 5m
severity: critical
```
**Response:** Check KGC system integrity

### Warning Alerts (P1)

#### HighAPILatency
```yaml
alert: HighAPILatency
expr: histogram_quantile(0.95, rate(unrdf_api_duration_seconds_bucket[5m])) > 1.0
for: 5m
severity: warning
```
**Response:** Investigate slow queries, check resources

#### HighMemoryUsage
```yaml
alert: HighMemoryUsage
expr: (node_memory_MemTotal_bytes - node_memory_MemAvailable_bytes) /
      node_memory_MemTotal_bytes > 0.9
for: 5m
severity: warning
```
**Response:** Check for memory leaks, consider scaling

#### HighCPUUsage
```yaml
alert: HighCPUUsage
expr: 100 - (avg by(instance) (irate(node_cpu_seconds_total{mode="idle"}[5m])) * 100) > 80
for: 5m
severity: warning
```
**Response:** Check for CPU-intensive operations, scale horizontally

## Alert Routing

### Slack Integration

Alerts are sent to:
- `#unrdf-alerts` - All alerts
- `#unrdf-warnings` - Warning level
- `#unrdf-app` - Application-specific
- `#unrdf-performance` - Performance alerts

### PagerDuty Integration

Critical alerts (P0) trigger PagerDuty:
- Immediate page to on-call engineer
- Escalation after 15 minutes if not acknowledged
- Auto-resolve when alert clears

### Alert Silencing

Temporarily silence alerts during maintenance:

```bash
# Silence for 1 hour
curl -X POST http://alertmanager.example.com:9093/api/v2/silences \
  -H "Content-Type: application/json" \
  -d '{
    "matchers": [
      {"name": "job", "value": "unrdf-app", "isRegex": false}
    ],
    "startsAt": "'$(date -u +%Y-%m-%dT%H:%M:%SZ)'",
    "endsAt": "'$(date -u -d '+1 hour' +%Y-%m-%dT%H:%M:%SZ)'",
    "createdBy": "maintenance",
    "comment": "Planned maintenance"
  }'
```

## Query Examples

### Troubleshooting Queries

#### Find top error endpoints
```promql
topk(10, sum by (endpoint) (rate(unrdf_api_requests_total{status="error"}[5m])))
```

#### Find slowest endpoints
```promql
topk(10,
  histogram_quantile(0.95,
    sum by (endpoint, le) (rate(unrdf_api_duration_seconds_bucket[5m]))
  )
)
```

#### Memory usage trend
```promql
predict_linear(process_resident_memory_bytes{job="unrdf-app"}[1h], 3600)
```

#### Request success rate by endpoint
```promql
sum by (endpoint) (rate(unrdf_api_requests_total{status="success"}[5m])) /
sum by (endpoint) (rate(unrdf_api_requests_total[5m])) * 100
```

### Capacity Planning Queries

#### Daily request volume
```promql
sum(increase(unrdf_api_requests_total[24h]))
```

#### Peak request rate
```promql
max_over_time(rate(unrdf_api_requests_total[5m])[24h:5m])
```

#### 95th percentile latency trend
```promql
histogram_quantile(0.95,
  sum by (le) (rate(unrdf_api_duration_seconds_bucket[5m]))
)
```

## SLO Monitoring

### Service Level Objectives

| SLO | Target | Measurement |
|-----|--------|-------------|
| Availability | 99.9% | `up{job="unrdf-app"} == 1` |
| Latency (P95) | < 100ms | `histogram_quantile(0.95, ...)` |
| Error Rate | < 0.1% | `rate(unrdf_errors_total) / rate(unrdf_api_requests_total)` |
| Receipt Creation | < 1ms | `histogram_quantile(0.95, rate(unrdf_receipt_creation_duration_seconds_bucket[5m]))` |

### SLO Dashboard

Create SLO tracking dashboard:

```promql
# Availability SLO (99.9% = 43.8 minutes downtime per month)
avg_over_time(up{job="unrdf-app"}[30d]) * 100

# Error budget remaining
(1 - (sum(rate(unrdf_errors_total[30d])) /
      sum(rate(unrdf_api_requests_total[30d])))) - 0.999
```

## Log Aggregation

### Viewing Logs

```bash
# Real-time logs
docker-compose logs -f unrdf

# Last 100 lines
docker-compose logs --tail=100 unrdf

# Filter by level
docker-compose logs unrdf | grep ERROR

# Filter by time
docker-compose logs --since 30m unrdf
```

### Log Queries

```bash
# Count errors by type
docker-compose logs --since 1h unrdf | \
  grep ERROR | \
  awk '{print $NF}' | \
  sort | uniq -c | sort -rn

# Find slow queries
docker-compose logs --since 1h unrdf | \
  grep "Slow query" | \
  awk '{print $8}' | \
  sort -rn | head -10
```

## Performance Monitoring

### Key Performance Indicators (KPIs)

#### Response Time
```promql
# Average response time
avg(rate(unrdf_api_duration_seconds_sum[5m]) /
    rate(unrdf_api_duration_seconds_count[5m]))

# Response time by percentile
histogram_quantile(0.50, rate(unrdf_api_duration_seconds_bucket[5m])) # P50
histogram_quantile(0.95, rate(unrdf_api_duration_seconds_bucket[5m])) # P95
histogram_quantile(0.99, rate(unrdf_api_duration_seconds_bucket[5m])) # P99
```

#### Throughput
```promql
# Requests per second
rate(unrdf_api_requests_total[5m])

# Receipts per second
rate(unrdf_receipt_created_total[5m])

# SPARQL queries per second
rate(unrdf_sparql_queries_total[5m])
```

#### Resource Efficiency
```promql
# Requests per CPU core
rate(unrdf_api_requests_total[5m]) /
count(node_cpu_seconds_total{mode="idle"})

# Memory per request
process_resident_memory_bytes /
rate(unrdf_api_requests_total[5m])
```

## Monitoring Best Practices

### 1. Alert Fatigue Prevention
- Set appropriate thresholds
- Use `for` duration to avoid flapping
- Group related alerts
- Regular alert tuning

### 2. Dashboard Organization
- One overview dashboard
- Service-specific dashboards
- Troubleshooting dashboards
- SLO dashboards

### 3. Metric Naming
- Use `unrdf_` prefix
- Include unit in name (`_seconds`, `_bytes`)
- Use consistent naming patterns

### 4. Data Retention
- High-resolution: 7 days
- Medium-resolution: 30 days
- Low-resolution: 1 year

## Troubleshooting Monitoring

### Prometheus Not Scraping

```bash
# Check Prometheus targets
curl http://localhost:9091/api/v1/targets

# Check Prometheus config
curl http://localhost:9091/api/v1/status/config

# Verify metrics endpoint
curl http://localhost:9090/metrics
```

### Grafana Connection Issues

```bash
# Test datasource
curl -H "Authorization: Bearer $GRAFANA_API_KEY" \
  http://localhost:3001/api/datasources

# Check Grafana logs
docker-compose logs grafana
```

### Missing Metrics

```bash
# List all available metrics
curl -s http://localhost:9090/metrics | grep unrdf_

# Check metric registration
docker-compose exec unrdf node -e "
  const metrics = require('./metrics');
  console.log(metrics.getMetricsRegistry().getMetricsAsJSON());
"
```

## Related Documentation

- [Deployment Runbook](./DEPLOYMENT_RUNBOOK.md)
- [Rollback Runbook](./ROLLBACK_RUNBOOK.md)
- [Incident Response](./INCIDENT_RESPONSE.md)

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-11 | 1.0 | Initial guide | DevOps Team |
