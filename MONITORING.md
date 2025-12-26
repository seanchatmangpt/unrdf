# UNRDF Production Monitoring Guide

**Version**: 1.0.0
**Last Updated**: 2025-12-25
**Methodology**: Big Bang 80/20 - Production-Grade Observability

---

## Overview

This document describes the production monitoring and observability system for UNRDF. The system follows the **80/20 principle** - focusing on the 20% of metrics that provide 80% of actionable insights.

### Core Principles

1. **Actionable Metrics Only** - Every metric must support a decision or prevent an incident
2. **OTEL is Truth** - Agent claims require OpenTelemetry validation ≥80/100
3. **Evidence-Based** - No assumptions, only measured reality
4. **Poka-Yoke** - Built-in error prevention through automated checks
5. **Andon Principle** - Stop and fix root cause when thresholds exceeded

---

## Quick Start

### 1. Health Checks

```javascript
import { createUnrdfHealthChecks } from '@unrdf/core/health';

// Create health check system
const health = createUnrdfHealthChecks({
  serviceName: 'unrdf-api',
  version: '5.0.1',
  dependencies: {
    database: async () => await db.ping(),
    cache: async () => await redis.ping(),
    otel: async () => await otelProvider.isActive()
  }
});

// Liveness probe (K8s: /health)
const liveness = await health.liveness();
// Returns: { status: 'healthy', uptime: 12345, ... }

// Readiness probe (K8s: /health/ready)
const readiness = await health.readiness();
// Returns: { status: 'healthy', dependencies: {...}, ... }

// Metrics (Prometheus: /health/metrics)
const metrics = await health.metrics();
// Returns: { requests: {...}, memory: {...}, cpu: {...} }

// Prometheus format (Prometheus: /metrics)
const prometheus = await health.prometheus();
// Returns: Prometheus exposition format string
```

### 2. Structured Logging

```javascript
import { createLogger, performanceTimer } from '@unrdf/core/logger';

// Create logger
const logger = createLogger({
  service: 'unrdf-api',
  level: 'info',
  context: { environment: 'production' }
});

// Standard logging
logger.info('User action', { userId: '123', action: 'query' });
logger.error('Database error', { query: 'SELECT *' }, error);

// Performance logging
const timer = performanceTimer();
await doWork();
logger.performance('Query execution', timer.end());
// Output: { duration: 42.5, timestamp: '2025-12-25T...' }

// Slow query detection (auto-warns if >100ms)
logger.slowQuery('complex-query', 150, 100);

// Request-scoped logging
const requestLogger = logger.child({ requestId: '456' });
requestLogger.info('Processing request');
```

### 3. Metrics Collection

```javascript
import { createMetrics } from '@unrdf/core/metrics';

const metrics = createMetrics({ prefix: 'unrdf' });

// Counter: Total requests
metrics.incrementCounter('requests_total', { method: 'GET', status: 200 });

// Gauge: Active connections
metrics.recordGauge('active_connections', 42);

// Histogram: Request duration
const timer = metrics.startTimer();
await handleRequest();
metrics.recordDuration('request_duration', timer, { endpoint: '/api/query' });

// Summary: Custom percentiles (P50, P95, P99)
metrics.recordSummary('query_duration', 0.042);

// Export to Prometheus
const prometheusFormat = metrics.toPrometheus();

// Export to JSON
const jsonFormat = metrics.toJSON();
```

### 4. OTEL Validation

```bash
# Run comprehensive OTEL validation (80/100 threshold)
timeout 5s node validation/run-all.mjs comprehensive

# Check results
grep "Score:" validation-output.log  # Must be ≥80/100
grep "FAILED" validation-output.log  # Must be 0 results
```

---

## Monitoring Stack

### Architecture

```
Application Code
    ↓
OTEL SDK (Traces, Metrics, Logs)
    ↓
OTEL Collector (Aggregation, Filtering)
    ↓
    ├→ Prometheus (Metrics Storage)
    ├→ Jaeger (Trace Storage)
    └→ Loki (Log Storage)
    ↓
Grafana (Visualization & Alerting)
```

### Components

| Component | Purpose | Endpoint |
|-----------|---------|----------|
| **Health Check** | Liveness/readiness probes | `GET /health`, `GET /health/ready` |
| **Metrics** | Prometheus metrics | `GET /metrics` |
| **OTEL Collector** | Trace/metric aggregation | `POST localhost:4318/v1/traces` |
| **Prometheus** | Metrics storage/queries | `http://localhost:9090` |
| **Grafana** | Dashboards/alerts | `http://localhost:3000` |

---

## Key Metrics (80/20 Focus)

### 1. Health Metrics

```promql
# Service availability
up{job="unrdf"} == 1

# OTEL validation score
unrdf_otel_validation_score >= 80

# Dependency health
unrdf_dependency_status{status="connected"} == 1
```

### 2. Performance Metrics

```promql
# P95 latency (target: <100ms)
histogram_quantile(0.95, rate(unrdf_request_duration_seconds_bucket[5m])) < 0.1

# P99 latency (target: <500ms)
histogram_quantile(0.99, rate(unrdf_request_duration_seconds_bucket[5m])) < 0.5

# Slow queries (target: <0.1/sec)
rate(unrdf_slow_queries_total[5m]) < 0.1
```

### 3. Error Metrics

```promql
# Error rate (target: <1%)
rate(unrdf_errors_total[5m]) / rate(unrdf_requests_total[5m]) < 0.01

# Test failure rate (target: <5%)
unrdf_test_failures_total / unrdf_test_total < 0.05
```

### 4. Resource Metrics

```promql
# Memory usage (target: <85%)
unrdf_memory_heap_used_bytes / unrdf_memory_heap_total_bytes < 0.85

# CPU usage (target: <80%)
rate(process_cpu_seconds_total[5m]) < 0.8

# Memory leak detection (increase >1MB/s for 1h)
rate(unrdf_memory_heap_used_bytes[1h]) < 1048576
```

---

## Alerts

### Critical Alerts (PagerDuty)

1. **ServiceDown** - Service not responding for 1 minute
2. **OTELScoreBelowThreshold** - OTEL score <80 for 5 minutes
3. **HighErrorRate** - Error rate >1% for 5 minutes
4. **HighP99Latency** - P99 latency >500ms for 5 minutes
5. **MemoryLeak** - Memory increasing >1MB/s for 1 hour

### Warning Alerts (Slack)

1. **HighTestFailureRate** - Test failure >5% for 5 minutes
2. **HighP95Latency** - P95 latency >100ms for 5 minutes
3. **SlowQueryDetected** - Slow queries >0.1/sec for 5 minutes
4. **HighMemoryUsage** - Memory >85% for 10 minutes
5. **IncreasedErrorRate** - Error rate 2x hourly average

See `monitoring/alerts.yml` for full configuration.

---

## Dashboards

### Main Dashboard: `unrdf-overview.json`

**Purpose**: Executive overview of system health and performance

**Panels**:
1. **System Health** - Overall service status (green/red)
2. **OTEL Validation Score** - Gauge with 80/100 threshold
3. **Request Rate** - Requests per second over time
4. **Error Rate** - Error percentage with 1% threshold
5. **Request Latency** - P50/P95/P99 percentiles
6. **Memory Usage** - Heap used/total, RSS
7. **Test Quality** - Test failure rate
8. **Slow Queries** - Slow query rate
9. **Active Alerts** - Current firing alerts
10. **CPU Usage** - User/system CPU time
11. **Dependency Health** - Status of all dependencies

**Import**:
```bash
# Import into Grafana
curl -X POST http://localhost:3000/api/dashboards/import \
  -H "Content-Type: application/json" \
  -d @monitoring/dashboards/unrdf-overview.json
```

---

## Integration Examples

### Express.js

```javascript
import express from 'express';
import { createHealthMiddleware } from '@unrdf/core/health';
import { requestLogger } from '@unrdf/core/logger';
import { metrics } from '@unrdf/core/metrics';

const app = express();

// Request logging middleware
app.use(requestLogger({ logBody: false }));

// Health check endpoints
const health = createHealthMiddleware({
  serviceName: 'unrdf-api',
  version: '5.0.1',
  dependencies: {
    database: async () => await db.ping()
  }
});

app.get('/health', health.liveness);
app.get('/health/ready', health.readiness);
app.get('/health/metrics', health.metrics);
app.get('/metrics', health.prometheus);

// Business endpoints with metrics
app.get('/api/query', async (req, res) => {
  const timer = metrics.startTimer();

  try {
    const result = await executeQuery(req.query);
    metrics.incrementCounter('requests_total', { endpoint: '/api/query', status: 200 });
    metrics.recordDuration('request_duration', timer, { endpoint: '/api/query' });
    res.json(result);
  } catch (error) {
    metrics.incrementCounter('errors_total', { endpoint: '/api/query' });
    req.log.error('Query failed', { query: req.query }, error);
    res.status(500).json({ error: error.message });
  }
});

app.listen(3000);
```

### Kubernetes Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: unrdf-api
spec:
  template:
    spec:
      containers:
      - name: unrdf-api
        image: unrdf/api:5.0.1
        ports:
        - containerPort: 3000

        # Liveness probe (is process running?)
        livenessProbe:
          httpGet:
            path: /health
            port: 3000
          initialDelaySeconds: 10
          periodSeconds: 10
          timeoutSeconds: 5
          failureThreshold: 3

        # Readiness probe (can handle traffic?)
        readinessProbe:
          httpGet:
            path: /health/ready
            port: 3000
          initialDelaySeconds: 5
          periodSeconds: 5
          timeoutSeconds: 5
          failureThreshold: 2

        # Resource limits
        resources:
          requests:
            memory: "512Mi"
            cpu: "250m"
          limits:
            memory: "1Gi"
            cpu: "1000m"

        # Environment
        env:
        - name: NODE_ENV
          value: "production"
        - name: LOG_LEVEL
          value: "info"
        - name: OTEL_EXPORTER_OTLP_ENDPOINT
          value: "http://otel-collector:4318"

---
apiVersion: v1
kind: Service
metadata:
  name: unrdf-api
  annotations:
    prometheus.io/scrape: "true"
    prometheus.io/path: "/metrics"
    prometheus.io/port: "3000"
spec:
  selector:
    app: unrdf-api
  ports:
  - port: 80
    targetPort: 3000
```

---

## Verification Checklist

Before declaring monitoring complete, verify:

### Health Checks
- [ ] `/health` returns 200 with uptime
- [ ] `/health/ready` checks all dependencies
- [ ] `/health/ready` returns 503 when dependencies fail
- [ ] `/metrics` returns Prometheus format
- [ ] All endpoints respond within 5 seconds

### Logging
- [ ] All logs are JSON formatted
- [ ] Logs include timestamp, level, service, message
- [ ] OTEL trace IDs appear in logs (when available)
- [ ] Slow queries are detected and logged (>100ms)
- [ ] Request logging includes duration

### Metrics
- [ ] Counters increment correctly
- [ ] Histograms calculate percentiles (P50, P95, P99)
- [ ] Gauges reflect current state
- [ ] Prometheus export format is valid
- [ ] Metrics include service and version labels

### Alerts
- [ ] All critical alerts have runbook links
- [ ] Alert thresholds match SLOs
- [ ] Alert routing configured (PagerDuty, Slack)
- [ ] Alerts fire within 5 minutes of threshold breach
- [ ] Alerts resolve automatically when issue fixed

### OTEL Validation
- [ ] OTEL score ≥80/100
- [ ] All expected spans present
- [ ] Span attributes include required fields
- [ ] Performance thresholds met
- [ ] Error rate <1%

---

## Troubleshooting

### High OTEL Score but Application Errors

**Symptom**: OTEL validation passes but users report errors

**Diagnosis**:
```bash
# Check error logs
grep '"level":"error"' logs/*.log

# Check error rate metric
curl http://localhost:3000/health/metrics | jq '.requests.errorRate'
```

**Fix**: OTEL validates span structure, not business logic. Add integration tests.

### Memory Usage Increasing

**Symptom**: `unrdf_memory_heap_used_bytes` increasing linearly

**Diagnosis**:
```bash
# Check memory growth rate
curl http://localhost:3000/health/metrics | jq '.memory'

# Capture heap snapshot
node --heap-prof app.mjs
```

**Fix**: See `monitoring/RUNBOOK.md#memory-leak`

### Slow Queries Not Logged

**Symptom**: Performance degradation but no slow query logs

**Diagnosis**:
```bash
# Check log level
echo $LOG_LEVEL  # Should be 'info' or lower

# Check logger configuration
grep 'slowQuery' logs/*.log
```

**Fix**: Ensure `logger.slowQuery()` called with correct threshold (default: 100ms)

---

## Best Practices

### 1. Avoid Vanity Metrics

❌ **Bad**: Total requests (unbounded counter, no context)
✅ **Good**: Request rate by endpoint and status (actionable)

❌ **Bad**: Average latency (hides outliers)
✅ **Good**: P95/P99 latency (shows user experience)

### 2. Use Consistent Labels

```javascript
// ✅ Consistent labels across metrics
metrics.incrementCounter('requests_total', { endpoint: '/api/query', status: 200 });
metrics.recordDuration('request_duration', timer, { endpoint: '/api/query' });

// ❌ Inconsistent labels
metrics.incrementCounter('requests_total', { path: '/api/query' });
metrics.recordDuration('request_duration', timer, { endpoint: '/api/query' });
```

### 3. Set Meaningful Thresholds

Thresholds should be based on:
- **SLOs** (Service Level Objectives): P95 latency <100ms
- **Capacity planning**: Memory <85% to handle spikes
- **User impact**: Error rate <1% = good UX

### 4. Test Your Alerts

```bash
# Trigger test alert
curl -X POST http://localhost:9090/api/v1/alerts \
  -d '{"alerts": [{"labels": {"alertname": "TestAlert", "severity": "warning"}}]}'

# Verify alert fired and routed correctly
```

---

## References

- **Alerting Rules**: `monitoring/alerts.yml`
- **Runbook**: `monitoring/RUNBOOK.md`
- **Dashboards**: `monitoring/dashboards/`
- **OTEL Configuration**: `validation/otel-provider.mjs`
- **Big Bang 80/20 Methodology**: `docs/bb80-20-methodology.md`

---

## Support

For monitoring issues:
1. Check runbook: `monitoring/RUNBOOK.md`
2. Review logs: `grep '"level":"error"' logs/*.log`
3. Verify OTEL score: `node validation/run-all.mjs comprehensive`
4. Check Grafana dashboard: `http://localhost:3000`

**Remember**: OTEL is truth. If OTEL score ≥80/100 and metrics look good, the system is healthy. Trust the data, not assumptions.
