# Observability and OpenTelemetry

This guide explains how to configure and use OpenTelemetry for distributed tracing, metrics collection, and observability in Knowd deployments.

## Overview

Knowd integrates with OpenTelemetry to provide comprehensive observability capabilities:

- **Distributed Tracing**: Track requests across services and components
- **Metrics Collection**: Monitor performance, throughput, and resource usage
- **Structured Logging**: Consistent, searchable log output
- **Health Checks**: Application and system health monitoring

## Quick Start

### Enable Basic Observability

```bash
# Enable tracing and metrics
KNOWD_OTEL_TRACING=true \
KNOWD_OTEL_METRICS=true \
./knowd
```

### Environment Variables

**Core Configuration:**
```bash
# Service identification
KNOWD_ENVIRONMENT=production
KNOWD_SERVICE_NAME=knowd-api
KNOWD_SERVICE_VERSION=1.0.0

# Tracing configuration
KNOWD_OTEL_TRACING=true
KNOWD_OTEL_SAMPLE_RATIO=1.0

# Metrics configuration
KNOWD_OTEL_METRICS=true
```

**Advanced Configuration:**
```bash
# OTLP endpoints (for production)
KNOWD_OTEL_TRACE_ENDPOINT=http://jaeger:14268/api/traces
KNOWD_OTEL_METRICS_ENDPOINT=http://prometheus:9090/api/v1/otlp

# Custom resource attributes
KNOWD_OTEL_RESOURCE_ATTRIBUTES=deployment.region=us-west-2,service.team=platform
```

## Distributed Tracing

### Configuration

**Enable tracing:**
```bash
KNOWD_OTEL_TRACING=true \
KNOWD_OTEL_SAMPLE_RATIO=0.1 \  # Sample 10% of traces
./knowd
```

**Production setup with Jaeger:**
```bash
KNOWD_OTEL_TRACE_ENDPOINT=http://jaeger:14268/api/traces \
KNOWD_OTEL_INSECURE=false \
./knowd
```

### Trace Visualization

**Transaction trace example:**
```
Transaction Processing (250ms)
├── Request Validation (5ms)
├── Namespace Resolution (2ms)
├── Authentication (3ms)
├── Delta Application (50ms)
├── Hook Evaluation (150ms)
│   ├── SPARQL-ASK Hook (45ms)
│   ├── SHACL Validation (85ms)
│   └── Delta Hook (20ms)
├── Receipt Generation (35ms)
└── Response Generation (5ms)
```

**Query execution trace:**
```
SPARQL Query (180ms)
├── Query Parsing (15ms)
├── Plan Generation (25ms)
├── Optimization (40ms)
├── Cache Lookup (2ms)
├── Execution (85ms)
│   ├── BGP Matching (45ms)
│   ├── Join Processing (25ms)
│   └── Filter Application (15ms)
└── Result Serialization (13ms)
```

### Custom Tracing

**Add custom spans in your code:**
```go
import "github.com/unrdf/knowd/internal/telemetry"

// In your function
ctx, span := telemetry.StartSpan(ctx, "my-operation")
defer span.End()

// Add attributes
telemetry.AddEvent(ctx, "operation.started", telemetry.Attribute("user_id", userID))

// Record errors
if err != nil {
    telemetry.RecordError(ctx, err)
}
```

## Metrics Collection

### Built-in Metrics

Knowd automatically exposes metrics via the `/metrics` endpoint:

**Query Performance:**
- `knowd_query_duration_seconds` - Query execution latency histogram
- `knowd_query_total` - Total queries executed counter
- `knowd_plan_cache_hits_total` - Query plan cache hit counter

**Transaction Performance:**
- `knowd_tx_duration_seconds` - Transaction processing latency
- `knowd_tx_total` - Total transactions processed

**Storage Metrics:**
- `knowd_store_quads_total` - Total stored quads
- `knowd_store_segments_total` - WAL segments count
- `knowd_store_snapshot_age_seconds` - Time since last snapshot

**Hook Performance:**
- `knowd_hook_execution_duration_seconds` - Hook execution time
- `knowd_hook_total` - Total hook executions

**Cluster Metrics:**
- `knowd_cluster_replication_lag_seconds` - Replication lag
- `knowd_cluster_wal_size_bytes` - WAL size

### Prometheus Integration

**Scrape configuration:**
```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'knowd'
    static_configs:
      - targets: ['knowd:9090']
    scrape_interval: 15s
    metrics_path: '/metrics'
```

**Example queries:**
```promql
# Query latency p95
histogram_quantile(0.95, rate(knowd_query_duration_seconds_bucket[5m]))

# Cache hit rate
rate(knowd_plan_cache_hits_total[5m]) / rate(knowd_plan_cache_total[5m])

# Error rate
rate(knowd_http_requests_total{status=~"5.."}[5m]) / rate(knowd_http_requests_total[5m])
```

### Grafana Dashboards

**Application dashboard:**
```json
{
  "dashboard": {
    "title": "Knowd Application Metrics",
    "panels": [
      {
        "title": "Query Performance",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, rate(knowd_query_duration_seconds_bucket[5m]))",
            "legendFormat": "p95 Query Latency"
          }
        ]
      },
      {
        "title": "Throughput",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(knowd_query_total[5m])",
            "legendFormat": "Queries/sec"
          }
        ]
      }
    ]
  }
}
```

## Structured Logging

### JSON Logging

**Enable structured logging:**
```bash
KNOWD_LOG_FORMAT=json \
KNOWD_LOG_LEVEL=info \
./knowd
```

**Log format:**
```json
{
  "timestamp": "2025-01-01T10:00:00Z",
  "level": "info",
  "logger": "knowd",
  "message": "Transaction completed",
  "transaction_id": "tx-12345",
  "duration_ms": 150,
  "quads_affected": 5,
  "hooks_executed": 2,
  "trace_id": "abc123",
  "span_id": "def456"
}
```

### Log Aggregation

**Fluent Bit configuration:**
```ini
# fluent-bit.conf
[SERVICE]
    Flush         5
    Log_Level     info

[INPUT]
    Name              tail
    Path              /var/log/knowd/*.log
    Parser            json
    Tag               knowd.*

[FILTER]
    Name              kubernetes
    Match             knowd.*
    Kube_URL          https://kubernetes.default.svc:443

[OUTPUT]
    Name  elasticsearch
    Match *
    Host  elasticsearch
    Port  9200
```

## Health Checks

### Application Health

**Basic health check:**
```bash
curl http://localhost:8090/healthz
# Response: {"status": "ok", "timestamp": "2025-01-01T10:00:00Z"}
```

**Detailed health status:**
```bash
curl http://localhost:8090/v1/store/stats
# Response: {"quads": 15000, "segments": 5, "snapshot_age_sec": 300}
```

### Kubernetes Health Checks

**Liveness probe:**
```yaml
livenessProbe:
  httpGet:
    path: /healthz
    port: 8090
  initialDelaySeconds: 30
  periodSeconds: 10
```

**Readiness probe:**
```yaml
readinessProbe:
  httpGet:
    path: /healthz
    port: 8090
  initialDelaySeconds: 5
  periodSeconds: 5
```

## Performance Profiling

### CPU Profiling

**Enable pprof endpoints:**
```bash
KNOWD_PPROF_ADDR=:6060 ./knowd
```

**Collect profiles:**
```bash
# CPU profile (30 seconds)
curl http://localhost:6060/debug/pprof/profile?seconds=30 > cpu.prof

# Memory profile
curl http://localhost:6060/debug/pprof/heap > heap.prof

# Goroutine dump
curl http://localhost:6060/debug/pprof/goroutine > goroutines.txt
```

**Analyze profiles:**
```bash
# CPU analysis
go tool pprof cpu.prof
(pprof) top10

# Memory analysis
go tool pprof heap.prof
(pprof) top -cum
```

### Custom Profiling

**Add custom profiling points:**
```go
import "runtime/pprof"

// In your code
func expensiveOperation() {
    defer pprof.Do(context.Background(), pprof.Labels("operation", "expensive"), func(ctx context.Context) {
        // Your expensive operation here
    })
}
```

## Configuration Examples

### Development Setup

```bash
# Development with all observability features
KNOWD_ENVIRONMENT=development \
KNOWD_LOG_LEVEL=debug \
KNOWD_LOG_FORMAT=json \
KNOWD_OTEL_TRACING=true \
KNOWD_OTEL_METRICS=true \
KNOWD_OTEL_SAMPLE_RATIO=1.0 \
KNOWD_PPROF_ADDR=:6060 \
./knowd
```

### Production Setup

```bash
# Production with OTLP exporters
KNOWD_ENVIRONMENT=production \
KNOWD_LOG_LEVEL=info \
KNOWD_LOG_FORMAT=json \
KNOWD_OTEL_TRACING=true \
KNOWD_OTEL_METRICS=true \
KNOWD_OTEL_SAMPLE_RATIO=0.1 \
KNOWD_OTEL_TRACE_ENDPOINT=http://jaeger-collector:14268/api/traces \
KNOWD_OTEL_METRICS_ENDPOINT=http://prometheus:9090/api/v1/otlp \
KNOWD_METRICS_ADDR=:9090 \
./knowd
```

### Docker Compose with Observability

```yaml
version: '3.8'
services:
  knowd:
    image: knowd:latest
    environment:
      - KNOWD_ENVIRONMENT=production
      - KNOWD_OTEL_TRACING=true
      - KNOWD_OTEL_METRICS=true
      - KNOWD_OTEL_TRACE_ENDPOINT=http://jaeger:14268/api/traces
      - KNOWD_OTEL_METRICS_ENDPOINT=http://prometheus:9090/api/v1/otlp
      - KNOWD_METRICS_ADDR=:9090
    ports:
      - "8090:8090"
      - "9090:9090"  # Metrics port
      - "6060:6060"  # pprof port

  jaeger:
    image: jaegertracing/all-in-one:latest
    ports:
      - "16686:16686"  # UI
      - "14268:14268"  # OTLP

  prometheus:
    image: prom/prometheus:latest
    ports:
      - "9090:9090"
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml
```

## Best Practices

### 1. Environment-Specific Configuration

**Development:**
- Full sampling (1.0 ratio)
- Debug logging
- All observability features enabled
- Local exporters (stdout, file)

**Production:**
- Sampled tracing (0.1-0.01 ratio)
- Info/warn logging
- OTLP exporters to centralized systems
- Resource limits and quotas

### 2. Resource Management

**Memory limits:**
```bash
# Set Go memory limits
GOGC=200 \
GOMEMLIMIT=16GiB \
./knowd
```

**CPU optimization:**
```bash
# Pin to specific cores for consistent performance
taskset -c 0-7 ./knowd
```

### 3. Security Considerations

**Secure endpoints:**
```bash
# Use TLS for observability endpoints
KNOWD_METRICS_ADDR=:9090 \
KNOWD_PPROF_ADDR=:6060 \
./knowd
```

**Access control:**
```bash
# Restrict access to debug endpoints
iptables -A INPUT -p tcp --dport 6060 -s 10.0.0.0/8 -j ACCEPT
iptables -A INPUT -p tcp --dport 6060 -j DROP
```

### 4. Performance Impact

**Tracing overhead:**
- ~1-5% CPU overhead for sampled tracing
- Minimal memory impact with proper sampling
- Network overhead for OTLP exports

**Metrics overhead:**
- <1% CPU overhead for standard metrics
- Memory usage scales with metric cardinality
- Network overhead for remote exports

### 5. Monitoring Strategy

**Define SLIs/SLOs:**
- Query latency: <300ms p95
- Error rate: <1%
- Availability: >99.9%
- Throughput: >500 queries/sec

**Alerting thresholds:**
- High latency: >500ms p95
- High error rate: >5%
- Resource exhaustion: >80% CPU/memory
- Service unavailability: >1 minute

## Troubleshooting

### Common Issues

**"No traces appearing in Jaeger":**
```bash
# Check trace endpoint
curl http://jaeger:14268/api/traces

# Verify endpoint configuration
echo $KNOWD_OTEL_TRACE_ENDPOINT

# Check sampling ratio
echo $KNOWD_OTEL_SAMPLE_RATIO
```

**"Metrics not appearing in Prometheus":**
```bash
# Test metrics endpoint
curl http://localhost:9090/metrics | head -5

# Verify Prometheus scrape config
curl http://prometheus:9090/api/v1/targets
```

**"High resource usage":**
```bash
# Check memory usage
curl http://localhost:6060/debug/pprof/heap | head -10

# Monitor goroutines
curl http://localhost:6060/debug/pprof/goroutine | wc -l

# Check active connections
netstat -an | grep :809 | wc -l
```

### Debug Commands

**Enable debug logging:**
```bash
KNOWD_LOG_LEVEL=debug \
KNOWD_LOG_FORMAT=json \
./knowd 2>&1 | jq .
```

**Profile specific operations:**
```bash
# Profile query execution
curl -X POST http://localhost:8090/v1/query/analyze \
     -H "Content-Type: application/json" \
     -d '{"query": "SELECT * WHERE { ?s ?p ?o }"}'
```

**Monitor resource usage:**
```bash
# CPU and memory
top -p $(pgrep knowd)

# Network I/O
iftop -i eth0

# Disk I/O
iostat -x 1
```

## Integration Examples

### Application-Level Tracing

**Add tracing to your code:**
```go
import "github.com/unrdf/knowd/internal/telemetry"

// In your function
func processTransaction(ctx context.Context, tx Transaction) error {
    ctx, span := telemetry.StartSpan(ctx, "transaction.process")
    defer span.End()

    telemetry.AddEvent(ctx, "transaction.started",
        telemetry.Attribute("tx_id", tx.ID),
        telemetry.Attribute("quads_count", len(tx.Delta.Add)))

    // Process transaction
    if err := applyDelta(ctx, tx.Delta); err != nil {
        telemetry.RecordError(ctx, err)
        return err
    }

    telemetry.AddEvent(ctx, "transaction.completed")
    return nil
}
```

### Custom Metrics

**Add application metrics:**
```go
import "go.opentelemetry.io/otel/metric"

// Get meter
meter := telemetry.Meter("my-service")

// Create counters
requestsTotal, _ := meter.Int64Counter("requests_total")
errorsTotal, _ := meter.Int64Counter("errors_total")

// Use in code
func handleRequest() {
    requestsTotal.Add(context.Background(), 1)

    if err := process(); err != nil {
        errorsTotal.Add(context.Background(), 1)
    }
}
```

This observability guide provides comprehensive coverage for monitoring, debugging, and optimizing Knowd deployments using OpenTelemetry and related tools.
