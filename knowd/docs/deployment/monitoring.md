# Monitoring and Observability

This guide covers monitoring, logging, and observability for Knowd deployments, including metrics collection, distributed tracing, and alerting.

## Metrics Collection

### Prometheus Metrics

**Built-in metrics endpoint:**
```bash
# Enable metrics endpoint
KNOWD_METRICS_ADDR=:9090 ./knowd

# Scrape metrics
curl http://localhost:9090/metrics
```

**Prometheus configuration:**
```yaml
# prometheus.yml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'knowd'
    static_configs:
      - targets: ['knowd-leader:9090', 'knowd-follower1:9090', 'knowd-follower2:9090']
    scrape_interval: 15s
    metrics_path: '/metrics'
```

**Key metrics to monitor:**
```bash
# Query performance
knowd_query_duration_seconds_bucket{le="0.1"}  # p50 query latency
knowd_query_duration_seconds_bucket{le="0.5"}  # p95 query latency
knowd_query_total                               # Total queries executed

# Transaction performance
knowd_tx_duration_seconds_bucket{le="0.5"}     # p95 transaction latency
knowd_tx_total                                 # Total transactions

# Cache performance
knowd_plan_cache_hits_total                    # Query plan cache hits
knowd_plan_cache_misses_total                  # Query plan cache misses
knowd_plan_cache_size                          # Current cache size

# Storage metrics
knowd_store_quads_total                        # Total stored quads
knowd_store_segments_total                     # WAL segments
knowd_store_snapshot_age_seconds               # Time since last snapshot

# Hook performance
knowd_hook_execution_duration_seconds          # Hook execution time
knowd_hook_total                               # Total hook executions

# Cluster metrics
knowd_cluster_replication_lag_seconds          # Replication lag
knowd_cluster_wal_size_bytes                   # WAL size
```

### Grafana Dashboards

**Application dashboard:**
```json
{
  "dashboard": {
    "title": "Knowd Application Metrics",
    "panels": [
      {
        "title": "Query Latency",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, rate(knowd_query_duration_seconds_bucket[5m]))",
            "legendFormat": "p95 Query Latency"
          }
        ]
      },
      {
        "title": "Query Throughput",
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

**Infrastructure dashboard:**
```json
{
  "dashboard": {
    "title": "Knowd Infrastructure Metrics",
    "panels": [
      {
        "title": "CPU Usage",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(container_cpu_usage_seconds_total{container=\"knowd\"}[5m])",
            "legendFormat": "CPU Usage"
          }
        ]
      },
      {
        "title": "Memory Usage",
        "type": "graph",
        "targets": [
          {
            "expr": "container_memory_usage_bytes{container=\"knowd\"}",
            "legendFormat": "Memory Usage"
          }
        ]
      }
    ]
  }
}
```

## Distributed Tracing

### OpenTelemetry Integration

**Enable tracing:**
```bash
# Enable OTLP tracing
KNOWD_OTEL_EXPORTER=otlp \
KNOWD_OTEL_ENDPOINT=http://jaeger:14268/api/traces \
./knowd
```

**Jaeger configuration:**
```yaml
# jaeger-config.yaml
service:
  name: knowd
  version: v1.0.0

reporter:
  localAgentHostPort: jaeger-agent:6831

sampler:
  type: const
  param: 1  # Sample all traces

collector:
  http:
    endpoint: http://jaeger-collector:14268/api/traces
```

**Key traces to monitor:**
- **Transaction Processing**: Full transaction lifecycle
- **Query Execution**: SPARQL query planning and execution
- **Hook Execution**: Policy evaluation and effects
- **Cluster Operations**: Replication and federation

### Trace Visualization

**Transaction trace:**
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

## Logging

### Structured Logging

**JSON logging configuration:**
```bash
# Enable structured logging
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
  "hooks_executed": 2
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
    Path              /var/log/containers/*knowd*.log
    Parser            docker
    Tag               knowd.*
    Refresh_Interval  5

[FILTER]
    Name              kubernetes
    Match             knowd.*
    Kube_URL          https://kubernetes.default.svc:443
    Kube_CA_File      /var/run/secrets/kubernetes.io/serviceaccount/ca.crt
    Kube_Token_File   /var/run/secrets/kubernetes.io/serviceaccount/token
    Kube_Tag_Prefix   knowd.var.log.containers.

[OUTPUT]
    Name  elasticsearch
    Match *
    Host  elasticsearch
    Port  9200
    Logstash_Format On
```

**Elasticsearch mapping:**
```json
{
  "mappings": {
    "properties": {
      "timestamp": {"type": "date"},
      "level": {"type": "keyword"},
      "logger": {"type": "keyword"},
      "message": {"type": "text"},
      "transaction_id": {"type": "keyword"},
      "duration_ms": {"type": "integer"},
      "quads_affected": {"type": "integer"},
      "hooks_executed": {"type": "integer"}
    }
  }
}
```

## Alerting

### Prometheus Alert Rules

**Critical alerts:**
```yaml
# alert-rules.yml
groups:
  - name: knowd.critical
    rules:
    - alert: KnowdDown
      expr: up{job="knowd"} == 0
      for: 1m
      labels:
        severity: critical
      annotations:
        summary: "Knowd instance is down"
        description: "Knowd instance {{ $labels.instance }} has been down for more than 1 minute"

    - alert: HighErrorRate
      expr: rate(knowd_http_requests_total{status=~"5.."}[5m]) / rate(knowd_http_requests_total[5m]) > 0.05
      for: 5m
      labels:
        severity: critical
      annotations:
        summary: "High error rate detected"
        description: "Error rate is {{ $value | humanizePercentage }} on {{ $labels.instance }}"

    - alert: HighLatency
      expr: histogram_quantile(0.95, rate(knowd_query_duration_seconds_bucket[5m])) > 1
      for: 5m
      labels:
        severity: warning
      annotations:
        summary: "High query latency"
        description: "95th percentile query latency is {{ $value }}s on {{ $labels.instance }}"
```

**Performance alerts:**
```yaml
- alert: LowCacheHitRate
  expr: rate(knowd_plan_cache_hits_total[5m]) / rate(knowd_plan_cache_total[5m]) < 0.5
  for: 10m
  labels:
    severity: warning
  annotations:
    summary: "Low query plan cache hit rate"
    description: "Cache hit rate is {{ $value | humanizePercentage }} on {{ $labels.instance }}"

- alert: HighReplicationLag
  expr: knowd_cluster_replication_lag_seconds > 60
  for: 5m
  labels:
    severity: warning
  annotations:
    summary: "High replication lag"
    description: "Replication lag is {{ $value }}s on {{ $labels.instance }}"
```

### AlertManager Configuration

**Routing configuration:**
```yaml
# alertmanager.yml
route:
  group_by: ['alertname']
  group_wait: 10s
  group_interval: 10s
  repeat_interval: 1h
  receiver: 'knowd-team'

receivers:
- name: 'knowd-team'
  slack_configs:
  - api_url: 'https://hooks.slack.com/services/YOUR/SLACK/WEBHOOK'
    channel: '#knowd-alerts'
    send_resolved: true
    title: 'Knowd Alert'
    text: '{{ range .Alerts }}{{ .Annotations.summary }}: {{ .Annotations.description }}\n{{ end }}'
```

## Performance Profiling

### CPU Profiling

**Enable pprof endpoints:**
```bash
# Enable profiling
KNOWD_PPROF_ADDR=:6060 ./knowd

# Collect CPU profile
curl http://localhost:6060/debug/pprof/profile?seconds=30 > cpu.prof

# Analyze profile
go tool pprof cpu.prof
```

**Top functions by CPU usage:**
```bash
(pprof) top10
Showing nodes accounting for 85.50% of 30.02s total
      flat  flat%   sum%        cum   cum%
    15.23s 50.74% 50.74%     15.23s 50.74%  knowd/internal/sparql.(*Executor).executeSelect
     5.67s 18.89% 69.63%      5.67s 18.89%  knowd/internal/hooks.(*Batch).Execute
     2.34s  7.79% 77.42%      2.34s  7.79%  knowd/internal/store.(*diskStore).FindQuads
     1.23s  4.10% 81.52%      1.23s  4.10%  knowd/internal/shacl.(*Validator).Validate
     0.89s  2.96% 84.48%      0.89s  2.96%  knowd/internal/lockchain.(*Lockchain).WriteReceipt
```

### Memory Profiling

**Heap profiling:**
```bash
# Collect heap profile
curl http://localhost:6060/debug/pprof/heap > heap.prof

# Analyze memory usage
go tool pprof heap.prof

# Top memory consumers
(pprof) top10 -cum
```

**Memory leak detection:**
```bash
# Compare heap profiles over time
go tool pprof -diff_base=heap1.prof heap2.prof
```

## Application-Specific Monitoring

### Query Performance

**Slow query detection:**
```bash
# Find slow queries
curl "http://localhost:9090/metrics" | grep knowd_query_duration_seconds_bucket

# Query plan analysis
curl -X POST http://localhost:8090/v1/query/analyze \
     -H "Content-Type: application/json" \
     -d '{"query": "SELECT * WHERE { ?s ?p ?o }"}'
```

**Cache effectiveness:**
```bash
# Monitor cache hit rates
curl "http://localhost:9090/metrics" | grep knowd_plan_cache

# Cache size monitoring
curl "http://localhost:9090/metrics" | grep knowd_plan_cache_size
```

### Hook Performance

**Hook execution monitoring:**
```bash
# Monitor hook execution times
curl "http://localhost:9090/metrics" | grep knowd_hook

# Hook failure rates
curl "http://localhost:9090/metrics" | grep knowd_hook_failures
```

### Cluster Health

**Replication monitoring:**
```bash
# Check replication lag
curl http://leader:8090/v1/cluster/status

# Monitor WAL size
curl "http://localhost:9090/metrics" | grep knowd_cluster_wal

# Network latency between nodes
ping follower1
```

## Log Analysis

### Log Patterns

**Error patterns:**
```bash
# Find authentication errors
grep "authentication failed" /var/log/knowd/*.log

# Find timeout errors
grep "context deadline exceeded" /var/log/knowd/*.log

# Find validation errors
grep "SHACL validation failed" /var/log/knowd/*.log
```

**Performance patterns:**
```bash
# Find slow queries
grep "query execution" /var/log/knowd/*.log | awk '$NF > 500 {print}'

# Find high-latency transactions
grep "transaction completed" /var/log/knowd/*.log | awk '$NF > 1000 {print}'
```

### Log Aggregation Queries

**Elasticsearch queries:**
```bash
# Error rate over time
GET /knowd-*/_search
{
  "query": {
    "range": {
      "@timestamp": {
        "gte": "now-1h"
      }
    }
  },
  "aggs": {
    "error_rate": {
      "terms": {
        "field": "level.keyword",
        "size": 10
      }
    }
  }
}

# Performance trends
GET /knowd-*/_search
{
  "query": {
    "match": {
      "message": "query execution"
    }
  },
  "aggs": {
    "avg_latency": {
      "avg": {
        "field": "duration_ms"
      }
    }
  }
}
```

## Dashboard Setup

### Grafana Configuration

**Datasource setup:**
```yaml
# grafana-datasources.yml
apiVersion: 1

datasources:
- name: Prometheus
  type: prometheus
  access: proxy
  url: http://prometheus:9090
  isDefault: true

- name: Elasticsearch
  type: elasticsearch
  access: proxy
  url: http://elasticsearch:9200
  database: "knowd-*"
```

**Dashboard JSON:**
```json
{
  "dashboard": {
    "title": "Knowd Production Dashboard",
    "tags": ["knowd", "production"],
    "timezone": "browser",
    "panels": [
      {
        "title": "Query Performance",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, rate(knowd_query_duration_seconds_bucket[5m]))",
            "legendFormat": "p95 Query Latency"
          }
        ],
        "yAxes": [
          {
            "label": "Seconds",
            "max": 2
          }
        ]
      },
      {
        "title": "Error Rate",
        "type": "singlestat",
        "targets": [
          {
            "expr": "rate(knowd_http_requests_total{status=~\"5..\"}[5m]) / rate(knowd_http_requests_total[5m]) * 100",
            "legendFormat": "Error Rate %"
          }
        ],
        "thresholds": "0.1,1,5",
        "colors": ["rgba(50, 172, 45, 0.97)", "rgba(237, 129, 40, 0.89)", "rgba(245, 54, 54, 0.9)"]
      }
    ],
    "time": {
      "from": "now-6h",
      "to": "now"
    },
    "refresh": "30s"
  }
}
```

## Operational Monitoring

### Health Checks

**Application health:**
```bash
# Basic health check
curl http://localhost:8090/healthz

# Detailed health status
curl http://localhost:8090/v1/store/stats

# Cluster health
curl http://localhost:8090/v1/cluster/status
```

**Container health:**
```yaml
# Kubernetes health checks
livenessProbe:
  httpGet:
    path: /healthz
    port: 8090
  initialDelaySeconds: 30
  periodSeconds: 10
  timeoutSeconds: 5

readinessProbe:
  httpGet:
    path: /healthz
    port: 8090
  initialDelaySeconds: 5
  periodSeconds: 5
  timeoutSeconds: 3
```

### Resource Monitoring

**System metrics:**
```bash
# CPU and memory usage
curl "http://localhost:9090/metrics" | grep container_cpu_usage_seconds_total
curl "http://localhost:9090/metrics" | grep container_memory_usage_bytes

# Disk usage
df -h /var/lib/knowd

# Network I/O
iftop -i eth0
```

**Application-specific metrics:**
```bash
# Query throughput
curl "http://localhost:9090/metrics" | grep knowd_query_total

# Cache statistics
curl "http://localhost:9090/metrics" | grep knowd_plan_cache

# Hook execution metrics
curl "http://localhost:9090/metrics" | grep knowd_hook
```

## Troubleshooting Tools

### Diagnostic Commands

**Application debugging:**
```bash
# Check configuration
curl http://localhost:8090/v1/store/stats

# View active connections
netstat -tuln | grep :8090

# Check process status
ps aux | grep knowd

# View environment variables
cat /proc/$(pgrep knowd)/environ | tr '\0' '\n'
```

**Log analysis:**
```bash
# Real-time log monitoring
tail -f /var/log/knowd/knowd.log | jq .

# Search for specific errors
grep -r "authentication failed" /var/log/knowd/

# Count error occurrences
grep "ERROR" /var/log/knowd/knowd.log | wc -l
```

**Performance analysis:**
```bash
# Memory usage analysis
curl http://localhost:6060/debug/pprof/heap > heap.prof
go tool pprof heap.prof

# CPU profiling
curl http://localhost:6060/debug/pprof/profile?seconds=30 > cpu.prof
go tool pprof cpu.prof

# Goroutine dump
curl http://localhost:6060/debug/pprof/goroutine > goroutines.txt
```

## Best Practices

### Monitoring Strategy

1. **Define SLIs/SLOs** - Set clear performance targets
2. **Alert on symptoms** - Monitor user experience, not implementation details
3. **Use dashboards** - Visualize trends and patterns
4. **Automate responses** - Implement automated remediation where possible
5. **Regular reviews** - Continuously improve monitoring coverage

### Alerting Best Practices

1. **Actionable alerts** - Every alert should have a clear remediation path
2. **Alert fatigue prevention** - Use proper thresholds and grouping
3. **Escalation policies** - Define clear escalation procedures
4. **False positive reduction** - Tune thresholds based on historical data
5. **Documentation** - Document alert meaning and response procedures

### Performance Monitoring

1. **Baseline establishment** - Measure performance before optimizations
2. **Regression detection** - Alert on performance degradation
3. **Resource planning** - Use metrics for capacity planning
4. **Bottleneck identification** - Use profiling to find optimization opportunities
5. **Trend analysis** - Track performance changes over time

This monitoring and observability guide ensures comprehensive visibility into Knowd's operation, enabling proactive issue detection, performance optimization, and reliable service delivery.
