# Daemon Observability Metrics Guide

## Overview

This guide covers comprehensive observability for the UNRDF daemon and YAWL workflow integration using Prometheus metrics, OpenTelemetry (OTEL) tracing, and alert management.

## Architecture

The observability system comprises five integrated components:

1. **Prometheus Metrics Exporter** - Collects and exports metrics in Prometheus format
2. **OpenTelemetry Span Tracer** - Distributed tracing for operations
3. **YAWL Metrics Collector** - Workflow-specific metrics
4. **Alert Manager** - Rule-based alerting on metric thresholds
5. **Health Monitor** - Daemon health status tracking

## Prometheus Metrics Reference

### Counter Metrics

Counters track cumulative values that only increase over time.

#### `daemon.operations.scheduled`
- **Type**: Counter
- **Description**: Total operations scheduled for execution
- **Labels**: `operation_type`, `priority`
- **Unit**: Count
- **Example Use**: Monitor scheduling rate

#### `daemon.operations.executed`
- **Type**: Counter
- **Description**: Total operations successfully executed
- **Labels**: `operation_type`, `status`
- **Unit**: Count
- **Example Use**: Measure successful execution rate

#### `daemon.operations.failed`
- **Type**: Counter
- **Description**: Total operations that failed
- **Labels**: `operation_type`, `error_type`, `error_code`
- **Unit**: Count
- **Example Use**: Track error patterns

#### `daemon.queue.enqueued`
- **Type**: Counter
- **Description**: Total items added to queue
- **Labels**: `queue_id`, `priority`
- **Unit**: Count

#### `daemon.queue.dequeued`
- **Type**: Counter
- **Description**: Total items removed from queue
- **Labels**: `queue_id`, `result`
- **Unit**: Count

#### `yawl.cases.created`
- **Type**: Counter
- **Description**: Total YAWL cases created
- **Labels**: `spec_id`, `priority`
- **Unit**: Count

#### `yawl.cases.completed`
- **Type**: Counter
- **Description**: Total YAWL cases completed
- **Labels**: `spec_id`, `result`
- **Unit**: Count

#### `yawl.tasks.started`
- **Type**: Counter
- **Description**: Total YAWL tasks started
- **Labels**: `spec_id`, `task_type`
- **Unit**: Count

#### `yawl.tasks.completed`
- **Type**: Counter
- **Description**: Total YAWL tasks completed
- **Labels**: `spec_id`, `result`
- **Unit**: Count

#### `yawl.task.retries`
- **Type**: Counter
- **Description**: Total task retry attempts
- **Labels**: `spec_id`, `retry_count`
- **Unit**: Count

### Gauge Metrics

Gauges track values that can go up or down.

#### `daemon.queue.backlog`
- **Type**: Gauge
- **Description**: Current number of pending operations in queue
- **Labels**: `queue_id`
- **Unit**: Count
- **Alert**: Warning if > 100, Critical if > 500
- **SLO**: Should stay < 100 for 95% of time

#### `daemon.operations.pending`
- **Type**: Gauge
- **Description**: Current number of executing operations
- **Labels**: None
- **Unit**: Count
- **Alert**: Critical if > max_concurrent * 1.5

#### `daemon.cluster.nodes_healthy`
- **Type**: Gauge
- **Description**: Number of healthy nodes in cluster
- **Labels**: `cluster_id`
- **Unit**: Count
- **Alert**: Warning if < total_nodes * 0.8

#### `daemon.cluster.nodes_total`
- **Type**: Gauge
- **Description**: Total nodes in cluster
- **Labels**: `cluster_id`
- **Unit**: Count

#### `daemon.memory.usage_bytes`
- **Type**: Gauge
- **Description**: Current memory usage
- **Labels**: `node_id`
- **Unit**: Bytes
- **Alert**: Warning if > 70% available, Critical if > 90%

#### `daemon.health.status`
- **Type**: Gauge
- **Description**: Current health status (1=healthy, 0.5=degraded, 0=unhealthy)
- **Labels**: None
- **Unit**: Numeric

#### `yawl.concurrent_cases`
- **Type**: Gauge
- **Description**: Currently active YAWL cases
- **Labels**: `spec_id`
- **Unit**: Count

### Histogram Metrics

Histograms track distribution of values.

#### `daemon.operation.duration`
- **Type**: Histogram
- **Description**: Operation execution duration
- **Labels**: `operation_type`
- **Unit**: Milliseconds
- **Buckets**: 0.001, 0.01, 0.1, 1, 10, 100, 1000
- **SLO**: P95 < 1000ms, P99 < 5000ms

#### `daemon.queue.latency`
- **Type**: Histogram
- **Description**: Queue processing latency
- **Labels**: `queue_id`
- **Unit**: Milliseconds
- **Buckets**: 1, 10, 50, 100, 500, 1000

#### `yawl.case.duration`
- **Type**: Histogram
- **Description**: Case execution duration
- **Labels**: `spec_id`
- **Unit**: Milliseconds
- **Buckets**: 100, 1000, 5000, 30000, 60000

#### `yawl.task.duration`
- **Type**: Histogram
- **Description**: Task execution duration
- **Labels**: `spec_id`, `task_type`
- **Unit**: Milliseconds
- **Buckets**: 10, 100, 1000, 10000

## OpenTelemetry Span Integration

### Span Attributes

All spans include standard attributes:

```javascript
{
  // Standard attributes
  "service.name": "unrdf-daemon",
  "service.version": "6.0.0",
  "service.namespace": "unrdf",

  // Operation context
  "operation.id": "op-123",
  "operation.type": "schedule",
  "operation.priority": "high",

  // Resource context
  "resource.id": "resource-456",
  "resource.type": "workflow",

  // Execution context
  "execution.start_time": 1234567890,
  "execution.duration_ms": 150,

  // Error context (if applicable)
  "error.type": "timeout",
  "error.message": "Operation exceeded 5000ms"
}
```

### Span Events

Events are added to spans during execution:

#### Operation Started
```javascript
{
  name: "operation.started",
  attributes: {
    operation_id: "op-123",
    operation_type: "schedule",
    timestamp: 1234567890
  }
}
```

#### Validation Started
```javascript
{
  name: "validation.started",
  attributes: {
    validator_id: "validator-1",
    rule_count: 5
  }
}
```

#### Validation Completed
```javascript
{
  name: "validation.completed",
  attributes: {
    result: "success",
    violations_count: 0,
    duration_ms: 45
  }
}
```

#### Operation Failed
```javascript
{
  name: "operation.failed",
  attributes: {
    error_type: "validation_error",
    error_message: "Invalid input",
    recovery_action: "retry"
  }
}
```

## YAWL Workflow Metrics

### Case Metrics

Tracked for each YAWL case:

- **Case ID**: Unique identifier
- **Specification ID**: Workflow specification
- **Created At**: Case creation timestamp
- **Status**: created, executing, completed, failed
- **Duration**: Total case execution time
- **Task Count**: Number of tasks in case
- **Completed Tasks**: Tasks finished
- **Failed Tasks**: Tasks that failed

### Task Metrics

Tracked for each task:

- **Task ID**: Unique identifier
- **Case ID**: Parent case
- **Start Time**: Task start timestamp
- **Duration**: Task execution time
- **Status**: executing, success, failure, timeout
- **Retries**: Number of retry attempts
- **Metadata**: Custom task data

### Workflow Metrics

Aggregated per specification:

```javascript
{
  specId: "spec-approval",
  totalCases: 1250,
  completedCases: 1200,
  failedCases: 50,
  avgDuration: 45000,  // milliseconds
  totalTasks: 3750,
  successRate: 0.96    // 96% success
}
```

## Alert Manager Rules

### Built-in Rules

#### High Queue Backlog
```yaml
- name: "high_queue_backlog"
  condition: "daemon.queue.backlog > 100"
  severity: "warning"
  duration: "5m"
  description: "Queue backlog exceeded 100 items"
  action: "scale_workers"
```

#### Queue Backlog Critical
```yaml
- name: "queue_backlog_critical"
  condition: "daemon.queue.backlog > 500"
  severity: "critical"
  duration: "2m"
  description: "Queue backlog exceeded 500 items"
  action: "page_oncall"
```

#### High Error Rate
```yaml
- name: "high_error_rate"
  condition: "rate(daemon.operations.failed[5m]) > 0.1"
  severity: "warning"
  duration: "5m"
  description: "Error rate > 10%"
  action: "investigate_logs"
```

#### Operation Timeout
```yaml
- name: "operation_timeout"
  condition: "daemon.operation.duration[p99] > 5000"
  severity: "warning"
  duration: "10m"
  description: "Operation P99 latency exceeds 5 seconds"
  action: "analyze_slow_queries"
```

#### Low Health Status
```yaml
- name: "daemon_health_degraded"
  condition: "daemon.health.status < 0.8"
  severity: "warning"
  duration: "5m"
  description: "Daemon health status degraded"
  action: "check_resource_usage"
```

#### YAWL Case Timeout
```yaml
- name: "yawl_case_timeout"
  condition: "yawl.case.duration > 3600000"
  severity: "warning"
  duration: "5m"
  description: "YAWL case execution exceeded 1 hour"
  action: "review_workflow"
```

### Custom Rule Format

```javascript
{
  name: "rule_name",              // Unique rule identifier
  condition: "metric > threshold", // Description of condition
  severity: "info|warning|error|critical",
  evaluate: (context) => {
    // Function returning boolean
    return context.value > context.threshold;
  },
  metadata: {
    // Optional metadata
    category: "performance",
    owner: "platform-team"
  }
}
```

## Grafana Dashboard Templates

### Dashboard: Daemon Overview

```json
{
  "dashboard": {
    "title": "UNRDF Daemon Overview",
    "tags": ["daemon", "unrdf"],
    "timezone": "browser",
    "panels": [
      {
        "title": "Operations Per Minute",
        "targets": [
          {
            "expr": "rate(daemon.operations.executed[1m])"
          }
        ],
        "type": "graph"
      },
      {
        "title": "Queue Backlog",
        "targets": [
          {
            "expr": "daemon.queue.backlog"
          }
        ],
        "type": "gauge",
        "thresholds": "100,500"
      },
      {
        "title": "Operation Duration P95",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, daemon.operation.duration)"
          }
        ],
        "type": "graph"
      },
      {
        "title": "Success Rate",
        "targets": [
          {
            "expr": "daemon.operations.executed / (daemon.operations.executed + daemon.operations.failed)"
          }
        ],
        "type": "stat",
        "unit": "percentunit"
      },
      {
        "title": "Health Status",
        "targets": [
          {
            "expr": "daemon.health.status"
          }
        ],
        "type": "gauge",
        "thresholds": "0.8,0.95"
      },
      {
        "title": "Cluster Health",
        "targets": [
          {
            "expr": "daemon.cluster.nodes_healthy"
          }
        ],
        "type": "stat"
      }
    ]
  }
}
```

### Dashboard: YAWL Workflow Metrics

```json
{
  "dashboard": {
    "title": "YAWL Workflow Metrics",
    "tags": ["yawl", "workflow"],
    "panels": [
      {
        "title": "Cases Per Hour",
        "targets": [
          {
            "expr": "rate(yawl.cases.created[1h])"
          }
        ],
        "type": "graph"
      },
      {
        "title": "Success Rate by Workflow",
        "targets": [
          {
            "expr": "yawl.cases.completed / (yawl.cases.completed + yawl.cases.failed)"
          }
        ],
        "type": "table",
        "legendFormat": "{{spec_id}}"
      },
      {
        "title": "Case Duration by Workflow",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, yawl.case.duration)"
          }
        ],
        "type": "graph",
        "legendFormat": "{{spec_id}}"
      },
      {
        "title": "Active Cases",
        "targets": [
          {
            "expr": "yawl.concurrent_cases"
          }
        ],
        "type": "graph"
      },
      {
        "title": "Task Retry Rate",
        "targets": [
          {
            "expr": "rate(yawl.task.retries[5m])"
          }
        ],
        "type": "graph"
      }
    ]
  }
}
```

### Dashboard: Alert Status

```json
{
  "dashboard": {
    "title": "Alert Status",
    "tags": ["alerts"],
    "panels": [
      {
        "title": "Active Alerts",
        "targets": [
          {
            "expr": "alertmanager_alerts{state=\"firing\"}"
          }
        ],
        "type": "stat"
      },
      {
        "title": "Alerts by Severity",
        "targets": [
          {
            "expr": "alertmanager_alerts by (severity)"
          }
        ],
        "type": "pie"
      },
      {
        "title": "Alert Firing Timeline",
        "targets": [
          {
            "expr": "changes(alertmanager_alerts[1m])"
          }
        ],
        "type": "graph"
      }
    ]
  }
}
```

## AlertManager Configuration

### AlertManager YAML Config

```yaml
global:
  resolve_timeout: 5m
  slack_api_url: 'https://hooks.slack.com/services/YOUR/WEBHOOK/URL'

route:
  group_by: ['alertname', 'cluster', 'service']
  group_wait: 30s
  group_interval: 5m
  repeat_interval: 12h
  receiver: 'default'
  routes:
    - match:
        severity: critical
      receiver: 'critical'
      continue: true
    - match:
        severity: warning
      receiver: 'warning'

receivers:
  - name: 'default'
    slack_configs:
      - channel: '#alerts'
        title: 'Daemon Alert'
        text: '{{ range .Alerts }}{{ .Annotations.description }}{{ end }}'

  - name: 'critical'
    slack_configs:
      - channel: '#critical-alerts'
        title: 'CRITICAL: {{ .GroupLabels.alertname }}'
        text: '{{ range .Alerts }}{{ .Annotations.description }}\n{{ end }}'
    pagerduty_configs:
      - service_key: 'YOUR_SERVICE_KEY'

  - name: 'warning'
    slack_configs:
      - channel: '#warning-alerts'

inhibit_rules:
  - source_match:
      severity: critical
    target_match:
      severity: warning
    equal: ['alertname', 'service']
```

## Best Practices for Monitoring

### 1. Metric Collection

- **Collect all operation types**: Schedule, execute, fail metrics
- **Track latencies**: Use histograms for P50, P95, P99
- **Monitor queue health**: Backlog and latency together
- **Measure resource usage**: Memory, CPU, disk

### 2. Alert Configuration

- **Avoid alert fatigue**: Set appropriate thresholds
- **Group related alerts**: Use route matching
- **Include context**: Add description and runbook links
- **Test alerts regularly**: Ensure notifications work

### 3. Dashboard Design

- **Single view summary**: Health status, key metrics
- **Drill-down capability**: Link to detailed views
- **Time correlation**: Show related metrics together
- **Threshold visualization**: Make SLO status visible

### 4. SLO Definition

Define Service Level Objectives:

```
Operation Latency SLO:
- P95 < 1000ms (99% availability)
- P99 < 5000ms (99% availability)

Operation Success SLO:
- Success rate > 95% (99% availability)

Queue Backlog SLO:
- Backlog < 100 for 95% of time
- Backlog never > 500

YAWL Workflow SLO:
- Case completion < 1 hour P95
- Success rate > 95%
```

### 5. Runbook Links

Add runbook URLs to alerts:

```yaml
- alert: HighQueueBacklog
  annotations:
    runbook_url: "https://docs.example.com/runbooks/queue-backlog"
    description: "Queue backlog > 100 (current: {{ $value }})"
```

### 6. Metrics Retention

- **Real-time**: 5 minutes (highest resolution)
- **Short-term**: 1 hour (1 minute resolution)
- **Medium-term**: 7 days (5 minute resolution)
- **Long-term**: 90 days (1 hour resolution)

### 7. Integration Points

- **Prometheus**: Central metrics collection
- **Grafana**: Visualization and dashboards
- **AlertManager**: Alert routing and grouping
- **PagerDuty**: Incident management (critical only)
- **Slack**: Team notifications
- **ELK Stack**: Log correlation and analysis

## Troubleshooting

### High Queue Backlog

**Symptoms**: `daemon.queue.backlog > 100`

**Investigation**:
1. Check `daemon.operations.executed` rate
2. Compare with `daemon.operations.scheduled` rate
3. Look at operation duration percentiles
4. Check cluster node health

**Resolution**:
1. Scale worker processes
2. Optimize slow operations
3. Check for resource constraints
4. Review operation priorities

### Low Health Status

**Symptoms**: `daemon.health.status < 0.8`

**Investigation**:
1. Check `daemon.operations.failed` count
2. Review error types and messages
3. Check YAWL case/task failures
4. Review system resource usage

**Resolution**:
1. Identify failing operation types
2. Fix error root causes
3. Add error handling
4. Increase resource limits if needed

### YAWL Workflow Delays

**Symptoms**: `yawl.case.duration[p95] > 1 hour`

**Investigation**:
1. Check task durations by type
2. Look for stuck cases
3. Review task retry patterns
4. Check queue backlog during execution

**Resolution**:
1. Optimize slow task types
2. Adjust timeout thresholds
3. Review workflow design
4. Increase concurrent case limit

## Implementation Example

```javascript
import {
  PrometheusMetricsExporter,
  OtelSpanTracer,
  YawlMetricsCollector,
  AlertManager,
  DaemonHealthMonitor,
} from '@unrdf/daemon/integrations/observability';

// Initialize components
const prometheus = new PrometheusMetricsExporter({ serviceName: 'my-daemon' });
const tracer = new OtelSpanTracer({ serviceName: 'my-daemon' });
const yawlMetrics = new YawlMetricsCollector();
const alerts = new AlertManager();
const health = new DaemonHealthMonitor();

// Register metrics
prometheus.registerCounter('daemon.operations', 'Operations count');
prometheus.registerHistogram('daemon.duration', 'Operation duration', [0.1, 1, 10, 100]);

// Add alert rules
alerts.addRule({
  name: 'high_backlog',
  condition: 'backlog > 100',
  severity: 'warning',
  evaluate: ({ value }) => value > 100,
});

// Track operation
const spanId = tracer.startSpan('process_operation', { operationId: 'op-1' });
const startTime = Date.now();

try {
  // Do work
  const duration = Date.now() - startTime;

  health.recordSuccess(duration);
  prometheus.incrementCounter('daemon.operations', 1);
  prometheus.recordHistogram('daemon.duration', duration);
  tracer.endSpan(spanId, 'ok');
} catch (error) {
  health.recordFailure();
  prometheus.incrementCounter('daemon.operations.failed', 1);
  tracer.addSpanEvent(spanId, 'error', { message: error.message });
  tracer.endSpan(spanId, 'error');
}

// Export metrics
console.log(prometheus.export());
console.log(tracer.exportTraces());
```

## Additional Resources

- [Prometheus Documentation](https://prometheus.io/docs/)
- [OpenTelemetry Guide](https://opentelemetry.io/docs/)
- [Grafana Dashboard Guide](https://grafana.com/docs/grafana/latest/features/panels/graph/)
- [AlertManager Configuration](https://prometheus.io/docs/alerting/latest/configuration/)
- [YAWL Engine Documentation](https://yawlfoundation.org/)
