# @unrdf/yawl-observability

Comprehensive workflow observability framework for YAWL (Yet Another Workflow Language) engine with Prometheus metrics, OpenTelemetry distributed tracing, and custom Service Level Indicators (SLIs).

## Features

- **Prometheus Metrics**: Complete workflow execution metrics in Prometheus format
- **OpenTelemetry Tracing**: Distributed tracing with receipt-based proof correlation
- **Custom SLIs**: Service Level Indicators for workflow performance monitoring
- **Grafana Integration**: Pre-built dashboard for visualization
- **SLO Compliance**: Automated Service Level Objective monitoring

## Installation

```bash
pnpm add @unrdf/yawl-observability
```

## Quick Start

```javascript
import { createWorkflowEngine } from '@unrdf/yawl';
import {
  YAWLMetricsCollector,
  YAWLTracer,
  YAWLSLICalculator
} from '@unrdf/yawl-observability';

// Create YAWL engine
const engine = createWorkflowEngine();

// Setup observability
const metrics = new YAWLMetricsCollector(engine, {
  defaultLabels: { environment: 'production', region: 'us-east-1' }
});

const tracer = new YAWLTracer(engine, {
  includeReceiptHashes: true
});

const sli = new YAWLSLICalculator(engine, {
  targetCompletionRate: 0.95,
  targetTaskSuccessRate: 0.99,
  targetP95Latency: 5.0
});

// Expose metrics endpoint (Express example)
app.get('/metrics', async (req, res) => {
  res.set('Content-Type', metrics.contentType);
  res.end(await metrics.getMetrics());
});

// Get SLI snapshot
const snapshot = sli.getSnapshot();
console.log(`SLO Compliance: ${snapshot.sloCompliance.score * 100}%`);
```

## Metrics Catalog

### Case Lifecycle Metrics

| Metric | Type | Description | Labels |
|--------|------|-------------|--------|
| `yawl_workflow_cases_total` | Counter | Total cases by status | `workflow_id`, `status` |
| `yawl_case_completion_time_seconds` | Histogram | End-to-end case duration | `workflow_id` |

**Status values**: `created`, `started`, `completed`

**Example queries**:
```promql
# Completion rate by workflow
rate(yawl_workflow_cases_total{status="completed"}[5m])
  / rate(yawl_workflow_cases_total{status="created"}[5m])

# Average case completion time
histogram_quantile(0.50, rate(yawl_case_completion_time_seconds_bucket[5m]))
```

### Task Execution Metrics

| Metric | Type | Description | Labels |
|--------|------|-------------|--------|
| `yawl_workflow_tasks_total` | Counter | Total tasks by status | `workflow_id`, `task_id`, `status` |
| `yawl_task_duration_seconds` | Histogram | Task execution time | `workflow_id`, `task_id` |
| `yawl_task_wait_time_seconds` | Histogram | Time from enabled to started | `workflow_id`, `task_id` |
| `yawl_workflow_active_tasks` | Gauge | Currently running tasks | `workflow_id` |
| `yawl_workflow_enabled_tasks` | Gauge | Currently enabled tasks | `workflow_id` |
| `yawl_task_errors_total` | Counter | Task failures and timeouts | `workflow_id`, `task_id`, `error_type` |

**Status values**: `enabled`, `started`, `completed`, `cancelled`

**Error types**: `cancelled`, `timeout`, `failed`

**Example queries**:
```promql
# Task p95 execution time
histogram_quantile(0.95, rate(yawl_task_duration_seconds_bucket[5m]))

# Task error rate by workflow
rate(yawl_task_errors_total[5m]) / rate(yawl_workflow_tasks_total{status="started"}[5m])

# Current workload
sum(yawl_workflow_active_tasks) by (workflow_id)
```

### Pattern Usage Metrics

| Metric | Type | Description | Labels |
|--------|------|-------------|--------|
| `yawl_pattern_usage_count` | Counter | YAWL pattern usage | `workflow_id`, `task_id`, `pattern_type`, `operation` |

**Pattern types**: `and`, `xor`, `or`, `sequence`

**Operations**: `split`, `join`

**Example queries**:
```promql
# Pattern usage distribution
sum by (pattern_type, operation) (yawl_pattern_usage_count)

# XOR split usage rate
rate(yawl_pattern_usage_count{pattern_type="xor", operation="split"}[5m])
```

### Resource Metrics

| Metric | Type | Description | Labels |
|--------|------|-------------|--------|
| `yawl_resource_allocations_total` | Counter | Resource allocation events | `workflow_id`, `role`, `resource_id` |
| `yawl_resource_utilization` | Gauge | Resource utilization (0-1) | `resource_id`, `role` |

**Example queries**:
```promql
# Average resource utilization
avg(yawl_resource_utilization)

# Resource allocation rate by role
rate(yawl_resource_allocations_total[5m]) by (role)
```

### Service Level Indicators (SLIs)

| Metric | Type | Description |
|--------|------|-------------|
| `yawl_sli_completion_rate` | Gauge | Workflow completion rate (0-1) |
| `yawl_sli_task_success_rate` | Gauge | Task success rate (0-1) |
| `yawl_sli_task_error_rate` | Gauge | Task error rate (0-1) |
| `yawl_sli_p95_latency_seconds` | Gauge | 95th percentile task latency |
| `yawl_sli_resource_utilization` | Gauge | Average resource utilization |
| `yawl_slo_compliance` | Gauge | SLO compliance score (0-1) |

**Example queries**:
```promql
# SLO compliance over time
yawl_slo_compliance

# Task success rate trend
rate(yawl_sli_task_success_rate[5m])
```

## OpenTelemetry Tracing

### Span Structure

```
workflow.case (caseId)
├── task.taskId1 (workItemId1)
├── task.taskId2 (workItemId2)
└── task.taskId3 (workItemId3)
```

### Span Attributes

**Case Spans**:
- `workflow.id`: Workflow definition ID
- `workflow.case.id`: Unique case ID
- `workflow.case.status`: Case status

**Task Spans**:
- `workflow.id`: Workflow definition ID
- `workflow.case.id`: Case ID
- `workflow.task.id`: Task definition ID
- `workflow.task.work_item_id`: Work item instance ID
- `workflow.task.status`: Task status
- `workflow.task.resource_id`: Assigned resource
- `workflow.task.actor`: Actor who executed task

**Receipt Correlation** (when enabled):
- `workflow.receipt.hash`: BLAKE3 receipt hash
- `workflow.receipt.previous_hash`: Previous receipt hash
- `workflow.receipt.event_type`: Event type

### Receipt-Correlated Tracing

```javascript
const tracer = new YAWLTracer(engine, {
  includeReceiptHashes: true
});

// Execute operation with cryptographic proof
await tracer.spanWithReceipt('external.approval', receipt, async () => {
  return await externalService.approve(data);
});
```

### Custom Spans

```javascript
// Create custom span within case context
await tracer.spanInCase(caseId, 'custom.validation', async () => {
  // Your custom logic
  return validateData(data);
});
```

## Service Level Indicators (SLIs)

### Configuration

```javascript
const sli = new YAWLSLICalculator(engine, {
  windowMs: 300000, // 5 minute window
  targetCompletionRate: 0.95, // 95% completion target
  targetTaskSuccessRate: 0.99, // 99% success target
  targetP95Latency: 5.0, // 5 second p95 target
  targetResourceUtilization: 0.80 // 80% utilization target
});
```

### Getting SLI Snapshots

```javascript
const snapshot = sli.getSnapshot();
console.log({
  completionRate: snapshot.completionRate,
  taskSuccessRate: snapshot.taskSuccessRate,
  p95Latency: snapshot.p95Latency,
  sloCompliance: snapshot.sloCompliance.score
});
```

### SLO Compliance Report

```javascript
const report = sli.getSLOReport();
console.log(report);
// {
//   timestamp: '2024-01-15T10:30:00.000Z',
//   overall: {
//     compliant: true,
//     score: 1.0,
//     meetsCount: 4,
//     totalCount: 4
//   },
//   metrics: [
//     {
//       name: 'Completion Rate',
//       current: 0.98,
//       target: 0.95,
//       compliant: true,
//       status: 'PASS'
//     },
//     ...
//   ]
// }
```

## Grafana Dashboard

Import the pre-built dashboard for complete visualization:

```bash
# Dashboard location
packages/yawl-observability/src/examples/grafana-dashboard.json
```

### Dashboard Panels

1. **SLO Compliance Score** - Overall compliance gauge
2. **Workflow Completion Rate** - Success rate over time
3. **Task Success Rate** - Task-level reliability
4. **P95 Task Latency** - Performance monitoring
5. **Task Error Rate** - Error trending
6. **Resource Utilization** - Resource efficiency
7. **Cases by Status** - Case throughput
8. **Task Duration Distribution** - Latency percentiles (p50, p95, p99)
9. **Active vs Enabled Tasks** - Workflow state
10. **Pattern Usage** - YAWL pattern distribution
11. **Task Errors by Type** - Error breakdown
12. **Case Completion Time** - End-to-end duration heatmap
13. **Task Wait Time** - Queueing metrics
14. **Resource Allocations** - Resource activity
15. **Per-Resource Utilization** - Individual resource metrics

### Configuration Steps

1. Install Grafana and Prometheus
2. Configure Prometheus to scrape your metrics endpoint
3. Import `grafana-dashboard.json` into Grafana
4. Select Prometheus datasource
5. Set refresh interval (recommended: 10s)

## Advanced Usage

### Custom Metrics Labels

```javascript
const metrics = new YAWLMetricsCollector(engine, {
  prefix: 'myapp_yawl',
  defaultLabels: {
    environment: 'production',
    region: 'us-east-1',
    cluster: 'workflow-cluster-1',
    version: '2.1.0'
  }
});
```

### Multiple Exporters

```javascript
import { NodeSDK } from '@opentelemetry/sdk-node';
import { PrometheusExporter } from '@opentelemetry/exporter-prometheus';
import { JaegerExporter } from '@opentelemetry/exporter-jaeger';

// Setup Prometheus + Jaeger
const prometheusExporter = new PrometheusExporter({ port: 9464 });
const jaegerExporter = new JaegerExporter({ endpoint: 'http://jaeger:14268/api/traces' });

const sdk = new NodeSDK({
  metricReader: prometheusExporter,
  traceExporter: jaegerExporter
});

sdk.start();

// Now create YAWL observability
const tracer = new YAWLTracer(engine);
```

### Alerting Rules

Example Prometheus alerting rules:

```yaml
groups:
  - name: yawl_slo_alerts
    rules:
      - alert: WorkflowCompletionRateLow
        expr: yawl_sli_completion_rate < 0.95
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Workflow completion rate below target"
          description: "Completion rate {{ $value }} is below 95% target"

      - alert: TaskErrorRateHigh
        expr: yawl_sli_task_error_rate > 0.05
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: "Task error rate above threshold"
          description: "Error rate {{ $value }} exceeds 5% threshold"

      - alert: P95LatencyHigh
        expr: yawl_sli_p95_latency_seconds > 10
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "P95 latency exceeds target"
          description: "P95 latency {{ $value }}s exceeds 10s target"
```

## API Reference

### YAWLMetricsCollector

#### Constructor
```javascript
new YAWLMetricsCollector(engine, config)
```

**Config Options**:
- `prefix`: Metric name prefix (default: `'yawl'`)
- `collectDefaultMetrics`: Enable Node.js metrics (default: `true`)
- `defaultMetricsInterval`: Collection interval in ms (default: `10000`)
- `defaultLabels`: Custom labels for all metrics (default: `{}`)
- `durationBuckets`: Histogram buckets in seconds (default: `[0.001, 0.01, 0.1, 0.5, 1, 2.5, 5, 10, 30, 60]`)

#### Methods

- `async getMetrics()`: Get metrics in Prometheus format
- `getMetric(name)`: Get specific metric by name
- `resetMetrics()`: Reset all metric values
- `destroy()`: Remove event listeners and cleanup

### YAWLTracer

#### Constructor
```javascript
new YAWLTracer(engine, config)
```

**Config Options**:
- `tracerName`: Tracer name (default: `'@unrdf/yawl'`)
- `tracerVersion`: Tracer version (optional)
- `enableContextPropagation`: Auto context propagation (default: `true`)
- `includeReceiptHashes`: Add receipt hashes to spans (default: `true`)
- `includeTaskData`: Include task I/O in spans (default: `false`)
- `maxTaskDataSize`: Max task data size in chars (default: `1000`)

#### Methods

- `getCaseSpan(caseId)`: Get active case span
- `getTaskSpan(workItemId)`: Get active task span
- `async spanWithReceipt(name, receipt, fn)`: Execute with receipt correlation
- `async spanInCase(caseId, name, fn)`: Execute custom span in case context
- `destroy()`: Remove event listeners and cleanup

### YAWLSLICalculator

#### Constructor
```javascript
new YAWLSLICalculator(engine, config)
```

**Config Options**:
- `windowMs`: Time window in ms (default: `300000` = 5 min)
- `targetCompletionRate`: Target completion rate (default: `0.95`)
- `targetTaskSuccessRate`: Target task success rate (default: `0.99`)
- `targetP95Latency`: Target p95 latency in seconds (default: `5.0`)
- `targetResourceUtilization`: Target resource utilization (default: `0.80`)

#### Methods

- `calculateCompletionRate()`: Get workflow completion rate
- `calculateTaskSuccessRate()`: Get task success rate
- `calculateTaskErrorRate()`: Get task error rate
- `calculateP95Latency()`: Get p95 task latency
- `calculateP99Latency()`: Get p99 task latency
- `calculateMedianLatency()`: Get median task latency
- `calculateResourceUtilization()`: Get resource utilization
- `calculateSLOCompliance()`: Get SLO compliance status
- `getSnapshot()`: Get complete SLI snapshot
- `getSLOReport()`: Get detailed SLO report
- `toPrometheus()`: Export SLIs as Prometheus metrics
- `reset()`: Clear collected data
- `destroy()`: Remove event listeners and cleanup

## Examples

See `src/examples/basic-usage.mjs` for a complete working example.

## License

MIT

## Contributing

Contributions welcome! Please see the main UNRDF repository for contribution guidelines.
