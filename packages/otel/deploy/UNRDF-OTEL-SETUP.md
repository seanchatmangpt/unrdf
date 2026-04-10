# UNRDF OTEL Integration with pm4py-rust

This guide explains how UNRDF's OTEL package integrates with pm4py-rust's telemetry infrastructure.

## Overview

UNRDF now uses pm4py-rust's comprehensive observability stack:
- **Metrics Collection**: pm4py's MetricCollector
- **Alerting**: pm4py's AlertManager
- **Dashboards**: pm4py's MonitoringDashboard
- **OTEL Collector**: Shared collector configuration

## Architecture

```
UNRDF Packages
    ↓
@unrdf/otel (wrapper)
    ↓
pm4py-rust (~/chatmangpt/pm4py)
    ↓
OpenTelemetry Collector
    ↓
Prometheus + Grafana + Jaeger
```

## Quick Start

### 1. Set PM4Py Path

```bash
export PM4PY_PATH=~/chatmangpt/pm4py
```

### 2. Use Monitoring in UNRDF Code

```javascript
import { monitorOperation, UNRDFOperation } from '@unrdf/otel/monitoring';

// Monitor a SPARQL query
const result = await monitorOperation(
  UNRDFOperation.SPARQL_QUERY,
  async () => {
    return await engine.query(sparql);
  },
  { graph: 'my-graph' }
);
```

### 3. Access Metrics

```javascript
import { getSystemMetrics, getPrometheusMetrics } from '@unrdf/otel/monitoring';

// Get current system metrics
const metrics = await getSystemMetrics();
console.log('CPU:', metrics.cpu_percent);

// Get Prometheus-formatted metrics
const promMetrics = await getPrometheusMetrics();
console.log(promMetrics);
```

## PM4Py Integration

The `@unrdf/otel/pm4py` module provides JavaScript bindings to pm4py's Python monitoring:

```javascript
import { getPM4Py } from '@unrdf/otel/pm4py';

const pm4py = getPM4Py();

// Record custom metric
await pm4py.recordMetric('custom_metric', 42, 'gauge', {
  service: 'unrdf',
  component: 'daemon'
});

// Get all metrics
const metrics = await pm4py.getMetrics();
```

## Available Metrics

### System Metrics
- `unrdf_graph_loads_total`: Total graph loads
- `unrdf_sparql_queries_total`: Total SPARQL queries
- `unrdf_hook_executions_total`: Total hook executions
- `unrdf_federation_queries_total`: Total federation queries
- `unrdf_streaming_events_total`: Total streaming events

### Gauge Metrics
- `unrdf_active_graphs`: Active RDF graphs
- `unrdf_cached_embeddings`: Cached embeddings
- `unrdf_hnsw_index_size`: HNSW index size

### Histogram Metrics
- `unrdf_sparql_query_duration_seconds`: SPARQL query duration
- `unrdf_hook_execution_duration_seconds`: Hook execution duration
- `unrdf_federation_latency_seconds`: Federation latency

## Monitoring Stack URLs

- **Prometheus**: http://localhost:3001
- **Grafana**: http://localhost:3000
- **Jaeger**: http://localhost:16686
- **OTLP gRPC**: http://localhost:14317
- **OTLP HTTP**: http://localhost:14318
- **Health Check**: http://localhost:13133

## Configuration

### OTEL Collector Config

Located at: `packages/otel/deploy/otel-collector-config.yaml`

Key settings:
- Receivers: OTLP gRPC (4317), OTLP HTTP (4318)
- Exporters: Jaeger (14250), Prometheus (9090)
- Service name: `unrdf`
- Service version: `26.4.7`

### Sync with pm4py

To sync with latest pm4py configuration:

```bash
pnpm --filter @unrdf/otel run sync:pm4py
```

## Testing

Test the integration:

```bash
# Check pm4py availability
cd packages/otel
node test/integration/pm4py-integration.test.mjs

# Run with PM4PY_PATH set
PM4PY_PATH=~/chatmangpt/pm4py pnpm test
```

## Troubleshooting

### PM4Py Not Found

```bash
# Check pm4py path
ls -la ~/chatmangpt/pm4py/pm4py/monitoring/

# Set environment variable
export PM4PY_PATH=~/chatmangpt/pm4py
```

### OTEL Collector Not Running

```bash
# Check if collector is running
curl http://localhost:13133

# Start collector (if using Docker)
docker-compose up -d otel-collector
```

### Python Dependencies Missing

```bash
cd ~/chatmangpt/pm4py
pip install -r requirements.txt
```

## Migration from Standalone OTEL

Before (standalone):
```javascript
import { trace, context } from '@opentelemetry/api';
```

After (with pm4py integration):
```javascript
import { trace, context } from '@opentelemetry/api';
import { monitorOperation } from '@unrdf/otel/monitoring';

const result = await monitorOperation('my_operation', async () => {
  // Your code here
});
```

## Benefits of pm4py Integration

1. **Unified Observability**: Single metrics/monitoring stack for both projects
2. **Rich Dashboards**: Leverage pm4py's existing monitoring dashboards
3. **Process Mining Metrics**: Native support for process mining KPIs
4. **Alert Management**: Use pm4py's alerting infrastructure
5. **Reduced Overhead**: Shared collector and storage

## Future Enhancements

- [ ] Add UNRDF-specific dashboards in Grafana
- [ ] Integrate process mining metrics with semantic query performance
- [ ] Add alerting rules for UNRDF-specific anomalies
- [ ] Export UNRDF metrics to pm4py's dashboards
