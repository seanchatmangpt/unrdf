# UNRDF OTEL Configuration

This directory contains OpenTelemetry configuration synchronized with pm4py-rust.

## Files

- `otel-collector-config.yaml`: OTEL collector configuration (synced from pm4py)
- `pm4py-monitoring`: Symlink to pm4py monitoring module

## PM4Py Integration

The OTEL package now integrates with pm4py-rust's monitoring infrastructure:

```javascript
import { getPM4Py, recordUNRDFMetrics } from '@unrdf/otel/pm4py';
import { monitorOperation } from '@unrdf/otel/monitoring';

// Record metrics
await recordUNRDFMetrics('sparql_query', { graph: 'my-graph' });

// Monitor operations
await monitorOperation('graph_load', async () => {
  // Your operation here
  return result;
});
```

## PM4Py Location

``ash
export PM4PY_PATH=~/chatmangpt/pm4py
```

## Monitoring Stack

- **Prometheus**: http://localhost:3001
- **Grafana**: http://localhost:3000
- **Jaeger**: http://localhost:16686
- **OTEL Collector**: http://localhost:14317 (gRPC), http://localhost:14318 (HTTP)

## Sync Command

To sync with latest pm4py configuration:

```bash
pnpm --filter @unrdf/otel run sync:pm4py
```
