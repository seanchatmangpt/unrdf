# OTEL Weaver Integration

**Status**: ✅ **FULLY IMPLEMENTED**
**Version**: 26.4.4
**Last Updated**: 2026-04-04

---

## Overview

The UNRDF project uses OpenTelemetry (OTEL) for observability with Weaver for semantic convention enforcement and monitoring. This document describes the OTEL Weaver integration architecture and implementation across **all UNRDF components**.

### Implementation Status

| Component         | Status        | Coverage                                                 |
| ----------------- | ------------- | -------------------------------------------------------- |
| **Registry**      | ✅ Complete   | 43 attrs, 13 metrics, 8 YAML files, `weaver check` clean |
| **`@unrdf/otel`** | ✅ Complete   | Generated constants, 100% live-check coverage            |
| **Daemon**        | ✅ Complete   | 36+ MCP tools instrumented, attrs match registry         |
| **CLI**           | ⬜ Partial    | Attrs defined in registry, runtime spans not yet emitted |
| **Grafana**       | ✅ Configured | Dashboard templates configured                           |

---

## Architecture

### Components

1. **OpenTelemetry SDK**
   - `@opentelemetry/sdk-node`: SDK initialization
   - `@opentelemetry/sdk-trace-node`: Tracing implementation
   - `@opentelemetry/exporter-trace-otlp-grpc`: OTLP gRPC exporter
   - `@opentelemetry/resources`: Resource attributes
   - `@opentelemetry/semantic-conventions`: Semantic attribute definitions

2. **Weaver Platform**
   - Semantic convention registry validation
   - Custom conventions enforcement
   - SLO tracking and monitoring
   - Grafana dashboard integration

3. **Sidecar gRPC Client**
   - Full trace context propagation
   - Metric exemplars
   - OTLP gRPC ingestion
   - Connection management

4. **Daemon MCP Server**
   - 36 MCP tools instrumented with OTEL spans
   - Semantic conventions for daemon operations
   - Integration with daemon lifecycle (start/stop)
   - Feature flag support via `OTEL_ENABLED`

5. **Validation Package**
   - Synthetic span generation
   - Validation package testing
   - Feature validation suite

## Tracing

### Trace Context Propagation

The sidecar client implements complete OTLP trace context propagation:

```javascript
// Headers
const traceparent = `00-${traceId}-${spanId}-01`;
const tracestate = 'consuming-context-value';

// Headers sent to gRPC
metadata.set('traceparent', traceparent);
metadata.set('tracestate', tracestate);
```

### Active Span Management

Spans are automatically started and ended within gRPC client methods:

```javascript
async connect(address, options = {}) {
  const span = tracer.startActiveSpan('sidecar.connect', {
    attributes: {
      'rpc.system': 'grpc',
      'rpc.method': 'Connect',
      'rpc.service': 'kgc.sidecar.v1.KGCSidecar'
    }
  });

  try {
    // Connection logic
    span.setStatus({ code: SpanStatusCode.OK });
    return connection;
  } catch (error) {
    span.recordException(error);
    span.setStatus({
      code: SpanStatusCode.ERROR,
      message: error.message
    });
    throw error;
  } finally {
    span.end();
  }
}
```

### Semantic Conventions

All spans follow OpenTelemetry semantic conventions:

- `rpc.system`: Always set to `grpc`
- `rpc.method`: gRPC method name
- `rpc.service`: gRPC service name
- `rpc.peer`: Client address
- `rpc.namespace`: gRPC protocol namespace

## Metrics

### Metrics Exported

The sidecar client exports the following metrics:

1. **Request Metrics**
   - `kgc_requests_total`: Total requests (counter)
   - `kgc_request_duration_ms`: Request latency (histogram)
   - `kgc_requests_in_flight`: Active requests (gauge)

2. **SLO Metrics**
   - `api_latency`: API latency SLO (gauge)
   - `availability`: Service availability SLO (gauge)
   - `error_rate`: Error rate SLO (gauge)

### Metric Exemplars

Critical metric samples include trace context:

```javascript
if (metricsCallbacks.length > 0) {
  metricsCallbacks.forEach(cb =>
    cb({
      name: 'kgc_requests_total',
      value: 1,
      exemplar: {
        traceId: spanContext.traceId,
        spanId: spanContext.spanId,
        attributes: {
          /* span attributes */
        },
      },
    })
  );
}
```

## Weaver Integration

### Semantic Convention Registry

UNRDF uses a local OTel Weaver registry at `otel/registry/` — 8 YAML files covering every domain:

| File                  | Domain                      | Attributes |
| --------------------- | --------------------------- | ---------- |
| `mcp.yaml`            | MCP server/tool invocations | 6          |
| `cli.yaml`            | CLI command instrumentation | 7          |
| `knowledge_hook.yaml` | Hook evaluation and effects | 10         |
| `rdf.yaml`            | RDF store operations        | 6          |
| `policy.yaml`         | Policy pack enforcement     | 5          |
| `crypto.yaml`         | Cryptographic provenance    | 5          |
| `transaction.yaml`    | Transaction lifecycle       | 4          |
| `metrics.yaml`        | All metric definitions      | 13 metrics |

The registry manifest at `otel/registry/manifest.yaml` pins the schema version:

```yaml
schema_url: https://opentelemetry.io/schemas/1.28.0
```

### Generated Package: `@unrdf/otel`

ESM constants are generated from the registry by Weaver and committed to `packages/otel/src/generated/`:

```javascript
import { ATTR_MCP_TOOL_NAME, ATTR_MCP_SERVER_NAME } from '@unrdf/otel/attributes';
import { METRIC_MCP_TOOL_DURATION, METRIC_UNRDF_RDF_QUAD_COUNT } from '@unrdf/otel/metrics';
```

Use these constants everywhere instead of raw strings — typos become compile-time errors.

Re-generate after editing the registry:

```bash
cd otel && weaver registry generate --registry registry/ js ../packages/otel/src/generated/
```

## Configuration

### Weaver Registry Manifest (`otel/registry/manifest.yaml`)

```yaml
schema_url: https://opentelemetry.io/schemas/1.28.0
```

### MiniJinja Templates (`otel/templates/registry/js/weaver.yaml`)

```yaml
templates:
  - pattern: '*.j2'
    filter: '.'
    application_mode: single
```

Templates at `otel/templates/registry/js/` produce `attributes.mjs` and `metrics.mjs` via Weaver's MiniJinja engine.

## Live Check Workflow

### Validate the Registry

```bash
cd otel && weaver registry check --registry registry/
```

Expected output:

```
ℹ Found registry manifest: registry/manifest.yaml
✔ No `after_resolution` policy violation
```

### Run a Live Check Against Sample Telemetry

```bash
cd otel && weaver registry live-check \
  --registry registry/ \
  --input-source live-check-samples.json \
  --input-format json \
  --no-stream
```

Expected output (clean):

```
Advisories:    0
Coverage:      100.0%
✔ Performed live check for registry `registry/`
```

### Sample Telemetry Format

Weaver's JSON ingester expects an array of tagged entities — **not** the OTLP protobuf JSON format:

```json
[
  {
    "span": {
      "name": "mcp.tool.call",
      "kind": "server",
      "attributes": [
        { "name": "mcp.tool.name", "value": "hooks_execute" },
        { "name": "mcp.server.name", "value": "unrdf-daemon" },
        { "name": "mcp.tool.success", "value": true }
      ]
    }
  },
  {
    "metric": {
      "name": "mcp.tool.duration",
      "instrument": "histogram",
      "unit": "s",
      "value": 0.042,
      "attributes": [
        { "name": "mcp.tool.name", "value": "hooks_execute" },
        { "name": "mcp.server.name", "value": "unrdf-daemon" },
        { "name": "mcp.tool.success", "value": true }
      ]
    }
  }
]
```

The full sample set covering all 56 registry entities lives at `otel/live-check-samples.json`.

## Testing

### Unit Tests

```bash
# Validate OTEL Weaver integration
pnpm test validate-otel-weaver

# Run validation
node scripts/validate-otel-weaver.mjs
```

### Integration Tests

```bash
# Test sidecar with OTEL tracing
pnpm test sidecar:otel

# Validate with live-check
weaver registry live-check < otlp-data.json
```

## Monitoring

### Grafana Dashboards

Dashboards are automatically generated and include:

1. **Trace Visualization**: Full request tracing
2. **SLO Monitoring**: Real-time SLO status
3. **Metric Trends**: Latency and error rate charts
4. **Sidecar Health**: Connection pool and health checks

### Alerts

Key alerts configured:

- **High Latency**: API latency > 100ms
- **Low Availability**: Availability < 99.9%
- **High Error Rate**: Error rate > 1%
- **Sidecar Failure**: Connection loss or health check failures

## Troubleshooting

### Issue: No spans being collected

**Solution**:

1. Verify provider registration: `trace.getTracerProvider()`
2. Check exporter is registered: `processor.getSpanProcessors()`
3. Ensure `forceFlush()` is called before collection

### Issue: ProxyTracer (Weaver) overriding provider

**Solution**:

```javascript
// Use independent tracer for testing
const independentTracer = trace.getTracer(
  `unrdf-validator-${validationId}`,
  customProvider // Pass custom provider
);
```

### Issue: Missing OTEL imports in gRPC client

**Solution**: Ensure client.mjs imports `@opentelemetry/api`:

```javascript
import { trace, metrics } from '@opentelemetry/api';
```

## References

- [OTel Weaver](https://github.com/open-telemetry/weaver) — semantic convention registry toolchain
- [OpenTelemetry Semantic Conventions](https://github.com/open-telemetry/semantic-conventions)
- [OTLP Protocol](https://opentelemetry.io/docs/reference/specification/protocol/otlp/)
- [Grafana Dashboards](https://grafana.com/docs/grafana/latest/visualizations/dashboards/)

## Status

### Semantic Convention Registry (`otel/registry/`)

- ✅ 8 registry YAML files (7 attribute groups + 1 metrics file)
- ✅ 43 attributes across 7 domains
- ✅ 13 metrics (histogram/counter/updowncounter)
- ✅ `manifest.yaml` with schema_url (no deprecation warnings)
- ✅ `weaver registry check` — 0 violations
- ✅ `weaver registry live-check` — 0 advisories, 100% coverage

### Generated Package (`@unrdf/otel`)

- ✅ `packages/otel/src/generated/attributes.mjs` — 44 `ATTR_*` constants
- ✅ `packages/otel/src/generated/metrics.mjs` — 13 `METRIC_*` constants
- ✅ MiniJinja templates at `otel/templates/registry/js/`
- ✅ Re-generated with a single `weaver registry generate` command

### Daemon Integration

- ✅ 36+ MCP tools instrumented via `withMcpSpan()` in `otel-instrumentation.mjs`
- ✅ Attributes emitted match `otel/registry/mcp.yaml` definitions
- ✅ W3C trace context propagation
- ✅ Feature flag support (`OTEL_ENABLED`)

### CLI Integration

- ✅ Attribute names defined in `otel/registry/cli.yaml`
- ⬜ Runtime instrumentation (CLI spans not yet emitted)

## Related Documentation

- **Daemon Integration**: See `packages/daemon/OTEL-ENVIRONMENT.md` for production configuration
- **Implementation Details**: See `DAEMON-OTEL-IMPLEMENTATION.md` for complete implementation notes
- **Verification Report**: See `DAEMON-OTEL-VERIFICATION.md` for verification results
- **Sidecar Integration**: See main integration guide for sidecar details
- **Validation Package**: See `packages/validation/` for validation test suite
