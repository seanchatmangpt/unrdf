# OTEL Weaver Integration

**Status**: ✅ **Implemented**
**Version**: 26.4.3
**Last Updated**: 2026-04-03

---

## Overview

The UNRDF project uses OpenTelemetry (OTEL) for observability with Weaver for semantic convention enforcement and monitoring. This document describes the OTEL Weaver integration architecture and implementation.

## Architecture

### Components

1. **OpenTelemetry SDK**
   - `@opentelemetry/sdk-trace-node`: Tracing implementation
   - `@opentelemetry/sdk-metrics`: Metrics collection
   - `@opentelemetry/resources`: Resource attributes
   - `@opentelemetry/instrumentation`: Auto-instrumentation

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
  metricsCallbacks.forEach(cb => cb({
    name: 'kgc_requests_total',
    value: 1,
    exemplar: {
      traceId: spanContext.traceId,
      spanId: spanContext.spanId,
      attributes: { /* span attributes */ }
    }
  }));
}
```

## Weaver Integration

### Custom Conventions

Weaver enforces custom semantic conventions defined in `custom-conventions.yaml`:

- `knowledge_hook`: Knowledge base hook invocations
- `policy_pack`: Policy pack executions
- `rdf_graph`: RDF graph operations
- `effect_sandbox`: Sandbox execution boundaries
- `crypto_provenance`: Cryptographic operation tracking
- `kgc_transaction`: Knowledge graph consistency transactions
- `grpc_sidecar`: gRPC sidecar communications

### Semantic Convention Registry

The project validates against the OpenTelemetry semantic convention registry:

- **Default Registry**: `https://github.com/open-telemetry/semantic-conventions.git[model]`
- **Validation Mode**: Live-check on ingestion
- **Enforcement**: Policy-based validation

## Configuration

### Weaver Configuration (`weaver.yaml`)

```yaml
version: 1.0.0
project_name: "unrdf"
registry: "https://github.com/open-telemetry/semantic-conventions.git[model]"

generation:
  project_root: "./src"
  instrumented_packages:
    - "kgc-scope"
    - "sidecar"

enforcement:
  active: true
  custom_conventions: "custom-conventions.yaml"

export:
  grpc:
    address: "0.0.0.0"
    port: 4317
    protocol: "otlp"

context:
  propagation:
    format: "w3c"

slo:
  api_latency:
    threshold: 100  # ms
    error_if_exceeded: true

grafana:
  dashboards:
    path: "./grafana/dashboards"
  templates:
    name: "unrdf-dashboards"
    description: "UNRDF monitoring dashboards"
```

## Live Check Workflow

### Command

```bash
weaver registry live-check \
  --registry 'https://github.com/open-telemetry/semantic-conventions.git[model]' \
  --input-source stdin \
  --input-format json \
  --output none
```

### Sample OTLP Telemetry

```json
{
  "resourceSpans": [{
    "resource": {
      "attributes": {
        "service.name": "unrdf-knowledge-graph"
      }
    },
    "scopeSpans": [{
      "scope": {
        "name": "kgc-sidecar"
      },
      "spans": [{
        "name": "kgc.query.execute",
        "kind": "SPAN_KIND_INTERNAL",
        "attributes": {
          "kgc.query.type": "SELECT",
          "kgc.query.returned_triples": 100,
          "kgc.query.duration_ms": 15
        }
      }]
    }]
  }]
}
```

### Expected Validation

- ✅ All span attributes match semantic conventions
- ✅ Required attributes present for each operation
- ✅ Values within acceptable ranges
- ✅ No unknown or deprecated attributes

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
  customProvider  // Pass custom provider
);
```

### Issue: Missing OTEL imports in gRPC client

**Solution**: Ensure client.mjs imports `@opentelemetry/api`:

```javascript
import { trace, metrics } from '@opentelemetry/api';
```

## References

- [OpenTelemetry Semantic Conventions](https://github.com/open-telemetry/semantic-conventions)
- [Weaver Documentation](https://github.com/weaver-ops/weaver)
- [OTLP Protocol](https://opentelemetry.io/docs/reference/specification/protocol/otlp/)
- [Grafana Dashboards](https://grafana.com/docs/grafana/latest/visualizations/dashboards/)

## Status

- ✅ All required files implemented
- ✅ OTEL tracing working
- ✅ Custom conventions enforced
- ✅ Live-check validation functional
- ✅ Grafana dashboards configured
- ⏳ Sample telemetry needed for integration testing
