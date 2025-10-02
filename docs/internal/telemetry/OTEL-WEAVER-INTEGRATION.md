# OpenTelemetry Weaver Integration

## Overview

UNRDF implements enterprise-grade distributed tracing using OpenTelemetry with Weaver for type-safe semantic conventions, W3C Trace Context propagation, and comprehensive observability.

## Architecture

```
┌─────────────┐     traceparent     ┌─────────────┐     gRPC metadata     ┌─────────────┐
│   CLI Tool  │ ──────────────────> │   Sidecar   │ ──────────────────> │    Hooks    │
│  (Context)  │                     │ (Middleware) │                     │ (Effects)   │
└─────────────┘                     └─────────────┘                     └─────────────┘
      │                                    │                                    │
      │                                    │                                    │
      v                                    v                                    v
  Trace ID: 4bf92f...              Trace ID: 4bf92f...              Trace ID: 4bf92f...
  Span ID:  00f067...              Span ID:  a1b2c3...              Span ID:  d4e5f6...
  Parent:   (none)                 Parent:   00f067...              Parent:   a1b2c3...
```

## Components

### 1. Weaver Configuration (`/Users/sac/unrdf/weaver.yaml`)

Defines semantic conventions, code generation, and instrumentation rules:

```yaml
version: 1.0.0
project_name: unrdf-kgc-sidecar

registry:
  imports:
    - https://github.com/open-telemetry/semantic-conventions/blob/main/model/registry/
  local:
    - ./custom-conventions.yaml

generation:
  targets:
    - language: javascript
      output_dir: ./generated/otel
```

**Key Features**:
- Import standard OTEL conventions
- Define custom UNRDF conventions
- Auto-generate type-safe instrumentation
- Enforce semantic convention compliance

### 2. Custom Semantic Conventions (`/Users/sac/unrdf/custom-conventions.yaml`)

UNRDF-specific attributes for:
- **Knowledge Hooks**: `knowledge_hook.*` attributes
- **Policy Packs**: `policy_pack.*` attributes
- **RDF Graphs**: `rdf.*` attributes
- **Effect Sandboxes**: `effect.*` attributes
- **Crypto Provenance**: `crypto.*` attributes
- **Transactions**: `transaction.*` attributes
- **gRPC Sidecar**: `sidecar.*` attributes

Example:
```yaml
- id: knowledge_hook
  prefix: knowledge_hook
  attributes:
    - id: hook_id
      type: string
      requirement_level: required
    - id: hook_type
      type: enum
      members: [pre_commit, post_commit, validation]
    - id: hook_result
      type: enum
      members: [success, failure, skipped]
```

### 3. Context Propagation (`/Users/sac/unrdf/sidecar/server/utils/otel-context-propagation.mjs`)

W3C Trace Context implementation:

```javascript
import {
  extractTraceContextFromHeaders,
  injectTraceContextIntoMetadata,
  getCurrentTraceContext,
  enrichLogWithTraceContext
} from './otel-context-propagation.mjs';

// Extract from HTTP headers
const ctx = extractTraceContextFromHeaders(req.headers);

// Inject into gRPC metadata
injectTraceContextIntoMetadata(metadata, ctx);

// Add to logs
console.info('Request processed', enrichLogWithTraceContext({
  status: 'success',
  duration_ms: 42
}));
```

**Key Functions**:
- `parseTraceparent()` - Parse W3C traceparent header
- `formatTraceparent()` - Format trace context as header
- `extractTraceContextFromHeaders()` - Extract from HTTP
- `extractTraceContextFromMetadata()` - Extract from gRPC
- `injectTraceContextIntoHeaders()` - Inject into HTTP
- `injectTraceContextIntoMetadata()` - Inject into gRPC
- `getCurrentTraceContext()` - Get active context
- `enrichLogWithTraceContext()` - Add trace to logs
- `addMetricExemplar()` - Link metrics to traces

### 4. Telemetry Middleware (`/Users/sac/unrdf/sidecar/server/middleware/01.telemetry.mjs`)

HTTP request instrumentation:

```javascript
export default defineEventHandler(async (event) => {
  // Extract incoming trace context
  const incomingTraceContext = extractTraceContextFromHeaders(event.node.req.headers);

  // Create span with context
  const span = tracer.startSpan(`HTTP ${method} ${path}`, {
    attributes: {
      'http.method': method,
      'service.name': 'unrdf-sidecar',
    }
  });

  // Add trace context to logs
  event.context.logContext = {
    trace_id: getTraceIdForLogging(),
    span_id: getSpanIdForLogging(),
  };

  // Execute in trace context
  return await context.with(trace.setSpan(context.active(), span), async () => {
    // Handler logic
  });
});
```

### 5. gRPC Client Propagation (`/Users/sac/unrdf/src/sidecar/client.mjs`)

Automatic trace context injection:

```javascript
async _call(client, method, request, options = {}) {
  const metadata = new grpc.Metadata();

  // Extract current OTEL trace context
  const { trace, context } = await import('@opentelemetry/api');
  const currentSpan = trace.getSpan(context.active());

  if (currentSpan) {
    const spanContext = currentSpan.spanContext();

    // Inject into gRPC metadata
    metadata.set('x-trace-id', spanContext.traceId);
    metadata.set('x-span-id', spanContext.spanId);

    // Add W3C traceparent
    const traceparent = `00-${spanContext.traceId}-${spanContext.spanId}-01`;
    metadata.set('traceparent', traceparent);
  }

  // Execute gRPC call with metadata
  return client[method](request, metadata, { deadline });
}
```

### 6. SLO Tracker (`/Users/sac/unrdf/sidecar/server/utils/slo-tracker.mjs`)

Service Level Objective monitoring:

```javascript
import { createDefaultSLOTracker, SLOType } from './slo-tracker.mjs';

// Create tracker with default UNRDF SLOs
const tracker = createDefaultSLOTracker();

// Record measurements
tracker.recordMeasurement('api_latency', {
  value: 45,  // ms
  success: true,
  timestamp: Date.now()
});

// Get SLO status
const status = tracker.getSLOStatus('api_latency');
// {
//   status: 'healthy',
//   compliance: 0.98,
//   budgetRemaining: 0.008
// }

// Listen for alerts
tracker.on('alert', (alert) => {
  console.error('SLO violation:', alert);
});
```

**Default SLOs**:
- **API Latency**: P95 < 100ms, 99% compliance
- **Availability**: 99.9% uptime
- **Error Rate**: < 1% errors

## Metrics with Exemplars

All metrics include trace exemplars for seamless Grafana → Tempo linking:

```javascript
const metric = {
  name: 'http.server.request.duration',
  value: 42,
  attributes: {
    'http.method': 'POST',
    'http.status_code': 200,
  },
  exemplar: {
    traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
    spanId: '00f067aa0ba902b7',
    timestamp: 1633024800000
  }
};
```

In Grafana, clicking a metric data point jumps directly to the trace in Tempo.

## CI/CD Validation (`/Users/sac/unrdf/.github/workflows/otel-weaver-validate.yml`)

Automated validation on every PR:

1. **Validate Conventions**: YAML syntax, completeness checks
2. **Test Instrumentation**: Run OTEL utility tests
3. **Check Propagation**: Verify W3C traceparent injection
4. **Validate SLOs**: Check SLO definitions
5. **Generate Dashboards**: Create Grafana JSON configs

## Usage Examples

### CLI Tool Integration

```javascript
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-cli');

const span = tracer.startSpan('deploy-policy-pack', {
  attributes: {
    'policy_pack.name': 'production-rules',
    'deployment.environment': 'staging',
  }
});

try {
  // Call sidecar (trace context auto-propagated)
  await sidecarClient.applyTransaction(request);

  span.setStatus({ code: SpanStatusCode.OK });
} catch (error) {
  span.recordException(error);
  span.setStatus({ code: SpanStatusCode.ERROR });
} finally {
  span.end();
}
```

### Knowledge Hook Instrumentation

```javascript
import { enrichLogWithTraceContext } from './otel-context-propagation.mjs';

export async function executeHook(hookId, event) {
  console.info('[Hook] Starting execution', enrichLogWithTraceContext({
    'knowledge_hook.hook_id': hookId,
    'knowledge_hook.hook_type': 'validation',
  }));

  // Hook logic with automatic trace context
  const result = await validateEntity(event);

  console.info('[Hook] Execution complete', enrichLogWithTraceContext({
    'knowledge_hook.hook_result': result.success ? 'success' : 'failure',
    'knowledge_hook.duration_ms': result.duration,
  }));

  return result;
}
```

### Policy Pack Validation Tracing

```javascript
const span = tracer.startSpan('validate-graph', {
  attributes: {
    'policy_pack.pack_name': 'core-validation',
    'policy_pack.strict_mode': true,
    'rdf.quad_count': quads.length,
  }
});

const validationResult = await validateGraph(quads, policyPack);

span.setAttributes({
  'policy_pack.validation_result': validationResult.valid ? 'valid' : 'invalid',
  'policy_pack.violation_count': validationResult.violations.length,
});

span.end();
```

## Grafana Dashboards

Auto-generated dashboards in `/Users/sac/unrdf/grafana/dashboards/`:

- **UNRDF Overview**: Request rate, latency, error rate, SLO compliance
- **Service Mesh**: Distributed trace visualization
- **SLO Compliance**: Error budgets, compliance trends

## Performance Targets

- **Trace Context Propagation**: < 1ms overhead
- **Span Creation**: < 0.5ms per span
- **Metric Exemplar**: < 0.1ms per metric
- **SLO Calculation**: < 10ms per check

## Best Practices

1. **Always propagate context**: Use `extractTraceContext` → `injectTraceContext`
2. **Enrich logs**: Add `trace_id` and `span_id` to all logs
3. **Record errors**: Call `span.recordException(error)` on failures
4. **Add exemplars**: Link metrics to traces with `addMetricExemplar()`
5. **Monitor SLOs**: Track error budgets and alert on violations
6. **Use semantic conventions**: Follow UNRDF custom conventions

## Troubleshooting

### Missing Trace Context

**Problem**: Traces not appearing in Grafana Tempo

**Solution**:
```javascript
// Check if trace context is being extracted
const ctx = extractTraceContextFromHeaders(headers);
console.log('Trace context:', ctx);
// Should show: { traceId: '...', spanId: '...', traceFlags: '01' }
```

### Broken Trace Links

**Problem**: Spans not connected in trace view

**Solution**:
```javascript
// Ensure parent span ID is set correctly
span.setAttributes({
  'parent.span_id': parentContext.spanId,
});
```

### SLO Alerts Firing

**Problem**: Constant SLO violation alerts

**Solution**:
```javascript
// Check current SLO status
const status = tracker.getSLOStatus('api_latency');
console.log('Budget remaining:', status.budgetRemaining);

// Adjust SLO targets if needed
tracker.addSLO({
  name: 'api_latency',
  target: 0.90,  // Lower from 0.95
  threshold: 150,  // Increase from 100ms
});
```

## Future Enhancements

- [ ] OTEL Weaver CLI integration (when available)
- [ ] Auto-generated instrumentation code
- [ ] OpenTelemetry Collector integration
- [ ] Distributed context propagation for async operations
- [ ] Span link support for batch operations
- [ ] Custom samplers for intelligent trace sampling
- [ ] Integration with cloud tracing backends (AWS X-Ray, Google Cloud Trace)

## References

- [OpenTelemetry Specification](https://opentelemetry.io/docs/specs/otel/)
- [W3C Trace Context](https://www.w3.org/TR/trace-context/)
- [Semantic Conventions](https://opentelemetry.io/docs/specs/semconv/)
- [OTEL Weaver](https://github.com/open-telemetry/weaver)
