# Grafana Faro RUM — Real User Monitoring

Self-contained script for capturing frontend telemetry in any UNRDF web UI.

## Quick Start

Drop the script into your HTML `<head>`:

```html
<head>
  <script>
    // Optional: configure before loading faro-init.js
    window.__FARO_CONFIG = {
      endpoint: 'http://localhost:4318/v1/logs',
      appName: 'unrdf-sidecar',
      appVersion: '1.0.0',
      environment: 'production',
    };
  </script>
  <script src="path/to/faro-init.js"></script>
</head>
```

## What It Captures

| Signal        | Details                                            |
| ------------- | -------------------------------------------------- |
| Page loads    | Navigation, full page loads, SPA transitions       |
| JS errors     | Unhandled exceptions, unhandled promise rejections |
| Web Vitals    | LCP, FID, CLS, TTFB, INP                           |
| Custom events | Application-specific events via `pushEvent`        |

## API

After initialization, `window.__FARO` is available:

```javascript
// Push custom events
window.__FARO.pushEvent('knowledge_graph_query', {
  queryType: 'SPARQL',
  quadCount: 42,
});

// Push measurements
window.__FARO.pushMeasurement('query_latency_ms', 127);

// Push structured logs
window.__FARO.pushLog('info', 'Cache invalidated', { store: 'oxigraph' });

// Set user identity
window.__FARO.setUser({ id: 'user-123', email: 'user@example.com' });

// Get current trace ID (for correlating with backend traces)
const traceId = window.__FARO.getTraceId();
```

## Configuration Options

Set `window.__FARO_CONFIG` before loading the script:

| Field         | Type   | Default                         | Description                       |
| ------------- | ------ | ------------------------------- | --------------------------------- |
| `endpoint`    | string | `http://localhost:4318/v1/logs` | OTEL Collector OTLP HTTP endpoint |
| `appName`     | string | `unrdf-web`                     | Application identifier            |
| `appVersion`  | string | `1.0.0`                         | Application version string        |
| `environment` | string | `development`                   | Deployment environment            |

## Backend Requirements

The OTLP collector must accept Faro payloads on the traces endpoint. The existing `otel-collector-config.yaml` already exposes:

- **4318** — OTLP HTTP (Faro connects here)
- **4317** — OTLP gRPC (daemon/sidecar connect here)

Faro sends telemetry to the `/v1/traces` path on the configured endpoint.

## Nuxt Integration

For the sidecar Nuxt app (`sidecar/nuxt.config.ts`), add to `app.head.script`:

```typescript
// nuxt.config.ts
export default defineNuxtConfig({
  app: {
    head: {
      script: [{ src: '/otel/faro/faro-init.js', defer: true }],
    },
  },
});
```

## Production Notes

- **Vendor the SDK**: The CDN URLs load from unpkg. For production, install `@grafana/faro-web-sdk` and `@grafana/faro-web-vitals` as npm dependencies and bundle them.
- **Sampling**: The session sample rate defaults to `1.0` (100%). Reduce for high-traffic deployments.
- **Trace correlation**: Use `window.__FARO.getTraceId()` to correlate frontend traces with backend OTEL traces by passing the trace ID in request headers.
