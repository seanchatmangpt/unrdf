# UNRDF OpenTelemetry Documentation

Enterprise-grade distributed tracing, SLO tracking, and observability for UNRDF Knowledge Graph Commitlog.

## Quick Links

- **[OTEL Weaver Integration Guide](./OTEL-WEAVER-INTEGRATION.md)** - Full implementation details
- **[Implementation Summary](./IMPLEMENTATION-SUMMARY.md)** - What was built and why

## What's Inside

### 1. Distributed Tracing

Full W3C Trace Context propagation across the entire UNRDF stack:

```
CLI Tool → Sidecar → Knowledge Hooks → Effect Sandboxes
   ↓          ↓             ↓                ↓
traceparent propagation (4bf92f3577b34da6...)
```

**Files**:
- `/weaver.yaml` - OTEL configuration
- `/custom-conventions.yaml` - UNRDF semantic conventions
- `/sidecar/server/utils/otel-context-propagation.mjs` - Context utilities
- `/sidecar/server/middleware/01.telemetry.mjs` - HTTP instrumentation
- `/src/sidecar/client.mjs` - gRPC propagation

### 2. SLO Tracking

Real-time Service Level Objective monitoring with error budgets:

```javascript
import { createDefaultSLOTracker } from './slo-tracker.mjs';

const tracker = createDefaultSLOTracker();

// Record measurements
tracker.recordMeasurement('api_latency', {
  value: 45,  // ms
  success: true
});

// Get status
const status = tracker.getSLOStatus('api_latency');
// { status: 'healthy', compliance: 0.98, budgetRemaining: 0.008 }

// Listen for violations
tracker.on('alert', (alert) => {
  console.error('SLO violation:', alert);
});
```

**Default SLOs**:
- **API Latency**: P95 < 100ms (99% compliance, 1% error budget)
- **Availability**: 99.9% uptime (0.1% error budget)
- **Error Rate**: < 1% errors (1% error budget)

**File**: `/sidecar/server/utils/slo-tracker.mjs`

### 3. Semantic Conventions

UNRDF-specific attributes for knowledge graph operations:

| Namespace | Prefix | Attributes | Use Case |
|-----------|--------|------------|----------|
| Knowledge Hooks | `knowledge_hook.*` | 8 | Hook execution tracking |
| Policy Packs | `policy_pack.*` | 6 | Policy validation |
| RDF Graphs | `rdf.*` | 6 | Graph operations |
| Effect Sandboxes | `effect.*` | 4 | Sandbox execution |
| Crypto Provenance | `crypto.*` | 4 | Signature verification |
| Transactions | `transaction.*` | 5 | KGC transaction processing |
| gRPC Sidecar | `sidecar.*` | 4 | Sidecar communication |

**File**: `/custom-conventions.yaml`

### 4. Metrics with Exemplars

All metrics link to traces for seamless debugging:

```javascript
{
  name: 'http.server.request.duration',
  value: 42,
  attributes: { method: 'POST', status_code: 200 },
  exemplar: {
    traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
    spanId: '00f067aa0ba902b7',
    timestamp: 1633024800000
  }
}
```

Click a metric in Grafana → Jump to trace in Tempo ✨

### 5. CI/CD Validation

GitHub Actions workflow validates on every PR:

```bash
✅ Semantic convention completeness
✅ Trace context propagation
✅ SLO definitions
✅ Metric exemplar configuration
✅ Instrumentation tests
```

**File**: `/.github/workflows/otel-weaver-validate.yml`

### 6. Grafana Dashboards

Auto-generated dashboards for monitoring:

- **UNRDF Overview**: Request rate, latency, error rate, SLO compliance
- **Service Mesh**: Distributed trace visualization
- **SLO Compliance**: Error budgets, compliance trends

**Directory**: `/grafana/dashboards/`

## Quick Start

### 1. Install Dependencies

```bash
cd sidecar
pnpm add @opentelemetry/api-logs @opentelemetry/sdk-logs
```

### 2. Validate Implementation

```bash
node scripts/validate-otel-weaver.mjs
```

Expected output:
```
✅ ALL CHECKS PASSED
🎉 OTEL Weaver integration is fully implemented and operational!
```

### 3. Use in Your Code

**Trace Context Propagation**:
```javascript
import {
  extractTraceContextFromHeaders,
  enrichLogWithTraceContext
} from './sidecar/server/utils/otel-context-propagation.mjs';

// Extract from HTTP headers
const ctx = extractTraceContextFromHeaders(req.headers);

// Add to logs
console.info('Processing request', enrichLogWithTraceContext({
  status: 'success'
}));
```

**SLO Tracking**:
```javascript
import { createDefaultSLOTracker } from './sidecar/server/utils/slo-tracker.mjs';

const tracker = createDefaultSLOTracker();

tracker.recordMeasurement('api_latency', {
  value: responseTime,
  success: statusCode < 400
});
```

### 4. Run CI/CD Validation

```bash
# Validate semantic conventions
npx js-yaml weaver.yaml
npx js-yaml custom-conventions.yaml

# Run instrumentation tests
cd sidecar && pnpm test -- --run otel
```

## Architecture

### Trace Propagation Flow

```
┌─────────────────────────────────────────────────────────────┐
│                    Distributed Tracing                       │
└─────────────────────────────────────────────────────────────┘

CLI Tool                 Sidecar                 Hooks
   │                        │                      │
   │  traceparent header    │  gRPC metadata       │
   ├───────────────────────>├─────────────────────>│
   │  00-4bf92f...-00f...   │  x-trace-id: 4bf...  │
   │                        │  x-span-id: a1b...   │
   │                        │                      │
   │<───────────────────────┤<─────────────────────┤
   │  trace_id in response  │  trace context       │
   │                        │                      │

All logs include trace_id and span_id for correlation
All metrics include exemplars linking to traces
```

### SLO Calculation Flow

```
┌─────────────────────────────────────────────────────────────┐
│                    SLO Tracking System                       │
└─────────────────────────────────────────────────────────────┘

Request → Middleware → Record Measurement
             ↓              ↓
          Metrics ←── SLO Tracker
             ↓              ↓
         Exemplar ←── Check Compliance
             ↓              ↓
         Grafana ←── Alert (if budget exhausted)
```

## Performance

**Overhead**:
- Trace context propagation: < 1ms
- Span creation: < 0.5ms per span
- Metric exemplar: < 0.1ms per metric
- SLO calculation: < 10ms per check

**Optimizations**:
- Batch processing (512 spans per batch)
- Sampling (10% default, 100% errors/slow)
- Connection pooling for gRPC
- In-memory SLO calculation

## Files Reference

| File | Purpose | Lines |
|------|---------|-------|
| `weaver.yaml` | OTEL Weaver configuration | 170 |
| `custom-conventions.yaml` | UNRDF semantic conventions | 351 |
| `sidecar/server/utils/otel-context-propagation.mjs` | Trace context utilities | 289 |
| `sidecar/server/utils/slo-tracker.mjs` | SLO tracking system | 350 |
| `sidecar/server/middleware/01.telemetry.mjs` | HTTP instrumentation | 123 |
| `src/sidecar/client.mjs` | gRPC trace propagation | 459 |
| `.github/workflows/otel-weaver-validate.yml` | CI/CD validation | 278 |
| `docs/telemetry/OTEL-WEAVER-INTEGRATION.md` | Full documentation | 396 |
| `docs/telemetry/IMPLEMENTATION-SUMMARY.md` | Implementation summary | 450 |
| `scripts/validate-otel-weaver.mjs` | Validation script | 250 |

## Standards Compliance

- ✅ OpenTelemetry 1.0 API
- ✅ W3C Trace Context 1.0
- ✅ Semantic Conventions 1.23.0
- ✅ gRPC Core 1.9

## Support

**Issues?** Check the troubleshooting section in [OTEL-WEAVER-INTEGRATION.md](./OTEL-WEAVER-INTEGRATION.md#troubleshooting)

**Questions?** Refer to:
- [Full Documentation](./OTEL-WEAVER-INTEGRATION.md)
- [Implementation Summary](./IMPLEMENTATION-SUMMARY.md)

**Validation**:
```bash
node scripts/validate-otel-weaver.mjs
```

---

**Status**: ✅ Production Ready

**Last Updated**: 2025-10-01

**Maintainer**: Mesh Network Swarm Coordinator
