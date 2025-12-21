# OpenTelemetry Monitoring Configuration

Complete guide to configuring OpenTelemetry (OTEL) observability for UNRDF in production.

## Table of Contents

- [Overview](#overview)
- [Quick Start](#quick-start)
- [Configuration](#configuration)
- [Instrumentation](#instrumentation)
- [Exporters](#exporters)
- [Grafana Dashboards](#grafana-dashboards)
- [Metrics Reference](#metrics-reference)
- [Traces Reference](#traces-reference)
- [Best Practices](#best-practices)

---

## Overview

UNRDF uses **OpenTelemetry** for production-grade observability:

- **Traces:** Distributed tracing across SPARQL queries, RDF operations, and transactions
- **Metrics:** Performance counters, store sizes, query durations
- **Logs:** Structured logging with correlation IDs

**Benefits:**
- Identify slow queries and bottlenecks
- Track RDF store growth over time
- Debug distributed systems
- Correlate logs with traces

---

## Quick Start

### 1. Install Dependencies

```bash
pnpm add @opentelemetry/api \
         @opentelemetry/sdk-node \
         @opentelemetry/exporter-trace-otlp-http \
         @opentelemetry/instrumentation
```

### 2. Enable OTEL

```bash
# Environment variables
export OTEL_ENABLED=true
export OTEL_SERVICE_NAME=unrdf-api
export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
```

### 3. Start Collector and Jaeger

```bash
# Using Docker Compose
docker-compose up -d otel-collector jaeger

# Access Jaeger UI
open http://localhost:16686
```

### 4. Run Application

```bash
node packages/cli/src/server.mjs
```

Traces will appear in Jaeger UI within seconds.

---

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `OTEL_ENABLED` | `false` | Enable/disable OTEL |
| `OTEL_SERVICE_NAME` | `unrdf` | Service name in traces |
| `OTEL_EXPORTER_OTLP_ENDPOINT` | `http://localhost:4318` | OTLP HTTP endpoint |
| `OTEL_TRACES_SAMPLER` | `always_on` | Sampling strategy |
| `OTEL_TRACES_SAMPLER_ARG` | - | Sampler arguments |
| `OTEL_LOG_LEVEL` | `info` | OTEL SDK log level |

### Sampling Strategies

**always_on** - Sample every trace (development):
```bash
OTEL_TRACES_SAMPLER=always_on
```

**always_off** - Sample no traces (disabled):
```bash
OTEL_TRACES_SAMPLER=always_off
```

**traceidratio** - Sample X% of traces (production):
```bash
OTEL_TRACES_SAMPLER=traceidratio
OTEL_TRACES_SAMPLER_ARG=0.1  # 10% sampling
```

**parentbased_always_on** - Sample if parent is sampled:
```bash
OTEL_TRACES_SAMPLER=parentbased_always_on
```

---

## Instrumentation

### Application Instrumentation

Create `otel.mjs`:

```javascript
import { NodeSDK } from '@opentelemetry/sdk-node';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';
import { Resource } from '@opentelemetry/resources';
import { SemanticResourceAttributes } from '@opentelemetry/semantic-conventions';

const sdk = new NodeSDK({
  resource: new Resource({
    [SemanticResourceAttributes.SERVICE_NAME]: process.env.OTEL_SERVICE_NAME || 'unrdf',
    [SemanticResourceAttributes.SERVICE_VERSION]: '5.0.1',
  }),
  traceExporter: new OTLPTraceExporter({
    url: process.env.OTEL_EXPORTER_OTLP_ENDPOINT || 'http://localhost:4318/v1/traces',
  }),
});

sdk.start();

// Graceful shutdown
process.on('SIGTERM', () => {
  sdk.shutdown()
    .then(() => console.log('OTEL SDK shut down'))
    .catch((error) => console.error('Error shutting down OTEL SDK', error))
    .finally(() => process.exit(0));
});

export { sdk };
```

### Usage in Application

```javascript
// Import OTEL first
import './otel.mjs';

// Then import application code
import { createKnowledgeSubstrateCore } from '@unrdf/core';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-app');

async function processData() {
  const span = tracer.startSpan('processData');

  try {
    const core = await createKnowledgeSubstrateCore();

    // Nested spans
    const parseSpan = tracer.startSpan('parseRDF', { parent: span });
    const store = core.parseRdf(turtleData);
    parseSpan.end();

    const querySpan = tracer.startSpan('querySPARQL', { parent: span });
    const results = await core.query(store, sparql);
    querySpan.end();

    span.setStatus({ code: 1 });  // OK
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message });  // ERROR
    throw error;
  } finally {
    span.end();
  }
}
```

### Automatic Instrumentation

UNRDF automatically creates spans for:

- **SPARQL queries:** `sparql.query`
- **RDF parsing:** `rdf.parse`
- **SHACL validation:** `shacl.validate`
- **Transactions:** `transaction.begin`, `transaction.commit`
- **Store operations:** `store.addQuad`, `store.deleteQuad`

**Example trace:**
```
processData [5.2s]
├─ rdf.parse [1.2s]
├─ sparql.query [3.8s]
│  ├─ query.plan [0.5s]
│  ├─ query.execute [3.0s]
│  └─ query.format [0.3s]
└─ transaction.commit [0.2s]
```

---

## Exporters

### OTLP HTTP Exporter (Recommended)

```javascript
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';

const exporter = new OTLPTraceExporter({
  url: 'http://otel-collector:4318/v1/traces',
  headers: {
    'Authorization': 'Bearer YOUR_TOKEN'
  }
});
```

### Jaeger Exporter

```javascript
import { JaegerExporter } from '@opentelemetry/exporter-jaeger';

const exporter = new JaegerExporter({
  endpoint: 'http://jaeger:14268/api/traces',
});
```

### Console Exporter (Development)

```javascript
import { ConsoleSpanExporter } from '@opentelemetry/sdk-trace-base';

const exporter = new ConsoleSpanExporter();
// Prints spans to console for debugging
```

---

## Grafana Dashboards

### Import Pre-built Dashboard

1. **Download dashboard JSON:**
   ```bash
   wget https://github.com/unrdf/unrdf/blob/main/grafana/dashboards/unrdf-metrics.json
   ```

2. **Import to Grafana:**
   - Open Grafana UI (http://localhost:3001)
   - Go to Dashboards → Import
   - Upload `unrdf-metrics.json`

### Custom Dashboard

Create `grafana/dashboards/custom.json`:

```json
{
  "dashboard": {
    "title": "UNRDF Production Metrics",
    "panels": [
      {
        "id": 1,
        "title": "SPARQL Query Rate",
        "targets": [{
          "expr": "rate(sparql_queries_total[5m])",
          "legendFormat": "{{query_type}}"
        }],
        "type": "graph"
      },
      {
        "id": 2,
        "title": "Query Duration (p95)",
        "targets": [{
          "expr": "histogram_quantile(0.95, rate(sparql_query_duration_seconds_bucket[5m]))",
          "legendFormat": "p95"
        }],
        "type": "graph"
      },
      {
        "id": 3,
        "title": "RDF Store Size",
        "targets": [{
          "expr": "rdf_store_triples_total"
        }],
        "type": "graph"
      },
      {
        "id": 4,
        "title": "Transaction Success Rate",
        "targets": [{
          "expr": "rate(transactions_committed_total[5m]) / rate(transactions_begun_total[5m])"
        }],
        "type": "singlestat"
      }
    ]
  }
}
```

---

## Metrics Reference

### SPARQL Metrics

| Metric | Type | Description |
|--------|------|-------------|
| `sparql_queries_total` | Counter | Total SPARQL queries executed |
| `sparql_query_duration_seconds` | Histogram | Query execution time |
| `sparql_query_complexity` | Histogram | Estimated query complexity |
| `sparql_query_results_total` | Histogram | Number of results per query |
| `sparql_query_errors_total` | Counter | Query execution errors |

**Labels:**
- `query_type`: `SELECT`, `CONSTRUCT`, `ASK`, `DESCRIBE`
- `status`: `success`, `error`, `timeout`

**Example PromQL:**
```promql
# Query rate by type
rate(sparql_queries_total[5m])

# p95 query duration
histogram_quantile(0.95, rate(sparql_query_duration_seconds_bucket[5m]))

# Error rate
rate(sparql_query_errors_total[5m]) / rate(sparql_queries_total[5m])
```

### RDF Store Metrics

| Metric | Type | Description |
|--------|------|-------------|
| `rdf_store_triples_total` | Gauge | Total triples in store |
| `rdf_store_add_operations_total` | Counter | Triple add operations |
| `rdf_store_delete_operations_total` | Counter | Triple delete operations |
| `rdf_parse_duration_seconds` | Histogram | RDF parsing time |
| `rdf_serialize_duration_seconds` | Histogram | RDF serialization time |

### Transaction Metrics

| Metric | Type | Description |
|--------|------|-------------|
| `transactions_begun_total` | Counter | Transactions started |
| `transactions_committed_total` | Counter | Transactions committed |
| `transactions_rolled_back_total` | Counter | Transactions rolled back |
| `transaction_duration_seconds` | Histogram | Transaction lifetime |

### System Metrics

| Metric | Type | Description |
|--------|------|-------------|
| `process_cpu_usage_percent` | Gauge | CPU usage |
| `process_memory_usage_bytes` | Gauge | Memory usage |
| `nodejs_heap_size_total_bytes` | Gauge | Node.js heap size |
| `nodejs_heap_size_used_bytes` | Gauge | Node.js heap used |

---

## Traces Reference

### Span Naming Convention

UNRDF uses hierarchical span naming:

```
{operation}.{sub_operation}

Examples:
- sparql.query
- sparql.query.plan
- sparql.query.execute
- rdf.parse
- rdf.serialize
- transaction.begin
- transaction.commit
```

### Span Attributes

Standard attributes added to spans:

| Attribute | Description | Example |
|-----------|-------------|---------|
| `rdf.query.type` | SPARQL query type | `SELECT` |
| `rdf.query.complexity` | Estimated complexity | `42` |
| `rdf.query.results` | Result count | `150` |
| `rdf.store.size` | Store size (triples) | `10000` |
| `rdf.format` | RDF serialization format | `turtle` |
| `transaction.isolation_level` | Isolation level | `serializable` |

### Example Trace Analysis

**Find slow queries:**
```
Service: unrdf-api
Operation: sparql.query
Duration: > 5s
```

**Trace slow query details:**
```
sparql.query [8.2s]
├─ sparql.query.plan [2.1s]
│  └─ Cause: Complex graph pattern
├─ sparql.query.execute [5.8s]
│  ├─ store.match [5.5s]  ← Bottleneck
│  └─ result.format [0.3s]
└─ Attributes:
   - rdf.query.type: SELECT
   - rdf.query.complexity: 85
   - rdf.store.size: 1000000
```

**Solution:** Add indexes or optimize query pattern.

---

## Best Practices

### 1. Use Sampling in Production

```bash
# Sample 10% of traces (reduce overhead)
OTEL_TRACES_SAMPLER=traceidratio
OTEL_TRACES_SAMPLER_ARG=0.1
```

### 2. Add Custom Attributes

```javascript
span.setAttribute('user.id', userId);
span.setAttribute('query.complexity', complexity);
span.setAttribute('cache.hit', cacheHit);
```

### 3. Record Exceptions

```javascript
try {
  await executeQuery();
} catch (error) {
  span.recordException(error);
  span.setStatus({ code: 2, message: error.message });
  throw error;
}
```

### 4. Use Baggage for Context

```javascript
import { propagation, context } from '@opentelemetry/api';

const baggage = propagation.createBaggage({
  'tenant.id': { value: 'tenant-123' }
});

context.with(propagation.setBaggage(context.active(), baggage), () => {
  // All spans in this context inherit baggage
  executeQuery();
});
```

### 5. Monitor Key Metrics

**Set up alerts:**
```yaml
# Prometheus alerting rules
groups:
  - name: unrdf
    rules:
      - alert: HighQueryLatency
        expr: histogram_quantile(0.95, rate(sparql_query_duration_seconds_bucket[5m])) > 5
        for: 5m
        annotations:
          summary: "SPARQL queries are slow (p95 > 5s)"

      - alert: HighErrorRate
        expr: rate(sparql_query_errors_total[5m]) / rate(sparql_queries_total[5m]) > 0.05
        for: 5m
        annotations:
          summary: "Query error rate > 5%"
```

---

## Troubleshooting

### Traces Not Appearing

**Check:**
1. OTEL_ENABLED=true
2. Collector endpoint reachable: `curl http://localhost:4318/v1/traces`
3. SDK initialized before application code
4. No firewall blocking ports

**Debug:**
```javascript
// Enable debug logging
process.env.OTEL_LOG_LEVEL = 'debug';
```

### High Overhead

**Solutions:**
- Reduce sampling rate
- Disable detailed spans
- Use batch processor

```javascript
import { BatchSpanProcessor } from '@opentelemetry/sdk-trace-base';

const processor = new BatchSpanProcessor(exporter, {
  maxQueueSize: 2048,
  maxExportBatchSize: 512,
  scheduledDelayMillis: 5000
});
```

---

**Next:** [Grafana Dashboard Tutorial](grafana-tutorial.md)
