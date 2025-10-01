# Cleanroom Testcontainer Architecture with OpenTelemetry Weaver

## Executive Summary

This document defines a comprehensive cleanroom testcontainer architecture for validating the complete CLI v2 to KGC Sidecar integration stack with full OpenTelemetry observability.

**Design Principle**: 80/20 Focus
- **20% of test scenarios** validate **80% of critical functionality**
- **Core workflows**: CLI commands → gRPC → Sidecar → Knowledge Engine → RDF Store
- **Full observability**: Distributed traces, metrics, and logs with correlation

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    Cleanroom Test Network                       │
│                     (cleanroom-network)                          │
│                                                                  │
│  ┌──────────────┐         ┌─────────────────┐                  │
│  │  CLI v2      │────────▶│  OTEL Collector │                  │
│  │  (Host)      │  OTLP   │  (otel-collector)│                  │
│  │              │  gRPC   │                 │                  │
│  └──────┬───────┘         └────────┬────────┘                  │
│         │                          │                            │
│         │ gRPC                     │ Traces/Metrics             │
│         │ :50051                   │                            │
│         │                          │                            │
│         ▼                          ▼                            │
│  ┌──────────────┐         ┌─────────────────┐                  │
│  │  KGC Sidecar │────────▶│     Jaeger      │                  │
│  │  (sidecar)   │  OTLP   │  (jaeger)       │                  │
│  │  gRPC Server │         │  UI: 16686      │                  │
│  └──────┬───────┘         └─────────────────┘                  │
│         │                                                        │
│         │ Knowledge Engine                                      │
│         │                                                        │
│         ▼                                                        │
│  ┌──────────────┐                                               │
│  │  PostgreSQL  │                                               │
│  │  (postgres)  │                                               │
│  │  RDF Store   │                                               │
│  └──────────────┘                                               │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Component Specifications

### 1. OTEL Collector

**Purpose**: Central telemetry hub for traces, metrics, and logs

**Container Spec**:
- **Image**: `otel/opentelemetry-collector-contrib:0.91.0`
- **Network Alias**: `otel-collector`
- **Ports**:
  - `4317`: gRPC OTLP receiver
  - `4318`: HTTP OTLP receiver
  - `8888`: Prometheus metrics
  - `13133`: Health check
- **Configuration**: `/etc/otel/config.yaml`
- **Startup**: < 5 seconds
- **Health Check**: `http://localhost:13133/`

**Pipeline Configuration**:
```yaml
receivers:
  - otlp (gRPC + HTTP)

processors:
  - batch (1s timeout, 1024 batch size)
  - memory_limiter (80% soft, 90% hard)
  - attributes (resource detection, span enrichment)

exporters:
  - jaeger (distributed tracing)
  - prometheus (metrics exposition)
  - logging (debug output)
```

### 2. Jaeger All-in-One

**Purpose**: Distributed trace visualization and query interface

**Container Spec**:
- **Image**: `jaegertracing/all-in-one:1.52`
- **Network Alias**: `jaeger`
- **Ports**:
  - `16686`: Jaeger UI (query interface)
  - `14250`: gRPC collector endpoint
  - `14268`: HTTP collector endpoint
- **Storage**: In-memory (ephemeral for tests)
- **Startup**: < 10 seconds
- **Health Check**: `http://localhost:14269/`

**Features**:
- Service dependency graphs
- Trace search and filtering
- Latency percentiles
- Error rate tracking

### 3. KGC Sidecar

**Purpose**: gRPC server for knowledge graph operations with OTEL instrumentation

**Container Spec**:
- **Image**: Custom build from `sidecar.Dockerfile`
- **Base**: `node:20-alpine`
- **Network Alias**: `sidecar`
- **Ports**:
  - `50051`: gRPC server
  - `9464`: Prometheus metrics (OTEL)
- **Environment**:
  - `OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317`
  - `OTEL_SERVICE_NAME=kgc-sidecar`
  - `OTEL_RESOURCE_ATTRIBUTES=service.version=2.0.0,deployment.environment=cleanroom`
- **Startup**: < 15 seconds
- **Health Check**: gRPC health probe on `:50051`

**Instrumentation**:
- `@opentelemetry/sdk-node`
- `@opentelemetry/instrumentation-grpc`
- `@opentelemetry/instrumentation-http`
- Custom knowledge engine spans

### 4. PostgreSQL

**Purpose**: RDF triple store backend

**Container Spec**:
- **Image**: `postgres:16-alpine`
- **Network Alias**: `postgres`
- **Port**: `5432`
- **Database**: `unrdf_cleanroom`
- **User**: `cleanroom_user`
- **Password**: `cleanroom_pass` (test only)
- **Startup**: < 8 seconds
- **Health Check**: `pg_isready -U cleanroom_user`

**Schema**:
- RDF quad table (subject, predicate, object, graph)
- B-tree indexes on subject/predicate
- GiST indexes for full-text search

## Network Architecture

### Docker Network Configuration

**Network Name**: `cleanroom-network`
**Driver**: `bridge`
**Subnet**: `172.30.0.0/16`
**Gateway**: `172.30.0.1`

**Static IP Assignments**:
- OTEL Collector: `172.30.0.10`
- Jaeger: `172.30.0.11`
- KGC Sidecar: `172.30.0.20`
- PostgreSQL: `172.30.0.30`

**DNS Resolution**:
- Automatic service discovery via Docker DNS
- Container aliases resolve within network
- No external network dependencies

## OpenTelemetry Instrumentation

### Trace Context Propagation

**Flow**: CLI → gRPC Metadata → Sidecar → Knowledge Engine

**Format**: W3C Trace Context
- `traceparent`: `00-{trace-id}-{span-id}-{flags}`
- `tracestate`: Vendor-specific data

**Implementation**:
```javascript
// CLI side
import { trace, context, propagation } from '@opentelemetry/api';

const span = tracer.startSpan('cli.command.execute');
const carrier = {};
propagation.inject(context.active(), carrier);

// Inject into gRPC metadata
const metadata = new grpc.Metadata();
metadata.set('traceparent', carrier.traceparent);

grpcClient.executeCommand(request, metadata);
```

```javascript
// Sidecar side
import { propagation, context } from '@opentelemetry/api';

// Extract from gRPC metadata
const carrier = {
  traceparent: call.metadata.get('traceparent')[0]
};
const extractedContext = propagation.extract(context.active(), carrier);

// Continue trace
const span = tracer.startSpan('sidecar.hook.evaluate', {}, extractedContext);
```

### Span Hierarchy

```
CLI Command (cli.command.execute)
  └─ gRPC Call (grpc.client.unary)
      └─ Sidecar Hook Evaluation (sidecar.hook.evaluate)
          ├─ Knowledge Engine Query (knowledge.query.execute)
          │   └─ RDF Store Query (rdf.store.sparql)
          ├─ Hook Execution (hook.execute)
          └─ Transaction Commit (transaction.commit)
```

### Metrics Collection

**CLI Metrics**:
- `cli.commands.executed` (counter)
- `cli.command.duration` (histogram)
- `cli.grpc.errors` (counter)

**Sidecar Metrics**:
- `sidecar.hooks.evaluated` (counter)
- `sidecar.hook.duration` (histogram)
- `sidecar.transactions.committed` (counter)
- `sidecar.rdf.triples.count` (gauge)

**Knowledge Engine Metrics**:
- `knowledge.queries.executed` (counter)
- `knowledge.query.duration` (histogram)
- `knowledge.reasoner.inferences` (counter)

### Log Correlation

**Structured Logging Format**:
```json
{
  "timestamp": "2025-10-01T12:34:56.789Z",
  "level": "info",
  "message": "Hook evaluated successfully",
  "service": "kgc-sidecar",
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "00f067aa0ba902b7",
  "hook_id": "validate-person",
  "duration_ms": 23.5
}
```

**Correlation IDs**:
- Trace ID propagates through entire request chain
- Span ID identifies specific operation
- Logs linked to traces in Jaeger

## Service Startup Sequence

**Dependency Order**:
1. **PostgreSQL** (0-8s): RDF storage backend
2. **OTEL Collector** (8-13s): Telemetry pipeline
3. **Jaeger** (13-23s): Trace storage and UI
4. **KGC Sidecar** (23-38s): gRPC server with knowledge engine

**Total Startup Time**: < 30 seconds

**Wait Strategies**:
```javascript
// PostgreSQL: Wait for connection
await waitForPort(postgres, 5432, { timeout: 10000 });
await execInContainer(postgres, 'pg_isready -U cleanroom_user');

// OTEL Collector: Wait for health endpoint
await waitForHttp('http://otel-collector:13133/', { timeout: 5000 });

// Jaeger: Wait for UI
await waitForHttp('http://jaeger:16686/', { timeout: 10000 });

// Sidecar: Wait for gRPC health check
await waitForGrpc('sidecar:50051', 'grpc.health.v1.Health/Check', { timeout: 15000 });
```

## Cleanroom Environment Guarantees

### Isolation

**Container Isolation**:
- Each test run gets fresh containers
- No shared state between test runs
- Network isolated from host (except exposed ports)
- Ephemeral storage (no persistent volumes)

**Data Isolation**:
- PostgreSQL database dropped and recreated
- No test data persists between runs
- OTEL/Jaeger use in-memory storage

### Reproducibility

**Deterministic Setup**:
- Fixed container image versions (no `:latest` tags)
- Static IP assignments for predictable networking
- Seeded test data for consistent state
- Environment variables explicitly set

**Idempotent Cleanup**:
- Containers stopped and removed after each run
- Networks deleted
- Temporary files cleaned up
- No leftover state

### Speed

**Optimization Strategies**:
- **Image caching**: Pre-pull images on CI
- **Parallel startup**: Launch independent containers simultaneously
- **Minimal images**: Alpine-based containers
- **Health checks**: Fast polling intervals
- **Batch operations**: Group setup operations

**Performance Targets**:
- Full stack startup: < 30 seconds
- Individual test execution: < 5 seconds
- Cleanup: < 10 seconds
- Total test suite: < 2 minutes

## Integration Points

### CLI to Sidecar Communication

**Protocol**: gRPC with OTLP metadata

**Service Definition** (`kgc.proto`):
```protobuf
service KnowledgeGraphController {
  rpc ExecuteHook(HookRequest) returns (HookResponse);
  rpc QueryKnowledge(QueryRequest) returns (QueryResponse);
  rpc CommitTransaction(TransactionRequest) returns (TransactionResponse);
}

message HookRequest {
  string hook_id = 1;
  string graph_data = 2;
  map<string, string> context = 3;
}

message HookResponse {
  bool success = 1;
  string message = 2;
  repeated string validations = 3;
  map<string, string> trace_context = 4;
}
```

**Trace Context in Metadata**:
```javascript
const metadata = new grpc.Metadata();
metadata.set('traceparent', '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01');
metadata.set('baggage', 'user=alice,session=abc123');
```

### Sidecar to OTEL Collector

**Export Protocol**: OTLP over gRPC

**Batch Configuration**:
- Max batch size: 512 spans
- Batch timeout: 5 seconds
- Max queue size: 2048 spans
- Retry: Exponential backoff

**Resource Attributes**:
```javascript
{
  'service.name': 'kgc-sidecar',
  'service.version': '2.0.0',
  'service.instance.id': 'sidecar-1',
  'deployment.environment': 'cleanroom',
  'host.name': 'sidecar',
  'process.pid': 1234
}
```

### OTEL Collector to Jaeger

**Export Protocol**: Jaeger Thrift over gRPC

**Configuration**:
```yaml
exporters:
  jaeger:
    endpoint: jaeger:14250
    tls:
      insecure: true
```

**Data Transformation**:
- OTLP spans → Jaeger Thrift format
- Resource attributes → Process tags
- Span attributes → Span tags
- Span events → Span logs

## Testing Scenarios (80/20 Focus)

### Critical Path Tests (80% Coverage)

#### 1. CLI Command Execution with Trace Propagation
**Scenario**: Execute `unrdf hook evaluate` and verify trace propagation

**Steps**:
1. CLI starts root span `cli.command.execute`
2. CLI calls sidecar via gRPC with trace context
3. Sidecar extracts context and creates child span
4. Sidecar evaluates hook and queries RDF store
5. All spans exported to OTEL collector
6. Query Jaeger for complete trace

**Validation**:
- Trace has 4+ spans (CLI → gRPC → Sidecar → RDF)
- Parent-child relationships correct
- All spans have service.name attribute
- Trace duration < 500ms

#### 2. Hook Evaluation with Error Propagation
**Scenario**: Trigger SHACL validation error and verify error in trace

**Steps**:
1. Execute hook with invalid RDF data
2. SHACL validation fails in knowledge engine
3. Error span recorded with exception event
4. Error status propagates to parent spans
5. CLI receives error response

**Validation**:
- Span has `error=true` tag
- Exception event recorded with stack trace
- Error message propagated to CLI
- Jaeger shows error in service graph

#### 3. Multi-Hook Transaction with Metrics
**Scenario**: Execute transaction with multiple hooks and collect metrics

**Steps**:
1. Begin transaction
2. Evaluate hook A (validation)
3. Evaluate hook B (transformation)
4. Commit transaction
5. Verify metrics exported

**Validation**:
- `sidecar.hooks.evaluated` = 2
- `sidecar.transactions.committed` = 1
- `knowledge.queries.executed` >= 2
- All metrics have correct labels

#### 4. Service Health and Resilience
**Scenario**: Verify health checks and container recovery

**Steps**:
1. Start all containers
2. Verify health checks pass
3. Query Jaeger UI (verify reachable)
4. Execute CLI command (verify end-to-end)

**Validation**:
- All containers healthy within 30s
- Jaeger UI accessible
- CLI command succeeds
- No container restarts

## Observability Queries

### Jaeger Trace Queries

**Find slow operations**:
```
service=kgc-sidecar duration>500ms
```

**Find errors**:
```
service=kgc-sidecar error=true
```

**Find specific hook executions**:
```
service=kgc-sidecar hook.id=validate-person
```

**Service dependency graph**:
Navigate to "System Architecture" in Jaeger UI

### Prometheus Metrics Queries

**Hook evaluation rate**:
```promql
rate(sidecar_hooks_evaluated_total[1m])
```

**95th percentile latency**:
```promql
histogram_quantile(0.95,
  rate(sidecar_hook_duration_seconds_bucket[5m])
)
```

**Error rate**:
```promql
rate(cli_grpc_errors_total[1m])
```

## Deployment and Usage

### Quick Start

```bash
# Start cleanroom environment
cd test/e2e/cleanroom
docker-compose up -d

# Wait for services
npm run cleanroom:wait

# Run integration tests
npm test test/e2e/cleanroom/

# View traces
open http://localhost:16686

# Cleanup
docker-compose down -v
```

### Programmatic Usage

```javascript
import { CleanroomEnvironment } from './setup-cleanroom.mjs';

const cleanroom = new CleanroomEnvironment();

// Start all services
await cleanroom.start();

// Execute CLI command with tracing
const result = await cleanroom.executeCliCommand('hook', 'evaluate', {
  hookId: 'validate-person',
  data: personData
});

// Query traces from Jaeger
const traces = await cleanroom.queryTraces({
  service: 'kgc-sidecar',
  operation: 'sidecar.hook.evaluate'
});

// Cleanup
await cleanroom.cleanup();
```

## Quality Attributes

### Performance
- **Startup**: < 30 seconds for full stack
- **Latency**: < 100ms for simple hook evaluations
- **Throughput**: > 100 operations/second

### Reliability
- **Container Health**: 100% healthy within startup window
- **Test Determinism**: Same results on every run
- **Cleanup Success**: 100% of containers removed

### Observability
- **Trace Completeness**: 100% of operations traced
- **Metric Accuracy**: ±1% of actual values
- **Log Correlation**: 100% of logs linked to traces

## Future Enhancements

### Phase 2
- Add Prometheus for long-term metrics storage
- Add Grafana for metrics visualization
- Add distributed load testing with k6

### Phase 3
- Add chaos engineering (container failures)
- Add performance regression detection
- Add trace-based test generation

## References

- [OpenTelemetry Specification](https://opentelemetry.io/docs/specs/otel/)
- [W3C Trace Context](https://www.w3.org/TR/trace-context/)
- [Jaeger Documentation](https://www.jaegertracing.io/docs/)
- [Testcontainers Documentation](https://testcontainers.com/)
