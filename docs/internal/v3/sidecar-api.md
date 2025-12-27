# KGC Sidecar API Documentation

## Overview

The KGC (Knowledge Graph Conformance) Sidecar is a high-performance gRPC service that provides transaction management, hook evaluation, policy validation, and lockchain operations for UNRDF v3.

## Architecture

```
┌─────────────────┐
│   CLI v2        │
│  (Commands)     │
└────────┬────────┘
         │ gRPC
         ▼
┌─────────────────┐
│  Sidecar Client │
│  ┌────────────┐ │
│  │ Circuit    │ │
│  │ Breaker    │ │
│  ├────────────┤ │
│  │ Connection │ │
│  │ Pool       │ │
│  ├────────────┤ │
│  │ Retry      │ │
│  │ Strategy   │ │
│  ├────────────┤ │
│  │ Health     │ │
│  │ Monitor    │ │
│  └────────────┘ │
└────────┬────────┘
         │ gRPC
         ▼
┌─────────────────┐
│   KGC Sidecar   │
│   (Go Service)  │
│  ┌────────────┐ │
│  │Transaction │ │
│  │Engine      │ │
│  ├────────────┤ │
│  │Hook        │ │
│  │Evaluator   │ │
│  ├────────────┤ │
│  │Policy Pack │ │
│  │Validator   │ │
│  ├────────────┤ │
│  │Lockchain   │ │
│  │Writer      │ │
│  └────────────┘ │
└─────────────────┘
```

## gRPC Service Definition

Location: `proto/kgc-sidecar.proto`

### Service Methods

#### ApplyTransaction

Apply an RDF transaction with hooks and receive a cryptographic receipt.

**Request:**
```protobuf
message ApplyTransactionRequest {
  string transaction_id = 1;
  Delta delta = 2;
  TransactionOptions options = 3;
  string actor = 4;
  map<string, string> context = 5;
}
```

**Response:**
```protobuf
message ApplyTransactionResponse {
  bool success = 1;
  TransactionReceipt receipt = 2;
  repeated Error errors = 3;
  PerformanceMetrics metrics = 4;
}
```

**Example:**
```javascript
const response = await client.applyTransaction({
  transactionId: 'tx-123',
  delta: {
    additions: [
      {
        subject: 'ex:Person1',
        predicate: 'rdf:type',
        object: 'foaf:Person'
      }
    ],
    deletions: []
  },
  actor: 'user@example.com'
});
```

#### ValidateGraph

Validate an RDF graph against a policy pack using SHACL.

**Request:**
```protobuf
message ValidateGraphRequest {
  string graph_id = 1;
  repeated Quad quads = 2;
  string policy_pack = 3;
  bool strict_mode = 4;
  map<string, string> options = 5;
}
```

**Response:**
```protobuf
message ValidateGraphResponse {
  bool valid = 1;
  repeated ValidationResult violations = 2;
  repeated Error errors = 3;
  PerformanceMetrics metrics = 4;
}
```

**Example:**
```javascript
const response = await client.validateGraph({
  graphId: 'graph-123',
  quads: [...],
  policyPack: 'healthcare-policy',
  strictMode: true
});
```

#### EvaluateHook

Evaluate a single knowledge hook against a transaction.

**Request:**
```protobuf
message EvaluateHookRequest {
  string hook_id = 1;
  HookDefinition hook = 2;
  HookEvent event = 3;
  map<string, string> options = 4;
}
```

**Response:**
```protobuf
message EvaluateHookResponse {
  bool success = 1;
  HookResult result = 2;
  repeated Error errors = 3;
  PerformanceMetrics metrics = 4;
}
```

**Example:**
```javascript
const response = await client.evaluateHook({
  hookId: 'hook-validation-123',
  hook: {
    meta: { name: 'age-validator' },
    when: { kind: 'sparql-ask' },
    then: { kind: 'veto' }
  },
  event: {
    transactionId: 'tx-123',
    delta: {...}
  }
});
```

#### QueryPolicy

Query policy pack information and metadata.

**Request:**
```protobuf
message QueryPolicyRequest {
  string policy_pack = 1;
  string query_type = 2;
  map<string, string> filters = 3;
}
```

**Response:**
```protobuf
message QueryPolicyResponse {
  PolicyPack policy_pack = 1;
  repeated Error errors = 2;
}
```

**Example:**
```javascript
const response = await client.queryPolicy({
  policyPack: 'healthcare-policy',
  queryType: 'info'
});
```

#### HealthCheck

Check service health and uptime.

**Request:**
```protobuf
message HealthCheckRequest {
  string service = 1;
}
```

**Response:**
```protobuf
message HealthCheckResponse {
  enum ServingStatus {
    UNKNOWN = 0;
    SERVING = 1;
    NOT_SERVING = 2;
    SERVICE_UNKNOWN = 3;
  }
  ServingStatus status = 1;
  map<string, string> details = 2;
  int64 uptime_seconds = 3;
}
```

**Example:**
```javascript
const response = await client.healthCheck({
  service: 'kgc.sidecar.v1.KGCSidecar'
});
```

#### GetMetrics

Retrieve operational metrics and performance data.

**Request:**
```protobuf
message GetMetricsRequest {
  repeated string metric_names = 1;
  int64 since_timestamp = 2;
}
```

**Response:**
```protobuf
message GetMetricsResponse {
  map<string, MetricValue> metrics = 1;
  int64 timestamp = 2;
}
```

**Example:**
```javascript
const response = await client.getMetrics({
  metricNames: ['transactions.applied', 'hooks.evaluated'],
  sinceTimestamp: Date.now() - 3600000 // Last hour
});
```

## CLI Commands

### Status Command

Get sidecar connection status and basic metrics.

```bash
# Basic status
unrdf sidecar status

# JSON output
unrdf sidecar status --output json

# Verbose mode with detailed metrics
unrdf sidecar status --verbose

# Custom address
unrdf sidecar status --address prod-sidecar:50051
```

**Output:**
```
CONNECTED | STATUS  | ADDRESS           | UPTIME (s)
true      | SERVING | localhost:50051   | 3600
```

### Health Command

Detailed health diagnostics with circuit breaker and connection pool metrics.

```bash
# Single health check
unrdf sidecar health

# Continuous monitoring
unrdf sidecar health --watch --interval 5

# JSON output
unrdf sidecar health --output json
```

**Output:**
```json
{
  "service": {
    "status": "✅ SERVING",
    "uptime_seconds": 3600,
    "details": {
      "version": "2.0.0",
      "build": "abc123"
    }
  },
  "connection": {
    "address": "localhost:50051",
    "connected": true
  },
  "circuit_breaker": {
    "state": "CLOSED",
    "failures": 0,
    "successes": 42,
    "half_open_successes": 0
  },
  "connection_pool": {
    "active_connections": 2,
    "available_connections": 8,
    "total_connections": 10,
    "pending_requests": 0
  },
  "request_metrics": {
    "total_requests": 42,
    "successful_requests": 40,
    "failed_requests": 2,
    "success_rate": "95.24%"
  }
}
```

### Config Command

Manage sidecar configuration and contexts.

```bash
# Get entire configuration
unrdf sidecar config get

# Get specific key
unrdf sidecar config get endpoint.address

# Set configuration value
unrdf sidecar config set endpoint.port 9999

# List all contexts
unrdf sidecar config list

# Switch context
unrdf sidecar config use-context production

# Show current context
unrdf sidecar config current
```

**Configuration File:** `~/.kgc/config.json`

```json
{
  "currentContext": "local",
  "contexts": [
    {
      "name": "local",
      "endpoint": {
        "address": "localhost",
        "port": 50051
      },
      "timeout": 5000,
      "maxRetries": 3
    },
    {
      "name": "production",
      "endpoint": {
        "address": "prod-sidecar.example.com",
        "port": 50051,
        "tls": {
          "enabled": true,
          "ca": "/path/to/ca.crt",
          "cert": "/path/to/client.crt",
          "key": "/path/to/client.key"
        }
      },
      "timeout": 10000,
      "maxRetries": 5
    }
  ],
  "defaults": {
    "timeout": 5000,
    "maxRetries": 3,
    "keepAlive": true
  }
}
```

### Logs Command

Stream logs and metrics from the sidecar.

```bash
# Fetch recent metrics (last 100)
unrdf sidecar logs

# Follow logs in real-time
unrdf sidecar logs --follow --interval 2

# Filter by specific metrics
unrdf sidecar logs --metrics "transactions.applied,hooks.evaluated"

# Tail last N entries
unrdf sidecar logs --tail 50

# Since specific timestamp
unrdf sidecar logs --since 1633046400

# JSON output
unrdf sidecar logs --output json
```

**Output:**
```
TIMESTAMP                 | METRIC                     | VALUE
2025-10-01T10:00:00Z     | transactions.applied       | 42 count
2025-10-01T10:00:00Z     | hooks.evaluated            | 128 count
2025-10-01T10:00:00Z     | lockchain.receipts         | 35 count
2025-10-01T10:00:00Z     | performance.p99_latency    | 1.85 ms
2025-10-01T10:00:00Z     | errors.total               | 2 count
```

## Client Configuration

### Basic Client

```javascript
import { createSidecarClient } from 'unrdf/sidecar';

const client = createSidecarClient();
await client.connect('localhost:50051');

// Use client
const health = await client.healthCheck();

// Cleanup
await client.disconnect();
```

### Advanced Client with Options

```javascript
const client = createSidecarClient({
  maxRetries: 5,
  timeout: 10000,
  circuitBreaker: {
    threshold: 10,
    resetTimeout: 60000
  },
  connectionPool: {
    minConnections: 5,
    maxConnections: 20
  },
  enableHealthCheck: true
});

await client.connect('prod-sidecar:50051', {
  'grpc.keepalive_time_ms': 20000,
  'grpc.keepalive_timeout_ms': 10000
});
```

### Environment Variables

```bash
# Sidecar address
export KGC_SIDECAR_ADDRESS="localhost:50051"

# Context
export KGC_SIDECAR_CONTEXT="production"

# TLS configuration
export KGC_SIDECAR_TLS_ENABLED="true"
export KGC_SIDECAR_TLS_CA="/path/to/ca.crt"
export KGC_SIDECAR_TLS_CERT="/path/to/client.crt"
export KGC_SIDECAR_TLS_KEY="/path/to/client.key"

# Timeout and retries
export KGC_SIDECAR_TIMEOUT="10000"
export KGC_SIDECAR_MAX_RETRIES="5"

# Namespace
export KGC_SIDECAR_NAMESPACE="production"
```

## Resilience Patterns

### Circuit Breaker

The client implements circuit breaker pattern to prevent cascading failures:

- **CLOSED**: Normal operation, requests flow through
- **OPEN**: Too many failures, requests fail fast
- **HALF_OPEN**: Testing if service recovered

**States:**
```javascript
const metrics = client.getClientMetrics();
console.log(metrics.circuitBreaker.state); // CLOSED, OPEN, HALF_OPEN
```

### Retry Strategy

Exponential backoff with jitter:

```javascript
{
  maxRetries: 3,
  initialDelay: 100,    // 100ms
  maxDelay: 5000,       // 5s
  backoffMultiplier: 2  // 100ms, 200ms, 400ms, ...
}
```

### Connection Pooling

Maintains pool of gRPC connections for optimal performance:

```javascript
const poolMetrics = client.getClientMetrics().connectionPool;
console.log(`Active: ${poolMetrics.active}/${poolMetrics.total}`);
```

### Health Monitoring

Continuous health checks detect service degradation:

```javascript
client.on('healthStatusChanged', ({ from, to }) => {
  console.log(`Health changed: ${from} → ${to}`);
});
```

## OpenTelemetry Integration

The sidecar client automatically propagates OpenTelemetry trace context via gRPC metadata:

```javascript
import { trace } from '@opentelemetry/api';

const span = trace.getTracer('unrdf').startSpan('apply-transaction');

// Trace context automatically propagated to sidecar
const response = await client.applyTransaction({...});

span.end();
```

**Headers propagated:**
- `x-trace-id`: Trace ID
- `x-span-id`: Span ID
- `x-trace-flags`: Trace flags
- `traceparent`: W3C traceparent header
- `tracestate`: W3C tracestate header

## Performance Targets

| Operation              | P99 Latency | Throughput    |
|-----------------------|-------------|---------------|
| HealthCheck           | < 10ms      | 1000+ req/s   |
| ApplyTransaction      | < 2ms       | 500+ tx/s     |
| ValidateGraph         | < 5ms       | 200+ req/s    |
| EvaluateHook          | < 2ms       | 1000+ req/s   |
| QueryPolicy           | < 1ms       | 2000+ req/s   |
| GetMetrics            | < 5ms       | 500+ req/s    |

## Error Handling

All gRPC errors are transformed into semantic JavaScript errors:

```javascript
try {
  await client.applyTransaction({...});
} catch (error) {
  console.log(error.message);  // Human-readable message
  console.log(error.code);     // gRPC status code
  console.log(error.metadata); // Additional context
}
```

**Common error codes:**
- `UNAVAILABLE`: Service not reachable
- `DEADLINE_EXCEEDED`: Request timeout
- `INVALID_ARGUMENT`: Invalid request parameters
- `NOT_FOUND`: Resource not found
- `PERMISSION_DENIED`: Insufficient permissions
- `RESOURCE_EXHAUSTED`: Rate limit exceeded

## Best Practices

### 1. Connection Management

```javascript
// ✅ GOOD: Reuse connections
const client = createSidecarClient();
await client.connect();

for (let i = 0; i < 100; i++) {
  await client.applyTransaction({...});
}

await client.disconnect();

// ❌ BAD: New connection per request
for (let i = 0; i < 100; i++) {
  const client = createSidecarClient();
  await client.connect();
  await client.applyTransaction({...});
  await client.disconnect();
}
```

### 2. Error Recovery

```javascript
// ✅ GOOD: Let client handle retries
try {
  const response = await client.applyTransaction({...});
} catch (error) {
  // Only handle non-retryable errors
  if (error.code === 'INVALID_ARGUMENT') {
    console.error('Invalid transaction:', error.message);
  }
  throw error; // Re-throw for upper layers
}
```

### 3. Health Monitoring

```javascript
// ✅ GOOD: Use built-in health monitoring
const client = createSidecarClient({
  enableHealthCheck: true
});

client.on('healthStatusChanged', ({ to }) => {
  if (to === 'UNHEALTHY') {
    alert('Sidecar unhealthy!');
  }
});
```

### 4. Configuration

```javascript
// ✅ GOOD: Use configuration file
const config = SidecarConfig.fromFile('~/.kgc/config.json');
const client = createSidecarClient({ config });

// ❌ BAD: Hardcode configuration
const client = createSidecarClient();
await client.connect('hardcoded-address:50051');
```

## Testing

```javascript
import { describe, it, expect, beforeEach } from 'vitest';
import { createSidecarClient } from 'unrdf/sidecar';

describe('Sidecar Integration', () => {
  let client;

  beforeEach(async () => {
    client = createSidecarClient();
    await client.connect();
  });

  it('should apply transaction', async () => {
    const response = await client.applyTransaction({
      transactionId: 'test-tx',
      delta: { additions: [], deletions: [] },
      actor: 'test'
    });

    expect(response.success).toBe(true);
    expect(response.receipt).toBeDefined();
  });
});
```

## Troubleshooting

### Connection Issues

```bash
# Check sidecar is running
unrdf sidecar health

# Check network connectivity
nc -zv localhost 50051

# Enable verbose logging
export GRPC_VERBOSITY=DEBUG
export GRPC_TRACE=all
unrdf sidecar status --verbose
```

### Performance Issues

```bash
# Monitor connection pool
unrdf sidecar health --watch --interval 1

# Check metrics
unrdf sidecar logs --follow --metrics "performance.*"
```

### Circuit Breaker Tripping

```bash
# Check failure rate
unrdf sidecar status --verbose

# Adjust circuit breaker thresholds
unrdf sidecar config set circuitBreaker.threshold 10
```

## See Also

- [Proto Definitions](../../proto/kgc-sidecar.proto)
- [Client Implementation](../../src/sidecar/client.mjs)
- [Configuration Management](../../src/sidecar/config.mjs)
- [CLI Commands](../../src/cli-v2/commands/sidecar/)
- [E2E Tests](../../test/e2e/kgc-sidecar-testcontainer.test.mjs)
