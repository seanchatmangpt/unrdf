# Sidecar gRPC API Reference

Complete API reference for the UNRDF KGC Sidecar - a production-grade gRPC service for knowledge graph operations.

## Overview

The KGC Sidecar is a Node.js reference implementation providing transactional knowledge-graph mutation with policy-pack governance for any host application (Erlang/C/C++/Go/etc.).

**Key Features:**

- ✅ gRPC-based communication (Protocol Buffers)
- ✅ Circuit breaker resilience pattern
- ✅ Exponential backoff retry strategy
- ✅ Connection pooling
- ✅ Health monitoring with probes
- ✅ OpenTelemetry integration
- ✅ Multi-environment configuration

## Client API

### SidecarClient

Main gRPC client for sidecar communication.

#### Constructor

```javascript
import { SidecarClient } from 'unrdf/sidecar';

const client = new SidecarClient({
  address: 'localhost:50051',
  timeout: 30000,
  maxRetries: 3,
  circuitBreaker: {
    enabled: true,
    threshold: 5,
    resetTimeout: 30000
  },
  tls: {
    enabled: false
  }
});
```

**Options:**

- `address` (string): Sidecar address (host:port)
- `timeout` (number): Request timeout in ms (default: 30000)
- `maxRetries` (number): Max retry attempts (default: 3)
- `circuitBreaker` (Object): Circuit breaker configuration
- `tls` (Object): TLS configuration

#### connect()

Connect to the sidecar service.

```javascript
await client.connect();
```

**Returns:** `Promise<void>`

**Throws:** `Error` if connection fails

#### disconnect()

Gracefully disconnect from the sidecar.

```javascript
await client.disconnect();
```

#### healthCheck()

Check sidecar health status.

```javascript
const health = await client.healthCheck();
console.log('Status:', health.status);  // 'HEALTHY' | 'UNHEALTHY'
```

**Returns:** `Promise<HealthCheckResponse>`

```typescript
{
  status: 'HEALTHY' | 'UNHEALTHY' | 'DEGRADED';
  timestamp: number;
  details?: string;
}
```

#### applyTransaction(delta, options)

Apply an RDF transaction to the knowledge graph.

```javascript
const result = await client.applyTransaction({
  additions: [
    {
      subject: 'http://example.org/Alice',
      predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      object: 'http://example.org/Person',
      graph: 'http://example.org/graph/default'
    }
  ],
  removals: []
}, {
  validate: true,
  strictMode: true
});

console.log('Committed:', result.committed);
console.log('Receipt hash:', result.receipt.hash);
```

**Parameters:**

- `delta` (Object): Transaction delta
  - `additions` (Array): Quads to add
  - `removals` (Array): Quads to remove
- `options` (Object): Transaction options
  - `validate` (boolean): Enable validation (default: true)
  - `strictMode` (boolean): Strict error handling (default: false)

**Returns:** `Promise<TransactionResult>`

```typescript
{
  committed: boolean;
  receipt: {
    hash: string;
    timestamp: number;
    hookResults?: Array<HookResult>;
  };
  error?: string;
}
```

#### validateGraph(graph, shapes, options)

Validate RDF graph against SHACL shapes.

```javascript
const validation = await client.validateGraph(
  graphTriples,
  shaclShapes,
  { strictMode: true }
);

console.log('Conforms:', validation.conforms);
if (!validation.conforms) {
  console.log('Violations:', validation.results);
}
```

**Returns:** `Promise<ValidationReport>`

```typescript
{
  conforms: boolean;
  results: Array<{
    focusNode: string;
    resultPath: string;
    message: string;
    severity: 'Violation' | 'Warning' | 'Info';
  }>;
}
```

#### evaluateHook(hookName, event, options)

Evaluate a knowledge hook.

```javascript
const result = await client.evaluateHook('compliance:large-tx', {
  name: 'manual-trigger',
  payload: { amount: 50000 },
  context: { transactionId: 'tx-123' }
});

console.log('Hook result:', result);
```

**Returns:** `Promise<HookResult>`

#### query(sparql, options)

Execute a SPARQL query.

```javascript
const results = await client.query(`
  PREFIX ex: <http://example.org/>
  SELECT ?name WHERE {
    ?person a ex:Person ;
      ex:name ?name .
  }
`, {
  limit: 100,
  timeout: 5000
});

console.log('Results:', results);
```

**Returns:** `Promise<QueryResult>`

## Connection Pool

### ConnectionPool

Manage multiple sidecar connections.

```javascript
import { ConnectionPool } from 'unrdf/sidecar';

const pool = new ConnectionPool({
  minConnections: 2,
  maxConnections: 10,
  idleTimeout: 60000,
  healthCheckInterval: 30000
});

await pool.initialize({
  address: 'localhost:50051',
  timeout: 30000
});

// Acquire connection
const conn = await pool.acquire();

try {
  await conn.healthCheck();
} finally {
  pool.release(conn);
}

// Cleanup
await pool.drain();
await pool.clear();
```

**Options:**

- `minConnections` (number): Minimum pool size (default: 2)
- `maxConnections` (number): Maximum pool size (default: 10)
- `idleTimeout` (number): Idle connection timeout in ms (default: 60000)
- `healthCheckInterval` (number): Health check frequency in ms (default: 30000)

### Methods

#### acquire()

Acquire a connection from the pool.

**Returns:** `Promise<SidecarClient>`

#### release(connection)

Release a connection back to the pool.

#### drain()

Wait for all active connections to be released.

**Returns:** `Promise<void>`

#### clear()

Close all connections.

**Returns:** `Promise<void>`

#### getStats()

Get pool statistics.

```javascript
const stats = pool.getStats();
console.log('Active connections:', stats.active);
console.log('Idle connections:', stats.idle);
console.log('Total connections:', stats.total);
```

**Returns:**

```typescript
{
  active: number;
  idle: number;
  total: number;
  waiting: number;
}
```

## Circuit Breaker

### CircuitBreaker

Resilience pattern for handling failures.

```javascript
import { CircuitBreaker } from 'unrdf/sidecar';

const breaker = new CircuitBreaker({
  threshold: 5,          // Open after 5 failures
  resetTimeout: 30000,   // Try again after 30s
  monitoringPeriod: 60000
});

// Execute operation with circuit breaker
const result = await breaker.execute(async () => {
  return await client.healthCheck();
});

// Check breaker state
console.log('State:', breaker.getState());  // 'CLOSED' | 'OPEN' | 'HALF_OPEN'
```

**Configuration:**

- `threshold` (number): Failure threshold before opening (default: 5)
- `resetTimeout` (number): Time before attempting reset in ms (default: 30000)
- `monitoringPeriod` (number): Monitoring window in ms (default: 60000)

**States:**

- `CLOSED`: Normal operation, requests pass through
- `OPEN`: Too many failures, requests fail immediately
- `HALF_OPEN`: Testing if service recovered

### Methods

#### execute(fn)

Execute function with circuit breaker protection.

**Returns:** `Promise<T>`

**Throws:** `Error` if circuit is open

#### getState()

Get current circuit state.

**Returns:** `'CLOSED' | 'OPEN' | 'HALF_OPEN'`

#### getMetrics()

Get circuit breaker metrics.

```javascript
const metrics = breaker.getMetrics();
console.log('Failures:', metrics.failures);
console.log('Success rate:', metrics.successRate);
```

## Retry Strategy

### RetryStrategy

Exponential backoff retry logic.

```javascript
import { RetryStrategy } from 'unrdf/sidecar';

const retry = new RetryStrategy({
  maxRetries: 3,
  initialDelay: 1000,
  maxDelay: 30000,
  multiplier: 2,
  jitter: true
});

const result = await retry.execute(async () => {
  return await client.applyTransaction(delta);
});
```

**Configuration:**

- `maxRetries` (number): Maximum retry attempts (default: 3)
- `initialDelay` (number): Initial delay in ms (default: 1000)
- `maxDelay` (number): Maximum delay in ms (default: 30000)
- `multiplier` (number): Backoff multiplier (default: 2)
- `jitter` (boolean): Add randomness to delays (default: true)

## Health Monitor

### HealthCheck

Continuous health monitoring.

```javascript
import { HealthCheck } from 'unrdf/sidecar';

const health = new HealthCheck({
  checkInterval: 30000,
  consecutiveFailures: 3,
  startupGracePeriod: 10000
});

await health.start(async () => {
  return await client.healthCheck();
});

health.on('statusChange', (status) => {
  console.log('Health status changed:', status);
});

// Get current status
const status = health.getStatus();
console.log('Status:', status);  // 'HEALTHY' | 'UNHEALTHY' | 'STARTING'
```

## Telemetry

### OpenTelemetry Integration

Built-in observability with OpenTelemetry.

```javascript
import { initTelemetry } from 'unrdf/sidecar';

const telemetry = await initTelemetry({
  serviceName: 'my-app',
  endpoint: 'http://jaeger:14268/api/traces',
  metrics: {
    enabled: true,
    port: 9464
  }
});

// Telemetry is automatically instrumented
// All gRPC calls are traced and metered
```

**Metrics Exported:**

- `grpc_client_requests_total` - Total requests
- `grpc_client_request_duration_seconds` - Request latency histogram
- `grpc_client_errors_total` - Total errors
- `circuit_breaker_state` - Circuit breaker state gauge
- `connection_pool_active` - Active connections
- `connection_pool_idle` - Idle connections

**Traces:**

- Distributed tracing for all gRPC calls
- Context propagation across services
- Span tags: method, status, error

## Configuration Management

### Multi-Environment Config

```javascript
import { loadConfig } from 'unrdf/sidecar';

const config = await loadConfig({
  configPath: '~/.kgc/config.json',
  context: 'production'
});

const client = new SidecarClient(config.endpoint);
```

**Config File Structure** (`~/.kgc/config.json`):

```json
{
  "currentContext": "production",
  "contexts": {
    "production": {
      "endpoint": {
        "address": "kgc.example.com",
        "port": 443
      },
      "tls": {
        "enabled": true,
        "ca": "/path/to/ca.crt",
        "cert": "/path/to/client.crt",
        "key": "/path/to/client.key"
      },
      "timeout": 30000,
      "maxRetries": 3
    },
    "dev": {
      "endpoint": {
        "address": "localhost",
        "port": 50051
      },
      "tls": { "enabled": false }
    }
  }
}
```

## Kubernetes Deployment

### Sidecar Pattern

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: my-app
spec:
  containers:
  - name: app
    image: my-app:latest
    env:
    - name: KGC_SIDECAR_ADDRESS
      value: "localhost:50051"

  - name: kgc-sidecar
    image: unrdf/sidecar:latest
    ports:
    - containerPort: 50051
      name: grpc
    resources:
      requests:
        memory: "256Mi"
        cpu: "100m"
      limits:
        memory: "512Mi"
        cpu: "500m"
    livenessProbe:
      grpc:
        port: 50051
      initialDelaySeconds: 10
      periodSeconds: 10
    readinessProbe:
      grpc:
        port: 50051
      initialDelaySeconds: 5
      periodSeconds: 5
```

### Service Mesh Integration

Works with Istio, Linkerd for mTLS and traffic management:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: kgc-sidecar
  annotations:
    service.beta.kubernetes.io/aws-load-balancer-type: "nlb"
spec:
  type: LoadBalancer
  selector:
    app: kgc-sidecar
  ports:
  - port: 50051
    targetPort: 50051
    protocol: TCP
```

## Error Handling

### Error Types

```javascript
import {
  ConnectionError,
  ValidationError,
  TransactionError,
  TimeoutError
} from 'unrdf/sidecar';

try {
  await client.applyTransaction(delta);
} catch (error) {
  if (error instanceof ConnectionError) {
    console.error('Connection failed:', error.message);
  } else if (error instanceof ValidationError) {
    console.error('Validation failed:', error.violations);
  } else if (error instanceof TransactionError) {
    console.error('Transaction failed:', error.reason);
  }
}
```

### Retry-able Errors

The retry strategy automatically retries these errors:

- `UNAVAILABLE` - Service unavailable
- `DEADLINE_EXCEEDED` - Request timeout
- `RESOURCE_EXHAUSTED` - Rate limited
- `INTERNAL` - Internal server error

## Performance Targets

| Metric | Target | Status |
|--------|--------|--------|
| Health Check (p99) | < 10ms | ✅ |
| Transaction Apply (p99) | < 50ms | ✅ |
| Graph Validation (p99) | < 100ms | ✅ |
| Hook Evaluation (p99) | < 200ms | ✅ |
| Concurrent Requests | > 1000 RPS | ✅ |

## Best Practices

### 1. Use Connection Pooling

```javascript
// Good: Use connection pool
const pool = new ConnectionPool({ maxConnections: 10 });
const conn = await pool.acquire();

// Bad: Create new client per request
const client = new SidecarClient({ address: '...' });
```

### 2. Enable Circuit Breaker

```javascript
// Good: Circuit breaker prevents cascade failures
const client = new SidecarClient({
  circuitBreaker: { enabled: true }
});

// Bad: No protection
const client = new SidecarClient({
  circuitBreaker: { enabled: false }
});
```

### 3. Configure Timeouts

```javascript
// Good: Reasonable timeout
await client.query(sparql, { timeout: 5000 });

// Bad: No timeout
await client.query(sparql);
```

### 4. Monitor Health

```javascript
// Good: Continuous health monitoring
const health = new HealthCheck();
await health.start(async () => await client.healthCheck());

// Bad: No health checks
```

## See Also

- [Knowledge Hooks API](/docs/api/knowledge-hooks.md)
- [CLI Reference](/docs/api/cli-reference.md)
- [Deployment Guide](/docs/deployment/)
- [Architecture Overview](/docs/architecture/sidecar.md)
