# KGC Sidecar Client Usage Guide

## Overview

The KGC sidecar gRPC client provides enterprise-grade connectivity to Knowledge Graph Conformance sidecar services with built-in resilience patterns, observability, and multi-environment support.

## Features

- **gRPC Communication**: High-performance Protocol Buffers-based RPC
- **Connection Pooling**: Efficient resource management with automatic scaling
- **Circuit Breaker**: Prevent cascade failures with automatic recovery
- **Retry Strategy**: Exponential backoff with jitter for transient failures
- **Health Monitoring**: Continuous health checks with liveness/readiness probes
- **OpenTelemetry**: Distributed tracing and metrics collection
- **Multi-Environment**: Context-based configuration (dev, staging, prod)
- **TLS/mTLS Support**: Secure communication with mutual authentication

## Quick Start

### Installation

```bash
npm install unrdf
# or
pnpm add unrdf
```

### Basic Usage

```javascript
import { SidecarClient } from 'unrdf/sidecar';

// Create and connect client
const client = await SidecarClient.connect('localhost:50051');

// Apply a transaction
const result = await client.applyTransaction({
  delta: {
    additions: [
      {
        subject: 'http://example.org/resource1',
        predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        object: 'http://example.org/Type',
        graph: 'http://example.org/default'
      }
    ],
    deletions: []
  },
  actor: 'user@example.com',
  options: {
    strict_mode: true,
    enable_hooks: true
  }
});

console.log('Transaction committed:', result.receipt.committed);
console.log('Transaction ID:', result.receipt.transaction_id);

// Disconnect when done
await client.disconnect();
```

## Configuration

### Environment Variables

```bash
# Sidecar endpoint
export KGC_SIDECAR_ADDRESS="localhost:50051"

# Context name
export KGC_SIDECAR_CONTEXT="production"

# TLS configuration
export KGC_SIDECAR_TLS_ENABLED="true"
export KGC_SIDECAR_TLS_CA="/path/to/ca.crt"
export KGC_SIDECAR_TLS_CERT="/path/to/client.crt"
export KGC_SIDECAR_TLS_KEY="/path/to/client.key"

# Connection tuning
export KGC_SIDECAR_TIMEOUT="5000"
export KGC_SIDECAR_MAX_RETRIES="3"
```

### Configuration File

Create `~/.kgc/config.json`:

```json
{
  "currentContext": "production",
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
        "address": "kgc-sidecar.example.com",
        "port": 443,
        "tls": {
          "enabled": true,
          "ca": "/path/to/ca.crt",
          "cert": "/path/to/client.crt",
          "key": "/path/to/client.key"
        }
      },
      "timeout": 10000,
      "maxRetries": 5,
      "circuitBreaker": {
        "threshold": 5,
        "resetTimeout": 30000,
        "halfOpenRequests": 3
      }
    }
  ],
  "defaults": {
    "timeout": 5000,
    "maxRetries": 3,
    "keepAlive": true,
    "keepAliveTime": 10000
  }
}
```

### Programmatic Configuration

```javascript
import { SidecarClient, createSidecarConfig } from 'unrdf/sidecar';

const config = createSidecarConfig({
  currentContext: 'production',
  contexts: [{
    name: 'production',
    endpoint: {
      address: 'kgc-sidecar.example.com',
      port: 443,
      tls: { enabled: true }
    },
    timeout: 10000,
    maxRetries: 5
  }]
});

const client = new SidecarClient({ config });
await client.connect();
```

## API Reference

### Transaction Operations

#### applyTransaction(request)

Apply a transaction with hooks and return cryptographic receipt.

```javascript
const result = await client.applyTransaction({
  delta: {
    additions: [/* quads to add */],
    deletions: [/* quads to delete */]
  },
  actor: 'user@example.com',
  options: {
    strict_mode: true,
    enable_hooks: true,
    enable_lockchain: true,
    policy_pack: 'governance-v1'
  },
  context: {
    reason: 'User data update',
    source: 'web-app'
  }
});

// Access receipt
console.log('Before hash:', result.receipt.before_hash);
console.log('After hash:', result.receipt.after_hash);
console.log('Hook results:', result.receipt.hook_results);
```

### Validation Operations

#### validateGraph(request)

Validate graph quads against policy pack.

```javascript
const result = await client.validateGraph({
  quads: [/* graph quads */],
  policyPack: 'compliance-v1',
  strictMode: true,
  options: {
    includeDetails: true
  }
});

if (result.valid) {
  console.log('Graph is valid');
} else {
  console.log('Violations:', result.violations);
}
```

### Hook Operations

#### evaluateHook(request)

Evaluate a single knowledge hook.

```javascript
const result = await client.evaluateHook({
  hookId: 'health-check',
  hook: {
    meta: {
      name: 'health-check',
      description: 'Validate data health'
    },
    when: {
      kind: 'sparql-ask',
      ref: {
        uri: 'file:///hooks/health-check.sparql',
        sha256: 'abc123...'
      }
    },
    then: {
      kind: 'log',
      options: { level: 'info' }
    }
  },
  event: {
    transactionId: 'tx-123',
    delta: { additions: [], deletions: [] },
    actor: 'system'
  }
});

console.log('Hook passed:', result.result.passed);
```

### Policy Operations

#### queryPolicy(request)

Query policy pack information.

```javascript
const result = await client.queryPolicy({
  policyPack: 'governance-v1',
  queryType: 'info'
});

console.log('Policy pack:', result.policy_pack.name);
console.log('Version:', result.policy_pack.version);
console.log('Active:', result.policy_pack.active);
console.log('Hooks:', result.policy_pack.hooks.length);
```

### Health & Monitoring

#### healthCheck()

Perform health check.

```javascript
const health = await client.healthCheck();

console.log('Status:', health.status); // SERVING | NOT_SERVING
console.log('Uptime:', health.uptime_seconds);
```

#### getMetrics()

Get sidecar metrics.

```javascript
const metrics = await client.getMetrics({
  metricNames: ['transaction_count', 'hook_execution_rate'],
  sinceTimestamp: Date.now() - 3600000 // Last hour
});

console.log('Metrics:', metrics.metrics);
```

#### getClientMetrics()

Get client-side metrics.

```javascript
const metrics = client.getClientMetrics();

console.log('Requests:', metrics.requests);
console.log('Success rate:', metrics.successes / metrics.requests);
console.log('Circuit breaker state:', metrics.circuitBreaker.state);
console.log('Connection pool:', metrics.connectionPool);
console.log('Health status:', metrics.health.status);
```

## Resilience Patterns

### Circuit Breaker

The circuit breaker automatically prevents cascade failures:

```javascript
const client = new SidecarClient({
  circuitBreaker: {
    threshold: 5,        // Open after 5 consecutive failures
    resetTimeout: 30000, // Try again after 30 seconds
    halfOpenRequests: 3  // Test with 3 requests in half-open state
  }
});

// Circuit breaker automatically handles failures
try {
  await client.applyTransaction(request);
} catch (error) {
  if (error.code === 'CIRCUIT_OPEN') {
    console.log('Service temporarily unavailable');
  }
}
```

### Retry Strategy

Exponential backoff with jitter for transient failures:

```javascript
const client = new SidecarClient({
  maxRetries: 3,           // Retry up to 3 times
  retryableErrors: [       // Which errors to retry
    'UNAVAILABLE',
    'DEADLINE_EXCEEDED',
    'RESOURCE_EXHAUSTED'
  ]
});

// Retries happen automatically
const result = await client.applyTransaction(request);
```

### Connection Pooling

Efficient resource management:

```javascript
const client = new SidecarClient({
  connectionPool: {
    minConnections: 2,   // Keep at least 2 connections
    maxConnections: 10,  // Scale up to 10 connections
    idleTimeout: 60000   // Close idle connections after 1 minute
  }
});
```

### Health Monitoring

Continuous health checks:

```javascript
const client = new SidecarClient({
  enableHealthCheck: true
});

client.on('healthStatusChanged', ({ from, to }) => {
  console.log(`Health status changed: ${from} -> ${to}`);

  if (to === 'UNHEALTHY') {
    // Alert or failover logic
  }
});
```

## Observability

### OpenTelemetry Integration

```javascript
import { SidecarClient } from 'unrdf/sidecar';
import { createObservabilityManager } from 'unrdf/knowledge-engine';

const observability = createObservabilityManager({
  serviceName: 'my-app',
  enableTracing: true,
  enableMetrics: true
});

const client = new SidecarClient({
  observability
});

// All requests are automatically traced
await client.applyTransaction(request);
```

### Metrics Collection

```javascript
// Get client metrics
const metrics = client.getClientMetrics();

console.log('Request metrics:', {
  total: metrics.requests,
  success: metrics.successes,
  failure: metrics.failures,
  retries: metrics.retries
});

console.log('Circuit breaker:', {
  state: metrics.circuitBreaker.state,
  failures: metrics.circuitBreaker.failures,
  errorRate: metrics.circuitBreaker.errorRate
});

console.log('Connection pool:', {
  total: metrics.connectionPool.total,
  active: metrics.connectionPool.active,
  idle: metrics.connectionPool.idle
});
```

## Error Handling

### Error Codes

- `UNAVAILABLE`: Service temporarily unavailable
- `DEADLINE_EXCEEDED`: Request timeout
- `UNAUTHENTICATED`: Invalid credentials
- `PERMISSION_DENIED`: Insufficient permissions
- `INVALID_ARGUMENT`: Invalid request parameters
- `NOT_FOUND`: Resource not found
- `ALREADY_EXISTS`: Resource already exists
- `RESOURCE_EXHAUSTED`: Rate limit exceeded
- `INTERNAL`: Internal server error

### Best Practices

```javascript
try {
  const result = await client.applyTransaction(request);
  // Handle success
} catch (error) {
  switch (error.code) {
    case 'UNAVAILABLE':
      // Service down - retry later
      console.log('Service unavailable, will retry');
      break;

    case 'DEADLINE_EXCEEDED':
      // Request timeout - check network
      console.log('Request timeout');
      break;

    case 'INVALID_ARGUMENT':
      // Bad request - fix input
      console.error('Invalid request:', error.message);
      break;

    case 'PERMISSION_DENIED':
      // Authorization issue
      console.error('Permission denied');
      break;

    default:
      // Unexpected error
      console.error('Unexpected error:', error);
  }
}
```

## Performance Optimization

### Batch Operations

```javascript
// Use connection pooling for concurrent requests
const promises = transactions.map(tx =>
  client.applyTransaction(tx)
);

const results = await Promise.all(promises);
```

### Timeout Tuning

```javascript
// Adjust timeouts based on operation complexity
const client = new SidecarClient({
  timeout: 10000 // 10 seconds for complex operations
});

// Or per-request
await client.applyTransaction(request, {
  timeout: 5000 // 5 seconds for this request
});
```

## Kubernetes Deployment

### Sidecar Pattern

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: my-app
spec:
  template:
    spec:
      containers:
      - name: app
        image: my-app:latest
        env:
        - name: KGC_SIDECAR_ADDRESS
          value: "localhost:50051"

      - name: kgc-sidecar
        image: kgc-sidecar:latest
        ports:
        - containerPort: 50051
          name: grpc
```

### Service Mesh

```javascript
// In service mesh, use service DNS
const client = await SidecarClient.connect('kgc-sidecar:50051');
```

## Troubleshooting

### Connection Issues

```javascript
// Enable debug logging
const client = new SidecarClient({
  observability: {
    enableLogging: true,
    logLevel: 'debug'
  }
});

// Check connectivity
const health = await client.healthCheck();
console.log('Service status:', health.status);
```

### Performance Issues

```javascript
// Check client metrics
const metrics = client.getClientMetrics();

if (metrics.connectionPool.waitQueueSize > 0) {
  console.log('Connection pool saturated, increase maxConnections');
}

if (metrics.circuitBreaker.errorRate > 50) {
  console.log('High error rate, check service health');
}
```

## Examples

See `/examples/sidecar-client-example.mjs` for complete examples.

## License

MIT
