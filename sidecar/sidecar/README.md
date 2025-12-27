# KGC Sidecar gRPC Client

Enterprise-grade gRPC client for Knowledge Graph Conformance sidecar with full resilience patterns and observability.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Sidecar Client                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │   Circuit    │  │    Retry     │  │  Connection  │     │
│  │   Breaker    │  │   Strategy   │  │     Pool     │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │    Health    │  │  Interceptors│  │  Telemetry   │     │
│  │   Monitor    │  │   (OTEL)     │  │   (OTEL)     │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
│                                                             │
│  ┌────────────────────────────────────────────────────┐    │
│  │             gRPC Channel (@grpc/grpc-js)           │    │
│  └────────────────────────────────────────────────────┘    │
│                           ↓                                 │
└─────────────────────────────────────────────────────────────┘
                            ↓
          ┌──────────────────────────────────┐
          │   KGC Sidecar Service (gRPC)     │
          └──────────────────────────────────┘
```

## Features

### Core Capabilities

- **gRPC Communication**: Protocol Buffers-based RPC with streaming support
- **Transaction Management**: Atomic transactions with cryptographic receipts
- **Graph Validation**: Policy-based validation with SHACL/SPARQL
- **Hook Evaluation**: Knowledge hook execution with sandbox isolation
- **Policy Queries**: Policy pack management and querying

### Resilience Patterns

- **Circuit Breaker**: Automatic failure detection and recovery
  - States: CLOSED → OPEN → HALF_OPEN → CLOSED
  - Configurable thresholds and reset timeouts
  - Error rate tracking and metrics

- **Retry Strategy**: Exponential backoff with jitter
  - Configurable retry counts and delays
  - Retryable error detection
  - Backoff multiplier with max delay cap

- **Connection Pooling**: Efficient resource management
  - Min/max connection limits
  - Idle connection eviction
  - Health-based connection removal
  - Automatic scaling under load

- **Health Monitoring**: Continuous service health checks
  - Liveness and readiness probes
  - Startup grace period
  - Status change notifications
  - Consecutive failure tracking

### Observability

- **OpenTelemetry Integration**
  - Distributed tracing with spans
  - Metrics collection (counters, histograms, gauges)
  - Context propagation
  - Request/response logging

- **Metrics Tracking**
  - Request latency (p50, p95, p99)
  - Error rates and counts
  - Circuit breaker state
  - Connection pool statistics
  - Retry attempts

## File Structure

```
src/sidecar/
├── client.mjs              # Main gRPC client
├── config.mjs              # Multi-environment configuration
├── circuit-breaker.mjs     # Circuit breaker implementation
├── retry-strategy.mjs      # Retry logic with backoff
├── connection-pool.mjs     # gRPC connection pooling
├── health-check.mjs        # Health monitoring system
├── interceptors.mjs        # gRPC interceptors
├── telemetry.mjs           # OpenTelemetry integration
└── index.mjs               # Module exports

proto/
└── kgc-sidecar.proto       # gRPC service definition

test/sidecar/
├── circuit-breaker.test.mjs    # Circuit breaker tests
├── retry-strategy.test.mjs     # Retry strategy tests
├── connection-pool.test.mjs    # Connection pool tests
├── client.test.mjs             # Client unit tests
└── integration.test.mjs        # End-to-end tests
```

## Quick Start

### Installation

```bash
npm install @grpc/grpc-js @grpc/proto-loader
```

### Basic Usage

```javascript
import { SidecarClient } from './src/sidecar/client.mjs';

// Create and connect
const client = await SidecarClient.connect('localhost:50051');

// Apply transaction
const result = await client.applyTransaction({
  delta: {
    additions: [/* quads */],
    deletions: []
  },
  actor: 'user@example.com'
});

console.log('Transaction committed:', result.receipt.committed);

// Disconnect
await client.disconnect();
```

## Configuration

### Environment Variables

```bash
KGC_SIDECAR_ADDRESS=localhost:50051
KGC_SIDECAR_CONTEXT=production
KGC_SIDECAR_TLS_ENABLED=true
KGC_SIDECAR_TLS_CA=/path/to/ca.crt
```

### Configuration File (~/.kgc/config.json)

```json
{
  "currentContext": "production",
  "contexts": [{
    "name": "production",
    "endpoint": {
      "address": "kgc-sidecar.example.com",
      "port": 443,
      "tls": { "enabled": true }
    },
    "timeout": 10000,
    "maxRetries": 5
  }]
}
```

## Performance Characteristics

### Latency Targets

- **Health Check**: < 10ms (p99)
- **Transaction Apply**: < 50ms (p99)
- **Graph Validation**: < 100ms (p99)
- **Hook Evaluation**: < 200ms (p99)

### Throughput

- **Concurrent Requests**: 1000+ RPS
- **Connection Pool**: 2-10 connections
- **Retry Budget**: 3 retries with exponential backoff

### Resource Usage

- **Memory**: ~10MB baseline, scales with connection pool
- **CPU**: Minimal overhead (<1% for client operations)
- **Network**: Efficient Protocol Buffers encoding

## Testing

### Run Tests

```bash
# All sidecar tests
npm test -- test/sidecar/

# Specific test file
npm test -- test/sidecar/circuit-breaker.test.mjs

# Integration tests (requires running sidecar)
RUN_INTEGRATION_TESTS=true npm test -- test/sidecar/integration.test.mjs
```

### Test Coverage

- Circuit Breaker: 95%+ coverage
- Retry Strategy: 95%+ coverage
- Connection Pool: 90%+ coverage
- Client Integration: 85%+ coverage

## Production Deployment

### Kubernetes Sidecar Pattern

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: app-with-kgc-sidecar
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

### Service Mesh Integration

Works with Istio, Linkerd, and other service meshes for:
- mTLS encryption
- Traffic management
- Observability
- Policy enforcement

## Troubleshooting

### Connection Issues

```javascript
// Enable debug logging
const client = new SidecarClient({
  observability: { enableLogging: true, logLevel: 'debug' }
});

// Check health
const health = await client.healthCheck();
console.log('Status:', health.status);
```

### Performance Issues

```javascript
// Check metrics
const metrics = client.getClientMetrics();
console.log('Error rate:', metrics.circuitBreaker.errorRate);
console.log('Pool saturation:', metrics.connectionPool.waitQueueSize);
```

### Circuit Breaker Tripping

```javascript
// Monitor circuit state
client.circuitBreaker.on('stateChange', ({ from, to }) => {
  console.log(`Circuit ${from} → ${to}`);
});

// Force reset if needed
client.circuitBreaker.forceClosed();
```

## API Documentation

See [Sidecar Client Usage Guide](../../docs/sidecar-client-usage.md) for complete API reference.

## Contributing

Please ensure:
- All tests pass (`npm test -- test/sidecar/`)
- Code coverage > 90%
- JSDoc documentation for all public APIs
- Integration tests for new features

## License

MIT
