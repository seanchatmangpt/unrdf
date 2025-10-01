# Cleanroom Testcontainer Architecture

**Isolated, reproducible integration testing with full OpenTelemetry observability**

## Overview

The cleanroom testcontainer stack provides a complete, isolated environment for validating the CLI v2 → KGC Sidecar integration with distributed tracing, metrics, and logs.

**Design Principle**: 80/20 Focus
- 20% of test scenarios validate 80% of critical functionality
- Complete observability with minimal configuration
- Startup time < 30 seconds

## Quick Start

```bash
# Start cleanroom environment
cd test/e2e/cleanroom
docker-compose up -d

# Wait for services (< 30 seconds)
docker-compose ps

# View Jaeger UI
open http://localhost:16686

# Run integration tests
npm test test/e2e/cleanroom/example-integration-test.mjs

# Cleanup
docker-compose down -v
```

## Architecture

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

## Services

| Service | Port | Purpose | Health Check |
|---------|------|---------|--------------|
| OTEL Collector | 4317 (gRPC), 4318 (HTTP) | Telemetry hub | http://localhost:13133/ |
| Jaeger | 16686 (UI), 14250 (collector) | Trace visualization | http://localhost:14269/ |
| KGC Sidecar | 50051 (gRPC), 9464 (metrics) | Knowledge graph operations | gRPC health probe |
| PostgreSQL | 5432 | RDF triple store | pg_isready |

## Files

### Core Infrastructure
- **docker-compose.yml** - Container orchestration
- **otel-collector-config.yaml** - OpenTelemetry configuration
- **sidecar.Dockerfile** - KGC sidecar image with OTEL
- **init-postgres.sql** - PostgreSQL schema initialization

### Programmatic API
- **testcontainer-stack.mjs** - Testcontainers programmatic setup
- **setup-cleanroom.mjs** - High-level environment manager
- **health-checks.mjs** - Service health validation
- **network-config.mjs** - Network and DNS utilities
- **otel-instrumentation.mjs** - OTEL SDK initialization

### Documentation & Examples
- **architecture.md** - Complete architecture documentation
- **example-integration-test.mjs** - Example test suite
- **README.md** - This file

## Usage

### Docker Compose

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f sidecar

# Check health
docker-compose ps

# Stop and cleanup
docker-compose down -v
```

### Programmatic API

```javascript
import { CleanroomEnvironment } from './setup-cleanroom.mjs';

const env = new CleanroomEnvironment();

// Start environment
await env.initialize();

// Get endpoints
const endpoints = env.getEndpoints();
console.log('Sidecar:', endpoints.sidecarGrpc);
console.log('Jaeger UI:', endpoints.jaegerUi);

// Execute test scenario
const result = await env.executeScenario('my-test', async ({ endpoints }) => {
  // Your test logic here
  return { success: true };
});

// Query traces
const traces = await env.queryTraces({ service: 'kgc-sidecar' });

// Cleanup
await env.shutdown();
```

### Vitest Tests

```javascript
import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { CleanroomEnvironment } from './setup-cleanroom.mjs';

describe('My Integration Test', () => {
  let cleanroom;

  beforeAll(async () => {
    cleanroom = new CleanroomEnvironment();
    await cleanroom.initialize();
  }, 60000);

  afterAll(async () => {
    await cleanroom.shutdown();
  });

  it('should work', async () => {
    const health = await cleanroom.checkHealth();
    expect(health.allHealthy).toBe(true);
  });
});
```

## OpenTelemetry Instrumentation

### Automatic Instrumentation

The KGC Sidecar is automatically instrumented with:
- gRPC calls (client and server)
- HTTP requests
- Database queries
- Custom knowledge engine spans

### Trace Context Propagation

CLI → Sidecar trace context flows via gRPC metadata:

```javascript
// CLI side
const metadata = new grpc.Metadata();
metadata.set('traceparent', '00-{trace-id}-{span-id}-01');

// Sidecar side
const traceparent = call.metadata.get('traceparent')[0];
// Continue trace...
```

### Viewing Traces

1. Open Jaeger UI: http://localhost:16686
2. Select service: `kgc-sidecar`
3. Find traces
4. View complete request chain

## Performance Targets

- **Startup**: < 30 seconds for full stack
- **Latency**: < 100ms for simple hook evaluations
- **Throughput**: > 100 operations/second
- **Cleanup**: < 10 seconds

## Troubleshooting

### Services Not Starting

```bash
# Check Docker
docker ps

# View logs
docker-compose logs

# Restart specific service
docker-compose restart sidecar
```

### Slow Startup

```bash
# Pre-pull images
docker-compose pull

# Check Docker resources
docker stats
```

### Traces Not Appearing

```bash
# Check OTEL Collector logs
docker-compose logs otel-collector

# Verify Jaeger is receiving traces
curl http://localhost:16686/api/services

# Check sidecar OTEL config
docker-compose exec sidecar env | grep OTEL
```

## Development

### Rebuild Sidecar

```bash
docker-compose build sidecar
docker-compose up -d sidecar
```

### Update OTEL Config

```bash
# Edit otel-collector-config.yaml
# Restart collector
docker-compose restart otel-collector
```

### Add Test Data

```bash
# Edit init-postgres.sql
# Recreate database
docker-compose down -v
docker-compose up -d postgres
```

## CI/CD Integration

```yaml
# .github/workflows/cleanroom-tests.yml
name: Cleanroom Integration Tests

on: [push, pull_request]

jobs:
  integration:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '20'

      - name: Start cleanroom
        run: docker-compose -f test/e2e/cleanroom/docker-compose.yml up -d

      - name: Wait for services
        run: npm run cleanroom:wait

      - name: Run tests
        run: npm test test/e2e/cleanroom/

      - name: Cleanup
        if: always()
        run: docker-compose -f test/e2e/cleanroom/docker-compose.yml down -v
```

## References

- [Architecture Documentation](./architecture.md)
- [OpenTelemetry Specification](https://opentelemetry.io/docs/specs/otel/)
- [Jaeger Documentation](https://www.jaegertracing.io/docs/)
- [Testcontainers](https://testcontainers.com/)
