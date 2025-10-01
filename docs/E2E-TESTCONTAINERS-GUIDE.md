# E2E Testing with Testcontainers — Complete Guide
## UNRDF Knowledge Graph Control JS Sidecar

**Version**: 1.0
**Last Updated**: 2025-09-30

---

## Overview

This guide documents the comprehensive E2E testing infrastructure for the UNRDF KGC sidecar using Testcontainers. The infrastructure spins up real services (PostgreSQL, Redis, Jaeger, Prometheus, etc.) in Docker containers for integration and E2E testing.

---

## Architecture

### Test Infrastructure Stack

```
┌─────────────────────────────────────────────────────────┐
│                 KGC Sidecar Test Suite                  │
│                   (Vitest + Node.js)                     │
└───────────────────┬─────────────────────────────────────┘
                    │
        ┌───────────┴───────────┐
        │  Testcontainers API   │
        │   (Test Harness)      │
        └───────────┬───────────┘
                    │
    ┌───────────────┼───────────────┐
    │   Docker Network (Bridge)     │
    │   kgc-test-network            │
    ├────────────────────────────────┤
    │  ┌──────────┐   ┌───────────┐ │
    │  │PostgreSQL│   │   Redis   │ │
    │  │  :5432   │   │  :6379    │ │
    │  └──────────┘   └───────────┘ │
    │                                │
    │  ┌──────────┐   ┌───────────┐ │
    │  │  Jaeger  │   │Prometheus │ │
    │  │:14268/..│   │  :9090    │ │
    │  └──────────┘   └───────────┘ │
    │                                │
    │  ┌──────────┐   ┌───────────┐ │
    │  │ Grafana  │   │  MinIO    │ │
    │  │  :3000   │   │:9000/9001 │ │
    │  └──────────┘   └───────────┘ │
    │                                │
    │  ┌──────────┐   ┌───────────┐ │
    │  │ElasticSrch│   │  Kibana  │ │
    │  │  :9200   │   │  :5601    │ │
    │  └──────────┘   └───────────┘ │
    └────────────────────────────────┘
```

---

## Services Configuration

### Core Services

#### 1. PostgreSQL
- **Image**: `postgres:15-alpine`
- **Port**: 5432
- **Database**: `kgc_test`
- **Credentials**: `test` / `test`
- **Use Case**: Persistent storage for policy packs, lockchain metadata
- **Health Check**: `pg_isready`

#### 2. Redis
- **Image**: `redis:7-alpine`
- **Port**: 6379
- **Config**: Append-only mode, 256MB max memory
- **Use Case**: Hook execution caching, session storage
- **Health Check**: `redis-cli ping`

### Observability Services

#### 3. Jaeger
- **Image**: `jaegertracing/all-in-one:latest`
- **Ports**:
  - 14268 (Jaeger Collector)
  - 16686 (Jaeger UI)
  - 14250 (gRPC)
- **Use Case**: Distributed tracing for hook execution
- **Features**: OTLP enabled, Zipkin compatibility

#### 4. Prometheus
- **Image**: `prom/prometheus:latest`
- **Port**: 9090
- **Use Case**: Metrics collection (hook latency, error rates, throughput)
- **Scrape Interval**: 15s

#### 5. Grafana
- **Image**: `grafana/grafana:latest`
- **Port**: 3000
- **Credentials**: `admin` / `admin`
- **Use Case**: Dashboards for observability metrics
- **Pre-configured Datasources**: Prometheus, Jaeger

### Storage & Logging Services

#### 6. MinIO
- **Image**: `minio/minio:latest`
- **Ports**:
  - 9000 (API)
  - 9001 (Console)
- **Credentials**: `minioadmin` / `minioadmin`
- **Use Case**: S3-compatible storage for policy packs, receipts
- **Buckets**: `policy-packs`, `lockchain-receipts`

#### 7. Elasticsearch
- **Image**: `docker.elastic.co/elasticsearch/elasticsearch:8.11.0`
- **Port**: 9200
- **Config**: Single-node, security disabled (test only)
- **Use Case**: Structured logging, audit trail indexing
- **Heap**: 512MB

#### 8. Kibana
- **Image**: `docker.elastic.co/kibana/kibana:8.11.0`
- **Port**: 5601
- **Use Case**: Log visualization and analysis
- **Connected to**: Elasticsearch

---

## Quick Start

### Prerequisites

```bash
# Install dependencies
pnpm install

# Ensure Docker is running
docker info

# Ensure testcontainers module is available
npm list testcontainers
```

### Start All Services

```bash
# Start full observability stack
pnpm run testcontainers:start

# Or use the API directly
node -e "
import { testcontainersManager } from './test/e2e/testcontainers-setup.mjs';
await testcontainersManager.startAll();
console.log('Services ready:', testcontainersManager.getConnectionInfo());
"
```

### Start Minimal Services (Fast Tests)

```bash
# Start only PostgreSQL, Redis, Jaeger
node -e "
import { testcontainersManager } from './test/e2e/testcontainers-setup.mjs';
await testcontainersManager.startMinimal();
"
```

### Stop All Services

```bash
pnpm run testcontainers:stop

# Or cleanup with data deletion
node -e "
import { testcontainersManager } from './test/e2e/testcontainers-setup.mjs';
await testcontainersManager.cleanup();
"
```

---

## API Reference

### TestcontainersManager Class

```javascript
import { TestcontainersManager } from './test/e2e/testcontainers-setup.mjs';

const manager = new TestcontainersManager();

// Initialize network
await manager.initializeNetwork();

// Start individual services
await manager.startPostgreSQL();
await manager.startRedis();
await manager.startJaeger();

// Start all services
await manager.startAll();

// Start minimal set (faster)
await manager.startMinimal();

// Get container info
const postgres = manager.getContainer('postgres');
const connectionInfo = manager.getConnectionInfo();
const envVars = manager.getEnvironmentVariables();

// Stop all
await manager.stopAll();

// Cleanup (stop + delete data)
await manager.cleanup();
```

### Connection Info

```javascript
const info = manager.getConnectionInfo();
// Returns:
{
  postgres: {
    host: 'localhost',
    ports: { 5432: 54321 }, // mapped port
    networkAliases: ['postgres']
  },
  redis: {
    host: 'localhost',
    ports: { 6379: 63791 },
    networkAliases: ['redis']
  },
  jaeger: {
    host: 'localhost',
    ports: { 14268: 14269, 16686: 16687, 14250: 14251 },
    networkAliases: ['jaeger']
  },
  // ... other services
}
```

### Environment Variables

```javascript
const env = manager.getEnvironmentVariables();
// Returns:
{
  DATABASE_URL: 'postgresql://test:test@postgres:5432/kgc_test',
  POSTGRES_HOST: 'postgres',
  POSTGRES_PORT: '5432',
  REDIS_URL: 'redis://redis:6379',
  REDIS_HOST: 'redis',
  JAEGER_ENDPOINT: 'http://jaeger:14268/api/traces',
  PROMETHEUS_URL: 'http://prometheus:9090',
  GRAFANA_URL: 'http://grafana:3000',
  MINIO_URL: 'http://minio:9000',
  ELASTICSEARCH_URL: 'http://elasticsearch:9200',
  KIBANA_URL: 'http://kibana:5601',
  // ... more
}
```

---

## Usage in Tests

### Example E2E Test

```javascript
import { describe, it, beforeAll, afterAll, expect } from 'vitest';
import { testcontainersManager } from './testcontainers-setup.mjs';
import { TransactionManager } from '../../src/knowledge-engine/transaction.mjs';

describe('E2E: Transaction with Lockchain', () => {
  let manager;
  let connectionInfo;
  let env;

  beforeAll(async () => {
    // Start minimal services (faster)
    await testcontainersManager.startMinimal();

    connectionInfo = testcontainersManager.getConnectionInfo();
    env = testcontainersManager.getEnvironmentVariables();

    console.log('PostgreSQL:', connectionInfo.postgres);
    console.log('Redis:', connectionInfo.redis);
  }, 120000); // 2 minute timeout for container startup

  afterAll(async () => {
    await testcontainersManager.stopAll();
  });

  it('should persist transaction receipt to PostgreSQL', async () => {
    const tm = new TransactionManager({
      databaseUrl: env.DATABASE_URL,
      redisUrl: env.REDIS_URL
    });

    const store = new Store();
    const delta = {
      add: [quad(namedNode('ex:s'), namedNode('ex:p'), literal('value'))],
      remove: []
    };

    const receipt = await tm.apply(store, delta);

    expect(receipt).toBeDefined();
    expect(receipt.id).toBeTruthy();
    expect(receipt.lockchainId).toBeTruthy();

    // Verify in database
    const result = await queryPostgreSQL(
      env.DATABASE_URL,
      'SELECT * FROM receipts WHERE id = $1',
      [receipt.id]
    );

    expect(result.rows).toHaveLength(1);
  });

  it('should export metrics to Prometheus', async () => {
    // Execute transactions
    // ...

    // Query Prometheus
    const response = await fetch(
      `${env.PROMETHEUS_URL}/api/v1/query?query=transaction_duration_seconds`
    );
    const metrics = await response.json();

    expect(metrics.status).toBe('success');
    expect(metrics.data.result).toHaveLength(greaterThan(0));
  });

  it('should send traces to Jaeger', async () => {
    // Execute hooks with tracing
    // ...

    // Query Jaeger
    const response = await fetch(
      `${env.JAEGER_ENDPOINT.replace('/api/traces', '/api/traces?service=unrdf-kgc')}`
    );
    const traces = await response.json();

    expect(traces.data).toBeDefined();
    expect(traces.data.length).toBeGreaterThan(0);
  });
});
```

### Example Integration Test with All Services

```javascript
import { describe, it, beforeAll, afterAll, expect } from 'vitest';
import { testcontainersManager } from './testcontainers-setup.mjs';

describe('Full Stack Integration', () => {
  beforeAll(async () => {
    // Start all services (observability + storage + logging)
    await testcontainersManager.startAll();
  }, 300000); // 5 minute timeout

  afterAll(async () => {
    await testcontainersManager.cleanup();
  });

  it('should have all services healthy', async () => {
    const info = testcontainersManager.getConnectionInfo();

    // Check PostgreSQL
    const pgHealth = await checkPostgresHealth(info.postgres);
    expect(pgHealth).toBe(true);

    // Check Redis
    const redisHealth = await checkRedisHealth(info.redis);
    expect(redisHealth).toBe(true);

    // Check Jaeger
    const jaegerHealth = await checkJaegerHealth(info.jaeger);
    expect(jaegerHealth).toBe(true);

    // Check Prometheus
    const prometheusHealth = await checkPrometheusHealth(info.prometheus);
    expect(prometheusHealth).toBe(true);

    // Check all other services...
  });

  it('should demonstrate full observability pipeline', async () => {
    const env = testcontainersManager.getEnvironmentVariables();

    // 1. Execute transaction with hooks
    // 2. Transaction writes to PostgreSQL
    // 3. Metrics exported to Prometheus
    // 4. Traces sent to Jaeger
    // 5. Logs indexed in Elasticsearch
    // 6. Policy pack stored in MinIO

    // Verify each step...
  });
});
```

---

## Test Scenarios

### 1. Database Persistence

```javascript
it('should persist lockchain to PostgreSQL', async () => {
  const writer = new LockchainWriter({
    databaseUrl: env.DATABASE_URL
  });

  const receipt = createTestReceipt();
  const entryId = await writer.writeReceipt(receipt);

  // Query from database
  const entry = await queryLockchainEntry(entryId);
  expect(entry.receiptHash).toBe(receipt.canonicalHash);
  expect(entry.previousHash).toBeDefined();
});
```

### 2. Cache Performance

```javascript
it('should cache hook results in Redis', async () => {
  const executor = new HookExecutor({
    redisUrl: env.REDIS_URL,
    enableCache: true
  });

  // First execution (cache miss)
  const start1 = Date.now();
  const result1 = await executor.execute(hook, context);
  const duration1 = Date.now() - start1;

  // Second execution (cache hit)
  const start2 = Date.now();
  const result2 = await executor.execute(hook, context);
  const duration2 = Date.now() - start2;

  expect(result1).toEqual(result2);
  expect(duration2).toBeLessThan(duration1 * 0.1); // 10x faster
});
```

### 3. Distributed Tracing

```javascript
it('should trace multi-hop hook execution', async () => {
  const tracer = createTracer({ endpoint: env.JAEGER_ENDPOINT });

  // Execute nested hooks
  await tracer.startActiveSpan('transaction', async (span) => {
    await executeHook1();
    await executeHook2();
    await executeHook3();
    span.end();
  });

  // Query traces
  const traces = await fetchJaegerTraces('transaction');
  expect(traces).toHaveLength(1);
  expect(traces[0].spans).toHaveLength(4); // parent + 3 hooks
});
```

### 4. Metrics Collection

```javascript
it('should export RED metrics to Prometheus', async () => {
  // Execute transactions
  for (let i = 0; i < 100; i++) {
    await tm.apply(store, delta);
  }

  await new Promise(resolve => setTimeout(resolve, 30000)); // Wait for scrape

  // Query Prometheus
  const rate = await queryPrometheus('rate(transaction_total[1m])');
  const errors = await queryPrometheus('transaction_errors_total');
  const duration = await queryPrometheus('histogram_quantile(0.99, transaction_duration_seconds)');

  expect(rate).toBeGreaterThan(0);
  expect(errors).toBe(0);
  expect(duration).toBeLessThan(0.1); // <100ms p99
});
```

### 5. Policy Pack Storage

```javascript
it('should store policy packs in MinIO', async () => {
  const s3Client = createS3Client({
    endpoint: env.MINIO_URL,
    accessKey: env.MINIO_ACCESS_KEY,
    secretKey: env.MINIO_SECRET_KEY
  });

  const policyPack = createTestPolicyPack();
  await uploadPolicyPack(s3Client, 'policy-packs', policyPack);

  // Retrieve
  const retrieved = await downloadPolicyPack(s3Client, 'policy-packs', policyPack.id);
  expect(retrieved).toEqual(policyPack);
});
```

### 6. Log Aggregation

```javascript
it('should index logs in Elasticsearch', async () => {
  const logger = createLogger({
    elasticsearchUrl: env.ELASTICSEARCH_URL
  });

  // Generate logs
  logger.info('Transaction started', { txId: 'tx-123' });
  logger.warn('Hook timeout', { hookId: 'hook-456' });
  logger.error('Validation failed', { error: 'SHACL violation' });

  await new Promise(resolve => setTimeout(resolve, 5000)); // Wait for indexing

  // Query Elasticsearch
  const results = await searchElasticsearch({
    index: 'kgc-logs-*',
    query: { match: { txId: 'tx-123' } }
  });

  expect(results.hits.total.value).toBeGreaterThan(0);
});
```

---

## Performance Benchmarks

### Startup Times (M1 Mac, 16GB RAM)

| Configuration | Startup Time | Memory |
|---------------|--------------|--------|
| Minimal (3 services) | 15-20s | ~500MB |
| Core (5 services) | 30-40s | ~1GB |
| Full (8 services) | 60-90s | ~2GB |

### Service Resource Usage

| Service | CPU | Memory | Disk |
|---------|-----|--------|------|
| PostgreSQL | 5% | 50MB | 100MB |
| Redis | 2% | 30MB | 50MB |
| Jaeger | 3% | 80MB | 20MB |
| Prometheus | 5% | 100MB | 50MB |
| Grafana | 3% | 60MB | 30MB |
| MinIO | 2% | 40MB | 100MB |
| Elasticsearch | 15% | 512MB | 200MB |
| Kibana | 5% | 150MB | 50MB |

---

## Troubleshooting

### Container Fails to Start

```bash
# Check Docker daemon
docker info

# Check available resources
docker system df

# Clean up old containers
docker system prune -a

# Check network conflicts
docker network ls
docker network rm kgc-test-network
```

### Port Conflicts

```bash
# Check which ports are in use
lsof -i :5432  # PostgreSQL
lsof -i :6379  # Redis
lsof -i :9090  # Prometheus

# Kill processes or change mapped ports in config
```

### Slow Startup

```bash
# Pre-pull images
docker pull postgres:15-alpine
docker pull redis:7-alpine
docker pull jaegertracing/all-in-one:latest
docker pull prom/prometheus:latest
docker pull grafana/grafana:latest
docker pull minio/minio:latest
docker pull docker.elastic.co/elasticsearch/elasticsearch:8.11.0
docker pull docker.elastic.co/kibana/kibana:8.11.0

# Use minimal configuration for faster tests
await manager.startMinimal();
```

### Memory Issues

```bash
# Increase Docker memory limit
# Docker Desktop → Preferences → Resources → Memory: 4GB+

# Or use minimal configuration
await manager.startMinimal();
```

### Network Issues

```bash
# Inspect network
docker network inspect kgc-test-network

# Check container connectivity
docker exec <container-id> ping postgres
docker exec <container-id> ping redis

# Restart network
docker network rm kgc-test-network
docker network create kgc-test-network
```

---

## Best Practices

### 1. Use Minimal Config for Unit/Integration Tests

```javascript
// Fast tests - only essential services
beforeAll(async () => {
  await testcontainersManager.startMinimal();
}, 30000); // 30s timeout
```

### 2. Use Full Config for E2E/Acceptance Tests

```javascript
// Comprehensive tests - all services
beforeAll(async () => {
  await testcontainersManager.startAll();
}, 300000); // 5min timeout
```

### 3. Always Cleanup After Tests

```javascript
afterAll(async () => {
  // Stop containers
  await testcontainersManager.stopAll();

  // Or cleanup with data deletion
  await testcontainersManager.cleanup();
});
```

### 4. Use Test Fixtures

```javascript
// Create reusable test data
const fixtures = {
  stores: createTestStores(),
  hooks: createTestHooks(),
  policyPacks: createTestPolicyPacks()
};

// Setup fixtures before tests
beforeEach(async () => {
  await loadFixtures(manager, fixtures);
});
```

### 5. Parallel Test Execution

```javascript
// vitest.config.mjs
export default {
  test: {
    poolOptions: {
      threads: { maxThreads: 4 } // Run tests in parallel
    }
  }
};

// But serialize testcontainer setup
let globalManager;
beforeAll(async () => {
  if (!globalManager) {
    globalManager = testcontainersManager;
    await globalManager.startAll();
  }
});
```

---

## CI/CD Integration

### GitHub Actions

```yaml
name: E2E Tests

on: [push, pull_request]

jobs:
  e2e:
    runs-on: ubuntu-latest

    services:
      docker:
        image: docker:dind
        options: --privileged

    steps:
      - uses: actions/checkout@v3

      - uses: actions/setup-node@v3
        with:
          node-version: '18'

      - name: Install dependencies
        run: pnpm install

      - name: Pull Docker images (cache)
        run: |
          docker pull postgres:15-alpine
          docker pull redis:7-alpine
          docker pull jaegertracing/all-in-one:latest

      - name: Run E2E tests
        run: pnpm run test:e2e
        env:
          CI: true

      - name: Upload test results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: test-results
          path: test-results/
```

---

## Future Enhancements

### Planned Features

1. **K8s Testcontainers** - Test deployment to Kubernetes cluster
2. **Terraform Integration** - Test infrastructure provisioning
3. **Chaos Engineering** - Network partitions, service failures
4. **Performance Testing** - Load testing with k6
5. **Browser E2E** - Playwright/Puppeteer integration
6. **Multi-Region** - Test distributed deployment
7. **RBAC Testing** - Authentication/authorization scenarios
8. **Dark Launch** - Feature flag testing

---

## References

- [Testcontainers Documentation](https://testcontainers.com/)
- [Testcontainers Node](https://github.com/testcontainers/testcontainers-node)
- [Vitest Documentation](https://vitest.dev/)
- [UNRDF Test Strategy](./test-strategy-browser-migration.md)

---

**Maintained By**: UNRDF Team
**Last Updated**: 2025-09-30
**Version**: 1.0
