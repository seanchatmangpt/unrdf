# Testcontainer Tests - Quick Start Guide

## Overview

This directory contains comprehensive E2E tests using Testcontainers to validate the KGC JS sidecar with real services (PostgreSQL, Redis, Jaeger).

## Prerequisites

1. **Docker must be running**
   ```bash
   docker info
   ```

2. **Dependencies installed**
   ```bash
   pnpm install
   ```

3. **System Resources**
   - RAM: 2GB+ available
   - Disk: 1GB+ available
   - CPU: 2+ cores

## Quick Start

### Run All E2E Tests

```bash
npm run test:e2e
```

### Run Specific Test Suite

```bash
# KGC Sidecar testcontainer tests
npm run test:e2e -- test/e2e/kgc-sidecar-testcontainer.test.mjs

# Simple testcontainer validation
npm run test:e2e -- test/e2e/simple-testcontainer.test.mjs

# Redis integration tests
npm run test:e2e -- test/e2e/redis-testcontainer.test.mjs
```

### Run with Verbose Output

```bash
npx vitest test/e2e/kgc-sidecar-testcontainer.test.mjs --reporter=verbose
```

## Test Files

### Main Test Suites

| File | Purpose | Services | Tests |
|------|---------|----------|-------|
| `kgc-sidecar-testcontainer.test.mjs` | KGC sidecar integration | PostgreSQL, Redis, Jaeger | 15 |
| `simple-testcontainer.test.mjs` | Testcontainer validation | Nginx | 2 |
| `redis-testcontainer.test.mjs` | Redis operations | Redis | 4 |

### Infrastructure

| File | Purpose |
|------|---------|
| `testcontainers-setup.mjs` | Container management and configuration |
| `testcontainer-setup.mjs` | E2E test environment setup |

## Test Coverage

### 1. PostgreSQL Lockchain Persistence (3 tests)
- ✅ Receipt persistence with JSONB data
- ✅ Sequential chain integrity validation
- ✅ Unique constraint enforcement

### 2. Redis Hook Caching (4 tests)
- ✅ Cache hit/miss behavior
- ✅ Performance improvement validation (10x+ speedup)
- ✅ TTL expiration handling
- ✅ Metadata storage (hash operations)

### 3. Jaeger Distributed Tracing (2 tests)
- ✅ Infrastructure accessibility
- ✅ Trace context preparation

### 4. Full-Stack Workflow (2 tests)
- ✅ Complete transaction workflow
- ✅ High-throughput batch processing (50 tx/sec)

### 5. Error Handling (3 tests)
- ✅ Database constraint violations
- ✅ Redis timeout handling
- ✅ Consistency during failures

## Performance Expectations

### Container Startup

- **Minimal Config** (3 services): 15-20 seconds
- **Full Config** (8 services): 60-90 seconds

### Test Execution

- **PostgreSQL write**: ~5-10ms per receipt
- **Redis cache hit**: 2-5ms
- **Cache speedup**: 10x+ (hit vs miss)
- **Batch throughput**: 50 tx/sec

## Troubleshooting

### Error: Could not find a working container runtime strategy

**Cause**: Docker is not running

**Solution**:
```bash
# macOS with Docker Desktop
open -a Docker

# Linux
sudo systemctl start docker

# Verify
docker info
```

### Error: Port already in use

**Cause**: Service already running on port

**Solution**:
```bash
# Check port usage
lsof -i :5432  # PostgreSQL
lsof -i :6379  # Redis
lsof -i :16686 # Jaeger

# Stop existing containers
docker ps
docker stop <container-id>
```

### Tests timeout after 120 seconds

**Cause**: Slow Docker image download

**Solution**:
```bash
# Pre-pull images
docker pull postgres:15-alpine
docker pull redis:7-alpine
docker pull jaegertracing/all-in-one:latest
```

### Network creation fails

**Cause**: Network API version mismatch

**Status**: ✅ Fixed with fallback to default network

## Service Ports

When containers are running:

| Service | Container Port | Mapped Port | Purpose |
|---------|---------------|-------------|---------|
| PostgreSQL | 5432 | Random | Database |
| Redis | 6379 | Random | Cache |
| Jaeger UI | 16686 | Random | Trace viewer |
| Jaeger Collector | 14268 | Random | Trace ingestion |

**Note**: Mapped ports are random to avoid conflicts. Use `connectionInfo` to get actual ports.

## Database Schema

```sql
CREATE TABLE lockchain_receipts (
  id SERIAL PRIMARY KEY,
  receipt_id VARCHAR(255) UNIQUE NOT NULL,
  transaction_hash VARCHAR(255) NOT NULL,
  previous_hash VARCHAR(255),
  timestamp BIGINT NOT NULL,
  receipt_data JSONB NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)
```

## Configuration

### Minimal Services (Default)

```javascript
await testcontainers.startMinimal();
```

**Includes**:
- PostgreSQL (lockchain storage)
- Redis (caching)
- Jaeger (tracing)

**Startup**: ~15-20 seconds

### All Services

```javascript
await testcontainers.startAll();
```

**Adds**:
- Prometheus (metrics)
- Grafana (dashboards)
- MinIO (object storage)
- Elasticsearch (logs)
- Kibana (log viewer)

**Startup**: ~60-90 seconds

## CI/CD Integration

### GitHub Actions Example

```yaml
name: E2E Tests

on: [push, pull_request]

jobs:
  testcontainers:
    runs-on: ubuntu-latest

    services:
      docker:
        image: docker:dind
        options: --privileged

    steps:
      - uses: actions/checkout@v3

      - uses: actions/setup-node@v3
        with:
          node-version: '20'

      - name: Install dependencies
        run: pnpm install

      - name: Cache Docker images
        run: |
          docker pull postgres:15-alpine
          docker pull redis:7-alpine
          docker pull jaegertracing/all-in-one:latest

      - name: Run E2E tests
        run: npm run test:e2e
        env:
          CI: true
```

## Development Workflow

### Running Tests During Development

```bash
# Watch mode for quick feedback
npx vitest watch test/e2e/kgc-sidecar-testcontainer.test.mjs

# Single run
npx vitest run test/e2e/kgc-sidecar-testcontainer.test.mjs
```

### Adding New Tests

1. **Add test case** to existing suite:
   ```javascript
   it('should test new feature', async () => {
     // Test implementation
   });
   ```

2. **Create new test suite**:
   ```javascript
   describe('New Feature Tests', () => {
     // Setup and tests
   });
   ```

3. **Run your new tests**:
   ```bash
   npm run test:e2e
   ```

## Best Practices

### Do's ✅

- Clean up test data in `beforeEach()`
- Use descriptive test names
- Add performance assertions where relevant
- Include error handling tests
- Document expected behavior

### Don'ts ❌

- Don't share state between tests
- Don't hardcode container ports
- Don't skip cleanup in `afterAll()`
- Don't commit test data files
- Don't ignore timeout warnings

## Documentation

### Detailed Guides

- **Implementation Notes**: `/docs/testcontainer-implementation-notes.md`
- **E2E Guide**: `/docs/E2E-TESTCONTAINERS-GUIDE.md`
- **Summary**: `/docs/E2E-SUMMARY-RECOMMENDATIONS.md`
- **Completion Report**: `/docs/coder-agent-completion-summary.md`

### Code Comments

All test files include comprehensive JSDoc comments explaining:
- Test purpose and scope
- Expected behavior
- Integration points
- Performance expectations

## Support

### Getting Help

1. **Check Documentation**:
   - This README
   - Implementation notes
   - E2E guides

2. **Review Test Output**:
   - Verbose logs
   - Container startup messages
   - Error stack traces

3. **Debug Mode**:
   ```bash
   DEBUG=testcontainers* npm run test:e2e
   ```

4. **Report Issues**:
   - GitHub Issues: https://github.com/unrdf/unrdf/issues
   - Include test output and Docker version

## Quick Reference

### Common Commands

```bash
# Run all E2E tests
npm run test:e2e

# Run specific test
npm run test:e2e -- test/e2e/kgc-sidecar-testcontainer.test.mjs

# Watch mode
npx vitest watch test/e2e/

# Verbose output
npx vitest test/e2e/ --reporter=verbose

# Coverage report
npm run test:e2e -- --coverage

# Check Docker
docker info
docker ps
docker images
```

### Environment Variables

```bash
# PostgreSQL
DATABASE_URL=postgresql://test:test@localhost:5432/kgc_test
POSTGRES_HOST=postgres
POSTGRES_PORT=5432

# Redis
REDIS_URL=redis://localhost:6379
REDIS_HOST=redis
REDIS_PORT=6379

# Jaeger
JAEGER_ENDPOINT=http://localhost:14268/api/traces
```

## Success Metrics

### Test Execution

- ✅ All 15 tests pass
- ✅ Container startup < 30 seconds
- ✅ Test execution < 10 seconds
- ✅ No memory leaks

### Performance

- ✅ PostgreSQL write: ≤ 10ms
- ✅ Redis cache hit: ≤ 5ms
- ✅ Batch throughput: ≥ 50 tx/sec
- ✅ Cache speedup: ≥ 10x

## Next Steps

1. **Run the tests**: `npm run test:e2e`
2. **Review results**: Check for any failures
3. **Update tests**: Add new scenarios as needed
4. **Document changes**: Keep this README current

---

**Last Updated**: October 1, 2025
**Maintained By**: UNRDF Team
**Status**: ✅ Production Ready
