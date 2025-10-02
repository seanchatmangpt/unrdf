# KGC Sidecar Testing Strategy - London BDD

## Overview

This document describes the comprehensive testing strategy for the KGC Nuxt 4 Sidecar using **London School Test-Driven Development (TDD)** principles with **Behavior-Driven Development (BDD)** scenarios.

## Testing Philosophy: London School TDD

The London School of TDD (also called "mockist" or "outside-in" TDD) emphasizes:

1. **Test Behavior, Not State** - Validate interactions and contracts between components
2. **Mock External Dependencies** - Isolate units under test with mocks/stubs
3. **Outside-In Development** - Start with acceptance tests, work inward
4. **Collaboration Testing** - Focus on how objects communicate

### Advantages for KGC Sidecar

- **Clear Contracts**: API routes define contracts with KGC library
- **Fast Feedback**: Unit tests run without full Nuxt initialization
- **Isolation**: Test each layer independently
- **Maintainability**: Changes to implementation don't break tests unless contracts change

## Test Architecture

### Three-Layer Testing Pyramid

```
         /\
        /  \  E2E Tests (5 scenarios)
       /----\  20% of effort, 80% of confidence
      /      \
     /--------\ Integration Tests (API routes with mocked managers)
    /          \ 30% of effort, validates contracts
   /------------\
  /              \ Unit Tests (validation, errors, utilities)
 /----------------\ 50% of effort, fast feedback
```

### Test Projects (Vitest Configuration)

**1. Unit Tests** (`test/unit/`)
- **Environment**: Node (no Nuxt runtime)
- **Purpose**: Test pure functions and utilities
- **Speed**: ⚡ Very fast (< 1ms per test)
- **Coverage**: Validation schemas, error classes, response builders

**2. Integration Tests** (`test/nuxt/`)
- **Environment**: Nuxt runtime with mocked managers
- **Purpose**: Test API routes in isolation
- **Speed**: ⚡ Fast (< 100ms per test)
- **Coverage**: All API endpoints with mocked KGC managers

**3. E2E Tests** (`test/e2e/scenarios/`)
- **Environment**: Node with Docker testcontainers
- **Purpose**: Test complete flows through real infrastructure
- **Speed**: 🐢 Slow (2-3 minutes for full suite)
- **Coverage**: 5 scenarios covering 100% of critical paths

## Test File Organization

```
sidecar/
├── test/
│   ├── unit/
│   │   ├── validation.test.mjs       # Zod schema tests
│   │   ├── errors.test.mjs           # Error class tests
│   │   └── response.test.mjs         # Response builder tests
│   │
│   ├── nuxt/
│   │   ├── api/
│   │   │   ├── health.test.mjs       # Health endpoint
│   │   │   ├── hooks.test.mjs        # Hook registration
│   │   │   ├── transaction.test.mjs  # Transaction apply
│   │   │   ├── policy.test.mjs       # Policy registration
│   │   │   └── effects.test.mjs      # Effect registration
│   │   └── middleware/
│   │       ├── telemetry.test.mjs    # OTel middleware
│   │       └── error-handler.test.mjs # Error handling
│   │
│   └── e2e/
│       └── scenarios/
│           ├── 01-transaction-lifecycle.test.mjs  # 35%
│           ├── 02-policy-governance.test.mjs      # 25%
│           ├── 03-effect-sandbox.test.mjs         # 15%
│           ├── 04-lockchain-audit.test.mjs        # 15%
│           └── 05-observability.test.mjs          # 10%
```

## 80/20 Ultrathink: 5 Critical Scenarios

Following the Pareto Principle, we identified **5 scenarios that validate 100% of critical system paths**:

### Scenario 1: Transaction Lifecycle + OTel Traces (35%)

**Coverage**: Core transaction flow with observability

**Tests**:
1. ✅ Register knowledge hook via HTTP API
2. ✅ Apply transaction via HTTP API
3. ✅ Verify hook execution during transaction
4. ✅ Validate OpenTelemetry trace in Jaeger
5. ✅ Validate Prometheus metrics
6. ✅ Verify p99 latency < 100ms (HTTP E2E)

**Why 35%**: This validates the primary use case and observability weaver

### Scenario 2: Policy Pack Governance + Metrics (25%)

**Coverage**: SHACL validation and compliance

**Tests**:
1. ✅ Register SHACL policy pack via HTTP API
2. ✅ Apply conforming transaction (200 OK)
3. ✅ Apply violating transaction (422 error with violations)
4. ✅ Validate violation details in response
5. ✅ Verify compliance metrics in Prometheus

**Why 25%**: Policy governance is critical for autonomic RDF

### Scenario 3: Effect Sandbox Security (15%)

**Coverage**: VM2/worker thread isolation

**Tests**:
1. ✅ Register JavaScript effect via HTTP API
2. ✅ Verify timeout enforcement (1s limit)
3. ✅ Verify memory limit enforcement (1MB limit)
4. ✅ Verify isolation (cannot access `require('fs')`)
5. ✅ Verify error tracing for sandbox violations

**Why 15%**: Security and isolation are essential

### Scenario 4: Lockchain Audit Trail (15%)

**Coverage**: Git-anchored cryptographic audit trail

**Tests**:
1. ✅ Initialize lockchain with Git repository
2. ✅ Write transaction receipt to Git
3. ✅ Verify Merkle proof generation
4. ✅ Verify tampering detection
5. ✅ Verify Git notes storage

**Why 15%**: Audit trail ensures integrity

### Scenario 5: Health & Observability (10%)

**Coverage**: Service health and metrics

**Tests**:
1. ✅ Health endpoint returns correct status
2. ✅ Manager initialization status reported
3. ✅ OTel middleware creates spans
4. ✅ Service version reported correctly
5. ✅ Request count metrics tracked

**Why 10%**: Essential for production monitoring

## Test Infrastructure

### Testcontainer Environment

The E2E tests use Docker Compose with 6 services:

```
┌─────────────────────────────────────────────────────────────┐
│                   Testcontainer Environment                  │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐  │
│  │ KGC Sidecar  │───▶│ OTel         │───▶│   Jaeger     │  │
│  │ Port: 3000   │    │ Collector    │    │   UI 16686   │  │
│  │ (Nuxt 4)     │    │ OTLP: 4318   │    │ (Traces)     │  │
│  └──────────────┘    └──────────────┘    └──────────────┘  │
│         │                   │                               │
│         │                   ▼                               │
│         │          ┌──────────────┐    ┌──────────────┐    │
│         │          │ Prometheus   │───▶│   Grafana    │    │
│         │          │ Port: 9090   │    │   UI 3001    │    │
│         │          │ (Metrics)    │    │ (Dashboards) │    │
│         │          └──────────────┘    └──────────────┘    │
│         │                                                   │
│         ▼                                                   │
│  ┌──────────────┐                                          │
│  │    Gitea     │                                          │
│  │ Port: 3002   │                                          │
│  │ (Git Server) │                                          │
│  └──────────────┘                                          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Service Configuration

**KGC Sidecar** (Nuxt 4)
- Built from `./sidecar/` directory
- Runs on port 3000
- Environment: production mode
- Health check: `GET /api/health`

**OpenTelemetry Collector**
- Receives OTLP traces on port 4318
- Exports to Jaeger and Prometheus
- Health check extension enabled

**Jaeger** (All-in-One)
- UI on port 16686
- Stores distributed traces
- Accessible via HTTP API

**Prometheus**
- Scrapes OTel Collector metrics
- UI on port 9090
- PromQL query API

**Grafana**
- Dashboards on port 3001
- Pre-configured KGC dashboard
- Login: admin/admin

**Gitea** (Rootless)
- Git server on port 3002
- Stores lockchain repository
- SQLite database

## Running Tests

### Quick Start

```bash
# Install dependencies
cd sidecar
pnpm install

# Run all tests
pnpm test

# Run specific test suites
pnpm test:unit          # Unit tests only (< 1 second)
pnpm test:nuxt          # Integration tests (< 10 seconds)
pnpm test:e2e           # E2E scenarios (2-3 minutes)

# Watch mode for development
pnpm test:watch

# With coverage
pnpm test:coverage
```

### E2E Test Prerequisites

**System Requirements**:
- Docker Desktop 4.0+ (or Docker Engine + Docker Compose)
- Node.js 20+ with pnpm
- Available ports: 3000, 4317, 4318, 8888, 9090, 16686, 3001, 3002
- Memory: 4GB RAM for Docker
- Disk: 2GB for container images

**Setup**:
```bash
# Pull Docker images (optional, speeds up first run)
docker pull otel/opentelemetry-collector:0.91.0
docker pull jaegertracing/all-in-one:1.52
docker pull prom/prometheus:v2.48.1
docker pull grafana/grafana:10.2.3
docker pull gitea/gitea:1.21-rootless
docker pull node:20-alpine

# Start infrastructure manually (optional)
cd test/e2e/testcontainers
docker compose up -d

# Run E2E tests
cd ../../..
pnpm test:e2e

# Stop infrastructure
cd test/e2e/testcontainers
docker compose down -v
```

## Performance SLOs

The tests validate these Service Level Objectives:

| Metric | Target | Validation |
|--------|--------|------------|
| **Transaction Latency (p50)** | ≤ 200µs | Core library (not HTTP) |
| **Transaction Latency (p99)** | ≤ 2ms | Core library (not HTTP) |
| **HTTP E2E Latency (p99)** | ≤ 100ms | Full HTTP round-trip |
| **Hook Execution (p99)** | ≤ 100ms | Prometheus query |
| **Effect Timeout** | 30s (configurable) | Scenario 3 |
| **Effect Memory Limit** | 64MB (configurable) | Scenario 3 |

## Mocking Strategy (London School)

### Unit Tests: Pure Functions Only

```javascript
// ❌ DON'T mock in unit tests for pure functions
import { registerHookSchema } from '../../server/utils/validation.mjs'

it('validates hook registration', () => {
  const result = registerHookSchema.safeParse({ ... })
  expect(result.success).toBe(true)
})
```

### Integration Tests: Mock KGC Managers

```javascript
// ✅ DO mock KGC managers in integration tests
import { vi } from 'vitest'
import { mockNuxtImport } from '@nuxt/test-utils/runtime'

// Mock getManagers utility
const mockHookManager = {
  defineHook: vi.fn().mockResolvedValue({ id: 'test-hook' })
}

mockNuxtImport('getManagers', () => ({
  hookManager: mockHookManager,
  transactionManager: mockTransactionManager,
  // ... other managers
}))

// Now test API route
const response = await $fetch('/api/hooks/register', {
  method: 'POST',
  body: { ... }
})

expect(mockHookManager.defineHook).toHaveBeenCalledWith({ ... })
```

### E2E Tests: Real Infrastructure

```javascript
// ✅ DO use real services in E2E tests
beforeAll(async () => {
  // Start real Docker containers
  await execAsync('docker compose up -d')
})

it('applies transaction and creates trace', async () => {
  // Make real HTTP request
  const response = await fetch('http://localhost:3000/api/transaction/apply', {
    method: 'POST',
    body: JSON.stringify({ ... })
  })

  // Verify real Jaeger trace
  const traces = await fetch('http://localhost:16686/api/traces?...')
  expect(traces.data.length).toBeGreaterThan(0)
})
```

## CLI Integration

The UNRDF CLI (`/Users/sac/unrdf/cli/unrdf.mjs`) already includes sidecar commands:

```bash
# Existing sidecar commands
unrdf sidecar status        # Check sidecar status
unrdf sidecar health        # Health check
unrdf sidecar metrics       # View metrics
unrdf sidecar config get    # Get configuration
unrdf sidecar config set    # Set configuration
```

**Planned additions**:
```bash
# New sidecar commands
unrdf sidecar start         # Start Nuxt sidecar server
unrdf sidecar dev           # Start in development mode
unrdf sidecar build         # Build Nuxt application
unrdf sidecar test          # Run sidecar tests
unrdf sidecar testcontainer # Start testcontainer infrastructure
```

## CI/CD Integration

### GitHub Actions Workflow

```yaml
name: KGC Sidecar Tests

on: [push, pull_request]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
      - uses: actions/setup-node@v4
        with:
          node-version: 20
          cache: pnpm
      - run: cd sidecar && pnpm install
      - run: cd sidecar && pnpm test:unit

  integration-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
      - uses: actions/setup-node@v4
        with:
          node-version: 20
          cache: pnpm
      - run: cd sidecar && pnpm install
      - run: cd sidecar && pnpm test:nuxt

  e2e-tests:
    runs-on: ubuntu-latest
    services:
      docker:
        image: docker:dind
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
      - uses: actions/setup-node@v4
        with:
          node-version: 20
          cache: pnpm
      - run: cd sidecar && pnpm install
      - run: cd sidecar && pnpm test:e2e
        env:
          CI: true
```

## Test Coverage Metrics

When all scenarios pass, you have validated:

- ✅ **35%**: Transaction lifecycle with distributed tracing
- ✅ **25%**: Policy governance with SHACL validation
- ✅ **15%**: Effect sandbox security and isolation
- ✅ **15%**: Lockchain audit trail with Git integration
- ✅ **10%**: Multi-agent conflict resolution
- **= 100%** of critical system paths validated

## Debugging Tests

### View Testcontainer Logs

```bash
# Start infrastructure manually
cd test/e2e/testcontainers
docker compose up

# View logs in real-time
docker compose logs -f kgc-sidecar
docker compose logs -f otel-collector
docker compose logs -f jaeger

# Inspect services
docker compose ps
```

### Access UIs During Tests

While E2E tests are running:

```bash
# Jaeger traces
open http://localhost:16686

# Prometheus metrics
open http://localhost:9090

# Grafana dashboards
open http://localhost:3001  # admin/admin

# Gitea repository
open http://localhost:3002
```

### Debug Individual Tests

```bash
# Run single test file
pnpm vitest run test/e2e/scenarios/01-transaction-lifecycle.test.mjs

# Run with verbose output
pnpm vitest run --reporter=verbose

# Keep containers running after failure
TESTCONTAINERS_RYUK_DISABLED=true pnpm test:e2e
```

## Best Practices

### London School TDD Workflow

1. **Write Acceptance Test First** (E2E scenario)
   - Define expected behavior from user's perspective
   - Test should fail (red)

2. **Write Integration Tests** (API routes)
   - Define contracts with mocked dependencies
   - Tests should fail (red)

3. **Write Implementation** (API route handlers)
   - Implement just enough to pass integration tests
   - Tests should pass (green)

4. **Write Unit Tests** (utilities)
   - Test pure functions in isolation
   - Refactor implementation if needed

5. **Run Acceptance Test** (E2E scenario)
   - Should now pass with real infrastructure
   - Validates complete flow

### Mock vs. Real Services

**Always Mock**:
- KGC library managers in integration tests
- External APIs in unit/integration tests
- Time-dependent functions (Date.now, setTimeout)

**Never Mock**:
- Pure functions (validation, formatting)
- Infrastructure in E2E tests (Docker, OTel, Git)
- HTTP layer in E2E tests

**Sometimes Mock**:
- Database in integration tests (fast feedback)
- File system in unit tests (predictability)
- Network in integration tests (reliability)

## Troubleshooting

### Common Issues

**1. Port Already in Use**
```bash
# Find process using port
lsof -i :3000
lsof -i :16686

# Kill process or change port in docker-compose.yml
```

**2. Docker Not Running**
```bash
# Check Docker daemon
docker info

# Restart Docker Desktop
```

**3. Tests Timeout**
```bash
# Increase timeout in vitest.config.mjs
testTimeout: 180000  # 3 minutes

# Or pull images beforehand
docker compose pull
```

**4. Nuxt Build Fails**
```bash
# Clear Nuxt cache
rm -rf .nuxt .output node_modules/.cache

# Reinstall dependencies
pnpm install --force
```

## References

- [Nuxt Testing Documentation](https://nuxt.com/docs/getting-started/testing)
- [Vitest Documentation](https://vitest.dev/)
- [@nuxt/test-utils](https://github.com/nuxt/test-utils)
- [Testcontainers Node](https://node.testcontainers.org/)
- [OpenTelemetry JavaScript](https://opentelemetry.io/docs/instrumentation/js/)
- [London School TDD](http://www.growing-object-oriented-software.com/)
- [BDD with Cucumber](https://cucumber.io/docs/bdd/)

## Next Steps

1. **Implement remaining API endpoints** for lockchain verification
2. **Add integration tests** for all API routes with mocked managers
3. **Enhance E2E scenarios** with more edge cases
4. **Add performance benchmarking** to track SLO trends
5. **Create mutation testing** to validate test quality
6. **Add visual regression testing** for Grafana dashboards

---

**Testing is not just validation—it's specification through examples.**

The London School approach ensures our tests document **how the system should behave**, not just **what it does**.
