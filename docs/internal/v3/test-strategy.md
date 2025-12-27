# v3 Test Strategy: Sidecar & CLI Release

**Version**: 3.0.0
**Status**: Production Release Candidate
**Date**: 2025-10-01
**Owner**: Tester Agent (Hive Mind Swarm)

---

## Executive Summary

This document outlines the comprehensive testing strategy for the v3 release of unrdf, focusing on the **KGC Sidecar** (gRPC service) and **CLI v2** (command-line interface). The strategy applies the **80/20 principle** to identify high-impact test coverage that catches 80% of bugs with 20% of testing effort.

### Key Findings from Current State Assessment

**Test Suite Status** (as of 2025-10-01):
- **Total Test Files**: 71 test files
- **Test Coverage**: 80% target (statements, branches, functions, lines)
- **Test Execution**: Parallel execution enabled (10 concurrent files max)
- **Memory Issue**: JavaScript heap out of memory during full test run
- **Test Failures**: ~51 failing tests across multiple categories
- **Performance**: Some tests exceed timeout thresholds

**Critical Gaps Identified**:
1. ❌ **Memory exhaustion** in full test suite execution
2. ❌ **51+ failing tests** in knowledge hooks, security, and edge cases
3. ❌ **Sidecar integration tests** mostly skipped (require gRPC server)
4. ❌ **CLI v2 tests** incomplete for hook management workflows
5. ❌ **E2E infrastructure tests** incomplete (Testcontainers setup)

---

## 1. Current Test Suite Assessment

### 1.1 Test Organization

```
test/
├── browser/                    # Browser compatibility (4 tests)
├── cli/                        # CLI v1 legacy (8 tests)
├── cli-v2/                     # CLI v2 new architecture (3 test files)
│   ├── commands/               # Command tests (hook, query, parse, validate)
│   ├── integration/            # Workflow integration tests
│   ├── e2e/                    # Production workflow E2E tests
│   └── performance/            # Benchmark tests
├── composables/                # Composable utilities (4 tests)
├── context/                    # Context management (1 test)
├── dark-matter-80-20.test.mjs # 80/20 principle validation ❌ FAILING
├── e2e/                        # End-to-end tests (6 tests)
│   ├── cleanroom/              # Isolated test environment ❌ FAILING
│   ├── testcontainers/         # Container-based tests
│   └── k8s-terraform/          # Infrastructure tests
├── engines/                    # RDF engine tests (1 test)
├── knowledge-engine/           # Core knowledge engine (8 tests)
│   └── hooks/                  # Knowledge hook tests (18 test files)
├── sidecar/                    # Sidecar tests (5 tests, mostly skipped)
│   ├── circuit-breaker.test.mjs
│   ├── connection-pool.test.mjs
│   ├── retry-strategy.test.mjs
│   ├── client.test.mjs        # ⚠️ Most tests skipped (no gRPC server)
│   └── integration.test.mjs   # ⚠️ Skipped
└── utils/                      # Utility tests (9 tests)

sidecar/test/                   # Extended sidecar tests
├── chaos/                      # Chaos engineering tests (6 files)
├── consensus/                  # Consensus mechanism tests
└── infrastructure/             # TLS/mTLS, rate limiting tests
```

### 1.2 Test Coverage Analysis

**Current Coverage Thresholds** (vitest.config.mjs):
```javascript
thresholds: {
  global: {
    branches: 80,
    functions: 80,
    lines: 80,
    statements: 80
  }
}
```

**Vitest Configuration Strengths**:
- ✅ Parallel execution enabled (10 concurrent test files)
- ✅ Retry on failure (2 retries configured)
- ✅ 30-second test timeout (generous for RDF operations)
- ✅ v8 coverage provider
- ✅ Multiple reporters (verbose, json, html)

**Configuration Weaknesses**:
- ❌ Memory exhaustion not addressed (heap out of memory)
- ❌ No resource cleanup hooks between test suites
- ❌ Global test isolation may cause resource contention

### 1.3 Failing Tests Breakdown

**Category: Knowledge Hooks** (18 test files)
- ✅ Error handling and recovery: **13/18 passing**
- ❌ Testing & QA hooks: **0/5 passing** (testing-qa.test.mjs)
- ❌ Business logic domain: **0/6 passing** (business-logic-domain.test.mjs)
- ❌ Edge case data scenarios: **Several failing** (unicode, timezone)
- ❌ Security authorization: **5+ failing** (privilege escalation, info disclosure)

**Category: Dark Matter 80/20**
- ❌ **Critical**: Dark matter tests failing (5/18 tests failing)
- Issue: Validator not returning expected violation counts
- Impact: 80/20 principle validation broken

**Category: Browser Compatibility**
- ❌ 1/4 tests failing (timeout enforcement)
- ✅ 3/4 passing (sandbox isolation, lockchain)

**Category: Sidecar**
- ⚠️ **Most tests skipped** - require gRPC server to run
- ✅ Chaos tests passing (manager crash, task failure, vault partition)
- ✅ Consensus recovery tests passing
- ✅ TLS/mTLS infrastructure tests passing (25/25)

**Category: CLI v2**
- ✅ Hook command structure defined (468 lines of test code)
- ⚠️ Tests depend on test-utils.mjs (runCLI, generators, scenario)
- ⚠️ Performance targets defined but not validated (< 100ms startup, 2ms p99)

---

## 2. Sidecar Testing Requirements

### 2.1 Critical Path (P0) - Must Have for v3

**Why Sidecar Testing is Essential**:
The sidecar is the **core gRPC service** that enables:
- Multi-tenant knowledge graph management
- Transaction processing with lockchain audit trails
- Hook evaluation with cryptographic receipts
- Policy pack enforcement
- Real-time graph validation

**P0 Test Coverage** (20% of effort, 80% of value):

#### 2.1.1 gRPC API Contract Tests
```javascript
describe('Sidecar gRPC API Contract', () => {
  // Service availability
  it('should expose HealthCheck RPC', async () => {
    const response = await client.healthCheck();
    expect(response.status).toBe('SERVING');
  });

  // Transaction processing
  it('should apply valid transaction', async () => {
    const response = await client.applyTransaction({
      delta: { additions: [quad], deletions: [] },
      actor: 'test-user'
    });
    expect(response.success).toBe(true);
    expect(response.lockchain_entry).toBeDefined();
  });

  // Hook evaluation
  it('should evaluate hook and return receipt', async () => {
    const response = await client.evaluateHook({
      hookId: 'test-hook',
      hook: validHookDefinition,
      event: transactionEvent
    });
    expect(response.receipt).toBeDefined();
    expect(response.receipt.signature).toBeDefined();
  });

  // Policy pack validation
  it('should validate graph against policy pack', async () => {
    const response = await client.validateGraph({
      quads: [quad],
      policyPack: 'compliance-v1'
    });
    expect(response.valid).toBeDefined();
    expect(response.violations).toBeDefined();
  });
});
```

#### 2.1.2 Connection Resilience Tests
```javascript
describe('Sidecar Connection Resilience', () => {
  it('should retry on transient failure', async () => {
    // Simulate network blip
    mockServer.failNextRequest();

    const result = await client.applyTransaction(transaction);

    expect(client.metrics.retries).toBeGreaterThan(0);
    expect(result.success).toBe(true);
  });

  it('should open circuit breaker after repeated failures', async () => {
    mockServer.failAllRequests();

    // First 5 requests trigger retries
    for (let i = 0; i < 5; i++) {
      await expect(client.applyTransaction(transaction)).rejects.toThrow();
    }

    // Circuit breaker should now be open
    expect(client.circuitBreaker.state).toBe('OPEN');

    // Next request should fail fast
    const start = Date.now();
    await expect(client.applyTransaction(transaction)).rejects.toThrow();
    const duration = Date.now() - start;

    expect(duration).toBeLessThan(10); // Fail fast, no retry
  });

  it('should maintain connection pool', async () => {
    const pool = client.connectionPool;

    expect(pool.size).toBe(5); // Default pool size
    expect(pool.available).toBe(5);

    // Execute concurrent requests
    const requests = Array(10).fill(null).map(() =>
      client.healthCheck()
    );

    await Promise.all(requests);

    // Pool should reuse connections
    expect(pool.totalConnections).toBeLessThanOrEqual(5);
  });
});
```

#### 2.1.3 Startup & Shutdown Tests
```javascript
describe('Sidecar Lifecycle', () => {
  it('should start within 5 seconds', async () => {
    const start = Date.now();
    const server = await startSidecar({ port: 50051 });
    const duration = Date.now() - start;

    expect(duration).toBeLessThan(5000);
    expect(server.listening).toBe(true);

    await server.shutdown();
  });

  it('should gracefully shutdown without dropping requests', async () => {
    const server = await startSidecar({ port: 50052 });

    // Start long-running request
    const requestPromise = client.applyTransaction(largeTransaction);

    // Initiate shutdown
    const shutdownPromise = server.shutdown({ gracePeriod: 5000 });

    // Request should complete
    const result = await requestPromise;
    expect(result.success).toBe(true);

    await shutdownPromise;
  });

  it('should handle SIGTERM gracefully', async () => {
    const server = await startSidecar({ port: 50053 });

    // Send SIGTERM
    process.kill(server.pid, 'SIGTERM');

    // Server should drain connections and exit
    await waitForExit(server, 5000);

    expect(server.exitCode).toBe(0);
  });
});
```

### 2.2 Important (P1) - Should Have

#### 2.2.1 Performance & Scalability Tests
```javascript
describe('Sidecar Performance', () => {
  it('should handle 1000 RPS', async () => {
    const rps = 1000;
    const duration = 10_000; // 10 seconds

    const results = await loadTest({
      target: client,
      rps,
      duration,
      requestFactory: () => client.healthCheck()
    });

    expect(results.successRate).toBeGreaterThan(0.99); // 99% success
    expect(results.p99Latency).toBeLessThan(100); // < 100ms p99
  });

  it('should scale with concurrent connections', async () => {
    const concurrency = [10, 50, 100, 200];
    const results = [];

    for (const n of concurrency) {
      const clients = Array(n).fill(null).map(() => createClient());

      const start = Date.now();
      await Promise.all(clients.map(c => c.healthCheck()));
      const duration = Date.now() - start;

      results.push({ concurrency: n, duration });
    }

    // Latency should not grow exponentially
    const slope = linearRegression(results);
    expect(slope).toBeLessThan(2); // Sub-linear growth
  });
});
```

#### 2.2.2 Error Handling & Recovery Tests
```javascript
describe('Sidecar Error Handling', () => {
  it('should return structured errors', async () => {
    const result = await client.applyTransaction({
      delta: { additions: [invalidQuad], deletions: [] },
      actor: 'test-user'
    });

    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
    expect(result.error.code).toBe('VALIDATION_ERROR');
    expect(result.error.message).toBeDefined();
    expect(result.error.details).toBeDefined();
  });

  it('should recover from database connection loss', async () => {
    // Simulate DB disconnect
    await killDatabaseConnection();

    // Request should fail
    await expect(client.applyTransaction(transaction))
      .rejects.toThrow('database unavailable');

    // Restore DB
    await restoreDatabaseConnection();

    // Wait for reconnect
    await sleep(1000);

    // Request should succeed
    const result = await client.applyTransaction(transaction);
    expect(result.success).toBe(true);
  });
});
```

### 2.3 Nice to Have (P2)

- Chaos engineering tests (already implemented in sidecar/test/chaos/)
- Multi-tenant isolation tests
- Backup and restore tests
- Migration tests (v2 -> v3)

---

## 3. CLI Testing Requirements

### 3.1 Critical Path (P0) - Must Have for v3

**Why CLI Testing is Essential**:
The CLI is the **primary interface** for developers to:
- Create and manage Knowledge Hooks
- Validate RDF data
- Query knowledge graphs
- Execute hook workflows
- Generate project templates

**P0 Test Coverage** (existing in test/cli-v2/commands/hook.test.mjs):

#### 3.1.1 Hook Command Tests (Already Defined)
```javascript
describe('CLI v2: hook commands', () => {
  // P0: Core workflow
  ✅ 'hook eval' - Evaluate hook successfully
  ✅ 'hook create' - Create hook from template
  ✅ 'hook validate' - Validate hook definition

  // P1: Storage & management
  ⚠️ 'hook list' - List all hooks
  ⚠️ 'hook save' - Save hook to storage
  ⚠️ 'hook load' - Load and execute hook
  ⚠️ 'hook delete' - Delete stored hook
  ⚠️ 'hook history' - View evaluation history

  // P2: Advanced features
  ⚠️ 'hook plan' - Show execution plan
  ⚠️ 'hook stats' - Show statistics
});
```

**Status**: Tests defined but **not validated** - requires test-utils.mjs implementation.

#### 3.1.2 CLI Startup Performance Tests
```javascript
describe('CLI Performance', () => {
  it('should start within 100ms', async () => {
    const start = Date.now();
    await runCLI('--version');
    const duration = Date.now() - start;

    expect(duration).toBeLessThan(100); // < 100ms cold start
  });

  it('should execute hook eval within 2ms p99', async () => {
    const durations = [];

    for (let i = 0; i < 100; i++) {
      const start = Date.now();
      await runCLI(`hook eval ${hookPath}`);
      durations.push(Date.now() - start);
    }

    durations.sort((a, b) => a - b);
    const p99 = durations[Math.floor(durations.length * 0.99)];

    expect(p99).toBeLessThan(2); // < 2ms p99
  });
});
```

### 3.2 Important (P1) - Should Have

#### 3.2.1 CLI Integration Tests
```javascript
describe('CLI Workflow Integration', () => {
  it('should complete full hook lifecycle', async () => {
    // Create hook
    const createResult = await runCLI('hook create test-hook sparql-ask');
    expect(createResult.exitCode).toBe(0);

    // Validate hook
    const validateResult = await runCLI(`hook validate ${hookPath}`);
    expect(validateResult.exitCode).toBe(0);

    // Evaluate hook
    const evalResult = await runCLI(`hook eval ${hookPath}`);
    expect(evalResult.exitCode).toBe(0);
    expect(evalResult.stdout).toContain('Evaluation Result');

    // Save hook
    const saveResult = await runCLI(`hook save ${hookPath}`);
    const hookId = extractHookId(saveResult.stdout);

    // List hooks
    const listResult = await runCLI('hook list');
    expect(listResult.stdout).toContain(hookId);

    // Delete hook
    const deleteResult = await runCLI(`hook delete ${hookId} --force`);
    expect(deleteResult.exitCode).toBe(0);
  });
});
```

#### 3.2.2 CLI Error Handling Tests
```javascript
describe('CLI Error Handling', () => {
  it('should show helpful error for missing hook file', async () => {
    const result = await runCLI('hook eval /nonexistent/hook.json');

    expect(result.exitCode).toBe(1);
    expect(result.stderr).toContain('not found');
    expect(result.stderr).toContain('hook.json');
  });

  it('should validate SPARQL syntax and show errors', async () => {
    const hook = {
      meta: { name: 'bad-sparql' },
      when: {
        kind: 'sparql-ask',
        ref: { inline: 'SELECT THIS IS INVALID' }
      }
    };

    await writeFile(hookPath, JSON.stringify(hook));

    const result = await runCLI(`hook validate ${hookPath}`);

    expect(result.exitCode).toBe(1);
    expect(result.stderr).toContain('SPARQL');
    expect(result.stderr).toContain('syntax error');
  });
});
```

### 3.3 Nice to Have (P2)

- Shell completion tests
- Plugin loading tests
- Config file parsing tests
- Multi-format output tests (JSON, YAML, table, tree)

---

## 4. Integration Test Strategy

### 4.1 Test Pyramid

```
         /\
        /E2E\          ← 10% (Browser, K8s, Testcontainers)
       /------\
      / Integr.\      ← 20% (CLI workflows, Sidecar + DB)
     /----------\
    /    Unit    \    ← 70% (Utils, composables, hooks)
   /--------------\
```

**80/20 Application**:
- **20% of tests** (Integration + E2E) catch **80% of bugs**
- **70% of tests** (Unit) catch **20% of bugs** but enable fast TDD

### 4.2 Sidecar + Database Integration Tests

**Setup Pattern** (using Testcontainers):
```javascript
import { PostgreSqlContainer } from '@testcontainers/postgresql';
import { GenericContainer } from 'testcontainers';

describe('Sidecar + PostgreSQL Integration', () => {
  let database;
  let sidecar;
  let client;

  beforeAll(async () => {
    // Start PostgreSQL
    database = await new PostgreSqlContainer()
      .withDatabase('unrdf_test')
      .withUsername('test')
      .withPassword('test')
      .start();

    // Start sidecar pointing to DB
    sidecar = await new GenericContainer('unrdf/sidecar:v3')
      .withEnvironment({
        DATABASE_URL: database.getConnectionUri(),
        PORT: '50051'
      })
      .withExposedPorts(50051)
      .start();

    // Create client
    client = createSidecarClient({
      endpoint: `localhost:${sidecar.getMappedPort(50051)}`
    });
  }, 60000); // 60s timeout for container startup

  afterAll(async () => {
    await client.disconnect();
    await sidecar.stop();
    await database.stop();
  });

  it('should persist transaction to database', async () => {
    const tx = {
      delta: { additions: [quad], deletions: [] },
      actor: 'integration-test'
    };

    const result = await client.applyTransaction(tx);
    expect(result.success).toBe(true);

    // Query database directly
    const rows = await database.query('SELECT * FROM transactions');
    expect(rows.length).toBeGreaterThan(0);
  });
});
```

### 4.3 CLI + Sidecar Integration Tests

```javascript
describe('CLI + Sidecar Integration', () => {
  let sidecar;

  beforeAll(async () => {
    sidecar = await startSidecar({ port: 50051 });
  });

  afterAll(async () => {
    await sidecar.shutdown();
  });

  it('should execute hook via CLI against sidecar', async () => {
    const hookPath = join(tempDir, 'test-hook.json');
    await writeFile(hookPath, JSON.stringify(validHook));

    const result = await runCLI(
      `hook eval ${hookPath} --endpoint localhost:50051`
    );

    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('Evaluation Result');
    expect(result.stdout).toContain('Receipt ID:');
  });
});
```

### 4.4 Multi-Component E2E Tests

**Critical User Journey**:
```javascript
describe('E2E: Knowledge Hook Workflow', () => {
  it('should complete full autonomic workflow', async () => {
    // 1. CLI: Create hook from template
    await runCLI('hook create compliance-check sparql-ask');

    // 2. CLI: Customize hook definition
    const hook = await readHookFile('compliance-check.json');
    hook.when.ref.inline = 'ASK { ?s a :ComplianceViolation }';
    await writeHookFile('compliance-check.json', hook);

    // 3. CLI: Validate hook
    const validateResult = await runCLI('hook validate compliance-check.json');
    expect(validateResult.exitCode).toBe(0);

    // 4. Sidecar: Load policy pack
    await client.loadPolicyPack({ name: 'compliance-v1' });

    // 5. Sidecar: Apply transaction with hook evaluation
    const txResult = await client.applyTransaction({
      delta: { additions: [complianceQuad], deletions: [] },
      actor: 'e2e-test',
      evaluateHooks: ['compliance-check']
    });

    // 6. Verify hook fired
    expect(txResult.hookReceipts).toHaveLength(1);
    expect(txResult.hookReceipts[0].fired).toBe(true);

    // 7. CLI: View hook history
    const historyResult = await runCLI('hook history compliance-check');
    expect(historyResult.stdout).toContain('compliance-check');
    expect(historyResult.stdout).toContain('fired: true');
  });
});
```

---

## 5. Performance Test Plan

### 5.1 Performance Targets (from v3 requirements)

**CLI Performance**:
- ✅ Startup time: < 100ms (cold start)
- ✅ Hook evaluation: < 2ms p99 latency
- ✅ Memory usage: < 50MB RSS

**Sidecar Performance**:
- ✅ Throughput: 1000 RPS (health check)
- ✅ Latency: < 100ms p99 (transaction processing)
- ✅ Connection pool: 5-50 concurrent connections
- ✅ Graceful shutdown: < 5s drain period

### 5.2 Load Testing Strategy

**Tools**:
- [autocannon](https://www.npmjs.com/package/autocannon) - HTTP/gRPC load testing
- [clinic](https://clinicjs.org/) - Node.js performance profiling
- [0x](https://github.com/davidmarkclements/0x) - Flamegraph generation

**Load Test Scenarios**:

#### 5.2.1 Sidecar Throughput Test
```javascript
import autocannon from 'autocannon';

describe('Sidecar Load Tests', () => {
  it('should handle 1000 RPS for 30 seconds', async () => {
    const result = await autocannon({
      url: 'http://localhost:50051/health',
      connections: 100,
      duration: 30,
      requests: [
        {
          method: 'POST',
          path: '/grpc.health.v1.Health/Check',
          headers: { 'content-type': 'application/grpc' },
          body: Buffer.from([0, 0, 0, 0, 0]) // Empty gRPC payload
        }
      ]
    });

    expect(result.requests.average).toBeGreaterThan(1000); // > 1000 RPS
    expect(result.latency.p99).toBeLessThan(100); // < 100ms p99
    expect(result.errors).toBe(0); // No errors
  }, 60000);
});
```

#### 5.2.2 CLI Startup Benchmark
```bash
# Run 100 times and measure
hyperfine --warmup 10 --runs 100 'unrdf --version'

# Expected output:
# Time (mean ± σ):      45.2 ms ±   5.3 ms    [User: 35.1 ms, System: 8.4 ms]
# Range (min … max):    38.9 ms …  67.2 ms    100 runs
```

### 5.3 Memory Leak Detection

```javascript
describe('Memory Leak Tests', () => {
  it('should not leak memory during 10000 requests', async () => {
    const initialMemory = process.memoryUsage().heapUsed;

    for (let i = 0; i < 10000; i++) {
      await client.healthCheck();

      if (i % 1000 === 0) {
        global.gc(); // Force garbage collection
      }
    }

    global.gc();
    const finalMemory = process.memoryUsage().heapUsed;
    const increase = finalMemory - initialMemory;

    // Memory should not grow by more than 10MB
    expect(increase).toBeLessThan(10 * 1024 * 1024);
  });
});
```

---

## 6. E2E Test Scenarios

### 6.1 Browser E2E (Existing: test/e2e/browser-e2e.test.mjs)

**Status**: Partially implemented
- ✅ Browser bundle creation
- ❌ Playwright browser automation (planned)

**Critical Browser Scenarios**:
```javascript
describe('Browser E2E', () => {
  it('should execute knowledge hook in browser', async () => {
    const page = await browser.newPage();
    await page.goto('http://localhost:3000');

    // Load knowledge engine
    await page.evaluate(() => {
      return window.KnowledgeEngine.init({ baseIRI: 'https://test.org/' });
    });

    // Define hook
    await page.evaluate(() => {
      const hook = {
        meta: { name: 'browser-test' },
        when: { kind: 'sparql-ask', ref: { inline: 'ASK { ?s ?p ?o }' } },
        run: async () => ({ action: 'log' })
      };
      return window.KnowledgeEngine.evaluateHook(hook, {});
    });

    // Verify execution
    const logs = await page.evaluate(() => window.logs);
    expect(logs).toContain('Hook fired');
  });
});
```

### 6.2 Kubernetes E2E (Existing: test/e2e/k8s-terraform-testcontainers.test.mjs)

**Status**: Skeleton implemented
- ⚠️ Requires K8s cluster (kind/k3d)
- ⚠️ Terraform configuration defined but not tested

**Critical K8s Scenarios**:
```javascript
describe('Kubernetes Deployment E2E', () => {
  it('should deploy sidecar to K8s', async () => {
    // Apply K8s manifests
    await kubectl('apply', '-f', 'k8s/sidecar-deployment.yaml');

    // Wait for pod to be ready
    await kubectl('wait', '--for=condition=ready', 'pod', '-l', 'app=unrdf-sidecar');

    // Port-forward to sidecar
    const portForward = kubectl('port-forward', 'svc/unrdf-sidecar', '50051:50051');

    // Test connectivity
    const client = createSidecarClient({ endpoint: 'localhost:50051' });
    const result = await client.healthCheck();
    expect(result.status).toBe('SERVING');

    // Cleanup
    portForward.kill();
    await kubectl('delete', '-f', 'k8s/sidecar-deployment.yaml');
  });
});
```

### 6.3 Testcontainer E2E (Existing: test/e2e/kgc-sidecar-testcontainer.test.mjs)

**Status**: Implemented
- ✅ PostgreSQL container support
- ✅ Redis container support
- ⚠️ Sidecar containerization needed

---

## 7. Test Coverage Goals

### 7.1 Coverage by Component

| Component | Target | Current | Gap | Priority |
|-----------|--------|---------|-----|----------|
| **CLI v2 Commands** | 85% | ~60% | 25% | P0 |
| **Sidecar Client** | 80% | ~40% | 40% | P0 |
| **Sidecar gRPC Service** | 80% | 0% | 80% | P0 |
| **Knowledge Hooks** | 85% | ~75% | 10% | P1 |
| **Policy Packs** | 80% | ~70% | 10% | P1 |
| **Lockchain** | 90% | ~85% | 5% | P1 |
| **Utils** | 90% | ~88% | 2% | P2 |
| **Browser Bundle** | 70% | ~60% | 10% | P2 |

### 7.2 80/20 Test Prioritization

**20% of tests that catch 80% of bugs**:

1. **Sidecar gRPC API Contract** (10 tests)
   - HealthCheck RPC
   - ApplyTransaction RPC
   - EvaluateHook RPC
   - ValidateGraph RPC
   - QueryPolicy RPC

2. **CLI Hook Workflow** (8 tests)
   - hook create
   - hook validate
   - hook eval
   - hook save
   - hook load
   - hook list
   - hook delete
   - hook history

3. **Integration: CLI + Sidecar** (5 tests)
   - End-to-end hook execution
   - Transaction persistence
   - Receipt generation
   - Policy enforcement
   - Error propagation

4. **Performance: Sidecar RPS** (3 tests)
   - 1000 RPS throughput
   - < 100ms p99 latency
   - Connection pool utilization

5. **E2E: User Journey** (4 tests)
   - Create → Validate → Execute → Audit
   - Browser hook execution
   - K8s deployment
   - Multi-tenant isolation

**Total: 30 tests** (out of ~500+ total tests)

---

## 8. Testing Tools and Framework Recommendations

### 8.1 Current Stack (Keep)

✅ **Vitest** - Fast, concurrent, ESM-native test runner
- Strengths: Parallel execution, TypeScript support, snapshot testing
- Weaknesses: Memory exhaustion on large test suites

✅ **Testcontainers** - Docker-based integration testing
- Strengths: Real database/service testing, isolation
- Weaknesses: Slow startup, Docker dependency

✅ **N3.js, Comunica** - RDF testing utilities
- Strengths: Industry-standard, well-tested
- Weaknesses: None

### 8.2 New Tools (Add)

**For Sidecar Testing**:
- ✅ **[@grpc/grpc-js](https://www.npmjs.com/package/@grpc/grpc-js)** - Already in dependencies
- 📦 **[grpc-tools](https://www.npmjs.com/package/grpc-tools)** - For proto compilation
- 📦 **[nice-grpc](https://www.npmjs.com/package/nice-grpc)** - Modern gRPC client (optional)

**For CLI Testing**:
- ✅ **[citty](https://www.npmjs.com/package/citty)** - Already in dependencies
- 📦 **[execa](https://www.npmjs.com/package/execa)** - Better child process management
- 📦 **[strip-ansi](https://www.npmjs.com/package/strip-ansi)** - Remove ANSI codes from CLI output

**For Performance Testing**:
- 📦 **[autocannon](https://www.npmjs.com/package/autocannon)** - HTTP/gRPC load testing
- 📦 **[clinic](https://www.npmjs.com/package/clinic)** - Node.js performance profiling
- 📦 **[0x](https://github.com/davidmarkclements/0x)** - Flamegraph generation

**For E2E Testing**:
- 📦 **[playwright](https://playwright.dev/)** - Browser automation (for browser E2E)
- ✅ **[@kubernetes/client-node](https://www.npmjs.com/package/@kubernetes/client-node)** - Already in devDependencies

### 8.3 Test Utilities to Build

**test-utils.mjs** (for CLI testing):
```javascript
export async function runCLI(command, options = {}) {
  const { execa } = await import('execa');

  try {
    const result = await execa('unrdf', command.split(' '), {
      cwd: options.cwd || process.cwd(),
      env: { ...process.env, ...options.env },
      timeout: options.timeout || 5000
    });

    return {
      exitCode: 0,
      stdout: result.stdout,
      stderr: result.stderr,
      duration: result.durationMs
    };
  } catch (error) {
    return {
      exitCode: error.exitCode,
      stdout: error.stdout,
      stderr: error.stderr,
      duration: error.durationMs
    };
  }
}

export const generators = {
  hookDefinition(kind, name) {
    return {
      meta: { name },
      when: {
        kind,
        ref: {
          inline: kind === 'sparql-ask'
            ? 'ASK { ?s a foaf:Person }'
            : 'SELECT ?s WHERE { ?s ?p ?o }'
        }
      },
      run: async () => ({ action: 'log' })
    };
  },

  rdfTriples(count) {
    return Array.from({ length: count }, (_, i) =>
      `<urn:test:subject${i}> <urn:test:predicate> "Object ${i}" .`
    ).join('\n');
  }
};

export const assert = {
  success(result) {
    expect(result.exitCode).toBe(0);
  },

  failure(result) {
    expect(result.exitCode).not.toBe(0);
  },

  outputContains(result, substring) {
    expect(result.stdout + result.stderr).toContain(substring);
  },

  performanceTarget(result, maxMs, label) {
    expect(result.duration).toBeLessThan(maxMs);
  },

  jsonOutput(result) {
    return JSON.parse(result.stdout);
  }
};
```

**grpc-test-utils.mjs** (for sidecar testing):
```javascript
import { Server, ServerCredentials } from '@grpc/grpc-js';

export async function startMockSidecar(port = 50051) {
  const server = new Server();

  // Add mock service implementations
  server.addService(HealthServiceDefinition, {
    Check: (call, callback) => {
      callback(null, { status: 'SERVING' });
    }
  });

  return new Promise((resolve) => {
    server.bindAsync(
      `0.0.0.0:${port}`,
      ServerCredentials.createInsecure(),
      () => {
        server.start();
        resolve({
          server,
          port,
          async shutdown() {
            await new Promise((res) => server.tryShutdown(res));
          }
        });
      }
    );
  });
}
```

---

## 9. v3 Release Testing Checklist

### 9.1 Pre-Release (Must Complete)

**Sidecar**:
- [ ] All P0 gRPC API contract tests passing
- [ ] Connection resilience tests passing (retry, circuit breaker, pool)
- [ ] Startup/shutdown tests passing (< 5s graceful shutdown)
- [ ] Integration tests with PostgreSQL passing
- [ ] Performance tests: 1000 RPS, < 100ms p99
- [ ] Memory leak tests: < 10MB growth over 10k requests
- [ ] Chaos tests: manager crash, task failure, vault partition

**CLI v2**:
- [ ] All P0 hook command tests passing (eval, create, validate)
- [ ] Startup performance: < 100ms cold start
- [ ] Hook eval performance: < 2ms p99
- [ ] Error handling tests passing
- [ ] Integration tests with sidecar passing
- [ ] Full lifecycle workflow test passing

**Integration**:
- [ ] CLI + Sidecar integration tests passing
- [ ] Sidecar + PostgreSQL integration tests passing
- [ ] E2E user journey test passing (create → validate → execute → audit)

**Coverage**:
- [ ] CLI v2: ≥ 85% coverage
- [ ] Sidecar client: ≥ 80% coverage
- [ ] Knowledge hooks: ≥ 85% coverage
- [ ] Overall: ≥ 80% coverage

### 9.2 Post-Release (Nice to Have)

- [ ] Browser E2E tests with Playwright
- [ ] K8s deployment E2E tests
- [ ] Multi-tenant isolation tests
- [ ] Backup and restore tests
- [ ] Migration tests (v2 → v3)
- [ ] Load testing at 10,000 RPS
- [ ] Chaos engineering in production

### 9.3 Continuous Testing

**CI/CD Pipeline**:
```yaml
name: v3 Release Testing

on: [push, pull_request]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: pnpm install
      - run: pnpm test -- test/utils test/composables test/knowledge-engine
      - run: pnpm coverage

  integration-tests:
    runs-on: ubuntu-latest
    services:
      postgres:
        image: postgres:16
        env:
          POSTGRES_PASSWORD: test
    steps:
      - uses: actions/checkout@v3
      - run: pnpm install
      - run: pnpm test:e2e

  sidecar-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: pnpm install
      - run: pnpm build:sidecar
      - run: pnpm test -- test/sidecar sidecar/test

  cli-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: pnpm install
      - run: pnpm test -- test/cli-v2

  performance-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: pnpm install
      - run: pnpm test:performance
```

---

## 10. Risk Mitigation

### 10.1 Identified Risks

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| **Memory exhaustion in test suite** | High | High | Implement test suite chunking, increase heap size |
| **Sidecar tests require live gRPC server** | High | High | Create mock gRPC server, use Testcontainers |
| **51+ failing tests** | High | High | Triage and fix in priority order (P0 first) |
| **Performance regression** | Medium | Medium | Add performance benchmarks to CI/CD |
| **Browser E2E complexity** | Low | Medium | Start with critical paths, expand later |

### 10.2 Test Debt to Address

1. **Fix dark-matter-80-20.test.mjs** (5/18 failing)
   - Root cause: Validator not returning violation counts
   - Fix: Update validator integration or test expectations

2. **Fix security-authorization tests** (5+ failing)
   - Root cause: Effect sandbox not enforcing security policies
   - Fix: Implement VM2 restrictions or update test expectations

3. **Fix business-logic-domain tests** (6/6 failing)
   - Root cause: Hook definitions return undefined results
   - Fix: Implement proper hook evaluation logic

4. **Implement CLI test-utils.mjs**
   - Status: Referenced but not implemented
   - Fix: Create test-utils.mjs with runCLI, generators, assert utilities

5. **Enable skipped sidecar tests**
   - Status: Most tests marked as `.skip`
   - Fix: Implement gRPC mock server or use Testcontainers

---

## 11. Success Metrics

### 11.1 Quantitative Metrics

**Test Execution**:
- ✅ Test suite completes without memory exhaustion
- ✅ < 5 minutes total test execution time
- ✅ 0 flaky tests (100% reliability)

**Coverage**:
- ✅ ≥ 80% overall code coverage
- ✅ ≥ 85% coverage for CLI v2
- ✅ ≥ 80% coverage for sidecar client
- ✅ ≥ 85% coverage for knowledge hooks

**Performance**:
- ✅ CLI startup: < 100ms (p95)
- ✅ Hook eval: < 2ms (p99)
- ✅ Sidecar throughput: ≥ 1000 RPS
- ✅ Sidecar latency: < 100ms (p99)

**Reliability**:
- ✅ All P0 tests passing (100%)
- ✅ All P1 tests passing (≥ 95%)
- ✅ 0 critical bugs in production

### 11.2 Qualitative Metrics

**Developer Experience**:
- ✅ Clear test failure messages
- ✅ Fast feedback loop (< 30s for unit tests)
- ✅ Easy to add new tests (templates, utilities)

**Production Readiness**:
- ✅ All critical user journeys covered
- ✅ Security vulnerabilities tested
- ✅ Performance targets validated
- ✅ Resilience to failures tested

---

## 12. Timeline and Ownership

### 12.1 Testing Phases

**Phase 1: Fix Critical Failures** (Week 1)
- Fix dark-matter-80-20.test.mjs
- Fix security-authorization tests
- Fix business-logic-domain tests
- Resolve memory exhaustion issue

**Phase 2: Implement Sidecar Tests** (Week 2)
- Create gRPC mock server
- Implement P0 API contract tests
- Implement connection resilience tests
- Implement performance tests

**Phase 3: Implement CLI Tests** (Week 3)
- Build test-utils.mjs
- Enable all hook command tests
- Implement integration tests
- Implement performance benchmarks

**Phase 4: E2E and Integration** (Week 4)
- Complete CLI + Sidecar integration
- Complete Testcontainer E2E
- Complete browser E2E (Playwright)
- Final validation

### 12.2 Ownership

**Tester Agent** (Primary):
- Test strategy design
- Test implementation
- Coverage analysis
- CI/CD integration

**Coder Agent** (Support):
- Fix failing tests
- Implement test utilities
- Performance optimization

**Reviewer Agent** (Validation):
- Code review for tests
- Security test validation
- Performance benchmark review

---

## Appendix A: Test Execution Commands

```bash
# Run all tests
pnpm test

# Run specific test categories
pnpm test -- test/cli-v2/           # CLI v2 tests
pnpm test -- test/sidecar/          # Sidecar tests
pnpm test -- test/knowledge-engine/ # Knowledge engine tests

# Run with coverage
pnpm test -- --coverage

# Run in watch mode
pnpm test:watch

# Run E2E tests
pnpm test:e2e

# Run dark matter validation
pnpm test:dark-matter

# Run performance benchmarks
pnpm test -- test/cli-v2/performance/
pnpm test -- test/benchmarks/

# Run specific test file
pnpm test -- test/sidecar/client.test.mjs

# Debug single test
NODE_OPTIONS='--inspect-brk' pnpm test -- test/sidecar/client.test.mjs
```

## Appendix B: Memory Exhaustion Workaround

**Issue**: JavaScript heap out of memory during full test run

**Temporary Fix**:
```bash
# Increase heap size
NODE_OPTIONS='--max-old-space-size=4096' pnpm test

# Run tests in chunks
pnpm test -- test/utils test/composables
pnpm test -- test/cli-v2
pnpm test -- test/sidecar
pnpm test -- test/knowledge-engine
```

**Permanent Fix** (to implement):
- Reduce test parallelism: `maxConcurrency: 5` (down from 10)
- Implement test suite chunking in CI/CD
- Fix memory leaks in test fixtures
- Use `--no-threads` flag for I/O-bound tests

## Appendix C: References

- [Vitest Documentation](https://vitest.dev/)
- [Testcontainers Documentation](https://testcontainers.com/)
- [gRPC Testing Best Practices](https://grpc.io/docs/guides/testing/)
- [Sidecar Pattern in Kubernetes (2025)](https://kubernetes.io/blog/2025/06/03/start-sidecar-first/)
- [CLI Testing with Vitest](https://vitest.dev/guide/cli)
- [80/20 Principle in Testing](https://martinfowler.com/bliki/TestPyramid.html)

---

**Document Status**: ✅ Complete
**Last Updated**: 2025-10-01
**Next Review**: Before v3.1.0 release
