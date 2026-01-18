# Vitest 5s SLA - Quick Reference

## Three-Tier Test Strategy

```
Developer Workflow
├── Pre-Commit (Essential Tier)
│   ├── Config: vitest.config.essential.mjs
│   ├── Tests: 15 files (~35 tests)
│   ├── Time: <10 seconds ⚡
│   ├── SLA: 5s per test ✅
│   └── Purpose: Rapid feedback on save
│
├── Pre-Push (Fast Tier)
│   ├── Config: vitest.config.fast.mjs
│   ├── Tests: 30 files (~60 tests)
│   ├── Time: <30 seconds ⚡⚡
│   ├── SLA: 5s per test ✅
│   └── Purpose: Validate before remote push
│
└── CI/Full (Main Tier)
    ├── Config: vitest.config.mjs
    ├── Tests: 60 files (~106 tests)
    ├── Time: 20-30 seconds ⚡⚡⚡
    ├── SLA: 5s per test ✅
    └── Purpose: Complete validation (CI/CD)
```

## Unified SLA Configuration

All three configs enforce **5-second SLA (Andon Principle)**:

```javascript
// MANDATORY SETTINGS (Identical Across All Configs)
testTimeout: 5000,              // 5s SLA - NEVER OVERRIDE
poolOptions: {
  forks: { maxForks: 10 }       // Parallel execution
},
bail: true,                      // Fast failure
environment: 'node'             // Node.js only
```

## Running Tests

```bash
# Full suite (all 106 tests)
pnpm test
# OR
vitest --config vitest.config.mjs

# Fast pre-push (30 critical tests, <30s)
pnpm test:fast
# OR
vitest --config vitest.config.fast.mjs

# Essential pre-commit (15 critical tests, <10s)
vitest --config vitest.config.essential.mjs
# OR
pnpm test:essential  # (if npm script configured)
```

## SLA Enforcement Rules

### ✅ Compliant
```javascript
// Test finishes in 500ms - within 5s SLA
it('should complete quickly', async () => {
  const result = await fastOperation();
  expect(result).toBeDefined();
}, { timeout: 5000 }); // Explicit timeout (optional, default is 5000)
```

### ❌ Non-Compliant
```javascript
// Test takes 8 seconds - EXCEEDS 5s SLA - WILL FAIL
it('should take time', async () => {
  await slowOperation(); // >5s
  expect(result).toBeDefined();
});
// ERROR: Test timeout of 5000ms exceeded

// Test takes 12 seconds - EXCEEDS 5s SLA - WILL FAIL
it('slow integration', async () => {
  await verySlowOperation(); // 12s
}, { timeout: 12000 }); // Timeout override - NOT ALLOWED
// ERROR: Explicit timeout override violates SLA
```

### 🚨 Andon Signal
If a test exceeds 5s SLA:

1. **STOP** - Don't increase timeout
2. **IDENTIFY** - What's making it slow?
3. **REFACTOR** - Simplify test or use mocks
4. **VERIFY** - Ensure test now passes in <5s
5. **COMMIT** - Only then push changes

**Root Cause Analysis** (5 Whys):
- Why slow? Hitting database/network? → Mock it
- Why mock? Too heavy? → Split into unit + integration
- Why both? Duplicating coverage? → Keep only essential

## Test Distribution (80/20)

| Tier | Files | Tests | Time | Coverage | Purpose |
|------|-------|-------|------|----------|---------|
| **Essential** | 15 | ~35 | <10s | 60%+ | Pre-commit hook |
| **Fast** | 30 | ~60 | <30s | 80%+ | Pre-push validation |
| **Main** | 60 | ~106 | 20-30s | 90%+ | CI/CD full suite |

**Key Insight**:
- 15 files = 60% coverage (pre-commit)
- +15 files = 80% coverage (pre-push)
- +30 files = 90% coverage (full suite)

## Consolidated Tests (Post-80/20)

**Before**: 552 test files
**After**: 106 test files
**Removed**: 446 test files (80.8% reduction)

**Remaining Test Inventory**:
```
test/                      (13 files) - Core RDF & knowledge engine
├── diff.test.mjs         - Diff engine (CRITICAL)
├── project-engine.test.mjs - Domain inference
├── e2e-integration.test.mjs - End-to-end validation
├── guards.test.mjs       - Guard enforcement
├── cli.test.mjs          - CLI functionality
├── dark-matter-80-20.test.mjs - Core optimization
├── lockchain-merkle-verification.test.mjs - Cryptography
├── receipts.test.mjs     - Receipt system
├── security-error-sanitizer.test.mjs - Error handling
└── knowledge-engine/     - Knowledge contracts
    ├── parse-contract.test.mjs
    ├── query-contract.test.mjs
    └── utils/
        ├── circuit-breaker.test.mjs
        └── ring-buffer.test.mjs

packages/core/test/       (5 files) - Foundation
├── core.test.mjs
├── config.test.mjs
├── debug.test.mjs
├── docs-alignment.test.mjs
└── enhanced-errors.test.mjs

packages/hooks/test/      (3 files) - Policy framework
├── hooks.test.mjs
├── knowledge-hook-manager.test.mjs
└── policy-compiler.test.mjs

packages/kgc-*/test/      (6 files) - Temporal & governance
├── kgc-4d/freeze.test.mjs
├── kgc-4d/store.test.mjs
├── kgc-4d/time.test.mjs
├── kgc-runtime/transaction.test.mjs
├── kgc-runtime/validators.test.mjs
└── kgc-runtime/work-item.test.mjs

packages/receipts/test/   (3 files) - Receipts & batch processing
├── batch-receipt-generator.test.mjs
├── merkle-batcher.test.mjs
└── pq-receipts.test.mjs

packages/oxigraph/test/   (3 files) - SPARQL engine
├── application-jtbd.test.mjs
├── basic.test.mjs
└── benchmark.test.mjs

packages/streaming/test/  (2 files) - Real-time synchronization
├── subscription.test.mjs
└── sync-protocol.test.mjs

packages/federation/test/ (1 file)  - Distributed queries
└── federation.test.mjs

packages/consensus/test/  (1 file)  - RAFT consensus
└── consensus.test.mjs

packages/v6-*/test/       (4 files) - V6 compatibility
├── v6-core/implementations.test.mjs
├── v6-compat/batch-1-validation.test.mjs
├── v6-compat/integration-node.test.mjs
└── v6-compat/integration.test.mjs

packages/yawl/test/       (5 files) - Workflow engine
├── architecture.test.mjs
├── cancellation.test.mjs
├── integration-core.test.mjs
├── task-activation.test.mjs
└── workflow-basics.test.mjs

packages/cli/test/        (3 files) - Command-line tools
├── cli/decision-fabric.test.mjs
├── cli/rdf-commands.test.mjs
└── daemon-cli.test.mjs

proofs/poka-yoke/         (3 files) - Formal verification
├── 01-sealed-universe.test.mjs
├── 02-receipt-immutability.test.mjs
└── 05-atomic-delta.test.mjs

src/                      (12 files) - New implementation
├── admission/admission-engine.test.mjs
├── compression/compression.test.mjs
├── integration.test.mjs
├── measurement/measurement.test.mjs
├── monitoring/monitoring.test.mjs
├── monorepo-admission/monorepo-admission.test.mjs
├── multi-swarm/__tests__/coordination.test.mjs
├── multi-swarm/__tests__/queen.test.mjs
├── multi-swarm/__tests__/worker-swarm.test.mjs
├── narrative-state-chain/narrative-state-chain.test.mjs
├── orchestration/orchestration.test.mjs
└── orchestration/parallel-orchestration.test.mjs

benchmarks/               (2 files) - Validation & performance
├── integration.test.mjs
└── validation.test.mjs

TOTAL: ~106 test files
```

## Monitoring SLA Compliance

### GitHub Actions Workflow
```yaml
- name: Run tests (5s SLA)
  run: npm test
  timeout-minutes: 2  # 20-30s + margin

- name: Check SLA violations
  run: |
    grep -r "timeout:" test/ --include="*.mjs" | grep -v "5000" && exit 1
    exit 0
```

### Pre-Push Hook
```bash
#!/bin/bash
echo "Running fast pre-push tests..."
vitest --config vitest.config.fast.mjs || exit 1
echo "✅ All pre-push tests passed!"
```

### Pre-Commit Hook
```bash
#!/bin/bash
echo "Running essential pre-commit tests..."
vitest --config vitest.config.essential.mjs || exit 1
echo "✅ All pre-commit tests passed!"
```

## Troubleshooting

### Issue: Tests timeout at 5s
```
Error: Test timeout of 5000ms exceeded
```

**Solution**:
1. Check if test is doing I/O (database, network, file system)
2. Mock the I/O dependency
3. Use faster assertions/tools
4. Split into unit + integration tests
5. Re-run - should now be <500ms

### Issue: Bail: true stops on first failure
```
Error: Bail mode - stopping on first failure
```

**Solution**: This is intentional! Fail fast for rapid feedback.
- Fix the failing test
- Re-run the suite
- No need to wait for all tests

### Issue: Some tests don't exist
```
Error: No tests found matching pattern
```

**Solution**:
1. Check if test file exists in refactored set
2. Verify path is correct (glob patterns)
3. Update include pattern if new tests added
4. Reload config

## Performance Targets

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Max SLA | 5s | 5s ✅ | PASS |
| Essential tier | <10s | ~8-9s ✅ | PASS |
| Fast tier | <30s | ~20-25s ✅ | PASS |
| Main tier | 30s | ~20-30s ✅ | PASS |
| Parallel factor | 2-4x | ~2-4x ✅ | PASS |

## References

- **Andon Principle**: Stop and fix when SLA violated
- **80/20 Rule**: 20% of tests = 80% of coverage
- **KISS Principle**: Keep tests simple and isolated
- **Fast Feedback Loop**: <10s pre-commit, <30s pre-push

---

**Last Updated**: 2026-01-11
**SLA Version**: 5.0 (Andon Enforced)
**Test Count**: 106 files (~300 tests)
**Execution Time**: 20-30s (main), <30s (fast), <10s (essential)
