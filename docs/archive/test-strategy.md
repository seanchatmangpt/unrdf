# UNRDF latest Test Strategy

**Version:** latest
**Date:** 2025-10-02
**Status:** PRODUCTION READY
**Primary Validation:** OTEL Span-Based

---

## Executive Summary

This test strategy replaces traditional unit testing with **OTEL span-based validation** as the primary truth source. Agent reports CANNOT be trusted without OTEL validation scores ≥80/100. This strategy ensures production readiness through comprehensive observability, automated validation, and strict acceptance criteria.

### Key Metrics
- **Current Coverage:** 6 test files covering core functionality
- **Target Coverage:** 80% minimum for critical code paths
- **OTEL Validation Suites:** 15 comprehensive feature validations
- **Validation Score Threshold:** ≥80/100 for production deployment
- **Source Files:** 74 .mjs files requiring validation

---

## 1. OTEL Span-Based Validation (PRIMARY TRUTH SOURCE)

### 1.1 Core Principle

**OTEL spans are the ONLY source of truth for feature validation.**

Traditional unit tests validate isolated behavior, but OTEL validation validates:
- ✅ Real execution traces with actual spans
- ✅ Performance metrics under realistic conditions
- ✅ Error rates and recovery behavior
- ✅ Integration between components
- ✅ Production-like observability data

### 1.2 Validation Architecture

```
┌─────────────────────────────────────────────────────────┐
│                  Feature Execution                       │
│  (Generates OTEL Spans, Metrics, Traces)                │
└──────────────────┬──────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────┐
│              OTEL Validator                              │
│  - Collects spans from feature execution                │
│  - Validates span existence & attributes                │
│  - Checks performance thresholds                        │
│  - Applies custom validation rules                      │
└──────────────────┬──────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────┐
│           Validation Score (0-100)                       │
│  - 40% Span validation (existence, status, attributes)  │
│  - 40% Metric validation (latency, errors, throughput)  │
│  - 20% Custom rule validation                           │
│                                                          │
│  ✅ Score ≥80: PRODUCTION READY                          │
│  ⚠️  Score 60-79: NEEDS IMPROVEMENT                      │
│  ❌ Score <60: NOT PRODUCTION READY                      │
└─────────────────────────────────────────────────────────┘
```

### 1.3 Validation Scoring Formula

```javascript
Overall Score = (
  SpanValidation.score * 0.4 +
  MetricValidation.score * 0.4 +
  RuleValidation.score * 0.2
)

// Span Validation Score
SpanValidation.score = max(0, 100 - (missingSpans * 10) - (errorSpans * 10))

// Metric Validation Score
MetricValidation.score = max(0, 100 - (thresholdViolations * 15))

// Rule Validation Score
RuleValidation.score = max(0, 100 - (failedRules * 20))
```

### 1.4 Current OTEL Validation Suites (6)

| Suite | Description | Expected Spans | Threshold Score |
|-------|-------------|----------------|-----------------|
| **knowledge-engine** | Core parse/query/validate/reason | 5 spans | ≥80/100 |
| **cli-parse** | CLI parse command | 3 spans | ≥80/100 |
| **cli-query** | CLI query command | 3 spans | ≥80/100 |
| **cli-validate** | CLI validate command | 3 spans | ≥80/100 |
| **cli-hook** | CLI hook command | 3 spans | ≥80/100 |
| **transaction-manager** | Transaction lifecycle | 3 spans | ≥80/100 |

**Total Validation Suites:** 6
**Expected by latest:** 15 suites

### 1.5 Required OTEL Validation Suites for latest

#### New Suites to Implement (9):

1. **hook-lifecycle** (CRITICAL)
   - Expected Spans: `hook.before`, `hook.run`, `hook.after`, `hook.result`
   - Validates: Hook registration, execution order, cleanup
   - Performance: <100ms per hook execution

2. **condition-sparql-ask** (CRITICAL)
   - Expected Spans: `condition.evaluate`, `sparql.ask`, `condition.result`
   - Validates: SPARQL ASK query evaluation
   - Performance: <200ms per evaluation

3. **condition-sparql-select** (CRITICAL)
   - Expected Spans: `condition.evaluate`, `sparql.select`, `condition.result`
   - Validates: SPARQL SELECT query evaluation with bindings
   - Performance: <300ms per evaluation

4. **condition-shacl** (CRITICAL)
   - Expected Spans: `condition.evaluate`, `shacl.validate`, `condition.result`
   - Validates: SHACL validation condition evaluation
   - Performance: <500ms per evaluation

5. **policy-pack-loader** (HIGH)
   - Expected Spans: `policy.load`, `policy.validate`, `policy.activate`
   - Validates: Policy pack loading, validation, activation
   - Performance: <1000ms per policy pack

6. **lockchain-writer** (HIGH)
   - Expected Spans: `lockchain.write`, `lockchain.sign`, `lockchain.verify`
   - Validates: Cryptographic audit trail generation
   - Performance: <200ms per entry

7. **effect-sandbox** (MEDIUM)
   - Expected Spans: `sandbox.create`, `sandbox.execute`, `sandbox.cleanup`
   - Validates: Safe hook effect execution in VM2 sandbox
   - Performance: <500ms per effect execution

8. **query-optimizer** (MEDIUM)
   - Expected Spans: `query.optimize`, `query.plan`, `query.execute`
   - Validates: Query optimization and execution planning
   - Performance: <100ms optimization overhead

9. **dark-matter-optimizer** (LOW)
   - Expected Spans: `dark-matter.analyze`, `dark-matter.optimize`, `dark-matter.profile`
   - Validates: 80/20 critical path optimization
   - Performance: <50ms analysis overhead

---

## 2. Integration Test Coverage

### 2.1 Critical Integration Paths (Priority 1)

These paths MUST have comprehensive integration tests with OTEL validation:

#### latest Knowledge Hook Lifecycle
```javascript
// test/integration/knowledge-hooks-lifecycle.test.mjs
describe('Knowledge Hook Lifecycle Integration', () => {
  it('should execute hooks in correct order with OTEL spans', async () => {
    // Setup hook manager with observability
    const manager = createKnowledgeHookManager({
      observability: createObservabilityManager()
    });

    // Register hooks
    manager.addKnowledgeHook(beforeHook);
    manager.addKnowledgeHook(afterHook);

    // Execute with OTEL tracing
    const result = await manager.apply(store, delta);

    // OTEL Validation
    const validation = await validateFeature('hook-lifecycle', {
      expectedSpans: ['hook.before', 'hook.run', 'hook.after'],
      requiredAttributes: ['hook.id', 'hook.kind', 'hook.fired'],
      performanceThresholds: { maxLatency: 100 }
    });

    expect(validation.score).toBeGreaterThanOrEqual(80);
    expect(validation.passed).toBe(true);
  });
});
```

**Coverage Target:** ≥90% (critical path)
**OTEL Validation:** Required
**Files:** `src/knowledge-engine/knowledge-hook-manager.mjs`

#### latest Transaction + Hooks Integration
```javascript
// test/integration/transaction-hooks.test.mjs
describe('Transaction with Hooks Integration', () => {
  it('should execute hooks during transaction lifecycle', async () => {
    // Create transaction with hooks
    const tx = createTransaction({
      hooks: [validationHook, auditHook],
      observability: createObservabilityManager()
    });

    // Execute transaction
    await tx.addQuads(quads);
    const receipt = await tx.commit();

    // Validate receipt contains hook results
    expect(receipt.knowledgeHookResults).toHaveLength(2);
    expect(receipt.knowledgeHookResults[0].fired).toBe(true);

    // OTEL Validation
    const validation = await validateFeature('transaction-hooks', {
      expectedSpans: [
        'transaction.start',
        'hook.before',
        'hook.after',
        'transaction.commit'
      ],
      performanceThresholds: { maxLatency: 500 }
    });

    expect(validation.score).toBeGreaterThanOrEqual(80);
  });
});
```

**Coverage Target:** ≥90%
**OTEL Validation:** Required
**Files:** `src/knowledge-engine/transaction.mjs`, `src/knowledge-engine/knowledge-hook-manager.mjs`

#### latest Policy Pack Activation
```javascript
// test/integration/policy-pack.test.mjs
describe('Policy Pack Integration', () => {
  it('should load and activate policy pack with hooks', async () => {
    const manager = createKnowledgeHookManager();

    // Load policy pack
    const policyPack = await manager.loadPolicyPack('./examples/compliance-pack');

    // Validate hooks were registered
    expect(manager.getActiveHooks()).toHaveLength(policyPack.hooks.length);

    // Test policy enforcement
    const result = await manager.apply(store, forbiddenDelta);

    // OTEL Validation
    const validation = await validateFeature('policy-pack', {
      expectedSpans: [
        'policy.load',
        'policy.validate',
        'hook.before',
        'policy.enforce'
      ],
      performanceThresholds: { maxLatency: 1000 }
    });

    expect(validation.score).toBeGreaterThanOrEqual(80);
    expect(result.violations).toHaveLength(1);
  });
});
```

**Coverage Target:** ≥85%
**OTEL Validation:** Required
**Files:** `src/knowledge-engine/policy-pack.mjs`, `src/knowledge-engine/knowledge-hook-manager.mjs`

### 2.2 Integration Test Organization

```
test/
├── integration/                    # Integration tests
│   ├── knowledge-hooks-lifecycle.test.mjs
│   ├── transaction-hooks.test.mjs
│   ├── policy-pack.test.mjs
│   ├── lockchain-integration.test.mjs
│   ├── cli-workflow.test.mjs
│   └── query-optimization.test.mjs
├── knowledge-engine/              # Unit tests (existing)
│   └── parse.test.mjs
├── e2e/                           # E2E tests (existing)
│   └── k8s-terraform-testcontainers.test.mjs
└── helpers/                       # Test utilities
    ├── otel-test-helpers.mjs     # OTEL validation helpers
    ├── hook-builders.mjs         # Hook factory functions
    └── assertion-matchers.mjs    # Custom matchers
```

---

## 3. Unit Test Coverage

### 3.1 Current Unit Test Coverage

**Existing Tests (6 files):**
- ✅ `test/knowledge-engine/parse.test.mjs` - Turtle/JSON-LD parsing
- ✅ `test/cli/context.test.mjs` - CLI context commands
- ✅ `test/cli/graph.test.mjs` - CLI graph commands
- ⚠️ `test/dark-matter-80-20.test.mjs` - Dark matter optimizer (incomplete)
- ⚠️ `test/e2e/` - E2E infrastructure (testcontainers)

### 3.2 Required Unit Test Coverage for latest

**Priority 1 - Critical Components (90% coverage required):**

1. **src/knowledge-engine/knowledge-hook-manager.mjs**
   - Hook registration/removal
   - Condition evaluation
   - Effect execution
   - Error handling
   - Policy pack loading

2. **src/knowledge-engine/define-hook.mjs**
   - Schema validation (Zod)
   - Security validation
   - Hook builder functions
   - Serialization/deserialization

3. **src/knowledge-engine/condition-evaluator.mjs**
   - SPARQL ASK evaluation
   - SPARQL SELECT with bindings
   - SHACL validation
   - Custom condition functions

4. **src/knowledge-engine/hook-executor.mjs**
   - Effect execution in sandbox
   - Receipt generation
   - Error isolation
   - Timeout handling

**Priority 2 - Important Components (80% coverage required):**

5. **src/knowledge-engine/transaction.mjs**
   - Transaction lifecycle
   - Hook integration points
   - Rollback behavior
   - Receipt generation

6. **src/knowledge-engine/lockchain-writer.mjs**
   - Cryptographic signing
   - Audit trail generation
   - Verification

7. **src/knowledge-engine/policy-pack.mjs**
   - Policy pack schema
   - Hook loading
   - Validation rules

**Priority 3 - Standard Components (70% coverage required):**

8. **src/knowledge-engine/query-optimizer.mjs**
9. **src/knowledge-engine/dark-matter-core.mjs**
10. **src/knowledge-engine/observability.mjs**

### 3.3 Unit Test Template

```javascript
/**
 * @file Unit tests for knowledge-hook-manager
 * @description Tests hook registration, execution, and lifecycle management
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { createKnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { createObservabilityManager } from '../../src/knowledge-engine/observability.mjs';

describe('KnowledgeHookManager', () => {
  let manager;
  let observability;

  beforeEach(() => {
    observability = createObservabilityManager({
      serviceName: 'test-hook-manager',
      enableTracing: true,
      enableMetrics: true
    });

    manager = createKnowledgeHookManager({
      basePath: './test/fixtures/hooks',
      observability
    });
  });

  describe('Hook Registration', () => {
    it('should register hook with valid schema', () => {
      const hook = {
        id: 'test-hook',
        kind: 'sparql-ask',
        when: 'before',
        condition: 'ASK { ?s a :Person }',
        effect: (store) => console.log('Hook fired')
      };

      manager.addKnowledgeHook(hook);
      expect(manager.getActiveHooks()).toHaveLength(1);
    });

    it('should reject hook with invalid schema', () => {
      const invalidHook = {
        id: 'invalid',
        // Missing required fields
      };

      expect(() => manager.addKnowledgeHook(invalidHook))
        .toThrow('Hook validation failed');
    });

    it('should prevent duplicate hook IDs', () => {
      const hook1 = { id: 'dup', kind: 'sparql-ask', when: 'before' };
      const hook2 = { id: 'dup', kind: 'sparql-ask', when: 'before' };

      manager.addKnowledgeHook(hook1);
      expect(() => manager.addKnowledgeHook(hook2))
        .toThrow('Hook with id "dup" already exists');
    });
  });

  describe('Condition Evaluation', () => {
    it('should evaluate SPARQL ASK condition', async () => {
      const hook = {
        id: 'ask-test',
        kind: 'sparql-ask',
        when: 'before',
        condition: 'ASK { ?s a :Person }'
      };

      manager.addKnowledgeHook(hook);

      const store = /* create test store */;
      const result = await manager.evaluateCondition(hook, store);

      expect(result).toBeDefined();
      expect(typeof result).toBe('boolean');
    });

    it('should evaluate SPARQL SELECT condition', async () => {
      const hook = {
        id: 'select-test',
        kind: 'sparql-select',
        when: 'before',
        condition: 'SELECT ?name WHERE { ?s :name ?name }'
      };

      manager.addKnowledgeHook(hook);

      const store = /* create test store */;
      const result = await manager.evaluateCondition(hook, store);

      expect(result).toBeInstanceOf(Array);
      expect(result.length).toBeGreaterThan(0);
    });

    it('should handle condition evaluation errors', async () => {
      const hook = {
        id: 'error-test',
        kind: 'sparql-ask',
        when: 'before',
        condition: 'INVALID SPARQL'
      };

      manager.addKnowledgeHook(hook);

      await expect(manager.evaluateCondition(hook, store))
        .rejects.toThrow('SPARQL evaluation failed');
    });
  });

  describe('Effect Execution', () => {
    it('should execute effect when condition is true', async () => {
      const effectSpy = vi.fn();
      const hook = {
        id: 'effect-test',
        kind: 'sparql-ask',
        when: 'before',
        condition: 'ASK { ?s a :Person }',
        effect: effectSpy
      };

      manager.addKnowledgeHook(hook);

      const store = /* create store with Person */;
      await manager.apply(store, delta);

      expect(effectSpy).toHaveBeenCalledTimes(1);
    });

    it('should NOT execute effect when condition is false', async () => {
      const effectSpy = vi.fn();
      const hook = {
        id: 'no-effect-test',
        kind: 'sparql-ask',
        when: 'before',
        condition: 'ASK { ?s a :Robot }',
        effect: effectSpy
      };

      manager.addKnowledgeHook(hook);

      const store = /* create store WITHOUT Robot */;
      await manager.apply(store, delta);

      expect(effectSpy).not.toHaveBeenCalled();
    });

    it('should isolate effect errors', async () => {
      const hook = {
        id: 'error-effect',
        kind: 'sparql-ask',
        when: 'before',
        condition: 'ASK { ?s a :Person }',
        effect: () => { throw new Error('Effect error'); }
      };

      manager.addKnowledgeHook(hook);

      const store = /* create store */;
      const result = await manager.apply(store, delta);

      // Transaction should continue despite effect error
      expect(result.receipt.knowledgeHookResults[0].error).toBeDefined();
      expect(result.committed).toBe(true);
    });
  });

  describe('OTEL Integration', () => {
    it('should generate OTEL spans for hook execution', async () => {
      const hook = {
        id: 'otel-test',
        kind: 'sparql-ask',
        when: 'before',
        condition: 'ASK { ?s a :Person }'
      };

      manager.addKnowledgeHook(hook);

      const store = /* create store */;
      await manager.apply(store, delta);

      // Validate OTEL spans were created
      const validation = await validateFeature('hook-lifecycle', {
        expectedSpans: ['hook.before', 'hook.run', 'hook.after'],
        requiredAttributes: ['hook.id', 'hook.kind', 'hook.fired']
      });

      expect(validation.score).toBeGreaterThanOrEqual(80);
    });
  });
});
```

---

## 4. Validation Scripts (Truth Verification)

### 4.1 Automated OTEL Validation Runner

**Location:** `validation/run-all.mjs` (ALREADY EXISTS)

**Usage:**
```bash
# Run comprehensive validation (all 15 suites)
node validation/run-all.mjs comprehensive

# Run individual validation suites
node validation/run-all.mjs individual

# Run specific feature validation
node validation/knowledge-engine.validation.mjs
node validation/hook-lifecycle.validation.mjs
```

**Acceptance Criteria:**
- ✅ Overall score ≥80/100
- ✅ All critical features pass (score ≥80)
- ✅ No OTEL spans with error status
- ✅ Performance thresholds met

### 4.2 CI/CD Integration

**GitHub Actions Workflow:**
```yaml
# .github/workflows/validation.yml
name: OTEL Validation

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  otel-validation:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'

      - name: Install dependencies
        run: pnpm install

      - name: Run OTEL Validation
        run: |
          node validation/run-all.mjs comprehensive > validation-results.log

      - name: Check Validation Score
        run: |
          SCORE=$(grep "Overall Score:" validation-results.log | awk '{print $3}' | cut -d'/' -f1)
          if [ "$SCORE" -lt 80 ]; then
            echo "❌ Validation score $SCORE < 80 - FAILING BUILD"
            exit 1
          fi
          echo "✅ Validation score $SCORE ≥ 80 - PASSING"

      - name: Upload Validation Report
        uses: actions/upload-artifact@v3
        with:
          name: validation-report
          path: validation-results.log
```

### 4.3 Pre-Commit Validation Hook

**Location:** `.git/hooks/pre-commit`

```bash
#!/bin/bash
# Pre-commit OTEL validation

echo "🔍 Running OTEL validation before commit..."

# Run validation
node validation/run-all.mjs comprehensive > /tmp/validation-results.log

# Check score
SCORE=$(grep "Overall Score:" /tmp/validation-results.log | awk '{print $3}' | cut -d'/' -f1)

if [ "$SCORE" -lt 80 ]; then
  echo "❌ OTEL validation score $SCORE < 80"
  echo "❌ Commit rejected - fix validation failures first"
  cat /tmp/validation-results.log
  exit 1
fi

echo "✅ OTEL validation passed with score $SCORE/100"
exit 0
```

---

## 5. Performance Benchmarks

### 5.1 Performance Requirements

| Component | Metric | Target | Critical Threshold |
|-----------|--------|--------|-------------------|
| Hook Execution | Latency (p95) | <100ms | <200ms |
| Condition Evaluation (SPARQL ASK) | Latency (p95) | <200ms | <500ms |
| Condition Evaluation (SPARQL SELECT) | Latency (p95) | <300ms | <1000ms |
| Condition Evaluation (SHACL) | Latency (p95) | <500ms | <2000ms |
| Transaction Commit | Latency (p95) | <500ms | <1000ms |
| Policy Pack Load | Latency (p95) | <1000ms | <3000ms |
| Lockchain Write | Latency (p95) | <200ms | <500ms |
| Effect Sandbox | Latency (p95) | <500ms | <2000ms |
| Query Optimization | Overhead (p95) | <100ms | <500ms |

### 5.2 Performance Benchmark Suite

**Location:** `test/benchmarks/performance.bench.mjs`

```javascript
import { describe, bench } from 'vitest';
import { createKnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';

describe('Knowledge Hook Performance', () => {
  bench('Hook registration', async () => {
    const manager = createKnowledgeHookManager();
    manager.addKnowledgeHook({
      id: 'perf-test',
      kind: 'sparql-ask',
      when: 'before',
      condition: 'ASK { ?s a :Person }'
    });
  }, { iterations: 10000 });

  bench('SPARQL ASK condition evaluation', async () => {
    const manager = createKnowledgeHookManager();
    const hook = /* ... */;
    const store = /* ... */;

    await manager.evaluateCondition(hook, store);
  }, { iterations: 1000 });

  bench('Transaction with 5 hooks', async () => {
    const manager = createKnowledgeHookManager();
    // Add 5 hooks
    const store = /* ... */;
    const delta = /* ... */;

    await manager.apply(store, delta);
  }, { iterations: 500 });
});
```

**Run Benchmarks:**
```bash
pnpm vitest bench --config vitest.config.mjs
```

### 5.3 Load Testing

**Tool:** Apache Bench (ab) / autocannon
**Scenarios:**
1. 100 concurrent hook executions
2. 1000 transactions with hooks in 1 minute
3. Policy pack activation under load
4. Sustained 10 TPS (transactions per second) for 5 minutes

---

## 6. Error Handling Verification

### 6.1 Error Scenarios to Test

**Priority 1 - Critical Error Paths:**

1. **Hook Execution Errors**
   - Effect throws exception
   - Timeout during effect execution
   - Sandbox security violation
   - Resource exhaustion

2. **Condition Evaluation Errors**
   - Invalid SPARQL syntax
   - SHACL shape not found
   - Query timeout
   - Store access error

3. **Transaction Errors**
   - Rollback after hook failure
   - Partial commit handling
   - Lockchain write failure
   - Concurrent transaction conflict

4. **Policy Pack Errors**
   - Invalid policy pack schema
   - Circular hook dependencies
   - Hook conflict detection
   - Permission denied

### 6.2 Error Recovery Validation

```javascript
describe('Error Recovery', () => {
  it('should isolate effect errors and continue transaction', async () => {
    const manager = createKnowledgeHookManager();

    const failingHook = {
      id: 'failing',
      kind: 'sparql-ask',
      when: 'before',
      condition: 'ASK { ?s a :Person }',
      effect: () => { throw new Error('Intentional failure'); }
    };

    const successHook = {
      id: 'success',
      kind: 'sparql-ask',
      when: 'before',
      condition: 'ASK { ?s a :Person }',
      effect: () => console.log('Success')
    };

    manager.addKnowledgeHook(failingHook);
    manager.addKnowledgeHook(successHook);

    const result = await manager.apply(store, delta);

    // Transaction should commit despite one hook failing
    expect(result.committed).toBe(true);

    // Failing hook should be recorded in receipt
    expect(result.receipt.knowledgeHookResults[0].error).toBeDefined();

    // Success hook should execute
    expect(result.receipt.knowledgeHookResults[1].fired).toBe(true);
  });

  it('should rollback transaction on critical error', async () => {
    const manager = createKnowledgeHookManager();

    const criticalHook = {
      id: 'critical',
      kind: 'sparql-ask',
      when: 'before',
      condition: 'ASK { ?s a :Person }',
      effect: () => { throw new Error('CRITICAL: Security violation'); },
      critical: true  // Flag as critical
    };

    manager.addKnowledgeHook(criticalHook);

    await expect(manager.apply(store, delta))
      .rejects.toThrow('Transaction aborted due to critical hook failure');

    // Verify rollback
    expect(store.size).toBe(originalSize);
  });
});
```

---

## 7. Acceptance Criteria & Production Readiness

### 7.1 OTEL Validation Acceptance Criteria

**Production deployment is ONLY approved when ALL criteria are met:**

✅ **OTEL Validation Score ≥80/100**
- Overall comprehensive validation score
- Calculated from 15 feature validation suites
- No critical features below 80/100

✅ **All Required Spans Present**
- Expected spans exist for each feature
- Span status is "ok" (not "error")
- Required attributes are populated

✅ **Performance Thresholds Met**
- Latency (p95) within targets
- Error rate <1%
- Throughput meets requirements
- Memory usage within limits

✅ **Zero OTEL Error Spans**
- No spans with status "error" in production paths
- All exceptions properly recorded and handled
- Error isolation verified

✅ **CI/CD Pipeline Passes**
- GitHub Actions OTEL validation passes
- Pre-commit validation hook passes
- All integration tests pass with OTEL validation

### 7.2 Test Coverage Acceptance Criteria

✅ **Overall Test Coverage ≥80%**
- Statements: ≥80%
- Branches: ≥75%
- Functions: ≥80%
- Lines: ≥80%

✅ **Critical Component Coverage ≥90%**
- `knowledge-hook-manager.mjs` ≥90%
- `define-hook.mjs` ≥90%
- `condition-evaluator.mjs` ≥90%
- `hook-executor.mjs` ≥90%

✅ **Integration Test Coverage**
- All critical paths have integration tests
- Integration tests include OTEL validation
- E2E tests cover full workflows

### 7.3 Agent Performance Evaluation

**NEVER trust agent reports without OTEL validation.**

| Agent Report | OTEL Validation | Verdict |
|--------------|-----------------|---------|
| "Production ready, 100% coverage" | Score: 45/100, 3/6 features failed | ❌ REJECTED |
| "5/5 stars, all tests passing" | Score: 60/100, missing critical spans | ❌ REJECTED |
| "Minor issues, mostly working" | Score: 85/100, all features passed | ✅ APPROVED |
| "Incomplete, needs work" | Score: 95/100, all validation green | ✅ APPROVED |

**Grading Scale (OTEL-Based):**
- A: Score ≥90/100 (Excellent)
- B: Score 80-89/100 (Good - Production Ready)
- C: Score 70-79/100 (Needs Improvement)
- D: Score 60-69/100 (Significant Issues)
- F: Score <60/100 (Not Production Ready)

---

## 8. Testing Tools & Infrastructure

### 8.1 Testing Framework: Vitest

**Configuration:** `vitest.config.mjs`

```javascript
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'lcov', 'html'],
      exclude: [
        'node_modules/',
        'test/',
        'examples/',
        'docs/',
        'coverage/',
        '**/*.config.mjs'
      ],
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 75,
        statements: 80
      }
    },
    testTimeout: 30000,
    hookTimeout: 30000,
    reporters: ['verbose', 'html'],
    outputFile: {
      html: './coverage/test-results.html'
    }
  }
});
```

### 8.2 OTEL Validation Framework

**Framework:** Custom OTEL Validator (ALREADY IMPLEMENTED)
**Location:** `src/validation/otel-validator.mjs`

**Key Features:**
- Span collection and analysis
- Metric validation against thresholds
- Custom validation rule engine
- Automated scoring (0-100)
- Production readiness assessment

### 8.3 Test Infrastructure

**Docker Compose for E2E Tests:**
```yaml
# docker-compose.test.yml
version: '3.8'
services:
  postgres:
    image: postgres:15
    environment:
      POSTGRES_DB: test_db
      POSTGRES_USER: test_user
      POSTGRES_PASSWORD: test_pass
    ports:
      - "5432:5432"

  redis:
    image: redis:7
    ports:
      - "6379:6379"

  jaeger:
    image: jaegertracing/all-in-one:latest
    ports:
      - "16686:16686"  # UI
      - "4318:4318"    # OTLP HTTP
```

**Testcontainers Integration:**
- PostgreSQL container for persistence tests
- Redis container for caching tests
- Jaeger container for OTEL trace collection

---

## 9. Test Execution Plan for latest

### Phase 1: OTEL Validation Expansion (Day 1-2)

**Deliverables:**
1. ✅ Implement 9 new OTEL validation suites
2. ✅ Update `validation/run-all.mjs` to include new suites
3. ✅ Set up CI/CD validation pipeline
4. ✅ Create pre-commit validation hook

**Success Criteria:**
- All 15 validation suites implemented
- Comprehensive validation score ≥80/100
- CI/CD pipeline passes

### Phase 2: Integration Test Implementation (Day 2-3)

**Deliverables:**
1. ✅ Knowledge hooks lifecycle integration test
2. ✅ Transaction + hooks integration test
3. ✅ Policy pack activation integration test
4. ✅ Lockchain integration test
5. ✅ CLI workflow integration test

**Success Criteria:**
- All critical integration paths tested
- Each integration test includes OTEL validation
- Integration test coverage ≥90%

### Phase 3: Unit Test Completion (Day 3-4)

**Deliverables:**
1. ✅ `knowledge-hook-manager.mjs` unit tests (≥90% coverage)
2. ✅ `define-hook.mjs` unit tests (≥90% coverage)
3. ✅ `condition-evaluator.mjs` unit tests (≥90% coverage)
4. ✅ `hook-executor.mjs` unit tests (≥90% coverage)
5. ✅ `transaction.mjs` unit tests (≥80% coverage)

**Success Criteria:**
- Overall unit test coverage ≥80%
- Critical components ≥90% coverage
- All unit tests pass

### Phase 4: Performance & Error Validation (Day 4-5)

**Deliverables:**
1. ✅ Performance benchmark suite
2. ✅ Load testing scenarios
3. ✅ Error handling validation tests
4. ✅ Recovery behavior verification

**Success Criteria:**
- All performance benchmarks within targets
- Error scenarios properly handled
- Recovery mechanisms validated

### Phase 5: Documentation & Finalization (Day 5)

**Deliverables:**
1. ✅ Test strategy documentation (this document)
2. ✅ CI/CD pipeline documentation
3. ✅ OTEL validation guide
4. ✅ Final production readiness assessment

**Success Criteria:**
- Documentation complete and accurate
- Production readiness checklist verified
- OTEL validation score ≥80/100

---

## 10. Continuous Validation & Monitoring

### 10.1 Production OTEL Monitoring

**Setup Jaeger for Production:**
```bash
# docker-compose.prod.yml
services:
  jaeger:
    image: jaegertracing/all-in-one:latest
    environment:
      COLLECTOR_OTLP_ENABLED: true
    ports:
      - "16686:16686"  # Jaeger UI
      - "4318:4318"    # OTLP HTTP endpoint

  unrdf:
    build: .
    environment:
      OTEL_EXPORTER_OTLP_ENDPOINT: "http://jaeger:4318"
      OTEL_SERVICE_NAME: "unrdf-production"
    depends_on:
      - jaeger
```

**Production Monitoring Dashboard:**
- Hook execution rate (per minute)
- Hook success/failure ratio
- Condition evaluation latency (p50, p95, p99)
- Transaction commit latency
- Error rate and types
- Memory usage trends

### 10.2 Regression Detection

**Automated Regression Tests:**
```bash
# Run OTEL validation after each commit
git commit && node validation/run-all.mjs comprehensive

# Compare validation scores
CURRENT_SCORE=$(grep "Overall Score:" validation-current.log | awk '{print $3}' | cut -d'/' -f1)
BASELINE_SCORE=85

if [ "$CURRENT_SCORE" -lt "$BASELINE_SCORE" ]; then
  echo "⚠️  REGRESSION DETECTED: Score dropped from $BASELINE_SCORE to $CURRENT_SCORE"
  exit 1
fi
```

### 10.3 Performance Regression Detection

**Benchmark Comparison:**
```javascript
// test/benchmarks/regression-check.mjs
import { readFileSync } from 'fs';

const currentBenchmarks = JSON.parse(readFileSync('benchmarks-current.json'));
const baselineBenchmarks = JSON.parse(readFileSync('benchmarks-baseline.json'));

const regressions = [];

for (const [name, current] of Object.entries(currentBenchmarks)) {
  const baseline = baselineBenchmarks[name];

  if (!baseline) continue;

  const percentChange = ((current.mean - baseline.mean) / baseline.mean) * 100;

  if (percentChange > 10) {  // >10% slower is regression
    regressions.push({
      name,
      baseline: baseline.mean,
      current: current.mean,
      change: percentChange
    });
  }
}

if (regressions.length > 0) {
  console.error('❌ Performance regressions detected:');
  console.table(regressions);
  process.exit(1);
}

console.log('✅ No performance regressions detected');
```

---

## 11. Summary & Next Steps

### 11.1 Current State

✅ **Completed:**
- OTEL validation framework implemented
- 6 validation suites operational
- Basic integration test infrastructure
- Observability instrumentation in place

⚠️ **In Progress:**
- Unit test coverage (currently ~10%)
- Integration test coverage (currently ~20%)
- 9 additional OTEL validation suites needed

❌ **Not Started:**
- Performance benchmark suite
- CI/CD validation pipeline
- Pre-commit validation hooks
- Production monitoring setup

### 11.2 Immediate Next Steps (Day 1)

**Priority 1 (CRITICAL - 4 hours):**
1. ✅ Implement 3 critical OTEL validation suites:
   - `hook-lifecycle.validation.mjs`
   - `condition-sparql-ask.validation.mjs`
   - `condition-sparql-select.validation.mjs`

2. ✅ Run comprehensive validation and document baseline score

**Priority 2 (HIGH - 2 hours):**
3. ✅ Create integration test for knowledge hooks lifecycle
4. ✅ Set up CI/CD validation pipeline (GitHub Actions)

**Priority 3 (MEDIUM - 2 hours):**
5. ✅ Implement unit tests for `knowledge-hook-manager.mjs` (≥50% coverage)
6. ✅ Create pre-commit validation hook

### 11.3 Success Metrics

**Day 1 Success:**
- ✅ 9/15 OTEL validation suites implemented
- ✅ Comprehensive validation score ≥70/100
- ✅ CI/CD pipeline operational

**Day 3 Success:**
- ✅ All 15 OTEL validation suites implemented
- ✅ Comprehensive validation score ≥80/100
- ✅ Integration test coverage ≥80%

**Day 5 Success (latest Release):**
- ✅ OTEL validation score ≥80/100
- ✅ Unit test coverage ≥80%
- ✅ Integration test coverage ≥90%
- ✅ All acceptance criteria met
- ✅ Production readiness verified

---

## 12. Appendix: OTEL Validation Examples

### A. Example OTEL Validation Suite

```javascript
// validation/hook-lifecycle.validation.mjs
import { createValidationRunner, createValidationHelpers } from '../src/validation/index.mjs';

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

const hookLifecycleSuite = {
  name: 'hook-lifecycle',
  description: 'OTEL validation for knowledge hook lifecycle',

  features: [
    {
      name: 'hook-before-execution',
      description: 'Validate before hook execution',
      config: {
        expectedSpans: ['hook.before', 'condition.evaluate', 'effect.execute'],
        requiredAttributes: ['hook.id', 'hook.kind', 'hook.when', 'hook.fired'],
        performanceThresholds: {
          maxLatency: 100,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 50 * 1024 * 1024
        },
        validationRules: [
          helpers.createSpanExistenceRule('hook.before', { 'hook.when': 'before' }),
          helpers.createSpanStatusRule('hook.before', 'ok'),
          helpers.createPerformanceRule('latency', 100, '<'),
          helpers.createValidationRule(
            'hook-fired-attribute',
            (spans) => {
              const hookSpan = spans.find(s => s.name === 'hook.before');
              return hookSpan && 'hook.fired' in hookSpan.attributes;
            },
            'error'
          )
        ]
      }
    }
  ],

  globalConfig: {
    timeout: 30000,
    retries: 1,
    parallel: false
  }
};

export async function runHookLifecycleValidation() {
  console.log('🔍 Starting Hook Lifecycle OTEL Validation...');

  const report = await runner.runSuite(hookLifecycleSuite);

  console.log(`\n📊 Hook Lifecycle Validation: ${report.summary.score}/100`);
  console.log(`   Features: ${report.summary.passed}/${report.summary.total} passed`);

  return report;
}

if (import.meta.url === `file://${process.argv[1]}`) {
  runHookLifecycleValidation()
    .then(report => process.exit(report.summary.failed === 0 ? 0 : 1))
    .catch(error => {
      console.error('Validation failed:', error);
      process.exit(1);
    });
}
```

### B. Example Integration Test with OTEL

```javascript
// test/integration/knowledge-hooks-lifecycle.test.mjs
import { describe, it, expect, beforeEach } from 'vitest';
import { createKnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { createObservabilityManager } from '../../src/knowledge-engine/observability.mjs';
import { createOTELValidator } from '../../src/validation/otel-validator.mjs';

describe('Knowledge Hooks Lifecycle Integration', () => {
  let manager;
  let observability;
  let validator;

  beforeEach(async () => {
    observability = createObservabilityManager({
      serviceName: 'test-hook-lifecycle',
      enableTracing: true,
      enableMetrics: true
    });

    await observability.initialize();

    validator = createOTELValidator({ serviceName: 'test-validator' });

    manager = createKnowledgeHookManager({
      basePath: './test/fixtures/hooks',
      observability
    });
  });

  it('should execute hooks in correct order with OTEL spans', async () => {
    // Register hooks
    const beforeHook = {
      id: 'before-test',
      kind: 'sparql-ask',
      when: 'before',
      condition: 'ASK { ?s a :Person }',
      effect: (store) => console.log('Before hook fired')
    };

    const afterHook = {
      id: 'after-test',
      kind: 'sparql-ask',
      when: 'after',
      condition: 'ASK { ?s a :Person }',
      effect: (store) => console.log('After hook fired')
    };

    manager.addKnowledgeHook(beforeHook);
    manager.addKnowledgeHook(afterHook);

    // Execute with OTEL tracing
    const store = /* create test store */;
    const delta = /* create test delta */;
    const result = await manager.apply(store, delta);

    // Verify execution
    expect(result.receipt.knowledgeHookResults).toHaveLength(2);
    expect(result.receipt.knowledgeHookResults[0].hookId).toBe('before-test');
    expect(result.receipt.knowledgeHookResults[1].hookId).toBe('after-test');

    // OTEL Validation
    const validation = await validator.validateFeature('hook-lifecycle', {
      expectedSpans: ['hook.before', 'hook.after', 'condition.evaluate', 'effect.execute'],
      requiredAttributes: ['hook.id', 'hook.kind', 'hook.when', 'hook.fired'],
      performanceThresholds: {
        maxLatency: 100,
        maxErrorRate: 0.01,
        minThroughput: 1,
        maxMemoryUsage: 50 * 1024 * 1024
      },
      validationRules: []
    });

    // Assert OTEL validation passed
    expect(validation.score).toBeGreaterThanOrEqual(80);
    expect(validation.passed).toBe(true);
    expect(validation.violations).toHaveLength(0);

    // Verify specific spans
    const beforeSpan = validation.spans.find(s => s.name === 'hook.before');
    expect(beforeSpan).toBeDefined();
    expect(beforeSpan.status).toBe('ok');
    expect(beforeSpan.attributes['hook.id']).toBe('before-test');

    const afterSpan = validation.spans.find(s => s.name === 'hook.after');
    expect(afterSpan).toBeDefined();
    expect(afterSpan.status).toBe('ok');
    expect(afterSpan.attributes['hook.id']).toBe('after-test');
  });
});
```

---

**END OF TEST STRATEGY**

**Version:** latest
**Last Updated:** 2025-10-02
**Author:** UNRDF QA Team (Tester Agent)
**Status:** READY FOR IMPLEMENTATION
