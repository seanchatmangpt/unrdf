# UNRDF Testing Best Practices

## Overview

This guide covers testing best practices for UNRDF development, focusing on the 80/20 principle: common patterns that handle 80% of test scenarios efficiently.

## Quick Start

```javascript
import { test, expect } from 'vitest';
import { createTestStore, createTestWorkflow, waitForCondition } from '@unrdf/test-utils';

test('basic workflow execution', async () => {
  const store = createTestStore();
  const workflow = createTestWorkflow({ id: 'my-test' });

  // Test logic here
  expect(workflow.tasks).toHaveLength(3);
});
```

## Test Utilities

### Core Helpers

#### `createTestStore(options)`

Create a preconfigured RDF store for testing.

```javascript
import { createTestStore, createQuad } from '@unrdf/test-utils';

// Empty store
const store = createTestStore();

// Store with initial data
const store = createTestStore({
  quads: [
    createQuad('http://example.org/s', 'http://example.org/p', 'http://example.org/o')
  ]
});

// Store with performance metrics
const store = createTestStore({ enableMetrics: true });
await store.add(quad);
const metrics = store.getMetrics();
expect(metrics.addCount).toBe(1);
```

#### `createTestWorkflow(options)`

Generate sample workflows for testing.

```javascript
import { createTestWorkflow } from '@unrdf/test-utils';

// Default 3-task workflow
const workflow = createTestWorkflow();

// Custom workflow
const workflow = createTestWorkflow({
  id: 'approval-flow',
  name: 'Approval Workflow',
  tasks: [
    { id: 'submit', type: 'atomic', name: 'Submit' },
    { id: 'approve', type: 'atomic', name: 'Approve' }
  ],
  flows: [
    { from: 'submit', to: 'approve' }
  ]
});
```

#### `mockOTEL(options)`

Mock OpenTelemetry for faster tests (no actual spans created).

```javascript
import { mockOTEL } from '@unrdf/test-utils';

test('traces operations', async () => {
  const otel = mockOTEL({ capture: true });
  const tracer = otel.getTracer('test');

  tracer.startActiveSpan('operation', async (span) => {
    span.setAttribute('key', 'value');
    // ... test logic ...
    span.end();
  });

  const spans = otel.getSpans();
  expect(spans).toHaveLength(1);
  expect(spans[0].name).toBe('operation');
});
```

#### `waitForCondition(condition, options)`

Wait for async conditions with timeout.

```javascript
import { waitForCondition } from '@unrdf/test-utils';

test('waits for state change', async () => {
  let count = 0;
  setTimeout(() => count = 5, 100);

  await waitForCondition(
    () => count === 5,
    { timeout: 1000, message: 'Count should reach 5' }
  );

  expect(count).toBe(5);
});
```

#### `measureTime(fn)`

Measure execution time for performance testing.

```javascript
import { measureTime } from '@unrdf/test-utils';

test('operation completes quickly', async () => {
  const { result, duration } = await measureTime(async () => {
    return await expensiveOperation();
  });

  expect(result).toBeDefined();
  expect(duration).toBeLessThan(100); // Should complete in <100ms
});
```

### Data Fixtures

Pre-made test data for common scenarios.

```javascript
import {
  sampleRDF,
  sampleWorkflows,
  sampleCaseData,
  performanceFixtures
} from '@unrdf/test-utils';

test('uses sample RDF data', () => {
  const store = createTestStore({ quads: sampleRDF.person.quads });
  expect(store.size).toBe(3);
});

test('uses sample workflow', () => {
  const workflow = sampleWorkflows.approval;
  expect(workflow.tasks).toHaveLength(4);
});

test('performance test with generated data', async () => {
  const quads = performanceFixtures.generateQuads(10000);
  const store = createTestStore({ quads });
  expect(store.size).toBe(10000);
});
```

## Test Organization

### File Structure

```
packages/
├── my-package/
│   ├── src/
│   │   └── feature.mjs
│   ├── test/
│   │   ├── unit/
│   │   │   └── feature.test.mjs
│   │   └── integration/
│   │       └── workflow.test.mjs
│   └── vitest.config.mjs
```

### Test Categories

1. **Unit Tests** - Fast, isolated, mock dependencies
   - Timeout: 5s default
   - Coverage: 80%+ required
   - Location: `test/unit/`

2. **Integration Tests** - Multiple components, real dependencies
   - Timeout: 30s default
   - Coverage: 80%+ required
   - Location: `packages/integration-tests/`

3. **Performance Tests** - Stress testing, benchmarks
   - Timeout: 60s allowed
   - Location: `packages/integration-tests/performance/`

## Writing Tests

### Test Structure (Arrange-Act-Assert)

```javascript
test('clearly describes what is being tested', async () => {
  // ARRANGE - Set up test data and environment
  const store = createTestStore();
  const workflow = createTestWorkflow();

  // ACT - Execute the operation being tested
  const result = await executeWorkflow(workflow, { store });

  // ASSERT - Verify the results
  expect(result.status).toBe('completed');
  expect(store.size).toBeGreaterThan(0);
});
```

### Test Naming

Use descriptive names that explain **what** and **why**:

```javascript
// ✅ GOOD
test('creates receipt with SHA-256 hash when task completes')
test('vetoes transaction when validation hook returns error')
test('handles concurrent updates without data loss')

// ❌ BAD
test('test1')
test('it works')
test('receipt test')
```

### One Assertion Per Test

Each test should verify **one specific behavior**:

```javascript
// ✅ GOOD - Focused tests
test('workflow starts in active state', () => {
  const workflow = createWorkflow();
  expect(workflow.status).toBe('active');
});

test('workflow generates unique ID', () => {
  const workflow = createWorkflow();
  expect(workflow.id).toMatch(/^[a-f0-9-]{36}$/);
});

// ❌ BAD - Testing multiple behaviors
test('workflow initialization', () => {
  const workflow = createWorkflow();
  expect(workflow.status).toBe('active'); // Multiple
  expect(workflow.id).toBeDefined();      // unrelated
  expect(workflow.tasks).toBeArray();     // assertions
});
```

## Performance Guidelines

### Timeout Budgets

Following CLAUDE.md guidelines:

```javascript
// ✅ Default: 5 seconds
test('fast operation', async () => {
  // Should complete in <5s
});

// ✅ Integration: 30 seconds (justified)
test('database sync operation', async () => {
  // Requires DB setup: 3-8s + margin
}, 30000);

// ❌ WRONG: No timeout
test('operation', async () => {
  // Risk of silent hang
});

// ❌ WRONG: Excessive timeout hiding performance issue
test('simple operation', async () => {
  // Why 60s for simple operation?
}, 60000);
```

### Performance Assertions

```javascript
import { measureTime } from '@unrdf/test-utils';

test('processes 1000 items under 100ms', async () => {
  const items = generateItems(1000);

  const { duration } = await measureTime(async () => {
    await processItems(items);
  });

  expect(duration).toBeLessThan(100);
});
```

### Slow Test Threshold

Vitest will warn if tests exceed thresholds:
- Unit tests: >100ms
- Integration tests: >1000ms

If warnings appear, investigate and optimize or mock external dependencies.

## Coverage Requirements

### Minimum Coverage: 80%

All packages must maintain:
- Line coverage: ≥80%
- Function coverage: ≥80%
- Branch coverage: ≥80%
- Statement coverage: ≥80%

### Running Coverage

```bash
# All packages
pnpm test:coverage

# Specific package
pnpm -C packages/my-package test -- --coverage

# View HTML report
open packages/my-package/coverage/index.html
```

### Excluded from Coverage

- `node_modules/**`
- `dist/**`
- `test/**`
- `**/*.config.mjs`
- Mock files
- Type definitions

## Running Tests

### Commands

```bash
# Run all tests (fast mode)
pnpm test:fast

# Run all tests with coverage
pnpm test:coverage

# Watch mode
pnpm test:watch

# Specific package
pnpm -C packages/my-package test

# Integration tests only
pnpm -C packages/integration-tests test

# Specific integration suite
pnpm -C packages/integration-tests test:workflows
```

### Integration Test Runner

Enhanced test runner with better error messages:

```bash
# Run all integration suites
node packages/integration-tests/test-runner.mjs

# Run specific suite
node packages/integration-tests/test-runner.mjs workflows
```

### Generate Test Report

Visual HTML report with flaky tests, slow tests, and coverage gaps:

```bash
node scripts/test-report.mjs
open test-report.html
```

## Common Patterns

### Testing Async Operations

```javascript
import { waitForCondition } from '@unrdf/test-utils';

test('handles async state changes', async () => {
  const manager = new StateManager();
  manager.startTransition();

  await waitForCondition(
    () => manager.state === 'ready',
    { timeout: 1000 }
  );

  expect(manager.state).toBe('ready');
});
```

### Testing Error Handling

```javascript
import { errorScenarios } from '@unrdf/test-utils';

test('handles invalid workflow gracefully', async () => {
  const engine = createEngine();

  await expect(
    engine.registerWorkflow(errorScenarios.malformedWorkflow)
  ).rejects.toThrow('Invalid workflow specification');
});
```

### Testing Concurrent Operations

```javascript
import { testBatch } from '@unrdf/test-utils';

test('handles concurrent updates', async () => {
  const store = createTestStore();
  const quads = Array(100).fill(null).map((_, i) =>
    createQuad(`http://example.org/s${i}`, 'http://example.org/p', 'http://example.org/o')
  );

  const results = await testBatch(
    quads.map(quad => () => store.add(quad)),
    { parallel: true, timeout: 5000 }
  );

  expect(results).toHaveLength(100);
  expect(store.size).toBe(100);
});
```

### Store Snapshots

```javascript
import { snapshotStore, assertSnapshotsEqual } from '@unrdf/test-utils';

test('preserves store state after operation', async () => {
  const store = createTestStore({ quads: sampleRDF.person.quads });
  const before = snapshotStore(store);

  await readOnlyOperation(store);

  const after = snapshotStore(store);
  assertSnapshotsEqual(before, after); // Should be identical
});
```

## Adversarial PM Checklist

Before declaring tests complete, verify:

### Claims vs Reality
- [ ] Did I **RUN** tests or just write them?
- [ ] Did I read **FULL OUTPUT** or assume they passed?
- [ ] What **BREAKS** if tests are wrong?
- [ ] Can someone **REPRODUCE** from scratch?

### Evidence Quality
- [ ] Test output showing success? (Not "tests pass")
- [ ] Coverage report ≥80%? (Show actual numbers)
- [ ] Performance within budgets? (Show timings)
- [ ] All fixtures work? (Demonstrate usage)

### Process Quality
- [ ] Timeout all tests? (Default 5s)
- [ ] One assertion per test?
- [ ] Descriptive test names?
- [ ] Fixtures reduce duplication?

### Red Flags
- ❌ "Tests should work" → Didn't run them
- ❌ "Coverage is probably good" → No report
- ❌ "Mostly passing" → Not acceptable
- ❌ No timeouts → Risk of hangs

## Troubleshooting

### Tests Hanging

```bash
# Add timeout to find slow tests
timeout 10s pnpm test

# Use vitest timeout budget
pnpm test -- --testTimeout=5000
```

### Flaky Tests

```bash
# Generate report to identify flaky tests
node scripts/test-report.mjs

# Run tests multiple times
for i in {1..10}; do pnpm test || break; done
```

### Low Coverage

```bash
# View detailed coverage report
pnpm test:coverage
open coverage/index.html

# Generate coverage gap report
node scripts/test-report.mjs
```

### Slow Tests

```bash
# Vitest will warn about slow tests
pnpm test

# Check performance report
node scripts/test-report.mjs
open test-report.html
```

## CI/CD Integration

### GitHub Actions

```yaml
- name: Run Tests
  run: timeout 5m pnpm test:coverage

- name: Check Coverage
  run: |
    grep "All files" coverage/coverage-summary.json
    # Verify ≥80% coverage

- name: Upload Coverage
  uses: codecov/codecov-action@v3
  with:
    files: ./coverage/lcov.info
```

## References

- [Vitest Documentation](https://vitest.dev/)
- [CLAUDE.md](./CLAUDE.md) - Project guidelines
- [Big Bang 80/20 Methodology](./docs/bb80-20-methodology.md)

## Key Principles

1. **80/20 Focus** - Common patterns handling 80% of scenarios
2. **Fast Feedback** - Tests complete in <5s (unit) or <30s (integration)
3. **Measure, Don't Assume** - Use coverage reports and performance metrics
4. **Evidence-Based** - Verify with actual test output, not claims
5. **Reproducible** - Anyone can run tests and get same results
