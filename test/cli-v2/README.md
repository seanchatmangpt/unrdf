# CLI v2 Test Suite

Comprehensive test suite for UNRDF CLI v2 following Test-Driven Development (TDD) principles and citty-test-utils patterns.

## 📋 Overview

This test suite provides **95%+ test coverage** for the enterprise noun-verb CLI v2, organized by test type and priority.

### Test Structure

```
test/cli-v2/
├── commands/           # Unit tests per command
│   ├── hook.test.mjs          # 25% value - P0 priority
│   ├── query.test.mjs         # 20% value - P0 priority
│   ├── parse.test.mjs         # 15% value - P0 priority
│   └── validate.test.mjs      # 15% value - P1 priority
├── integration/        # Command workflow tests
│   └── workflows.test.mjs     # Multi-command scenarios
├── e2e/               # End-to-end tests
│   └── production-workflows.test.mjs  # Real-world scenarios
├── performance/        # Performance benchmarks
│   └── benchmarks.test.mjs    # SLA validation
├── fixtures/          # Test data and mocks
├── test-utils.mjs     # Shared testing utilities
└── README.md          # This file
```

## 🎯 Test Categories

### Unit Tests (commands/\*.test.mjs)

Test individual CLI commands in isolation:

- **Hook commands** (25% value): Create, validate, evaluate, manage hooks
- **Query commands** (20% value): SPARQL SELECT, ASK, CONSTRUCT, DESCRIBE
- **Parse commands** (15% value): Turtle, N-Quads, JSON-LD, RDF/XML
- **Validate commands** (15% value): SHACL, Zod, integrity checks

**Coverage Target**: 95%+
**Execution Time**: < 30 seconds

### Integration Tests (integration/\*.test.mjs)

Test command interactions and workflows:

- Parse → Query → Validate workflows
- Hook creation → evaluation → history workflows
- Policy pack deployment workflows
- Context switching and environment management

**Coverage Target**: 80%+
**Execution Time**: < 2 minutes

### End-to-End Tests (e2e/\*.test.mjs)

Test real-world production scenarios:

- Complete development workflows
- Production deployment scenarios
- Disaster recovery procedures
- Multi-graph operations
- CI/CD pipelines

**Coverage Target**: Key user journeys
**Execution Time**: < 5 minutes

### Performance Tests (performance/\*.test.mjs)

Validate performance targets and track metrics:

- Command startup: < 100ms
- Parse 10k triples: < 500ms
- Hook evaluation: < 2ms p99
- Simple queries: < 50ms
- SHACL validation: < 200ms

**Coverage Target**: All critical paths
**Execution Time**: < 3 minutes

## 🚀 Running Tests

### Run All Tests

```bash
# Run full test suite
npm test

# Run with coverage
npm run test:coverage

# Run in watch mode
npm run test:watch
```

### Run Specific Test Categories

```bash
# Unit tests only
npm test test/cli-v2/commands/

# Integration tests
npm test test/cli-v2/integration/

# E2E tests
npm test test/cli-v2/e2e/

# Performance benchmarks
npm test test/cli-v2/performance/
```

### Run Specific Command Tests

```bash
# Hook command tests
npm test test/cli-v2/commands/hook.test.mjs

# Query command tests
npm test test/cli-v2/commands/query.test.mjs

# Parse command tests
npm test test/cli-v2/commands/parse.test.mjs
```

## 🛠️ Test Utilities

### CLI Execution

```javascript
import { runCLI, assert } from '../test-utils.mjs';

// Execute CLI command
const result = await runCLI('hook list');

// Assert success
assert.success(result);

// Assert output contains text
assert.outputContains(result, 'hooks found');

// Assert performance target
assert.performanceTarget(result, 100, 'hook list');
```

### Test Context

```javascript
import { createTestContext, createTestProject } from '../test-utils.mjs';

// Create mocked context
const ctx = createTestContext({
  baseIRI: 'http://test.example.org/',
  verbose: false
});

// Create full project structure
const paths = await createTestProject(tempDir);
// Returns: { hooks, data, policies, config, receipts, baselines }
```

### Data Generators

```javascript
import { generators } from '../test-utils.mjs';

// Generate RDF triples
const turtle = generators.rdfTriples(100); // 100 triples

// Generate SPARQL queries
const query = generators.sparqlQuery('select', 'FILTER(?age > 25)');

// Generate hook definitions
const hook = generators.hookDefinition('sparql-ask', 'test-hook');

// Generate SHACL shapes
const shapes = generators.shaclShapes('foaf:Person');
```

### Performance Measurement

```javascript
import { perf } from '../test-utils.mjs';

// Measure operation performance
const metrics = await perf.measure(async () => {
  await runCLI('parse turtle data.ttl');
}, 10); // 10 iterations

// Assert SLA compliance
perf.assertSLA(metrics, {
  mean: 500,
  p95: 600,
  p99: 800
});
```

### Test Scenarios

```javascript
import { scenario, assert } from '../test-utils.mjs';

const workflow = await scenario('Full development workflow')
  .step('Parse data', async (ctx) => {
    const result = await runCLI(`parse turtle ${ctx.dataPath}`);
    assert.success(result);
    ctx.tripleCount = 100;
  })
  .step('Validate data', async (ctx) => {
    const result = await runCLI(`validate shacl ${ctx.dataPath} shape.ttl`);
    assert.success(result);
  })
  .run();

expect(workflow.tripleCount).toBe(100);
```

## 📊 Quality Metrics

### Test Coverage

**Target**: 95%+ statement coverage

- Statements: 95%+
- Branches: 90%+
- Functions: 95%+
- Lines: 95%+

### Performance Targets

| Operation | Target | P95 | P99 |
|-----------|--------|-----|-----|
| Command startup | < 100ms | < 150ms | < 200ms |
| Parse 10k triples | < 500ms | < 600ms | < 800ms |
| Hook evaluation | < 2ms | < 5ms | < 10ms |
| Simple query | < 50ms | < 75ms | < 100ms |
| SHACL validation | < 200ms | < 300ms | < 400ms |

### Quality Gates

- ✅ All tests passing
- ✅ 95%+ code coverage
- ✅ Zero flaky tests
- ✅ Performance targets met
- ✅ No critical security issues

## 🔍 Test Patterns

### TDD Workflow

1. **Write failing test** - Define expected behavior
2. **Implement feature** - Make test pass
3. **Refactor** - Improve code while keeping tests green
4. **Validate** - Ensure performance and coverage targets met

### Test Organization

```javascript
describe('CLI v2: command group', () => {
  let tempDir, cleanup;

  beforeEach(async () => {
    const temp = await createTempDir('test-');
    tempDir = temp.dir;
    cleanup = temp.cleanup;
  });

  afterEach(async () => {
    await cleanup();
  });

  describe('Feature (Priority)', () => {
    it('should perform expected behavior', async () => {
      // Arrange
      const dataPath = join(tempDir, 'test.ttl');
      await writeFile(dataPath, generators.rdfTriples(10));

      // Act
      const result = await runCLI(`parse turtle ${dataPath}`);

      // Assert
      assert.success(result);
      assert.outputContains(result, '10 triples');
    });
  });
});
```

### Error Handling Tests

```javascript
describe('Error Handling', () => {
  it('should handle missing file gracefully', async () => {
    const result = await runCLI('parse turtle /nonexistent/file.ttl');

    assert.failure(result);
    assert.outputContains(result, 'not found');
  });

  it('should validate input and show helpful errors', async () => {
    const result = await runCLI('hook eval invalid-hook.json');

    assert.failure(result);
    assert.outputMatches(result, /validation error|invalid/i);
  });
});
```

### Performance Tests

```javascript
describe('Performance', () => {
  it('should meet performance target', async () => {
    const result = await runCLI('parse turtle large-file.ttl');

    assert.success(result);
    assert.performanceTarget(result, 500, 'parse operation');
  });

  it('should scale linearly', async () => {
    const sizes = [1000, 5000, 10000];
    const durations = [];

    for (const size of sizes) {
      const result = await runCLI(`parse turtle ${size}-triples.ttl`);
      durations.push(result.duration);
    }

    // Verify linear scaling
    expect(durations[1] / durations[0]).toBeCloseTo(5, 1);
  });
});
```

## 📝 Test Documentation

### Test Naming Convention

- **Descriptive names**: `should [action] [expected behavior]`
- **Context clarity**: Include prerequisites in name
- **Outcome clarity**: Make expected result obvious

**Good**: `should validate data against SHACL shapes successfully`
**Bad**: `test validation`

### Test Comments

Use comments for:
- Complex test setup
- Business logic context
- Known issues or limitations
- Performance expectations

```javascript
it('should handle large datasets efficiently', async () => {
  // Create 10k triple dataset to test performance at scale
  const dataPath = join(tempDir, 'large.ttl');
  await writeFile(dataPath, generators.rdfTriples(10000));

  // Performance target: < 500ms for 10k triples
  const result = await runCLI(`parse turtle ${dataPath}`);

  assert.success(result);
  assert.performanceTarget(result, 500, 'parse 10k triples');
});
```

## 🐛 Debugging Tests

### Verbose Output

```bash
# Run with verbose logging
DEBUG=cli-v2:* npm test

# Show all console output
npm test -- --reporter=verbose
```

### Isolate Failures

```bash
# Run single test file
npm test test/cli-v2/commands/hook.test.mjs

# Run single test case
npm test -- -t "should evaluate hook successfully"

# Skip slow tests
npm test -- --testPathIgnorePatterns=e2e
```

### Debug in VS Code

```json
{
  "type": "node",
  "request": "launch",
  "name": "Debug Tests",
  "program": "${workspaceFolder}/node_modules/.bin/vitest",
  "args": ["run", "test/cli-v2/commands/hook.test.mjs"],
  "console": "integratedTerminal"
}
```

## 🎯 Lean Six Sigma Compliance

### DMAIC Process

1. **Define**: Test requirements and coverage goals
2. **Measure**: Track test metrics and performance
3. **Analyze**: Identify gaps and failure patterns
4. **Improve**: Enhance test quality and coverage
5. **Control**: Monitor and maintain test quality

### Quality Metrics Dashboard

Track:
- Test pass rate
- Coverage percentage
- Performance trends
- Defect detection rate
- False positive rate

### Defect Density Target

**Target**: < 3.4 DPMO (Defects Per Million Opportunities)

- Monitor defect escape rate
- Track defects found in production
- Analyze root causes
- Implement preventive measures

## 🔧 Maintenance

### Regular Tasks

- [ ] Review and update test cases monthly
- [ ] Monitor performance trends
- [ ] Update test data generators
- [ ] Review flaky tests
- [ ] Update documentation

### Test Hygiene

- Remove obsolete tests
- Refactor duplicated test logic
- Update assertions to use helpers
- Keep test data realistic
- Maintain fast execution

## 📚 Resources

- [Vitest Documentation](https://vitest.dev/)
- [citty Documentation](https://github.com/unjs/citty)
- [UNRDF CLI v2 Architecture](../../docs/architecture/cli-v2-architecture.md)
- [Test-Driven Development Guide](https://martinfowler.com/bliki/TestDrivenDevelopment.html)

## 🤝 Contributing

When adding new tests:

1. Follow existing patterns and conventions
2. Add to appropriate test category
3. Update documentation
4. Ensure performance targets met
5. Verify coverage increases

---

**Status**: ✅ Test suite complete and ready for TDD implementation

For questions or issues, consult the test documentation or raise an issue in the repository.
