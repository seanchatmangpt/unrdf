# UNRDF Integration Tests

Comprehensive integration tests for multi-package workflows across the UNRDF ecosystem.

## Overview

This package contains **5 real-world integration test scenarios** that validate the interaction between:

- **@unrdf/yawl** - Workflow engine
- **@unrdf/hooks** - Hook system
- **@unrdf/kgc-4d** - Time-travel and event sourcing
- **@unrdf/federation** - Federated query coordination
- **@unrdf/streaming** - RDF stream processing
- **@unrdf/oxigraph** - RDF triple store

## Test Scenarios

### Scenario 1: Complete Workflow Execution

**Location**: `workflows/complete-workflow.test.mjs`

**Tests**: YAWL + Hooks + KGC-4D + Receipts

**Real-world use case**: Document approval workflow with validation hooks, time-travel snapshots, receipt verification, and audit trail generation.

**Key validations**:
- ✅ Workflow engine initialization
- ✅ Task flow execution (submit → review → approve)
- ✅ Hook-based data validation
- ✅ Receipt chain integrity
- ✅ Time-travel snapshots (before/after review)
- ✅ State reconstruction
- ✅ Complete audit trail
- ✅ Performance (<30s execution)

### Scenario 2: Federated Knowledge Query

**Location**: `federation/federated-query.test.mjs`

**Tests**: Federation + Multiple Stores + SPARQL

**Real-world use case**: Query across distributed knowledge graphs (HR, Locations, Projects).

**Key validations**:
- ✅ Multiple RDF stores populated
- ✅ Cross-store manual federation
- ✅ Join operations (employees + locations + projects)
- ✅ Data integrity across stores
- ✅ Query performance (<1s for 100 queries)
- ✅ Missing data handling

### Scenario 3: Stream Processing with Validation

**Location**: `streaming/stream-validation.test.mjs`

**Tests**: Streaming + Hooks + Validation

**Real-world use case**: Real-time RDF stream processing with hook-based validation.

**Key validations**:
- ✅ Change stream monitoring
- ✅ IRI format validation hook
- ✅ Blank node rejection hook
- ✅ Invalid quad detection
- ✅ High-throughput streaming (>100 quads/s)
- ✅ Validation error handling

### Scenario 4: Multi-Package Error Recovery

**Location**: `error-recovery/multi-package-errors.test.mjs`

**Tests**: Error handling across YAWL + KGC-4D + Hooks

**Real-world use case**: Robust error handling and recovery in payment processing.

**Key validations**:
- ✅ Payment validation hooks (amount limits, fraud checks)
- ✅ Multiple error scenarios
- ✅ State snapshots for rollback
- ✅ Error propagation across packages
- ✅ Concurrent workflow independence
- ✅ Successful completion after validation

### Scenario 5: Performance Under Load

**Location**: `performance/load-testing.test.mjs`

**Tests**: All packages under high load

**Real-world use case**: System performance validation under stress.

**Key validations**:
- ✅ High-volume workflow execution (100+ cases)
- ✅ Workflow throughput (>2 complete workflows/s)
- ✅ RDF write throughput (>1000 quads/s)
- ✅ RDF read throughput (>500 reads/s)
- ✅ Hook execution performance (>1000 executions/s)
- ✅ Batch hook optimization
- ✅ Memory efficiency (<500MB for 50k quads, <10KB/quad)
- ✅ Concurrent workflows with snapshots (50 concurrent)

## Running Tests

### All Tests

```bash
pnpm test
```

### By Scenario

```bash
# Workflow execution
pnpm run test:workflows

# Federation
pnpm run test:federation

# Streaming
pnpm run test:streaming

# Error recovery
pnpm run test:error-recovery

# Performance
pnpm run test:performance
```

### With Coverage

```bash
pnpm run test:coverage
```

### Watch Mode

```bash
pnpm run test:watch
```

## Success Criteria

All integration tests must meet these criteria:

### Functionality
- ✅ **All tests pass** - 100% pass rate
- ✅ **Real-world scenarios** - Tests reflect actual usage patterns
- ✅ **Multi-package integration** - Tests cross package boundaries
- ✅ **Error handling** - Edge cases and failures tested

### Performance
- ✅ **Test completion** - All tests complete within 30s timeout (except load tests: 60s)
- ✅ **Workflow throughput** - ≥2 complete workflows/second
- ✅ **RDF operations** - ≥1000 writes/s, ≥500 reads/s
- ✅ **Hook execution** - ≥1000 executions/second
- ✅ **Memory efficiency** - <500MB for 50k quads

### Coverage
- ✅ **Critical path coverage** - ≥70% of critical integration paths
- ✅ **Package interactions** - All major package combinations tested
- ✅ **Error scenarios** - Failure modes validated

## Test Architecture

### Test Structure

```
integration-tests/
├── workflows/           # YAWL + Hooks + KGC-4D
├── federation/          # Multi-store queries
├── streaming/           # Stream + validation
├── error-recovery/      # Error handling
├── performance/         # Load testing
├── package.json
├── vitest.config.mjs
└── README.md
```

### Dependencies

All integration tests use:
- **Vitest** - Test framework
- **@unrdf/*** - UNRDF packages
- **Zod** - Schema validation

### Timeouts

- **Default**: 30s for integration tests
- **Performance tests**: 60s for load scenarios
- **Unit operations**: <5s expected

## CI/CD Integration

### GitHub Actions Workflow

See `.github/workflows/integration-tests.yml`

### When Tests Run

- ✅ On pull requests
- ✅ On main branch commits
- ✅ Nightly scheduled runs
- ✅ Manual trigger

### Reporting

- ✅ Test results summary
- ✅ Coverage reports
- ✅ Performance metrics
- ✅ Failure notifications

## Troubleshooting

### Tests Timing Out

If tests timeout:
1. Check system resources (CPU, memory)
2. Run individual scenarios: `pnpm run test:workflows`
3. Increase timeout in `vitest.config.mjs` (if justified)
4. Check for deadlocks or infinite loops

### Coverage Too Low

If coverage <70%:
1. Review which critical paths are missing
2. Add targeted integration tests
3. Verify test scenarios reflect real usage
4. Check that mocks aren't hiding integration issues

### Performance Degradation

If performance metrics fail:
1. Profile with `node --prof`
2. Check memory leaks with `--expose-gc`
3. Compare baseline metrics
4. Investigate recent package changes

## Contributing

When adding new integration tests:

1. **Follow naming**: `<scenario-name>.test.mjs`
2. **Document thoroughly**: Add scenario description
3. **Verify success criteria**: Include explicit validation steps
4. **Test locally first**: `pnpm test` before PR
5. **Update README**: Document new scenarios
6. **Measure performance**: Include timing validations

## Metrics

### Current Performance Baselines

| Metric | Target | Typical |
|--------|--------|---------|
| Workflow Throughput | ≥2/s | ~5-10/s |
| RDF Write | ≥1000/s | ~2000-5000/s |
| RDF Read | ≥500/s | ~1000-2000/s |
| Hook Execution | ≥1000/s | ~2000-5000/s |
| Memory/Quad | <10KB | ~5-8KB |
| Test Completion | <30s | ~10-20s |

### Coverage Goals

| Package | Critical Path Coverage |
|---------|------------------------|
| @unrdf/yawl | ≥70% |
| @unrdf/hooks | ≥70% |
| @unrdf/kgc-4d | ≥70% |
| @unrdf/federation | ≥70% |
| @unrdf/streaming | ≥70% |
| **Overall** | **≥70%** |

## License

MIT - Same as UNRDF workspace

## Questions?

- Check test output for detailed error messages
- Review scenario documentation above
- See individual test files for step-by-step validation
- Open issue if tests fail unexpectedly
