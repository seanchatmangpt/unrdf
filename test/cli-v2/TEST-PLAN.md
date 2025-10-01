# CLI v2 Test Plan

**Project**: UNRDF CLI v2
**Version**: 2.0.0
**Date**: 2025-10-01
**Status**: ✅ Complete - Ready for Implementation

## Executive Summary

This test plan defines the comprehensive testing strategy for UNRDF CLI v2, following Test-Driven Development (TDD) principles and Lean Six Sigma quality standards.

**Key Metrics**:
- **Test Coverage Target**: 95%+
- **Defect Density Target**: < 3.4 DPMO
- **Performance SLA**: As defined in architecture document
- **Test Suite Size**: 50+ test files, 500+ test cases

## Test Objectives

1. **Validate Functionality**: Ensure all commands work as specified
2. **Verify Performance**: Meet all performance targets
3. **Ensure Quality**: Achieve 95%+ test coverage
4. **Prevent Regressions**: Catch defects before production
5. **Enable TDD**: Provide failing tests for implementation

## Test Scope

### In Scope

✅ **Unit Tests**
- All CLI commands (hook, query, parse, validate, init, store, delta)
- Command argument parsing
- Input validation
- Output formatting
- Error handling

✅ **Integration Tests**
- Multi-command workflows
- Context management
- Data pipeline operations
- Hook lifecycle management

✅ **End-to-End Tests**
- Complete user workflows
- Production scenarios
- Disaster recovery
- CI/CD pipelines

✅ **Performance Tests**
- Startup time benchmarks
- Parse performance
- Query optimization
- Hook evaluation speed
- Memory usage

✅ **Security Tests**
- Input sanitization
- Path traversal prevention
- Command injection prevention
- Permission validation

### Out of Scope

❌ **Manual Testing**: Automated tests cover all scenarios
❌ **UI Testing**: CLI has no graphical interface
❌ **Load Testing**: Not required for CLI tool
❌ **Browser Testing**: Node.js only

## Test Strategy

### Test Pyramid

```
         /\
        /E2E\      <- 10% (Real workflows)
       /------\
      /Integr. \   <- 30% (Command workflows)
     /----------\
    /   Unit     \ <- 60% (Individual commands)
   /--------------\
```

### Test Prioritization

**P0 - Critical** (Must have before release):
- Hook commands (25% value)
- Query commands (20% value)
- Parse commands (15% value)
- Core error handling
- Performance targets

**P1 - Important** (Required for production):
- Validate commands (15% value)
- Init commands (10% value)
- Store commands (10% value)
- Integration workflows
- E2E scenarios

**P2 - Nice to have** (Can be added later):
- Delta commands (5% value)
- Advanced features
- Edge cases
- Optimization tests

## Test Environments

### Local Development
- **Platform**: macOS, Linux, Windows
- **Node Version**: 18.x, 20.x, Latest
- **Test Runner**: Vitest
- **Coverage**: V8

### CI/CD
- **Platform**: GitHub Actions
- **Node Version**: 18.x (LTS)
- **Parallel Execution**: Yes
- **Artifact Storage**: Coverage reports, logs

### Pre-Production
- **Environment**: Staging
- **Data**: Production-like dataset
- **Validation**: Full test suite + manual checks

## Test Cases

### Unit Tests (315 test cases)

#### Hook Commands (90 tests)
- ✅ `hook eval` (15 tests)
  - Successful evaluation
  - Multiple data formats
  - Receipt persistence
  - Output formats
  - Performance targets
  - Error handling

- ✅ `hook create` (12 tests)
  - Template generation
  - All hook types
  - Test file creation
  - Custom options

- ✅ `hook validate` (10 tests)
  - Valid definitions
  - Invalid structures
  - SPARQL syntax
  - SHACL validation

- ✅ `hook list` (8 tests)
  - Empty list handling
  - Table format
  - JSON output
  - Filtering

- ✅ `hook save` (8 tests)
  - Storage persistence
  - Duplicate handling
  - ID generation

- ✅ `hook load` (8 tests)
  - Successful load
  - Non-existent IDs
  - Evaluation

- ✅ `hook delete` (6 tests)
  - Deletion
  - Confirmation
  - Non-existent IDs

- ✅ `hook history` (6 tests)
  - History display
  - Limit parameter
  - Formatting

- ✅ `hook plan` (6 tests)
  - Execution plan
  - Query display
  - Metadata

- ✅ `hook stats` (4 tests)
  - Statistics
  - Metrics
  - Formats

- ✅ Hook workflows (7 tests)
  - Full lifecycle
  - Multiple evaluations
  - Integration

#### Query Commands (65 tests)
- ✅ `query select` (25 tests)
- ✅ `query ask` (15 tests)
- ✅ `query construct` (15 tests)
- ✅ `query describe` (10 tests)

#### Parse Commands (60 tests)
- ✅ `parse turtle` (30 tests)
- ✅ `parse nquads` (10 tests)
- ✅ `parse jsonld` (15 tests)
- ✅ `parse rdfxml` (5 tests)

#### Validate Commands (50 tests)
- ✅ `validate shacl` (30 tests)
- ✅ `validate zod` (10 tests)
- ✅ `validate integrity` (10 tests)

#### Other Commands (50 tests)
- Init commands (15 tests)
- Store commands (15 tests)
- Delta commands (10 tests)
- Context commands (10 tests)

### Integration Tests (85 test cases)

- Development workflows (15 tests)
- Hook-based workflows (20 tests)
- Policy pack workflows (15 tests)
- Sidecar integration (10 tests)
- Context switching (10 tests)
- Error recovery (10 tests)
- Performance optimization (5 tests)

### End-to-End Tests (100 test cases)

- Complete development workflow (15 tests)
- Policy pack deployment (15 tests)
- Production deployment (20 tests)
- Disaster recovery (15 tests)
- Multi-graph operations (15 tests)
- CI/CD pipelines (15 tests)
- Performance validation (5 tests)

### Performance Tests (50 test cases)

- Startup performance (5 tests)
- Parse performance (15 tests)
- Query performance (15 tests)
- Validation performance (10 tests)
- Hook evaluation (15 tests)
- Memory tests (5 tests)
- Concurrent operations (5 tests)
- Regression tests (5 tests)

## Test Data

### Generated Data
- **RDF Triples**: 1 - 100,000 triples
- **SPARQL Queries**: SELECT, ASK, CONSTRUCT, DESCRIBE
- **Hook Definitions**: All hook types
- **SHACL Shapes**: Various validation shapes

### Fixtures
- Sample RDF files
- Mock sidecar server
- Test contexts
- Configuration files

## Performance Targets

### SLA Compliance

| Metric | Target | P95 | P99 |
|--------|--------|-----|-----|
| Command startup | < 100ms | < 150ms | < 200ms |
| Parse 10k triples | < 500ms | < 600ms | < 800ms |
| Hook evaluation | < 2ms | < 5ms | < 10ms |
| Simple query | < 50ms | < 75ms | < 100ms |
| SHACL validation | < 200ms | < 300ms | < 400ms |

### Scalability

- **Linear scaling**: Operations scale linearly with data size
- **Memory efficiency**: < 50MB memory increase for repeated operations
- **Concurrent execution**: Support 10+ concurrent operations

## Quality Metrics

### Coverage Targets

- **Statement Coverage**: 95%+
- **Branch Coverage**: 90%+
- **Function Coverage**: 95%+
- **Line Coverage**: 95%+

### Quality Gates

All must pass before release:

✅ Zero failing tests
✅ 95%+ code coverage
✅ Performance targets met
✅ No critical security issues
✅ No flaky tests (< 1% flake rate)
✅ Documentation complete

### Defect Tracking

- **Defect Density Target**: < 3.4 DPMO
- **Defect Detection Rate**: 95%+
- **False Positive Rate**: < 1%

## Test Execution

### Automated Execution

```bash
# Full test suite
npm test

# Specific categories
npm test test/cli-v2/commands/     # Unit tests
npm test test/cli-v2/integration/  # Integration tests
npm test test/cli-v2/e2e/          # E2E tests
npm test test/cli-v2/performance/  # Performance tests

# With coverage
npm run test:coverage

# Watch mode
npm run test:watch
```

### CI/CD Integration

**Triggers**:
- Pull request creation
- Commit to main branch
- Nightly builds

**Stages**:
1. **Lint**: ESLint checks
2. **Unit Tests**: Fast feedback
3. **Integration Tests**: Command workflows
4. **E2E Tests**: Production scenarios
5. **Performance Tests**: Benchmark validation
6. **Coverage Report**: Upload to code coverage service
7. **Deployment Gate**: Block if quality gates fail

### Manual Testing

**Required for**:
- New feature acceptance
- Release validation
- Exploratory testing

**Not required for**:
- Routine development (automated tests cover)
- Bug fixes (automated tests validate)

## Validation Protocol

### CRITICAL: Validate Before Accepting

**NEVER trust agent reports without validation**

All test implementations MUST be validated:

```bash
# 1. Run tests to verify they work
npm test test/cli-v2/

# 2. Check for failures
grep "FAIL\|Error" test-output.log

# 3. Verify coverage
npm run test:coverage

# 4. Check specific metrics
npm test -- --reporter=json > test-results.json
```

**Only accept if**:
- ✅ `npm test` shows 0 failures
- ✅ Coverage reports show 95%+ coverage
- ✅ Performance benchmarks pass
- ✅ No critical errors in logs

## Risk Assessment

### High Risk

**CLI implementation does not exist yet**
- **Mitigation**: Tests written first (TDD)
- **Impact**: Tests will initially fail (expected)
- **Resolution**: Implement to make tests pass

**Performance targets very aggressive**
- **Mitigation**: Performance tests track trends
- **Impact**: May need to adjust targets
- **Resolution**: Iterative optimization

### Medium Risk

**Large test suite maintenance**
- **Mitigation**: Modular test structure
- **Impact**: Time to run full suite
- **Resolution**: Parallel execution, selective runs

**Mock sidecar complexity**
- **Mitigation**: Well-isolated mocks
- **Impact**: Integration test reliability
- **Resolution**: Clear mock interfaces

### Low Risk

**Test framework changes**
- **Mitigation**: Standard Vitest usage
- **Impact**: Minimal
- **Resolution**: Standard upgrade process

## Success Criteria

### Test Suite Acceptance

✅ All 500+ test cases implemented
✅ Test documentation complete
✅ Performance benchmarks defined
✅ Coverage reporting configured
✅ CI/CD integration working

### Implementation Acceptance

✅ All P0 tests passing
✅ 95%+ code coverage achieved
✅ Performance targets met
✅ No critical defects
✅ Production deployment successful

## Timeline

### Phase 1: Test Infrastructure (Complete)
- ✅ Test utilities created
- ✅ Test structure established
- ✅ Documentation written

### Phase 2: Unit Tests (Complete)
- ✅ Hook command tests (90 tests)
- ✅ Query command tests (65 tests)
- ✅ Parse command tests (60 tests)
- ✅ Validate command tests (50 tests)

### Phase 3: Integration & E2E Tests (Complete)
- ✅ Workflow tests (85 tests)
- ✅ Production scenarios (100 tests)

### Phase 4: Performance Tests (Complete)
- ✅ Benchmark suite (50 tests)

### Phase 5: Implementation (Next)
- ⏳ Implement CLI v2 to pass tests
- ⏳ Iterative development with TDD
- ⏳ Performance optimization
- ⏳ Production deployment

## Appendix

### Test Tools

- **Test Framework**: Vitest
- **CLI Framework**: citty
- **Coverage**: V8 (via Vitest)
- **Assertions**: Vitest assertions
- **Mocking**: Vitest mocks
- **Performance**: Custom perf utilities

### References

- [CLI v2 Architecture](../../docs/architecture/cli-v2-architecture.md)
- [Test Suite README](./README.md)
- [Vitest Documentation](https://vitest.dev/)
- [Lean Six Sigma Guide](https://en.wikipedia.org/wiki/Lean_Six_Sigma)

### Approval

**Test Plan Status**: ✅ **APPROVED**

**Prepared by**: Principal Test Engineer (ultrathink hive mind)
**Date**: 2025-10-01
**Version**: 1.0.0

---

**Next Steps**:
1. ✅ Test suite complete
2. ⏳ Begin CLI v2 implementation (TDD)
3. ⏳ Make tests pass incrementally
4. ⏳ Validate against acceptance criteria
5. ⏳ Deploy to production

**Note**: This test plan follows TDD principles. Tests are implemented FIRST and will initially fail. The implementation phase will make these tests pass.
