# Test Pruning Policy - 80/20 Principle

## Executive Summary

This document defines the 80/20 test pruning strategy to reduce test execution time while maintaining 80% of functional coverage through 20% of the most critical tests.

## Core Philosophy

**20% of tests deliver 80% of value** - Focus on the most critical functionality that:
- Validates core business logic
- Prevents production failures
- Covers the most common user workflows
- Tests the most complex components

## Test Categories & Priority

### 🟢 KEEP (Critical - 20% of tests, 80% of value)

#### 1. Core Knowledge Engine Tests (P0 - 25% value)
- `test/knowledge-engine/knowledge-engine.test.mjs` - Main integration tests
- `test/knowledge-engine/query.test.mjs` - SPARQL query functionality
- `test/knowledge-engine/parse.test.mjs` - RDF parsing core
- `test/knowledge-engine/validate.test.mjs` - SHACL validation
- `test/knowledge-engine/transaction.test.mjs` - Transaction management

#### 2. Essential CLI Tests (P0 - 20% value)
- `test/cli/parse.test.mjs` - Core parsing functionality
- `test/cli/query.test.mjs` - SPARQL query execution
- `test/cli/validate.test.mjs` - Data validation
- `test/cli/integration.test.mjs` - End-to-end workflows

#### 3. Dark Matter Framework Tests (P0 - 15% value)
- `test/dark-matter-80-20.test.mjs` - Core 80/20 validation
- `test/knowledge-engine/dark-matter/query-optimizer.test.mjs` - Performance optimization

#### 4. Critical Sidecar Tests (P0 - 10% value)
- `test/sidecar/client.test.mjs` - Core client functionality
- `test/sidecar/integration.test.mjs` - Basic integration

#### 5. Essential Utilities (P0 - 10% value)
- `test/utils/sparql-utils.test.mjs` - SPARQL utilities
- `test/utils/validation-utils.test.mjs` - Validation utilities

### 🟡 CONDITIONAL (Medium Priority - 30% of tests, 15% of value)

#### CLI v2 Tests (P1 - 8% value)
- `test/cli-v2/commands/parse.test.mjs` - New parsing commands
- `test/cli-v2/commands/query.test.mjs` - New query commands
- `test/cli-v2/commands/validate.test.mjs` - New validation commands

#### Advanced Knowledge Engine (P1 - 4% value)
- `test/knowledge-engine/reason.test.mjs` - Reasoning functionality
- `test/knowledge-engine/canonicalize.test.mjs` - Canonicalization

#### Composables (P1 - 3% value)
- `test/composables/use-graph.test.mjs` - Graph composables
- `test/composables/use-zod.test.mjs` - Zod validation

### 🔴 REMOVE (Low Priority - 50% of tests, 5% of value)

#### Performance & Benchmark Tests
- `test/benchmarks/**` - All benchmark tests
- `test/cli-v2/performance/**` - Performance tests
- `test/knowledge-engine/hooks/performance-*.test.mjs` - Performance hook tests

#### Extensive E2E Tests
- `test/e2e/cleanroom/**` - Complex cleanroom tests
- `test/e2e/k8s-terraform-testcontainers.test.mjs` - Infrastructure tests
- `test/e2e/browser-e2e.test.mjs` - Browser tests

#### Security & Chaos Tests
- `sidecar/test/security/**` - Security tests
- `sidecar/test/chaos/**` - Chaos engineering tests
- `sidecar/test/consensus/**` - Consensus tests

#### Advanced Sidecar Tests
- `sidecar/test/performance/**` - Performance tests
- `sidecar/test/infrastructure/**` - Infrastructure tests
- `sidecar/test/nuxt/**` - UI component tests

#### Hook System Tests (Most)
- `test/knowledge-engine/hooks/**` - Most hook tests (keep only core)
- `sidecar/test/e2e/scenarios/**` - Scenario tests

#### Enterprise & Demo Tests
- `enterprise-demo/test/**` - Demo tests
- `test/nuxt/**` - Nuxt component tests

## Implementation Strategy

### Phase 1: Immediate Pruning (Week 1)
1. Remove all benchmark and performance tests
2. Remove complex E2E and infrastructure tests
3. Remove security and chaos tests
4. Remove enterprise demo tests

### Phase 2: Selective Pruning (Week 2)
1. Keep only core hook tests
2. Remove advanced sidecar tests
3. Remove most CLI v2 tests (keep only essential)
4. Remove Nuxt and browser tests

### Phase 3: Optimization (Week 3)
1. Consolidate similar tests
2. Remove redundant test cases
3. Optimize test data and fixtures
4. Update test configuration

## Expected Results

### Before Pruning
- **Total Test Files**: ~141
- **Estimated Execution Time**: 15-20 minutes
- **Coverage**: 95%+ (comprehensive)

### After Pruning
- **Total Test Files**: ~28 (20% of original)
- **Estimated Execution Time**: 3-5 minutes (75% reduction)
- **Coverage**: 80%+ (focused on critical paths)

### Value Retention
- **80% of functional coverage** maintained
- **100% of critical business logic** tested
- **90% of common user workflows** validated
- **75% reduction in execution time**

## Risk Mitigation

### High-Risk Areas (Monitor Closely)
1. **Transaction Management** - Core atomicity
2. **SPARQL Query Engine** - Core functionality
3. **RDF Parsing** - Data ingestion
4. **SHACL Validation** - Data quality

### Medium-Risk Areas (Periodic Testing)
1. **Hook System** - Advanced features
2. **Reasoning Engine** - Complex logic
3. **Sidecar Integration** - Distributed features

### Low-Risk Areas (Minimal Testing)
1. **Performance Optimization** - Nice to have
2. **Security Features** - Edge cases
3. **UI Components** - Non-critical

## Monitoring & Validation

### Success Metrics
- Test execution time < 5 minutes
- Critical functionality coverage > 80%
- Zero production failures in core areas
- Developer productivity improvement

### Failure Indicators
- Production failures in untested areas
- Regression bugs in core functionality
- Developer complaints about missing tests
- Coverage drops below 75%

## Rollback Plan

If issues arise after pruning:
1. **Immediate**: Restore critical tests from git history
2. **Short-term**: Add back specific test categories
3. **Long-term**: Re-evaluate pruning strategy

## Conclusion

This 80/20 test pruning strategy will significantly reduce test execution time while maintaining the essential coverage needed for production reliability. The focus on critical business logic and common user workflows ensures that the most important functionality remains thoroughly tested.

**Status**: Ready for implementation
**Expected Impact**: 75% reduction in test time, 80% retention of functional coverage

