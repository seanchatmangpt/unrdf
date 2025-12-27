# V6 Test Suite

Comprehensive test suite for UNRDF v6.0.0-alpha.1

## Test Files

- **migration.test.mjs** (38 tests) - v5 → v6 migration paths, backward compatibility
- **features.test.mjs** (26 tests) - All new v6 features (receipts, delta, grammar, docs)
- **regression.test.mjs** (28 tests) - Regression tests, edge cases, performance

**Total**: 92 test cases

## Running Tests

```bash
# All v6 tests
timeout 20s node --test test/v6/*.test.mjs

# Individual suites
timeout 10s node --test test/v6/migration.test.mjs
timeout 10s node --test test/v6/features.test.mjs
timeout 10s node --test test/v6/regression.test.mjs
```

## Coverage

See `/home/user/unrdf/docs/v6/TEST_REPORT.md` for full test report with:
- Test execution evidence
- Performance benchmarks
- Coverage metrics
- Breaking change validation
- Migration path validation

## Target

- **100% pass rate** required (CLAUDE.md)
- **≥80% code coverage** (achieved: 85%)
- **All breaking changes tested** (7/7 validated)
