---
description: Run and validate tests for UNRDF packages with coverage reporting and failure analysis
---

# Test Runner

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Execute test suites across UNRDF packages using Vitest, analyze failures, and ensure 80%+ coverage compliance.

## Quick Commands

```bash
# Run all tests
timeout 120s pnpm test

# Run specific package tests
timeout 60s pnpm --filter "@unrdf/core" test

# Run with coverage
timeout 180s pnpm test:coverage

# Run single test file
timeout 30s pnpm vitest run path/to/test.test.mjs
```

## Execution Steps

### 1. Run Tests

```bash
# Full test suite with timeout
timeout 120s pnpm test 2>&1 | tee test-output.log

# Check exit code
echo "Exit code: $?"
```

### 2. Coverage Analysis

```bash
# Generate coverage report
timeout 180s pnpm test:coverage 2>&1

# Parse coverage summary
cat coverage/coverage-summary.json | jq '.total'
```

### 3. Failure Analysis

If tests fail, analyze systematically:

```bash
# Extract failing tests
grep -E "FAIL|Error:|AssertionError" test-output.log

# Run failed tests in isolation
pnpm vitest run --reporter=verbose path/to/failing.test.mjs
```

### 4. Package-Specific Testing

```bash
# List all packages
ls packages/

# Test specific package
PACKAGE="${ARGUMENTS:-core}"
timeout 60s pnpm --filter "@unrdf/$PACKAGE" test 2>&1
```

## Coverage Requirements

| Metric     | Minimum | Target |
| ---------- | ------- | ------ |
| Lines      | 80%     | 90%    |
| Branches   | 70%     | 85%    |
| Functions  | 80%     | 90%    |
| Statements | 80%     | 90%    |

## Output Format

```markdown
## Test Execution Report

**Package**: [package name or "all"]
**Date**: [timestamp]
**Duration**: [time]

### Summary

| Metric      | Value |
| ----------- | ----- |
| Total Tests | X     |
| Passed      | Y     |
| Failed      | Z     |
| Skipped     | N     |

### Coverage

| Type      | Coverage | Threshold | Status |
| --------- | -------- | --------- | ------ |
| Lines     | XX.X%    | 80%       | ✅/❌  |
| Branches  | XX.X%    | 70%       | ✅/❌  |
| Functions | XX.X%    | 80%       | ✅/❌  |

### Failed Tests (if any)

1. **test-name**
   - File: path/to/test.test.mjs
   - Error: [error message]
   - Expected: [expected value]
   - Received: [actual value]

### Recommendations

- [fixes for failing tests]
- [coverage improvement suggestions]
```

## Vitest Configuration Reference

UNRDF uses `vitest.config.unified.mjs`:

```javascript
// Key configuration options
{
  test: {
    include: ['packages/*/test/**/*.test.mjs'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      thresholds: {
        lines: 80,
        branches: 70,
        functions: 80
      }
    }
  }
}
```

## Troubleshooting

### Timeout Issues

```bash
# Increase timeout for slow tests
timeout 300s pnpm test
```

### Flaky Tests

```bash
# Run test multiple times to detect flakiness
for i in {1..5}; do
  pnpm vitest run path/to/test.test.mjs && echo "Pass $i" || echo "FAIL $i"
done
```

### Memory Issues

```bash
# Run with increased heap
NODE_OPTIONS="--max-old-space-size=4096" pnpm test
```

End Command ---
