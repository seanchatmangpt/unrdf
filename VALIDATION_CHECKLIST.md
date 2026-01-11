# Production Validation Checklist

This checklist provides exact commands to verify each blocker is resolved.

## P0 Blockers - Verification Commands

### P0-1: Test Failures (28 failing)

**Current Status**: 28/578 tests failing (95.2% pass rate)
**Target**: 100% pass rate (0 failures)

**Verify**:
```bash
timeout 30s pnpm test:fast 2>&1 | tee test-results.log
grep "Tests.*passed" test-results.log
# Expected: "Tests: XXX passed (XXX)" with no failures mentioned
```

**Evidence Required**:
- Full test output showing 0 failures
- Pass rate: 100%

---

### P0-2: Lint Violations (4 warnings)

**Current Status**: 4 JSDoc warnings in USAGE-EXAMPLE.mjs
**Target**: 0 warnings, 0 errors

**Verify**:
```bash
timeout 30s pnpm lint 2>&1 | tee lint-results.log
grep -E "error|warning" lint-results.log | wc -l
# Expected: 0
```

**Evidence Required**:
- Lint output showing "Done" for all packages
- 0 errors, 0 warnings

---

### P0-3: TODO Comments (12 found)

**Current Status**: 12 TODOs in production code
**Target**: 0

**Verify**:
```bash
grep -r "TODO\|FIXME" /home/user/unrdf/packages/*/src --include="*.mjs" | wc -l
# Expected: 0
```

**Evidence Required**:
- Command output: 0
- No TODOs in production code (test TODOs are acceptable)

---

### P0-4: Skipped Tests (14 found)

**Current Status**: 14 it.skip/describe.skip
**Target**: 0

**Verify**:
```bash
grep -r "it.skip\|describe.skip" /home/user/unrdf/packages/*/test --include="*.test.mjs"
# Expected: no output
```

**Evidence Required**:
- Empty grep output
- All tests enabled and passing

---

### P0-5: Forbidden N3 Imports (3 violations)

**Current Status**: 3 direct N3 imports in CLI commands
**Target**: 0

**Verify**:
```bash
grep -r "from 'n3'" /home/user/unrdf/packages/*/src --include="*.mjs" | grep -v "n3-justified" | wc -l
# Expected: 0
```

**Files to fix**:
- packages/cli/src/cli/commands/convert.mjs
- packages/cli/src/cli/commands/graph.mjs
- packages/cli/src/cli/commands/query.mjs

**Replacement pattern**:
```javascript
// BEFORE
import { Parser, Writer } from 'n3';

// AFTER
import { Parser, Writer } from '@unrdf/oxigraph';
```

**Evidence Required**:
- Grep count: 0
- All CLI commands using @unrdf/oxigraph

---

### P0-6: Build Failures (nextra package)

**Current Status**: nextra build fails with Next.js module error
**Target**: All packages build successfully

**Verify**:
```bash
timeout 60s pnpm build 2>&1 | tee build-results.log
grep "Failed" build-results.log | wc -l
# Expected: 0
```

**Evidence Required**:
- Build output showing all packages complete
- No "Failed" messages
- Exit code 0

---

### P0-7: Security Integration (1 reference, need 13+)

**Current Status**: Security module exists but not integrated
**Target**: Security-audit integrated into daemon integrations

**Verify**:
```bash
grep -r "security-audit" /home/user/unrdf/packages/daemon/src/integrations --include="*.mjs" | wc -l
# Expected: ≥13
```

**Integration points** (examples):
- consensus.mjs - validate inputs
- federation-query.mjs - sanitize queries
- hooks-policy.mjs - audit policy changes
- kgc-4d-sourcing.mjs - verify event integrity
- knowledge-rules.mjs - validate rule syntax
- observability.mjs - audit logging
- receipts-merkle.mjs - cryptographic verification
- v6-deltagate.mjs - delta validation
- yawl.mjs - workflow input validation

**Evidence Required**:
- Grep count ≥13
- Security checks in all integration files

---

## Additional Checks

### Coverage (Not Measured)

**Target**: ≥80% coverage

**Verify**:
```bash
pnpm test:coverage 2>&1 | tee coverage-results.log
grep "All files" coverage-results.log
# Expected: Lines ≥80%, Functions ≥80%, Branches ≥80%, Statements ≥80%
```

---

### File Size (156 files >500 lines)

**Target**: Justified or refactored

**Verify**:
```bash
find /home/user/unrdf/packages/*/src -name "*.mjs" -exec wc -l {} + | awk '$1 > 500' | wc -l
# Expected: 0 or documented justifications
```

**Note**: This is a warning, not a blocker. Large files should be justified or refactored over time.

---

## OTEL Validation (Already Passing)

**Current Status**: 100/100 ✅
**Target**: ≥80/100

**Verify** (for final confirmation):
```bash
timeout 30s node /home/user/unrdf/validation/run-all.mjs comprehensive 2>&1 | grep "Score:"
# Expected: Score: ≥80
```

---

## Final Validation Suite

Run all checks in sequence:

```bash
#!/bin/bash
echo "=== Running Production Validation Suite ==="

echo "1. Testing..."
timeout 30s pnpm test:fast || echo "FAIL: Tests"

echo "2. Linting..."
timeout 30s pnpm lint || echo "FAIL: Lint"

echo "3. Building..."
timeout 60s pnpm build || echo "FAIL: Build"

echo "4. Checking TODOs..."
TODO_COUNT=$(grep -r "TODO\|FIXME" packages/*/src --include="*.mjs" | wc -l)
echo "TODOs: $TODO_COUNT (expected: 0)"

echo "5. Checking skipped tests..."
SKIP_COUNT=$(grep -r "it.skip\|describe.skip" packages/*/test --include="*.test.mjs" | wc -l)
echo "Skipped tests: $SKIP_COUNT (expected: 0)"

echo "6. Checking N3 imports..."
N3_COUNT=$(grep -r "from 'n3'" packages/*/src --include="*.mjs" | grep -v "n3-justified" | wc -l)
echo "N3 imports: $N3_COUNT (expected: 0)"

echo "7. Checking security integration..."
SEC_COUNT=$(grep -r "security-audit" packages/daemon/src/integrations --include="*.mjs" | wc -l)
echo "Security integrations: $SEC_COUNT (expected: ≥13)"

echo "8. Running OTEL validation..."
node validation/run-all.mjs comprehensive 2>&1 | grep "Score:"

echo "9. Measuring coverage..."
pnpm test:coverage 2>&1 | grep "All files"

echo "=== Validation Complete ==="
```

Save to: `/home/user/unrdf/scripts/validate-production.sh`

---

## Sign-Off Criteria

All of the following MUST be true:

- [ ] Test pass rate: 100% (0 failures)
- [ ] Lint violations: 0
- [ ] TODOs in production code: 0
- [ ] Skipped tests: 0
- [ ] Forbidden N3 imports: 0
- [ ] Build status: SUCCESS (all packages)
- [ ] Code coverage: ≥80%
- [ ] OTEL validation: ≥80/100
- [ ] Security integration: ≥13 references
- [ ] Documentation: All public APIs have JSDoc

**Current Status**: 1/10 criteria met (OTEL only)

When all criteria are met, regenerate this validation report and submit for final approval.

---

## Quick Reference

### Most Critical (Fix First)

1. Test failures (28) - Blocks everything
2. Lint violations (4) - Quick fix, 30 minutes
3. N3 imports (3) - Quick fix, 1 hour

### Medium Priority

4. TODOs (12) - Requires implementation decisions
5. Skipped tests (14) - Requires investigation
6. Build failures - Requires dependency fixes

### Long Term

7. Security integration - Requires architecture work
8. File size compliance - Gradual refactoring
9. Coverage measurement - After tests pass

---

**Created**: 2026-01-11 03:44 UTC
**Purpose**: Step-by-step verification for production validation
**Usage**: Run commands in order, verify expected outputs, check off criteria
