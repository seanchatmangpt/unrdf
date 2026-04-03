# UNRDF v6 Blocking Issues - April 3, 2026

## Critical Blocking Issues (Fix Before Production)

### Issue #1: N3 Backward Compat Test Failure

**Severity**: CRITICAL
**File**: `packages/core/test/sparql/n3-backward-compat.test.mjs:253`
**Test**: "preserves result format between N3 Store and UnrdfStore"

**Error**:
```
AssertionError: expected Literal{ __wbg_ptr: 1650712 } to have property "type"
```

**Root Cause**: 
Oxigraph WASM returns Literal objects without `.type` property, but test expects it.
This is incompatibility between N3 library expectations and Oxigraph WASM format.

**Fix**:
Update test to handle WASM Literal format:
- Check for `__wbg_ptr` (WASM object indicator)
- Extract value differently for WASM objects
- OR skip this specific assertion for WASM backend

**Estimated Time**: 1 hour
**Test Pass Rate Impact**: 99.7% → 100%

---

### Issue #2: Merkle Tree CVE-2012-2459 Mitigation Broken

**Severity**: CRITICAL
**File**: `packages/daemon/test/e2e-receipts-merkle.test.mjs`
**Tests**: 2 failures
1. "should prevent odd-leaf duplication attack (CVE-2012-2459)"
2. "should verify proofs correctly for odd-leaf trees"

**Error 1**:
```
expected 'e6bd183377f0fe3a64650a35ba31ccebd90d8...' 
  not to be 'e6bd183377f0fe3a64650a35ba31ccebd90d8...'
```
Hash is identical when it should be different (duplication vulnerability).

**Error 2**:
```
expected false to be true
```
Proof verification fails for odd-leaf trees.

**Root Cause**:
Merkle tree algorithm doesn't properly differentiate odd-leaf nodes.
This leaves the system vulnerable to CVE-2012-2459 odd-leaf duplication attacks.

**Fix**:
1. Review merkle tree construction algorithm
2. Ensure odd leaves are marked/tagged differently
3. Verify Merkle inclusion proofs handle odd leaves correctly
4. Add node count to hash computation to prevent duplication

**Estimated Time**: 2-3 hours
**Security Impact**: CRITICAL (CVE-2012-2459)
**Test Pass Rate Impact**: 99.7% → 99.9%

---

### Issue #3: Lint Violations

**Severity**: BLOCKING (CI/CD Gate)
**Total Warnings**: 34 across 2 packages
**Max Allowed**: 0

#### Package: `packages/chatman-equation`
- **Warnings**: 19
- **Errors**: 0
- **Fixable**: All

**Issues**:
```
Unused variables (15):
  - z (DP mechanism)
  - randomBytes (FedAvg)
  - model (neural symbolic)
  - otherId (secure aggregation)
  - etc.

Missing JSDoc (4):
  - Lines 713, 717, 722, 726 in temporal-gnn.mjs

Unused imports:
  - ChatmanExampleSchema
  - DeltaSchema
  - ClosureOperatorSchema
  - ArtifactSchema
```

#### Package: `packages/ai-ml-innovations`
- **Warnings**: 15
- **Errors**: 0
- **Fixable**: All

**Issues**:
```
Unused variables in:
  - src/dp-mechanism.mjs: z, randomBytes
  - src/fedavg.mjs: z
  - src/federated-embeddings.mjs: randomBytes, model
  - src/neural-symbolic-reasoner.mjs: triple
  - src/privacy-budget.mjs: z, range
  - src/secure-aggregation.mjs: z, otherId
  - src/temporal-gnn.mjs: maxLen + 4 missing JSDoc
```

**Fix**:
```bash
pnpm lint:fix
```

This will:
- Prefix unused vars with `_` (e.g., `_z`)
- Add auto-generated JSDoc stubs

Then manually review and complete JSDoc.

**Estimated Time**: 30 minutes
**CI/CD Gate Impact**: BLOCKING → CLEAR

---

### Issue #4: Missing ROLLBACK_PLAN.md

**Severity**: HIGH
**Type**: Documentation
**Impact**: Cannot execute incident response if v6 deployment fails

**Required Content**:
1. How to revert from v6.0.0-rc.1 to v5.x
2. Database migration rollback procedures
3. .unrdf.toml configuration downgrade path
4. Data format compatibility issues
5. CLI command differences
6. Breaking changes reversion guide

**Why Critical**:
- If v6 breaks in production, need documented rollback
- Customers need safety net
- SLA compliance requires incident procedures
- Insurance/compliance likely requires documented recovery

**Estimated Time**: 1-2 hours
**Customers Impacted**: ALL (production safety)

---

## Summary

| Issue | Type | Time | Impact | Action |
|-------|------|------|--------|--------|
| N3 Compat Test | Code | 1h | Test suite | Fix test expectations |
| Merkle CVE | Code | 2-3h | Security | Review algorithm |
| Lint | Code | 30m | CI/CD gate | Run pnpm lint:fix |
| Rollback Doc | Doc | 1-2h | Incident Response | Create ROLLBACK_PLAN.md |

**Total Time**: ~5 hours
**Deadline**: Before v6.0.0 stable release

---

## Commands to Execute

```bash
# View lint issues
pnpm lint 2>&1 | grep -A 2 "chatman-equation\|ai-ml-innovations"

# Fix lint violations
pnpm lint:fix

# Re-run tests
timeout 30s pnpm test:fast

# Verify specific test
pnpm -C packages/core test -- n3-backward-compat.test.mjs
pnpm -C packages/daemon test -- e2e-receipts-merkle.test.mjs

# Run all tests
timeout 60s pnpm test
```

---

**Generated**: 2026-04-03
**Report**: /Users/sac/unrdf/PRODUCTION_AUDIT_2026-04-03.md
**Summary**: /Users/sac/unrdf/DEPLOYMENT_SUMMARY_2026-04-03.txt
