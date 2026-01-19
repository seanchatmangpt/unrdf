# v6.0.0-rc.3 Release Blockers

**Target Release Date**: 2026-01-20
**Time Estimate**: 3 hours
**Quality Gate Target**: ≥6/8 (75%)
**Current Status**: 1.5/8 (18.75%)

---

## Blocker Summary

- [ ] **BLOCKER 1**: Build System Failure (P0) - 15 min
- [ ] **BLOCKER 2**: Test Infrastructure Errors (P0) - 30 min
- [ ] **BLOCKER 3**: LaTeX Pipeline Test Failures (P1) - 15 min (document as experimental)
- [ ] **BLOCKER 4**: Security Vulnerabilities (P1) - 30 min
- [ ] **BLOCKER 5**: Benchmark Module Resolution (P1) - 15 min
- [ ] **ISSUE 1**: Lint Performance (30 min)
- [ ] **ISSUE 2**: TODO Markers (15 min)
- [ ] **ISSUE 3**: Forbidden N3 Imports (15 min)

---

## BLOCKER 1: Build System Failure (P0)

**Issue**: Nextra build blocked by stale Next.js lock file
**Impact**: Cannot generate production artifacts
**Priority**: P0 (Release Blocker)
**Time**: 15 minutes

### Error
```
⨯ Unable to acquire lock at /home/user/unrdf/packages/nextra/.next/lock
```

### Solution
```bash
# Create cleanup script
cat > scripts/clean-locks.sh << 'EOF'
#!/bin/bash
find packages -name 'lock' -path '*/.next/lock' -delete
EOF
chmod +x scripts/clean-locks.sh

# Add to package.json "prebuild" script
# Test: pnpm build should complete without errors
```

### Acceptance Criteria
- [ ] `pnpm build` completes successfully
- [ ] No lock file errors in output
- [ ] Cleanup script runs automatically before build

---

## BLOCKER 2: Test Infrastructure Errors (P0)

**Issue**: Oxigraph coverage file generation fails with ENOENT
**Impact**: Cannot trust test results, CI/CD unreliable
**Priority**: P0 (Release Blocker)
**Time**: 30 minutes

### Error
```
Error: ENOENT: no such file or directory,
  open '/home/user/unrdf/packages/oxigraph/coverage/.tmp/coverage-5.json'
```

### Solution
```bash
# Create temp directory
mkdir -p packages/oxigraph/coverage/.tmp

# Update vitest.config.mjs with proper coverage config
# See RC3_BLOCKER_FIXES.md for full config
```

### Acceptance Criteria
- [ ] `pnpm -C packages/oxigraph test:coverage` completes without errors
- [ ] Coverage reports generated successfully
- [ ] No unhandled rejections in test output

---

## BLOCKER 3: LaTeX Pipeline Test Failures (P1)

**Issue**: 11/15 latex-pipeline tests failing (26.7% pass rate)
**Impact**: LaTeX integration non-functional
**Priority**: P1 (Feature Quality)
**Time**: 15 minutes (document as experimental)

### Error
```
❯ test/latex-pipeline.test.mjs (15 tests | 11 failed)
```

### Solution (Pragmatic - Document as Experimental)
```bash
# Add to README.md
# Document LaTeX as experimental feature
# Create GitHub issue for LaTeX test failures
# Update CHANGELOG.md with known limitations
```

### Acceptance Criteria
- [ ] LaTeX marked as experimental in README
- [ ] Known limitations documented in CHANGELOG
- [ ] GitHub issue created for tracking fixes
- [ ] Test failures documented (don't block RC.3)

---

## BLOCKER 4: Security Vulnerabilities (P1)

**Issue**: 7 high-severity vulnerabilities in dependencies
**Impact**: Production deployment risk
**Priority**: P1 (Security)
**Time**: 30 minutes

### Vulnerabilities
- qs: arrayLimit bypass
- Preact: JSON VNode injection
- devalue: Denial of service (2 instances)
- h3: Request smuggling
- node-tar: Arbitrary file overwrite

### Solution
```bash
# Update vulnerable packages
pnpm update qs preact devalue h3 tar

# If needed, add overrides to package.json
# Verify: pnpm audit --audit-level=high (0 high/critical)
```

### Acceptance Criteria
- [ ] `pnpm audit --audit-level=high` shows 0 high/critical vulnerabilities
- [ ] `pnpm test:fast` passes after updates (no regressions)
- [ ] Updated dependencies documented in CHANGELOG

---

## BLOCKER 5: Benchmark Module Resolution (P1)

**Issue**: Cannot find @unrdf/kgc-4d package
**Impact**: Cannot verify performance claims
**Priority**: P1 (Quality Assurance)
**Time**: 15 minutes

### Error
```
Error [ERR_MODULE_NOT_FOUND]: Cannot find package '@unrdf/kgc-4d'
  imported from /home/user/unrdf/benchmarks/core/engine-performance.mjs
```

### Solution
```bash
# Verify package exists
ls -la packages/kgc-4d/package.json

# Rebuild workspace
pnpm install

# Test: pnpm benchmark:core
```

### Acceptance Criteria
- [ ] `pnpm benchmark:core` executes without module errors
- [ ] All benchmark suites complete successfully
- [ ] Performance metrics match CHANGELOG claims

---

## ISSUE 1: Lint Performance

**Issue**: Lint command times out (>35s)
**Impact**: CI/CD pipeline delays
**Priority**: P2 (Developer Experience)
**Time**: 30 minutes

### Solution
```bash
# Profile lint execution
time pnpm lint

# Optimize ESLint caching
# Target: <30s execution
```

### Acceptance Criteria
- [ ] `pnpm lint` completes in <30s
- [ ] ESLint cache enabled and working
- [ ] No hanging or slow linters identified

---

## ISSUE 2: TODO Markers in Source Code

**Issue**: 2 TODO markers in production code
**Impact**: Code quality standards violation
**Priority**: P2 (Code Quality)
**Time**: 15 minutes

### Files
- `packages/yawl/src/integrations/index.mjs`
- `packages/yawl/src/worklets/worklet-runner.mjs`

### Solution
```bash
# Create GitHub issues for TODOs
# Remove TODO comments from source
# Verify: grep -r "TODO" packages/*/src --include="*.mjs" (0 results)
```

### Acceptance Criteria
- [ ] GitHub issues created for each TODO
- [ ] TODO comments removed from source files
- [ ] 0 TODOs in `packages/*/src/**/*.mjs`

---

## ISSUE 3: Forbidden N3 Imports

**Issue**: 2 files import directly from 'n3' package
**Impact**: Violates CLAUDE.md Rule #7
**Priority**: P2 (Code Quality)
**Time**: 15 minutes

### Files
- `packages/v6-compat/src/adapters.mjs`
- `packages/v6-compat/src/lint-rules.mjs`

### Solution
```bash
# Option A: Replace with @unrdf/oxigraph
# Option B: Document as exception for v6-compat (legacy bridge)
```

### Acceptance Criteria
- [ ] N3 imports replaced or documented as exceptions
- [ ] `find packages/*/src -name "*.mjs" -exec grep -l "from 'n3'" {} \;` shows 0 or documented exceptions
- [ ] Exception justification added to CLAUDE.md if retained

---

## Quality Gate Checklist

### Before Fixes (Current State)
- [ ] ❌ Gate 1: Code Quality (lint timeout, N3 imports, TODOs)
- [ ] ⚠️ Gate 2: Test Coverage (partial, 102% kgc-cli, 0% graph-analytics)
- [ ] ❌ Gate 3: Test Pass Rate (97.5%, target ≥99%)
- [ ] ❌ Gate 4: Build Success (nextra lock error)
- [ ] ✅ Gate 5: OTEL Validation (100/100)
- [ ] ❌ Gate 6: Security (7 high-severity vulnerabilities)
- [ ] ❌ Gate 7: Performance (benchmarks cannot run)
- [ ] ⚠️ Gate 8: Documentation (1269 files, some examples broken)

**Current**: 1.5/8 (18.75%) - NO-GO

### After Fixes (Expected)
- [ ] ✅ Gate 1: Code Quality (lint <30s, TODOs removed, N3 documented)
- [ ] ⚠️ Gate 2: Test Coverage (partial acceptable)
- [ ] ⚠️ Gate 3: Test Pass Rate (98%+ excluding experimental)
- [ ] ✅ Gate 4: Build Success (locks cleaned automatically)
- [ ] ✅ Gate 5: OTEL Validation (100/100)
- [ ] ✅ Gate 6: Security (0 high/critical vulnerabilities)
- [ ] ✅ Gate 7: Performance (benchmarks running, metrics verified)
- [ ] ✅ Gate 8: Documentation (examples fixed, LaTeX documented)

**Expected**: 6.5/8 (81.25%) - GO for RC.3

---

## Validation Script

```bash
#!/bin/bash
# Save as: scripts/validate-rc3.sh
# See RC3_BLOCKER_FIXES.md for full script

chmod +x scripts/validate-rc3.sh
./scripts/validate-rc3.sh
```

**Expected Output**: "✅ GO for v6.0.0-rc.3 release"

---

## Release Timeline

### Phase 1: Fix Blockers (2 hours)
- **10:00-10:15**: BLOCKER 1 (Build system)
- **10:15-10:45**: BLOCKER 2 (Test infrastructure)
- **10:45-11:15**: BLOCKER 4 (Security vulnerabilities)
- **11:15-11:30**: BLOCKER 5 (Benchmark resolution)
- **11:30-12:00**: BLOCKER 3 (LaTeX documentation)
- **12:00-12:30**: ISSUE 1 (Lint performance)
- **12:30-12:45**: ISSUE 2 (TODO removal)
- **12:45-13:00**: ISSUE 3 (N3 imports)

### Phase 2: Validate Quality Gates (1 hour)
- **13:00-13:30**: Run full validation suite
- **13:30-14:00**: Address any validation failures

### Phase 3: Release (30 minutes)
- **14:00-14:15**: Update version, CHANGELOG, commit
- **14:15-14:30**: Publish to npm, create GitHub release

**Total Time**: 3.5 hours
**Target Completion**: 2026-01-20 14:30 UTC

---

## References

- **Assessment**: `FINAL_RELEASE_DECISION_v6.0.0-rc.2.md`
- **Fix Guide**: `RC3_BLOCKER_FIXES.md`
- **CHANGELOG**: `CHANGELOG.md` (section 6.0.0-rc.2)

---

## Labels

`release-blocker` `v6.0.0-rc.3` `priority-p0` `priority-p1`
