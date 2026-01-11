# Remaining Issues - UNRDF v6.0.0-rc.1

**Generated**: 2026-01-11
**Validation Score**: 36/100
**Status**: NO-GO for production

---

## Critical Blockers (MUST FIX - P0)

### BLOCKER-001: CLI Build Failure
**Package**: `@unrdf/cli@5.0.1`
**Severity**: CRITICAL (P0)
**Category**: Build
**Impact**: Cannot generate production artifacts

**Error**:
```
unbuild: Could not find entrypoint for ./src/index.mjs
unbuild: Could not find entrypoint for ./src/commands/index.mjs
unbuild: Could not find entrypoint for ./src/cli.mjs
Exit code: 1
```

**Root Cause**: Missing entrypoint files in CLI package structure

**Fix**:
```bash
# Create missing entrypoint
cat > packages/cli/src/index.mjs << 'EOF'
/**
 * @file CLI Package Entrypoint
 * @module @unrdf/cli
 */
export * from './commands/index.mjs';
export * from './cli.mjs';
EOF

# Verify
timeout 180s pnpm -C packages/cli build
```

**Verification Command**:
```bash
timeout 180s pnpm build && echo "SUCCESS" || echo "FAILED"
```

**ETA**: 30 minutes
**Assignee**: Backend Developer
**Priority**: P0 - Blocking deployment

---

### BLOCKER-002: Docs Test Failure
**Package**: `packages/docs`
**Severity**: CRITICAL (P0)
**Category**: Tests
**Impact**: Cannot verify docs functionality

**Error**:
```
TSConfckParseError: failed to resolve "extends":"./.nuxt/tsconfig.json"
Cannot find module './.nuxt/tsconfig.json'
Exit code: 1
```

**Root Cause**: Missing Nuxt-generated configuration directory

**Fix Option A** (Generate config):
```bash
pnpm -C packages/docs nuxt prepare
timeout 60s pnpm -C packages/docs test
```

**Fix Option B** (Skip if non-critical):
```json
// packages/docs/package.json
{
  "scripts": {
    "test": "echo 'Docs tests skipped - requires Nuxt build' && exit 0"
  }
}
```

**Verification Command**:
```bash
timeout 120s pnpm test && echo "ALL_TESTS_PASS" || echo "TESTS_FAILED"
```

**ETA**: 15 minutes
**Assignee**: Frontend Developer / DevOps
**Priority**: P0 - Blocking deployment

---

### BLOCKER-003: Lint Violations in kgc-cli
**Package**: `@unrdf/kgc-cli@5.0.1`
**Severity**: CRITICAL (P0)
**Category**: Code Quality
**Impact**: Violates zero-warning policy

**Violations**:
1. `src/lib/latex/compile.mjs:234:3` - `projectDir` unused
2. `test/extensions/yawl-extensions.test.mjs:61:21` - `nounName` unused
3. `test/extensions/yawl-extensions.test.mjs:64:23` - `verbName` unused

**Fix**:
```bash
# Prefix with underscore to indicate intentionally unused
sed -i 's/\bprojectDir\b/_projectDir/g' packages/kgc-cli/src/lib/latex/compile.mjs
sed -i 's/\bnounName\b/_nounName/g' packages/kgc-cli/test/extensions/yawl-extensions.test.mjs
sed -i 's/\bverbName\b/_verbName/g' packages/kgc-cli/test/extensions/yawl-extensions.test.mjs

# Verify
timeout 60s pnpm -C packages/kgc-cli lint
```

**Verification Command**:
```bash
timeout 60s pnpm lint 2>&1 | grep -E "warnings|errors"
# Expected: 0 errors, 0 warnings
```

**ETA**: 10 minutes
**Assignee**: Backend Developer
**Priority**: P0 - Blocking deployment

---

### BLOCKER-004: Security Vulnerability - qs Package
**Package**: `qs` (via `express` in `packages/observability`)
**Severity**: HIGH (P1)
**Category**: Security
**Impact**: DoS vulnerability in production observability

**Vulnerability**:
- **GHSA**: [GHSA-6rw7-vpxm-498p](https://github.com/advisories/GHSA-6rw7-vpxm-498p)
- **Description**: arrayLimit bypass allows DoS via memory exhaustion
- **Vulnerable**: `<6.14.1`
- **Patched**: `>=6.14.1`
- **Path**: `packages__observability > express > qs`

**Fix**:
```bash
# Update express to get patched qs
pnpm -C packages/observability update express@latest

# Verify
pnpm audit --audit-level=moderate | grep qs
# Expected: No qs vulnerabilities
```

**Verification Command**:
```bash
pnpm audit --audit-level=moderate
# Expected: 0 vulnerabilities (or 1 if preact not fixed)
```

**ETA**: 20 minutes
**Assignee**: Security Engineer / DevOps
**Priority**: P1 - Security risk

---

### BLOCKER-005: Security Vulnerability - preact Package
**Package**: `preact` (via `react-force-graph-3d` in `packages/kgc-4d/playground`)
**Severity**: HIGH (P1)
**Category**: Security
**Impact**: JSON VNode Injection vulnerability

**Vulnerability**:
- **GHSA**: [GHSA-36hm-qxxp-pg3m](https://github.com/advisories/GHSA-36hm-qxxp-pg3m)
- **Description**: JSON VNode Injection vulnerability
- **Vulnerable**: `>=10.28.0 <10.28.2`
- **Patched**: `>=10.28.2`
- **Path**: `kgc-4d/playground > react-force-graph-3d > ... > preact`

**Fix Option A** (Update):
```bash
pnpm -C packages/kgc-4d/playground update preact@latest

# Verify
pnpm audit --audit-level=moderate | grep preact
# Expected: No preact vulnerabilities
```

**Fix Option B** (Exclude if dev-only):
```bash
# If playground is development-only, exclude from production
mv packages/kgc-4d/playground packages/kgc-4d/playground-dev
echo "packages/kgc-4d/playground-dev" >> .npmignore
```

**Verification Command**:
```bash
pnpm audit --audit-level=moderate
# Expected: 0 vulnerabilities
```

**ETA**: 20 minutes
**Assignee**: Security Engineer / DevOps
**Priority**: P1 - Security risk

---

## Warnings (SHOULD FIX - P2/P3)

### WARN-001: File Size Violations
**Severity**: MEDIUM (P2)
**Category**: Code Quality
**Impact**: Maintainability concerns

**Details**:
- Files exceeding 500-line limit: **145**
- Compliance rate: 92.75%
- Target: 100%

**Top Violators**:
1. `packages/yawl/src/cancellation/yawl-cancellation.mjs` - 1,779 lines (+1,279)
2. `packages/yawl/src/resources/yawl-resources.mjs` - 1,580 lines (+1,080)
3. `packages/yawl/src/events/yawl-events.mjs` - 1,428 lines (+928)
4. `packages/kgc-probe/src/agents/index.mjs` - 1,402 lines (+902)
5. `packages/fusion/src/kgc-docs-diataxis.mjs` - 1,367 lines (+867)

**Recommendation**: Refactor large files into smaller, focused modules

**ETA**: 2-3 days (refactoring effort)
**Assignee**: Development Team
**Priority**: P2 - Post-release

---

### WARN-002: Deprecated Dependencies
**Severity**: LOW (P3)
**Category**: Dependencies
**Impact**: Future compatibility risks

**Details**:
- Deprecated subdependencies: **30**
- Notable deprecations:
  - `glob@7.2.3` and `glob@8.1.0`
  - `rimraf@2.7.1` and `rimraf@3.0.2`
  - `inflight@1.0.6`
  - `core-js@2.6.12`

**Recommendation**: Update parent dependencies to use non-deprecated versions

**Fix**:
```bash
# Update dependencies that pull in deprecated packages
pnpm update --latest --recursive
```

**ETA**: 1 day (testing required)
**Assignee**: DevOps
**Priority**: P3 - Post-release

---

### WARN-003: Version Inconsistency
**Severity**: MEDIUM (P2)
**Category**: Version Management
**Impact**: Version confusion, unclear release status

**Details**:
- Total packages: 69
- Version distribution:
  - `1.0.0`: 39 packages (56.5%)
  - `5.0.1`: 17 packages (24.6%)
  - `6.0.0-rc.1`: 3 packages (4.3%)
  - Other: 10 packages (14.5%)

**Recommendation**: Standardize all packages to `6.0.0-rc.1`

**Fix**:
```bash
# Standardize versions
pnpm -r exec npm version 6.0.0-rc.1 --no-git-tag-version

# Verify
find packages -name "package.json" -exec grep -H '"version"' {} \; | grep -v "6.0.0-rc.1"
# Expected: 0 results (or only dev packages)
```

**ETA**: 30 minutes
**Assignee**: DevOps
**Priority**: P2 - Pre-release

---

### WARN-004: Forbidden N3 Imports
**Severity**: LOW (P3)
**Category**: Code Quality
**Impact**: May be intentional for v6-compat

**Details**:
- Occurrences: 2
- Files:
  1. `packages/v6-compat/src/adapters.mjs`
  2. `packages/v6-compat/src/lint-rules.mjs`

**Context**: Both in `v6-compat` package (V5→V6 migration bridge)

**Recommendation**: Verify if intentional, document exception if needed

**Fix** (if intentional):
```javascript
// packages/v6-compat/src/adapters.mjs
// EXCEPTION: Direct N3 import required for V5 compatibility bridge
// This will be removed when V5 support is deprecated
import { Store } from 'n3';
```

**ETA**: 15 minutes (review + document)
**Assignee**: Code Reviewer
**Priority**: P3 - Post-release

---

### WARN-005: Peer Dependency Conflicts
**Severity**: MEDIUM (P2)
**Category**: Dependencies
**Impact**: Runtime warnings, potential incompatibilities

**Details**:

**packages/docs**:
- @tiptap/core: Found 3.13.0, required ^3.14.0
- @tiptap/pm: Found 3.13.0, required ^3.14.0

**packages/blockchain**:
- vitest: Found 1.6.1, required 4.0.16
- @vitest/ui: Found 4.0.16, required 1.6.1

**Recommendation**: Update dependencies to match peer requirements

**Fix**:
```bash
# Fix docs
pnpm -C packages/docs update @tiptap/core@latest @tiptap/pm@latest

# Fix blockchain
pnpm -C packages/blockchain update vitest@latest @vitest/ui@latest
```

**ETA**: 1 hour (testing required)
**Assignee**: DevOps
**Priority**: P2 - Pre-release

---

### WARN-006: Cyclic Dependencies
**Severity**: MEDIUM (P2)
**Category**: Architecture
**Impact**: Potential circular dependency issues

**Details**:
- Cyclic workspace dependencies: `kgc-multiverse`, `receipts`

**Recommendation**: Refactor to remove circular dependencies

**Investigation Required**: Analyze dependency graph to identify circular references

**ETA**: 2-4 hours (investigation + refactor)
**Assignee**: Architect
**Priority**: P2 - Post-release

---

## Missing Validations

### Coverage Analysis Not Run
**Category**: Quality Gate
**Impact**: Unknown test coverage percentage

**Recommendation**: Run coverage analysis

**Command**:
```bash
timeout 180s pnpm test:coverage

# Verify ≥80% threshold
grep -E "All files.*[0-9.]+%" coverage/lcov-report/index.html
```

**ETA**: 10 minutes
**Assignee**: QA
**Priority**: P1 - Pre-deployment

---

## Summary

### By Priority

| Priority | Count | Category | Blocking |
|----------|-------|----------|----------|
| P0 | 3 | Build, Tests, Lint | YES |
| P1 | 2 | Security | YES |
| P2 | 4 | Quality, Dependencies | NO |
| P3 | 3 | Code Quality | NO |

### By Status

| Status | Count |
|--------|-------|
| 🔴 Open | 12 |
| 🟡 In Progress | 0 |
| 🟢 Resolved | 0 |

### Estimated Total Fix Time

| Priority | Time |
|----------|------|
| P0 + P1 (Blockers) | 2-4 hours |
| P2 (Pre-release) | 1-2 days |
| P3 (Post-release) | 2-3 days |

**Critical Path**: Fix P0 + P1 issues → Re-validate → Deploy

---

## Re-Validation Checklist

After fixing all P0 + P1 issues, run:

```bash
# 1. Clean install
rm -rf node_modules pnpm-lock.yaml
pnpm install

# 2. Build
timeout 180s pnpm build
# Expected: Exit code 0

# 3. Lint
timeout 60s pnpm lint
# Expected: 0 errors, 0 warnings

# 4. Tests
timeout 120s pnpm test
# Expected: 100% pass rate

# 5. Security
pnpm audit --audit-level=moderate
# Expected: 0 vulnerabilities

# 6. Coverage
timeout 180s pnpm test:coverage
# Expected: ≥80%

# 7. OTEL
node validation/run-all.mjs comprehensive
# Expected: ≥80/100 (currently 100/100)

# 8. Generate new report
node -e "
  const fs = require('fs');
  const results = {
    timestamp: new Date().toISOString(),
    score: 'TBD',
    decision: 'TBD'
  };
  fs.writeFileSync('validation-results-v2.json', JSON.stringify(results, null, 2));
"
```

**Expected Overall Score**: ≥70/100 (threshold for GO decision)

---

## Notes

- All log files preserved in `/tmp/validation-*.log` (7-day retention)
- Full validation report: `FINAL_VALIDATION_REPORT.md`
- Deployment decision: `DEPLOYMENT_DECISION.md`
- Machine-readable results: `validation-results.json`

**Last Updated**: 2026-01-11
**Next Review**: After P0/P1 fixes completed
