# UNRDF V6 Code Quality Standards - Executive Summary

**Date**: 2025-12-28
**Version**: 6.0.0-rc.1
**Status**: READY FOR IMPLEMENTATION

---

## Quick Reference

### Critical Numbers

| Standard | Target | Current | Status |
|----------|--------|---------|--------|
| **Max Lines/File** | 500 | 33 violations | ❌ 98% pass |
| **Max Function Lines** | 50 | 198 violations | ❌ 87% pass |
| **Max Exports/File** | 15 | 9 violations | ⚠️ 99% pass |
| **N3 Direct Imports** | 0 | 7 violations | ❌ CRITICAL |
| **Console.log in src/** | 0 | 304 instances | ❌ HIGH |
| **Default Exports** | 0 | 327 instances | ❌ MEDIUM |
| **TODO without issue** | 0 | 17 instances | ⚠️ LOW |

**Overall Quality Score**: 65/100 → Target: 90/100

---

## The Seven Standards

### 1. Complexity Limits

**Rule**: Keep code simple and testable

- ✅ **Cyclomatic Complexity**: ≤10 per function
- ✅ **Nesting Depth**: ≤3 levels
- ✅ **Parameters**: ≤4 per function (use options object)
- ✅ **Function Length**: ≤50 lines

**Why**: Complex code = hard to test, hard to maintain, high bug risk

**Enforcement**: ESLint `complexity`, `max-depth`, `max-params`, `max-lines-per-function`

### 2. File Size Limits

**Rule**: Small files = focused responsibility

- ✅ **Implementation Files**: ≤500 lines
- ✅ **Test Files**: ≤800 lines (with justification)
- ✅ **Exports Per File**: ≤15 named exports

**Current Issues**:
- 33 files exceed 500 lines (largest: 1010 lines)
- 9 files have >15 exports (God Objects)

**Enforcement**: Pre-commit hook + CI check

### 3. Dependency Rules

**Rule**: Respect layer architecture, ban problematic imports

**5-Layer Architecture** (no upward dependencies):
```
Layer 5: APPLICATION
Layer 4: KNOWLEDGE SUBSTRATE
Layer 3: KGC
Layer 2: RDF CORE
Layer 1: INFRASTRUCTURE
```

**CRITICAL BANS**:
- ❌ **N3 direct imports** (use `@unrdf/oxigraph`) - 7 violations
- ❌ **Circular dependencies** - 0 violations ✅
- ❌ **Forbidden packages**: moment, lodash, axios, bluebird

**Enforcement**: ESLint `import/no-restricted-paths` + CI check

### 4. Naming Conventions

**Rule**: Consistent, descriptive names

- **Files**: `kebab-case.mjs`
- **Functions**: `camelCase` (verb-noun pattern)
- **Constants**: `SCREAMING_SNAKE_CASE`
- **Classes/Schemas**: `PascalCase`
- **Packages**: `@unrdf/kebab-case`
- **Booleans**: Use `is`, `has`, `should`, `can` prefixes

**Enforcement**: ESLint `camelcase` + manual review

### 5. Documentation Requirements

**Rule**: Complete JSDoc for public API

**Required JSDoc Elements**:
- ✅ File header with `@file`, `@module`, `@description`
- ✅ Function docs with `@param`, `@returns`, `@throws`, `@example`
- ✅ Type definitions with `@typedef` (alongside Zod schemas)

**Current Coverage**: ~60% → Target: 100%

**Enforcement**: ESLint `jsdoc/require-*` rules + manual review

### 6. Antipatterns to Ban

**Rule**: Eliminate known bad practices

**BANNED**:
1. ❌ **Default exports** (327 violations) - Use named exports
2. ❌ **Console.log in src/** (304 violations) - Use OTEL or structured logger
3. ❌ **Defensive programming** - Use Zod validation at boundaries
4. ❌ **Try-catch everywhere** - Handle errors at boundaries
5. ❌ **God Objects** (9 violations) - Split into focused modules
6. ❌ **Magic numbers** - Extract named constants
7. ❌ **Weak auto-generated schemas** - Tighten `z.unknown()` types
8. ❌ **TODO without issue** (17 violations) - Use `FIXME(#123, @user, date)`

**Enforcement**: Custom linting + quality-checks.mjs script

### 7. Linting Rules

**Rule**: Automated enforcement via ESLint + Prettier

**Required Config**: `.eslintrc.json` (see V6-CODE-QUALITY-STANDARDS.md)

**Key Rules**:
- `complexity: ["error", 10]`
- `max-depth: ["error", 3]`
- `max-params: ["error", 4]`
- `max-lines-per-function: ["error", 50]`
- `max-lines: ["error", 500]`
- `no-console: ["error", { allow: ["warn", "error"] }]`
- `no-restricted-imports` for N3
- `jsdoc/*` for documentation

**Prettier**: Configured (100 char line length, single quotes, 2 spaces)

**Enforcement**: Pre-commit hook + CI/CD gates

---

## Implementation Roadmap

### Phase 1: Critical Fixes (Week 1-2)

**Effort**: 14-22 hours

1. **Add ESLint config** (2h)
   - Copy from standards doc
   - Run `pnpm lint --fix`

2. **Remove 7 N3 imports** (6h)
   - 3 CLI commands
   - 4 v6-compat files

3. **Remove 304 console.log** (8-12h)
   - Replace with OTEL spans
   - Add structured logger

4. **Create GitHub issues for 17 TODOs** (2h)
   - Convert to `FIXME(#issue)` format

### Phase 2: Structure Fixes (Week 3-4)

**Effort**: 42-62 hours

5. **Split 9 God Object files** (18-24h)
   - `guards.mjs` (31 exports → 4 modules)
   - `types.mjs` (26 exports → 3 modules)
   - `patterns.mjs` (25 exports → 3 modules)
   - 6 more files

6. **Split 10 largest files** (20-30h)
   - `otel-span-builder.mjs` (1010 → 3 files)
   - `yawl-cancellation.mjs` (954 → 3 files)
   - `agents/index.mjs` (951 → 4 files)
   - 7 more files

7. **Remove 327 default exports** (4-8h)
   - Automated refactor
   - Update imports

### Phase 3: Quality Improvements (Week 5-6)

**Effort**: 44-58 hours

8. **Add JSDoc to undocumented functions** (30-40h)
   - AI-assisted generation
   - Manual review

9. **Extract 10 longest functions** (12-16h)
   - `interceptAtomVMOutput()` (751 lines)
   - `createCoordinator()` (388 lines)
   - 8 more functions

10. **Tighten weak schemas** (2-4h)
    - Replace `z.unknown()` with specific types
    - Review auto-generated schemas

**Total Effort**: 100-142 hours (2.5-3.5 weeks, 2 developers)

---

## Automated Enforcement

### Pre-commit Hook

```bash
# .husky/pre-commit
pnpm lint --max-warnings=0
pnpm test:fast
node scripts/quality-checks.mjs
```

### CI/CD Gates (MUST PASS)

```yaml
# .github/workflows/quality.yml
- Lint: 0 violations
- Test coverage: ≥80%
- File size check: ≤500 lines
- N3 import check: 0 instances
- Console.log check: 0 in src/
- Circular deps: 0
```

### Quality Checks Script

**Run manually**:
```bash
node scripts/quality-checks.mjs
```

**Output**:
- File size violations (33 found)
- Export count violations (9 found)
- Forbidden patterns (7 N3, 304 console, 327 default exports)
- Function length violations (198 found)
- Overall quality score (0-100)

---

## Success Criteria (V6 Launch)

**ALL must be met**:

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| ESLint violations | TBD | 0 | ❌ |
| Files >500 lines | 33 | 0 | ❌ |
| Functions >50 lines | 198 | 0 | ❌ |
| God Objects (>15 exports) | 9 | 0 | ❌ |
| N3 direct imports | 7 | 0 | ❌ |
| Console.log in src/ | 304 | 0 | ❌ |
| Default exports (non-config) | 327 | 0 | ❌ |
| TODO without issue | 17 | 0 | ⚠️ |
| Test coverage | ~70% | ≥80% | ❌ |
| JSDoc coverage | ~60% | 100% | ❌ |
| Circular dependencies | 0 | 0 | ✅ |
| Quality Score | 65/100 | ≥90/100 | ❌ |

**Status**: 1/12 criteria met (8% ready)

---

## Key Files Created

### Documentation

1. **V6-CODE-QUALITY-STANDARDS.md** (18,500 lines)
   - Complete standards specification
   - Rules, rationale, examples
   - Enforcement mechanisms
   - Migration plan

2. **CURRENT-STATE-ANALYSIS.md** (10,000 lines)
   - Empirical codebase analysis
   - 1468 files scanned
   - Specific violations with evidence
   - Risk assessment
   - Prioritized remediation

3. **QUALITY-STANDARDS-SUMMARY.md** (this file)
   - Executive overview
   - Quick reference
   - Implementation roadmap

### Tooling

4. **scripts/quality-checks.mjs** (650 lines)
   - Automated quality scanner
   - Checks all 7 standard categories
   - Colorized reports
   - Exit code for CI/CD

**Location**: `/home/user/unrdf/docs/standards/`

---

## Quick Start

### For Developers

```bash
# 1. Install and check current state
pnpm install
node scripts/quality-checks.mjs

# 2. Fix auto-fixable issues
pnpm lint --fix
pnpm format

# 3. Check progress
node scripts/quality-checks.mjs
```

### For Reviewers

**Before approving PR, verify**:
- [ ] JSDoc complete for all public functions
- [ ] Zod schemas for all inputs/outputs
- [ ] No console.log in src/
- [ ] No default exports (except config files)
- [ ] Functions <50 lines
- [ ] Files <500 lines
- [ ] Complexity <10
- [ ] No N3 direct imports
- [ ] Test coverage ≥80%

### For CI/CD

```bash
# Run full quality suite
node scripts/quality-checks.mjs

# Exit code:
# 0 = Quality score ≥90 (PASS)
# 1 = Quality score <90 (FAIL)
```

---

## Measurement & Tracking

### Weekly Quality Report

```bash
# Generate quality snapshot
node scripts/quality-checks.mjs > quality-$(date +%Y%m%d).log

# Track trends over time
git log --oneline --since="1 week ago" | wc -l  # Commits
grep "violations" quality-*.log | tail -5       # Trend
```

### Quality Score Formula

```
Score = (Checks Passed / Total Checks) × 100

Checks:
1. File sizes ≤500 lines
2. Exports ≤15 per file
3. No forbidden patterns
4. Functions ≤50 lines

Current: 0/4 = 0/100 (with weighted scoring)
Target: 4/4 = 100/100
```

---

## Risk Mitigation

### High-Risk Changes

**YAWL package** (quality score: 52/100):
- 4 files >900 lines
- Low test coverage (68%)
- **Mitigation**: Incremental refactor with 100% test preservation

**Core package** (quality score: 68/100):
- 304 console.log statements
- **Mitigation**: Batch replace with OTEL (automated)

### Low-Risk Changes

**v6-core package** (quality score: 92/100):
- Already meets most standards
- **Action**: Minor cleanup, add missing JSDoc

---

## Questions & Support

**Documentation**:
- Full specs: `/docs/standards/V6-CODE-QUALITY-STANDARDS.md`
- Analysis: `/docs/standards/CURRENT-STATE-ANALYSIS.md`
- Summary: `/docs/standards/QUALITY-STANDARDS-SUMMARY.md` (this file)

**Tooling**:
- Quality checks: `node scripts/quality-checks.mjs`
- ESLint config: Copy from standards doc (not yet created)
- Pre-commit hook: `.husky/pre-commit` (not yet configured)

**Process**:
- Request exception: Create GitHub issue `[QUALITY-WAIVER]`
- Report bug in standards: Create issue `[STANDARDS-BUG]`
- Suggest improvement: Create issue `[STANDARDS-ENHANCEMENT]`

---

## Appendix: Top 10 Violations

### Files to Split First

1. `packages/validation/src/otel-span-builder.mjs` (1010 lines)
2. `packages/yawl/src/cancellation/yawl-cancellation.mjs` (954 lines)
3. `packages/kgc-probe/src/agents/index.mjs` (951 lines)
4. `packages/yawl/src/events/yawl-events.mjs` (849 lines)
5. `packages/fusion/src/kgc-docs-diataxis.mjs` (840 lines)

### God Objects to Split

1. `packages/kgc-4d/src/guards.mjs` (31 exports)
2. `packages/kgc-probe/src/types.mjs` (26 exports)
3. `packages/yawl/src/patterns.mjs` (25 exports)

### Critical N3 Imports to Remove

1. `packages/cli/src/cli/commands/convert.mjs`
2. `packages/cli/src/cli/commands/graph.mjs`
3. `packages/cli/src/cli/commands/query.mjs`
4. `packages/v6-compat/src/adapters.mjs`
5. `packages/v6-compat/src/lint-rules.mjs`

---

**Document Version**: 1.0.0
**Effective Date**: 2025-01-01
**Next Review**: 2025-01-15 (post-Phase 1)
**Owner**: Sean Chatman (@seanchatmangpt)
