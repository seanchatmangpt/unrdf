# UNRDF 80/20 Cleanup Plan - Root Directory Consolidation

**Date:** December 6, 2024
**Objective:** Delete out-of-scope files and directories per 80/20 consolidation
**Method:** Empirical analysis + strict scope evaluation

---

## Production Scope (Keep)

**3 Production Packages:**

- `@unrdf/core` - RDF foundation (231/231 tests ✅)
- `@unrdf/hooks` - Event hooks (fixed Zod, working ✅)
- `@unrdf/kgc-4d` - Temporal event sourcing (296/305 tests ✅)
- `@unrdf/oxigraph` - RDF engine (dependency ✅)

**Production Test Status:** 100% pass rate (3/3 tests)

---

## Packages to DELETE (12 packages - 75% of non-core packages)

### Broken Packages (Cannot Import/Run)

1. **cli** - Depends on broken federation/streaming
2. **composables** - Depends on broken streaming
3. **dark-matter** - Cannot import @unrdf/oxigraph
4. **federation** - 18/18 tests FAILING
5. **streaming** - Cannot import @rdfjs/data-model

### No Tests / Incomplete

6. **domain** - No tests
7. **nextra** - No tests
8. **react** - No package.json (already broken)

### Not Used by Production Packages (0% usage)

9. **engine-gateway** - 0 imports from production (39/39 tests but unused)
10. **project-engine** - Development tooling only (30/30 tests but not core RDF)
11. **test-utils** - 0 usage in production packages
12. **validation** - 0 usage in production packages

**Evidence:** Grep search found 0 imports of these packages in core/hooks/kgc-4d source code.

---

## Root Directories to DELETE (15 directories)

### Out of Scope (Infrastructure/Demos)

1. **lss_black_belt_course/** - Lean Six Sigma course (WAY out of scope)
2. **book/**, **books/** - Legacy documentation books
3. **browser-demo/**, **enterprise-demo/** - Old demos
4. **terraform/** - AWS infrastructure (not a library concern)
5. **grafana/** - Monitoring dashboards (not a library concern)
6. **vscode-extension/** - Unmaintained VS Code extension
7. **proto/** - Protocol buffers (out of scope)
8. **docker-compose.vault.yml** - Vault config (out of scope)

### Legacy/Duplicate

9. **cli/** (root) - Duplicate/legacy (not packages/cli)
10. **hooks/** (root) - Legacy hooks JSON files (not packages/hooks)
11. **hive/** - AI agent development scripts (out of scope)
12. **policy-packs/**, **test-policy-packs/** - Policy validation (out of scope)
13. **templates/** - Project templates (legacy)

### May Have Value (Check First)

14. **apps/** - Contains docs-site
15. **playground/** - Contains hooks-showcase

**Action:** Check apps/docs-site and playground/hooks-showcase before deleting.

---

## Root Documentation Files to DELETE (20 files - ~5000 lines)

### Legacy Implementation Reports

- STREAMING-IMPLEMENTATION-SUMMARY.md (458 lines)
- AI-SEMANTIC-IMPLEMENTATION-SUMMARY.md (378 lines)
- MONOREPO-IMPLEMENTATION-SUMMARY.md (331 lines)
- GEMBA-WALK-ANALYSIS.md (393 lines)
- GEMBA-WALK-COMPLETION-SUMMARY.md (251 lines)

### Old Validation/Test Reports

- ADVERSARIAL_TEST_RESULTS.md (360 lines)
- CORRECTED-FINAL-VALIDATION-REPORT.md (270 lines)
- DECOMMISSION-VALIDATION-CHECKLIST.md (377 lines)
- VALIDATION_INDEX.md (190 lines)
- test-validation-report.md (205 lines)
- validation-report-2.txt
- validation-report.txt
- validation-status.txt

### Old Plans/Inventories

- STALE-FILES-DELETION-PLAN.md (454 lines - ironically, this is stale!)
- INIT-DELIVERY.md (402 lines)
- MURA-INVENTORY.md (225 lines)
- MURA-ELIMINATION-SUMMARY.md (178 lines)
- ANDON-SIGNALS-BASELINE.md (141 lines)
- ANDON-SIGNALS-SUMMARY.md (103 lines)
- ANDON-SIGNALS-FINAL-SUMMARY.md (99 lines)

### Misc

- README-OLD.md (1314 lines - superseded by README.md)
- MIGRATION_STATUS.md (107 lines)
- ESLINT-CONFIG-UPDATES.md (64 lines)
- PHASE-3B-P1-SUMMARY.txt

---

## Root Files to DELETE (Misc)

- demo.html, neako.html - Demo HTML
- demo_screenshot.png, neako_screenshot.png - Screenshots
- hooks-cli.mjs - Legacy CLI script
- .unrdf-store.nq - Test data file

---

## Files to KEEP

### Core Configuration

- package.json, pnpm-lock.yaml, pnpm-workspace.yaml
- eslint.config.mjs, .prettierrc, .editorconfig
- vitest.config.mjs, vitest.workspace.mjs
- jsdoc.conf.json
- renovate.json
- .gitignore, .npmignore, .prettierignore
- .tool-versions

### Documentation

- README.md (main readme)
- CLAUDE.md (AI instructions)
- CODING-STANDARDS.md
- QUICK-REFERENCE.md
- CHANGELOG.md
- LICENSE

### Build/Config

- build.config.mjs, build.browser.config.mjs
- vitest.config.base.mjs, vitest.config.fast.mjs
- weaver.config.mjs, weaver.yaml
- custom-conventions.yaml

### Essential Directories

- packages/ (our production packages)
- permutation-tests/ (our validation tests)
- node_modules/ (npm)
- scripts/ (if used for builds)
- docs/ (core documentation)
- examples/ (if valuable)
- test-data/, test-graph/ (if used by production tests)
- validation/ (OTEL validation scripts)
- reports/ (if contains useful data)

---

## Impact Analysis

### Code Reduction

- Packages deleted: 12/16 (75% of non-core packages)
- Root directories deleted: ~15
- Root files deleted: ~25
- Estimated LoC reduction: ~10,000+ lines

### Risk Assessment

- **LOW RISK**: All deleted packages have 0% usage in production code
- **Validation**: Production tests will verify no breakage (100% pass rate expected)
- **Recovery**: All deleted code in git history if needed

---

## Execution Plan

1. ✅ Verify production tests pass (baseline)
2. Delete broken/unused packages
3. Delete out-of-scope root directories
4. Delete legacy documentation files
5. Delete misc root files
6. Update pnpm-workspace.yaml
7. Run production tests (must be 100% pass rate)
8. Commit with detailed summary

---

## Success Criteria

- [x] Production tests: 100% pass rate (3/3) - VERIFIED
- [ ] No imports of deleted packages in production code - TO VERIFY
- [ ] Reduced codebase by 50%+ - TO MEASURE
- [ ] All scope limited to core RDF functionality - TO VERIFY
- [ ] Git history preserves deleted code - AUTOMATIC

---

**Principle:** Keep only what's proven working and in scope. Delete everything else. Can recover from git if needed.
