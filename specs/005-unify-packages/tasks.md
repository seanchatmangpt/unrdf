# Task Breakdown: Unify Packages

**Feature**: 005-unify-packages
**Date**: 2025-12-20
**Status**: Phase 2 (Task Generation)
**Generated From**: spec.md + plan.md + research.md + data-model.md

---

## Executive Summary

This document translates the feature specification and design into 67 actionable tasks organized by:
- **Phase** (setup, implementation, validation)
- **Priority** (P1 critical → P3 nice-to-have)
- **User Story** (which story each task serves)
- **Dependencies** (what must complete first)

**Total Effort**: ~80-120 developer-hours across 17 packages
**Success Criteria**: All 17 packages standardized, build <30s, lint 0 violations, tests ≥80% coverage

---

## Phase 1: Setup & Infrastructure (Shared, All Stories)

These tasks create the foundation for all package unification. Complete sequentially before user story implementation.

### Phase 1.1: Monorepo Root Configuration

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 1.1.1 | P0 | Create `/esbuild.config.mjs` with all 17 packages as entry points | None (first) |
| 1.1.2 | P0 | Create `/ruffrc.toml` with 400+ rules (0 suppression comments allowed) | 1.1.1 |
| 1.1.3 | P0 | Create `/vitest.config.mjs` with v8 coverage provider, 80% thresholds | 1.1.2 |
| 1.1.4 | P0 | Update root `/package.json` scripts: build, lint, test, coverage, check-deps | 1.1.3 |
| 1.1.5 | P0 | Create `/pnpm-workspace.yaml` (or verify existing) with all 17 packages | 1.1.4 |
| 1.1.6 | P1 | Create `/docs/MONOREPO-DEVELOPMENT.md` with unification guide | 1.1.5 |

**Validation**:
- `pnpm run build` succeeds (dry run, no files changed yet)
- `pnpm run lint` succeeds on root config files
- All root config files parse without errors

---

### Phase 1.2: Dependency Audit & Alignment

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 1.2.1 | P0 | Audit all 17 `package.json` files for shared dependencies | 1.1.5 |
| 1.2.2 | P0 | Identify version conflicts for top 30 shared dependencies | 1.2.1 |
| 1.2.3 | P0 | Create dependency alignment spreadsheet (pkg name → all versions → recommended) | 1.2.2 |
| 1.2.4 | P1 | Resolve version conflicts to highest compatible version | 1.2.3 |
| 1.2.5 | P1 | Run full test suite after version alignment (verify no regressions) | 1.2.4 |
| 1.2.6 | P1 | Update all `package.json` files with aligned versions | 1.2.5 |
| 1.2.7 | P1 | Rebuild `pnpm-lock.yaml` with `pnpm install` (deterministic) | 1.2.6 |
| 1.2.8 | P0 | Install circular dependency detection tool (madge or similar) | 1.2.7 |
| 1.2.9 | P1 | Run circular dependency scan on all packages | 1.2.8 |
| 1.2.10 | P1 | Document any found circular dependencies and refactoring plan | 1.2.9 |

**Validation**:
- `pnpm list` shows all shared deps at single version
- `pnpm install --frozen-lockfile` produces identical lock file
- Circular dependency scan: 0 found (or documented exceptions)
- All tests pass after version alignment

---

### Phase 1.3: Tooling Installation & Validation

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 1.3.1 | P0 | Install esbuild dependencies: `esbuild`, `esbuild-plugin-dts` | 1.1.5 |
| 1.3.2 | P0 | Install Ruff (system or via pnpm) | 1.3.1 |
| 1.3.3 | P0 | Install Vitest + v8 coverage provider | 1.3.2 |
| 1.3.4 | P0 | Install depcheck for dependency auditing | 1.3.3 |
| 1.3.5 | P1 | Verify all tools work: `pnpm run build`, `pnpm run lint`, `pnpm test` | 1.3.4 |
| 1.3.6 | P1 | Update CI/CD pipeline (e.g., `.github/workflows/ci.yml`) to use unified commands | 1.3.5 |

**Validation**:
- All tools installed and runnable
- Root-level build/lint/test commands work
- CI/CD updated to use unified commands

---

## Phase 2: User Story 1 - Standardize Package Structure (P1)

Implement for all 17 packages. This is the foundation for Stories 2-5. Each package follows the same pattern.

### Phase 2.1: Per-Package Structure Migration

For each of 17 packages, execute tasks 2.1.1 → 2.1.7 in order:

| Task | Priority | Description | Per-Package? | Effort |
|------|----------|-------------|--------------|--------|
| 2.1.1 | P1 | Create `packages/{name}/src/` directory | ✓ 17x | 1h |
| 2.1.2 | P1 | Move source files from `lib/` or root to `src/` | ✓ 17x | 2h |
| 2.1.3 | P1 | Move test files to `src/` with `*.test.mjs` pattern | ✓ 17x | 1h |
| 2.1.4 | P1 | Create `src/index.mjs` with consolidated named exports (no default) | ✓ 17x | 1h |
| 2.1.5 | P1 | Update `packages/{name}/package.json` main/exports fields | ✓ 17x | 30m |
| 2.1.6 | P1 | Verify no broken imports: run linting on migrated package | ✓ 17x | 30m |
| 2.1.7 | P1 | Verify tests pass for migrated package (at least smoke test) | ✓ 17x | 1h |

**Dependencies**: Phase 1 complete (1.1-1.3)

**Validation per package**:
- `src/index.mjs` exists and exports named exports only
- `src/*.test.mjs` files exist and can be run
- No broken imports detected
- Smoke tests pass

**Estimated effort**: 42 hours (2.5h × 17 packages), can be parallelized by package

---

### Phase 2.2: Centralized Structure Validation

After all 17 packages migrated:

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 2.2.1 | P1 | Write and run structure audit script (verify all 17 have src/index.mjs) | 2.1.1→2.1.7 for all 17 |
| 2.2.2 | P1 | Verify all 17 packages have consistent directory layout | 2.2.1 |
| 2.2.3 | P1 | Document structure in `/docs/MONOREPO-DEVELOPMENT.md` | 2.2.2 |
| 2.2.4 | P1 | Update project templates for new packages to use standard structure | 2.2.3 |

**Validation**:
- Audit script: 17/17 packages pass
- Consistency verified across all packages
- Documentation complete

---

## Phase 3: User Story 2 - Unify Build and Tooling (P1)

Build all packages with unified esbuild config; ensure 0 linting violations; run tests with unified Vitest config.

### Phase 3.1: Build System Unification

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 3.1.1 | P1 | Delete per-package build configs (*.esbuild.config.*, *.rollup.config.*) | Phase 2 complete |
| 3.1.2 | P1 | Delete per-package build scripts from all `package.json` files | 3.1.1 |
| 3.1.3 | P1 | Test root build: `pnpm run build` on all 17 packages | 3.1.2 |
| 3.1.4 | P1 | Verify all `packages/*/dist/index.mjs` files created | 3.1.3 |
| 3.1.5 | P1 | Verify all `packages/*/dist/index.d.ts` (TypeScript definitions) created | 3.1.4 |
| 3.1.6 | P1 | Verify build time <30 seconds (measure with `time pnpm run build`) | 3.1.5 |
| 3.1.7 | P1 | Document build troubleshooting in development guide | 3.1.6 |

**Validation**:
- All 17 packages build successfully
- dist/ files exist for all packages
- Build time <30s
- No build errors or warnings

---

### Phase 3.2: Linting Unification

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 3.2.1 | P1 | Delete per-package lint configs (.eslintrc*, .prettierrc*, etc.) | Phase 2 complete |
| 3.2.2 | P1 | Run `pnpm run lint` on all packages | 3.2.1 |
| 3.2.3 | P1 | Fix ALL linting violations (0 suppression comments allowed) | 3.2.2 |
| 3.2.4 | P1 | Re-run linting to verify 0 violations | 3.2.3 |
| 3.2.5 | P1 | Add pre-commit hook to run linting automatically | 3.2.4 |
| 3.2.6 | P1 | Document linting rules and exceptions (if any) | 3.2.5 |

**Validation**:
- `pnpm run lint` exits with code 0
- No linting violations in any package
- Pre-commit hook installed and working

**⚠️ Critical Rule**: NO suppression comments allowed. Fix violations, don't suppress them.

---

### Phase 3.3: Test Runner Unification

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 3.3.1 | P1 | Delete per-package test configs (jest.config.*, vitest.config.* from packages/) | Phase 2 complete |
| 3.3.2 | P1 | Delete per-package test scripts from `package.json` | 3.3.1 |
| 3.3.3 | P1 | Verify all test files follow `*.test.mjs` pattern | 3.3.2 |
| 3.3.4 | P1 | Run `pnpm test` to execute all tests | 3.3.3 |
| 3.3.5 | P1 | Run `pnpm run coverage` to generate coverage report | 3.3.4 |
| 3.3.6 | P1 | Identify packages below 80% coverage | 3.3.5 |
| 3.3.7 | P1 | For each package <80%: add tests to reach ≥80% coverage | 3.3.6 |
| 3.3.8 | P1 | Re-run coverage to verify all ≥80% | 3.3.7 |
| 3.3.9 | P1 | Generate final coverage report (HTML) | 3.3.8 |

**Validation**:
- All tests pass (0 failures)
- All 17 packages ≥80% coverage
- Coverage report generated and verified

---

## Phase 4: User Story 3 - Consolidate Dependencies (P1)

Clean up unused dependencies; verify deterministic installs.

### Phase 4.1: Unused Dependency Removal

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 4.1.1 | P1 | Run `depcheck` on each package to find unused dependencies | Phase 3 complete |
| 4.1.2 | P1 | Manually verify depcheck findings (false positives possible) | 4.1.1 |
| 4.1.3 | P1 | Remove all confirmed unused dependencies from `package.json` | 4.1.2 |
| 4.1.4 | P1 | Run `pnpm install` to rebuild lock file | 4.1.3 |
| 4.1.5 | P1 | Verify all tests still pass after dependency removal | 4.1.4 |
| 4.1.6 | P1 | Document any edge cases or false positives found | 4.1.5 |

**Validation**:
- depcheck: 0 unused dependencies (or documented exceptions)
- All tests pass after removal
- Lock file updated

---

### Phase 4.2: Dependency Determinism Verification

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 4.2.1 | P1 | Clean install from scratch: `rm pnpm-lock.yaml && pnpm install` | 4.1.6 |
| 4.2.2 | P1 | Compare new lock file with previous: `git diff pnpm-lock.yaml` | 4.2.1 |
| 4.2.3 | P1 | Verify identical (only diff should be lock version metadata if applicable) | 4.2.2 |
| 4.2.4 | P1 | Verify node_modules size is acceptable (measure with `du -sh node_modules`) | 4.2.3 |
| 4.2.5 | P1 | Document final dependency state: shared versions, optional dependencies | 4.2.4 |

**Validation**:
- Clean install produces identical lock file
- node_modules size documented
- Determinism confirmed

---

## Phase 5: User Story 4 - Align Exports & Generate Definitions (P2)

Convert any remaining default exports to named exports; generate TypeScript definitions.

### Phase 5.1: Export Alignment

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 5.1.1 | P2 | Audit `src/index.mjs` in each package for default exports | Phase 3 complete |
| 5.1.2 | P2 | Convert any default exports to named exports | 5.1.1 |
| 5.1.3 | P2 | Update consumer code if any internal packages import defaults | 5.1.2 |
| 5.1.4 | P2 | Verify all exports are named-only (no `export default`) | 5.1.3 |
| 5.1.5 | P2 | Document public API for each package | 5.1.4 |

**Validation**:
- All exports are named
- No default exports
- Consumer code updated
- API documentation complete

---

### Phase 5.2: TypeScript Definition Generation

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 5.2.1 | P2 | Verify `esbuild-plugin-dts` installed and configured | Phase 1 complete |
| 5.2.2 | P2 | Run `pnpm run build` to trigger .d.ts generation | 5.1.5 |
| 5.2.3 | P2 | Verify all `packages/*/dist/index.d.ts` files generated | 5.2.2 |
| 5.2.4 | P2 | Validate TypeScript definitions: `npx tsc --noEmit --allowJs dist/**/*.d.ts` | 5.2.3 |
| 5.2.5 | P2 | Fix any type errors in definitions (usually from JSDoc annotations) | 5.2.4 |
| 5.2.6 | P2 | Verify all public exports have JSDoc 100% type coverage | 5.2.5 |

**Validation**:
- All .d.ts files generated
- TypeScript validation: 0 errors
- JSDoc coverage 100% on public APIs

---

## Phase 6: User Story 5 - Standardize Metadata (P2)

Complete package.json metadata, README files, LICENSE files.

### Phase 6.1: Package Metadata Standardization

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 6.1.1 | P2 | Audit all 17 `package.json` for: name, version, description, license, keywords | Phase 5 complete |
| 6.1.2 | P2 | Add/update descriptions for packages missing them (clear, one-line) | 6.1.1 |
| 6.1.3 | P2 | Verify all packages have `license: "MIT"` | 6.1.2 |
| 6.1.4 | P2 | Add/verify keywords (min 3, max 10) for each package | 6.1.3 |
| 6.1.5 | P2 | Update repository field to point to monorepo + package directory | 6.1.4 |
| 6.1.6 | P2 | Verify all export configurations correct in package.json | 6.1.5 |
| 6.1.7 | P2 | Validate all package.json files with `jq` or JSON schema validator | 6.1.6 |

**Validation**:
- All package.json files have required fields
- Descriptions are clear and accurate
- Repository fields correct
- Keywords relevant

---

### Phase 6.2: README & LICENSE Files

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 6.2.1 | P2 | Copy monorepo LICENSE to all 17 packages | 6.1.7 |
| 6.2.2 | P2 | Create/update README.md for each package with: title, description, installation, usage, API | 6.2.1 |
| 6.2.3 | P2 | Include public API section in each README (exported functions/classes) | 6.2.2 |
| 6.2.4 | P2 | Verify all READMEs follow template format | 6.2.3 |
| 6.2.5 | P2 | Validate all README links work (relative and external) | 6.2.4 |

**Validation**:
- All 17 packages have README.md
- All 17 packages have LICENSE
- READMEs follow standard format
- No broken links

---

## Phase 7: Validation & Testing (P1 - Blocking)

Comprehensive validation that entire unification is production-ready.

### Phase 7.1: Structure Validation

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 7.1.1 | P1 | Run structure audit: verify all 17 packages have `src/index.mjs` | Phase 6 complete |
| 7.1.2 | P1 | Verify all packages build successfully | 7.1.1 |
| 7.1.3 | P1 | Verify all `dist/` directories created correctly | 7.1.2 |
| 7.1.4 | P1 | Verify all test files follow `*.test.mjs` pattern | 7.1.3 |

**Validation**: All checks pass

---

### Phase 7.2: Quality Gates

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 7.2.1 | P1 | Run linting: `pnpm run lint` → exit code 0 | Phase 6 complete |
| 7.2.2 | P1 | Run tests: `pnpm test` → all tests pass | 7.2.1 |
| 7.2.3 | P1 | Run coverage: `pnpm run coverage` → all ≥80% | 7.2.2 |
| 7.2.4 | P1 | Verify build time: `time pnpm run build` → <30 seconds | 7.2.3 |
| 7.2.5 | P1 | Verify no circular dependencies: `pnpm run check-deps` | 7.2.4 |

**Validation**: All quality gates pass

---

### Phase 7.3: Documentation & Knowledge Transfer

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 7.3.1 | P2 | Update `/docs/MONOREPO-DEVELOPMENT.md` with unification details | Phase 7.2 complete |
| 7.3.2 | P2 | Create migration guide for external consumers (if breaking changes) | 7.3.1 |
| 7.3.3 | P2 | Document troubleshooting guide for common issues | 7.3.2 |
| 7.3.4 | P2 | Create pull request with all changes | 7.3.3 |
| 7.3.5 | P2 | Internal review: verify unification meets all spec requirements | 7.3.4 |

**Validation**: Documentation complete, PR ready

---

## Phase 8: Polish & Cross-Cutting Concerns (P3 - Optional)

Nice-to-have improvements after core unification complete.

### Phase 8.1: Performance & Optimization

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 8.1.1 | P3 | Profile build time per package: identify slowest builds | Phase 7 complete |
| 8.1.2 | P3 | Optimize esbuild config if any package >5s build | 8.1.1 |
| 8.1.3 | P3 | Profile test execution time per package | 8.1.2 |
| 8.1.4 | P3 | Optimize test setup/teardown if tests >60s | 8.1.3 |
| 8.1.5 | P3 | Document performance baseline (build time, test time, bundle size) | 8.1.4 |

---

### Phase 8.2: Enhanced Automation

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 8.2.1 | P3 | Set up automated dependency update tool (Dependabot) | Phase 7 complete |
| 8.2.2 | P3 | Create GitHub Actions workflow for CI/CD (if not present) | 8.2.1 |
| 8.2.3 | P3 | Configure pre-commit hooks for all developers | 8.2.2 |
| 8.2.4 | P3 | Document CI/CD pipeline in development guide | 8.2.3 |

---

### Phase 8.3: Observability & Monitoring (OTEL)

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| 8.3.1 | P3 | Add OTEL instrumentation to build process (optional: measure build telemetry) | Phase 7 complete |
| 8.3.2 | P3 | Add metrics collection for test execution (coverage per package, duration) | 8.3.1 |
| 8.3.3 | P3 | Create dashboard/report showing monorepo health metrics | 8.3.2 |

---

## Task Summary & Effort Estimation

### By Priority

| Priority | Count | Effort | Critical? |
|----------|-------|--------|-----------|
| **P0** | 17 | 8h | YES - Must complete |
| **P1** | 40 | 96h | YES - Must complete |
| **P2** | 10 | 16h | SHOULD - Feature completeness |
| **P3** | 5 | 8h | OPTIONAL - Polish |

**Total**: 67 tasks, ~128 developer-hours

### By Phase

| Phase | Tasks | Effort | Duration |
|-------|-------|--------|----------|
| 1: Setup | 20 | 8h | 1-2 days |
| 2: Structure | 12 | 42h | 2-3 weeks (parallelizable) |
| 3: Build/Lint/Test | 20 | 24h | 1-2 weeks |
| 4: Dependencies | 11 | 12h | 1-2 days |
| 5: Exports | 11 | 8h | 1-2 days |
| 6: Metadata | 12 | 8h | 1-2 days |
| 7: Validation | 13 | 12h | 1 day |
| 8: Polish | 5 | 8h | 1-2 days (optional) |

**Critical Path** (P0 + P1 only): ~104 hours, ~4-6 weeks with serial execution, ~1-2 weeks with parallelization

---

## Execution Strategy

### Sequential (Safe, Predictable)
Execute phases 1 → 2 → 3 → 4 → 5 → 6 → 7 → 8 in order.
- Pros: Low risk, easy to debug
- Cons: ~6 weeks
- Use when: First time or high-risk environment

### Parallelized (Fast, Requires Coordination)
1. **Phase 1** (Setup): 1-2 days (required before anything else)
2. **Phase 2** (Structure): 17 packages in parallel, 2-3 weeks
3. **Phases 3-6** (Tooling/Deps/Exports/Metadata): Parallel, 2-3 weeks
4. **Phase 7** (Validation): 1 day
5. **Phase 8** (Polish): Optional, 1-2 days

**Recommended**: Parallelized by package for Phase 2, sequential for phases 1, 3-7.

---

## Rollback & Recovery

If critical issue found during implementation:

1. **Before phase completion**: Use `git stash` to discard changes
2. **During phase**: Revert affected packages only
3. **After validation**: Branch + PR review before merge to main

All work tracked in feature branch `005-unify-packages` until Phase 7 validation passes.

---

## Success Metrics (Must Pass All)

✅ **Structure**: All 17 packages have `src/index.mjs`
✅ **Build**: `pnpm run build` succeeds in <30s, all dist/ created
✅ **Lint**: `pnpm run lint` exits code 0, 0 violations
✅ **Tests**: All tests pass, ≥80% coverage in all 17 packages
✅ **Dependencies**: Single version per shared dep, 0 unused deps, 0 circular deps
✅ **Exports**: All named exports, all .d.ts generated, 0 type errors
✅ **Metadata**: All package.json/README/LICENSE complete and valid

---

## Next Steps (After Phase 2 Complete)

1. Run `/speckit.implement` to begin Phase 3 (Refinement) with code generation
2. Or: Assign Phase 2 implementation tasks to individual developers
3. Sync with team on parallelization strategy
4. Set up monitoring/metrics for build time, test duration, coverage

---

**Generated**: 2025-12-20
**Branch**: 005-unify-packages
**Status**: Ready for implementation
