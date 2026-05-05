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
| latest | P0 | Create `/esbuild.config.mjs` with all 17 packages as entry points | None (first) |
| latest | P0 | Create `/ruffrc.toml` with 400+ rules (0 suppression comments allowed) | latest |
| latest | P0 | Create `/vitest.config.mjs` with v8 coverage provider, 80% thresholds | latest |
| latest | P0 | Update root `/package.json` scripts: build, lint, test, coverage, check-deps | latest |
| latest | P0 | Create `/pnpm-workspace.yaml` (or verify existing) with all 17 packages | latest |
| latest | P1 | Create `/docs/MONOREPO-DEVELOPMENT.md` with unification guide | latest |

**Validation**:
- `pnpm run build` succeeds (dry run, no files changed yet)
- `pnpm run lint` succeeds on root config files
- All root config files parse without errors

---

### Phase 1.2: Dependency Audit & Alignment

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P0 | Audit all 17 `package.json` files for shared dependencies | latest |
| latest | P0 | Identify version conflicts for top 30 shared dependencies | latest |
| latest | P0 | Create dependency alignment spreadsheet (pkg name → all versions → recommended) | latest |
| latest | P1 | Resolve version conflicts to highest compatible version | latest |
| latest | P1 | Run full test suite after version alignment (verify no regressions) | latest |
| latest | P1 | Update all `package.json` files with aligned versions | latest |
| latest | P1 | Rebuild `pnpm-lock.yaml` with `pnpm install` (deterministic) | latest |
| latest | P0 | Install circular dependency detection tool (madge or similar) | latest |
| latest | P1 | Run circular dependency scan on all packages | latest |
| latest | P1 | Document any found circular dependencies and refactoring plan | latest |

**Validation**:
- `pnpm list` shows all shared deps at single version
- `pnpm install --frozen-lockfile` produces identical lock file
- Circular dependency scan: 0 found (or documented exceptions)
- All tests pass after version alignment

---

### Phase 1.3: Tooling Installation & Validation

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P0 | Install esbuild dependencies: `esbuild`, `esbuild-plugin-dts` | latest |
| latest | P0 | Install Ruff (system or via pnpm) | latest |
| latest | P0 | Install Vitest + v8 coverage provider | latest |
| latest | P0 | Install depcheck for dependency auditing | latest |
| latest | P1 | Verify all tools work: `pnpm run build`, `pnpm run lint`, `pnpm test` | latest |
| latest | P1 | Update CI/CD pipeline (e.g., `.github/workflows/ci.yml`) to use unified commands | latest |

**Validation**:
- All tools installed and runnable
- Root-level build/lint/test commands work
- CI/CD updated to use unified commands

---

## Phase 2: User Story 1 - Standardize Package Structure (P1)

Implement for all 17 packages. This is the foundation for Stories 2-5. Each package follows the same pattern.

### Phase 2.1: Per-Package Structure Migration

For each of 17 packages, execute tasks latest → latest in order:

| Task | Priority | Description | Per-Package? | Effort |
|------|----------|-------------|--------------|--------|
| latest | P1 | Create `packages/{name}/src/` directory | ✓ 17x | 1h |
| latest | P1 | Move source files from `lib/` or root to `src/` | ✓ 17x | 2h |
| latest | P1 | Move test files to `src/` with `*.test.mjs` pattern | ✓ 17x | 1h |
| latest | P1 | Create `src/index.mjs` with consolidated named exports (no default) | ✓ 17x | 1h |
| latest | P1 | Update `packages/{name}/package.json` main/exports fields | ✓ 17x | 30m |
| latest | P1 | Verify no broken imports: run linting on migrated package | ✓ 17x | 30m |
| latest | P1 | Verify tests pass for migrated package (at least smoke test) | ✓ 17x | 1h |

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
| latest | P1 | Write and run structure audit script (verify all 17 have src/index.mjs) | latest→latest for all 17 |
| latest | P1 | Verify all 17 packages have consistent directory layout | latest |
| latest | P1 | Document structure in `/docs/MONOREPO-DEVELOPMENT.md` | latest |
| latest | P1 | Update project templates for new packages to use standard structure | latest |

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
| latest | P1 | Delete per-package build configs (*.esbuild.config.*, *.rollup.config.*) | Phase 2 complete |
| latest | P1 | Delete per-package build scripts from all `package.json` files | latest |
| latest | P1 | Test root build: `pnpm run build` on all 17 packages | latest |
| latest | P1 | Verify all `packages/*/dist/index.mjs` files created | latest |
| latest | P1 | Verify all `packages/*/dist/index.d.ts` (TypeScript definitions) created | latest |
| latest | P1 | Verify build time <30 seconds (measure with `time pnpm run build`) | latest |
| latest | P1 | Document build troubleshooting in development guide | latest |

**Validation**:
- All 17 packages build successfully
- dist/ files exist for all packages
- Build time <30s
- No build errors or warnings

---

### Phase 3.2: Linting Unification

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P1 | Delete per-package lint configs (.eslintrc*, .prettierrc*, etc.) | Phase 2 complete |
| latest | P1 | Run `pnpm run lint` on all packages | latest |
| latest | P1 | Fix ALL linting violations (0 suppression comments allowed) | latest |
| latest | P1 | Re-run linting to verify 0 violations | latest |
| latest | P1 | Add pre-commit hook to run linting automatically | latest |
| latest | P1 | Document linting rules and exceptions (if any) | latest |

**Validation**:
- `pnpm run lint` exits with code 0
- No linting violations in any package
- Pre-commit hook installed and working

**⚠️ Critical Rule**: NO suppression comments allowed. Fix violations, don't suppress them.

---

### Phase 3.3: Test Runner Unification

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P1 | Delete per-package test configs (jest.config.*, vitest.config.* from packages/) | Phase 2 complete |
| latest | P1 | Delete per-package test scripts from `package.json` | latest |
| latest | P1 | Verify all test files follow `*.test.mjs` pattern | latest |
| latest | P1 | Run `pnpm test` to execute all tests | latest |
| latest | P1 | Run `pnpm run coverage` to generate coverage report | latest |
| latest | P1 | Identify packages below 80% coverage | latest |
| latest | P1 | For each package <80%: add tests to reach ≥80% coverage | latest |
| latest | P1 | Re-run coverage to verify all ≥80% | latest |
| latest | P1 | Generate final coverage report (HTML) | latest |

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
| latest | P1 | Run `depcheck` on each package to find unused dependencies | Phase 3 complete |
| latest | P1 | Manually verify depcheck findings (false positives possible) | latest |
| latest | P1 | Remove all confirmed unused dependencies from `package.json` | latest |
| latest | P1 | Run `pnpm install` to rebuild lock file | latest |
| latest | P1 | Verify all tests still pass after dependency removal | latest |
| latest | P1 | Document any edge cases or false positives found | latest |

**Validation**:
- depcheck: 0 unused dependencies (or documented exceptions)
- All tests pass after removal
- Lock file updated

---

### Phase 4.2: Dependency Determinism Verification

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P1 | Clean install from scratch: `rm pnpm-lock.yaml && pnpm install` | latest |
| latest | P1 | Compare new lock file with previous: `git diff pnpm-lock.yaml` | latest |
| latest | P1 | Verify identical (only diff should be lock version metadata if applicable) | latest |
| latest | P1 | Verify node_modules size is acceptable (measure with `du -sh node_modules`) | latest |
| latest | P1 | Document final dependency state: shared versions, optional dependencies | latest |

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
| latest | P2 | Audit `src/index.mjs` in each package for default exports | Phase 3 complete |
| latest | P2 | Convert any default exports to named exports | latest |
| latest | P2 | Update consumer code if any internal packages import defaults | latest |
| latest | P2 | Verify all exports are named-only (no `export default`) | latest |
| latest | P2 | Document public API for each package | latest |

**Validation**:
- All exports are named
- No default exports
- Consumer code updated
- API documentation complete

---

### Phase 5.2: TypeScript Definition Generation

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P2 | Verify `esbuild-plugin-dts` installed and configured | Phase 1 complete |
| latest | P2 | Run `pnpm run build` to trigger .d.ts generation | latest |
| latest | P2 | Verify all `packages/*/dist/index.d.ts` files generated | latest |
| latest | P2 | Validate TypeScript definitions: `npx tsc --noEmit --allowJs dist/**/*.d.ts` | latest |
| latest | P2 | Fix any type errors in definitions (usually from JSDoc annotations) | latest |
| latest | P2 | Verify all public exports have JSDoc 100% type coverage | latest |

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
| latest | P2 | Audit all 17 `package.json` for: name, version, description, license, keywords | Phase 5 complete |
| latest | P2 | Add/update descriptions for packages missing them (clear, one-line) | latest |
| latest | P2 | Verify all packages have `license: "MIT"` | latest |
| latest | P2 | Add/verify keywords (min 3, max 10) for each package | latest |
| latest | P2 | Update repository field to point to monorepo + package directory | latest |
| latest | P2 | Verify all export configurations correct in package.json | latest |
| latest | P2 | Validate all package.json files with `jq` or JSON schema validator | latest |

**Validation**:
- All package.json files have required fields
- Descriptions are clear and accurate
- Repository fields correct
- Keywords relevant

---

### Phase 6.2: README & LICENSE Files

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P2 | Copy monorepo LICENSE to all 17 packages | latest |
| latest | P2 | Create/update README.md for each package with: title, description, installation, usage, API | latest |
| latest | P2 | Include public API section in each README (exported functions/classes) | latest |
| latest | P2 | Verify all READMEs follow template format | latest |
| latest | P2 | Validate all README links work (relative and external) | latest |

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
| latest | P1 | Run structure audit: verify all 17 packages have `src/index.mjs` | Phase 6 complete |
| latest | P1 | Verify all packages build successfully | latest |
| latest | P1 | Verify all `dist/` directories created correctly | latest |
| latest | P1 | Verify all test files follow `*.test.mjs` pattern | latest |

**Validation**: All checks pass

---

### Phase 7.2: Quality Gates

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P1 | Run linting: `pnpm run lint` → exit code 0 | Phase 6 complete |
| latest | P1 | Run tests: `pnpm test` → all tests pass | latest |
| latest | P1 | Run coverage: `pnpm run coverage` → all ≥80% | latest |
| latest | P1 | Verify build time: `time pnpm run build` → <30 seconds | latest |
| latest | P1 | Verify no circular dependencies: `pnpm run check-deps` | latest |

**Validation**: All quality gates pass

---

### Phase 7.3: Documentation & Knowledge Transfer

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P2 | Update `/docs/MONOREPO-DEVELOPMENT.md` with unification details | Phase 7.2 complete |
| latest | P2 | Create migration guide for external consumers (if breaking changes) | latest |
| latest | P2 | Document troubleshooting guide for common issues | latest |
| latest | P2 | Create pull request with all changes | latest |
| latest | P2 | Internal review: verify unification meets all spec requirements | latest |

**Validation**: Documentation complete, PR ready

---

## Phase 8: Polish & Cross-Cutting Concerns (P3 - Optional)

Nice-to-have improvements after core unification complete.

### Phase 8.1: Performance & Optimization

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P3 | Profile build time per package: identify slowest builds | Phase 7 complete |
| latest | P3 | Optimize esbuild config if any package >5s build | latest |
| latest | P3 | Profile test execution time per package | latest |
| latest | P3 | Optimize test setup/teardown if tests >60s | latest |
| latest | P3 | Document performance baseline (build time, test time, bundle size) | latest |

---

### Phase 8.2: Enhanced Automation

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P3 | Set up automated dependency update tool (Dependabot) | Phase 7 complete |
| latest | P3 | Create GitHub Actions workflow for CI/CD (if not present) | latest |
| latest | P3 | Configure pre-commit hooks for all developers | latest |
| latest | P3 | Document CI/CD pipeline in development guide | latest |

---

### Phase 8.3: Observability & Monitoring (OTEL)

| Task | Priority | Description | Dependencies |
|------|----------|-------------|--------------|
| latest | P3 | Add OTEL instrumentation to build process (optional: measure build telemetry) | Phase 7 complete |
| latest | P3 | Add metrics collection for test execution (coverage per package, duration) | latest |
| latest | P3 | Create dashboard/report showing monorepo health metrics | latest |

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
