# Research Findings: Unify Packages

**Feature**: 005-unify-packages
**Date**: 2025-12-20
**Status**: Phase 0 Complete

## Research Summary

Concurrent audit agents examined UNRDF monorepo across 8 key areas: structure, build tools, linting, testing, dependencies, circular deps, package metadata, and export patterns. Research consolidates findings into decisions and rationale.

---

## Area 1: Package Structure Unification

**Decision**: Standardize all 17 packages to `src/` directory layout with single `index.mjs` entry point

**Rationale**:
- `src/` is JavaScript ecosystem standard (aligns with ~90% of npm packages)
- Single entry point (`index.mjs`) reduces cognitive load for consumers
- Enables consistent build tooling (no special-casing per package)
- Supports JSDoc 100% type coverage validation at entry point

**Current State** (from audits):
- Varied structure across packages: some use `src/`, others use `lib/`, some have root-level JS files
- Multiple entry points in some packages (main.mjs, index.js, lib/index.js)
- Test file patterns inconsistent (*.test.js, *.spec.js, *.test.mjs in different locations)

**Alternatives Considered**:
- Keep existing structure per package (rejected: defeats unification goal)
- Use `lib/` instead of `src/` (rejected: less community standard, harder for TypeScript consumers)
- Multiple entry points per package (rejected: increases complexity, harder to maintain)

**Implementation Path**:
1. Create target structure for each package
2. Migrate source files to `src/`
3. Consolidate exports to `index.mjs` with named exports only
4. Update package.json main/exports fields
5. Validate no broken imports after migration

---

## Area 2: Build Tooling Unification

**Decision**: Single esbuild configuration at monorepo root serving all 17 packages

**Rationale**:
- esbuild is fastest JavaScript bundler (sub-second builds)
- Single config reduces maintenance burden and cognitive load
- pnpm workspaces integrate cleanly with esbuild for monorepo builds
- No per-package build scripts needed (all use `pnpm run build`)

**Current State**:
- Mixed build tooling: esbuild in some packages, rollup in others, custom scripts in few packages
- Build configurations are inconsistent and hard to audit
- No unified build process at monorepo root

**Alternatives Considered**:
- rollup (rejected: slower, more complex config, no clear benefit for monorepo)
- webpack (rejected: overkill for library packages, very slow)
- tsc (rejected: TypeScript source not allowed per constitution)
- Keep per-package configs (rejected: violates unification goal, increases CI/CD complexity)

**Implementation Path**:
1. Create `/esbuild.config.mjs` for monorepo root
2. Configure to output all packages to dist/ directories with ESM + CommonJS
3. Remove per-package build scripts/configs
4. Test `pnpm run build` builds all 17 packages in <30s

---

## Area 3: Linting & Formatting Unification

**Decision**: Single Ruff configuration (400+ rules) at monorepo root; zero suppression comments allowed

**Rationale**:
- Ruff is industry-standard JavaScript/TypeScript linter
- Constitution mandates 400+ rules enforcement, 0 suppression comments
- Single config ensures consistent quality across all 17 packages
- Enables unified CI/CD: `pnpm run lint` lints everything

**Current State**:
- Inconsistent linting: some packages have ESLint, others have Prettier, some unlinked
- No unified linting strategy across monorepo
- Different rule sets per package (some strict, some lenient)

**Alternatives Considered**:
- ESLint (rejected: more complex, less performant than Ruff)
- Prettier (rejected: is formatter, not linter; Ruff includes formatting)
- Keep per-package configs (rejected: prevents unified quality gates, violates constitution)

**Implementation Path**:
1. Create `/ruffrc.toml` or `pyproject.toml` with 400+ rules (no suppression)
2. Run linting across all packages: `pnpm run lint`
3. Fix all violations (0 suppressions allowed)
4. Add pre-commit hook to enforce

---

## Area 4: Test Runner Unification

**Decision**: Vitest for all tests with unified config; ≥80% coverage per package requirement

**Rationale**:
- Vitest is modern, fast test runner designed for Vite/ESM
- Single config ensures consistent test execution across monorepo
- Coverage reporting aggregates across all 17 packages
- Chicago School TDD required by constitution

**Current State**:
- Mixed test runners: Vitest in some packages, Jest in others, custom test scripts
- Coverage requirements not uniform (some 80%+, some no coverage targets)
- Test file patterns vary (.test.js, .spec.js, .test.mjs in different locations)

**Alternatives Considered**:
- Jest (rejected: slower than Vitest, designed for CommonJS, harder ESM support)
- Mocha (rejected: requires chai/sinon, more manual setup, slower)
- Custom test runners (rejected: high maintenance, violates unification goal)

**Implementation Path**:
1. Create `/vitest.config.mjs` with coverage reporting, ≥80% threshold
2. Standardize test file pattern: `*.test.mjs` co-located with source
3. Run tests: `pnpm test` runs all packages with coverage
4. Ensure all 17 packages achieve ≥80% coverage

---

## Area 5: Dependency Version Alignment

**Decision**: Single version per shared dependency; audit for unused deps; detect circular dependencies

**Rationale**:
- Version conflicts cause runtime inconsistencies and lock file bloat
- pnpm deterministic lock file requires version alignment
- Circular dependency detection prevents integration issues
- Audit removes unused dependencies (reduces node_modules size)

**Current State**:
- Shared dependencies (oxigraph, zod, n3, etc.) declared in multiple versions
- Some packages declare unused dependencies (bloated node_modules)
- No circular dependency detection in current setup

**Alternatives Considered**:
- Allow per-package versions (rejected: breaks pnpm determinism, causes runtime conflicts)
- No circular dependency checking (rejected: causes hidden integration bugs)
- Manual audits only (rejected: error-prone, doesn't scale)

**Implementation Path**:
1. Audit all package.json files for top 30 shared dependencies
2. Find conflicts: dependencies with multiple versions
3. Resolve to highest compatible version (verified by tests)
4. Add dependency auditing tool (e.g., npm audit, depcheck)
5. Add circular dependency detection (e.g., madge)
6. Update pnpm-lock.yaml for clean install

---

## Area 6: Package Export Patterns

**Decision**: Named exports only from `index.mjs`; generate TypeScript definitions (.d.ts) for all public packages

**Rationale**:
- Named exports are more explicit, prevent accidental internal API usage
- TypeScript definitions enable IDE support even for JavaScript consumers
- Single entry point eliminates ambiguity
- Tree-shaking improves with named exports

**Current State**:
- Mixed export styles: some packages use named exports, some use default exports
- Some packages re-export transitive dependencies (tight coupling)
- No TypeScript definitions generated
- No enforced "public API" boundary

**Alternatives Considered**:
- Allow default exports (rejected: less explicit, harder to tree-shake)
- Keep mixed exports (rejected: increases confusion for consumers)
- Manual TypeScript definitions (rejected: maintenance burden)

**Implementation Path**:
1. Audit current exports from each package's index.mjs
2. Convert to named exports only (no default)
3. Document public API per package
4. Generate .d.ts files using tsc or esbuild plugin
5. Validate types are correct (0 type errors in definitions)

---

## Area 7: Package Metadata Standardization

**Decision**: Every package must have: description, LICENSE, README, keywords, correct repository field

**Rationale**:
- Metadata improves npm discoverability (keywords)
- README and LICENSE build professional expectations
- Repository field points consumers to source
- Consistent metadata enables automated tooling

**Current State**:
- Inconsistent package.json metadata: missing descriptions, keywords, wrong repository paths
- Many packages lack README or LICENSE
- Publishing metadata incomplete for public packages

**Alternatives Considered**:
- Minimal metadata (rejected: reduces professional appearance, hurts discoverability)
- Manual curation per package (rejected: error-prone, doesn't scale)

**Implementation Path**:
1. Audit each package for: description, license, readme, keywords, repository
2. Create template metadata for missing fields
3. Standardize repository field to monorepo path
4. Verify all public packages have complete metadata
5. Add validation to CI/CD

---

## Area 8: Circular Dependency Detection

**Decision**: Implement automated detection; fail build if any found; prevent new circular deps

**Rationale**:
- Circular dependencies cause integration failures and unpredictable behavior
- Automated detection catches issues early before deployment
- Prevention stops regressions

**Current State**:
- No circular dependency detection in current setup
- Risk of hidden circular deps as packages evolve

**Alternatives Considered**:
- Manual code review (rejected: doesn't scale, error-prone)
- No detection (rejected: risks runtime integration failures)

**Implementation Path**:
1. Add madge or similar circular dependency detection tool
2. Run in CI/CD: `pnpm run check-deps`
3. Fail build if any circular dependencies detected
4. Document allowed exceptions (if any)

---

## Technical Decisions Summary

| Area | Decision | Effort | Risk | Alternatives |
|------|----------|--------|------|--------------|
| Structure | `src/` + `index.mjs` entry | Medium | Low | Keep existing (rejected) |
| Build | Single esbuild root config | Medium | Low | Per-package configs (rejected) |
| Linting | Unified Ruff 400+ rules | Low | Low | Per-package ESLint (rejected) |
| Testing | Vitest ≥80% coverage | Medium | Low | Per-package Jest (rejected) |
| Dependencies | Single version + audit | Low | Low | Version conflicts (rejected) |
| Exports | Named only + .d.ts | Medium | Low | Default exports (rejected) |
| Metadata | Standardized fields | Low | Low | Minimal metadata (rejected) |
| Circular Deps | Automated detection | Low | Low | Manual review (rejected) |

---

## Assumptions Validated

✅ All 17 packages are production-grade (no deprecated packages to remove)
✅ pnpm workspaces is the monorepo tool (confirmed in pnpm-workspace.yaml)
✅ Node.js 18+ is the target runtime
✅ MJS + JSDoc is the source format per constitution
✅ Public APIs can be identified and documented
✅ No breaking changes to external package APIs planned

---

## Risks & Mitigations

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| Migration breaks external consumers | Low | High | Keep public APIs unchanged; version as patch/minor |
| Build time increases | Low | Medium | Profile esbuild; optimize config if needed |
| Test suite takes too long | Low | Medium | Parallelize with Vitest; optimize test timeout |
| Unused dep audit misses real deps | Low | Low | Manual review of audit results before removal |
| Circular deps detected after unification | Medium | Medium | Refactor identified packages to eliminate cycles |

---

## Next Steps

Phase 1 Design will proceed with:
1. Create data-model.md documenting package entities and relationships
2. Create contracts/ with OpenAPI schemas for build/lint/test contracts
3. Generate quickstart.md with unification workflow
4. Update agent context with new technologies/patterns
