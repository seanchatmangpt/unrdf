# Implementation Plan: Unify Packages

**Branch**: `005-unify-packages` | **Date**: 2025-12-20 | **Spec**: [specs/005-unify-packages/spec.md](spec.md)
**Input**: Feature specification from `specs/005-unify-packages/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Establish consistent structure, tooling, and configuration across all 17 packages in the UNRDF monorepo. Unification covers: package layouts (`src/` directories, `index.mjs` exports), build tooling (esbuild, Ruff, Vitest), dependency management (version alignment, circular dependency detection), public APIs (named exports, TypeScript definitions), and package metadata (descriptions, READMEs, keywords). Result: single dev workflow, consistent CI/CD, reduced onboarding friction, improved code quality enforcement.

## Technical Context

**Language/Version**: Node.js 18+ with ES modules (MJS + JSDoc type annotations)
**Primary Dependencies**:
- Build: esbuild (unified build tool)
- Linting: Ruff (400+ rules, no TypeScript linting)
- Testing: Vitest (Chicago School TDD)
- Monorepo: pnpm workspaces
- Type Safety: JSDoc annotations + Zod validation

**Storage**: File system (package.json, pnpm-lock.yaml, monorepo structure) - N/A for external storage
**Testing**: Vitest test runner with ≥80% coverage per package
**Target Platform**: Node.js 18+ (CommonJS and ESM compatible)
**Project Type**: Monorepo with 17 npm packages organized by capability
**Performance Goals**:
- Build time: <30s for full monorepo (single machine)
- Linting: <5s across all packages
- Test suite: <2min for full coverage

**Constraints**:
- No breaking changes to public package APIs during unification
- Must preserve backward compatibility for external consumers
- All 17 packages must be functional after migration

**Scale/Scope**:
- 17 packages affecting ~200K lines of code
- ~150 developers potential users (internal UNRDF team)
- ~1K external npm consumers (estimated)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Verification**:
- [x] Feature fits within SPARC 5-phase methodology (no shortcuts) - ✓ Full 5-phase execution required
- [x] RDF/SPARQL/SHACL usage confirmed (if data-driven feature) - ✓ NOT applicable (infrastructure/tooling feature)
- [x] Test scope ≥80% coverage confirmed for all deliverables - ✓ All 17 packages must reach ≥80% coverage
- [x] No "lean" version planned - delivering production-ready first - ✓ Full unification required; no staged rollout
- [x] Evidence-based: Planning includes measurement approach (OTEL/metrics) - ✓ Metrics: build time, lint violations, coverage, test pass rate
- [x] All related operations will run concurrent (batched messages) - ✓ Max 10 agents concurrent per user request
- [x] Pattern reuse identified and documented (no novel approaches without review) - ✓ Reuse monorepo patterns from existing working configs
- [x] No suppression comments or workarounds in scope - ✓ Linting violations must be 0; no suppression comments allowed

**Scope Justification**:

| Aspect | Status | Rationale |
|--------|--------|-----------|
| API Breaking Changes | Acceptable | Package structure changes are internal; public APIs unchanged |
| Full 17-Package Coverage | Required | No partial unification; foundation for future work |
| CI/CD Pipeline Alignment | Required | Single unified pipeline enables future automation |
| No Feature Additions | Compliant | This is pure standardization, zero new functionality |

## Project Structure

### Documentation (this feature)

```text
specs/[###-feature]/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (Monorepo - 17 Packages)

**Unified Package Structure** (applied to all 17 packages):

```text
packages/[package-name]/
├── src/                              # All source code in src/
│   ├── index.mjs                     # Single entry point (named exports only)
│   ├── [module1].mjs
│   ├── [module2].mjs
│   └── index.test.mjs                # Co-located test files
│
├── package.json                      # Unified format & metadata
├── README.md                         # Purpose, usage, public API
├── LICENSE                           # Copy of monorepo license
├── tsconfig.json                     # (generated) For TypeScript consumer support
└── .d.ts files                       # (generated) TypeScript definitions

packages/
├── atomvm/
├── composables/
├── core/
├── domain/
├── engine-gateway/
├── federation/
├── hooks/
├── kgc-4d/
├── kgn/
├── knowledge-engine/
├── nextra/
├── oxigraph/
├── project-engine/
├── streaming/
├── test-utils/
├── validation/
└── cli/
```

**Monorepo Root Configuration**:

```text
/
├── pnpm-workspace.yaml               # Unified workspace definition
├── package.json                      # Root scripts: build, lint, test, coverage
├── .ruffrc.toml                      # Single Ruff config for all packages
├── vitest.config.mjs                 # Single Vitest config for all packages
├── esbuild.config.mjs                # Single build config for all packages
├── .github/
│   └── workflows/
│       └── ci.yml                    # Unified CI/CD (no per-package scripts)
└── docs/
    └── MONOREPO-DEVELOPMENT.md       # Single dev guide
```
└── [platform-specific structure: feature modules, UI flows, platform tests]
```

**Structure Decision**: Monorepo with unified structure across all 17 packages. Each package follows identical layout (src/ → index.mjs → dist/). Single build, lint, test configurations at monorepo root serve all packages. No per-package overrides or exceptions (except documented binary module cases if applicable).

## Phase 0: Research (COMPLETE)

**Deliverable**: `research.md`

Concurrent audit agents investigated:
1. Package structure consistency across 17 packages
2. Build tooling variation (esbuild vs rollup vs custom)
3. Linting configuration differences (ESLint vs Ruff vs none)
4. Test runner diversity (Vitest vs Jest vs custom)
5. Dependency version conflicts across packages
6. Circular dependency detection needs
7. Package.json metadata completeness
8. Export patterns and TypeScript definition gaps

**Key Findings**:
- Significant structural inconsistency across 17 packages
- Mixed build tools requiring manual context-switching
- Inconsistent linting enforcement
- No circular dependency detection
- Missing metadata and TypeScript definitions

**Decisions Made**:
- ✅ Unify on `src/` + `index.mjs` structure
- ✅ Single esbuild configuration
- ✅ Unified Ruff linting (400+ rules, 0 suppressions)
- ✅ Vitest for all tests (≥80% coverage)
- ✅ Version alignment for shared dependencies
- ✅ Named exports + TypeScript definitions for all
- ✅ Standardized package.json metadata
- ✅ Circular dependency detection tool integration

## Phase 1: Design (COMPLETE)

**Deliverables**:
- ✅ `plan.md` (this file) - Technical context, Constitution check, project structure
- ✅ `data-model.md` - Package, SourceModule, PackageManifest, BuildConfig entities
- ✅ `contracts/build-contract.md` - Build system interface specification
- ✅ `quickstart.md` - Step-by-step implementation guide for developers

**Architecture Decisions**:
1. **Monorepo Tool**: pnpm workspaces (existing, proven, deterministic)
2. **Build System**: esbuild (root config, 17 entry points, dual ESM/CJS output)
3. **Linting**: Ruff with 400+ rules (per constitution), zero suppressions
4. **Testing**: Vitest with v8 coverage, ≥80% threshold per package
5. **Structure**: src/ → index.mjs → dist/ (JavaScript ecosystem standard)
6. **Exports**: Named only, no transitive re-exports, TypeScript definitions required
7. **Dependencies**: Single version per shared dep, no unused deps, circular detection

## Phase 2: Task Breakdown (COMPLETE)

**Deliverable**: `tasks.md` - 67 actionable tasks organized by phase, priority, and dependencies

**Task Organization**:
- **Phase 1**: Setup & Infrastructure (20 tasks, 8h) - Create monorepo config, audit dependencies, install tooling
- **Phase 2**: User Story 1 - Standardize Package Structure (12 tasks, 42h) - src/ + index.mjs migration for 17 packages
- **Phase 3**: User Story 2 - Unify Build and Tooling (20 tasks, 24h) - esbuild, Ruff, Vitest unification
- **Phase 4**: User Story 3 - Consolidate Dependencies (11 tasks, 12h) - Dependency audit and version alignment
- **Phase 5**: User Story 4 - Align Exports & Generate Definitions (11 tasks, 8h) - Named exports, TypeScript definitions
- **Phase 6**: User Story 5 - Standardize Metadata (12 tasks, 8h) - package.json, README, LICENSE
- **Phase 7**: Validation & Testing (13 tasks, 12h) - Quality gates, coverage, circular deps
- **Phase 8**: Polish & Cross-Cutting Concerns (5 tasks, 8h) - Performance, automation, observability (optional)

**Total**: 67 tasks, ~128 developer-hours, 4-6 weeks serial / 1-2 weeks parallelized

**Success Metrics** (all must pass):
- Structure: All 17 packages have src/index.mjs
- Build: <30s, all dist/ created
- Lint: 0 violations
- Tests: 100% pass rate, ≥80% coverage
- Dependencies: Single version, 0 unused, 0 circular
- Exports: Named only, .d.ts generated, 0 type errors
- Metadata: All fields complete and valid

## Post-Phase 1 Constitution Re-check

**Mandatory Compliance Verification** (re-evaluated):
- [x] Feature fits within SPARC 5-phase methodology - ✓ Full 5 phases
- [x] Test scope ≥80% coverage confirmed - ✓ Vitest config with 80% threshold
- [x] No "lean" version planned - ✓ Full monorepo unification (all 17 packages)
- [x] Evidence-based measurement approach - ✓ Build time, lint violations, coverage metrics
- [x] All operations concurrent in batched messages - ✓ 10 agents parallel, consolidated Phase 1
- [x] Pattern reuse documented - ✓ esbuild/Ruff/Vitest reuse from existing proven configs
- [x] No suppression comments in scope - ✓ Linting violations must be 0; no workarounds

**Status**: ✅ **ALL GATES PASS** - Ready for Phase 2 task breakdown
