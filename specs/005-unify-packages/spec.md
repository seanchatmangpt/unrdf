# Feature Specification: Unify Packages

**Feature Branch**: `005-unify-packages`
**Created**: 2025-12-20
**Status**: Draft
**Input**: User description: "unify the packages"

## User Scenarios & Testing *(mandatory)*

<!--
  IMPORTANT: User stories should be PRIORITIZED as user journeys ordered by importance.
  Each user story/journey must be INDEPENDENTLY TESTABLE - meaning if you implement just ONE of them,
  you should still have a viable MVP (Minimum Viable Product) that delivers value.
  
  Assign priorities (P1, P2, P3, etc.) to each story, where P1 is the most critical.
  Think of each story as a standalone slice of functionality that can be:
  - Developed independently
  - Tested independently
  - Deployed independently
  - Demonstrated to users independently
-->

### User Story 1 - Standardize Package Structure (Priority: P1)

**Scenario**: A maintainer audits the UNRDF monorepo and discovers inconsistent package layouts—some have `src/`, others have `lib/`, build configs vary widely. This creates friction during development, makes onboarding difficult, and increases the risk of bugs when patterns aren't consistently followed.

**Why this priority**: Standardized structure is the foundation for all other improvements. Without it, no other unification efforts can succeed. This is blocking for developer productivity and code quality.

**Independent Test**: Run a structure audit tool against all 17 packages. Tool MUST verify every package has:
- `src/` directory at package root
- `index.mjs` as entry point
- JSDoc type annotations (100% coverage)
- `*.test.mjs` files co-located with source
- Consistent module export patterns
- No broken imports or missing dependencies

Can be verified immediately without other changes.

**Acceptance Scenarios**:

1. **Given** a freshly cloned UNRDF monorepo, **When** a developer navigates to any package, **Then** the structure follows the standard pattern (src → index.mjs → test files)
2. **Given** a package with legacy structure (e.g., `lib/`, multiple entry points), **When** the migration runs, **Then** all code is moved to `src/`, exports are consolidated to `index.mjs`, and tests still pass
3. **Given** a developer reading the monorepo guide, **When** they open any package, **Then** the actual structure matches the documented pattern exactly

---

### User Story 2 - Unify Build and Tooling Configuration (Priority: P1)

**Scenario**: Different packages use different build tools, lint configurations, and test runners. Package A uses a custom esbuild config, Package B uses rollup, Package C doesn't build at all. Linting rules vary wildly (some have prettier, some don't; some use ESLint, others use Ruff). Test runners are inconsistent (Vitest, Jest, custom scripts). This means:
- Developers must context-switch between tools when moving between packages
- Build failures are hard to diagnose across packages
- Quality gates are unevenly enforced
- CI/CD is complex with many conditional steps

**Why this priority**: Consistent tooling reduces cognitive load, enables shared CI/CD pipelines, and ensures quality is uniformly enforced across all 17 packages.

**Independent Test**:
1. Run `pnpm run build` at monorepo root → all 17 packages build successfully
2. Run `pnpm run lint` → all packages linted with same Ruff config, 0 violations
3. Run `pnpm test` → all packages tested with Vitest, results aggregated
4. Verify no package-specific build scripts or workarounds needed

Can be validated in isolation from other stories.

**Acceptance Scenarios**:

1. **Given** a monorepo with mixed build tools, **When** `pnpm run build` is executed, **Then** all packages build successfully using the same build process (no special cases)
2. **Given** a new package created from template, **When** developer runs standard commands, **Then** they work identically to all other packages (no learning curve)
3. **Given** a CI pipeline, **When** it runs, **Then** it uses single unified config for all packages (no conditional logic per package)
4. **Given** a developer working on Package A then Package B, **When** they switch packages, **Then** the dev workflow is identical (same linting, testing, building)

---

### User Story 3 - Consolidate Dependency Management (Priority: P1)

**Scenario**: Today, packages declare overlapping but slightly different versions of shared dependencies:
- `@unrdf/core` requires `oxigraph@1.2.0`, `@unrdf/streaming` requires `oxigraph@1.2.1`
- Some packages use `zod@3.20.0`, others use `3.21.0`
- CLI packages include unused dependencies not declared

This causes:
- Version conflicts resolved unpredictably by pnpm
- Potential runtime inconsistencies
- Difficulty identifying unused dependencies
- Bloated `node_modules`

**Why this priority**: Unified dependencies reduce version conflicts, ensure consistent behavior across packages, and reduce bundle size.

**Independent Test**:
1. Audit monorepo dependency tree → all shared deps have single version
2. Run dependency checker → no unused dependencies declared
3. Install from scratch on clean machine → identical `pnpm-lock.yaml`, same `node_modules` size
4. Run full test suite → all tests pass with unified versions

**Acceptance Scenarios**:

1. **Given** packages with conflicting dependency versions, **When** unification runs, **Then** all packages resolve to single version (highest compatible)
2. **Given** a package with unused dependencies, **When** audit runs, **Then** unused deps are identified and can be removed
3. **Given** `pnpm-lock.yaml`, **When** fresh install occurs, **Then** lock file is identical across machines (deterministic)

---

### User Story 4 - Align Package Exports and Public APIs (Priority: P2)

**Scenario**: Consumers of UNRDF struggle because:
- Some packages have named exports, others use default exports
- Some re-export transitive dependencies (coupling consumers to internal choices)
- TypeScript definitions are inconsistent or missing
- No clear "public API" boundary—internal utilities are sometimes exported publicly

**Why this priority**: Clear, consistent APIs make it easier for external developers to use UNRDF correctly. This affects user experience but comes after structural unification.

**Independent Test**:
1. Document public API for each package (what's intentionally exported)
2. Generate TypeScript definition files
3. Test that consumers can't accidentally import internal modules
4. Verify all exports follow consistent pattern (named exports from `index.mjs`)

**Acceptance Scenarios**:

1. **Given** a package with mixed export styles, **When** API is aligned, **Then** all public APIs use named exports only
2. **Given** external consumers importing from package, **When** they use TypeScript, **Then** types are available and accurate

---

### User Story 5 - Establish Consistent Package Metadata (Priority: P2)

**Scenario**: Package metadata varies widely:
- Some have comprehensive `package.json` descriptions, others are minimal
- LICENSE files are missing from some packages
- README documentation ranges from detailed to nonexistent
- Repository fields point to inconsistent locations
- Publishing metadata (keywords, categories) is incomplete

**Why this priority**: Consistent metadata improves discoverability, helps users understand purpose of each package, and sets professional expectations.

**Independent Test**:
1. Verify every package has: description, LICENSE, README, consistent keywords
2. Ensure repository fields and homepage URLs are correct
3. Check that public packages have proper publication metadata

**Acceptance Scenarios**:

1. **Given** any package in the monorepo, **When** user opens its package.json, **Then** description, license, and keywords are present and consistent
2. **Given** a package directory, **When** user looks for README, **Then** it exists and documents purpose, usage, and public API

---

### Edge Cases

- **Circular dependencies**: What happens if packages have mutual dependencies after unification? (Must detect and fail fast with clear error)
- **Legacy imports**: How to handle code importing from old paths during migration? (Gradual migration path or breaking change?)
- **Binary/native modules**: Some packages might include native addons—must preserve special build logic
- **Private vs public packages**: Some packages are internal only—how to mark as private?

## Requirements *(mandatory)*

### Functional Requirements

**Structure Unification**:
- **FR-001**: Every package MUST have a `src/` directory containing all source code
- **FR-002**: Every package MUST export its public API through `src/index.mjs` only
- **FR-003**: Every package MUST use MJS files for all source code (no TypeScript source)
- **FR-004**: Every package MUST have JSDoc type annotations on all public exports (100% coverage)
- **FR-005**: Test files MUST be co-located with source code using `.test.mjs` suffix

**Build and Tooling**:
- **FR-006**: All packages MUST build using the same build system (no per-package custom scripts)
- **FR-007**: All packages MUST use Ruff for linting with consistent configuration (0 suppressions without justification)
- **FR-008**: All packages MUST use Vitest as test runner with consistent configuration
- **FR-009**: All packages MUST generate code coverage reports in consistent format (≥80% line coverage)
- **FR-010**: All packages MUST use `pnpm` as package manager exclusively

**Dependency Management**:
- **FR-011**: All shared dependencies across packages MUST use identical versions (single version per dependency)
- **FR-012**: Each package MUST declare only dependencies it directly uses (no transitive re-exports)
- **FR-013**: System MUST prevent circular dependencies (detect and fail during build)
- **FR-014**: Each package MUST have no unused declared dependencies (automated audit)

**Package Exports**:
- **FR-015**: All public APIs MUST use named exports (no default exports for public APIs)
- **FR-016**: Internal modules MUST NOT be exported from package root (enforce through index.mjs)
- **FR-017**: TypeScript definitions MUST be generated for all public packages
- **FR-018**: Package exports MUST be documented in README

**Package Metadata**:
- **FR-019**: Every package MUST have a descriptive `description` field in package.json
- **FR-020**: Every package MUST include LICENSE file (matching monorepo license)
- **FR-021**: Every package MUST include README.md documenting purpose, usage, and public API
- **FR-022**: Every public package MUST have keywords for discoverability
- **FR-023**: Package repository field MUST point to correct location in monorepo

### Key Entities

**Package**: A self-contained, versioned unit of functionality
- **Attributes**: name, version, description, license, entry point, dependencies
- **Relationships**: depends on (other packages), exports (public API), contains (source files + tests)

**Package Manifest** (package.json): Configuration file defining package metadata, dependencies, scripts
- **Attributes**: name, version, description, main, exports, dependencies, devDependencies, keywords, license
- **Consistency requirements**: All fields present and follow same pattern

**Build Configuration**: Unified tooling configuration (esbuild, linting, testing)
- **Single source**: Monorepo root config inherits down
- **No package-specific overrides** except documented exceptions (binary modules, etc.)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 100% of packages conform to standard structure (17/17 packages have src/, index.mjs, consistent test layout)
- **SC-002**: All 17 packages build successfully with single `pnpm run build` command with 0 special-case handling
- **SC-003**: Monorepo linting produces 0 violations across all packages using unified Ruff config (0 suppression comments without justification)
- **SC-004**: Test coverage across all packages is ≥80% line coverage (verified by coverage report)
- **SC-005**: All shared dependencies resolve to single version across monorepo (verified by `pnpm ls` audit)
- **SC-006**: No circular dependencies detected (verified by automated dependency checker)
- **SC-007**: Installation from scratch produces consistent `pnpm-lock.yaml` and identical node_modules footprint
- **SC-008**: Developer onboarding time reduced by 50% (less context-switching between tooling)
- **SC-009**: All CI/CD pipelines execute identical steps for all packages (no per-package conditional logic)
- **SC-010**: 100% of public packages have complete metadata (description, license, README, keywords present)
- **SC-011**: TypeScript definitions generated and validated for all public packages (0 type errors in generated .d.ts files)

## Assumptions

1. **No breaking changes to public APIs during unification**: Existing users should not be affected by internal reorganization. If API changes needed, they're captured separately.

2. **All 17 packages are intended to be first-class packages**: None are temporary or deprecated. (If some are, this feature would first mark those for removal.)

3. **pnpm workspaces is the monorepo tool**: Configuration assumes workspace-based dependency resolution.

4. **Node.js 18+ is the target runtime**: Build tooling and syntax choices assume Node 18+.

5. **MJS + JSDoc is the source format**: All packages use MJS files with JSDoc annotations (not TypeScript).

6. **Ruff is the linter of choice**: Configuration assumes Ruff 400+ rules enforced, no ESLint or other linters mixed in.

7. **Vitest is the test runner**: All test configurations use Vitest for consistency.

8. **Public API boundary is clearly defined**: Some packages have public APIs (published to npm), others are internal. This feature assumes the boundary is known.

## Out of Scope

- **API design changes**: Refactoring the public interfaces of packages (separate feature)
- **Major version bumps**: This is a structural unification, not a semver MAJOR change
- **Documentation rewrite**: Improving documentation content is separate from unifying structure
- **Performance optimization**: Performance work is separate from standardization
- **Feature additions**: New capabilities in packages are separate from unification

## Related Features

- Package publishing and npm registry metadata
- Public API documentation and generation
- Monorepo CI/CD pipeline design
- Release and versioning strategy for 17 packages
