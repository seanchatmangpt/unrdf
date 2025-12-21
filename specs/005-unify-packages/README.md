# Feature 005: Unify Packages

**Status**: ✅ Planning Complete | Ready for Phase 3 Implementation
**Branch**: `005-unify-packages`
**Created**: 2025-12-20
**Last Updated**: 2025-12-20

---

## Quick Navigation

| Document | Purpose | Audience |
|----------|---------|----------|
| **[spec.md](spec.md)** | Feature specification with user stories | Product managers, stakeholders |
| **[plan.md](plan.md)** | Technical implementation plan | Technical leads, architects |
| **[tasks.md](tasks.md)** | 67 actionable tasks with effort estimates | Development team |
| **[research.md](research.md)** | Phase 0 research findings | Architects, decision makers |
| **[data-model.md](data-model.md)** | Entity definitions and validation rules | Developers implementing Phase 3 |
| **[quickstart.md](quickstart.md)** | Step-by-step implementation guide | Individual developers |
| **[contracts/build-contract.md](contracts/build-contract.md)** | Build system interface | DevOps, CI/CD engineers |
| **[COMPLETION-STATUS.md](COMPLETION-STATUS.md)** | Planning phase completion verification | Project leads |

---

## Feature Overview

Establish consistent structure, tooling, and configuration across **all 17 packages** in the UNRDF monorepo.

### What Gets Unified

| Area | Before | After | Benefit |
|------|--------|-------|---------|
| **Structure** | Inconsistent (lib/, src/, root mix) | Unified `src/` + `index.mjs` | Single dev model |
| **Build Tool** | Mixed (esbuild, rollup, custom) | Single esbuild config | <30s builds |
| **Linting** | Inconsistent rules (ESLint, Ruff, none) | Unified Ruff 400+ rules | 0 violations |
| **Testing** | Fragmented (Vitest, Jest, custom) | Unified Vitest | ≥80% coverage everywhere |
| **Dependencies** | Version conflicts (1.2.0 vs 1.2.1) | Single version per dep | Deterministic installs |
| **Exports** | Mixed (default + named) | Named only + .d.ts | IDE support, tree-shaking |
| **Metadata** | Missing/incomplete | Standardized fields | npm discoverability |

---

## Execution Summary

### Timeline & Effort

- **Total Tasks**: 67 actionable tasks
- **Total Effort**: ~128 developer-hours
- **Serial Duration**: 4-6 weeks
- **Parallelized Duration**: 1-2 weeks (recommended)

### By Phase

```
Phase 1 (Setup)              → 20 tasks,  8h  (1-2 days)      [Sequential]
Phase 2 (Structure)          → 12 tasks, 42h  (2-3 weeks)    [Parallelizable by package]
Phase 3 (Build/Lint/Test)    → 20 tasks, 24h  (1-2 weeks)    [Sequential]
Phase 4 (Dependencies)       → 11 tasks, 12h  (1-2 days)      [Sequential]
Phase 5 (Exports)            → 11 tasks,  8h  (1-2 days)      [Sequential]
Phase 6 (Metadata)           → 12 tasks,  8h  (1-2 days)      [Sequential]
Phase 7 (Validation)         → 13 tasks, 12h  (1 day)         [Blocking Quality Gate]
Phase 8 (Polish - Optional)  →  5 tasks,  8h  (1-2 days)     [Optional]
```

### Success Criteria (All Must Pass)

✅ **Structure**: 17/17 packages have `src/index.mjs`
✅ **Build**: Completes in <30s, all `dist/` created
✅ **Lint**: 0 violations, 0 suppression comments
✅ **Tests**: 100% pass rate, ≥80% coverage everywhere
✅ **Dependencies**: Single version, 0 unused, 0 circular
✅ **Exports**: Named only, .d.ts generated, 0 type errors
✅ **Metadata**: All package.json/README/LICENSE complete

---

## User Stories (Prioritized)

### User Story 1 (P1): Standardize Package Structure
All 17 packages follow: `src/` → `index.mjs` → `dist/`
- Acceptance: Audit tool verifies all 17 packages have correct structure
- Blocking for: All other stories depend on this

### User Story 2 (P1): Unify Build and Tooling
Single esbuild config, unified Ruff linting, unified Vitest testing
- Acceptance: `pnpm run build` / `pnpm run lint` / `pnpm test` work uniformly
- Enables: Quality gates, CI/CD simplification

### User Story 3 (P1): Consolidate Dependencies
All shared deps at single version, no unused deps, no circular deps
- Acceptance: `pnpm list` shows single version per shared dep, 0 unused
- Enables: Deterministic installs, reduced node_modules

### User Story 4 (P2): Align Exports & Generate Definitions
Named exports only + TypeScript definitions for all packages
- Acceptance: All .d.ts generated, 0 type errors
- Enables: IDE support, tree-shaking

### User Story 5 (P2): Standardize Metadata
Complete package.json metadata, README, LICENSE for all packages
- Acceptance: npm packages discoverable and professional
- Enables: External publication

---

## Key Decisions & Rationale

### Monorepo Tool: pnpm Workspaces
✅ Already in use, proven, deterministic lock file
❌ Rejected: npm/yarn (lack determinism or full workspace support)

### Build System: Single esbuild Root Config
✅ Fastest bundler, single build process for all 17 packages
✅ Enables performance targets (<30s)
❌ Rejected: Per-package configs (defeats unification goal)

### Linting: Ruff 400+ Rules (No Suppressions)
✅ Industry standard, 400+ rules per constitution
✅ Single config reduces complexity
❌ Rejected: Mixed ESLint/Prettier (inconsistent rules)

### Testing: Vitest ≥80% Coverage
✅ Modern, fast, designed for ESM
✅ Unified coverage reporting
❌ Rejected: Jest (slower, CommonJS-focused)

### Structure: src/ → index.mjs → dist/
✅ JavaScript ecosystem standard (~90% of npm packages)
✅ Simplifies TypeScript consumer support
❌ Rejected: lib/ (less community standard)

---

## How to Use These Documents

### For Stakeholders (1-5 min read)
1. Read feature overview above
2. Skim **[spec.md](spec.md)** (user stories, requirements)
3. Review success criteria section above

### For Technical Leads (15 min read)
1. Read **[plan.md](plan.md)** (technical context, architecture decisions)
2. Review **[research.md](research.md)** (why each decision was made)
3. Check COMPLETION-STATUS.md for Constitution compliance

### For Developers Starting Implementation (30 min)
1. Read **[quickstart.md](quickstart.md)** (step-by-step 5-step guide)
2. Reference **[data-model.md](data-model.md)** while coding
3. Use **[tasks.md](tasks.md)** to track progress

### For Build/DevOps (15 min)
1. Read **[contracts/build-contract.md](contracts/build-contract.md)**
2. Review esbuild/Ruff/Vitest configuration examples in **[quickstart.md](quickstart.md)**

---

## Phase 0 & 1 Completion Summary

### Phase 0: Research (✅ Complete)
- **Audit Scope**: 8 key areas (structure, build tools, linting, testing, dependencies, circular deps, metadata, exports)
- **Agents**: 10 concurrent audit agents
- **Output**: [research.md](research.md) with decisions, rationale, alternatives, technical summary

### Phase 1: Design (✅ Complete)
- **Deliverables**: 8 documents, 2,397 lines of documentation
- **Data Model**: 8 core entities defined with validation rules
- **Task Breakdown**: 67 actionable tasks across 8 phases
- **Constitution Compliance**: All 8 mandatory checks pass

### Status: Ready for Phase 3 (Refinement & Implementation)

---

## Risks & Mitigations

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| Migration breaks external consumers | Low | High | Keep public APIs unchanged, version as patch/minor |
| Build time regression | Low | Medium | Profile each phase, optimize if needed |
| Circular dependencies found | Medium | Medium | Plan refactoring, document in research.md |
| Coverage <80% after migration | Low | High | Target 85-90%, add missing tests |
| Dependency audit false positives | Low | Low | Manual verification before removal |

---

## Next Steps: Transition to Phase 3

### Option 1: Automated Refinement Phase
```bash
# Run refinement phase (generates pseudocode, tests, implementation stubs)
/speckit.implement unify-packages
```

### Option 2: Manual Implementation Assignment
1. Assign Phase 1 (Setup) to 1 developer: 1-2 days
2. Assign Phase 2 (Structure) to team: parallelizable by package group
3. Assign Phases 3-7 sequentially as each phase completes

### What Happens Next
- Phase 3 (Refinement): Generate pseudocode + TDD test anchors
- Phase 4 (Completion): Implement using test-driven development
- Phase 5 (Integration): Validate across all 17 packages
- Production deployment when all success criteria pass

---

## Reference Materials

**Internal**:
- `.specify/memory/constitution.md` - UNRDF Constitution v1.0.0 (ratified)
- `/docs/MONOREPO-DEVELOPMENT.md` - Development guide (will be updated)

**External**:
- esbuild docs: https://esbuild.github.io/
- Ruff docs: https://docs.astral.sh/ruff/
- Vitest docs: https://vitest.dev/

---

## Questions?

Refer to the appropriate document:
- **"What does this feature do?"** → Read feature overview above, then [spec.md](spec.md)
- **"How do we build this?"** → Read [plan.md](plan.md) and [quickstart.md](quickstart.md)
- **"What are all the tasks?"** → Read [tasks.md](tasks.md)
- **"Why this decision?"** → Read [research.md](research.md)
- **"How do I code this?"** → Read [quickstart.md](quickstart.md) and [data-model.md](data-model.md)
- **"Is this production-ready?"** → Read [COMPLETION-STATUS.md](COMPLETION-STATUS.md)

---

**Feature Status**: ✅ Planning Phase Complete
**Approval Status**: ✅ Constitution Compliance Verified (8/8 checks)
**Next Phase**: Ready for Implementation (Phase 3)

Generated: 2025-12-20
