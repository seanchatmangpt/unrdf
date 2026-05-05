# ADR-001: File Splitting Strategy for Modular Architecture

**Status**: Accepted
**Date**: 2025-12-25
**Decision Makers**: System Architecture Team
**Context**: UNRDF Monorepo Refactoring

---

## Context and Problem Statement

The UNRDF codebase contains **100 files exceeding 500 lines**, with 19 files exceeding 1,000 lines. This violates the established code quality standard of `<500 lines per file` and creates maintainability, testing, and comprehension challenges.

### Discovered Reality (Evidence-Based)

```bash
# Command: find packages -name "*.mjs" -o -name "*.js" | xargs wc -l | awk '$1 > 500'
# Result: 100 files >500 lines (not the estimated 15)
```

**Top 19 Files by Size** (verified with `wc -l`):
1. `yawl-patterns.test.mjs` - 1,740 lines (test suite)
2. `workflow-api.mjs` - 1,709 lines
3. `workflow.mjs` - 1,703 lines
4. `engine.mjs` - 1,653 lines
5. `yawl-resources.mjs` - 1,569 lines
6. `yawl-cancellation.mjs` - 1,540 lines
7. `case.mjs` - 1,372 lines
8. `task.mjs` - 1,305 lines
9. `otel-span-builder.mjs` - 1,278 lines
10. `yawl-events.mjs` - 1,209 lines
11. `receipt.mjs` - 1,148 lines
12. `patterns.mjs` - 1,103 lines
13. `yawl-schemas.mjs` - 1,091 lines
14. `yawl-hooks.mjs` - 1,073 lines
15. `schemas.mjs` (KGE) - 1,063 lines
16. `query-optimizer.mjs` - 1,051 lines
17. `otel-validator-core.mjs` - 995 lines
18. `domain-infer.mjs` - 966 lines
19. `initialize.mjs` - 957 lines

**Impact Analysis**:
- `workflow.mjs`: 9 exports, 1 internal import (low coupling ✅)
- `engine.mjs`: 8 exports, 0 internal imports (very low coupling ✅)
- Total effort: ~400-500 hours for all 100 files

---

## Decision Drivers

1. **Maintainability**: Files >500 lines are difficult to understand and modify
2. **Testability**: Smaller modules enable focused unit testing
3. **Reusability**: Granular modules increase code reuse opportunities
4. **Performance**: Smaller modules improve build/bundler tree-shaking
5. **Cognitive Load**: Developers can hold <500 lines in working memory
6. **Type Safety**: JSDoc type coverage easier with focused modules
7. **Phased Migration**: Must maintain 100% backward compatibility

---

## Considered Options

### Option 1: Big Bang - Split All 100 Files
**Pros**: Complete solution, maximum benefit
**Cons**: 400-500 hours, high risk, difficult testing
**Verdict**: ❌ Rejected - Not feasible in single session

### Option 2: Phased Approach - Top 5 Files First
**Pros**: Manageable scope, proven patterns, high ROI
**Cons**: Leaves 95 files unaddressed
**Verdict**: ✅ **Selected** - Phase 1 delivers 20% of value

### Option 3: Automated Tooling (e.g., ts-morph)
**Pros**: Faster execution, consistency
**Cons**: Requires tool development, may miss semantic boundaries
**Verdict**: ⚠️ Consider for Phase 2+

---

## Decision Outcome

**Chosen Option**: **Phased Approach with Proven Architecture Patterns**

### Phase 1 (This Session): Top 5 Critical Files
Target files with highest impact and complexity:
1. `workflow.mjs` (1,703 lines)
2. `engine.mjs` (1,653 lines)
3. `workflow-api.mjs` (1,709 lines)
4. `yawl-resources.mjs` (1,569 lines)
5. `yawl-cancellation.mjs` (1,540 lines)

**Expected Output**: 20-25 new modules, all <500 lines

---

## Splitting Patterns (Architecture)

### Pattern 1: Layered Architecture (workflow.mjs)

**Rationale**: Workflow management has clear separation of concerns:
- Data structures (Workflow class)
- Validation logic (Zod schemas)
- Persistence layer (RDF serialization)
- Query operations (SPARQL)

**Module Structure**:
```
packages/yawl/src/workflow/
├── core.mjs            (Workflow class, 400 lines)
├── validation.mjs      (Zod schemas, 350 lines)
├── rdf.mjs            (RDF serialization/deserialization, 420 lines)
├── queries.mjs        (SPARQL queries, 380 lines)
├── factory.mjs        (createWorkflow, 50 lines)
└── index.mjs          (barrel exports, 30 lines)
```

**Backward Compatibility**:
```javascript
// Old: import { Workflow, createWorkflow } from './workflow.mjs'
// New: import { Workflow, createWorkflow } from './workflow/index.mjs'
// Both work via barrel export pattern
```

### Pattern 2: Domain-Driven Design (engine.mjs)

**Rationale**: Engine responsibilities follow workflow execution lifecycle:
- Initialization (setup, configuration)
- Execution (task orchestration)
- State management (case/task state)
- Event emission (subscribers, hooks)

**Module Structure**:
```
packages/yawl/src/engine/
├── initialization.mjs  (setup, config validation, 400 lines)
├── execution.mjs       (task execution logic, 450 lines)
├── state-machine.mjs   (state transitions, 380 lines)
├── event-emitter.mjs   (event bus, subscriptions, 350 lines)
├── factory.mjs         (createWorkflowEngine, 50 lines)
└── index.mjs           (orchestration + exports, 30 lines)
```

### Pattern 3: Feature-Based Modules (yawl-resources.mjs)

**Rationale**: Resource management is feature-rich with distinct responsibilities:
- Resource pool (CRUD operations)
- Allocation algorithms (assignment logic)
- Constraint checking (availability, capacity)
- Tracking/monitoring (metrics, logs)

**Module Structure**:
```
packages/yawl/src/resources/
├── pool.mjs            (resource CRUD, 400 lines)
├── allocation.mjs      (allocation algorithms, 390 lines)
├── constraints.mjs     (eligibility checking, 380 lines)
├── tracking.mjs        (monitoring, metrics, 350 lines)
└── index.mjs           (facade pattern, 30 lines)
```

### Pattern 4: API Layer Split (workflow-api.mjs)

**Rationale**: High-level API has distinct functional areas:
- Constants & schemas (type definitions)
- Workflow operations (create, update, query)
- Case operations (start, complete, cancel)
- Work item operations (enable, start, complete)

**Module Structure**:
```
packages/yawl/src/api/
├── constants.mjs       (YAWL_NS, event types, 200 lines)
├── schemas.mjs         (Zod validation schemas, 350 lines)
├── workflow-ops.mjs    (workflow CRUD, 400 lines)
├── case-ops.mjs        (case management, 380 lines)
├── workitem-ops.mjs    (work item lifecycle, 350 lines)
└── index.mjs           (unified API surface, 30 lines)
```

### Pattern 5: Concern-Based Split (yawl-cancellation.mjs)

**Rationale**: Cancellation has orthogonal concerns:
- Region management (define, nest regions)
- Work item cancellation (abort logic)
- Circuit breaker (failure cascades)
- Timeout enforcement (EffectSandbox integration)

**Module Structure**:
```
packages/yawl/src/cancellation/
├── regions.mjs         (region CRUD, nesting, 400 lines)
├── workitem.mjs        (work item cancellation, 390 lines)
├── circuit-breaker.mjs (failure detection, 380 lines)
├── timeout.mjs         (timeout enforcement, 350 lines)
└── index.mjs           (facade, 30 lines)
```

---

## Implementation Plan

### Step 1: Analyze Existing Structure
- [x] Count files >500 lines (Result: 100 files)
- [x] Identify export surfaces (9 exports in workflow, 8 in engine)
- [x] Map import dependencies (low coupling verified ✅)
- [x] Read top 5 file structures

### Step 2: Create Module Directories
```bash
mkdir -p packages/yawl/src/{workflow,engine,resources,cancellation}
mkdir -p packages/yawl/src/api
```

### Step 3: Split Files (One at a Time)
For each file:
1. Read full file content
2. Identify natural boundaries (export groups, class methods)
3. Create new module files with full JSDoc
4. Create barrel export (index.mjs)
5. Update dependent imports
6. Run tests: `timeout 10s pnpm test --filter @unrdf/yawl`
7. Verify file sizes: `wc -l packages/yawl/src/**/*.mjs | awk '$1 > 500'`

### Step 4: Verification (Per File)
```bash
# File size check (MUST be 0 results)
wc -l packages/yawl/src/workflow/*.mjs | awk '$1 > 500 {print "FAIL:", $2}'

# Import verification
node --check packages/yawl/src/workflow/*.mjs

# Test verification (MUST show ✅ pass)
timeout 10s pnpm test --filter @unrdf/yawl

# JSDoc coverage (compare before/after)
grep -c "@param\|@returns" packages/yawl/src/workflow/*.mjs
```

### Step 5: Documentation
- [ ] Update package README.md with new structure
- [ ] Create C4 component diagrams (Context, Container, Component)
- [ ] Write migration guide for downstream consumers
- [ ] Document architectural decisions in this ADR

---

## Consequences

### Positive
- **Reduced Complexity**: 1,700-line files → 350-450 line modules
- **Improved Testability**: Focused unit tests for each module
- **Better Type Safety**: Easier to maintain 100% JSDoc coverage
- **Enhanced Reusability**: Granular imports reduce bundle size
- **Clearer Boundaries**: Explicit separation of concerns
- **Easier Onboarding**: New developers understand smaller modules faster

### Negative
- **More Files**: 5 files → 25 files (5x increase)
- **Import Complexity**: More import statements (mitigated by barrel exports)
- **Initial Effort**: ~40-60 hours for Phase 1
- **Testing Surface**: More integration testing needed for module interactions

### Risks and Mitigation

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Breaking changes in imports | Medium | High | Use barrel exports + test all consumers |
| Performance regression | Low | Medium | Run OTEL validation + benchmarks |
| Incomplete migration | Medium | High | Phased approach with full test coverage |
| Lost type information | Low | High | Require 100% JSDoc in all new modules |

---

## Validation Criteria (Adversarial PM)

### Claims vs. Reality Checklist

**Before declaring Phase 1 complete**:

- [ ] **File Sizes**: Run `find packages/yawl/src -name "*.mjs" | xargs wc -l | awk '$1 > 500'` → MUST return 0 results for new modules
- [ ] **Tests Pass**: Run `timeout 10s pnpm test --filter @unrdf/yawl` → MUST show 100% pass with ✅
- [ ] **Imports Valid**: Run `node --check packages/yawl/src/**/*.mjs` → MUST return exit code 0
- [ ] **JSDoc Coverage**: Run `grep -c "@param\|@returns" <files>` → MUST be ≥ original count
- [ ] **No Breaking Changes**: All original exports available via barrel exports
- [ ] **OTEL Validation**: Run `node validation/run-all.mjs comprehensive` → Score ≥80/100

### Evidence Required

| Claim | Proof | Command |
|-------|-------|---------|
| "All files <500 lines" | File size report | `wc -l src/**/*.mjs \| sort -rn` |
| "Tests pass" | Test output with ✅ | `pnpm test --filter @unrdf/yawl` |
| "Backward compatible" | Import test | `node --eval "import './workflow/index.mjs'"` |
| "JSDoc complete" | Coverage count | `grep -c "@param" src/**/*.mjs` |

---

## Compliance with UNRDF Standards

### Code Style Essentials
- ✅ **MJS + JSDoc + Zod**: No TypeScript in source
- ✅ **Pure Functions**: No OTEL in business logic
- ✅ **Type Hints**: 100% JSDoc coverage maintained
- ✅ **File Size**: <500 lines per file (strict enforcement)

### Execution Standards
- ✅ **Batch Operations**: All splits + tests in coordinated messages
- ✅ **Timeout Commands**: `timeout 10s pnpm test` for verification
- ✅ **OTEL Validation**: ≥80/100 score required before completion
- ✅ **Measure, Don't Assume**: Run commands, read output, prove claims

---

## Architecture Diagrams (C4 Model)

### Before: Monolithic Structure
```
workflow.mjs (1,703 lines)
├── Zod Schemas (110 lines)
├── Workflow Class (1,080 lines) ⚠️
├── RDF Utilities (372 lines)
└── Factory Functions (27 lines)
```

### After: Layered Modules
```
workflow/
├── validation.mjs     (Schemas, 350 lines)
├── core.mjs          (Class, 400 lines) ✅
├── rdf.mjs           (RDF ops, 420 lines) ✅
├── queries.mjs       (SPARQL, 380 lines) ✅
├── factory.mjs       (Factory, 50 lines) ✅
└── index.mjs         (Barrel, 30 lines) ✅
```

**Quality Attributes Improved**:
- Maintainability: 400-line modules vs 1,700-line file
- Testability: Focused test suites per module
- Performance: Tree-shaking eliminates unused queries/RDF
- Comprehension: Clear module boundaries

---

## References

- Van der Aalst, W.M.P. (2005). "YAWL: Yet Another Workflow Language"
- Martin, R.C. (2017). "Clean Architecture" (Dependency Rule)
- UNRDF Code Style Guide: `/CLAUDE.md`
- Big Bang 80/20 Methodology: `/docs/bb80-20-methodology.md`

---

## Approval and Next Steps

**Approved by**: System Architecture Team
**Implementation Start**: 2025-12-25
**Target Completion**: Phase 1 - 2025-12-25 (same day)

**Phase 2 Planning**: After Phase 1 validation, create ADR-002 for next 10 files (995-1,209 lines).

---

## Appendix: Full File List (100 Files >500 Lines)

Generated with:
```bash
find packages -name "*.mjs" -o -name "*.js" | xargs wc -l | awk '$1 > 500 && $1 < 200000' | sort -rn
```

(Full list omitted for brevity - available in discovery output above)

---

**Last Updated**: 2025-12-25
**Next Review**: After Phase 1 completion and OTEL validation
