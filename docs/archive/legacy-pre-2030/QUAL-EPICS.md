# Code Quality & Technical Debt - EPICS

**Domain**: Code Quality & Technical Debt
**Version**: 6.0.0
**Status**: DRAFT
**Created**: 2025-12-28

---

## Overview

This document defines 7 strategic EPICs to improve UNRDF v6 code quality from **65/100 → 90/100** while eliminating critical technical debt and establishing automated enforcement.

**Current State**:
- Quality Score: 65/100
- ESLint Violations: 7+
- Files >500 lines: 33
- Functions >50 lines: 198
- God Objects: 9
- N3 Direct Imports: 7 (CRITICAL)
- Console.log in src/: 304
- Default Exports: 327

**Target State**:
- Quality Score: 90/100
- Zero ESLint violations
- Zero antipatterns
- 100% automated enforcement
- 80%+ test coverage

---

## EPIC-QUAL-001: N3 Import Elimination (CRITICAL)

**Goal**: Remove all 7 direct N3 imports from application code, migrate to @unrdf/oxigraph for 10-100x performance improvement.

**Value**:
- **Performance**: 10-100x faster SPARQL queries (empirically proven)
- **Consistency**: Single RDF store implementation across codebase
- **Security**: Eliminates duplicate parsing/validation logic vulnerabilities
- **Maintainability**: Reduces dependency surface area

**Scope**:
- 7 direct N3 imports in application code (packages/*/src)
- 11 additional imports in docs/examples (acceptable but should document)
- Oxigraph migration patterns and helper utilities
- ESLint rule enforcement to prevent regression

### Acceptance Criteria
- [ ] Zero `import ... from 'n3'` statements in `packages/*/src/**/*.mjs` (excluding n3-justified-only.mjs)
- [ ] All 7 violating files migrated to @unrdf/oxigraph
- [ ] ESLint `no-restricted-imports` rule blocks N3 imports (CI gate)
- [ ] Migration guide documented in docs/how-to/migrate-n3-to-oxigraph.md
- [ ] Performance benchmarks show ≥10x improvement on migrated code paths
- [ ] All tests pass (100% pass rate)
- [ ] CI check `grep -r "from 'n3'" packages/*/src | grep -v n3-justified | wc -l` returns 0

### Key Stories
1. **QUAL-001-1**: Audit and catalog all 7 N3 direct imports with usage patterns
2. **QUAL-001-2**: Create Oxigraph migration helper utilities (createStore, addQuads, executeQuery wrappers)
3. **QUAL-001-3**: Migrate first 2 critical files (validate pattern works)
4. **QUAL-001-4**: Migrate remaining 5 files in parallel
5. **QUAL-001-5**: Add ESLint no-restricted-imports rule for 'n3' pattern
6. **QUAL-001-6**: Document n3-justified-only.mjs exemption (streaming parser only)
7. **QUAL-001-7**: Add CI check to block future N3 imports
8. **QUAL-001-8**: Run performance benchmarks comparing N3 vs Oxigraph (prove 10x claim)

### Dependencies
- **Blocked by**: None (can start immediately)
- **Blocks**: EPIC-QUAL-003 (ESLint enforcement needs this pattern), EPIC-PERF-* (performance EPICs)

### Estimated Effort
- **T-shirt size**: M
- **Weeks**: 1-2 weeks
- **Team**: 1 backend developer + 1 tester
- **Risk**: Low (proven migration pattern from CLAUDE.md)

### Success Metrics
- Direct N3 imports: 7 → 0
- SPARQL query P95: Baseline → <10ms (simple queries)
- CI check passing: Yes
- No regressions: 100% test pass rate maintained

---

## EPIC-QUAL-002: God Object Decomposition

**Goal**: Split 9 God Object files (>15 exports) into focused, single-responsibility modules with ≤15 exports each.

**Value**:
- **Readability**: Smaller, focused modules easier to understand and navigate
- **Tree-shaking**: Better bundle optimization (30-40% size reduction potential)
- **Testing**: Isolated modules easier to test comprehensively
- **SRP Compliance**: Each module has single, clear responsibility

**Scope**:
- 9 files with >15 exports (largest: guards.mjs with 31 exports)
- Package reorganization with backward-compatible re-exports
- Documentation updates for new module structure
- Import path migrations across dependent code

### Acceptance Criteria
- [ ] All 9 God Object files split into focused modules (≤15 exports each)
- [ ] Zero files with >15 exports in packages/*/src
- [ ] Aggregator index.mjs files maintain backward compatibility
- [ ] ESLint `max-exports-per-file` rule enforced (custom plugin)
- [ ] 100% import paths updated across codebase
- [ ] All tests pass with zero regressions
- [ ] Documentation updated (API reference, migration guide)
- [ ] Tree-shaking effectiveness measured (bundle size reduction ≥20%)

### Key Stories
1. **QUAL-002-1**: Audit 9 God Objects, create decomposition plans (logical groupings)
2. **QUAL-002-2**: Split guards.mjs (31 exports) → guards/{env,file,network,command}.mjs (pattern)
3. **QUAL-002-3**: Split yawl-store.mjs (19 exports) → yawl/{store,state,events}.mjs
4. **QUAL-002-4**: Split remaining 7 God Objects following established pattern
5. **QUAL-002-5**: Create aggregator index.mjs files for backward compatibility
6. **QUAL-002-6**: Update all import paths across dependent packages
7. **QUAL-002-7**: Add custom ESLint rule to prevent future God Objects (max 15 exports)
8. **QUAL-002-8**: Document decomposition patterns in docs/explanation/module-design.md
9. **QUAL-002-9**: Measure bundle size reduction with tree-shaking analysis

### Dependencies
- **Blocked by**: None (can start immediately)
- **Blocks**: EPIC-QUAL-003 (ESLint enforcement), EPIC-ARCH-* (architecture cleanup)

### Estimated Effort
- **T-shirt size**: L
- **Weeks**: 2-3 weeks
- **Team**: 2 backend developers + 1 architect reviewer
- **Risk**: Medium (requires careful import path migration)

### Success Metrics
- God Objects: 9 → 0
- Average exports per file: 8-12 (down from 15+)
- Bundle size reduction: ≥20% (tree-shaking improvement)
- Zero breaking changes (backward compatibility maintained)
- Test pass rate: 100%

---

## EPIC-QUAL-003: ESLint Enforcement Automation

**Goal**: Implement comprehensive ESLint configuration with automated CI/CD gates to prevent all code quality violations from entering codebase.

**Value**:
- **Prevention**: Block violations at commit/PR stage (shift-left quality)
- **Consistency**: Uniform code style across 56 packages
- **Automation**: Zero manual code review for style/complexity issues
- **Fast Feedback**: Developers see violations in IDE immediately

**Scope**:
- ESLint configuration with 20+ rules (complexity, size, naming, imports)
- Pre-commit hooks (Husky)
- CI/CD quality gates (GitHub Actions)
- IDE integration (VS Code settings)
- Custom ESLint plugins for UNRDF-specific rules

### Acceptance Criteria
- [ ] `.eslintrc.json` deployed to all 56 packages (via root config)
- [ ] All 20+ rules from V6-CODE-QUALITY-STANDARDS.md enforced
- [ ] Pre-commit hook blocks commits with violations (Husky)
- [ ] CI/CD quality gate blocks PRs with violations (max-warnings=0)
- [ ] Custom rules: max-exports-per-file (≤15), no-n3-imports, no-console-in-src
- [ ] IDE integration documented (VS Code extensions + settings.json)
- [ ] Zero ESLint violations across entire codebase
- [ ] 100% of developers using pre-commit hooks (tracked via commits)

### Key Stories
1. **QUAL-003-1**: Create root .eslintrc.json with all V6 standards rules
2. **QUAL-003-2**: Set up Prettier integration (eslint-config-prettier)
3. **QUAL-003-3**: Develop custom ESLint plugin for UNRDF rules (max-exports, no-n3, etc.)
4. **QUAL-003-4**: Configure Husky pre-commit hook (lint + test:fast)
5. **QUAL-003-5**: Add CI/CD quality gate in .github/workflows/quality.yml
6. **QUAL-003-6**: Create IDE integration guide (VS Code, JetBrains)
7. **QUAL-003-7**: Fix all existing violations (blocking → run in parallel with other EPICs)
8. **QUAL-003-8**: Document ESLint rules and rationale in docs/explanation/code-standards.md
9. **QUAL-003-9**: Set up ESLint performance monitoring (lint time <30s for full codebase)

### Dependencies
- **Blocked by**: EPIC-QUAL-001 (N3 elimination), EPIC-QUAL-002 (God Objects) - to reduce violation count
- **Blocks**: All future development (enforces standards)

### Estimated Effort
- **T-shirt size**: M
- **Weeks**: 1-2 weeks (setup), + 2-3 weeks (fixing existing violations in parallel)
- **Team**: 1 DevOps engineer + 2 developers (fixing violations)
- **Risk**: Low (standard tooling, proven patterns)

### Success Metrics
- ESLint violations: Current → 0
- Lint time: <30s for full codebase
- Pre-commit hook adoption: 100%
- CI gate pass rate: 100% (after initial fixes)
- Developer satisfaction: ≥80% (survey after 1 month)

---

## EPIC-QUAL-004: File Size Reduction & Modularization

**Goal**: Split 33 oversized files (>500 lines) into focused modules of ≤500 lines each, improving readability and maintainability.

**Value**:
- **Cognitive Load**: Smaller files easier to understand and modify
- **Merge Conflicts**: Reduced conflicts in version control
- **Modularity**: Better separation of concerns
- **Testability**: Smaller modules easier to test in isolation

**Scope**:
- 33 files exceeding 500 lines (largest: 1779 lines)
- Module extraction and reorganization
- Import path updates across dependent code
- Automated file size checking in CI

### Acceptance Criteria
- [ ] Zero files >500 lines in packages/*/src (excluding exempted files)
- [ ] All 33 oversized files refactored into ≤500 line modules
- [ ] Logical module boundaries follow Single Responsibility Principle
- [ ] 100% import paths updated across codebase
- [ ] CI check enforces 500-line limit (fails build on violations)
- [ ] Exemptions documented in file headers (auto-generated schemas only)
- [ ] All tests pass with zero regressions
- [ ] Refactoring patterns documented for future reference

### Key Stories
1. **QUAL-004-1**: Audit 33 oversized files, prioritize by size and complexity
2. **QUAL-004-2**: Create refactoring playbook (extraction patterns, naming conventions)
3. **QUAL-004-3**: Refactor top 10 largest files (>800 lines) - Phase 1
4. **QUAL-004-4**: Refactor next 15 files (600-800 lines) - Phase 2
5. **QUAL-004-5**: Refactor remaining 8 files (500-600 lines) - Phase 3
6. **QUAL-004-6**: Update all import paths and re-export aggregators
7. **QUAL-004-7**: Add CI file size check (awk script in quality.yml)
8. **QUAL-004-8**: Document exemption process in V6-CODE-QUALITY-STANDARDS.md
9. **QUAL-004-9**: Add ESLint max-lines rule (error at 500 lines)

### Dependencies
- **Blocked by**: EPIC-QUAL-002 (God Object decomposition provides patterns)
- **Blocks**: EPIC-DOC-* (documentation EPICs need stable module structure)

### Estimated Effort
- **T-shirt size**: XL
- **Weeks**: 3-4 weeks
- **Team**: 3 developers (parallel refactoring), 1 code reviewer
- **Risk**: Medium (import path migrations risky without good test coverage)

### Success Metrics
- Files >500 lines: 33 → 0
- Average file size: <300 lines
- Import path errors: 0 (caught by TypeScript/tests)
- Test pass rate: 100%
- Code review time: -30% (smaller PRs easier to review)

---

## EPIC-QUAL-005: Console.log Elimination & OTEL Migration

**Goal**: Remove all 304 console.log statements from application code, replace with structured OTEL observability.

**Value**:
- **Production Ready**: No debug output leaking to production
- **Observability**: Structured telemetry with spans, metrics, events
- **Performance**: Eliminate string concatenation overhead in hot paths
- **Security**: No accidental logging of sensitive data

**Scope**:
- 304 console.log/warn/error statements in packages/*/src
- OTEL logger wrapper creation and migration
- Structured logging patterns and best practices
- ESLint no-console rule enforcement (except CLI)

### Acceptance Criteria
- [ ] Zero console.log/warn/error statements in packages/*/src/**/*.mjs
- [ ] Exemptions: packages/cli/src, *.test.mjs, scripts/ (documented)
- [ ] Structured logger wrapper created (@unrdf/observability/logger)
- [ ] All 304 console statements migrated to structured logging or OTEL events
- [ ] ESLint no-console rule enforced (blocks commits)
- [ ] CI check `grep -r "console\." packages/*/src | wc -l` returns 0
- [ ] Logging levels standardized (ERROR, WARN, INFO, DEBUG, TRACE)
- [ ] Migration guide documented in docs/how-to/structured-logging.md

### Key Stories
1. **QUAL-005-1**: Audit 304 console statements, categorize by severity/context
2. **QUAL-005-2**: Design structured logger API (@unrdf/observability/logger.mjs)
3. **QUAL-005-3**: Implement logger with OTEL integration (spans, events, attributes)
4. **QUAL-005-4**: Migrate ERROR level (50 instances) - highest priority
5. **QUAL-005-5**: Migrate WARN level (80 instances)
6. **QUAL-005-6**: Migrate INFO/DEBUG level (174 instances)
7. **QUAL-005-7**: Remove orphaned console statements (no useful info)
8. **QUAL-005-8**: Add ESLint no-console rule with CLI exemption
9. **QUAL-005-9**: Document structured logging patterns and examples
10. **QUAL-005-10**: Add CI check to block future console statements

### Dependencies
- **Blocked by**: None (can start immediately, OTEL already in place)
- **Blocks**: EPIC-OBS-* (observability EPICs need clean logger API)

### Estimated Effort
- **T-shirt size**: M
- **Weeks**: 2 weeks
- **Team**: 2 developers (parallel migration), 1 observability expert (design)
- **Risk**: Low (mechanical refactoring, low regression risk)

### Success Metrics
- Console statements in src/: 304 → 0
- Structured log events: 0 → 200+ (useful logging)
- OTEL event coverage: 80%+ of critical paths
- CI check passing: Yes
- Zero production debug output incidents

---

## EPIC-QUAL-006: Default Export Elimination

**Goal**: Remove all 327 default exports from implementation files, use named exports for better tree-shaking and refactoring.

**Value**:
- **Tree-shaking**: 20-30% bundle size reduction (proven in ecosystem)
- **Refactoring**: Find all usages with IDE search (impossible with default)
- **Consistency**: Uniform import style across codebase
- **Type Safety**: Better IDE autocomplete and type inference

**Scope**:
- 327 default exports in packages (excluding config files)
- Conversion to named exports with re-export aggregators
- Import statement updates across dependent code
- ESLint no-default-export rule enforcement

### Acceptance Criteria
- [ ] Zero `export default` in packages/*/src/**/*.mjs (excluding exemptions)
- [ ] Exemptions: *.config.mjs, framework-required files (Next.js pages)
- [ ] All 327 default exports converted to named exports
- [ ] 100% import statements updated (`import X from` → `import { X } from`)
- [ ] ESLint no-default-export rule enforced (custom rule or plugin)
- [ ] Bundle size reduction measured (≥20% for tree-shaking improvement)
- [ ] All tests pass with zero regressions
- [ ] Migration guide documented in docs/how-to/eliminate-default-exports.md

### Key Stories
1. **QUAL-006-1**: Audit 327 default exports, categorize by type (function, object, class)
2. **QUAL-006-2**: Create automated codemod script for safe conversion
3. **QUAL-006-3**: Convert default function exports (150 instances) - Phase 1
4. **QUAL-006-4**: Convert default object exports (120 instances) - Phase 2
5. **QUAL-006-5**: Convert default class exports (57 instances) - Phase 3
6. **QUAL-006-6**: Update all import statements across codebase (use codemod)
7. **QUAL-006-7**: Add ESLint no-default-export rule (error except for exemptions)
8. **QUAL-006-8**: Document exemptions (config files) in .eslintrc.json
9. **QUAL-006-9**: Measure bundle size improvement with tree-shaking analysis
10. **QUAL-006-10**: Update coding standards documentation

### Dependencies
- **Blocked by**: None (can start immediately)
- **Blocks**: EPIC-QUAL-003 (ESLint enforcement needs this pattern)

### Estimated Effort
- **T-shirt size**: L
- **Weeks**: 2-3 weeks
- **Team**: 1 developer (codemod), 2 developers (validation), 1 reviewer
- **Risk**: Medium (import path changes risky, but codemod reduces manual error)

### Success Metrics
- Default exports: 327 → 0 (excluding exemptions)
- Bundle size reduction: ≥20%
- Import statement errors: 0 (caught by tests)
- Tree-shaking effectiveness: 90%+ unused exports removed
- Test pass rate: 100%

---

## EPIC-QUAL-007: Complexity Reduction & Function Refactoring

**Goal**: Refactor 198 long functions (>50 lines) and high-complexity functions (cyclomatic >10) into focused, testable units.

**Value**:
- **Testability**: Smaller functions easier to test comprehensively
- **Readability**: Focused functions easier to understand and maintain
- **Debugging**: Reduced cognitive load when tracking bugs
- **Reusability**: Extracted helpers can be reused across codebase

**Scope**:
- 198 functions >50 lines
- Functions with cyclomatic complexity >10 (to be measured)
- Helper function extraction and naming
- ESLint complexity and max-lines-per-function enforcement

### Acceptance Criteria
- [ ] Zero functions >50 lines in packages/*/src
- [ ] Zero functions with cyclomatic complexity >10
- [ ] Zero functions with nesting depth >3
- [ ] All 198 long functions refactored using extraction patterns
- [ ] ESLint rules enforced: max-lines-per-function (50), complexity (10), max-depth (3)
- [ ] Helper functions have clear, descriptive names (verb-noun pattern)
- [ ] All tests pass with coverage maintained or improved (≥80%)
- [ ] Refactoring patterns documented in docs/explanation/function-design.md

### Key Stories
1. **QUAL-007-1**: Audit 198 long functions, measure cyclomatic complexity
2. **QUAL-007-2**: Create function refactoring playbook (extraction, naming, testing)
3. **QUAL-007-3**: Refactor critical path functions (highest complexity) - 30 functions
4. **QUAL-007-4**: Refactor medium complexity functions - 80 functions
5. **QUAL-007-5**: Refactor low complexity but long functions - 88 functions
6. **QUAL-007-6**: Extract common helper patterns into utility modules
7. **QUAL-007-7**: Add ESLint complexity rules (max 10) and max-lines (max 50)
8. **QUAL-007-8**: Improve test coverage on refactored functions (target 90%+)
9. **QUAL-007-9**: Document function design patterns (early returns, guard clauses, etc.)
10. **QUAL-007-10**: Measure complexity reduction and maintainability improvement

### Dependencies
- **Blocked by**: EPIC-QUAL-004 (file size reduction creates space for extracted functions)
- **Blocks**: EPIC-TEST-* (testing EPICs need stable, testable functions)

### Estimated Effort
- **T-shirt size**: XL
- **Weeks**: 4-5 weeks
- **Team**: 4 developers (parallel refactoring), 2 code reviewers, 1 test engineer
- **Risk**: High (behavior changes risky, requires strong test coverage)

### Success Metrics
- Functions >50 lines: 198 → 0
- Average cyclomatic complexity: <5 (down from current average)
- Functions with complexity >10: → 0
- Test coverage: Maintained or improved (≥80%)
- Code review time: -20% (smaller functions easier to review)
- Bug density: -30% (less complex code = fewer bugs)

---

## Cross-Epic Dependencies

```
EPIC-QUAL-001 (N3 Elimination)
    ↓ enables
EPIC-QUAL-003 (ESLint Enforcement) ← needs violations reduced
    ↑ needs patterns from
EPIC-QUAL-002 (God Objects) → provides decomposition patterns
    ↓ enables
EPIC-QUAL-004 (File Size) → uses decomposition patterns
    ↓ creates space for
EPIC-QUAL-007 (Complexity) → extracted helpers need homes

EPIC-QUAL-005 (Console.log) → independent, can run in parallel
EPIC-QUAL-006 (Default Exports) → independent, can run in parallel
```

## Recommended Implementation Order

### Phase 1: Critical Violations (Weeks 1-2)
**Parallel Execution**:
- EPIC-QUAL-001 (N3 Elimination) - 1-2 weeks
- EPIC-QUAL-005 (Console.log) - 2 weeks
- EPIC-QUAL-006 (Default Exports) - 2-3 weeks (start)

**Rationale**: Remove critical security/performance issues first

### Phase 2: Structural Improvements (Weeks 3-5)
**Sequential Execution**:
- EPIC-QUAL-002 (God Objects) - 2-3 weeks
- EPIC-QUAL-004 (File Size) - 3-4 weeks (overlaps with QUAL-002)

**Rationale**: Establish modular structure before complexity work

### Phase 3: Complexity & Enforcement (Weeks 6-10)
**Parallel Execution**:
- EPIC-QUAL-007 (Complexity) - 4-5 weeks
- EPIC-QUAL-003 (ESLint Enforcement) - 1-2 weeks setup + ongoing fixes

**Rationale**: Refactor complex code, then lock in standards with automation

## Success Criteria (All EPICs Complete)

| Metric | Current | Target | Epic |
|--------|---------|--------|------|
| Quality Score | 65/100 | 90/100 | ALL |
| N3 Direct Imports | 7 | 0 | QUAL-001 |
| God Objects | 9 | 0 | QUAL-002 |
| Files >500 lines | 33 | 0 | QUAL-004 |
| Functions >50 lines | 198 | 0 | QUAL-007 |
| Console.log in src/ | 304 | 0 | QUAL-005 |
| Default Exports | 327 | 0 | QUAL-006 |
| ESLint Violations | 7+ | 0 | QUAL-003 |
| Cyclomatic >10 | TBD | 0 | QUAL-007 |
| Test Coverage | ~70% | 80%+ | ALL |
| Bundle Size | Baseline | -25% | QUAL-002,006 |

## Risk Register

| Risk | Probability | Impact | Mitigation | Owner |
|------|-------------|--------|------------|-------|
| Import path migration errors | 60% | High | Comprehensive test coverage, codemod validation | QUAL-002,004,006 |
| Behavior changes during refactoring | 45% | Critical | Snapshot tests, 100% coverage on refactored functions | QUAL-007 |
| ESLint performance degradation | 35% | Medium | Optimize rules, use caching, parallel execution | QUAL-003 |
| Developer resistance to hooks | 40% | Medium | Clear documentation, IDE integration, training | QUAL-003 |
| Regression bugs from complexity work | 55% | High | Incremental refactoring, extensive testing, peer review | QUAL-007 |

## Resource Requirements

**Team Composition**:
- 4-5 Backend Developers (refactoring, migration)
- 1 DevOps Engineer (CI/CD, ESLint setup)
- 1 System Architect (design review, patterns)
- 2 QA Engineers (testing, validation)
- 1 Tech Lead (coordination, code review)

**Timeline**: 10 weeks total (with parallelization)

**Budget**: 10 person-weeks × 9 people = 90 person-weeks

---

## Appendix: Measurement Commands

### Current State Baseline
```bash
# Quality score
pnpm run quality --json > baseline-quality.json

# N3 imports
grep -r "from 'n3'" packages/*/src --include="*.mjs" | grep -v n3-justified | wc -l

# God objects
find packages -name "*.mjs" -not -path "*/test/*" \
  -exec awk '/^export / {exports++} END {if (exports > 15) print FILENAME ": " exports}' {} \;

# Oversized files
find packages -name "*.mjs" -not -path "*/test/*" \
  -exec awk 'NF && !/^[[:space:]]*\/\// {count++} END {if (count > 500) print FILENAME ": " count}' {} \;

# Long functions
find packages -name "*.mjs" \
  -exec awk '/^(export )?function/ {lines=0} {lines++} /^}/ && lines>50 {print FILENAME ":" lines}' {} \;

# Console.log
grep -r "console\." packages/*/src --include="*.mjs" | wc -l

# Default exports
grep -r "export default" packages/*/src --include="*.mjs" | wc -l

# Cyclomatic complexity
npx eslint --rule 'complexity: ["error", 10]' packages/*/src 2>&1 | grep "complexity" | wc -l
```

### Progress Tracking
Run weekly:
```bash
./scripts/quality-report.sh > reports/quality-week-$(date +%U).json
node scripts/quality-trends.mjs reports/quality-*.json
```

---

**Document Version**: 1.0.0
**Next Review**: 2025-01-04 (weekly during execution)
**Owner**: Code Quality Domain Lead
**Status**: Ready for breakdown into stories
