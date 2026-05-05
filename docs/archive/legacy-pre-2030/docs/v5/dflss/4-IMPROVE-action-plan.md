# IMPROVE Phase: Action Plan & Implementation Roadmap

## Overview

This document provides the detailed implementation roadmap for transforming UNRDF from v4.2.3 (bloated, 65,867 LOC) to v5.0.0 (lean, 35,000 LOC). The plan follows Lean Six Sigma principles with clear phases, milestones, dependencies, and success metrics.

---

## Strategic Objectives

### Primary Goal
Reduce UNRDF core from **65,867 LOC → 35,000 LOC** (-47%) while maintaining 100% functionality through strategic feature separation and API simplification.

### Secondary Goals
1. **API Simplification**: 300+ functions → 70 functions (-77%)
2. **Package Reduction**: 35 dependencies → 20 dependencies (-43%)
3. **Bundle Size**: 2.91 MB → <1 MB (-66%)
4. **Onboarding**: 30+ minutes → 5 minutes (-83%)
5. **Blue Ocean Positioning**: Lightweight RDF client with enterprise power

---

## Implementation Phases (12-Week Roadmap)

### Timeline Overview
```
Week 1-2:   Phase 1 (Refactoring Core)
Week 3-4:   Phase 2 (Package Separation)
Week 5-6:   Phase 3 (Documentation Restructuring)
Week 7-8:   Phase 4 (Testing & Optimization)
Week 9-10:  Phase 5 (Quality Assurance & Hardening)
Week 11-12: Phase 6 (Release & Deployment)
```

---

## PHASE 1: Core Refactoring (Weeks 1-2)

### Objective
Simplify core UNRDF API by removing functions that should be in optional packages.

### Tasks

#### 1.1 API Audit & Classification
**Effort**: 2 days
**Owner**: Architecture + Senior Dev

**Deliverables**:
- Classify all 300+ current functions into 4 tiers
- Document which 70 functions stay in core
- Tag 230 functions for removal/move
- Create migration guide for deprecated functions

**Checkpoints**:
- [ ] All functions classified
- [ ] No functions missing from classification
- [ ] Tier 4 functions documented with replacement APIs

#### 1.2 Remove/Move Advanced Features
**Effort**: 4 days
**Owner**: 2 Senior Developers (parallel work)

**Remove from core** (move to unrdf-advanced):
- Dark Matter 80/20 analysis (1,200 LOC)
- Code complexity metrics (400 LOC)
- Rule-based optimization (500 LOC)

**Remove from core** (move to unrdf-enterprise):
- Advanced policy packs (2,100 LOC)
- Vault integration (500 LOC)
- HTF framework (2,867 LOC)

**Remove from core** (move to unrdf-federation):
- Consensus algorithms (2,200 LOC)
- Replication logic (2,100 LOC)
- Node synchronization (2,500 LOC)

**Remove from core** (move to unrdf-streaming):
- Async iterators (1,500 LOC)
- Stream subscriptions (1,200 LOC)
- Change feeds (800 LOC)

**Checkpoints**:
- [ ] Code successfully moved to separate branches
- [ ] Imports updated in removed modules
- [ ] No dangling references remain
- [ ] Tests pass for affected areas

**Testing After Removal**:
```bash
# Verify core still works without removed features
pnpm test:fast           # 737 tests must pass
pnpm lint               # Zero warnings
pnpm format:check       # All formatted
```

#### 1.3 Simplify Core APIs
**Effort**: 3 days
**Owner**: 1 Senior Dev

**Remove Redundant Functions**:
| Function | Replacement | Why Remove |
|----------|-------------|-----------|
| execQueryWithFallback | queryStore | Single path, no fallback |
| validateWithWarnings | validateShape | Single validation API |
| parseWithCache | parseN3 | Built-in caching |
| executeHooksAsync | executeHooks | Always async now |
| createStoreWithMetrics | createStore + observer | Separate concerns |

**Consolidate Utilities**:
- Merge 35 utility functions into 15 essential ones
- Remove helper functions (users can call public APIs directly)
- Keep only functions in "Essential 20" list

**Checkpoints**:
- [ ] All redundant functions removed
- [ ] No function aliasing (no deprecated shortcuts)
- [ ] Type definitions updated
- [ ] Tests pass (may need 5-10 test updates)

#### 1.4 Update Type Definitions & Exports
**Effort**: 2 days
**Owner**: 1 Dev

**Changes**:
- Update index.mjs exports (remove 230 functions)
- Simplify type definitions (remove deprecated types)
- Update package.json exports field
- Create deprecation guide for removed APIs

**Checkpoints**:
- [ ] index.mjs exports only 70 functions
- [ ] No breaking changes for kept functions
- [ ] Migration guide complete
- [ ] Build succeeds, no type errors

### Phase 1 Success Criteria
- ✅ Core LOC reduced from 65,867 to ~48,000
- ✅ 737/737 tests pass (no regressions)
- ✅ All 70 essential functions documented
- ✅ Removed code moved to feature branches
- ✅ Zero lint warnings

**Milestone**: Core refactoring complete

---

## PHASE 2: Package Separation (Weeks 3-4)

### Objective
Create separate npm packages for advanced features while maintaining shared infrastructure.

### New Packages to Create

#### 2.1 unrdf-federation
**Size**: ~6,800 LOC
**Dependencies**: unrdf (core), @grpc/grpc-js, testcontainers
**Exports**:
- Consensus (Raft, Byzantine)
- Replication engines
- Node management

**Task Steps**:
1. Create repo structure: `git checkout -b release/v5.0.0-federation`
2. Copy federation code to unrdf-federation/
3. Update imports to depend on unrdf
4. Create separate package.json, vitest config
5. Move federation tests
6. Update CI/CD for separate publishing
7. Create migration guide

**Timeline**: 3 days

#### 2.2 unrdf-streaming
**Size**: ~3,500 LOC
**Dependencies**: unrdf (core), ws
**Exports**:
- AsyncIterator streams
- Change subscriptions
- WebSocket adapters

**Timeline**: 2 days

#### 2.3 unrdf-advanced
**Size**: ~5,000 LOC
**Dependencies**: unrdf (core), typhonjs-escomplex
**Exports**:
- Dark Matter optimization
- Code complexity analysis
- Rule-based optimization

**Timeline**: 2 days

#### 2.4 unrdf-enterprise
**Size**: ~8,000 LOC
**Dependencies**: unrdf (core), vault SDK (optional)
**Exports**:
- Advanced policies
- Vault integration
- Compliance frameworks
- HTF (Hierarchical Task Framework)

**Timeline**: 3 days

#### 2.5 unrdf-experimental
**Size**: ~2,000 LOC
**Dependencies**: unrdf (core), dspy (optional)
**Exports**:
- DSPy integration
- AI/ML reasoning
- Research prototypes

**Timeline**: 1 day

### Package Creation Checklist

For each package:
- [ ] Repository created
- [ ] Code migrated with tests
- [ ] package.json configured
- [ ] Exports defined (tree-shakable)
- [ ] README with examples
- [ ] CI/CD configured
- [ ] npm registry configured
- [ ] Namespace: @unrdf/<package> or unrdf-<feature>
- [ ] Version pinned to match core (initially v5.0.0)

### Phase 2 Success Criteria
- ✅ 5 new packages created and tested
- ✅ All tests pass in separated packages
- ✅ Zero regressions from extraction
- ✅ Each package independently versioned
- ✅ Migration guides written for each

**Milestone**: Feature packages created and publishable

---

## PHASE 3: Documentation Restructuring (Weeks 5-6)

### Objective
Redesign documentation around beginner → advanced progression with clear "STOP HERE" points.

### 3.1 Reorganize Documentation Structure
**Effort**: 3 days

```
docs/v5/
├─ START-HERE.md          ← Entry point: 11-line example
├─ GETTING-STARTED.md     ← Installation & first query
├─ CORE-CONCEPTS.md       ← Key ideas (RDF, SPARQL, SHACL)
├─ how-to/
│  ├─ ESSENTIAL/          ← [ESSENTIAL] 80% of users need these
│  │  ├─ 01-parse-rdf.md
│  │  ├─ 02-query-sparql.md
│  │  ├─ 03-validate-shacl.md
│  │  ├─ 04-transform-data.md
│  │  └─ README.md → "STOP HERE - you're done"
│  └─ ADVANCED/           ← [ADVANCED] For the 20%
│     ├─ hooks-autonomy.md
│     ├─ streaming-realtime.md
│     ├─ federation-sync.md
│     └─ optimization-rules.md
├─ examples/
│  ├─ 01-minimal-parse.mjs          ← 5 min, beginner
│  ├─ 02-query-sparql.mjs           ← 5 min, beginner
│  ├─ 03-validate-shacl.mjs         ← 5 min, beginner
│  ├─ 04-transform-data.mjs         ← 5 min, beginner
│  ├─ 05-knowledge-hooks.mjs        ← 15 min, intermediate
│  └─ 06-federation.mjs             ← 30 min, advanced
├─ reference/             ← API documentation
├─ architecture/          ← Design decisions
└─ FAQ.md                 ← Common questions
```

### 3.2 Update README.md
**Effort**: 1 day

**New Structure**:
```markdown
# UNRDF v5: Lightweight RDF Client with Enterprise Power

## Quick Start (11 lines, parse + query)

[The pit of success example - parsing + querying]

## That's It. You Have a Working RDF System

**Most users stop here. Explore advanced features only if needed.**

## Next Steps (Optional)
- [Parse & Query](docs/how-to/ESSENTIAL/)
- [Validate & Transform](docs/how-to/ESSENTIAL/)
- [Knowledge Hooks](docs/how-to/ADVANCED/) (for autonomous behaviors)
- [Streaming](docs/how-to/ADVANCED/) (for real-time)
- [Federation](docs/how-to/ADVANCED/) (for distributed systems)

## Core API (10 Essential Functions)
[Table with function, purpose, example]

## Advanced Features (Optional Packages)
- unrdf-federation
- unrdf-streaming
- unrdf-advanced
- unrdf-enterprise
```

### 3.3 Create "STOP HERE" Progression Markers
**Effort**: 2 days

**Markers**:
```markdown
## ESSENTIAL SKILLS (Required)
✓ Parse RDF Turtle
✓ Execute SPARQL Queries
✓ Validate with SHACL
✓ Transform & Serialize

---

## YOU'RE DONE - YOU HAVE A COMPLETE RDF SYSTEM

---

## ADVANCED FEATURES (Optional, Only If You Need Them)
→ Knowledge Hooks (for autonomous behaviors)
→ Streaming (for real-time updates)
→ Federation (for distributed systems)
→ Optimization (for performance)
```

### 3.4 Create Example Progression
**Effort**: 2 days

Create 6 examples showing beginner → advanced:
1. `01-minimal-parse.mjs` - Parse Turtle (3 lines)
2. `02-query-sparql.mjs` - Execute SPARQL (5 lines)
3. `03-validate-shacl.mjs` - Validate shapes (5 lines)
4. `04-transform-data.mjs` - Transform triples (10 lines)
5. `05-knowledge-hooks.mjs` - Autonomous behaviors (20 lines)
6. `06-federation.mjs` - Distributed sync (30 lines)

Each example:
- [ ] Runnable standalone
- [ ] Time estimate (5min, 10min, 15min, etc.)
- [ ] Complexity level (beginner, intermediate, advanced)
- [ ] Required packages (core, +streaming, +federation)

### 3.5 Add Priority Tags
**Effort**: 1 day

Tag all guides:
```
[ESSENTIAL] Parse RDF Turtle
[ESSENTIAL] Execute SPARQL Queries
[ESSENTIAL] Validate with SHACL
[ADVANCED] Knowledge Hooks (autonomous behaviors)
[ADVANCED] Streaming (real-time)
[ADVANCED] Federation (distributed)
[ENTERPRISE] Policy Packs (compliance)
```

### Phase 3 Success Criteria
- ✅ Documentation restructured around progression
- ✅ "STOP HERE" markers visible after core skills
- ✅ 6 examples in beginner → advanced order
- ✅ All guides tagged [ESSENTIAL] or [ADVANCED]
- ✅ Onboarding time: 5 min (vs. 30 min before)

**Milestone**: Documentation enables pit of success

---

## PHASE 4: Testing & Optimization (Weeks 7-8)

### Objective
Ensure v5 maintains quality while being leaner.

### 4.1 Core Test Suite Optimization
**Effort**: 3 days

**Current**: 737 tests (test:fast), 2,594 total
**Target**: 900-1000 tests in core (test:fast <15s), optional packages with their tests

**Changes**:
- Move federation tests → unrdf-federation/test
- Move streaming tests → unrdf-streaming/test
- Move advanced tests → unrdf-advanced/test
- Keep 737 critical tests in core
- Add ~150-200 new tests for simplified APIs

**New vitest.config.fast.mjs** (after separation):
```javascript
// Test files for v5 core (lean but complete)
include: [
  "test/diff.test.mjs",           // 127 tests
  "test/parse-contract.test.mjs", // 3 tests
  "test/query-contract.test.mjs", // 5 tests
  "test/composables/*.test.mjs",  // 50 tests (new)
  "test/project-engine.test.mjs",  // 142 tests
  "test/dark-matter-80-20.test.mjs", // Move to unrdf-advanced
  "test/ring-buffer.test.mjs",    // 156 tests
  "test/circuit-breaker.test.mjs", // 189 tests
  "test/lockchain-merkle.test.mjs", // 34 tests
  "test/hook-executor-deps.test.mjs", // 12 tests
  "test/e2e-integration.test.mjs", // 18 tests
  "test/cli/baseline-cli.test.mjs", // 6 tests
  "test/knowledge-engine/*.test.mjs", // 100+ tests
]
// Expected: ~900 tests, <15 seconds
```

### 4.2 Performance Profiling
**Effort**: 2 days

**Benchmarks to Capture**:
```bash
# Parse performance
time node examples/01-minimal-parse.mjs
# Expected: <5ms for 1KB Turtle

# Query performance
time node examples/02-query-sparql.mjs
# Expected: <10ms for simple query

# Validation performance
time node examples/03-validate-shacl.mjs
# Expected: <5ms for simple shape

# Build bundle size
npm run build
ls -lh dist/unrdf.mjs
# Expected: <1 MB gzipped
```

### 4.3 Bundle Size Analysis
**Effort**: 2 days

**Current Breakdown** (2.91 MB):
- n3.js: 800 KB
- Comunica: 1,200 KB
- SHACL: 300 KB
- OTEL: 400 KB
- Other: 291 KB

**Target Breakdown** (<1 MB):
- n3.js: 800 KB (same)
- Comunica: 1,200 KB (same)
- SHACL: 300 KB (same, kept for validation)
- OTEL (lite): 40 KB (stripped to essential spans)
- Knowledge Engine: 120 KB
- Other: 40 KB

**Optimization Tasks**:
- Remove unused Comunica plugins
- Treeshake OTEL infrastructure
- Minify built-in constants
- Compress example data

### 4.4 Regression Testing
**Effort**: 2 days

**Test Matrices**:
```
Node.js Versions: 18.x, 20.x, 22.x
Test Frameworks: vitest, jest (compatibility)
RDF Formats: Turtle, JSON-LD, N-Triples
SPARQL Queries: SELECT, CONSTRUCT, ASK, DESCRIBE
Browsers: (minimal - core focus is Node.js + browser fetch)
```

**Checklist**:
- [ ] All tests pass on Node 18.x
- [ ] All tests pass on Node 20.x
- [ ] All tests pass on Node 22.x
- [ ] Backwards compat tests (deprecated APIs show warnings)
- [ ] Feature package tests (each separate package)
- [ ] Integration tests (core + one feature package)

### Phase 4 Success Criteria
- ✅ Core tests: 737 critical tests, <15 seconds
- ✅ Optional packages: tests move with code
- ✅ Bundle size: <1 MB gzipped
- ✅ Performance: All operations <20ms
- ✅ Zero regressions from v4.2.3 core functionality

**Milestone**: Quality assurance complete

---

## PHASE 5: Release Preparation (Weeks 9-10)

### Objective
Prepare v5.0.0 for production release.

### 5.1 Migration Guide Creation
**Effort**: 3 days

**Deliverables**:
- `docs/MIGRATION-v4-to-v5.md` - Complete upgrade path
- Changelog generation
- Breaking changes documented
- Deprecated APIs with replacements

**Structure**:
```markdown
# Migration Guide: v4.2.3 → v5.0.0

## Breaking Changes
[List all removed functions with replacements]

## Package Separation
- Advanced features moved to separate packages
- Import paths changed slightly
- Functionality preserved, just separated

## API Simplification
- 300+ functions → 70 functions
- Composables pattern primary
- Direct API available for compatibility

## Examples
[Before/After code examples for common scenarios]
```

### 5.2 Release Notes & Changelog
**Effort**: 1 day

**Highlights**:
```markdown
# UNRDF v5.0.0 Release

## Highlights
- 47% smaller core (65,867 → 35,000 LOC)
- 77% fewer exports (300+ → 70 functions)
- 66% smaller bundle (<1 MB, was 2.91 MB)
- 83% faster onboarding (5 min, was 30 min)
- Feature packages for advanced functionality

## Major Features
- [NEW] Streamlined core RDF client
- [NEW] Composables as primary API
- [NEW] Pit of success pattern
- [MOVED] Federation → unrdf-federation
- [MOVED] Streaming → unrdf-streaming
- [MOVED] Enterprise → unrdf-enterprise
- [REMOVED] Bloat features (see migration guide)

## Installation
npm install unrdf@5.0.0

# Optional Features
npm install unrdf-federation  # For distributed RDF
npm install unrdf-streaming   # For real-time
npm install unrdf-enterprise  # For policies & vault
```

### 5.3 Version Bump & Publishing Prep
**Effort**: 1 day

**Tasks**:
- [ ] Update package.json: version 5.0.0
- [ ] Update src/index.mjs version constant
- [ ] Update all documentation version references
- [ ] Create git tag: v5.0.0-rc1 (release candidate)
- [ ] Test npm dry-run: npm publish --dry-run
- [ ] Prepare npm announcement

### 5.4 Communication & Announcements
**Effort**: 2 days

**Channels**:
- [ ] GitHub release notes (detailed)
- [ ] npm registry (summary)
- [ ] Community channels (if applicable)
- [ ] Social media (announce new positioning)

**Message**: "UNRDF v5: Lightweight yet Enterprise-Powerful. Simplified core with optional feature packages."

### Phase 5 Success Criteria
- ✅ Migration guide complete and tested
- ✅ Release notes published
- ✅ All versions updated consistently
- ✅ npm dry-run successful
- ✅ Communication prepared

**Milestone**: Ready for production release

---

## PHASE 6: Final Release & Deployment (Weeks 11-12)

### Objective
Release v5.0.0 and ensure smooth adoption.

### 6.1 Final Testing
**Effort**: 2 days

**Pre-Release Checklist**:
- [ ] npm run test:fast passes (737 tests)
- [ ] npm run test passes (full suite)
- [ ] npm run lint passes (zero warnings)
- [ ] npm run format:check passes (all formatted)
- [ ] npm run build succeeds
- [ ] Bundle size verified (<1 MB)
- [ ] All documentation links valid
- [ ] Migration guide tested (real upgrade scenario)

### 6.2 Publish to npm
**Effort**: 1 day

**Command**:
```bash
npm version major  # 4.2.3 → 5.0.0
npm publish        # Publish v5.0.0
npm publish --tag latest
```

**Also Publish Optional Packages**:
```bash
npm publish        # unrdf-federation@5.0.0
npm publish        # unrdf-streaming@5.0.0
npm publish        # unrdf-enterprise@5.0.0
npm publish        # unrdf-advanced@5.0.0
npm publish        # unrdf-experimental@5.0.0
```

### 6.3 Post-Release Validation
**Effort**: 2 days

**Validation Steps**:
1. Verify npm registry shows v5.0.0
2. Test installation: `npm install unrdf@5.0.0`
3. Test imports work correctly
4. Test migration from v4.2.3
5. Monitor GitHub issues (new v5 bugs)
6. Respond to community feedback

### 6.4 Documentation Deployment
**Effort**: 1 day

**Deploy**:
- [ ] Publish docs to website/documentation site
- [ ] Create version switcher (docs v4 vs. v5)
- [ ] Archive v4 documentation
- [ ] Update all external links

### Phase 6 Success Criteria
- ✅ v5.0.0 published to npm
- ✅ All 5 feature packages published
- ✅ Installation works smoothly
- ✅ Migration from v4 succeeds
- ✅ No critical bugs reported in first week
- ✅ Community adoption begins

**Milestone**: v5.0.0 GA (General Availability)

---

## Success Metrics Dashboard

### Quantitative Targets

| Metric | v4.2.3 | v5.0.0 Target | Status |
|--------|--------|---------------|--------|
| Core LOC | 65,867 | 35,000 | -47% |
| Functions | 300+ | 70 | -77% |
| Dependencies | 35 | 20 | -43% |
| Bundle Size (gzip) | 2.91 MB | <1 MB | -66% |
| test:fast time | ~30s | <15s | 2x faster |
| Onboarding time | 30+ min | 5 min | 6x faster |
| Documentation files | 64 active | 40-50 | Focused |
| Type coverage | 100% JSDoc | 100% JSDoc | ✓ Maintained |
| Test coverage | 95%+ | 95%+ | ✓ Maintained |

### Qualitative Targets

- ✅ Users immediately find pit of success (11-line example)
- ✅ 80% of users never need advanced features
- ✅ Composables pattern clearly primary
- ✅ "STOP HERE" markers visible after core skills
- ✅ Feature packages cleanly separated
- ✅ Zero backwards-incompatibility issues for core APIs
- ✅ Performance unchanged or improved

---

## Risk Management

### High-Risk Items

| Risk | Impact | Mitigation |
|------|--------|-----------|
| Incorrect feature separation | High | Test all separated features thoroughly |
| Missed breaking changes | High | Audit API changes carefully, document all |
| Bundle size not <1 MB | Medium | Profile and optimize aggressively |
| Test regressions | High | Run full test matrix on all platforms |
| User adoption delays | Low | Clear migration path, good docs |

### Mitigation Strategies

1. **Testing**: Run full test matrix before each phase
2. **Documentation**: Keep migration guide updated in real-time
3. **Communication**: Announce v5 early, get feedback
4. **Rollback**: Keep v4.2.3 branch for security patches
5. **Monitoring**: Track npm downloads, GitHub issues post-release

---

## Resource Requirements

### Team Composition
- 1 Architect (planning, design, reviews)
- 2 Senior Developers (implementation, testing)
- 1 Documentation Specialist (docs restructuring)
- 1 DevOps Engineer (CI/CD, publishing)
- **Total**: 5 people, 12 weeks

### Tools & Infrastructure
- GitHub (source control)
- npm (package publishing)
- CI/CD pipeline (vitest, eslint, build)
- Documentation platform (markdown hosting)
- Performance monitoring (bundle analysis)

---

## Success Definition

### v5.0.0 is DONE when:

1. **Code Quality**: All tests pass, zero lint warnings
2. **Performance**: Bundle <1 MB, test:fast <15s
3. **API**: Only 70 essential functions exposed
4. **Documentation**: Beginner-first structure, clear progression
5. **Separation**: All optional features in separate packages
6. **Migration**: Clear upgrade path from v4.2.3
7. **Adoption**: First 100 users successfully upgrade
8. **Feedback**: No critical bugs in first 2 weeks

---

## Next Step: CONTROL Phase

After IMPROVE is complete, proceed to CONTROL phase to define:
- Ongoing metrics monitoring
- Quality gates for future development
- Regression prevention strategies
- Continuous improvement cycles

See `5-CONTROL-monitoring-strategy.md` for details.

---

**Document Version**: 1.0
**Methodology**: Lean Six Sigma IMPROVE Phase
**Status**: Ready for execution
**Last Updated**: 2025-12-03
