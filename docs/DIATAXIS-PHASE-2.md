# Phase 2: Core Packages (Detailed Week-by-Week Breakdown)

**Duration:** 3 weeks (Weeks 3-5 of 12-week plan)
**Goal:** Complete documentation for 6 extended-feature packages
**Effort:** ~180-220 hours (3-4 FTE)
**Packages:** streaming, federation, knowledge-engine, browser, cli, react

---

## Overview

Phase 2 builds on Phase 1's foundation. The 6 Phase 2 packages are **feature extensions** that build on @unrdf/core. They have different characteristics than core:

| Package | Type | Complexity | Dependencies | Unique Aspects |
|---------|------|-----------|--------------|-----------------|
| **@unrdf/streaming** | Feature | High | core, oxigraph | Memory efficiency, backpressure |
| **@unrdf/federation** | Feature | Medium | core | Distributed queries, cross-store |
| **@unrdf/knowledge-engine** | Feature | High | core, streaming | Reasoning, inference rules |
| **@unrdf/browser** | Integration | Medium | core | Client-side, IndexedDB |
| **@unrdf/cli** | Integration | Low | core, others | Command reference, examples |
| **@unrdf/react** | Integration | Low | core, browser | React hooks, state binding |

**Key difference from Phase 1:** Rather than generic tutorials, these need **use-case focused** documentation.

---

## Week 3: Streaming & Federation

### Parallel Packages (2 teams working independently)

#### Team A: @unrdf/streaming (Week 3)

**Package purpose:** Process large RDF graphs without loading entire dataset into memory

**Documentation strategy:** Use-case focused
- **Tutorial 1:** Processing 1 million triples
- **Tutorial 2:** Streaming with backpressure
- **Tutorial 3:** Real-time data sync
- **How-To 1:** Optimize memory usage
- **How-To 2:** Handle streaming errors
- **How-To 3:** Batch processing pattern
- **How-To 4:** Performance monitoring
- **Reference:** API (parser, writer, stream adapters)
- **Explanation:** Stream architecture, backpressure design

**Writing assignments:**
- Subject Matter Expert (SME): All reference docs (API, types, config, errors)
- Tutorial Author: All tutorials
- How-To Writer: All how-to guides
- Architect: All explanation docs

**Timeline:**

**Day 1-2: Setup & API Reference** (16-20 hours)
- [ ] Initialize docs structure using script from Phase 1
- [ ] Extract API from JSDoc comments
- [ ] Document all stream functions
- [ ] Create types reference
- [ ] Create configuration reference
- [ ] Document error codes
- **Output:** 5 reference files complete
- **Validation:** ✅ REFERENCE/ section 100%

**Day 2-3: Tutorials** (16-20 hours)
- [ ] Tutorial 1: "Processing Large Graphs" (6-8 hours)
  - Real example: Load 1M triples from file
  - Streaming parser setup
  - Progress tracking
  - Memory comparison (streaming vs full load)

- [ ] Tutorial 2: "Stream Backpressure" (6-8 hours)
  - What is backpressure?
  - When it matters
  - Handling in application
  - Code example

- [ ] Tutorial 3: "Real-Time Sync" (4-6 hours)
  - Use case: Sync RDF updates
  - Stream new data
  - Merge with existing

**Output:** 3 tutorials complete
**Validation:** ✅ TUTORIALS/ section 100%

**Day 4-5: How-To Guides** (16-20 hours)
- [ ] How-To 1: "Optimize Memory" (4-6 hours)
  - Problem: Streaming uses memory
  - Solutions: Filtering, batching, windowing
  - Performance comparison

- [ ] How-To 2: "Handle Errors" (4-6 hours)
  - Problems: Parser errors, stream errors
  - Recovery patterns
  - Logging strategy

- [ ] How-To 3: "Batch Processing" (4-6 hours)
  - Problem: Process triples in batches
  - Solution: Window pattern
  - Real-world example

- [ ] How-To 4: "Monitor Performance" (4-6 hours)
  - What to measure
  - Code for profiling
  - Memory tracking

**Output:** 4 how-to guides complete
**Validation:** ✅ HOW-TO/ section 100%

**Day 5: Explanations** (12-16 hours)
- [ ] Architecture: Stream pipeline design (4-6 hours)
- [ ] Design Decisions: Why backpressure? (3-4 hours)
- [ ] Concepts: Streams in RDF context (3-4 hours)
- [ ] Performance: Throughput vs memory (2-3 hours)

**Output:** 4 explanation files
**Validation:** ✅ EXPLANATION/ section 100%

**Daily standup:** ~30 min
**Code examples:** 15+ working examples
**Peer review:** 2 hours (day 5)

**Time estimate:** 60-76 hours for streaming team

---

#### Team B: @unrdf/federation (Week 3)

**Package purpose:** Query across multiple RDF stores simultaneously

**Documentation strategy:** Multi-store use cases

**Same timeline as Team A (parallel execution):**

**Writing assignments:**
- SME: Reference docs
- Tutorial Author: Tutorials (multi-store queries)
- How-To Writer: How-to guides (distributed patterns)
- Architect: Explanations (federated architecture)

**Tutorials (16-20 hours):**
1. "Your First Federated Query" - Query 2 stores at once (6-8 hours)
2. "Cross-Store Joins" - Join data from multiple sources (6-8 hours)
3. "Distributed Reasoning" - Inference across stores (4-6 hours)

**How-To Guides (16-20 hours):**
1. "Optimize Federated Queries" (4-6 hours)
2. "Handle Remote Failures" (4-6 hours)
3. "Load Balancing Across Stores" (4-6 hours)
4. "Caching Patterns" (4-6 hours)

**Reference (12-16 hours):**
- API: Federation query builder, result merging
- Types: Federated query options, result types
- Configuration: Store registration, timeout settings
- Errors: Network errors, timeout errors
- Migration: Converting single-store queries

**Explanation (12-16 hours):**
- Architecture: Query distribution, result merging
- Design Decisions: Why this approach vs alternatives
- Concepts: Federated query execution model
- Performance: Latency vs bandwidth trade-offs

**Time estimate:** 60-76 hours for federation team

---

**Week 3 Summary:**
- ✅ Streaming package: 16 files, 100% validated
- ✅ Federation package: 16 files, 100% validated
- ✅ 120-150 hours effort
- ✅ 2 packages production-ready
- ✅ Code examples: 30+ working examples

**Blockers:** None (independent packages)

---

## Week 4: Knowledge-Engine & Browser

### Parallel Packages (2 teams working independently)

#### Team C: @unrdf/knowledge-engine (Week 4)

**Package purpose:** Inference and semantic reasoning over RDF graphs

**Documentation strategy:** Reasoning-focused (most conceptually complex)

**Unique challenge:** Need to explain EYE engine, inference rules, reasoning

**Day 1-2: Reference Docs** (20-24 hours)
- API documentation (larger than streaming/federation)
  - Rule definition API
  - Inference execution
  - Query with inference
  - Explain reasoning traces

- Types: Rule objects, inference results, reasoning context
- Configuration: Inference settings, rule loading, caching
- Errors: Rule syntax errors, infinite loops, timeout
- Migration: Adding inference to existing queries

**Day 2-3: Tutorials** (18-22 hours)
- [ ] Tutorial 1: "Your First Inference Rule" (8-10 hours)
  - Simple RDFS rule
  - Property transitivity
  - Implicit results
  - Code example

- [ ] Tutorial 2: "Building Reasoning Chains" (6-8 hours)
  - Multiple rules
  - Complex inference
  - Real-world example

- [ ] Tutorial 3: "Debugging Inference" (4-6 hours)
  - Understanding reasoning traces
  - Why inference isn't working
  - Performance considerations

**Day 4-5: How-To Guides** (20-24 hours)
- [ ] "Write Efficient Rules" (6-8 hours)
- [ ] "Handle Infinite Loops" (4-6 hours)
- [ ] "Optimize Inference Performance" (6-8 hours)
- [ ] "Debug Reasoning Issues" (4-6 hours)

**Day 5: Explanation** (14-18 hours)
- Architecture: Reasoning engine design
- Design Decisions: EYE engine choice, rule format
- Concepts: Inference vs query, closed-world vs open-world
- Performance: Reasoning complexity, optimization

**Time estimate:** 72-88 hours for knowledge-engine team

**Challenge:** Most conceptually difficult package. Need strong SME with reasoning background.

---

#### Team D: @unrdf/browser (Week 4)

**Package purpose:** Run UNRDF in web browsers with IndexedDB persistence

**Documentation strategy:** Client-side focus, integration patterns

**Day 1-2: Reference Docs** (16-20 hours)
- API: Browser-specific APIs (IndexedDB, Web Workers, Service Workers)
- Types: Browser storage options, client-side constraints
- Configuration: Persistence settings, storage quotas, offline mode
- Errors: Storage quota exceeded, browser compatibility
- Migration: Server to browser code patterns

**Day 2-3: Tutorials** (16-20 hours)
- [ ] Tutorial 1: "RDF in the Browser" (6-8 hours)
  - First RDF query in React/Vue app
  - IndexedDB setup
  - Persistent storage

- [ ] Tutorial 2: "Offline-First Application" (6-8 hours)
  - Load data from server
  - Work offline
  - Sync when reconnected

- [ ] Tutorial 3: "Web Worker Integration" (4-6 hours)
  - Off-thread query execution
  - Performance benefits
  - Code patterns

**Day 4-5: How-To Guides** (16-20 hours)
- [ ] "Handle Browser Storage Limits" (4-6 hours)
- [ ] "Implement Offline Sync" (6-8 hours)
- [ ] "Use Web Workers" (4-6 hours)
- [ ] "Debug Browser Issues" (2-4 hours)

**Day 5: Explanation** (12-16 hours)
- Architecture: Browser constraints, IndexedDB usage
- Design Decisions: Why IndexedDB? Why Web Workers?
- Concepts: Client-side vs server-side reasoning
- Performance: Memory limits, storage quotas

**Time estimate:** 60-76 hours for browser team

---

**Week 4 Summary:**
- ✅ Knowledge-engine: 16 files (most complex)
- ✅ Browser: 16 files
- ✅ 130-165 hours effort
- ✅ 2 more packages complete
- ✅ Total so far: 4 packages done

---

## Week 5: CLI & React

### Parallel Packages (2 teams working independently)

#### Team E: @unrdf/cli (Week 5)

**Package purpose:** Command-line interface for RDF operations

**Documentation strategy:** Task-oriented (different from library packages)

**Unique aspect:** Reference documentation is command reference, not API reference

**Day 1: Setup & Command Reference** (8-12 hours)
- [ ] Generate command reference from CLI help
  - query: Run SPARQL queries
  - validate: Check SHACL
  - convert: Change formats
  - load: Import RDF data
  - export: Export RDF data
  - etc.

- For each command:
  - Syntax
  - Options/flags
  - Examples
  - Output format

**Output:** Comprehensive command reference

**Day 2-3: Tutorials** (12-16 hours)
- [ ] Tutorial 1: "CLI Quickstart" (4-6 hours)
  - Install CLI
  - First query
  - Common tasks

- [ ] Tutorial 2: "Data Workflows" (4-6 hours)
  - Load, query, export
  - Chaining commands
  - Scripting

- [ ] Tutorial 3: "CI/CD Integration" (4-6 hours)
  - Using in scripts
  - Batch processing
  - Automation

**Day 4-5: How-To Guides** (12-16 hours)
- [ ] "Query Large Files" (2-3 hours)
- [ ] "Convert Between Formats" (2-3 hours)
- [ ] "Use in Scripts/Bash" (3-4 hours)
- [ ] "Performance Tips" (2-3 hours)

**Day 5: Explanation & Index** (8-12 hours)
- Design: Why a CLI? When to use it?
- Concepts: Integration point
- Use cases: Data loading, validation, conversion

**Time estimate:** 40-56 hours for CLI team

**Note:** Simpler package, shorter timeline

---

#### Team F: @unrdf/react (Week 5)

**Package purpose:** React hooks and state bindings for RDF data

**Documentation strategy:** Component-focused, state management patterns

**Day 1-2: Reference** (12-16 hours)
- API: All React hooks
  - useRdfQuery: Execute queries
  - useRdfStore: Manage store
  - useRdfValidation: SHACL validation
  - useRdfHooks: React hook integration
  - useRdfBinding: Data binding

- Types: Hook options, result types, subscription options
- Configuration: Store setup for React, optimization options
- Errors: Hook errors, query errors
- Migration: Class components to hooks

**Day 2-3: Tutorials** (12-16 hours)
- [ ] Tutorial 1: "First React Component" (4-6 hours)
  - Set up provider
  - useRdfQuery hook
  - Render results
  - Bindings

- [ ] Tutorial 2: "State Management" (4-6 hours)
  - useRdfStore hook
  - Update data
  - Reactive updates

- [ ] Tutorial 3: "Form Integration" (4-6 hours)
  - useRdfBinding for forms
  - Two-way binding
  - Validation

**Day 4-5: How-To Guides** (12-16 hours)
- [ ] "Optimize Query Performance" (3-4 hours)
- [ ] "Handle Errors Gracefully" (3-4 hours)
- [ ] "Debug with React DevTools" (2-3 hours)
- [ ] "Pattern: Search with Filters" (3-4 hours)

**Day 5: Explanation** (10-12 hours)
- Architecture: React integration design
- Design Decisions: Hooks vs classes, integration approach
- Concepts: Reactive data binding
- Performance: Re-render optimization

**Time estimate:** 46-60 hours for React team

---

**Week 5 Summary:**
- ✅ CLI: 12 files (simpler)
- ✅ React: 16 files (React-specific)
- ✅ 85-115 hours effort
- ✅ 2 more packages complete
- ✅ **Total Phase 2: 6 packages done (120 files)**

---

## Phase 2 Summary

### Packages Completed
1. ✅ @unrdf/streaming (16 files)
2. ✅ @unrdf/federation (16 files)
3. ✅ @unrdf/knowledge-engine (16 files)
4. ✅ @unrdf/browser (16 files)
5. ✅ @unrdf/cli (12 files)
6. ✅ @unrdf/react (16 files)

**Total:** 108 files, ~100,000 words

### Documentation Metrics
- **Tutorials:** 18 files (3 per package × 6, except CLI=2)
- **How-To:** 24 files (4 per package)
- **Reference:** 30 files (5 per package)
- **Explanation:** 24 files (4 per package, except CLI=2)
- **Total validation:** 100% on all 6 packages

### Team Effort
| Package | FTE | Hours | Completion |
|---------|-----|-------|-----------|
| Streaming | 1 | 60-76 | Week 3 |
| Federation | 1 | 60-76 | Week 3 |
| Knowledge-Engine | 1.2 | 72-88 | Week 4 |
| Browser | 1 | 60-76 | Week 4 |
| CLI | 0.5 | 40-56 | Week 5 |
| React | 0.8 | 46-60 | Week 5 |
| **Total** | **~5.5 FTE** | **338-412 hours** | **3 weeks** |

### Quality Gates
Each package must pass before moving on:
- ✅ validate-diataxis.js returns 100%
- ✅ No TODO/FIXME placeholders
- ✅ All code examples tested
- ✅ Peer review completed
- ✅ Links all working

---

## Key Adaptations from Phase 1

### @unrdf/core (Foundation)
- **Focus:** Basic operations, foundational
- **Examples:** Simple social network
- **Tone:** Educational, build from ground up

### @unrdf/streaming (Feature)
- **Focus:** Large graph handling, memory
- **Examples:** 1 million triple processing
- **Tone:** Problem-solving (memory constraints)

### @unrdf/federation (Feature)
- **Focus:** Multi-store operations
- **Examples:** Querying 2+ stores simultaneously
- **Tone:** Integration-focused

### @unrdf/knowledge-engine (Feature)
- **Focus:** Reasoning and inference
- **Examples:** Semantic rules, derived facts
- **Tone:** Conceptually deep (most theory)

### @unrdf/browser (Integration)
- **Focus:** Client-side constraints
- **Examples:** Web app, offline sync
- **Tone:** Practical, real-world web patterns

### @unrdf/cli (Tool)
- **Focus:** Command reference
- **Examples:** Terminal workflows
- **Tone:** Task-oriented (not code library)

### @unrdf/react (Integration)
- **Focus:** React-specific patterns
- **Examples:** Components, hooks, state
- **Tone:** React developer expectations

---

## Parallel Execution Strategy

### Week 3 (2 parallel teams)
```
Team A: Streaming      Team B: Federation
Day 1-2: Reference     Day 1-2: Reference
Day 2-3: Tutorials     Day 2-3: Tutorials
Day 4-5: How-To        Day 4-5: How-To
Day 5: Explanation     Day 5: Explanation
```

### Week 4 (2 parallel teams)
```
Team C: Knowledge-Eng  Team D: Browser
Day 1-2: Reference     Day 1-2: Reference
Day 2-3: Tutorials     Day 2-3: Tutorials
Day 4-5: How-To        Day 4-5: How-To
Day 5: Explanation     Day 5: Explanation
```

### Week 5 (2 parallel teams)
```
Team E: CLI           Team F: React
Day 1-2: Reference    Day 1-2: Reference
Day 2-3: Tutorials    Day 2-3: Tutorials
Day 4-5: How-To       Day 4-5: How-To
Day 5: Explanation    Day 5: Explanation
```

**No dependencies** between teams → Full parallelization

---

## Communication & Coordination

### Daily (5-10 min)
- Team standup on progress
- Blocker identification
- Peer review scheduling

### Mid-week (30 min)
- Cross-team sync (different package types)
- Consistency check (tone, structure)
- Template adaptation discussion

### End of week (1 hour)
- Validation run (all 6 packages)
- Sign-off from team leads
- Blockers for next week

### Post-phase (2 hours)
- Phase 2 retrospective
- Lessons learned
- Adaptation for Phase 3

---

## Success Criteria

### Per Package
- [ ] Validation score: 100%
- [ ] All code examples tested
- [ ] No TODO/FIXME
- [ ] Peer review: ✅ Passed
- [ ] Internal links: ✅ All valid
- [ ] Readability: Appropriate for audience

### Phase 2 Overall
- [ ] All 6 packages documented
- [ ] 108+ files completed
- [ ] ~100,000 words written
- [ ] 350+ hours effort
- [ ] 0 blockers blocking Phase 3
- [ ] Ready for Phase 3 start

---

## Dependencies to Phase 3

Phase 3 (Weeks 6-8) covers 4 more packages:
- @unrdf/composables (Vue)
- @unrdf/dark-matter (optimization)
- @unrdf/project-engine (tools)
- @unrdf/engine-gateway (API)

Each can adapt patterns from whichever Phase 2 package is closest:
- Composables → Follow React pattern
- Dark-matter → Follow streaming pattern (performance)
- Project-engine → Follow core pattern (foundational)
- Engine-gateway → Follow federation pattern (distributed)

---

## Risk Mitigation

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|-----------|
| Knowledge-engine too complex | Medium | High | Assign strongest SME |
| Team falls behind | Low | Medium | Extra resources available |
| Code examples fail | Low | High | Daily testing, CI validation |
| Scope creep | Medium | Low | Stick to 16/12 file template |
| Quality variance | Medium | Medium | Weekly peer review |

---

## Budget & Resources

### Staffing
- **6 teams** (1 per package, can be 0.5-1.2 FTE each)
- **Total:** ~5.5 FTE equivalent
- **Duration:** 3 weeks
- **Backfill needed:** Yes (6 people unavailable for 3 weeks)

### Tools
- Phase 1 automation scripts (already built)
- validate-diataxis.js (already built)
- Time tracking (recommended)
- GitHub project for tracking

### Budget Impact
- **Developer cost:** 5.5 FTE × 3 weeks = ~$35K-50K (at avg rates)
- **Tooling:** Included (already built)
- **QA/Review:** ~40 hours ($3K-5K)

---

## Next Phase Preview

**Phase 3 (Weeks 6-8):** Supporting packages
- 4 packages: composables, dark-matter, project-engine, engine-gateway
- ~6-8 weeks of work
- Following Phase 2 patterns
- Similar execution model

---

**Ready to execute Phase 2?** All planning complete. Start Week 3 with Team A & B.
