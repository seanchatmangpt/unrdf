# Phase 3 Implementation: Week 6-8 Execution Plan

**Phase:** 3 of 6
**Duration:** 3 weeks (Weeks 6-8)
**Effort:** 250-320 hours (4 FTE)
**Packages:** 4 (composables, dark-matter, project-engine, engine-gateway)
**Files:** 64 (16 per package)
**Words:** ~80,000

---

## Overview

Phase 3 documents the final 4 public packages before moving to private packages and root-level integration. These packages span all three types:

- **Type 3 (Integration):** @unrdf/composables - Vue composition utilities
- **Type 2 (Feature):** @unrdf/dark-matter - Query optimization engine
- **Type 1 (Foundation-like):** @unrdf/project-engine - RDF project management
- **Type 2 (Feature):** @unrdf/engine-gateway - Multi-engine query federation

Parallel team execution eliminates all blockers. Each team is independent.

---

## Week 6: Composables + Dark-Matter

### Team G: @unrdf/composables (Type 3: Integration - Vue)

**Package Type:** Integration (Vue composition functions)
**Audience:** Vue developers familiar with composition API, new to UNRDF
**Effort:** 50-64 hours
**Files:** 16
**Words:** 14,000-18,000

**Monday-Tuesday (Reference Documentation, 12-16 hours)**

1. **API.md** (4-5 hours) - Composition functions documentation
   - `useRdfStore()` - Initialize store, reactivity setup
   - `useQuery()` - Execute SPARQL, track results
   - `useMutation()` - Add/update/delete triples
   - `useSubscription()` - Real-time RDF updates
   - `useIndexing()` - Configure indexing strategies
   - Each function: signature, parameters table, returns, throws, example usage
   - 25+ code snippets showing Vue composition patterns

2. **Types.md** (2-3 hours) - TypeScript/JSDoc interfaces
   - `ComposableStore` interface (properties, methods)
   - `QueryResult<T>` for typed query results
   - `MutationOptions` configuration object
   - `SubscriptionHandler` callback type
   - `IndexConfig` indexing configuration
   - All interfaces with property descriptions, types, optional flags

3. **Configuration.md** (2-3 hours) - Composables configuration options
   - Store initialization options (persistence, indexing)
   - Query performance tuning (caching, debounce)
   - Update batching (transaction size, flush interval)
   - Vue integration options (auto-unwrap refs, error handling)
   - Configuration table format with defaults and descriptions

4. **Errors.md** (2-3 hours) - Error reference and recovery
   - Store initialization errors (file not found, corruption)
   - Query execution errors (syntax, timeout, memory)
   - Mutation errors (constraint violation, permission)
   - Subscription errors (handler crash, network)
   - Error code table with message, cause, solution

5. **Performance.md** (2-3 hours) - Composition performance characteristics
   - Reactivity overhead compared to direct store access
   - Query result caching strategy
   - Memory impact of multiple stores
   - Batching benefits for mutations
   - Performance tuning recommendations

**Wednesday-Thursday (Tutorials, 12-16 hours)**

1. **01-getting-started-with-vue.md** (5-6 hours)
   - **Problem:** Want to use RDF data in Vue component
   - **Solution:** useRdfStore composition function
   - **Structure:**
     - Vue setup: composition API basics (2 min review)
     - Initialize store: `const store = useRdfStore()`
     - Execute query: `const results = useQuery(sparql)`
     - Bind to template: reactive ref binding
     - Verify: component renders results
   - **Code:** Complete working Vue component (copy-paste ready)
   - **Outcome:** User can query RDF data in Vue components

2. **02-reactive-query-updates.md** (4-5 hours)
   - **Problem:** Data changes, component should update
   - **Solution:** useSubscription for reactive updates
   - **Analogy:** Watching for changes like Vue watch()
   - **Code:**
     - Subscribe to RDF updates: `useSubscription(quad => ...)`
     - Update component state reactively
     - Unsubscribe on component unmount
   - **Demo:** Live updating dashboard with RDF data

3. **03-adding-and-modifying-data.md** (3-5 hours)
   - **Use case:** Form submission creates RDF
   - **Pattern:** useMutation composition
   - **Code:**
     - Create quads from form input
     - Add to store: `await mutation.add(quads)`
     - Handle errors gracefully
     - Show optimistic updates
   - **Real example:** Blog post creation form

**Friday (How-To Guides, 12-16 hours)**

1. **optimize-vue-reactivity.md** (3-4 hours)
   - **Problem:** Component re-renders too frequently
   - **Solutions:**
     - Use computed() for stable query results
     - Debounce subscription callbacks
     - Batch mutations into transactions
     - Configure result caching
   - **Before/after:** Performance comparison

2. **handle-large-datasets.md** (3-4 hours)
   - **Problem:** Too much data, component slows down
   - **Solutions:**
     - Pagination in queries (LIMIT/OFFSET)
     - Virtual scrolling with component
     - Lazy load related data
     - Filter at query time, not in component
   - **Code example:** Paginated table component

3. **debug-queries-in-vue.md** (3-4 hours)
   - **Problem:** Query not returning expected data
   - **Diagnosis:** Check query syntax, check data in store
   - **Solutions:**
     - Use Vue DevTools to inspect store state
     - Log query execution to console
     - Validate RDF data before querying
     - Test query in isolation
   - **Tools:** Browser DevTools, console logging patterns

4. **integrate-with-vuex-pinia.md** (3-4 hours)
   - **Problem:** Need store in multiple components
   - **Solution:** Store composable with state management
   - **Patterns:**
     - Create single store instance
     - Export from composable
     - Use in any component
     - Alternatively: Pinia store wrapper
   - **Code:** Store composable pattern, Pinia integration

**Saturday (Explanation, 10-14 hours)**

1. **vue-reactivity-model.md** (3-4 hours)
   - **Big picture:** How Vue's reactivity works
   - **Definition:** Reactive proxies track changes
   - **How it works:**
     - Vue wraps objects in Proxy
     - Changes trigger re-renders
     - Computed and watch() depend on reactivity
   - **Why:** Automatic synchronization
   - **When to use:** Building interactive UIs
   - **Why composables work:** Encapsulate reactivity logic

2. **composition-design-decisions.md** (3-4 hours)
   - **Why composition API?** Organize by feature, not type
   - **Why reactive refs?** Automatic Vue integration
   - **Why batching mutations?** Performance and consistency
   - **Why debounce subscriptions?** Prevent update storms
   - **Trade-offs:** Flexibility vs simplicity

3. **rdf-in-vue-ecosystem.md** (2-3 hours)
   - **RDF's role:** Semantic data format
   - **Vue's role:** UI reactivity engine
   - **How they fit:** UNRDF data + Vue rendering
   - **When to use:** Knowledge-heavy UIs, linked data
   - **Examples:** Recipe browser, knowledge graph explorer

---

### Team H: @unrdf/dark-matter (Type 2: Feature - Optimization)

**Package Type:** Feature Extension (Query optimization engine)
**Audience:** Developers with complex SPARQL queries
**Effort:** 56-72 hours
**Files:** 16
**Words:** 16,000-20,000

**Monday-Tuesday (Reference Documentation, 12-16 hours)**

1. **API.md** (4-5 hours) - Optimizer functions
   - `optimizeQuery(sparql)` - Main entry point
   - `analyzeQuery(sparql)` - Profile query complexity
   - `suggestIndexes(sparql)` - Index recommendations
   - `rewriteQuery(sparql, rules)` - Apply optimization rules
   - `benchmarkQuery(sparql, store)` - Measure performance
   - Parameter tables, return types, examples

2. **Types.md** (2-3 hours) - Optimization interfaces
   - `OptimizationProfile` - Query characteristics
   - `OptimizationRule` - Rewrite rules
   - `PerformanceMetrics` - Timing and resource data
   - `QueryPlan` - Execution strategy

3. **Configuration.md** (2-3 hours) - Optimizer settings
   - Optimization levels (conservative, balanced, aggressive)
   - Rule selection (which optimizations to apply)
   - Timeout settings (max optimization time)
   - Memory limits (max intermediate results)

4. **Errors.md** (2-3 hours) - Optimization errors
   - Unsupported query patterns
   - Optimization timeout
   - Memory exceeded during optimization
   - Invalid optimization rules

5. **Performance.md** (2-3 hours) - Expected improvements
   - Before/after benchmarks
   - Query type impact (patterns, aggregates, unions)
   - Trade-offs (optimization time vs execution time)
   - Scalability (works with millions of triples)

**Wednesday-Thursday (Tutorials, 12-16 hours)**

1. **01-identifying-slow-queries.md** (5-6 hours)
   - **Problem:** Query takes 5+ seconds to run
   - **Solution:** Use dark-matter to analyze
   - **Code:**
     ```javascript
     const profile = analyzeQuery(sparql);
     // Output: complexity score, bottlenecks
     const plan = optimizeQuery(sparql);
     // Output: rewritten query, expected speedup
     ```
   - **Outcome:** Understand why query is slow

2. **02-applying-optimizations.md** (4-5 hours)
   - **Problem:** Rewritten query is faster, but need to apply systematically
   - **Solution:** Integration with query builder
   - **Patterns:**
     - Analyze → Get suggestions → Apply → Benchmark
     - Safe rewrite (equivalent semantics)
     - Validation (results match original)

3. **03-building-automatic-optimization.md** (3-5 hours)
   - **Use case:** Optimize all queries automatically
   - **Pattern:** Middleware/decorator pattern
   - **Code:** Auto-optimizer that runs before execution
   - **Result:** Faster queries without developer effort

**Friday (How-To Guides, 12-16 hours)**

1. **optimize-complex-joins.md** (3-4 hours)
   - **Problem:** Query with 5+ JOIN patterns is slow
   - **Solutions:**
     - Reorder joins (most selective first)
     - Use nested patterns efficiently
     - Pre-compute intermediate results
   - **Before/after:** 50x+ speedup examples

2. **handle-large-result-sets.md** (3-4 hours)
   - **Problem:** LIMIT 1000 returns 100K results anyway
   - **Solutions:**
     - Add FILTER clauses early
     - Use LIMIT correctly (pagination)
     - Aggregate before returning
   - **Code:** Pagination patterns

3. **index-strategy.md** (3-4 hours)
   - **Problem:** Should I add indexes? Which ones?
   - **Solution:** Use suggestIndexes()
   - **Strategies:**
     - Predicate indexing (common predicates)
     - Object indexing (types, categories)
     - Combined indexing (subject+predicate)
   - **Trade-off:** Storage vs query speed

4. **benchmark-and-validate.md** (3-4 hours)
   - **Problem:** Did optimization actually help?
   - **Solution:** benchmarkQuery() and validation
   - **Process:**
     - Run original query (record time)
     - Run optimized query (record time)
     - Validate results match
     - Compare metrics (throughput, memory)

**Saturday (Explanation, 10-14 hours)**

1. **query-optimization-concepts.md** (3-4 hours)
   - **Big picture:** Why queries get slow
   - **Concepts:**
     - Cardinality (rows produced)
     - Selectivity (filters reduce rows)
     - Join order (matters for performance)
     - Indexing (trades space for speed)
   - **When optimization helps:** Complex patterns, large graphs

2. **optimization-strategies.md** (3-4 hours)
   - **Strategy 1:** Join reordering (most selective first)
   - **Strategy 2:** Early filtering (FILTER before JOIN)
   - **Strategy 3:** Pattern simplification (split complex patterns)
   - **Strategy 4:** Materialization (pre-compute subqueries)
   - **Trade-offs:** Complexity vs clarity

3. **when-to-optimize.md** (2-3 hours)
   - **When optimization helps:** Complex queries, repeated queries, large graphs
   - **When optimization doesn't help:** Simple patterns, small graphs, I/O bound
   - **Red flags:** Query takes >1 second, returns millions of rows
   - **Measurement first:** Benchmark before optimizing

---

## Week 7: Project-Engine + Engine-Gateway

### Team I: @unrdf/project-engine (Type 1: Foundation-like - Project Management)

**Package Type:** Foundation-like (RDF project management tools)
**Audience:** Developers building RDF-based systems
**Effort:** 60-76 hours
**Files:** 16
**Words:** 18,000-22,000

**Monday-Tuesday (Reference Documentation, 12-16 hours)**

1. **API.md** (4-5 hours) - Project functions
   - `createProject(config)` - Initialize project
   - `loadProject(path)` - Open existing project
   - `saveProject()` - Persist to disk
   - `getProjectStats()` - Metadata and statistics
   - `exportProject(format)` - Export to RDF/JSON
   - `validateProject()` - Check integrity
   - Full documentation with examples

2. **Types.md** (2-3 hours) - Project interfaces
   - `Project` - Main project object
   - `ProjectConfig` - Configuration structure
   - `ProjectStats` - Statistics (files, triples, graphs)
   - `ValidationResult` - Integrity check results

3. **Configuration.md** (2-3 hours) - Project settings
   - Project metadata (name, version, description)
   - Storage location and format
   - Validation rules
   - Export options
   - Backup strategy

4. **Errors.md** (2-3 hours) - Project errors
   - Project not found
   - Corrupted project file
   - Storage permission denied
   - Validation failed (constraint errors)
   - Out of disk space

5. **ProjectStructure.md** (2-3 hours) - Project layout
   - Directory structure (graphs/, metadata/, config.json)
   - Naming conventions
   - Graph organization patterns
   - Recommended practices

**Wednesday-Thursday (Tutorials, 12-16 hours)**

1. **01-your-first-project.md** (5-6 hours)
   - **Problem:** Have RDF files, want to organize as project
   - **Solution:** createProject + add graphs
   - **Steps:**
     1. Create project: `createProject({ name: 'MyProject' })`
     2. Add graph: `project.addGraph('people')`
     3. Import data: `graph.import(file)`
     4. Save: `project.save()`
   - **Outcome:** Project created and persisted

2. **02-managing-multiple-graphs.md** (4-5 hours)
   - **Problem:** Project has multiple datasets (people, organizations, relationships)
   - **Solution:** Multiple graphs in single project
   - **Pattern:**
     - Graph per entity type
     - Query across graphs when needed
     - Validate constraints per graph
   - **Code:** Project with 3+ graphs

3. **03-project-validation-and-export.md** (3-5 hours)
   - **Use case:** Quality check before publishing
   - **Pattern:** validateProject() → exportProject()
   - **Code:**
     - Run validation (find errors)
     - Fix issues
     - Export clean data
   - **Result:** High-quality RDF artifact

**Friday (How-To Guides, 12-16 hours)**

1. **organize-large-projects.md** (3-4 hours)
   - **Problem:** Project has 100M+ triples, organization matters
   - **Solutions:**
     - Graph partitioning (by type, by domain)
     - Indexing strategy
     - Directory layout
     - Naming conventions
   - **Pattern:** Hierarchy that makes sense

2. **migrate-data-between-projects.md** (3-4 hours)
   - **Problem:** Move data from old project to new
   - **Solution:** Export from old → Import to new
   - **Process:**
     - Export selection: `project.exportGraphs([graphs])`
     - Create new project
     - Import: `newProject.importGraphs(data)`
     - Validate: compare statistics

3. **backup-and-recovery.md** (3-4 hours)
   - **Problem:** Project becomes corrupted, need to recover
   - **Solutions:**
     - Regular backups
     - Version control for RDF
     - Incremental exports
     - Recovery from backups
   - **Tools:** Git integration example

4. **version-control-for-rdf.md** (3-4 hours)
   - **Problem:** How to use Git with RDF projects?
   - **Solutions:**
     - Export as N-Triples (git-friendly)
     - Store config separately
     - Diff strategies (quad-level)
     - Branching workflows
   - **Tool:** Example Git hooks

**Saturday (Explanation, 10-14 hours)**

1. **project-architecture.md** (3-4 hours)
   - **Components:** File system, graph store, metadata index
   - **How they interact:** Save/load pipeline
   - **Why this design:** Scalability, backup, organization
   - **Trade-offs:** Simplicity vs features

2. **when-to-use-projects.md** (3-4 hours)
   - **Use case 1:** Organizing related graphs
   - **Use case 2:** Long-lived RDF databases
   - **Use case 3:** Publishing RDF artifacts
   - **Not needed:** One-off queries, in-memory graphs
   - **Alternatives:** Direct store usage

3. **best-practices.md** (2-3 hours)
   - **Design patterns:** Graph naming, organization
   - **Performance:** When to use multiple graphs
   - **Validation:** Constraints per graph
   - **Testing:** Project fixtures

---

### Team J: @unrdf/engine-gateway (Type 2: Feature - Multi-Engine Federation)

**Package Type:** Feature Extension (Query federation across engines)
**Audience:** Developers querying multiple RDF sources
**Effort:** 60-76 hours
**Files:** 16
**Words:** 18,000-22,000

**Monday-Tuesday (Reference Documentation, 12-16 hours)**

1. **API.md** (4-5 hours) - Federation functions
   - `createGateway(engines)` - Initialize gateway
   - `executeQuery(sparql)` - Query all engines
   - `addEngine(engine)` - Register data source
   - `removeEngine(id)` - Unregister source
   - `getEngineStats()` - Performance metrics per engine
   - `setEnginePolicy(rules)` - Route queries
   - Full documentation with federation examples

2. **Types.md** (2-3 hours) - Gateway interfaces
   - `Engine` - Data source interface
   - `FederationResult` - Combined results
   - `EnginePolicy` - Query routing rules
   - `EngineStats` - Performance metrics

3. **Configuration.md** (2-3 hours) - Gateway settings
   - Engine registration
   - Query routing policies
   - Timeout per engine
   - Result merging strategy
   - Error handling (skip failed engines?)

4. **Errors.md** (2-3 hours) - Federation errors
   - Engine connection failed
   - Query timeout
   - Incompatible schema between engines
   - Result merge conflicts
   - Engine unreachable

5. **Performance.md** (2-3 hours) - Federation characteristics
   - Parallel vs sequential execution
   - Network latency impact
   - Result set size handling
   - Optimization for slow engines

**Wednesday-Thursday (Tutorials, 12-16 hours)**

1. **01-querying-multiple-sources.md** (5-6 hours)
   - **Problem:** Data exists in 2 different RDF sources
   - **Solution:** createGateway to query both
   - **Code:**
     ```javascript
     const gateway = createGateway([engine1, engine2]);
     const results = await gateway.executeQuery(sparql);
     // Results merged from both sources
     ```
   - **Outcome:** Query across multiple sources

2. **02-handling-partial-failures.md** (4-5 hours)
   - **Problem:** One engine is slow/down
   - **Solution:** Timeout + error handling
   - **Patterns:**
     - Set timeout per engine
     - Return partial results if some engines fail
     - Retry logic
     - Fallback to cached results

3. **03-optimizing-federated-queries.md** (3-5 hours)
   - **Use case:** Query is slow across federated sources
   - **Pattern:** Push filters to engines
   - **Code:**
     - Let each engine filter independently
     - Merge already-filtered results
     - Minimize data transfer
   - **Result:** Sub-second queries

**Friday (How-To Guides, 12-16 hours)**

1. **route-queries-to-specific-engines.md** (3-4 hours)
   - **Problem:** Which data is in which engine?
   - **Solution:** Routing policy
   - **Strategies:**
     - Engine type (local vs remote)
     - Graph pattern matching
     - Predicate-based routing
     - Custom routing function
   - **Code:** Routing examples

2. **handle-schema-differences.md** (3-4 hours)
   - **Problem:** Engines use different ontologies
   - **Solutions:**
     - Alignment layer
     - Query rewriting per engine
     - Mapping table (entity equivalence)
     - Normalized result format
   - **Example:** Product data from different sources

3. **monitor-federated-performance.md** (3-4 hours)
   - **Problem:** Which engine is slow?
   - **Solution:** getEngineStats() and metrics
   - **Metrics:**
     - Query latency per engine
     - Result size per engine
     - Error rate per engine
     - Throughput trending
   - **Visualization:** Engine performance dashboard

4. **federate-read-and-write.md** (3-4 hours)
   - **Problem:** Need to update data across engines
   - **Solutions:**
     - Master engine for writes
     - Distributed transaction
     - Sync strategy (eventual consistency)
     - Conflict resolution
   - **Pattern:** Write-through cache

**Saturday (Explanation, 10-14 hours)**

1. **federation-architecture.md** (3-4 hours)
   - **Big picture:** How federation works
   - **Components:** Engine adapter, router, merger
   - **Query execution:** Split → Execute → Merge
   - **Why this matters:** Access distributed data

2. **federation-strategies.md** (3-4 hours)
   - **Strategy 1:** Query broadcast (all engines)
   - **Strategy 2:** Smart routing (right engine for each pattern)
   - **Strategy 3:** Caching (avoid redundant queries)
   - **Strategy 4:** Hybrid (cache + live)
   - **Trade-offs:** Consistency vs latency

3. **when-to-federate.md** (2-3 hours)
   - **When useful:** Multiple RDF sources, distributed data
   - **When overkill:** Single source, small graphs
   - **Challenges:** Schema alignment, latency, consistency
   - **Alternatives:** ETL, data replication

---

## Week 8: Integration and Validation

### Phase 3 Wrap-Up (2 days)

**Team Lead Activities (10 hours total)**

1. **Final Validation Run (3 hours)**
   - Run `validate-diataxis.js` on all 4 packages
   - Check: 100% completion per package
   - Check: No TODO/FIXME placeholders
   - Check: All code examples tested

2. **Cross-Team Peer Review (4 hours)**
   - Team G reviews Team H work
   - Team I reviews Team J work
   - Feedback: consistency, completeness, clarity
   - 24-hour turnaround for feedback

3. **Phase Retrospective (2 hours)**
   - What worked well?
   - What was challenging?
   - Lessons for Phase 4
   - Team feedback on planning/execution

4. **Phase 3 Sign-Off (1 hour)**
   - All 4 packages at 100% validation
   - All files merged to main documentation
   - Ready for Phase 4

---

## Success Criteria

### Per Team (Daily Checks)
- [ ] All assigned files exist
- [ ] No TODO/FIXME placeholders
- [ ] All code examples tested and working
- [ ] Peer review: passed
- [ ] validate-diataxis.js returns 100%

### Per Week
- [ ] Both teams completed on time
- [ ] All validation: 100%
- [ ] All blockers: 0
- [ ] All retrospectives: completed

### Phase 3 Complete
- [ ] 4 packages documented
- [ ] 64 files written
- [ ] ~80,000 words
- [ ] 250-320 hours effort
- [ ] 0 rework needed
- [ ] Ready for Phase 4

---

## Dependencies & Blockers

**No external blockers:**
- Phase 2 is complete (patterns proven)
- All automation scripts ready
- DIATAXIS-GUIDE.md provides standards
- Each team is independent

**Internal dependencies:**
- Composables and Dark-Matter are independent
- Project-Engine and Engine-Gateway are independent
- **All 4 packages can be worked simultaneously**

---

## Effort Breakdown

| Package | Type | Hours | Files | Week | Team |
|---------|------|-------|-------|------|------|
| Composables | Integration | 50-64 | 16 | 6 | G |
| Dark-Matter | Feature | 56-72 | 16 | 6 | H |
| Project-Eng | Foundation | 60-76 | 16 | 7 | I |
| Engine-Gate | Feature | 60-76 | 16 | 7 | J |
| **Total** | | **250-320** | **64** | | |

---

## Resources

### Documents
- DIATAXIS-PHASE-3.md - This execution plan
- DIATAXIS-COMPOSABLES-ROADMAP.md - Package example
- DIATAXIS-PACKAGE-TYPES.md - Adaptation guide
- DIATAXIS-GUIDE.md - Writing standards
- DIATAXIS-EXAMPLES.md - Real examples

### Tools
- validate-diataxis.js - Validation script
- init-package-docs.sh - Setup script

### Templates
- docs/_templates/PACKAGE-INDEX.md
- docs/_templates/*.template.md files

---

## Handoff to Phase 4

Phase 4 (Weeks 9-10) covers private packages:
- @unrdf/common (internal utilities)
- @unrdf/testing (test helpers)
- @unrdf/benchmarks (performance utilities)
- Root-level documentation (monorepo guides)

Phase 3 provides:
- ✅ All public package patterns (foundation, feature, integration)
- ✅ 10 complete examples to follow
- ✅ Proven automation and validation
- ✅ Team experience with Diataxis framework

---

**Status:** ✅ Phase 3 ready for implementation

**Start date:** Week 6 (after Phase 2 completion)

**Expected completion:** End of Week 8

**Sign-off:** All 4 packages at 100% validation
