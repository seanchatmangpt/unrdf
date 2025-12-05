# Phase 4 Implementation: Weeks 9-10 Execution Plan

**Phase:** 4 of 6
**Duration:** 2 weeks (Weeks 9-10)
**Effort:** 200-280 hours (3.5 FTE)
**Packages:** 4 (common, testing, benchmarks, root-documentation)
**Files:** 60 (12-16 per package)
**Words:** ~75,000

---

## Overview

Phase 4 documents the private/internal packages and prepares root-level integration documentation. These packages are critical for development infrastructure but have smaller public surface areas than Phase 2-3 packages.

Package Types in Phase 4:
- **Type 1 (Foundation-like):** @unrdf/common - Internal utilities
- **Type 1 (Foundation-like):** @unrdf/testing - Test infrastructure
- **Type 2 (Feature):** @unrdf/benchmarks - Performance measurement tools
- **Type 3 (Integration):** Root-level monorepo documentation

Parallel team execution remains the strategy for Weeks 9-10.

---

## Week 9: Common + Testing

### Team K: @unrdf/common (Type 1: Foundation-like - Internal Utilities)

**Package Type:** Foundation-like (Internal utilities for all packages)
**Audience:** Package developers (internal audience)
**Effort:** 50-64 hours
**Files:** 14
**Words:** 12,000-16,000

**Monday-Tuesday (Reference Documentation, 12-16 hours)**

1. **API.md** (4-5 hours) - Utility functions
   - `deepMerge(obj1, obj2)` - Recursive object merge
   - `createNamespace(prefix)` - URI namespace helpers
   - `resolveUri(uri, namespaces)` - URI resolution
   - `normalizeQuad(quad)` - Quad canonicalization
   - `createLogger(name)` - Structured logging
   - Full documentation with usage examples

2. **Types.md** (2-3 hours) - Utility interfaces
   - `Namespace` interface (prefix, URI)
   - `LogLevel` enum (debug, info, warn, error)
   - `MergeOptions` configuration
   - `QuadFormat` types

3. **Configuration.md** (2-3 hours) - Utility settings
   - Logger configuration (level, format, output)
   - Namespace defaults
   - Merge behavior options
   - URI resolution strategies

4. **Errors.md** (2-3 hours) - Error reference
   - Invalid URI format
   - Namespace not found
   - Merge conflict
   - Resolution failed

5. **InternalPatterns.md** (2-3 hours) - Internal best practices
   - How other packages use common utilities
   - Recommended patterns
   - Anti-patterns to avoid
   - Performance considerations

**Wednesday-Thursday (Tutorials, 12-16 hours)**

1. **01-setting-up-logging.md** (5-6 hours)
   - **Problem:** Need structured logging across package
   - **Solution:** Use createLogger utility
   - **Code:** Setup logger, use at different levels
   - **Example:** Real logging from actual package

2. **02-working-with-namespaces.md** (4-5 hours)
   - **Problem:** Manage many URI prefixes
   - **Solution:** Namespace utilities
   - **Code:** Create namespaces, resolve URIs
   - **Example:** Common ontologies (RDF, RDFS, OWL)

3. **03-merging-configurations.md** (3-5 hours)
   - **Use case:** Merge user config with defaults
   - **Pattern:** deepMerge with validation
   - **Code:** Safe merging strategy
   - **Result:** Type-safe configuration

**Friday (How-To Guides, 12-16 hours)**

1. **debug-with-logging.md** (3-4 hours)
   - **Problem:** Need to troubleshoot issue
   - **Solutions:** Enable logging, trace execution
   - **Strategies:** Log levels, filtering, output formats

2. **work-with-many-ontologies.md** (3-4 hours)
   - **Problem:** Multiple RDF vocabularies
   - **Solutions:** Namespace organization
   - **Pattern:** Define shared namespace module

3. **handle-uri-edge-cases.md** (3-4 hours)
   - **Problem:** URIs with special characters
   - **Solutions:** Resolution strategies
   - **Examples:** Real-world URI patterns

4. **optimize-utility-usage.md** (3-4 hours)
   - **Problem:** Performance concerns
   - **Solutions:** Caching, batching
   - **Profiling:** Identify bottlenecks

**Saturday (Explanation, 8-12 hours)**

1. **common-architecture.md** (3-4 hours)
   - **Design:** Why these utilities?
   - **Components:** Logging, namespace, merge
   - **Usage patterns:** How packages use them

2. **logging-strategy.md** (2-3 hours)
   - **Purpose:** Troubleshooting and monitoring
   - **Levels:** What to log at each level
   - **Performance:** Async logging

3. **uri-handling.md** (3-4 hours)
   - **Concepts:** Namespace resolution, URI encoding
   - **Standards:** W3C standards
   - **Performance:** Caching implications

---

### Team L: @unrdf/testing (Type 1: Foundation-like - Test Infrastructure)

**Package Type:** Foundation-like (Testing helpers for all packages)
**Audience:** Package developers (internal audience)
**Effort:** 56-72 hours
**Files:** 14
**Words:** 14,000-18,000

**Monday-Tuesday (Reference Documentation, 12-16 hours)**

1. **API.md** (4-5 hours) - Testing utilities
   - `createTestStore(data)` - Initialize store with test data
   - `createTestDataset(quads)` - Create quad datasets
   - `expectQuads(actual, expected)` - Assertion helper
   - `mockEngine(responses)` - Mock RDF engine
   - `captureMetrics(fn)` - Performance measurement
   - Full documentation with testing examples

2. **Types.md** (2-3 hours) - Testing interfaces
   - `TestStore` interface
   - `TestDataset` configuration
   - `MockEngineOptions` setup
   - `MetricsSnapshot` results

3. **Configuration.md** (2-3 hours) - Test settings
   - Store initialization options
   - Data fixtures directory
   - Mock behavior configuration
   - Metrics collection options

4. **Errors.md** (2-3 hours) - Test-specific errors
   - Fixture not found
   - Mock setup failed
   - Assertion failed (detailed)
   - Metrics collection error

5. **FixturesGuide.md** (2-3 hours) - Test data fixtures
   - Fixture organization (by package)
   - Fixture naming conventions
   - Common test datasets
   - Fixture generation tools

**Wednesday-Thursday (Tutorials, 12-16 hours)**

1. **01-writing-your-first-test.md** (5-6 hours)
   - **Problem:** New test file, where to start?
   - **Solution:** Use testing helpers
   - **Code:**
     - Create test store
     - Load fixtures
     - Execute code
     - Assert results
   - **Example:** Real test from codebase

2. **02-mocking-engines.md** (4-5 hours)
   - **Problem:** Need to test without real engine
   - **Solution:** Mock engine configuration
   - **Patterns:**
     - Setup mock responses
     - Configure error scenarios
     - Verify calls
   - **Example:** Full mock test

3. **03-measuring-performance.md** (3-5 hours)
   - **Use case:** Ensure optimization didn't regress
   - **Pattern:** Benchmark in tests
   - **Code:** captureMetrics usage
   - **Result:** Automated performance gates

**Friday (How-To Guides, 12-16 hours)**

1. **organize-test-fixtures.md** (3-4 hours)
   - **Problem:** Fixtures scattered, hard to find
   - **Solutions:**
     - Directory organization
     - Naming conventions
     - Reusable fixtures
     - Fixture generators

2. **debug-failing-tests.md** (3-4 hours)
   - **Problem:** Test assertion failed
   - **Solutions:**
     - Print actual vs expected
     - Inspect test store state
     - Enable debug logging
     - Isolate failing code

3. **share-test-data-between-packages.md** (3-4 hours)
   - **Problem:** Multiple packages test same ontology
   - **Solutions:**
     - Central fixture library
     - Import shared datasets
     - Version control fixtures
     - Generate fixtures programmatically

4. **test-edge-cases-and-errors.md** (3-4 hours)
   - **Problem:** How to test error handling?
   - **Solutions:**
     - Mock error responses
     - Create invalid data
     - Inject failures
     - Verify error handling

**Saturday (Explanation, 8-12 hours)**

1. **testing-philosophy.md** (3-4 hours)
   - **Why test?** Confidence, regression prevention
   - **What to test?** Critical paths, edge cases
   - **When to test?** Development, CI/CD
   - **Coverage goals:** 80%+ per package

2. **mocking-strategies.md** (2-3 hours)
   - **When to mock?** External dependencies
   - **Types:** Full mock, partial mock, spy
   - **Tradeoffs:** Realism vs isolation

3. **fixture-design.md** (3-4 hours)
   - **What makes good fixtures?** Representative, minimal
   - **Organization:** By domain, by use case
   - **Reusability:** Composable fixtures

---

## Week 10: Benchmarks + Root Documentation

### Team M: @unrdf/benchmarks (Type 2: Feature - Performance Measurement)

**Package Type:** Feature Extension (Benchmark and profiling tools)
**Audience:** Package developers and DevOps
**Effort:** 50-64 hours
**Files:** 14
**Words:** 12,000-16,000

**Monday-Tuesday (Reference Documentation, 12-16 hours)**

1. **API.md** (4-5 hours) - Benchmark functions
   - `createBenchmark(name, fn)` - Create benchmark
   - `runBenchmark(bench)` - Execute measurement
   - `compareBenchmarks(before, after)` - Compare results
   - `trackMetrics(store)` - Collect store metrics
   - `generateReport(results)` - Create benchmark report
   - Full documentation with examples

2. **Types.md** (2-3 hours) - Benchmark interfaces
   - `Benchmark` configuration
   - `BenchmarkResult` output
   - `MetricsSnapshot` data
   - `ComparisonReport` format

3. **Configuration.md** (2-3 hours) - Benchmark settings
   - Iterations (number of runs)
   - Warmup runs
   - GC settings
   - Output format

4. **Errors.md** (2-3 hours) - Benchmark errors
   - Timeout
   - Memory exceeded
   - Invalid baseline
   - Comparison failed

5. **CI-Integration.md** (2-3 hours) - CI/CD integration
   - GitHub Actions setup
   - Performance gates
   - Regression detection
   - Report generation

**Wednesday-Thursday (Tutorials, 12-16 hours)**

1. **01-running-your-first-benchmark.md** (5-6 hours)
   - **Problem:** Want to measure query performance
   - **Solution:** createBenchmark utility
   - **Code:** Setup benchmark, run, view results
   - **Example:** Real benchmark from codebase

2. **02-comparing-performance-changes.md** (4-5 hours)
   - **Problem:** Did optimization help?
   - **Solution:** Compare before/after benchmarks
   - **Pattern:** Baseline → Change → Compare → Report
   - **Code:** Automated comparison

3. **03-detecting-performance-regressions.md** (3-5 hours)
   - **Use case:** CI/CD should fail if slower
   - **Pattern:** Benchmark in test suite
   - **Code:** Performance gate assertion
   - **Result:** Automated performance gates

**Friday (How-To Guides, 12-16 hours)**

1. **create-meaningful-benchmarks.md** (3-4 hours)
   - **Problem:** Benchmark doesn't reflect real usage
   - **Solutions:**
     - Representative datasets
     - Real query patterns
     - Multiple scenarios
     - Document assumptions

2. **analyze-benchmark-results.md** (3-4 hours)
   - **Problem:** Which metric matters?
   - **Solutions:**
     - Throughput vs latency
     - P99 vs average
     - Memory vs CPU
     - Identify bottlenecks

3. **integrate-benchmarks-into-ci.md** (3-4 hours)
   - **Problem:** Manual benchmarking is tedious
   - **Solutions:**
     - GitHub Actions workflow
     - Automated comparison
     - Performance gates
     - Trend tracking

4. **profile-and-optimize.md** (3-4 hours)
   - **Problem:** Too slow, where's the bottleneck?
   - **Solutions:**
     - Node.js profiler
     - Flame graphs
     - Memory profiling
     - Identify hot paths

**Saturday (Explanation, 8-12 hours)**

1. **performance-measurement-principles.md** (3-4 hours)
   - **Big picture:** Why measure?
   - **Types:** Throughput, latency, memory
   - **Tradeoffs:** Accuracy vs speed
   - **Interpretation:** Statistical significance

2. **benchmark-pitfalls.md** (2-3 hours)
   - **Common mistakes:** Not warming up, small datasets
   - **GC interference:** Impact on measurements
   - **Caching effects:** Realistic vs optimistic
   - **Solutions:** Proper configuration

3. **regression-detection.md** (3-4 hours)
   - **Purpose:** Catch slowdowns before merge
   - **Strategies:** Baseline comparison, gates
   - **Thresholds:** What's acceptable variance?
   - **Automation:** CI/CD integration

---

### Team N: Root Documentation (Type 3: Integration - Monorepo Level)

**Package Type:** Integration (Root-level monorepo documentation)
**Audience:** All users (contributors and external users)
**Effort:** 44-56 hours
**Files:** 12
**Words:** 10,000-14,000

**Monday-Tuesday (Reference Documentation, 8-10 hours)**

1. **MonorepoIndex.md** (3-4 hours) - Complete package index
   - All 17 packages with descriptions
   - Package relationships and dependencies
   - Quick lookup by use case
   - Links to full documentation

2. **ArchitectureOverview.md** (2-3 hours) - System architecture
   - Layered diagram
   - Package organization
   - Data flow between packages
   - Design principles

3. **DependencyMap.md** (2-3 hours) - Package dependency matrix
   - Visual dependency graph
   - Internal vs external dependencies
   - Version compatibility
   - Breaking changes

4. **ContributionGuidelines.md** (3-4 hours) - How to contribute
   - Getting started (clone, setup, build, test)
   - Code style (JSDoc, pnpm, ESM)
   - Documentation requirements
   - PR process

**Wednesday-Thursday (Tutorials, 12-16 hours)**

1. **01-choosing-the-right-package.md** (4-5 hours)
   - **Problem:** New to UNRDF, which package do I use?
   - **Solution:** Flowchart and decision tree
   - **Scenarios:**
     - "I want to query RDF" → @unrdf/core
     - "I'm using Vue" → @unrdf/composables
     - "I need CLI tools" → @unrdf/cli
     - "I have multiple sources" → @unrdf/federation
   - **Outcome:** User knows where to start

2. **02-building-a-multipackage-application.md** (5-7 hours)
   - **Problem:** Complex app needs multiple packages
   - **Solution:** Integration pattern
   - **Architecture:**
     - Core store: @unrdf/core
     - Persistence: @unrdf/storage
     - React UI: @unrdf/react
     - Optimization: @unrdf/dark-matter
   - **Code:** Full working example
   - **Result:** Understand package composition

3. **03-setting-up-development-environment.md** (3-5 hours)
   - **Problem:** Want to contribute or fork
   - **Solution:** Local development setup
   - **Steps:**
     - Clone monorepo
     - Install with pnpm
     - Build and test
     - Make changes
     - Run validation

**Friday (How-To Guides, 8-12 hours)**

1. **choose-package-for-use-case.md** (2-3 hours)
   - **Problem:** Which packages for my project?
   - **Solutions:** Decision trees for common cases
   - **Examples:** Blog platform, knowledge graph, real-time sync

2. **set-up-ci-cd-for-contributions.md** (2-3 hours)
   - **Problem:** Contributing to monorepo
   - **Solutions:** Local testing setup
   - **Process:** Format → Lint → Test → Build

3. **publish-fork-or-extension.md** (2-3 hours)
   - **Problem:** Want to extend UNRDF
   - **Solutions:** Build on public packages
   - **Examples:** Custom RDF type, new integration

4. **get-help-and-contribute.md** (2-3 hours)
   - **Problem:** Have questions or found issues
   - **Solutions:** Channels for feedback
   - **Process:** Issue reporting, feature requests, PRs

**Saturday (Explanation, 8-10 hours)**

1. **monorepo-design-rationale.md** (3-4 hours)
   - **Why monorepo?** Coordinated development, shared patterns
   - **Tradeoffs:** Coupling vs convenience
   - **Organization:** 17 packages in 3 categories
   - **Benefits:** Version alignment, consistent APIs

2. **package-dependency-philosophy.md** (2-3 hours)
   - **Design principle:** Minimal dependencies
   - **Internal dependencies:** Use workspace:*
   - **External dependencies:** Conservative approach
   - **Versioning:** Semantic versioning strictly

3. **choosing-between-packages.md** (3-4 hours)
   - **Core vs extensions:** Foundation vs features
   - **Integrations:** Framework-specific packages
   - **Tools:** CLI, benchmarks, testing helpers
   - **Private packages:** Infrastructure, not for external use

---

## Week 10: Integration and Validation

### Phase 4 Wrap-Up (3 days)

**Team Lead Activities (12 hours total)**

1. **Final Validation Run (3 hours)**
   - Run `validate-diataxis.js` on all 4 packages
   - Check: 100% completion per package
   - Check: No TODO/FIXME placeholders
   - Check: All code examples tested

2. **Cross-Team Peer Review (4 hours)**
   - Team K reviews Team L work
   - Team M reviews Team N work
   - Feedback: consistency, completeness, clarity
   - 24-hour turnaround for feedback

3. **Phase Retrospective (2 hours)**
   - What worked well?
   - What was challenging?
   - Lessons for Phase 5-6
   - Team feedback

4. **Phase 4 Sign-Off (1 hour)**
   - All 4 packages at 100% validation
   - Root documentation complete
   - Ready for Phase 5

5. **Phase 5-6 Preparation (2 hours)**
   - Review Phase 5-6 plans
   - Assign final teams
   - Prepare resources

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

### Phase 4 Complete
- [ ] 4 packages documented (3 internal + 1 root)
- [ ] 60 files written
- [ ] ~75,000 words
- [ ] 200-280 hours effort
- [ ] 0 rework needed
- [ ] Ready for Phase 5-6

---

## Dependencies & Blockers

**No external blockers:**
- Phases 1-3 complete (patterns proven)
- All automation scripts ready
- DIATAXIS-GUIDE.md provides standards
- Each team is independent

**Internal dependencies:**
- Week 9 packages independent (common and testing)
- Week 10 packages independent (benchmarks and root docs)
- **All 4 packages can be worked simultaneously**

---

## Effort Breakdown

| Package | Type | Hours | Files | Week | Team |
|---------|------|-------|-------|------|------|
| Common | Foundation | 50-64 | 14 | 9 | K |
| Testing | Foundation | 56-72 | 14 | 9 | L |
| Benchmarks | Feature | 50-64 | 14 | 10 | M |
| Root Docs | Integration | 44-56 | 12 | 10 | N |
| **Total** | | **200-280** | **60** | | |

---

## Resources

### Documents
- DIATAXIS-PHASE-4.md - This execution plan
- DIATAXIS-COMMON-ROADMAP.md - Package example
- DIATAXIS-GUIDE.md - Writing standards
- DIATAXIS-EXAMPLES.md - Real examples

### Tools
- validate-diataxis.js - Validation script
- init-package-docs.sh - Setup script

### Templates
- docs/_templates/*.template.md files

---

## Handoff to Phase 5-6

Phases 5-6 (Weeks 11-12) cover:
- Final validation and auditing
- Cross-package documentation links
- Publication preparation
- Breaking down monorepo-level patterns

Phase 4 provides:
- ✅ Complete internal/infrastructure documentation
- ✅ Root-level monorepo guidance
- ✅ 14 total documented packages (10 from Phases 2-4)
- ✅ 200,000+ total words across all phases
- ✅ Proven patterns for all package types

---

**Status:** ✅ Phase 4 ready for implementation

**Start date:** Week 9 (after Phase 3 completion)

**Expected completion:** End of Week 10

**Sign-off:** All 4 packages at 100% validation
