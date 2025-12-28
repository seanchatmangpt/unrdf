# UNRDF v6 Performance Contracts & Benchmarking EPICs

**Domain**: Performance Engineering
**Owner**: Performance Benchmarker Agent
**Status**: Active
**Version**: v6.0.0-rc.1
**Last Updated**: 2025-12-28

---

## Context & Current State

**Current Performance** (v6.0.0 post-merge):
- Receipt creation: 0.009ms median (target <1ms) - **EXCEEDS 111x**
- Throughput: 83,895 receipts/sec (target >5,000/sec) - **EXCEEDS 16.8x**
- Memory: 4.1MB per 1K triples (target <20MB) - **EXCEEDS 4.9x**
- System throughput: 474.7 ops/sec (target >50/sec) - **EXCEEDS 9.5x**
- Cold start: 185ms median (target <200ms) - **EXCEEDS**

**Challenge**: System EXCEEDS all targets by 4-10x, but lacks formalized contracts, regression detection, and CI enforcement.

**Vision**: Establish performance as a **first-class contract** with automated enforcement, regression prevention, and continuous optimization.

---

## EPIC-PERF-001: Performance Baseline Establishment & Tracking

**Goal**: Establish definitive performance baselines with 95%+ confidence intervals and automated tracking across all v6 operations.

**Value**:
- Enables data-driven performance decisions backed by statistical evidence
- Provides regression detection foundation with quantified thresholds
- Creates performance knowledge base for capacity planning

**Scope**: All 34 core operations across 12 packages (store, governance, hooks, workflows, runtime, etc.)

### Acceptance Criteria
- [ ] Baseline measurements for ALL 34 core operations with ≥1000 samples each
- [ ] P50, P95, P99 latency established for each operation with <5% variance
- [ ] Throughput baselines (ops/sec) for single-threaded and concurrent (10/100/1000 workers)
- [ ] Memory baselines (peak, average, growth rate) for all operation types
- [ ] Baseline data stored in `/benchmarks/baselines/v6.0.0-baseline.json` with metadata
- [ ] Historical tracking with 30-day retention showing trend analysis
- [ ] Baseline validation runs in CI on every release candidate (rc-*) tag
- [ ] Statistical confidence intervals (95%) documented for each metric

### Key Stories
1. **Core Operation Baselines** - Measure and document store creation, triple insertion (1/100/10k), SPARQL queries (simple/medium/complex)
2. **Cryptographic Operation Baselines** - Receipt creation/verification, chain operations, Merkle tree building
3. **Memory Profiling Baselines** - Per-operation peak, heap growth, GC behavior, leak detection
4. **Concurrent Load Baselines** - Throughput at 10/100/1000 concurrent workers, contention analysis
5. **Cold Start Baselines** - Process initialization, import resolution, first operation ready time
6. **Baseline Storage & Retrieval** - JSON schema for baseline storage, comparison utilities, version tracking
7. **Statistical Analysis** - Percentile calculation, outlier detection, variance analysis, confidence intervals

### Dependencies
- Blocked by: None (can start immediately)
- Blocks: EPIC-PERF-002 (CI Integration), EPIC-PERF-003 (Performance Contracts)

### Estimated Effort
- T-shirt size: **L**
- Weeks: **3-4**
- Reasoning: Comprehensive measurement across 34 operations + statistical analysis + storage infrastructure

---

## EPIC-PERF-002: CI/CD Regression Detection Pipeline

**Goal**: Implement automated performance regression detection in CI/CD that blocks merges on critical performance degradation.

**Value**:
- Prevents performance regressions from reaching production
- Reduces manual performance testing overhead by 90%
- Provides immediate feedback on performance impact of code changes

**Scope**: GitHub Actions workflows, benchmark execution, comparison logic, merge blocking

### Acceptance Criteria
- [ ] GitHub Action workflow runs benchmarks on every PR to `main` or release branches
- [ ] Regression detection compares against baseline with configurable thresholds (latency +15%, throughput -10%, memory +20%)
- [ ] CI blocks merge if ANY critical threshold breached (receipt P95 >5ms, triple insert >1ms, query >10ms, memory >20MB/1K)
- [ ] Performance report posted as PR comment with comparison table and trends
- [ ] Benchmark execution completes in <5 minutes using parallelization
- [ ] Warning conditions (non-blocking) logged for investigation (GC pause >250ms, memory growth >2MB/min)
- [ ] Historical comparison shows performance trend over last 10 commits
- [ ] Manual override capability for justified performance trade-offs with approval

### Key Stories
1. **Benchmark Execution Workflow** - GitHub Action to run benchmark suite on PR events
2. **Baseline Comparison Logic** - Compare current results against baseline with threshold enforcement
3. **Merge Blocking Integration** - Fail CI status check if critical regression detected
4. **Performance Report Generation** - Format comparison as markdown table with visual indicators
5. **Parallelized Benchmark Execution** - Run independent benchmarks concurrently to stay under 5min
6. **Trend Analysis** - Compare against last 10 commits to detect gradual degradation
7. **Override Mechanism** - Allow performance team to approve justified regressions with documentation

### Dependencies
- Blocked by: EPIC-PERF-001 (requires baselines)
- Blocks: EPIC-PERF-005 (SLA Enforcement)

### Estimated Effort
- T-shirt size: **M**
- Weeks: **2-3**
- Reasoning: Workflow setup + comparison logic + blocking mechanism + reporting

---

## EPIC-PERF-003: Performance Contracts as Code

**Goal**: Encode all performance SLAs as executable TypeScript/Zod contracts that fail at runtime if violated.

**Value**:
- Makes performance requirements explicit and enforceable
- Enables contract-driven development with immediate feedback
- Provides runtime performance guardrails in production

**Scope**: Performance contracts for all 34 core operations, runtime enforcement, OTEL integration

### Acceptance Criteria
- [ ] Zod schemas define performance contracts for each operation (P95 thresholds, memory limits, throughput minimums)
- [ ] Runtime contract validation in OTEL spans with automatic violation detection
- [ ] Contract violations logged as structured errors with context (operation, threshold, actual, margin)
- [ ] Development mode shows contract violations in console with actionable guidance
- [ ] Production mode respects error budgets (0.1% for queries, 0.01% for receipts)
- [ ] Contract configuration stored in `/packages/observability/src/performance-contracts.mjs`
- [ ] Contract testing suite validates enforcement with synthetic violations
- [ ] Documentation explains contract philosophy and how to modify thresholds

### Key Stories
1. **Contract Schema Definition** - Zod schemas for latency, throughput, memory contracts per operation
2. **Runtime Validation Integration** - Hook into OTEL span end events to validate against contracts
3. **Violation Logging** - Structured error logging with operation context, threshold, actual value, margin
4. **Development Mode Guardrails** - Console warnings with actionable suggestions (e.g., "Query exceeded 10ms P95 - consider indexing")
5. **Production Error Budgets** - Respect SLA error budgets (0.1-0.01%) before alerting
6. **Contract Testing** - Synthetic tests that intentionally violate contracts to verify enforcement
7. **Contract Documentation** - How contracts work, how to modify, when to override

### Dependencies
- Blocked by: EPIC-PERF-001 (requires baseline thresholds)
- Blocks: EPIC-PERF-005 (SLA Enforcement)

### Estimated Effort
- T-shirt size: **M**
- Weeks: **2-3**
- Reasoning: Schema design + runtime integration + testing + documentation

---

## EPIC-PERF-004: Memory Profiling & Leak Detection

**Goal**: Implement comprehensive memory profiling with automated leak detection and heap analysis for all v6 operations.

**Value**:
- Prevents memory leaks before they reach production (0% leak rate maintained)
- Provides actionable insights for memory optimization opportunities
- Ensures memory usage stays within bounds (<20MB/1K triples, <100MB cold start)

**Scope**: Heap profiling, GC analysis, leak detection, memory bounds validation

### Acceptance Criteria
- [ ] Automated heap snapshots before/after operations with diff analysis
- [ ] Leak detection runs on 10k operation stress tests with 0% growth tolerance after GC
- [ ] GC pause monitoring with alerts for major pauses >100ms
- [ ] Memory bounds validation for all operations (per-operation and system-wide limits)
- [ ] Heap dump generation on memory threshold breach for offline analysis
- [ ] Weekly memory profiling reports showing trends and optimization opportunities
- [ ] V8 heap statistics collection (heap size, used, external, arrayBuffers)
- [ ] Memory profiling integrated into benchmark suite with pass/fail criteria

### Key Stories
1. **Heap Snapshot Tooling** - Before/after heap snapshots with diff analysis for growth detection
2. **Automated Leak Detection** - 10k operation stress test with pre/post GC comparison
3. **GC Pause Monitoring** - V8 GC event listeners with pause duration tracking and alerting
4. **Memory Bounds Validation** - Runtime checks against per-operation and system-wide memory limits
5. **Heap Dump on Breach** - Automatic heap dump generation when thresholds exceeded
6. **Memory Trend Reporting** - Weekly reports showing memory usage trends and anomalies
7. **V8 Statistics Collection** - Comprehensive heap metrics from V8 diagnostics API

### Dependencies
- Blocked by: EPIC-PERF-001 (requires memory baselines)
- Blocks: None (parallel track)

### Estimated Effort
- T-shirt size: **L**
- Weeks: **3-4**
- Reasoning: V8 profiling APIs + leak detection algorithms + heap analysis + reporting

---

## EPIC-PERF-005: Latency SLA Enforcement & Monitoring

**Goal**: Enforce P95/P99 latency SLAs for all user-facing operations with real-time monitoring and alerting.

**Value**:
- Guarantees consistent user experience with quantified latency bounds
- Enables proactive performance issue detection before user impact
- Provides SLA compliance metrics for stakeholders

**Scope**: User-facing SLAs (API queries, receipt operations), internal SLAs (store operations), OTEL monitoring

### Acceptance Criteria
- [ ] P95 SLAs defined and enforced for API queries (<10ms simple, <250ms complex)
- [ ] P95 SLAs defined for receipt operations (<5ms creation, <2ms verification)
- [ ] Real-time latency monitoring via OTEL with 1-minute aggregation intervals
- [ ] Critical alerts fired when P95 >2x target for 5 minutes
- [ ] Warning alerts fired when P95 >1.5x target for 15 minutes
- [ ] SLA compliance dashboards showing current vs target with historical trends
- [ ] Automated incident reports on SLA breach with root cause analysis hints
- [ ] Latency breakdown by operation phase (submission, consensus, application)

### Key Stories
1. **SLA Definition & Configuration** - Define P95/P99 thresholds for all user-facing operations
2. **Real-time Latency Monitoring** - OTEL instrumentation with 1-minute aggregation
3. **Critical Alert Configuration** - P95 >2x target for 5min → immediate alert
4. **Warning Alert Configuration** - P95 >1.5x target for 15min → investigation alert
5. **SLA Compliance Dashboards** - Visual dashboards showing current vs SLA with trends
6. **Incident Report Generation** - Automated reports on breach with context and hints
7. **Latency Phase Breakdown** - Track latency by phase for root cause identification

### Dependencies
- Blocked by: EPIC-PERF-002 (CI Integration), EPIC-PERF-003 (Performance Contracts)
- Blocks: None (parallel track)

### Estimated Effort
- T-shirt size: **M**
- Weeks: **2-3**
- Reasoning: OTEL instrumentation + alerting configuration + dashboards + incident automation

---

## EPIC-PERF-006: Throughput Benchmarking & Scaling Analysis

**Goal**: Establish throughput benchmarks across concurrency levels (1/10/100/1000 workers) with scaling efficiency analysis.

**Value**:
- Validates system capacity for production load requirements
- Identifies scaling bottlenecks and contention points
- Provides capacity planning data for infrastructure sizing

**Scope**: Concurrent load testing, throughput measurement, scaling efficiency, bottleneck identification

### Acceptance Criteria
- [ ] Throughput benchmarks at 1, 10, 100, 1000 concurrent workers for all core operations
- [ ] Scaling efficiency metrics showing linear/sub-linear/super-linear scaling characteristics
- [ ] Bottleneck identification for operations with <80% scaling efficiency
- [ ] Throughput targets validated (>5000 receipts/sec, >500 queries/sec, >50 system ops/sec)
- [ ] Contention analysis identifying lock contention, resource saturation, queue depth
- [ ] Load testing framework supporting sustained load and burst scenarios
- [ ] Throughput SLA enforcement (minimum ops/sec per operation type)
- [ ] Concurrency recommendations based on observed scaling characteristics

### Key Stories
1. **Concurrent Load Framework** - Parameterized load generator supporting 1-1000 workers
2. **Throughput Measurement** - Accurate ops/sec measurement under concurrent load
3. **Scaling Efficiency Analysis** - Compare actual vs ideal linear scaling, calculate efficiency %
4. **Bottleneck Detection** - Identify contention points (locks, resources, queues)
5. **Sustained Load Testing** - Run sustained load for 5+ minutes to detect degradation
6. **Burst Load Testing** - Spike traffic scenarios to test elasticity
7. **Throughput SLA Validation** - Verify minimum throughput targets under load
8. **Concurrency Recommendations** - Document optimal worker counts per operation type

### Dependencies
- Blocked by: EPIC-PERF-001 (requires baselines)
- Blocks: None (parallel track)

### Estimated Effort
- T-shirt size: **L**
- Weeks: **3-4**
- Reasoning: Load framework + concurrent testing + scaling analysis + bottleneck identification

---

## EPIC-PERF-007: Performance Observability & Profiling Tools

**Goal**: Build comprehensive performance observability suite with CPU/memory profiling, flame graphs, and performance debugging tools.

**Value**:
- Enables rapid performance issue diagnosis with visual tools
- Reduces time-to-resolution for performance incidents by 80%
- Provides self-service profiling for development teams

**Scope**: CPU profiling, memory profiling, flame graphs, OTEL integration, performance CLI tools

### Acceptance Criteria
- [ ] CPU profiling with flame graph generation for any operation via CLI flag
- [ ] Memory profiling with allocation flame graphs and heap snapshots
- [ ] OTEL trace visualization showing operation breakdown and critical path
- [ ] CLI tools for on-demand profiling (`unrdf profile --cpu query.sparql`)
- [ ] Performance debugging guide with profiling workflow and interpretation
- [ ] Continuous profiling in development mode with low overhead (<5%)
- [ ] Production profiling with sampling to minimize overhead (<1%)
- [ ] Performance regression investigation tooling (compare two commits)

### Key Stories
1. **CPU Profiling Integration** - V8 CPU profiler with flamegraph.pl output
2. **Memory Profiling Integration** - Heap profiler with allocation flamegraphs
3. **OTEL Trace Visualization** - Waterfall view of operation breakdown
4. **Performance CLI Tools** - `unrdf profile` command with CPU/memory/trace modes
5. **Profiling Documentation** - How to profile, interpret results, act on findings
6. **Continuous Profiling (Dev)** - Always-on profiling in development with <5% overhead
7. **Sampled Profiling (Prod)** - Production-safe profiling with 1% sampling
8. **Regression Investigation Tools** - Compare performance profiles between commits

### Dependencies
- Blocked by: EPIC-PERF-001 (requires baselines for comparison)
- Blocks: None (parallel track)

### Estimated Effort
- T-shirt size: **L**
- Weeks: **3-4**
- Reasoning: Profiling infrastructure + visualization + CLI tools + documentation

---

## Summary & Roadmap

### Epic Priority Matrix

| Epic | Priority | Effort | Dependencies | Start Week |
|------|----------|--------|--------------|------------|
| PERF-001: Baselines | **P0** | L (3-4w) | None | Week 1 |
| PERF-003: Contracts | **P0** | M (2-3w) | PERF-001 | Week 3 |
| PERF-002: CI Integration | **P0** | M (2-3w) | PERF-001 | Week 4 |
| PERF-005: SLA Enforcement | **P1** | M (2-3w) | PERF-002, PERF-003 | Week 6 |
| PERF-004: Memory Profiling | **P1** | L (3-4w) | PERF-001 | Week 3 |
| PERF-006: Throughput Scaling | **P2** | L (3-4w) | PERF-001 | Week 5 |
| PERF-007: Observability Tools | **P2** | L (3-4w) | PERF-001 | Week 7 |

### Critical Path (16-week timeline)

```
Phase 1: Foundation (Weeks 1-4)
├── Week 1-4: EPIC-PERF-001 (Baselines) [P0]
└── Week 3-5: EPIC-PERF-003 (Contracts) [P0]

Phase 2: Automation (Weeks 4-8)
├── Week 4-6: EPIC-PERF-002 (CI Integration) [P0]
├── Week 3-6: EPIC-PERF-004 (Memory Profiling) [P1]
└── Week 5-8: EPIC-PERF-006 (Throughput Scaling) [P2]

Phase 3: Production (Weeks 6-10)
├── Week 6-8: EPIC-PERF-005 (SLA Enforcement) [P1]
└── Week 7-10: EPIC-PERF-007 (Observability Tools) [P2]

Phase 4: Optimization (Weeks 10-16)
└── Continuous improvement based on PERF-007 insights
```

### Success Metrics

**By Week 8** (Phase 2 complete):
- [ ] All 34 operations have documented baselines with 95% confidence
- [ ] CI/CD blocks merges on critical performance regressions
- [ ] Performance contracts enforced in runtime with OTEL integration
- [ ] Zero memory leaks detected in 10k operation stress tests

**By Week 16** (Phase 3 complete):
- [ ] P95/P99 SLAs enforced with real-time monitoring and alerting
- [ ] Throughput scaling characteristics documented for all operations
- [ ] Performance profiling tools available via CLI
- [ ] Performance regression incidents reduced to 0/month

### Risk Register

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Baseline variance >5% | 30% | Medium | Increase sample size to 10k, control environment |
| CI benchmark timeout >5min | 40% | High | Parallelize independent benchmarks, cache dependencies |
| Performance contracts too strict | 25% | Medium | Start with warning mode, tune over 2-week observation |
| Memory profiling overhead >5% | 20% | Low | Use sampling in production, full profiling only in dev |
| Throughput tests cause OOM | 35% | Medium | Implement memory limits per worker, gradual ramp-up |

### Alignment with v6 Vision

These EPICs directly support the v6 Complete Rewrite Vision:

- **Radical Simplification**: Performance contracts codify the "12 package, 34 exports" philosophy
- **Evidence-Based Design**: All baselines derived from actual measurements, not aspirations
- **Production Gates**: EPIC-PERF-002 implements Gate #5 (Performance P95 <50ms)
- **Zero Backwards Compatibility**: No legacy performance considerations, start fresh
- **Big Bang 80/20**: Focus on 34 core operations (20%) that deliver 80% of value

### Next Steps (This Week)

1. **Review EPICs** with Performance Benchmarker agent and System Architect
2. **Create GitHub Issues** for EPIC-PERF-001 stories (baseline establishment)
3. **Set up `/benchmarks/baselines/` directory** structure and schema
4. **Begin measurements** for core operations (store, governance, receipts)
5. **Document methodology** for reproducible baseline establishment

---

**Document Status**: DRAFT FOR REVIEW
**Review Required By**: Product Owner, System Architect, Performance Benchmarker
**Target Approval Date**: 2025-12-30
**Implementation Start**: 2026-01-02 (Week 1)

---

**Credits**:
- **Author**: Performance Benchmarker Agent (Agent #4)
- **Input**: V6 Complete Rewrite Vision (10 agents synthesis)
- **Data**: V6 Performance Targets (evidence-based)
- **Methodology**: Adversarial PM + Big Bang 80/20 + CLAUDE.md principles
