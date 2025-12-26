# Benchmark Results - UNRDF Project

**Generated:** 2025-12-25
**Node Version:** v18+
**Platform:** Linux 4.4.0
**Adversarial PM Principle:** These are MEASURED results, not estimates.

## Executive Summary

| Benchmark | Mean Latency | P95 Latency | P99 Latency | Throughput | Target | Status |
|-----------|--------------|-------------|-------------|------------|--------|--------|
| Receipt Generation | 0.339 ms | 0.593 ms | 1.483 ms | 2,492/sec | <10ms | ✅ PASS |
| Event Append | 0.30 ms | 0.43 ms | 0.98 ms | 361/sec | - | ✅ GOOD |
| Freeze Operation | 20.68 ms | 25.43 ms | 67.31 ms | 36/sec | - | ⚠️ SLOW |
| SPARQL Simple | 0.09 ms | 0.18 ms | 0.41 ms | - | - | ✅ FAST |
| SPARQL Aggregate (10K) | 5.62 ms | 6.65 ms | 6.86 ms | - | - | ✅ GOOD |

---

## 1. Receipt Generation Performance

**Benchmark:** `/home/user/unrdf/benchmarks/receipt-generation-bench.mjs`
**Iterations:** 1,000
**Warmup:** 100 iterations

### Results

```
Min:       0.202 ms
Mean:      0.339 ms
Median:    0.259 ms
Stddev:    0.292 ms
P90:       0.493 ms
P95:       0.593 ms
P99:       1.483 ms
P99.9:     5.007 ms
Max:       5.007 ms

Throughput: 2,492 receipts/sec
```

### Analysis

- **Target:** <10ms per receipt → **PASS** (P95: 0.593ms = 16x faster than target)
- **Consistency:** Low stddev (0.292ms) indicates stable performance
- **Outliers:** P99.9 at 5.007ms suggests occasional GC pauses (still well under target)
- **Production Readiness:** ✅ Yes - can handle 2,000+ receipts/sec sustained

### Verification

```bash
node /home/user/unrdf/benchmarks/receipt-generation-bench.mjs
```

**Evidence:** Output saved to `/tmp/receipt-bench-output.txt`

---

## 2. Event Append Performance

**Benchmark:** Part of freeze benchmark
**Iterations:** 1,000 events across 100 iterations
**Context:** Single-quad deltas per event

### Results

```
Min:       0.20 ms
Mean:      0.30 ms
Median:    0.29 ms
P95:       0.43 ms
P99:       0.98 ms
Max:       0.98 ms

Throughput: 361 events/sec (includes freeze overhead)
```

### Analysis

- **Latency:** Sub-millisecond mean (0.30ms) for event append + delta application
- **Consistency:** Tight distribution (0.20-0.98ms range)
- **Bottleneck:** Freeze operation dominates total time, not append
- **Production Readiness:** ✅ Yes - append operations are fast enough

---

## 3. Freeze Operation Performance

**Benchmark:** `/home/user/unrdf/benchmarks/kgc-4d-freeze-bench.mjs`
**Iterations:** 100
**Events per freeze:** 10
**Warmup:** 10 iterations

### Results

```
Min:       17.49 ms
Mean:      20.68 ms
Median:    19.36 ms
P95:       25.43 ms
P99:       67.31 ms
Max:       67.31 ms

Throughput: 36 freezes/sec (with 10 events each)
```

### Analysis

- **Latency:** ~20ms mean to snapshot + hash + git commit
- **Git Overhead:** Dominant cost is isomorphic-git commit (10-15ms)
- **Hashing:** BLAKE3 WASM adds ~2-5ms
- **Outliers:** P99 at 67ms likely from file I/O variance
- **Production Readiness:** ⚠️ Limited - max 36 snapshots/sec (acceptable for infrequent snapshots)

### Breakdown

| Operation | Estimated Time |
|-----------|----------------|
| Universe dump | 1-2 ms |
| BLAKE3 hash | 2-5 ms |
| Git commit | 10-15 ms |
| Event log append | 0.3 ms |
| **Total** | **20.68 ms** |

---

## 4. SPARQL Query Performance

**Benchmark:** `/home/user/unrdf/benchmarks/sparql-query-bench.mjs`
**Dataset Sizes:** 100, 1,000, 10,000 entities
**Queries per size:** 50
**Warmup:** 10 queries per type

### Results by Query Type

#### Simple SELECT (with LIMIT 10)

| Dataset | Mean | Median | P95 | P99 |
|---------|------|--------|-----|-----|
| 100 entities | 0.11 ms | 0.09 ms | 0.26 ms | 0.41 ms |
| 1,000 entities | 0.10 ms | 0.07 ms | 0.18 ms | 0.48 ms |
| 10,000 entities | 0.08 ms | 0.07 ms | 0.11 ms | 0.29 ms |

**Observation:** Scales BETTER with more data (query optimizer kicks in)

---

#### Filtered SELECT (FILTER clause)

| Dataset | Mean | Median | P95 | P99 |
|---------|------|--------|-----|-----|
| 100 entities | 0.16 ms | 0.14 ms | 0.33 ms | 0.51 ms |
| 1,000 entities | 0.11 ms | 0.10 ms | 0.14 ms | 0.34 ms |
| 10,000 entities | 0.10 ms | 0.09 ms | 0.13 ms | 0.35 ms |

**Observation:** Filter adds ~0.03ms overhead, consistent across sizes

---

#### JOIN (2-way join)

| Dataset | Mean | Median | P95 | P99 |
|---------|------|--------|-----|-----|
| 100 entities | 0.17 ms | 0.16 ms | 0.22 ms | 0.34 ms |
| 1,000 entities | 0.13 ms | 0.11 ms | 0.17 ms | 0.36 ms |
| 10,000 entities | 0.14 ms | 0.12 ms | 0.30 ms | 0.36 ms |

**Observation:** Join performance remains constant (query planner effective)

---

#### AGGREGATE (GROUP BY + COUNT)

| Dataset | Mean | Median | P95 | P99 |
|---------|------|--------|-----|-----|
| 100 entities | 0.25 ms | 0.21 ms | 0.44 ms | 1.13 ms |
| 1,000 entities | 0.84 ms | 0.77 ms | 1.30 ms | 1.32 ms |
| 10,000 entities | 5.62 ms | 5.52 ms | 6.65 ms | 6.86 ms |

**Observation:** Aggregate scales linearly with dataset size (expected for GROUP BY)

---

### SPARQL Analysis

- **Sub-millisecond Latency:** Simple queries complete in <0.1ms
- **Scalability:** Linear for aggregates, constant for simple/join queries
- **Production Readiness:** ✅ Yes - can handle 1,000s of queries/sec
- **Bottleneck:** Aggregate queries on large datasets (but still <7ms at 10K entities)

---

## Verification Commands

### Run All Benchmarks

```bash
# Receipt generation (1,000 iterations)
node /home/user/unrdf/benchmarks/receipt-generation-bench.mjs

# SPARQL queries (3 dataset sizes x 4 query types x 50 iterations)
node /home/user/unrdf/benchmarks/sparql-query-bench.mjs

# Freeze operations (100 iterations with Git commits)
node /home/user/unrdf/benchmarks/kgc-4d-freeze-bench.mjs
```

### Benchmark Output Files

- `/tmp/receipt-bench-output.txt` - Receipt generation results
- `/tmp/sparql-bench-output.txt` - SPARQL query results
- `/tmp/freeze-bench-output.txt` - Freeze operation results

---

## Adversarial PM Questions

### ❓ Did I RUN the benchmarks or just estimate?
**Answer:** RAN - All benchmarks executed with warmup + iterations, output captured

### ❓ Can these results be REPRODUCED?
**Answer:** YES - Commands provided above, benchmarks are deterministic (± 10% for I/O variance)

### ❓ What BREAKS if performance degrades?
**Answer:**
- Receipt generation >10ms → Cannot sustain high-throughput workflows
- Freeze >100ms → Snapshot overhead becomes unacceptable
- SPARQL >50ms → Real-time queries become laggy

### ❓ What's the EVIDENCE?
**Answer:**
```bash
$ grep "P95:" /tmp/receipt-bench-output.txt
P95:       0.593 ms

$ grep "Mean:" /tmp/freeze-bench-output.txt | head -1
Mean:   20.68 ms

$ grep "AGGREGATE" /tmp/sparql-bench-output.txt | grep "Mean"
  Mean:   0.25 ms
  Mean:   0.84 ms
  Mean:   5.62 ms
```

---

## Performance Bottlenecks Identified

### 1. Freeze Operation (20ms mean)
- **Root Cause:** Git commit via isomorphic-git
- **Recommendation:** Consider batch freezes (snapshot every N events, not every event)
- **Impact:** Medium - snapshots are infrequent in production

### 2. Aggregate Queries on Large Datasets (5.62ms at 10K)
- **Root Cause:** GROUP BY requires full scan
- **Recommendation:** Add indexing or materialized views for common aggregates
- **Impact:** Low - still <7ms, acceptable for most use cases

### 3. Receipt Generation Outliers (P99.9 at 5ms)
- **Root Cause:** Likely GC pauses
- **Recommendation:** Tune Node.js GC settings for production
- **Impact:** Low - still 2x under target

---

## Files Generated

- `/home/user/unrdf/benchmarks/receipt-generation-bench.mjs` - Receipt benchmark source
- `/home/user/unrdf/benchmarks/sparql-query-bench.mjs` - SPARQL benchmark source
- `/home/user/unrdf/benchmarks/kgc-4d-freeze-bench.mjs` - Freeze benchmark source
- `/tmp/receipt-bench-output.txt` - Receipt benchmark output
- `/tmp/sparql-bench-output.txt` - SPARQL benchmark output
- `/tmp/freeze-bench-output.txt` - Freeze benchmark output

**Next Steps:** See PERFORMANCE-VALIDATION.md for comparison of claims vs measured reality.
