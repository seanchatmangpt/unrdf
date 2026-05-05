# UNRDF v6 Performance Benchmark Report

**Generated**: 2025-12-27T11:05:latestZ

**Node Version**: vlatest

**Platform**: linux x64

**Cryptographic Hash**: SHA-256 (Node.js crypto module)


## Core Operations

| Operation | Median | P95 | P99 | Throughput | Target | Status |
|-----------|--------|-----|-----|------------|--------|--------|
| Receipt Creation | latestms | latestms | latestms | 83895/s | <1ms | ✓ PASS |
| Delta Validation | latestms | latestms | latestms | 211311/s | <5ms | ✓ PASS |
| Receipt Verification | latestms | latestms | latestms | 4573038/s | <latestms | ✓ PASS |
| Receipt Chain (10) | latestms | latestms | latestms | 6088/s | <50ms | ✓ PASS |
| Chain Verification (10) | latestms | latestms | latestms | 594421/s | <20ms | ✓ PASS |

## Memory Profiling

| Test | Result |
|------|--------|
| 1,000 receipts heap | +latest KB |
| Avg per receipt | +latest B |
| Memory leak detected | ✓ NO |
| Memory growth | latest% |
| Stress test (10k receipts) | +latest MB |
| Stress test throughput | 71655 receipts/sec |

## Scalability Analysis


### Receipt Chain Verification

| Chain Length | Median | P95 |
|--------------|--------|-----|
| 1 | latestms | latestms |
| 10 | latestms | latestms |
| 50 | latestms | latestms |
| 100 | latestms | latestms |
| 500 | latestms | latestms |
| 1000 | latestms | latestms |

**Scaling behavior**: linear

- 1→10 ratio: latestx
- 10→100 ratio: latestx


## Performance Claims Validation


### v6 Performance Targets vs Actual

| Claim | Target | Actual (P95) | Status | Note |
|-------|--------|--------------|--------|------|
| Receipt Creation | <1ms | latestms | ✓ PASS | -latest% faster |
| Delta Validation | <5ms | latestms | ✓ PASS | -latest% faster |
| Receipt Verification | <latestms | latestms | ✓ PASS | -latest% faster |
| Receipt Chain (10) | <50ms | latestms | ✓ PASS | -latest% faster |
| Chain Verification (10) | <20ms | latestms | ✓ PASS | -latest% faster |

## Interpretation

- **PASS**: Operation meets performance target (up to 10% slower acceptable)
- **FAIL**: Operation >10% slower than target (requires optimization)
- **Memory leak**: >10% heap growth after 10,000 iterations
- **Scalability**: Preference order: logarithmic > linear > exponential

### Key Findings

- **Pass rate**: 5/5 (latest%)
- **Memory efficiency**: +latest B/receipt
- **Memory stability**: Stable ✓
- **Chain verification scaling**: linear