# UNRDF v6 Performance Benchmark Report

**Generated**: 2025-12-27T11:05:39.145Z

**Node Version**: v22.21.1

**Platform**: linux x64

**Cryptographic Hash**: SHA-256 (Node.js crypto module)


## Core Operations

| Operation | Median | P95 | P99 | Throughput | Target | Status |
|-----------|--------|-----|-----|------------|--------|--------|
| Receipt Creation | 0.009ms | 0.017ms | 0.030ms | 83895/s | <1ms | ✓ PASS |
| Delta Validation | 0.003ms | 0.005ms | 0.037ms | 211311/s | <5ms | ✓ PASS |
| Receipt Verification | 0.000ms | 0.000ms | 0.001ms | 4573038/s | <0.5ms | ✓ PASS |
| Receipt Chain (10) | 0.122ms | 0.347ms | 0.590ms | 6088/s | <50ms | ✓ PASS |
| Chain Verification (10) | 0.001ms | 0.002ms | 0.007ms | 594421/s | <20ms | ✓ PASS |

## Memory Profiling

| Test | Result |
|------|--------|
| 1,000 receipts heap | +819.41 KB |
| Avg per receipt | +839.07 B |
| Memory leak detected | ✓ NO |
| Memory growth | 0.31% |
| Stress test (10k receipts) | +11.39 MB |
| Stress test throughput | 71655 receipts/sec |

## Scalability Analysis


### Receipt Chain Verification

| Chain Length | Median | P95 |
|--------------|--------|-----|
| 1 | 0.001ms | 0.007ms |
| 10 | 0.002ms | 0.007ms |
| 50 | 0.003ms | 0.006ms |
| 100 | 0.004ms | 0.010ms |
| 500 | 0.020ms | 0.071ms |
| 1000 | 0.006ms | 0.043ms |

**Scaling behavior**: linear

- 1→10 ratio: 2.68x
- 10→100 ratio: 2.51x


## Performance Claims Validation


### v6 Performance Targets vs Actual

| Claim | Target | Actual (P95) | Status | Note |
|-------|--------|--------------|--------|------|
| Receipt Creation | <1ms | 0.017ms | ✓ PASS | -98.3% faster |
| Delta Validation | <5ms | 0.005ms | ✓ PASS | -99.9% faster |
| Receipt Verification | <0.5ms | 0.000ms | ✓ PASS | -99.9% faster |
| Receipt Chain (10) | <50ms | 0.347ms | ✓ PASS | -99.3% faster |
| Chain Verification (10) | <20ms | 0.002ms | ✓ PASS | -100.0% faster |

## Interpretation

- **PASS**: Operation meets performance target (up to 10% slower acceptable)
- **FAIL**: Operation >10% slower than target (requires optimization)
- **Memory leak**: >10% heap growth after 10,000 iterations
- **Scalability**: Preference order: logarithmic > linear > exponential

### Key Findings

- **Pass rate**: 5/5 (100.0%)
- **Memory efficiency**: +839.07 B/receipt
- **Memory stability**: Stable ✓
- **Chain verification scaling**: linear