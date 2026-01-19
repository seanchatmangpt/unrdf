# UNRDF v6 Performance Benchmark Report

**Generated**: 2026-01-19T07:29:49.898Z

**Node Version**: v22.21.1

**Platform**: linux x64

**Cryptographic Hash**: SHA-256 (Node.js crypto module)


## Core Operations

| Operation | Median | P95 | P99 | Throughput | Target | Status |
|-----------|--------|-----|-----|------------|--------|--------|
| Receipt Creation | 0.008ms | 0.013ms | 0.030ms | 86985/s | <1ms | ✓ PASS |
| Delta Validation | 0.003ms | 0.005ms | 0.029ms | 232483/s | <5ms | ✓ PASS |
| Receipt Verification | 0.000ms | 0.000ms | 0.000ms | 6095925/s | <0.5ms | ✓ PASS |
| Receipt Chain (10) | 0.112ms | 0.335ms | 0.881ms | 6207/s | <50ms | ✓ PASS |
| Chain Verification (10) | 0.001ms | 0.002ms | 0.006ms | 717726/s | <20ms | ✓ PASS |

## Memory Profiling

| Test | Result |
|------|--------|
| 1,000 receipts heap | +1.40 MB |
| Avg per receipt | +1.43 KB |
| Memory leak detected | ✓ NO |
| Memory growth | -10.57% |
| Stress test (10k receipts) | +10.75 MB |
| Stress test throughput | 89878 receipts/sec |

## Scalability Analysis


### Receipt Chain Verification

| Chain Length | Median | P95 |
|--------------|--------|-----|
| 1 | 0.001ms | 0.005ms |
| 10 | 0.001ms | 0.003ms |
| 50 | 0.003ms | 0.006ms |
| 100 | 0.006ms | 0.011ms |
| 500 | 0.026ms | 0.080ms |
| 1000 | 0.005ms | 0.028ms |

**Scaling behavior**: linear

- 1→10 ratio: 1.94x
- 10→100 ratio: 3.72x


## Performance Claims Validation


### v6 Performance Targets vs Actual

| Claim | Target | Actual (P95) | Status | Note |
|-------|--------|--------------|--------|------|
| Receipt Creation | <1ms | 0.013ms | ✓ PASS | -98.7% faster |
| Delta Validation | <5ms | 0.005ms | ✓ PASS | -99.9% faster |
| Receipt Verification | <0.5ms | 0.000ms | ✓ PASS | -99.9% faster |
| Receipt Chain (10) | <50ms | 0.335ms | ✓ PASS | -99.3% faster |
| Chain Verification (10) | <20ms | 0.002ms | ✓ PASS | -100.0% faster |

## Interpretation

- **PASS**: Operation meets performance target (up to 10% slower acceptable)
- **FAIL**: Operation >10% slower than target (requires optimization)
- **Memory leak**: >10% heap growth after 10,000 iterations
- **Scalability**: Preference order: logarithmic > linear > exponential

### Key Findings

- **Pass rate**: 5/5 (100.0%)
- **Memory efficiency**: +1.43 KB/receipt
- **Memory stability**: Stable ✓
- **Chain verification scaling**: linear