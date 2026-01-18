# UNRDF Load Testing Suite

Comprehensive load testing infrastructure for performance validation and production readiness assessment.

## Overview

This suite implements 5 critical load test scenarios:

1. **Baseline Load** - 100 concurrent users, ~1000 req/min
2. **Peak Load** - 1000 concurrent users, ~10000 req/min
3. **Sustained Load** - 500 concurrent users for 30 minutes
4. **Spike Test** - 0→5000 users in 10 seconds
5. **Soak Test** - 100 users for 2 hours (memory leak detection)

## Quick Start

### Run Quick Tests (1 minute each)

```bash
# Run all tests in quick mode
node test/load-testing/run-all-load-tests.mjs --quick

# Analyze results
node test/load-testing/analyze-results.mjs
```

### Run Standard Tests

```bash
# Run with standard durations
node test/load-testing/run-all-load-tests.mjs

# Sustained: 1 minute, Soak: 1 minute
```

### Run Full Tests (Production)

```bash
# Run with full production durations
node test/load-testing/run-all-load-tests.mjs --full

# Sustained: 30 minutes, Soak: 2 hours
```

## Test Scenarios

### 1. Baseline Load Test

**Purpose:** Establish performance baseline under normal conditions

**Configuration:**
- Connections: 100
- Duration: 60 seconds
- Target: ~1000 requests/minute

**Pass Criteria:**
- P95 Latency < 100ms
- P99 Latency < 500ms
- Error Rate < 0.1%
- Throughput > 10 req/s

### 2. Peak Load Test

**Purpose:** Validate system behavior under peak traffic

**Configuration:**
- Connections: 1000
- Duration: 60 seconds
- Target: ~10000 requests/minute

**Pass Criteria:**
- P95 Latency < 200ms
- P99 Latency < 1000ms
- Error Rate < 0.1%
- Throughput > 100 req/s

### 3. Sustained Load Test

**Purpose:** Detect performance degradation and memory leaks

**Configuration:**
- Connections: 500
- Duration: 30 minutes (configurable)
- Mixed endpoint requests

**Pass Criteria:**
- P95 Latency < 150ms
- P99 Latency < 750ms
- Error Rate < 0.1%
- Throughput > 50 req/s
- Memory Growth < 1MB/min

### 4. Spike Test

**Purpose:** Test resilience under sudden traffic spikes

**Configuration:**
- Phase 1: Warm-up (5s, 10 connections)
- Phase 2: Spike (10s, 5000 connections)
- Phase 3: Recovery (15s, 100 connections)

**Pass Criteria:**
- Error Rate during spike < 1%
- Latency degradation < 500%
- Recovery to baseline within 15s

### 5. Soak Test

**Purpose:** Long-running stability test for memory leaks

**Configuration:**
- Connections: 100
- Duration: 2 hours (configurable)
- Continuous mixed requests

**Pass Criteria:**
- P95 Latency < 100ms
- P99 Latency < 500ms
- Error Rate < 0.1%
- No memory leaks detected
- CPU < 80% average

## Metrics Collected

### Latency Metrics
- P50, P75, P90, P95, P99, P99.9 percentiles
- Mean, min, max, standard deviation

### Throughput Metrics
- Requests per second
- Total requests
- Min, max, mean throughput

### Error Metrics
- Total error count
- Error rate percentage
- Status code distribution

### Resource Metrics
- Memory usage (RSS, heap)
- CPU usage percentage
- Memory growth rate
- Memory leak detection

## Architecture

```
test/load-testing/
├── test-server.mjs              # Test HTTP server
├── 01-baseline-load.mjs         # Baseline load test
├── 02-peak-load.mjs             # Peak load test
├── 03-sustained-load.mjs        # Sustained load test
├── 04-spike-test.mjs            # Spike test
├── 05-soak-test.mjs             # Soak test
├── run-all-load-tests.mjs       # Test orchestrator
├── analyze-results.mjs          # Results analyzer
└── results/                     # Test results (generated)
    ├── load-test-results-*.json
    ├── load-test-results-*-analysis.json
    └── load-test-results-*-report.md
```

## Test Server Endpoints

The test server exposes the following endpoints:

- `GET /health` - Health check
- `GET /api/receipt` - Create receipt (fast operation)
- `GET /api/triples` - Return test triples
- `GET /api/query?q=<query>` - SPARQL query simulation
- `POST /api/insert` - Insert triple
- `GET /api/stress` - CPU-intensive operation
- `GET /metrics` - Server metrics

## Running Individual Tests

```bash
# Start test server
node test/load-testing/test-server.mjs &

# Run individual tests
node test/load-testing/01-baseline-load.mjs
node test/load-testing/02-peak-load.mjs
node test/load-testing/03-sustained-load.mjs 60  # 60 seconds
node test/load-testing/04-spike-test.mjs
node test/load-testing/05-soak-test.mjs 60       # 60 seconds

# Stop test server
kill %1
```

## Output Files

### Results JSON
```json
{
  "startTime": "2026-01-11T...",
  "config": { ... },
  "tests": {
    "baseline": {
      "latency": { "p95": 45.2, "p99": 67.8, ... },
      "throughput": { "requestsPerSecond": 125.4, ... },
      "errors": { "errorRate": 0.0001, ... },
      "passed": { "latencyP95": true, ... }
    },
    ...
  },
  "endTime": "2026-01-11T...",
  "totalDuration": 300000
}
```

### Analysis JSON
```json
{
  "overallStatus": "PASS",
  "summary": { ... },
  "bottlenecks": [
    {
      "type": "HIGH_LATENCY_P99",
      "severity": "HIGH",
      "description": "...",
      "value": 567.8,
      "threshold": 500
    }
  ],
  "recommendations": [
    {
      "priority": "HIGH",
      "category": "PERFORMANCE",
      "title": "Optimize Request Processing",
      "actions": [ ... ]
    }
  ]
}
```

### Markdown Report

See generated `-report.md` files for human-readable analysis with:
- Executive summary
- Detailed test results
- Bottleneck identification
- Prioritized recommendations
- Production readiness assessment

## Performance Targets

| Metric | Baseline | Peak | Sustained | Spike | Soak |
|--------|----------|------|-----------|-------|------|
| P95 Latency | <100ms | <200ms | <150ms | <500ms¹ | <100ms |
| P99 Latency | <500ms | <1000ms | <750ms | <2000ms¹ | <500ms |
| Error Rate | <0.1% | <0.1% | <0.1% | <1%² | <0.1% |
| Throughput | >10/s | >100/s | >50/s | N/A | >10/s |
| Memory | N/A | N/A | <1MB/min growth | N/A | No leaks |
| CPU | N/A | N/A | N/A | N/A | <80% avg |

¹ During spike phase only
² Spike phase only; recovery must return to <0.1%

## CI/CD Integration

Add to CI pipeline:

```yaml
- name: Run Load Tests
  run: |
    node test/load-testing/run-all-load-tests.mjs --quick
    node test/load-testing/analyze-results.mjs

- name: Upload Results
  uses: actions/upload-artifact@v3
  with:
    name: load-test-results
    path: test/load-testing/results/
```

## Troubleshooting

### Test Server Won't Start

```bash
# Check if port is in use
lsof -i :3000

# Kill existing process
kill -9 $(lsof -ti :3000)
```

### High Error Rates

- Check server logs
- Increase system resource limits
- Verify network configuration
- Review timeout settings

### Memory Issues

- Enable garbage collection: `node --expose-gc`
- Increase heap size: `node --max-old-space-size=4096`
- Profile with heap snapshots

## References

- [autocannon Documentation](https://github.com/mcollina/autocannon)
- [Load Testing Best Practices](https://www.loadimpact.com/load-testing)
- UNRDF Performance Targets: `/benchmarks/V6-PERFORMANCE-TARGETS.md`
