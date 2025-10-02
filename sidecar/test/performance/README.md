# UNRDF Performance & Security Test Suite

Comprehensive performance benchmarks, security validation, and chaos engineering tests for Fortune 5 SLA compliance.

## 🎯 Performance Targets

| Metric | Target | Test Suite |
|--------|--------|------------|
| Transaction Throughput | >1,000 tx/sec | `transaction-throughput.test.mjs` |
| P99 Latency | <100ms | `transaction-throughput.test.mjs` |
| Hook Execution | <2ms p99 | `hook-latency.test.mjs` |
| SPARQL Simple Queries | <50ms | `sparql-performance.test.mjs` |
| SPARQL Complex Queries | <500ms | `sparql-performance.test.mjs` |
| Sustained Load | 10k tx/sec for 1 hour | `scripts/load-test.mjs` |
| Spike Handling | 50k tx/sec burst | `scripts/load-test.mjs` |

## 📋 Test Suites

### Performance Tests (`test/performance/`)

#### Transaction Throughput (`transaction-throughput.test.mjs`)
- **Sustained Load**: 1000+ tx/sec with <100ms p99 latency
- **Concurrent Users**: 50 simultaneous users with transaction isolation
- **Load Patterns**: Sustained, spike, and ramp-up scenarios
- **Error Rate**: <1% under all load conditions

```bash
pnpm test test/performance/transaction-throughput.test.mjs
```

#### Hook Latency (`hook-latency.test.mjs`)
- **Validation Hooks**: <2ms p99 latency (10,000 iterations)
- **SHACL Validation**: <4ms p99 latency
- **Cryptographic Provenance**: <2ms p99 latency
- **Policy Enforcement**: <2ms p99 latency
- **Memory Profiling**: <10MB growth for 10k iterations

```bash
pnpm test test/performance/hook-latency.test.mjs
```

#### SPARQL Performance (`sparql-performance.test.mjs`)
- **Simple Queries**: <50ms (SELECT with LIMIT)
- **Filtered Queries**: <100ms (with FILTER clauses)
- **JOIN Operations**: <150ms (multi-pattern)
- **Complex Queries**: <500ms (OPTIONAL, aggregation)
- **Index Optimization**: Validates SPO index usage

```bash
pnpm test test/performance/sparql-performance.test.mjs
```

### Security Tests (`test/security/`)

#### OWASP Top 10 (`owasp-top10.test.mjs`)

Validates protection against all OWASP Top 10 vulnerabilities:

1. **A01:2021 - Broken Access Control**
   - Policy-based access control
   - Privilege escalation prevention
   - Resource ownership validation

2. **A02:2021 - Cryptographic Failures**
   - Strong signature algorithms (Ed25519, ECDSA-P256)
   - No sensitive data in logs
   - Secure provenance validation

3. **A03:2021 - Injection**
   - SPARQL injection prevention
   - Parameterized queries
   - Input sanitization

4. **A05:2021 - Security Misconfiguration**
   - Secure defaults
   - Production hardening
   - HTTPS enforcement

5. **A07:2021 - Authentication Failures**
   - Rate limiting (5 attempts max)
   - Secure session management
   - Session timeout enforcement

6. **A09:2021 - Security Logging**
   - Security event logging
   - Correlation IDs
   - Audit trails

7. **A10:2021 - SSRF Prevention**
   - External URL validation
   - Domain whitelisting
   - Internal network protection

```bash
pnpm test test/security/owasp-top10.test.mjs
```

### Chaos Engineering (`test/chaos/`)

#### Network Partition (`network-partition.test.mjs`)

Validates system resilience under adverse conditions:

- **Circuit Breakers**: Opens after 5 failures, half-opens after timeout
- **Retry Policies**: Exponential backoff with jitter
- **Network Failures**: Handles 30% failure rate gracefully
- **Timeouts**: 1-second timeout enforcement
- **Graceful Degradation**: Fallback to cached data
- **Resource Management**: Connection pooling with limits
- **Request Queuing**: Max 50 queue size, 5 concurrent
- **Load Shedding**: Disables non-critical features at 90% capacity
- **Priority Queues**: Critical requests processed first

```bash
pnpm test test/chaos/network-partition.test.mjs
```

## 🚀 Load Testing

### k6 Load Test Script (`scripts/load-test.mjs`)

Comprehensive load testing with 4 scenarios:

#### 1. Sustained Load Test
- **Duration**: 1 hour
- **Rate**: 10,000 tx/sec
- **VUs**: 500-1000
- **Validates**: Long-term stability

#### 2. Spike Test
- **Duration**: 2 minutes
- **Peak**: 50,000 tx/sec burst
- **VUs**: Up to 5000
- **Validates**: Burst handling

#### 3. Ramp-up Test
- **Duration**: 30 minutes
- **Progressive**: 100 → 1000 VUs
- **Validates**: Scaling behavior

#### 4. Stress Test
- **Duration**: 19 minutes
- **Progressive**: 1k → 30k tx/sec
- **Validates**: Breaking point

### Running Load Tests

```bash
# Install k6 (macOS)
brew install k6

# Run load test
k6 run sidecar/scripts/load-test.mjs

# With custom base URL
k6 run -e BASE_URL=https://production.example.com sidecar/scripts/load-test.mjs

# Run specific scenario
k6 run --scenarios sustained_load sidecar/scripts/load-test.mjs
```

### Load Test Outputs

- **Console**: Real-time metrics
- **JSON**: `/tmp/unrdf-load-test.json`
- **HTML Report**: `/tmp/unrdf-load-test.html`
- **Grafana**: Integration via StatsD/InfluxDB

## 📊 Performance Profiling

### Using Clinic.js

```bash
# Doctor - Overall health diagnosis
clinic doctor -- node sidecar/server/index.mjs

# Flame - CPU profiling
clinic flame -- node sidecar/server/index.mjs

# Bubbleprof - Async operations
clinic bubbleprof -- node sidecar/server/index.mjs

# Heap Profiler
clinic heapprofiler -- node sidecar/server/index.mjs
```

### Using 0x (Flamegraphs)

```bash
# Generate flamegraph
0x -- node sidecar/server/index.mjs

# With load generation
0x --output-dir ./profiles -- node sidecar/server/index.mjs
```

### Using autocannon (Quick benchmarks)

```bash
# Simple benchmark
autocannon -c 100 -d 30 http://localhost:3456/api/transaction

# With POST body
autocannon -c 100 -d 30 -m POST \
  -H "Content-Type: application/json" \
  -b '{"operation":"query","query":"SELECT ?s WHERE { ?s ?p ?o } LIMIT 10"}' \
  http://localhost:3456/api/transaction
```

## 🎯 CI/CD Integration

### GitHub Actions

```yaml
name: Performance Tests

on: [push, pull_request]

jobs:
  performance:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '20'

      - run: pnpm install
      - run: pnpm test:performance

      # Load testing
      - uses: grafana/k6-action@v0.3.0
        with:
          filename: sidecar/scripts/load-test.mjs
          flags: --out json=results.json

      # Fail if performance targets not met
      - run: pnpm test:validate-sla
```

### SLA Validation

```bash
# Run all performance tests
pnpm test test/performance/

# Run security tests
pnpm test test/security/

# Run chaos tests
pnpm test test/chaos/

# Run everything
pnpm test
```

## 📈 Monitoring & Observability

### Metrics Collection

All tests emit OpenTelemetry metrics:

- **Throughput**: Transactions per second
- **Latency**: p50, p95, p99, p99.9 percentiles
- **Error Rate**: Percentage of failed requests
- **Resource Usage**: CPU, memory, network I/O
- **Queue Depth**: Pending operations

### Dashboard Integration

Metrics available in:
- **Grafana**: Real-time dashboards
- **Prometheus**: Metric storage
- **Jaeger**: Distributed tracing
- **Elasticsearch**: Log aggregation

## 🔧 Troubleshooting

### Common Issues

**High latency in tests:**
```bash
# Check for resource contention
clinic doctor -- node sidecar/server/index.mjs

# Profile CPU usage
0x -- node sidecar/server/index.mjs
```

**Memory leaks:**
```bash
# Heap profiling
clinic heapprofiler -- node sidecar/server/index.mjs

# Check for unclosed resources
node --trace-warnings sidecar/server/index.mjs
```

**Connection errors:**
```bash
# Increase file descriptor limit
ulimit -n 65536

# Check port availability
lsof -i :3456
```

## 📚 References

- [k6 Documentation](https://k6.io/docs/)
- [Clinic.js](https://clinicjs.org/)
- [autocannon](https://github.com/mcollina/autocannon)
- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [Chaos Engineering Principles](https://principlesofchaos.org/)
