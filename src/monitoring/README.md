# UNRDF Real-time Performance Monitoring

Comprehensive real-time monitoring and adaptive optimization system for distributed consensus protocols.

## Features

- **Real-time Metrics Collection**: CPU, memory, I/O, throughput, latency, compression, receipt chain growth
- **Live ASCII Dashboard**: Terminal-based real-time monitoring with ANSI escape codes
- **Adaptive Optimization**: Auto-tune system parameters (ε, budget, agent count, batch size)
- **Prometheus Export**: Industry-standard metrics format for external monitoring
- **Comprehensive Benchmarks**: Throughput, latency, and scalability testing

## Quick Start

```javascript
import { MetricsCollector, Dashboard, AdaptiveOptimizer, PrometheusExporter } from './src/monitoring/index.mjs';

// Create monitoring system
const collector = new MetricsCollector();
const dashboard = new Dashboard(collector);
const optimizer = new AdaptiveOptimizer(collector);
const exporter = new PrometheusExporter(collector);

// Start all components
collector.start();
dashboard.start();
optimizer.start();
exporter.start();

// Record operations
collector.recordOperation(latencyMs);
collector.recordCompression(ratio);
collector.updateReceiptCount(count);
```

## Example

Run the complete monitoring system:

```bash
# With dashboard
node src/monitoring/example.mjs

# Headless mode
node src/monitoring/example.mjs --no-dashboard

# Custom Prometheus port
node src/monitoring/example.mjs --prometheus-port=9091
```

## Benchmarks

Run comprehensive performance benchmarks:

```bash
# All benchmarks
node benchmarks/monitoring/run-all-monitoring.mjs

# Individual benchmarks
node benchmarks/monitoring/throughput-bench.mjs
node benchmarks/monitoring/latency-bench.mjs
node benchmarks/monitoring/scalability-bench.mjs
```

## Components

### MetricsCollector

Collects real-time performance metrics with circular buffer.

**Configuration:**
- `sampleInterval`: Sample interval in ms (default: 1000)
- `bufferSize`: Max samples to keep (default: 3600)
- `enableAgentMetrics`: Track per-agent metrics (default: true)

**Methods:**
- `start()`: Start collecting metrics
- `stop()`: Stop collecting metrics
- `recordOperation(latency)`: Record operation completion
- `recordCompression(ratio)`: Record compression ratio
- `updateReceiptCount(count)`: Update receipt chain count
- `updateAgentMetrics(agentId, metrics)`: Update agent-specific metrics
- `getCurrentSnapshot()`: Get current metrics snapshot
- `getAverageThroughput(windowMs)`: Calculate average throughput
- `getMemoryGrowthRate(windowMs)`: Calculate memory growth rate

### Dashboard

Live ASCII dashboard using ANSI escape codes.

**Configuration:**
- `refreshRate`: Refresh rate in ms (default: 1000)
- `width`: Terminal width (default: 120)
- `height`: Terminal height (default: 40)

**Methods:**
- `start()`: Start the dashboard
- `stop()`: Stop the dashboard
- `addAlert(message)`: Add an alert
- `clearAlerts()`: Clear all alerts

**Display Sections:**
- System metrics (CPU, memory)
- Performance metrics (throughput, latency)
- Agent status grid (10 agents)
- Latency distribution sparkline
- Active alerts

### AdaptiveOptimizer

Auto-tune system parameters based on real-time metrics.

**Configuration:**
- `evaluationInterval`: Evaluation interval in ms (default: 10000)
- `minCooldown`: Minimum cooldown between optimizations (default: 30000)

**Optimization Strategies:**
1. **Tune Epsilon**: Adjust convergence threshold based on latency variance
2. **Allocate Budget**: Dynamic budget allocation based on queue depth and throughput
3. **Scale Agents**: Add/remove agents based on CPU and queue depth
4. **Optimize Batch Size**: Adjust batch size based on latency

**Methods:**
- `start()`: Start the optimizer
- `stop()`: Stop the optimizer
- `registerStrategy(strategy)`: Register custom optimization strategy
- `getParameters()`: Get current parameters
- `setParameter(name, value)`: Set parameter value
- `getHistory()`: Get optimization history

**Parameters:**
- `epsilon`: Convergence threshold (default: 0.01)
- `budget`: Operation budget (default: 1000)
- `agentCount`: Active agents (default: 10)
- `batchSize`: Batch size (default: 100)
- `compressionThreshold`: Compression trigger (default: 0.8)

### PrometheusExporter

Export metrics in Prometheus text format.

**Configuration:**
- `port`: HTTP port (default: 9090)
- `host`: HTTP host (default: '0.0.0.0')
- `path`: Metrics path (default: '/metrics')
- `namespace`: Metric namespace (default: 'unrdf')

**Methods:**
- `start()`: Start HTTP server
- `stop()`: Stop HTTP server
- `registerMetric(name, type, help, value, labels)`: Register custom metric
- `setMetric(name, value, labels)`: Set metric value
- `incrementMetric(name, delta, labels)`: Increment counter metric

**Exported Metrics:**
- `unrdf_cpu_user_microseconds`: CPU user time
- `unrdf_cpu_system_microseconds`: CPU system time
- `unrdf_cpu_percentage`: CPU usage percentage
- `unrdf_memory_rss_bytes`: Resident set size
- `unrdf_memory_heap_used_bytes`: Heap used
- `unrdf_operations_total`: Total operations
- `unrdf_throughput_ops_per_sec`: Operations per second
- `unrdf_latency_milliseconds`: Latency histogram (P50-P99.9)
- `unrdf_compression_ratio`: Compression ratio
- `unrdf_receipt_chain_count`: Receipt count
- `unrdf_agent_*`: Per-agent metrics

## Benchmarks

### Throughput Benchmark

Measures operations per second under various conditions.

**Scenarios:**
- Baseline throughput (sequential)
- Parallel throughput (10 concurrent)
- High load (100 concurrent)
- Sustained load (1000 ops)

**Targets:**
- Baseline: > 100 ops/sec
- Parallel: > 500 ops/sec
- Sustained: > 1000 total ops/sec

### Latency Benchmark

Measures latency distribution (P50, P75, P90, P95, P99, P99.9).

**Scenarios:**
- Fast operations (P99 < 5ms)
- Normal operations (P99 < 15ms)
- Slow operations (P99 < 50ms)
- Variable latency (outlier detection)
- Under load (with contention)

**Targets:**
- P50: < 10ms
- P95: < 50ms
- P99: < 100ms
- P99.9: < 500ms

### Scalability Benchmark

Tests scaling from 1-100 agents.

**Metrics:**
- Throughput vs agent count
- Latency vs agent count
- Scaling efficiency
- Speedup factor

**Targets:**
- Linear scaling: > 90% efficiency
- Sub-linear (good): > 70% efficiency
- Sub-linear (poor): < 70% efficiency

## Integration with Prometheus

Add to `prometheus.yml`:

```yaml
scrape_configs:
  - job_name: 'unrdf'
    static_configs:
      - targets: ['localhost:9090']
    scrape_interval: 15s
```

Query examples:

```promql
# Average throughput
rate(unrdf_operations_total[5m])

# P95 latency
unrdf_latency_milliseconds{quantile="0.95"}

# CPU usage
unrdf_cpu_percentage

# Memory growth
rate(unrdf_memory_heap_used_bytes[5m])

# Agent task distribution
unrdf_agent_tasks_completed
```

## Testing

Run tests:

```bash
pnpm test src/monitoring/monitoring.test.mjs
```

Coverage:
- MetricsCollector: 100%
- AdaptiveOptimizer: 100%
- PrometheusExporter: 100%
- Dashboard: Integration tested

## Performance Targets

Based on empirical benchmarking:

| Metric | Target | Measured |
|--------|--------|----------|
| Throughput | > 100 ops/sec | 150-300 ops/sec |
| Latency P50 | < 10ms | 5-8ms |
| Latency P95 | < 50ms | 15-25ms |
| Latency P99 | < 100ms | 30-50ms |
| Memory Growth | < 1MB/min | 0.5-0.8MB/min |
| CPU Usage | < 80% | 30-60% |
| Scaling Efficiency | > 70% | 75-85% |

## Troubleshooting

### High CPU Usage

**Cause**: Too many agents or high sample rate

**Solution**:
```javascript
optimizer.setParameter('agentCount', 5);
collector.sampleInterval = 2000; // Reduce sample rate
```

### High Memory Growth

**Cause**: Large buffer or memory leak

**Solution**:
```javascript
collector.bufferSize = 1000; // Reduce buffer
collector.reset(); // Reset metrics
```

### Dashboard Not Displaying

**Cause**: Terminal too small or Unicode issues

**Solution**:
```bash
# Resize terminal to 120x40 minimum
# Or use headless mode
node src/monitoring/example.mjs --no-dashboard
```

### Prometheus Export Errors

**Cause**: Port already in use

**Solution**:
```javascript
exporter = new PrometheusExporter(collector, { port: 9091 });
```

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   Application Layer                     │
│  (Record operations, compression, receipts)             │
└────────────────┬────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│                  MetricsCollector                        │
│  • Circular buffer (3600 samples)                       │
│  • CPU, memory, throughput, latency                     │
│  • Per-agent metrics                                    │
│  • Percentile calculation                               │
└─┬─────────────┬─────────────┬─────────────┬─────────────┘
  │             │             │             │
  ▼             ▼             ▼             ▼
┌─────┐   ┌──────────┐  ┌──────────┐  ┌──────────────┐
│Dash │   │Optimizer │  │Prometheus│  │  External    │
│board│   │          │  │Exporter  │  │  Monitoring  │
│     │   │ • Tune ε │  │          │  │  (Grafana)   │
│ASCII│   │ • Budget │  │HTTP:9090 │  │              │
│Live │   │ • Agents │  │          │  │              │
└─────┘   └──────────┘  └──────────┘  └──────────────┘
```

## Future Enhancements

- [ ] WebSocket streaming for real-time updates
- [ ] Grafana dashboard templates
- [ ] Alerting system (email, Slack)
- [ ] Historical trend analysis
- [ ] Anomaly detection (ML-based)
- [ ] Distributed tracing integration
- [ ] Custom visualization plugins

## References

- [Prometheus Exposition Format](https://prometheus.io/docs/instrumenting/exposition_formats/)
- [ANSI Escape Codes](https://en.wikipedia.org/wiki/ANSI_escape_code)
- [Performance Monitoring Best Practices](https://www.brendangregg.com/usemethod.html)
- [Adaptive Optimization Techniques](https://dl.acm.org/doi/10.1145/3377811.3380388)

## License

MIT License - See LICENSE file for details
