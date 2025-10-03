# Debugging Guide

This guide provides comprehensive debugging techniques and tools for diagnosing issues in Knowd deployments.

## Debugging Philosophy

### 1. Systematic Approach

**Gather Information:**
- **Logs**: Check application and system logs
- **Metrics**: Monitor key performance indicators
- **Traces**: Follow request flow through the system
- **Configuration**: Verify settings and environment

**Form Hypothesis:**
- **Root Cause Analysis**: Identify the most likely cause
- **Impact Assessment**: Understand scope of the issue
- **Reproduction**: Create minimal test cases

**Test and Validate:**
- **Isolated Testing**: Test components independently
- **Controlled Environment**: Use consistent test conditions
- **Measurement**: Quantify the issue and improvements

### 2. Tools and Techniques

**Built-in Debug Tools:**
- **pprof endpoints**: CPU, memory, goroutine profiling
- **Metrics endpoint**: Prometheus-compatible metrics
- **Health checks**: Application and cluster health
- **Debug logging**: Detailed operational logging

**External Tools:**
- **Wireshark/tcpdump**: Network traffic analysis
- **strace/ltrace**: System call tracing
- **gdb**: Low-level debugging (advanced)
- **heap dump analysis**: Memory leak detection

## Debugging Workflows

### 1. Application Startup Issues

**Symptom:** Knowd fails to start or crashes immediately

**Debug Steps:**

1. **Check basic requirements:**
   ```bash
   # Verify Go installation
   go version

   # Check dependencies
   go mod tidy
   go mod verify

   # Verify binary
   ls -la ./knowd
   file ./knowd
   ```

2. **Enable verbose logging:**
   ```bash
   KNOWD_LOG_LEVEL=debug ./knowd 2>&1 | tee startup.log
   ```

3. **Check for port conflicts:**
   ```bash
   # Find processes using Knowd ports
   netstat -tulpn | grep :8090
   lsof -i :8090

   # Kill conflicting processes
   kill -9 <PID>
   ```

4. **Validate configuration:**
   ```bash
   # Check environment variables
   env | grep KNOWD

   # Validate data directory permissions
   ls -ld /var/lib/knowd
   ```

### 2. Runtime Performance Issues

**Symptom:** Slow response times, high resource usage, or timeouts

**Debug Steps:**

1. **Enable profiling:**
   ```bash
   KNOWD_PPROF_ADDR=:6060 ./knowd
   ```

2. **Collect performance data:**
   ```bash
   # CPU profiling
   curl http://localhost:6060/debug/pprof/profile?seconds=30 > cpu.prof

   # Memory profiling
   curl http://localhost:6060/debug/pprof/heap > heap.prof

   # Goroutine dump
   curl http://localhost:6060/debug/pprof/goroutine > goroutines.txt
   ```

3. **Analyze profiles:**
   ```bash
   # CPU analysis
   go tool pprof cpu.prof
   (pprof) top10
   (pprof) web

   # Memory analysis
   go tool pprof heap.prof
   (pprof) top -cum
   ```

4. **Monitor metrics:**
   ```bash
   curl http://localhost:9090/metrics | grep knowd_query_duration
   curl http://localhost:9090/metrics | grep knowd_memory_usage
   ```

### 3. Query Execution Issues

**Symptom:** SPARQL queries fail, return incorrect results, or are slow

**Debug Steps:**

1. **Enable query debugging:**
   ```bash
   KNOWD_LOG_LEVEL=debug KNOWD_SPARQL_DEBUG=true ./knowd
   ```

2. **Analyze query plans:**
   ```bash
   curl -X POST http://localhost:8090/v1/query/analyze \
        -H "Content-Type: application/json" \
        -d '{"query": "SELECT * WHERE { ?s ?p ?o }"}'
   ```

3. **Test query syntax:**
   ```bash
   # Test with simple queries
   curl -X POST http://localhost:8090/v1/query \
        -H "Content-Type: application/json" \
        -d '{"query": "SELECT * WHERE { ?s ?p ?o } LIMIT 10", "kind": "sparql-select"}'
   ```

4. **Check data consistency:**
   ```bash
   # Verify store statistics
   curl http://localhost:8090/v1/store/stats

   # Check for data corruption
   KNOWD_STORE=disk KNOWD_DATA_DIR=/var/lib/knowd ./knowd
   ```

### 4. Hook Execution Issues

**Symptom:** Hooks not triggering, failing, or causing performance issues

**Debug Steps:**

1. **Enable hook debugging:**
   ```bash
   KNOWD_LOG_LEVEL=debug KNOWD_HOOKS_DEBUG=true ./knowd
   ```

2. **Test hook execution:**
   ```bash
   curl -X POST http://localhost:8090/v1/hooks/evaluate \
        -H "Content-Type: application/json" \
        -d '{"hook": {"kind": "sparql-ask", "query": "ASK { ?s ?p ?o }"}}'
   ```

3. **Check hook configuration:**
   ```bash
   # Validate policy pack syntax
   KNOWD_PACKS=/path/to/packs ./knowd

   # Monitor hook execution metrics
   curl "http://localhost:9090/metrics" | grep knowd_hook
   ```

4. **Debug hook failures:**
   ```bash
   # Check hook execution logs
   grep -i hook /var/log/knowd/knowd.log

   # Test individual hook types
   # SPARQL-ASK hooks
   curl -X POST http://localhost:8090/v1/hooks/evaluate \
        -H "Content-Type: application/json" \
        -d '{"hook": {"kind": "sparql-ask", "query": "ASK { <http://example.org/s> <http://example.org/p> <http://example.org/o> }"}}'
   ```

### 5. Cluster Issues

**Symptom:** Replication problems, node communication failures, or inconsistent data

**Debug Steps:**

1. **Check cluster status:**
   ```bash
   curl http://leader:8090/v1/cluster/status
   ```

2. **Monitor replication metrics:**
   ```bash
   curl "http://localhost:9090/metrics" | grep knowd_cluster
   ```

3. **Test node connectivity:**
   ```bash
   # Test between nodes
   curl http://follower1:8090/healthz
   ping follower1

   # Check network configuration
   netstat -rn
   ```

4. **Debug WAL issues:**
   ```bash
   # Check WAL size and growth
   ls -lh /var/lib/knowd/wal/
   curl "http://localhost:9090/metrics" | grep knowd_store_segments

   # Monitor compaction
   curl "http://localhost:9090/metrics" | grep knowd_compaction
   ```

## Advanced Debugging Techniques

### 1. Distributed Tracing

**Enable OpenTelemetry tracing:**
```bash
KNOWD_OTEL_EXPORTER=otlp \
KNOWD_OTEL_ENDPOINT=http://jaeger:14268/api/traces \
KNOWD_LOG_LEVEL=debug \
./knowd
```

**Trace analysis:**
- **Transaction traces**: Full request lifecycle
- **Query traces**: SPARQL execution flow
- **Hook traces**: Policy evaluation flow
- **Cluster traces**: Cross-node operations

### 2. Memory Analysis

**Memory leak detection:**
```bash
# Collect heap profiles over time
curl http://localhost:6060/debug/pprof/heap > heap1.prof
sleep 300
curl http://localhost:6060/debug/pprof/heap > heap2.prof

# Compare profiles
go tool pprof -diff_base=heap1.prof heap2.prof
```

**Memory optimization:**
```bash
# Analyze allocation patterns
go tool pprof -alloc_space heap.prof

# Find memory hotspots
(pprof) list knowd/internal/sparql
```

### 3. Network Analysis

**Packet capture:**
```bash
# Capture cluster traffic
tcpdump -i eth0 -w cluster.pcap port 8090 or port 8091

# Analyze with Wireshark
wireshark cluster.pcap
```

**Connection analysis:**
```bash
# Monitor active connections
netstat -an | grep :809

# Check connection states
ss -tuln | grep :809

# Monitor network I/O
iftop -i eth0
```

### 4. System-Level Debugging

**Process monitoring:**
```bash
# Monitor process resource usage
top -p $(pgrep knowd)

# Check file descriptor usage
lsof -p $(pgrep knowd) | wc -l

# Monitor system calls
strace -p $(pgrep knowd) -f
```

**Kernel debugging:**
```bash
# Check system load
uptime

# Monitor I/O statistics
iostat -x 1

# Check memory pressure
vmstat 1
```

## Log Analysis

### Structured Log Analysis

**Search patterns:**
```bash
# Find errors
grep -i error /var/log/knowd/knowd.log

# Find slow operations
grep "duration_ms" /var/log/knowd/knowd.log | awk '$NF > 1000 {print}'

# Find authentication issues
grep -i auth /var/log/knowd/knowd.log

# Find cluster issues
grep -i cluster /var/log/knowd/knowd.log
```

**Log aggregation:**
```bash
# Ship logs to Elasticsearch
KNOWD_LOG_FORMAT=json \
KNOWD_OTEL_EXPORTER=otlp \
KNOWD_OTEL_ENDPOINT=http://elasticsearch:9200 \
./knowd
```

**Log analysis queries:**
```bash
# Error rate analysis
GET /knowd-*/_search
{
  "query": {
    "range": {
      "@timestamp": {
        "gte": "now-1h"
      }
    }
  },
  "aggs": {
    "error_rate": {
      "terms": {
        "field": "level.keyword"
      }
    }
  }
}
```

## Interactive Debugging

### 1. Attach Debugger

**Using Delve:**
```bash
# Start Knowd with debugger
dlv debug ./cmd/knowd -- -addr=:8090

# Set breakpoints
(dlv) break main.main
(dlv) break internal/server/http.go:100

# Run and inspect
(dlv) continue
(dlv) locals
(dlv) print variable
```

**Using GDB:**
```bash
# Attach to running process
gdb attach $(pgrep knowd)

# Set breakpoints
(gdb) break main.main
(gdb) break internal/sparql/exec.go:100

# Continue execution
(gdb) continue
```

### 2. Runtime Inspection

**Goroutine analysis:**
```bash
# Dump goroutine stack traces
curl http://localhost:6060/debug/pprof/goroutine > goroutines.txt

# Analyze goroutine dumps
go tool pprof goroutines.txt
```

**Mutex contention:**
```bash
# Enable mutex profiling
GOMAXPROCS=4 ./knowd

# Collect mutex profile
curl http://localhost:6060/debug/pprof/mutex > mutex.prof
go tool pprof mutex.prof
```

### 3. Request Tracing

**Enable request tracing:**
```bash
KNOWD_HTTP_DEBUG=true ./knowd
```

**Trace specific requests:**
```bash
# Add trace headers
curl -H "X-Trace-ID: debug-123" \
     -H "X-Span-ID: span-456" \
     http://localhost:8090/healthz
```

## Common Debug Scenarios

### Scenario 1: High Memory Usage

**Investigation Steps:**
1. **Check memory metrics**: `curl http://localhost:9090/metrics | grep memory`
2. **Profile heap usage**: `curl http://localhost:6060/debug/pprof/heap > heap.prof`
3. **Identify leaks**: Compare heap profiles over time
4. **Check cache sizes**: `curl "http://localhost:9090/metrics" | grep cache`

**Common Causes:**
- Large query result sets not being consumed
- Memory leaks in hook execution
- Inefficient data structures
- Cache growing unbounded

**Solutions:**
- Increase streaming buffer limits
- Add memory limits to caches
- Optimize data structure usage
- Enable garbage collection tuning

### Scenario 2: Slow Queries

**Investigation Steps:**
1. **Analyze query plans**: Use `/v1/query/analyze` endpoint
2. **Check cache hit rates**: Monitor `knowd_plan_cache_*` metrics
3. **Profile query execution**: Enable SPARQL debugging
4. **Monitor resource usage**: Check CPU, memory, I/O during queries

**Common Causes:**
- Missing query optimization
- Inefficient join algorithms
- Large result sets without LIMIT
- Resource contention

**Solutions:**
- Enable cost-based optimization
- Add appropriate indexes
- Use LIMIT for large queries
- Optimize resource allocation

### Scenario 3: Hook Failures

**Investigation Steps:**
1. **Enable hook debugging**: Set `KNOWD_HOOKS_DEBUG=true`
2. **Test individual hooks**: Use `/v1/hooks/evaluate` endpoint
3. **Check hook metrics**: Monitor `knowd_hook_*` metrics
4. **Review hook logs**: Search for hook-related log entries

**Common Causes:**
- Invalid SPARQL in hook queries
- Missing data dependencies
- Resource exhaustion during hook execution
- Hook timeout issues

**Solutions:**
- Validate hook SPARQL syntax
- Ensure required data exists
- Increase hook execution timeouts
- Add error handling in hook definitions

### Scenario 4: Cluster Issues

**Investigation Steps:**
1. **Check cluster status**: Use `/v1/cluster/status` endpoint
2. **Monitor replication metrics**: Check `knowd_cluster_*` metrics
3. **Test node connectivity**: Ping and curl between nodes
4. **Review cluster logs**: Check for network and sync errors

**Common Causes:**
- Network partitions
- Certificate expiration
- Resource exhaustion on nodes
- Configuration mismatches

**Solutions:**
- Verify network connectivity
- Renew certificates before expiry
- Balance resource usage across nodes
- Ensure consistent configuration

## Debug Tool Setup

### Local Development Environment

**Enable all debug features:**
```bash
KNOWD_LOG_LEVEL=debug \
KNOWD_LOG_FORMAT=json \
KNOWD_PPROF_ADDR=:6060 \
KNOWD_METRICS_ADDR=:9090 \
KNOWD_HTTP_DEBUG=true \
KNOWD_SPARQL_DEBUG=true \
KNOWD_HOOKS_DEBUG=true \
./knowd
```

**Access debug endpoints:**
```bash
# Health and metrics
curl http://localhost:8090/healthz
curl http://localhost:9090/metrics

# Profiling
curl http://localhost:6060/debug/pprof/
curl http://localhost:6060/debug/pprof/profile?seconds=30 > cpu.prof

# Application-specific
curl http://localhost:8090/v1/store/stats
curl http://localhost:8090/v1/cluster/status
```

### Production Debugging

**Enable selective debugging:**
```bash
# Production-safe debugging
KNOWD_LOG_LEVEL=info \
KNOWD_METRICS_ADDR=:9090 \
KNOWD_PPROF_ADDR=:6060 \
./knowd
```

**Debug specific issues:**
```bash
# Enable SPARQL debugging for query issues
KNOWD_SPARQL_DEBUG=true ./knowd

# Enable hook debugging for policy issues
KNOWD_HOOKS_DEBUG=true ./knowd

# Enable cluster debugging for replication issues
KNOWD_CLUSTER_DEBUG=true ./knowd
```

## Automated Debugging

### Health Check Scripts

**Basic health monitoring:**
```bash
#!/bin/bash
# health-check.sh

echo "=== Knowd Health Check ==="

# Basic connectivity
echo "1. Testing basic connectivity..."
curl -f http://localhost:8090/healthz || exit 1

# Store statistics
echo "2. Checking store statistics..."
curl http://localhost:8090/v1/store/stats

# Cluster status (if clustered)
echo "3. Checking cluster status..."
curl http://localhost:8090/v1/cluster/status

echo "✅ All health checks passed"
```

**Performance monitoring:**
```bash
#!/bin/bash
# perf-monitor.sh

echo "=== Performance Monitoring ==="

# Query latency
echo "1. Query latency (p95)..."
curl -s "http://localhost:9090/metrics" | grep knowd_query_duration_seconds_bucket | tail -1

# Memory usage
echo "2. Memory usage..."
curl -s "http://localhost:9090/metrics" | grep go_memstats_heap_inuse_bytes

# Cache hit rate
echo "3. Cache hit rate..."
curl -s "http://localhost:9090/metrics" | grep knowd_plan_cache_hit_ratio

echo "=== End Performance Check ==="
```

### Log Analysis Scripts

**Error detection:**
```bash
#!/bin/bash
# error-detector.sh

LOG_FILE="/var/log/knowd/knowd.log"

echo "=== Error Detection ==="

# Find recent errors
echo "Recent errors (last 10):"
grep -i error "$LOG_FILE" | tail -10

# Find authentication failures
echo "Authentication failures:"
grep -i "auth.*fail" "$LOG_FILE" | wc -l

# Find timeout errors
echo "Timeout errors:"
grep -i "timeout\|deadline" "$LOG_FILE" | wc -l

echo "=== End Error Detection ==="
```

## Getting Help

### Community Resources

**GitHub Issues:**
- Report bugs with reproduction steps
- Request features with use cases
- Ask questions about implementation details

**GitHub Discussions:**
- General questions and discussions
- Architecture and design discussions
- Community support and help

**Stack Overflow:**
- Technical questions about Knowd usage
- Integration and deployment issues
- Performance optimization questions

### Professional Support

For enterprise deployments:
- **Architecture reviews**: System design validation
- **Performance optimization**: Custom tuning for workloads
- **Security assessments**: Vulnerability analysis
- **Custom development**: Feature requests and integrations

### Debug Information Collection

When reporting issues, include:

**System Information:**
```bash
# Operating system and version
uname -a

# Go version
go version

# Knowd version
./knowd --version

# System resources
free -h
df -h
```

**Configuration:**
```bash
# Environment variables
env | grep KNOWD

# Configuration files
cat /etc/knowd/config.yaml
```

**Logs and Metrics:**
```bash
# Recent logs
tail -50 /var/log/knowd/knowd.log

# Current metrics
curl http://localhost:9090/metrics | head -20
```

**Debug Profiles:**
```bash
# CPU profile (30 seconds)
curl http://localhost:6060/debug/pprof/profile?seconds=30 > cpu.prof

# Heap profile
curl http://localhost:6060/debug/pprof/heap > heap.prof
```

This debugging guide provides comprehensive tools and techniques for diagnosing and resolving issues in Knowd deployments, from basic troubleshooting to advanced performance analysis and system optimization.
