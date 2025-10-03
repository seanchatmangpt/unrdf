# Performance Tuning Guide

This guide provides comprehensive strategies for optimizing Knowd's performance across different deployment scenarios and workload patterns.

## Performance Baseline

### Target Performance Metrics

**Query Performance:**
- **SELECT queries**: <300ms p95 latency
- **ASK queries**: <100ms p95 latency
- **CONSTRUCT queries**: <500ms p95 latency
- **Vector search**: <5ms p95 latency
- **Hook execution**: <75ms p95 latency

**Throughput:**
- **Single node**: 500+ queries/second
- **Clustered (3 nodes)**: 1500+ queries/second
- **Vector searches**: 1000+ searches/second

**Resource Utilization:**
- **CPU**: <70% average utilization
- **Memory**: <80% of available RAM
- **Disk I/O**: <50% of available bandwidth
- **Network**: <30% of available bandwidth

## Profiling and Analysis

### CPU Profiling

**Enable profiling:**
```bash
KNOWD_PPROF_ADDR=:6060 ./knowd
```

**Collect and analyze:**
```bash
# Collect CPU profile
curl http://localhost:6060/debug/pprof/profile?seconds=30 > cpu.prof

# Analyze profile
go tool pprof cpu.prof

# Top functions by CPU usage
(pprof) top10

# Function call graph
(pprof) web
```

**Common optimization targets:**
- SPARQL query execution (`internal/sparql/exec.go`)
- Hook batch processing (`internal/hooks/batch.go`)
- RDF store operations (`internal/store/disk/`)
- SHACL validation (`internal/shacl/validator.go`)

### Memory Profiling

**Memory analysis:**
```bash
# Collect heap profile
curl http://localhost:6060/debug/pprof/heap > heap.prof

# Analyze memory usage
go tool pprof heap.prof

# Memory allocation tracking
(pprof) top -cum
```

**Memory optimization strategies:**
- **Reduce allocations**: Reuse buffers and objects
- **Optimize data structures**: Choose appropriate types for access patterns
- **Garbage collection tuning**: Adjust GC targets based on workload

### Query Performance Analysis

**Query plan analysis:**
```bash
# Analyze query execution plan
curl -X POST http://localhost:8090/v1/query/analyze \
     -H "Content-Type: application/json" \
     -d '{"query": "SELECT * WHERE { ?s ?p ?o }"}'
```

**Cache performance:**
```bash
# Monitor cache hit rates
curl "http://localhost:9090/metrics" | grep knowd_plan_cache
```

## Configuration Optimization

### Memory Management

**Garbage Collection Tuning:**
```bash
# Aggressive GC for memory-constrained environments
GOGC=50 ./knowd

# Relaxed GC for CPU-optimized environments
GOGC=200 ./knowd

# Set memory limit
GOMEMLIMIT=16GiB ./knowd
```

**Application Memory:**
```bash
# Query plan cache
KNOWD_PLAN_CACHE_SIZE=2000 ./knowd

# Hook batch size
KNOWD_HOOKS_BATCH_SIZE=128 ./knowd

# Vector index parameters
KNOWD_VEC_HNSW_M=32 ./knowd
KNOWD_VEC_HNSW_EFC=400 ./knowd
```

### Storage Optimization

**WAL Configuration:**
```bash
# Optimize for write-heavy workloads
KNOWD_WAL_MAX_BYTES=134217728 ./knowd  # 128MB segments
KNOWD_COMPACT_THRESHOLD=0.3 ./knowd      # Compact at 30% fragmentation

# Optimize for read-heavy workloads
KNOWD_SNAPSHOT_SEC=1800 ./knowd          # Snapshot every 30 minutes
KNOWD_COMPACT_THRESHOLD=0.7 ./knowd      # Less frequent compaction
```

**Compression Settings:**
```bash
# Enable compression for storage efficiency
KNOWD_STORE_COMPRESSION=zstd ./knowd

# Adjust compression level (1-22, higher = better compression, slower)
KNOWD_STORE_COMPRESSION_LEVEL=3 ./knowd
```

### Network Optimization

**Cluster Communication:**
```bash
# Optimize replication performance
KNOWD_REPL_STREAM_CHUNK_BYTES=4194304 ./knowd  # 4MB chunks
KNOWD_CLUSTER_READ_TIMEOUT=60s ./knowd
KNOWD_CLUSTER_WRITE_TIMEOUT=120s ./knowd

# Enable connection pooling
KNOWD_CLUSTER_MAX_CONNECTIONS=50 ./knowd
```

**HTTP Server:**
```bash
# Optimize HTTP performance
KNOWD_HTTP_MAX_CONNS=10000 ./knowd
KNOWD_HTTP_READ_TIMEOUT=30s ./knowd
KNOWD_HTTP_WRITE_TIMEOUT=60s ./knowd
```

## Workload-Specific Optimization

### Read-Heavy Workloads

**Characteristics:**
- High query volume
- Low transaction volume
- Vector search intensive

**Optimizations:**
```bash
# Increase cache sizes for read performance
KNOWD_PLAN_CACHE_SIZE=5000 ./knowd
KNOWD_VEC_HNSW_EFC=600 ./knowd

# Optimize for query throughput
KNOWD_PLANNER_CBO=true ./knowd
KNOWD_ANALYZE_SAMPLE=1000000 ./knowd

# Reduce snapshot frequency
KNOWD_SNAPSHOT_SEC=3600 ./knowd
```

### Write-Heavy Workloads

**Characteristics:**
- High transaction volume
- Frequent data updates
- Hook-intensive processing

**Optimizations:**
```bash
# Optimize for write throughput
KNOWD_HOOKS_BATCH_SIZE=256 ./knowd
KNOWD_WAL_MAX_BYTES=268435456 ./knowd  # 256MB segments

# Enable parallel hook processing
KNOWD_HOOKS_WORKERS=8 ./knowd

# Increase snapshot frequency for faster recovery
KNOWD_SNAPSHOT_SEC=300 ./knowd
```

### Mixed Workloads

**Characteristics:**
- Balanced read/write ratio
- Moderate query and transaction volume

**Optimizations:**
```bash
# Balanced configuration
KNOWD_PLAN_CACHE_SIZE=2000 ./knowd
KNOWD_HOOKS_BATCH_SIZE=64 ./knowd
KNOWD_WAL_MAX_BYTES=134217728 ./knowd
KNOWD_SNAPSHOT_SEC=900 ./knowd
```

## Database Schema Optimization

### Index Strategy

**Query Pattern Analysis:**
```bash
# Analyze common query patterns
curl "http://localhost:9090/metrics" | grep knowd_query_patterns

# Optimize indexes based on access patterns
KNOWD_STORE_INDEX_STRATEGY=adaptive ./knowd
```

**Index Configuration:**
```bash
# SPO index for subject lookups
KNOWD_STORE_INDEX_SPO=true ./knowd

# POS index for predicate lookups
KNOWD_STORE_INDEX_POS=true ./knowd

# OSP index for object lookups
KNOWD_STORE_INDEX_OSP=true ./knowd
```

### Data Layout Optimization

**Partitioning Strategy:**
```bash
# Namespace-based partitioning
KNOWD_NAMESPACE_ISOLATION=true ./knowd

# Time-based partitioning for temporal data
KNOWD_TIMETRAVEL_ENABLED=true ./knowd
KNOWD_TIMETRAVEL_RETENTION=365d ./knowd
```

**Compression Strategy:**
```bash
# Enable data compression
KNOWD_STORE_COMPRESSION=lz4 ./knowd

# Optimize for space vs speed
KNOWD_STORE_COMPRESSION_LEVEL=6 ./knowd
```

## Operating System Tuning

### Linux Kernel Optimization

**System limits:**
```bash
# Increase file descriptor limits
echo 'fs.file-max = 2097152' >> /etc/sysctl.conf

# Increase socket buffer sizes
echo 'net.core.rmem_max = 134217728' >> /etc/sysctl.conf
echo 'net.core.wmem_max = 134217728' >> /etc/sysctl.conf

# Enable TCP optimizations
echo 'net.ipv4.tcp_rmem = 4096 87380 134217728' >> /etc/sysctl.conf
echo 'net.ipv4.tcp_wmem = 4096 65536 134217728' >> /etc/sysctl.conf

# Apply changes
sysctl -p
```

**File system tuning:**
```bash
# Mount with performance options
mount -o noatime,nodiratime,discard /dev/nvme0n1 /var/lib/knowd

# Disable access time updates
echo 'options ext4 noauto_da_alloc' >> /etc/modprobe.d/ext4.conf
```

### CPU and Memory

**CPU affinity:**
```bash
# Pin Knowd to specific CPU cores
taskset -c 0-7 ./knowd

# Use CPU sets for isolation
cset shield --cpu 0-3 -- ./knowd
```

**Memory management:**
```bash
# Lock memory to prevent swapping
echo 3 > /proc/sys/vm/drop_caches

# Disable swap for performance
swapoff -a
```

## Container Optimization

### Docker Configuration

**Resource limits:**
```yaml
# docker-compose.yml
services:
  knowd:
    deploy:
      resources:
        limits:
          cpus: '4.0'
          memory: 8G
        reservations:
          cpus: '2.0'
          memory: 4G
```

**Shared memory:**
```bash
# Increase shared memory size
docker run --shm-size=2g knowd:latest
```

### Kubernetes Configuration

**Resource requests and limits:**
```yaml
# k8s-deployment.yaml
spec:
  template:
    spec:
      containers:
      - name: knowd
        resources:
          requests:
            memory: "4Gi"
            cpu: "2000m"
          limits:
            memory: "8Gi"
            cpu: "4000m"
```

**Node selection:**
```yaml
nodeSelector:
  knowd-node: "true"

tolerations:
- key: "knowd"
  operator: "Equal"
  value: "dedicated"
  effect: "NoSchedule"
```

## Monitoring-Based Optimization

### Performance Monitoring Setup

**Prometheus configuration:**
```yaml
# prometheus.yml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'knowd-performance'
    static_configs:
      - targets: ['knowd:9090']
    scrape_interval: 5s
    metrics_path: '/metrics'
```

**Key metrics for optimization:**
```bash
# Query performance
histogram_quantile(0.95, rate(knowd_query_duration_seconds_bucket[5m]))

# Cache performance
rate(knowd_plan_cache_hits_total[5m]) / rate(knowd_plan_cache_total[5m])

# Storage performance
rate(knowd_store_read_bytes_total[5m]) / rate(knowd_store_write_bytes_total[5m])

# Resource utilization
rate(container_cpu_usage_seconds_total[5m]) * 100
container_memory_usage_bytes / container_spec_memory_limit_bytes * 100
```

### Automated Performance Tuning

**Adaptive configuration:**
```go
// Example: Dynamic cache sizing based on memory pressure
func adjustCacheSize() {
    memUsage := getMemoryUsage()
    if memUsage > 80 {
        setCacheSize(getCurrentCacheSize() * 0.8)
    } else if memUsage < 50 {
        setCacheSize(getCurrentCacheSize() * 1.2)
    }
}
```

**Load-based optimization:**
```bash
# Monitor query load and adjust batch sizes
if query_rate > 1000_per_second {
    KNOWD_HOOKS_BATCH_SIZE=32 ./knowd
} else {
    KNOWD_HOOKS_BATCH_SIZE=128 ./knowd
}
```

## Benchmarking and Validation

### Performance Benchmarks

**Query benchmarks:**
```bash
# Benchmark different query types
go test -bench=BenchmarkQuery -benchmem ./internal/sparql/

# Compare query performance
go test -bench=BenchmarkQuery -count=5 ./internal/sparql/ > before.txt
# Make optimization changes
go test -bench=BenchmarkQuery -count=5 ./internal/sparql/ > after.txt
benchcmp before.txt after.txt
```

**Load testing:**
```bash
# Concurrent query load test
go test -run TestLoad_ConcurrentQueries ./integration_test.go

# Throughput testing
go test -bench=BenchmarkThroughput ./integration_test.go
```

### Regression Testing

**Performance regression detection:**
```bash
# Set baseline performance
go test -bench=. ./... > baseline.txt

# After changes, compare against baseline
go test -bench=. ./... > current.txt
benchcmp baseline.txt current.txt
```

**Automated regression checks:**
```yaml
# CI/CD pipeline
- name: Performance Regression Check
  run: |
    go test -bench=. ./... > current.txt
    benchcmp baseline.txt current.txt || exit 1
```

## Advanced Optimization Techniques

### Memory Pooling

**Buffer reuse:**
```go
// Use sync.Pool for frequently allocated objects
var bufferPool = sync.Pool{
    New: func() interface{} {
        return make([]byte, 4096)
    },
}

func processData(data []byte) {
    buf := bufferPool.Get().([]byte)
    defer bufferPool.Put(buf)

    // Use buf instead of allocating new slice
    copy(buf, data)
    // Process data...
}
```

### Parallel Processing

**Concurrent query execution:**
```bash
# Enable parallel hook processing
KNOWD_HOOKS_WORKERS=4 ./knowd

# Enable parallel SPARQL execution
KNOWD_SPARQL_WORKERS=4 ./knowd
```

**Batch processing:**
```bash
# Increase batch sizes for better throughput
KNOWD_HOOKS_BATCH_SIZE=256 ./knowd
KNOWD_TX_BATCH_SIZE=100 ./knowd
```

### Caching Strategies

**Multi-level caching:**
```bash
# Query plan cache
KNOWD_PLAN_CACHE_SIZE=5000 ./knowd

# Result cache for expensive queries
KNOWD_RESULT_CACHE_SIZE=1000 ./knowd
KNOWD_RESULT_CACHE_TTL=300s ./knowd

# RDF data cache
KNOWD_STORE_CACHE_SIZE=1000000 ./knowd
```

## Troubleshooting Performance Issues

### Slow Queries

**Query analysis:**
```bash
# Get detailed query plan
curl -X POST http://localhost:8090/v1/query/analyze \
     -H "Content-Type: application/json" \
     -d '{"query": "SELECT * WHERE { ?s ?p ?o }"}'

# Check for missing indexes
curl "http://localhost:9090/metrics" | grep knowd_store_index
```

**Optimization strategies:**
```bash
# Enable cost-based optimization
KNOWD_PLANNER_CBO=true ./knowd

# Increase statistics sampling
KNOWD_ANALYZE_SAMPLE=1000000 ./knowd

# Use query hints
# SELECT /* +INDEX(?s ?p) */ * WHERE { ?s ?p ?o }
```

### High Memory Usage

**Memory leak detection:**
```bash
# Compare heap profiles over time
curl http://localhost:6060/debug/pprof/heap > heap1.prof
sleep 300
curl http://localhost:6060/debug/pprof/heap > heap2.prof
go tool pprof -diff_base=heap1.prof heap2.prof
```

**Memory optimization:**
```bash
# Reduce cache sizes
KNOWD_PLAN_CACHE_SIZE=1000 ./knowd

# Enable memory pooling
KNOWD_MEMORY_POOLING=true ./knowd

# Set memory limits
GOMEMLIMIT=12GiB ./knowd
```

### Network Bottlenecks

**Cluster network optimization:**
```bash
# Increase network buffer sizes
KNOWD_REPL_STREAM_CHUNK_BYTES=8388608 ./knowd

# Reduce network timeouts for faster failure detection
KNOWD_CLUSTER_READ_TIMEOUT=30s ./knowd
KNOWD_CLUSTER_WRITE_TIMEOUT=60s ./knowd
```

**Connection pooling:**
```bash
# Increase connection pool sizes
KNOWD_CLUSTER_MAX_CONNECTIONS=100 ./knowd
KNOWD_HTTP_MAX_CONNS=5000 ./knowd
```

## Continuous Optimization

### Performance Monitoring

**Automated performance tracking:**
```bash
# Continuous benchmarking
while true; do
    go test -bench=BenchmarkQuery -count=1 ./internal/sparql/ >> perf.log
    sleep 3600  # Every hour
done
```

**Trend analysis:**
```bash
# Analyze performance trends
cat perf.log | grep BenchmarkQuery | awk '{print $3}' | \
    datamash mean 1 p95 1 | tee perf-trends.txt
```

### A/B Testing

**Compare configurations:**
```bash
# Test configuration A
KNOWD_PLAN_CACHE_SIZE=1000 ./knowd &
PID_A=$!
sleep 300
kill $PID_A

# Test configuration B
KNOWD_PLAN_CACHE_SIZE=2000 ./knowd &
PID_B=$!
sleep 300
kill $PID_B
```

### Load Testing

**Realistic load simulation:**
```bash
# Simulate production workload
go test -run TestLoad_RealisticWorkload ./integration_test.go

# Stress test with increasing load
for load in 100 500 1000 2000; do
    echo "Testing with $load concurrent requests"
    # Run load test
    go test -run TestLoad_Stress -load=$load ./integration_test.go
done
```

## Best Practices Summary

### 1. Measurement First

- **Establish baselines** before making changes
- **Measure impact** of each optimization
- **Use realistic workloads** for testing

### 2. Incremental Optimization

- **Start with profiling** to identify bottlenecks
- **Optimize one area at a time** to isolate effects
- **Validate improvements** with benchmarks

### 3. Monitor Continuously

- **Track key metrics** in production
- **Alert on regressions** automatically
- **Review performance** regularly

### 4. Configuration Management

- **Document all changes** with rationale
- **Use version control** for configuration
- **Test configurations** before deployment

### 5. Resource Planning

- **Right-size deployments** based on workload
- **Plan for growth** with headroom
- **Monitor resource utilization** trends

This performance tuning guide provides comprehensive strategies for optimizing Knowd's performance across different deployment scenarios, workloads, and infrastructure configurations.
