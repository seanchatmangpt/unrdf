# Configuration Guide

Knowd offers extensive configuration options through command-line flags, environment variables, and configuration files. This guide covers all available configuration methods.

## Configuration Methods

Knowd supports configuration through:

1. **Command-line flags** - For runtime configuration
2. **Environment variables** - For containerized deployments
3. **Configuration files** - For complex setups (planned feature)

## Command-Line Flags

### Basic Server Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-addr` | `KNOWD_ADDR` | `:8090` | HTTP server address |
| `-data-dir` | `KNOWD_DATA_DIR` | `./data` | Data directory path |
| `-core-url` | `KNOWD_CORE_URL` | `native://` | Core URL |

### Storage Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-store` | `KNOWD_STORE` | `disk` | Storage backend (`disk` or `mem`) |
| `-wal-max-bytes` | `KNOWD_WAL_MAX_BYTES` | `64MB` | WAL maximum size before compaction |
| `-snapshot-sec` | `KNOWD_SNAPSHOT_SEC` | `300` | Snapshot interval in seconds |
| `-compact-threshold` | `KNOWD_COMPACT_THRESHOLD` | `0.5` | WAL compaction threshold (0.0-1.0) |
| `-compact-interval-sec` | `KNOWD_COMPACT_INTERVAL_SEC` | `600` | Compaction interval in seconds |

### Query Engine Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-planner-cbo` | `KNOWD_PLANNER_CBO` | `true` | Enable cost-based query planner |
| `-analyze-sample` | `KNOWD_ANALYZE_SAMPLE` | `100000` | Sample size for query statistics |
| `-analyze-ttl-sec` | `KNOWD_ANALYZE_TTL_SEC` | `3600` | TTL for cached query statistics |
| `-plan-cache-size` | `KNOWD_PLAN_CACHE_SIZE` | `256` | Query plan cache size |
| `-plan-cache-persist` | `KNOWD_PLAN_CACHE_PERSIST` | `true` | Persist plan cache across restarts |
| `-query-stream-max-bytes` | `KNOWD_QUERY_STREAM_MAX_BYTES` | `8MB` | Maximum streaming query buffer size |

### SHACL Validation Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-shacl-enabled` | `KNOWD_SHACL_ENABLED` | `true` | Enable SHACL validation |
| `-shacl-report-format` | `KNOWD_SHACL_REPORT_FORMAT` | `compact` | SHACL report format (`compact` or `detailed`) |

### Hook System Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-hooks-batch-size` | `KNOWD_HOOKS_BATCH_SIZE` | `64` | Hook evaluation batch size |
| `-window-sec` | `KNOWD_WINDOW_SEC` | `120` | Time window for window hooks |

### Clustering Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-cluster-mode` | `KNOWD_CLUSTER_MODE` | `single` | Cluster mode (`single`, `leader`, `follower`) |
| `-peer-addrs` | `KNOWD_PEER_ADDRS` | `""` | Comma-separated list of peer addresses |
| `-repl-snapshot-sec` | `KNOWD_REPL_SNAPSHOT_SEC` | `300` | Replication snapshot interval |
| `-repl-stream-chunk-bytes` | `KNOWD_REPL_STREAM_CHUNK_BYTES` | `1MB` | Replication stream chunk size |

### Security Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-mtls-cert` | `KNOWD_MTLS_CERT` | `""` | mTLS certificate file path |
| `-mtls-key` | `KNOWD_MTLS_KEY` | `""` | mTLS private key file path |
| `-mtls-ca` | `KNOWD_MTLS_CA` | `""` | mTLS CA certificate file path |
| `-token-secret` | `KNOWD_TOKEN_SECRET` | `""` | HMAC token secret for authentication |

### Namespacing Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-namespace-default` | `KNOWD_NAMESPACE_DEFAULT` | `default` | Default namespace name |

### Vector Search Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-vec-enabled` | `KNOWD_VEC_ENABLED` | `true` | Enable vector search |
| `-vec-topk-default` | `KNOWD_VEC_TOPK_DEFAULT` | `5` | Default top-K for vector search |
| `-vec-hnsw-M` | `KNOWD_VEC_HNSW_M` | `16` | HNSW M parameter |
| `-vec-hnsw-efc` | `KNOWD_VEC_HNSW_EFC` | `200` | HNSW efConstruction parameter |

### WASM Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-wasm-enabled` | `KNOWD_WASM_ENABLED` | `true` | Enable WASM hook execution |
| `-wasm-max-mem` | `KNOWD_WASM_MAX_MEM` | `64MB` | Maximum WASM memory per instance |

### Telemetry and Monitoring

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-otel-exporter` | `KNOWD_OTEL_EXPORTER` | `none` | OpenTelemetry exporter (`none`, `stdout`, `otlp`) |
| `-otel-endpoint` | `KNOWD_OTEL_ENDPOINT` | `http://localhost:4318` | OTLP endpoint |
| `-otel-service` | `KNOWD_OTEL_SERVICE` | `knowd` | Service name for telemetry |
| `-pprof-addr` | `KNOWD_PPROF_ADDR` | `""` | pprof server address (empty = disabled) |
| `-trace-addr` | `KNOWD_TRACE_ADDR` | `""` | Trace server address (empty = disabled) |

### Policy and Packs Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-packs` | `KNOWD_PACKS` | `""` | Comma-separated list of policy pack paths |
| `-watch-packs` | `KNOWD_WATCH_PACKS` | `true` | Enable policy pack hot-reloading |
| `-pack-registry` | `KNOWD_PACK_REGISTRY` | `""` | Policy pack registry URL |

### Quotas Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-quota-qps` | `KNOWD_QUOTA_QPS` | `0` | Queries per second limit (0 = unlimited) |
| `-quota-rowsps` | `KNOWD_QUOTA_ROWSPS` | `0` | Rows per second limit (0 = unlimited) |

### Advanced Configuration

| Flag | Environment Variable | Default | Description |
|------|---------------------|---------|-------------|
| `-views-enabled` | `KNOWD_VIEWS_ENABLED` | `true` | Enable materialized views |
| `-views-refresh-workers` | `KNOWD_VIEWS_REFRESH_WORKERS` | `2` | Number of view refresh workers |
| `-timetravel-enabled` | `KNOWD_TIMETRAVEL_ENABLED` | `true` | Enable time-travel queries |
| `-rollout-enabled` | `KNOWD_ROLLOUT_ENABLED` | `true` | Enable policy rollouts |

## Environment Variable Examples

### Development Setup
```bash
export KNOWD_ADDR=:8090
export KNOWD_DATA_DIR=./data
export KNOWD_STORE=mem
export KNOWD_SHACL_ENABLED=false
./knowd
```

### Production Setup
```bash
export KNOWD_ADDR=:8090
export KNOWD_DATA_DIR=/var/lib/knowd
export KNOWD_STORE=disk
export KNOWD_SHACL_ENABLED=true
export KNOWD_CLUSTER_MODE=leader
export KNOWD_PEER_ADDRS=follower1:8090,follower2:8090
export KNOWD_MTLS_CERT=/etc/ssl/knowd.crt
export KNOWD_MTLS_KEY=/etc/ssl/knowd.key
export KNOWD_MTLS_CA=/etc/ssl/ca.crt
export KNOWD_OTEL_EXPORTER=otlp
export KNOWD_OTEL_ENDPOINT=https://otel-collector:4318
./knowd
```

### Docker Compose Example
```yaml
version: '3.8'
services:
  knowd:
    image: unrdf/knowd:latest
    ports:
      - "8090:8090"
    environment:
      - KNOWD_ADDR=:8090
      - KNOWD_DATA_DIR=/data
      - KNOWD_STORE=disk
      - KNOWD_SHACL_ENABLED=true
      - KNOWD_CLUSTER_MODE=single
      - KNOWD_OTEL_EXPORTER=otlp
      - KNOWD_OTEL_ENDPOINT=http://otel-collector:4318
    volumes:
      - knowd-data:/data
      - ./certs:/etc/ssl:ro
    depends_on:
      - otel-collector

volumes:
  knowd-data:
```

## Configuration File Support (Planned)

Future versions will support configuration files:

```yaml
# knowd.yaml
server:
  addr: ":8090"
  dataDir: "./data"

storage:
  backend: "disk"
  walMaxBytes: 67108864  # 64MB
  snapshotIntervalSec: 300

query:
  plannerCBO: true
  planCacheSize: 256
  streamMaxBytes: 8388608  # 8MB

shacl:
  enabled: true
  reportFormat: "compact"

hooks:
  batchSize: 64
  windowSec: 120

cluster:
  mode: "single"
  peers: []

security:
  mtls:
    cert: "/etc/ssl/knowd.crt"
    key: "/etc/ssl/knowd.key"
    ca: "/etc/ssl/ca.crt"

telemetry:
  otel:
    exporter: "otlp"
    endpoint: "http://localhost:4318"
    serviceName: "knowd"
```

## Flag Precedence

Configuration values are applied in this order (later values override earlier ones):

1. **Default values** (hardcoded in the binary)
2. **Configuration file** (when implemented)
3. **Environment variables**
4. **Command-line flags**

## Validation

Knowd validates configuration on startup and provides helpful error messages:

```bash
$ ./knowd -store invalid
ERROR: invalid value for -store: "invalid" (must be "disk" or "mem")

$ ./knowd -quota-qps -1
ERROR: invalid value for -quota-qps: -1 (must be >= 0)
```

## Performance Tuning

### Memory Usage
- **Small datasets**: Use `mem` store for fastest queries
- **Large datasets**: Use `disk` store for better memory management
- **High concurrency**: Increase plan cache size and hook batch size

### Query Performance
- **Complex queries**: Enable CBO planner for better optimization
- **Frequent queries**: Increase plan cache size for better hit rates
- **Large result sets**: Adjust streaming buffer size

### Clustering
- **Leader mode**: Single writer, multiple readers
- **Follower mode**: Read-only replica for scaling queries
- **Replication**: Tune snapshot and chunk sizes for your network

## Security Considerations

### Production Security
- **Always use mTLS** in cluster mode
- **Set appropriate quotas** to prevent resource exhaustion
- **Enable SHACL validation** for data integrity
- **Use proper file permissions** on certificates and keys

### Network Security
- **Firewall rules**: Only expose necessary ports (8090 for HTTP, 9090 for gRPC)
- **TLS termination**: Use reverse proxy for HTTPS if needed
- **Network isolation**: Run in private network for cluster communication

## Monitoring Configuration

### OpenTelemetry Setup
```bash
# Enable OTLP exporter
export KNOWD_OTEL_EXPORTER=otlp
export KNOWD_OTEL_ENDPOINT=https://your-otel-collector:4318

# Enable pprof for debugging
export KNOWD_PPROF_ADDR=:6060
```

### Metrics Collection
Knowd exports metrics in OpenTelemetry format including:
- Query execution times and counts
- Storage operation metrics
- Hook evaluation statistics
- Cluster replication metrics

## Troubleshooting Configuration

### Common Configuration Issues

**"Store limit reached"**
- Increase WAL size: `-wal-max-bytes 134217728` (128MB)
- Or reduce compaction threshold: `-compact-threshold 0.3`

**"Query timeout"**
- Increase streaming buffer: `-query-stream-max-bytes 16777216` (16MB)
- Or disable complex features: `-planner-cbo false`

**"Out of memory"**
- Use disk store: `-store disk`
- Reduce cache sizes: `-plan-cache-size 128`
- Enable compaction: `-compact-interval-sec 300`

**"Connection refused" in cluster**
- Verify mTLS certificates: `-mtls-cert /path/to/cert.pem`
- Check peer addresses: `-peer-addrs follower1:8090,follower2:8090`

For more troubleshooting help, see the [Troubleshooting Guide](../../troubleshooting/common-issues.md).
