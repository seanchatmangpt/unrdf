# Production Deployment Guide

This guide covers deploying Knowd in production environments with high availability, security, and performance considerations.

## Deployment Architecture

### Single Node Deployment

**Recommended for:** Development, testing, and small-scale production workloads.

```yaml
# docker-compose.yml
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
      - KNOWD_OTEL_EXPORTER=otlp
      - KNOWD_OTEL_ENDPOINT=http://jaeger:14268/api/traces
    volumes:
      - knowd-data:/data
      - ./certs:/etc/ssl:ro
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8090/healthz"]
      interval: 30s
      timeout: 10s
      retries: 3

volumes:
  knowd-data:
```

### Cluster Deployment

**Recommended for:** Production workloads requiring high availability and scalability.

#### Leader-Follower Configuration

```yaml
# docker-compose.cluster.yml
version: '3.8'
services:
  leader:
    image: unrdf/knowd:latest
    ports:
      - "8090:8090"
    environment:
      - KNOWD_ADDR=:8090
      - KNOWD_DATA_DIR=/data
      - KNOWD_STORE=disk
      - KNOWD_CLUSTER_MODE=leader
      - KNOWD_PEER_ADDRS=follower1:8090,follower2:8090
      - KNOWD_MTLS_CERT=/etc/ssl/leader.crt
      - KNOWD_MTLS_KEY=/etc/ssl/leader.key
      - KNOWD_MTLS_CA=/etc/ssl/ca.crt
      - KNOWD_OTEL_EXPORTER=otlp
      - KNOWD_OTEL_ENDPOINT=http://jaeger:14268/api/traces
    volumes:
      - leader-data:/data
      - ./certs:/etc/ssl:ro
    networks:
      - knowd-cluster
    restart: unless-stopped

  follower1:
    image: unrdf/knowd:latest
    environment:
      - KNOWD_ADDR=:8090
      - KNOWD_DATA_DIR=/data
      - KNOWD_STORE=disk
      - KNOWD_CLUSTER_MODE=follower
      - KNOWD_PEER_ADDRS=leader:8090
      - KNOWD_MTLS_CERT=/etc/ssl/follower1.crt
      - KNOWD_MTLS_KEY=/etc/ssl/follower1.key
      - KNOWD_MTLS_CA=/etc/ssl/ca.crt
      - KNOWD_OTEL_EXPORTER=otlp
      - KNOWD_OTEL_ENDPOINT=http://jaeger:14268/api/traces
    volumes:
      - follower1-data:/data
      - ./certs:/etc/ssl:ro
    networks:
      - knowd-cluster
    restart: unless-stopped

  follower2:
    image: unrdf/knowd:latest
    environment:
      - KNOWD_ADDR=:8090
      - KNOWD_DATA_DIR=/data
      - KNOWD_STORE=disk
      - KNOWD_CLUSTER_MODE=follower
      - KNOWD_PEER_ADDRS=leader:8090
      - KNOWD_MTLS_CERT=/etc/ssl/follower2.crt
      - KNOWD_MTLS_KEY=/etc/ssl/follower2.key
      - KNOWD_MTLS_CA=/etc/ssl/ca.crt
      - KNOWD_OTEL_EXPORTER=otlp
      - KNOWD_OTEL_ENDPOINT=http://jaeger:14268/api/traces
    volumes:
      - follower2-data:/data
      - ./certs:/etc/ssl:ro
    networks:
      - knowd-cluster
    restart: unless-stopped

networks:
  knowd-cluster:
    driver: bridge

volumes:
  leader-data:
  follower1-data:
  follower2-data:
```

## Security Configuration

### Certificate Generation

```bash
# Generate CA certificate
openssl genrsa -out ca.key 4096
openssl req -x509 -new -nodes -key ca.key -sha256 -days 3650 -out ca.crt -subj "/CN=Knowd CA"

# Generate server certificates
openssl genrsa -out leader.key 2048
openssl req -new -key leader.key -out leader.csr -subj "/CN=leader"
openssl x509 -req -in leader.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out leader.crt -days 365 -sha256

# Generate client certificates for followers
openssl genrsa -out follower1.key 2048
openssl req -new -key follower1.key -out follower1.csr -subj "/CN=follower1"
openssl x509 -req -in follower1.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out follower1.crt -days 365 -sha256
```

### Security Best Practices

1. **Use strong certificates** with 2048+ bit keys
2. **Enable SHACL validation** for data integrity
3. **Set appropriate quotas** to prevent resource exhaustion
4. **Monitor for anomalies** using telemetry data
5. **Regular certificate rotation** for security

## Performance Tuning

### Memory Configuration

```yaml
# Large dataset configuration
environment:
  - KNOWD_STORE=disk
  - KNOWD_WAL_MAX_BYTES=134217728  # 128MB
  - KNOWD_COMPACT_THRESHOLD=0.3
  - KNOWD_PLAN_CACHE_SIZE=1024
  - KNOWD_QUERY_STREAM_MAX_BYTES=33554432  # 32MB
```

### Query Performance

```yaml
# High-performance query configuration
environment:
  - KNOWD_PLANNER_CBO=true
  - KNOWD_ANALYZE_SAMPLE=1000000
  - KNOWD_HOOKS_BATCH_SIZE=128
  - KNOWD_VIEWS_ENABLED=true
  - KNOWD_VEC_ENABLED=true
```

### Clustering Performance

```yaml
# Optimized cluster configuration
environment:
  - KNOWD_REPL_SNAPSHOT_SEC=600
  - KNOWD_REPL_STREAM_CHUNK_BYTES=2097152  # 2MB
  - KNOWD_VEC_TOPK_DEFAULT=10
```

## Monitoring and Observability

### OpenTelemetry Configuration

```yaml
# Complete OTLP setup
environment:
  - KNOWD_OTEL_EXPORTER=otlp
  - KNOWD_OTEL_ENDPOINT=https://otel-collector:4318
  - KNOWD_OTEL_SERVICE=knowd-production
  - KNOWD_PPROF_ADDR=:6060
  - KNOWD_TRACE_ADDR=:6061
```

### Metrics Collection

Knowd exports comprehensive metrics:

- **Query metrics**: execution times, result counts, cache hit rates
- **Storage metrics**: quad counts, WAL size, compaction ratios
- **Hook metrics**: evaluation counts, execution times
- **Cluster metrics**: replication lag, follower counts, network latency
- **Resource metrics**: memory usage, CPU utilization, disk I/O

### Health Checks

```bash
# Health check endpoint
curl http://localhost:8090/healthz

# Cluster status (leader only)
curl http://localhost:8090/v1/cluster/status

# Store statistics
curl http://localhost:8090/v1/store/stats
```

## Backup and Recovery

### Automated Backups

```bash
#!/bin/bash
# backup.sh
BACKUP_DIR="/backup/knowd"

# Create backup directory
mkdir -p $BACKUP_DIR/$(date +%Y%m%d)

# Backup data directory
cp -r /var/lib/knowd/* $BACKUP_DIR/$(date +%Y%m%d)/

# Backup receipts
cp -r /var/lib/knowd/receipts/* $BACKUP_DIR/$(date +%Y%m%d)/receipts/

# Create compressed archive
tar -czf $BACKUP_DIR/knowd-backup-$(date +%Y%m%d-%H%M%S).tar.gz $BACKUP_DIR/$(date +%Y%m%d)
```

### Disaster Recovery

```bash
#!/bin/bash
# restore.sh
BACKUP_FILE="knowd-backup-20250101.tar.gz"

# Stop knowd service
systemctl stop knowd

# Extract backup
tar -xzf $BACKUP_FILE -C /

# Restore permissions
chown -R knowd:knowd /var/lib/knowd

# Start knowd service
systemctl start knowd

# Verify health
curl http://localhost:8090/healthz
```

## Scaling Considerations

### Horizontal Scaling

1. **Read scaling**: Add follower nodes for query load distribution
2. **Write scaling**: Single leader handles all writes (limitation)
3. **Query optimization**: Use materialized views for complex queries
4. **Vector search**: Enable HNSW indexing for similarity queries

### Vertical Scaling

1. **Memory**: Increase for larger datasets and cache sizes
2. **CPU**: More cores for concurrent query processing
3. **Storage**: SSD for better I/O performance
4. **Network**: High-bandwidth for cluster replication

## Troubleshooting

### Common Issues

**High memory usage:**
- Switch to disk store: `KNOWD_STORE=disk`
- Reduce cache sizes: `KNOWD_PLAN_CACHE_SIZE=128`
- Enable compaction: `KNOWD_COMPACT_INTERVAL_SEC=300`

**Slow queries:**
- Enable CBO: `KNOWD_PLANNER_CBO=true`
- Increase analysis sample: `KNOWD_ANALYZE_SAMPLE=1000000`
- Use materialized views for complex queries

**Cluster sync issues:**
- Check mTLS certificates
- Verify peer addresses
- Monitor replication lag: `curl /v1/cluster/status`

**Storage corruption:**
- Check WAL integrity
- Verify snapshot consistency
- Monitor compaction ratios

### Debug Mode

```bash
# Enable debug logging
export KNOWD_LOG_LEVEL=debug

# Enable pprof profiling
export KNOWD_PPROF_ADDR=:6060

# Enable tracing
export KNOWD_TRACE_ADDR=:6061
```

## Production Checklist

- [ ] Security certificates configured and valid
- [ ] SHACL validation enabled for data integrity
- [ ] Quotas configured to prevent resource exhaustion
- [ ] Monitoring and alerting configured
- [ ] Backup strategy implemented and tested
- [ ] Health checks configured and working
- [ ] Load testing completed
- [ ] Documentation updated for deployment
- [ ] Team trained on operational procedures

## Support

For production deployment issues:

1. **Check logs**: `journalctl -u knowd -f`
2. **Verify configuration**: `knowd --help` and validate settings
3. **Monitor metrics**: Check OpenTelemetry dashboard
4. **Test connectivity**: Verify all endpoints respond correctly
5. **Review security**: Ensure certificates and permissions are correct

**Need help?** [Open an issue](https://github.com/unrdf/knowd/issues) or [start a discussion](https://github.com/unrdf/knowd/discussions).
