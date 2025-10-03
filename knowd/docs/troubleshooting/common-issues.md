# Common Issues and Solutions

This document covers frequently encountered problems when running Knowd and provides step-by-step solutions.

## Startup Issues

### "Failed to bind to address" Error

**Symptoms:**
- Server fails to start with "bind: address already in use" or similar error

**Causes:**
- Port 8090 is already occupied by another process
- Previous Knowd instance didn't shut down cleanly

**Solutions:**

1. **Check port usage:**
   ```bash
   # Find what's using port 8090
   lsof -i :8090
   netstat -tulpn | grep :8090

   # Kill conflicting process (if safe to do so)
   kill -9 <PID>
   ```

2. **Change port:**
   ```bash
   KNOWD_ADDR=:8091 ./knowd
   ```

3. **Clean shutdown previous instance:**
   ```bash
   # Send SIGTERM to gracefully shut down
   kill -TERM $(pgrep knowd)

   # Or force kill if unresponsive
   kill -9 $(pgrep knowd)
   ```

### "Permission denied" Error

**Symptoms:**
- Cannot access data directory or configuration files
- File permission errors during startup

**Causes:**
- Insufficient permissions on data directory
- Running as wrong user

**Solutions:**

1. **Fix data directory permissions:**
   ```bash
   # Ensure knowd can read/write data directory
   sudo chown -R knowd:knowd /var/lib/knowd
   sudo chmod -R 755 /var/lib/knowd
   ```

2. **Run as correct user:**
   ```bash
   # Run as knowd user
   sudo -u knowd ./knowd

   # Or fix user permissions
   usermod -a -G knowd $USER
   ```

### "Module not found" or Import Errors

**Symptoms:**
- Go build fails with "cannot find package" errors
- Runtime import failures

**Causes:**
- Missing dependencies
- Incorrect GOPATH/GOMOD setup
- Module cache corruption

**Solutions:**

1. **Update dependencies:**
   ```bash
   go mod tidy
   go mod download
   go mod verify
   ```

2. **Clean module cache:**
   ```bash
   go clean -modcache
   go mod download
   ```

3. **Check Go version:**
   ```bash
   go version  # Should be 1.22+
   ```

## Runtime Issues

### High Memory Usage

**Symptoms:**
- Knowd consuming excessive memory
- Out of memory errors
- Poor system performance

**Causes:**
- Memory leaks in application
- Large query results not being consumed
- Inefficient caching
- Large datasets in memory

**Solutions:**

1. **Monitor memory usage:**
   ```bash
   # Check process memory
   ps aux | grep knowd

   # Use pprof for detailed analysis
   curl http://localhost:6060/debug/pprof/heap > heap.prof
   go tool pprof heap.prof
   ```

2. **Adjust memory settings:**
   ```bash
   # Reduce cache sizes
   KNOWD_PLAN_CACHE_SIZE=500 \
   KNOWD_VEC_HNSW_M=12 \
   ./knowd

   # Enable GC tuning
   GOGC=200 \
   GOMEMLIMIT=8GiB \
   ./knowd
   ```

3. **Streaming large results:**
   ```bash
   # Use streaming endpoints for large queries
   curl -X POST http://localhost:8090/v1/query/stream \
        -H "Content-Type: application/json" \
        -d '{"query": "SELECT * WHERE { ?s ?p ?o }"}'
   ```

### Slow Query Performance

**Symptoms:**
- Queries taking longer than expected
- High query latency (>500ms p95)
- Poor throughput

**Causes:**
- Inefficient query plans
- Large datasets without proper indexing
- Resource contention
- Network issues in clustered deployments

**Solutions:**

1. **Analyze query plans:**
   ```bash
   # Get query plan analysis
   curl -X POST http://localhost:8090/v1/query/analyze \
        -H "Content-Type: application/json" \
        -d '{"query": "SELECT * WHERE { ?s ?p ?o }"}'
   ```

2. **Enable query optimization:**
   ```bash
   KNOWD_PLANNER_CBO=true \
   KNOWD_ANALYZE_SAMPLE=100000 \
   ./knowd
   ```

3. **Monitor cache performance:**
   ```bash
   curl "http://localhost:9090/metrics" | grep knowd_plan_cache
   ```

4. **Check resource usage:**
   ```bash
   # CPU usage
   top -p $(pgrep knowd)

   # I/O usage
   iotop -p $(pgrep knowd)
   ```

### Authentication Failures

**Symptoms:**
- HTTP 401/403 errors
- mTLS certificate validation failures
- Token authentication issues

**Causes:**
- Invalid certificates
- Expired tokens
- Incorrect authentication configuration

**Solutions:**

1. **Check certificate validity:**
   ```bash
   # Verify certificate dates
   openssl x509 -in /etc/knowd/certs/server.crt -noout -dates

   # Check certificate chain
   openssl verify -CAfile /etc/knowd/certs/ca.crt /etc/knowd/certs/server.crt
   ```

2. **Validate token configuration:**
   ```bash
   # Check token secret
   echo $KNOWD_TOKEN_SECRET

   # Test token generation
   echo -n "user:namespace" | openssl dgst -sha256 -hmac "$KNOWD_TOKEN_SECRET"
   ```

3. **Debug authentication logs:**
   ```bash
   KNOWD_LOG_LEVEL=debug ./knowd 2>&1 | grep -i auth
   ```

### Cluster Issues

**Symptoms:**
- Replication lag
- Node communication failures
- Inconsistent data across cluster

**Causes:**
- Network partitions
- Resource exhaustion
- Configuration mismatches

**Solutions:**

1. **Check cluster status:**
   ```bash
   curl http://leader:8090/v1/cluster/status
   ```

2. **Monitor replication lag:**
   ```bash
   curl "http://localhost:9090/metrics" | grep knowd_cluster_replication_lag
   ```

3. **Verify network connectivity:**
   ```bash
   # Test connectivity between nodes
   ping follower1
   nc -zv follower1 8090

   # Check firewall rules
   sudo ufw status
   ```

4. **Check resource usage:**
   ```bash
   # Monitor CPU/memory on all nodes
   ssh follower1 'top -bn1 | head -5'

   # Check disk space
   ssh follower1 'df -h /var/lib/knowd'
   ```

### Storage Issues

**Symptoms:**
- WAL corruption
- Snapshot failures
- Disk space exhaustion

**Causes:**
- Insufficient disk space
- I/O errors
- File system corruption

**Solutions:**

1. **Check disk space:**
   ```bash
   df -h /var/lib/knowd
   du -sh /var/lib/knowd/*
   ```

2. **Monitor WAL growth:**
   ```bash
   ls -lh /var/lib/knowd/wal/
   curl "http://localhost:9090/metrics" | grep knowd_store_segments
   ```

3. **Check file system integrity:**
   ```bash
   # For ext4
   sudo e2fsck -f /dev/sda1

   # For XFS
   sudo xfs_repair /dev/sda1
   ```

4. **Clean up old snapshots:**
   ```bash
   # Remove snapshots older than 7 days
   find /var/lib/knowd/snapshots/ -name "*.snapshot" -mtime +7 -delete

   # Clean WAL segments
   KNOWD_COMPACT_THRESHOLD=0.3 ./knowd
   ```

## Query Issues

### SPARQL Query Failures

**Symptoms:**
- Query parsing errors
- Execution failures
- Incorrect results

**Causes:**
- Malformed SPARQL syntax
- Missing namespaces
- Resource limits exceeded

**Solutions:**

1. **Validate SPARQL syntax:**
   ```bash
   # Test query syntax
   curl -X POST http://localhost:8090/v1/query \
        -H "Content-Type: application/json" \
        -d '{"query": "SELECT * WHERE { ?s ?p ?o }", "kind": "sparql-select"}'
   ```

2. **Check query complexity:**
   ```bash
   # Monitor query duration
   curl "http://localhost:9090/metrics" | grep knowd_query_duration

   # Enable query timeouts
   KNOWD_QUERY_TIMEOUT=30s ./knowd
   ```

3. **Debug with query analysis:**
   ```bash
   # Get detailed query plan
   curl -X POST http://localhost:8090/v1/query/analyze \
        -H "Content-Type: application/json" \
        -d '{"query": "SELECT * WHERE { ?s ?p ?o }"}'
   ```

### SHACL Validation Issues

**Symptoms:**
- Validation failures
- Performance issues with large datasets

**Causes:**
- Invalid SHACL shapes
- Complex validation rules
- Large data volumes

**Solutions:**

1. **Test SHACL shapes:**
   ```bash
   # Validate shapes syntax
   curl -X POST http://localhost:8090/v1/validate \
        -H "Content-Type: application/json" \
        -d '{
          "shapes": "@prefix sh: <http://www.w3.org/ns/shacl#> . @prefix ex: <http://example.org/> . ex:PersonShape a sh:NodeShape .",
          "data": "@prefix ex: <http://example.org/> . ex:alice a ex:Person ."
        }'
   ```

2. **Monitor validation performance:**
   ```bash
   curl "http://localhost:9090/metrics" | grep knowd_shacl
   ```

3. **Optimize validation rules:**
   ```bash
   # Use targeted shapes instead of broad validation
   # Cache frequently used shapes
   KNOWD_SHACL_CACHE_SIZE=1000 ./knowd
   ```

### Hook Execution Issues

**Symptoms:**
- Hooks not triggering
- Hook failures
- Performance degradation

**Causes:**
- Hook configuration errors
- Dependency issues
- Resource exhaustion

**Solutions:**

1. **Check hook configuration:**
   ```bash
   # Validate policy pack syntax
   KNOWD_PACKS=/path/to/packs ./knowd

   # Monitor hook execution
   curl "http://localhost:9090/metrics" | grep knowd_hook
   ```

2. **Debug hook failures:**
   ```bash
   KNOWD_LOG_LEVEL=debug ./knowd 2>&1 | grep -i hook
   ```

3. **Test hook execution:**
   ```bash
   # Manually trigger hook evaluation
   curl -X POST http://localhost:8090/v1/hooks/evaluate \
        -H "Content-Type: application/json" \
        -d '{"hook": {"kind": "sparql-ask", "query": "ASK { ?s ?p ?o }"}}'
   ```

## Network Issues

### Connection Problems

**Symptoms:**
- Clients cannot connect to Knowd
- Inter-node communication failures

**Causes:**
- Network misconfiguration
- Firewall blocking ports
- DNS resolution issues

**Solutions:**

1. **Check network configuration:**
   ```bash
   # Test local connectivity
   curl http://localhost:8090/healthz

   # Test from other machines
   curl http://knowd-server:8090/healthz

   # Check DNS resolution
   nslookup knowd-server
   ```

2. **Verify firewall rules:**
   ```bash
   # Check UFW status
   sudo ufw status

   # Check iptables
   sudo iptables -L | grep 8090

   # Allow Knowd ports
   sudo ufw allow 8090
   sudo ufw allow 8091
   ```

3. **Test network connectivity:**
   ```bash
   # Test TCP connectivity
   nc -zv localhost 8090

   # Test HTTP connectivity
   wget --spider http://localhost:8090/healthz
   ```

### TLS/SSL Issues

**Symptoms:**
- Certificate validation failures
- Connection refused on HTTPS ports

**Causes:**
- Invalid certificates
- Wrong certificate paths
- Certificate expiry

**Solutions:**

1. **Check certificate validity:**
   ```bash
   # Verify certificate details
   openssl x509 -in /etc/knowd/certs/server.crt -text -noout

   # Check certificate expiry
   openssl x509 -in /etc/knowd/certs/server.crt -noout -enddate

   # Test certificate chain
   openssl verify -CAfile /etc/knowd/certs/ca.crt /etc/knowd/certs/server.crt
   ```

2. **Fix certificate paths:**
   ```bash
   # Ensure correct certificate paths
   ls -la /etc/knowd/certs/

   # Update environment variables
   KNOWD_MTLS_CERT=/etc/knowd/certs/server.crt \
   KNOWD_MTLS_KEY=/etc/knowd/certs/server.key \
   KNOWD_MTLS_CA=/etc/knowd/certs/ca.crt \
   ./knowd
   ```

## Performance Issues

### High CPU Usage

**Symptoms:**
- Knowd consuming excessive CPU
- Slow response times

**Causes:**
- Inefficient query plans
- Large concurrent requests
- Background processing overload

**Solutions:**

1. **Profile CPU usage:**
   ```bash
   curl http://localhost:6060/debug/pprof/profile?seconds=30 > cpu.prof
   go tool pprof cpu.prof
   ```

2. **Optimize query patterns:**
   ```bash
   # Enable query optimization
   KNOWD_PLANNER_CBO=true \
   KNOWD_ANALYZE_SAMPLE=100000 \
   ./knowd
   ```

3. **Adjust concurrency:**
   ```bash
   # Limit concurrent requests
   KNOWD_HTTP_MAX_CONNS=1000 \
   KNOWD_HOOKS_BATCH_SIZE=32 \
   ./knowd
   ```

### High Latency

**Symptoms:**
- Slow API responses
- Poor user experience

**Causes:**
- Network latency
- Database I/O bottlenecks
- Inefficient caching

**Solutions:**

1. **Monitor latency metrics:**
   ```bash
   curl "http://localhost:9090/metrics" | grep knowd_query_duration
   ```

2. **Optimize storage I/O:**
   ```bash
   # Use faster storage
   KNOWD_DATA_DIR=/mnt/nvme/knowd ./knowd

   # Tune WAL settings
   KNOWD_WAL_MAX_BYTES=134217728 ./knowd
   ```

3. **Improve caching:**
   ```bash
   # Increase cache sizes
   KNOWD_PLAN_CACHE_SIZE=2000 \
   KNOWD_HOOKS_BATCH_SIZE=128 \
   ./knowd
   ```

## Configuration Issues

### Environment Variable Problems

**Symptoms:**
- Settings not taking effect
- Unexpected behavior

**Causes:**
- Environment variables not set correctly
- Variable precedence issues

**Solutions:**

1. **Check environment variables:**
   ```bash
   # List all Knowd environment variables
   env | grep KNOWD

   # Check specific variable
   echo $KNOWD_STORE

   # Verify variable is exported
   export KNOWD_STORE=disk
   ```

2. **Debug configuration loading:**
   ```bash
   KNOWD_LOG_LEVEL=debug ./knowd 2>&1 | grep -i config
   ```

3. **Validate configuration files:**
   ```bash
   # Check YAML syntax
   python3 -c "import yaml; yaml.safe_load(open('config.yaml'))"

   # Validate JSON
   cat config.json | jq .
   ```

### Dependency Issues

**Symptoms:**
- Build failures
- Runtime import errors

**Causes:**
- Missing system dependencies
- Incorrect Go module setup

**Solutions:**

1. **Install system dependencies:**
   ```bash
   # For Ubuntu/Debian
   sudo apt update
   sudo apt install -y build-essential git

   # For macOS
   brew install go git
   ```

2. **Fix Go module issues:**
   ```bash
   go mod tidy
   go mod verify
   go mod download
   ```

3. **Check Go installation:**
   ```bash
   go version
   go env GOPATH GOMOD
   ```

## Monitoring Issues

### Metrics Not Available

**Symptoms:**
- `/metrics` endpoint returns errors
- Prometheus cannot scrape metrics

**Causes:**
- Metrics endpoint not enabled
- Incorrect configuration

**Solutions:**

1. **Enable metrics endpoint:**
   ```bash
   KNOWD_METRICS_ADDR=:9090 ./knowd
   curl http://localhost:9090/metrics
   ```

2. **Check Prometheus configuration:**
   ```yaml
   scrape_configs:
     - job_name: 'knowd'
       static_configs:
         - targets: ['localhost:9090']
       scrape_interval: 15s
   ```

3. **Verify metrics collection:**
   ```bash
   curl http://localhost:9090/metrics | head -20
   ```

### Alerting Not Working

**Symptoms:**
- Alerts not firing
- No notifications received

**Causes:**
- Alert rules not configured
- Notification channels not set up

**Solutions:**

1. **Test alert rules:**
   ```bash
   # Check if metrics match alert conditions
   curl "http://localhost:9090/metrics" | grep knowd_query_duration

   # Test Prometheus queries
   curl "http://prometheus:9090/api/v1/query?query=up{job=\"knowd\"}"
   ```

2. **Configure AlertManager:**
   ```yaml
   # Test AlertManager configuration
   curl -X POST http://alertmanager:9093/api/v1/alerts \
        -H "Content-Type: application/json" \
        -d '[{"labels":{"alertname":"TestAlert"}}]'
   ```

## Data Recovery

### WAL Corruption

**Symptoms:**
- WAL files corrupted
- Cannot start due to WAL errors

**Causes:**
- Disk failure
- Power loss during writes
- File system corruption

**Solutions:**

1. **Check WAL integrity:**
   ```bash
   ls -la /var/lib/knowd/wal/
   file /var/lib/knowd/wal/*.wal
   ```

2. **Recover from snapshot:**
   ```bash
   # Find latest snapshot
   ls -la /var/lib/knowd/snapshots/

   # Restore from snapshot
   KNOWD_DATA_DIR=/var/lib/knowd-restored \
   KNOWD_STORE=disk \
   ./knowd
   ```

3. **Manual WAL repair:**
   ```bash
   # Backup corrupted WAL
   cp /var/lib/knowd/wal/ /var/lib/knowd/wal.backup/

   # Remove corrupted files
   rm /var/lib/knowd/wal/*.wal

   # Restart to create new WAL
   ./knowd
   ```

### Data Loss

**Symptoms:**
- Missing data after restart
- Inconsistent query results

**Causes:**
- Incomplete transactions
- Snapshot corruption
- Storage backend failure

**Solutions:**

1. **Check data integrity:**
   ```bash
   # Verify store statistics
   curl http://localhost:8090/v1/store/stats

   # Check for data consistency
   KNOWD_STORE=disk KNOWD_DATA_DIR=/var/lib/knowd ./knowd
   ```

2. **Restore from backup:**
   ```bash
   # Stop Knowd
   systemctl stop knowd

   # Restore from backup
   cp -r /backup/knowd-latest/* /var/lib/knowd/

   # Start Knowd
   systemctl start knowd
   ```

3. **Validate data integrity:**
   ```bash
   # Run integrity checks
   curl -X POST http://localhost:8090/v1/admin/validate
   ```

## Getting Help

### Logs Analysis

**Enable debug logging:**
```bash
KNOWD_LOG_LEVEL=debug \
KNOWD_LOG_FORMAT=json \
./knowd 2>&1 | tee knowd-debug.log
```

**Search logs for patterns:**
```bash
# Find errors
grep -i error /var/log/knowd/knowd.log

# Find slow operations
grep "duration_ms" /var/log/knowd/knowd.log | awk '$NF > 1000 {print}'

# Find specific issues
grep -A5 -B5 "authentication failed" /var/log/knowd/knowd.log
```

### Support Channels

**Issue reporting:**
- Create GitHub issues for bugs and feature requests
- Include logs, configuration, and reproduction steps
- Use issue templates for consistent reporting

**Community support:**
- GitHub Discussions for questions and ideas
- Stack Overflow for technical questions
- Community chat for real-time help

### Professional Support

For enterprise deployments:
- Contact the Knowd team for support contracts
- Schedule architecture reviews
- Request custom feature development
- Get priority bug fixes and security updates

This troubleshooting guide provides comprehensive solutions for common issues encountered when running Knowd in production environments.
