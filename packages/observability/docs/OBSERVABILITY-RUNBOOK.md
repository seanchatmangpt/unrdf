# UNRDF Observability Runbook

Operational procedures for monitoring, troubleshooting, and maintaining UNRDF observability infrastructure.

## Quick Reference

| Alert | Severity | Response Time | Action |
|-------|----------|---------------|--------|
| ServiceDown | Critical | 1 min | [Service Recovery](#service-down) |
| InjectionAttempt | Critical | 2 min | [Security Incident](#injection-attempt) |
| CriticalMemoryUsage | Critical | 5 min | [Memory Issues](#high-memory-usage) |
| HighP99Latency | Critical | 5 min | [Performance](#high-latency) |
| LowSuccessRate | Warning | 10 min | [Success Rate](#low-success-rate) |

## Incident Response

### Service Down

**Alert**: `ServiceDown`
**Severity**: Critical
**Response Time**: 1 minute

#### Symptoms
- Prometheus shows `up{job="unrdf-*"} == 0`
- No metrics received in last 60 seconds
- Health check endpoints unreachable

#### Diagnosis

```bash
# Check service status
systemctl status unrdf-core

# Check logs
journalctl -u unrdf-core -n 100 --no-pager

# Check process
ps aux | grep unrdf

# Check port availability
netstat -tulpn | grep 9464
```

#### Resolution

**Option 1: Restart Service**
```bash
# Graceful restart
systemctl restart unrdf-core

# Verify recovery
curl http://localhost:9464/metrics
```

**Option 2: Check Configuration**
```bash
# Validate config
node --check /path/to/unrdf/index.mjs

# Check environment variables
env | grep UNRDF
```

**Option 3: Resource Issues**
```bash
# Check disk space
df -h

# Check memory
free -h

# Check OOM killer
dmesg | grep -i "out of memory"
```

#### Prevention
- Set up health check monitoring (every 10s)
- Configure auto-restart on failure
- Set resource limits appropriately

### Injection Attempt

**Alert**: `InjectionAttempt`
**Severity**: Critical
**Response Time**: 2 minutes

#### Symptoms
- Alert: `increase(event_total{event_type="security.injection.attempt"}[5m]) > 0`
- Security events in Grafana dashboard
- Unusual query patterns

#### Diagnosis

```bash
# Query injection events
curl "http://prometheus:9090/api/v1/query?query=event_total{event_type=\"security.injection.attempt\"}"

# Check event details via API
curl http://unrdf-api:3000/api/events?type=security.injection.attempt&limit=10

# Review logs
grep "injection.attempt" /var/log/unrdf/security.log
```

#### Resolution

**Immediate Actions**:
1. **Block IP Address**:
   ```bash
   # Add to firewall
   sudo ufw deny from <ATTACKER_IP>

   # Or via API
   curl -X POST http://sidecar:3000/api/admin/blacklist \
     -H "Content-Type: application/json" \
     -d '{"ip": "<ATTACKER_IP>", "reason": "injection_attempt"}'
   ```

2. **Review Attack Payload**:
   ```javascript
   // Via custom events API
   const events = await getEventsByType('security.injection.attempt');
   events.forEach(e => {
     console.log('Attack type:', e.attributes['injection.type']);
     console.log('Payload hash:', e.attributes['injection.payload_hash']);
     console.log('IP:', e.attributes['injection.ip_address']);
   });
   ```

3. **Validate Input Sanitization**:
   ```bash
   # Run security audit
   npm run security:audit

   # Check SPARQL injection protection
   npm run test:security:sparql-injection
   ```

#### Prevention
- Enable strict input validation
- Use parameterized queries
- Implement rate limiting
- Monitor for abnormal patterns

### High Memory Usage

**Alert**: `HighMemoryUsage` / `CriticalMemoryUsage`
**Severity**: Warning / Critical
**Response Time**: 5 minutes

#### Symptoms
- Alert: `resource_heap_used_bytes / resource_heap_total_bytes > 0.85`
- Slow performance
- Frequent GC activity

#### Diagnosis

```bash
# Check current memory usage
curl http://localhost:9464/metrics | grep resource_heap

# Take heap snapshot
kill -SIGUSR2 <PID>
# Snapshot saved to: /tmp/heapsnapshot-<timestamp>.heapsnapshot

# Analyze with Chrome DevTools
# 1. Open Chrome DevTools
# 2. Memory tab > Load snapshot
```

#### Resolution

**Option 1: Immediate Relief**
```bash
# Trigger manual GC (if --expose-gc enabled)
curl -X POST http://localhost:3000/api/admin/gc

# Clear caches
curl -X POST http://localhost:3000/api/admin/clear-cache
```

**Option 2: Identify Memory Leaks**
```bash
# Run leak detection
npm run memory:leak-check

# Profile memory usage
node --inspect index.mjs
# Connect Chrome DevTools to chrome://inspect
```

**Option 3: Increase Memory**
```bash
# Update systemd service
sudo systemctl edit unrdf-core

# Add memory limit
[Service]
Environment="NODE_OPTIONS=--max-old-space-size=4096"

# Restart
sudo systemctl restart unrdf-core
```

#### Prevention
- Set appropriate memory limits
- Monitor memory growth trends
- Implement cache eviction policies
- Use streaming for large datasets

### High Latency

**Alert**: `HighP95Latency` / `HighP99Latency`
**Severity**: Warning / Critical
**Response Time**: 5 minutes

#### Symptoms
- Alert: `latency_p95_ms > 1000`
- Slow query responses
- Timeout warnings in logs

#### Diagnosis

```bash
# Check latency percentiles
curl -s "http://prometheus:9090/api/v1/query?query=latency_p95_ms" | jq

# Identify slow operations
curl -s "http://prometheus:9090/api/v1/query?query=topk(10,%20latency_p95_ms)" | jq

# Check slow query events
curl http://localhost:3000/api/events?type=performance.slow_query&limit=20
```

#### Resolution

**Option 1: Query Optimization**
```bash
# Analyze slow queries
npm run query:analyze

# Check query plan
curl -X POST http://localhost:3000/api/query/explain \
  -H "Content-Type: application/json" \
  -d '{"query": "SELECT * WHERE { ?s ?p ?o }"}'
```

**Option 2: Scale Resources**
```bash
# Increase worker threads
export UNRDF_WORKERS=4

# Restart service
systemctl restart unrdf-core
```

**Option 3: Enable Caching**
```bash
# Enable query cache
curl -X POST http://localhost:3000/api/admin/cache/enable

# Set cache TTL
curl -X POST http://localhost:3000/api/admin/cache/config \
  -d '{"ttl": 300}'
```

#### Prevention
- Optimize common query patterns
- Use query complexity limits
- Implement connection pooling
- Enable aggressive caching

### Low Success Rate

**Alert**: `LowSuccessRate` / `CriticalSuccessRate`
**Severity**: Warning / Critical
**Response Time**: 10 minutes

#### Symptoms
- Alert: `rate(business_operations_total{result="success"}[5m]) / rate(business_operations_total[5m]) < 0.95`
- Increased error rate
- User reports

#### Diagnosis

```bash
# Check error distribution
curl -s "http://prometheus:9090/api/v1/query?query=sum%20by%20(error_type)%20(rate(business_failures_by_type%5B5m%5D))" | jq

# Review error logs
journalctl -u unrdf-core --since "10 minutes ago" | grep ERROR

# Check dependencies
curl http://localhost:3000/api/health/dependencies
```

#### Resolution

**Option 1: Identify Root Cause**
```bash
# Group errors by type
curl "http://prometheus:9090/api/v1/query?query=topk(5,%20sum%20by%20(error_type)%20(rate(business_failures_by_type%5B5m%5D)))"

# Check error traces
curl http://localhost:16686/api/traces?service=unrdf-core&limit=20
```

**Option 2: Rollback Recent Changes**
```bash
# Check recent deployments
git log --since="1 hour ago" --oneline

# Rollback if needed
git revert <commit-hash>
npm run deploy
```

**Option 3: Circuit Breaker**
```bash
# Enable circuit breaker for failing dependencies
curl -X POST http://localhost:3000/api/admin/circuit-breaker/enable \
  -d '{"service": "external-api", "threshold": 0.5}'
```

#### Prevention
- Comprehensive error handling
- Graceful degradation
- Dependency health checks
- Canary deployments

## Monitoring Checklist

### Daily Checks
- [ ] Review Grafana dashboard
- [ ] Check alert status in Alertmanager
- [ ] Verify metrics ingestion (no staleness)
- [ ] Review security events
- [ ] Check resource utilization trends

### Weekly Checks
- [ ] Analyze latency trends
- [ ] Review error patterns
- [ ] Check disk space on Prometheus/Jaeger
- [ ] Validate backup processes
- [ ] Review alert tuning

### Monthly Checks
- [ ] Review dashboard effectiveness
- [ ] Update alert thresholds based on trends
- [ ] Analyze cost/performance trade-offs
- [ ] Update runbook based on incidents
- [ ] Review sampling strategies

## Maintenance Procedures

### Prometheus Data Retention

```bash
# Check disk usage
df -h /var/lib/prometheus

# Compact old data
curl -X POST http://prometheus:9090/api/v1/admin/tsdb/clean_tombstones

# Adjust retention (in prometheus.yml)
storage:
  tsdb:
    retention.time: 30d
    retention.size: 50GB
```

### Jaeger Storage Cleanup

```bash
# Clean old traces (Cassandra)
docker exec jaeger-cassandra cqlsh -e \
  "TRUNCATE jaeger_v1_dc1.traces WHERE timestamp < dateOf(now()) - 604800000;"

# Or use Jaeger's built-in cleanup
curl -X DELETE "http://jaeger:16686/api/traces?service=unrdf-core&lookback=30d"
```

### OTEL Collector Restart

```bash
# Graceful restart
docker restart otel-collector

# Verify health
curl http://localhost:13133/
```

## Performance Tuning

### Sampling Strategy Adjustment

```javascript
// Update sampling rates based on load
const tracing = createDistributedTracing({
  sampling: {
    defaultRate: 0.001,   // 0.1% for high-volume production
    errorRate: 1.0,       // Always sample errors
    slowThreshold: 5000,  // 5s threshold
    slowRate: 0.5,        // 50% of slow operations
  },
});
```

### Metric Aggregation

```yaml
# Prometheus recording rules for faster queries
groups:
  - name: unrdf_aggregates
    interval: 30s
    rules:
      - record: job:latency_p95_ms:5m
        expr: histogram_quantile(0.95, rate(latency_operation_duration_ms_bucket[5m]))

      - record: job:success_rate:5m
        expr: |
          rate(business_operations_total{result="success"}[5m]) /
          rate(business_operations_total[5m])
```

### Resource Optimization

```bash
# Optimize Prometheus scrape interval
# High-priority: 10s
# Medium: 30s
# Low: 60s

# Reduce cardinality
# Drop unnecessary labels
metric_relabel_configs:
  - source_labels: [__name__]
    regex: '.*_bucket|.*_sum|.*_count'
    action: drop
```

## Backup and Recovery

### Prometheus Backup

```bash
# Snapshot current data
curl -X POST http://prometheus:9090/api/v1/admin/tsdb/snapshot

# Backup snapshot
tar -czf prometheus-backup-$(date +%Y%m%d).tar.gz \
  /var/lib/prometheus/snapshots/

# Upload to S3
aws s3 cp prometheus-backup-$(date +%Y%m%d).tar.gz \
  s3://backups/prometheus/
```

### Jaeger Backup

```bash
# Export traces to storage
docker exec jaeger-collector \
  jaeger-tools export --service unrdf-core \
  --output /backups/traces-$(date +%Y%m%d).json
```

### Restore Procedure

```bash
# Stop Prometheus
systemctl stop prometheus

# Restore data
tar -xzf prometheus-backup-YYYYMMDD.tar.gz -C /var/lib/prometheus/

# Start Prometheus
systemctl start prometheus
```

## Escalation

### L1 Support
- Check alert status
- Review runbook procedures
- Attempt standard remediation

### L2 Support (Escalate if)
- Alert persists >15 minutes
- Multiple cascading failures
- Security incidents
- Unknown root cause

### L3 Support (Escalate if)
- System-wide outage
- Data loss suspected
- Critical security breach
- Architecture-level issues

## Contact Information

```yaml
Teams:
  Platform:
    Slack: "#unrdf-platform"
    Pagerduty: "@unrdf-oncall"

  Security:
    Slack: "#security-incidents"
    Email: "security@example.com"

  Infrastructure:
    Slack: "#infrastructure"
    Pagerduty: "@infra-oncall"
```

## Additional Resources

- [Observability Patterns](./OBSERVABILITY-PATTERNS.md)
- [Prometheus Querying](https://prometheus.io/docs/prometheus/latest/querying/basics/)
- [Jaeger UI Guide](https://www.jaegertracing.io/docs/latest/frontend-ui/)
- [Grafana Troubleshooting](https://grafana.com/docs/grafana/latest/troubleshooting/)
