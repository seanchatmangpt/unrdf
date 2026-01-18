# UNRDF v6 Rollback Runbook

## Overview

This runbook provides step-by-step instructions for rolling back a failed UNRDF v6 deployment to the previous stable version.

## When to Rollback

Initiate rollback immediately if ANY of the following occur:

- [ ] Health checks fail for more than 10 minutes
- [ ] Error rate exceeds 10% of requests
- [ ] API P95 latency exceeds 5 seconds
- [ ] Critical data corruption detected
- [ ] Security vulnerability discovered
- [ ] Complete service outage
- [ ] Deployment script fails critically

## Rollback Decision Matrix

| Severity | Error Rate | Latency | Action | Timeline |
|----------|-----------|---------|--------|----------|
| P0 Critical | > 10% | > 5s | ROLLBACK NOW | < 5 min |
| P1 High | > 5% | > 2s | Rollback | < 15 min |
| P2 Medium | > 1% | > 1s | Investigate | 30 min |
| P3 Low | < 1% | < 1s | Monitor | 1 hour |

## Pre-Rollback Checklist

### 1. Verify Rollback is Necessary

```bash
# Check current error rate
curl -s http://localhost:9090/api/v1/query?query=rate\(unrdf_errors_total[5m]\)

# Check current latency
curl -s http://localhost:9090/api/v1/query?query=histogram_quantile\(0.95,rate\(unrdf_api_duration_seconds_bucket[5m]\)\)

# Check health status
./scripts/health-check.sh
```

### 2. Notify Stakeholders

```bash
# Immediate notification
curl -X POST $SLACK_WEBHOOK_URL \
  -H 'Content-Type: application/json' \
  -d '{"text":"🚨 ROLLBACK INITIATED - Production deployment failed"}'

# Page on-call if P0
curl -X POST https://events.pagerduty.com/v2/enqueue \
  -H "Content-Type: application/json" \
  -d '{
    "routing_key": "'$PAGERDUTY_KEY'",
    "event_action": "trigger",
    "payload": {
      "summary": "UNRDF Production Rollback",
      "severity": "critical",
      "source": "deployment-system"
    }
  }'
```

### 3. Verify Backup Exists

```bash
# Check latest backup
ls -lh /var/backups/unrdf/

# Verify backup timestamp
cat /var/backups/unrdf/latest
```

## Rollback Process

### Step 1: Execute Rollback Script

```bash
# Automated rollback (recommended)
./scripts/rollback.sh --auto

# Interactive rollback (manual confirmation)
./scripts/rollback.sh
```

**Expected Output:**
```
[2026-01-11 12:30:00] UNRDF v6 Rollback Starting
=========================================
Latest backup: unrdf-backup-20260111-115500
[2026-01-11 12:30:05] Stopping current services...
[SUCCESS] Services stopped
[2026-01-11 12:30:15] Restoring backup: unrdf-backup-20260111-115500
[SUCCESS] Environment restored
[SUCCESS] Data restored
[2026-01-11 12:30:25] Pulling previous Docker images...
[2026-01-11 12:30:45] Starting services with previous configuration...
[SUCCESS] Services started
[2026-01-11 12:30:50] Running health checks...
[SUCCESS] Health check passed
[2026-01-11 12:31:00] Verifying rollback...
[SUCCESS] Rollback verified
=========================================
[SUCCESS] Rollback completed successfully!
=========================================
```

### Step 2: Verify Rollback

```bash
# Run health check
./scripts/health-check.sh https://production.example.com

# Verify version
curl https://production.example.com/api/version

# Check container status
docker-compose ps

# Verify all services running
docker-compose ps | grep -c "Up" # Should match container count
```

### Step 3: Verify Data Integrity

```bash
# Test SPARQL query
curl -X POST https://production.example.com/sparql \
  -H "Content-Type: application/sparql-query" \
  -d "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# Verify receipts
curl https://production.example.com/api/receipts/count

# Test hook execution
curl -X POST https://production.example.com/api/hooks/test
```

### Step 4: Monitor Metrics

Check Grafana for improvement in:
- [ ] Error rate returning to < 0.01/s
- [ ] P95 latency returning to < 100ms
- [ ] Request success rate > 99%
- [ ] All services healthy

### Step 5: Confirm Stability

**Wait 30 minutes** and monitor:

```bash
# Check error rate over last 30 minutes
curl "http://localhost:9090/api/v1/query?query=rate(unrdf_errors_total[30m])"

# Check for any new alerts
curl http://localhost:9093/api/v2/alerts

# Review logs
docker-compose logs --since 30m unrdf | grep -i "error\|warn"
```

### Step 6: Update Status

```bash
# Notify team of successful rollback
curl -X POST $SLACK_WEBHOOK_URL \
  -H 'Content-Type: application/json' \
  -d '{"text":"✅ ROLLBACK COMPLETED - System stable on previous version"}'

# Create Grafana annotation
curl -X POST https://grafana.example.com/api/annotations \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $GRAFANA_API_KEY" \
  -d '{
    "text": "Production rollback completed",
    "tags": ["rollback", "production"],
    "time": '$(date +%s000)'
  }'

# Resolve PagerDuty incident
curl -X POST https://events.pagerduty.com/v2/enqueue \
  -H "Content-Type: application/json" \
  -d '{
    "routing_key": "'$PAGERDUTY_KEY'",
    "event_action": "resolve",
    "dedup_key": "deployment-rollback-'$(date +%Y%m%d)'"
  }'
```

## Manual Rollback (If Script Fails)

### 1. Stop Current Services

```bash
docker-compose down --timeout 30
```

### 2. Restore Configuration

```bash
# Get backup name
backup_name=$(cat /var/backups/unrdf/latest)

# Restore environment
cp /var/backups/unrdf/$backup_name-env .env.production

# Restore data
tar -xzf /var/backups/unrdf/$backup_name-data.tar.gz -C /
```

### 3. Pull Previous Images

```bash
# List available tags
docker images | grep unrdf

# Tag previous version
docker tag <previous-image-id> unrdf:rollback

# Update docker-compose to use rollback tag
sed -i 's/image: unrdf:.*/image: unrdf:rollback/' docker-compose.yml
```

### 4. Start Services

```bash
docker-compose up -d
```

### 5. Verify

```bash
./scripts/health-check.sh
```

## Post-Rollback Actions

### Immediate (Within 1 Hour)

1. **Root Cause Analysis**
   - Collect all logs from failed deployment
   - Review deployment timeline
   - Identify what changed
   - Document failure mode

2. **Preserve Evidence**
   ```bash
   # Save failed deployment logs
   mkdir -p /var/log/unrdf/incidents/$(date +%Y%m%d-%H%M%S)
   docker-compose logs > /var/log/unrdf/incidents/$(date +%Y%m%d-%H%M%S)/docker-logs.txt

   # Save metrics snapshot
   curl "http://localhost:9090/api/v1/query?query=rate(unrdf_errors_total[1h])" \
     > /var/log/unrdf/incidents/$(date +%Y%m%d-%H%M%S)/metrics.json
   ```

3. **Communicate Status**
   - Update status page
   - Send post-mortem notice
   - Schedule incident review

### Within 24 Hours

1. **Create Incident Report**
   - Timeline of events
   - Root cause analysis
   - Impact assessment
   - Remediation steps

2. **Update Runbooks**
   - Document new failure mode
   - Update troubleshooting section
   - Add prevention measures

3. **Plan Fix**
   - Identify fix requirements
   - Create fix timeline
   - Schedule next deployment attempt

## Troubleshooting Rollback Issues

### Rollback Script Fails

**Symptom:** `rollback.sh` exits with error

**Resolution:**
1. Check backup directory exists: `ls /var/backups/unrdf/`
2. Verify backup files: `ls -lh /var/backups/unrdf/unrdf-backup-*`
3. Check disk space: `df -h`
4. Use manual rollback procedure
5. Contact DevOps lead immediately

### Containers Won't Start After Rollback

**Symptom:** Docker containers fail to start

**Resolution:**
1. Check logs: `docker-compose logs`
2. Verify images exist: `docker images | grep unrdf`
3. Check ports are free: `netstat -tulpn | grep -E '3000|8080|9090'`
4. Verify environment file: `cat .env.production`
5. Try starting one service at a time

### Data Corruption Detected

**Symptom:** Database or receipts corrupted

**Resolution:**
1. **STOP ALL WRITES IMMEDIATELY**
2. Restore from oldest known-good backup
3. Check backup integrity before restore
4. Consider point-in-time recovery if available
5. Escalate to database team

### Health Checks Still Failing

**Symptom:** Health checks fail after rollback

**Resolution:**
1. Verify correct version rolled back: `curl /api/version`
2. Check all dependencies are up
3. Verify network connectivity
4. Check configuration for corruption
5. Consider restoring from even older backup
6. **DO NOT** attempt another deployment

## Rollback Metrics

Track these metrics for each rollback:

| Metric | Target | Actual |
|--------|--------|--------|
| Time to detect issue | < 5 min | ___ |
| Time to decision | < 2 min | ___ |
| Time to execute rollback | < 10 min | ___ |
| Time to verify | < 5 min | ___ |
| Total rollback time | < 20 min | ___ |
| Data loss | 0 records | ___ |
| Downtime | < 15 min | ___ |

## Prevention

After rollback, implement these preventions:

1. **Enhanced Testing**
   - Add test coverage for failure scenario
   - Extend integration tests
   - Add performance regression tests

2. **Improved Monitoring**
   - Add alerts for new failure mode
   - Improve detection time
   - Add canary deployment checks

3. **Process Improvements**
   - Update deployment checklist
   - Add validation gates
   - Implement blue-green deployment

## Emergency Contacts

| Role | Name | Contact | Escalation |
|------|------|---------|-----------|
| On-Call Engineer | TBD | PagerDuty | Primary |
| DevOps Lead | TBD | Slack @devops | Secondary |
| Backend Lead | TBD | Slack @backend | Secondary |
| VP Engineering | TBD | Phone | Escalation |

## Related Documentation

- [Deployment Runbook](./DEPLOYMENT_RUNBOOK.md)
- [Incident Response](./INCIDENT_RESPONSE.md)
- [Monitoring Guide](./MONITORING_GUIDE.md)

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-11 | 1.0 | Initial runbook | DevOps Team |
