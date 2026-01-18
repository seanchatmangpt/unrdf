# UNRDF v6 Production Deployment Runbook

## Overview

This runbook provides step-by-step instructions for deploying UNRDF v6 to production environments.

## Prerequisites

### Required Access
- [ ] SSH access to production servers
- [ ] Docker Registry credentials (GitHub Container Registry)
- [ ] AWS credentials (if using AWS)
- [ ] Slack webhook access for notifications
- [ ] PagerDuty access for critical alerts

### Required Tools
- [ ] Docker >= 20.10
- [ ] Docker Compose >= 2.0
- [ ] Git >= 2.30
- [ ] curl
- [ ] ssh client

### Required Knowledge
- Docker and container orchestration
- UNRDF v6 architecture
- Production environment topology
- Rollback procedures

## Pre-Deployment Checklist

### 1. Code Verification
```bash
# Verify branch/tag
git checkout <version-tag>
git pull origin <version-tag>

# Verify commit
git log -1
git verify-tag <version-tag>  # If tag is signed
```

### 2. Configuration Validation
```bash
# Validate environment configuration
node scripts/validate-env-config.mjs

# Verify secrets are set
grep -v '^#' .env.production | grep '=$' && echo "Missing values!" || echo "OK"
```

### 3. Build Validation
```bash
# Run tests
timeout 120s pnpm test:fast

# Run linter
timeout 60s pnpm lint

# Run security audit
pnpm audit --prod
```

### 4. Infrastructure Check
```bash
# Check disk space (require 10GB+)
df -h /

# Check memory
free -h

# Check Docker daemon
docker info

# Check network connectivity
curl -I https://ghcr.io
```

### 5. Monitoring Setup
```bash
# Verify Grafana is accessible
curl -f https://grafana.example.com/api/health

# Verify Prometheus is accessible
curl -f https://prometheus.example.com/-/healthy

# Verify AlertManager is configured
curl -f https://alertmanager.example.com/-/healthy
```

## Deployment Process

### Step 1: Notify Team

```bash
# Post to Slack
curl -X POST $SLACK_WEBHOOK_URL \
  -H 'Content-Type: application/json' \
  -d '{"text":"🚀 Production deployment starting for UNRDF v6.0.0-rc.1"}'
```

### Step 2: Enable Maintenance Mode (Optional)

```bash
# Create maintenance page
echo "System maintenance in progress" > /var/www/maintenance.html

# Configure load balancer to show maintenance page
# (Instructions specific to your LB)
```

### Step 3: Create Backup

```bash
# Automatic backup is created by deploy script
# Manual backup (if needed):
timestamp=$(date +%Y%m%d-%H%M%S)
tar -czf /var/backups/unrdf/manual-backup-$timestamp.tar.gz \
  /app/data \
  .env.production \
  docker-compose.yml
```

### Step 4: Run Deployment Script

```bash
# Set required environment variables
export IMAGE_TAG="ghcr.io/unrdf/unrdf:6.0.0-rc.1"
export DEPLOY_HOST="production.example.com"
export HEALTH_CHECK_URL="https://production.example.com/health"

# Execute deployment
./scripts/deploy-production.sh
```

**Expected Output:**
```
[2026-01-11 12:00:00] Running pre-deployment checks...
[SUCCESS] Pre-deployment checks passed
[2026-01-11 12:00:05] Creating backup...
[SUCCESS] Backup created: unrdf-backup-20260111-120005
[2026-01-11 12:00:10] Pulling latest Docker images...
[SUCCESS] Images pulled successfully
[2026-01-11 12:00:30] Stopping services gracefully...
[SUCCESS] Services stopped
[2026-01-11 12:00:40] Starting services...
[SUCCESS] Services started
[2026-01-11 12:00:45] Running health checks...
[SUCCESS] Health check passed
[2026-01-11 12:01:00] Verifying deployment...
[SUCCESS] Deployment verified
[2026-01-11 12:01:05] Running post-deployment tasks...
[SUCCESS] Post-deployment tasks completed
=========================================
[SUCCESS] Deployment completed successfully!
=========================================
```

### Step 5: Verify Deployment

```bash
# Run comprehensive health check
./scripts/health-check.sh https://production.example.com

# Check all endpoints
curl -f https://production.example.com/health
curl -f https://production.example.com/metrics
curl -f https://production.example.com/api

# Verify containers
docker-compose ps

# Check logs for errors
docker-compose logs --tail=100 unrdf
```

### Step 6: Smoke Tests

```bash
# Test SPARQL endpoint
curl -X POST https://production.example.com/sparql \
  -H "Content-Type: application/sparql-query" \
  -d "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# Test receipt creation
curl -X POST https://production.example.com/api/receipts \
  -H "Content-Type: application/json" \
  -d '{"operation":"create","entityType":"Triple"}'

# Test hook execution
curl -X POST https://production.example.com/api/hooks/test
```

### Step 7: Monitor Metrics

Check Grafana dashboard for:
- [ ] API request rate
- [ ] API latency (P95 < 1s)
- [ ] Error rate (< 0.05/s)
- [ ] Memory usage (< 80%)
- [ ] CPU usage (< 80%)
- [ ] Receipt creation rate

### Step 8: Disable Maintenance Mode

```bash
# Remove maintenance page
rm /var/www/maintenance.html

# Restore normal load balancer routing
# (Instructions specific to your LB)
```

### Step 9: Post-Deployment Notification

```bash
# Notify team of success
curl -X POST $SLACK_WEBHOOK_URL \
  -H 'Content-Type: application/json' \
  -d '{"text":"✅ Production deployment completed successfully for UNRDF v6.0.0-rc.1"}'

# Create deployment annotation in Grafana
curl -X POST https://grafana.example.com/api/annotations \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $GRAFANA_API_KEY" \
  -d '{
    "text": "Production deployment: v6.0.0-rc.1",
    "tags": ["deployment", "production"],
    "time": '$(date +%s000)'
  }'
```

## Post-Deployment Monitoring

### First 30 Minutes
- Monitor Grafana dashboard continuously
- Watch logs for errors: `docker-compose logs -f unrdf`
- Check AlertManager for any alerts
- Monitor response times

### First 24 Hours
- Review metrics hourly
- Check error rates
- Monitor resource usage
- Review logs for warnings

### Performance Baselines
| Metric | Target | Alert Threshold |
|--------|--------|-----------------|
| API P95 Latency | < 100ms | > 1s |
| Error Rate | < 0.01/s | > 0.05/s |
| Memory Usage | < 60% | > 80% |
| CPU Usage | < 50% | > 80% |
| Receipt Creation | < 1ms | > 5ms |

## Troubleshooting

### Deployment Script Fails

**Symptom:** `deploy-production.sh` exits with error

**Resolution:**
1. Check error message in log file
2. Verify all prerequisites are met
3. Check disk space: `df -h`
4. Verify Docker is running: `docker info`
5. Check network connectivity
6. Review recent changes to deployment scripts

### Health Check Fails

**Symptom:** Health check never returns healthy

**Resolution:**
1. Check container logs: `docker-compose logs unrdf`
2. Verify environment variables: `docker-compose config`
3. Check database connectivity
4. Verify port mappings: `docker-compose ps`
5. Check firewall rules
6. Initiate rollback if unresolved after 10 minutes

### High Error Rate After Deployment

**Symptom:** Error rate > 0.05/s

**Resolution:**
1. Check logs for error patterns: `docker-compose logs unrdf | grep ERROR`
2. Verify database is accessible
3. Check configuration for typos
4. Review breaking changes in release notes
5. Consider rollback if error rate > 0.1/s

### Performance Degradation

**Symptom:** P95 latency > 1s

**Resolution:**
1. Check resource usage: `docker stats`
2. Review query complexity
3. Check for slow database queries
4. Verify caching is enabled
5. Scale horizontally if needed

## Rollback Procedure

If deployment fails, refer to [ROLLBACK_RUNBOOK.md](./ROLLBACK_RUNBOOK.md)

## Emergency Contacts

| Role | Name | Contact |
|------|------|---------|
| DevOps Lead | TBD | Slack: @devops-lead |
| Backend Lead | TBD | Slack: @backend-lead |
| On-Call Engineer | TBD | PagerDuty |
| VP Engineering | TBD | Phone: XXX-XXX-XXXX |

## Related Documentation

- [Rollback Runbook](./ROLLBACK_RUNBOOK.md)
- [Incident Response](./INCIDENT_RESPONSE.md)
- [Monitoring Guide](./MONITORING_GUIDE.md)
- [Architecture Documentation](../substrate/architecture.md)

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-11 | 1.0 | Initial runbook | DevOps Team |
