# UNRDF v6 Deployment Checklist

## Pre-Deployment Checklist

### Code Quality ✅

- [ ] All tests passing (`pnpm test:fast`)
  ```bash
  timeout 120s pnpm test:fast
  ```

- [ ] Lint checks passing (`pnpm lint`)
  ```bash
  timeout 60s pnpm lint
  ```

- [ ] No security vulnerabilities (`pnpm audit`)
  ```bash
  pnpm audit --prod
  ```

- [ ] Code review approved (2+ reviewers)

- [ ] All CI/CD checks passing

- [ ] Version number updated in `package.json`

- [ ] CHANGELOG.md updated with release notes

### Configuration ✅

- [ ] Environment variables validated
  ```bash
  node scripts/validate-env-config.mjs
  ```

- [ ] `.env.production` file exists with all required values

- [ ] Secrets properly configured (no placeholder values)

- [ ] Database connection string verified

- [ ] External service URLs verified

- [ ] API keys and tokens updated

- [ ] SSL/TLS certificates valid and not expiring within 30 days
  ```bash
  echo | openssl s_client -connect production.example.com:443 2>/dev/null | \
    openssl x509 -noout -dates
  ```

### Infrastructure ✅

- [ ] Disk space available (minimum 10GB)
  ```bash
  df -h / | awk 'NR==2 {if (substr($4, 1, length($4)-1) < 10) exit 1}'
  ```

- [ ] Memory available (minimum 4GB)
  ```bash
  free -g | awk 'NR==2 {if ($7 < 4) exit 1}'
  ```

- [ ] Docker daemon running
  ```bash
  docker info > /dev/null 2>&1
  ```

- [ ] Docker Compose version >= 2.0
  ```bash
  docker-compose version
  ```

- [ ] Network connectivity verified
  ```bash
  ping -c 3 ghcr.io
  ```

- [ ] Load balancer health checks configured

- [ ] Firewall rules verified

### Backup ✅

- [ ] Backup directory exists and is writable
  ```bash
  test -w /var/backups/unrdf || mkdir -p /var/backups/unrdf
  ```

- [ ] Recent backup verified (< 24 hours old)
  ```bash
  find /var/backups/unrdf -name "*.tar.gz" -mtime -1 | grep -q . || \
    echo "No recent backup found!"
  ```

- [ ] Backup restoration tested in last 7 days

- [ ] Database backup completed
  ```bash
  tar -czf /var/backups/unrdf/pre-deploy-$(date +%Y%m%d-%H%M%S).tar.gz /app/data
  ```

### Monitoring ✅

- [ ] Grafana accessible
  ```bash
  curl -f https://grafana.example.com/api/health
  ```

- [ ] Prometheus accessible
  ```bash
  curl -f https://prometheus.example.com/-/healthy
  ```

- [ ] AlertManager configured
  ```bash
  curl -f https://alertmanager.example.com/-/healthy
  ```

- [ ] Slack webhook working
  ```bash
  curl -X POST $SLACK_WEBHOOK_URL \
    -H 'Content-Type: application/json' \
    -d '{"text":"Pre-deployment check: Slack integration working"}'
  ```

- [ ] PagerDuty integration tested (staging)

- [ ] Log aggregation working

### Team Coordination ✅

- [ ] Deployment window scheduled and communicated

- [ ] On-call engineer available

- [ ] Stakeholders notified of deployment

- [ ] Deployment communication sent
  ```
  Subject: Production Deployment - UNRDF v6.0.0-rc.1
  Time: 2026-01-11 14:00 UTC
  Duration: ~30 minutes
  Expected Impact: None (zero-downtime deployment)
  Rollback Plan: Available
  ```

- [ ] Change management ticket created and approved

- [ ] Documentation updated

### Build Verification ✅

- [ ] Docker image built successfully
  ```bash
  docker build -t unrdf:test .
  ```

- [ ] Image size reasonable (< 1GB)
  ```bash
  docker images unrdf:test --format "{{.Size}}"
  ```

- [ ] Image scanned for vulnerabilities
  ```bash
  docker scan unrdf:test || echo "Scan not available"
  ```

- [ ] SBOM (Software Bill of Materials) generated

## Deployment Checklist

### Pre-Deployment (T-30 minutes)

- [ ] **T-30m** Create deployment announcement
  ```bash
  curl -X POST $SLACK_WEBHOOK_URL \
    -H 'Content-Type: application/json' \
    -d '{"text":"🚀 Production deployment starting in 30 minutes\nVersion: v6.0.0-rc.1\nExpected duration: 30 minutes"}'
  ```

- [ ] **T-25m** Enable maintenance mode (if needed)

- [ ] **T-20m** Verify no active incidents

- [ ] **T-15m** Final backup created

- [ ] **T-10m** All team members ready

- [ ] **T-5m** Monitoring dashboards open

### Deployment (T-0)

- [ ] **T+0m** Start deployment
  ```bash
  export IMAGE_TAG="ghcr.io/unrdf/unrdf:6.0.0-rc.1"
  export DEPLOY_HOST="production.example.com"
  ./scripts/deploy-production.sh
  ```

- [ ] **T+5m** Deployment script completed successfully

- [ ] **T+5m** All containers running
  ```bash
  docker-compose ps | grep -v "Up" && echo "Containers not running!" || echo "All containers up"
  ```

- [ ] **T+7m** Health checks passing
  ```bash
  ./scripts/health-check.sh https://production.example.com
  ```

- [ ] **T+10m** Smoke tests passed
  ```bash
  curl -f https://production.example.com/health
  curl -f https://production.example.com/metrics
  curl -f https://production.example.com/api
  ```

### Post-Deployment (T+10 to T+30)

- [ ] **T+10m** API endpoints responding

- [ ] **T+12m** SPARQL queries working
  ```bash
  curl -X POST https://production.example.com/sparql \
    -H "Content-Type: application/sparql-query" \
    -d "SELECT * WHERE { ?s ?p ?o } LIMIT 1"
  ```

- [ ] **T+15m** Receipt creation working
  ```bash
  curl -X POST https://production.example.com/api/receipts \
    -H "Content-Type: application/json" \
    -d '{"operation":"create","entityType":"Triple"}'
  ```

- [ ] **T+15m** Error rate < 0.01/s
  ```bash
  curl -s "http://localhost:9090/api/v1/query?query=rate(unrdf_errors_total[5m])"
  ```

- [ ] **T+15m** P95 latency < 100ms
  ```bash
  curl -s "http://localhost:9090/api/v1/query?query=histogram_quantile(0.95,rate(unrdf_api_duration_seconds_bucket[5m]))"
  ```

- [ ] **T+20m** Memory usage normal (< 80%)

- [ ] **T+20m** CPU usage normal (< 80%)

- [ ] **T+25m** No alerts firing

- [ ] **T+30m** Disable maintenance mode

## Post-Deployment Checklist

### Immediate (Within 1 Hour)

- [ ] Deployment success notification sent
  ```bash
  curl -X POST $SLACK_WEBHOOK_URL \
    -H 'Content-Type: application/json' \
    -d '{"text":"✅ Production deployment completed successfully\nVersion: v6.0.0-rc.1\nDuration: XX minutes\nStatus: All systems operational"}'
  ```

- [ ] Grafana annotation created
  ```bash
  curl -X POST https://grafana.example.com/api/annotations \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $GRAFANA_API_KEY" \
    -d '{
      "text": "Production deployment: v6.0.0-rc.1",
      "tags": ["deployment", "production"],
      "time": '$(date +%s000)'
    }'
  ```

- [ ] Git deployment tag created
  ```bash
  git tag -a "deployed-$(date +%Y%m%d-%H%M%S)" -m "Production deployment v6.0.0-rc.1"
  git push origin --tags
  ```

- [ ] Deployment log archived

- [ ] Change management ticket updated

- [ ] Metrics reviewed (no anomalies)

- [ ] Logs reviewed (no errors)

### Short-Term (Within 24 Hours)

- [ ] Performance metrics stable

- [ ] No increase in error rate

- [ ] No memory leaks detected

- [ ] No alerts fired

- [ ] Customer feedback reviewed (if applicable)

- [ ] Deployment retrospective scheduled

- [ ] Documentation updated with any changes

### Long-Term (Within 1 Week)

- [ ] SLO metrics reviewed

- [ ] Performance baselines updated

- [ ] Deployment process improvements identified

- [ ] Runbooks updated if needed

- [ ] Team feedback collected

## Rollback Checklist

**If deployment fails, immediately execute:**

- [ ] Stop deployment process

- [ ] Assess severity (use P0/P1/P2 framework)

- [ ] Notify team of rollback decision
  ```bash
  curl -X POST $SLACK_WEBHOOK_URL \
    -H 'Content-Type: application/json' \
    -d '{"text":"🚨 ROLLBACK INITIATED - Deployment failed"}'
  ```

- [ ] Execute rollback script
  ```bash
  ./scripts/rollback.sh --auto
  ```

- [ ] Verify rollback successful

- [ ] Update stakeholders

- [ ] Create incident report

- [ ] Schedule post-mortem

## Emergency Contacts

| Role | Contact | When to Escalate |
|------|---------|------------------|
| On-Call Engineer | PagerDuty | Immediate for P0/P1 |
| DevOps Lead | Slack @devops | Deployment issues |
| Backend Lead | Slack @backend | Application errors |
| VP Engineering | Phone | Critical failures |

## Sign-Off

**Deployment Lead:** _________________ **Date:** _________

**Technical Lead:** _________________ **Date:** _________

**Change Manager:** _________________ **Date:** _________

---

**Deployment Notes:**

_Use this space to record any issues, deviations from plan, or observations during deployment._

---

## Related Documentation

- [Deployment Runbook](./DEPLOYMENT_RUNBOOK.md)
- [Rollback Runbook](./ROLLBACK_RUNBOOK.md)
- [Incident Response](./INCIDENT_RESPONSE.md)
- [Monitoring Guide](./MONITORING_GUIDE.md)
