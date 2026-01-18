# UNRDF v6 Incident Response Runbook

## Overview

This runbook provides procedures for responding to production incidents affecting UNRDF v6.

## Incident Severity Levels

| Level | Definition | Response Time | Examples |
|-------|-----------|---------------|----------|
| **P0 - Critical** | Complete service outage, data loss | < 15 minutes | All services down, database corruption |
| **P1 - High** | Major functionality impaired | < 1 hour | SPARQL queries failing, high error rate |
| **P2 - Medium** | Partial functionality impaired | < 4 hours | Slow queries, some features degraded |
| **P3 - Low** | Minor issues, no user impact | < 24 hours | Monitoring alerts, performance warnings |

## Incident Response Team

### Roles and Responsibilities

**Incident Commander (IC)**
- Overall incident coordination
- Communication with stakeholders
- Decision making authority
- Post-incident review

**Technical Lead**
- Technical investigation
- Fix implementation
- Verification of resolution

**Communications Lead**
- Status updates
- Customer communication
- Documentation

**On-Call Engineer**
- First responder
- Initial triage
- Escalation to team

## P0 Critical Incident Response

### Immediate Actions (First 5 Minutes)

1. **Acknowledge and Assess**
   ```bash
   # Check system status
   ./scripts/health-check.sh

   # Check all services
   docker-compose ps

   # Check recent errors
   docker-compose logs --tail=100 unrdf | grep -i "error\|fatal"
   ```

2. **Declare Incident**
   ```bash
   # Create incident in PagerDuty
   curl -X POST https://api.pagerduty.com/incidents \
     -H "Authorization: Token token=$PAGERDUTY_TOKEN" \
     -H "Content-Type: application/json" \
     -d '{
       "incident": {
         "type": "incident",
         "title": "UNRDF Production P0 Incident",
         "service": {"id": "'$SERVICE_ID'", "type": "service_reference"},
         "urgency": "high",
         "incident_key": "unrdf-'$(date +%Y%m%d-%H%M%S)'"
       }
     }'

   # Notify team
   curl -X POST $SLACK_WEBHOOK_URL \
     -H 'Content-Type: application/json' \
     -d '{"text":"🚨 P0 INCIDENT - UNRDF Production Down\nIncident Commander: '@${USER}'\nWar room: #incident-response"}'
   ```

3. **Establish War Room**
   - Create dedicated Slack channel: `#incident-YYYYMMDD-HHMM`
   - Start Zoom/video call for real-time coordination
   - Page additional team members if needed

### Investigation Phase (5-15 Minutes)

1. **Gather Information**
   ```bash
   # System metrics
   docker stats --no-stream

   # Recent deployments
   git log --oneline -10
   cat /var/backups/unrdf/latest

   # Error patterns
   docker-compose logs --since 30m | grep -i "error" | sort | uniq -c | sort -rn

   # Resource usage
   df -h
   free -h
   uptime
   ```

2. **Check Dependencies**
   ```bash
   # Database connectivity
   docker-compose exec unrdf node -e "console.log('DB check')" || echo "DB unreachable"

   # External services
   curl -I https://external-service.example.com

   # Network connectivity
   ping -c 3 8.8.8.8
   ```

3. **Review Monitoring**
   - Check Grafana dashboards
   - Review Prometheus alerts
   - Check application logs

### Mitigation Phase (15-30 Minutes)

Choose appropriate mitigation strategy:

#### Strategy 1: Rollback (Preferred for deployment issues)
```bash
# Execute rollback
./scripts/rollback.sh --auto

# Verify
./scripts/health-check.sh
```

#### Strategy 2: Service Restart
```bash
# Restart specific service
docker-compose restart unrdf

# Or full restart
docker-compose down --timeout 30
docker-compose up -d
```

#### Strategy 3: Scale Horizontally
```bash
# Scale up instances
docker-compose up -d --scale unrdf=3

# Verify load distribution
docker-compose ps
```

#### Strategy 4: Emergency Patch
```bash
# Apply hotfix
git cherry-pick <hotfix-commit>
docker-compose build
docker-compose up -d
```

### Resolution Phase (30-60 Minutes)

1. **Verify Fix**
   ```bash
   # Run comprehensive health check
   ./scripts/health-check.sh

   # Check metrics
   curl "http://localhost:9090/api/v1/query?query=rate(unrdf_errors_total[5m])"

   # Test critical paths
   curl -X POST https://production.example.com/sparql \
     -H "Content-Type: application/sparql-query" \
     -d "SELECT * WHERE { ?s ?p ?o } LIMIT 1"
   ```

2. **Monitor Stability**
   - Watch metrics for 15 minutes
   - Verify error rate < 0.01/s
   - Confirm P95 latency < 100ms

3. **Declare Resolution**
   ```bash
   # Update incident
   curl -X PUT https://api.pagerduty.com/incidents/$INCIDENT_ID \
     -H "Authorization: Token token=$PAGERDUTY_TOKEN" \
     -H "Content-Type: application/json" \
     -d '{"incident":{"type":"incident_reference","status":"resolved"}}'

   # Notify team
   curl -X POST $SLACK_WEBHOOK_URL \
     -H 'Content-Type: application/json' \
     -d '{"text":"✅ P0 INCIDENT RESOLVED\nDuration: XX minutes\nRoot cause: TBD\nPost-mortem scheduled"}'
   ```

## P1 High Priority Incident Response

### Assessment (First 15 Minutes)

1. **Identify Impact**
   - How many users affected?
   - Which features impaired?
   - Revenue impact?
   - SLA breach risk?

2. **Check Recent Changes**
   ```bash
   # Recent deployments
   git log --since="2 hours ago" --oneline

   # Configuration changes
   git diff HEAD~5 .env.production

   # Infrastructure changes
   git log --since="1 day ago" -- docker-compose.yml Dockerfile
   ```

3. **Gather Evidence**
   ```bash
   # Save current state
   mkdir -p /var/log/unrdf/incidents/$(date +%Y%m%d-%H%M%S)
   incident_dir=/var/log/unrdf/incidents/$(date +%Y%m%d-%H%M%S)

   # Save logs
   docker-compose logs > $incident_dir/logs.txt

   # Save metrics
   curl "http://localhost:9090/api/v1/query?query=rate(unrdf_errors_total[1h])" \
     > $incident_dir/error-metrics.json

   # Save config
   docker-compose config > $incident_dir/config.yml
   ```

### Mitigation

Follow similar strategies as P0 but with more deliberate investigation.

## Common Incident Scenarios

### Scenario: High Error Rate

**Symptoms:**
- Error rate > 5%
- Alert: HighErrorRate firing

**Diagnosis:**
```bash
# Find error patterns
docker-compose logs --since 10m | grep ERROR | awk '{print $NF}' | sort | uniq -c | sort -rn

# Check specific error types
docker-compose logs --since 10m | grep "ValidationError"
```

**Resolution:**
1. Identify error source (API, database, external service)
2. Check for recent configuration changes
3. Verify input validation
4. Consider rollback if related to recent deployment

### Scenario: High Latency

**Symptoms:**
- P95 latency > 1s
- Alert: HighAPILatency firing

**Diagnosis:**
```bash
# Check slow queries
docker-compose logs | grep "Slow query"

# Check resource usage
docker stats

# Check database performance
# (database-specific commands)
```

**Resolution:**
1. Identify slow endpoints
2. Check for missing indexes
3. Review query complexity
4. Scale horizontally if needed
5. Enable query caching

### Scenario: Memory Leak

**Symptoms:**
- Memory usage steadily increasing
- OOM kills
- Container restarts

**Diagnosis:**
```bash
# Monitor memory trend
docker stats --format "table {{.Name}}\t{{.MemUsage}}" --no-stream

# Check heap size
docker-compose exec unrdf node -e "console.log(process.memoryUsage())"

# Enable heap snapshot
docker-compose exec unrdf node --heap-prof app.js
```

**Resolution:**
1. Restart service to free memory (immediate)
2. Analyze heap snapshots
3. Identify memory leak source
4. Deploy fix
5. Increase memory limits if legitimate usage

### Scenario: Database Connectivity Loss

**Symptoms:**
- Database connection errors
- SPARQL queries timing out

**Diagnosis:**
```bash
# Test connectivity
docker-compose exec unrdf nc -zv database 5432

# Check database status
docker-compose ps database

# Check database logs
docker-compose logs database
```

**Resolution:**
1. Verify database is running
2. Check network connectivity
3. Verify credentials
4. Check connection pool exhaustion
5. Restart database if needed

### Scenario: Certificate Expiration

**Symptoms:**
- TLS handshake errors
- HTTPS endpoints failing

**Diagnosis:**
```bash
# Check certificate expiry
echo | openssl s_client -connect production.example.com:443 2>/dev/null | \
  openssl x509 -noout -dates
```

**Resolution:**
1. Renew certificate immediately
2. Update certificate in load balancer
3. Verify HTTPS works
4. Add certificate expiry monitoring

## Post-Incident Activities

### Incident Timeline

Document detailed timeline:
```
12:00:00 - Alert fired: HighErrorRate
12:01:30 - On-call engineer acknowledged
12:03:00 - P0 incident declared
12:05:00 - Investigation started
12:15:00 - Root cause identified: bad deployment
12:20:00 - Rollback initiated
12:25:00 - Services restored
12:30:00 - Health checks passing
12:45:00 - Monitoring confirms stability
13:00:00 - Incident resolved
```

### Post-Mortem Template

```markdown
# Incident Post-Mortem: [Incident Title]

**Date:** YYYY-MM-DD
**Duration:** XX minutes
**Severity:** P0/P1/P2/P3
**Incident Commander:** [Name]

## Summary
[Brief description of incident]

## Impact
- Users affected: [number/percentage]
- Services affected: [list]
- Downtime: [duration]
- Revenue impact: [if applicable]

## Root Cause
[Detailed explanation of what caused the incident]

## Timeline
[Detailed timeline from detection to resolution]

## Resolution
[What fixed the issue]

## What Went Well
- [Things that worked well]

## What Went Wrong
- [Things that didn't work well]

## Action Items
- [ ] [Action 1] - Owner: [Name] - Due: [Date]
- [ ] [Action 2] - Owner: [Name] - Due: [Date]

## Lessons Learned
[Key takeaways]
```

### Follow-Up Actions

1. **Within 24 Hours**
   - Complete post-mortem document
   - Share with team
   - Create action items

2. **Within 1 Week**
   - Implement preventive measures
   - Update runbooks
   - Add monitoring/alerts
   - Update tests

3. **Within 1 Month**
   - Review effectiveness of fixes
   - Share learnings company-wide
   - Update incident response procedures

## Incident Communication

### Internal Communication

**During Incident:**
- Update #incident-response every 15 minutes
- Include: status, ETA, next steps

**After Resolution:**
- Summary in #general
- Detailed post-mortem in email
- Incident review meeting

### External Communication (If Customer-Facing)

**Status Page Updates:**
```
Investigating: We are currently investigating reports of [issue]
Identified: The issue has been identified and we are working on a fix
Monitoring: A fix has been implemented and we are monitoring the results
Resolved: This incident has been resolved
```

**Customer Email Template:**
```
Subject: [Resolved] Service Disruption - [Date]

Dear UNRDF Users,

We experienced a service disruption today from [start time] to [end time] affecting [description].

Impact: [brief description]
Root Cause: [high-level explanation]
Resolution: [what we did]
Prevention: [what we're doing to prevent recurrence]

We apologize for any inconvenience.

The UNRDF Team
```

## Emergency Contacts

| Role | Primary | Secondary | Escalation |
|------|---------|-----------|-----------|
| On-Call | PagerDuty | Slack @oncall | Phone tree |
| DevOps Lead | Slack | Email | VP Eng |
| Backend Lead | Slack | Email | VP Eng |
| Database Admin | Slack | Email | CTO |
| Security Lead | Slack | Email | CISO |

## Related Documentation

- [Deployment Runbook](./DEPLOYMENT_RUNBOOK.md)
- [Rollback Runbook](./ROLLBACK_RUNBOOK.md)
- [Monitoring Guide](./MONITORING_GUIDE.md)

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-11 | 1.0 | Initial runbook | DevOps Team |
