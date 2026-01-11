# UNRDF v6 Production Deployment Guide

## Overview

This document provides an overview of the complete production deployment configuration for UNRDF v6.

## Table of Contents

- [Quick Start](#quick-start)
- [Architecture](#architecture)
- [Deployment Assets](#deployment-assets)
- [Deployment Process](#deployment-process)
- [Monitoring](#monitoring)
- [Troubleshooting](#troubleshooting)

## Quick Start

### Prerequisites

1. **Required Tools**
   - Docker >= 20.10
   - Docker Compose >= 2.0
   - Git >= 2.30
   - Node.js >= 18.0
   - pnpm >= 8.0

2. **Required Access**
   - GitHub Container Registry
   - Production servers (SSH)
   - Monitoring systems (Grafana, Prometheus)

### Quick Deployment

```bash
# 1. Validate configuration
node scripts/validate-env-config.mjs

# 2. Create .env.production (copy from .env.example)
cp .env.example .env.production
# Edit .env.production with production values

# 3. Deploy to production
./scripts/deploy-production.sh

# 4. Verify deployment
./scripts/health-check.sh https://production.example.com
```

## Architecture

### Deployment Architecture

```
┌─────────────────────────────────────────────────────────┐
│ GitHub Actions (CI/CD)                                  │
│  - Build & Test                                         │
│  - Docker Image Build                                   │
│  - Deployment Automation                                │
└─────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────┐
│ GitHub Container Registry                               │
│  - Docker Images                                        │
│  - Version Tagging                                      │
└─────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────┐
│ Production Environment                                  │
│  ┌─────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │ UNRDF App   │  │ Prometheus   │  │ Grafana      │  │
│  │ :3000       │  │ :9091        │  │ :3001        │  │
│  └─────────────┘  └──────────────┘  └──────────────┘  │
│  ┌─────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │ OTEL        │  │ AlertManager │  │ Node Exp.    │  │
│  │ :4318       │  │ :9093        │  │ :9100        │  │
│  └─────────────┘  └──────────────┘  └──────────────┘  │
└─────────────────────────────────────────────────────────┘
```

### Multi-Stage Docker Build

```
Stage 1: deps     → Install production dependencies
Stage 2: builder  → Build application
Stage 3: runner   → Production runtime (optimized)
```

## Deployment Assets

### 1. Docker Configuration

#### `/Dockerfile`
- Multi-stage optimized build
- Security: non-root user, minimal base image
- Size: ~300-500MB (estimated)
- Health check included

#### `/docker-compose.yml`
- Complete stack definition
- Services: app, prometheus, grafana, otel-collector, alertmanager, node-exporter
- Resource limits configured
- Network isolation

#### `/.dockerignore`
- Optimized build context
- Excludes: node_modules, tests, docs, examples

### 2. Environment Configuration

#### `.env.example`
- Production environment template
- All required variables documented
- Secure defaults

#### `.env.staging`
- Staging environment configuration
- Debug mode enabled
- Reduced resource limits

### 3. Deployment Scripts

#### `scripts/deploy-production.sh`
**Purpose:** Production deployment automation
**Features:**
- Pre-deployment validation
- Automatic backup creation
- Zero-downtime deployment
- Health check verification
- Post-deployment validation

**Usage:**
```bash
export IMAGE_TAG="ghcr.io/unrdf/unrdf:6.0.0-rc.1"
./scripts/deploy-production.sh
```

#### `scripts/deploy-staging.sh`
**Purpose:** Staging deployment automation
**Features:**
- Quick deployment for testing
- Smoke test execution
- Log viewing

**Usage:**
```bash
export IMAGE_TAG="ghcr.io/unrdf/unrdf:staging-latest"
./scripts/deploy-staging.sh
```

#### `scripts/rollback.sh`
**Purpose:** Emergency rollback automation
**Features:**
- Restore from latest backup
- Service restart
- Verification

**Usage:**
```bash
# Interactive mode
./scripts/rollback.sh

# Automatic mode (for CI/CD)
./scripts/rollback.sh --auto
```

#### `scripts/health-check.sh`
**Purpose:** Comprehensive health validation
**Features:**
- Endpoint checks (health, API, metrics)
- Container status validation
- Resource usage monitoring
- Log analysis

**Usage:**
```bash
./scripts/health-check.sh https://production.example.com
```

#### `scripts/validate-env-config.mjs`
**Purpose:** Environment configuration validation
**Features:**
- Zod schema validation
- Type checking
- Security warnings

**Usage:**
```bash
node scripts/validate-env-config.mjs
```

### 4. GitHub Actions Workflows

#### `.github/workflows/deploy-production.yml`
**Trigger:** Manual dispatch or release tag
**Steps:**
1. Pre-deployment validation (tests, lint, security)
2. Build & push Docker image
3. Deploy to production
4. Post-deployment verification
5. Rollback on failure

#### `.github/workflows/deploy-staging.yml`
**Trigger:** Push to develop branch
**Steps:**
1. Build & test
2. Build Docker image
3. Deploy to staging
4. Run integration tests
5. Notify team

### 5. Monitoring Configuration

#### `monitoring/prometheus/prometheus.yml`
- Scrape configurations for all services
- 15-second scrape interval
- Alert rule loading

#### `monitoring/prometheus/alerts.yml`
- 10+ production alerts
- P0 (Critical) and P1 (Warning) levels
- Metrics: error rate, latency, resources, KGC

#### `monitoring/grafana/dashboards/unrdf-overview.json`
- Overview dashboard
- Panels: request rate, latency, errors, memory, CPU
- Real-time updates

#### `monitoring/otel/otel-collector-config.yaml`
- OpenTelemetry collector setup
- Trace/metric/log pipelines
- Export to Prometheus and OTLP

#### `monitoring/alertmanager/config.yml`
- Alert routing configuration
- Slack integration
- PagerDuty for critical alerts

### 6. Production Runbooks

All located in `docs/runbooks/`:

#### `DEPLOYMENT_RUNBOOK.md` (7.9KB)
- Complete deployment procedure
- Pre/post-deployment checklists
- Verification steps
- Troubleshooting guide

#### `ROLLBACK_RUNBOOK.md` (9.4KB)
- When to rollback (decision matrix)
- Automated rollback procedure
- Manual rollback steps
- Post-rollback actions

#### `INCIDENT_RESPONSE.md` (12KB)
- Incident severity levels (P0-P3)
- Response procedures
- Common incident scenarios
- Post-incident activities

#### `MONITORING_GUIDE.md` (12KB)
- Metrics catalog
- Dashboard usage
- Alert configuration
- Query examples

#### `DEPLOYMENT_CHECKLIST.md` (8.5KB)
- Pre-deployment checklist
- Deployment timeline
- Post-deployment verification
- Rollback checklist

## Deployment Process

### Standard Deployment (Production)

1. **Preparation (T-30 minutes)**
   ```bash
   # Validate configuration
   node scripts/validate-env-config.mjs

   # Run tests
   pnpm test:fast

   # Run security audit
   pnpm audit --prod
   ```

2. **Notification (T-15 minutes)**
   ```bash
   # Notify team
   curl -X POST $SLACK_WEBHOOK_URL \
     -d '{"text":"Deployment starting in 15 minutes"}'
   ```

3. **Deployment (T-0)**
   ```bash
   # Execute deployment
   export IMAGE_TAG="ghcr.io/unrdf/unrdf:6.0.0-rc.1"
   ./scripts/deploy-production.sh
   ```

4. **Verification (T+10 minutes)**
   ```bash
   # Health check
   ./scripts/health-check.sh

   # Smoke tests
   curl -f https://production.example.com/health
   curl -f https://production.example.com/metrics
   ```

5. **Monitoring (T+30 minutes)**
   - Watch Grafana dashboards
   - Monitor error rates
   - Check for alerts

### CI/CD Deployment (Automated)

**Production:**
```bash
# Trigger via GitHub Actions
# Go to Actions → Deploy Production → Run workflow
# Input: version tag (e.g., 6.0.0-rc.1)
```

**Staging:**
```bash
# Automatic on push to develop
git push origin develop
```

## Monitoring

### Accessing Dashboards

- **Grafana:** http://grafana.example.com:3001
  - Default dashboard: UNRDF Overview
  - Login: admin / (see GRAFANA_ADMIN_PASSWORD)

- **Prometheus:** http://prometheus.example.com:9091
  - Query interface
  - Target status
  - Alert status

- **AlertManager:** http://alertmanager.example.com:9093
  - Active alerts
  - Silences
  - Alert routing

### Key Metrics to Watch

| Metric | Target | Alert Threshold |
|--------|--------|-----------------|
| Error Rate | < 0.01/s | > 0.05/s |
| P95 Latency | < 100ms | > 1s |
| Memory Usage | < 60% | > 80% |
| CPU Usage | < 50% | > 80% |
| Receipt Creation | < 1ms | > 5ms |

### Alert Channels

- **Slack:** #unrdf-alerts (all alerts)
- **PagerDuty:** On-call rotation (critical only)
- **Email:** DevOps team

## Troubleshooting

### Common Issues

#### 1. Deployment Script Fails

**Symptoms:** Script exits with error

**Check:**
```bash
# Disk space
df -h

# Docker status
docker info

# Network connectivity
ping ghcr.io
```

**Solution:** Review error message, check prerequisites

#### 2. Health Check Fails

**Symptoms:** Service not responding

**Check:**
```bash
# Container status
docker-compose ps

# Container logs
docker-compose logs unrdf

# Port availability
netstat -tulpn | grep -E '3000|8080|9090'
```

**Solution:** Review logs, restart service, or rollback

#### 3. High Error Rate

**Symptoms:** Errors > 0.05/s

**Check:**
```bash
# Error patterns
docker-compose logs | grep ERROR | sort | uniq -c

# Recent changes
git log --oneline -5
```

**Solution:** Identify error source, consider rollback

### Emergency Procedures

#### Immediate Rollback
```bash
./scripts/rollback.sh --auto
```

#### Complete Service Restart
```bash
docker-compose down --timeout 30
docker-compose up -d
```

#### View Real-Time Logs
```bash
docker-compose logs -f unrdf
```

## Security Considerations

### Secrets Management

**DO:**
- Use environment variables for secrets
- Rotate secrets regularly
- Use different secrets per environment

**DON'T:**
- Commit secrets to git
- Use default/example secrets in production
- Share secrets in Slack/email

### Required Secrets

```bash
# Production (.env.production)
JWT_SECRET=<strong-random-key>
KGC_SIGNING_KEY=<base64-encoded-key>
GRAFANA_ADMIN_PASSWORD=<secure-password>
API_KEY=<service-api-key>

# CI/CD (GitHub Secrets)
AWS_ACCESS_KEY_ID
AWS_SECRET_ACCESS_KEY
PRODUCTION_SSH_KEY
STAGING_SSH_KEY
SLACK_WEBHOOK_URL
PAGERDUTY_SERVICE_KEY
GRAFANA_API_KEY
```

### Network Security

- All services run in isolated Docker network
- Expose only necessary ports
- Use TLS for all external endpoints
- Firewall rules configured

## Performance Optimization

### Docker Image Optimization

- Multi-stage build reduces size by ~70%
- Production dependencies only
- Alpine Linux base image
- Layer caching enabled

### Resource Limits

```yaml
# docker-compose.yml
limits:
  cpus: '2.0'
  memory: 4G
reservations:
  cpus: '1.0'
  memory: 2G
```

### Scaling

**Horizontal Scaling:**
```bash
# Scale to 3 instances
docker-compose up -d --scale unrdf=3
```

**Load Balancing:**
- Use external load balancer (e.g., nginx, AWS ALB)
- Health check endpoint: `/health`
- Session affinity: not required (stateless)

## Compliance & Auditing

### Deployment Audit Trail

- Git tags for every deployment
- Deployment logs stored in `/var/log/unrdf/`
- Grafana annotations for all deployments
- Change management tickets

### Data Retention

- Logs: 30 days
- Metrics: 30 days (high-res), 1 year (low-res)
- Backups: 30 days

## Support & Contacts

### Documentation

- [Deployment Runbook](docs/runbooks/DEPLOYMENT_RUNBOOK.md)
- [Rollback Runbook](docs/runbooks/ROLLBACK_RUNBOOK.md)
- [Incident Response](docs/runbooks/INCIDENT_RESPONSE.md)
- [Monitoring Guide](docs/runbooks/MONITORING_GUIDE.md)
- [Deployment Checklist](docs/runbooks/DEPLOYMENT_CHECKLIST.md)

### Team Contacts

| Role | Contact |
|------|---------|
| On-Call Engineer | PagerDuty |
| DevOps Lead | Slack @devops-lead |
| Backend Lead | Slack @backend-lead |
| VP Engineering | [Phone] |

### External Resources

- [Docker Documentation](https://docs.docker.com/)
- [Prometheus Documentation](https://prometheus.io/docs/)
- [Grafana Documentation](https://grafana.com/docs/)

## Changelog

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-11 | 1.0 | Initial production deployment configuration |

---

**Maintained by:** DevOps Team
**Last Updated:** 2026-01-11
**Next Review:** 2026-02-11
