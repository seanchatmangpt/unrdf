# Production Deployment Guide - UNRDF v6.0.0

**Comprehensive checklist for deploying UNRDF v6.0.0 to production**

## Overview

This guide provides step-by-step instructions for deploying UNRDF v6.0.0 with the @unrdf/daemon package to production environments.

**Deployment Time**: 2-4 hours
**Complexity**: Medium
**Prerequisites**: Node.js 18+, Production server access, SSL certificates
**Support**: Enterprise-grade security and monitoring

---

## Pre-Deployment Checklist

### ✅ Environment Preparation

- [ ] Node.js 18+ installed on production servers
- [ ] pnpm 7+ package manager installed
- [ ] Production database configured (if using persistent storage)
- [ ] SSL/TLS certificates obtained and validated
- [ ] Firewall rules configured
- [ ] Monitoring systems ready (Prometheus, Grafana, etc.)
- [ ] Log aggregation configured (ELK, Splunk, etc.)

### ✅ Security Requirements

- [ ] API keys generated for production
- [ ] Secrets management system configured (Vault, AWS Secrets Manager)
- [ ] HTTPS/TLS enabled (minimum TLS 1.2)
- [ ] Security scanning completed (npm audit, Snyk)
- [ ] Penetration testing completed (if required)
- [ ] Access control policies defined
- [ ] Backup and disaster recovery plan documented

### ✅ Testing Validation

- [ ] All tests passing (pnpm test)
- [ ] Integration tests completed
- [ ] Load testing completed
- [ ] Security tests passed
- [ ] Performance benchmarks acceptable

---

## Deployment Steps

### Step 1: Server Preparation (30 minutes)

#### 1.1 Install Node.js

```bash
# Ubuntu/Debian
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt-get install -y nodejs

# Verify installation
node --version  # Should be >= 18.0.0
```

#### 1.2 Install pnpm

```bash
npm install -g pnpm
pnpm --version
```

#### 1.3 Create Application User

```bash
# Create dedicated user for UNRDF
sudo useradd -r -s /bin/bash -d /opt/unrdf unrdf

# Create application directory
sudo mkdir -p /opt/unrdf
sudo chown unrdf:unrdf /opt/unrdf
```

#### 1.4 Configure System Limits

```bash
# /etc/security/limits.conf
unrdf soft nofile 65536
unrdf hard nofile 65536
unrdf soft nproc 32768
unrdf hard nproc 32768
```

---

### Step 2: Application Deployment (45 minutes)

#### 2.1 Deploy Application Code

```bash
# As unrdf user
sudo -u unrdf -i

cd /opt/unrdf

# Clone repository or copy built artifacts
# Option A: Git clone
git clone https://github.com/your-org/your-app.git app
cd app

# Option B: Upload pre-built package
# scp build.tar.gz unrdf@server:/opt/unrdf/
# tar xzf build.tar.gz
```

#### 2.2 Install Dependencies

```bash
# Production dependencies only
pnpm install --prod --frozen-lockfile

# Verify installation
pnpm list
```

#### 2.3 Build Application (if needed)

```bash
# If using TypeScript or requiring build step
pnpm build

# Verify build artifacts
ls -la dist/
```

---

### Step 3: Security Configuration (60 minutes)

#### 3.1 Generate Production API Keys

```javascript
// generate-keys.mjs
import { generateApiKeyPair } from '@unrdf/daemon';

const { key, hash } = await generateApiKeyPair();

console.log('API Key (distribute to users):', key);
console.log('API Key Hash (store in secrets manager):', hash);
```

```bash
node generate-keys.mjs > /tmp/api-keys.txt
chmod 600 /tmp/api-keys.txt

# Securely store keys
# Option A: AWS Secrets Manager
aws secretsmanager create-secret \
  --name unrdf/production/api-key-hash \
  --secret-string "$(cat /tmp/api-keys.txt | grep Hash | cut -d: -f2)"

# Option B: HashiCorp Vault
vault kv put secret/unrdf/production/api-key-hash \
  value="$(cat /tmp/api-keys.txt | grep Hash | cut -d: -f2)"

# Securely delete temp file
shred -u /tmp/api-keys.txt
```

#### 3.2 Configure Environment Variables

```bash
# /opt/unrdf/app/.env.production
NODE_ENV=production
DAEMON_PORT=8080
DAEMON_CONCURRENCY=10
DAEMON_LOG_LEVEL=info

# API Key (from secrets manager at runtime)
# UNRDF_API_KEY will be injected at startup

# Database (if using persistent storage)
DATABASE_URL=postgresql://user:pass@localhost/unrdf
DATABASE_POOL_SIZE=20

# Observability
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
OTEL_SERVICE_NAME=unrdf-daemon
```

**Security Note**: Never commit `.env.production` to version control.

#### 3.3 Configure SSL/TLS

**Option A: nginx reverse proxy**

```nginx
# /etc/nginx/sites-available/unrdf
server {
  listen 443 ssl http2;
  server_name api.yourcompany.com;

  ssl_certificate /etc/ssl/certs/unrdf.crt;
  ssl_certificate_key /etc/ssl/private/unrdf.key;
  ssl_protocols TLSv1.2 TLSv1.3;
  ssl_ciphers HIGH:!aNULL:!MD5;

  location / {
    proxy_pass http://localhost:8080;
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;

    # Pass API key header
    proxy_set_header X-API-Key $http_x_api_key;
  }
}
```

```bash
sudo ln -s /etc/nginx/sites-available/unrdf /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

**Option B: Direct TLS in Node.js**

```javascript
// server.mjs
import https from 'https';
import fs from 'fs';

const options = {
  key: fs.readFileSync('/etc/ssl/private/unrdf.key'),
  cert: fs.readFileSync('/etc/ssl/certs/unrdf.crt'),
};

https.createServer(options, app).listen(8443);
```

---

### Step 4: Process Management (30 minutes)

#### 4.1 Create systemd Service

```ini
# /etc/systemd/system/unrdf-daemon.service
[Unit]
Description=UNRDF Background Daemon
Documentation=https://github.com/unrdf/unrdf
After=network.target

[Service]
Type=simple
User=unrdf
Group=unrdf
WorkingDirectory=/opt/unrdf/app
Environment=NODE_ENV=production

# Load secrets from secrets manager
ExecStartPre=/opt/unrdf/scripts/load-secrets.sh

ExecStart=/usr/bin/node /opt/unrdf/app/src/index.mjs
Restart=always
RestartSec=10

# Security hardening
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=/opt/unrdf/app/logs

# Resource limits
LimitNOFILE=65536
LimitNPROC=32768

# Logging
StandardOutput=journal
StandardError=journal
SyslogIdentifier=unrdf-daemon

[Install]
WantedBy=multi-user.target
```

#### 4.2 Create Secrets Loading Script

```bash
# /opt/unrdf/scripts/load-secrets.sh
#!/bin/bash
set -euo pipefail

# Load API key hash from AWS Secrets Manager
export UNRDF_API_KEY=$(aws secretsmanager get-secret-value \
  --secret-id unrdf/production/api-key-hash \
  --query SecretString \
  --output text)

# Or from HashiCorp Vault
# export UNRDF_API_KEY=$(vault kv get -field=value secret/unrdf/production/api-key-hash)

echo "Secrets loaded successfully"
```

```bash
chmod +x /opt/unrdf/scripts/load-secrets.sh
```

#### 4.3 Enable and Start Service

```bash
sudo systemctl daemon-reload
sudo systemctl enable unrdf-daemon
sudo systemctl start unrdf-daemon

# Verify service is running
sudo systemctl status unrdf-daemon

# Check logs
sudo journalctl -u unrdf-daemon -f
```

---

### Step 5: Monitoring & Observability (45 minutes)

#### 5.1 Configure OTEL Collector

```yaml
# /etc/otel-collector/config.yaml
receivers:
  otlp:
    protocols:
      http:
        endpoint: 0.0.0.0:4318

processors:
  batch:
    timeout: 10s

exporters:
  prometheus:
    endpoint: "0.0.0.0:9090"
  logging:
    loglevel: debug

service:
  pipelines:
    traces:
      receivers: [otlp]
      processors: [batch]
      exporters: [logging]
    metrics:
      receivers: [otlp]
      processors: [batch]
      exporters: [prometheus]
```

#### 5.2 Configure Prometheus

```yaml
# /etc/prometheus/prometheus.yml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'unrdf-daemon'
    static_configs:
      - targets: ['localhost:9090']
```

#### 5.3 Create Grafana Dashboards

Import UNRDF dashboard template:

- Dashboard ID: (to be published)
- Panels: Health metrics, operation latency, success rate, active operations

#### 5.4 Configure Alerting

```yaml
# prometheus-alerts.yml
groups:
  - name: unrdf-alerts
    interval: 30s
    rules:
      - alert: HighFailureRate
        expr: (rate(unrdf_operations_failed[5m]) / rate(unrdf_operations_total[5m])) > 0.1
        for: 5m
        annotations:
          summary: "High daemon operation failure rate"

      - alert: ServiceDown
        expr: up{job="unrdf-daemon"} == 0
        for: 1m
        annotations:
          summary: "UNRDF daemon is down"
```

---

### Step 6: Backup & Disaster Recovery (30 minutes)

#### 6.1 Configure Automated Backups

```bash
# /opt/unrdf/scripts/backup.sh
#!/bin/bash
BACKUP_DIR=/var/backups/unrdf
DATE=$(date +%Y%m%d-%H%M%S)

# Backup RDF data
pg_dump unrdf | gzip > "$BACKUP_DIR/unrdf-$DATE.sql.gz"

# Backup configuration
tar czf "$BACKUP_DIR/config-$DATE.tar.gz" /opt/unrdf/app/.env.production

# Retain 30 days of backups
find "$BACKUP_DIR" -type f -mtime +30 -delete

echo "Backup completed: $DATE"
```

#### 6.2 Schedule Backups

```bash
# crontab -e (as unrdf user)
0 2 * * * /opt/unrdf/scripts/backup.sh >> /var/log/unrdf-backup.log 2>&1
```

#### 6.3 Test Recovery Procedure

```bash
# Simulate recovery
gunzip -c /var/backups/unrdf/unrdf-YYYYMMDD-HHMMSS.sql.gz | psql unrdf_test
```

---

### Step 7: Verification & Testing (30 minutes)

#### 7.1 Health Check

```bash
curl -k https://api.yourcompany.com/api/daemon/health
# Expected: {"isRunning":true,...}
```

#### 7.2 Authentication Test

```bash
# Without API key (should fail)
curl -X POST https://api.yourcompany.com/api/daemon/execute \
  -H "Content-Type: application/json" \
  -d '{"operationId":"test"}'
# Expected: 401 Unauthorized

# With valid API key (should succeed)
curl -X POST https://api.yourcompany.com/api/daemon/execute \
  -H "Content-Type: application/json" \
  -H "X-API-Key: YOUR_API_KEY" \
  -d '{"operationId":"test"}'
# Expected: 200 OK
```

#### 7.3 Load Test

```bash
# Using Apache Bench
ab -n 1000 -c 10 \
  -H "X-API-Key: YOUR_API_KEY" \
  https://api.yourcompany.com/api/daemon/health

# Verify:
# - Success rate >99%
# - P95 latency <100ms
# - No errors in logs
```

#### 7.4 Security Scan

```bash
# Scan for vulnerabilities
pnpm audit --audit-level=high
# Expected: 0 vulnerabilities

# SSL/TLS test
nmap --script ssl-enum-ciphers -p 443 api.yourcompany.com
# Expected: Only TLS 1.2+ enabled
```

---

## Post-Deployment

### Immediate Actions (Day 1)

- [ ] Monitor logs for errors
- [ ] Verify all operations executing successfully
- [ ] Check authentication metrics
- [ ] Review performance dashboards
- [ ] Test backup restoration
- [ ] Document any issues

### First Week

- [ ] Review audit logs daily
- [ ] Monitor resource usage trends
- [ ] Validate alerting is working
- [ ] Conduct security review
- [ ] Performance optimization if needed

### Ongoing Maintenance

- [ ] Weekly security scans
- [ ] Monthly dependency updates
- [ ] Quarterly penetration testing
- [ ] Regular backup testing
- [ ] Key rotation (quarterly)

---

## Rollback Procedure

If issues arise during deployment:

### Step 1: Stop New Service

```bash
sudo systemctl stop unrdf-daemon
```

### Step 2: Restore Previous Version

```bash
cd /opt/unrdf
mv app app.new
mv app.old app
```

### Step 3: Restart Previous Service

```bash
sudo systemctl start unrdf-daemon-old
sudo systemctl status unrdf-daemon-old
```

### Step 4: Verify Rollback

```bash
curl https://api.yourcompany.com/api/daemon/health
# Verify service is operational
```

---

## Troubleshooting

### Service Won't Start

**Check logs**:
```bash
sudo journalctl -u unrdf-daemon -n 100
```

**Common issues**:
1. Missing environment variables
2. Port already in use
3. Database connection failed
4. Secrets not loaded

### Authentication Failing

**Check API key**:
```bash
echo $UNRDF_API_KEY  # Should be set
```

**Verify hash**:
```javascript
import { verifyApiKey } from '@unrdf/daemon';
const isValid = await verifyApiKey(key, hash);
```

### High Memory Usage

**Monitor memory**:
```bash
top -p $(pgrep -f unrdf-daemon)
```

**Adjust Node.js memory**:
```bash
# In systemd service file
Environment=NODE_OPTIONS="--max-old-space-size=4096"
```

---

## Security Hardening

### Additional Recommendations

1. **Rate Limiting**: Add nginx rate limiting
   ```nginx
   limit_req_zone $binary_remote_addr zone=api:10m rate=10r/s;
   limit_req zone=api burst=20;
   ```

2. **Fail2ban**: Block repeated failed auth
   ```bash
   sudo apt-get install fail2ban
   # Configure filter for UNRDF auth failures
   ```

3. **SELinux/AppArmor**: Confine daemon process

4. **Network Isolation**: Use VPC/security groups

5. **Intrusion Detection**: Configure OSSEC/Wazuh

---

## Compliance Checklist

### GDPR Compliance

- [ ] Data encryption at rest and in transit
- [ ] Audit logging enabled
- [ ] Data retention policies configured
- [ ] Right to deletion implemented

### SOC 2 Compliance

- [ ] Access controls documented
- [ ] Change management process defined
- [ ] Incident response plan documented
- [ ] Regular security audits scheduled

### HIPAA Compliance (if applicable)

- [ ] PHI encryption validated
- [ ] Access audit logs enabled
- [ ] BAA agreements in place
- [ ] Breach notification procedures documented

---

## Resources

- [Security Configuration](SECURITY_CONFIGURATION.md)
- [Performance Tuning](PERFORMANCE_TUNING.md)
- [Migration Guide](../MIGRATING_TO_V6.md)
- [API Documentation](../API_DOCUMENTATION_V6.md)

## Support

- **Documentation**: https://unrdf.dev
- **GitHub Issues**: https://github.com/unrdf/unrdf/issues
- **Security Issues**: security@unrdf.dev
- **Enterprise Support**: support@unrdf.dev

---

**Deployment Status**: Production-Ready ✅
**Last Updated**: 2026-01-11
**Version**: 6.0.0
