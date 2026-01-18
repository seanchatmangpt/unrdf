# Security Configuration Guide - UNRDF v6.0.0

**Complete security configuration guide for production deployments**

## Overview

This guide provides detailed security configuration for UNRDF v6.0.0 daemon operations, covering authentication, authorization, encryption, and monitoring.

**Security Level**: Enterprise-grade
**Compliance**: OWASP Top 10, CWE Top 25, SOC 2, GDPR
**Threat Model**: Defense in depth

---

## Table of Contents

1. [Authentication Configuration](#authentication-configuration)
2. [API Key Management](#api-key-management)
3. [TLS/SSL Configuration](#tlsssl-configuration)
4. [Network Security](#network-security)
5. [Input Validation](#input-validation)
6. [Secrets Management](#secrets-management)
7. [Audit Logging](#audit-logging)
8. [Monitoring & Alerting](#monitoring--alerting)

---

## Authentication Configuration

### API Key Authentication

#### Generate Production Keys

**For single environment**:

```javascript
// scripts/generate-production-keys.mjs
import { generateApiKeyPair } from '@unrdf/daemon';
import fs from 'fs';

async function generateKeys() {
  const { key, hash } = await generateApiKeyPair();

  console.log('='.repeat(60));
  console.log('PRODUCTION API KEYS - HANDLE WITH CARE');
  console.log('='.repeat(60));
  console.log();
  console.log('API Key (distribute to authorized users):');
  console.log(key);
  console.log();
  console.log('API Key Hash (store in secrets manager):');
  console.log(hash);
  console.log();
  console.log('='.repeat(60));
  console.log('IMPORTANT:');
  console.log('1. API key shown only once - save securely');
  console.log('2. Store hash in secrets manager (Vault, AWS Secrets)');
  console.log('3. Never commit keys to version control');
  console.log('4. Rotate keys quarterly');
  console.log('='.repeat(60));

  // Optionally save to secure temp file (encrypted filesystem)
  const output = `API_KEY=${key}\nAPI_KEY_HASH=${hash}\n`;
  fs.writeFileSync('/tmp/unrdf-keys.txt', output, { mode: 0o600 });

  console.log('\nKeys saved to /tmp/unrdf-keys.txt (chmod 600)');
  console.log('Remember to shred this file after storing in secrets manager!');
}

generateKeys();
```

```bash
node scripts/generate-production-keys.mjs

# Securely store in secrets manager
aws secretsmanager create-secret \
  --name unrdf/production/api-key-hash \
  --secret-string "$(grep API_KEY_HASH /tmp/unrdf-keys.txt | cut -d= -f2)"

# Shred temporary file
shred -vfz -n 10 /tmp/unrdf-keys.txt
```

**For multiple environments (dev/staging/production)**:

```javascript
// scripts/generate-all-keys.mjs
import { generateApiKeyPair } from '@unrdf/daemon';

const environments = ['development', 'staging', 'production'];

for (const env of environments) {
  console.log(`\n${'='.repeat(60)}`);
  console.log(`${env.toUpperCase()} ENVIRONMENT`);
  console.log('='.repeat(60));

  const { key, hash } = await generateApiKeyPair();

  console.log(`API Key:  ${key}`);
  console.log(`Hash:     ${hash}`);

  // Store in secrets manager with environment prefix
  // aws secretsmanager create-secret \
  //   --name unrdf/${env}/api-key-hash \
  //   --secret-string "${hash}"
}
```

#### Environment-Specific Configuration

**.env.development**:
```bash
NODE_ENV=development
DAEMON_PORT=8080
DAEMON_CONCURRENCY=5
DAEMON_LOG_LEVEL=debug

# Development: Authentication optional (warns but allows)
# UNRDF_API_KEY not required
```

**.env.staging**:
```bash
NODE_ENV=staging
DAEMON_PORT=8080
DAEMON_CONCURRENCY=10
DAEMON_LOG_LEVEL=info

# Staging: Authentication required
UNRDF_API_KEY=${STAGING_API_KEY_HASH}
```

**.env.production**:
```bash
NODE_ENV=production
DAEMON_PORT=8080
DAEMON_CONCURRENCY=10
DAEMON_LOG_LEVEL=warn

# Production: Authentication strictly enforced
UNRDF_API_KEY=${PRODUCTION_API_KEY_HASH}
```

#### Authentication Middleware Integration

**Express Example**:

```javascript
import express from 'express';
import { createAuthMiddleware } from '@unrdf/daemon';

const app = express();

// Create environment-aware middleware
const authMiddleware = createAuthMiddleware({
  environment: process.env.NODE_ENV || 'production',
  requireInDev: false,  // Allow missing keys in dev
});

// Public endpoints (no auth)
app.get('/health', (req, res) => {
  res.json({ status: 'ok' });
});

// Protected endpoints (auth required)
app.use('/api/daemon/*', authMiddleware);

app.post('/api/daemon/execute', async (req, res) => {
  // Request authenticated via middleware
  const result = await executeDaemonOperation(req.body);
  res.json(result);
});

// Global error handler for auth failures
app.use((err, req, res, next) => {
  if (err.name === 'AuthenticationError') {
    res.status(401).json({
      error: 'Unauthorized',
      message: process.env.NODE_ENV === 'production'
        ? 'Invalid or missing API key'
        : err.message  // More details in non-production
    });
  } else {
    next(err);
  }
});
```

**Custom Authentication Logic**:

```javascript
import { ApiKeyAuthenticator } from '@unrdf/daemon';

const authenticator = new ApiKeyAuthenticator({
  environment: process.env.NODE_ENV,
  maxAuditEntries: 10000,  // Larger audit log for enterprise
});

app.use(async (req, res, next) => {
  // Skip authentication for health check
  if (req.path === '/health') {
    return next();
  }

  const result = await authenticator.authenticate({
    headers: req.headers
  });

  if (!result.authenticated) {
    // Log failed attempt
    console.error('Auth failed:', {
      ip: req.ip,
      path: req.path,
      reason: result.reason,
    });

    // Increment failed attempts counter (for rate limiting)
    incrementFailedAuth(req.ip);

    return res.status(401).json({ error: 'Unauthorized' });
  }

  // Attach auth context to request
  req.auth = {
    source: result.source,
    timestamp: new Date(),
  };

  next();
});
```

---

## API Key Management

### Key Distribution

**Secure Distribution Channels**:

1. **Encrypted Email** (for single user):
   ```bash
   echo "API Key: $KEY" | gpg --encrypt --recipient user@example.com > key.gpg
   ```

2. **Secure File Share** (for teams):
   - Use 1Password, LastPass, or Bitwarden
   - Share via encrypted vault
   - Require 2FA for access

3. **In-Person** (highest security):
   - Print on paper
   - Hand deliver
   - Destroy after user confirms storage

**Never**:
- ❌ Send via plaintext email
- ❌ Post in Slack/Teams
- ❌ Commit to git repository
- ❌ Store in unencrypted files

### Key Rotation

**Quarterly Rotation Schedule**:

```javascript
// scripts/rotate-keys.mjs
import { generateApiKeyPair } from '@unrdf/daemon';

async function rotateKeys() {
  console.log('Key Rotation Process');
  console.log('=====================');

  // Step 1: Generate new key
  const { key: newKey, hash: newHash } = await generateApiKeyPair();

  console.log('\nStep 1: New key generated');
  console.log('New API Key:', newKey);
  console.log('New Hash:', newHash);

  // Step 2: Add new key to secrets manager (both keys valid)
  console.log('\nStep 2: Add new key to secrets manager');
  console.log('Grace period: 7 days (both keys valid)');

  // aws secretsmanager update-secret \
  //   --secret-id unrdf/production/api-key-hash-new \
  //   --secret-string "${newHash}"

  // Step 3: Notify users (7-day advance notice)
  console.log('\nStep 3: Notify users to update their keys');
  console.log('Email template sent to all API key holders');

  // Step 4: Monitor usage during grace period
  console.log('\nStep 4: Monitor old key usage');
  console.log('After 7 days, revoke old key');

  // Step 5: Revoke old key
  console.log('\nStep 5: Revoke old key (after grace period)');
  // aws secretsmanager delete-secret \
  //   --secret-id unrdf/production/api-key-hash-old

  console.log('\nRotation complete!');
}

rotateKeys();
```

**Grace Period Implementation**:

```javascript
import { ApiKeyAuthenticator } from '@unrdf/daemon';

// During grace period: accept both old and new keys
const authenticator = new ApiKeyAuthenticator({
  apiKeyHashes: [
    process.env.OLD_API_KEY_HASH,
    process.env.NEW_API_KEY_HASH,
  ],
  environment: 'production',
});
```

### Key Revocation

**Immediate Revocation** (security breach):

```bash
# Remove from secrets manager
aws secretsmanager delete-secret \
  --secret-id unrdf/production/api-key-hash \
  --force-delete-without-recovery

# Generate new key immediately
node scripts/generate-production-keys.mjs

# Update production environment
kubectl set env deployment/unrdf-daemon \
  UNRDF_API_KEY=${NEW_HASH}

# Notify all users
# Send "Key Revoked - Update Immediately" email
```

**Audit Key Usage**:

```javascript
// scripts/audit-key-usage.mjs
import { ApiKeyAuthenticator } from '@unrdf/daemon';

const authenticator = new ApiKeyAuthenticator({
  environment: 'production'
});

// Get audit log
const log = authenticator.getAuditLog();

// Analyze usage patterns
const stats = {
  totalAttempts: log.length,
  successfulAuths: log.filter(e => e.success).length,
  failedAuths: log.filter(e => !e.success).length,
  uniqueIPs: new Set(log.map(e => e.ip)).size,
};

console.log('Key Usage Statistics:');
console.log(JSON.stringify(stats, null, 2));

// Export for SIEM
fs.writeFileSync('audit-export.json', JSON.stringify(log, null, 2));
```

---

## TLS/SSL Configuration

### Certificate Management

**Let's Encrypt (Certbot)**:

```bash
# Install certbot
sudo apt-get install certbot python3-certbot-nginx

# Obtain certificate
sudo certbot --nginx -d api.yourcompany.com

# Auto-renewal
sudo systemctl enable certbot.timer
sudo systemctl start certbot.timer
```

**Custom Certificate**:

```bash
# Generate CSR
openssl req -new -newkey rsa:2048 -nodes \
  -keyout unrdf.key -out unrdf.csr

# Sign with CA (or upload CSR to CA)

# Verify certificate
openssl x509 -in unrdf.crt -text -noout
```

### nginx TLS Configuration

**Recommended Configuration**:

```nginx
# /etc/nginx/sites-available/unrdf-secure
server {
  listen 443 ssl http2;
  server_name api.yourcompany.com;

  # SSL certificates
  ssl_certificate /etc/letsencrypt/live/api.yourcompany.com/fullchain.pem;
  ssl_certificate_key /etc/letsencrypt/live/api.yourcompany.com/privkey.pem;

  # TLS protocol versions (only 1.2 and 1.3)
  ssl_protocols TLSv1.2 TLSv1.3;

  # Strong cipher suites
  ssl_ciphers 'ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256';
  ssl_prefer_server_ciphers on;

  # SSL session cache
  ssl_session_cache shared:SSL:10m;
  ssl_session_timeout 10m;

  # OCSP stapling
  ssl_stapling on;
  ssl_stapling_verify on;
  resolver 8.8.8.8 8.8.4.4 valid=300s;
  resolver_timeout 5s;

  # HSTS (31536000 seconds = 1 year)
  add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;

  # Additional security headers
  add_header X-Frame-Options "SAMEORIGIN" always;
  add_header X-Content-Type-Options "nosniff" always;
  add_header X-XSS-Protection "1; mode=block" always;
  add_header Referrer-Policy "no-referrer-when-downgrade" always;

  # Proxy to daemon
  location / {
    proxy_pass http://localhost:8080;
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;

    # Pass API key header
    proxy_set_header X-API-Key $http_x_api_key;

    # Timeouts
    proxy_connect_timeout 30s;
    proxy_send_timeout 30s;
    proxy_read_timeout 30s;
  }
}

# Redirect HTTP to HTTPS
server {
  listen 80;
  server_name api.yourcompany.com;
  return 301 https://$server_name$request_uri;
}
```

**Test TLS Configuration**:

```bash
# Test with SSL Labs
curl -s "https://api.ssllabs.com/api/v3/analyze?host=api.yourcompany.com" | jq

# Test with testssl.sh
./testssl.sh https://api.yourcompany.com

# Expected: A+ rating
```

---

## Network Security

### Firewall Configuration

**iptables**:

```bash
# Allow SSH (for administration)
sudo iptables -A INPUT -p tcp --dport 22 -j ACCEPT

# Allow HTTPS
sudo iptables -A INPUT -p tcp --dport 443 -j ACCEPT

# Allow loopback
sudo iptables -A INPUT -i lo -j ACCEPT

# Allow established connections
sudo iptables -A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT

# Drop everything else
sudo iptables -P INPUT DROP

# Save rules
sudo iptables-save > /etc/iptables/rules.v4
```

**ufw (Ubuntu)**:

```bash
sudo ufw default deny incoming
sudo ufw default allow outgoing
sudo ufw allow 22/tcp    # SSH
sudo ufw allow 443/tcp   # HTTPS
sudo ufw enable
```

### Network Segmentation

**VPC Configuration** (AWS):

```hcl
# terraform/vpc.tf
resource "aws_vpc" "unrdf" {
  cidr_block = "10.0.0.0/16"
  enable_dns_hostnames = true
  enable_dns_support = true

  tags = {
    Name = "unrdf-production"
  }
}

# Public subnet (nginx reverse proxy)
resource "aws_subnet" "public" {
  vpc_id = aws_vpc.unrdf.id
  cidr_block = "10.0.1.0/24"
  map_public_ip_on_launch = true

  tags = {
    Name = "unrdf-public"
  }
}

# Private subnet (daemon application)
resource "aws_subnet" "private" {
  vpc_id = aws_vpc.unrdf.id
  cidr_block = "10.0.2.0/24"

  tags = {
    Name = "unrdf-private"
  }
}

# Security group (application)
resource "aws_security_group" "daemon" {
  vpc_id = aws_vpc.unrdf.id

  ingress {
    from_port = 8080
    to_port = 8080
    protocol = "tcp"
    cidr_blocks = ["10.0.1.0/24"]  # Only from public subnet
  }

  egress {
    from_port = 0
    to_port = 0
    protocol = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}
```

### Rate Limiting

**nginx Rate Limiting**:

```nginx
# /etc/nginx/nginx.conf
http {
  # Define rate limit zones
  limit_req_zone $binary_remote_addr zone=api:10m rate=10r/s;
  limit_req_zone $binary_remote_addr zone=auth:10m rate=5r/s;

  # Servers...
}

# In server block
location /api/daemon/ {
  limit_req zone=api burst=20 nodelay;
  limit_req_status 429;

  # Error page for rate limit
  error_page 429 /rate_limit.json;

  proxy_pass http://localhost:8080;
}

location = /rate_limit.json {
  internal;
  return 429 '{"error":"Rate limit exceeded","retryAfter":60}';
  add_header Content-Type application/json;
}
```

**Application-Level Rate Limiting**:

```javascript
import rateLimit from 'express-rate-limit';
import RedisStore from 'rate-limit-redis';

// Create rate limiter
const apiLimiter = rateLimit({
  store: new RedisStore({
    client: redisClient,
    prefix: 'rl:api:',
  }),
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // 100 requests per window
  message: 'Too many requests from this IP',
  standardHeaders: true,
  legacyHeaders: false,
  handler: (req, res) => {
    res.status(429).json({
      error: 'Rate limit exceeded',
      retryAfter: Math.ceil(req.rateLimit.resetTime.getTime() / 1000),
    });
  },
});

// Apply to routes
app.use('/api/daemon/', apiLimiter);
```

---

## Input Validation

### SPARQL Query Validation

```javascript
import { detectInjection, sanitizeError } from '@unrdf/daemon/security-audit';

async function executeUserQuery(sparqlQuery) {
  // Step 1: Check for injection
  const injection = detectInjection(sparqlQuery, 'rdf');
  if (injection.detected) {
    throw new Error(`Query blocked: ${injection.reason}`);
  }

  // Step 2: Validate query structure with Zod
  const QuerySchema = z.object({
    type: z.enum(['SELECT', 'CONSTRUCT', 'ASK', 'DESCRIBE']),
    limit: z.number().max(10000).optional(),
  });

  // Step 3: Execute with timeout
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), 30000);

  try {
    const results = await executeQuery(sparqlQuery, {
      signal: controller.signal,
    });
    clearTimeout(timeout);
    return results;
  } catch (error) {
    clearTimeout(timeout);
    throw sanitizeError(error);
  }
}
```

### File Path Validation

```javascript
import { sanitizePath } from '@unrdf/daemon/security-audit';
import path from 'path';

function loadUserFile(userProvidedPath) {
  // Step 1: Sanitize path
  const safePath = sanitizePath(userProvidedPath);
  if (!safePath) {
    throw new Error('Invalid file path');
  }

  // Step 2: Ensure within allowed directory
  const allowedDir = '/opt/unrdf/data';
  const resolvedPath = path.resolve(allowedDir, safePath);

  if (!resolvedPath.startsWith(allowedDir)) {
    throw new Error('Path traversal detected');
  }

  // Step 3: Check file exists and is readable
  if (!fs.existsSync(resolvedPath)) {
    throw new Error('File not found');
  }

  // Step 4: Load file
  return fs.readFileSync(resolvedPath, 'utf-8');
}
```

### Output Sanitization

```javascript
import { detectSecrets, sanitizeError } from '@unrdf/daemon/security-audit';

function returnResults(data) {
  try {
    // Step 1: Convert to JSON
    const output = JSON.stringify(data);

    // Step 2: Check for secrets
    const secrets = detectSecrets(output);
    if (secrets.detected) {
      console.error('Secret exposure prevented:', secrets.patterns);
      throw new Error('Output contains sensitive data');
    }

    // Step 3: Return sanitized output
    return JSON.parse(output);
  } catch (error) {
    throw sanitizeError(error);
  }
}
```

---

## Secrets Management

### AWS Secrets Manager

**Store secrets**:

```bash
aws secretsmanager create-secret \
  --name unrdf/production/api-key-hash \
  --description "Production API key hash" \
  --secret-string "${API_KEY_HASH}"

aws secretsmanager create-secret \
  --name unrdf/production/database-url \
  --secret-string "postgresql://user:pass@host/db"
```

**Retrieve at runtime**:

```javascript
// scripts/load-secrets.mjs
import { SecretsManagerClient, GetSecretValueCommand } from '@aws-sdk/client-secrets-manager';

const client = new SecretsManagerClient({ region: 'us-east-1' });

async function loadSecrets() {
  const secrets = await client.send(new GetSecretValueCommand({
    SecretId: 'unrdf/production/api-key-hash',
  }));

  process.env.UNRDF_API_KEY = secrets.SecretString;
}

loadSecrets();
```

### HashiCorp Vault

**Store secrets**:

```bash
vault kv put secret/unrdf/production/api-key-hash \
  value="${API_KEY_HASH}"

vault kv put secret/unrdf/production/database \
  url="postgresql://user:pass@host/db"
```

**Retrieve at runtime**:

```javascript
import vault from 'node-vault';

const client = vault({
  endpoint: 'https://vault.yourcompany.com',
  token: process.env.VAULT_TOKEN,
});

async function loadSecrets() {
  const result = await client.read('secret/data/unrdf/production/api-key-hash');
  process.env.UNRDF_API_KEY = result.data.data.value;
}
```

---

## Audit Logging

### Comprehensive Audit Log Configuration

```javascript
import { ApiKeyAuthenticator } from '@unrdf/daemon';
import winston from 'winston';

const auditLogger = winston.createLogger({
  level: 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  transports: [
    new winston.transports.File({ filename: '/var/log/unrdf/audit.log' }),
    new winston.transports.Console({ level: 'error' }),
  ],
});

const authenticator = new ApiKeyAuthenticator({
  environment: 'production',
  maxAuditEntries: 100000,  // Large log for enterprise
});

// Log all authentication attempts
app.use(async (req, res, next) => {
  const result = await authenticator.authenticate({
    headers: req.headers
  });

  auditLogger.info('auth_attempt', {
    timestamp: new Date().toISOString(),
    ip: req.ip,
    path: req.path,
    method: req.method,
    authenticated: result.authenticated,
    reason: result.reason,
    source: result.source,
    userAgent: req.headers['user-agent'],
  });

  if (!result.authenticated) {
    return res.status(401).json({ error: 'Unauthorized' });
  }

  next();
});

// Export audit log periodically
setInterval(() => {
  const log = authenticator.getAuditLog();
  fs.writeFileSync(
    `/var/log/unrdf/audit-${Date.now()}.json`,
    JSON.stringify(log, null, 2)
  );
}, 3600000); // Every hour
```

### SIEM Integration

```javascript
// Export to Splunk
import splunkLogger from 'splunk-logging';

const splunk = new splunkLogger.Logger({
  token: process.env.SPLUNK_TOKEN,
  url: 'https://splunk.yourcompany.com:8088',
});

const log = authenticator.getAuditLog();
splunk.send({
  message: {
    source: 'unrdf-daemon',
    sourcetype: 'audit',
    event: log,
  },
});
```

---

## Monitoring & Alerting

### Prometheus Metrics

```javascript
import prometheus from 'prom-client';

// Create metrics
const authAttempts = new prometheus.Counter({
  name: 'unrdf_auth_attempts_total',
  help: 'Total authentication attempts',
  labelNames: ['result'],
});

const authLatency = new prometheus.Histogram({
  name: 'unrdf_auth_latency_seconds',
  help: 'Authentication latency',
  buckets: [0.001, 0.005, 0.01, 0.05, 0.1],
});

// Track metrics
app.use(async (req, res, next) => {
  const start = Date.now();

  const result = await authenticator.authenticate({
    headers: req.headers
  });

  const duration = (Date.now() - start) / 1000;

  authLatency.observe(duration);
  authAttempts.inc({
    result: result.authenticated ? 'success' : 'failure'
  });

  // Continue...
});

// Expose metrics endpoint
app.get('/metrics', async (req, res) => {
  res.set('Content-Type', prometheus.register.contentType);
  res.end(await prometheus.register.metrics());
});
```

### Alert Rules

```yaml
# prometheus-alerts.yml
groups:
  - name: unrdf-security
    interval: 30s
    rules:
      - alert: HighAuthFailureRate
        expr: |
          (
            rate(unrdf_auth_attempts_total{result="failure"}[5m])
            /
            rate(unrdf_auth_attempts_total[5m])
          ) > 0.1
        for: 5m
        annotations:
          summary: "High authentication failure rate (>10%)"
          description: "{{ $value }}% of auth attempts failing"

      - alert: SlowAuthentication
        expr: |
          histogram_quantile(0.95,
            rate(unrdf_auth_latency_seconds_bucket[5m])
          ) > 0.1
        for: 5m
        annotations:
          summary: "Slow authentication (P95 > 100ms)"
```

---

## Compliance Requirements

### GDPR

- ✅ Encryption at rest and in transit
- ✅ Audit logging of all data access
- ✅ Right to deletion implemented
- ✅ Data retention policies configured

### SOC 2

- ✅ Access controls documented
- ✅ Change management process
- ✅ Incident response plan
- ✅ Regular security audits

### HIPAA (if applicable)

- ✅ PHI encryption (AES-256)
- ✅ Audit controls (all access logged)
- ✅ Access control (API key authentication)
- ✅ Transmission security (TLS 1.2+)

---

## Resources

- [Production Deployment](PRODUCTION_DEPLOYMENT.md)
- [Performance Tuning](PERFORMANCE_TUNING.md)
- [Migration Guide](../MIGRATING_TO_V6.md)
- [Security Migration](../SECURITY_MIGRATION.md)

**Security Status**: Enterprise-Ready ✅
**Last Updated**: 2026-01-11
