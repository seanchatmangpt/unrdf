# Security and Secrets Management Guide

Best practices for securing UNRDF deployments and managing secrets in production.

## Table of Contents

- [Threat Model](#threat-model)
- [Authentication](#authentication)
- [Authorization](#authorization)
- [Secrets Management](#secrets-management)
- [SPARQL Query Security](#sparql-query-security)
- [Rate Limiting](#rate-limiting)
- [Encryption](#encryption)
- [Audit Logging](#audit-logging)
- [Security Checklist](#security-checklist)

---

## Threat Model

### Attack Vectors

1. **SPARQL Injection:** Malicious queries that access unauthorized data
2. **Query Complexity Attacks:** Expensive queries that DoS the system
3. **Exposed Secrets:** API keys, database credentials leaked
4. **Unauthorized Access:** Missing authentication/authorization
5. **Data Exfiltration:** Bulk export of RDF data
6. **Man-in-the-Middle:** Unencrypted network traffic

---

## Authentication

### API Key Authentication

**Implementation:**

```javascript
// middleware/auth.mjs
import { z } from 'zod';

const ApiKeySchema = z.string().min(32);

export function requireApiKey(req, res, next) {
  const apiKey = req.headers['x-api-key'];

  if (!apiKey) {
    return res.status(401).json({ error: 'Missing API key' });
  }

  try {
    ApiKeySchema.parse(apiKey);

    if (apiKey !== process.env.API_KEY) {
      return res.status(401).json({ error: 'Invalid API key' });
    }

    next();
  } catch (error) {
    return res.status(401).json({ error: 'Invalid API key format' });
  }
}
```

**Usage:**

```javascript
import express from 'express';
import { requireApiKey } from './middleware/auth.mjs';

const app = express();

// Protect all routes
app.use(requireApiKey);

app.post('/sparql', async (req, res) => {
  // Only authenticated requests reach here
  const results = await query(store, req.body.query);
  res.json(results);
});
```

**Generate Strong API Keys:**

```bash
# Generate 256-bit random API key
node -e "console.log(require('crypto').randomBytes(32).toString('hex'))"

# Output: a3f8b91c4e2d...  (64 hex characters)
```

---

### JWT Authentication

**For Multi-tenant Systems:**

```javascript
// middleware/jwt-auth.mjs
import jwt from 'jsonwebtoken';

export function requireJWT(req, res, next) {
  const token = req.headers['authorization']?.replace('Bearer ', '');

  if (!token) {
    return res.status(401).json({ error: 'Missing JWT token' });
  }

  try {
    const decoded = jwt.verify(token, process.env.JWT_SECRET);
    req.user = decoded;  // { userId, tenantId, roles }
    next();
  } catch (error) {
    return res.status(401).json({ error: 'Invalid token' });
  }
}
```

**Usage:**

```javascript
app.use(requireJWT);

app.post('/sparql', async (req, res) => {
  const { tenantId } = req.user;

  // Scope query to tenant's graph
  const results = await query(store, req.body.query, {
    defaultGraph: `http://tenant/${tenantId}/`
  });

  res.json(results);
});
```

---

## Authorization

### Role-Based Access Control (RBAC)

**Define Roles:**

```javascript
// config/roles.mjs
export const ROLES = {
  ADMIN: 'admin',
  EDITOR: 'editor',
  VIEWER: 'viewer'
};

export const PERMISSIONS = {
  READ_RDF: 'read:rdf',
  WRITE_RDF: 'write:rdf',
  DELETE_RDF: 'delete:rdf',
  QUERY_SPARQL: 'query:sparql',
  MANAGE_USERS: 'manage:users'
};

export const ROLE_PERMISSIONS = {
  [ROLES.ADMIN]: [
    PERMISSIONS.READ_RDF,
    PERMISSIONS.WRITE_RDF,
    PERMISSIONS.DELETE_RDF,
    PERMISSIONS.QUERY_SPARQL,
    PERMISSIONS.MANAGE_USERS
  ],
  [ROLES.EDITOR]: [
    PERMISSIONS.READ_RDF,
    PERMISSIONS.WRITE_RDF,
    PERMISSIONS.QUERY_SPARQL
  ],
  [ROLES.VIEWER]: [
    PERMISSIONS.READ_RDF,
    PERMISSIONS.QUERY_SPARQL
  ]
};
```

**Check Permissions:**

```javascript
// middleware/permissions.mjs
export function requirePermission(permission) {
  return (req, res, next) => {
    const userRole = req.user.role;
    const allowedPermissions = ROLE_PERMISSIONS[userRole];

    if (!allowedPermissions.includes(permission)) {
      return res.status(403).json({ error: 'Forbidden' });
    }

    next();
  };
}
```

**Usage:**

```javascript
import { requirePermission } from './middleware/permissions.mjs';
import { PERMISSIONS } from './config/roles.mjs';

// Read-only endpoint
app.get('/sparql', requirePermission(PERMISSIONS.QUERY_SPARQL), async (req, res) => {
  // SELECT queries only
});

// Write endpoint
app.post('/rdf', requirePermission(PERMISSIONS.WRITE_RDF), async (req, res) => {
  // Add/update triples
});

// Admin endpoint
app.delete('/users/:id', requirePermission(PERMISSIONS.MANAGE_USERS), async (req, res) => {
  // Admin only
});
```

---

## Secrets Management

### Environment Variables (Development)

**`.env` file (NEVER commit to Git):**

```bash
# API Keys
API_KEY=a3f8b91c4e2d...  # 256-bit random key
JWT_SECRET=d8f2a1b9c3e4...

# Database
OXIGRAPH_PATH=/data/rdf.db

# External Services
SPARQL_ENDPOINT_URL=https://api.example.org/sparql
SPARQL_ENDPOINT_API_KEY=xyz123...

# OTEL
OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318
```

**Load with `dotenv`:**

```javascript
import 'dotenv/config';

const apiKey = process.env.API_KEY;
```

**`.gitignore`:**

```
.env
.env.*
!.env.example
```

**`.env.example` (template, safe to commit):**

```bash
API_KEY=your-secret-api-key-here
JWT_SECRET=your-jwt-secret-here
OXIGRAPH_PATH=/data/rdf.db
```

---

### AWS Secrets Manager (Production)

**Store Secrets:**

```bash
aws secretsmanager create-secret \
  --name unrdf/production/api-key \
  --secret-string "a3f8b91c4e2d..."
```

**Retrieve Secrets:**

```javascript
import { SecretsManagerClient, GetSecretValueCommand } from '@aws-sdk/client-secrets-manager';

const client = new SecretsManagerClient({ region: 'us-east-1' });

async function getSecret(secretName) {
  const command = new GetSecretValueCommand({ SecretId: secretName });
  const response = await client.send(command);
  return response.SecretString;
}

// Usage
const apiKey = await getSecret('unrdf/production/api-key');
process.env.API_KEY = apiKey;
```

---

### HashiCorp Vault (Multi-Cloud)

**Retrieve Secrets:**

```javascript
import vault from 'node-vault';

const client = vault({
  endpoint: 'https://vault.example.com',
  token: process.env.VAULT_TOKEN
});

async function getSecret(path) {
  const response = await client.read(path);
  return response.data;
}

// Usage
const secrets = await getSecret('secret/unrdf/production');
process.env.API_KEY = secrets.api_key;
```

---

### Kubernetes Secrets

**Create Secret:**

```bash
kubectl create secret generic unrdf-secrets \
  --from-literal=api-key=a3f8b91c4e2d... \
  --from-literal=jwt-secret=d8f2a1b9c3e4...
```

**Mount as Environment Variables:**

```yaml
# k8s/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: unrdf-api
spec:
  template:
    spec:
      containers:
      - name: unrdf-api
        image: unrdf-api:latest
        env:
        - name: API_KEY
          valueFrom:
            secretKeyRef:
              name: unrdf-secrets
              key: api-key
        - name: JWT_SECRET
          valueFrom:
            secretKeyRef:
              name: unrdf-secrets
              key: jwt-secret
```

---

## SPARQL Query Security

### SPARQL Injection Prevention

**Vulnerable Code:**

```javascript
// ❌ DANGEROUS: User input directly in query
app.post('/search', async (req, res) => {
  const searchTerm = req.body.term;

  const sparql = `
    SELECT ?name WHERE {
      ?person foaf:name "${searchTerm}" .
    }
  `;

  const results = await query(store, sparql);
  res.json(results);
});

// Attack: searchTerm = '" . ?person ?p ?o . "'
// Resulting query exposes all triples!
```

**Secure Code (Parameterized Queries):**

```javascript
// ✅ SAFE: Use parameterized queries
import { prepareQuery } from '@unrdf/core/sparql';

app.post('/search', async (req, res) => {
  const searchTerm = req.body.term;

  const sparql = `
    SELECT ?name WHERE {
      ?person foaf:name ?searchTerm .
    }
  `;

  const results = await query(store, sparql, {
    bindings: {
      searchTerm: literal(searchTerm)
    }
  });

  res.json(results);
});
```

---

### Query Complexity Limits

**Prevent DoS Attacks:**

```javascript
// middleware/query-limits.mjs
const MAX_QUERY_COMPLEXITY = 100;
const MAX_RESULTS = 10000;

function estimateComplexity(sparql) {
  // Simple heuristic: count graph patterns
  const patterns = (sparql.match(/\{[^}]*\}/g) || []).length;
  const optionals = (sparql.match(/OPTIONAL/gi) || []).length;
  const unions = (sparql.match(/UNION/gi) || []).length;

  return patterns + optionals * 2 + unions * 3;
}

export function enforceQueryLimits(req, res, next) {
  const { query } = req.body;

  // Check complexity
  const complexity = estimateComplexity(query);
  if (complexity > MAX_QUERY_COMPLEXITY) {
    return res.status(400).json({
      error: 'Query too complex',
      complexity,
      maxAllowed: MAX_QUERY_COMPLEXITY
    });
  }

  // Enforce LIMIT clause
  if (!query.includes('LIMIT')) {
    req.body.query = `${query}\nLIMIT ${MAX_RESULTS}`;
  }

  next();
}
```

**Usage:**

```javascript
app.post('/sparql', enforceQueryLimits, async (req, res) => {
  const results = await query(store, req.body.query);
  res.json(results);
});
```

---

## Rate Limiting

### Per-IP Rate Limiting

```javascript
import rateLimit from 'express-rate-limit';

const limiter = rateLimit({
  windowMs: 15 * 60 * 1000,  // 15 minutes
  max: 100,                   // Limit each IP to 100 requests per window
  message: 'Too many requests, please try again later.',
  standardHeaders: true,
  legacyHeaders: false,
});

app.use('/sparql', limiter);
```

### Per-User Rate Limiting

```javascript
import rateLimit from 'express-rate-limit';
import RedisStore from 'rate-limit-redis';
import { createClient } from 'redis';

const redisClient = createClient({
  url: process.env.REDIS_URL
});

const limiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 1000,  // Higher limit for authenticated users
  keyGenerator: (req) => req.user.userId,  // Rate limit per user
  store: new RedisStore({
    client: redisClient,
    prefix: 'rate-limit:',
  }),
});

app.use(requireJWT);
app.use('/sparql', limiter);
```

---

## Encryption

### HTTPS Enforcement

```javascript
// Redirect HTTP to HTTPS
app.use((req, res, next) => {
  if (req.headers['x-forwarded-proto'] !== 'https' && process.env.NODE_ENV === 'production') {
    return res.redirect(301, `https://${req.hostname}${req.url}`);
  }
  next();
});
```

### Encrypt RDF Data at Rest

**Using Oxigraph with SQLite Encryption:**

```bash
# Install SQLCipher (encrypted SQLite)
pnpm add better-sqlite3-sqlcipher

# Set encryption key via environment variable
export OXIGRAPH_ENCRYPTION_KEY=your-encryption-key
```

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = createStore({
  path: '/data/encrypted.db',
  encryptionKey: process.env.OXIGRAPH_ENCRYPTION_KEY
});
```

---

## Audit Logging

### Log All Queries

```javascript
// middleware/audit-log.mjs
import { trace } from '@opentelemetry/api';

export function auditLog(req, res, next) {
  const span = trace.getActiveSpan();

  const auditEntry = {
    timestamp: new Date().toISOString(),
    userId: req.user?.userId,
    ip: req.ip,
    method: req.method,
    path: req.path,
    query: req.body.query,
    traceId: span?.spanContext().traceId
  };

  console.log(JSON.stringify(auditEntry));

  // Optionally: Store in database for compliance
  // await storeAuditLog(auditEntry);

  next();
}
```

**Usage:**

```javascript
app.use(auditLog);
```

**Example Log:**

```json
{
  "timestamp": "2024-01-15T10:30:00.000Z",
  "userId": "user-123",
  "ip": "203.0.113.45",
  "method": "POST",
  "path": "/sparql",
  "query": "SELECT * WHERE { ?s ?p ?o } LIMIT 100",
  "traceId": "a3f8b91c4e2d..."
}
```

---

## Security Checklist

### Deployment Security

- [ ] **HTTPS enforced** (redirect HTTP → HTTPS)
- [ ] **API keys rotated regularly** (every 90 days)
- [ ] **Secrets stored in vault** (AWS Secrets Manager, Vault, etc.)
- [ ] **No `.env` files committed to Git**
- [ ] **Rate limiting enabled** (per-IP and per-user)
- [ ] **Query complexity limits enforced**
- [ ] **Audit logging enabled**
- [ ] **RBAC permissions configured**

### Code Security

- [ ] **No hardcoded secrets** (use `process.env`)
- [ ] **Parameterized SPARQL queries** (prevent injection)
- [ ] **Input validation with Zod**
- [ ] **Error messages sanitized** (no stack traces in production)
- [ ] **Dependencies scanned** (`pnpm audit`)

### Runtime Security

- [ ] **Non-root user in Docker** (`USER node`)
- [ ] **Minimal Docker image** (Alpine Linux)
- [ ] **Resource limits set** (CPU, memory)
- [ ] **Health checks configured**
- [ ] **Firewall rules applied**

---

**Verification:**

```bash
# Scan for hardcoded secrets
grep -r "api_key\|apiKey\|password" packages/*/src
# Should return 0 results

# Check dependencies for vulnerabilities
pnpm audit

# Verify HTTPS redirect
curl -I http://your-domain.com/sparql
# Should return: HTTP/1.1 301 Moved Permanently

# Test rate limiting
for i in {1..150}; do curl http://localhost:3000/sparql; done
# Should return 429 Too Many Requests after 100 requests
```

---

**Next:** [Production Deployment Guide](../deployment/production.md)
