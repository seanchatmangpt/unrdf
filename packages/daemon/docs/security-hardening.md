# Security Hardening Guide: @unrdf/daemon

Complete security hardening procedures for @unrdf/daemon in production. Covers API authentication/authorization, audit logging, network segmentation, secrets management, and CVE monitoring.

**Audience**: Security engineers, platform engineers
**Time to read**: 20-30 minutes
**Updated**: January 2026

---

## Table of Contents

1. [Authentication & Authorization](#authentication--authorization)
2. [Audit Logging](#audit-logging)
3. [Network Segmentation](#network-segmentation)
4. [Secrets Management](#secrets-management)
5. [CVE Monitoring & Patching](#cve-monitoring--patching)
6. [Security Checklist](#security-checklist)

---

## Authentication & Authorization

### API Authentication

All daemon API endpoints require authentication tokens. Supported methods:

#### 1. Bearer Token (Recommended for services)

```bash
# Generate token
TOKEN=$(openssl rand -hex 32)

# Store securely (Kubernetes secret)
kubectl create secret generic daemon-api-token \
  --from-literal=token=$TOKEN \
  -n unrdf-daemons

# Use token in requests
curl -H "Authorization: Bearer $TOKEN" \
  http://daemon:8080/health
```

#### 2. mTLS (Recommended for cluster communication)

```bash
# Generate client certificate
openssl req -new -x509 -days 365 -keyout client-key.pem -out client-cert.pem \
  -subj "/CN=daemon-client"

# Configure daemon to require mTLS
export DAEMON_MTLS_ENABLED=true
export DAEMON_MTLS_CA=/etc/daemon/tls/ca-cert.pem
export DAEMON_MTLS_REQUIRE_CLIENT_CERT=true

# Client connects with certificate
curl --cert client-cert.pem --key client-key.pem \
  https://daemon:8443/health
```

#### 3. API Key (Legacy, for backwards compatibility)

```javascript
// Configure daemon
const daemon = new Daemon({
  apiKeys: [
    {
      key: 'sk-prod-abc123...',
      name: 'Integration Service',
      permissions: ['read:operations', 'execute:operations'],
      rateLimit: 1000,  // requests per minute
      expiresAt: new Date('2025-12-31')
    }
  ]
});

// Use in requests
curl -H "X-API-Key: sk-prod-abc123..." \
  http://daemon:8080/operations
```

### Authorization (RBAC)

Define roles and permissions:

```javascript
const rbac = {
  roles: {
    reader: {
      permissions: [
        'read:health',
        'read:metrics',
        'read:operations',
        'list:operations'
      ]
    },
    operator: {
      permissions: [
        ...reader.permissions,
        'execute:operations',
        'retry:operations'
      ]
    },
    admin: {
      permissions: [
        'read:*',
        'write:*',
        'execute:*',
        'admin:*'
      ]
    }
  },

  bindings: [
    {
      role: 'reader',
      subject: 'monitoring-system'
    },
    {
      role: 'operator',
      subject: 'integration-service'
    },
    {
      role: 'admin',
      subject: 'platform-engineers'
    }
  ]
};
```

### Kubernetes RBAC Integration

```yaml
# daemon-rbac.yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: daemon-operator
  namespace: unrdf-daemons
rules:
- apiGroups: [""]
  resources: ["configmaps"]
  resourceNames: ["daemon-config"]
  verbs: ["get"]
- apiGroups: [""]
  resources: ["secrets"]
  resourceNames: ["daemon-secrets"]
  verbs: ["get"]

---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: daemon-operator-binding
  namespace: unrdf-daemons
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: Role
  name: daemon-operator
subjects:
- kind: ServiceAccount
  name: integration-service-sa
  namespace: integration

---
# Restrict daemon itself to read-only on config
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: daemon-readonly
rules:
- apiGroups: [""]
  resources: ["configmaps", "secrets"]
  verbs: ["get", "list"]
- apiGroups: [""]
  resources: ["pods"]
  verbs: ["get", "list"]
```

---

## Audit Logging

### Operation Audit Trail

Every operation execution must be logged:

```javascript
// Daemon emits audit events
daemon.on('operation:audit', (event) => {
  const auditLog = {
    timestamp: event.timestamp,
    operationId: event.operationId,
    operationName: event.operationName,
    userId: event.userId,  // From auth token
    action: event.action,  // 'execute', 'schedule', 'retry', etc.
    result: event.result,  // 'success', 'failure', 'timeout'
    duration: event.duration,
    nodeId: event.nodeId,
    clusterId: event.clusterId,

    // Sensitive data hashed
    inputHash: hash(event.input),
    outputHash: hash(event.output),

    // For compliance
    sourceIp: event.sourceIp,
    userAgent: event.userAgent
  };

  // Send to audit system (Splunk, ELK, CloudWatch, etc.)
  auditSystem.log(auditLog);
});
```

### Audit Log Centralization

**Ship logs to centralized system**:

```javascript
// Send to Splunk
import SplunkLogger from 'splunk-logging';

const splunk = new SplunkLogger({
  token: process.env.SPLUNK_HEC_TOKEN,
  host: 'splunk.example.com',
  port: 8088
});

daemon.on('operation:audit', (event) => {
  splunk.logEvent({
    event: JSON.stringify(event),
    source: 'unrdf-daemon',
    sourcetype: 'operation_audit',
    index: 'security'
  });
});
```

**Or use OTEL collector**:

```javascript
// Send via OpenTelemetry
import { logs } from '@opentelemetry/api-logs';

const logger = logs.getLogger('@unrdf/daemon');

daemon.on('operation:audit', (event) => {
  logger.emit({
    severityNumber: SeverityNumber.INFO,
    severityText: 'INFO',
    body: JSON.stringify(event),
    attributes: {
      'operation.id': event.operationId,
      'operation.result': event.result,
      'user.id': event.userId
    }
  });
});
```

### Audit Log Retention Policy

```yaml
# Kubernetes ConfigMap
apiVersion: v1
kind: ConfigMap
metadata:
  name: audit-policy
  namespace: unrdf-daemons
data:
  retention.json: |
    {
      "hot": {
        "duration": "30 days",
        "storage": "SSD",
        "searchable": true
      },
      "warm": {
        "duration": "90 days",
        "storage": "HDD",
        "searchable": true
      },
      "cold": {
        "duration": "1 year",
        "storage": "S3 Glacier",
        "searchable": false
      },
      "deleted": {
        "duration": "after 1 year",
        "action": "purge"
      }
    }
```

---

## Network Segmentation

### Zero-Trust Network Architecture

**Principle**: No implicit trust, verify all connections

```yaml
# 01-egress-policy.yaml - Default deny all egress
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: daemon-default-deny
  namespace: unrdf-daemons
spec:
  podSelector: {}
  policyTypes:
  - Ingress
  - Egress
  ingress: []  # All ingress denied by default
  egress: []   # All egress denied by default

---
# 02-allow-dns.yaml - Allow DNS only
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: allow-dns
  namespace: unrdf-daemons
spec:
  podSelector: {}
  policyTypes:
  - Egress
  egress:
  - to:
    - namespaceSelector:
        matchLabels:
          name: kube-system
    ports:
    - protocol: UDP
      port: 53

---
# 03-daemon-ingress.yaml - Allow daemon cluster communication only
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: daemon-cluster-only
  namespace: unrdf-daemons
spec:
  podSelector:
    matchLabels:
      app: unrdf-daemon
  policyTypes:
  - Ingress
  - Egress

  ingress:
  # From other daemon pods
  - from:
    - podSelector:
        matchLabels:
          app: unrdf-daemon
    ports:
    - protocol: TCP
      port: 8080
    - protocol: TCP
      port: 8081  # Raft

  # From Prometheus
  - from:
    - namespaceSelector:
        matchLabels:
          name: monitoring
    ports:
    - protocol: TCP
      port: 8080

  egress:
  # DNS
  - to:
    - namespaceSelector:
        matchLabels:
          name: kube-system
    ports:
    - protocol: UDP
      port: 53

  # To database/backend services
  - to:
    - namespaceSelector:
        matchLabels:
          name: data-layer
    ports:
    - protocol: TCP
      port: 5432  # PostgreSQL

  # To other daemon pods
  - to:
    - podSelector:
        matchLabels:
          app: unrdf-daemon
    ports:
    - protocol: TCP
      port: 8080
    - protocol: TCP
      port: 8081

---
# 04-verify-policies.yaml - Verification
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: allow-tests
  namespace: unrdf-daemons
spec:
  podSelector:
    matchLabels:
      role: test
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: unrdf-daemon
```

### Verify Network Policies

```bash
# List all policies
kubectl get networkpolicies -n unrdf-daemons

# Test connectivity
kubectl run -it --rm debug --image=busybox --restart=Never \
  -n unrdf-daemons -- \
  wget -O- http://unrdf-daemon:8080/health
# Expected: Success

# Test blocked connection
kubectl run -it --rm debug --image=busybox --restart=Never \
  --namespace=default -- \
  wget -O- http://unrdf-daemon.unrdf-daemons:8080/health
# Expected: Connection timeout (blocked)
```

---

## Secrets Management

### Secret Storage

**Never** hardcode secrets. Use secure storage:

```javascript
// WRONG - Never do this
const config = {
  apiKey: 'sk-prod-abc123...'  // ❌ EXPOSED
};

// RIGHT - Use environment variables
const config = {
  apiKey: process.env.DAEMON_API_KEY  // ✓ From secure store
};

// RIGHT - Use Kubernetes secrets
import { readFileSync } from 'fs';
const config = {
  apiKey: readFileSync('/etc/daemon/secrets/api-key').toString()
};
```

### Kubernetes Secret Management

```bash
# Create secret from file
kubectl create secret generic daemon-secrets \
  --from-file=api-key=./api-key.txt \
  --from-file=tls-cert=./daemon-cert.pem \
  --from-file=tls-key=./daemon-key.pem \
  -n unrdf-daemons

# Verify secret created
kubectl get secret daemon-secrets -n unrdf-daemons -o yaml | head -20

# Rotate secret safely
kubectl create secret generic daemon-secrets-v2 \
  --from-file=api-key=./new-api-key.txt \
  -n unrdf-daemons

# Update daemon to use v2
kubectl patch statefulset unrdf-daemon \
  -n unrdf-daemons \
  -p '{"spec":{"template":{"spec":{"volumes":[{"name":"secrets","secret":{"secretName":"daemon-secrets-v2"}}]}}}}'

# Delete old secret after rollout complete
kubectl delete secret daemon-secrets -n unrdf-daemons
```

### Secret Encryption at Rest

**Enable encryption for secrets in etcd**:

```yaml
# encryption-config.yaml
apiVersion: apiserver.config.k8s.io/v1
kind: EncryptionConfiguration
resources:
  - resources:
    - secrets
    providers:
    - aescbc:
        keys:
        - name: key1
          secret: $(ENCRYPTION_KEY_B64)  # 32 bytes, base64-encoded
    - identity: {}
```

### Secrets Rotation

**Rotate secrets every 90 days**:

```bash
#!/bin/bash
# rotate-secrets.sh

# 1. Generate new secret
NEW_API_KEY=$(openssl rand -hex 32)

# 2. Create new secret version
kubectl create secret generic daemon-api-key-v2 \
  --from-literal=key=$NEW_API_KEY \
  -n unrdf-daemons

# 3. Update daemon config to use new key
kubectl patch configmap daemon-config \
  -n unrdf-daemons \
  -p '{"data":{"api-key-version":"v2"}}'

# 4. Rolling update daemon pods
kubectl rollout restart statefulset/unrdf-daemon -n unrdf-daemons

# 5. Wait for pods to restart
sleep 60

# 6. Delete old secret
kubectl delete secret daemon-api-key-v1 -n unrdf-daemons

echo "Secret rotation complete"
```

---

## CVE Monitoring & Patching

### Vulnerability Scanning

**Scan container image for vulnerabilities**:

```bash
# Using Trivy (recommended)
trivy image unrdf-daemon:1.0.0

# Using Grype
grype unrdf-daemon:1.0.0

# Using Snyk
snyk container test unrdf-daemon:1.0.0

# Expected output format:
# ✓ 0 critical vulnerabilities
# ✓ 0 high vulnerabilities
# ✓ < 5 medium vulnerabilities (acceptable)
```

### Dependency Updates

**Keep dependencies current**:

```bash
# Check outdated packages
npm outdated

# Audit for security issues
npm audit

# Fix automatically (with caution)
npm audit fix

# Major version upgrades (test in staging first)
npm update --major
```

### CVE Response Procedure

**When CVE is published**:

```bash
#!/bin/bash
# cve-response.sh

CVE_ID=$1  # e.g., CVE-2024-1234
SEVERITY=$2  # critical, high, medium

echo "=== CVE Response for $CVE_ID ($SEVERITY) ==="

# 1. Identify affected packages
npm ls vulnerable-package

# 2. Check if we're affected
grep "vulnerable-package" package.json

if [ $? -eq 0 ]; then
  echo "✗ We are affected"

  if [ "$SEVERITY" = "critical" ]; then
    # 4. Critical: immediate patch
    npm update vulnerable-package
    npm audit fix

    # 5. Run tests
    npm test

    if [ $? -eq 0 ]; then
      # 6. Rebuild and deploy
      docker build -t unrdf-daemon:emergency-patch .
      kubectl set image statefulset/unrdf-daemon \
        daemon=unrdf-daemon:emergency-patch \
        -n unrdf-daemons

      echo "✓ Emergency patch deployed"
    else
      echo "✗ Tests failed, manual intervention required"
      exit 1
    fi
  else
    # For high/medium, schedule update in next release
    echo "! Schedule for next release cycle"
  fi
else
  echo "✓ We are not affected"
fi
```

### Automated CVE Monitoring

**Setup GitHub Dependabot or similar**:

```yaml
# .github/dependabot.yml
version: 2
updates:
  # Check for npm security updates daily
  - package-ecosystem: "npm"
    directory: "packages/daemon"
    schedule:
      interval: "daily"
      time: "02:00"
    security-updates-only: true
    reviewers:
      - "security-team"
    labels:
      - "security"
    auto-approve: false  # Manual review required

  # Check for GitHub Actions updates
  - package-ecosystem: "github-actions"
    directory: "/"
    schedule:
      interval: "weekly"
```

### Security Patching SLA

| Severity | Response Time | Patch Deployment |
|----------|---------------|------------------|
| Critical | 1 hour | Same day |
| High | 24 hours | Within 3 days |
| Medium | 1 week | Next release |
| Low | 2 weeks | Next release |

---

## Security Checklist

### Pre-Deployment

- [ ] All secrets stored in Kubernetes secrets (not ConfigMap)
- [ ] TLS enabled for all communication
- [ ] mTLS configured for cluster nodes
- [ ] API authentication enabled (Bearer tokens or API keys)
- [ ] RBAC policies defined and tested
- [ ] Network policies enforced (default deny)
- [ ] Container image scanned for CVEs (0 critical, 0 high)
- [ ] Dependencies audited (`npm audit fix` run)
- [ ] Security labels added to manifests
- [ ] Audit logging configured and tested

### Runtime

- [ ] Pods run as non-root user (uid != 0)
- [ ] No privileged containers
- [ ] Filesystem is read-only where possible
- [ ] Resource limits enforced (CPU, memory)
- [ ] Security context configured
  - [ ] `allowPrivilegeEscalation: false`
  - [ ] `readOnlyRootFilesystem: true`
  - [ ] `capabilities.drop: ALL`
- [ ] No hardcoded secrets in logs
- [ ] Sensitive data masked in error messages
- [ ] Environment variables not printed in startup logs

### Ongoing

- [ ] Weekly vulnerability scans running
- [ ] CVE monitoring alerts configured
- [ ] Dependency updates reviewed monthly
- [ ] Security patches applied within SLA
- [ ] Audit logs stored in immutable storage
- [ ] Audit logs encrypted at rest
- [ ] Secrets rotated quarterly
- [ ] Access logs reviewed monthly
- [ ] Security incidents documented and reviewed
- [ ] Penetration testing scheduled annually

### Compliance (if applicable)

- [ ] SOC 2 Type II compliance
  - [ ] Audit trail for all operations
  - [ ] Encryption in transit and at rest
  - [ ] Access controls documented
  - [ ] Incident response procedures documented
- [ ] GDPR compliance (if processing EU data)
  - [ ] Data deletion capability (DPIA)
  - [ ] Audit trail retention < 1 year
  - [ ] Third-party processor agreements (DPA)
- [ ] HIPAA compliance (if healthcare data)
  - [ ] Encryption for all data
  - [ ] Access logs immutable
  - [ ] Incident notification procedures

---

## Security Reference

### Useful Commands

```bash
# Verify TLS is working
echo "Q" | openssl s_client -connect localhost:8443

# Check certificate validity
openssl x509 -in daemon-cert.pem -text -noout

# Verify secret exists
kubectl get secrets -n unrdf-daemons

# Check audit logs
kubectl logs -n unrdf-daemons unrdf-daemon-0 | grep AUDIT

# Verify network policies
kubectl get networkpolicies -n unrdf-daemons

# Test RBAC
kubectl auth can-i execute operations --as=daemon-sa -n unrdf-daemons
```

### Security Resources

- **OWASP Top 10**: https://owasp.org/www-project-top-ten/
- **Kubernetes Security**: https://kubernetes.io/docs/concepts/security/
- **CVE Database**: https://cve.mitre.org
- **NIST Cybersecurity Framework**: https://www.nist.gov/cyberframework

### Related Guides

- **[Production Deployment](./production-deployment.md)** - Infrastructure security
- **[Operational Runbooks](./operational-runbooks.md)** - Incident response
- **[API Reference](./reference.md)** - Configuration security options

---

## Questions or Issues?

- Security concerns: security@unrdf.org
- Vulnerability disclosure: https://unrdf.org/security
- GitHub Security Advisories: https://github.com/unrdf/unrdf/security
