# Security Incident Response Runbook

**Version**: 1.0.0
**Last Updated**: 2026-01-11
**Classification**: INTERNAL USE ONLY

---

## Table of Contents

1. [Overview](#overview)
2. [Incident Classification](#incident-classification)
3. [Response Procedures](#response-procedures)
4. [Secrets Rotation](#secrets-rotation)
5. [Breach Response](#breach-response)
6. [Post-Incident](#post-incident)
7. [Contact Information](#contact-information)

---

## Overview

This runbook provides step-by-step procedures for responding to security incidents affecting the UNRDF Daemon. Follow these procedures to ensure consistent, effective incident response.

### When to Use This Runbook

Trigger this runbook when any of the following occur:

- ⚠️ Suspected API key compromise
- ⚠️ Unusual authentication patterns detected
- ⚠️ Rate limiting violations exceeding threshold
- ⚠️ Injection attack attempts detected
- ⚠️ Unauthorized access attempts
- ⚠️ DoS/DDoS attack in progress
- ⚠️ Security vulnerability disclosed
- ⚠️ Data breach suspected or confirmed

### Response Team Roles

| Role | Responsibility | Contact |
|------|----------------|---------|
| Incident Commander | Overall coordination | [on-call rotation] |
| Security Lead | Technical investigation | [security team] |
| Operations Lead | System stability | [ops team] |
| Communications Lead | Stakeholder updates | [comms team] |
| Legal Counsel | Legal compliance | [legal team] |

---

## Incident Classification

### Severity Levels

#### P0 - Critical (Response Time: Immediate)
- Active data breach
- Complete service compromise
- Mass credential theft
- Customer data exposure

**Actions:**
- Page on-call immediately
- Initiate emergency response
- Notify executive team
- Consider service shutdown

#### P1 - High (Response Time: <1 hour)
- API key compromise
- Successful injection attack
- Sustained DoS attack
- Privilege escalation

**Actions:**
- Alert security team
- Begin investigation
- Implement immediate mitigations
- Prepare for escalation

#### P2 - Medium (Response Time: <4 hours)
- Failed attack attempts (high volume)
- Suspicious activity patterns
- Security misconfiguration
- Vulnerability disclosure

**Actions:**
- Document incident
- Investigate patterns
- Review and harden security
- Update monitoring

#### P3 - Low (Response Time: <24 hours)
- Single failed attack attempt
- False positive alerts
- Minor misconfigurations
- Dependency vulnerabilities (non-critical)

**Actions:**
- Log for tracking
- Review during normal hours
- Update documentation
- Schedule fixes

---

## Response Procedures

### 1. Initial Response (First 15 Minutes)

#### Step 1: Assess and Classify

```bash
# Check current security status
node scripts/security-status.mjs

# Review recent logs
tail -n 1000 /var/log/daemon/security.log | grep -E "WARN|ERROR|CRITICAL"

# Check authentication failures
grep "authentication_failed" /var/log/daemon/security.log | tail -n 100

# Check rate limiting violations
grep "rate_limit_exceeded" /var/log/daemon/security.log | tail -n 100
```

**Document:**
- When was incident first detected?
- What triggered the alert?
- What systems are affected?
- Initial severity assessment

#### Step 2: Contain the Incident

For **API Key Compromise**:
```bash
# Immediately revoke compromised key
node scripts/revoke-api-key.mjs --key-id=<KEY_ID>

# Enable emergency rate limiting
export EMERGENCY_RATE_LIMIT=true
export RATE_LIMIT_MAX_REQUESTS=10

# Restart daemon with new limits
pm2 restart daemon
```

For **DoS Attack**:
```bash
# Enable strict rate limiting
export RATE_LIMIT_WINDOW_MS=60000
export RATE_LIMIT_MAX_REQUESTS=10

# Block attacking IPs (if identifiable)
node scripts/block-ips.mjs --ips=<IP_LIST>

# Restart daemon
pm2 restart daemon
```

For **Injection Attack**:
```bash
# Enable strict input sanitization
export STRICT_SANITIZATION=true

# Review and sanitize recent requests
node scripts/review-recent-requests.mjs --hours=24

# Restart daemon
pm2 restart daemon
```

#### Step 3: Notify Stakeholders

```bash
# Send initial incident notification
node scripts/send-incident-alert.mjs \
  --severity=P1 \
  --summary="API key compromise detected" \
  --affected-systems="daemon" \
  --status="investigating"
```

**Initial Notification Template:**
```
Subject: [P1] Security Incident - [Brief Description]

Summary: [1-2 sentence description]

Status: Investigating

Affected Systems:
- System 1
- System 2

Timeline:
- [Time] Incident detected
- [Time] Response initiated
- [Time] Initial containment

Next Update: [Time + 1 hour]

Incident Commander: [Name]
```

### 2. Investigation (First Hour)

#### Gather Evidence

```bash
# Export logs for analysis
node scripts/export-security-logs.mjs \
  --start="2026-01-11T00:00:00Z" \
  --end="2026-01-11T23:59:59Z" \
  --output="/tmp/incident-logs.json"

# Analyze authentication patterns
node scripts/analyze-auth-patterns.mjs \
  --logs="/tmp/incident-logs.json" \
  --output="/tmp/auth-analysis.json"

# Check for data access
node scripts/check-data-access.mjs \
  --logs="/tmp/incident-logs.json" \
  --output="/tmp/data-access.json"

# Identify affected accounts
node scripts/identify-affected-accounts.mjs \
  --logs="/tmp/incident-logs.json" \
  --output="/tmp/affected-accounts.json"
```

#### Timeline Reconstruction

Create detailed timeline:
```markdown
## Incident Timeline

### Before Incident
- [Time - 24h] Last known good state
- [Time - 1h] Suspicious activity begins

### Incident
- [Time] First attack attempt
- [Time + 5m] Multiple failed authentications
- [Time + 10m] Successful authentication (compromised key)
- [Time + 15m] Data access detected
- [Time + 20m] Alert triggered

### Response
- [Time + 21m] On-call paged
- [Time + 25m] Investigation began
- [Time + 30m] Key revoked
- [Time + 35m] Service secured
```

#### Root Cause Analysis

Use 5 Whys:
```markdown
## Root Cause Analysis

1. Why did the incident occur?
   → API key was compromised

2. Why was the API key compromised?
   → Key was exposed in client-side code

3. Why was the key in client-side code?
   → Developer error during implementation

4. Why wasn't this caught in review?
   → No automated secret scanning in CI/CD

5. Why no automated scanning?
   → Secret scanning not configured

**Root Cause:** Lack of automated secret scanning in CI/CD pipeline
```

### 3. Mitigation (Within 4 Hours)

#### Immediate Actions

**For API Key Compromise:**
```bash
# 1. Rotate all API keys
node scripts/rotate-all-keys.mjs

# 2. Notify all API key holders
node scripts/notify-key-holders.mjs \
  --message="Security incident requires key rotation. New keys sent via secure channel."

# 3. Update environment variables
cat > .env.new <<EOF
API_KEY_HASH=<NEW_HASH>
API_KEY_ROTATION_DATE=$(date -u +%Y-%m-%dT%H:%M:%SZ)
EOF

# 4. Deploy updated configuration
./scripts/deploy-config.sh .env.new

# 5. Verify new keys working
node scripts/verify-new-keys.mjs
```

**For Injection Attack:**
```bash
# 1. Review and patch vulnerable endpoint
vim src/vulnerable-endpoint.mjs

# 2. Add additional validation
node scripts/add-validation.mjs --endpoint=/api/vulnerable

# 3. Deploy fix
pnpm build && pm2 restart daemon

# 4. Verify fix
node scripts/test-injection-defense.mjs
```

**For DoS Attack:**
```bash
# 1. Implement aggressive rate limiting
export RATE_LIMIT_MAX_REQUESTS=5
export RATE_LIMIT_WINDOW_MS=60000

# 2. Enable IP blocking
node scripts/enable-ip-blocking.mjs

# 3. Configure auto-blocking
cat > config/auto-block.json <<EOF
{
  "enabled": true,
  "failureThreshold": 10,
  "blockDuration": 3600000
}
EOF

# 4. Restart with new config
pm2 restart daemon
```

#### Long-term Actions

```markdown
## Long-term Mitigation Plan

### Week 1
- [ ] Implement secret scanning in CI/CD
- [ ] Add automated security testing
- [ ] Update security documentation
- [ ] Conduct team training

### Week 2
- [ ] Deploy enhanced monitoring
- [ ] Implement anomaly detection
- [ ] Review all access controls
- [ ] Audit all API keys

### Month 1
- [ ] External security audit
- [ ] Penetration testing
- [ ] Update incident response plan
- [ ] Disaster recovery drill
```

---

## Secrets Rotation

### API Key Rotation

#### Standard Rotation (Scheduled)

```bash
# 1. Generate new API key pair
node scripts/generate-api-key.mjs

# Output:
# New API Key: 1234567890abcdef...
# Key Hash: abcdef1234567890...
# Rotation ID: rotation_2026_01_11_001

# 2. Update environment variable
export NEW_API_KEY_HASH="abcdef1234567890..."

# 3. Notify stakeholders of pending rotation
node scripts/notify-rotation.mjs \
  --rotation-id=rotation_2026_01_11_001 \
  --effective-date="2026-01-18T00:00:00Z" \
  --grace-period="7 days"

# 4. Deploy new configuration (parallel acceptance)
./scripts/deploy-with-dual-keys.sh

# 5. Wait for grace period
sleep 604800  # 7 days

# 6. Remove old key
./scripts/remove-old-key.sh

# 7. Verify rotation complete
node scripts/verify-rotation.mjs --rotation-id=rotation_2026_01_11_001
```

#### Emergency Rotation (Compromise)

```bash
# 1. Immediately revoke compromised key
node scripts/emergency-revoke.mjs --key-id=<COMPROMISED_KEY_ID>

# 2. Generate new key
node scripts/generate-api-key.mjs --emergency

# 3. Deploy new key immediately (NO grace period)
./scripts/emergency-deploy.sh --key-hash=<NEW_HASH>

# 4. Notify all stakeholders
node scripts/emergency-notification.mjs \
  --reason="Key compromise" \
  --action="Immediate rotation" \
  --new-key-delivery="secure channel"

# 5. Deliver new keys via secure channel
# - Use encrypted email
# - Or secure key management system
# - Or phone verification + secure portal

# 6. Monitor for authentication failures
watch -n 5 'grep "authentication_failed" /var/log/daemon/security.log | tail -n 20'

# 7. Provide support for migration
# - Open dedicated support channel
# - Provide migration documentation
# - Assist with integration testing
```

#### Rotation Schedule

| Key Type | Rotation Frequency | Grace Period | Method |
|----------|-------------------|--------------|--------|
| Production API Keys | 90 days | 7 days | Standard |
| Development API Keys | 30 days | 3 days | Standard |
| Service Account Keys | 180 days | 14 days | Standard |
| Emergency Rotation | Immediate | 0 days | Emergency |

### Other Secrets Rotation

#### Database Credentials

```bash
# 1. Create new database user
psql -c "CREATE USER daemon_new WITH PASSWORD '<NEW_PASSWORD>';"

# 2. Grant permissions
psql -c "GRANT ALL PRIVILEGES ON DATABASE daemon TO daemon_new;"

# 3. Update application config
export DB_USER="daemon_new"
export DB_PASSWORD="<NEW_PASSWORD>"

# 4. Restart application
pm2 restart daemon

# 5. Verify connectivity
node scripts/verify-db-connection.mjs

# 6. Remove old user (after verification)
psql -c "DROP USER daemon_old;"
```

#### TLS Certificates

```bash
# 1. Generate new certificate
openssl req -x509 -newkey rsa:4096 -keyout new-key.pem -out new-cert.pem -days 365

# 2. Update configuration
cp new-key.pem /etc/ssl/private/daemon-key.pem
cp new-cert.pem /etc/ssl/certs/daemon-cert.pem

# 3. Restart services
systemctl restart nginx
pm2 restart daemon

# 4. Verify certificate
openssl s_client -connect localhost:443 -servername daemon.example.com
```

---

## Breach Response

### Data Breach Procedures

#### Step 1: Confirm Breach (Within 1 Hour)

```bash
# 1. Identify scope of data access
node scripts/audit-data-access.mjs \
  --start-time="<INCIDENT_START>" \
  --end-time="<INCIDENT_END>" \
  --output="/tmp/breach-audit.json"

# 2. Identify affected records
node scripts/identify-affected-data.mjs \
  --audit-file="/tmp/breach-audit.json" \
  --output="/tmp/affected-records.json"

# 3. Classify data sensitivity
node scripts/classify-data-sensitivity.mjs \
  --records="/tmp/affected-records.json" \
  --output="/tmp/data-classification.json"
```

#### Step 2: Legal Notification (Within 4 Hours)

```bash
# Notify legal team
node scripts/notify-legal.mjs \
  --incident-id=<INCIDENT_ID> \
  --breach-type="unauthorized access" \
  --affected-records="/tmp/affected-records.json" \
  --data-classification="/tmp/data-classification.json"
```

#### Step 3: Regulatory Compliance (Within 72 Hours)

**GDPR Breach Notification:**
```markdown
## Breach Notification to Supervisory Authority

**Within 72 hours of detection**

Required Information:
1. Nature of personal data breach
2. Categories and number of data subjects affected
3. Categories and number of personal data records affected
4. Likely consequences of the breach
5. Measures taken or proposed to address the breach
6. Contact point for more information

Submission:
- Via supervisory authority portal
- Include incident report
- Include mitigation steps
- Include timeline
```

**Affected Individual Notification:**
```markdown
## Notification to Affected Individuals

**When required:**
- High risk to rights and freedoms of individuals
- Breach likely to result in identity theft, fraud, etc.

**Content:**
- Description of the breach
- Contact point for more information
- Likely consequences
- Measures taken to mitigate
- Recommendations for individuals

**Method:**
- Direct communication (email, mail)
- Clear and plain language
- Without undue delay
```

#### Step 4: Customer Communication

**Communication Template:**
```markdown
Subject: Important Security Notice - Data Breach Notification

Dear [Customer Name],

We are writing to inform you of a security incident that may have affected your data.

What Happened:
[Brief description of the incident]

What Information Was Involved:
[List of data types potentially accessed]

What We Are Doing:
- [Immediate actions taken]
- [Long-term improvements]
- [Support provided]

What You Can Do:
- [Recommended actions for customers]
- [Where to get help]

We take the security of your data very seriously and sincerely apologize for any concern this may cause.

For more information: [Contact details]

Sincerely,
[Security Team]
```

---

## Post-Incident

### Post-Incident Review (Within 1 Week)

#### Conduct Post-Mortem

**Attendees:**
- Incident Commander
- Security Lead
- Operations Lead
- Engineering team
- Product team

**Agenda:**
```markdown
## Post-Incident Review - [Incident ID]

### 1. Incident Summary
- What happened?
- When did it happen?
- Who was affected?
- What was the impact?

### 2. Timeline Review
- Detection time
- Response time
- Resolution time
- Total duration

### 3. What Went Well
- [List successes]

### 4. What Went Wrong
- [List failures]

### 5. Root Cause
- [Root cause analysis]

### 6. Action Items
- [Preventive measures]
- [Detective improvements]
- [Response improvements]

### 7. Lessons Learned
- [Key takeaways]
```

#### Document Findings

```bash
# Create incident report
node scripts/generate-incident-report.mjs \
  --incident-id=<INCIDENT_ID> \
  --output="/docs/incidents/incident-<ID>-report.md"

# Update runbook with lessons learned
vim SECURITY_INCIDENT_RESPONSE.md

# Update security documentation
vim SECURITY_HARDENING.md
```

#### Track Action Items

```markdown
## Action Items - [Incident ID]

### Immediate (Week 1)
- [ ] Deploy security patch
- [ ] Update monitoring alerts
- [ ] Rotate all credentials
- [ ] Update documentation

### Short-term (Month 1)
- [ ] Implement additional controls
- [ ] Conduct security training
- [ ] Update incident response plan
- [ ] External security review

### Long-term (Quarter 1)
- [ ] Architecture improvements
- [ ] Automated security testing
- [ ] Enhanced monitoring
- [ ] Regular security audits
```

### Continuous Improvement

#### Quarterly Security Reviews

```bash
# Review incident trends
node scripts/analyze-incidents.mjs --quarter=Q1-2026

# Update threat model
node scripts/update-threat-model.mjs

# Review and update controls
node scripts/review-security-controls.mjs

# Plan improvements
node scripts/plan-security-improvements.mjs
```

#### Annual Security Audit

```markdown
## Annual Security Audit Checklist

### Access Controls
- [ ] Review all API keys
- [ ] Audit user permissions
- [ ] Review service accounts
- [ ] Check for unused accounts

### Code Security
- [ ] Static analysis scan
- [ ] Dependency vulnerability scan
- [ ] Secret scanning
- [ ] Code review for security

### Infrastructure
- [ ] Network security review
- [ ] Firewall rules audit
- [ ] TLS configuration review
- [ ] Backup and recovery test

### Monitoring
- [ ] Alert effectiveness review
- [ ] Log coverage audit
- [ ] Monitoring gaps analysis
- [ ] Incident response drill

### Documentation
- [ ] Update security policies
- [ ] Review incident runbooks
- [ ] Update architecture diagrams
- [ ] Training materials current
```

---

## Contact Information

### Emergency Contacts

| Role | Contact Method | Availability |
|------|---------------|--------------|
| On-Call Security Engineer | PagerDuty: security-oncall | 24/7 |
| Security Team Lead | email: security-lead@example.com | Business hours |
| Incident Commander | PagerDuty: incident-commander | 24/7 |
| CTO | Phone: [redacted] | Escalations only |
| Legal Counsel | email: legal@example.com | Business hours |

### External Contacts

| Organization | Contact | Purpose |
|-------------|---------|---------|
| Law Enforcement | [Local cybercrime unit] | Criminal activity |
| CERT/CSIRT | cert@example.org | Incident coordination |
| Cloud Provider | support@provider.com | Infrastructure issues |
| Security Vendor | support@vendor.com | Tool support |

### Communication Channels

- **Incident Channel**: #incident-response (Slack)
- **Security Team**: #security (Slack)
- **Status Updates**: status.example.com
- **Customer Support**: support@example.com

---

## Appendices

### A. Incident Report Template

```markdown
# Incident Report - [ID]

## Executive Summary
[1-2 paragraph summary for executives]

## Incident Details
- **Incident ID**: [ID]
- **Severity**: [P0/P1/P2/P3]
- **Category**: [Type of incident]
- **Detection Time**: [Timestamp]
- **Resolution Time**: [Timestamp]
- **Duration**: [Hours/Minutes]

## Impact
- **Systems Affected**: [List]
- **Data Affected**: [Description]
- **Users Affected**: [Count]
- **Financial Impact**: [Estimate]

## Timeline
[Detailed timeline of events]

## Root Cause
[Root cause analysis]

## Response Actions
[Actions taken during incident]

## Lessons Learned
[What we learned]

## Action Items
[Preventive measures]

## Appendices
- Logs
- Evidence
- Communications
```

### B. Security Checklist

```markdown
## Pre-Incident Preparation
- [ ] Incident response plan documented
- [ ] Contact information current
- [ ] Monitoring and alerting configured
- [ ] Backup and recovery tested
- [ ] Team trained on procedures

## During Incident
- [ ] Incident classified
- [ ] Stakeholders notified
- [ ] Evidence collected
- [ ] Containment actions taken
- [ ] Timeline documented

## Post-Incident
- [ ] Post-mortem conducted
- [ ] Report generated
- [ ] Action items tracked
- [ ] Lessons learned documented
- [ ] Runbook updated
```

### C. Useful Commands

```bash
# Check authentication logs
grep "authentication" /var/log/daemon/security.log | tail -n 100

# Check rate limiting
grep "rate_limit" /var/log/daemon/security.log | tail -n 100

# Find suspicious IPs
awk '{print $1}' /var/log/daemon/access.log | sort | uniq -c | sort -nr | head -20

# Check for injection attempts
grep -E "(SELECT|INSERT|UPDATE|DELETE|DROP|script|iframe)" /var/log/daemon/security.log

# Monitor in real-time
tail -f /var/log/daemon/security.log | grep --color -E "WARN|ERROR|CRITICAL"
```

---

## Version History

| Version | Date | Changes | Author |
|---------|------|---------|--------|
| 1.0.0 | 2026-01-11 | Initial version | Security Team |

---

**END OF RUNBOOK**

For questions or updates to this runbook, contact the Security Team.
