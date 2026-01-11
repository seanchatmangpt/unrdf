# Security Penetration Testing Suite

Comprehensive security penetration testing framework for UNRDF v6.0.0.

## Overview

This suite simulates **real-world attack scenarios** to validate security hardening measures. All attacks MUST be blocked for production deployment.

## Test Suites

### 1. Injection Attacks (`01-injection-attacks.test.mjs`)

**Attacks Tested:**
- SQL injection (UNION, boolean-based, time-based, stacked queries)
- SPARQL injection (UNION, FILTER, BIND)
- Command injection (pipe, semicolon, substitution, redirects)
- Path traversal (dot-dot-slash, URL encoding, null bytes)
- Cross-Site Scripting (XSS) - script tags, event handlers, javascript: protocol

**Expected Result:** ALL injection attempts detected and blocked.

### 2. Authentication Attacks (`02-authentication-attacks.test.mjs`)

**Attacks Tested:**
- Password brute force
- API key enumeration
- Credential stuffing
- Timing attacks on password/token comparison
- Session fixation
- Session hijacking
- MFA bypass attempts

**Expected Result:** Rate limiting prevents brute force. Timing attacks prevented by constant-time comparison.

### 3. DoS Attacks (`03-dos-attacks.test.mjs`)

**Attacks Tested:**
- Request flooding (high-volume, rapid-fire, distributed)
- Large payload attacks (huge JSON, nested depth bombs, massive arrays)
- Slowloris / slow HTTP attacks
- Resource exhaustion (memory, CPU, file descriptors)
- Application-layer DoS (complex SPARQL queries, infinite loops)

**Expected Result:** Rate limiting prevents floods. Payload size limits prevent DoS.

### 4. Data Exfiltration (`04-data-exfiltration.test.mjs`)

**Attacks Tested:**
- Secret extraction from errors/logs
- API key exposure
- AWS credential leakage
- Private key exposure
- Error message information leakage
- Debug endpoint exposure
- Information disclosure via HTTP headers
- Timing-based user enumeration
- Side-channel data leakage

**Expected Result:** ZERO secrets leaked. All sensitive data sanitized.

### 5. Cryptographic Attacks (`05-cryptographic-attacks.test.mjs`)

**Attacks Tested:**
- Hash collision attacks (birthday, chosen-prefix, length extension)
- Merkle tree attacks (odd-leaf vulnerability, second preimage, proof forgery)
- Receipt tampering (payload modification, chain reordering, replay, timestamp manipulation)
- Weak randomness attacks (prediction, seed attacks)
- Key derivation attacks (brute force, rainbow tables)
- Signature verification bypass

**Expected Result:** All cryptographic guarantees hold. No tampering possible.

## Running Tests

### Run All Tests

```bash
./run-penetration-tests.mjs
```

### Run Individual Suite

```bash
npx vitest run test/security-testing/01-injection-attacks.test.mjs
```

### Run with Coverage

```bash
npx vitest run test/security-testing --coverage
```

### Run in Watch Mode

```bash
npx vitest watch test/security-testing
```

## Expected Results

### Production-Ready Criteria

✅ **ALL** test suites must PASS
✅ **ZERO** critical vulnerabilities
✅ **ZERO** high-severity vulnerabilities
✅ **100%** attack mitigation rate

### Failure = Deployment Blocked

If ANY test fails:
- ❌ System is NOT production ready
- ❌ Remediation required
- ❌ Re-test after fixes
- ❌ DO NOT DEPLOY

## Attack Coverage

### OWASP Top 10 Coverage

- ✅ A1: Injection (SQL, SPARQL, Command, XSS)
- ✅ A2: Broken Authentication (Brute force, timing attacks)
- ✅ A3: Sensitive Data Exposure (Secret detection, error sanitization)
- ✅ A4: XML External Entities (N/A - not using XML)
- ✅ A5: Broken Access Control (Rate limiting, authentication)
- ✅ A6: Security Misconfiguration (Debug endpoints disabled)
- ✅ A7: Cross-Site Scripting (XSS prevention)
- ✅ A8: Insecure Deserialization (Payload validation)
- ✅ A9: Using Components with Known Vulnerabilities (N/A - dependency scanning separate)
- ✅ A10: Insufficient Logging & Monitoring (Audit trail validation)

### Additional Coverage

- ✅ DoS/DDoS attacks
- ✅ Cryptographic attacks
- ✅ Side-channel attacks
- ✅ Timing attacks
- ✅ Replay attacks
- ✅ Man-in-the-middle (via cryptographic verification)

## Security Hardening Validation

This suite validates the following security measures:

### Input Validation
- SQL/SPARQL injection prevention
- Command injection prevention
- Path traversal prevention
- XSS prevention

### Authentication & Authorization
- Rate limiting (brute force prevention)
- Timing-safe comparison (prevents timing attacks)
- API key validation
- Session management

### Data Protection
- Secret detection & redaction
- Error message sanitization
- Audit log sanitization
- Receipt data sanitization

### Cryptography
- SHA-256 hash collision resistance
- Merkle tree integrity
- Receipt chain integrity
- Secure random generation
- PBKDF2 key derivation

### DoS Protection
- Request rate limiting
- Payload size limits
- Nesting depth limits
- Query complexity limits
- Connection limits

## Report Generation

After running tests, a detailed report is generated:

```
test/security-testing/PENETRATION-TEST-REPORT.md
```

The report includes:
- Test suite results (pass/fail)
- Security posture assessment
- Attack coverage matrix
- Vulnerability details (if any)
- Remediation recommendations

## Integration with CI/CD

### GitHub Actions

```yaml
- name: Security Penetration Tests
  run: ./test/security-testing/run-penetration-tests.mjs
```

### Pre-Deployment Gate

```bash
# Must pass before deployment
npm run security:pentest || exit 1
```

## Continuous Security

### Regular Testing
- Run before each release
- Run after security patches
- Run after dependency updates

### Monitoring
- Monitor audit logs for attack patterns
- Track rate limit violations
- Alert on repeated failed authentication

### Updates
- Add new attack scenarios as discovered
- Update patterns based on CVE database
- Incorporate findings from security audits

## Architecture

```
test/security-testing/
├── 01-injection-attacks.test.mjs       # 150+ injection attack scenarios
├── 02-authentication-attacks.test.mjs  # 50+ auth attack scenarios
├── 03-dos-attacks.test.mjs             # 40+ DoS attack scenarios
├── 04-data-exfiltration.test.mjs       # 60+ exfiltration scenarios
├── 05-cryptographic-attacks.test.mjs   # 50+ crypto attack scenarios
├── run-penetration-tests.mjs           # Test orchestrator
├── PENETRATION-TEST-REPORT.md          # Generated report (gitignored)
└── README.md                           # This file
```

## Threat Model

### Threat Actors
- External attackers (internet-facing)
- Malicious users (authenticated)
- Compromised dependencies (supply chain)

### Assets Protected
- User credentials & API keys
- RDF graph data
- Receipts & audit trail
- System availability
- Cryptographic keys

### Attack Vectors Tested
- Network (injection, DoS)
- Application (logic flaws, auth bypass)
- Cryptographic (tampering, collisions)
- Side-channel (timing, error messages)

## Compliance

This testing suite helps meet:

- **SOC 2 Type II** - Security controls validation
- **ISO 27001** - Information security management
- **PCI DSS** - Payment card data security (if applicable)
- **GDPR** - Data protection by design

## Contact

For security issues found:
- DO NOT open public GitHub issues
- Email: security@unrdf.example.com (update with real contact)
- Use responsible disclosure

---

**Last Updated:** 2026-01-11
**Version:** 1.0.0
**Status:** Production Ready
