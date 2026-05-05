# Security Policy

## Supported Versions

We release patches for security vulnerabilities in the following versions:

| Version | Supported          |
| ------- | ------------------ |
| 1.x.x   | :white_check_mark: |
| < 1.0   | :x:                |

## Reporting a Vulnerability

We take the security of UNRDF seriously. If you believe you have found a security vulnerability, please report it to us as described below.

### Where to Report

**Please do NOT report security vulnerabilities through public GitHub issues.**

Instead, please report them via email to: **security@unrdf.dev** (or primary maintainer email)

### What to Include

Please include the following information in your report:

1. **Description**: Clear description of the vulnerability
2. **Impact**: What an attacker could do with this vulnerability
3. **Steps to Reproduce**: Detailed steps to reproduce the issue
4. **Affected Versions**: Which versions are affected
5. **Suggested Fix**: If you have a suggested fix (optional)
6. **Your Contact Info**: For follow-up questions

### Response Timeline

- **Initial Response**: Within 48 hours
- **Status Update**: Within 7 days
- **Fix Timeline**: Depends on severity
  - Critical: 24-48 hours
  - High: 7 days
  - Medium: 30 days
  - Low: 90 days

### Disclosure Policy

- We will confirm receipt of your vulnerability report within 48 hours
- We will provide regular updates on our progress
- We will notify you when the vulnerability is fixed
- We will publicly disclose the vulnerability after a fix is released
- We will credit you in the security advisory (unless you prefer to remain anonymous)

## Security Best Practices

### For Users

1. **Keep Dependencies Updated**: Run `pnpm update` regularly
2. **Use Environment Variables**: Never hardcode secrets in your code
3. **Validate Inputs**: Always validate and sanitize user inputs
4. **Enable Security Headers**: Use the provided `getSecurityHeaders()` utility
5. **Rate Limiting**: Implement rate limiting for public APIs
6. **HTTPS Only**: Always use HTTPS in production

### For Contributors

1. **Security Review**: All PRs undergo automated security scanning
2. **Dependency Audit**: Run `pnpm audit` before submitting PRs
3. **No Secrets in Code**: Never commit API keys, passwords, or tokens
4. **Input Validation**: Use Zod schemas for all user inputs
5. **OWASP Top 10**: Follow OWASP Top 10 guidelines

## Known Vulnerabilities

We maintain a public list of known vulnerabilities and their status:

### Current Vulnerabilities

None currently known.

### Fixed Vulnerabilities

None yet (project is new).

## Security Features

UNRDF includes the following security features:

### 1. Automated Security Scanning

- **Dependency Scanning**: Daily scans with pnpm audit
- **SAST**: CodeQL static analysis on every PR
- **Secret Scanning**: TruffleHog and Gitleaks
- **License Compliance**: Automated license checking
- **Container Scanning**: Trivy and Docker Scout

### 2. Input Validation

```javascript
import { EmailSchema, SafeStringSchema } from '@unrdf/core/security-schemas';

// Validate email
const result = EmailSchema.safeParse(userInput);
if (!result.success) {
  throw new Error('Invalid email');
}

// Validate safe string
const safeText = SafeStringSchema.parse(userInput);
```

### 3. Output Sanitization

```javascript
import { sanitizeHTML } from '@unrdf/core/security';

// Prevent XSS
const safeHTML = sanitizeHTML(userInput);
```

### 4. Rate Limiting

```javascript
import { RateLimiter } from '@unrdf/core/security';

const limiter = new RateLimiter({ maxRequests: 100, windowMs: 60000 });

if (!limiter.tryConsume(userId)) {
  throw new Error('Rate limit exceeded');
}
```

### 5. CSRF Protection

```javascript
import { CSRFTokenManager } from '@unrdf/core/security';

const csrf = new CSRFTokenManager();
const token = csrf.generate(sessionId);

// Later, verify the token
if (!csrf.verify(sessionId, userToken)) {
  throw new Error('Invalid CSRF token');
}
```

### 6. Security Headers

```javascript
import { getSecurityHeaders } from '@unrdf/core/security';

const headers = getSecurityHeaders();
// Apply to your HTTP response
```

## OWASP Top 10 Coverage

| Vulnerability | Status | Mitigation |
|---------------|--------|------------|
| A01:2021 – Broken Access Control | ✅ | Rate limiting, CSRF tokens |
| A02:2021 – Cryptographic Failures | ✅ | PBKDF2 password hashing, secure random generation |
| A03:2021 – Injection | ✅ | Input validation, parameterized queries |
| A04:2021 – Insecure Design | ✅ | Security-first architecture, threat modeling |
| A05:2021 – Security Misconfiguration | ✅ | Security headers, secure defaults |
| A06:2021 – Vulnerable Components | ✅ | Automated dependency scanning, Dependabot |
| A07:2021 – Authentication Failures | ✅ | Strong password requirements, rate limiting |
| A08:2021 – Software/Data Integrity | ✅ | Package integrity verification, signed commits |
| A09:2021 – Logging/Monitoring Failures | ✅ | OpenTelemetry instrumentation, audit logs |
| A10:2021 – SSRF | ✅ | URL validation, domain whitelisting |

## Compliance

UNRDF follows these security standards:

- **OWASP Top 10 2021**: Full coverage
- **CWE Top 25**: Addressed in design
- **NIST Cybersecurity Framework**: Aligned with core functions

## Security Tooling

### GitHub Actions Workflows

- `.github/workflows/security.yml`: Comprehensive security scanning
- Daily automated scans
- PR-blocking for critical vulnerabilities

### Dependabot

- Automated dependency updates
- Security patches auto-merged
- Weekly update schedule

### Local Development

```bash
# Run security audit
pnpm audit

# Check for secrets
npx gitleaks detect --verbose

# Run all tests
pnpm test

# Type check
pnpm run lint
```

## Security Contacts

- **Security Email**: security@unrdf.dev
- **Project Maintainer**: @seanchatmangpt
- **GitHub Security Advisories**: [Create Advisory](https://github.com/seanchatmangpt/unrdf/security/advisories/new)

## Bug Bounty

We currently do not have a bug bounty program, but we deeply appreciate security researchers who responsibly disclose vulnerabilities. We will publicly acknowledge your contribution in our security advisories.

## Hall of Fame

Security researchers who have helped improve UNRDF's security:

(None yet - be the first!)

## Additional Resources

- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [CWE Top 25](https://cwe.mitre.org/top25/)
- [GitHub Security Best Practices](https://docs.github.com/en/code-security)
- [Node.js Security Best Practices](https://nodejs.org/en/docs/guides/security/)

---

**Last Updated**: December 25, 2025

Thank you for helping keep UNRDF and our users safe!
