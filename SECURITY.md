# Security Policy

## Supported Versions

We actively support and provide security updates for the following versions:

| Version | Supported          | Status |
| ------- | ------------------ | ------ |
| 5.x     | ✅ Yes            | Active |
| 4.x     | ❌ No             | End of Life |
| < 4.0   | ❌ No             | End of Life |

## Reporting a Vulnerability

**IMPORTANT: DO NOT open public issues for security vulnerabilities.**

We take security seriously and appreciate your efforts to responsibly disclose vulnerabilities.

### How to Report

**Email:** security@unrdf.io

**Subject Line:** `[SECURITY] Brief description of the issue`

**Include the following information:**
- Type of vulnerability (e.g., XSS, SQL injection, authentication bypass)
- Full paths of source file(s) related to the vulnerability
- Location of the affected source code (tag/branch/commit or direct URL)
- Step-by-step instructions to reproduce the issue
- Proof-of-concept or exploit code (if possible)
- Impact of the issue, including how an attacker might exploit it

### Response Timeline

| Severity | Initial Response | Fix Timeline | Disclosure |
|----------|-----------------|--------------|------------|
| **Critical** | Within 24 hours | 7 days | After fix released |
| **High** | Within 48 hours | 30 days | After fix released |
| **Medium** | Within 5 days | 90 days | After fix released |
| **Low** | Within 10 days | 120 days | After fix released |

### What to Expect

1. **Acknowledgment:** We will acknowledge receipt of your report within the timeframe above
2. **Communication:** We will keep you informed of the progress toward a fix
3. **Verification:** We may ask for additional information or guidance
4. **Credit:** We will credit you in the security advisory (unless you prefer to remain anonymous)
5. **Disclosure:** We will coordinate disclosure timing with you

## Security Scanning

Our security measures include:

### Automated Scanning
- **Dependency Audits:** Weekly automated scans via Dependabot and GitHub Security
- **SAST (Static Application Security Testing):** CodeQL analysis on every push
- **Secrets Scanning:** TruffleHog and Gitleaks for credential detection
- **Container Scanning:** Trivy and Docker Scout for container vulnerabilities
- **License Compliance:** Automated license checker for dependency compliance

### Manual Reviews
- Security-focused code reviews for all pull requests
- Periodic security audits by the core team
- Third-party security assessments for major releases

### CI/CD Security
All commits and pull requests undergo:
- ESLint security rules enforcement
- Dependency vulnerability checks (moderate+ severity blocks merge)
- Supply chain attack prevention via locked dependencies
- Integrity verification of all packages

## Security Best Practices

When contributing to UNRDF, please follow these security guidelines:

### Code Security
- ✅ Never commit secrets, API keys, or credentials
- ✅ Use environment variables for sensitive configuration
- ✅ Validate and sanitize all user inputs
- ✅ Use parameterized queries to prevent injection attacks
- ✅ Follow the principle of least privilege
- ✅ Keep dependencies up to date

### Dependency Management
- ✅ Use `pnpm install --frozen-lockfile` to ensure reproducible builds
- ✅ Review dependency changes in pull requests
- ✅ Prefer well-maintained, audited libraries
- ✅ Pin exact versions for production dependencies

### Authentication & Authorization
- ✅ Use strong cryptographic algorithms (e.g., bcrypt for passwords)
- ✅ Implement proper session management
- ✅ Enforce authentication on all protected endpoints
- ✅ Use HTTPS for all network communication

## Known Security Considerations

### RDF Processing
- SPARQL query injection: All user-provided queries are validated
- Resource consumption: Query timeouts and memory limits enforced
- Malicious ontologies: Input validation and schema restrictions

### Service Worker
- Cross-origin isolation: Proper COOP/COEP headers required
- SharedArrayBuffer: Used with security considerations
- WASM execution: Sandboxed within worker context

## Security Updates

Security updates are released as:
- **Patch releases** (x.y.Z) for all supported versions
- **GitHub Security Advisories** for critical issues
- **npm security advisories** when packages are affected

Subscribe to our release notifications to stay informed:
- Watch this repository on GitHub
- Follow npm package updates
- Subscribe to our security mailing list (security@unrdf.io)

## Bug Bounty Program

We currently do not offer a formal bug bounty program, but we deeply appreciate security researchers who report vulnerabilities responsibly. We provide:

- Public acknowledgment (with your permission)
- Direct communication with maintainers
- Priority handling of your reports
- Attribution in release notes and security advisories

## Contact

For security-related questions or concerns:
- **Email:** security@unrdf.io
- **Response Time:** Within 48 hours
- **PGP Key:** Available on request

For general support:
- **GitHub Issues:** https://github.com/commontoolsinc/unrdf/issues
- **Discussions:** https://github.com/commontoolsinc/unrdf/discussions

---

**Last Updated:** 2025-12-20

Thank you for helping keep UNRDF and its users safe!
