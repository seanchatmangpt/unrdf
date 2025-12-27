# Security Policy

This document defines the security invariants, guards, and procedures for the UNRDF monorepo. Security is architectural - not an afterthought.

## Table of Contents

1. [Security Invariants (Q_security)](#security-invariants)
2. [Security Guards (H_security)](#security-guards)
3. [Enforcement Mechanisms](#enforcement-mechanisms)
4. [Remediation Procedures](#remediation-procedures)
5. [Audit Trail Requirements](#audit-trail-requirements)
6. [Reporting Vulnerabilities](#reporting-vulnerabilities)

---

## Security Invariants

Security invariants are properties that MUST hold true across all 42 packages. These are enforced at compile-time, commit-time, and runtime.

### Q_no_hardcoded_secrets

**Definition**: No credentials, API keys, tokens, or secrets shall be committed to the codebase.

**Detection Methods**:
- Pattern-based regex matching for common secret formats
- Shannon entropy analysis (threshold: 4.5 bits per character)
- Context-aware false positive filtering

**Detected Secret Types**:
| Type | Pattern | Severity |
|------|---------|----------|
| AWS Keys | `AKIA[A-Z0-9]{16}` | Critical |
| GitHub Tokens | `gh[pousr]_[A-Za-z0-9]{36,}` | Critical |
| Private Keys | `-----BEGIN.*PRIVATE KEY-----` | Critical |
| JWT Tokens | `eyJ[A-Za-z0-9_-]+\.eyJ[A-Za-z0-9_-]+` | High |
| Database URLs | `(mongodb|postgres|mysql)://.*:.*@` | Critical |
| Generic Secrets | High-entropy strings in code | Medium |

**Remediation**:
1. Immediately rotate any exposed credentials
2. Remove secret from git history using `git filter-branch` or BFG
3. Store secrets in environment variables or secret management service
4. Use `.env.example` with placeholder values for documentation

---

### Q_input_validation

**Definition**: All external inputs must be validated using Zod schemas before processing.

**Requirements**:
- HTTP request bodies validated before use
- Query parameters validated and typed
- File uploads scanned and validated
- Environment variables parsed with defaults

**Example**:
```javascript
import { z } from 'zod';

const RequestSchema = z.object({
  userId: z.string().uuid(),
  action: z.enum(['read', 'write', 'delete']),
  data: z.record(z.unknown()).optional()
});

// Validate before processing
const validated = RequestSchema.parse(request.body);
```

---

### Q_no_dangerous_apis

**Definition**: No usage of eval(), Function constructor, or dynamic code execution.

**Blocked Patterns**:
| Pattern | Risk | Severity |
|---------|------|----------|
| `eval()` | Arbitrary code execution | Critical |
| `new Function()` | Code injection | Critical |
| `setTimeout(string)` | Eval equivalent | High |
| `child_process.exec()` | Command injection | Critical |
| `vm.runInContext()` | Sandbox escape | Critical |
| `innerHTML =` | XSS vulnerability | High |

**Safe Alternatives**:
- Use `JSON.parse()` instead of `eval()` for JSON
- Use `execFile()` with argument arrays instead of `exec()`
- Use `textContent` or sanitized HTML instead of `innerHTML`
- Use `vm2` or `isolated-vm` for sandboxed execution

---

### Q_dependency_security

**Definition**: No packages with known CVEs in critical or high severity.

**Enforcement**:
- Pre-commit: Quick offline check against known vulnerabilities
- CI/CD: Full `pnpm audit` with severity threshold
- Nightly: Comprehensive dependency scan with CVE database sync

**Known Vulnerability Database**:
- lodash < 4.17.21 (CVE-2021-23337)
- minimist < 1.2.6 (CVE-2021-44906)
- node-fetch < 2.6.7 (CVE-2022-0235)
- shell-quote < 1.7.3 (CVE-2021-42740)

**Remediation**:
```bash
# Check for vulnerabilities
pnpm audit

# Update specific package
pnpm update lodash@latest

# Update all packages
pnpm update
```

---

### Q_license_compliance

**Definition**: All dependencies must use licenses compatible with MIT.

**Allowed Licenses**:
- MIT, ISC, BSD-2-Clause, BSD-3-Clause, Apache-2.0
- 0BSD, CC0-1.0, Unlicense, Zlib
- LGPL-2.0, LGPL-2.1, LGPL-3.0, MPL-2.0 (weak copyleft)

**Blocked Licenses**:
- GPL-2.0, GPL-3.0, AGPL-3.0 (strong copyleft)
- SSPL-1.0, EUPL-1.1 (copyleft)
- Proprietary, Commercial (require explicit approval)

**Checking Compliance**:
```bash
node src/security/cli.mjs license-check /path/to/project
```

---

### Q_code_signing

**Definition**: All commits must be signed with a verified GPG key.

**Requirements**:
- GPG key registered with GitHub
- `git config commit.gpgsign true`
- Verified badge on all commits

**Setup**:
```bash
# Generate GPG key
gpg --full-generate-key

# Add to git config
git config --global user.signingkey YOUR_KEY_ID
git config --global commit.gpgsign true

# Add public key to GitHub
gpg --armor --export YOUR_KEY_ID
```

---

### Q_audit_trail

**Definition**: All state-changing operations must emit receipts.

**Receipt Structure**:
```javascript
{
  id: "receipt-abc123",
  timestamp: "2024-01-15T10:30:00Z",
  operation: "write",
  actor: "agent-xyz",
  status: "success",
  result: { /* operation result */ },
  hash: "sha256:..."
}
```

**Required Fields**:
| Field | Type | Required |
|-------|------|----------|
| id | string | Yes |
| timestamp | ISO 8601 | Yes |
| operation | string | Yes |
| actor | string | Yes (for mutations) |
| status | enum | Yes |
| hash | string | Computed |

---

## Security Guards

Security guards are hard blocks that prevent dangerous operations.

### H_hardcoded_credential

**Trigger**: Attempt to commit file containing credentials.

**Action**: Block commit, display warning with file location.

**Override**: Not possible. Credentials must be removed.

### H_code_injection

**Trigger**: Use of eval, Function, or dynamic code execution.

**Action**: Block commit with security warning.

**Override**: Add `// @security-ignore` with justification (reviewed by security team).

### H_dependency_exploit

**Trigger**: Add dependency with critical CVE.

**Action**: Block dependency installation.

**Override**: Create security exception with documented risk acceptance.

### H_license_incompatible

**Trigger**: Add dependency with blocked license.

**Action**: Block dependency installation.

**Override**: Legal review and explicit approval in `license-exceptions.json`.

---

## Enforcement Mechanisms

### Pre-Commit Hooks

```bash
# .husky/pre-commit
#!/bin/sh
node src/security/cli.mjs quick-check --staged
```

### CI/CD Pipeline

```yaml
# .github/workflows/security.yml
security-check:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - name: Run security checks
      run: node src/security/cli.mjs full-check
    - name: Block on failure
      if: failure()
      run: exit 1
```

### Nightly Scans

- Full dependency audit against latest CVE databases
- License compliance matrix generation
- Security trend analysis
- Dashboard update

---

## Remediation Procedures

### Secret Exposure

1. **Immediate**: Rotate the exposed credential
2. **Within 1 hour**: Remove from git history
3. **Within 24 hours**: Audit access logs for misuse
4. **Within 1 week**: Post-mortem and prevention measures

### Vulnerability Discovery

1. **Critical (CVSS 9.0+)**: Patch within 24 hours
2. **High (CVSS 7.0-8.9)**: Patch within 1 week
3. **Moderate (CVSS 4.0-6.9)**: Patch within 1 month
4. **Low (CVSS < 4.0)**: Include in next release

### License Issue

1. Identify affected package(s)
2. Find alternative with compatible license
3. If no alternative, request legal review
4. Document decision in `license-decisions.md`

---

## Audit Trail Requirements

### What Must Be Logged

- All write operations to knowledge graph
- User authentication events
- Permission changes
- Configuration modifications
- Security-related events

### Retention Policy

| Event Type | Retention |
|------------|-----------|
| Security events | 2 years |
| Write operations | 1 year |
| Read operations | 90 days |
| Debug logs | 30 days |

### Integrity Verification

All receipts include SHA-256 hash for tamper detection:

```javascript
const hash = crypto
  .createHash('sha256')
  .update(JSON.stringify(receipt))
  .digest('hex');
```

---

## Reporting Vulnerabilities

### Responsible Disclosure

If you discover a security vulnerability:

1. **Do NOT** create a public GitHub issue
2. Email security findings to the maintainers
3. Include:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact
   - Suggested fix (if any)

### Response Timeline

| Severity | Initial Response | Resolution Target |
|----------|------------------|-------------------|
| Critical | 4 hours | 24 hours |
| High | 24 hours | 1 week |
| Moderate | 1 week | 1 month |
| Low | 1 month | Next release |

### Recognition

Security researchers who report valid vulnerabilities will be:
- Credited in the security advisory (if desired)
- Listed in SECURITY.md acknowledgments

---

## Security Dashboard

Access the security dashboard:

```bash
# Generate dashboard
node src/security/cli.mjs dashboard --output ./security-report.html

# View in browser
open ./security-report.html
```

Dashboard shows:
- Overall security score (0-100)
- Component scores (secrets, injection, dependencies, licenses, audit)
- Trend over time (improving/stable/degrading)
- Critical findings requiring immediate action

---

## Compliance

This security policy aligns with:
- OWASP Top 10 (2021)
- CWE/SANS Top 25
- NIST Cybersecurity Framework
- SOC 2 Type II requirements

Regular audits ensure continued compliance.

---

*Last updated: 2024-12-26*
*Version: 1.0.0*
