# Security Checklist for Pull Requests

Use this checklist to ensure your PR meets security standards before requesting review.

## Pre-Submit Checklist

### 1. Code Security

- [ ] **No hardcoded secrets** - API keys, passwords, tokens in environment variables only
- [ ] **No sensitive data in logs** - No passwords, tokens, or PII in console.log/debug statements
- [ ] **Input validation** - All user inputs validated with Zod schemas
- [ ] **Output sanitization** - HTML/SQL/command outputs properly escaped
- [ ] **No eval()** - No use of eval(), Function constructor, or similar
- [ ] **No command injection** - No unsanitized data in child_process.exec()
- [ ] **Path traversal prevention** - File paths validated, no `..` or `~` in user-provided paths
- [ ] **Type safety** - JSDoc types for all functions (100% coverage)

### 2. Authentication & Authorization

- [ ] **Access control** - Proper authorization checks for protected resources
- [ ] **Rate limiting** - Rate limiting implemented for public endpoints
- [ ] **CSRF protection** - CSRF tokens for state-changing operations
- [ ] **Password security** - Passwords hashed with PBKDF2 (if applicable)
- [ ] **Session management** - Secure session handling with timeouts

### 3. Dependencies

- [ ] **pnpm audit passes** - Run `pnpm audit` with no high/critical issues
- [ ] **Dependencies up to date** - No outdated packages with known vulnerabilities
- [ ] **License compliance** - All dependencies use approved licenses
- [ ] **No unnecessary dependencies** - Only essential packages included
- [ ] **Package integrity** - Lockfile committed and integrity verified

### 4. API Security

- [ ] **HTTPS only** - No HTTP endpoints in production config
- [ ] **CORS configured** - Proper CORS settings (not `*` in production)
- [ ] **Security headers** - Using `getSecurityHeaders()` utility
- [ ] **Content-Type validation** - Strict Content-Type checking
- [ ] **Request size limits** - Max request body size enforced

### 5. Data Security

- [ ] **Data encryption** - Sensitive data encrypted at rest
- [ ] **Secure transmission** - TLS for all external communications
- [ ] **Data minimization** - Only collect necessary data
- [ ] **Data retention** - Proper data cleanup and retention policies
- [ ] **Backup security** - Backups encrypted and access-controlled

### 6. SPARQL/RDF Security

- [ ] **Query validation** - SPARQL queries validated with SPARQLQuerySchema
- [ ] **Query limits** - Maximum query complexity and timeout enforced
- [ ] **No dangerous operations** - No LOAD, CLEAR, DROP in untrusted queries
- [ ] **Namespace validation** - Prefixes and namespaces validated
- [ ] **Triple validation** - RDF triples validated before insertion

### 7. Error Handling

- [ ] **No sensitive info in errors** - Error messages don't leak implementation details
- [ ] **Proper error codes** - HTTP status codes correctly used
- [ ] **Error logging** - Errors logged with proper context (no sensitive data)
- [ ] **Graceful degradation** - Failures don't expose system internals
- [ ] **Stack traces hidden** - No stack traces in production responses

### 8. Testing

- [ ] **Security tests included** - Tests for input validation, authorization, etc.
- [ ] **Edge cases covered** - Boundary conditions and malicious inputs tested
- [ ] **No secrets in tests** - Test data uses mock values, not real secrets
- [ ] **Coverage maintained** - Security-critical code 100% covered
- [ ] **Tests pass locally** - Run `timeout 5s pnpm test` successfully

### 9. Documentation

- [ ] **Security implications documented** - Any security-relevant changes explained
- [ ] **API docs updated** - Authentication/authorization requirements documented
- [ ] **Example code secure** - Code examples follow security best practices
- [ ] **Migration guide** - Breaking security changes clearly communicated

### 10. CI/CD

- [ ] **Security workflow passes** - `.github/workflows/security.yml` succeeds
- [ ] **No secrets in repo** - Gitleaks scan passes
- [ ] **CodeQL passes** - SAST analysis completes without issues
- [ ] **Dependency review passes** - No new vulnerable dependencies

## Automated Checks (Run Locally)

```bash
# 1. Run security audit
timeout 5s pnpm audit --audit-level high

# 2. Run all tests
timeout 10s pnpm test

# 3. Check for secrets (requires gitleaks)
npx gitleaks detect --no-git

# 4. Type checking
timeout 5s pnpm run lint

# 5. Check licenses
npx license-checker --onlyAllow "MIT;Apache-2.0;BSD-2-Clause;BSD-3-Clause;ISC"
```

## Common Vulnerabilities to Avoid

### SQL Injection
```javascript
// ❌ WRONG - Vulnerable to SQL injection
const query = `SELECT * FROM users WHERE id = ${userId}`;

// ✅ CORRECT - Use parameterized queries
const query = 'SELECT * FROM users WHERE id = ?';
db.query(query, [userId]);
```

### XSS (Cross-Site Scripting)
```javascript
// ❌ WRONG - Vulnerable to XSS
element.innerHTML = userInput;

// ✅ CORRECT - Sanitize HTML
import { sanitizeHTML } from '@unrdf/core/security';
element.innerHTML = sanitizeHTML(userInput);
```

### Path Traversal
```javascript
// ❌ WRONG - Vulnerable to path traversal
const filePath = `./uploads/${req.params.filename}`;
fs.readFile(filePath);

// ✅ CORRECT - Validate path
import { isPathSafe } from '@unrdf/core/security';
const filename = req.params.filename;
if (!isPathSafe(filename, './uploads')) {
  throw new Error('Invalid filename');
}
const filePath = path.join('./uploads', path.basename(filename));
```

### Command Injection
```javascript
// ❌ WRONG - Vulnerable to command injection
exec(`git clone ${userUrl}`);

// ✅ CORRECT - Validate input and use array form
import { URLSchema } from '@unrdf/core/security-schemas';
const url = URLSchema.parse(userUrl);
execFile('git', ['clone', url]);
```

### Hardcoded Secrets
```javascript
// ❌ WRONG - Hardcoded API key
const apiKey = 'sk-1234567890abcdef';

// ✅ CORRECT - Use environment variables
const apiKey = process.env.API_KEY;
if (!apiKey) throw new Error('API_KEY not configured');
```

## Severity Guidelines

| Severity | Examples | Action Required |
|----------|----------|-----------------|
| **Critical** | Authentication bypass, RCE, SQL injection | Fix immediately, block PR |
| **High** | XSS, CSRF, sensitive data exposure | Fix before merge |
| **Medium** | Information disclosure, weak crypto | Fix or document mitigation |
| **Low** | Missing security headers, verbose errors | Fix when possible |
| **Info** | Best practice violations | Consider fixing |

## Resources

- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [OWASP Cheat Sheet Series](https://cheatsheetseries.owasp.org/)
- [Node.js Security Best Practices](https://nodejs.org/en/docs/guides/security/)
- [CWE Top 25](https://cwe.mitre.org/top25/)
- [UNRDF Security Policy](../SECURITY.md)

## Questions?

If you're unsure about any security aspect:

1. Check the [SECURITY.md](../SECURITY.md) documentation
2. Review existing secure code patterns in `/examples/security-patterns.mjs`
3. Ask in PR comments (tag @seanchatmangpt)
4. Consult OWASP guidelines

---

**Remember**: Security is not optional. Every line of code is a potential vulnerability. When in doubt, ask!
