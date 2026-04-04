# How to Configure Security

**Goal**: Enable API key authentication for daemon operations and understand the injection detection that protects all inputs.

---

## Enable API key authentication

The daemon ships with BLAKE3-based API key authentication. Use `createAuthenticator` to generate a key and validate incoming requests:

```javascript
import { createAuthenticator } from '@unrdf/daemon';

// Generate a key for production
const { authenticator, key } = await createAuthenticator({
  environment: 'production',
});

console.log('API Key:', key); // Distribute this to authorized callers

// Authenticate each incoming request
const result = await authenticator.authenticate({
  headers: { 'x-api-key': receivedKey },
});

if (result.authenticated) {
  await executeDaemonOperation();
} else {
  throw new Error('Unauthorized');
}
```

The API key can also be supplied via the `UNRDF_API_KEY` environment variable:

```bash
export UNRDF_API_KEY=your-secret-key-here
```

---

## Understand environment-aware enforcement

Security behavior differs by environment to avoid blocking development:

| Environment   | No API key provided      | Invalid API key          |
| ------------- | ------------------------ | ------------------------ |
| `development` | Warns in logs, continues | Warns in logs, continues |
| `production`  | Blocks with 401          | Blocks with 401          |

Set the environment via the `environment` option in `createAuthenticator` or through `NODE_ENV`.

---

## Understand injection detection

All daemon operations automatically scan inputs for injection patterns before execution. This runs on every operation handler invocation and covers:

- **SQL injection** — common SQL keywords and patterns
- **SPARQL injection** — malformed SPARQL fragments
- **Command injection** — shell metacharacters and command substitution patterns
- **Secret detection** — prevents credential strings from leaking into logs or results
- **Path traversal** — `../` and encoded equivalents in file path arguments

If an injection is detected, the operation is rejected before the handler is called and an error is emitted via `operation:failure`.

---

## Audit security events

The authenticator emits comprehensive audit logs for all authentication and rejection events. Connect these to your logging infrastructure:

```javascript
import { createAuthenticator } from '@unrdf/daemon';

const { authenticator } = await createAuthenticator({
  environment: 'production',
  onAuditEvent: event => {
    logger.info('security_audit', {
      type: event.type, // 'auth_success' | 'auth_failure' | 'injection_detected'
      timestamp: event.timestamp,
      requestId: event.requestId,
      reason: event.reason,
    });
  },
});
```

---

## Example: hardened operation handler

Combine authentication, input validation, and error sanitization in a real handler:

```javascript
import { Daemon, createAuthenticator, validateInput } from '@unrdf/daemon';

const { authenticator } = await createAuthenticator({ environment: 'production' });
const daemon = new Daemon({ id: 'secure-daemon' });

const secureOp = {
  id: 'process-query',
  name: 'Process SPARQL Query',
  handler: async ({ apiKey, query }) => {
    // 1. Authenticate
    const auth = await authenticator.authenticate({
      headers: { 'x-api-key': apiKey },
    });
    if (!auth.authenticated) throw new Error('Unauthorized');

    // 2. Validate input (injection detection)
    validateInput(query, { type: 'sparql' });

    // 3. Execute
    return await runQuery(query);
  },
};

await daemon.start();
daemon.schedule(secureOp);
```

---

## Key security properties

| Feature            | Detail                                                           |
| ------------------ | ---------------------------------------------------------------- |
| Key hashing        | BLAKE3 256-bit, constant-time comparison                         |
| Timing attacks     | Constant-time verification prevents timing-based key enumeration |
| Error sanitization | Error messages strip sensitive values before emitting            |
| Audit trail        | Every auth event is logged with timestamp and request context    |

---

## See also

- [Environment Variables Reference](../reference/environment-variables.md)
- [Daemon API Reference](../reference/daemon-api.md)
- `packages/daemon/AUTHENTICATION.md` — complete authentication guide
- `packages/daemon/docs/security-hardening.md` — detailed security implementation
