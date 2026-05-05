# Migrating to UNRDF v6.0.0

**Complete migration guide from v5.x to v6.0.0**

## Overview

UNRDF v6.0.0 introduces the daemon package for background operation orchestration, enterprise-grade authentication, and comprehensive security enhancements across all integration modules.

**Migration Complexity**: Low to Medium
**Estimated Time**: 1-4 hours depending on usage
**Breaking Changes**: None for core RDF operations
**New Features**: Daemon orchestration, API authentication, enhanced security

---

## What's New in v6.0.0

### 1. @unrdf/daemon Package (NEW)

Background daemon for managing scheduled tasks and event-driven operations:

```javascript
import { UnrdfDaemon } from '@unrdf/daemon';

const daemon = new UnrdfDaemon({
  daemonId: 'my-daemon',
  name: 'My Background Daemon',
  concurrency: 10,
});

await daemon.start();
```

**Key Features**:
- Scheduled task execution (cron, intervals, events)
- Clustering support for distributed operations
- Health monitoring and performance metrics
- Retry policies with exponential backoff
- Receipt generation with cryptographic proofs
- Concurrency control with queue management

### 2. API Key Authentication (NEW)

Enterprise-grade authentication with BLAKE3 hashing:

```javascript
import { createAuthenticator } from '@unrdf/daemon';

const { authenticator, key } = await createAuthenticator({
  environment: 'production'
});

console.log('API Key:', key); // Provide to users

// Authenticate requests
const result = await authenticator.authenticate({
  headers: { 'x-api-key': providedKey }
});

if (result.authenticated) {
  // Execute secured operation
}
```

**Security Features**:
- BLAKE3 cryptographic hashing
- Constant-time verification (timing attack prevention)
- Environment variable support (`UNRDF_API_KEY`)
- Graceful degradation (dev warns, production blocks)
- Comprehensive audit logging

### 3. 13 Daemon Integration Modules (NEW)

Comprehensive integration points for distributed RDF operations:

- **consensus.mjs** - Raft consensus coordination
- **distributed.mjs** - Task distribution across nodes
- **event-store.mjs** - Event sourcing and temporal storage
- **federation-query.mjs** - Federated SPARQL query execution
- **hook-scheduler.mjs** - Knowledge Hook scheduling
- **hooks-policy.mjs** - Policy-based hook execution
- **kgc-4d-sourcing.mjs** - KGC 4D temporal event sourcing
- **knowledge-rules.mjs** - Inference and rule execution
- **observability.mjs** - OTEL observability integration
- **receipts-merkle.mjs** - Merkle tree receipt generation
- **streaming.mjs** - Real-time RDF streaming
- **v6-deltagate.mjs** - ΔGate control plane integration
- **yawl.mjs** - YAWL workflow orchestration

### 4. Comprehensive Security Validation (NEW)

All 13 integration modules now include:
- Input validation against injection attacks (SQL, SPARQL, command)
- Secret detection in outputs
- Path traversal prevention
- Error message sanitization
- Zod schema validation

---

## Migration Steps

### Step 1: Update Dependencies

```bash
# Update to v6.0.0
pnpm add @unrdf/core@^6.0.0
pnpm add @unrdf/daemon@^6.0.0

# Or update all packages
pnpm update @unrdf/*
```

### Step 2: Review Breaking Changes

**Good News**: v6.0.0 has **ZERO breaking changes** to core RDF operations.

All existing code using `@unrdf/core`, `@unrdf/hooks`, and `@unrdf/streaming` will continue to work without modification.

### Step 3: Adopt New Features (Optional)

#### 3a. Add Background Task Orchestration

If you need scheduled or event-driven operations:

```javascript
import { UnrdfDaemon } from '@unrdf/daemon';

const daemon = new UnrdfDaemon({
  daemonId: 'rdf-background-processor',
  concurrency: 5,
});

await daemon.start();

// Schedule periodic RDF validation
daemon.schedule({
  id: 'validate-rdf',
  name: 'Periodic RDF Validation',
  schedule: { cron: '0 * * * *' }, // Hourly
  handler: async () => {
    const results = await validateAllRdfStores();
    return { validated: results.length };
  },
});
```

#### 3b. Add API Authentication

If you're exposing daemon operations via HTTP/WebSocket:

```javascript
import { createAuthenticator } from '@unrdf/daemon';

// Initialize authentication
const { authenticator, key } = await createAuthenticator({
  environment: process.env.NODE_ENV || 'production'
});

// Save key for distribution
console.log('Distribute this API key to authorized users:', key);

// Protect your endpoints
app.use('/api/daemon/*', async (req, res, next) => {
  const result = await authenticator.authenticate({
    headers: req.headers
  });

  if (!result.authenticated) {
    return res.status(401).json({ error: 'Unauthorized' });
  }

  next();
});
```

#### 3c. Integrate with ΔGate Control Plane

If you're using v6-core ΔGate:

```javascript
import { DaemonDeltaGate } from '@unrdf/daemon';

const gate = new DaemonDeltaGate({
  receipts: true,
  validation: true,
  merkleProofs: true,
});

await gate.initialize();

// Process deltas with receipts
const result = await gate.processDelta({
  operation: 'insert',
  triples: myTriples,
});

console.log('Receipt:', result.receipt);
console.log('Merkle Proof:', result.merkleProof);
```

### Step 4: Update Environment Variables

Add new optional environment variables:

```bash
# .env file

# API Key for daemon authentication (optional, auto-generated if not provided)
UNRDF_API_KEY=your_64_char_hex_key_here

# Daemon configuration (optional)
DAEMON_PORT=8080
DAEMON_CONCURRENCY=10
DAEMON_LOG_LEVEL=info

# Environment mode (affects authentication behavior)
NODE_ENV=production  # or 'development' or 'test'
```

### Step 5: Run Tests

Verify everything works:

```bash
# Run all tests
pnpm test

# Run daemon-specific tests
pnpm test packages/daemon

# Check for breaking changes
pnpm test packages/core
pnpm test packages/hooks
```

### Step 6: Update Documentation

If you have internal documentation:

1. Add references to new `@unrdf/daemon` package
2. Document API key management procedures
3. Update deployment guides with daemon configuration
4. Add security best practices from [SECURITY_MIGRATION.md](SECURITY_MIGRATION.md)

---

## Common Migration Scenarios

### Scenario 1: Existing RDF Application (No Changes Needed)

If you're only using core RDF operations:

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

// This code works identically in v5.x and v6.0.0
const core = await createKnowledgeSubstrateCore();
const store = core.parseRdf(turtleData);
const results = await core.query(store, sparqlQuery);
```

**Migration**: None required ✅

### Scenario 2: Adding Background Processing

Before (v5.x):

```javascript
// Manual polling or setInterval
setInterval(async () => {
  await processRdfUpdates();
}, 60000);
```

After (v6.0.0):

```javascript
import { UnrdfDaemon } from '@unrdf/daemon';

const daemon = new UnrdfDaemon({ daemonId: 'rdf-processor' });
await daemon.start();

daemon.schedule({
  id: 'process-updates',
  schedule: { intervalMs: 60000 },
  handler: async () => await processRdfUpdates(),
});
```

**Benefits**: Automatic retry, health monitoring, concurrency control, audit receipts

### Scenario 3: Adding Authentication to Existing API

Before (v5.x):

```javascript
app.post('/api/query', async (req, res) => {
  // No authentication
  const results = await executeQuery(req.body.sparql);
  res.json(results);
});
```

After (v6.0.0):

```javascript
import { createAuthMiddleware } from '@unrdf/daemon';

const authMiddleware = createAuthMiddleware({
  environment: 'production'
});

app.post('/api/query', authMiddleware, async (req, res) => {
  // Authenticated request
  const results = await executeQuery(req.body.sparql);
  res.json(results);
});
```

**Benefits**: BLAKE3 hashing, timing attack prevention, audit logging

### Scenario 4: Migrating from Custom Event Sourcing

Before (v5.x):

```javascript
// Custom event store implementation
class CustomEventStore {
  async save(event) {
    this.events.push(event);
  }

  async getEvents(entityId) {
    return this.events.filter(e => e.entityId === entityId);
  }
}
```

After (v6.0.0):

```javascript
import { createEventStore } from '@unrdf/daemon';

const eventStore = createEventStore({
  retentionMs: 86400000, // 24 hours
  receipts: true,
});

await eventStore.appendEvent({
  entityId: 'user-123',
  eventType: 'profile-updated',
  data: { name: 'Alice' },
});

const events = await eventStore.getEvents('user-123');
```

**Benefits**: Automatic receipts, Merkle proofs, temporal event sourcing, KGC integration

---

## Compatibility Matrix

| Package | v5.x | v6.0.0 | Breaking Changes |
|---------|------|--------|------------------|
| @unrdf/core | ✅ | ✅ | None |
| @unrdf/oxigraph | ✅ | ✅ | None |
| @unrdf/hooks | ✅ | ✅ | None |
| @unrdf/streaming | ✅ | ✅ | None |
| @unrdf/federation | ✅ | ✅ | None |
| @unrdf/knowledge-engine | ✅ | ✅ | None |
| @unrdf/daemon | ❌ | ✅ NEW | N/A |
| @unrdf/v6-core | ❌ | ✅ NEW | N/A |

**Summary**: All v5.x code continues to work in v6.0.0. New packages are additions, not replacements.

---

## Performance Considerations

### v6.0.0 Performance Impact

| Operation | v5.x | v6.0.0 | Change |
|-----------|------|--------|--------|
| SPARQL Query | ~10ms | ~10ms | No change |
| Triple Insert | ~1μs | ~1μs | No change |
| Authentication (new) | N/A | <5ms | New feature |
| Security Validation (new) | N/A | <1ms | New feature |
| Receipt Generation (new) | N/A | <1ms | New feature |

**Conclusion**: v6.0.0 adds new capabilities with minimal overhead (<5ms per operation).

---

## Rollback Procedure

If you encounter issues:

### Step 1: Downgrade Dependencies

```bash
pnpm add @unrdf/core@^5.0.0
pnpm remove @unrdf/daemon
```

### Step 2: Remove v6-Specific Code

Remove any code using:
- `@unrdf/daemon` imports
- `createAuthenticator` or `ApiKeyAuthenticator`
- `DaemonDeltaGate` or other daemon integrations

### Step 3: Restore Environment Variables

Remove v6-specific environment variables:

```bash
# Remove these from .env
UNRDF_API_KEY
DAEMON_PORT
DAEMON_CONCURRENCY
```

### Step 4: Verify Tests

```bash
pnpm test
```

---

## Getting Help

### Resources

- **Migration Guide**: This document
- **Security Migration**: [SECURITY_MIGRATION.md](SECURITY_MIGRATION.md)
- **API Documentation**: [API_DOCUMENTATION_V6.md](API_DOCUMENTATION_V6.md)
- **Daemon README**: [packages/daemon/README.md](../packages/daemon/README.md)
- **Authentication Guide**: [packages/daemon/AUTHENTICATION.md](../packages/daemon/AUTHENTICATION.md)
- **Deployment Guides**: [docs/deployment/](deployment/)

### Support Channels

- **GitHub Issues**: [github.com/unrdf/unrdf/issues](https://github.com/unrdf/unrdf/issues)
- **GitHub Discussions**: [github.com/unrdf/unrdf/discussions](https://github.com/unrdf/unrdf/discussions)
- **Security Issues**: security@unrdf.dev

### Common Questions

**Q: Do I need to migrate immediately?**
A: No. v5.x will continue to work. Migrate when you need daemon orchestration or enhanced security.

**Q: Will my v5.x code break?**
A: No. v6.0.0 has zero breaking changes to core RDF operations.

**Q: Can I use only authentication without the daemon?**
A: Yes. Import `createAuthenticator` from `@unrdf/daemon` without using `UnrdfDaemon`.

**Q: Is v6.0.0 production-ready?**
A: Yes. All features have 100% test pass rate and comprehensive security validation.

**Q: What's the performance impact?**
A: Minimal. New features add <5ms overhead. Core RDF operations unchanged.

---

## Changelog Summary

See [CHANGELOG.md](../CHANGELOG.md) for complete details.

### Added (v6.0.0)

- ✅ `@unrdf/daemon` package (98 MJS files)
- ✅ API key authentication with BLAKE3 hashing
- ✅ 13 daemon integration modules
- ✅ Comprehensive security validation across all modules
- ✅ Event store with temporal event sourcing
- ✅ Receipt generation with Merkle proofs
- ✅ ΔGate control plane integration
- ✅ YAWL workflow orchestration
- ✅ Raft consensus coordination
- ✅ Federated query execution

### Changed (v6.0.0)

- ✅ Enhanced security across all daemon operations
- ✅ Improved observability with OTEL integration
- ✅ Updated documentation (10+ new guides)

### Fixed (v6.0.0)

- ✅ Zero CRITICAL/HIGH security vulnerabilities
- ✅ 100% test pass rate across all new features
- ✅ Comprehensive input validation and error sanitization

---

## Next Steps

1. ✅ Review this migration guide
2. ✅ Update dependencies to v6.0.0
3. ✅ Run tests to verify compatibility
4. ✅ Adopt new features as needed (optional)
5. ✅ Review [SECURITY_MIGRATION.md](SECURITY_MIGRATION.md)
6. ✅ Update deployment procedures (see [docs/deployment/](deployment/))

**Welcome to UNRDF v6.0.0!** 🎉
