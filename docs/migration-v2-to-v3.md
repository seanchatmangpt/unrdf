# Migration Guide: v2.1.1 to v3.0.0

Complete guide for upgrading from UNRDF v2.1.1 to v3.0.0.

## Overview

UNRDF v3.0.0 introduces significant improvements:

- ✅ **Modern CLI** - kubectl/docker-style noun-verb interface with citty
- ✅ **Production Sidecar** - Enterprise-grade gRPC sidecar pattern
- ✅ **Improved APIs** - Enhanced composables and knowledge hooks
- ⚠️ **Breaking Changes** - Some APIs have changed

**Estimated Migration Time:** 30-60 minutes for most projects

## Breaking Changes

### 1. CLI Command Structure

**v2.1.1 (Old):**

```bash
# Old imperative style
unrdf parse-file data.ttl
unrdf run-query "SELECT * WHERE { ?s ?p ?o }"
unrdf validate-shacl data.ttl shapes.ttl
```

**v3.0.0 (New):**

```bash
# New noun-verb style
unrdf parse data.ttl
unrdf query select "SELECT * WHERE { ?s ?p ?o }"
unrdf graph validate data.ttl --shapes shapes.ttl
```

**Migration:**

Update all CLI scripts to use the new noun-verb pattern:

```bash
# Find and replace in scripts
sed -i 's/unrdf parse-file/unrdf parse/g' *.sh
sed -i 's/unrdf run-query/unrdf query select/g' *.sh
sed -i 's/unrdf validate-shacl/unrdf graph validate/g' *.sh
```

### 2. Knowledge Hook Definition

**v2.1.1 (Old):**

```javascript
// Old: Inline SPARQL strings
export const hook = {
  id: 'compliance:large-tx',
  condition: `ASK { ?tx ex:amount ?amt . FILTER(?amt > 10000) }`,
  action: async (payload) => {
    // ...
  }
};
```

**v3.0.0 (New):**

```javascript
// New: Content-addressed file references
import { defineHook } from 'unrdf';

export default defineHook({
  meta: {
    name: 'compliance:large-tx',
    description: 'Alert on large transactions'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/large-tx.ask.rq',
      sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
      mediaType: 'application/sparql-query'
    }
  },
  async run({ payload, context }) {
    // ...
  }
});
```

**Migration Steps:**

1. Extract inline SPARQL to files:

```bash
# Create hooks directory
mkdir -p hooks

# Extract SPARQL from JavaScript
# (Manual step - copy inline queries to .rq files)
```

2. Compute SHA256 hashes:

```bash
# Compute hash for verification
sha256sum hooks/large-tx.ask.rq
```

3. Update hook definitions:

```javascript
// Before
export const hook = {
  id: 'my-hook',
  condition: `ASK { ... }`,
  action: async (payload) => { /* ... */ }
};

// After
import { defineHook } from 'unrdf';

export default defineHook({
  meta: { name: 'my-hook' },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/my-hook.ask.rq',
      sha256: '<computed-hash>',
      mediaType: 'application/sparql-query'
    }
  },
  async run({ payload }) { /* ... */ }
});
```

### 3. Store Context Initialization

**v2.1.1 (Old):**

```javascript
import { createStore } from 'unrdf';

const store = createStore({
  baseIRI: 'http://example.org/'
});

// Direct store usage
store.add(quad);
```

**v3.0.0 (New):**

```javascript
import { initStore, useGraph } from 'unrdf';

const runApp = initStore([], {
  baseIRI: 'http://example.org/'
});

await runApp(() => {
  // Use composables within context
  const graph = useGraph();
  // ...
});
```

**Migration:**

Replace direct store creation with context-based initialization:

```javascript
// Before
const store = createStore(config);
const engine = new RdfEngine(store);
// ... direct store operations

// After
const runApp = initStore([], config);
await runApp(() => {
  const graph = useGraph();
  // ... use composables
});
```

### 4. Hook Manager API

**v2.1.1 (Old):**

```javascript
import { TransactionManager } from 'unrdf';

const manager = new TransactionManager();

manager.registerHook({
  id: 'my-hook',
  mode: 'pre',
  condition: () => true,
  effect: 'veto'
});
```

**v3.0.0 (New):**

```javascript
import { KnowledgeHookManager, defineHook } from 'unrdf';

const manager = new KnowledgeHookManager({
  basePath: process.cwd(),
  enableKnowledgeHooks: true
});

const hook = defineHook({ /* ... */ });
manager.addKnowledgeHook(hook);
```

**Migration:**

```javascript
// Before
manager.registerHook({
  id: 'my-hook',
  mode: 'pre',
  condition: (store, delta) => {
    // Check condition
    return true;
  },
  effect: 'veto'
});

// After
const hook = defineHook({
  meta: { name: 'my-hook' },
  when: {
    kind: 'sparql-ask',
    ref: { /* file reference */ }
  },
  async run({ payload, context }) {
    // Hook logic
    return { result: { /* ... */ } };
  }
});

manager.addKnowledgeHook(hook);
```

## New Features in v3.0.0

### 1. Sidecar Pattern

New in v3: Deploy knowledge engine as a sidecar service.

**CLI Auto-Detection:**

```bash
# Set sidecar address (optional)
export KGC_SIDECAR_ADDRESS=localhost:50051

# CLI automatically uses sidecar if available
unrdf hook eval health-check.mjs

# Falls back to embedded engine if sidecar unavailable
```

**Programmatic Usage:**

```javascript
import { SidecarClient } from 'unrdf/sidecar';

const client = new SidecarClient({
  address: 'localhost:50051',
  timeout: 30000
});

await client.connect();

const result = await client.applyTransaction({
  additions: [/* quads */],
  removals: []
});

console.log('Committed:', result.committed);
```

**Kubernetes Deployment:**

```yaml
apiVersion: v1
kind: Pod
spec:
  containers:
  - name: app
    image: my-app:latest
    env:
    - name: KGC_SIDECAR_ADDRESS
      value: "localhost:50051"
  - name: kgc-sidecar
    image: unrdf/sidecar:latest
    ports:
    - containerPort: 50051
```

### 2. Context Management

New kubectl-style context switching:

```bash
# Create contexts for different environments
unrdf context create dev --sidecar=localhost:50051
unrdf context create prod --sidecar=kgc.example.com:443 --tls

# Switch contexts
unrdf context use prod

# List contexts
unrdf context list
```

**Config File** (`~/.unrdf/config.json`):

```json
{
  "currentContext": "production",
  "contexts": [
    {
      "name": "production",
      "endpoint": {
        "address": "kgc.example.com",
        "port": 443,
        "tls": { "enabled": true }
      }
    }
  ]
}
```

### 3. Hook Lifecycle Functions

Enhanced hook lifecycle with `before`, `run`, `after`:

```javascript
defineHook({
  meta: { name: 'my-hook' },
  when: { /* condition */ },

  // New: Pre-execution gate
  async before({ payload }) {
    if (!isValid(payload)) {
      return { cancel: true, reason: 'Invalid payload' };
    }
    return { ...payload, normalized: true };
  },

  // Main execution
  async run({ payload, context }) {
    return { result: { /* ... */ } };
  },

  // New: Post-execution cleanup
  async after({ result, cancelled }) {
    await auditLog(result);
    return { result: { audited: true } };
  }
});
```

### 4. Security Validation

Automatic security validation for all hooks:

```javascript
import { createSecurityValidator } from 'unrdf/knowledge-engine';

const validator = createSecurityValidator({ strictMode: true });

const validation = validator.validateKnowledgeHook(hook);

if (!validation.valid) {
  console.error('Security issue:', validation.blockReason);
}
```

**Checks Include:**

- SHA256 integrity verification
- SPARQL injection prevention
- File access controls
- Determinism enforcement

### 5. Observability Integration

Built-in OpenTelemetry support:

```javascript
import { initTelemetry } from 'unrdf/sidecar';

await initTelemetry({
  serviceName: 'my-app',
  endpoint: 'http://jaeger:14268/api/traces'
});

// All operations are automatically traced and metered
```

**Metrics Exported:**

- Hook execution time (p50, p95, p99)
- Transaction commit rate
- SPARQL query performance
- Validation failures
- Circuit breaker state

## Migration Checklist

### Pre-Migration

- [ ] Read this guide completely
- [ ] Backup existing codebase
- [ ] Review [CHANGELOG](/docs/CHANGELOG.md)
- [ ] Test in dev environment first

### Code Migration

- [ ] Update `package.json` to v3.0.0
- [ ] Run `pnpm install`
- [ ] Update CLI scripts (noun-verb pattern)
- [ ] Extract inline SPARQL to files
- [ ] Compute SHA256 hashes for files
- [ ] Update hook definitions (use `defineHook`)
- [ ] Replace `createStore` with `initStore`
- [ ] Update `TransactionManager` to `KnowledgeHookManager`
- [ ] Add lifecycle functions (`before`, `after`)

### Testing

- [ ] Run unit tests: `pnpm test`
- [ ] Run integration tests: `pnpm test:e2e`
- [ ] Validate hooks: `unrdf hook test <hook-file>`
- [ ] Check SPARQL queries work
- [ ] Verify SHACL validation
- [ ] Test sidecar connection (if using)

### Deployment

- [ ] Update CI/CD pipelines
- [ ] Deploy sidecar (if using)
- [ ] Configure contexts (dev, staging, prod)
- [ ] Set environment variables
- [ ] Update Kubernetes manifests (if applicable)
- [ ] Monitor observability metrics

### Post-Migration

- [ ] Verify all features work
- [ ] Check performance metrics
- [ ] Monitor error rates
- [ ] Update documentation
- [ ] Train team on new CLI

## Common Migration Issues

### Issue 1: Hook Not Found

**Error:**

```
Error: Knowledge hook "my-hook" not found
```

**Solution:**

Ensure hook is registered with `addKnowledgeHook`:

```javascript
const hook = defineHook({ /* ... */ });
manager.addKnowledgeHook(hook);  // Must call this!
```

### Issue 2: SHA256 Mismatch

**Error:**

```
Error: SHA256 hash mismatch for file://hooks/check.ask.rq
Expected: abc123...
Actual:   def456...
```

**Solution:**

Recompute hash after file changes:

```bash
sha256sum hooks/check.ask.rq
# Update ref.sha256 in hook definition
```

### Issue 3: Context Not Initialized

**Error:**

```
Error: Store context not initialized
```

**Solution:**

Wrap code in `runApp`:

```javascript
const runApp = initStore();
await runApp(() => {
  const graph = useGraph();  // Now works
});
```

### Issue 4: Sidecar Connection Failed

**Error:**

```
ConnectionError: Failed to connect to sidecar at localhost:50051
```

**Solution:**

1. Check sidecar is running: `unrdf sidecar status`
2. Verify address: `echo $KGC_SIDECAR_ADDRESS`
3. Use embedded engine: `unset KGC_SIDECAR_ADDRESS`

### Issue 5: CLI Command Not Found

**Error:**

```
bash: unrdf: command not found
```

**Solution:**

Reinstall globally or use npx:

```bash
pnpm install -g unrdf

# Or use npx
npx unrdf --version
```

## Performance Improvements

v3.0.0 includes several performance optimizations:

| Operation | v2.1.1 | v3.0.0 | Improvement |
|-----------|--------|--------|-------------|
| Hook evaluation (p99) | 3.2ms | 1.8ms | 43% faster |
| Transaction commit | 6.1ms | 4.5ms | 26% faster |
| SPARQL SELECT (10k triples) | 145ms | 98ms | 32% faster |
| SHACL validation | 212ms | 187ms | 12% faster |
| CLI startup | 487ms | 89ms | 82% faster |

## Rollback Plan

If migration fails, rollback to v2.1.1:

```bash
# 1. Revert package.json
git checkout HEAD~1 package.json

# 2. Reinstall dependencies
pnpm install

# 3. Restore code
git checkout HEAD~1 src/

# 4. Restart services
pm2 restart all
```

## Getting Help

**Resources:**

- [Documentation](/docs)
- [API Reference](/docs/api)
- [Examples](/examples)
- [GitHub Issues](https://github.com/unrdf/unrdf/issues)

**Common Questions:**

- **Q: Do I need to use the sidecar?**
  - A: No, sidecar is optional. CLI auto-detects and falls back to embedded mode.

- **Q: Can I keep inline SPARQL?**
  - A: No, content-addressed files are required for security and provenance.

- **Q: Will v2 hooks work in v3?**
  - A: No, hooks must be updated to use `defineHook` with file references.

- **Q: How long does migration take?**
  - A: 30-60 minutes for most projects, depending on number of hooks.

---

**Ready to migrate?** Follow the checklist above and test thoroughly in dev first.

**Need help?** Open an issue on GitHub with the `migration` label.
