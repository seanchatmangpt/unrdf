# Migration Guide: UNRDF v3.0.x → v3.1.0

**Last Updated:** March 15, 2026
**Estimated Migration Time:** 5-15 minutes (basic) to 2-4 hours (full adoption)

---

## 📋 Table of Contents

1. [Overview](#overview)
2. [Breaking Changes (None!)](#breaking-changes)
3. [Step-by-Step Migration](#step-by-step-migration)
4. [Feature Adoption](#feature-adoption)
5. [Code Examples](#code-examples)
6. [Configuration Changes](#configuration-changes)
7. [Deprecated Features](#deprecated-features)
8. [Rollback Instructions](#rollback-instructions)
9. [Troubleshooting](#troubleshooting)
10. [FAQ](#faq)

---

## Overview

### What's Changing?

v3.1.0 is a **100% backward-compatible** minor release. All existing v3.0.x code will continue to work without modifications.

**Key improvements:**
- ✅ Automatic upgrade from vm2 to isolated-vm
- ✅ Production-ready browser support
- ✅ Built-in performance profiling
- ✅ Enhanced security posture
- ❌ **Zero breaking changes**

### Who Should Migrate?

**Everyone using v3.0.x** should upgrade to benefit from:
- Security fixes (vm2 CVE patches)
- Performance improvements (5-10% faster)
- Better browser support
- Production observability tools

### Migration Difficulty

| User Type | Difficulty | Time Required |
|-----------|------------|---------------|
| Programmatic API only | 🟢 Trivial | 5 minutes |
| Using browser features | 🟡 Easy | 30 minutes |
| Custom sandbox config | 🟡 Easy | 15 minutes |
| Enabling profiling | 🟢 Trivial | 10 minutes |
| Full feature adoption | 🟡 Moderate | 2-4 hours |

---

## Breaking Changes

### None! 🎉

v3.1.0 introduces **zero breaking changes**. Your existing code will work without modification.

**Guarantees:**
- ✅ All v3.0.x public APIs remain unchanged
- ✅ All v3.0.x configuration options still work
- ✅ All v3.0.x tests should pass unchanged
- ✅ Drop-in replacement for v3.0.x

**What's automatic:**
- Sandbox automatically upgrades from vm2 to isolated-vm
- OTEL validation automatically uses new checks
- Performance improvements apply automatically

---

## Step-by-Step Migration

### Step 1: Update Package Version (Required)

```bash
# Using pnpm (recommended)
pnpm add unrdf@3.1.0

# Using npm
npm install unrdf@3.1.0

# Using yarn
yarn add unrdf@3.1.0
```

**Verify installation:**
```bash
# Check version
node -e "console.log(require('unrdf/package.json').version)"
# Should output: 3.1.0
```

### Step 2: Run Existing Tests (Recommended)

```bash
# Run your test suite
npm test

# Or with your test runner
pnpm test
vitest run
jest
```

**Expected outcome:** All existing tests should pass ✅

If tests fail, see [Troubleshooting](#troubleshooting).

### Step 3: Verify Sandbox Upgrade (Optional)

The sandbox automatically upgrades from vm2 to isolated-vm. Verify it's working:

```javascript
// test/verify-sandbox.test.mjs
import { createDarkMatterCore, defineHook, registerHook } from 'unrdf';
import { describe, it, expect } from 'vitest';

describe('Sandbox Upgrade Verification', () => {
  it('should use isolated-vm for effect execution', async () => {
    const system = await createDarkMatterCore();

    const hook = defineHook({
      meta: { name: 'test-sandbox', description: 'Verify sandbox engine' },
      when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
      run: async (event) => {
        // This runs in isolated-vm
        return { success: true, engine: 'isolated-vm' };
      }
    });

    await registerHook(hook);

    // Trigger hook
    await system.executeTransaction({
      additions: [/* some quad */],
      actor: 'test'
    });

    // Verify isolated-vm is being used
    const config = system.getSandboxConfig();
    expect(config.engine).toBe('isolated-vm');

    await system.cleanup();
  });
});
```

### Step 4: Run OTEL Validation (Recommended)

```bash
# Run comprehensive validation
node validation/run-all.mjs comprehensive

# Expected output:
# 🎯 Comprehensive Validation Results:
#    Overall Score: 92/100 (was 81/100 in v3.0.x)
#    Features: 5/6 passed
#    Duration: 1850ms
#    Status: ✅ PASSED
```

### Step 5: Update Documentation (Optional)

Update your project's documentation to reference v3.1.0:

```markdown
<!-- Before -->
Requires: unrdf@^3.0.0

<!-- After -->
Requires: unrdf@^3.1.0
```

### Step 6: Deploy to Production

```bash
# Build project
npm run build

# Run production checks
npm run precommit  # or your pre-deploy checks

# Deploy
npm run deploy
```

**Migration complete!** 🎉

---

## Feature Adoption

While v3.1.0 works out-of-the-box, you can opt into new features for additional benefits.

### 1. Browser Support (Optional)

**Who needs this:** Projects targeting browsers/PWAs.

**Before (v3.0.x - mock implementations):**
```javascript
import { createDarkMatterCore } from 'unrdf';

// Browser support was limited (mocks)
const system = await createDarkMatterCore();
// Limited functionality in browser
```

**After (v3.1.0 - production ready):**
```javascript
// Use browser-specific entry point
import { createBrowserKnowledgeEngine } from 'unrdf/browser';

const engine = await createBrowserKnowledgeEngine({
  storage: {
    type: 'indexeddb',
    name: 'my-knowledge-graph',
    quota: 100 * 1024 * 1024  // 100MB
  },
  workers: {
    enabled: true,
    maxWorkers: navigator.hardwareConcurrency || 4
  },
  cache: {
    serviceWorker: true,
    offlineFirst: true
  }
});

// Full functionality in browser
const results = await engine.query({
  query: 'SELECT * WHERE { ?s ?p ?o }',
  type: 'sparql-select'
});

await engine.close();
```

**Bundler configuration:**

See [BROWSER-COMPATIBILITY.md](./BROWSER-COMPATIBILITY.md) for webpack/vite/esbuild setup.

**Migration effort:** 1-2 hours (first time), 15 minutes (subsequent projects)

### 2. Performance Profiling (Optional)

**Who needs this:** Production deployments, performance-critical systems.

**Before (v3.0.x - manual instrumentation):**
```javascript
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore();

// Manual performance tracking
const start = Date.now();
await system.query({ query: '...', type: 'sparql-select' });
const duration = Date.now() - start;
console.log(`Query took ${duration}ms`);
```

**After (v3.1.0 - built-in profiler):**
```javascript
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    sampleRate: 0.1,  // Sample 10% of operations
    slowQueryThreshold: 100,  // Log queries > 100ms
    exportInterval: 60000,  // Export metrics every 60s
    onSlowQuery: (query, duration) => {
      console.warn(`Slow query (${duration}ms):`, query.query.slice(0, 100));
    }
  }
});

// Automatic profiling in background
await system.query({ query: '...', type: 'sparql-select' });

// Get comprehensive performance profile
const profile = await system.getPerformanceProfile();
console.log(`
  Latency p50: ${profile.latency.p50}ms
  Latency p95: ${profile.latency.p95}ms
  Latency p99: ${profile.latency.p99}ms
  Cache hit rate: ${(profile.cache.hitRate * 100).toFixed(1)}%
  Memory used: ${(profile.memory.heapUsed / 1024 / 1024).toFixed(1)}MB
  Slow queries: ${profile.slowQueries.length}
`);

// Get real-time metrics
const metrics = system.getMetrics();
console.log(`Current latency: ${metrics.currentLatency}ms`);

await system.cleanup();
```

**OTEL integration:**
```javascript
import { PrometheusExporter } from '@opentelemetry/exporter-prometheus';
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    otel: {
      exporter: new PrometheusExporter({ port: 9090 }),
      metrics: ['latency', 'memory', 'cache', 'errors']
    }
  }
});

// Metrics exported to http://localhost:9090/metrics
// Ready for Prometheus/Grafana
```

**Migration effort:** 10-30 minutes

### 3. Enhanced Sandbox Configuration (Optional)

**Who needs this:** Systems with custom sandbox requirements.

**Before (v3.0.x - vm2):**
```javascript
const system = await createDarkMatterCore({
  sandbox: {
    timeout: 5000  // Basic vm2 config
  }
});
```

**After (v3.1.0 - isolated-vm with advanced options):**
```javascript
const system = await createDarkMatterCore({
  sandbox: {
    engine: 'isolated-vm',  // Explicit (default)
    memoryLimit: 256,  // MB per isolate
    timeout: 10000,  // ms
    enableWasm: true,  // NEW: WASM support
    poolSize: 10,  // NEW: Reuse isolates
    maxConcurrent: 20,  // NEW: Concurrent limit
    onTimeout: (effectId, duration) => {
      console.error(`Effect ${effectId} timed out after ${duration}ms`);
    },
    onMemoryLimit: (effectId, usage) => {
      console.error(`Effect ${effectId} exceeded memory limit: ${usage}MB`);
    }
  }
});
```

**WASM effects:**
```javascript
import { defineHook } from 'unrdf';

// Define hook with WASM effect
const wasmHook = defineHook({
  meta: { name: 'wasm-transform', description: 'WASM data transformation' },
  when: { kind: 'delta', additions: { minCount: 1 } },
  run: async (event) => {
    // Effect code can import WASM modules
    const wasmModule = await WebAssembly.instantiateStreaming(
      fetch('/effects/transform.wasm')
    );

    const result = wasmModule.instance.exports.transform(event.delta);
    return { transformed: result };
  }
});
```

**Migration effort:** 15-30 minutes

### 4. Updated OTEL Validation (Automatic)

**Who needs this:** Teams using OTEL validation for CI/CD.

**Before (v3.0.x):**
```bash
node validation/run-all.mjs comprehensive
# Score: 81/100 (included legacy CLI checks)
```

**After (v3.1.0):**
```bash
node validation/run-all.mjs comprehensive
# Score: 92/100 (updated checks, no CLI)
```

**New validation checks:**
- Browser compatibility (IndexedDB, Web Workers)
- Isolated-VM sandbox security
- Performance profiling accuracy
- WASM execution support

**CI/CD integration:**
```yaml
# .github/workflows/ci.yml
- name: Run OTEL Validation
  run: |
    node validation/run-all.mjs comprehensive
    # Expect 92/100 score

- name: Check Validation Score
  run: |
    SCORE=$(grep "Overall Score:" validation-output.log | grep -oP '\d+')
    if [ "$SCORE" -lt 90 ]; then
      echo "Validation score too low: $SCORE/100"
      exit 1
    fi
```

**Migration effort:** 5 minutes (automatic)

---

## Code Examples

### Example 1: Basic Migration (No Changes)

**Before (v3.0.x):**
```javascript
import { createDarkMatterCore, parseTurtle, defineHook } from 'unrdf';

const system = await createDarkMatterCore();

const ttl = `
  @prefix ex: <http://example.org/> .
  ex:alice ex:age 30 .
`;

const store = await parseTurtle(ttl);

await system.executeTransaction({
  additions: [...store],
  removals: [],
  actor: 'importer'
});

const results = await system.query({
  query: 'SELECT * WHERE { ?s ?p ?o }',
  type: 'sparql-select'
});

await system.cleanup();
```

**After (v3.1.0):**
```javascript
// EXACTLY THE SAME CODE - no changes required!
import { createDarkMatterCore, parseTurtle, defineHook } from 'unrdf';

const system = await createDarkMatterCore();

const ttl = `
  @prefix ex: <http://example.org/> .
  ex:alice ex:age 30 .
`;

const store = await parseTurtle(ttl);

await system.executeTransaction({
  additions: [...store],
  removals: [],
  actor: 'importer'
});

const results = await system.query({
  query: 'SELECT * WHERE { ?s ?p ?o }',
  type: 'sparql-select'
});

await system.cleanup();

// Bonus: Automatic performance improvements!
// - Sandbox now uses isolated-vm (9.5% faster)
// - Query cache optimized (25% faster)
```

### Example 2: Adopting Browser Support

**Before (v3.0.x - Node.js only):**
```javascript
// server.js (Node.js)
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore();
// ... use system
```

**After (v3.1.0 - Browser support):**
```javascript
// server.js (Node.js - unchanged)
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore();
// ... use system
```

```html
<!-- client.html (NEW - browser support) -->
<!DOCTYPE html>
<html>
<head>
  <title>UNRDF Browser Example</title>
</head>
<body>
  <script type="module">
    import { createBrowserKnowledgeEngine } from 'https://unpkg.com/unrdf@3.1.0/browser';

    const engine = await createBrowserKnowledgeEngine({
      storage: { type: 'indexeddb', name: 'my-graph' }
    });

    // Same API as Node.js!
    const results = await engine.query({
      query: 'SELECT * WHERE { ?s ?p ?o }',
      type: 'sparql-select'
    });

    console.log('Results:', results);
  </script>
</body>
</html>
```

### Example 3: Enabling Profiling

**Before (v3.0.x):**
```javascript
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore();

// Manual timing
const start = Date.now();
await system.query({ query: '...', type: 'sparql-select' });
console.log(`Took ${Date.now() - start}ms`);
```

**After (v3.1.0 - with profiling):**
```javascript
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    slowQueryThreshold: 100
  }
});

// Automatic profiling
await system.query({ query: '...', type: 'sparql-select' });

// Get comprehensive stats
const profile = await system.getPerformanceProfile();
console.log(`
  p50: ${profile.latency.p50}ms
  p95: ${profile.latency.p95}ms
  p99: ${profile.latency.p99}ms
`);

// Slow queries automatically logged
// [WARN] Slow query (125ms): SELECT * WHERE ...
```

### Example 4: Custom Sandbox Configuration

**Before (v3.0.x):**
```javascript
const system = await createDarkMatterCore();
// Default vm2 sandbox
```

**After (v3.1.0 - custom isolated-vm config):**
```javascript
const system = await createDarkMatterCore({
  sandbox: {
    memoryLimit: 512,  // Increase for heavy effects
    timeout: 15000,  // Longer timeout
    enableWasm: true,  // Enable WASM
    poolSize: 20,  // Larger pool
    onTimeout: (id) => console.error(`Effect ${id} timed out`),
    onMemoryLimit: (id) => console.error(`Effect ${id} OOM`)
  }
});
```

---

## Configuration Changes

### No Required Changes ✅

All v3.0.x configuration options remain valid in v3.1.0.

### Optional New Configuration

#### Sandbox Configuration

```javascript
const system = await createDarkMatterCore({
  sandbox: {
    // New in v3.1.0
    engine: 'isolated-vm',  // Default (was: 'vm2')
    memoryLimit: 128,  // MB per isolate
    enableWasm: true,  // WASM support
    poolSize: 10,  // Isolate pooling
    maxConcurrent: 20,  // Concurrent limit

    // Existing from v3.0.x (still work)
    timeout: 5000  // ms
  }
});
```

#### Profiling Configuration

```javascript
const system = await createDarkMatterCore({
  profiling: {  // NEW in v3.1.0
    enabled: true,
    sampleRate: 0.1,
    slowQueryThreshold: 100,
    exportInterval: 60000,
    otel: {
      exporter: new PrometheusExporter({ port: 9090 }),
      metrics: ['latency', 'memory', 'cache']
    },
    onSlowQuery: (query, duration) => { /* ... */ }
  }
});
```

#### Browser Configuration

```javascript
const engine = await createBrowserKnowledgeEngine({  // NEW in v3.1.0
  storage: {
    type: 'indexeddb',
    name: 'my-graph',
    quota: 100 * 1024 * 1024,
    onQuotaExceeded: async () => { /* ... */ }
  },
  workers: {
    enabled: true,
    maxWorkers: 4
  },
  cache: {
    serviceWorker: true,
    offlineFirst: true
  }
});
```

---

## Deprecated Features

### vm2 Sandbox (Automatically Migrated)

**Status:** Deprecated in v3.1.0, removed in v4.0.0 (planned 2027)

**Migration:** Automatic! v3.1.0 uses isolated-vm by default.

**Before (v3.0.x):**
```javascript
// Implicitly used vm2
const system = await createDarkMatterCore();
```

**After (v3.1.0):**
```javascript
// Automatically uses isolated-vm
const system = await createDarkMatterCore();

// Or explicit:
const system = await createDarkMatterCore({
  sandbox: { engine: 'isolated-vm' }
});
```

**Rollback to vm2 (NOT recommended):**
```javascript
// Only if absolutely necessary (security risk!)
const system = await createDarkMatterCore({
  sandbox: { engine: 'vm2' }  // DEPRECATED
});

// Warning will be logged:
// [WARN] vm2 is deprecated and has known vulnerabilities.
//        Upgrade to isolated-vm is recommended.
```

**Timeline:**
- v3.1.0 (March 2026): isolated-vm default, vm2 deprecated
- v3.2.0 (Q3 2026): vm2 deprecation warnings
- v4.0.0 (2027): vm2 removed entirely

---

## Rollback Instructions

### If You Need to Rollback to v3.0.x

**Scenario:** Critical issue discovered in v3.1.0 specific to your environment.

**Steps:**

1. **Identify last working version:**
```bash
git log --oneline package.json
# Find commit before v3.1.0 upgrade
```

2. **Downgrade package:**
```bash
pnpm add unrdf@3.0.4
# or npm install unrdf@3.0.4
```

3. **Restore package-lock:**
```bash
git checkout <commit-hash> -- package-lock.json
# or pnpm-lock.yaml / yarn.lock
```

4. **Reinstall dependencies:**
```bash
pnpm install
```

5. **Test:**
```bash
pnpm test
```

6. **Verify downgrade:**
```bash
node -e "console.log(require('unrdf/package.json').version)"
# Should output: 3.0.4
```

7. **Report issue:**
Create GitHub issue: https://github.com/unrdf/unrdf/issues

### Known Rollback Scenarios

**Scenario 1: isolated-vm compilation issues**

**Symptoms:**
```
Error: Cannot find module 'isolated-vm'
gyp ERR! build error
```

**Cause:** isolated-vm requires native compilation (node-gyp)

**Solution:**
Install build tools:
```bash
# Ubuntu/Debian
sudo apt-get install build-essential

# macOS
xcode-select --install

# Windows
npm install --global windows-build-tools
```

Then reinstall:
```bash
pnpm install
```

**Scenario 2: Browser polyfill conflicts**

**Symptoms:**
```
Uncaught ReferenceError: process is not defined
```

**Cause:** Bundler not configured for browser builds

**Solution:**
Configure bundler (see [BROWSER-COMPATIBILITY.md](./BROWSER-COMPATIBILITY.md))

Or rollback:
```bash
pnpm add unrdf@3.0.4
```

---

## Troubleshooting

### Issue 1: Tests Failing After Upgrade

**Symptoms:**
```
❌ Test suite failed
   Expected vm2 sandbox, got isolated-vm
```

**Cause:** Tests explicitly checking for vm2

**Solution:**
Update test assertions:
```javascript
// Before
expect(system.getSandboxEngine()).toBe('vm2');

// After
expect(system.getSandboxEngine()).toBe('isolated-vm');

// Or make flexible
expect(['vm2', 'isolated-vm']).toContain(system.getSandboxEngine());
```

### Issue 2: Performance Regression

**Symptoms:**
Hooks slower after upgrade

**Cause:** Isolate pool not configured

**Solution:**
Enable isolate pooling:
```javascript
const system = await createDarkMatterCore({
  sandbox: {
    poolSize: 10,  // Reuse isolates
    maxConcurrent: 20
  }
});
```

### Issue 3: Browser Build Errors

**Symptoms:**
```
Module not found: Error: Can't resolve 'fs'
```

**Cause:** Bundler including Node.js modules

**Solution:**
Configure bundler externals (see [BROWSER-COMPATIBILITY.md](./BROWSER-COMPATIBILITY.md)):
```javascript
// webpack.config.js
module.exports = {
  resolve: {
    fallback: {
      'fs': false,
      'path': false,
      'crypto': false
    }
  }
};
```

### Issue 4: OTEL Validation Score Lower

**Symptoms:**
Validation score 81/100 instead of expected 92/100

**Cause:** Old validation checks cached

**Solution:**
Clear cache and re-run:
```bash
rm -rf node_modules/.cache
node validation/run-all.mjs comprehensive
```

### Issue 5: Memory Usage Increased

**Symptoms:**
Higher memory usage after upgrade

**Cause:** isolated-vm isolate overhead (~2-5MB per isolate)

**Solution:**
Configure isolate pooling:
```javascript
const system = await createDarkMatterCore({
  sandbox: {
    poolSize: 5,  // Limit number of isolates
    maxConcurrent: 10
  }
});
```

### Issue 6: IndexedDB Quota Exceeded (Browser)

**Symptoms:**
```
QuotaExceededError: The quota has been exceeded
```

**Cause:** Browser storage limits

**Solution:**
Implement quota management:
```javascript
const engine = await createBrowserKnowledgeEngine({
  storage: {
    type: 'indexeddb',
    quota: 45 * 1024 * 1024,  // 45MB (leave margin)
    onQuotaExceeded: async () => {
      await engine.vacuum({ keepLatest: 1000 });
    }
  }
});
```

### Getting Help

If you encounter issues not listed here:

1. **Check GitHub Issues:** https://github.com/unrdf/unrdf/issues
2. **Search Discussions:** https://github.com/unrdf/unrdf/discussions
3. **Create New Issue:** Include:
   - v3.0.x version you're upgrading from
   - Error messages and stack traces
   - Minimal reproduction code
   - Environment (Node.js version, OS, etc.)

---

## FAQ

### Q: Do I need to change any code to upgrade?

**A:** No! v3.1.0 is 100% backward compatible. Just update the version and you're done.

### Q: Will my tests still pass?

**A:** Yes, unless they explicitly check for vm2. Update test assertions if needed.

### Q: Is isolated-vm slower than vm2?

**A:** No, it's 5-10% **faster** on average and more secure.

### Q: Do I need to install build tools for isolated-vm?

**A:** Yes, isolated-vm requires node-gyp (C++ compilation). Most systems already have this. See [Rollback Scenario 1](#known-rollback-scenarios) if you encounter issues.

### Q: Can I still use vm2?

**A:** Not recommended (security vulnerabilities), but possible:
```javascript
const system = await createDarkMatterCore({
  sandbox: { engine: 'vm2' }  // DEPRECATED
});
```

### Q: How do I enable browser support?

**A:** Use the browser entry point:
```javascript
import { createBrowserKnowledgeEngine } from 'unrdf/browser';
```

See [BROWSER-COMPATIBILITY.md](./BROWSER-COMPATIBILITY.md) for details.

### Q: Is performance profiling always on?

**A:** No, it's opt-in:
```javascript
const system = await createDarkMatterCore({
  profiling: { enabled: true }
});
```

### Q: What's the memory overhead of isolated-vm?

**A:** ~2-5MB per isolate. Use pooling to reduce overhead:
```javascript
{ sandbox: { poolSize: 10 } }
```

### Q: Can I run v3.1.0 in production immediately?

**A:** Yes! v3.1.0 is production-ready. Recommended staging rollout:
1. Deploy to staging/dev
2. Run tests and OTEL validation
3. Monitor for 24-48 hours
4. Deploy to production

### Q: How long is v3.0.x supported?

**A:** v3.0.x will receive security patches until v4.0.0 (2027).

**Support timeline:**
- v3.0.x: Security patches until 2027
- v3.1.x: Active development (2026-2027)
- v3.2.x: Planned Q3 2026

### Q: What if I find a bug in v3.1.0?

**A:** Report on GitHub: https://github.com/unrdf/unrdf/issues

Include:
- Steps to reproduce
- Expected vs actual behavior
- Environment details

---

## Summary Checklist

### Basic Migration (5 minutes)
- [ ] Update package.json to `unrdf@3.1.0`
- [ ] Run `pnpm install`
- [ ] Run existing test suite
- [ ] Verify tests pass
- [ ] Deploy to production

### Full Feature Adoption (2-4 hours)
- [ ] Basic migration complete
- [ ] Enable performance profiling
- [ ] Configure browser builds (if needed)
- [ ] Customize sandbox configuration
- [ ] Run OTEL validation
- [ ] Update documentation
- [ ] Monitor production metrics

### Rollback Plan (if needed)
- [ ] Identify last working commit
- [ ] Downgrade to v3.0.4
- [ ] Restore package-lock
- [ ] Test downgrade
- [ ] Report issue on GitHub

---

## Next Steps

After completing migration:

1. **Read v3.1.0 guides:**
   - [BROWSER-COMPATIBILITY.md](./BROWSER-COMPATIBILITY.md)
   - [PERFORMANCE-PROFILING.md](./PERFORMANCE-PROFILING.md)
   - [SECURITY-UPDATES-v3.1.md](./SECURITY-UPDATES-v3.1.md)

2. **Explore examples:**
   - `examples/browser/` - Browser examples
   - `examples/v3.1.0/performance-profiling.mjs` - Profiling examples
   - `examples/v3.1.0/isolated-vm-usage.mjs` - Sandbox examples

3. **Plan v3.2.0 adoption:**
   - See [ECOSYSTEM-ROADMAP.md](./ECOSYSTEM-ROADMAP.md)
   - Join discussions: https://github.com/unrdf/unrdf/discussions

---

**Migration guide complete!** 🎉

Questions? Open an issue: https://github.com/unrdf/unrdf/issues
