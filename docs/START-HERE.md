# Start Here: The Knowledge Substrate

**One function gives you everything.**

## The Pit of Success

```javascript
import { createKnowledgeSubstrateCore } from 'unrdf';

const core = await createKnowledgeSubstrateCore();

// Parse
const store = core.parseTurtle(`
  @prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .
`);

// Query
const results = core.query(store, `SELECT ?person WHERE { ?person ?p ?o }`);
console.log(results);
```

**That's a production-ready RDF system with:**
- Transactions
- Hooks
- Sandboxing
- Audit trails
- Performance optimization
- Observability

---

## What `createKnowledgeSubstrateCore()` Gives You

| Component | Purpose | Enabled |
|-----------|---------|---------|
| TransactionManager | Atomic operations with rollback | Default |
| KnowledgeHookManager | Autonomous behaviors | Default |
| EffectSandbox | Safe execution environment | Default |
| LockchainWriter | Cryptographic audit trails | Default |
| PerformanceOptimizer | 80/20 optimization | Default |
| Observability | OTEL tracing | Default |
| PolicyPackManager | Governance | Optional |
| ResolutionLayer | Multi-agent consensus | Optional |

**The defaults are the 20% of components that deliver 80% of value.**

---

## The Hierarchy

```
Level 1: createKnowledgeSubstrateCore()  ← START HERE (recommended)
    ↓
Level 2: defineHook() / registerHook()   ← Custom hooks (when needed)
    ↓
Level 3: parseTurtle() / query()         ← Low-level (rare)
```

**Drop down a level only when you need something the higher level doesn't provide.**

---

## When to Use Each Level

### Level 1: Knowledge Substrate (Default)

**Use for:** 80% of applications
**What you get:** Everything, optimized and configured

```javascript
const core = await createKnowledgeSubstrateCore();
```

### Level 2: Knowledge Hooks

**Use for:** Custom autonomous behaviors beyond defaults
**When:** You need hooks that aren't built into the Substrate

```javascript
import { defineHook, registerHook } from 'unrdf';

const customHook = defineHook({
  meta: { name: 'my-validator' },
  before(event) { /* ... */ }
});
registerHook(customHook);
```

### Level 3: Low-level APIs

**Use for:** Edge cases, multi-store scenarios
**When:** You need direct control over individual stores

```javascript
import { parseTurtle, query } from 'unrdf';

const store1 = parseTurtle(data1);
const store2 = parseTurtle(data2);
// Manage stores independently
```

---

## Configuration

```javascript
const core = await createKnowledgeSubstrateCore({
  // Core (enabled by default)
  enableTransactionManager: true,
  enableKnowledgeHookManager: true,
  enableEffectSandbox: true,
  enableLockchainWriter: true,
  enableObservability: true,
  enablePerformanceOptimizer: true,

  // Optional (disabled by default)
  enablePolicyPackManager: false,  // Enable for governance
  enableResolutionLayer: false,    // Enable for multi-agent

  // Performance tuning
  maxConcurrency: 10,
  cacheSize: 10000,
  batchSize: 1000,
  timeoutMs: 2000,
});
```

---

## Accessing Components

```javascript
const core = await createKnowledgeSubstrateCore();

// Get specific components
const txManager = core.getComponent('TransactionManager');
const hookManager = core.getComponent('KnowledgeHookManager');
const lockchain = core.getComponent('LockchainWriter');
const sandbox = core.getComponent('EffectSandbox');

// Use the transaction manager
await txManager.apply(store, {
  additions: [quad],
  removals: []
});

// Use the lockchain
await lockchain.writeReceipt(transaction);
```

---

## The 80/20 Principle

The Knowledge Substrate embodies the 80/20 principle:

- **20% of components** (TransactionManager, KnowledgeHookManager, etc.) deliver **80% of value**
- **80% of components** (PolicyPackManager, ResolutionLayer) deliver **20% of value**

The defaults enable the essential 20%. Enable the optional 80% only when you have specific requirements.

---

## Next Steps

1. **Use `createKnowledgeSubstrateCore()`** for your application
2. **Only drop to Level 2** if you need custom hooks
3. **Only drop to Level 3** if you have multi-store edge cases
4. **Consult [WHICH-FEATURES.md](WHICH-FEATURES.md)** for decision trees
