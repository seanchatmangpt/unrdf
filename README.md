# UNRDF

**RDF knowledge graphs with batteries included.**

```bash
npm install unrdf
```

---

## Quick Start

```javascript
import { createKnowledgeSubstrateCore } from 'unrdf';

// One function gives you everything
const core = await createKnowledgeSubstrateCore();

// Parse RDF
const store = core.parseTurtle(`
  @prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .
`);

// Query
const results = core.query(store, `SELECT ?person WHERE { ?person ?p ?o }`);
console.log(results);
```

**That's it.** `createKnowledgeSubstrateCore()` gives you:
- TransactionManager (atomic operations)
- KnowledgeHookManager (autonomous behaviors)
- EffectSandbox (safe execution)
- LockchainWriter (audit trails)
- Performance optimization
- Observability

---

## The Hierarchy

| Level | Use When | Entry Point |
|-------|----------|-------------|
| **1. Knowledge Substrate** | Default (recommended) | `createKnowledgeSubstrateCore()` |
| **2. Knowledge Hooks** | Need custom autonomous behaviors | `defineHook()`, `registerHook()` |
| **3. Low-level APIs** | Edge cases, multi-store scenarios | `parseTurtle()`, `query()`, etc. |

**Start at Level 1. Drop down only when necessary.**

---

## Level 1: Knowledge Substrate (Recommended)

```javascript
import { createKnowledgeSubstrateCore } from 'unrdf';

const core = await createKnowledgeSubstrateCore({
  // All enabled by default - the 20% that delivers 80% of value
  enableTransactionManager: true,
  enableKnowledgeHookManager: true,
  enableEffectSandbox: true,
  enableLockchainWriter: true,
  enableObservability: true,

  // Optional - enable only if needed
  enablePolicyPackManager: false,
  enableResolutionLayer: false,
});

// Access components
const txManager = core.getComponent('TransactionManager');
const hookManager = core.getComponent('KnowledgeHookManager');
const lockchain = core.getComponent('LockchainWriter');
```

---

## Level 2: Knowledge Hooks (When Needed)

Only if you need custom autonomous behaviors beyond what the Substrate provides:

```javascript
import { defineHook, registerHook } from 'unrdf';

const myHook = defineHook({
  meta: { name: 'custom-validator' },
  before(event) { /* validate */ },
  run(event) { /* execute */ },
  after(result) { /* cleanup */ }
});

registerHook(myHook);
```

---

## Level 3: Low-level APIs (Rare)

Only for edge cases like multi-store scenarios:

```javascript
import { parseTurtle, query, validateShacl } from 'unrdf';

const store1 = parseTurtle(data1);
const store2 = parseTurtle(data2);
const results = query(store1, sparql);
```

---

## Documentation

| Guide | Purpose |
|-------|---------|
| [START-HERE.md](docs/START-HERE.md) | Quick orientation |
| [WHICH-FEATURES.md](docs/WHICH-FEATURES.md) | Decision trees |
| [tutorials/](docs/tutorials/README.md) | Step-by-step learning |
| [how-to/](docs/how-to/README.md) | Task recipes |
| [reference/](docs/reference/README.md) | API documentation |

---

## Examples

```bash
node examples/01-minimal-parse-query.mjs
```

---

## Requirements

- Node.js 18+
- ESM modules

---

## License

MIT
