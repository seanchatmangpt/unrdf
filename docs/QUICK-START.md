# UNRDF Quick Start Guide

**Goal: Working RDF knowledge graph in 5 minutes.**

## 🚀 Installation (30 seconds)

```bash
# Node.js (recommended: pnpm)
pnpm add @unrdf/core @unrdf/oxigraph

# npm
npm install @unrdf/core @unrdf/oxigraph
```

**Requirements:** Node.js ≥18.0.0, ES Modules enabled

---

## 📦 Hello World Examples (One per package)

### 1. Core: Store & Query RDF Data

```javascript
import { createStore, executeSelectSync } from '@unrdf/core';
import { createStore as createOxiStore } from '@unrdf/oxigraph';

// Create in-memory triple store
const store = createOxiStore();

// Add triples (Turtle format)
store.load(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:Alice foaf:name "Alice" ;
           foaf:knows ex:Bob .
  ex:Bob foaf:name "Bob" .
`);

// Query with SPARQL
const results = executeSelectSync(store, `
  SELECT ?name WHERE {
    ?person <http://xmlns.com/foaf/0.1/name> ?name .
  }
`);

// Access results
for (const row of results) {
  console.log(row.get('name').value);
}
// Output: Alice, Bob
```

**What just happened?**
- ✅ Created an RDF triple store (Oxigraph backend)
- ✅ Loaded Turtle data (standard RDF format)
- ✅ Executed SPARQL query synchronously
- ✅ Iterated results

---

### 2. KGC-4D: Time-Travel Event Sourcing

```javascript
import { KGCStore, freezeUniverse, reconstructState } from '@unrdf/kgc-4d';

// Initialize 4D event store
const kgc = new KGCStore();

// Record events with nanosecond precision
await kgc.record({
  type: 'ORDER_CREATED',
  subject: 'order:12345',
  data: { amount: 99.99, customer: 'Alice' }
});

await kgc.record({
  type: 'ORDER_PAID',
  subject: 'order:12345',
  data: { method: 'credit_card' }
});

// Freeze universe snapshot (Git-backed)
const snapshot = await freezeUniverse(kgc, {
  message: 'End of day snapshot',
  tag: 'EOD-2024-12-25'
});

// Time-travel: reconstruct state at any point
const stateAtSnapshot = await reconstructState(snapshot.commit);
console.log(stateAtSnapshot.events.length); // All events up to snapshot
```

**What just happened?**
- ✅ Event sourcing with nanosecond timestamps
- ✅ Immutable Git-backed snapshots
- ✅ Time-travel state reconstruction
- ✅ Audit trail with cryptographic receipts

---

### 3. Hooks: Reactive Knowledge Behaviors

```javascript
import { defineHook, registerHook, executeHook } from '@unrdf/hooks';
import { namedNode, literal, quad } from '@unrdf/core';

// Define validation hook (runs on INSERT)
const validateEmail = defineHook({
  meta: { name: 'validate-email-format' },
  trigger: 'INSERT',
  pattern: '?person <http://xmlns.com/foaf/0.1/mbox> ?email .',

  validate(context) {
    const email = context.quad.object.value;
    if (!email.includes('@')) {
      return { passed: false, error: 'Invalid email format' };
    }
    return { passed: true };
  }
});

// Register hook
registerHook(validateEmail);

// Test: valid email (hook passes)
const validQuad = quad(
  namedNode('http://example.org/Alice'),
  namedNode('http://xmlns.com/foaf/0.1/mbox'),
  literal('alice@example.com')
);
const result1 = await executeHook(validateEmail, { quad: validQuad, trigger: 'INSERT' });
console.log(result1.passed); // true

// Test: invalid email (hook vetoes)
const invalidQuad = quad(
  namedNode('http://example.org/Bob'),
  namedNode('http://xmlns.com/foaf/0.1/mbox'),
  literal('bob-invalid-email')
);
const result2 = await executeHook(validateEmail, { quad: invalidQuad, trigger: 'INSERT' });
console.log(result2.passed); // false
console.log(result2.error);  // "Invalid email format"
```

**What just happened?**
- ✅ Defined reactive validation hook
- ✅ Pattern-based trigger (SPARQL-like)
- ✅ Automatic veto on invalid data
- ✅ Composable behaviors (can chain hooks)

---

### 4. YAWL: Workflow Engine

```javascript
import { createWorkflow, createCase, enableTask, startTask, completeTask } from '@unrdf/yawl';

// Define workflow (Van der Aalst patterns)
const approvalWorkflow = createWorkflow({
  id: 'expense-approval',
  name: 'Expense Approval Process',
  tasks: [
    { id: 'submit', name: 'Submit Expense', kind: 'atomic' },
    { id: 'approve', name: 'Manager Approval', kind: 'atomic' },
    { id: 'pay', name: 'Process Payment', kind: 'atomic' }
  ],
  flows: [
    { from: 'submit', to: 'approve' },
    { from: 'approve', to: 'pay' }
  ]
});

// Create workflow case instance
const workCase = await createCase(approvalWorkflow, {
  caseId: 'EXP-001',
  data: { amount: 250.00, employee: 'Alice' }
});

// Enable first task
await enableTask(workCase, 'submit');

// Start and complete task
await startTask(workCase, 'submit', { submittedBy: 'Alice' });
await completeTask(workCase, 'submit', { approved: true });

// Check case status
console.log(workCase.status); // 'active'
console.log(workCase.completedTasks); // ['submit']
```

**What just happened?**
- ✅ Defined workflow with tasks and flows
- ✅ Created case instance with data
- ✅ Executed task lifecycle (enable → start → complete)
- ✅ Automatic state management with YAWL semantics

---

## 🔥 Common Use Cases (80/20)

### Use Case 1: Knowledge Graph Query

```javascript
import { createStore, executeSelectSync } from '@unrdf/core';
import { createStore as createOxiStore } from '@unrdf/oxigraph';

const store = createOxiStore();
store.load(`
  @prefix ex: <http://example.org/> .
  ex:Alice ex:skill "JavaScript" , "Python" .
  ex:Bob ex:skill "JavaScript" , "Rust" .
`);

// Find experts with JavaScript skill
const jsExperts = executeSelectSync(store, `
  SELECT ?person WHERE {
    ?person <http://example.org/skill> "JavaScript" .
  }
`);

console.log([...jsExperts].map(r => r.get('person').value));
// ["http://example.org/Alice", "http://example.org/Bob"]
```

---

### Use Case 2: Event Audit Trail

```javascript
import { KGCStore } from '@unrdf/kgc-4d';

const kgc = new KGCStore();

// Record business events
await kgc.record({ type: 'USER_CREATED', subject: 'user:001', data: { email: 'alice@example.com' } });
await kgc.record({ type: 'USER_UPDATED', subject: 'user:001', data: { role: 'admin' } });
await kgc.record({ type: 'USER_DELETED', subject: 'user:001', data: {} });

// Query audit trail
const userEvents = kgc.queryEvents({ subject: 'user:001' });
console.log(userEvents.map(e => e.type));
// ["USER_CREATED", "USER_UPDATED", "USER_DELETED"]
```

---

### Use Case 3: Policy Enforcement

```javascript
import { defineHook, registerHook, createHookRegistry } from '@unrdf/hooks';

// Policy: No one under 18 can have admin role
const agePolicy = defineHook({
  meta: { name: 'age-restriction-admin' },
  trigger: 'INSERT',
  pattern: '?person <http://example.org/role> "admin" .',

  async validate(context) {
    const person = context.quad.subject;
    const age = await queryAge(person); // Your query logic

    if (age < 18) {
      return { passed: false, error: 'Must be 18+ for admin role' };
    }
    return { passed: true };
  }
});

registerHook(agePolicy);
```

---

### Use Case 4: Multi-Step Workflow

```javascript
import { createWorkflow, createCase, enableTask, completeTask } from '@unrdf/yawl';

// Order fulfillment workflow
const orderFlow = createWorkflow({
  id: 'order-fulfillment',
  tasks: [
    { id: 'validate', name: 'Validate Order' },
    { id: 'pay', name: 'Process Payment' },
    { id: 'ship', name: 'Ship Product' }
  ],
  flows: [
    { from: 'validate', to: 'pay' },
    { from: 'pay', to: 'ship' }
  ]
});

const orderCase = await createCase(orderFlow, { orderId: 'ORD-123' });
await enableTask(orderCase, 'validate');
await completeTask(orderCase, 'validate', { valid: true });

// Payment task auto-enabled by control flow
await completeTask(orderCase, 'pay', { transactionId: 'TXN-456' });
```

---

### Use Case 5: Federation (Query Multiple Stores)

```javascript
import { createStore } from '@unrdf/oxigraph';
import { executeFederatedQuery } from '@unrdf/federation';
import { createCoordinator } from '@unrdf/federation';

const store1 = createStore(); // Local data
const store2 = createStore(); // Remote data

// Create federation coordinator
const coordinator = createCoordinator({
  stores: [store1, store2]
});

// Query across both stores
const results = await executeFederatedQuery(coordinator, `
  SELECT ?name ?skill WHERE {
    ?person foaf:name ?name ;
            ex:skill ?skill .
  }
`);
```

---

## ⚠️ Troubleshooting (Top 5 Issues)

### Issue 1: "Module not found" Error

**Problem:**
```javascript
import { createStore } from 'unrdf'; // ❌ WRONG
```

**Solution:**
```javascript
import { createStore } from '@unrdf/oxigraph'; // ✅ CORRECT
```

UNRDF uses scoped packages (`@unrdf/*`), not a single package.

---

### Issue 2: SPARQL Query Returns Empty

**Problem:** Query returns no results, but data exists.

**Debug Steps:**
```javascript
// 1. Verify data loaded
console.log(store.size); // Should be > 0

// 2. Check exact URIs (case-sensitive!)
const results = executeSelectSync(store, `
  SELECT * WHERE { ?s ?p ?o } LIMIT 10
`);
console.log([...results]); // Inspect actual triples

// 3. Verify prefixes match
// If data uses http://example.org/, query must too
```

**Common mistake:** Prefix mismatches
```javascript
// Data uses: <http://example.org/Alice>
// Query uses: <http://example.com/Alice> ❌
```

---

### Issue 3: Hook Not Executing

**Problem:** Defined hook doesn't run.

**Solution:**
```javascript
import { registerHook } from '@unrdf/hooks';

// ❌ WRONG: Just defining doesn't activate it
const myHook = defineHook({ /* ... */ });

// ✅ CORRECT: Must register
registerHook(myHook);
```

---

### Issue 4: Async/Await Confusion

**Problem:** Using sync APIs with `await` or vice versa.

**Solution:**
```javascript
// ✅ Sync API (no await)
const results = executeSelectSync(store, sparql);

// ✅ Async API (with await)
const results = await executeSelect(store, sparql);
```

Check API docs for which variant to use (prefer sync for better performance).

---

### Issue 5: Import Path Errors

**Problem:** Can't import specific submodules.

**Solution:**
```javascript
// ✅ Main exports
import { createStore } from '@unrdf/core';

// ✅ Subpath exports
import { createStore as createOxiStore } from '@unrdf/oxigraph';

// ❌ WRONG: Internal paths
import { Store } from '@unrdf/core/src/rdf/store.mjs'; // Don't do this
```

Use documented exports only (see API Reference).

---

## 🎯 Next Steps

| Learn | Link |
|-------|------|
| **Full API Reference** | [API-REFERENCE.md](API-REFERENCE.md) |
| **Migration from N3** | [MIGRATION.md](MIGRATION.md) |
| **Architecture** | [ARCHITECTURE.md](ARCHITECTURE.md) |
| **Examples** | [examples/](../examples/) |
| **ADRs (Why Oxigraph, YAWL, etc.)** | [adr/](adr/) |

---

## 💡 Quick Tips

1. **Start with `@unrdf/core`** - It's the foundation
2. **Use Oxigraph for persistence** - Much faster than N3
3. **Prefer sync APIs** - Lower overhead (`executeSelectSync` vs `executeSelect`)
4. **Read ADRs** - Understand architectural decisions
5. **Check examples/** - Copy-paste working code

---

**Ready for advanced features?** → See [API-REFERENCE.md](API-REFERENCE.md) for the top 20% of APIs used 80% of the time.
