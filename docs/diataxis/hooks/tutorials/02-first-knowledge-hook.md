# Tutorial 02: Your First KnowledgeHook

By the end of this tutorial you will have a JSON-defined KnowledgeHook that uses a
`sparql-ask` condition to detect whether any `ex:Person` nodes exist in an RDF store, and a
`sparql-construct` effect that stamps them with a validation timestamp.

## Prerequisites

```bash
pnpm add @unrdf/hooks @unrdf/oxigraph @unrdf/v6-core
```

## Step 1 — Create a store and the engine

`KnowledgeHookEngine` wraps an Oxigraph store and provides the evaluation runtime.

```javascript
// my-first-knowledge-hook.mjs
import { KnowledgeHookEngine, createKnowledgeHook } from '@unrdf/hooks';
import { createStore } from '@unrdf/oxigraph';
import { createContext } from '@unrdf/v6-core/receipt-pattern';

const store = createStore();
const engine = new KnowledgeHookEngine(store);
```

## Step 2 — Define the hook as a JSON object

A KnowledgeHook has three required top-level fields: `name`, `condition`, and `effects`.

```javascript
const personValidationHook = createKnowledgeHook({
  name: 'validate-persons',

  // condition.kind = 'sparql-ask' evaluates a SPARQL ASK query against the store
  condition: {
    kind: 'sparql-ask',
    query: 'ASK { ?s a <http://schema.org/Person> }',
  },

  // effects run when the condition returns true
  effects: [
    {
      kind: 'sparql-construct',
      query: `
        CONSTRUCT {
          ?s <http://example.org/validated> true .
        }
        WHERE {
          ?s a <http://schema.org/Person> .
        }
      `,
    },
  ],

  metadata: {
    author: 'tutorial',
    version: '1.0.0',
  },
});
```

## Step 3 — Evaluate the condition directly

You can test the condition before running full execution:

```javascript
const conditionPassed = await engine.evaluateCondition(personValidationHook.condition);
console.log('Condition passed:', conditionPassed); // false — store is empty
```

## Step 4 — Populate the store and re-evaluate

```javascript
store.add({
  subject: { termType: 'NamedNode', value: 'http://example.org/alice' },
  predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
  object: { termType: 'NamedNode', value: 'http://schema.org/Person' },
  graph: { termType: 'DefaultGraph', value: '' },
});

const passed = await engine.evaluateCondition(personValidationHook.condition);
console.log('Condition passed:', passed); // true
```

## Step 5 — Execute the full hook with receipt chaining

```javascript
const context = createContext({
  nodeId: 'tutorial-app',
  t_ns: BigInt(Date.now() * 1_000_000),
});

const result = await engine.execute(context, [personValidationHook]);

console.log('Successful hooks:', result.successful);
console.log('Failed hooks:', result.failed);
console.log('Receipt hash:', result.receipt.receiptHash);
console.log('Input hash:', result.receipt.input_hash);
console.log('Output hash:', result.receipt.output_hash);
```

The receipt is a BLAKE3 cryptographic fingerprint of the store state before and after
execution. It links to the previous receipt via `previousReceiptHash`, forming a tamper-evident
audit chain.

## Step 6 — Register the hook in the engine

If you want the engine to manage the hook's lifecycle (deduplication, ID tracking):

```javascript
const hookId = engine.registerHook(personValidationHook);
console.log('Registered as:', hookId); // UUID string
```

## What you built

- A KnowledgeHook with a `sparql-ask` condition and a `sparql-construct` effect.
- Direct condition evaluation via `engine.evaluateCondition()`.
- Full execution with `engine.execute()` that returns a receipt.

## Next steps

- How-To: [Chain hooks with a registry](../how-to/02-chain-hooks-with-registry.md) for
  low-level per-quad pipelines.
- How-To: [Use a SHACL condition](../how-to/03-use-shacl-condition.md) for shape-based
  governance.
- Reference: [KnowledgeHook schema](../reference/knowledge-hook-schema.md) for all fields.
