# How To: Use a SHACL Condition in a KnowledgeHook

A `shacl` condition validates the RDF store against a SHACL shapes file. It supports three
enforcement modes that control how violations are handled.

## Prerequisites

A SHACL shapes file accessible via a URI (file, http, or data URL). For this guide we assume
a local file `/shapes/person.ttl`.

## Enforcement modes

| Mode              | Behaviour                                                             | Returns                   |
| ----------------- | --------------------------------------------------------------------- | ------------------------- |
| `block` (default) | Halt if any SHACL violation exists                                    | `false` on violation      |
| `annotate`        | Allow the operation; write `sh:ValidationResult` triples to the store | `true` always             |
| `repair`          | Run `repairConstruct` SPARQL query, then re-validate                  | `true` if repair succeeds |

## Block mode — strictest

Stop execution if the store violates the shapes.

```javascript
import { createKnowledgeHook, KnowledgeHookEngine } from '@unrdf/hooks';
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
const engine = new KnowledgeHookEngine(store);

const hook = createKnowledgeHook({
  name: 'enforce-person-shapes',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///shapes/person.ttl' },
    enforcementMode: 'block',
  },
  effects: [],
});

const passed = await engine.evaluateCondition(hook.condition);
// false if any sh:ValidationResult violations found; true if fully conformant
```

You may optionally supply a `sha256` hash in `ref` to verify the shapes file has not changed:

```javascript
ref: {
  uri: 'file:///shapes/person.ttl',
  sha256: 'a3f1...64hex...chars',  // 64 lowercase hex chars
}
```

## Annotate mode — soft governance

Write violation triples to the store without blocking. Useful for audit logging and dashboards.

```javascript
const hook = createKnowledgeHook({
  name: 'log-person-violations',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///shapes/person.ttl' },
    enforcementMode: 'annotate',
  },
  effects: [
    {
      kind: 'sparql-construct',
      query: `
        CONSTRUCT { ?s <http://example.org/checkedAt> ?now }
        WHERE { ?s a <http://schema.org/Person> . BIND(NOW() AS ?now) }
      `,
    },
  ],
});
```

In `annotate` mode `evaluateCondition` always returns `true`. Violation details are written
as `sh:ValidationResult` triples into the store.

## Repair mode — automatic fixing

Run a SPARQL CONSTRUCT to fix violations, then re-validate. `repairConstruct` is required
when `enforcementMode` is `'repair'`.

```javascript
const hook = createKnowledgeHook({
  name: 'repair-missing-risk-score',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///shapes/trade.ttl' },
    enforcementMode: 'repair',
    repairConstruct: `
      CONSTRUCT {
        ?trade <http://example.org/riskScore> 50 .
      }
      WHERE {
        ?trade a <http://example.org/Trade> .
        FILTER NOT EXISTS { ?trade <http://example.org/riskScore> ?existing }
      }
    `,
  },
  effects: [],
});

const context = createContext({
  nodeId: 'risk-system',
  t_ns: BigInt(Date.now() * 1_000_000),
});

const result = await engine.execute(context, [hook]);
// result.successful lists hooks whose repair + re-validation passed
// result.failed lists hooks where repair did not resolve all violations
```

## Full example with all three modes

```javascript
import { createKnowledgeHook, KnowledgeHookEngine } from '@unrdf/hooks';
import { createStore } from '@unrdf/oxigraph';
import { createContext } from '@unrdf/v6-core/receipt-pattern';

const store = createStore();
const engine = new KnowledgeHookEngine(store);

const hooks = [
  // Strict: block on any violation
  createKnowledgeHook({
    name: 'block-on-schema-violation',
    condition: {
      kind: 'shacl',
      ref: { uri: 'file:///shapes/schema.ttl' },
      enforcementMode: 'block',
    },
    effects: [],
  }),

  // Soft: log but allow
  createKnowledgeHook({
    name: 'annotate-optional-fields',
    condition: {
      kind: 'shacl',
      ref: { uri: 'file:///shapes/optional.ttl' },
      enforcementMode: 'annotate',
    },
    effects: [],
  }),
];

const ctx = createContext({ nodeId: 'example', t_ns: BigInt(Date.now() * 1_000_000) });
const result = await engine.execute(ctx, hooks);
console.log('Receipt:', result.receipt.receiptHash);
```

## See also

- [Reference: KnowledgeHook schema — ShaclCondition](../reference/knowledge-hook-schema.md)
- [Explanation: Two hook systems](../explanation/01-two-hook-systems.md)
