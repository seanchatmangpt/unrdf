# Reference: KnowledgeHook Schema

`KnowledgeHookSchema` is a Zod schema. Use `createKnowledgeHook(obj)` to construct a validated
instance and `validateKnowledgeHook(obj)` to get a `{ success, error }` result.

---

## Top-level fields

| Field       | Type                  | Required | Constraint     | Description                                             |
| ----------- | --------------------- | -------- | -------------- | ------------------------------------------------------- |
| `name`      | `string`              | Yes      | min 1, max 100 | Human-readable hook identifier                          |
| `condition` | `Condition`           | Yes      | one of 9 kinds | Evaluated to decide whether effects run                 |
| `effects`   | `Effect[]`            | Yes      | 0–10 items     | Applied when condition is true                          |
| `metadata`  | `Record<string, any>` | No       | —              | Arbitrary key-value pairs (author, version, tags, etc.) |

---

## Condition kinds

### `sparql-ask`

```typescript
{
  kind: 'sparql-ask';
  query: string; /* min 1 */
}
```

Evaluates a SPARQL ASK query. Returns `true` when the query returns `true`.

### `sparql-select`

```typescript
{
  kind: 'sparql-select';
  query: string; /* min 1 */
}
```

Evaluates a SPARQL SELECT query. Returns `true` when the result set is non-empty.

### `shacl`

```typescript
{
  kind: 'shacl';
  ref: {
    uri: string;           // Shape file URI (file://, http://, data:)
    sha256?: string;       // 64 lowercase hex chars — optional integrity check
    mediaType?: string;    // MIME type, e.g. 'text/turtle'
  };
  enforcementMode: 'block' | 'annotate' | 'repair'; // default: 'block'
  repairConstruct?: string; // SPARQL CONSTRUCT — required when enforcementMode='repair'
}
```

| `enforcementMode` | Behaviour                                                          |
| ----------------- | ------------------------------------------------------------------ |
| `block`           | Returns `false` on any SHACL violation. Halts effect chain.        |
| `annotate`        | Writes `sh:ValidationResult` triples; always returns `true`.       |
| `repair`          | Runs `repairConstruct`, then re-validates. Returns repair success. |

### `delta`

```typescript
{
  kind: 'delta';
  spec: {
    change: 'any' | 'increase' | 'decrease' | 'modify';
    threshold: number; // 0–1, default 0.1 (10% of total quads)
    baseline?: string; // URI to baseline state
  };
}
```

Detects store size changes between executions.

### `window`

```typescript
{
  kind: 'window';
  spec: {
    size: number; // milliseconds
    aggregate: 'count' | 'sum' | 'avg' | 'min' | 'max';
    query: string; // SPARQL SELECT
  }
}
```

Aggregates query results within a sliding time window.

### `threshold`

```typescript
{
  kind: 'threshold'; /* threshold specification */
}
```

Numeric threshold condition. Check the Zod schema in `src/hooks/schemas.mjs` for the full spec.

### `count`

```typescript
{
  kind: 'count'; /* count specification */
}
```

Count-based condition against the store or a query result.

### `n3`

```typescript
{
  kind: 'n3';
  rules: string; // N3/Notation3 forward-chaining rules (EYE reasoner syntax)
  askQuery: string; // SPARQL ASK query evaluated after inference
}
```

Runs N3 forward-chaining inference via EYE, then evaluates `askQuery` against the inferred graph.

### `datalog`

```typescript
{
  kind: 'datalog';
  // Datalog logic program evaluated via bottom-up fixpoint
  // Check schemas.mjs for exact fields
}
```

---

## Effect kinds

### `sparql-construct`

```typescript
{
  kind: 'sparql-construct';
  query: string;
}
```

Runs a SPARQL CONSTRUCT query against the store. The resulting triples are added to the store
as the effect output.

### `function`

```typescript
{
  kind: 'function'; /* function effect spec */
}
```

Executes a sandboxed JavaScript function effect. See `src/hooks/effect-sandbox.mjs`.

---

## Receipt fields (returned by `engine.execute()`)

| Field                 | Type             | Description                                 |
| --------------------- | ---------------- | ------------------------------------------- |
| `receiptHash`         | `string`         | BLAKE3 hash of the entire receipt           |
| `input_hash`          | `string`         | BLAKE3 hash of store state before execution |
| `output_hash`         | `string`         | BLAKE3 hash of store state after execution  |
| `previousReceiptHash` | `string \| null` | Links to the prior receipt in the chain     |
| `delta.adds`          | `Quad[]`         | Quads added by effects                      |
| `delta.deletes`       | `Quad[]`         | Quads removed by effects                    |

---

## `KnowledgeHookEngine` constructor

```javascript
import { KnowledgeHookEngine } from '@unrdf/hooks';
import { createStore } from '@unrdf/oxigraph';

const engine = new KnowledgeHookEngine(store);
```

**Methods**

| Method                         | Returns                    | Description                                               |
| ------------------------------ | -------------------------- | --------------------------------------------------------- |
| `registerHook(hook)`           | `string` (UUID)            | Register hook; throws `ValidationError` if schema invalid |
| `evaluateCondition(condition)` | `Promise<boolean>`         | Evaluate any condition kind                               |
| `execute(context, hooks)`      | `Promise<ExecutionResult>` | Execute hooks with receipt chaining                       |
| `getReceiptChain()`            | `Receipt[]`                | Return full receipt history                               |
| `clearReceiptChain()`          | `void`                     | Clear receipt history                                     |
