# Explanation: Two Hook Systems

`@unrdf/hooks` exposes two distinct, non-overlapping hook APIs. Understanding why they exist
separately helps you choose the right tool for each problem.

---

## System 1 — Low-level hooks (`defineHook`)

**API:** `defineHook` / `executeHook` / `executeHookChain` / `executeHooksByTrigger`

Low-level hooks are JavaScript functions. You define them in code, attach them to a named
trigger, and execute them synchronously per quad.

```javascript
const hook = defineHook({
  name: 'reject-blanks',
  trigger: 'before-add',
  validate: quad => quad.subject.termType !== 'BlankNode',
});

const result = executeHook(hook, quad);
result.valid; // boolean — synchronous, no await
```

**Design rationale:**

- **Speed is the primary constraint.** Quad ingestion pipelines can process millions of quads.
  `executeHook` uses a Zod-free hot path (pre-computed `_hasValidation` / `_hasTransformation`
  flags set at `defineHook()` time) to reach sub-microsecond execution in the common case.
- **Scope is a single quad.** Validation and transformation operate on one quad in isolation.
  There is no store access.
- **Synchronous execution** means no event-loop yielding, no async overhead, and predictable
  latency in tight ingestion loops.
- **Authored in JavaScript** — developers write ordinary functions. No query language required.

**When to use low-level hooks:**

- Validate structural correctness before adding a quad to the store (IRI format, term type,
  language tag BCP 47 compliance).
- Normalise literals at ingestion time (trim whitespace, lowercase language tags).
- Build a pipeline that combines multiple checks and transforms in a single pass.

---

## System 2 — KnowledgeHooks (JSON schema)

**API:** `KnowledgeHookEngine` / `createKnowledgeHook` / `engine.evaluateCondition` /
`engine.execute`

KnowledgeHooks are JSON-declarative policy objects. You define them as data (not code),
evaluate conditions against the entire RDF store, and apply SPARQL-driven effects with
cryptographic receipt chaining.

```javascript
const hook = createKnowledgeHook({
  name: 'validate-compliance',
  condition: {
    kind: 'sparql-ask',
    query: 'ASK { ?s a ex:Trade ; ex:riskScore ?r . FILTER(?r > 100) }',
  },
  effects: [
    {
      kind: 'sparql-construct',
      query: 'CONSTRUCT { ?s ex:flagged true } WHERE { ?s a ex:Trade }',
    },
  ],
});

const result = await engine.execute(ctx, [hook]); // async
result.receipt.receiptHash; // BLAKE3 audit hash
```

**Design rationale:**

- **Scope is the whole store.** Conditions such as SPARQL ASK, SHACL validation, delta
  detection, and N3 inference cannot be evaluated per-quad — they require access to the
  complete graph.
- **Asynchronous execution** is unavoidable: SPARQL queries, SHACL shape loading, and EYE
  reasoner invocations are all I/O-bound operations.
- **JSON-declarative format** allows hooks to be stored in databases, transmitted over APIs,
  and validated with Zod schemas before execution. A governance team can author policy rules
  without writing JavaScript.
- **Cryptographic audit trail.** `engine.execute()` computes BLAKE3 hashes of the store state
  before and after each execution, linking them into a tamper-evident chain via
  `previousReceiptHash`. This satisfies audit and compliance requirements that per-quad hooks
  cannot address.
- **9 condition kinds** cover boolean SPARQL (sparql-ask), result-set SPARQL (sparql-select),
  structural validation (shacl), change detection (delta), time-windowed aggregation (window),
  threshold/count checks, N3 forward-chaining inference (n3), and logic programming (datalog).

**When to use KnowledgeHooks:**

- Enforce SHACL shapes on the whole graph (block / annotate / repair modes).
- Trigger actions when the store grows or shrinks beyond a threshold (delta conditions).
- Apply SPARQL CONSTRUCT transformations as a side-effect of a governance rule.
- Build a verifiable audit trail of all policy decisions.
- Define policy in JSON that non-developer governance teams can inspect and modify.

---

## Why `ChainResult` is an object, not an array

`executeHookChain` and `executeHooksByTrigger` both return a `ChainResult`:

```typescript
{
  valid: boolean;
  quad: Quad;
  results: HookResult[];  // per-hook detail
  error?: string;
  failedHook?: string;
}
```

An earlier design returned `HookResult[]` directly. That design had two problems:

1. **Accessing `valid` required iterating the array** (`results.every(r => r.valid)`). Every
   call site had to repeat this aggregation.
2. **The final quad was ambiguous.** After a transform chain, the output quad lives in
   `results[results.length - 1].quad`. The index is not obvious and is wrong if the chain is
   empty.

`ChainResult` promotes the two most commonly needed values — `valid` and `quad` — to the top
level. The per-hook detail is still available in `results` when needed. This eliminated a
class of bugs (`result[0].valid` would silently return `undefined` on an empty chain).

---

## Choosing between the two systems

| Concern                                | Use                                                 |
| -------------------------------------- | --------------------------------------------------- |
| Per-quad validation before store write | Low-level hooks (`defineHook`)                      |
| Per-quad literal normalisation         | Low-level hooks (`defineHook`)                      |
| Whole-store SHACL shape validation     | KnowledgeHook (`condition.kind = 'shacl'`)          |
| SPARQL-driven data enrichment          | KnowledgeHook (`effect.kind = 'sparql-construct'`)  |
| Time-windowed anomaly detection        | KnowledgeHook (`condition.kind = 'window'`)         |
| N3 forward-chaining inference          | KnowledgeHook (`condition.kind = 'n3'`)             |
| Cryptographic audit trail              | KnowledgeHook (`engine.execute()`)                  |
| High-throughput batch (10 000+ quads)  | Low-level hooks + `executeBatch` or pooled variants |

The two systems are complementary. A typical production pipeline uses low-level hooks for
ingestion integrity and KnowledgeHooks for periodic governance evaluation.
