# Reference: API Documentation

**Look up answers for narrative state chain APIs.**

## Available References

### Skeleton (To Be Completed)

All reference docs are placeholders pending completion.

---

### Core APIs

1. **[Universe API](api-universe.md)** — KGCStore operations
   - `createStore()`
   - `store.add()`, `store.match()`, `store.getQuads()`
   - `freezeUniverse()`, `reconstructState()`
   - **Evidence:** [packages/kgc-4d/src/store.mjs](/home/user/unrdf/packages/kgc-4d/src/store.mjs)

2. **[Scene API](api-scene.md)** — Delta schema and operations
   - `DeltaSchema` (Zod)
   - `createDelta(op, subject, predicate, object, source)`
   - Operation types: `add`, `delete`, `update`
   - **Evidence:** [packages/v6-core/src/delta/schema.mjs](/home/user/unrdf/packages/v6-core/src/delta/schema.mjs)

3. **[Bridge API](api-bridge.md)** — Adapter patterns
   - `WorkflowAdapter` — YAWL → Delta
   - `ResourceAdapter` — Resource allocation → Delta
   - `GraphQLAdapter` — GraphQL mutations → Delta
   - **Evidence:** [packages/v6-core/src/delta/adapters/](/home/user/unrdf/packages/v6-core/src/delta/adapters/)

4. **[Receipt API](api-receipt.md)** — Receipt schema and validation
   - `DeltaReceiptSchema` (Zod)
   - `validateDeltaReceipt(data)`
   - Receipt properties: `applied`, `stateHash`, `reason`
   - **Evidence:** [packages/v6-core/src/delta/schema.mjs](/home/user/unrdf/packages/v6-core/src/delta/schema.mjs):137

---

### Utilities

5. **[Error Codes](error-codes.md)** — Standard error taxonomy
   - Validation errors
   - Policy denials
   - Reconciliation failures

6. **[Data Shapes](data-shapes.md)** — RDF schemas (TTL examples)
   - Delta shape
   - Receipt shape
   - Event log shape

---

## Quick Lookup

| Task | API |
|------|-----|
| Create a universe | [Universe API](api-universe.md) → `createStore()` |
| Propose a change | [Scene API](api-scene.md) → `createDelta()` |
| Add a guard policy | [packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs):245 → `gate.addPolicy()` |
| Verify a receipt | [Receipt API](api-receipt.md) → `validateDeltaReceipt()` |
| Translate domain objects | [Bridge API](api-bridge.md) → Adapters |

---

**Next:** [Explanation Documentation](../explanation/README.md)
