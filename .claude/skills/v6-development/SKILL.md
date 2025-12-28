# V6 Development Skill

Expert knowledge for developing UNRDF v6.0.0 features with ΔGate control plane, unified receipts, and delta contracts.

## Activation

Use this skill when:
- Implementing v6-core features
- Working with DeltaGate, receipts, or delta contracts
- Migrating from v5 to v6

## Core Patterns

### Receipt Creation
```javascript
import { createReceipt } from '@unrdf/v6-core/receipts';

const receipt = createReceipt({
  operation: 'create',
  entityType: 'Triple',
  data: { subject, predicate, object },
  timestamp: Date.now(),
  agent: 'system'
});
```

### Delta Validation
```javascript
import { validateDelta, DeltaSchema } from '@unrdf/v6-core/delta';

const delta = {
  id: crypto.randomUUID(),
  type: 'insert',
  payload: triples,
  timestamp: Date.now()
};

const validated = DeltaSchema.parse(delta);
```

### DeltaGate Control Plane
```javascript
import { DeltaGate } from '@unrdf/v6-core/deltagate';

const gate = new DeltaGate({
  validators: [schemaValidator, policyValidator],
  interceptors: [auditInterceptor],
  receipts: true
});

await gate.process(delta);
```

## File Conventions

- All files use `.mjs` extension (100% ESM)
- Schemas co-located with `.schema.mjs` suffix
- Max 500 lines per file
- JSDoc for all public APIs

## Testing Pattern

```javascript
import { describe, it, expect } from 'vitest';
import { createReceipt } from '@unrdf/v6-core/receipts';

describe('Receipt Creation', () => {
  it('should create valid receipt with all fields', () => {
    const receipt = createReceipt({ /* ... */ });
    expect(receipt.id).toBeDefined();
    expect(receipt.timestamp).toBeTypeOf('number');
  });
});
```

## Performance Targets

| Operation | P95 Target |
|-----------|------------|
| Receipt Creation | <1ms |
| Delta Validation | <5ms |
| Receipt Verification | <0.5ms |
| Receipt Chain (10) | <50ms |

## Common Errors

### Import Errors
- Use `@unrdf/v6-core/receipts` not `@unrdf/v6-core/src/receipts`
- Use `@unrdf/oxigraph` not `n3` directly

### Schema Errors
- Always use Zod for validation
- Use `safeParse` for user input, `parse` for internal data
