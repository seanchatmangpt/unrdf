# Hook API Reference

**Purpose:** Complete API reference for UNRDF Knowledge Hooks system.

**Audience:** All developers using hooks

**Version:** 5.0.1

---

## Overview

Knowledge Hooks are reactive triggers for knowledge graph operations. This reference covers:
- Hook definition API
- Hook lifecycle (before/run/after)
- Hook context and bindings
- Hook evaluation engine
- Receipt integration

---

## Core API

### `defineHook(config)`

**[Placeholder - Content to be filled]**

```javascript
import { defineHook } from '@unrdf/hooks';

const hook = defineHook({
  meta: HookMetadata,
  channel: ChannelConfig,
  when: ConditionConfig,
  determinism: DeterminismConfig,
  receipt: ReceiptConfig,
  before: BeforeFunction,
  run: RunFunction,
  after: AfterFunction
});
```

**Evidence:** Implementation at `/home/user/unrdf/packages/hooks/src/define-hook.mjs`

---

#### Parameters

##### `meta` (required)

**[Placeholder - Meta field details]**

```typescript
interface HookMetadata {
  name: string;              // Unique hook identifier
  description?: string;      // Human-readable description
  version?: string;          // Hook version (semver)
  ontology?: string;         // Ontology URI
  tags?: string[];          // Classification tags
}
```

**Evidence:** Meta schema at `/home/user/unrdf/packages/hooks/src/schemas/meta.mjs`

---

##### `channel` (optional)

**[Placeholder - Channel configuration]**

```typescript
interface ChannelConfig {
  graphs?: string[];         // Named graphs to observe
  view?: ViewConfig;         // Delta view configuration
  scope?: 'local' | 'global'; // Hook scope
}
```

**Evidence:** Channel config at `/home/user/unrdf/packages/hooks/src/schemas/channel.mjs`

---

##### `when` (required)

**[Placeholder - Condition configuration]**

```typescript
interface ConditionConfig {
  kind: 'sparql-ask' | 'sparql-select' | 'shacl';
  ref: {
    uri: string;             // Condition URI
    sha256: string;          // Content hash
    mediaType: string;       // MIME type
  };
}
```

**Evidence:** Condition schema at `/home/user/unrdf/packages/hooks/src/schemas/condition.mjs`

---

##### `determinism` (optional)

**[Placeholder - Determinism configuration]**

**Evidence:** Determinism at `/home/user/unrdf/packages/hooks/src/schemas/determinism.mjs`

---

##### `receipt` (optional)

**[Placeholder - Receipt configuration]**

**Evidence:** Receipt config at `/home/user/unrdf/packages/hooks/src/schemas/receipt.mjs`

---

### Lifecycle Functions

#### `before(context)`

**[Placeholder - Before function signature]**

```javascript
before: async (context: HookContext) => {
  // Pre-execution validation
  // Can veto execution by throwing error
  // Can modify context
}
```

**Evidence:** Before handler at `/home/user/unrdf/packages/hooks/src/lifecycle/before.mjs`

---

#### `run(context)`

**[Placeholder - Run function signature]**

```javascript
run: async (context: HookContext) => {
  // Main hook logic
  // Return veto decision or result
  return {
    veto: boolean,
    reason?: string,
    data?: any
  };
}
```

**Evidence:** Run handler at `/home/user/unrdf/packages/hooks/src/lifecycle/run.mjs`

---

#### `after(context)`

**[Placeholder - After function signature]**

```javascript
after: async (context: HookContext) => {
  // Post-execution cleanup
  // Audit trail
  // Cannot veto (already executed)
}
```

**Evidence:** After handler at `/home/user/unrdf/packages/hooks/src/lifecycle/after.mjs`

---

## Hook Context

**[Placeholder - Context object specification]**

```typescript
interface HookContext {
  // Operation info
  operation: {
    type: string;
    data: any;
    timestamp: Date;
  };

  // Store access
  store: Store;
  deltaView?: DeltaView;

  // Condition results
  conditionResult: any;

  // User data
  user?: string;
  metadata?: object;

  // Execution state
  phase: 'before' | 'run' | 'after';
  hookName: string;
}
```

**Evidence:** Context at `/home/user/unrdf/packages/hooks/src/context.mjs`

---

## Hook Evaluation Engine

**[Placeholder - Evaluation engine API]**

**Evidence:** Engine at `/home/user/unrdf/packages/hooks/src/engine.mjs`

---

## Receipt Integration

**[Placeholder - Receipt generation in hooks]**

**Evidence:** Receipt integration at `/home/user/unrdf/packages/hooks/src/receipt-integration.mjs`

---

## Examples

### Example 1: Simple Validation Hook

**[Placeholder - Simple example]**

**Evidence:** Examples at `/home/user/unrdf/examples/hooks/`

---

### Example 2: Policy Gate Hook

**[Placeholder - Policy gate example]**

---

### Example 3: Audit Trail Hook

**[Placeholder - Audit trail example]**

---

## Error Handling

**[Placeholder - Error types and handling]**

**Evidence:** Errors at `/home/user/unrdf/packages/hooks/src/errors.mjs`

---

## Performance Guidelines

**[Placeholder - Performance best practices]**

- Hook execution time limits
- Async vs sync operations
- Resource management

**Evidence:** Performance at `/home/user/unrdf/packages/hooks/src/performance.md`

---

## Related References

- **[Receipt Schema Reference](./receipt-schema.md)** - Receipt structure
- **[Policy Predicate Syntax](./policy-predicate-syntax.md)** - Condition syntax
- **[Tutorial 04: Implement Policy Gates](../tutorials/04-implement-policy-gates.md)** - Hands-on tutorial

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
