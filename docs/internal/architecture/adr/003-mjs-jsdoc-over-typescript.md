# ADR-003: MJS with JSDoc Over TypeScript

**Status**: Proposed
**Date**: 2025-10-01
**Decision Maker**: System Architecture Agent
**Context**: Type safety for dashboard codebase

---

## Context

The UNRDF project requires type safety for:
- API request/response validation
- Component props and emits
- Composable return types
- Form data validation

Common approaches for type safety in Vue/Nuxt:
1. **TypeScript** (`.ts`, `.vue <script lang="ts">`)
2. **MJS + JSDoc** (`.mjs`, `.vue <script>` with JSDoc comments)
3. **No types** (vanilla JavaScript)

The existing unrdf codebase uses MJS + JSDoc + Zod pattern:
- Source files: `.mjs` (not `.ts`)
- Type annotations: JSDoc comments
- Runtime validation: Zod schemas

## Decision

**We will use MJS with JSDoc annotations** for the sidecar dashboard instead of adding TypeScript compilation.

**Implementation**:
```javascript
/**
 * Knowledge Hooks composable
 * @returns {Object} Hook management state and methods
 * @property {Ref<KnowledgeHook[]>} hooks - Reactive hooks array
 * @property {Ref<boolean>} loading - Loading state
 * @property {Ref<string|null>} error - Error message
 * @property {() => Promise<void>} fetchHooks - Fetch hooks from API
 */
export const useKnowledgeHooks = () => {
  const hooks = ref([])
  const loading = ref(false)
  const error = ref(null)

  const fetchHooks = async () => {
    // ...
  }

  return { hooks, loading, error, fetchHooks }
}
```

**Zod for Runtime Validation**:
```javascript
// app/schemas/hooks.mjs
import { z } from 'zod'

export const KnowledgeHookSchema = z.object({
  id: z.string().min(1),
  select: z.string().min(1),
  predicates: z.array(HookPredicateSchema),
  combine: z.enum(['AND', 'OR'])
})

/** @typedef {z.infer<typeof KnowledgeHookSchema>} KnowledgeHook */
```

## Rationale

### Why MJS + JSDoc?

1. **Consistency with Main Codebase**:
   - unrdf CLI uses MJS + JSDoc pattern
   - All composables in `/src/composables/` are `.mjs`
   - Maintains architectural consistency

2. **No Build Step Complexity**:
   - TypeScript requires `tsc` compilation
   - MJS runs directly in Node.js
   - Faster development iteration
   - No source maps needed

3. **Runtime Type Safety with Zod**:
   - TypeScript only provides compile-time checks
   - Zod validates at runtime (catches API response errors)
   - Better error messages for users

4. **JSDoc Provides IDE Support**:
   - VSCode, WebStorm show JSDoc type hints
   - Auto-completion works
   - Hover documentation works
   - No TypeScript Language Server needed

5. **Nuxt 4 Works Without TypeScript**:
   - Nuxt auto-imports work with `.mjs`
   - Vue SFCs support `<script>` (no `lang="ts"` required)
   - No TypeScript configuration needed

### Why Not TypeScript?

❌ **Adds Build Complexity**:
- Need `tsconfig.json`
- Need `vue-tsc` for Vue SFC type checking
- Longer build times
- Must configure Nuxt for TypeScript

❌ **Compile-Time Only**:
- TypeScript types are erased at runtime
- API responses are NOT validated
- Users can submit invalid data

❌ **Inconsistent with Main Codebase**:
- unrdf uses MJS + Zod
- Would create split architecture (MJS in CLI, TS in dashboard)
- Harder to share code between CLI and dashboard

❌ **More Dependencies**:
- `typescript` package
- `@types/*` packages
- `vue-tsc` for SFC checking

**TypeScript is excellent, but not aligned with project philosophy:**
- UNRDF prioritizes runtime safety (Zod) over compile-time safety
- MJS + JSDoc provides "good enough" type hints for dashboard

## Consequences

### Positive

✅ **No Compilation Step**:
- `.mjs` files run directly
- Faster development
- Simpler debugging

✅ **Runtime Type Safety**:
- Zod validates API responses
- Catches errors TypeScript misses
- Better error messages

✅ **Consistent with Main Codebase**:
- Same patterns as unrdf CLI
- Easier to share code
- Unified architecture

✅ **IDE Support**:
- JSDoc provides auto-completion
- Type hints on hover
- Go-to-definition works

### Negative

❌ **Less Strict Type Checking**:
- JSDoc is optional (can skip annotations)
- No type errors at build time
- Mitigation: Use Zod for critical paths

❌ **Verbose Type Annotations**:
- JSDoc comments are longer than TypeScript syntax
- Mitigation: Use `@typedef` for reusable types

❌ **No Advanced TypeScript Features**:
- No mapped types, conditional types, etc.
- Mitigation: Rarely needed for dashboard UI

❌ **Vue SFC Prop Validation**:
- Props are runtime-validated, not compile-time
- Mitigation: Use Zod schemas in prop validators

### Neutral

⚖️ **Learning Curve**:
- Team must learn JSDoc syntax
- Less common than TypeScript
- Mitigation: Document patterns in codebase

⚖️ **Refactoring Path**:
- Can migrate to TypeScript incrementally if needed
- `.mjs` can coexist with `.ts` during migration

## Implementation

### Pattern: JSDoc Type Definitions

**Reusable Types** (`types/dashboard.mjs`):
```javascript
/**
 * @typedef {Object} KnowledgeHook
 * @property {string} id - Hook identifier
 * @property {string} select - SPARQL SELECT query
 * @property {Array<HookPredicate>} predicates - Predicates to evaluate
 * @property {'AND'|'OR'} combine - Combination strategy
 * @property {'pre'|'post'} [phase] - Execution phase (optional)
 */

/**
 * @typedef {Object} HookPredicate
 * @property {string} kind - Predicate kind (ASK, SHACL, THRESHOLD, etc.)
 * @property {Object} spec - Predicate specification
 */
```

**Using Types** (composables):
```javascript
/**
 * @typedef {import('~/types/dashboard.mjs').KnowledgeHook} KnowledgeHook
 */

/**
 * Create new hook
 * @param {KnowledgeHook} hookData - Hook definition
 * @returns {Promise<Object>} Creation result
 */
export const createHook = async (hookData) => {
  // ...
}
```

**Vue SFC Props**:
```vue
<script setup>
/**
 * @typedef {import('~/types/dashboard.mjs').KnowledgeHook} KnowledgeHook
 */

/**
 * Props
 * @type {{ hooks: KnowledgeHook[] }}
 */
const props = defineProps({
  hooks: {
    type: Array,
    required: true
  }
})
</script>
```

### Pattern: Zod + JSDoc Co-location

**Best Practice**: Define Zod schema AND JSDoc typedef together

```javascript
// app/schemas/hooks.mjs

import { z } from 'zod'

/**
 * Knowledge Hook schema for runtime validation
 */
export const KnowledgeHookSchema = z.object({
  id: z.string().min(1, 'Hook ID is required'),
  select: z.string().min(1, 'SPARQL SELECT query is required'),
  predicates: z.array(HookPredicateSchema).min(1, 'At least one predicate required'),
  combine: z.enum(['AND', 'OR']),
  phase: z.enum(['pre', 'post']).optional()
})

/**
 * @typedef {z.infer<typeof KnowledgeHookSchema>} KnowledgeHook
 */
```

**Benefits**:
- Single source of truth (Zod schema)
- JSDoc type derived from Zod (`z.infer`)
- Runtime validation + IDE hints

### Pattern: Generic JSDoc

**For Generic Functions**:
```javascript
/**
 * Fetch data from API with type safety
 * @template T
 * @param {string} endpoint - API endpoint
 * @param {z.ZodType<T>} schema - Zod schema for response
 * @returns {Promise<T>} Validated response
 */
export const fetchTyped = async (endpoint, schema) => {
  const response = await $fetch(endpoint)
  return schema.parse(response)
}

// Usage
const hooks = await fetchTyped('/api/hooks', KnowledgeHookSchema.array())
// hooks is typed as KnowledgeHook[]
```

## Alternatives Considered

### Alternative 1: Full TypeScript

**Approach**:
```typescript
// app/composables/useKnowledgeHooks.ts
import type { KnowledgeHook } from '~/types'

export const useKnowledgeHooks = () => {
  const hooks = ref<KnowledgeHook[]>([])
  const loading = ref<boolean>(false)
  const error = ref<string | null>(null)

  const fetchHooks = async (): Promise<void> => {
    // ...
  }

  return { hooks, loading, error, fetchHooks }
}
```

**Why Not Chosen**:
- Requires TypeScript compilation
- Inconsistent with main codebase (MJS)
- No runtime validation (Zod still needed)

**When to Reconsider**:
- If main unrdf codebase migrates to TypeScript
- If team prefers TypeScript ecosystem
- If advanced type features are needed

### Alternative 2: No Types (Vanilla JavaScript)

**Approach**:
```javascript
export const useKnowledgeHooks = () => {
  const hooks = ref([])
  const loading = ref(false)
  const error = ref(null)

  const fetchHooks = async () => {
    // ...
  }

  return { hooks, loading, error, fetchHooks }
}
```

**Why Not Chosen**:
- No IDE hints
- No auto-completion
- Harder to maintain

### Alternative 3: TypeScript for Types, MJS for Runtime

**Approach**:
- Define types in `.d.ts` files
- Implement logic in `.mjs` files
- Use JSDoc `@type` to reference `.d.ts` types

**Why Not Chosen**:
- Over-complicates architecture
- Requires both TypeScript AND MJS knowledge
- No significant benefit over pure JSDoc

## Validation

### Success Criteria

- ✅ IDE shows type hints for composables
- ✅ Auto-completion works for component props
- ✅ Zod validation catches runtime errors
- ✅ No TypeScript compilation errors (because no TypeScript)
- ✅ `pnpm run build` succeeds

### Test Plan

**IDE Hints** (manual test):
1. Open VSCode
2. Create new component
3. Use `useKnowledgeHooks()` composable
4. Hover over `fetchHooks` → Should show JSDoc comment
5. Type `hooks.` → Should show array methods

**Runtime Validation**:
```javascript
// test/unit/schemas/hooks.test.mjs
import { describe, it, expect } from 'vitest'
import { KnowledgeHookSchema } from '~/schemas/hooks.mjs'

describe('KnowledgeHookSchema', () => {
  it('validates correct hook', () => {
    const hook = {
      id: 'test',
      select: 'SELECT * WHERE { ?s ?p ?o }',
      predicates: [],
      combine: 'AND'
    }

    expect(() => KnowledgeHookSchema.parse(hook)).not.toThrow()
  })

  it('rejects invalid hook', () => {
    const hook = { id: '' } // Missing required fields

    expect(() => KnowledgeHookSchema.parse(hook)).toThrow()
  })
})
```

## Migration to TypeScript (If Needed)

**If project later migrates to TypeScript**:

**Step 1**: Install TypeScript
```bash
pnpm add -D typescript vue-tsc
```

**Step 2**: Create `tsconfig.json`
```json
{
  "extends": "./.nuxt/tsconfig.json",
  "compilerOptions": {
    "strict": true
  }
}
```

**Step 3**: Rename `.mjs` → `.ts` incrementally
- Existing JSDoc comments are compatible with TypeScript
- No breaking changes

**Step 4**: Update Zod schemas to export types
```typescript
import { z } from 'zod'

export const KnowledgeHookSchema = z.object({ /* ... */ })

export type KnowledgeHook = z.infer<typeof KnowledgeHookSchema>
```

## References

- [JSDoc Type Annotations](https://www.typescriptlang.org/docs/handbook/jsdoc-supported-types.html)
- [Zod Documentation](https://zod.dev/)
- [Nuxt TypeScript Support](https://nuxt.com/docs/guide/concepts/typescript)

---

**Status**: Awaiting Hive consensus approval
**Implementation**: Blocked until approved
**Next Review**: After 6 months (or if team requests TypeScript)
