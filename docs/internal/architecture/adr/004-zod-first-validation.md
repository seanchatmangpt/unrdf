# ADR-004: Zod-First Validation Strategy

**Status**: Proposed
**Date**: 2025-10-01
**Decision Maker**: System Architecture Agent
**Context**: Data validation for API requests, responses, and form inputs

---

## Context

The dashboard handles user input and API data that requires validation:
- **Form Inputs**: Hook creation, policy upload, SPARQL queries
- **API Responses**: Hooks, policies, transactions from backend
- **API Requests**: Creating/updating hooks, policies
- **WebSocket Messages**: Real-time event payloads

Common validation approaches:
1. **Zod** (runtime schema validation)
2. **Joi** (runtime validation)
3. **Yup** (runtime validation)
4. **TypeScript** (compile-time only)
5. **Manual validation** (if/else checks)

The unrdf codebase uses Zod extensively for knowledge hook definitions and SHACL validation.

## Decision

**We will use Zod as the primary validation strategy** for all data entering or leaving the dashboard.

**Zod schemas will be the single source of truth** for data structure definitions.

**Implementation**:
```javascript
// app/schemas/hooks.mjs
import { z } from 'zod'

export const KnowledgeHookSchema = z.object({
  id: z.string().min(1, 'Hook ID is required'),
  select: z.string().min(1, 'SPARQL SELECT query is required'),
  predicates: z.array(HookPredicateSchema).min(1, 'At least one predicate required'),
  combine: z.enum(['AND', 'OR']),
  phase: z.enum(['pre', 'post']).optional()
})

/** @typedef {z.infer<typeof KnowledgeHookSchema>} KnowledgeHook */
```

**Usage in API Routes**:
```javascript
// server/api/hooks/index.post.mjs
import { KnowledgeHookSchema } from '~/app/schemas/hooks.mjs'

export default defineEventHandler(async (event) => {
  const body = await readBody(event)

  // Validate with Zod
  const validated = KnowledgeHookSchema.parse(body)

  // validated is type-safe and guaranteed valid
  return await createHook(validated)
})
```

**Usage in Components**:
```vue
<script setup>
import { KnowledgeHookSchema } from '~/schemas/hooks.mjs'

const form = ref({
  id: '',
  select: '',
  predicates: [],
  combine: 'AND'
})

const errors = ref({})

const handleSubmit = () => {
  try {
    const validated = KnowledgeHookSchema.parse(form.value)
    // Submit validated data
    await createHook(validated)
  } catch (err) {
    if (err instanceof z.ZodError) {
      // Display validation errors
      errors.value = err.flatten().fieldErrors
    }
  }
}
</script>
```

## Rationale

### Why Zod?

1. **Runtime Validation**:
   - TypeScript only validates at compile-time (types are erased at runtime)
   - Zod validates actual data at runtime
   - Catches API response errors, user input errors, WebSocket payload errors

2. **Single Source of Truth**:
   - Zod schema defines data structure
   - JSDoc types derived from Zod (`z.infer`)
   - Same schema used client-side and server-side

3. **Better Error Messages**:
   - Zod provides detailed, user-friendly error messages
   - TypeScript only says "Type 'X' is not assignable to 'Y'"
   - Zod: `"Hook ID is required"`, `"Must be at least 1 character"`

4. **Composable Schemas**:
   - Schemas can extend, merge, pick, omit other schemas
   - Example: `UpdateHookSchema = KnowledgeHookSchema.partial()`

5. **Ecosystem Compatibility**:
   - Zod integrates with form libraries (VeeValidate, etc.)
   - Works with tRPC, Fastify, Express
   - Nuxt-friendly

6. **Consistency with Main Codebase**:
   - unrdf uses Zod for hook predicates, SHACL validation
   - Dashboard continues this pattern

### Why Not Joi or Yup?

**Joi**:
- Larger bundle size (~145 KB vs Zod ~8 KB)
- Less TypeScript-friendly
- More verbose syntax

**Yup**:
- Similar features to Zod
- Zod is faster (benchmarks show 2-5x speed)
- Zod has better TypeScript inference

**TypeScript Alone**:
- No runtime validation
- Cannot validate API responses
- Cannot validate user input

**Manual Validation**:
- Error-prone
- Verbose
- No type inference

## Consequences

### Positive

✅ **Runtime Safety**:
- API responses validated before use
- User input validated before submission
- WebSocket messages validated

✅ **Single Source of Truth**:
- Data structure defined once (Zod schema)
- Types inferred from schema
- No duplication

✅ **Better Error Messages**:
- User-friendly validation errors
- Field-level error messages
- Easier debugging

✅ **Type Safety**:
- `z.infer<typeof Schema>` generates TypeScript-compatible types
- Works with JSDoc
- IDE auto-completion

### Negative

❌ **Bundle Size**:
- Zod adds ~8 KB (minified + gzipped)
- Mitigation: Tree-shaking removes unused validators

❌ **Performance Overhead**:
- Runtime validation is slower than compile-time
- Mitigation: Cache parsed schemas where possible

❌ **Learning Curve**:
- Team must learn Zod API
- Mitigation: Document patterns, provide examples

### Neutral

⚖️ **Schema Maintenance**:
- Must keep schemas in sync with API
- Mitigation: Co-locate schemas with API routes

⚖️ **Error Handling**:
- Must handle `ZodError` exceptions
- Mitigation: Centralized error handler

## Implementation

### Pattern: Schema Definition

**File**: `app/schemas/hooks.mjs`

```javascript
import { z } from 'zod'

/**
 * Predicate kind enum
 */
export const PredicateKindSchema = z.enum([
  'ASK',
  'SHACL',
  'DELTA',
  'THRESHOLD',
  'COUNT',
  'WINDOW',
  'HEALTH_SCORE'
])

/**
 * Hook predicate schema
 */
export const HookPredicateSchema = z.object({
  kind: PredicateKindSchema,
  spec: z.object({
    var: z.string().optional(),
    op: z.enum(['>', '<', '=', '>=', '<=']).optional(),
    value: z.number().optional(),
    query: z.string().optional(),
    shapes: z.any().optional()
  })
})

/**
 * Knowledge hook schema
 */
export const KnowledgeHookSchema = z.object({
  id: z.string().min(1, 'Hook ID is required'),
  name: z.string().optional(),
  description: z.string().optional(),
  select: z.string().min(1, 'SPARQL SELECT query is required'),
  predicates: z.array(HookPredicateSchema).min(1, 'At least one predicate required'),
  combine: z.enum(['AND', 'OR']),
  phase: z.enum(['pre', 'post']).optional(),
  created: z.date().optional(),
  creator: z.string().optional()
})

/** @typedef {z.infer<typeof KnowledgeHookSchema>} KnowledgeHook */
/** @typedef {z.infer<typeof HookPredicateSchema>} HookPredicate */
```

### Pattern: Schema Extension

**Create Update Schema** (all fields optional):
```javascript
export const UpdateHookSchema = KnowledgeHookSchema.partial()

/** @typedef {z.infer<typeof UpdateHookSchema>} UpdateHook */
```

**Create Response Schema** (omit sensitive fields):
```javascript
export const HookResponseSchema = KnowledgeHookSchema.omit({ creator: true })

/** @typedef {z.infer<typeof HookResponseSchema>} HookResponse */
```

**Merge Schemas**:
```javascript
const ProvScnceSchema = z.object({
  signature: z.string(),
  timestamp: z.date()
})

export const HookWithProvenanceSchema = KnowledgeHookSchema.merge(ProvenanceSchema)
```

### Pattern: API Route Validation

**Input Validation**:
```javascript
// server/api/hooks/index.post.mjs
import { KnowledgeHookSchema } from '~/app/schemas/hooks.mjs'

export default defineEventHandler(async (event) => {
  const body = await readBody(event)

  try {
    const validated = KnowledgeHookSchema.parse(body)
    const result = await createHook(validated)
    return { success: true, hook: result }
  } catch (err) {
    if (err instanceof z.ZodError) {
      throw createError({
        statusCode: 400,
        message: 'Invalid hook definition',
        data: err.flatten()
      })
    }
    throw err
  }
})
```

**Output Validation** (optional but recommended):
```javascript
// server/api/hooks/index.get.mjs
import { KnowledgeHookSchema } from '~/app/schemas/hooks.mjs'

export default defineEventHandler(async (event) => {
  const hooks = await getAllHooks()

  // Validate before sending to client
  const validated = hooks.map(h => KnowledgeHookSchema.parse(h))

  return {
    hooks: validated,
    total: validated.length
  }
})
```

### Pattern: Component Form Validation

**Template**:
```vue
<template>
  <form @submit.prevent="handleSubmit">
    <div>
      <label for="hook-id">Hook ID</label>
      <input
        id="hook-id"
        v-model="form.id"
        type="text"
        :class="{ 'border-red-500': errors.id }"
      />
      <span v-if="errors.id" class="text-red-500">{{ errors.id[0] }}</span>
    </div>

    <div>
      <label for="hook-select">SPARQL SELECT</label>
      <textarea
        id="hook-select"
        v-model="form.select"
        :class="{ 'border-red-500': errors.select }"
      />
      <span v-if="errors.select" class="text-red-500">{{ errors.select[0] }}</span>
    </div>

    <button type="submit" :disabled="loading">Create Hook</button>
  </form>
</template>

<script setup>
import { KnowledgeHookSchema } from '~/schemas/hooks.mjs'

const form = ref({
  id: '',
  select: '',
  predicates: [],
  combine: 'AND'
})

const errors = ref({})
const loading = ref(false)

const handleSubmit = async () => {
  errors.value = {}
  loading.value = true

  try {
    // Validate form data
    const validated = KnowledgeHookSchema.parse(form.value)

    // Submit to API
    const { createHook } = useKnowledgeHooks()
    await createHook(validated)

    // Reset form
    form.value = { id: '', select: '', predicates: [], combine: 'AND' }

    // Redirect to hooks list
    navigateTo('/hooks')
  } catch (err) {
    if (err instanceof z.ZodError) {
      // Flatten errors for display
      errors.value = err.flatten().fieldErrors
    } else {
      // API error
      alert(err.message)
    }
  } finally {
    loading.value = false
  }
}
</script>
```

### Pattern: Composable with Validation

**Validate API Responses**:
```javascript
// app/composables/useKnowledgeHooks.mjs
import { KnowledgeHookSchema } from '~/schemas/hooks.mjs'

export const useKnowledgeHooks = () => {
  const hooks = ref([])

  const fetchHooks = async () => {
    try {
      const response = await $fetch('/api/hooks')

      // Validate each hook from API
      hooks.value = response.hooks.map(h => {
        try {
          return KnowledgeHookSchema.parse(h)
        } catch (err) {
          console.error('Invalid hook from API:', h, err)
          return null
        }
      }).filter(Boolean)  // Remove invalid hooks
    } catch (err) {
      console.error('Failed to fetch hooks:', err)
    }
  }

  return { hooks, fetchHooks }
}
```

### Pattern: Error Handling

**Centralized Error Handler**:
```javascript
// app/utils/validation.mjs

/**
 * Format Zod errors for display
 * @param {z.ZodError} error - Zod validation error
 * @returns {Object} Flattened field errors
 */
export const formatZodError = (error) => {
  if (!(error instanceof z.ZodError)) {
    return { _errors: [error.message] }
  }

  return error.flatten().fieldErrors
}

/**
 * Validate data with schema and return errors
 * @template T
 * @param {z.ZodType<T>} schema - Zod schema
 * @param {unknown} data - Data to validate
 * @returns {{ success: boolean, data?: T, errors?: Object }}
 */
export const safeValidate = (schema, data) => {
  try {
    const validated = schema.parse(data)
    return { success: true, data: validated }
  } catch (err) {
    return { success: false, errors: formatZodError(err) }
  }
}
```

**Usage**:
```javascript
import { safeValidate } from '~/utils/validation'

const result = safeValidate(KnowledgeHookSchema, formData)

if (result.success) {
  await createHook(result.data)
} else {
  errors.value = result.errors
}
```

## Alternatives Considered

### Alternative 1: TypeScript Only (No Runtime Validation)

**Approach**:
```typescript
interface KnowledgeHook {
  id: string
  select: string
  predicates: HookPredicate[]
  combine: 'AND' | 'OR'
}

const createHook = (hook: KnowledgeHook) => {
  // Assumes hook is valid (no runtime check)
}
```

**Why Not Chosen**:
- No runtime validation
- API responses are NOT type-checked
- User input is NOT validated
- Silent failures at runtime

### Alternative 2: Manual Validation

**Approach**:
```javascript
const validateHook = (hook) => {
  const errors = {}

  if (!hook.id || hook.id.length === 0) {
    errors.id = 'Hook ID is required'
  }

  if (!hook.select || hook.select.length === 0) {
    errors.select = 'SPARQL SELECT is required'
  }

  // ... 50 more lines of validation

  return Object.keys(errors).length > 0 ? errors : null
}
```

**Why Not Chosen**:
- Verbose and error-prone
- No type inference
- Hard to maintain

### Alternative 3: Joi

**Approach**:
```javascript
const Joi = require('joi')

const KnowledgeHookSchema = Joi.object({
  id: Joi.string().required(),
  select: Joi.string().required(),
  predicates: Joi.array().items(HookPredicateSchema).min(1),
  combine: Joi.string().valid('AND', 'OR')
})
```

**Why Not Chosen**:
- Larger bundle size (145 KB vs 8 KB)
- Less TypeScript-friendly
- Zod is faster

### Alternative 4: Yup

**Approach**:
```javascript
import * as yup from 'yup'

const KnowledgeHookSchema = yup.object({
  id: yup.string().required(),
  select: yup.string().required(),
  predicates: yup.array().of(HookPredicateSchema).min(1),
  combine: yup.string().oneOf(['AND', 'OR'])
})
```

**Why Not Chosen**:
- Zod is faster (benchmarks show 2-5x)
- Zod has better TypeScript inference
- Zod is more composable

## Validation

### Success Criteria

- ✅ Invalid API requests return 400 with detailed errors
- ✅ Form validation displays field-level errors
- ✅ API responses validated before use in components
- ✅ WebSocket messages validated
- ✅ Type inference works (`z.infer`)

### Test Plan

**Unit Tests** (`test/unit/schemas/hooks.test.mjs`):
```javascript
import { describe, it, expect } from 'vitest'
import { KnowledgeHookSchema } from '~/schemas/hooks.mjs'

describe('KnowledgeHookSchema', () => {
  it('validates correct hook', () => {
    const hook = {
      id: 'test-hook',
      select: 'SELECT * WHERE { ?s ?p ?o }',
      predicates: [
        { kind: 'THRESHOLD', spec: { var: 'x', op: '>', value: 10 } }
      ],
      combine: 'AND'
    }

    expect(() => KnowledgeHookSchema.parse(hook)).not.toThrow()
  })

  it('rejects hook with missing ID', () => {
    const hook = {
      select: 'SELECT * WHERE { ?s ?p ?o }',
      predicates: [],
      combine: 'AND'
    }

    expect(() => KnowledgeHookSchema.parse(hook)).toThrow('Hook ID is required')
  })

  it('rejects hook with empty predicates', () => {
    const hook = {
      id: 'test',
      select: 'SELECT * WHERE { ?s ?p ?o }',
      predicates: [],
      combine: 'AND'
    }

    expect(() => KnowledgeHookSchema.parse(hook)).toThrow('At least one predicate required')
  })
})
```

**Integration Tests** (API route validation):
```javascript
// test/e2e/api/hooks.test.mjs
import { describe, it, expect } from 'vitest'
import { setup, $fetch } from '@nuxt/test-utils/e2e'

describe('POST /api/hooks', async () => {
  await setup({ server: true })

  it('rejects invalid hook', async () => {
    const response = await $fetch('/api/hooks', {
      method: 'POST',
      body: { id: '' },  // Invalid: empty ID
      ignoreResponseError: true
    })

    expect(response.status).toBe(400)
    expect(response.data.message).toContain('Hook ID is required')
  })
})
```

## References

- [Zod Documentation](https://zod.dev/)
- [Zod Error Handling](https://zod.dev/ERROR_HANDLING)
- [TypeScript Type Inference with Zod](https://zod.dev/?id=type-inference)

---

**Status**: Awaiting Hive consensus approval
**Implementation**: Blocked until approved
**Next Review**: After production deployment
