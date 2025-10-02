# ADR-002: Composables Over Pinia for State Management

**Status**: Proposed
**Date**: 2025-10-01
**Decision Maker**: System Architecture Agent
**Context**: Dashboard needs reactive state for hooks, policies, transactions

---

## Context

The dashboard UI requires state management for:
- Knowledge hooks (list, create, update, delete)
- Policies (list, create, update, delete)
- Transactions (list, query)
- Runtime status (polling)
- User authentication (login state, JWT token)

Common state management options for Vue 3 / Nuxt 4:
1. **Pinia** (official Vue state management library)
2. **Vue Composables** (native Vue 3 Composition API)
3. **Vuex** (deprecated, not recommended for Vue 3)

## Decision

**We will use Vue composables for state management** instead of adding Pinia as a dependency.

**Implementation**:
```javascript
// app/composables/useKnowledgeHooks.mjs
export const useKnowledgeHooks = () => {
  const hooks = ref([])
  const loading = ref(false)
  const error = ref(null)

  const fetchHooks = async () => {
    loading.value = true
    try {
      const response = await $fetch('/api/hooks')
      hooks.value = response.hooks
    } catch (err) {
      error.value = err.message
    } finally {
      loading.value = false
    }
  }

  return { hooks, loading, error, fetchHooks }
}
```

## Rationale

### Why Composables?

1. **Zero Configuration**: Nuxt 4 auto-imports composables from `app/composables/`
   - No need to install Pinia
   - No need to configure store modules
   - Works out of the box

2. **Smaller Bundle Size**:
   - Pinia adds ~30 KB (minified + gzipped)
   - Composables use built-in Vue reactivity (0 KB overhead)
   - Better for dashboard performance

3. **Simpler Mental Model**:
   - Composables are just functions that return reactive state
   - No concepts like "stores", "modules", "getters", "actions"
   - Easier for new contributors to understand

4. **Better TypeScript/JSDoc Support**:
   - Composables have explicit return types
   - IDEs provide better auto-completion
   - JSDoc annotations work naturally

5. **Sufficient for Dashboard Use Case**:
   - Dashboard doesn't need complex state management (no deeply nested state)
   - Most state is fetched from API and displayed
   - No shared state between unrelated components

### Why Not Pinia?

**Pinia is excellent, but overkill for this project**:

❌ **Adds Dependency**:
- Need to install `pinia`
- Need to configure in `nuxt.config.mjs`
- One more package to maintain

❌ **Increases Complexity**:
- Must create store files (`stores/hooks.mjs`)
- Must understand Pinia API (defineStore, storeToRefs, etc.)
- More boilerplate for simple CRUD operations

❌ **DevTools Not Critical**:
- Pinia DevTools provide time-travel debugging
- Dashboard state is simple enough to debug with Vue DevTools

**When Would We Choose Pinia?**
- If state becomes deeply nested and complex
- If we need time-travel debugging
- If we need SSR state hydration edge cases
- If we have 10+ stores with complex interactions

**Current State**: Dashboard has 5-6 simple state domains (hooks, policies, transactions, runtime, auth). Composables are sufficient.

## Consequences

### Positive

✅ **Zero Configuration**:
- No setup required
- Nuxt auto-imports composables
- Works immediately

✅ **Smaller Bundle**:
- No additional dependencies
- Uses Vue built-in reactivity
- Better performance

✅ **Easier to Test**:
- Composables are just functions
- Can import and test directly
- No store mocking required

✅ **Type Safety**:
- JSDoc annotations for return types
- Better IDE auto-completion

### Negative

❌ **No DevTools Time-Travel**:
- Cannot rewind state changes in DevTools
- Mitigation: Use Vue DevTools for reactive state inspection

❌ **Manual State Sharing**:
- If two components need same state, must pass composable result
- Mitigation: Call composable in parent and pass props

❌ **No Global Store**:
- Each composable call creates new reactive state
- Mitigation: Use singleton pattern if needed

### Neutral

⚖️ **Refactoring Path**:
- If complexity grows, can migrate to Pinia incrementally
- Composables can wrap Pinia stores
- No breaking changes to components

## Implementation

### Pattern: Composable with CRUD Operations

```javascript
// app/composables/useKnowledgeHooks.mjs

import { ref, computed } from 'vue'
import { KnowledgeHookSchema } from '~/schemas/hooks.mjs'

/**
 * Knowledge Hooks composable
 * @returns {Object} Hook management state and methods
 */
export const useKnowledgeHooks = () => {
  // State
  const hooks = ref([])
  const loading = ref(false)
  const error = ref(null)

  // Actions
  const fetchHooks = async () => {
    loading.value = true
    error.value = null
    try {
      const response = await $fetch('/api/hooks')
      hooks.value = response.hooks.map(h => KnowledgeHookSchema.parse(h))
    } catch (err) {
      error.value = err.message
    } finally {
      loading.value = false
    }
  }

  const createHook = async (hookData) => {
    const validated = KnowledgeHookSchema.parse(hookData)
    const response = await $fetch('/api/hooks', {
      method: 'POST',
      body: validated
    })
    await fetchHooks() // Refresh list
    return response
  }

  const deleteHook = async (id) => {
    await $fetch(`/api/hooks/${id}`, { method: 'DELETE' })
    await fetchHooks()
  }

  // Computed
  const activeHooks = computed(() =>
    hooks.value.filter(h => !h.disabled)
  )

  return {
    // State
    hooks,
    loading,
    error,

    // Actions
    fetchHooks,
    createHook,
    deleteHook,

    // Computed
    activeHooks
  }
}
```

### Pattern: Singleton State (Shared Across Components)

**If multiple components need the same state instance**:

```javascript
// app/composables/useAuth.mjs

const _user = ref(null)
const _token = ref(null)

/**
 * Authentication composable (singleton)
 * @returns {Object} Auth state and methods
 */
export const useAuth = () => {
  const login = async (username, password) => {
    const response = await $fetch('/api/auth/login', {
      method: 'POST',
      body: { username, password }
    })
    _user.value = response.user
    _token.value = response.token
  }

  const logout = () => {
    _user.value = null
    _token.value = null
  }

  return {
    user: readonly(_user),      // Read-only to prevent external mutation
    token: readonly(_token),
    login,
    logout
  }
}
```

**Benefits**:
- All components calling `useAuth()` share same `_user` and `_token`
- Singleton pattern without Pinia
- Works across SSR and client

## Alternatives Considered

### Alternative 1: Use Pinia

**Approach**:
```javascript
// stores/hooks.mjs
import { defineStore } from 'pinia'

export const useHooksStore = defineStore('hooks', {
  state: () => ({
    hooks: [],
    loading: false,
    error: null
  }),
  actions: {
    async fetchHooks() {
      this.loading = true
      try {
        const response = await $fetch('/api/hooks')
        this.hooks = response.hooks
      } catch (err) {
        this.error = err.message
      } finally {
        this.loading = false
      }
    }
  }
})
```

**Why Not Chosen**:
- Adds dependency (Pinia)
- More boilerplate
- Overkill for simple CRUD state

**When to Reconsider**:
- If state becomes deeply nested
- If time-travel debugging is needed
- If 10+ stores are required

### Alternative 2: Global State with Provide/Inject

**Approach**:
```javascript
// app.vue
const hooks = ref([])
provide('hooks', hooks)

// Any component
const hooks = inject('hooks')
```

**Why Not Chosen**:
- Less explicit than composables
- Harder to type with JSDoc
- Not Nuxt convention

### Alternative 3: Vuex

**Why Not Chosen**:
- Vuex is deprecated for Vue 3
- Pinia is the official replacement
- No reason to use legacy library

## Validation

### Success Criteria

- ✅ Composables auto-imported by Nuxt
- ✅ State reactive across component re-renders
- ✅ API calls triggered correctly
- ✅ Error handling works
- ✅ Loading states display correctly

### Test Plan

**Unit Tests** (`test/unit/composables/useKnowledgeHooks.test.mjs`):
```javascript
import { describe, it, expect, vi } from 'vitest'
import { useKnowledgeHooks } from '~/composables/useKnowledgeHooks'

describe('useKnowledgeHooks', () => {
  it('fetches hooks from API', async () => {
    global.$fetch = vi.fn(() => Promise.resolve({ hooks: [] }))

    const { hooks, fetchHooks } = useKnowledgeHooks()
    await fetchHooks()

    expect(hooks.value).toEqual([])
    expect(global.$fetch).toHaveBeenCalledWith('/api/hooks')
  })
})
```

**Component Tests** (`test/nuxt/pages/hooks/index.test.mjs`):
```javascript
import { mountSuspended } from '@nuxt/test-utils/runtime'
import HooksIndexPage from '~/pages/hooks/index.vue'

describe('Hooks Index Page', () => {
  it('calls useKnowledgeHooks on mount', async () => {
    const wrapper = await mountSuspended(HooksIndexPage)

    // Verify fetchHooks was called
    // Verify hooks list is rendered
  })
})
```

## Migration to Pinia (If Needed)

**If complexity grows, migration is straightforward**:

**Step 1**: Install Pinia
```bash
pnpm add pinia @pinia/nuxt
```

**Step 2**: Add to `nuxt.config.mjs`
```javascript
modules: ['@pinia/nuxt']
```

**Step 3**: Wrap composable in Pinia store
```javascript
// stores/hooks.mjs
import { defineStore } from 'pinia'
import { useKnowledgeHooks } from '~/composables/useKnowledgeHooks'

export const useHooksStore = defineStore('hooks', () => {
  return useKnowledgeHooks()  // Reuse existing composable
})
```

**No changes required in components** (can continue using `useKnowledgeHooks()`).

## References

- [Vue 3 Composition API](https://vuejs.org/guide/reusability/composables.html)
- [Nuxt Composables](https://nuxt.com/docs/guide/directory-structure/composables)
- [Pinia Documentation](https://pinia.vuejs.org/)

---

**Status**: Awaiting Hive consensus approval
**Implementation**: Blocked until approved
**Next Review**: After 6 months (or when state complexity increases)
