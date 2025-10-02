# Sidecar Dashboard Architecture Summary

**Full Architecture**: See `/docs/architecture/sidecar-dashboard-architecture.md`

---

## Quick Reference

### Key Decisions

1. **Hybrid SSR/API Mode**: Enable pages + existing API routes
2. **MJS/JSDoc + Zod**: No TypeScript compilation, runtime validation
3. **Composables**: No Pinia, use Vue composables for state
4. **80/20 Phasing**: MVP in 2 weeks, advanced features deferred

### Directory Structure

```
sidecar/
├── app/                      # NEW: Nuxt UI components
│   ├── components/
│   │   ├── hooks/
│   │   ├── policies/
│   │   ├── transactions/
│   │   └── shared/
│   ├── composables/          # State management
│   ├── layouts/
│   ├── pages/
│   ├── schemas/              # Zod validation
│   └── utils/
├── server/                   # EXISTING: API routes
│   ├── api/
│   ├── middleware/
│   └── tasks/                # NEW: Scheduled tasks
└── test/
    ├── unit/
    ├── nuxt/                 # NEW: Component tests
    └── e2e/
```

### Component Architecture

**Pages**:
- `/` - Dashboard home (stats)
- `/hooks` - Knowledge Hooks CRUD
- `/policies` - Policy Packs CRUD
- `/transactions` - Transaction log
- `/lockchain` - Audit trail
- `/settings/*` - Configuration

**Key Components**:
- `HooksList.vue` - Hooks table
- `HookEditor.vue` - Hook form with Zod validation
- `PolicyEditor.vue` - SHACL editor
- `TransactionLog.vue` - Real-time transaction stream
- `SPARQLEditor.vue` - Query builder

### TypeScript → MJS Conversion

```javascript
// TypeScript interface → JSDoc + Zod
/** @typedef {Object} KnowledgeHook
 *  @property {string} id
 *  @property {string} select
 */

const KnowledgeHookSchema = z.object({
  id: z.string(),
  select: z.string()
})

/** @typedef {z.infer<typeof KnowledgeHookSchema>} KnowledgeHook */
```

### State Management Pattern

```javascript
// app/composables/useKnowledgeHooks.mjs
export const useKnowledgeHooks = () => {
  const hooks = ref([])
  const loading = ref(false)

  const fetchHooks = async () => {
    const response = await $fetch('/api/hooks')
    hooks.value = response.hooks.map(h => HookSchema.parse(h))
  }

  return { hooks, loading, fetchHooks }
}
```

### Phased Implementation

**Phase 1 (Week 1)** - Core Infrastructure
- Enable SSR, install @nuxt/ui
- Adapt dashboard layout
- Create basic pages (read-only)
- Build composables

**Phase 2 (Week 2)** - CRUD Operations
- Hook/policy forms with validation
- Delete operations
- Toast notifications

**Phase 3 (Week 3)** - Advanced Features
- Real-time SSE updates
- Lockchain browser
- SPARQL query UI

**Phase 4 (Deferred)** - Polish
- Analytics charts
- Themes
- Help docs

### Configuration Changes

```javascript
// nuxt.config.mjs
export default defineNuxtConfig({
  ssr: true,        // CHANGED: was false
  pages: true,      // CHANGED: was false
  modules: [
    '@nuxt/ui'      // ADDED
  ],
  css: ['~/assets/css/main.css']  // ADDED
})
```

### Testing Strategy

- **Unit**: 70% - Composables, utils, schemas
- **Nuxt**: 20% - Components with @nuxt/test-utils
- **E2E**: 10% - Full flows with Playwright

### Security

- JWT in HTTP-only cookies
- RBAC on all routes
- Zod validation on all forms
- HTTPS enforced
- mTLS optional

### Performance Targets

- Page Load: < 1s
- API Response: < 200ms
- Form Validation: < 50ms
- SSE Latency: < 100ms

---

## Next Steps

1. **Hive Consensus Vote** on this architecture
2. **Phase 1 Implementation** by frontend-dev agent
3. **Component Tests** by test-engineer agent
4. **E2E Validation** before Phase 2

---

**Architecture Version**: 1.0
**Status**: Awaiting Hive Consensus
**Full Document**: `/docs/architecture/sidecar-dashboard-architecture.md`
