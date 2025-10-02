# ADR-001: Enable SSR and Pages Mode in Nuxt

**Status**: Proposed
**Date**: 2025-10-01
**Decision Maker**: System Architecture Agent
**Context**: Dashboard UI requires server-rendered pages

---

## Context

The sidecar Nuxt application is currently configured in API-only mode:
```javascript
{
  ssr: false,
  pages: false
}
```

This configuration is suitable for a headless API server but prevents:
1. Server-rendered dashboard UI
2. File-based routing for pages
3. SEO-friendly content
4. Optimal initial page load performance

## Decision

**We will enable hybrid SSR + API mode** by changing the Nuxt configuration to:
```javascript
{
  ssr: true,    // Changed from false
  pages: true   // Changed from false
}
```

## Rationale

### Why Enable SSR?

1. **Performance**: Server-side rendering provides faster First Contentful Paint (FCP)
   - Client receives fully-rendered HTML on initial load
   - JavaScript hydrates the already-rendered page
   - Target: < 1s FCP vs. 2-3s with client-only rendering

2. **SEO**: Search engines can index dashboard pages
   - Internal documentation can be discovered
   - Accessibility tools can parse content without JavaScript

3. **Progressive Enhancement**: Dashboard works even if JavaScript fails to load
   - Core content visible immediately
   - Interactive features enhance the experience

4. **Nuxt Best Practice**: Nuxt is designed for SSR
   - Leveraging framework strengths
   - Better ecosystem compatibility

### Why Enable Pages?

1. **File-Based Routing**: Natural organization of dashboard views
   - `/pages/hooks/index.vue` → `/hooks` route
   - `/pages/hooks/[id].vue` → `/hooks/:id` route
   - No manual route configuration needed

2. **Code Splitting**: Automatic per-page bundles
   - Users only download JavaScript for visited pages
   - Smaller initial bundle size

3. **Layouts**: Reusable layout components
   - `layouts/dashboard.vue` for authenticated pages
   - `layouts/default.vue` for login page

### Hybrid Mode Benefits

**API Routes + Pages Coexist**:
- Existing `/server/api/` routes remain unchanged
- CLI tools continue to work without modification
- Dashboard UI adds new `/pages/` routes
- No breaking changes to API contracts

## Consequences

### Positive

✅ **Better User Experience**:
- Faster page loads
- SEO-friendly content
- Progressive enhancement

✅ **Developer Experience**:
- File-based routing simplifies code organization
- Nuxt conventions reduce boilerplate
- Auto-imports for components and composables

✅ **Ecosystem Compatibility**:
- Can use Nuxt UI (@nuxt/ui) components
- Access to Nuxt modules ecosystem

### Negative

❌ **Larger Server Footprint**:
- SSR adds memory overhead (Vue renderer)
- Mitigation: Use caching and static generation where possible

❌ **SSR Hydration Complexity**:
- Potential hydration mismatches if not careful
- Mitigation: Follow Vue SSR best practices (no DOM manipulation in setup)

❌ **Build Time Increase**:
- Pages must be rendered at build time
- Mitigation: Use Nitro prerendering strategically

### Neutral

⚖️ **Configuration Changes**:
- Must update `nuxt.config.mjs`
- Must add `app.vue` and layout files
- Existing API routes unaffected

⚖️ **Testing Changes**:
- Component tests need `@nuxt/test-utils`
- E2E tests use Playwright
- API route tests remain unchanged

## Alternatives Considered

### Alternative 1: Keep API-Only Mode, Build Separate SPA

**Approach**:
- Keep sidecar in API-only mode
- Build separate Vue SPA in `/frontend`
- Deploy SPA separately (e.g., Vercel, Netlify)

**Why Not Chosen**:
- Increased deployment complexity (two apps)
- No SSR benefits
- Duplicated authentication logic
- Harder to maintain API/UI consistency

### Alternative 2: Use Nuxt Layers for API-Only Base + Dashboard Layer

**Approach**:
- Base layer: API-only mode
- Dashboard layer: SSR + pages
- Compose layers at build time

**Why Not Chosen**:
- Over-engineering for current use case
- Nuxt layers are experimental
- Harder to debug and maintain
- No significant benefit over hybrid mode

### Alternative 3: Use Nitro Standalone for API, Separate Nuxt App for UI

**Approach**:
- Nitro-only API server (no Nuxt)
- Separate Nuxt app for dashboard

**Why Not Chosen**:
- Duplicates server configuration (OTEL, auth, mTLS)
- Two codebases to maintain
- Harder to keep API and UI in sync

## Implementation Plan

### Step 1: Update Configuration (15 minutes)

**File**: `sidecar/nuxt.config.mjs`
```javascript
export default defineNuxtConfig({
  ssr: true,
  pages: true,
  // ... rest of config
})
```

### Step 2: Create Layouts (30 minutes)

**Files**:
- `app/layouts/default.vue`
- `app/layouts/dashboard.vue`

### Step 3: Create Root App (15 minutes)

**File**: `app/app.vue`
```vue
<template>
  <NuxtPage />
</template>
```

### Step 4: Test (30 minutes)

```bash
pnpm run dev
# Visit http://localhost:3000/
# Should see Nuxt welcome page (no 404)

# Test API routes still work
curl http://localhost:3000/api/transaction
# Should return 200 OK
```

## Validation

### Success Criteria

- ✅ `pnpm run dev` starts without errors
- ✅ `/` route returns HTML (not 404)
- ✅ `/api/transaction` still returns 200 OK
- ✅ Nuxt DevTools shows "Pages" tab
- ✅ Build completes: `pnpm run build`

### Performance Baseline

**Before (API-only)**:
- Bundle size: 0 KB (no client bundle)
- Server memory: ~50 MB

**After (SSR + Pages)**:
- Target bundle size: < 500 KB (gzipped)
- Target server memory: < 150 MB
- Target FCP: < 1s

## References

- [Nuxt SSR Documentation](https://nuxt.com/docs/guide/concepts/rendering#universal-rendering)
- [Nuxt Pages Documentation](https://nuxt.com/docs/guide/directory-structure/pages)
- [Vue SSR Guide](https://vuejs.org/guide/scaling-up/ssr.html)

---

**Status**: Awaiting Hive consensus approval
**Implementation**: Blocked until approved
**Next Review**: After Phase 1 completion
