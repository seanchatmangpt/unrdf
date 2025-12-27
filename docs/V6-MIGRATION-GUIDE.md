# UNRDF v6 Migration Guide
## Upgrading from v5.x to v6.0

**Target Audience:** Existing UNRDF v5.x users
**Time to Migrate:** ~1 hour for typical project
**Difficulty:** ⭐⭐⚠️ Moderate (breaking changes present)
**Status:** v6.0.0-alpha.1

---

## 🚨 Before You Start

**IMPORTANT:** UNRDF v6.0 is currently in **ALPHA**. This migration guide covers known changes, but the v6 API may still evolve.

### Pre-Migration Checklist

- [ ] **Backup your project** (`git commit` or `git tag v5-backup`)
- [ ] **Review breaking changes** (see [V6-BREAKING-CHANGES.md](V6-BREAKING-CHANGES.md))
- [ ] **Test in development** environment first
- [ ] **Update Node.js** to 18.0.0 or higher (required for v6)
- [ ] **Read package consolidation guide** ([V6-PACKAGE-CONSOLIDATION.md](V6-PACKAGE-CONSOLIDATION.md))

---

## Migration Strategy

### Three Migration Paths

| Path | Best For | Effort | Risk |
|------|----------|--------|------|
| **A: Clean Install** | New projects, small codebases | Low | Low |
| **B: Gradual Migration** | Large projects, production systems | Medium | Low |
| **C: v5-compat Layer** | Legacy support required | High | Medium |

**Recommendation:** Path B (Gradual Migration) for most users.

---

## Path A: Clean Install (Recommended for New Projects)

### Step 1: Remove v5 Packages

```bash
# Uninstall all v5 packages
npm uninstall \
  @unrdf/core \
  @unrdf/hooks \
  @unrdf/federation \
  @unrdf/streaming \
  @unrdf/knowledge-engine \
  @unrdf/browser \
  @unrdf/composables \
  @unrdf/cli

# Or using pnpm
pnpm remove @unrdf/core @unrdf/hooks @unrdf/federation @unrdf/streaming @unrdf/knowledge-engine @unrdf/browser @unrdf/composables @unrdf/cli
```

### Step 2: Install v6 Packages

**⚠️ NOTE:** V6 package structure is under consolidation. Verify current recommendations in [V6-PACKAGE-CONSOLIDATION.md](V6-PACKAGE-CONSOLIDATION.md).

```bash
# Minimal v6 installation (recommended)
npm install @unrdf/v6-core @unrdf/oxigraph

# Or full stack (if available)
npm install @unrdf/v6-core @unrdf/oxigraph @unrdf/hooks @unrdf/v6-compat
```

### Step 3: Update Imports

**Before (v5):**
```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';
```

**After (v6):**
```javascript
// ⚠️ VERIFY: Actual v6 import may differ
import { createKnowledgeSubstrateCore } from '@unrdf/v6-core';

// OR if using compatibility layer:
import { createKnowledgeSubstrateCore } from '@unrdf/v6-compat';
```

### Step 4: Test Your Application

```bash
npm test
npm run build
npm start
```

---

## Path B: Gradual Migration (Recommended for Production)

### Phase 1: Audit Your Dependencies (30 minutes)

1. **List all UNRDF dependencies:**

```bash
npm ls | grep @unrdf
```

2. **Identify which packages you use:**

```javascript
// Create audit.mjs
import fs from 'fs';
import path from 'path';

const srcFiles = getAllJsFiles('./src'); // Implement this
const unrdfImports = new Set();

srcFiles.forEach(file => {
  const content = fs.readFileSync(file, 'utf-8');
  const matches = content.match(/from ['"]@unrdf\/[^'"]+['"]/g) || [];
  matches.forEach(m => {
    const pkg = m.match(/@unrdf\/([^'"]+)/)[0];
    unrdfImports.add(pkg);
  });
});

console.log('UNRDF packages in use:', [...unrdfImports]);
```

3. **Map to v6 equivalents:**

| v5 Package | v6 Equivalent | Notes |
|------------|---------------|-------|
| `@unrdf/core` | `@unrdf/v6-core` (?) | **VERIFY:** Check actual v6 package name |
| `@unrdf/hooks` | `@unrdf/hooks` (unchanged?) | **VERIFY:** May be compatible |
| `@unrdf/oxigraph` | `@unrdf/oxigraph` (unchanged?) | **VERIFY:** May be compatible |
| `@unrdf/federation` | `@unrdf/v6-core` (merged?) | **VERIFY:** Package consolidation TBD |
| `@unrdf/streaming` | `@unrdf/v6-core` (merged?) | **VERIFY:** Package consolidation TBD |

**⚠️ IMPORTANT:** The mapping above is PRELIMINARY. Consult [V6-PACKAGE-CONSOLIDATION.md](V6-PACKAGE-CONSOLIDATION.md) for actual v6 package structure.

### Phase 2: Install v6 Alongside v5 (15 minutes)

**Strategy:** Run both v5 and v6 side-by-side temporarily.

```json
// package.json
{
  "dependencies": {
    // v5 (keep temporarily)
    "@unrdf/core": "^5.0.1",

    // v6 (add with alias)
    "@unrdf/v6-core": "^6.0.0-alpha.1",
    "@unrdf/v6-compat": "^6.0.0-alpha.1"
  }
}
```

### Phase 3: Migrate Module by Module (Varies)

**Example: Migrate one module**

**Before (`src/knowledge-store.mjs` - v5):**
```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';
import { createStore } from '@unrdf/oxigraph';

export async function initializeStore() {
  const core = await createKnowledgeSubstrateCore();
  const store = createStore();
  return { core, store };
}
```

**After (`src/knowledge-store.mjs` - v6):**
```javascript
// ⚠️ VERIFY: Actual v6 API may differ
import { createKnowledgeSubstrateCore } from '@unrdf/v6-core';
import { createStore } from '@unrdf/oxigraph';

export async function initializeStore() {
  const core = await createKnowledgeSubstrateCore({
    version: 'v6', // If version flag exists
    // Other v6-specific options
  });
  const store = createStore();
  return { core, store };
}
```

**Test module in isolation:**
```bash
node --test src/knowledge-store.test.mjs
```

### Phase 4: Update Tests (Varies)

**Check for breaking test changes:**

**v5 Test:**
```javascript
import { expect } from 'vitest';
import { createKnowledgeSubstrateCore } from '@unrdf/core';

test('should create store', async () => {
  const core = await createKnowledgeSubstrateCore();
  expect(core).toBeDefined();
});
```

**v6 Test (if API changed):**
```javascript
import { expect } from 'vitest';
import { createKnowledgeSubstrateCore } from '@unrdf/v6-core';

test('should create store', async () => {
  // ⚠️ VERIFY: v6 may have different initialization
  const core = await createKnowledgeSubstrateCore();
  expect(core).toBeDefined();
  // Add v6-specific assertions if needed
});
```

### Phase 5: Remove v5 Dependencies (15 minutes)

**Only after all modules migrated and tests passing:**

```bash
npm uninstall @unrdf/core
# Uninstall other v5 packages
```

---

## Path C: v5-compat Layer (For Legacy Support)

**Use Case:** You need to support BOTH v5 and v6 APIs simultaneously.

### Setup

```bash
npm install @unrdf/v6-compat
```

### Usage

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/v6-compat';

// v6-compat provides backwards-compatible API
const core = await createKnowledgeSubstrateCore();

// Works with v5 code
const store = core.parseRdf(turtleData);
const results = await core.query(store, sparqlQuery);
```

**⚠️ NOTE:** `@unrdf/v6-compat` package status unknown. Verify availability in [V6-PACKAGE-CONSOLIDATION.md](V6-PACKAGE-CONSOLIDATION.md).

---

## Breaking Changes (Summary)

**Full list:** See [V6-BREAKING-CHANGES.md](V6-BREAKING-CHANGES.md)

### Known Breaking Changes

1. **Package Structure Changes**
   - `@unrdf/core` → `@unrdf/v6-core` (?)
   - Multiple packages consolidated (TBD)

2. **API Changes**
   - `createStore()` signature may have changed
   - Some v5 functions deprecated (list TBD)

3. **Import Paths**
   - Package names changed
   - Internal module paths may have changed

4. **Behavior Changes**
   - Query execution may have performance changes
   - Error handling may be different

**⚠️ CRITICAL:** This list is INCOMPLETE. The actual v6 changes need to be documented after analyzing v6-core and v6-compat packages.

---

## Common Migration Issues & Solutions

### Issue 1: "Cannot find module '@unrdf/core'"

**Problem:** Import path changed in v6.

**Solution:**
```bash
# Update import
- import { X } from '@unrdf/core';
+ import { X } from '@unrdf/v6-core';
```

### Issue 2: "Function createStore is not exported"

**Problem:** API changed or function moved.

**Solution:** Check [V6-API-REFERENCE.md](V6-API-REFERENCE.md) for new API.

### Issue 3: "Tests failing after upgrade"

**Problem:** Breaking changes in behavior.

**Solution:**
1. Check [V6-BREAKING-CHANGES.md](V6-BREAKING-CHANGES.md)
2. Update test assertions for new behavior
3. Use v6-compat layer if needed

### Issue 4: "Performance regression"

**Problem:** v6 may have different performance characteristics.

**Solution:**
1. Check [V6-PERFORMANCE-BENCHMARKS.md](V6-PERFORMANCE-BENCHMARKS.md)
2. Profile your application
3. Report issue if unexpected

---

## Rollback Plan

### If Migration Fails

**Immediate Rollback:**

```bash
# Restore from backup
git reset --hard v5-backup
npm install

# Or restore package.json
git checkout HEAD~1 -- package.json
npm install
```

**Gradual Rollback:**

```bash
# Uninstall v6
npm uninstall @unrdf/v6-core @unrdf/v6-compat

# Reinstall v5
npm install @unrdf/core@^5.0.1
```

---

## Post-Migration Checklist

After successful migration:

- [ ] All tests passing
- [ ] Application runs without errors
- [ ] Performance acceptable
- [ ] No console warnings/errors
- [ ] Documentation updated (README, etc.)
- [ ] Team trained on v6 changes
- [ ] Monitoring in place (for production)

---

## Migration Timeline Example

**Small Project (~10 files):**
- Audit: 15 min
- Migration: 30 min
- Testing: 15 min
- **Total:** ~1 hour

**Medium Project (~100 files):**
- Audit: 1 hour
- Migration: 4 hours
- Testing: 2 hours
- **Total:** ~1 day

**Large Project (~1000 files):**
- Audit: 4 hours
- Migration: 2-3 days
- Testing: 1 day
- **Total:** ~1 week

---

## Getting Help

### Resources

- **V6 Breaking Changes:** [V6-BREAKING-CHANGES.md](V6-BREAKING-CHANGES.md)
- **V6 API Reference:** [V6-API-REFERENCE.md](V6-API-REFERENCE.md)
- **V6 Package Guide:** [V6-PACKAGE-CONSOLIDATION.md](V6-PACKAGE-CONSOLIDATION.md)
- **V6 Examples:** [EXAMPLES.md](EXAMPLES.md)

### Support

- **GitHub Issues:** [github.com/unrdf/unrdf/issues](https://github.com/unrdf/unrdf/issues)
- **Discussions:** [github.com/unrdf/unrdf/discussions](https://github.com/unrdf/unrdf/discussions)
- **Discord:** (if available)

---

## FAQ

### Q: Is v6 production-ready?

**A:** v6.0.0-alpha.1 is in ALPHA. Use in production at your own risk. Wait for stable v6.0.0 release for production use.

### Q: Will v5 be supported?

**A:** Support timeline TBD. Check project roadmap for details.

### Q: Can I use v5 and v6 together?

**A:** Yes, temporarily during migration (see Path B). Not recommended long-term.

### Q: What if I can't migrate right away?

**A:** Stay on v5 until ready. v5 support timeline TBD.

### Q: Are there automated migration tools?

**A:** Not currently. Migration is manual. Consider creating codemod scripts for large projects.

---

## Appendix A: Code Migration Patterns

### Pattern 1: Store Creation

**v5:**
```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';
const core = await createKnowledgeSubstrateCore();
```

**v6 (VERIFY):**
```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/v6-core';
const core = await createKnowledgeSubstrateCore(/* options may differ */);
```

### Pattern 2: Query Execution

**v5:**
```javascript
const results = await core.query(store, sparqlQuery);
```

**v6 (VERIFY):**
```javascript
// May be unchanged, but verify behavior
const results = await core.query(store, sparqlQuery);
```

### Pattern 3: Hooks

**v5:**
```javascript
import { defineHook } from '@unrdf/hooks';
const myHook = defineHook({ /* config */ });
```

**v6 (VERIFY):**
```javascript
// May be unchanged if @unrdf/hooks is v6-compatible
import { defineHook } from '@unrdf/hooks';
const myHook = defineHook({ /* config may differ */ });
```

---

## Appendix B: Version Compatibility

| Component | v5 | v6 | Compatible? |
|-----------|----|----|-------------|
| **Node.js 18** | ✅ | ✅ | Yes |
| **Node.js 20** | ✅ | ✅ | Yes |
| **Node.js 22** | ⚠️ | ✅ | v6 better support |
| **Browser (Chrome 90+)** | ✅ | ❓ | TBD |
| **React 18** | ✅ | ❓ | TBD |
| **Vue 3** | ✅ | ❓ | TBD |

---

## Appendix C: Deprecation Timeline

**⚠️ TBD:** Deprecation timeline not yet established.

Recommended:
- v5 maintenance: 6 months after v6 stable release
- v5 security fixes: 12 months after v6 stable release
- v5 end-of-life: 18 months after v6 stable release

---

## Document Status

**Version:** 1.0.0-draft
**Last Updated:** 2025-12-27
**Status:** ⚠️ DRAFT - v6 API not finalized
**Next Review:** When v6.0.0-beta.1 released

**⚠️ CRITICAL DISCLAIMER:** This migration guide is based on PRELIMINARY analysis. Actual v6 API and package structure may differ. Verify all migration steps against official v6 documentation when available.

**TODO for Documentation Team:**
1. Analyze v6-core package actual API
2. Analyze v6-compat package (if it exists)
3. Document actual breaking changes
4. Test migration steps on real v5 project
5. Update this guide with verified information

---

**Migration Guide Created By:** Agent 9 - Documentation Specialist
**Based On:** v6.0.0-alpha.1 workspace analysis
**Confidence Level:** MEDIUM (pending v6 API verification)
