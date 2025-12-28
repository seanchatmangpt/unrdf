# UNRDF v6 Migration Guide

**Version**: 6.0.0
**Date**: 2025-12-27
**Estimated Migration Time**: 2-6 weeks

---

## Overview

This guide helps you migrate from **UNRDF v5** to **UNRDF v6**.

**Key Changes**:
- 12 breaking changes (see `/docs/v6/BREAKING-CHANGES.md`)
- 53% package reduction (54 → 25 packages)
- 70% auto-migration coverage

**Migration Path**:
1. **Preparation** (Week 1-2): Review changes, update dependencies
2. **Automated Migration** (Week 3-4): Run migration tool
3. **Manual Migration** (Week 5-6): Fix remaining issues
4. **Validation** (Week 7-8): Test thoroughly
5. **Production Rollout** (Week 9-10): Deploy incrementally

---

## Prerequisites

**Required**:
- Node.js ≥20.0.0 (upgrade if on 18.x)
- pnpm ≥7.0.0 (or npm ≥8.0.0)
- UNRDF v5.0.0 or later

**Recommended**:
- Git (for rollback if needed)
- CI/CD pipeline (for automated testing)
- Staging environment (for validation)

---

## Step 1: Preparation (Week 1-2)

### 1.1 Update Dependencies

```bash
# Update UNRDF packages
pnpm update '@unrdf/*@^6.0.0'

# Or manually in package.json
{
  "dependencies": {
    "@unrdf/core": "^6.0.0",
    "@unrdf/kgc-4d": "^6.0.0",
    "@unrdf/cli": "^6.0.0"
  }
}
```

### 1.2 Install Migration Tool

```bash
npm install -g @unrdf/migrate-v6
```

### 1.3 Analyze Codebase

```bash
# Generate migration report
npx @unrdf/migrate-v6 analyze .

# Output: migration-report.md
```

**Example Report**:
```
UNRDF v6 Migration Report
=========================

Total Files: 250
Affected Files: 180 (72%)

Breaking Changes Detected:
- BC-1 (Package Consolidation): 120 files
- BC-3 (SPARQL Signature): 45 files
- BC-12 (ESM-Only): 15 files

Estimated Migration Time: 3-4 weeks
Auto-Migration Coverage: 68%
```

### 1.4 Create Backup

```bash
# Commit current state
git add .
git commit -m "Pre-v6 migration checkpoint"

# Create migration branch
git checkout -b migration/v6
```

---

## Step 2: Automated Migration (Week 3-4)

### 2.1 Run Migration Tool

```bash
# Dry run (preview changes)
npx @unrdf/migrate-v6 migrate . --dry-run

# Apply automated migrations
npx @unrdf/migrate-v6 migrate . --auto
```

**What Gets Migrated Automatically**:
- ✅ Package imports (BC-1)
- ✅ Store creation (BC-2, partial)
- ✅ SPARQL signatures (BC-3)
- ✅ Capsule formats (BC-5)
- ✅ CLI commands (BC-7)
- ✅ ESM conversion (BC-12, partial)

### 2.2 Review Changes

```bash
# View diff
git diff

# Review file by file
git diff --stat
```

### 2.3 Fix Linting Errors

```bash
# Run linter
npm run lint

# Auto-fix where possible
npm run lint:fix
```

### 2.4 Commit Automated Changes

```bash
git add .
git commit -m "chore: apply automated v6 migrations"
```

---

## Step 3: Manual Migration (Week 5-6)

### 3.1 Migrate Hook Registrations (BC-4)

**Before**:
```javascript
import { registerHook } from '@unrdf/hooks'

registerHook({
  name: 'validate-person',
  trigger: 'INSERT',
  pattern: '?s a foaf:Person .',
  run: (event) => { /* ... */ }
})
```

**After**:
```javascript
// Register per-store
store.registerHook({
  name: 'validate-person',
  trigger: 'INSERT',
  pattern: '?s a foaf:Person .',
  run: (event) => { /* ... */ }
})
```

**Steps**:
1. Find all `registerHook()` calls: `grep -r "registerHook" src/`
2. Determine correct store for each hook
3. Move registration to store instance
4. Test hook execution

### 3.2 Update Federation Configs (BC-6)

**Before**:
```javascript
const federation = createFederation({
  nodes: ['node1', 'node2', 'node3'],
  consistency: 'eventual'
})
```

**After**:
```javascript
const federation = createFederation({
  nodes: ['node1', 'node2', 'node3'],
  consensus: 'raft'
})
```

**Steps**:
1. Update federation configs
2. Deploy nodes incrementally
3. Verify leader election
4. Test failover

### 3.3 Convert CommonJS to ESM (BC-12)

**Before (`index.js`)**:
```javascript
const { createStore } = require('@unrdf/core')

module.exports = {
  initStore() {
    return createStore()
  }
}
```

**After (`index.mjs`)**:
```javascript
import { createStore } from '@unrdf/core'

export function initStore() {
  return createStore()
}
```

**Steps**:
1. Add `"type": "module"` to package.json
2. Rename `.js` → `.mjs` (if not using `"type": "module"`)
3. Replace `require()` → `import`
4. Replace `module.exports` → `export`

### 3.4 Fix Zod Validation Errors (BC-11)

**Before**:
```javascript
const store = createStore({ invalid: 'option' })
// Silent failure
```

**After**:
```javascript
const store = createStore({ backend: 'oxigraph' })
// Throws ZodError if invalid
```

**Steps**:
1. Run tests to find validation errors
2. Fix invalid inputs (errors are descriptive)
3. Update code to match Zod schemas

### 3.5 Commit Manual Changes

```bash
git add .
git commit -m "chore: apply manual v6 migrations"
```

---

## Step 4: Validation (Week 7-8)

### 4.1 Run Test Suite

```bash
# Run all tests
npm test

# Target: 100% pass rate
```

**Common Test Failures**:
- Store parameter missing in SPARQL calls
- Hook registration scope issues
- Invalid Zod inputs

### 4.2 OTEL Validation

```bash
# Run OTEL validation
node validation/run-all.mjs comprehensive

# Target: ≥80/100 score
grep "Score:" validation-output.log
```

### 4.3 Performance Benchmarks

```bash
# Run benchmarks
npm run benchmark:regression

# Compare with v5 baseline
npm run benchmark:compare
```

**Expected Results**:
- SPARQL queries: 40-60% faster
- Memory usage: 40-60% lower
- No regressions

### 4.4 Deploy to Staging

```bash
# Build production bundle
npm run build

# Deploy to staging
npm run deploy:staging

# Smoke tests
npm run test:integration:staging
```

### 4.5 Verify Migration Completeness

```bash
# Check for v5 patterns
npx @unrdf/migrate-v6 verify .

# Should output: "Migration complete. No v5 patterns detected."
```

---

## Step 5: Production Rollout (Week 9-10)

### 5.1 Canary Deployment

```bash
# Deploy to 10% of servers
npm run deploy:canary

# Monitor for 24-48 hours
npm run monitor:canary
```

**Metrics to Watch**:
- Error rate (should not increase)
- Response time (should decrease 40-60%)
- Memory usage (should decrease 40-60%)

### 5.2 Full Rollout

```bash
# If canary successful, deploy to all servers
npm run deploy:production

# Monitor for 7 days
npm run monitor:production
```

### 5.3 Decommission v5

```bash
# Remove v5 compatibility layer (optional)
npm run cleanup:v5-compat

# Remove old dependencies
npm prune
```

---

## Troubleshooting

### Issue: Tests Failing After Migration

**Symptoms**: Test suite fails with SPARQL errors

**Solution**:
```bash
# Check for missing store parameters
grep -r "query(" src/ | grep -v "query(store"

# Add store parameter
# Before: query('SELECT * ...')
# After:  query(store, 'SELECT * ...')
```

### Issue: Hook Not Firing

**Symptoms**: Hook registered but not executing

**Solution**:
```javascript
// Check hook scope - must be registered per-store
store.registerHook(myHook) // ✅ Correct
registerHook(myHook)       // ❌ Wrong (v5 API)
```

### Issue: Zod Validation Errors

**Symptoms**: `ZodError: Unrecognized key 'X'`

**Solution**:
```javascript
// Read error message carefully
// Example: "Unrecognized key 'backend' in object"
// Solution: Use correct option name

// Before
createStore({ backend: 'oxigraph' })

// After (if schema changed)
createStore({ store: { type: 'oxigraph' } })
```

### Issue: ESM Import Errors

**Symptoms**: `Cannot use import statement outside a module`

**Solution**:
```json
// Add to package.json
{
  "type": "module"
}
```

### Issue: Performance Regression

**Symptoms**: Queries slower than v5

**Solution**:
```bash
# Profile application
npm run profile:perf

# Check for inefficient patterns
npx @unrdf/migrate-v6 analyze . --check-performance
```

---

## Rollback Plan

If critical issues arise:

### Option 1: Rollback Git

```bash
# Revert to pre-migration state
git checkout main
git branch -D migration/v6

# Redeploy v5
npm install
npm run deploy:production
```

### Option 2: Downgrade Dependencies

```bash
# Downgrade to v5
pnpm update '@unrdf/*@^5.0.0'

# Rebuild
npm run build
npm run deploy:production
```

### Option 3: Enable v5 Compatibility Layer

```javascript
// Enable v5 compatibility (temporary fix)
import { enableV5Compat } from '@unrdf/core/compat'

enableV5Compat({
  allowLegacyStoreAPI: true,
  allowGlobalHooks: true,
  warnOnDeprecated: true
})
```

**Note**: Compatibility layer removed in v6.1.0 (12 months after v6.0.0 GA).

---

## Migration Checklist

### Preparation
- [ ] Review breaking changes catalog
- [ ] Analyze codebase with migration tool
- [ ] Update dependencies to v6
- [ ] Create backup (git commit)
- [ ] Upgrade Node.js to ≥20.0.0

### Automated Migration
- [ ] Run migration tool (dry run)
- [ ] Review proposed changes
- [ ] Apply automated migrations
- [ ] Fix linting errors
- [ ] Commit changes

### Manual Migration
- [ ] Migrate hook registrations (BC-4)
- [ ] Update federation configs (BC-6)
- [ ] Convert CommonJS to ESM (BC-12)
- [ ] Fix Zod validation errors (BC-11)
- [ ] Commit changes

### Validation
- [ ] 100% test pass rate
- [ ] OTEL validation ≥80/100
- [ ] Performance benchmarks (no regressions)
- [ ] Deploy to staging
- [ ] Verify migration completeness

### Production Rollout
- [ ] Canary deployment (10% traffic)
- [ ] Monitor for 24-48 hours
- [ ] Full rollout (100% traffic)
- [ ] Monitor for 7 days
- [ ] Decommission v5 compatibility layer

---

## Support

**Need Help?**
- GitHub Discussions: https://github.com/unrdf/unrdf/discussions
- Migration FAQ: `/docs/v6/FAQ.md`
- Breaking Changes: `/docs/v6/BREAKING-CHANGES.md`
- Architecture: `/docs/v6/ARCHITECTURE.md`

**Found a Bug?**
- Report issues: https://github.com/unrdf/unrdf/issues
- Security issues: security@unrdf.dev

---

## Next Steps

After successful migration:
1. Explore new features (see `/docs/v6/NEW-FEATURES.md`)
2. Optimize performance (see `/docs/v6/PERFORMANCE-GUIDE.md`)
3. Enable advanced observability (see `/docs/v6/OBSERVABILITY.md`)
4. Join the community (see `/CONTRIBUTING.md`)

**Welcome to UNRDF v6!** 🎉
