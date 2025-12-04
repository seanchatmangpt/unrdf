# UNRDF v5.0.0-alpha Quality Gates

**Last Updated**: 2025-12-03
**Commit**: fb85767

---

## Release Readiness Checklist

### ❌ BLOCKERS (Must Fix Before Alpha Release)

#### 1. Federation Package - Missing Exports
**Issue**: Core peer management functions not exported
**Impact**: Package unusable for peer-to-peer operations
**Files**: `packages/federation/src/index.mjs`

**Required Changes**:
```javascript
// Add to packages/federation/src/index.mjs
export {
  createPeerManager,
  registerPeer,      // ← ADD
  unregisterPeer,    // ← ADD
  getPeer,           // ← ADD
  listPeers,         // ← ADD
} from './federation/peer-manager.mjs';
```

**Verification**:
```bash
cd packages/federation
pnpm test  # Should pass all 7 tests
```

**Status**: ❌ NOT FIXED
**Estimate**: 15 minutes

---

#### 2. Hooks Package - Broken Return Values
**Issue**: executeHook() returns undefined instead of result object
**Impact**: Hooks appear to run but don't report success/failure
**Files**: `packages/hooks/src/hooks/hook-executor.mjs`

**Required Changes**:
```javascript
// Fix in packages/hooks/src/hooks/hook-executor.mjs
export function executeHook(hook, context) {
  if (hook.validate) {
    const valid = hook.validate(context);
    // ❌ CURRENT: Missing return
    // ✅ FIX: Add return statement
    return { success: valid, quad: context.quad };
  }

  if (hook.transform) {
    const transformed = hook.transform(context);
    return { success: true, quad: transformed.quad };
  }

  return { success: true, quad: context.quad };
}
```

**Verification**:
```bash
cd packages/hooks
pnpm test  # Should pass 13/13 tests
```

**Status**: ❌ NOT FIXED
**Estimate**: 30 minutes

---

#### 3. Core Package - Missing SPARQL Exports
**Issue**: SPARQL query functions not exported from index.mjs
**Impact**: Cannot execute SPARQL queries via package API
**Files**: `packages/core/src/index.mjs`

**Required Changes**:
```javascript
// Verify these exist in packages/core/src/sparql/executor.mjs
// Then add to packages/core/src/index.mjs
export {
  executeQuery,        // Already exported ✅
  prepareQuery,        // Already exported ✅
  executeSelect,       // Already exported ✅
  executeConstruct,    // Already exported ✅
  executeAsk,          // Already exported ✅
  executeSparqlQuery,  // ← CHECK IF EXISTS, ADD IF MISSING
} from './sparql/executor.mjs';
```

**Verification**:
```bash
cd packages/core
pnpm test  # Should pass SPARQL query tests
```

**Status**: ⚠️ NEEDS INVESTIGATION
**Estimate**: 45 minutes (check if function exists, add export)

---

#### 4. Knowledge Engine Package - Inference Not Working
**Issue**: runInference() doesn't produce inferred quads
**Impact**: Inference engine is advertised but non-functional
**Files**: `packages/knowledge-engine/src/knowledge-engine/inference-engine.mjs`

**Required Changes**:
```javascript
// Implement inference logic in inference-engine.mjs
export function runInference(engine) {
  const inferredQuads = [];

  for (const rule of engine.rules) {
    // Match patterns against store
    const matches = matchPattern(engine.store, rule.patterns);

    // Apply inferences for matches
    for (const binding of matches) {
      const inferred = applyInference(rule.infer, binding);
      inferredQuads.push(inferred);
      addQuad(engine.store, inferred);
    }
  }

  return inferredQuads;
}
```

**Verification**:
```bash
cd packages/knowledge-engine
pnpm test  # Should pass inference tests
```

**Status**: ❌ NOT FIXED
**Estimate**: 2-3 hours (requires pattern matching + inference logic)

**Alternative**: Mark as experimental in README, remove from v5.0.0-alpha

---

### ⚠️ WARNINGS (Should Fix, Not Blockers)

#### 5. Dark Matter Package - Optimizer Returns Input Unchanged
**Issue**: optimizeQuery() doesn't actually optimize
**Impact**: Query performance advertised but not delivered
**Files**: `packages/dark-matter/src/dark-matter/query-optimizer.mjs`

**Status**: ⚠️ KNOWN LIMITATION
**Estimate**: 4-6 hours (requires optimization algorithms)
**Recommendation**: Mark as experimental, document as "analysis only" for alpha

---

#### 6. Composables Package - No Vue Test Utils
**Issue**: Vue composables not tested in Vue runtime
**Impact**: Can't verify composables work in real Vue apps
**Files**: `packages/composables/test/composables.test.mjs`

**Status**: ⚠️ ACCEPTABLE FOR ALPHA
**Estimate**: 2-3 hours (setup Vue test utils)
**Recommendation**: Mark as experimental, test manually in Vue app

---

#### 7. Streaming Package - Subscription Methods Not Exported
**Issue**: subscribe/unsubscribe not exported from index.mjs
**Impact**: Can create subscription manager but can't use it
**Files**: `packages/streaming/src/index.mjs`

**Required Changes**:
```javascript
// Add to packages/streaming/src/index.mjs
export {
  createSubscriptionManager,
  subscribe,      // ← ADD
  unsubscribe,    // ← ADD
} from './streaming/subscription-manager.mjs';
```

**Status**: ⚠️ SHOULD FIX
**Estimate**: 15 minutes

---

### ✅ PASSING (No Action Needed)

#### 8. CLI Package
**Status**: ✅ 19/19 tests passing
**Coverage**: 96%
**Quality**: Production-ready

#### 9. Browser Package
**Status**: ✅ 4/4 tests passing
**Coverage**: 100%
**Quality**: Production-ready

#### 10. Project Engine Package
**Status**: ✅ 4/4 tests passing
**Coverage**: 100%
**Quality**: Production-ready (intentional stubs)

---

## Quality Gates Summary

### Test Pass Rate
- **Current**: ~50% (varies by package)
- **Required for Alpha**: ≥80%
- **Required for GA**: ≥95%

### Code Coverage
- **Current**: 15-100% (varies by package)
- **Required for Alpha**: ≥60%
- **Required for GA**: ≥80%

### Export Completeness
- **Current**: 60% (missing key functions)
- **Required for Alpha**: ≥90%
- **Required for GA**: 100%

### Implementation Completeness
- **Current**: 50% (advertised vs working)
- **Required for Alpha**: ≥70%
- **Required for GA**: 100%

---

## Verification Commands

### Run All Tests
```bash
pnpm test
```

### Run Package-Specific Tests
```bash
cd packages/cli && pnpm test
cd packages/core && pnpm test
cd packages/hooks && pnpm test
cd packages/federation && pnpm test
cd packages/streaming && pnpm test
cd packages/browser && pnpm test
cd packages/dark-matter && pnpm test
cd packages/knowledge-engine && pnpm test
cd packages/composables && pnpm test
cd packages/project-engine && pnpm test
```

### Check Test Coverage
```bash
pnpm test --coverage
```

### Verify Exports
```bash
# Check that all functions are exported
node -e "import('@unrdf/core').then(m => console.log(Object.keys(m)))"
node -e "import('@unrdf/hooks').then(m => console.log(Object.keys(m)))"
node -e "import('@unrdf/federation').then(m => console.log(Object.keys(m)))"
```

---

## Release Decision Matrix

### Can Release Alpha If:
- [ ] All BLOCKERS fixed (federation exports, hooks return values, core SPARQL)
- [ ] Test pass rate ≥80%
- [ ] No critical security issues
- [ ] CLI, Browser, Project Engine working (reference implementations)
- [ ] README documents experimental packages

### Cannot Release Alpha If:
- [ ] Federation unusable (missing exports)
- [ ] Hooks broken (undefined returns)
- [ ] Core SPARQL missing (advertised but unavailable)
- [ ] Test pass rate <80%

### Can Release GA If:
- [ ] All alpha blockers + warnings fixed
- [ ] Knowledge Engine inference working
- [ ] Dark Matter optimizer implemented
- [ ] Composables tested in Vue
- [ ] Test pass rate ≥95%
- [ ] Code coverage ≥80%
- [ ] Security audit passed

---

## Effort Estimates

### Critical Path (Blockers Only)
1. Federation exports: 15 minutes
2. Hooks return values: 30 minutes
3. Core SPARQL investigation: 45 minutes
4. Knowledge Engine (if not deferred): 2-3 hours

**Total**: 4-5 hours to alpha-ready (if deferring knowledge-engine)
**Total**: 6-8 hours to alpha-ready (if including knowledge-engine)

### Full Alpha Quality (Blockers + Warnings)
- Blockers: 4-5 hours
- Streaming exports: 15 minutes
- Dark Matter documentation: 30 minutes
- Composables documentation: 30 minutes

**Total**: 5-6 hours to high-quality alpha

---

## Recommendations

### For v5.0.0-alpha Release

**MUST FIX**:
1. ✅ Federation exports (15 min)
2. ✅ Hooks return values (30 min)
3. ✅ Core SPARQL exports (45 min)

**SHOULD FIX**:
4. ✅ Streaming exports (15 min)

**DEFER TO LATER**:
5. ❌ Knowledge Engine (mark experimental)
6. ❌ Dark Matter optimizer (mark analysis-only)
7. ❌ Composables Vue tests (mark experimental)

**Total Effort**: 2 hours for minimal viable alpha

---

### For v5.0.0 GA Release

**MUST COMPLETE**:
- All alpha fixes
- Knowledge Engine inference working
- Dark Matter optimizer implemented
- Composables tested in Vue runtime
- Test coverage ≥80% all packages
- Security audit passed
- Performance benchmarks documented

**Total Effort**: 20-30 hours additional work

---

## Sign-off Checklist

Before releasing v5.0.0-alpha:

- [ ] All blocker fixes implemented
- [ ] All blocker tests passing
- [ ] Test pass rate ≥80%
- [ ] README documents experimental packages
- [ ] CHANGELOG updated
- [ ] Version bumped in all package.json
- [ ] Git tagged as v5.0.0-alpha.0
- [ ] Published to npm (alpha tag)

**Signed off by**: _______________
**Date**: _______________

---

**Report Generated**: 2025-12-03
**Next Review**: After blocker fixes
**Status**: ❌ NOT READY FOR RELEASE
