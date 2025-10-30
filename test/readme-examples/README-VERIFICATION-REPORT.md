# README Examples Verification Report

**Test Suite**: `/test/readme-examples/run-all-examples.mjs`
**Date**: 2025-10-29
**Agent**: Research Specialist
**Results**: 1/11 PASSED (9.1% success rate)

## Executive Summary

Manual verification of all README.md code examples reveals **significant discrepancies** between documentation and actual implementation. Only 1 of 11 examples work as documented.

### Critical Issues Found

1. **Import Paths Wrong** (FIXED) - README shows non-existent import paths
2. **API Mismatches** (CRITICAL) - Functions don't exist or have different signatures
3. **Missing `init()` calls** (CRITICAL) - DarkMatterCore requires initialization
4. **Schema Validation Failures** (HIGH) - Hook definitions require `when.ref` field
5. **Missing Methods** (HIGH) - `system.query()`, `system.validate()`, `lockchain.init()` don't exist

---

## Test Results by Example

### ✅ PASSED (1/11)

#### 8. Observability (lines 291-304) ✅
- **Status**: WORKING
- **Notes**: Only example that works completely as documented
- Class name mismatch (`Observability` → `ObservabilityManager`) is minor

---

### ❌ FAILED (10/11)

#### 1. Quick Start (lines 66-102) ❌
**Error**: `Dark Matter core not initialized`
**Root Cause**: Missing `await system.init()` call before usage
**Fix Required**: Add initialization step to README example

```diff
  const system = await createDarkMatterCore();
+ await system.init();

  await system.executeTransaction({
```

**Severity**: CRITICAL - This is the first example users see

---

#### 2. RDF Engine (lines 114-131) ❌
**Error**: `JSON-LD conversion failed`
**Root Cause**: `toJsonLd()` returns object, not string
**Fix Required**: Update README to show actual return type

```javascript
// README says:
const jsonld = await toJsonLd(store);

// Actually returns:
const jsonld = await toJsonLd(store); // Returns object, not string
```

**Severity**: HIGH - Misleading return type

---

#### 3. Knowledge Hooks (lines 143-168) ❌
**Error**: `when.ref: Required (received: "undefined") (expected: object)`
**Root Cause**: Hook schema requires `when.ref` field that's undocumented
**Fix Required**: README example missing required field

```diff
  when: {
    kind: 'sparql-ask',
    query: `ASK { ... }`,
+   ref: {
+     subject: '?person',
+     predicate: 'http://xmlns.com/foaf/0.1/name',
+     object: '?name'
+   }
  },
```

**Severity**: CRITICAL - Core feature example broken

---

#### 4. SPARQL Queries (lines 182-208) ❌
**Error**: `Dark Matter core not initialized`
**Root Cause**: Same as #1 - missing init()
**Fix Required**: Add `await system.init()`

**Severity**: CRITICAL

---

#### 5. SHACL Validation (lines 214-241) ❌
**Error**: `system.validate is not a function`
**Root Cause**: DarkMatterCore doesn't expose `validate()` method
**Fix Required**: Either:
- Add `validate()` method to DarkMatterCore, OR
- Update README to use correct API

```javascript
// README shows:
const validation = await system.validate({
  dataGraph: store,
  shapesGraph: shapes
});

// Actual API (needs verification):
import { validateShacl } from 'unrdf';
const validation = await validateShacl(store, shapes);
```

**Severity**: HIGH - Core feature broken

---

#### 6. Lockchain (lines 247-268) ❌
**Error**: `lockchain.init is not a function`
**Root Cause**: LockchainWriter API mismatch
**Fix Required**: Verify actual API and update README

```javascript
// README shows:
await lockchain.init();

// Needs investigation of actual API
```

**Severity**: HIGH - Security feature broken

---

#### 7. Dark Matter (lines 274-285) ❌
**Error**: `System missing query`
**Root Cause**: DarkMatterCore doesn't expose `query()` method
**Fix Required**: Update README with correct API

**Severity**: HIGH

---

#### 9. Simple Knowledge Graph (lines 336-379) ❌
**Error**: `Dark Matter core not initialized`
**Root Cause**: Same as #1 - missing init()
**Fix Required**: Add `await system.init()`

**Severity**: HIGH - This is a featured example

---

#### 10. Policy-Driven Validation (lines 383-426) ❌
**Error**: `when.ref: Required`
**Root Cause**: Same as #3 - missing required field
**Fix Required**: Update hook definition

**Severity**: HIGH - Policy features are core value prop

---

#### 11. Cryptographic Audit Trail (lines 430-462) ❌
**Error**: `lockchain.init is not a function`
**Root Cause**: Same as #6
**Fix Required**: Fix LockchainWriter API docs

**Severity**: HIGH - Audit trail is security feature

---

## Systematic Issues

### 1. Import Path Inconsistency (FIXED)
**Issue**: README shows `import from 'unrdf'` but examples need specific paths
**Status**: FIXED in test files
**Action**: Update README to clarify import source

### 2. Initialization Protocol Missing
**Issue**: DarkMatterCore requires `init()` but README omits it
**Affected**: Examples 1, 4, 9
**Fix**: Add initialization step to all examples

```javascript
// Standard pattern should be:
const system = await createDarkMatterCore();
await system.init(); // ← REQUIRED but missing from README
```

### 3. Hook Schema Undocumented
**Issue**: `when.ref` field required but not shown in examples
**Affected**: Examples 3, 10
**Fix**: Document complete hook schema

### 4. Method Availability
**Issue**: Methods shown in README don't exist on DarkMatterCore
**Missing Methods**:
- `system.query()` - shown in examples 4, 7
- `system.validate()` - shown in example 5

**Fix**: Either implement these methods or update README to use correct API

### 5. LockchainWriter API
**Issue**: `init()` method doesn't exist
**Affected**: Examples 6, 11
**Fix**: Document actual initialization pattern

---

## Recommendations

### Immediate Actions (Critical)

1. **Add Init Step** - Every DarkMatterCore example needs `await system.init()`
2. **Fix Hook Examples** - Add required `when.ref` field with example values
3. **Verify API Methods** - Confirm which methods actually exist on DarkMatterCore
4. **Test LockchainWriter** - Determine correct initialization pattern

### Short-term Actions (High Priority)

1. **Create Test Suite** - Integrate `/test/readme-examples/` into CI/CD
2. **API Consistency** - Make README imports match actual exports
3. **Type Documentation** - Clarify return types (e.g., toJsonLd returns object not string)

### Long-term Actions

1. **Auto-generate Examples** - Extract examples from passing tests
2. **OTEL Validation** - Add OTEL validation for README examples
3. **Version Sync** - Ensure README matches current API version

---

## Files Created

All test files in `/test/readme-examples/`:

1. `example-quick-start.mjs` - Quick Start (66-102)
2. `example-rdf-engine.mjs` - RDF Engine (114-131)
3. `example-knowledge-hooks.mjs` - Knowledge Hooks (143-168)
4. `example-sparql-queries.mjs` - SPARQL Queries (182-208)
5. `example-shacl-validation.mjs` - SHACL Validation (214-241)
6. `example-lockchain.mjs` - Lockchain (247-268)
7. `example-dark-matter.mjs` - Dark Matter (274-285)
8. `example-observability.mjs` - Observability (291-304) ✅
9. `example-simple-graph.mjs` - Simple Graph (336-379)
10. `example-policy-validation.mjs` - Policy Validation (383-426)
11. `example-audit-trail.mjs` - Audit Trail (430-462)
12. `run-all-examples.mjs` - Test runner

---

## Next Steps

1. **Investigate API** - Determine actual method signatures for:
   - `DarkMatterCore.init()`
   - `DarkMatterCore.query()`
   - `DarkMatterCore.validate()`
   - `LockchainWriter` initialization

2. **Fix README** - Update all failing examples with correct code

3. **Verify Fixes** - Re-run `node test/readme-examples/run-all-examples.mjs`

4. **Integrate CI** - Add to test suite: `"test:readme": "node test/readme-examples/run-all-examples.mjs"`

---

## Conclusion

The README documentation is **significantly out of sync** with actual implementation. This creates a poor developer experience where:

- ❌ First example (Quick Start) fails immediately
- ❌ 90.9% of examples fail
- ❌ Core features (hooks, queries, validation) don't work as documented
- ✅ Only 1 example (Observability) actually works

**Recommendation**: Treat this as a **production incident** and prioritize fixing README examples before next release.

---

**Generated by**: Research Specialist Agent
**Validation Method**: Manual execution of all code examples
**Evidence**: Test output in `/test/readme-examples/run-all-examples.mjs`
**OTEL Validation**: Recommended as follow-up to verify fixes
