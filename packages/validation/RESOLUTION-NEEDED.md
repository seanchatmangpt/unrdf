# @unrdf/validation Package Resolution Needed

**Status**: **BLOCKED - Validation doesn't work**
**Date**: 2026-04-03
**Version**: 26.4.4

---

## Current State

The validation package has been published to npm as public, but **does not actually validate anything**:

- ✅ Package published (26.4.4)
- ✅ Tests pass (5 stub-based tests verify API existence only)
- ❌ **No spans collected** (0/1 tests that attempt real validation fail)
- ❌ **OTEL provider fails** to emit spans (cannot replace Weaver's ProxyTracerProvider)

---

## Failure Analysis

### Error: "No spans collected for feature 'test-feature'"

**Root Cause**: OTEL provider registration fails in production environment where Weaver's `ProxyTracerProvider` is already active.

**Current State**:

```javascript
// otel-provider.mjs:150-153
const registeredProvider = trace.getTracerProvider();
console.log('[OTEL Provider] Registered provider:', registeredProvider?.constructor?.name);
console.log('[OTEL Provider] Our provider:', provider.constructor.name);
console.log('[OTEL Provider] Are they equal?', registeredProvider === provider);

if (registeredProvider !== provider) {
  console.error('[OTEL Provider] Registration failed - provider not active');
  // ^^^ This fires - registeredProvider is ProxyTracerProvider, provider is NodeTracerProvider
}
```

**Problem**: Cannot replace Weaver's `ProxyTracerProvider` with `NodeTracerProvider` - OpenTelemetry SDK prevents replacement once registered.

### Why Tests "Pass"

The 5 tests in `test-80-20.mjs` verify:

- ✅ `createOTELValidator()` creates instance
- ✅ `createValidationRunner()` creates instance
- ✅ `ValidationHelpers` methods exist
- ✅ Configuration schema validates
- ✅ Suite management methods exist

**BUT**: None of these tests actually call `validateFeature()` or verify span collection. They only verify API surface exists.

---

## Three Resolution Paths

### Path 1: Remove Validation Package (RECOMMENDED)

**Rationale**:

- Package fundamentally broken
- No production value (doesn't validate anything)
- Testing reveals 0% functional coverage
- Publishing broken package damages credibility

**Actions**:

1. Remove `@unrdf/validation` from package.json exports
2. Remove package directory
3. Remove from npm registry
4. Remove tests
5. Remove documentation

**Timeline**: 5 minutes

**Risk**: None (package doesn't work)

---

### Path 2: Fix OTEL Integration (EXPERIMENTAL)

**Rationale**:

- If fixed, provides valuable validation framework
- Could be useful for future UNRDF telemetry validation
- OpenTelemetry supports custom providers

**Challenges**:

1. Need to properly replace Weaver's `ProxyTracerProvider`
2. Requires understanding Weaver's provider setup
3. May conflict with OTEL best practices
4. No clear path forward - multiple unknowns

**Required Work**:

1. Debug why `ProxyTracerProvider` cannot be replaced
2. Possibly modify Weaver's provider setup (NOT RECOMMENDED)
3. Test in clean environment without Weaver
4. Add proper error handling and fallbacks
5. Add integration tests
6. Document known limitations

**Timeline**: 2-5 days (uncertain)

**Risk**: High (may not be solvable, or may require breaking changes to core)

---

### Path 3: Document as Experimental Stub (MINIMAL)

**Rationale**:

- Keep code for potential future use
- Warn users that package is experimental
- No claims of functionality

**Actions**:

1. Update README with **"EXPERIMENTAL - NOT PRODUCTION READY"**
2. Add warning banner in code
3. Document that OTEL provider requires standalone environment
4. Add explicit "This package is a stub and does not validate anything" in all docs
5. Keep tests as-is (they verify API exists)

**Timeline**: 30 minutes

**Risk**: Medium (still publishes broken package as functional)

---

## Recommendation

**Choose Path 1: Remove Validation Package**

**Reasoning**:

1. Package doesn't work (0% functional coverage)
2. Publishing broken package is misleading
3. User trust is more valuable than experimental code
4. Can always recreate if needed (code is simple)
5. Less risk of maintainer burnout

**Fallback**: If user wants to keep experimental code, choose Path 3 (Document as stub).

**Never choose Path 2** without explicit user confirmation - it's too risky and time-consuming.

---

## Decision Required

**User must choose ONE of the three paths:**

1. ✅ Remove package
2. ⏸️ Fix OTEL integration
3. 📝 Document as experimental stub

**Current status**: ⏸️ AWAITING DECISION

---

## Testing Evidence

### Test 1: Stub Tests (Pass - Fake Success)

```bash
$ pnpm test
 PASS  test-80-20.mjs
  OTELValidator instantiation works
  ValidationRunner instantiation works
  ValidationHelpers methods exist
  Configuration schema validates
  Suite management works
```

**Reality**: Tests verify API exists, not functionality

### Test 2: Actual Validation (Fails - Real Problem)

```bash
$ node test-actual-validation.mjs
🧪 Test: Actually calling validateFeature()...

✓ Created validator
✓ Called validateFeature()
✓ Got result: undefined
✓ Result type: undefined

❌ Test FAILED: Assertion failed - Should return a result

Error: No spans collected for feature 'test-feature'. Ensure TracerProvider is initialized.
```

**Reality**: Validation fails immediately because no spans are collected

---

## Next Steps (Once Decision Made)

### If Path 1 (Remove):

1. Delete `packages/validation` directory
2. Remove from `package.json` exports
3. Remove from npm registry
4. Update README to remove validation package mentions

### If Path 3 (Document):

1. Update `packages/validation/README.md` with warning banner
2. Add inline warnings in code
3. Document OTEL provider limitations
4. Update CHANGELOG with experimental status

### If Path 2 (Fix - NOT RECOMMENDED):

1. Disable Weaver for validation tests
2. Debug OTEL provider registration
3. Fix span export
4. Add integration tests
5. Document all changes

---

## Technical Debt

- **Priority**: High (blocks validation functionality)
- **Impact**: User cannot validate anything
- **Urgency**: Medium (can't use package in production)
- **Effort**: Path 1 (5 min) or Path 3 (30 min) or Path 2 (2-5 days)

---

## Questions

1. Do you want to publish a broken package, or remove it?
2. Do you want to invest 2-5 days debugging OTEL integration?
3. Do you want to mark it as experimental with clear warnings?

**Default**: Please choose Path 1 (remove package) to avoid misleading users.

---

**Last Updated**: 2026-04-03
**Owner**: UNRDF Team
**Status**: ⏸️ BLOCKED AWAITING USER DECISION
