# Implementation Plan: Deepen Oxigraph Integration

## Overview
Based on exploration, Oxigraph is already integrated in @unrdf/core (v0.5.2). The task is to deepen its usage across packages, particularly replacing the mock SPARQL implementation in the full-stack example.

## Current State
- ✅ Oxigraph integrated in @unrdf/core via `@unrdf/oxigraph` package
- ✅ Clean abstraction through `executeQuery`, `executeSelect`, `executeConstruct`, `executeAsk`
- ✅ Used by CLI and composables packages
- ❌ Full-stack example uses MOCK SPARQL (lines 191-277 in server/src/index.mjs)
- ❌ Mock implementation only does pattern matching, not real SPARQL

## Implementation Steps

### Phase 1: Replace Mock SPARQL in Full-Stack Example Server

**File**: `/Users/sac/unrdf/playground/full-stack-example/apps/server/src/index.mjs`

**Changes Needed**:
1. Add import: `import { executeQuery } from '@unrdf/core'`
2. Replace lines 191-277 (mock SPARQL implementation) with real Oxigraph execution
3. Update `POST /api/query` endpoint to use `executeQuery(store, sparql)`
4. Handle query types (SELECT, CONSTRUCT, ASK) properly
5. Maintain error handling and execution metrics

**Impact**: Low risk - isolated to server implementation, tests already exist

### Phase 2: Update Full-Stack Example Tests

**Files**:
- `/Users/sac/unrdf/playground/full-stack-example/apps/web/test/integration.test.mjs`
- `/Users/sac/unrdf/playground/full-stack-example/apps/server/test/server.test.mjs`

**Changes Needed**:
1. Update test expectations to match real SPARQL behavior
2. Verify query execution tests still pass
3. Add tests for advanced SPARQL features (FILTER, OPTIONAL, etc.)
4. Ensure API endpoint tests validate real query execution

**Impact**: Medium risk - need to ensure tests reflect real behavior

### Phase 3: Add Oxigraph Dependency to Server Package

**File**: `/Users/sac/unrdf/playground/full-stack-example/apps/server/package.json`

**Changes Needed**:
1. Verify `@unrdf/core` dependency exists (workspace:*)
2. Ensure all necessary exports are available
3. No additional dependencies needed (Oxigraph comes through @unrdf/core)

**Impact**: No risk - dependency already present via workspace

### Phase 4: Documentation and Validation

**Changes Needed**:
1. Update full-stack example README with real SPARQL capabilities
2. Add example queries demonstrating Oxigraph features
3. Run full test suite to ensure 100% pass rate
4. Validate production readiness with OTEL spans

## Success Criteria

- ✅ Full-stack server uses real Oxigraph SPARQL execution
- ✅ All tests pass (330+ tests in validation suite)
- ✅ API endpoints handle SELECT, CONSTRUCT, ASK queries correctly
- ✅ Performance metrics show acceptable query execution times
- ✅ Error handling covers SPARQL syntax errors and execution failures
- ✅ Documentation reflects real SPARQL capabilities

## Risk Assessment

**Low Risk**:
- Oxigraph already proven working in core package
- Clean abstraction through executeQuery interface
- Isolated changes to full-stack example only
- Comprehensive tests already exist

**Medium Risk**:
- Test expectations may need updates for real SPARQL behavior
- Query execution performance may differ from mock
- Need to handle async properly in server endpoints

## Dependencies

- @unrdf/core (workspace:*) - already available
- @unrdf/oxigraph (workspace:*) - transitive via core
- No new external dependencies required

## Timeline Estimate

- Phase 1: Replace mock SPARQL - 1 hour
- Phase 2: Update tests - 1 hour
- Phase 3: Verify dependencies - 15 minutes
- Phase 4: Documentation - 30 minutes

**Total: ~3 hours of focused work**

## Rollback Plan

If issues arise:
1. Revert server/src/index.mjs to mock implementation
2. Tests will continue passing with mock behavior
3. No breaking changes to other packages
