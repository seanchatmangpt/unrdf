# FMEA & Poka-Yoke Implementation Guide

## Executive Summary

This document outlines the FMEA (Failure Mode and Effects Analysis) and poka-yoke (mistake-proofing) implementation strategy for UNRDF, applying the 80/20 principle to identify and mitigate the critical 20% of failure modes that cause 80% of system problems.

## 1. Critical Failure Modes (80/20 Analysis)

### Top 8 Failure Modes Identified

| Rank | Failure Mode | Severity | Frequency | RPN | Impact |
|------|--------------|----------|-----------|-----|--------|
| 1 | Quad missing .value property | 9 | 9 | 81 | 18% |
| 2 | Hook transform returns undefined | 8 | 8 | 64 | 17% |
| 3 | SPARQL query timeout | 7 | 8 | 56 | 14% |
| 4 | Quad mutation in callbacks | 8 | 7 | 56 | 12% |
| 5 | Concurrent hook races | 9 | 4 | 36 | 11% |
| 6 | Undefined variables in results | 7 | 6 | 42 | 10% |
| 7 | N3 import violations | 6 | 7 | 42 | 9% |
| 8 | File format misdetection | 6 | 6 | 36 | 9% |

**Total Risk**: 2921 RPN across all modes
**Expected Risk Reduction**: 75-85% through poka-yoke implementation
**Post-Implementation Risk**: < 876 RPN (target)

## 2. Poka-Yoke Guard Architecture

### Design Principles

1. **Fail Fast**: Detect errors early before state mutation
2. **Zero Trust**: Validate all inputs at boundaries
3. **Immutability**: Clone data before transformation
4. **Actionable Errors**: Errors include context and fix suggestions
5. **Performance**: Guards add < 1% overhead
6. **Composability**: Guards work together across layers

### Guard Modules

#### 2.1 Quad Guards (`src/poka-yoke/quad-guards.mjs`)

**Purpose**: Validate RDF quad structure and prevent malformed quads

**Key Functions**:
- `validateQuadStructure(quad)` - Ensure .value properties on all terms
- `ensureQuadValid(quad)` - Throw on invalid structure
- `sanitizeQuad(quad)` - Deep clone and validate
- `validateQuadsArray(quads)` - Batch validation with error aggregation
- `filterValidQuads(quads)` - Remove invalid quads from array

**Prevents**: FM-001 (Quad missing .value property)

#### 2.2 Store Guards (`src/poka-yoke/store-guards.mjs`)

**Purpose**: Validate store operations before mutation

**Key Functions**:
- `validateStoreInterface(store)` - Verify getQuads/addQuad/removeQuad
- `validateAddQuadArgs(store, quad)` - Pre-add validation
- `validateRemoveQuadArgs(store, quad, graph)` - Pre-remove validation
- `createSafeStoreWrapper(store)` - Automatic validation wrapper

**Prevents**: FM-001, FM-004 (Store mutations with invalid quads)

#### 2.3 Hook Guards (`src/poka-yoke/hook-guards.mjs`)

**Purpose**: Prevent hook-related data corruption

**Key Functions**:
- `validateHookConfig(config)` - Enforce hook structure
- `validateTransformReturn(result, original)` - Ensure valid quad returned
- `cloneQuadForHook(quad)` - Deep clone to prevent mutation
- `validateBeforeHookResult(blocked, reason)` - Validate blocking decisions

**Prevents**: FM-002 (undefined returns), FM-004 (quad mutation)

#### 2.4 Query Guards (`src/poka-yoke/query-guards.mjs`)

**Purpose**: Timeout protection and result validation

**Key Functions**:
- `validateQueryTimeout(options)` - Enforce max 30s timeout
- `validateResultBindings(bindings)` - Check undefined variables
- `validateQueryResults(results, queryType)` - Format validation
- `createTimeoutSignal(timeout)` - AbortSignal with timeout

**Prevents**: FM-003 (query timeouts), FM-006 (undefined variables)

#### 2.5 File Guards (`src/poka-yoke/file-guards.mjs`)

**Purpose**: RDF file validation and format detection

**Key Functions**:
- `validateFileFormat(filePath, mimeType)` - Format consistency check
- `validateFormatMapping(ext, format)` - Extension validation
- `validateFileContent(content, format)` - RDF syntax validation

**Prevents**: FM-008 (file format misdetection)

### Guard Integration Points

```
Data Boundary Entry
  ↓
File I/O Guards (file-guards.mjs)
  ↓
Format Detection
  ↓
Quad Guards (quad-guards.mjs)
  ↓
Store Interface Validation
  ↓
Store Wrapper (store-guards.mjs)
  ↓
Hook Execution
  ↓
Hook Guards (hook-guards.mjs)
  ↓
Query Execution
  ↓
Query Guards (query-guards.mjs)
  ↓
Result Return
```

## 3. Implementation Patterns

### Pattern 1: Direct Validation

```javascript
import { validateQuadStructure } from './src/poka-yoke/quad-guards.mjs';

const quad = { /* ... */ };
const result = validateQuadStructure(quad);

if (!result.valid) {
  throw new Error(`Invalid quad: ${result.errors.join(', ')}`);
}
```

### Pattern 2: Safe Wrapper

```javascript
import { createSafeStoreWrapper } from './src/poka-yoke/store-guards.mjs';

const safeStore = createSafeStoreWrapper(store);
safeStore.addQuad(quad); // Automatically validated
```

### Pattern 3: Deep Clone for Hooks

```javascript
import { cloneQuadForHook } from './src/poka-yoke/hook-guards.mjs';

const original = { /* ... */ };
const cloned = cloneQuadForHook(original);
// Modify cloned, original is safe
```

### Pattern 4: Timeout Protection

```javascript
import { createTimeoutSignal } from './src/poka-yoke/query-guards.mjs';

const signal = createTimeoutSignal(5000);
store.query(sparql, { signal });
```

## 4. Error Handling Strategy

### Error Message Format

```
[GUARD_NAME] Validation Failed
What: <specific issue>
Expected: <expected behavior>
Got: <actual value>
Fix: <actionable suggestion>
Context: <file:line, operation>
```

### Recovery Patterns

1. **Fail Fast**: Throw immediately, don't hide errors
2. **Detailed Context**: Include what, why, and how to fix
3. **Input Validation**: Check at boundaries, trust internally
4. **No Defensive Code**: If something fails, it's a real error

## 5. Testing Strategy

### Test Coverage Targets

- Quad Guards: 100% (critical data validation)
- Store Guards: 100% (store mutations)
- Hook Guards: 95% (extensibility points)
- Query Guards: 90% (result handling)
- File Guards: 85% (I/O operations)

### Test Patterns

```javascript
describe('Guard Name', () => {
  it('should pass valid input', () => {
    const result = guard(validInput);
    expect(result.valid).toBe(true);
    expect(result.errors).toHaveLength(0);
  });

  it('should reject invalid input', () => {
    const result = guard(invalidInput);
    expect(result.valid).toBe(false);
    expect(result.errors.length).toBeGreaterThan(0);
  });

  it('should provide actionable error message', () => {
    const result = guard(invalidInput);
    expect(result.errors[0]).toContain('Fix:');
  });
});
```

## 6. Performance Impact

### Guard Overhead Analysis

| Guard | Operation | Overhead | Notes |
|-------|-----------|----------|-------|
| Quad validation | Per quad | < 0.1ms | Direct property checks |
| Store wrapper | Per operation | < 0.5ms | Minimal validation |
| Hook cloning | Per hook | < 1ms | structuredClone overhead |
| Query timeout | Per query | 0ms | AbortSignal is native |
| File validation | Per file | < 5ms | Content parsing cached |

**Total System Overhead**: < 1% with all guards enabled

## 7. Implementation Roadmap

### Phase 1: Critical (Immediate)
- Quad structure validation (FM-001: RPN 81)
- Hook transform validation (FM-002: RPN 64)
- **Risk Reduction**: 40%

### Phase 2: High Priority
- Query timeout guards (FM-003: RPN 56)
- Store operation wrappers (FM-004: RPN 56)
- **Risk Reduction**: 25%

### Phase 3: Planned
- Binding validation (FM-006: RPN 42)
- N3 compliance checks (FM-007: RPN 42)
- File format validation (FM-008: RPN 36)
- Race condition prevention (FM-005: RPN 36)
- **Risk Reduction**: 35%

**Overall Goal**: Reduce RPN from 2921 to < 876 (70% reduction)

## 8. Validation & Monitoring

### OTEL Span Requirements

Each guard integration must emit OTEL spans:

```javascript
tracer.startActiveSpan('guard-validation', (span) => {
  span.setAttribute('guard.name', 'quad-validation');
  span.setAttribute('guard.input', inputSummary);

  const result = validateQuadStructure(quad);

  span.setAttribute('guard.valid', result.valid);
  span.setAttribute('guard.errors', result.errors.length);

  return result;
});
```

### Success Metrics

- ✅ All guards emit validation OTEL spans
- ✅ OTEL validation score ≥ 80/100
- ✅ < 1ms overhead per guard
- ✅ 100% test coverage for critical guards
- ✅ All error messages include fix suggestions

## 9. Quick Reference

### When to Use Each Guard

| Scenario | Guard | Function |
|----------|-------|----------|
| Adding quads to store | Quad + Store | `createSafeStoreWrapper()` |
| Hook implementation | Hook | `cloneQuadForHook()` |
| Running queries | Query | `createTimeoutSignal()` |
| Processing RDF files | File | `validateFileFormat()` |
| Batch operations | Quad | `validateQuadsArray()` |

### Common Patterns

```javascript
// Pattern 1: Safe store operations
const safeStore = createSafeStoreWrapper(store);
safeStore.addQuad(quad);

// Pattern 2: Hook protection
const cloned = cloneQuadForHook(original);
const result = hook(cloned);
ensureValidReturn(result);

// Pattern 3: Query safety
const signal = createTimeoutSignal(5000);
const results = store.query(sparql, { signal });

// Pattern 4: Batch validation
const validation = validateQuadsArray(quads);
if (validation.valid) {
  quads = validation.validQuads;
}
```

## 10. References

**Related Documentation**:
- `/docs/audit/FMEA-MATRIX-80-20.md` - Complete FMEA matrix
- `/docs/audit/POKA-YOKE-STRATEGY.md` - Design strategy
- `/src/poka-yoke/` - Guard implementations
- `/test/poka-yoke/` - Guard tests

**Standards**:
- ISO 13849-1: Functional safety principles
- RDF/JS: RDF interface standards
- OTEL: Observability standards

## 11. Success Criteria

Implementation is successful when:

1. ✅ All 5 guard modules implemented and tested
2. ✅ Guards integrated at all data boundaries
3. ✅ OTEL validation score ≥ 80/100
4. ✅ RPN reduced from 2921 to < 876 (70% reduction)
5. ✅ Zero regression in test coverage
6. ✅ Performance overhead < 1%
7. ✅ All error messages include fix suggestions
8. ✅ CI/CD validates guards on every commit

---

**Last Updated**: 2025-12-04
**Status**: FMEA Complete, Poka-Yoke Design Complete, Implementation Ready
**Expected Completion**: Phase 1 (2 sprints), Phase 2-3 (4 sprints)
