# KGC Suite Fix Strategy - Detailed Implementation Plan

**Target**: Make all 10 KGC packages CLAUDE.md compliant and fully operational

**Timeline**: 6-8 hours with clear task breakdown

---

## Part 1: Root Cause Analysis

### Issue 1: kgc-4d Module Exports

**Symptom**: kgc-substrate and kgc-claude cannot import from @unrdf/kgc-4d
```
Error: Cannot find package '@unrdf/kgc-4d' imported from '/home/user/unrdf/packages/kgc-substrate/src/KnowledgeStore.mjs'
```

**Root Cause**: Need to verify:
1. kgc-4d build output exists
2. package.json exports are correct
3. Main entry point is valid

**Action**:
```bash
# 1. Check if kgc-4d built
ls -la packages/kgc-4d/src/index.mjs

# 2. Verify exports in package.json
cat packages/kgc-4d/package.json | grep -A 10 '"exports"'

# 3. Check what's being exported
head -30 packages/kgc-4d/src/index.mjs

# 4. Try rebuilding
pnpm -C packages/kgc-4d build

# 5. Test import
node -e "import('@unrdf/kgc-4d').then(m => console.log(Object.keys(m))).catch(e => console.error(e))"
```

---

### Issue 2: kgc-4d Doctest Failures

**Symptom**: 26 doctest failures in test/doctest/
```
× HistoryReconstructor example 1 (line 1) - FAILED
× cacheKey example 3 (line 63) - FAILED
```

**Root Cause**: Doctest generation script `scripts/generate-doctests.mjs` likely has issues with:
1. JSDoc example extraction
2. Code execution context
3. Assertion format

**Action**:
```bash
# Check the doctest generation script
cat packages/kgc-4d/scripts/generate-doctests.mjs | head -50

# Run with debug
node packages/kgc-4d/scripts/generate-doctests.mjs --debug

# Check generated doctests
ls packages/kgc-4d/test/doctest/

# Check the pattern of failures
grep -A 3 "× " <(pnpm -C packages/kgc-4d test 2>&1)
```

---

### Issue 3: kgc-runtime Test Failures (22 failures)

**Failing tests**:
1. `projections-cli.test.mjs` (7 failures) - Color output, table formatting
2. `projections-docs.test.mjs` (7 failures) - Markdown generation
3. `projections-ide.test.mjs` (4 failures) - LSP integration
4. `transaction.test.mjs` (4 failures) - Two-phase commit

**Action**: Debug each test file
```bash
# Test each individually
pnpm -C packages/kgc-runtime test -- projections-cli.test.mjs --reporter=verbose
pnpm -C packages/kgc-runtime test -- projections-docs.test.mjs --reporter=verbose
pnpm -C packages/kgc-runtime test -- projections-ide.test.mjs --reporter=verbose
pnpm -C packages/kgc-runtime test -- transaction.test.mjs --reporter=verbose
```

---

## Part 2: OTEL Instrumentation Pattern

**Pattern for all packages**: Add OTEL spans to critical operations.

### Example 1: Receipt Operations (kgc-probe, kgc-tools)

**File**: `packages/kgc-probe/src/index.mjs`

**Before**:
```javascript
export function createReceipt(options) {
  const receipt = {
    id: generateId(),
    timestamp: Date.now(),
    ...options
  };
  return receipt;
}
```

**After**:
```javascript
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('kgc-probe');

/**
 * Creates a cryptographic receipt for tracked operations
 * @param {Object} options - Receipt creation options
 * @param {string} options.operation - Operation type (create, update, delete)
 * @param {string} options.entityType - Type of entity affected
 * @param {string} [options.parentHash] - Parent receipt hash for chaining
 * @returns {Receipt} Verified receipt with BLAKE3 hash
 * @throws {ValidationError} If options fail Zod validation
 * @example
 * const receipt = createReceipt({ operation: 'create', entityType: 'Triple' });
 * console.log(receipt.id); // 'rec_sha256_...'
 */
export function createReceipt(options) {
  const span = tracer.startSpan('kgc-probe.createReceipt', {
    attributes: {
      'operation': options.operation,
      'entity_type': options.entityType
    }
  });

  try {
    const receipt = {
      id: generateId(),
      timestamp: Date.now(),
      ...options
    };

    span.addEvent('receipt_created', {
      'receipt_id': receipt.id
    });

    return receipt;
  } catch (error) {
    span.recordException(error);
    throw error;
  } finally {
    span.end();
  }
}
```

### Example 2: Schema Validation (all packages with Zod)

**File**: `packages/kgc-runtime/src/index.mjs`

**Before**:
```javascript
import { z } from 'zod';

const ReceiptSchema = z.object({
  id: z.string(),
  operation: z.enum(['create', 'update', 'delete'])
});

export function validateReceipt(data) {
  return ReceiptSchema.parse(data);
}
```

**After**:
```javascript
import { z } from 'zod';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('kgc-runtime');

const ReceiptSchema = z.object({
  id: z.string(),
  operation: z.enum(['create', 'update', 'delete'])
});

/**
 * Validates receipt against Zod schema
 * @param {unknown} data - Data to validate
 * @returns {Object} Validated receipt
 * @throws {ZodError} If validation fails
 */
export function validateReceipt(data) {
  const span = tracer.startSpan('kgc-runtime.validateReceipt');

  try {
    const result = ReceiptSchema.parse(data);
    span.addEvent('validation_success', {
      'operation': result.operation
    });
    return result;
  } catch (error) {
    span.addEvent('validation_failed', {
      'error_message': error.message
    });
    span.recordException(error);
    throw error;
  } finally {
    span.end();
  }
}
```

### Example 3: Transaction Operations (kgc-runtime)

**File**: `packages/kgc-runtime/src/transaction.mjs`

**Before**:
```javascript
export async function executeTwoPhaseCommit(operations) {
  // Phase 1: prepare
  const prepared = await Promise.all(
    operations.map(op => op.prepare())
  );

  // Phase 2: commit
  const committed = await Promise.all(
    prepared.map(p => p.commit())
  );

  return committed;
}
```

**After**:
```javascript
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('kgc-runtime');

/**
 * Executes two-phase commit for distributed transactions
 * @param {Operation[]} operations - Operations to commit
 * @returns {Promise<CommitResult[]>} Commit results per operation
 * @throws {CommitError} If any phase fails
 */
export async function executeTwoPhaseCommit(operations) {
  const span = tracer.startSpan('kgc-runtime.executeTwoPhaseCommit', {
    attributes: {
      'operation_count': operations.length
    }
  });

  try {
    // Phase 1: prepare
    const prepareSpan = tracer.startSpan('phase.prepare', { parent: span });
    const prepared = await Promise.all(
      operations.map(op => op.prepare())
    );
    prepareSpan.addEvent('prepare_complete', {
      'prepared_count': prepared.length
    });
    prepareSpan.end();

    // Phase 2: commit
    const commitSpan = tracer.startSpan('phase.commit', { parent: span });
    const committed = await Promise.all(
      prepared.map(p => p.commit())
    );
    commitSpan.addEvent('commit_complete', {
      'committed_count': committed.length
    });
    commitSpan.end();

    span.addEvent('two_phase_commit_success', {
      'operation_count': operations.length
    });

    return committed;
  } catch (error) {
    span.addEvent('two_phase_commit_failed');
    span.recordException(error);
    throw error;
  } finally {
    span.end();
  }
}
```

---

## Part 3: File Size Refactoring Pattern

### Large File Refactoring Pattern

**Problem**: `kgc-probe/src/agents/index.mjs` is 1,403 lines (should be ~350 lines)

**Pattern**:

1. **Identify logical sections** (functions by domain)
2. **Create separate files** per logical section
3. **Re-export from index** for backwards compatibility

**Example**:

**Before** (`agents/index.mjs` - 1,403 lines):
```javascript
// Agent 1 implementation (300 lines)
export function createAgent1() { ... }
export function runAgent1() { ... }

// Agent 2 implementation (300 lines)
export function createAgent2() { ... }
export function runAgent2() { ... }

// Agent 3 implementation (300 lines)
export function createAgent3() { ... }
export function runAgent3() { ... }

// Coordinator (303 lines)
export function coordinateAgents() { ... }
```

**After**:

```
agents/
├── index.mjs (50 lines - re-exports)
├── agent1.mjs (300 lines)
├── agent2.mjs (300 lines)
├── agent3.mjs (300 lines)
└── coordinator.mjs (300 lines)
```

**New `agents/index.mjs`**:
```javascript
/**
 * @file KGC Probe Agent Implementations
 * @module @unrdf/kgc-probe/agents
 */

export { createAgent1, runAgent1 } from './agent1.mjs';
export { createAgent2, runAgent2 } from './agent2.mjs';
export { createAgent3, runAgent3 } from './agent3.mjs';
export { coordinateAgents } from './coordinator.mjs';
```

---

## Part 4: JSDoc Coverage Template

**Required JSDoc format** for all exports:

```javascript
/**
 * [One-line description]
 *
 * [Optional: Multi-line explanation if complex]
 *
 * @param {Type} paramName - Description
 * @param {Object} [optionalParam] - Optional parameter
 * @param {string} optionalParam.subfield - Subfield description
 * @returns {ReturnType} Description of return value
 * @throws {ErrorType} When error condition occurs
 * @example
 * // Show typical usage
 * const result = functionName(arg1, { subfield: 'value' });
 * console.log(result); // expected output
 *
 * @internal
 * Internal implementation details (optional)
 */
export function functionName(paramName, optionalParam = {}) {
  // Implementation
}
```

### High-Priority JSDoc Targets

**kgc-probe** (101 functions without JSDoc):
- `createReceipt()` - Critical
- `verifyReceipt()` - Critical
- `createProof()` - Critical
- All agent creators: `createAgent1()`, etc.

**kgc-runtime** (71 functions):
- `validateReceipt()` - Critical
- `createProjection()` - Critical
- `executeTwoPhaseCommit()` - Critical
- All projection functions

**kgc-claude** (118 functions):
- `createRunCapsule()` - Critical
- `executeCheckpoint()` - Critical
- `createSwarm()` - Critical

---

## Part 5: Testing Strategy

### Verification Commands

**After OTEL additions**:
```bash
# Check OTEL imports
grep -r "@opentelemetry" packages/kgc-*/src | wc -l
# Expected: > 100 (at least 10+ per package)

# Check JSDoc coverage
grep -r "^export function\|^export const.*=.*=>" packages/kgc-*/src | wc -l
# Should match:
grep -B1 "^export function\|^export const.*=.*=>" packages/kgc-*/src | grep "/\\*\\*" | wc -l
# These counts should be equal
```

**After file size refactoring**:
```bash
# Check max file size
find packages/kgc-*/src -name "*.mjs" -exec wc -l {} + | awk '$1 > 500 {print $2 " (" $1 " lines)"}'
# Should return nothing
```

**After test fixes**:
```bash
# Run all tests
pnpm -r test

# Expected output:
# Test Files: 10 passed
# Tests: 500+ passed
```

---

## Part 6: Implementation Roadmap

### Week 1: Core Fixes (Days 1-2)

**Day 1**: Root cause fixes
- [ ] Fix kgc-4d module exports (1 hour)
- [ ] Fix kgc-4d doctest generation (2 hours)
- [ ] Fix kgc-runtime test failures (2 hours)

**Day 2**: OTEL instrumentation
- [ ] Add OTEL to kgc-4d (1.5 hours)
- [ ] Add OTEL to kgc-runtime (1.5 hours)
- [ ] Add OTEL to kgc-probe (2 hours)

### Week 1: Continuation (Days 3-4)

**Day 3**: File refactoring
- [ ] Refactor kgc-probe (agents: 1403 → 4 files) (3 hours)
- [ ] Refactor kgc-probe (types: 1029 → 3 files) (2 hours)

**Day 4**: JSDoc completion
- [ ] kgc-probe (101 functions) (2 hours)
- [ ] kgc-runtime (71 functions) (1.5 hours)
- [ ] kgc-claude (118 functions) (2 hours)

### Week 1: Final (Day 5)

**Day 5**: Verification and cleanup
- [ ] Add remaining OTEL to kgc-cli, kgc-multiverse, etc. (2 hours)
- [ ] Add remaining JSDoc to remaining packages (1 hour)
- [ ] Final test run and verification (1 hour)

---

## Acceptance Criteria

Before marking package as "COMPLIANT":

**kgc-4d**:
- [ ] All 26 doctests passing
- [ ] 100% test pass rate
- [ ] ≥50 OTEL spans added
- [ ] 100% JSDoc coverage on exports
- [ ] All files < 500 lines

**kgc-runtime**:
- [ ] All 22 failing tests fixed
- [ ] 100% test pass rate
- [ ] ≥40 OTEL spans added
- [ ] 100% JSDoc coverage on exports
- [ ] `schemas.mjs` split into 4 files

**kgc-claude**:
- [ ] All 13 test suites passing
- [ ] 100% test pass rate
- [ ] ≥80 OTEL spans added
- [ ] 100% JSDoc coverage on exports
- [ ] 20 oversized files refactored

**All packages**:
- [ ] Zero OTEL violations
- [ ] Zero missing JSDoc on exports
- [ ] All files < 500 lines
- [ ] All tests passing
- [ ] Zero lint warnings

---

## Success Metrics

| Metric | Current | Target |
|--------|---------|--------|
| Test Pass Rate | 60% | 100% |
| OTEL Coverage | 0% | 100% |
| JSDoc Coverage | 0% | 100% |
| Files > 500 lines | 59 | 0 |
| Test Failures | 40+ | 0 |
| Import Errors | 3+ | 0 |

---

## Notes

- All changes should be made in a single branch
- Run tests after EACH major change
- Use git commits for atomic changes
- Keep refactoring minimal (split vs. rewrite)
- Maintain backwards compatibility with exports
