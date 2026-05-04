# Issues Prioritized

**Package**: @unrdf/kgc-probe
**Review Date**: 2025-12-27
**Total Issues**: 12

---

## Critical (Must Fix Before Merge)

### Issue #1: Missing `createObservation` Export
**Severity**: CRITICAL
**Impact**: 12 test failures (35% of failing tests)
**Root Cause**: Test files import `createObservation` but function is not exported

**Location**:
- Tests using it: `test/test-merge-correctness.test.mjs:105, 116, 139, 179, 206, 315`

**Fix**:
```javascript
// Add to src/artifact.mjs or src/types.mjs:
export function createObservation(agent, kind, index, options = {}) {
  return {
    id: randomUUID(),
    agent,
    timestamp: options.timestamp || new Date().toISOString(),
    kind,
    severity: options.severity || 'info',
    subject: options.subject || 'unknown',
    predicate: options.predicate,
    object: options.object,
    evidence: options.evidence || { query: '', result: null, witnesses: [] },
    metrics: options.metrics || { confidence: 0.9, coverage: 0.9, latency_ms: 0 },
    tags: options.tags || []
  };
}
```

**Effort**: 30 minutes

---

### Issue #2: 47 Lint Warnings
**Severity**: CRITICAL
**Impact**: CI/CD gate failure
**Root Cause**: Unused variables, missing JSDoc

**Locations**:
- `src/agents/index.mjs`: 32 warnings
- `src/cli.mjs`: 10 warnings
- `src/artifact.mjs`: 3 warnings
- `src/orchestrator.mjs`: 1 warning
- `src/receipts/index.mjs`: 1 warning

**Fixes**:
```javascript
// 1. Prefix unused params with underscore
async scan(_config) { ... }  // instead of scan(config)

// 2. Remove unused imports
// artifact.mjs line 13-14:
// REMOVE: import { randomUUID } from 'crypto';
// REMOVE: import { ArtifactSchema, DiffResultSchema } from './types.mjs';

// 3. Add JSDoc to exported functions
/**
 * Create completion agent
 * @returns {CompletionAgent}
 */
export function createCompletionAgent() { ... }
```

**Effort**: 2 hours

---

### Issue #3: Missing 'guard_violation' Kind
**Severity**: CRITICAL
**Impact**: Runtime schema validation errors for guard observations
**Root Cause**: ObservationSchema.kind enum doesn't include 'guard_violation'

**Location**: `src/types.mjs:26-37`

**Current**:
```javascript
kind: z.enum([
  'completeness', 'consistency', 'conformance', 'coverage',
  'caching', 'completeness_level', 'coherence', 'clustering',
  'classification', 'collaboration'
])
```

**Fix**:
```javascript
kind: z.enum([
  'completeness', 'consistency', 'conformance', 'coverage',
  'caching', 'completeness_level', 'coherence', 'clustering',
  'classification', 'collaboration', 'guard_violation'
])
```

**Effort**: 5 minutes

---

## Major (Should Fix)

### Issue #4: DatabaseStorage Not Implemented
**Severity**: MAJOR
**Impact**: Cannot use production storage backend
**Location**: `src/storage/index.mjs:292-346`

**Current**: All methods throw "not implemented"

**Fix**: Implement using @unrdf/oxigraph as specified in dependencies

**Effort**: 4-8 hours

---

### Issue #5: Placeholder Hash Functions
**Severity**: MAJOR
**Impact**: Hashes are not cryptographically secure
**Locations**:
- `src/artifact.mjs:113-125` - `computeSimpleHash`
- `src/orchestrator.mjs:281-288` - `hashString`

**Current**: Returns string like 'blake3_placeholder_0000...'

**Fix**: Use hash-wasm (already in dependencies):
```javascript
import { blake3 } from 'hash-wasm';

async function computeBlake3Hash(data) {
  const hash = await blake3(data);
  return hash;
}
```

**Effort**: 2 hours

---

### Issue #6: 8 Agent Implementations Return Empty
**Severity**: MAJOR
**Impact**: Only 10% of agents produce observations
**Location**: `src/agents/index.mjs`

**Affected Agents**:
- ConsistencyAgent:102-104
- ConformanceAgent:119-121
- CoverageAgent:136-138
- CachingAgent:153-155
- CompletenessAgent:170-172
- CoherenceAgent:187-189
- ClusteringAgent:204-206
- ClassificationAgent:221-223

**Fix**: Implement actual SPARQL queries and observation generation

**Effort**: 2-4 hours per agent = 16-32 hours total

---

### Issue #7: 4 Guard Test Failures
**Severity**: MAJOR
**Impact**: 4 test failures in guards.test.mjs

**Failures**:
1. `.npmrc` not in forbidden paths
2. Backslash normalization failing
3. `github.com` not in network allowlist
4. One unknown

**Fix**: Update guard patterns in guards.mjs

**Effort**: 1 hour

---

## Minor (Nice to Fix)

### Issue #8: Use Object.hasOwn
**Severity**: MINOR
**Location**: `src/artifact.mjs:363`

**Current**: `summary.by_severity.hasOwnProperty(obs.severity)`
**Fix**: `Object.hasOwn(summary.by_severity, obs.severity)`

**Effort**: 5 minutes

---

### Issue #9: CLI Handler Placeholders
**Severity**: MINOR
**Location**: `packages/kgc-cli/src/extensions/kgc-probe.mjs`

**Affected**: validate, diff, list commands return placeholder data

**Fix**: Implement actual storage queries

**Effort**: 4 hours

---

### Issue #10: Missing SPARC Receipt Types
**Severity**: MINOR (for v1.0, major for v2.0)
**Impact**: Not following SPARC specification for receipts

**Missing**:
- ProbeObservationReceipt
- ProbeMergeReceipt
- ProbeVerificationReceipt
- Per-agent hash chains
- Merkle tree proofs

**Location**: Should be in `src/receipts/` (partially exists)

**Effort**: 16-24 hours

---

### Issue #11: Jaccard Similarity Calculation Bug
**Severity**: MINOR
**Impact**: 1 test failure
**Location**: `src/artifact.mjs:263-265`

**Test**: test-merge-correctness.test.mjs:388

**Expected**: Identical artifacts should have similarity 1.0
**Actual**: Returns 2

**Fix**: Review Jaccard formula implementation

**Effort**: 30 minutes

---

### Issue #12: Unused Imports
**Severity**: MINOR
**Impact**: Lint warnings only

**Locations**:
- `src/artifact.mjs:13` - randomUUID
- `src/artifact.mjs:14` - ArtifactSchema, DiffResultSchema
- `src/orchestrator.mjs:13` - ProbeConfigSchema
- `src/cli.mjs:20` - ProbeError
- `src/cli.mjs:22` - ArtifactNotFoundError
- `src/cli.mjs:200` - verifyMerkleProof

**Fix**: Remove unused imports

**Effort**: 10 minutes

---

## Summary by Effort

| Priority | Issue Count | Estimated Effort |
|----------|-------------|------------------|
| Critical | 3 | 2.5 hours |
| Major | 4 | 23-43 hours |
| Minor | 5 | 21-29 hours |
| **Total** | **12** | **46.5-74.5 hours** |

## Quick Wins (< 1 hour to fix)

1. Add 'guard_violation' to enum (5 min)
2. Export createObservation (30 min)
3. Use Object.hasOwn (5 min)
4. Remove unused imports (10 min)
5. Fix Jaccard calculation (30 min)

**Total quick wins**: 5 issues in ~1.5 hours
