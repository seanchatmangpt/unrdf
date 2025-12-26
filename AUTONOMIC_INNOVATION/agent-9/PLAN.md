# Agent 9: Shadow Modes and Mismatch Reporting

**Role**: Implement deterministic shadow modes to run legacy and new code in parallel, detect mismatches, and report them with content-addressable hashing.

---

## Overview

Shadow modes enable zero-trust migration by running both legacy and facade implementations side-by-side, comparing outputs deterministically, and generating content-addressed mismatch reports. This allows gradual rollout with full observability.

**Core Principle**: Every mismatch report is content-addressed. Same input + same difference = same hash, regardless of when/where it occurred.

---

## Files to Create

### Core Shadow Modes
1. **`./AUTONOMIC_INNOVATION/agent-9/src/shadow-write.mjs`**
   - Shadow write operations (mutations)
   - Execute both legacy and facade handlers
   - Compare results deterministically
   - Generate mismatch reports on divergence

2. **`./AUTONOMIC_INNOVATION/agent-9/src/shadow-read.mjs`**
   - Shadow read operations (queries)
   - Validate consistency between implementations
   - Handle timing differences gracefully

3. **`./AUTONOMIC_INNOVATION/agent-9/src/partial-serve.mjs`**
   - Traffic routing engine
   - Route percentage of requests to facade
   - Track routing decisions
   - Aggregate mismatch statistics

### Reporting Infrastructure
4. **`./AUTONOMIC_INNOVATION/agent-9/src/mismatch-report.mjs`**
   - Canonical mismatch report formatting
   - Content-addressable hashing
   - Deterministic serialization
   - Report aggregation and deduplication

5. **`./AUTONOMIC_INNOVATION/agent-9/src/canonical-diff.mjs`**
   - Deep structural comparison
   - Canonical ordering for objects/arrays
   - Diff path generation (e.g., "legacy.field[2].id differs: A vs B")
   - Handle RDF quads, JSON, primitives

### Public API
6. **`./AUTONOMIC_INNOVATION/agent-9/src/index.mjs`**
   - Re-export all public functions
   - JSDoc type definitions
   - Usage examples

### Tests
7. **`./AUTONOMIC_INNOVATION/agent-9/test/shadow-modes.test.mjs`**
   - Shadow write with matching outputs
   - Shadow write with mismatches
   - Shadow read consistency validation
   - Partial serve routing
   - Report determinism (same data → same hash)
   - Mismatch deduplication

---

## Key Functions and Signatures

### Shadow Write Mode

```javascript
/**
 * Execute write operation in shadow mode
 *
 * @param {Function} legacyHandler - Legacy implementation (input) => Promise<result>
 * @param {Function} facadeHandler - Facade implementation (input) => Promise<result>
 * @param {any} input - Operation input
 * @param {Object} options - Configuration
 * @param {boolean} options.useLegacyResult - Return legacy result (default: true)
 * @param {Function} options.onMismatch - Callback for mismatches
 * @returns {Promise<ShadowWriteResult>}
 *
 * @typedef {Object} ShadowWriteResult
 * @property {boolean} mismatch - Whether outputs differed
 * @property {any} result - The result to use (based on useLegacyResult)
 * @property {any} [legacyResult] - Legacy handler result
 * @property {any} [facadeResult] - Facade handler result
 * @property {string} [reportHash] - Content hash of mismatch report
 * @property {MismatchReport} [report] - Full mismatch report (if mismatch occurred)
 */
export async function shadowWrite(legacyHandler, facadeHandler, input, options = {}) {
  const { useLegacyResult = true, onMismatch } = options;

  // Execute both handlers in parallel
  const [legacyResult, facadeResult] = await Promise.allSettled([
    legacyHandler(input),
    facadeHandler(input)
  ]);

  // Handle execution failures
  const legacySuccess = legacyResult.status === 'fulfilled';
  const facadeSuccess = facadeResult.status === 'fulfilled';

  if (!legacySuccess && !facadeSuccess) {
    throw new Error('Both legacy and facade handlers failed');
  }

  // Compare results using canonical diff
  const diff = canonicalDiff(
    legacySuccess ? legacyResult.value : { error: legacyResult.reason },
    facadeSuccess ? facadeResult.value : { error: facadeResult.reason }
  );

  if (diff.hasDifference) {
    const report = createMismatchReport({
      type: 'write',
      input,
      legacyOutput: legacySuccess ? legacyResult.value : { error: legacyResult.reason },
      facadeOutput: facadeSuccess ? facadeResult.value : { error: facadeResult.reason },
      difference: diff
    });

    if (onMismatch) {
      await onMismatch(report);
    }

    return {
      mismatch: true,
      result: useLegacyResult ? legacyResult.value : facadeResult.value,
      legacyResult: legacyResult.value,
      facadeResult: facadeResult.value,
      reportHash: report.hash,
      report
    };
  }

  return {
    mismatch: false,
    result: legacySuccess ? legacyResult.value : facadeResult.value
  };
}
```

### Shadow Read Mode

```javascript
/**
 * Execute read operation in shadow mode
 *
 * @param {Function} legacyHandler - Legacy query implementation
 * @param {Function} facadeHandler - Facade query implementation
 * @param {any} query - Query input
 * @param {Object} options - Configuration
 * @returns {Promise<ShadowReadResult>}
 */
export async function shadowRead(legacyHandler, facadeHandler, query, options = {}) {
  // Similar to shadowWrite but optimized for reads
  // May include result set comparison (order-independent for SPARQL results)
  // Timing tolerance for eventual consistency scenarios
}
```

### Partial Serve Router

```javascript
/**
 * Route requests between legacy and facade based on percentage
 *
 * @param {Object} router - Routing configuration
 * @param {number} router.facadePercent - Percentage to route to facade (0-100)
 * @param {string} router.strategy - 'random' | 'hash-based' | 'canary'
 * @param {Function} legacyHandler - Legacy implementation
 * @param {Function} facadeHandler - Facade implementation
 * @param {any} request - Request to handle
 * @returns {Promise<PartialServeResult>}
 *
 * @typedef {Object} PartialServeResult
 * @property {any} response - Handler response
 * @property {string} routing - 'legacy' | 'facade' | 'shadow'
 * @property {MismatchReport[]} mismatches - Any detected mismatches (if shadow mode)
 */
export async function partialServe(router, legacyHandler, facadeHandler, request) {
  const { facadePercent = 0, strategy = 'random' } = router;

  // Determine routing based on strategy
  const routeToFacade = selectRoute(request, facadePercent, strategy);

  if (routeToFacade === 'shadow') {
    // Run both and compare
    const result = await shadowWrite(legacyHandler, facadeHandler, request, {
      useLegacyResult: true
    });

    return {
      response: result.result,
      routing: 'shadow',
      mismatches: result.mismatch ? [result.report] : []
    };
  }

  const handler = routeToFacade === 'facade' ? facadeHandler : legacyHandler;
  const response = await handler(request);

  return {
    response,
    routing: routeToFacade,
    mismatches: []
  };
}

/**
 * Select routing destination
 * @private
 */
function selectRoute(request, facadePercent, strategy) {
  if (facadePercent === 0) return 'legacy';
  if (facadePercent === 100) return 'facade';

  switch (strategy) {
    case 'random':
      return Math.random() * 100 < facadePercent ? 'facade' : 'legacy';

    case 'hash-based': {
      // Deterministic routing based on request hash
      const hash = hashRequest(request);
      const bucket = (hash % 100);
      return bucket < facadePercent ? 'facade' : 'legacy';
    }

    case 'shadow':
      // Always run both (100% shadow mode)
      return 'shadow';

    default:
      return 'legacy';
  }
}
```

### Canonical Mismatch Report

```javascript
/**
 * Create deterministic mismatch report
 *
 * @param {Object} params - Report parameters
 * @param {string} params.type - 'write' | 'read'
 * @param {any} params.input - Operation input
 * @param {any} params.legacyOutput - Legacy result
 * @param {any} params.facadeOutput - Facade result
 * @param {Object} params.difference - Canonical diff object
 * @returns {MismatchReport}
 *
 * @typedef {Object} MismatchReport
 * @property {bigint} timestamp - Nanosecond timestamp (NOT part of hash)
 * @property {string} type - Operation type
 * @property {any} input - Canonical input
 * @property {any} legacyOutput - Canonical legacy output
 * @property {any} facadeOutput - Canonical facade output
 * @property {string[]} differencePaths - Array of difference descriptions
 * @property {string} hash - Content hash (excludes timestamp)
 */
export function createMismatchReport({ type, input, legacyOutput, facadeOutput, difference }) {
  const timestamp = now(); // Nanosecond precision

  // Canonical serialization (deterministic ordering)
  const canonical = {
    type,
    input: canonicalSerialize(input),
    legacyOutput: canonicalSerialize(legacyOutput),
    facadeOutput: canonicalSerialize(facadeOutput),
    differencePaths: difference.paths.sort() // Lexicographic sort
  };

  // Hash ONLY the canonical content (NOT timestamp)
  const hash = hashMismatchReport(canonical);

  return {
    timestamp, // For logging/debugging, not part of hash
    ...canonical,
    hash
  };
}

/**
 * Hash mismatch report content (deterministic)
 *
 * @param {Object} canonical - Canonical report object (no timestamp)
 * @returns {string} - Hex hash
 */
export async function hashMismatchReport(canonical) {
  const { blake3 } = await import('hash-wasm');

  // Deterministic JSON serialization
  const json = JSON.stringify(canonical, canonicalReplacer);

  // Content-addressed hash
  return await blake3(json);
}

/**
 * Canonical JSON replacer (sort object keys)
 * @private
 */
function canonicalReplacer(key, value) {
  if (value && typeof value === 'object' && !Array.isArray(value)) {
    return Object.keys(value)
      .sort()
      .reduce((sorted, k) => {
        sorted[k] = value[k];
        return sorted;
      }, {});
  }
  return value;
}
```

### Canonical Diff

```javascript
/**
 * Deep comparison with canonical ordering
 *
 * @param {any} legacy - Legacy value
 * @param {any} facade - Facade value
 * @returns {DiffResult}
 *
 * @typedef {Object} DiffResult
 * @property {boolean} hasDifference - Whether values differ
 * @property {string[]} paths - Array of difference paths
 */
export function canonicalDiff(legacy, facade) {
  const paths = [];

  function compare(a, b, path = 'root') {
    // Type mismatch
    if (typeof a !== typeof b) {
      paths.push(`${path}: type differs (${typeof a} vs ${typeof b})`);
      return;
    }

    // Primitive comparison
    if (typeof a !== 'object' || a === null || b === null) {
      if (a !== b) {
        paths.push(`${path}: ${JSON.stringify(a)} vs ${JSON.stringify(b)}`);
      }
      return;
    }

    // Array comparison (order-sensitive)
    if (Array.isArray(a) && Array.isArray(b)) {
      if (a.length !== b.length) {
        paths.push(`${path}.length: ${a.length} vs ${b.length}`);
      }
      const len = Math.max(a.length, b.length);
      for (let i = 0; i < len; i++) {
        compare(a[i], b[i], `${path}[${i}]`);
      }
      return;
    }

    // Object comparison (key-order independent)
    const keysA = Object.keys(a).sort();
    const keysB = Object.keys(b).sort();

    const allKeys = new Set([...keysA, ...keysB]);
    for (const key of allKeys) {
      if (!(key in a)) {
        paths.push(`${path}.${key}: missing in legacy`);
      } else if (!(key in b)) {
        paths.push(`${path}.${key}: missing in facade`);
      } else {
        compare(a[key], b[key], `${path}.${key}`);
      }
    }
  }

  compare(legacy, facade);

  return {
    hasDifference: paths.length > 0,
    paths
  };
}

/**
 * Canonicalize value for deterministic serialization
 */
function canonicalSerialize(value) {
  if (value && typeof value === 'object') {
    if (Array.isArray(value)) {
      return value.map(canonicalSerialize);
    }

    // Sort object keys
    const sorted = {};
    Object.keys(value).sort().forEach(key => {
      sorted[key] = canonicalSerialize(value[key]);
    });
    return sorted;
  }

  return value;
}
```

---

## Test Scenarios

### 1. Shadow Write - Matching Outputs
```javascript
test('shadow write with matching outputs', async () => {
  const legacyAdd = async (x) => x + 1;
  const facadeAdd = async (x) => x + 1;

  const result = await shadowWrite(legacyAdd, facadeAdd, 5);

  assert.strictEqual(result.mismatch, false);
  assert.strictEqual(result.result, 6);
  assert.strictEqual(result.reportHash, undefined);
});
```

### 2. Shadow Write - Detected Mismatch
```javascript
test('shadow write detects and reports mismatch', async () => {
  const legacyAdd = async (x) => x + 1;
  const facadeAdd = async (x) => x + 2; // Bug!

  const reports = [];
  const result = await shadowWrite(legacyAdd, facadeAdd, 5, {
    onMismatch: (report) => reports.push(report)
  });

  assert.strictEqual(result.mismatch, true);
  assert.strictEqual(result.result, 6); // Uses legacy by default
  assert.strictEqual(result.legacyResult, 6);
  assert.strictEqual(result.facadeResult, 7);
  assert.ok(result.reportHash);
  assert.strictEqual(reports.length, 1);
  assert.ok(reports[0].timestamp);
  assert.strictEqual(reports[0].type, 'write');
});
```

### 3. Mismatch Report Determinism
```javascript
test('same mismatch produces same hash', async () => {
  const legacy = async (x) => ({ value: x + 1 });
  const facade = async (x) => ({ value: x + 2 });

  const result1 = await shadowWrite(legacy, facade, 5);
  const result2 = await shadowWrite(legacy, facade, 5);

  // Same hash despite different timestamps
  assert.strictEqual(result1.reportHash, result2.reportHash);
  assert.notStrictEqual(result1.report.timestamp, result2.report.timestamp);
});
```

### 4. Canonical Diff - Deep Object Comparison
```javascript
test('canonical diff detects nested differences', () => {
  const legacy = { user: { id: 1, name: 'Alice' }, count: 5 };
  const facade = { user: { id: 1, name: 'Bob' }, count: 5 };

  const diff = canonicalDiff(legacy, facade);

  assert.strictEqual(diff.hasDifference, true);
  assert.ok(diff.paths.includes('root.user.name: "Alice" vs "Bob"'));
  assert.strictEqual(diff.paths.length, 1);
});
```

### 5. Partial Serve - Random Routing
```javascript
test('partial serve routes based on percentage', async () => {
  const legacy = async () => 'legacy';
  const facade = async () => 'facade';

  const router = { facadePercent: 50, strategy: 'random' };

  const results = await Promise.all(
    Array(100).fill(0).map(() =>
      partialServe(router, legacy, facade, {})
    )
  );

  const facadeCount = results.filter(r => r.routing === 'facade').length;

  // Should be ~50% (allow 30-70% range for randomness)
  assert.ok(facadeCount > 30 && facadeCount < 70);
});
```

### 6. Partial Serve - Shadow Mode
```javascript
test('partial serve shadow mode runs both handlers', async () => {
  const legacy = async (x) => x + 1;
  const facade = async (x) => x + 2;

  const router = { strategy: 'shadow' };

  const result = await partialServe(router, legacy, facade, 5);

  assert.strictEqual(result.routing, 'shadow');
  assert.strictEqual(result.response, 6); // Legacy result
  assert.strictEqual(result.mismatches.length, 1);
  assert.ok(result.mismatches[0].hash);
});
```

### 7. Mismatch Deduplication
```javascript
test('duplicate mismatches share same hash', async () => {
  const legacy = async (x) => x * 2;
  const facade = async (x) => x * 3;

  const reports = [];
  const options = {
    onMismatch: (report) => reports.push(report)
  };

  // Generate same mismatch 5 times
  for (let i = 0; i < 5; i++) {
    await shadowWrite(legacy, facade, 10, options);
  }

  // All reports should have same hash
  const hashes = new Set(reports.map(r => r.hash));
  assert.strictEqual(hashes.size, 1);

  // But different timestamps
  const timestamps = new Set(reports.map(r => r.timestamp.toString()));
  assert.strictEqual(timestamps.size, 5);
});
```

---

## Dependencies

### External Packages (from INVENTORY.md)
- `hash-wasm` - Content-addressable hashing (root dependency)
- `@unrdf/kgc-4d` - Nanosecond timestamps via `now()`
- `zod` - Input validation (if needed)

### Internal AUTONOMIC_INNOVATION Modules
- Agent 1 shared utilities (canonical ordering, if provided)
- Agent 7 facades (integration target)
- Agent 8 atomic store (integration target)

---

## Exports (Public API)

```javascript
// Shadow modes
export { shadowWrite } from './shadow-write.mjs';
export { shadowRead } from './shadow-read.mjs';
export { partialServe } from './partial-serve.mjs';

// Reporting
export { createMismatchReport, hashMismatchReport } from './mismatch-report.mjs';
export { canonicalDiff, canonicalSerialize } from './canonical-diff.mjs';
```

---

## Integration with Other Agents

### Agent 7 (Facade Generation)
Shadow modes will test facades against legacy implementations:
```javascript
const facade = generateFacade(profile, lens);
const result = await shadowWrite(legacyStore.add, facade.add, quad);
```

### Agent 8 (Atomic Application)
Shadow modes validate atomic operations:
```javascript
const legacyApply = (capsule) => legacyStore.applyBatch(capsule.ops);
const facadeApply = (capsule) => atomicStore.applyCapsule(capsule);

await shadowWrite(legacyApply, facadeApply, capsule);
```

### Agent 1 (Orchestrator)
Integration demo will showcase full shadow mode workflow:
```javascript
// Phase 1: 0% facade (legacy only)
await partialServe({ facadePercent: 0 }, legacy, facade, req);

// Phase 2: 10% facade (canary)
await partialServe({ facadePercent: 10 }, legacy, facade, req);

// Phase 3: 100% shadow (compare all)
await partialServe({ strategy: 'shadow' }, legacy, facade, req);

// Phase 4: 100% facade (fully migrated)
await partialServe({ facadePercent: 100 }, legacy, facade, req);
```

---

## Determinism Guarantees

1. **Content-Addressed Reports**: Hash = f(input, legacyOutput, facadeOutput, diff), NOT f(timestamp)
2. **Canonical Serialization**: Object keys sorted, arrays preserved, types normalized
3. **Idempotent Hashing**: Same mismatch always produces same hash
4. **Timestamp Independence**: Two runs with same data produce same hash despite different timestamps

### Hash Function Choice
Using `blake3` from `hash-wasm`:
- Faster than SHA-256
- Cryptographically secure
- Deterministic and reproducible
- 32-byte output (64 hex chars)

---

## Performance Considerations

1. **Parallel Execution**: Legacy and facade run concurrently (not sequential)
2. **Early Return**: If legacy fails and facade succeeds (or vice versa), no comparison needed
3. **Lazy Report Generation**: Only create full report on mismatch
4. **Hash Caching**: Reports with same content share same hash (deduplication)
5. **Async Reporting**: `onMismatch` callback can log/store asynchronously

---

## Error Handling

### Handler Failures
- If legacy fails but facade succeeds: Report mismatch (legacy error vs facade result)
- If facade fails but legacy succeeds: Report mismatch (legacy result vs facade error)
- If both fail with same error: No mismatch (consistent behavior)
- If both fail with different errors: Mismatch report

### Comparison Failures
- Handle circular references (mark as "unparseable")
- Handle BigInt, Symbol, Function (convert to string representation)
- Handle undefined vs null (different types)

---

## Observability Hooks

Optional OTEL integration (if provided by Agent 1):
```javascript
await shadowWrite(legacy, facade, input, {
  onMismatch: async (report) => {
    // Log to console
    console.warn('[Shadow Mismatch]', report.hash, report.differencePaths);

    // Send to OTEL (if available)
    if (typeof trace !== 'undefined') {
      trace.getActiveSpan()?.addEvent('shadow_mismatch', {
        hash: report.hash,
        type: report.type,
        paths: report.differencePaths.join(', ')
      });
    }

    // Store in database
    await mismatchDB.insert(report);
  }
});
```

---

## Usage Examples

### Example 1: Gradual Rollout
```javascript
import { partialServe } from '@autonomic/agent-9';

// Week 1: 0% facade (establish baseline)
const phase1 = { facadePercent: 0 };

// Week 2: 5% facade (canary test)
const phase2 = { facadePercent: 5, strategy: 'hash-based' };

// Week 3: 50% facade (A/B test)
const phase3 = { facadePercent: 50, strategy: 'random' };

// Week 4: 100% shadow (full comparison)
const phase4 = { strategy: 'shadow' };

// Week 5: 100% facade (fully migrated)
const phase5 = { facadePercent: 100 };

for await (const request of requestStream) {
  const result = await partialServe(currentPhase, legacy, facade, request);

  if (result.mismatches.length > 0) {
    console.error('Mismatch detected:', result.mismatches[0].hash);
  }
}
```

### Example 2: Read Validation
```javascript
import { shadowRead } from '@autonomic/agent-9';

const legacyQuery = async (sparql) => legacyStore.query(sparql);
const facadeQuery = async (sparql) => facadeStore.query(sparql);

const result = await shadowRead(legacyQuery, facadeQuery, `
  SELECT ?s ?p ?o WHERE {
    ?s ?p ?o .
  } LIMIT 100
`);

if (result.mismatch) {
  console.error('Query results differ:', result.reportHash);
}
```

---

## Success Criteria

- [ ] All 7 source files implemented with JSDoc
- [ ] All 7 test scenarios passing
- [ ] Determinism verified: same mismatch → same hash
- [ ] Integration with Agent 7 facades
- [ ] Integration with Agent 8 atomic store
- [ ] Export all functions via `index.mjs`
- [ ] Zero external dependencies added (use only INVENTORY.md packages)
- [ ] Hash-based deduplication working
- [ ] Performance: shadow mode ≤2x latency of single handler

---

## Implementation Notes

### Phase 1: Core Shadow Modes (MVP)
- `shadow-write.mjs` - Basic parallel execution
- `canonical-diff.mjs` - Deep comparison
- `mismatch-report.mjs` - Hash generation
- Tests for matching/mismatching scenarios

### Phase 2: Advanced Routing
- `partial-serve.mjs` - Traffic routing
- Hash-based deterministic routing
- Mismatch aggregation

### Phase 3: Read Support
- `shadow-read.mjs` - Query validation
- Result set comparison (order-independent)
- Timing tolerance configuration

### Phase 4: Integration
- Connect to Agent 7 facades
- Connect to Agent 8 atomic store
- E2E demo with gradual rollout

---

## Open Questions

1. **SPARQL Result Comparison**: Should SPARQL result sets be compared order-independently?
   - **Answer**: Yes, sort by all variables before comparison

2. **Timing Tolerance**: Should we allow small timing differences in shadow read mode?
   - **Answer**: Add `timingToleranceMs` option, default 0 (strict)

3. **RDF Quad Comparison**: How to compare quads deterministically?
   - **Answer**: Use canonical N-Quads serialization, then string comparison

4. **Report Storage**: Where should mismatch reports be stored?
   - **Answer**: Caller's responsibility via `onMismatch` callback

---

## Appendix: Content-Addressed Hash Example

```javascript
// Same input, same mismatch → same hash

// Run 1 (timestamp: 1000000000000000n)
const report1 = {
  timestamp: 1000000000000000n,
  type: 'write',
  input: { x: 5 },
  legacyOutput: 6,
  facadeOutput: 7,
  differencePaths: ['root: 6 vs 7'],
  hash: 'a1b2c3...' // blake3(canonical content)
};

// Run 2 (timestamp: 2000000000000000n)
const report2 = {
  timestamp: 2000000000000000n, // Different timestamp
  type: 'write',
  input: { x: 5 },
  legacyOutput: 6,
  facadeOutput: 7,
  differencePaths: ['root: 6 vs 7'],
  hash: 'a1b2c3...' // Same hash! (timestamp not included)
};

assert(report1.hash === report2.hash); // ✅
assert(report1.timestamp !== report2.timestamp); // ✅
```

This enables:
- Deduplication across time
- Aggregation by hash
- Trend analysis ("hash X occurred 1000 times this week")
- Deterministic testing (same test → same hash)

---

**End of Plan**
