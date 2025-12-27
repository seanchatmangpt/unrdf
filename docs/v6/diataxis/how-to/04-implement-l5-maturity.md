# How-To: Implement L5 Maturity for Custom Package

**Time**: ~5-7 days
**Difficulty**: Advanced
**Prerequisites**: Package at L1, understanding of [Maturity Ladder](../../MATURITY_LADDER.md)

---

## The L1-L5 Maturity Ladder

| Level | Description | Key Requirements |
|-------|-------------|------------------|
| **L1** | Compiles, runs, minimal examples | ✅ Builds, ≥1 test, basic README |
| **L2** | Stable public contracts | ✅ JSDoc 100%, Zod schemas, receipts |
| **L3** | Deterministic + replayable | ✅ Same input → same output, no Date.now() |
| **L4** | Adversarial safety (Poka-Yoke) | ✅ Timeout guards, input validation, resource limits |
| **L5** | Cross-package compositional closure | ✅ Integration tests, receipt composition, benchmarks |

**Goal**: Achieve L5 for production-grade quality.

---

## Example Package: `@unrdf/my-package`

We'll take `@unrdf/my-package` from L1 → L5.

**Starting State (L1)**:
- ✅ Package builds (`pnpm build`)
- ✅ 1 example works
- ✅ Basic README
- ❌ No Zod schemas
- ❌ No receipts
- ❌ Non-deterministic code
- ❌ No timeout guards
- ❌ No integration tests

---

## Step 1: Achieve L2 (Stable Contracts)

### 1.1 Add JSDoc to All Public Functions

**Before:**
```javascript
export function processData(data) {
  return data.value * 2;
}
```

**After:**
```javascript
/**
 * Process input data by doubling the value.
 *
 * @param {Object} data - Input data
 * @param {number} data.value - Numeric value to process
 * @param {Object} [data.metadata] - Optional metadata
 * @param {string} data.metadata.source - Data source identifier
 * @returns {number} Doubled value
 * @throws {ZodError} If validation fails
 *
 * @example
 * const result = await processData({ value: 5, metadata: { source: 'api' } });
 * // Returns: 10
 */
export function processData(data) {
  // Implementation
}
```

**Check coverage:**
```bash
timeout 5s grep -r "@param\|@returns" src/ --include="*.mjs" | wc -l
# Should match number of public functions
```

### 1.2 Add Zod Schemas

```javascript
import { z } from 'zod';

export const ProcessDataSchema = z.object({
  value: z.number().positive('Value must be positive'),
  metadata: z.object({
    source: z.string().min(1),
    timestamp: z.number().int().optional()
  }).optional()
});

export const processData = withReceipt(function processData(data) {
  const validated = ProcessDataSchema.parse(data);
  return validated.value * 2;
});
```

**Check schemas:**
```bash
timeout 5s grep -r "z\.object" src/ --include="*.mjs" | wc -l
```

### 1.3 Define Receipt Schemas

```javascript
export const ProcessDataReceiptSchema = z.object({
  hash: z.string().startsWith('sha256:'),
  timestamp: z.number().int(),
  operation: z.literal('processData'),
  inputs: ProcessDataSchema,
  output: z.number(),
  proof: z.object({
    merkleRoot: z.string().startsWith('sha256:'),
    signature: z.string().startsWith('ed25519:')
  })
});
```

### 1.4 Update CHANGELOG.md

```markdown
# Changelog

## [2.0.0] - 2025-01-01

### Breaking Changes
- **Zod validation required**: All inputs now validated at runtime
- **Receipts mandatory**: All operations emit receipts

### Added
- JSDoc coverage 100%
- Zod schemas for all public APIs
- Receipt generation for all operations
```

**L2 Verification:**
```bash
timeout 5s pnpm lint           # 0 errors
timeout 5s pnpm test           # All pass
timeout 5s pnpm run typecheck  # 0 errors
```

✅ **L2 Achieved!**

---

## Step 2: Achieve L3 (Deterministic)

### 2.1 Remove Non-Deterministic Code

**Before (Non-Deterministic):**
```javascript
export function createRecord(data) {
  return {
    ...data,
    id: Math.random().toString(36),        // ❌ Non-deterministic
    timestamp: Date.now()                  // ❌ Non-deterministic
  };
}
```

**After (Deterministic):**
```javascript
export const createRecord = withReceipt(function createRecord(data, timestamp, seed) {
  return {
    ...data,
    id: deterministicId(data, seed),      // ✅ Deterministic (seeded)
    timestamp                             // ✅ Passed as parameter
  };
});

function deterministicId(data, seed) {
  const hash = sha256(`${JSON.stringify(data)}-${seed}`);
  return hash.substring(0, 16);
}
```

**Check for non-deterministic calls:**
```bash
timeout 5s grep -r "Date\.now\|Math\.random" src/ --include="*.mjs" | wc -l
# Should be 0
```

### 2.2 Add Reproducible Fixtures

Create `test/fixtures/`:

```javascript
// test/fixtures/processData.json
{
  "input": {
    "value": 42,
    "metadata": {
      "source": "test",
      "timestamp": 1704067200000
    }
  },
  "expectedOutput": 84,
  "expectedReceipt": {
    "hash": "sha256:expected-hash...",
    "operation": "processData"
  }
}
```

**Test with fixture:**
```javascript
import { test } from 'node:test';
import assert from 'node:assert';
import fixture from './fixtures/processData.json' assert { type: 'json' };

test('processData is deterministic', async () => {
  const { value, receipt } = await processData(fixture.input);

  assert.strictEqual(value, fixture.expectedOutput);
  assert.strictEqual(receipt.hash, fixture.expectedReceipt.hash);
});
```

### 2.3 Test Determinism

Run operation 10 times, verify identical results:

```bash
for i in {1..10}; do
  node test/determinism-check.mjs >> output.log
done
sort output.log | uniq | wc -l
# Should be 1 (all outputs identical)
```

**L3 Verification:**
```bash
timeout 5s node test/determinism-check.mjs  # Hashes match
timeout 5s pnpm test                        # All pass
```

✅ **L3 Achieved!**

---

## Step 3: Achieve L4 (Adversarial Safety)

### 3.1 Add Timeout Guards

**Before (No timeout):**
```javascript
export async function fetchData(url) {
  const response = await fetch(url);  // ❌ No timeout
  return await response.json();
}
```

**After (With timeout):**
```javascript
import { withTimeout } from '@unrdf/core/utils';

export const fetchData = withReceipt(async function fetchData(url) {
  const response = await withTimeout(fetch(url), 5000); // ✅ 5s timeout
  return await response.json();
});
```

**Check timeout coverage:**
```bash
timeout 5s grep -r "withTimeout\|timeout.*5[0-9]{3}" src/ --include="*.mjs" | wc -l
```

### 3.2 Add Input Validation (Zod)

Ensure ALL external inputs are validated:

```javascript
export const fetchData = withReceipt(async function fetchData(url) {
  const UrlSchema = z.string().url(); // Validates URL format
  const validatedUrl = UrlSchema.parse(url);

  const response = await withTimeout(fetch(validatedUrl), 5000);
  const data = await response.json();

  const ResponseSchema = z.object({
    status: z.enum(['success', 'error']),
    data: z.unknown()
  });

  return ResponseSchema.parse(data); // Validate response
});
```

### 3.3 Add Resource Limits

```javascript
export const processLargeFile = withReceipt(async function processLargeFile(filePath) {
  const MAX_FILE_SIZE = 10 * 1024 * 1024; // 10 MB

  const stats = await fs.stat(filePath);
  if (stats.size > MAX_FILE_SIZE) {
    throw new Error(`File too large: ${stats.size} bytes (max: ${MAX_FILE_SIZE})`);
  }

  // Process file
});
```

### 3.4 Write Adversarial Tests

Create `test/adversarial/`:

```javascript
// test/adversarial/invalid-inputs.test.mjs
import { test } from 'node:test';
import assert from 'node:assert';
import { processData } from '../src/index.mjs';

test('processData rejects negative values', async () => {
  await assert.rejects(
    () => processData({ value: -5 }),
    /Value must be positive/
  );
});

test('processData rejects missing metadata.source', async () => {
  await assert.rejects(
    () => processData({ value: 5, metadata: {} }),
    /source/
  );
});

test('processData handles large numbers safely', async () => {
  const { value } = await processData({ value: Number.MAX_SAFE_INTEGER / 2 });
  assert.ok(Number.isSafeInteger(value));
});
```

**L4 Verification:**
```bash
timeout 5s pnpm test:adversarial  # All adversarial tests pass
timeout 5s grep -r "eval\|Function(" src/ --include="*.mjs" | wc -l  # 0
```

✅ **L4 Achieved!**

---

## Step 4: Achieve L5 (Cross-Package Composition)

### 4.1 Write Integration Tests with Other L5 Packages

Create `test/integration/`:

```javascript
// test/integration/with-kgc-4d.test.mjs
import { test } from 'node:test';
import assert from 'node:assert';
import { processData } from '../../src/index.mjs';
import { freeze } from '@unrdf/kgc-4d';

test('processData integrates with KGC-4D freeze', async () => {
  const { value, receipt } = await processData({ value: 10 });

  // Freeze receipt to Git
  const { receipt: freezeReceipt } = await freeze(receipt);

  assert.ok(freezeReceipt.hash.startsWith('sha256:'));
  assert.deepStrictEqual(freezeReceipt.proof.parentReceipts, [receipt.hash]);
});
```

**Test matrix** (package × other L5 packages):
```bash
timeout 30s pnpm test:integration
# Tests with: @unrdf/kgc-4d, @unrdf/yawl, @unrdf/hooks, etc.
```

### 4.2 Fix Circular Dependencies

```bash
timeout 5s npx madge --circular src/
# Should output: "No circular dependencies found"
```

If found, refactor to break cycles:
```javascript
// ❌ Circular
// a.mjs imports b.mjs
// b.mjs imports a.mjs

// ✅ Fixed
// Extract shared logic to c.mjs
// a.mjs imports c.mjs
// b.mjs imports c.mjs
```

### 4.3 Implement Receipt Composition

```javascript
import { composeDelta } from '@unrdf/v6-core/delta';

export const processAndFreeze = withReceipt(async function processAndFreeze(data) {
  const { value, receipt: processReceipt } = await processData(data);
  const { receipt: freezeReceipt } = await freeze(processReceipt);

  // Compose receipts
  const compositeReceipt = await composeDelta([processReceipt, freezeReceipt]);

  return { value, receipt: compositeReceipt };
});
```

### 4.4 Add Performance Benchmarks

Create `benchmarks/`:

```javascript
// benchmarks/processData.bench.mjs
import { bench, group, run } from 'mitata';
import { processData } from '../src/index.mjs';

group('processData performance', () => {
  bench('small input (10 bytes)', async () => {
    await processData({ value: 5 });
  });

  bench('large input (1 KB)', async () => {
    await processData({
      value: 5,
      metadata: { source: 'x'.repeat(1024) }
    });
  });
});

run();
```

Run benchmarks:
```bash
timeout 10s node benchmarks/processData.bench.mjs
```

**Expected output:**
```
┌─────────┬──────────────────────────┬───────────┬──────────────┐
│ (index) │ Name                     │ ops/sec   │ Margin       │
├─────────┼──────────────────────────┼───────────┼──────────────┤
│ 0       │ small input (10 bytes)   │ 125,432   │ ±0.54%       │
│ 1       │ large input (1 KB)       │ 98,234    │ ±1.23%       │
└─────────┴──────────────────────────┴───────────┴──────────────┘
```

**Save baseline:**
```bash
node benchmarks/processData.bench.mjs > benchmarks/baseline.txt
```

**Regression test:**
```bash
node benchmarks/processData.bench.mjs > benchmarks/current.txt
node scripts/compare-benchmarks.mjs
# Fails if regression >10%
```

### 4.5 OTEL Validation

```bash
node validation/run-all.mjs comprehensive --package @unrdf/my-package
grep "Score:" validation-output.log
```

**Expected:**
```
Score: 85/100 (Pass threshold: 80/100)
✅ Package @unrdf/my-package passes OTEL validation
```

**L5 Verification:**
```bash
timeout 30s pnpm test:integration     # All integration tests pass
timeout 5s npx madge --circular src/  # No circular deps
timeout 10s pnpm benchmark:compare    # No regressions >10%
timeout 10s node validation/run-all.mjs comprehensive  # Score ≥80
```

✅ **L5 Achieved!**

---

## Maturity Certificate

Generate a maturity certificate as proof:

```bash
npx @unrdf/v6-core maturity-report --package @unrdf/my-package
```

**Output** (`maturity-report.json`):
```json
{
  "package": "@unrdf/my-package",
  "version": "2.0.0",
  "maturityLevel": "L5",
  "timestamp": 1704067200000,
  "checks": {
    "l1": {
      "builds": true,
      "tests": true,
      "examples": true
    },
    "l2": {
      "jsdoc": true,
      "zodSchemas": true,
      "receipts": true,
      "changelog": true
    },
    "l3": {
      "deterministic": true,
      "noDateNow": true,
      "fixtures": true
    },
    "l4": {
      "timeouts": true,
      "validation": true,
      "resourceLimits": true,
      "adversarialTests": true
    },
    "l5": {
      "integrationTests": true,
      "noCircularDeps": true,
      "receiptComposition": true,
      "benchmarks": true,
      "otelScore": 85
    }
  },
  "receipt": {
    "hash": "sha256:maturity-cert-hash...",
    "signature": "ed25519:sig..."
  }
}
```

**Save certificate:**
```bash
mv maturity-report.json docs/MATURITY_CERTIFICATE.json
```

---

## Summary Checklist

### L2: Stable Contracts
- ✅ JSDoc coverage ≥95%
- ✅ Zod schemas for all inputs/outputs
- ✅ Receipt schemas defined
- ✅ CHANGELOG.md updated
- ✅ `pnpm lint` passes

### L3: Deterministic
- ✅ No `Date.now()` or `Math.random()` in business logic
- ✅ Reproducible fixtures created
- ✅ Determinism tests pass 10/10 runs
- ✅ Replay tests pass

### L4: Adversarial Safety
- ✅ Timeout guards on all I/O (default 5s)
- ✅ Zod validation on ALL external inputs
- ✅ Resource limits enforced
- ✅ Adversarial tests pass 100%

### L5: Compositional Closure
- ✅ Integration tests with all L5 packages
- ✅ No circular dependencies
- ✅ Receipt composition implemented
- ✅ Performance benchmarks exist
- ✅ OTEL score ≥80/100

**Congratulations! Your package is production-ready at L5!** 🎉

---

## Next Steps

1. **Publish L5 package**: `pnpm publish`
2. **Add to v6 core**: Submit PR to include in core-10 list
3. **Maintain maturity**: Run regression tests on every commit
4. **Help others**: Contribute L5 templates and tooling

**Reference**: [Maturity Ladder Full Spec](../../MATURITY_LADDER.md)
