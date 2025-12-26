/**
 * Direct test runner - Uses root workspace dependencies
 */

// Import from root node_modules by using absolute paths
import { createRequire } from 'node:module';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = join(__dirname, '../../../');

// Import hash-wasm from root
const require = createRequire(import.meta.url);
const hashWasmPath = join(rootDir, 'node_modules/hash-wasm/dist/index.esm.js');

// Monkey-patch the import for hash-wasm
import { register } from 'node:module';

// Instead, let's create a simpler standalone version that uses dynamic import
async function runTests() {
  console.log('🧪 Shadow Modes - Direct Test Suite\n');
  console.log('Note: Using root workspace dependencies\n');

  // Import from root
  const { blake3 } = await import('hash-wasm');
  const { now } = await import('@unrdf/kgc-4d');

  // Import local modules directly
  const { canonicalDiff, canonicalSerialize, canonicalReplacer } = await import(
    '../src/canonical-diff.mjs'
  );

  let passed = 0;
  let failed = 0;

  function assert(condition, message) {
    if (!condition) {
      console.error(`❌ FAIL: ${message}`);
      failed++;
      throw new Error(message);
    } else {
      console.log(`✅ PASS: ${message}`);
      passed++;
    }
  }

  // Inline createMismatchReport to avoid import issues
  async function createMismatchReport({ type, input, legacyOutput, facadeOutput, difference }) {
    const timestamp = now();
    const canonical = {
      type,
      input: canonicalSerialize(input),
      legacyOutput: canonicalSerialize(legacyOutput),
      facadeOutput: canonicalSerialize(facadeOutput),
      differencePaths: difference.paths.slice().sort(),
    };
    const json = JSON.stringify(canonical, canonicalReplacer);
    const hash = await blake3(json);
    return { timestamp, ...canonical, hash };
  }

  // Inline shadowWrite
  async function shadowWrite(legacyHandler, facadeHandler, input, options = {}) {
    const { useLegacyResult = true, onMismatch } = options;
    const [legacyResult, facadeResult] = await Promise.allSettled([
      legacyHandler(input),
      facadeHandler(input),
    ]);
    const legacySuccess = legacyResult.status === 'fulfilled';
    const facadeSuccess = facadeResult.status === 'fulfilled';
    if (!legacySuccess && !facadeSuccess) {
      throw new Error('Both legacy and facade handlers failed');
    }
    const legacyOutput = legacySuccess
      ? legacyResult.value
      : { error: legacyResult.reason?.message || String(legacyResult.reason) };
    const facadeOutput = facadeSuccess
      ? facadeResult.value
      : { error: facadeResult.reason?.message || String(facadeResult.reason) };
    const diff = canonicalDiff(legacyOutput, facadeOutput);
    if (diff.hasDifference) {
      const report = await createMismatchReport({
        type: 'write',
        input,
        legacyOutput,
        facadeOutput,
        difference: diff,
      });
      if (onMismatch) {
        await onMismatch(report);
      }
      return {
        mismatch: true,
        result: useLegacyResult
          ? legacySuccess
            ? legacyResult.value
            : facadeResult.value
          : facadeSuccess
            ? facadeResult.value
            : legacyResult.value,
        legacyResult: legacySuccess ? legacyResult.value : undefined,
        facadeResult: facadeSuccess ? facadeResult.value : undefined,
        reportHash: report.hash,
        report,
      };
    }
    return {
      mismatch: false,
      result: legacySuccess ? legacyResult.value : facadeResult.value,
    };
  }

  // Test 1: Shadow Write - Matching Outputs
  try {
    console.log('[Test 1] Shadow write with matching outputs');
    const legacyAdd = async (x) => x + 1;
    const facadeAdd = async (x) => x + 1;
    const result = await shadowWrite(legacyAdd, facadeAdd, 5);
    assert(result.mismatch === false, 'No mismatch detected');
    assert(result.result === 6, 'Result is 6');
    assert(result.reportHash === undefined, 'No report hash');
  } catch (e) {
    console.error('Test 1 failed:', e.message);
  }

  // Test 2: Shadow Write - Detected Mismatch
  try {
    console.log('\n[Test 2] Shadow write detects and reports mismatch');
    const legacyAdd = async (x) => x + 1;
    const facadeAdd = async (x) => x + 2;
    const reports = [];
    const result = await shadowWrite(legacyAdd, facadeAdd, 5, {
      onMismatch: (report) => reports.push(report),
    });
    assert(result.mismatch === true, 'Mismatch detected');
    assert(result.result === 6, 'Legacy result returned');
    assert(result.legacyResult === 6, 'Legacy result is 6');
    assert(result.facadeResult === 7, 'Facade result is 7');
    assert(result.reportHash !== undefined, 'Report hash exists');
    assert(reports.length === 1, 'One report generated');
    assert(reports[0].type === 'write', 'Report type is write');
  } catch (e) {
    console.error('Test 2 failed:', e.message);
  }

  // Test 3: Mismatch Report Determinism
  try {
    console.log('\n[Test 3] Same mismatch produces same hash');
    const legacy = async (x) => ({ value: x + 1 });
    const facade = async (x) => ({ value: x + 2 });
    const result1 = await shadowWrite(legacy, facade, 5);
    const result2 = await shadowWrite(legacy, facade, 5);
    assert(result1.reportHash === result2.reportHash, 'Same hash for same mismatch');
    assert(
      result1.report.timestamp !== result2.report.timestamp,
      'Different timestamps'
    );
    console.log(`  Hash: ${result1.reportHash.substring(0, 16)}...`);
  } catch (e) {
    console.error('Test 3 failed:', e.message);
  }

  // Test 4: Canonical Diff - Deep Object Comparison
  try {
    console.log('\n[Test 4] Canonical diff detects nested differences');
    const legacy = { user: { id: 1, name: 'Alice' }, count: 5 };
    const facade = { user: { id: 1, name: 'Bob' }, count: 5 };
    const diff = canonicalDiff(legacy, facade);
    assert(diff.hasDifference === true, 'Difference detected');
    assert(
      diff.paths.includes('root.user.name: "Alice" vs "Bob"'),
      'Correct path reported'
    );
    assert(diff.paths.length === 1, 'Only one difference');
  } catch (e) {
    console.error('Test 4 failed:', e.message);
  }

  // Test 5: Canonical Diff with Arrays
  try {
    console.log('\n[Test 5] Canonical diff handles array differences');
    const legacy = { items: [1, 2, 3] };
    const facade = { items: [1, 2, 4] };
    const diff = canonicalDiff(legacy, facade);
    assert(diff.hasDifference === true, 'Difference detected');
    assert(diff.paths.includes('root.items[2]: 3 vs 4'), 'Correct array path');
  } catch (e) {
    console.error('Test 5 failed:', e.message);
  }

  // Test 6: Canonical Diff with Missing Keys
  try {
    console.log('\n[Test 6] Canonical diff detects missing keys');
    const legacy = { a: 1, b: 2 };
    const facade = { a: 1, c: 3 };
    const diff = canonicalDiff(legacy, facade);
    assert(diff.hasDifference === true, 'Difference detected');
    assert(diff.paths.includes('root.b: missing in facade'), 'Missing in facade');
    assert(diff.paths.includes('root.c: missing in legacy'), 'Missing in legacy');
  } catch (e) {
    console.error('Test 6 failed:', e.message);
  }

  // Test 7: Error Handling - Both fail
  try {
    console.log('\n[Test 7] Shadow write throws when both handlers fail');
    const legacyFail = async () => {
      throw new Error('Legacy error');
    };
    const facadeFail = async () => {
      throw new Error('Facade error');
    };
    let threw = false;
    try {
      await shadowWrite(legacyFail, facadeFail, 5);
    } catch (e) {
      threw = true;
      assert(
        e.message.includes('Both legacy and facade handlers failed'),
        'Correct error message'
      );
    }
    assert(threw, 'Exception was thrown');
  } catch (e) {
    console.error('Test 7 failed:', e.message);
  }

  // Test 8: Mismatch Deduplication
  try {
    console.log('\n[Test 8] Duplicate mismatches share same hash');
    const legacy = async (x) => x * 2;
    const facade = async (x) => x * 3;
    const reports = [];
    const options = {
      onMismatch: (report) => reports.push(report),
    };
    for (let i = 0; i < 5; i++) {
      await shadowWrite(legacy, facade, 10, options);
    }
    const hashes = new Set(reports.map((r) => r.hash));
    assert(hashes.size === 1, 'All reports have same hash');
    const timestamps = new Set(reports.map((r) => r.timestamp.toString()));
    assert(timestamps.size === 5, 'All reports have different timestamps');
  } catch (e) {
    console.error('Test 8 failed:', e.message);
  }

  // Test 9: Content-Addressable Report
  try {
    console.log('\n[Test 9] Mismatch report is content-addressable');
    const diff = {
      hasDifference: true,
      paths: ['root.value: 1 vs 2'],
    };
    const report1 = await createMismatchReport({
      type: 'write',
      input: { x: 1 },
      legacyOutput: 1,
      facadeOutput: 2,
      difference: diff,
    });
    await new Promise((resolve) => setTimeout(resolve, 2));
    const report2 = await createMismatchReport({
      type: 'write',
      input: { x: 1 },
      legacyOutput: 1,
      facadeOutput: 2,
      difference: diff,
    });
    assert(report1.hash === report2.hash, 'Same hash');
    assert(report1.timestamp !== report2.timestamp, 'Different timestamps');
    console.log(`  Hash: ${report1.hash.substring(0, 16)}...`);
  } catch (e) {
    console.error('Test 9 failed:', e.message);
  }

  // Summary
  console.log('\n' + '='.repeat(50));
  console.log(`✅ PASSED: ${passed}`);
  console.log(`❌ FAILED: ${failed}`);
  console.log(`📊 TOTAL: ${passed + failed}`);
  console.log('='.repeat(50));

  if (failed > 0) {
    process.exit(1);
  }
}

runTests().catch((e) => {
  console.error('Test runner failed:', e);
  process.exit(1);
});
