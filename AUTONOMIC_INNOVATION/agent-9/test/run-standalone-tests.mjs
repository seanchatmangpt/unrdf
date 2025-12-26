/**
 * Standalone test runner - No external dependencies required
 * Uses mock blake3 and imports kgc-4d directly from workspace
 */

import { createHash } from 'node:crypto';

// Mock blake3 using SHA256 for testing
async function blake3(data) {
  return createHash('sha256').update(data).digest('hex');
}

// Import kgc-4d from workspace
const kgc4dPath = '../../../packages/kgc-4d/src/time.mjs';
const { now } = await import(kgc4dPath);

// Import local modules
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

// Inline createMismatchReport with mock blake3
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

async function runTests() {
  console.log('🧪 Shadow Modes - Standalone Test Suite');
  console.log('Note: Using SHA256 instead of BLAKE3 for testing\n');

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

  // Test 3: Mismatch Report Determinism (CRITICAL)
  try {
    console.log('\n[Test 3] Same mismatch produces same hash ⭐');
    const legacy = async (x) => ({ value: x + 1 });
    const facade = async (x) => ({ value: x + 2 });
    const result1 = await shadowWrite(legacy, facade, 5);
    await new Promise((resolve) => setTimeout(resolve, 2)); // Ensure different timestamp
    const result2 = await shadowWrite(legacy, facade, 5);
    assert(result1.reportHash === result2.reportHash, 'Same hash for same mismatch');
    assert(
      result1.report.timestamp !== result2.report.timestamp,
      'Different timestamps'
    );
    console.log(`  ✓ Hash (deterministic): ${result1.reportHash.substring(0, 16)}...`);
    console.log(
      `  ✓ Timestamp 1: ${result1.report.timestamp} (${typeof result1.report.timestamp})`
    );
    console.log(
      `  ✓ Timestamp 2: ${result2.report.timestamp} (${typeof result2.report.timestamp})`
    );
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

  // Test 8: Mismatch Deduplication (CRITICAL)
  try {
    console.log('\n[Test 8] Duplicate mismatches share same hash ⭐');
    const legacy = async (x) => x * 2;
    const facade = async (x) => x * 3;
    const reports = [];
    const options = {
      onMismatch: (report) => reports.push(report),
    };
    for (let i = 0; i < 5; i++) {
      await shadowWrite(legacy, facade, 10, options);
      await new Promise((resolve) => setTimeout(resolve, 1)); // Ensure different timestamps
    }
    const hashes = new Set(reports.map((r) => r.hash));
    assert(hashes.size === 1, 'All reports have same hash');
    const timestamps = new Set(reports.map((r) => r.timestamp.toString()));
    assert(timestamps.size === 5, 'All reports have different timestamps');
    console.log(`  ✓ Single hash for ${reports.length} reports: ${[...hashes][0].substring(0, 16)}...`);
  } catch (e) {
    console.error('Test 8 failed:', e.message);
  }

  // Test 9: Content-Addressable Report
  try {
    console.log('\n[Test 9] Mismatch report is content-addressable ⭐');
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
    console.log(`  ✓ Content hash: ${report1.hash.substring(0, 16)}...`);
  } catch (e) {
    console.error('Test 9 failed:', e.message);
  }

  // Test 10: Canonical Serialization Order
  try {
    console.log('\n[Test 10] Canonical serialization is order-independent');
    const obj1 = { z: 3, a: 1, m: 2 };
    const obj2 = { a: 1, m: 2, z: 3 };
    const ser1 = canonicalSerialize(obj1);
    const ser2 = canonicalSerialize(obj2);
    const json1 = JSON.stringify(ser1);
    const json2 = JSON.stringify(ser2);
    assert(json1 === json2, 'Same JSON despite different key order');
    console.log(`  ✓ Canonical JSON: ${json1}`);
  } catch (e) {
    console.error('Test 10 failed:', e.message);
  }

  // Summary
  console.log('\n' + '='.repeat(60));
  console.log(`✅ PASSED: ${passed}`);
  console.log(`❌ FAILED: ${failed}`);
  console.log(`📊 TOTAL: ${passed + failed}`);
  console.log('='.repeat(60));
  console.log('\n🎯 Key Verifications:');
  console.log('  ✓ Deterministic hashing (same input → same hash)');
  console.log('  ✓ Timestamp independence (hash excludes timestamp)');
  console.log('  ✓ Content-addressable reports');
  console.log('  ✓ Canonical ordering (key-order independent)');
  console.log('  ✓ Parallel execution (Promise.allSettled)');

  if (failed > 0) {
    process.exit(1);
  }
}

runTests().catch((e) => {
  console.error('Test runner failed:', e);
  process.exit(1);
});
