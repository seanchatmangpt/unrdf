/**
 * Manual test runner - Verify shadow modes without vitest
 */

import {
  shadowWrite,
  shadowRead,
  partialServe,
  canonicalDiff,
  createMismatchReport,
} from '../src/index.mjs';

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

async function runTests() {
  console.log('🧪 Shadow Modes - Manual Test Suite\n');

  // Test 1: Shadow Write - Matching Outputs
  try {
    console.log('\n[Test 1] Shadow write with matching outputs');
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

  // Test 5: Partial Serve - Random Routing
  try {
    console.log('\n[Test 5] Partial serve routes based on percentage');
    const legacy = async () => 'legacy';
    const facade = async () => 'facade';
    const router = { facadePercent: 50, strategy: 'random' };
    const results = await Promise.all(
      Array(100)
        .fill(0)
        .map(() => partialServe(router, legacy, facade, {}))
    );
    const facadeCount = results.filter((r) => r.routing === 'facade').length;
    assert(facadeCount > 30 && facadeCount < 70, `~50% facade routing (got ${facadeCount}%)`);
  } catch (e) {
    console.error('Test 5 failed:', e.message);
  }

  // Test 6: Partial Serve - Shadow Mode
  try {
    console.log('\n[Test 6] Partial serve shadow mode runs both handlers');
    const legacy = async (x) => x + 1;
    const facade = async (x) => x + 2;
    const router = { strategy: 'shadow' };
    const result = await partialServe(router, legacy, facade, 5);
    assert(result.routing === 'shadow', 'Shadow routing');
    assert(result.response === 6, 'Legacy response');
    assert(result.mismatches.length === 1, 'One mismatch detected');
    assert(result.mismatches[0].hash !== undefined, 'Mismatch hash exists');
  } catch (e) {
    console.error('Test 6 failed:', e.message);
  }

  // Test 7: Mismatch Deduplication
  try {
    console.log('\n[Test 7] Duplicate mismatches share same hash');
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
    console.error('Test 7 failed:', e.message);
  }

  // Test 8: Shadow Read
  try {
    console.log('\n[Test 8] Shadow read detects query mismatches');
    const legacyQuery = async (q) => [{ id: 1, name: 'Alice' }];
    const facadeQuery = async (q) => [{ id: 1, name: 'Bob' }];
    const result = await shadowRead(legacyQuery, facadeQuery, 'SELECT * FROM users');
    assert(result.mismatch === true, 'Mismatch detected');
    assert(result.reportHash !== undefined, 'Report hash exists');
    assert(result.report.type === 'read', 'Report type is read');
  } catch (e) {
    console.error('Test 8 failed:', e.message);
  }

  // Test 9: Error Handling - Both fail
  try {
    console.log('\n[Test 9] Shadow write throws when both handlers fail');
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
    console.error('Test 9 failed:', e.message);
  }

  // Test 10: Content-Addressable Report
  try {
    console.log('\n[Test 10] Mismatch report is content-addressable');
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
  } catch (e) {
    console.error('Test 10 failed:', e.message);
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
