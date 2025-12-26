/**
 * Test suite for Agent 7 - Routing + Shadow Modes
 * Comprehensive tests for all routing functionality
 */

import {
  ROUTING_MODES,
  setMode,
  getMode,
  listModes,
  resetModes,
  shadowWrite,
  shadowRead,
  partialServe,
  execute,
  recordMismatch,
  getMismatches,
  exportLedger,
  clearLedger,
  detectDrift,
  calculateDriftScore,
  isAcceptableDrift,
  markForRollback,
  executeRollback,
  getRollbackStatus,
  autoRollback,
  getSystemStatus,
} from './index.mjs';

/**
 * Test counter
 */
let testCount = 0;
let passCount = 0;
let failCount = 0;

/**
 * Assert helper
 */
function assert(condition, message) {
  testCount++;
  if (condition) {
    passCount++;
    console.log(`✅ Test ${testCount}: ${message}`);
  } else {
    failCount++;
    console.error(`❌ Test ${testCount}: ${message}`);
  }
}

/**
 * Test routing modes
 */
async function testRoutingModes() {
  console.log('\n=== Testing Routing Modes ===\n');

  // Reset modes
  resetModes();

  // Test setMode
  const result = setMode('GET_USER', ROUTING_MODES.SHADOW_READ);
  assert(result.success === true, 'setMode returns success');
  assert(result.operation === 'GET_USER', 'setMode sets correct operation');
  assert(result.mode === ROUTING_MODES.SHADOW_READ, 'setMode sets correct mode');

  // Test getMode
  const mode = getMode('GET_USER');
  assert(mode === ROUTING_MODES.SHADOW_READ, 'getMode returns correct mode');

  // Test default mode
  const defaultMode = getMode('UNKNOWN_OP');
  assert(defaultMode === ROUTING_MODES.LEGACY_ONLY, 'getMode returns default for unknown operation');

  // Test listModes
  setMode('CREATE_ORDER', ROUTING_MODES.SHADOW_WRITE);
  const modes = listModes();
  assert(modes.length === 2, 'listModes returns all operations');
  assert(modes[0].operation === 'CREATE_ORDER', 'listModes is sorted alphabetically');

  // Test invalid mode
  try {
    setMode('TEST_OP', 'INVALID_MODE');
    assert(false, 'setMode should reject invalid mode');
  } catch (error) {
    assert(true, 'setMode rejects invalid mode');
  }
}

/**
 * Test shadow execution
 */
async function testShadowExecution() {
  console.log('\n=== Testing Shadow Execution ===\n');

  resetModes();
  clearLedger();

  // Setup test functions
  const legacyFn = async (payload) => {
    return { id: payload.id, name: 'John Doe', source: 'legacy' };
  };

  const substrateFn = async (payload) => {
    return { id: payload.id, name: 'John Doe', source: 'substrate' };
  };

  const substrateMismatchFn = async (payload) => {
    return { id: payload.id, name: 'john doe', source: 'substrate' }; // Different case
  };

  // Test shadow write (matching results)
  setMode('WRITE_USER', ROUTING_MODES.SHADOW_WRITE);
  const writeResult = await shadowWrite('WRITE_USER', { id: '123' }, legacyFn, substrateFn);
  assert(writeResult.success === true, 'shadowWrite executes successfully');
  assert(writeResult.result.source === 'legacy', 'shadowWrite returns legacy result');
  assert(writeResult.shadow.executed === true, 'shadowWrite executes substrate');
  assert(writeResult.shadow.mismatch === true, 'shadowWrite detects source field mismatch');

  // Test shadow write (mismatching results)
  const mismatchResult = await shadowWrite(
    'WRITE_USER',
    { id: '456' },
    legacyFn,
    substrateMismatchFn
  );
  assert(mismatchResult.shadow.mismatch === true, 'shadowWrite detects value mismatch');

  // Test shadow read
  setMode('READ_USER', ROUTING_MODES.SHADOW_READ);
  const readResult = await shadowRead('READ_USER', { id: '789' }, legacyFn, substrateFn);
  assert(readResult.success === true, 'shadowRead executes successfully');
  assert(readResult.result.source === 'legacy', 'shadowRead returns legacy result');
  assert(readResult.comparison.matched === false, 'shadowRead compares results');

  // Test partial serve
  setMode('PARTIAL_USER', ROUTING_MODES.PARTIAL_SERVE);
  const partialResult = await partialServe(
    'PARTIAL_USER',
    { id: '999' },
    50,
    legacyFn,
    substrateFn
  );
  assert(partialResult.success === true, 'partialServe executes successfully');
  assert(
    partialResult.routing.routedTo === 'legacy' ||
      partialResult.routing.routedTo === 'substrate',
    'partialServe routes to a system'
  );

  // Test execute wrapper
  setMode('EXECUTE_USER', ROUTING_MODES.LEGACY_ONLY);
  const execResult = await execute('EXECUTE_USER', { id: '111' }, legacyFn, substrateFn);
  assert(execResult.success === true, 'execute wrapper works');
  assert(execResult.mode === ROUTING_MODES.LEGACY_ONLY, 'execute uses correct mode');
}

/**
 * Test mismatch ledger
 */
async function testMismatchLedger() {
  console.log('\n=== Testing Mismatch Ledger ===\n');

  clearLedger();

  // Record mismatch
  const mismatch = recordMismatch(
    'GET_USER',
    { id: '123', name: 'John' },
    { id: '123', name: 'john' },
    { type: 'VALUE_MISMATCH', fields: ['name'] }
  );

  assert(mismatch.id.startsWith('mismatch_'), 'recordMismatch generates ID');
  assert(mismatch.operation === 'GET_USER', 'recordMismatch stores operation');
  assert(mismatch.diff.type === 'VALUE_MISMATCH', 'recordMismatch stores diff');

  // Get mismatches
  const mismatches = getMismatches({ operation: 'GET_USER' });
  assert(mismatches.length === 1, 'getMismatches filters by operation');

  // Export ledger
  const exported = exportLedger();
  assert(exported.hash !== undefined, 'exportLedger generates hash');
  assert(exported.entries.length === 1, 'exportLedger includes entries');
  assert(exported.metadata.entryCount === 1, 'exportLedger includes metadata');

  // Clear ledger
  const cleared = clearLedger();
  assert(cleared.clearedCount === 1, 'clearLedger removes entries');

  const afterClear = getMismatches();
  assert(afterClear.length === 0, 'clearLedger empties ledger');
}

/**
 * Test drift detection
 */
async function testDriftDetection() {
  console.log('\n=== Testing Drift Detection ===\n');

  clearLedger();

  // Create test mismatches
  recordMismatch('DRIFT_OP', { a: 1 }, { a: 2 }, { type: 'VALUE_MISMATCH' });
  recordMismatch('DRIFT_OP', { b: 1 }, { b: 'str' }, { type: 'TYPE_MISMATCH' });
  recordMismatch('DRIFT_OP', { c: 1 }, { c: 2 }, { type: 'VALUE_MISMATCH' });

  // Calculate drift score
  const mismatches = getMismatches({ operation: 'DRIFT_OP' });
  const score = calculateDriftScore(mismatches);
  assert(score > 0, 'calculateDriftScore returns non-zero for mismatches');
  assert(score <= 100, 'calculateDriftScore returns score <= 100');

  // Detect drift
  const drift = detectDrift('DRIFT_OP', { minSamples: 1 });
  assert(drift.hasDrift === true, 'detectDrift identifies drift');
  assert(drift.score > 0, 'detectDrift calculates score');
  assert(drift.severity !== 'NONE', 'detectDrift assigns severity');

  // Test acceptable drift
  assert(isAcceptableDrift(10, 50) === true, 'isAcceptableDrift accepts low drift');
  assert(isAcceptableDrift(80, 50) === false, 'isAcceptableDrift rejects high drift');

  clearLedger();
}

/**
 * Test rollback routing
 */
async function testRollbackRouting() {
  console.log('\n=== Testing Rollback Routing ===\n');

  resetModes();

  // Mark for rollback
  const marker = markForRollback('ROLLBACK_OP', {
    reason: 'High drift detected',
    driftScore: 85,
  });
  assert(marker.operation === 'ROLLBACK_OP', 'markForRollback creates marker');
  assert(marker.driftScore === 85, 'markForRollback stores drift score');
  assert(marker.executed === false, 'markForRollback starts as not executed');

  // Get rollback status
  const status = getRollbackStatus();
  assert(status.markedCount === 1, 'getRollbackStatus shows marked count');

  // Set operation to shadow mode
  setMode('ROLLBACK_OP', ROUTING_MODES.SHADOW_READ);

  // Execute rollback
  const result = executeRollback('ROLLBACK_OP');
  assert(result.success === true, 'executeRollback succeeds');
  assert(result.previousMode === ROUTING_MODES.SHADOW_READ, 'executeRollback records previous mode');
  assert(result.newMode === ROUTING_MODES.LEGACY_ONLY, 'executeRollback sets LEGACY_ONLY');

  // Verify mode changed
  const newMode = getMode('ROLLBACK_OP');
  assert(newMode === ROUTING_MODES.LEGACY_ONLY, 'executeRollback actually changes mode');
}

/**
 * Test auto rollback
 */
async function testAutoRollback() {
  console.log('\n=== Testing Auto Rollback ===\n');

  resetModes();
  clearLedger();

  // Create high drift scenario
  setMode('AUTO_OP', ROUTING_MODES.SHADOW_READ);

  for (let i = 0; i < 15; i++) {
    recordMismatch('AUTO_OP', { val: i }, { val: i + 100 }, { type: 'TYPE_MISMATCH' });
  }

  // Auto rollback with low threshold
  const autoResult = autoRollback('AUTO_OP', {
    threshold: 20,
    autoExecute: true,
  });

  assert(autoResult.triggered === true, 'autoRollback triggers on high drift');
  assert(autoResult.executed === true, 'autoRollback executes when autoExecute=true');
  assert(autoResult.rollbackResult.success === true, 'autoRollback successfully rolls back');

  const mode = getMode('AUTO_OP');
  assert(mode === ROUTING_MODES.LEGACY_ONLY, 'autoRollback changes mode to LEGACY_ONLY');

  clearLedger();
}

/**
 * Test system status
 */
async function testSystemStatus() {
  console.log('\n=== Testing System Status ===\n');

  const status = await getSystemStatus();

  assert(status.modes !== undefined, 'getSystemStatus includes modes');
  assert(status.mismatches !== undefined, 'getSystemStatus includes mismatches');
  assert(status.drift !== undefined, 'getSystemStatus includes drift');
  assert(status.rollbacks !== undefined, 'getSystemStatus includes rollbacks');
  assert(status.timestamp > 0, 'getSystemStatus includes timestamp');
}

/**
 * Run all tests
 */
async function runTests() {
  console.log('🧪 Agent 7 - Routing + Shadow Modes Test Suite\n');
  console.log('='.repeat(60));

  try {
    await testRoutingModes();
    await testShadowExecution();
    await testMismatchLedger();
    await testDriftDetection();
    await testRollbackRouting();
    await testAutoRollback();
    await testSystemStatus();

    console.log('\n' + '='.repeat(60));
    console.log('\n📊 Test Results:');
    console.log(`   Total:  ${testCount}`);
    console.log(`   ✅ Pass: ${passCount}`);
    console.log(`   ❌ Fail: ${failCount}`);
    console.log(`   Success Rate: ${((passCount / testCount) * 100).toFixed(1)}%\n`);

    if (failCount === 0) {
      console.log('🎉 All tests passed!\n');
      process.exit(0);
    } else {
      console.error('❌ Some tests failed\n');
      process.exit(1);
    }
  } catch (error) {
    console.error('💥 Test suite crashed:', error);
    process.exit(1);
  }
}

// Run tests
runTests();
