/**
 * Poka-Yoke Proof Test: Sealed Universe State Machine
 *
 * Demonstrates state machine enforcement: mutable → frozen → sealed
 *
 * Expected behavior:
 * - Mutable universe allows modifications
 * - Frozen universe rejects modifications (read-only)
 * - Sealed universe is terminal (cannot be unfrozen)
 */

// Universe state machine (standalone, no external dependencies)
const UniverseState = {
  MUTABLE: 'mutable',
  FROZEN: 'frozen',
  SEALED: 'sealed'
};

/**
 * Sealed Universe Guard - Prevents invalid state transitions
 */
class SealedUniverseGuard {
  constructor() {
    this.state = UniverseState.MUTABLE;
    this.freezeCount = 0;
    this.transitions = [];
  }

  /**
   * Attempt to freeze universe
   * @throws {Error} If universe is already sealed
   */
  freeze() {
    this.transitions.push({ from: this.state, to: UniverseState.FROZEN, timestamp: Date.now() });

    if (this.state === UniverseState.SEALED) {
      throw new Error('❌ Cannot freeze sealed universe - sealed state is terminal');
    }

    this.state = UniverseState.FROZEN;
    this.freezeCount++;
    return { state: this.state, freezeCount: this.freezeCount };
  }

  /**
   * Attempt to seal universe (terminal state)
   * @throws {Error} If universe is not frozen
   */
  seal() {
    this.transitions.push({ from: this.state, to: UniverseState.SEALED, timestamp: Date.now() });

    if (this.state !== UniverseState.FROZEN) {
      throw new Error(`❌ Cannot seal universe in ${this.state} state - must freeze first`);
    }

    this.state = UniverseState.SEALED;
    return { state: this.state, freezeCount: this.freezeCount };
  }

  /**
   * Attempt to admit delta (modify universe)
   * @throws {Error} If universe is frozen or sealed
   */
  admitDelta(delta) {
    if (this.state === UniverseState.FROZEN) {
      throw new Error(`❌ Cannot admit delta to frozen universe - state: ${this.state}`);
    }

    if (this.state === UniverseState.SEALED) {
      throw new Error(`❌ Cannot admit delta to sealed universe - state: ${this.state} (terminal)`);
    }

    return { admitted: true, state: this.state };
  }

  /**
   * Attempt to unfreeze universe
   * @throws {Error} If universe is sealed
   */
  unfreeze() {
    this.transitions.push({ from: this.state, to: UniverseState.MUTABLE, timestamp: Date.now() });

    if (this.state === UniverseState.SEALED) {
      throw new Error('❌ Cannot unfreeze sealed universe - sealed state is terminal');
    }

    if (this.state === UniverseState.MUTABLE) {
      throw new Error('❌ Cannot unfreeze mutable universe - already mutable');
    }

    this.state = UniverseState.MUTABLE;
    return { state: this.state };
  }

  getTransitionLog() {
    return this.transitions;
  }
}

/**
 * Run poka-yoke proof test
 */
async function runProofTest() {
  console.log('🔬 Poka-Yoke Proof Test: Sealed Universe State Machine\n');

  const guard = new SealedUniverseGuard();
  const results = {
    passed: 0,
    failed: 0,
    tests: []
  };

  // Test 1: Mutable state allows admits
  try {
    console.log('Test 1: Mutable universe accepts delta');
    const result = guard.admitDelta({ type: 'add', subject: 'ex:entity1' });
    console.log(`✅ PASS: Delta admitted in ${result.state} state\n`);
    results.passed++;
    results.tests.push({ name: 'Mutable accepts delta', status: 'PASS' });
  } catch (error) {
    console.log(`❌ FAIL: ${error.message}\n`);
    results.failed++;
    results.tests.push({ name: 'Mutable accepts delta', status: 'FAIL', error: error.message });
  }

  // Test 2: Freeze transition works
  try {
    console.log('Test 2: Freeze mutable universe');
    const result = guard.freeze();
    console.log(`✅ PASS: Universe frozen (state: ${result.state}, count: ${result.freezeCount})\n`);
    results.passed++;
    results.tests.push({ name: 'Freeze mutable universe', status: 'PASS' });
  } catch (error) {
    console.log(`❌ FAIL: ${error.message}\n`);
    results.failed++;
    results.tests.push({ name: 'Freeze mutable universe', status: 'FAIL', error: error.message });
  }

  // Test 3: Frozen state rejects admits
  try {
    console.log('Test 3: Frozen universe rejects delta');
    guard.admitDelta({ type: 'add', subject: 'ex:entity2' });
    console.log(`❌ FAIL: Delta should have been rejected in frozen state\n`);
    results.failed++;
    results.tests.push({ name: 'Frozen rejects delta', status: 'FAIL', error: 'No error thrown' });
  } catch (error) {
    console.log(`✅ PASS: ${error.message}\n`);
    results.passed++;
    results.tests.push({ name: 'Frozen rejects delta', status: 'PASS' });
  }

  // Test 4: Seal transition (frozen → sealed)
  try {
    console.log('Test 4: Seal frozen universe');
    const result = guard.seal();
    console.log(`✅ PASS: Universe sealed (state: ${result.state}, terminal)\n`);
    results.passed++;
    results.tests.push({ name: 'Seal frozen universe', status: 'PASS' });
  } catch (error) {
    console.log(`❌ FAIL: ${error.message}\n`);
    results.failed++;
    results.tests.push({ name: 'Seal frozen universe', status: 'FAIL', error: error.message });
  }

  // Test 5: Sealed state rejects admits
  try {
    console.log('Test 5: Sealed universe rejects delta');
    guard.admitDelta({ type: 'add', subject: 'ex:entity3' });
    console.log(`❌ FAIL: Delta should have been rejected in sealed state\n`);
    results.failed++;
    results.tests.push({ name: 'Sealed rejects delta', status: 'FAIL', error: 'No error thrown' });
  } catch (error) {
    console.log(`✅ PASS: ${error.message}\n`);
    results.passed++;
    results.tests.push({ name: 'Sealed rejects delta', status: 'PASS' });
  }

  // Test 6: Sealed state prevents freeze
  try {
    console.log('Test 6: Sealed universe rejects freeze');
    guard.freeze();
    console.log(`❌ FAIL: Freeze should have been rejected in sealed state\n`);
    results.failed++;
    results.tests.push({ name: 'Sealed rejects freeze', status: 'FAIL', error: 'No error thrown' });
  } catch (error) {
    console.log(`✅ PASS: ${error.message}\n`);
    results.passed++;
    results.tests.push({ name: 'Sealed rejects freeze', status: 'PASS' });
  }

  // Test 7: Sealed state prevents unfreeze
  try {
    console.log('Test 7: Sealed universe rejects unfreeze (terminal state)');
    guard.unfreeze();
    console.log(`❌ FAIL: Unfreeze should have been rejected in sealed state\n`);
    results.failed++;
    results.tests.push({ name: 'Sealed rejects unfreeze', status: 'FAIL', error: 'No error thrown' });
  } catch (error) {
    console.log(`✅ PASS: ${error.message}\n`);
    results.passed++;
    results.tests.push({ name: 'Sealed rejects unfreeze', status: 'PASS' });
  }

  // Display state transition log
  console.log('📊 State Transition Log:');
  const transitions = guard.getTransitionLog();
  transitions.forEach((t, i) => {
    console.log(`  ${i + 1}. ${t.from} → ${t.to} (${new Date(t.timestamp).toISOString()})`);
  });
  console.log();

  // Summary
  console.log('═══════════════════════════════════════════════════════════');
  console.log(`Results: ${results.passed} passed, ${results.failed} failed`);
  console.log(`Success Rate: ${((results.passed / (results.passed + results.failed)) * 100).toFixed(1)}%`);
  console.log('═══════════════════════════════════════════════════════════\n');

  // State Machine Diagram
  console.log('State Machine Diagram:');
  console.log('');
  console.log('  ┌─────────┐   freeze()   ┌─────────┐   seal()   ┌─────────┐');
  console.log('  │ MUTABLE ├──────────────→│ FROZEN  ├───────────→│ SEALED  │');
  console.log('  │         │◄──────────────┤         │            │ (TERM.) │');
  console.log('  └────┬────┘   unfreeze()  └────┬────┘            └────┬────┘');
  console.log('       │                          │                      │');
  console.log('       │ admitDelta() ✅          │ admitDelta() ❌      │ admitDelta() ❌');
  console.log('       │                          │ freeze() ✅           │ freeze() ❌');
  console.log('       │                          │ seal() ✅             │ unfreeze() ❌');
  console.log('');

  return results;
}

// Run test
runProofTest().catch(console.error);
