/**
 * Poka-Yoke Proof: Sealed State Machine
 *
 * Demonstrates that private fields prevent direct state modification,
 * making invalid operations impossible by design.
 *
 * Run: node proofs/poka-yoke-sealed-state.test.mjs
 */

// Simulated AtomVMRuntime with sealed state (Improvement #1)
class SealedAtomVMRuntime {
  #state = 'Uninitialized'; // Private field - cannot be modified externally
  #atomvmModule = null;

  get state() {
    return this.#state; // Read-only access
  }

  #transitionTo(newState, validFromStates) {
    if (!validFromStates.includes(this.#state)) {
      throw new Error(
        `Invalid state transition: ${this.#state} → ${newState}. ` +
        `Valid from: [${validFromStates.join(', ')}]`
      );
    }
    console.log(`  ✓ State transition: ${this.#state} → ${newState}`);
    this.#state = newState;
  }

  async loadWASM() {
    console.log('  Attempting loadWASM()...');
    this.#transitionTo('Loading', ['Uninitialized']);
    
    // Simulate async load
    await new Promise(resolve => setTimeout(resolve, 10));
    
    this.#transitionTo('Ready', ['Loading']);
    this.#atomvmModule = { mock: true };
    console.log('  ✓ WASM loaded successfully');
  }

  async executeBeam(avmPath) {
    console.log(`  Attempting executeBeam(${avmPath})...`);
    this.#transitionTo('Executing', ['Ready']);
    
    if (!this.#atomvmModule) {
      throw new Error('No WASM module loaded');
    }

    // Simulate execution
    await new Promise(resolve => setTimeout(resolve, 10));
    
    this.#transitionTo('Ready', ['Executing']);
    console.log('  ✓ Execution completed');
  }

  destroy() {
    console.log('  Destroying runtime...');
    this.#transitionTo('Destroyed', ['Uninitialized', 'Ready', 'Error']);
    this.#atomvmModule = null;
    console.log('  ✓ Runtime destroyed');
  }
}

// Test suite
async function runTests() {
  console.log('\n=== Poka-Yoke Proof: Sealed State Machine ===\n');

  // Test 1: State is read-only
  console.log('Test 1: State property is read-only');
  const runtime1 = new SealedAtomVMRuntime();
  console.log(`  Initial state: ${runtime1.state}`);
  
  try {
    // Attempt to modify state directly (should fail silently or throw)
    runtime1.state = 'Ready';
    console.log(`  ✓ State after attempted modification: ${runtime1.state}`);
    
    if (runtime1.state === 'Ready') {
      console.log('  ❌ FAIL: State was modified externally!');
      process.exit(1);
    } else {
      console.log('  ✓ PASS: State remains unchanged (assignment ignored)');
    }
  } catch (error) {
    console.log(`  ✓ PASS: Cannot modify state (${error.message})`);
  }

  // Test 2: Invalid transitions throw descriptive errors
  console.log('\nTest 2: Invalid state transitions are prevented');
  const runtime2 = new SealedAtomVMRuntime();
  
  try {
    await runtime2.executeBeam('/test.avm');
    console.log('  ❌ FAIL: Execute before load should have thrown!');
    process.exit(1);
  } catch (error) {
    console.log(`  ✓ PASS: Invalid transition prevented: ${error.message}`);
  }

  // Test 3: Valid transitions succeed
  console.log('\nTest 3: Valid state transitions succeed');
  const runtime3 = new SealedAtomVMRuntime();
  
  try {
    await runtime3.loadWASM();
    console.log(`  Current state: ${runtime3.state}`);
    
    await runtime3.executeBeam('/test.avm');
    console.log(`  Current state: ${runtime3.state}`);
    
    console.log('  ✓ PASS: All valid transitions succeeded');
  } catch (error) {
    console.log(`  ❌ FAIL: Valid transitions should succeed: ${error.message}`);
    process.exit(1);
  }

  // Test 4: Destroyed state is terminal
  console.log('\nTest 4: Destroyed state is terminal');
  const runtime4 = new SealedAtomVMRuntime();
  
  try {
    await runtime4.loadWASM();
    runtime4.destroy();
    console.log(`  State after destroy: ${runtime4.state}`);
    
    await runtime4.loadWASM();
    console.log('  ❌ FAIL: Operations after destroy should throw!');
    process.exit(1);
  } catch (error) {
    console.log(`  ✓ PASS: Operations after destroy prevented: ${error.message}`);
  }

  // Test 5: Private module cannot be accessed
  console.log('\nTest 5: Private atomvmModule cannot be accessed directly');
  const runtime5 = new SealedAtomVMRuntime();
  
  try {
    // Attempt to access private field
    const module = runtime5.atomvmModule;
    
    if (module === undefined) {
      console.log('  ✓ PASS: Private field not accessible (undefined)');
    } else {
      console.log('  ❌ FAIL: Private field was accessible!');
      process.exit(1);
    }
  } catch (error) {
    console.log(`  ✓ PASS: Private field not accessible (${error.message})`);
  }

  console.log('\n=== All Tests Passed ✓ ===\n');
  console.log('Vulnerability Prevented: State Leak (Vulnerability #2)');
  console.log('Invalid Operations Made Impossible:');
  console.log('  - Direct state modification');
  console.log('  - Bypassing state machine guards');
  console.log('  - Operations in invalid states');
  console.log('  - Access to private implementation details\n');
}

runTests().catch(error => {
  console.error('Test suite failed:', error);
  process.exit(1);
});
