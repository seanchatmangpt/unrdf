/**
 * State Machine Attack Adversarial Security Tests
 * Phase 5: 5 tests covering Invalid transitions, Double freeze, Backdoor mutation, Race conditions, Orphaned receipts
 *
 * @module @unrdf/integration-tests/test/adversarial/state-machine
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  UniverseState,
  guardStateTransition,
  guardMorphismApplication,
} from '@unrdf/kgc-multiverse';
import { blake3 } from 'hash-wasm';

/**
 * Protected Universe wrapper that enforces state machine guards
 */
class ProtectedUniverse {
  constructor(id) {
    this.id = id;
    this.state = UniverseState.GENESIS;
    this._data = new Map();
    this._frozen = false;
    this._eventLog = [];
    this._mutex = Promise.resolve();
  }

  /**
   * Transition to new state with guard check
   * @param {string} newState
   */
  transition(newState) {
    guardStateTransition(this.state, newState);
    this.state = newState;
    this._eventLog.push({ type: 'transition', to: newState, at: Date.now() });

    if (newState === UniverseState.FROZEN) {
      this._frozen = true;
    }
  }

  /**
   * Add data with mutation guard
   * @param {string} key
   * @param {*} value
   */
  add(key, value) {
    guardMorphismApplication(this.state);

    if (this._frozen) {
      throw new Error('Cannot mutate FROZEN universe');
    }

    this._data.set(key, value);
    this._eventLog.push({ type: 'add', key, at: Date.now() });
  }

  /**
   * Execute with mutex for race condition prevention
   * @param {Function} fn - Critical section
   */
  async withLock(fn) {
    const prevMutex = this._mutex;
    let release;
    this._mutex = new Promise(resolve => {
      release = resolve;
    });

    await prevMutex;
    try {
      return await fn();
    } finally {
      release();
    }
  }

  /**
   * Get data (read allowed in any state)
   * @param {string} key
   */
  get(key) {
    return this._data.get(key);
  }

  /**
   * Get size
   */
  get size() {
    return this._data.size;
  }
}

/**
 * Receipt chain manager with orphan detection
 */
class ReceiptChainManager {
  constructor() {
    this.receipts = new Map();
    this.rootHash = null;
  }

  /**
   * Add receipt to chain
   * @param {Object} receipt
   */
  addReceipt(receipt) {
    // Validate parent exists (except for first receipt)
    if (receipt.parentHash && !this.receipts.has(receipt.parentHash)) {
      throw new Error(`Orphaned receipt: parent ${receipt.parentHash} not found`);
    }

    this.receipts.set(receipt.hash, receipt);

    if (!receipt.parentHash) {
      this.rootHash = receipt.hash;
    }
  }

  /**
   * Validate chain integrity
   * @returns {Object} Validation result
   */
  validateChain() {
    const visited = new Set();
    const orphans = [];

    // Start from receipts with no parent (should be only root)
    for (const [hash, receipt] of this.receipts) {
      if (receipt.parentHash && !this.receipts.has(receipt.parentHash)) {
        orphans.push(hash);
      }
      visited.add(hash);
    }

    return {
      valid: orphans.length === 0,
      orphans,
      total: this.receipts.size,
    };
  }
}

describe('State Machine Attack Adversarial Tests', () => {
  // Test 1: Invalid state transitions - FROZEN to ACTIVE blocked
  it('should block invalid state transitions', async () => {
    // FROZEN -> ACTIVE should be blocked
    expect(() => guardStateTransition('FROZEN', 'ACTIVE')).toThrow(
      /Invalid transition FROZEN/
    );

    // GENESIS -> FROZEN should be blocked (must go through ACTIVE)
    expect(() => guardStateTransition('GENESIS', 'FROZEN')).toThrow(
      /Invalid transition GENESIS/
    );

    // DISCARDED -> anything should be blocked
    expect(() => guardStateTransition('DISCARDED', 'ACTIVE')).toThrow(
      /Invalid transition DISCARDED/
    );
    expect(() => guardStateTransition('DISCARDED', 'GENESIS')).toThrow(
      /Invalid transition DISCARDED/
    );

    // ACTIVE -> GENESIS should be blocked (can't go backwards)
    expect(() => guardStateTransition('ACTIVE', 'GENESIS')).toThrow(
      /Invalid transition ACTIVE/
    );

    // FORKED -> ACTIVE should be blocked (must merge first)
    expect(() => guardStateTransition('FORKED', 'ACTIVE')).toThrow(
      /Invalid transition FORKED/
    );

    // Valid transitions should work
    expect(() => guardStateTransition('GENESIS', 'ACTIVE')).not.toThrow();
    expect(() => guardStateTransition('ACTIVE', 'FROZEN')).not.toThrow();
    expect(() => guardStateTransition('ACTIVE', 'FORKED')).not.toThrow();
    expect(() => guardStateTransition('FROZEN', 'DISCARDED')).not.toThrow();
  });

  // Test 2: Double freeze - freeze FROZEN universe
  it('should handle double freeze attempt', async () => {
    const universe = new ProtectedUniverse('test-universe');
    universe.transition(UniverseState.ACTIVE);

    // First freeze should work
    universe.transition(UniverseState.FROZEN);
    expect(universe.state).toBe(UniverseState.FROZEN);

    // Attempt to freeze again - should fail
    expect(() => {
      guardStateTransition(UniverseState.FROZEN, UniverseState.FROZEN);
    }).toThrow(/Invalid transition FROZEN/);

    // State should remain FROZEN
    expect(universe.state).toBe(UniverseState.FROZEN);
  });

  // Test 3: Backdoor mutation - direct store.add on frozen
  it('should prevent backdoor mutation of FROZEN universe', () => {
    const universe = new ProtectedUniverse('test-universe');

    // Add data while GENESIS
    universe.transition(UniverseState.ACTIVE);
    universe.add('key1', 'value1');
    expect(universe.size).toBe(1);

    // Freeze the universe
    universe.transition(UniverseState.FROZEN);

    // Attempt backdoor mutation - guardMorphismApplication throws first
    expect(() => universe.add('key2', 'value2')).toThrow(
      /FROZEN.*immutable/
    );

    // Size should be unchanged
    expect(universe.size).toBe(1);
    expect(universe.get('key1')).toBe('value1');
    expect(universe.get('key2')).toBeUndefined();
  });

  // Test 4: Race condition - concurrent fork+merge serialized
  it('should serialize concurrent operations via mutex', async () => {
    const universe = new ProtectedUniverse('concurrent-test');
    universe.transition(UniverseState.ACTIVE);

    const operationOrder = [];

    // Simulate concurrent operations
    const op1 = universe.withLock(async () => {
      await new Promise(r => setTimeout(r, 10));
      universe.add('key1', 'value1');
      operationOrder.push('op1');
    });

    const op2 = universe.withLock(async () => {
      universe.add('key2', 'value2');
      operationOrder.push('op2');
    });

    const op3 = universe.withLock(async () => {
      universe.add('key3', 'value3');
      operationOrder.push('op3');
    });

    // Wait for all operations
    await Promise.all([op1, op2, op3]);

    // All operations should complete
    expect(universe.size).toBe(3);

    // Operations should be serialized (order preserved)
    expect(operationOrder).toEqual(['op1', 'op2', 'op3']);
  });

  // Test 5: Orphaned receipt - missing parent chain break detected
  it('should detect orphaned receipts with missing parent', async () => {
    const chainManager = new ReceiptChainManager();

    // Create valid chain
    const receipt1 = {
      id: 'receipt-1',
      hash: await blake3('receipt-1-data'),
      parentHash: null,
      data: { action: 'create' },
    };

    const receipt2 = {
      id: 'receipt-2',
      hash: await blake3('receipt-2-data'),
      parentHash: receipt1.hash,
      data: { action: 'update' },
    };

    const receipt3 = {
      id: 'receipt-3',
      hash: await blake3('receipt-3-data'),
      parentHash: receipt2.hash,
      data: { action: 'finalize' },
    };

    // Add valid receipts
    chainManager.addReceipt(receipt1);
    chainManager.addReceipt(receipt2);
    chainManager.addReceipt(receipt3);

    // Chain should be valid
    const validResult = chainManager.validateChain();
    expect(validResult.valid).toBe(true);
    expect(validResult.orphans.length).toBe(0);

    // Attempt to add orphaned receipt (parent doesn't exist)
    const orphanedReceipt = {
      id: 'orphan',
      hash: await blake3('orphan-data'),
      parentHash: 'nonexistent-parent-hash',
      data: { action: 'orphan' },
    };

    expect(() => chainManager.addReceipt(orphanedReceipt)).toThrow(
      /Orphaned receipt: parent nonexistent-parent-hash not found/
    );

    // Chain should still be valid after rejection
    const afterOrphanResult = chainManager.validateChain();
    expect(afterOrphanResult.valid).toBe(true);
    expect(afterOrphanResult.total).toBe(3);
  });
});
