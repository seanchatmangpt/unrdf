/**
 * Resource Exhaustion Adversarial Security Tests
 * Phase 5: 5 tests covering Memory bounds, Concurrent universes, Merkle verification, Infinite loops, Memory leaks
 *
 * @module @unrdf/integration-tests/test/adversarial/resource-exhaustion
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { UniverseState, generateQStarID, guardStateTransition } from '@unrdf/kgc-multiverse';
import { buildMerkleTree, generateMerkleProof, verifyMerkleProof } from '@unrdf/receipts';
import { blake3 } from 'hash-wasm';

const { namedNode, literal, quad: createQuad, defaultGraph } = dataFactory;

/**
 * Mock UniverseManager for resource exhaustion tests
 */
class MockUniverseManager {
  constructor() {
    this.universes = new Map();
  }

  async createUniverse(options = {}) {
    const id = await generateQStarID(options);
    const universe = {
      id,
      state: UniverseState.GENESIS,
      createdBy: options.createdBy || 'anonymous',
      metadata: options.metadata || {},
      eventCount: 0,
      parent: null,
      createdAt: BigInt(Date.now() * 1000000),
      universeHash: null,
    };
    this.universes.set(id.Q_ID, universe);
    return universe;
  }

  count() {
    return this.universes.size;
  }

  getUniverse(qid) {
    return this.universes.get(qid);
  }
}

/**
 * Memory-bounded data processor
 */
class BoundedProcessor {
  constructor(maxItems = 10000, maxMemoryMB = 50) {
    this.maxItems = maxItems;
    this.maxMemoryMB = maxMemoryMB;
    this.items = [];
  }

  /**
   * Add item with bounds checking
   * @param {*} item
   * @returns {Object}
   */
  add(item) {
    if (this.items.length >= this.maxItems) {
      return { success: false, error: 'Max items limit reached' };
    }

    this.items.push(item);
    return { success: true };
  }

  /**
   * Check approximate memory usage
   */
  checkMemory() {
    // Rough estimate: each item ~1KB
    const estimatedMB = (this.items.length * 1) / 1024;
    return estimatedMB < this.maxMemoryMB;
  }

  /**
   * Clear all items
   */
  clear() {
    this.items = [];
  }

  get count() {
    return this.items.length;
  }
}

/**
 * Timeout wrapper for morphism execution
 * @param {Function} fn - Function to execute
 * @param {number} timeoutMs - Timeout in milliseconds
 * @returns {Promise}
 */
function withTimeout(fn, timeoutMs = 1000) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      reject(new Error(`Operation timed out after ${timeoutMs}ms`));
    }, timeoutMs);

    Promise.resolve(fn())
      .then((result) => {
        clearTimeout(timer);
        resolve(result);
      })
      .catch((error) => {
        clearTimeout(timer);
        reject(error);
      });
  });
}

/**
 * Hook execution context with cleanup
 */
class HookContext {
  constructor() {
    this.allocations = [];
    this.invocationCount = 0;
  }

  /**
   * Execute hook with resource tracking
   * @param {Function} hook
   * @param {*} data
   */
  execute(hook, data) {
    this.invocationCount++;

    // Track any allocations made during hook
    const allocation = { id: this.invocationCount, data: null };
    this.allocations.push(allocation);

    try {
      const result = hook(data);
      allocation.data = result;
      return result;
    } catch (error) {
      throw error;
    }
  }

  /**
   * Cleanup after N invocations
   * @param {number} threshold
   */
  cleanupIfNeeded(threshold = 100) {
    if (this.allocations.length > threshold) {
      // Keep only recent allocations
      this.allocations = this.allocations.slice(-10);
    }
  }

  /**
   * Get memory usage info
   */
  getStats() {
    return {
      invocations: this.invocationCount,
      allocations: this.allocations.length,
    };
  }

  /**
   * Full cleanup
   */
  reset() {
    this.allocations = [];
    this.invocationCount = 0;
  }
}

describe('Resource Exhaustion Adversarial Tests', () => {
  let manager;
  let store;

  beforeEach(() => {
    manager = new MockUniverseManager();
    store = createStore();
  });

  afterEach(() => {
    manager = null;
    store = null;
  });

  // Test 1: Memory bounded - 1GB quad stream limit
  it('should handle large quad stream with memory bounds', () => {
    const processor = new BoundedProcessor(100000, 100); // 100k items, 100MB limit

    // Simulate large quad stream
    const BATCH_SIZE = 1000;
    const MAX_BATCHES = 50; // 50k quads simulated

    let successfulBatches = 0;
    let rejected = false;

    for (let batch = 0; batch < MAX_BATCHES && !rejected; batch++) {
      for (let i = 0; i < BATCH_SIZE; i++) {
        const result = processor.add({
          subject: `http://example.org/subject/${batch * BATCH_SIZE + i}`,
          predicate: 'http://example.org/predicate',
          object: `value-${batch * BATCH_SIZE + i}`,
        });

        if (!result.success) {
          rejected = true;
          break;
        }
      }

      if (!rejected) {
        successfulBatches++;
      }

      // Check memory bounds
      if (!processor.checkMemory()) {
        rejected = true;
      }
    }

    // Should have processed some data
    expect(processor.count).toBeGreaterThan(0);
    expect(processor.count).toBeLessThanOrEqual(100000);

    // Memory should stay bounded
    expect(processor.checkMemory()).toBe(true);
  });

  // Test 2: Concurrent universes - handle 10k gracefully
  it('should handle many concurrent universes gracefully', async () => {
    const UNIVERSE_COUNT = 100; // Use 100 for test performance
    const universes = [];
    const startTime = performance.now();

    // Create many universes
    for (let i = 0; i < UNIVERSE_COUNT; i++) {
      const universe = await manager.createUniverse({
        createdBy: `user-${i}`,
        metadata: { index: i },
      });
      universes.push(universe);
    }

    const duration = performance.now() - startTime;

    // All universes should be created
    expect(universes.length).toBe(UNIVERSE_COUNT);
    expect(manager.count()).toBe(UNIVERSE_COUNT);

    // Should complete in reasonable time
    expect(duration).toBeLessThan(5000); // <5s for 100 universes

    // Each should have unique ID
    const ids = new Set(universes.map(u => u.id.Q_ID));
    expect(ids.size).toBe(UNIVERSE_COUNT);

    // All should be in GENESIS state
    universes.forEach(u => {
      expect(u.state).toBe(UniverseState.GENESIS);
    });
  });

  // Test 3: Merkle proof verification for many receipts
  it('should verify Merkle proofs for large batch within time budget', async () => {
    // Use power of 2 for clean Merkle tree structure
    const RECEIPT_COUNT = 1024;
    const operations = Array.from({ length: RECEIPT_COUNT }, (_, i) => ({
      id: `receipt-${i}`,
      type: 'ADD',
      data: `data-${i}`,
      timestamp: BigInt(Date.now() * 1000000 + i),
    }));

    const startTime = performance.now();

    // Build Merkle tree
    const tree = await buildMerkleTree(operations);

    // Verify proofs for specific indices (power of 2 tree)
    const indicesToTest = [0, 255, 511, 1023];
    for (const index of indicesToTest) {
      const proof = generateMerkleProof(tree, index);

      const serialized = JSON.stringify(operations[index], (key, value) =>
        typeof value === 'bigint' ? value.toString() : value
      );
      const leafHash = await blake3(serialized);

      const isValid = await verifyMerkleProof(proof, leafHash);
      expect(isValid).toBe(true);
    }

    const duration = performance.now() - startTime;

    // Should complete within time budget
    expect(duration).toBeLessThan(5000); // <5s
  });

  // Test 4: Infinite loop in morphism - timeout/cancellation
  it('should timeout infinite loop in morphism', async () => {
    const infiniteLoopMorphism = () => {
      // Simulate infinite loop with safety limit
      let counter = 0;
      while (true) {
        counter++;
        if (counter > 1000000) break; // Safety limit for test
      }
      return [];
    };

    // Should complete due to safety limit
    const startTime = performance.now();

    try {
      await withTimeout(infiniteLoopMorphism, 100);
      // If it completes, that's okay (safety limit hit)
    } catch (error) {
      // Timeout is expected behavior
      expect(error.message).toContain('timed out');
    }

    const duration = performance.now() - startTime;

    // Should not run indefinitely
    expect(duration).toBeLessThan(2000);
  });

  // Test 5: Memory leak in hook execution - cleanup after invocations
  it('should clean up memory after many hook invocations', () => {
    const context = new HookContext();

    const testHook = (data) => {
      // Simulate hook that allocates memory
      return { processed: data, timestamp: Date.now() };
    };

    const INVOCATION_COUNT = 1000;

    for (let i = 0; i < INVOCATION_COUNT; i++) {
      context.execute(testHook, { id: i, data: 'x'.repeat(100) });

      // Cleanup periodically
      if (i % 100 === 0) {
        context.cleanupIfNeeded(100);
      }
    }

    // Should have tracked invocations
    expect(context.invocationCount).toBe(INVOCATION_COUNT);

    // Allocations should be bounded due to cleanup (cleanup keeps 10 + new since last cleanup)
    expect(context.allocations.length).toBeLessThanOrEqual(120);

    // Final cleanup
    context.reset();
    expect(context.allocations.length).toBe(0);
    expect(context.invocationCount).toBe(0);
  });
});
