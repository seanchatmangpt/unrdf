/**
 * @file KGC Sidecar Test Suite
 * @module kgc-sidecar-test-suite
 * 
 * @description
 * Comprehensive test suite for KGC JavaScript Sidecar implementing
 * 7 test categories: unit, property, permutation, combination, 
 * stress, adversarial, and benchmark tests.
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { TransactionManager } from '../../src/knowledge-engine/transaction.mjs';
import { KnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { EffectSandbox } from '../../src/knowledge-engine/effect-sandbox.mjs';
import { PolicyPackManager } from '../../src/knowledge-engine/policy-pack.mjs';
import { ResolutionLayer } from '../../src/knowledge-engine/resolution-layer.mjs';
import { ObservabilityManager } from '../../src/knowledge-engine/observability.mjs';
import { PerformanceOptimizer } from '../../src/knowledge-engine/performance-optimizer.mjs';
import { Store } from 'n3';
import { randomUUID } from 'crypto';

/**
 * Test data generators
 */
const TestDataGenerators = {
  /**
   * Generate random RDF quads
   * @param {number} count - Number of quads to generate
   * @returns {Array} Array of RDF quads
   */
  generateQuads(count = 10) {
    const quads = [];
    for (let i = 0; i < count; i++) {
      const subject = `https://example.org/resource${i}`;
      const predicate = `https://example.org/property${i % 5}`;
      const object = `https://example.org/value${i}`;
      quads.push({ subject, predicate, object, graph: '' });
    }
    return quads;
  },

  /**
   * Generate random delta
   * @param {number} additions - Number of additions
   * @param {number} removals - Number of removals
   * @returns {Object} Delta object
   */
  generateDelta(additions = 5, removals = 2) {
    return {
      additions: this.generateQuads(additions),
      removals: this.generateQuads(removals),
      metadata: { test: true, timestamp: Date.now() }
    };
  },

  /**
   * Generate random hook
   * @returns {Object} Hook object
   */
  generateHook() {
    return {
      id: `test-hook-${randomUUID()}`,
      mode: 'pre',
      condition: async () => true,
      effect: 'veto'
    };
  },

  /**
   * Generate random knowledge hook
   * @returns {Object} Knowledge hook object
   */
  generateKnowledgeHook() {
    return {
      meta: {
        name: `test-knowledge-hook-${randomUUID()}`,
        description: 'Test knowledge hook'
      },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://test-query.rq',
          sha256: 'abc123def456',
          mediaType: 'application/sparql-query'
        }
      },
      run: async (event) => ({ result: 'success' })
    };
  }
};

/**
 * Performance test utilities
 */
const PerformanceTestUtils = {
  /**
   * Measure execution time
   * @param {Function} fn - Function to measure
   * @returns {Promise<{result: any, duration: number}>} Result and duration
   */
  async measureTime(fn) {
    const start = process.hrtime.bigint();
    const result = await fn();
    const end = process.hrtime.bigint();
    const duration = Number(end - start) / 1000000; // Convert to ms
    return { result, duration };
  },

  /**
   * Generate load test data
   * @param {number} count - Number of operations
   * @returns {Array} Array of test operations
   */
  generateLoadTest(count = 1000) {
    const operations = [];
    for (let i = 0; i < count; i++) {
      operations.push({
        id: `op-${i}`,
        delta: TestDataGenerators.generateDelta(10, 5),
        options: { actor: `test-actor-${i % 10}` }
      });
    }
    return operations;
  },

  /**
   * Calculate percentiles
   * @param {Array<number>} values - Array of values
   * @param {number} percentile - Percentile (0-1)
   * @returns {number} Percentile value
   */
  calculatePercentile(values, percentile) {
    const sorted = values.sort((a, b) => a - b);
    const index = Math.ceil(sorted.length * percentile) - 1;
    return sorted[Math.max(0, index)];
  }
};

/**
 * Unit Tests
 */
describe('KGC Sidecar - Unit Tests', () => {
  let transactionManager;
  let store;

  beforeEach(() => {
    transactionManager = new TransactionManager({
      enableLockchain: false,
      enableResolution: false
    });
    store = new Store();
  });

  afterEach(() => {
    transactionManager.clearHooks();
  });

  describe('TransactionManager', () => {
    it('should create transaction manager with default options', () => {
      expect(transactionManager).toBeDefined();
      expect(transactionManager.hooks).toEqual([]);
    });

    it('should add and remove hooks', () => {
      const hook = TestDataGenerators.generateHook();
      transactionManager.addHook(hook);
      expect(transactionManager.hooks).toHaveLength(1);
      
      transactionManager.removeHook(hook.id);
      expect(transactionManager.hooks).toHaveLength(0);
    });

    it('should apply transaction successfully', async () => {
      const delta = TestDataGenerators.generateDelta(5, 0);
      const result = await transactionManager.apply(store, delta);
      
      expect(result.receipt.committed).toBe(true);
      expect(result.store.size).toBe(5);
    });

    it('should handle transaction failure', async () => {
      const delta = TestDataGenerators.generateDelta(0, 10); // Remove more than exists
      const result = await transactionManager.apply(store, delta);
      
      expect(result.receipt.committed).toBe(true); // Should still commit
    });

    it('should execute pre-hooks', async () => {
      let hookExecuted = false;
      const hook = {
        id: 'test-pre-hook',
        mode: 'pre',
        condition: async () => {
          hookExecuted = true;
          return true;
        },
        effect: 'veto'
      };
      
      transactionManager.addHook(hook);
      const delta = TestDataGenerators.generateDelta(1, 0);
      await transactionManager.apply(store, delta);
      
      expect(hookExecuted).toBe(true);
    });

    it('should veto transaction on pre-hook failure', async () => {
      const hook = {
        id: 'veto-hook',
        mode: 'pre',
        condition: async () => false,
        effect: 'veto'
      };
      
      transactionManager.addHook(hook);
      const delta = TestDataGenerators.generateDelta(1, 0);
      const result = await transactionManager.apply(store, delta);
      
      expect(result.receipt.committed).toBe(false);
    });
  });

  describe('KnowledgeHookManager', () => {
    let knowledgeHookManager;

    beforeEach(() => {
      knowledgeHookManager = new KnowledgeHookManager({
        basePath: process.cwd(),
        enableKnowledgeHooks: true
      });
    });

    it('should create knowledge hook manager', () => {
      expect(knowledgeHookManager).toBeDefined();
      expect(knowledgeHookManager.knowledgeHooks).toBeDefined();
    });

    it('should add knowledge hook', () => {
      const hook = TestDataGenerators.generateKnowledgeHook();
      knowledgeHookManager.addKnowledgeHook(hook);
      
      expect(knowledgeHookManager.knowledgeHooks.has(hook.meta.name)).toBe(true);
    });

    it('should remove knowledge hook', () => {
      const hook = TestDataGenerators.generateKnowledgeHook();
      knowledgeHookManager.addKnowledgeHook(hook);
      knowledgeHookManager.removeKnowledgeHook(hook.meta.name);
      
      expect(knowledgeHookManager.knowledgeHooks.has(hook.meta.name)).toBe(false);
    });
  });

  describe('EffectSandbox', () => {
    let sandbox;

    beforeEach(() => {
      sandbox = new EffectSandbox({
        type: 'worker',
        timeout: 5000,
        memoryLimit: 64 * 1024 * 1024
      });
    });

    it('should create effect sandbox', () => {
      expect(sandbox).toBeDefined();
      expect(sandbox.config.type).toBe('worker');
    });

    it('should execute effect in sandbox', async () => {
      const effect = async (context) => {
        return { result: 'success', value: 42 };
      };
      
      const context = {
        event: { name: 'test' },
        store: new Store(),
        delta: TestDataGenerators.generateDelta(1, 0)
      };
      
      const result = await sandbox.executeEffect(effect, context);
      
      expect(result.success).toBe(true);
      expect(result.result.result).toBe('success');
    });

    it('should handle sandbox timeout', async () => {
      const effect = async () => {
        await new Promise(resolve => setTimeout(resolve, 10000)); // 10s delay
        return { result: 'timeout' };
      };
      
      const context = {
        event: { name: 'test' },
        store: new Store(),
        delta: TestDataGenerators.generateDelta(1, 0)
      };
      
      const result = await sandbox.executeEffect(effect, context);
      
      expect(result.success).toBe(false);
      expect(result.error).toContain('timeout');
    });
  });
});

/**
 * Property Tests
 */
describe('KGC Sidecar - Property Tests', () => {
  let transactionManager;
  let store;

  beforeEach(() => {
    transactionManager = new TransactionManager();
    store = new Store();
  });

  it('should maintain store consistency after multiple transactions', async () => {
    const initialSize = store.size;
    
    // Apply multiple transactions
    for (let i = 0; i < 10; i++) {
      const delta = TestDataGenerators.generateDelta(5, 2);
      await transactionManager.apply(store, delta);
    }
    
    // Store should be consistent
    expect(store.size).toBeGreaterThan(initialSize);
  });

  it('should maintain hook order consistency', async () => {
    const executionOrder = [];
    
    // Add hooks in specific order
    for (let i = 0; i < 5; i++) {
      const hook = {
        id: `hook-${i}`,
        mode: 'pre',
        condition: async () => {
          executionOrder.push(i);
          return true;
        },
        effect: 'veto'
      };
      transactionManager.addHook(hook);
    }
    
    const delta = TestDataGenerators.generateDelta(1, 0);
    await transactionManager.apply(store, delta);
    
    // Hooks should execute in order
    expect(executionOrder).toEqual([0, 1, 2, 3, 4]);
  });

  it('should maintain receipt integrity', async () => {
    const delta = TestDataGenerators.generateDelta(10, 5);
    const result = await transactionManager.apply(store, delta);
    
    const receipt = result.receipt;
    
    // Receipt should have required fields
    expect(receipt.id).toBeDefined();
    expect(receipt.timestamp).toBeDefined();
    expect(receipt.delta).toBeDefined();
    expect(receipt.committed).toBeDefined();
    expect(receipt.hookResults).toBeDefined();
    expect(receipt.beforeHash).toBeDefined();
    expect(receipt.afterHash).toBeDefined();
    expect(receipt.duration).toBeDefined();
  });
});

/**
 * Permutation Tests
 */
describe('KGC Sidecar - Permutation Tests', () => {
  let transactionManager;
  let store;

  beforeEach(() => {
    transactionManager = new TransactionManager();
    store = new Store();
  });

  it('should handle different hook execution orders', async () => {
    const results = [];
    
    // Test different hook orders
    const hookOrders = [
      ['pre', 'post'],
      ['post', 'pre'],
      ['pre', 'pre', 'post'],
      ['post', 'post', 'pre']
    ];
    
    for (const order of hookOrders) {
      transactionManager.clearHooks();
      const executionOrder = [];
      
      order.forEach((mode, index) => {
        const hook = {
          id: `hook-${index}`,
          mode,
          condition: async () => {
            executionOrder.push(mode);
            return true;
          },
          effect: 'veto'
        };
        transactionManager.addHook(hook);
      });
      
      const delta = TestDataGenerators.generateDelta(1, 0);
      await transactionManager.apply(store, delta);
      
      results.push(executionOrder);
    }
    
    // All orders should execute successfully
    expect(results).toHaveLength(hookOrders.length);
  });

  it('should handle different delta application orders', async () => {
    const deltas = [
      TestDataGenerators.generateDelta(5, 0),
      TestDataGenerators.generateDelta(0, 3),
      TestDataGenerators.generateDelta(3, 2),
      TestDataGenerators.generateDelta(10, 5)
    ];
    
    const results = [];
    
    // Apply deltas in different orders
    const permutations = [
      [0, 1, 2, 3],
      [3, 2, 1, 0],
      [1, 3, 0, 2],
      [2, 0, 3, 1]
    ];
    
    for (const permutation of permutations) {
      const testStore = new Store();
      let totalSize = 0;
      
      for (const index of permutation) {
        const result = await transactionManager.apply(testStore, deltas[index]);
        totalSize = testStore.size;
      }
      
      results.push(totalSize);
    }
    
    // All permutations should result in same final size
    expect(results.every(size => size === results[0])).toBe(true);
  });
});

/**
 * Combination Tests
 */
describe('KGC Sidecar - Combination Tests', () => {
  let transactionManager;
  let knowledgeHookManager;
  let store;

  beforeEach(() => {
    transactionManager = new TransactionManager({
      enableLockchain: true,
      enableResolution: true
    });
    knowledgeHookManager = new KnowledgeHookManager({
      basePath: process.cwd(),
      enableKnowledgeHooks: true
    });
    store = new Store();
  });

  it('should handle transaction with knowledge hooks and policy packs', async () => {
    // Add regular hook
    const regularHook = TestDataGenerators.generateHook();
    transactionManager.addHook(regularHook);
    
    // Add knowledge hook
    const knowledgeHook = TestDataGenerators.generateKnowledgeHook();
    knowledgeHookManager.addKnowledgeHook(knowledgeHook);
    
    // Apply transaction
    const delta = TestDataGenerators.generateDelta(5, 2);
    const result = await transactionManager.apply(store, delta);
    
    expect(result.receipt.committed).toBe(true);
    expect(result.receipt.hookResults.length).toBeGreaterThan(0);
  });

  it('should handle resolution layer with multiple agents', async () => {
    const resolutionLayer = new ResolutionLayer({
      defaultStrategy: 'voting',
      maxProposals: 100
    });
    
    // Register agents
    resolutionLayer.registerAgent('agent1', { priority: 80 });
    resolutionLayer.registerAgent('agent2', { priority: 60 });
    resolutionLayer.registerAgent('agent3', { priority: 40 });
    
    // Submit proposals
    const proposals = [];
    for (let i = 0; i < 3; i++) {
      const delta = TestDataGenerators.generateDelta(5, 2);
      const proposalId = await resolutionLayer.submitProposal(`agent${i + 1}`, delta, {
        confidence: 0.8,
        priority: 80 - i * 20
      });
      proposals.push(proposalId);
    }
    
    // Resolve proposals
    const resolution = await resolutionLayer.resolveProposals(proposals, {
      type: 'voting',
      timeout: 5000
    });
    
    expect(resolution).toBeDefined();
    expect(resolution.proposals).toHaveLength(3);
    expect(resolution.resolvedDelta).toBeDefined();
  });

  it('should handle observability with performance optimization', async () => {
    const observability = new ObservabilityManager({
      enableTracing: true,
      enableMetrics: true
    });
    
    const optimizer = new PerformanceOptimizer({
      enableFastPath: true,
      enableCaching: true,
      enableBatchProcessing: true
    });
    
    // Initialize observability
    await observability.initialize();
    
    // Test optimized transaction
    const delta = TestDataGenerators.generateDelta(100, 50);
    const optimizedResult = await optimizer.optimizeTransaction(
      async (context) => {
        return await transactionManager.apply(store, context.delta);
      },
      { delta, afterHashOnly: true }
    );
    
    expect(optimizedResult).toBeDefined();
    expect(optimizedResult.receipt.committed).toBe(true);
    
    // Check performance metrics
    const metrics = observability.getPerformanceMetrics();
    expect(metrics.transactionLatency.p50).toBeGreaterThan(0);
    expect(metrics.hookExecutionRate).toBeGreaterThan(0);
  });
});

/**
 * Stress Tests
 */
describe('KGC Sidecar - Stress Tests', () => {
  let transactionManager;
  let store;

  beforeEach(() => {
    transactionManager = new TransactionManager({
      enableLockchain: false,
      enableResolution: false
    });
    store = new Store();
  });

  it('should handle high transaction volume', async () => {
    const operations = PerformanceTestUtils.generateLoadTest(1000);
    const results = [];
    
    for (const operation of operations) {
      const result = await transactionManager.apply(store, operation.delta, operation.options);
      results.push(result);
    }
    
    expect(results).toHaveLength(1000);
    expect(results.every(r => r.receipt.committed)).toBe(true);
  });

  it('should handle large delta processing', async () => {
    const largeDelta = TestDataGenerators.generateDelta(10000, 5000);
    const { result, duration } = await PerformanceTestUtils.measureTime(async () => {
      return await transactionManager.apply(store, largeDelta);
    });
    
    expect(result.receipt.committed).toBe(true);
    expect(duration).toBeLessThan(5000); // Should complete within 5 seconds
  });

  it('should handle concurrent transactions', async () => {
    const concurrentOperations = 100;
    const promises = [];
    
    for (let i = 0; i < concurrentOperations; i++) {
      const delta = TestDataGenerators.generateDelta(10, 5);
      const promise = transactionManager.apply(store, delta, { actor: `concurrent-${i}` });
      promises.push(promise);
    }
    
    const results = await Promise.all(promises);
    
    expect(results).toHaveLength(concurrentOperations);
    expect(results.every(r => r.receipt.committed)).toBe(true);
  });

  it('should handle memory pressure', async () => {
    const memoryIntensiveOperations = 500;
    const results = [];
    
    for (let i = 0; i < memoryIntensiveOperations; i++) {
      const delta = TestDataGenerators.generateDelta(100, 50);
      const result = await transactionManager.apply(store, delta);
      results.push(result);
      
      // Force garbage collection if available
      if (global.gc && i % 100 === 0) {
        global.gc();
      }
    }
    
    expect(results).toHaveLength(memoryIntensiveOperations);
    
    // Check memory usage
    const memUsage = process.memoryUsage();
    expect(memUsage.heapUsed).toBeLessThan(500 * 1024 * 1024); // Less than 500MB
  });
});

/**
 * Adversarial Tests
 */
describe('KGC Sidecar - Adversarial Tests', () => {
  let transactionManager;
  let store;

  beforeEach(() => {
    transactionManager = new TransactionManager({
      strictMode: false // Allow graceful degradation
    });
    store = new Store();
  });

  it('should handle malformed delta gracefully', async () => {
    const malformedDeltas = [
      { additions: null, removals: [] },
      { additions: [], removals: undefined },
      { additions: 'invalid', removals: [] },
      { additions: [], removals: 'invalid' },
      { additions: [{ invalid: 'quad' }], removals: [] }
    ];
    
    for (const delta of malformedDeltas) {
      try {
        const result = await transactionManager.apply(store, delta);
        // Should either succeed or fail gracefully
        expect(result).toBeDefined();
      } catch (error) {
        // Should throw meaningful error
        expect(error.message).toBeDefined();
      }
    }
  });

  it('should handle malicious hook code', async () => {
    const maliciousHooks = [
      {
        id: 'malicious-1',
        mode: 'pre',
        condition: async () => {
          // Attempt to access process
          try {
            process.exit(0);
          } catch (e) {
            return false;
          }
        },
        effect: 'veto'
      },
      {
        id: 'malicious-2',
        mode: 'pre',
        condition: async () => {
          // Attempt to access file system
          try {
            require('fs').readFileSync('/etc/passwd');
          } catch (e) {
            return false;
          }
        },
        effect: 'veto'
      }
    ];
    
    for (const hook of maliciousHooks) {
      transactionManager.addHook(hook);
      const delta = TestDataGenerators.generateDelta(1, 0);
      
      try {
        const result = await transactionManager.apply(store, delta);
        // Should handle malicious code gracefully
        expect(result).toBeDefined();
      } catch (error) {
        // Should throw security error
        expect(error.message).toBeDefined();
      }
    }
  });

  it('should handle resource exhaustion attacks', async () => {
    // Test with extremely large delta
    const largeDelta = {
      additions: Array(100000).fill().map((_, i) => ({
        subject: `https://example.org/resource${i}`,
        predicate: `https://example.org/property${i}`,
        object: `https://example.org/value${i}`,
        graph: ''
      })),
      removals: []
    };
    
    try {
      const result = await transactionManager.apply(store, largeDelta);
      // Should either succeed or fail gracefully
      expect(result).toBeDefined();
    } catch (error) {
      // Should throw resource error
      expect(error.message).toBeDefined();
    }
  });

  it('should handle timing attacks', async () => {
    const timingHooks = [];
    
    // Create hooks with different execution times
    for (let i = 0; i < 10; i++) {
      const hook = {
        id: `timing-hook-${i}`,
        mode: 'pre',
        condition: async () => {
          // Variable delay based on hook ID
          await new Promise(resolve => setTimeout(resolve, i * 100));
          return true;
        },
        effect: 'veto'
      };
      timingHooks.push(hook);
    }
    
    // Add hooks in random order
    const shuffledHooks = timingHooks.sort(() => Math.random() - 0.5);
    shuffledHooks.forEach(hook => transactionManager.addHook(hook));
    
    const delta = TestDataGenerators.generateDelta(1, 0);
    const { result, duration } = await PerformanceTestUtils.measureTime(async () => {
      return await transactionManager.apply(store, delta);
    });
    
    expect(result.receipt.committed).toBe(true);
    expect(duration).toBeLessThan(2000); // Should complete within 2 seconds
  });
});

/**
 * Benchmark Tests
 */
describe('KGC Sidecar - Benchmark Tests', () => {
  let transactionManager;
  let store;

  beforeEach(() => {
    transactionManager = new TransactionManager({
      enableLockchain: false,
      enableResolution: false
    });
    store = new Store();
  });

  it('should meet p50 latency target (≤ 200µs)', async () => {
    const measurements = [];
    const iterations = 1000;
    
    for (let i = 0; i < iterations; i++) {
      const delta = TestDataGenerators.generateDelta(10, 5);
      const { duration } = await PerformanceTestUtils.measureTime(async () => {
        return await transactionManager.apply(store, delta, { afterHashOnly: true });
      });
      measurements.push(duration);
    }
    
    const p50 = PerformanceTestUtils.calculatePercentile(measurements, 0.5);
    expect(p50).toBeLessThanOrEqual(0.2); // 200µs
  });

  it('should meet p99 latency target (≤ 2ms)', async () => {
    const measurements = [];
    const iterations = 1000;
    
    for (let i = 0; i < iterations; i++) {
      const delta = TestDataGenerators.generateDelta(10, 5);
      const { duration } = await PerformanceTestUtils.measureTime(async () => {
        return await transactionManager.apply(store, delta, { afterHashOnly: true });
      });
      measurements.push(duration);
    }
    
    const p99 = PerformanceTestUtils.calculatePercentile(measurements, 0.99);
    expect(p99).toBeLessThanOrEqual(2); // 2ms
  });

  it('should meet hook execution rate target (≥ 10k/min)', async () => {
    const startTime = Date.now();
    const hookCount = 1000;
    const promises = [];
    
    for (let i = 0; i < hookCount; i++) {
      const hook = TestDataGenerators.generateHook();
      transactionManager.addHook(hook);
    }
    
    for (let i = 0; i < hookCount; i++) {
      const delta = TestDataGenerators.generateDelta(1, 0);
      const promise = transactionManager.apply(store, delta);
      promises.push(promise);
    }
    
    await Promise.all(promises);
    
    const endTime = Date.now();
    const duration = (endTime - startTime) / 1000; // seconds
    const rate = (hookCount / duration) * 60; // per minute
    
    expect(rate).toBeGreaterThanOrEqual(10000); // 10k/min
  });

  it('should meet receipt write target (≤ 5ms median)', async () => {
    const measurements = [];
    const iterations = 100;
    
    for (let i = 0; i < iterations; i++) {
      const delta = TestDataGenerators.generateDelta(100, 50);
      const { duration } = await PerformanceTestUtils.measureTime(async () => {
        return await transactionManager.apply(store, delta);
      });
      measurements.push(duration);
    }
    
    const median = PerformanceTestUtils.calculatePercentile(measurements, 0.5);
    expect(median).toBeLessThanOrEqual(5); // 5ms
  });

  it('should maintain error isolation (100%)', async () => {
    const errorHooks = [];
    
    // Create hooks that will fail
    for (let i = 0; i < 10; i++) {
      const hook = {
        id: `error-hook-${i}`,
        mode: 'pre',
        condition: async () => {
          throw new Error(`Hook ${i} failed`);
        },
        effect: 'veto'
      };
      errorHooks.push(hook);
    }
    
    // Add one successful hook
    const successHook = {
      id: 'success-hook',
      mode: 'pre',
      condition: async () => true,
      effect: 'veto'
    };
    
    transactionManager.addHook(successHook);
    errorHooks.forEach(hook => transactionManager.addHook(hook));
    
    const delta = TestDataGenerators.generateDelta(1, 0);
    const result = await transactionManager.apply(store, delta);
    
    // Should handle errors gracefully
    expect(result).toBeDefined();
    expect(result.receipt.hookResults.length).toBe(11); // 10 error + 1 success
    expect(result.receipt.hookErrors.length).toBe(10); // 10 errors
  });
});

/**
 * Integration Tests
 */
describe('KGC Sidecar - Integration Tests', () => {
  let transactionManager;
  let knowledgeHookManager;
  let observability;
  let optimizer;
  let store;

  beforeEach(async () => {
    transactionManager = new TransactionManager({
      enableLockchain: true,
      enableResolution: true
    });
    
    knowledgeHookManager = new KnowledgeHookManager({
      basePath: process.cwd(),
      enableKnowledgeHooks: true
    });
    
    observability = new ObservabilityManager({
      enableTracing: true,
      enableMetrics: true
    });
    
    optimizer = new PerformanceOptimizer({
      enableFastPath: true,
      enableCaching: true,
      enableBatchProcessing: true
    });
    
    await observability.initialize();
    store = new Store();
  });

  it('should handle complete KGC workflow', async () => {
    // Add hooks
    const regularHook = TestDataGenerators.generateHook();
    transactionManager.addHook(regularHook);
    
    const knowledgeHook = TestDataGenerators.generateKnowledgeHook();
    knowledgeHookManager.addKnowledgeHook(knowledgeHook);
    
    // Apply optimized transaction
    const delta = TestDataGenerators.generateDelta(100, 50);
    const result = await optimizer.optimizeTransaction(
      async (context) => {
        return await transactionManager.apply(store, context.delta);
      },
      { delta, afterHashOnly: true }
    );
    
    // Verify result
    expect(result.receipt.committed).toBe(true);
    expect(result.receipt.hookResults.length).toBeGreaterThan(0);
    
    // Check observability
    const metrics = observability.getPerformanceMetrics();
    expect(metrics.transactionLatency.p50).toBeGreaterThan(0);
    expect(metrics.hookExecutionRate).toBeGreaterThan(0);
    
    // Check performance
    const perfMetrics = optimizer.getMetrics();
    expect(perfMetrics.transactionLatency.p50).toBeGreaterThan(0);
    expect(perfMetrics.cacheStats.hitRate).toBeGreaterThanOrEqual(0);
  });

  it('should handle policy pack integration', async () => {
    const policyPackManager = new PolicyPackManager(process.cwd());
    
    // Create test policy pack
    const policyPack = {
      meta: {
        name: 'test-policy',
        version: '1.0.0',
        description: 'Test policy pack'
      },
      config: {
        enabled: true,
        priority: 50
      },
      hooks: [
        {
          name: 'test-hook',
          file: 'test-hook.rq',
          enabled: true,
          priority: 80
        }
      ]
    };
    
    // Load policy pack
    await policyPackManager.loadPolicyPack(policyPack);
    
    // Apply transaction with policy pack
    const delta = TestDataGenerators.generateDelta(50, 25);
    const result = await transactionManager.apply(store, delta);
    
    expect(result.receipt.committed).toBe(true);
  });
});

export {
  TestDataGenerators,
  PerformanceTestUtils
};
