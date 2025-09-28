/**
 * @file Performance and Scalability Tests
 * @module performance-scalability
 * 
 * @description
 * Tests for performance bottlenecks, scalability issues, memory fragmentation,
 * and CPU-intensive operations in the knowledge hook system.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { writeFile, unlink, mkdir } from 'fs/promises';
import { defineHook } from '../../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store } from 'n3';
import { join } from 'path';
import { tmpdir } from 'os';

describe('Performance and Scalability', () => {
  let tempDir;
  let manager;
  let testStore;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `unrdf-performance-test-${Date.now()}`);
    await require('fs/promises').mkdir(tempDir, { recursive: true });
    manager = new KnowledgeHookManager({ basePath: tempDir });
    testStore = new Store();
  });

  afterEach(async () => {
    try {
      await require('fs/promises').rm(tempDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe('Hook Execution Bottlenecks', () => {
    it('should handle slow hook execution', async () => {
      const query = join(tempDir, 'slow-hook.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'slow-hook-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate slow operation
          await new Promise(resolve => setTimeout(resolve, 1000));
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const startTime = Date.now();
      const results = await manager.executeAllKnowledgeHooks(event);
      const endTime = Date.now();

      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      expect(endTime - startTime).toBeGreaterThan(1000);
    });

    it('should handle multiple slow hooks', async () => {
      const query1 = join(tempDir, 'slow-hook1.sparql');
      const query2 = join(tempDir, 'slow-hook2.sparql');
      const query3 = join(tempDir, 'slow-hook3.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query3, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook1 = defineHook({
        meta: { name: 'slow-hook1-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          await new Promise(resolve => setTimeout(resolve, 500));
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'slow-hook2-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          await new Promise(resolve => setTimeout(resolve, 300));
          return { success: true };
        }
      });

      const hook3 = defineHook({
        meta: { name: 'slow-hook3-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query3}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          await new Promise(resolve => setTimeout(resolve, 200));
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);
      manager.addKnowledgeHook(hook3);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const startTime = Date.now();
      const results = await manager.executeAllKnowledgeHooks(event);
      const endTime = Date.now();

      expect(results).toHaveLength(3);
      results.forEach(result => {
        expect(result.success).toBe(true);
      });
      
      // Total time should be less than sum of individual times (parallel execution)
      expect(endTime - startTime).toBeLessThan(1000);
    });

    it('should handle hook execution timeouts', async () => {
      const query = join(tempDir, 'timeout-hook.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'timeout-hook-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate operation that exceeds timeout
          await new Promise(resolve => setTimeout(resolve, 10000));
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const startTime = Date.now();
      const results = await manager.executeAllKnowledgeHooks(event);
      const endTime = Date.now();

      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toContain('timeout');
      expect(endTime - startTime).toBeLessThan(10000);
    });
  });

  describe('Cache Performance Degradation', () => {
    it('should handle cache performance degradation', async () => {
      const query = join(tempDir, 'cache-degradation.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      let cacheHits = 0;
      let cacheMisses = 0;
      
      const hook = defineHook({
        meta: { name: 'cache-degradation-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate cache operations
          const cacheKey = event.payload.cacheKey;
          if (cacheKey) {
            // Simulate cache hit
            cacheHits++;
          } else {
            // Simulate cache miss
            cacheMisses++;
          }
          
          return { success: true, cacheHits, cacheMisses };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      // Execute multiple times to test cache performance
      const results = [];
      for (let i = 0; i < 100; i++) {
        const event = {
          name: 'test-event',
          payload: {
            cacheKey: i % 10 === 0 ? `key-${i}` : null // 10% cache hit rate
          },
          context: { graph: testStore }
        };

        const result = await manager.executeAllKnowledgeHooks(event);
        results.push(result);
      }

      expect(results).toHaveLength(100);
      expect(cacheHits).toBe(10);
      expect(cacheMisses).toBe(90);
    });

    it('should handle cache eviction performance', async () => {
      const query = join(tempDir, 'cache-eviction.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      let cacheSize = 0;
      let evictions = 0;
      
      const hook = defineHook({
        meta: { name: 'cache-eviction-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate cache operations
          const maxCacheSize = 100;
          const newEntry = event.payload.newEntry;
          
          if (newEntry) {
            if (cacheSize >= maxCacheSize) {
              evictions++;
              cacheSize = maxCacheSize; // Evict oldest entry
            } else {
              cacheSize++;
            }
          }
          
          return { success: true, cacheSize, evictions };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      // Execute multiple times to test cache eviction
      const results = [];
      for (let i = 0; i < 150; i++) {
        const event = {
          name: 'test-event',
          payload: {
            newEntry: true
          },
          context: { graph: testStore }
        };

        const result = await manager.executeAllKnowledgeHooks(event);
        results.push(result);
      }

      expect(results).toHaveLength(150);
      expect(cacheSize).toBe(100);
      expect(evictions).toBe(50);
    });
  });

  describe('Memory Fragmentation', () => {
    it('should handle memory fragmentation', async () => {
      const query = join(tempDir, 'fragmentation.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'fragmentation-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create memory fragmentation
          const arrays = [];
          for (let i = 0; i < 1000; i++) {
            const size = Math.floor(Math.random() * 1000) + 1;
            arrays.push(new Array(size).fill(`data-${i}`));
          }
          
          // Randomly delete some arrays to create fragmentation
          for (let i = 0; i < 500; i++) {
            const index = Math.floor(Math.random() * arrays.length);
            arrays.splice(index, 1);
          }
          
          return { success: true, remaining: arrays.length };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      // Execute multiple times to create fragmentation
      const results = [];
      for (let i = 0; i < 10; i++) {
        const result = await manager.executeAllKnowledgeHooks(event);
        results.push(result);
      }

      expect(results).toHaveLength(10);
      results.forEach(result => {
        expect(result[0].success).toBe(true);
        expect(result[0].remaining).toBe(500);
      });
    });

    it('should handle memory pressure from large objects', async () => {
      const query = join(tempDir, 'memory-pressure.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'memory-pressure-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create memory pressure
          const memoryIntensiveData = [];
          for (let i = 0; i < 100000; i++) {
            memoryIntensiveData.push({
              id: i,
              data: new Array(100).fill(`data-${i}`),
              timestamp: Date.now()
            });
          }
          
          // Simulate processing
          const result = memoryIntensiveData.reduce((sum, item) => sum + item.id, 0);
          
          return { success: true, result };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      // Execute multiple times to create memory pressure
      const results = [];
      for (let i = 0; i < 5; i++) {
        const result = await manager.executeAllKnowledgeHooks(event);
        results.push(result);
      }

      expect(results).toHaveLength(5);
      results.forEach(result => {
        expect(result[0].success).toBe(true);
      });
    });
  });

  describe('CPU-Intensive Operations', () => {
    it('should handle CPU-intensive operations', async () => {
      const query = join(tempDir, 'cpu-intensive.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'cpu-intensive-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate CPU-intensive operation
          let result = 0;
          for (let i = 0; i < 1000000; i++) {
            result += Math.sqrt(i);
          }
          
          return { success: true, result };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const startTime = Date.now();
      const results = await manager.executeAllKnowledgeHooks(event);
      const endTime = Date.now();

      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      expect(endTime - startTime).toBeGreaterThan(100); // Should take some time
    });

    it('should handle blocking CPU operations', async () => {
      const query = join(tempDir, 'blocking-cpu.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'blocking-cpu-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate blocking CPU operation
          const startTime = Date.now();
          while (Date.now() - startTime < 100) {
            // Blocking loop
          }
          
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const startTime = Date.now();
      const results = await manager.executeAllKnowledgeHooks(event);
      const endTime = Date.now();

      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      expect(endTime - startTime).toBeGreaterThan(100);
    });
  });

  describe('I/O Bound Operations', () => {
    it('should handle I/O bound operations', async () => {
      const query = join(tempDir, 'io-bound.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'io-bound-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate I/O bound operation
          await new Promise(resolve => setTimeout(resolve, 100));
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const startTime = Date.now();
      const results = await manager.executeAllKnowledgeHooks(event);
      const endTime = Date.now();

      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      expect(endTime - startTime).toBeGreaterThan(100);
    });

    it('should handle concurrent I/O operations', async () => {
      const query1 = join(tempDir, 'concurrent-io1.sparql');
      const query2 = join(tempDir, 'concurrent-io2.sparql');
      const query3 = join(tempDir, 'concurrent-io3.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query3, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook1 = defineHook({
        meta: { name: 'concurrent-io1-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          await new Promise(resolve => setTimeout(resolve, 200));
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'concurrent-io2-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          await new Promise(resolve => setTimeout(resolve, 150));
          return { success: true };
        }
      });

      const hook3 = defineHook({
        meta: { name: 'concurrent-io3-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query3}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          await new Promise(resolve => setTimeout(resolve, 100));
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);
      manager.addKnowledgeHook(hook3);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const startTime = Date.now();
      const results = await manager.executeAllKnowledgeHooks(event);
      const endTime = Date.now();

      expect(results).toHaveLength(3);
      results.forEach(result => {
        expect(result.success).toBe(true);
      });
      
      // Total time should be less than sum of individual times (concurrent execution)
      expect(endTime - startTime).toBeLessThan(450);
    });
  });

  describe('Scalability Limits', () => {
    it('should handle large number of hooks', async () => {
      const hooks = [];
      const hookCount = 100;
      
      for (let i = 0; i < hookCount; i++) {
        const query = join(tempDir, `hook-${i}.sparql`);
        await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
        
        const hook = defineHook({
          meta: { name: `scalability-hook-${i}` },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: `file://${query}`,
              sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
              mediaType: 'application/sparql-query'
            }
          },
          run: async (event) => {
            return { success: true, hookId: i };
          }
        });
        
        hooks.push(hook);
        manager.addKnowledgeHook(hook);
      }

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const startTime = Date.now();
      const results = await manager.executeAllKnowledgeHooks(event);
      const endTime = Date.now();

      expect(results).toHaveLength(hookCount);
      results.forEach((result, index) => {
        expect(result.success).toBe(true);
        expect(result.hookId).toBe(index);
      });
      
      // Should complete within reasonable time
      expect(endTime - startTime).toBeLessThan(5000);
    });

    it('should handle large payload sizes', async () => {
      const query = join(tempDir, 'large-payload.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'large-payload-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Process large payload
          const largeData = event.payload.largeData;
          if (largeData && largeData.length > 1000000) {
            return { success: false, error: 'Payload too large' };
          }
          
          return { success: true, processed: largeData ? largeData.length : 0 };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      // Create large payload
      const largeData = new Array(1000000).fill(0).map((_, i) => ({
        id: i,
        data: `data-${i}`,
        timestamp: Date.now()
      }));

      const event = {
        name: 'test-event',
        payload: { largeData },
        context: { graph: testStore }
      };

      const startTime = Date.now();
      const results = await manager.executeAllKnowledgeHooks(event);
      const endTime = Date.now();

      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Payload too large');
      expect(endTime - startTime).toBeLessThan(1000);
    });
  });
});
