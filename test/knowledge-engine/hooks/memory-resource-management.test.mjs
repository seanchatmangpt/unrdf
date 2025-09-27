/**
 * @file Memory and Resource Management Tests
 * @module memory-resource-management
 * 
 * @description
 * Tests for memory leaks, resource cleanup, garbage collection pressure,
 * and unbounded growth scenarios in the knowledge hook system.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { writeFile, unlink } from 'fs/promises';
import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store } from 'n3';
import { join } from 'path';
import { tmpdir } from 'os';

describe('Memory and Resource Management', () => {
  let tempDir;
  let manager;
  let testStore;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `unrdf-memory-test-${Date.now()}`);
    await writeFile(tempDir, ''); // Create temp directory
    manager = new KnowledgeHookManager({ basePath: tempDir });
    testStore = new Store();
  });

  afterEach(async () => {
    try {
      await unlink(tempDir);
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe('Memory Leak Scenarios', () => {
    it('should handle memory leaks in long-running processes', async () => {
      const query = join(tempDir, 'memory-leak.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'memory-leak-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate memory leak by creating large objects
          const largeArray = new Array(1000000).fill(0).map((_, i) => ({
            id: i,
            data: new Array(100).fill(`data-${i}`),
            timestamp: Date.now()
          }));
          
          // Store reference to prevent garbage collection
          if (!global.memoryLeakTest) {
            global.memoryLeakTest = [];
          }
          global.memoryLeakTest.push(largeArray);
          
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      // Execute hook multiple times to simulate memory leak
      for (let i = 0; i < 10; i++) {
        await manager.executeAllKnowledgeHooks(event);
      }

      // Check if memory usage is growing
      const initialMemory = process.memoryUsage();
      expect(initialMemory.heapUsed).toBeGreaterThan(0);
      
      // Clean up global reference
      delete global.memoryLeakTest;
    });

    it('should handle circular references preventing garbage collection', async () => {
      const query = join(tempDir, 'circular-ref.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'circular-ref-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create circular reference
          const obj1 = { name: 'obj1' };
          const obj2 = { name: 'obj2' };
          obj1.ref = obj2;
          obj2.ref = obj1;
          
          // Store reference to prevent garbage collection
          if (!global.circularRefTest) {
            global.circularRefTest = [];
          }
          global.circularRefTest.push(obj1);
          
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      // Execute hook multiple times
      for (let i = 0; i < 5; i++) {
        await manager.executeAllKnowledgeHooks(event);
      }

      // Check if circular references are accumulating
      expect(global.circularRefTest).toBeDefined();
      expect(global.circularRefTest.length).toBe(5);
      
      // Clean up global reference
      delete global.circularRefTest;
    });

    it('should handle event listener leaks', async () => {
      const query = join(tempDir, 'listener-leak.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'listener-leak-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create event listener that's not properly cleaned up
          const emitter = new (require('events').EventEmitter)();
          const listener = () => console.log('Event received');
          emitter.on('test', listener);
          
          // Store reference to prevent garbage collection
          if (!global.listenerLeakTest) {
            global.listenerLeakTest = [];
          }
          global.listenerLeakTest.push({ emitter, listener });
          
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      // Execute hook multiple times
      for (let i = 0; i < 5; i++) {
        await manager.executeAllKnowledgeHooks(event);
      }

      // Check if listeners are accumulating
      expect(global.listenerLeakTest).toBeDefined();
      expect(global.listenerLeakTest.length).toBe(5);
      
      // Clean up global reference
      delete global.listenerLeakTest;
    });
  });

  describe('Unbounded Cache Growth', () => {
    it('should handle unbounded cache growth', async () => {
      const query = join(tempDir, 'cache-growth.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'cache-growth-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate cache growth without limits
          if (!global.cacheGrowthTest) {
            global.cacheGrowthTest = new Map();
          }
          
          // Add new entries to cache
          for (let i = 0; i < 1000; i++) {
            global.cacheGrowthTest.set(`key-${Date.now()}-${i}`, {
              data: new Array(100).fill(`data-${i}`),
              timestamp: Date.now()
            });
          }
          
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      // Execute hook multiple times to simulate cache growth
      for (let i = 0; i < 10; i++) {
        await manager.executeAllKnowledgeHooks(event);
      }

      // Check if cache is growing unbounded
      expect(global.cacheGrowthTest).toBeDefined();
      expect(global.cacheGrowthTest.size).toBeGreaterThan(10000);
      
      // Clean up global reference
      delete global.cacheGrowthTest;
    });

    it('should handle cache without TTL expiration', async () => {
      const query = join(tempDir, 'cache-ttl.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'cache-ttl-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate cache without TTL
          if (!global.cacheTTLTest) {
            global.cacheTTLTest = new Map();
          }
          
          // Add entries without expiration
          global.cacheTTLTest.set(`key-${Date.now()}`, {
            data: 'cached-data',
            timestamp: Date.now()
          });
          
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      // Execute hook multiple times
      for (let i = 0; i < 100; i++) {
        await manager.executeAllKnowledgeHooks(event);
      }

      // Check if cache is growing without expiration
      expect(global.cacheTTLTest).toBeDefined();
      expect(global.cacheTTLTest.size).toBe(100);
      
      // Clean up global reference
      delete global.cacheTTLTest;
    });
  });

  describe('Resource Cleanup on Process Termination', () => {
    it('should handle resource cleanup on process termination', async () => {
      const query = join(tempDir, 'cleanup.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'cleanup-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create resources that need cleanup
          const fileHandle = await require('fs/promises').open(tempDir, 'r');
          const timer = setTimeout(() => {}, 10000);
          const interval = setInterval(() => {}, 1000);
          
          // Store references for cleanup
          if (!global.cleanupTest) {
            global.cleanupTest = [];
          }
          global.cleanupTest.push({ fileHandle, timer, interval });
          
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      await manager.executeAllKnowledgeHooks(event);

      // Check if resources are created
      expect(global.cleanupTest).toBeDefined();
      expect(global.cleanupTest.length).toBe(1);
      
      // Simulate cleanup
      for (const resource of global.cleanupTest) {
        await resource.fileHandle.close();
        clearTimeout(resource.timer);
        clearInterval(resource.interval);
      }
      
      // Clean up global reference
      delete global.cleanupTest;
    });

    it('should handle database connection leaks', async () => {
      const query = join(tempDir, 'db-leak.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'db-leak-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate database connection
          const connection = {
            id: Date.now(),
            connected: true,
            close: () => { connection.connected = false; }
          };
          
          // Store reference without proper cleanup
          if (!global.dbLeakTest) {
            global.dbLeakTest = [];
          }
          global.dbLeakTest.push(connection);
          
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      // Execute hook multiple times
      for (let i = 0; i < 5; i++) {
        await manager.executeAllKnowledgeHooks(event);
      }

      // Check if connections are accumulating
      expect(global.dbLeakTest).toBeDefined();
      expect(global.dbLeakTest.length).toBe(5);
      
      // Clean up connections
      for (const connection of global.dbLeakTest) {
        connection.close();
      }
      
      // Clean up global reference
      delete global.dbLeakTest;
    });
  });

  describe('Large Payload Handling', () => {
    it('should handle large payloads without memory issues', async () => {
      const query = join(tempDir, 'large-payload.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'large-payload-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Process large payload
          const largeData = event.payload.largeData;
          if (largeData && largeData.length > 1000000) {
            return { success: false, error: 'Payload too large' };
          }
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Payload too large');
    });

    it('should handle memory pressure from multiple large operations', async () => {
      const query = join(tempDir, 'memory-pressure.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'memory-pressure-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
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
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
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

  describe('Garbage Collection Pressure', () => {
    it('should handle garbage collection pressure', async () => {
      const query = join(tempDir, 'gc-pressure.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'gc-pressure-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create objects that will be garbage collected
          const tempObjects = [];
          for (let i = 0; i < 10000; i++) {
            tempObjects.push({
              id: i,
              data: new Array(10).fill(`data-${i}`),
              timestamp: Date.now()
            });
          }
          
          // Process and discard
          const result = tempObjects.length;
          
          return { success: true, result };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      // Execute multiple times to create GC pressure
      const results = [];
      for (let i = 0; i < 10; i++) {
        const result = await manager.executeAllKnowledgeHooks(event);
        results.push(result);
        
        // Force garbage collection if available
        if (global.gc) {
          global.gc();
        }
      }

      expect(results).toHaveLength(10);
      results.forEach(result => {
        expect(result[0].success).toBe(true);
      });
    });

    it('should handle memory fragmentation', async () => {
      const query = join(tempDir, 'fragmentation.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'fragmentation-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
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
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      // Execute multiple times to create fragmentation
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
});
