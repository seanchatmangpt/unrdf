/**
 * @file Concurrency and Race Conditions Tests
 * @module concurrency-race-conditions
 * 
 * @description
 * Tests for concurrency issues, race conditions, deadlocks, and transaction
 * isolation violations in the knowledge hook system.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { writeFile, unlink } from 'fs/promises';
import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store } from 'n3';
import { join } from 'path';
import { tmpdir } from 'os';

describe('Concurrency and Race Conditions', () => {
  let tempDir;
  let manager;
  let testStore;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `unrdf-concurrency-test-${Date.now()}`);
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

  describe('Hook Execution Order Dependencies', () => {
    it('should handle hooks with execution order dependencies', async () => {
      const query1 = join(tempDir, 'query1.sparql');
      const query2 = join(tempDir, 'query2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook1 = defineHook({
        meta: { name: 'hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Hook1 should run first
          if (!event.payload.hook1Executed) {
            return { success: false, error: 'Hook1 not executed first' };
          }
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Hook2 should run after hook1
          return { success: true, hook1Executed: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(true);
    });

    it('should handle circular hook dependencies', async () => {
      const query1 = join(tempDir, 'circular1.sparql');
      const query2 = join(tempDir, 'circular2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook1 = defineHook({
        meta: { name: 'circular-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Hook1 depends on hook2
          if (!event.payload.hook2Executed) {
            return { success: false, error: 'Hook2 not executed' };
          }
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'circular-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Hook2 depends on hook1
          if (!event.payload.hook1Executed) {
            return { success: false, error: 'Hook1 not executed' };
          }
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      // At least one should fail due to circular dependency
      const failures = results.filter(r => !r.success);
      expect(failures.length).toBeGreaterThan(0);
    });
  });

  describe('Deadlock Scenarios', () => {
    it('should handle deadlocks between hooks', async () => {
      const query1 = join(tempDir, 'deadlock1.sparql');
      const query2 = join(tempDir, 'deadlock2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let lock1 = false;
      let lock2 = false;
      
      const hook1 = defineHook({
        meta: { name: 'deadlock-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Acquire lock1, then wait for lock2
          lock1 = true;
          await new Promise(resolve => setTimeout(resolve, 100));
          if (lock2) {
            return { success: false, error: 'Deadlock detected' };
          }
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'deadlock-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Acquire lock2, then wait for lock1
          lock2 = true;
          await new Promise(resolve => setTimeout(resolve, 100));
          if (lock1) {
            return { success: false, error: 'Deadlock detected' };
          }
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      // Should detect and handle deadlock
      const failures = results.filter(r => !r.success);
      expect(failures.length).toBeGreaterThan(0);
    });

    it('should handle resource contention', async () => {
      const query1 = join(tempDir, 'contention1.sparql');
      const query2 = join(tempDir, 'contention2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let resourceInUse = false;
      
      const hook1 = defineHook({
        meta: { name: 'contention-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          if (resourceInUse) {
            return { success: false, error: 'Resource in use' };
          }
          resourceInUse = true;
          await new Promise(resolve => setTimeout(resolve, 200));
          resourceInUse = false;
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'contention-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          if (resourceInUse) {
            return { success: false, error: 'Resource in use' };
          }
          resourceInUse = true;
          await new Promise(resolve => setTimeout(resolve, 200));
          resourceInUse = false;
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      // One should succeed, one should fail due to resource contention
      const successes = results.filter(r => r.success);
      const failures = results.filter(r => !r.success);
      expect(successes.length).toBe(1);
      expect(failures.length).toBe(1);
    });
  });

  describe('Transaction Isolation Violations', () => {
    it('should handle dirty reads', async () => {
      const query1 = join(tempDir, 'dirty1.sparql');
      const query2 = join(tempDir, 'dirty2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let sharedData = { value: 0 };
      
      const hook1 = defineHook({
        meta: { name: 'dirty-read-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Modify shared data
          sharedData.value = 100;
          await new Promise(resolve => setTimeout(resolve, 100));
          // Rollback
          sharedData.value = 0;
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'dirty-read-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Read shared data
          const value = sharedData.value;
          if (value === 100) {
            return { success: false, error: 'Dirty read detected' };
          }
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      // Should detect dirty read
      const failures = results.filter(r => !r.success);
      expect(failures.length).toBeGreaterThan(0);
    });

    it('should handle non-repeatable reads', async () => {
      const query1 = join(tempDir, 'non-repeatable1.sparql');
      const query2 = join(tempDir, 'non-repeatable2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let sharedData = { value: 0 };
      
      const hook1 = defineHook({
        meta: { name: 'non-repeatable-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Read value
          const value1 = sharedData.value;
          await new Promise(resolve => setTimeout(resolve, 100));
          // Read value again
          const value2 = sharedData.value;
          if (value1 !== value2) {
            return { success: false, error: 'Non-repeatable read detected' };
          }
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'non-repeatable-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Modify shared data
          sharedData.value = 100;
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      // Should detect non-repeatable read
      const failures = results.filter(r => !r.success);
      expect(failures.length).toBeGreaterThan(0);
    });

    it('should handle phantom reads', async () => {
      const query1 = join(tempDir, 'phantom1.sparql');
      const query2 = join(tempDir, 'phantom2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let sharedArray = [];
      
      const hook1 = defineHook({
        meta: { name: 'phantom-read-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Count items
          const count1 = sharedArray.length;
          await new Promise(resolve => setTimeout(resolve, 100));
          // Count items again
          const count2 = sharedArray.length;
          if (count1 !== count2) {
            return { success: false, error: 'Phantom read detected' };
          }
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'phantom-read-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Add item
          sharedArray.push('new-item');
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      // Should detect phantom read
      const failures = results.filter(r => !r.success);
      expect(failures.length).toBeGreaterThan(0);
    });
  });

  describe('Cache Invalidation Race Conditions', () => {
    it('should handle cache invalidation races', async () => {
      const query1 = join(tempDir, 'cache1.sparql');
      const query2 = join(tempDir, 'cache2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let cache = new Map();
      let cacheVersion = 0;
      
      const hook1 = defineHook({
        meta: { name: 'cache-race-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Read from cache
          const cached = cache.get('key');
          await new Promise(resolve => setTimeout(resolve, 100));
          // Check if cache was invalidated
          const currentVersion = cacheVersion;
          if (cached && cached.version !== currentVersion) {
            return { success: false, error: 'Cache invalidation race detected' };
          }
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'cache-race-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Invalidate cache
          cacheVersion++;
          cache.set('key', { value: 'new-value', version: cacheVersion });
          return { success: true };
        }
      });

      // Initialize cache
      cache.set('key', { value: 'old-value', version: 0 });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      // Should detect cache invalidation race
      const failures = results.filter(r => !r.success);
      expect(failures.length).toBeGreaterThan(0);
    });
  });

  describe('Concurrent File Access', () => {
    it('should handle concurrent file access conflicts', async () => {
      const query1 = join(tempDir, 'concurrent1.sparql');
      const query2 = join(tempDir, 'concurrent2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let fileLock = false;
      
      const hook1 = defineHook({
        meta: { name: 'concurrent-file-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          if (fileLock) {
            return { success: false, error: 'File access conflict' };
          }
          fileLock = true;
          await new Promise(resolve => setTimeout(resolve, 200));
          fileLock = false;
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'concurrent-file-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          if (fileLock) {
            return { success: false, error: 'File access conflict' };
          }
          fileLock = true;
          await new Promise(resolve => setTimeout(resolve, 200));
          fileLock = false;
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      // One should succeed, one should fail due to file access conflict
      const successes = results.filter(r => r.success);
      const failures = results.filter(r => !r.success);
      expect(successes.length).toBe(1);
      expect(failures.length).toBe(1);
    });
  });
});
