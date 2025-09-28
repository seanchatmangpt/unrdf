/**
 * @file Data Consistency and Corruption Tests
 * @module data-consistency-corruption
 * 
 * @description
 * Tests for data consistency issues, corruption scenarios, partial commits,
 * and inconsistent state handling in the knowledge hook system.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { writeFile, unlink, mkdir } from 'fs/promises';
import { defineHook } from '../../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store } from 'n3';
import { join } from 'path';
import { tmpdir } from 'os';

describe('Data Consistency and Corruption', () => {
  let tempDir;
  let manager;
  let testStore;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `unrdf-consistency-test-${Date.now()}`);
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

  describe('Partial Transaction Commits', () => {
    it('should handle partial transaction commits', async () => {
      const query1 = join(tempDir, 'partial1.sparql');
      const query2 = join(tempDir, 'partial2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let transactionState = { committed: false, partial: false };
      
      const hook1 = defineHook({
        meta: { name: 'partial-commit-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Hook1 commits successfully
          transactionState.committed = true;
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'partial-commit-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Hook2 fails after hook1 commits
          if (transactionState.committed) {
            transactionState.partial = true;
            throw new Error('Hook2 failed after hook1 commit');
          }
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      
      // Hook1 should succeed, hook2 should fail
      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(false);
      
      // Transaction should be in partial state
      expect(transactionState.partial).toBe(true);
    });

    it('should handle rollback after partial commit', async () => {
      const query1 = join(tempDir, 'rollback1.sparql');
      const query2 = join(tempDir, 'rollback2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let transactionState = { committed: false, rolledBack: false };
      
      const hook1 = defineHook({
        meta: { name: 'rollback-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Hook1 commits
          transactionState.committed = true;
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'rollback-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Hook2 fails and triggers rollback
          if (transactionState.committed) {
            transactionState.rolledBack = true;
            throw new Error('Hook2 failed, triggering rollback');
          }
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      
      // Hook1 should succeed, hook2 should fail
      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(false);
      
      // Transaction should be rolled back
      expect(transactionState.rolledBack).toBe(true);
    });
  });

  describe('Store Corruption Scenarios', () => {
    it('should handle store corruption during operations', async () => {
      const query = join(tempDir, 'corruption.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'corruption-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate store corruption
          const store = event.context.graph;
          if (store && typeof store.getQuads === 'function') {
            // Attempt to corrupt store
            try {
              store.getQuads = null; // Corrupt the method
              store.getQuads(); // This should fail
            } catch (error) {
              throw new Error('Store corruption detected');
            }
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Store corruption detected');
    });

    it('should handle data race conditions', async () => {
      const query1 = join(tempDir, 'race1.sparql');
      const query2 = join(tempDir, 'race2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let sharedData = { value: 0 };
      
      const hook1 = defineHook({
        meta: { name: 'race-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Read value
          const currentValue = sharedData.value;
          await new Promise(resolve => setTimeout(resolve, 100));
          // Write value
          sharedData.value = currentValue + 1;
          return { success: true, value: sharedData.value };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'race-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Read value
          const currentValue = sharedData.value;
          await new Promise(resolve => setTimeout(resolve, 100));
          // Write value
          sharedData.value = currentValue + 1;
          return { success: true, value: sharedData.value };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      
      // Both hooks should succeed, but final value should be consistent
      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(true);
      
      // Final value should be 2 (both hooks increment)
      expect(sharedData.value).toBe(2);
    });
  });

  describe('Inconsistent State After Failures', () => {
    it('should handle inconsistent state after hook failures', async () => {
      const query1 = join(tempDir, 'inconsistent1.sparql');
      const query2 = join(tempDir, 'inconsistent2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let systemState = { 
        data: { users: [], orders: [] },
        consistency: true 
      };
      
      const hook1 = defineHook({
        meta: { name: 'inconsistent-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Hook1 modifies data
          systemState.data.users.push({ id: 1, name: 'User1' });
          systemState.data.orders.push({ id: 1, userId: 1, amount: 100 });
          return { success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'inconsistent-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Hook2 fails after hook1 modifies data
          systemState.consistency = false;
          throw new Error('Hook2 failed, leaving inconsistent state');
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(2);
      
      // Hook1 should succeed, hook2 should fail
      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(false);
      
      // System should be in inconsistent state
      expect(systemState.consistency).toBe(false);
      expect(systemState.data.users.length).toBe(1);
      expect(systemState.data.orders.length).toBe(1);
    });

    it('should handle state recovery after failures', async () => {
      const query = join(tempDir, 'recovery.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      let systemState = { 
        data: { users: [], orders: [] },
        backup: null
      };
      
      const hook = defineHook({
        meta: { name: 'recovery-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create backup before modification
          systemState.backup = JSON.parse(JSON.stringify(systemState.data));
          
          // Modify data
          systemState.data.users.push({ id: 1, name: 'User1' });
          systemState.data.orders.push({ id: 1, userId: 1, amount: 100 });
          
          // Simulate failure
          throw new Error('Operation failed');
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      
      // System should have backup for recovery
      expect(systemState.backup).toBeDefined();
      expect(systemState.backup.users.length).toBe(0);
      expect(systemState.backup.orders.length).toBe(0);
    });
  });

  describe('Transaction Log Corruption', () => {
    it('should handle transaction log corruption', async () => {
      const query = join(tempDir, 'log-corruption.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      let transactionLog = [];
      
      const hook = defineHook({
        meta: { name: 'log-corruption-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate transaction log corruption
          transactionLog.push({ id: 1, operation: 'INSERT', data: 'user1' });
          transactionLog.push({ id: 2, operation: 'INSERT', data: 'user2' });
          
          // Corrupt the log
          transactionLog[0] = null;
          transactionLog[1] = { id: 2, operation: 'CORRUPTED', data: 'corrupted' };
          
          // Attempt to process corrupted log
          for (const entry of transactionLog) {
            if (entry && entry.operation === 'CORRUPTED') {
              throw new Error('Transaction log corruption detected');
            }
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Transaction log corruption detected');
    });

    it('should handle incomplete transaction logs', async () => {
      const query = join(tempDir, 'incomplete-log.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      let transactionLog = [];
      
      const hook = defineHook({
        meta: { name: 'incomplete-log-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate incomplete transaction log
          transactionLog.push({ id: 1, operation: 'BEGIN' });
          transactionLog.push({ id: 2, operation: 'INSERT', data: 'user1' });
          // Missing COMMIT or ROLLBACK
          
          // Check for incomplete transaction
          const beginCount = transactionLog.filter(e => e.operation === 'BEGIN').length;
          const commitCount = transactionLog.filter(e => e.operation === 'COMMIT').length;
          const rollbackCount = transactionLog.filter(e => e.operation === 'ROLLBACK').length;
          
          if (beginCount > (commitCount + rollbackCount)) {
            throw new Error('Incomplete transaction log detected');
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Incomplete transaction log detected');
    });
  });

  describe('Data Validation and Integrity', () => {
    it('should handle data validation failures', async () => {
      const query = join(tempDir, 'validation.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'validation-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Validate input data
          const userData = event.payload.userData;
          if (userData) {
            if (!userData.name || typeof userData.name !== 'string') {
              throw new Error('Invalid user name');
            }
            if (!userData.email || !userData.email.includes('@')) {
              throw new Error('Invalid email format');
            }
            if (userData.age && (userData.age < 0 || userData.age > 150)) {
              throw new Error('Invalid age range');
            }
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
        payload: {
          userData: {
            name: '',
            email: 'invalid-email',
            age: -5
          }
        },
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Invalid user name');
    });

    it('should handle data integrity violations', async () => {
      const query = join(tempDir, 'integrity.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      let database = {
        users: [
          { id: 1, name: 'User1', email: 'user1@example.com' },
          { id: 2, name: 'User2', email: 'user2@example.com' }
        ],
        orders: [
          { id: 1, userId: 1, amount: 100 },
          { id: 2, userId: 2, amount: 200 }
        ]
      };
      
      const hook = defineHook({
        meta: { name: 'integrity-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Check referential integrity
          const newOrder = event.payload.newOrder;
          if (newOrder) {
            const userExists = database.users.some(user => user.id === newOrder.userId);
            if (!userExists) {
              throw new Error('Referential integrity violation: user not found');
            }
            
            // Add order
            database.orders.push(newOrder);
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
        payload: {
          newOrder: {
            id: 3,
            userId: 999, // Non-existent user
            amount: 300
          }
        },
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Referential integrity violation: user not found');
    });
  });
});
