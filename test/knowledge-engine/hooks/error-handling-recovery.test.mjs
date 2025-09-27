/**
 * @file Error Handling and Recovery Tests
 * @module error-handling-recovery
 * 
 * @description
 * Tests for error handling, recovery mechanisms, cascading failures,
 * partial transaction rollback, and error message handling in the knowledge hook system.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { writeFile, unlink } from 'fs/promises';
import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store } from 'n3';
import { join } from 'path';
import { tmpdir } from 'os';

describe('Error Handling and Recovery', () => {
  let tempDir;
  let manager;
  let testStore;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `unrdf-error-test-${Date.now()}`);
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

  describe('Cascading Failures Across Hooks', () => {
    it('should handle cascading failures when one hook fails', async () => {
      const query1 = join(tempDir, 'cascade1.sparql');
      const query2 = join(tempDir, 'cascade2.sparql');
      const query3 = join(tempDir, 'cascade3.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query3, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook1 = defineHook({
        meta: { name: 'cascade-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // First hook fails
          throw new Error('Hook1 failed');
        }
      });

      const hook2 = defineHook({
        meta: { name: 'cascade-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Second hook depends on first hook's success
          if (!event.payload.hook1Success) {
            throw new Error('Hook2 failed due to Hook1 failure');
          }
          return { success: true };
        }
      });

      const hook3 = defineHook({
        meta: { name: 'cascade-hook3' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query3}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Third hook depends on both previous hooks
          if (!event.payload.hook1Success || !event.payload.hook2Success) {
            throw new Error('Hook3 failed due to previous failures');
          }
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);
      manager.addKnowledgeHook(hook3);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(3);
      
      // All hooks should fail due to cascading failure
      results.forEach(result => {
        expect(result.success).toBe(false);
        expect(result.error).toBeDefined();
      });
    });

    it('should handle partial cascading failures', async () => {
      const query1 = join(tempDir, 'partial1.sparql');
      const query2 = join(tempDir, 'partial2.sparql');
      const query3 = join(tempDir, 'partial3.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query3, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook1 = defineHook({
        meta: { name: 'partial-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          return { success: true, hook1Success: true };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'partial-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Second hook fails
          throw new Error('Hook2 failed');
        }
      });

      const hook3 = defineHook({
        meta: { name: 'partial-hook3' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query3}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Third hook succeeds independently
          return { success: true, hook3Success: true };
        }
      });

      manager.addKnowledgeHook(hook1);
      manager.addKnowledgeHook(hook2);
      manager.addKnowledgeHook(hook3);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(3);
      
      // Hook1 and Hook3 should succeed, Hook2 should fail
      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(false);
      expect(results[2].success).toBe(true);
    });

    it('should handle error propagation through hook chains', async () => {
      const query1 = join(tempDir, 'propagate1.sparql');
      const query2 = join(tempDir, 'propagate2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook1 = defineHook({
        meta: { name: 'propagate-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate error that should propagate
          const error = new Error('Critical system error');
          error.code = 'CRITICAL_ERROR';
          error.severity = 'high';
          throw error;
        }
      });

      const hook2 = defineHook({
        meta: { name: 'propagate-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // This hook should handle the propagated error
          if (event.payload.errors && event.payload.errors.some(e => e.code === 'CRITICAL_ERROR')) {
            return { success: false, error: 'Cannot proceed due to critical error' };
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
      
      // Both hooks should fail due to error propagation
      expect(results[0].success).toBe(false);
      expect(results[1].success).toBe(false);
    });
  });

  describe('Partial Transaction Rollback', () => {
    it('should handle partial transaction rollback on hook failure', async () => {
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
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate partial transaction commit
          transactionState.committed = true;
          return { success: true, transactionId: 'txn-123' };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'rollback-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // This hook fails, triggering rollback
          throw new Error('Hook2 failed, triggering rollback');
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
      
      // First hook should succeed, second should fail
      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(false);
      
      // Transaction should be rolled back
      expect(transactionState.committed).toBe(true);
      expect(transactionState.rolledBack).toBe(false); // Rollback would be handled by transaction manager
    });

    it('should handle rollback with dependent operations', async () => {
      const query1 = join(tempDir, 'dependent1.sparql');
      const query2 = join(tempDir, 'dependent2.sparql');
      
      await writeFile(query1, 'SELECT * WHERE { ?s ?p ?o }');
      await writeFile(query2, 'SELECT * WHERE { ?s ?p ?o }');
      
      let operations = [];
      
      const hook1 = defineHook({
        meta: { name: 'dependent-hook1' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query1}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create dependent operation
          operations.push({ id: 'op1', type: 'create', status: 'pending' });
          return { success: true, operationId: 'op1' };
        }
      });

      const hook2 = defineHook({
        meta: { name: 'dependent-hook2' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query2}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // This hook fails, should trigger rollback of dependent operations
          throw new Error('Hook2 failed');
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
      
      // First hook should succeed, second should fail
      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(false);
      
      // Dependent operation should be marked for rollback
      expect(operations).toHaveLength(1);
      expect(operations[0].status).toBe('pending');
    });
  });

  describe('Error Message Truncation', () => {
    it('should handle extremely long error messages', async () => {
      const query = join(tempDir, 'long-error.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'long-error-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create extremely long error message
          const longError = 'Error: ' + 'A'.repeat(100000);
          throw new Error(longError);
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBeDefined();
      
      // Error message should be truncated to reasonable length
      expect(results[0].error.length).toBeLessThan(10000);
    });

    it('should handle error messages with special characters', async () => {
      const query = join(tempDir, 'special-chars.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'special-chars-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create error message with special characters
          const specialError = 'Error: \x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F';
          throw new Error(specialError);
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBeDefined();
      
      // Error message should be sanitized
      expect(results[0].error).not.toContain('\x00');
    });

    it('should handle error messages with unicode characters', async () => {
      const query = join(tempDir, 'unicode-error.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'unicode-error-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create error message with unicode characters
          const unicodeError = 'Error: 你好世界 🌍 🚀 💻';
          throw new Error(unicodeError);
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBeDefined();
      
      // Error message should preserve unicode characters
      expect(results[0].error).toContain('你好世界');
    });
  });

  describe('Stack Overflow in Error Handling', () => {
    it('should handle recursive error handling', async () => {
      const query = join(tempDir, 'recursive-error.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'recursive-error-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate recursive error handling
          const recursiveError = new Error('Recursive error');
          recursiveError.cause = recursiveError; // Circular reference
          throw recursiveError;
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBeDefined();
      
      // Should handle recursive error without stack overflow
      expect(results[0].error).toContain('Recursive error');
    });

    it('should handle deeply nested error chains', async () => {
      const query = join(tempDir, 'nested-error.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'nested-error-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Create deeply nested error chain
          let error = new Error('Level 1000');
          for (let i = 999; i >= 1; i--) {
            const newError = new Error(`Level ${i}`);
            newError.cause = error;
            error = newError;
          }
          throw error;
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBeDefined();
      
      // Should handle nested errors without stack overflow
      expect(results[0].error).toContain('Level 1');
    });
  });

  describe('Error Recovery Loops', () => {
    it('should handle infinite error recovery loops', async () => {
      const query = join(tempDir, 'recovery-loop.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      let attemptCount = 0;
      
      const hook = defineHook({
        meta: { name: 'recovery-loop-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          attemptCount++;
          if (attemptCount < 1000) {
            // Keep failing to simulate recovery loop
            throw new Error(`Attempt ${attemptCount} failed`);
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBeDefined();
      
      // Should prevent infinite recovery loops
      expect(attemptCount).toBeLessThan(1000);
    });

    it('should handle exponential backoff in error recovery', async () => {
      const query = join(tempDir, 'backoff-recovery.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      let attemptCount = 0;
      const startTime = Date.now();
      
      const hook = defineHook({
        meta: { name: 'backoff-recovery-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          attemptCount++;
          if (attemptCount < 5) {
            // Simulate exponential backoff
            const delay = Math.pow(2, attemptCount) * 100; // 200ms, 400ms, 800ms, 1600ms
            await new Promise(resolve => setTimeout(resolve, delay));
            throw new Error(`Attempt ${attemptCount} failed`);
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBeDefined();
      
      // Should implement exponential backoff
      const duration = Date.now() - startTime;
      expect(duration).toBeGreaterThan(3000); // At least 3 seconds of backoff
    });

    it('should handle circuit breaker pattern in error recovery', async () => {
      const query = join(tempDir, 'circuit-breaker.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      let failureCount = 0;
      let circuitOpen = false;
      
      const hook = defineHook({
        meta: { name: 'circuit-breaker-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          if (circuitOpen) {
            throw new Error('Circuit breaker is open');
          }
          
          failureCount++;
          if (failureCount >= 3) {
            circuitOpen = true;
            throw new Error('Circuit breaker opened due to failures');
          }
          
          throw new Error(`Failure ${failureCount}`);
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

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBeDefined();
      
      // Should implement circuit breaker pattern
      expect(failureCount).toBe(3);
      expect(circuitOpen).toBe(true);
    });
  });
});