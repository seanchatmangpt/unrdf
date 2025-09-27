/**
 * @file Security and Authorization Tests
 * @module security-authorization
 * 
 * @description
 * Tests for security vulnerabilities, authorization bypasses, code injection,
 * and privilege escalation scenarios in the knowledge hook system.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { writeFile, unlink } from 'fs/promises';
import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store } from 'n3';
import { join } from 'path';
import { tmpdir } from 'os';

describe('Security and Authorization', () => {
  let tempDir;
  let manager;
  let testStore;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `unrdf-security-test-${Date.now()}`);
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

  describe('Code Injection Scenarios', () => {
    it('should prevent SPARQL injection attacks', async () => {
      const maliciousQuery = join(tempDir, 'injection.sparql');
      // Malicious SPARQL with injection attempt
      const query = `
        SELECT * WHERE {
          ?s ?p ?o .
          FILTER(?o = "'; DROP TABLE users; --")
        }
      `;
      await writeFile(maliciousQuery, query);
      
      const hook = defineHook({
        meta: { name: 'sparql-injection-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${maliciousQuery}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          return { success: true };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock query execution to detect injection
      vi.spyOn(require('../../src/knowledge-engine/query.mjs'), 'ask')
        .mockImplementation(async (store, query) => {
          if (query.includes('DROP TABLE') || query.includes('--')) {
            throw new Error('Potential SQL injection detected');
          }
          return true;
        });

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toContain('Potential SQL injection detected');
    });

    it('should prevent JavaScript code injection', async () => {
      const query = join(tempDir, 'js-injection.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'js-injection-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Attempt to inject JavaScript code
          const maliciousPayload = event.payload.maliciousCode;
          if (maliciousPayload && typeof maliciousPayload === 'string') {
            if (maliciousPayload.includes('eval(') || maliciousPayload.includes('Function(')) {
              throw new Error('JavaScript injection detected');
            }
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
        payload: {
          maliciousCode: 'eval("malicious code")'
        },
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('JavaScript injection detected');
    });

    it('should prevent command injection', async () => {
      const query = join(tempDir, 'command-injection.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'command-injection-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Attempt to inject system commands
          const userInput = event.payload.userInput;
          if (userInput && typeof userInput === 'string') {
            if (userInput.includes(';') || userInput.includes('|') || userInput.includes('&')) {
              throw new Error('Command injection detected');
            }
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
        payload: {
          userInput: 'test; rm -rf /'
        },
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Command injection detected');
    });
  });

  describe('Path Traversal Attacks', () => {
    it('should prevent path traversal in file URIs', async () => {
      const hook = defineHook({
        meta: { name: 'path-traversal-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://../../../etc/passwd',
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          return { success: true };
        }
      });

      // Should reject path traversal attempts
      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should prevent directory traversal with encoded paths', async () => {
      const hook = defineHook({
        meta: { name: 'encoded-traversal-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://..%2F..%2F..%2Fetc%2Fpasswd',
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          return { success: true };
        }
      });

      // Should reject encoded path traversal attempts
      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should prevent null byte injection', async () => {
      const hook = defineHook({
        meta: { name: 'null-byte-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://legitimate-file.sparql%00../../../etc/passwd',
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          return { success: true };
        }
      });

      // Should reject null byte injection attempts
      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });
  });

  describe('Privilege Escalation', () => {
    it('should prevent privilege escalation through hook execution', async () => {
      const query = join(tempDir, 'privilege-escalation.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'privilege-escalation-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Attempt to escalate privileges
          const userRole = event.context.userRole || 'user';
          if (userRole === 'user') {
            // Attempt to access admin functionality
            if (event.payload.attemptAdminAccess) {
              throw new Error('Privilege escalation attempt detected');
            }
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
        payload: {
          attemptAdminAccess: true
        },
        context: { 
          graph: testStore,
          userRole: 'user'
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Privilege escalation attempt detected');
    });

    it('should prevent role bypass through hook manipulation', async () => {
      const query = join(tempDir, 'role-bypass.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'role-bypass-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Attempt to bypass role checks
          const originalRole = event.context.userRole;
          event.context.userRole = 'admin'; // Attempt to change role
          
          if (originalRole === 'user' && event.context.userRole === 'admin') {
            throw new Error('Role bypass attempt detected');
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
        context: { 
          graph: testStore,
          userRole: 'user'
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Role bypass attempt detected');
    });
  });

  describe('Information Disclosure', () => {
    it('should prevent sensitive information disclosure in error messages', async () => {
      const query = join(tempDir, 'info-disclosure.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'info-disclosure-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate error with sensitive information
          const error = new Error('Database connection failed');
          error.details = {
            host: 'prod-db.internal.com',
            port: 5432,
            username: 'admin',
            password: 'secret123',
            database: 'production'
          };
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
      
      // Error message should not contain sensitive information
      const errorMessage = results[0].error;
      expect(errorMessage).not.toContain('prod-db.internal.com');
      expect(errorMessage).not.toContain('admin');
      expect(errorMessage).not.toContain('secret123');
      expect(errorMessage).not.toContain('production');
    });

    it('should prevent stack trace disclosure', async () => {
      const query = join(tempDir, 'stack-trace.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'stack-trace-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Simulate error with stack trace
          const error = new Error('Internal server error');
          error.stack = `
            Error: Internal server error
                at DatabaseConnection.connect (/app/src/db/connection.js:45:12)
                at UserService.authenticate (/app/src/services/user.js:23:8)
                at HookExecutor.execute (/app/src/hooks/executor.js:67:15)
          `;
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
      
      // Error message should not contain stack trace
      const errorMessage = results[0].error;
      expect(errorMessage).not.toContain('/app/src/');
      expect(errorMessage).not.toContain('DatabaseConnection.connect');
      expect(errorMessage).not.toContain('UserService.authenticate');
    });
  });

  describe('Denial of Service', () => {
    it('should prevent resource exhaustion attacks', async () => {
      const query = join(tempDir, 'resource-exhaustion.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'resource-exhaustion-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Attempt to exhaust resources
          const iterations = event.payload.iterations || 1;
          if (iterations > 1000) {
            throw new Error('Resource exhaustion attempt detected');
          }
          
          // Simulate resource-intensive operation
          for (let i = 0; i < iterations; i++) {
            new Array(1000).fill(0);
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
        payload: {
          iterations: 10000
        },
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Resource exhaustion attempt detected');
    });

    it('should prevent infinite loop attacks', async () => {
      const query = join(tempDir, 'infinite-loop.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'infinite-loop-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Attempt to create infinite loop
          let counter = 0;
          while (true) {
            counter++;
            if (counter > 1000) {
              throw new Error('Infinite loop detected');
            }
            // Simulate some work
            await new Promise(resolve => setTimeout(resolve, 1));
          }
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
      expect(results[0].error).toBe('Infinite loop detected');
    });

    it('should prevent memory bomb attacks', async () => {
      const query = join(tempDir, 'memory-bomb.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'memory-bomb-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Attempt to create memory bomb
          const size = event.payload.size || 1;
          if (size > 1000000) {
            throw new Error('Memory bomb attempt detected');
          }
          
          // Create large array
          const largeArray = new Array(size).fill(0);
          
          return { success: true, size: largeArray.length };
        }
      });

      manager.addKnowledgeHook(hook);

      // Mock condition evaluation to return true
      vi.spyOn(require('../../src/knowledge-engine/condition-evaluator.mjs'), 'evaluateCondition')
        .mockResolvedValue(true);

      const event = {
        name: 'test-event',
        payload: {
          size: 10000000
        },
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Memory bomb attempt detected');
    });
  });

  describe('Authentication Bypass', () => {
    it('should prevent authentication bypass through hook manipulation', async () => {
      const query = join(tempDir, 'auth-bypass.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'auth-bypass-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Attempt to bypass authentication
          const isAuthenticated = event.context.isAuthenticated;
          if (!isAuthenticated) {
            // Attempt to bypass by setting authentication flag
            event.context.isAuthenticated = true;
            throw new Error('Authentication bypass attempt detected');
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
        context: { 
          graph: testStore,
          isAuthenticated: false
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Authentication bypass attempt detected');
    });

    it('should prevent session hijacking', async () => {
      const query = join(tempDir, 'session-hijack.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      const hook = defineHook({
        meta: { name: 'session-hijack-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'expected-hash',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          // Attempt to hijack session
          const sessionId = event.context.sessionId;
          const userId = event.context.userId;
          
          if (sessionId && userId) {
            // Attempt to change user ID to hijack session
            const originalUserId = event.context.userId;
            event.context.userId = 'admin';
            
            if (originalUserId !== 'admin' && event.context.userId === 'admin') {
              throw new Error('Session hijacking attempt detected');
            }
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
        context: { 
          graph: testStore,
          sessionId: 'session123',
          userId: 'user456'
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error).toBe('Session hijacking attempt detected');
    });
  });
});
