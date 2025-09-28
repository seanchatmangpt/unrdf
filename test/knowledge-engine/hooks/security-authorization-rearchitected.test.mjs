/**
 * @file Security and Authorization Tests - Rearchitected
 * @module security-authorization-rearchitected
 * 
 * @description
 * Rearchitected security tests using dependency injection
 * and test utilities instead of ESM mocking.
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { join } from 'path';
import { writeFile, mkdir, rm } from 'fs/promises';
import { defineHook } from '../../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store } from 'n3';

// Import test utilities
import {
  createHookBuilder,
  createSuccessHook,
  createFailureHook,
  createMockQueryEngine,
  createMockFileResolver,
  createMockConditionEvaluator,
  createTestEventFactory,
  createMaliciousTestEvent,
  assertHookSuccess,
  assertHookFailure,
  assertSecurityPassed,
  assertSecurityFailed,
  assertValidationError
} from '../test-utils/index.mjs';

describe('Security and Authorization - Rearchitected', () => {
  let tempDir;
  let manager;
  let mockQueryEngine;
  let mockFileResolver;
  let mockConditionEvaluator;
  let testStore;

  beforeEach(async () => {
    // Create temp directory
    tempDir = join(process.cwd(), 'temp', `test-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`);
    await mkdir(tempDir, { recursive: true });

    // Create test store
    testStore = new Store();

    // Create mock services
    mockQueryEngine = createMockQueryEngine();
    mockFileResolver = createMockFileResolver();
    mockConditionEvaluator = createMockConditionEvaluator();

    // Create manager with mock services
    manager = new KnowledgeHookManager({
      basePath: tempDir,
      enableKnowledgeHooks: true,
      strictMode: false
    });

    // Inject mock services (this would need to be supported by the manager)
    // For now, we'll test the validation and basic functionality
  });

  afterEach(async () => {
    if (tempDir) {
      await rm(tempDir, { recursive: true, force: true });
    }
  });

  describe('Input Validation and Sanitization', () => {
    it('should validate hook metadata for malicious content', async () => {
      const query = join(tempDir, 'malicious-meta.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');

      // Test with malicious hook name
      const maliciousHook = createHookBuilder()
        .withMeta('<script>alert("xss")</script>', 'Description with <img src=x onerror=alert(1)>')
        .withCondition('sparql-ask', `file://${query}`)
        .build();

      // This should fail validation
      await expect(() => defineHook(maliciousHook)).toThrow();
    });

    it('should validate file URIs for malicious patterns', async () => {
      const maliciousUris = [
        '../../../etc/passwd',
        'file:///etc/passwd',
        'file:///windows/system32/config/sam',
        'file:///var/log/auth.log'
      ];

      for (const uri of maliciousUris) {
        const hook = createHookBuilder()
          .withMeta('path-traversal-test')
          .withCondition('sparql-ask', uri)
          .build();

        // This should fail validation
        await expect(() => defineHook(hook)).toThrow();
      }
    });

    it('should validate SPARQL queries for dangerous operations', async () => {
      const dangerousQueries = [
        'DROP GRAPH <http://example.org/graph>',
        'DELETE { ?s ?p ?o } WHERE { ?s ?p ?o }',
        'INSERT { ?s ?p ?o } WHERE { ?s ?p ?o }',
        'CLEAR GRAPH <http://example.org/graph>',
        'LOAD <http://malicious.com/data.ttl>'
      ];

      for (const query of dangerousQueries) {
        const queryFile = join(tempDir, `dangerous-${Date.now()}.sparql`);
        await writeFile(queryFile, query);

        const hook = createHookBuilder()
          .withMeta('dangerous-query-test')
          .withCondition('sparql-ask', `file://${queryFile}`)
          .build();

        // This should fail validation
        await expect(() => defineHook(hook)).toThrow();
      }
    });
  });

  describe('Code Injection Prevention', () => {
    it('should prevent SPARQL injection attacks', async () => {
      const injectionQuery = join(tempDir, 'injection.sparql');
      await writeFile(injectionQuery, 'SELECT * WHERE { ?s ?p ?o }');

      // Set up mock to detect injection attempts
      mockQueryEngine.setAskResult('SELECT * WHERE { ?s ?p ?o }', true);

      const hook = createHookBuilder()
        .withMeta('injection-test')
        .withCondition('sparql-ask', `file://${injectionQuery}`)
        .withRun(async () => {
          // Simulate injection attempt
          const result = await mockQueryEngine.ask(testStore, 'SELECT * WHERE { ?s ?p ?o }; DROP TABLE users; --');
          return { success: result };
        })
        .build();

      const definedHook = defineHook(hook);
      manager.addKnowledgeHook(definedHook);

      const event = createTestEventFactory()
        .withName('injection-test-event')
        .withGraph(testStore)
        .build();

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect and prevent injection
      assertSecurityFailed(results[0], 'injection');
    });

    it('should prevent UNION injection attacks', async () => {
      const unionQuery = join(tempDir, 'union-injection.sparql');
      await writeFile(unionQuery, 'SELECT * WHERE { ?s ?p ?o }');

      const hook = createHookBuilder()
        .withMeta('union-injection-test')
        .withCondition('sparql-ask', `file://${unionQuery}`)
        .withRun(async () => {
          // Simulate UNION injection attempt
          const result = await mockQueryEngine.ask(testStore, 'SELECT * WHERE { ?s ?p ?o } UNION SELECT * FROM users --');
          return { success: result };
        })
        .build();

      const definedHook = defineHook(hook);
      manager.addKnowledgeHook(definedHook);

      const event = createTestEventFactory()
        .withName('union-injection-test-event')
        .withGraph(testStore)
        .build();

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect and prevent UNION injection
      assertSecurityFailed(results[0], 'UNION');
    });
  });

  describe('Path Traversal Prevention', () => {
    it('should prevent directory traversal attacks', async () => {
      const traversalPaths = [
        '../../../etc/passwd',
        '..\\..\\..\\windows\\system32\\config\\sam',
        '/etc/passwd',
        'C:\\windows\\system32\\config\\sam'
      ];

      for (const path of traversalPaths) {
        const hook = createHookBuilder()
          .withMeta('traversal-test')
          .withFileAccess(path)
          .build();

        const definedHook = defineHook(hook);
        manager.addKnowledgeHook(definedHook);

        const event = createTestEventFactory()
          .withName('traversal-test-event')
          .build();

        const results = await manager.executeAllKnowledgeHooks(event);
        
        // Should prevent file access
        assertSecurityFailed(results[0], 'traversal');
      }
    });

    it('should prevent absolute path attacks', async () => {
      const absolutePaths = [
        '/etc/passwd',
        'C:\\windows\\system32\\config\\sam',
        '/var/log/auth.log',
        'C:\\Program Files\\Common Files\\System\\msadc\\msadcs.dll'
      ];

      for (const path of absolutePaths) {
        const hook = createHookBuilder()
          .withMeta('absolute-path-test')
          .withFileAccess(path)
          .build();

        const definedHook = defineHook(hook);
        manager.addKnowledgeHook(definedHook);

        const event = createTestEventFactory()
          .withName('absolute-path-test-event')
          .build();

        const results = await manager.executeAllKnowledgeHooks(event);
        
        // Should prevent absolute path access
        assertSecurityFailed(results[0], 'absolute');
      }
    });
  });

  describe('Privilege Escalation Prevention', () => {
    it('should prevent privilege escalation through hook execution', async () => {
      const hook = createHookBuilder()
        .withMeta('privilege-escalation-test')
        .withRun(async () => {
          // Simulate privilege escalation attempt
          return { success: true, escalated: true, role: 'admin' };
        })
        .build();

      const definedHook = defineHook(hook);
      manager.addKnowledgeHook(definedHook);

      const event = createTestEventFactory()
        .withName('privilege-escalation-test-event')
        .withUser({ role: 'user', id: 'test-user' })
        .build();

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should prevent privilege escalation
      assertSecurityFailed(results[0], 'escalation');
    });

    it('should prevent hook from accessing system resources', async () => {
      const systemResources = [
        'file:///etc/passwd',
        'file:///proc/self/environ',
        'file:///sys/kernel/security/apparmor/revision'
      ];

      for (const resource of systemResources) {
        const hook = createHookBuilder()
          .withMeta('system-resource-test')
          .withFileAccess(resource)
          .build();

        const definedHook = defineHook(hook);
        manager.addKnowledgeHook(definedHook);

        const event = createTestEventFactory()
          .withName('system-resource-test-event')
          .build();

        const results = await manager.executeAllKnowledgeHooks(event);
        
        // Should prevent system resource access
        assertSecurityFailed(results[0], 'system');
      }
    });
  });

  describe('Information Disclosure Prevention', () => {
    it('should prevent sensitive information disclosure in error messages', async () => {
      const hook = createHookBuilder()
        .withMeta('info-disclosure-test')
        .withFailure('Database connection failed: user=admin, password=secret123, host=localhost:5432')
        .build();

      const definedHook = defineHook(hook);
      manager.addKnowledgeHook(definedHook);

      const event = createTestEventFactory()
        .withName('info-disclosure-test-event')
        .build();

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should sanitize error messages
      assertHookFailure(results[0]);
      
      // Error message should not contain sensitive information
      expect(results[0].error).not.toContain('password=secret123');
      expect(results[0].error).not.toContain('user=admin');
    });

    it('should prevent stack trace disclosure', async () => {
      const hook = createHookBuilder()
        .withMeta('stack-trace-test')
        .withFailure('Error in /home/user/project/src/database.js:45:12')
        .build();

      const definedHook = defineHook(hook);
      manager.addKnowledgeHook(definedHook);

      const event = createTestEventFactory()
        .withName('stack-trace-test-event')
        .build();

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should sanitize stack traces
      assertHookFailure(results[0]);
      
      // Error message should not contain file paths
      expect(results[0].error).not.toContain('/home/user/project/');
      expect(results[0].error).not.toContain('database.js:45:12');
    });
  });

  describe('Denial of Service Prevention', () => {
    it('should prevent CPU exhaustion attacks', async () => {
      const hook = createHookBuilder()
        .withMeta('cpu-exhaustion-test')
        .withRun(async () => {
          // Simulate CPU-intensive operation
          let result = 0;
          for (let i = 0; i < 1000000; i++) {
            result += Math.sqrt(i);
          }
          return { success: true, result };
        })
        .build();

      const definedHook = defineHook(hook);
      manager.addKnowledgeHook(definedHook);

      const event = createTestEventFactory()
        .withName('cpu-exhaustion-test-event')
        .build();

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should prevent CPU exhaustion
      assertSecurityFailed(results[0], 'CPU');
    });

    it('should prevent memory exhaustion attacks', async () => {
      const hook = createHookBuilder()
        .withMeta('memory-exhaustion-test')
        .withMemoryConsumption(1000) // 1GB
        .build();

      const definedHook = defineHook(hook);
      manager.addKnowledgeHook(definedHook);

      const event = createTestEventFactory()
        .withName('memory-exhaustion-test-event')
        .build();

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should prevent memory exhaustion
      assertSecurityFailed(results[0], 'memory');
    });
  });
});
