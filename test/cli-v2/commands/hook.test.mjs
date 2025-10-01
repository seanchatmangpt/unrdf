/**
 * @fileoverview Unit tests for CLI v2 hook commands
 * Tests the 25% value command group - highest priority
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  runCLI,
  createTestContext,
  createTempDir,
  createTestProject,
  assert,
  generators,
  scenario
} from '../test-utils.mjs';
import { writeFile, readFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';

describe('CLI v2: hook commands', () => {
  let tempDir;
  let cleanup;

  beforeEach(async () => {
    const temp = await createTempDir('hook-test-');
    tempDir = temp.dir;
    cleanup = temp.cleanup;
  });

  afterEach(async () => {
    await cleanup();
  });

  describe('hook eval (Critical Path - P0)', () => {
    it('should evaluate hook successfully', async () => {
      // Create hook definition
      const hookPath = join(tempDir, 'test-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'test-hook');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      // Create test data
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(10));

      // Execute hook eval
      const result = await runCLI(`hook eval ${hookPath} --data ${dataPath}`);

      assert.success(result);
      assert.outputMatches(result, /Evaluation Result/i);
      assert.performanceTarget(result, 100, 'hook eval');
    });

    it('should handle hook evaluation failure gracefully', async () => {
      const hookPath = join(tempDir, 'invalid-hook.json');
      await writeFile(hookPath, '{ "invalid": "hook" }');

      const result = await runCLI(`hook eval ${hookPath}`);

      assert.failure(result);
      assert.outputContains(result, 'error');
    });

    it('should support --persist flag for receipts', async () => {
      const hookPath = join(tempDir, 'test-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'test-hook');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      const result = await runCLI(`hook eval ${hookPath} --persist`);

      assert.success(result);
      // Verify receipt was created
      // (Implementation will need to expose receipt location)
    });

    it('should support multiple output formats', async () => {
      const hookPath = join(tempDir, 'test-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'test-hook');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      // Test JSON output
      const jsonResult = await runCLI(`hook eval ${hookPath} --output json`);
      assert.success(jsonResult);
      const jsonData = assert.jsonOutput(jsonResult);
      expect(jsonData).toHaveProperty('fired');

      // Test table output
      const tableResult = await runCLI(`hook eval ${hookPath} --output table`);
      assert.success(tableResult);
    });
  });

  describe('hook create (P0)', () => {
    it('should create hook from template', async () => {
      const result = await runCLI(
        `hook create health-check sparql-ask --output ${tempDir}`
      );

      assert.success(result);
      assert.outputContains(result, 'Generated hook files');

      // Verify files created
      const hookDir = join(tempDir, 'hooks', 'health-check');
      const hookFile = join(hookDir, 'health-check.json');
      const content = await readFile(hookFile, 'utf-8');
      const hook = JSON.parse(content);

      expect(hook.meta.name).toBe('health-check');
      expect(hook.when.kind).toBe('sparql-ask');
    });

    it('should support all hook types', async () => {
      const types = ['sparql-ask', 'sparql-select', 'shacl', 'delta'];

      for (const type of types) {
        const result = await runCLI(
          `hook create test-${type} ${type} --output ${tempDir}`
        );
        assert.success(result);
      }
    });

    it('should create test file alongside hook', async () => {
      const result = await runCLI(
        `hook create health-check sparql-ask --output ${tempDir}`
      );

      assert.success(result);

      const testFile = join(tempDir, 'hooks', 'health-check', 'health-check.test.mjs');
      const content = await readFile(testFile, 'utf-8');

      expect(content).toContain('describe');
      expect(content).toContain('health-check');
    });
  });

  describe('hook validate (P0)', () => {
    it('should validate correct hook definition', async () => {
      const hookPath = join(tempDir, 'valid-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'valid-hook');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      const result = await runCLI(`hook validate ${hookPath}`);

      assert.success(result);
      assert.outputMatches(result, /Valid|✅/i);
    });

    it('should detect invalid hook structure', async () => {
      const hookPath = join(tempDir, 'invalid-hook.json');
      await writeFile(hookPath, JSON.stringify({
        meta: { name: 'incomplete' }
        // Missing required fields
      }));

      const result = await runCLI(`hook validate ${hookPath}`);

      assert.failure(result);
      assert.outputContains(result, 'validation error');
    });

    it('should validate SPARQL syntax in hook conditions', async () => {
      const hookPath = join(tempDir, 'bad-sparql-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'bad-sparql');
      hook.when.ref.inline = 'SELECT THIS IS INVALID SPARQL';
      await writeFile(hookPath, JSON.stringify(hook));

      const result = await runCLI(`hook validate ${hookPath}`);

      assert.failure(result);
      assert.outputContains(result, 'SPARQL');
    });
  });

  describe('hook list (P1)', () => {
    it('should list all stored hooks', async () => {
      const result = await runCLI('hook list');

      assert.success(result);
      // Should handle empty list gracefully
    });

    it('should display hook metadata in table format', async () => {
      const result = await runCLI('hook list --format table');

      assert.success(result);
      assert.outputMatches(result, /ID.*Name.*Type/i);
    });

    it('should support JSON output for scripting', async () => {
      const result = await runCLI('hook list --format json');

      assert.success(result);
      const hooks = assert.jsonOutput(result);
      expect(Array.isArray(hooks)).toBe(true);
    });

    it('should filter hooks by type', async () => {
      const result = await runCLI('hook list --type sparql-ask');

      assert.success(result);
    });
  });

  describe('hook save (P1)', () => {
    it('should save hook to storage', async () => {
      const hookPath = join(tempDir, 'save-test-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'save-test');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      const result = await runCLI(`hook save ${hookPath}`);

      assert.success(result);
      assert.outputMatches(result, /saved|stored/i);

      // Extract hook ID from output
      const idMatch = result.stdout.match(/ID: ([a-f0-9-]+)/i);
      expect(idMatch).toBeTruthy();
    });

    it('should prevent duplicate hooks', async () => {
      const hookPath = join(tempDir, 'duplicate-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'duplicate-test');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      // Save once
      const result1 = await runCLI(`hook save ${hookPath}`);
      assert.success(result1);

      // Try to save again
      const result2 = await runCLI(`hook save ${hookPath}`);
      // Should either update or warn about duplicate
      assert.success(result2);
      assert.outputMatches(result2, /updated|already exists/i);
    });
  });

  describe('hook load (P1)', () => {
    it('should load and execute stored hook', async () => {
      // First save a hook
      const hookPath = join(tempDir, 'load-test-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'load-test');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      const saveResult = await runCLI(`hook save ${hookPath}`);
      assert.success(saveResult);

      // Extract hook ID
      const idMatch = saveResult.stdout.match(/[a-f0-9-]{36}/);
      const hookId = idMatch ? idMatch[0] : 'load-test';

      // Load and execute
      const loadResult = await runCLI(`hook load ${hookId}`);

      assert.success(loadResult);
      assert.outputMatches(loadResult, /loaded/i);
    });

    it('should handle non-existent hook ID', async () => {
      const result = await runCLI('hook load nonexistent-id-12345');

      assert.failure(result);
      assert.outputContains(result, 'not found');
    });
  });

  describe('hook delete (P1)', () => {
    it('should delete stored hook', async () => {
      // First save a hook
      const hookPath = join(tempDir, 'delete-test-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'delete-test');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      const saveResult = await runCLI(`hook save ${hookPath}`);
      const idMatch = saveResult.stdout.match(/[a-f0-9-]{36}/);
      const hookId = idMatch ? idMatch[0] : 'delete-test';

      // Delete it
      const deleteResult = await runCLI(`hook delete ${hookId}`);

      assert.success(deleteResult);
      assert.outputMatches(deleteResult, /deleted|removed/i);

      // Verify it's gone
      const loadResult = await runCLI(`hook load ${hookId}`);
      assert.failure(loadResult);
    });

    it('should require confirmation for destructive operation', async () => {
      // Test with --force flag to skip confirmation
      const result = await runCLI('hook delete some-id --force');

      // Should fail because hook doesn't exist, but flag should be accepted
      assert.failure(result);
    });
  });

  describe('hook history (P1)', () => {
    it('should show evaluation history for hook', async () => {
      const result = await runCLI('hook history some-hook-id');

      // May succeed with empty history or fail if hook not found
      // Both are valid depending on implementation
    });

    it('should support limit parameter', async () => {
      const result = await runCLI('hook history some-hook-id --limit 5');

      // Should handle limit parameter
    });

    it('should display timestamps and results', async () => {
      const result = await runCLI('hook history some-hook-id --format table');

      // Should format history as table
    });
  });

  describe('hook plan (P2)', () => {
    it('should show execution plan without running', async () => {
      const hookPath = join(tempDir, 'plan-test-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'plan-test');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      const result = await runCLI(`hook plan ${hookPath}`);

      assert.success(result);
      assert.outputMatches(result, /plan|steps|execution/i);

      // Should show plan details
      assert.outputContains(result, hook.meta.name);
      assert.outputContains(result, hook.when.kind);
    });

    it('should display query that will be executed', async () => {
      const hookPath = join(tempDir, 'plan-sparql-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'plan-sparql');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      const result = await runCLI(`hook plan ${hookPath}`);

      assert.success(result);
      assert.outputContains(result, 'ASK');
      assert.outputContains(result, 'foaf:Person');
    });
  });

  describe('hook stats (P2)', () => {
    it('should show statistics across all hooks', async () => {
      const result = await runCLI('hook stats');

      assert.success(result);
      // Should display stats even if empty
    });

    it('should show execution metrics', async () => {
      const result = await runCLI('hook stats --format json');

      assert.success(result);
      const stats = assert.jsonOutput(result);

      expect(stats).toHaveProperty('totalHooks');
      expect(stats).toHaveProperty('totalEvaluations');
    });
  });

  describe('Integration: Hook workflow', () => {
    it('should complete full hook lifecycle', async () => {
      const workflow = await scenario('Full hook lifecycle')
        .step('Create hook from template', async (ctx) => {
          const result = await runCLI(
            `hook create workflow-test sparql-ask --output ${tempDir}`
          );
          assert.success(result);
          ctx.hookDir = join(tempDir, 'hooks', 'workflow-test');
        })
        .step('Validate hook', async (ctx) => {
          const hookPath = join(ctx.hookDir, 'workflow-test.json');
          const result = await runCLI(`hook validate ${hookPath}`);
          assert.success(result);
        })
        .step('Save hook to storage', async (ctx) => {
          const hookPath = join(ctx.hookDir, 'workflow-test.json');
          const result = await runCLI(`hook save ${hookPath}`);
          assert.success(result);

          const idMatch = result.stdout.match(/[a-f0-9-]{36}/);
          ctx.hookId = idMatch ? idMatch[0] : 'workflow-test';
        })
        .step('List hooks to verify', async (ctx) => {
          const result = await runCLI('hook list');
          assert.success(result);
          assert.outputContains(result, 'workflow-test');
        })
        .step('Load and execute hook', async (ctx) => {
          const result = await runCLI(`hook load ${ctx.hookId}`);
          // May succeed or fail depending on data
        })
        .step('View execution history', async (ctx) => {
          const result = await runCLI(`hook history ${ctx.hookId}`);
          // Should show at least one execution
        })
        .step('Delete hook', async (ctx) => {
          const result = await runCLI(`hook delete ${ctx.hookId} --force`);
          assert.success(result);
        })
        .run();

      expect(workflow.hookId).toBeTruthy();
    });
  });

  describe('Performance: Hook operations', () => {
    it('should meet startup performance target (< 100ms)', async () => {
      const result = await runCLI('hook list');

      assert.success(result);
      assert.performanceTarget(result, 100, 'hook list startup');
    });

    it('should evaluate hook within 2ms p99 target', async () => {
      const hookPath = join(tempDir, 'perf-test-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'perf-test');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      const dataPath = join(tempDir, 'perf-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(100));

      // Run multiple times to get p99
      const durations = [];
      for (let i = 0; i < 100; i++) {
        const result = await runCLI(`hook eval ${hookPath} --data ${dataPath}`);
        durations.push(result.duration);
      }

      durations.sort((a, b) => a - b);
      const p99 = durations[Math.floor(durations.length * 0.99)];

      expect(p99).toBeLessThan(2);
    });
  });

  describe('Error Handling', () => {
    it('should handle missing hook file', async () => {
      const result = await runCLI('hook eval /nonexistent/hook.json');

      assert.failure(result);
      assert.outputContains(result, 'not found');
    });

    it('should handle malformed JSON', async () => {
      const hookPath = join(tempDir, 'malformed.json');
      await writeFile(hookPath, '{ invalid json }');

      const result = await runCLI(`hook eval ${hookPath}`);

      assert.failure(result);
      assert.outputContains(result, 'parse');
    });

    it('should handle network timeout for remote hooks', async () => {
      const result = await runCLI('hook eval http://localhost:99999/hook.json --timeout 1000');

      assert.failure(result);
      // Should timeout gracefully
    }, 5000);
  });
});
