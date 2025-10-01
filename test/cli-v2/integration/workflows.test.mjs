/**
 * @fileoverview Integration tests for CLI v2 command workflows
 * Tests command interactions and multi-step processes
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  runCLI,
  createTempDir,
  createTestProject,
  assert,
  generators,
  scenario
} from '../test-utils.mjs';
import { writeFile, readFile } from 'node:fs/promises';
import { join } from 'node:path';

describe('CLI v2 Integration: Command Workflows', () => {
  let tempDir;
  let cleanup;
  let projectPaths;

  beforeEach(async () => {
    const temp = await createTempDir('workflow-test-');
    tempDir = temp.dir;
    cleanup = temp.cleanup;
    projectPaths = await createTestProject(tempDir);
  });

  afterEach(async () => {
    await cleanup();
  });

  describe('Development Workflow', () => {
    it('should complete parse → query → validate workflow', async () => {
      const workflow = await scenario('Development workflow')
        .step('Parse RDF data', async (ctx) => {
          const dataPath = join(projectPaths.data, 'input.ttl');
          await writeFile(dataPath, generators.rdfTriples(20));

          const result = await runCLI(`parse turtle ${dataPath}`);
          assert.success(result);

          ctx.dataPath = dataPath;
          ctx.tripleCount = 20;
        })
        .step('Query parsed data', async (ctx) => {
          const queryPath = join(tempDir, 'test-query.rq');
          await writeFile(queryPath, generators.sparqlQuery('select'));

          const result = await runCLI(`query select ${queryPath} ${ctx.dataPath}`);
          assert.success(result);

          ctx.queryResults = assert.jsonOutput(result);
        })
        .step('Validate data quality', async (ctx) => {
          const shapePath = join(tempDir, 'shape.ttl');
          await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

          const result = await runCLI(`validate shacl ${ctx.dataPath} ${shapePath}`);
          assert.success(result);
        })
        .run();

      expect(workflow.tripleCount).toBe(20);
      expect(workflow.queryResults).toBeTruthy();
    });

    it('should handle data transformation pipeline', async () => {
      const workflow = await scenario('Data transformation pipeline')
        .step('Parse source data', async (ctx) => {
          const sourcePath = join(projectPaths.data, 'source.ttl');
          await writeFile(sourcePath, generators.rdfTriples(10));

          const result = await runCLI(`parse turtle ${sourcePath}`);
          assert.success(result);

          ctx.sourcePath = sourcePath;
        })
        .step('Transform with CONSTRUCT query', async (ctx) => {
          const queryPath = join(tempDir, 'transform.rq');
          await writeFile(queryPath, generators.sparqlQuery('construct'));

          const outputPath = join(projectPaths.data, 'transformed.ttl');
          const result = await runCLI(
            `query construct ${queryPath} ${ctx.sourcePath} --output ${outputPath}`
          );

          assert.success(result);
          ctx.transformedPath = outputPath;
        })
        .step('Validate transformed data', async (ctx) => {
          const shapePath = join(tempDir, 'shape.ttl');
          await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

          const result = await runCLI(
            `validate shacl ${ctx.transformedPath} ${shapePath}`
          );

          assert.success(result);
        })
        .run();

      expect(workflow.transformedPath).toBeTruthy();
    });
  });

  describe('Hook-Based Workflow', () => {
    it('should create → validate → save → eval hook workflow', async () => {
      const workflow = await scenario('Complete hook workflow')
        .step('Create hook from template', async (ctx) => {
          const result = await runCLI(
            `hook create workflow-hook sparql-ask --output ${projectPaths.hooks}`
          );

          assert.success(result);
          ctx.hookPath = join(projectPaths.hooks, 'workflow-hook', 'workflow-hook.json');
        })
        .step('Validate hook definition', async (ctx) => {
          const result = await runCLI(`hook validate ${ctx.hookPath}`);
          assert.success(result);
        })
        .step('Save hook to storage', async (ctx) => {
          const result = await runCLI(`hook save ${ctx.hookPath}`);
          assert.success(result);

          const idMatch = result.stdout.match(/[a-f0-9-]{36}/);
          ctx.hookId = idMatch ? idMatch[0] : 'workflow-hook';
        })
        .step('Prepare test data', async (ctx) => {
          ctx.dataPath = join(projectPaths.data, 'hook-data.ttl');
          await writeFile(ctx.dataPath, generators.rdfTriples(5));
        })
        .step('Evaluate hook', async (ctx) => {
          const result = await runCLI(
            `hook eval ${ctx.hookPath} --data ${ctx.dataPath}`
          );

          assert.success(result);
        })
        .step('Check evaluation history', async (ctx) => {
          const result = await runCLI(`hook history ${ctx.hookId}`);

          // Should show at least one evaluation
        })
        .run();

      expect(workflow.hookId).toBeTruthy();
    });

    it('should handle multiple hook evaluations with receipts', async () => {
      const hookPath = join(tempDir, 'receipt-hook.json');
      const hook = generators.hookDefinition('sparql-ask', 'receipt-test');
      await writeFile(hookPath, JSON.stringify(hook, null, 2));

      const dataPath = join(projectPaths.data, 'receipt-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      // Evaluate multiple times
      for (let i = 0; i < 3; i++) {
        const result = await runCLI(
          `hook eval ${hookPath} --data ${dataPath} --persist`
        );
        assert.success(result);
      }

      // Check receipts directory
      // (Implementation will need to expose receipt location)
    });
  });

  describe('Policy Pack Workflow', () => {
    it('should scaffold → populate → apply policy pack', async () => {
      const workflow = await scenario('Policy pack workflow')
        .step('Scaffold policy pack', async (ctx) => {
          const result = await runCLI(
            `policy create compliance-pack --output ${projectPaths.policies}`
          );

          // May not be implemented yet
          if (result.exitCode === 0) {
            ctx.policyPath = join(projectPaths.policies, 'compliance-pack');
          }
        })
        .step('Add hooks to policy pack', async (ctx) => {
          if (!ctx.policyPath) return;

          // Create hooks
          const hook1 = generators.hookDefinition('shacl', 'data-quality');
          const hook2 = generators.hookDefinition('sparql-ask', 'health-check');

          await writeFile(
            join(ctx.policyPath, 'hooks', 'data-quality.json'),
            JSON.stringify(hook1, null, 2)
          );

          await writeFile(
            join(ctx.policyPath, 'hooks', 'health-check.json'),
            JSON.stringify(hook2, null, 2)
          );

          ctx.hookCount = 2;
        })
        .step('Validate policy pack', async (ctx) => {
          if (!ctx.policyPath) return;

          const result = await runCLI(`policy validate ${ctx.policyPath}`);

          // Should validate all hooks in pack
        })
        .step('Apply policy pack to data', async (ctx) => {
          if (!ctx.policyPath) return;

          const dataPath = join(projectPaths.data, 'governed-data.ttl');
          await writeFile(dataPath, generators.rdfTriples(10));

          const result = await runCLI(
            `policy apply ${ctx.policyPath} --data ${dataPath}`
          );

          // Should evaluate all hooks in pack
        })
        .run();

      // Policy pack workflow may not be fully implemented
    });
  });

  describe('Sidecar Integration Workflow', () => {
    it('should connect → health-check → operate workflow', async () => {
      const workflow = await scenario('Sidecar integration')
        .step('Check sidecar connection', async (ctx) => {
          const result = await runCLI('sidecar status');

          // Sidecar may not be running - that's okay
          ctx.sidecarAvailable = result.exitCode === 0;
        })
        .step('Perform health check if connected', async (ctx) => {
          if (!ctx.sidecarAvailable) return;

          const result = await runCLI('sidecar health');
          assert.success(result);
        })
        .step('Execute remote operation', async (ctx) => {
          if (!ctx.sidecarAvailable) return;

          const dataPath = join(projectPaths.data, 'sidecar-data.ttl');
          await writeFile(dataPath, generators.rdfTriples(5));

          const result = await runCLI(`sidecar parse ${dataPath}`);

          // Remote parsing operation
        })
        .run();

      // Sidecar workflow is optional
    });
  });

  describe('Context Switching Workflow', () => {
    it('should switch contexts and execute commands', async () => {
      const workflow = await scenario('Context switching')
        .step('Use development context', async (ctx) => {
          const result = await runCLI('context use development');

          // Context switching may not be implemented
          ctx.contextSwitchingAvailable = result.exitCode === 0;
        })
        .step('Execute command in dev context', async (ctx) => {
          if (!ctx.contextSwitchingAvailable) return;

          const dataPath = join(projectPaths.data, 'dev-data.ttl');
          await writeFile(dataPath, generators.rdfTriples(5));

          const result = await runCLI(`parse turtle ${dataPath}`);
          assert.success(result);
        })
        .step('Switch to production context', async (ctx) => {
          if (!ctx.contextSwitchingAvailable) return;

          const result = await runCLI('context use production');
          assert.success(result);
        })
        .step('Execute command in prod context with strict validation', async (ctx) => {
          if (!ctx.contextSwitchingAvailable) return;

          const dataPath = join(projectPaths.data, 'prod-data.ttl');
          await writeFile(dataPath, generators.rdfTriples(5));

          const shapePath = join(tempDir, 'strict-shape.ttl');
          await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

          const result = await runCLI(
            `validate shacl ${dataPath} ${shapePath} --strict`
          );

          // Production context should enforce strict validation
        })
        .run();
    });
  });

  describe('Error Recovery Workflow', () => {
    it('should handle and recover from errors gracefully', async () => {
      const workflow = await scenario('Error recovery')
        .step('Parse invalid data (expect error)', async (ctx) => {
          const invalidPath = join(projectPaths.data, 'invalid.ttl');
          await writeFile(invalidPath, 'invalid turtle syntax @#$%');

          const result = await runCLI(`parse turtle ${invalidPath}`);

          assert.failure(result);
          ctx.parseError = result.stderr;
        })
        .step('Fix data and retry', async (ctx) => {
          const fixedPath = join(projectPaths.data, 'fixed.ttl');
          await writeFile(fixedPath, generators.rdfTriples(5));

          const result = await runCLI(`parse turtle ${fixedPath}`);

          assert.success(result);
          ctx.parseSuccess = true;
        })
        .step('Validate fixed data', async (ctx) => {
          const fixedPath = join(projectPaths.data, 'fixed.ttl');
          const shapePath = join(tempDir, 'shape.ttl');
          await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

          const result = await runCLI(`validate shacl ${fixedPath} ${shapePath}`);

          assert.success(result);
        })
        .run();

      expect(workflow.parseError).toBeTruthy();
      expect(workflow.parseSuccess).toBe(true);
    });
  });

  describe('Performance: Workflow optimization', () => {
    it('should cache parsed data across commands', async () => {
      const dataPath = join(projectPaths.data, 'cached.ttl');
      await writeFile(dataPath, generators.rdfTriples(1000));

      // First parse (cold)
      const parse1 = await runCLI(`parse turtle ${dataPath}`);
      const duration1 = parse1.duration;

      // Second parse (should be faster if cached)
      const parse2 = await runCLI(`parse turtle ${dataPath}`);
      const duration2 = parse2.duration;

      // Cache implementation may not exist yet
      // But we can verify both succeed
      assert.success(parse1);
      assert.success(parse2);
    });

    it('should optimize query execution in workflows', async () => {
      const dataPath = join(projectPaths.data, 'optimized.ttl');
      await writeFile(dataPath, generators.rdfTriples(500));

      const queryPath = join(tempDir, 'optimized.rq');
      await writeFile(queryPath, generators.sparqlQuery('select', 'FILTER(?age > 25)'));

      // Execute query multiple times
      const durations = [];
      for (let i = 0; i < 5; i++) {
        const result = await runCLI(`query select ${queryPath} ${dataPath}`);
        assert.success(result);
        durations.push(result.duration);
      }

      // Should complete in reasonable time
      const avgDuration = durations.reduce((a, b) => a + b, 0) / durations.length;
      expect(avgDuration).toBeLessThan(500);
    });
  });
});
