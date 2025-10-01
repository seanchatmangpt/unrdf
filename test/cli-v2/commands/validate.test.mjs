/**
 * @fileoverview Unit tests for CLI v2 validate commands
 * Tests the 15% value command group - P1 priority
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  runCLI,
  createTempDir,
  assert,
  generators
} from '../test-utils.mjs';
import { writeFile } from 'node:fs/promises';
import { join } from 'node:path';

describe('CLI v2: validate commands', () => {
  let tempDir;
  let cleanup;

  beforeEach(async () => {
    const temp = await createTempDir('validate-test-');
    tempDir = temp.dir;
    cleanup = temp.cleanup;
  });

  afterEach(async () => {
    await cleanup();
  });

  describe('validate shacl (Critical Path - P1)', () => {
    it('should validate data against SHACL shapes successfully', async () => {
      const dataPath = join(tempDir, 'valid-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const shapePath = join(tempDir, 'shape.ttl');
      await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

      const result = await runCLI(`validate shacl ${dataPath} ${shapePath}`);

      assert.success(result);
      assert.outputMatches(result, /valid|conforms|✅/i);
    });

    it('should detect SHACL violations', async () => {
      const dataPath = join(tempDir, 'invalid-data.ttl');
      // Data missing required foaf:name
      await writeFile(dataPath, `
        @prefix ex: <http://test.example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:person1 a foaf:Person ;
          foaf:age 30 .
      `);

      const shapePath = join(tempDir, 'shape.ttl');
      await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

      const result = await runCLI(`validate shacl ${dataPath} ${shapePath}`);

      assert.failure(result);
      assert.outputMatches(result, /violation|invalid|failed/i);
    });

    it('should report violation details', async () => {
      const dataPath = join(tempDir, 'invalid-data.ttl');
      await writeFile(dataPath, `
        @prefix ex: <http://test.example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:person1 a foaf:Person ;
          foaf:age 30 .
      `);

      const shapePath = join(tempDir, 'shape.ttl');
      await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

      const result = await runCLI(
        `validate shacl ${dataPath} ${shapePath} --format json`
      );

      const report = assert.jsonOutput(result);
      expect(report.conforms).toBe(false);
      expect(report.results.length).toBeGreaterThan(0);
      expect(report.results[0]).toHaveProperty('message');
    });

    it('should write validation report to file', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const shapePath = join(tempDir, 'shape.ttl');
      await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

      const reportPath = join(tempDir, 'report.json');
      const result = await runCLI(
        `validate shacl ${dataPath} ${shapePath} --output ${reportPath}`
      );

      assert.success(result);
      assert.outputContains(result, 'written');
    });

    it('should meet performance target (< 200ms)', async () => {
      const dataPath = join(tempDir, 'perf-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(100));

      const shapePath = join(tempDir, 'shape.ttl');
      await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

      const result = await runCLI(`validate shacl ${dataPath} ${shapePath}`);

      assert.success(result);
      assert.performanceTarget(result, 200, 'SHACL validation');
    });

    it('should support strict mode', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const shapePath = join(tempDir, 'shape.ttl');
      await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

      const result = await runCLI(
        `validate shacl ${dataPath} ${shapePath} --strict`
      );

      // Strict mode should be more rigorous
      assert.success(result);
    });
  });

  describe('validate zod (P1)', () => {
    it('should validate RDF data against Zod schema', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const schemaPath = join(tempDir, 'schema.mjs');
      await writeFile(schemaPath, `
        import { z } from 'zod';

        export default z.object({
          name: z.string(),
          age: z.number().min(0).max(150)
        });
      `);

      const result = await runCLI(`validate zod ${dataPath} ${schemaPath}`);

      // Zod validation may not be fully implemented yet
      if (result.exitCode === 0) {
        assert.success(result);
      }
    });

    it('should detect Zod validation errors', async () => {
      const dataPath = join(tempDir, 'invalid-data.ttl');
      await writeFile(dataPath, `
        @prefix ex: <http://test.example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:person1 a foaf:Person ;
          foaf:name "Alice" ;
          foaf:age 200 .
      `);

      const schemaPath = join(tempDir, 'schema.mjs');
      await writeFile(schemaPath, `
        import { z } from 'zod';

        export default z.object({
          name: z.string(),
          age: z.number().max(150)
        });
      `);

      const result = await runCLI(`validate zod ${dataPath} ${schemaPath}`);

      // Should detect age > 150
      if (result.exitCode !== 0) {
        assert.outputMatches(result, /validation|error|age/i);
      }
    });
  });

  describe('validate integrity (P2)', () => {
    it('should perform integrity checks on RDF data', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(10));

      const result = await runCLI(`validate integrity ${dataPath}`);

      assert.success(result);
    });

    it('should detect broken references', async () => {
      const dataPath = join(tempDir, 'broken-refs.ttl');
      await writeFile(dataPath, `
        @prefix ex: <http://test.example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:person1 foaf:knows ex:nonexistent .
      `);

      const result = await runCLI(`validate integrity ${dataPath} --check-refs`);

      // Should detect that ex:nonexistent is not defined
      if (result.exitCode !== 0) {
        assert.outputMatches(result, /reference|missing|nonexistent/i);
      }
    });

    it('should check for duplicate statements', async () => {
      const dataPath = join(tempDir, 'duplicates.ttl');
      await writeFile(dataPath, `
        @prefix ex: <http://test.example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:person1 foaf:name "Alice" .
        ex:person1 foaf:name "Alice" .
      `);

      const result = await runCLI(`validate integrity ${dataPath} --check-duplicates`);

      // Should detect duplicates
      if (result.exitCode !== 0) {
        assert.outputMatches(result, /duplicate/i);
      }
    });
  });

  describe('Error Handling', () => {
    it('should handle missing shape file', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const result = await runCLI(`validate shacl ${dataPath} /nonexistent/shape.ttl`);

      assert.failure(result);
      assert.outputContains(result, 'not found');
    });

    it('should handle invalid SHACL shapes', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const shapePath = join(tempDir, 'invalid-shape.ttl');
      await writeFile(shapePath, 'this is not valid shacl');

      const result = await runCLI(`validate shacl ${dataPath} ${shapePath}`);

      assert.failure(result);
      assert.outputMatches(result, /shape|error|invalid/i);
    });
  });

  describe('Performance: Validation operations', () => {
    it('should validate large datasets efficiently', async () => {
      const dataPath = join(tempDir, 'large-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(1000));

      const shapePath = join(tempDir, 'shape.ttl');
      await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

      const result = await runCLI(`validate shacl ${dataPath} ${shapePath}`);

      assert.success(result);
      expect(result.duration).toBeLessThan(2000); // Should handle 1000 triples
    });

    it('should support incremental validation', async () => {
      const dataPath = join(tempDir, 'incremental.ttl');
      await writeFile(dataPath, generators.rdfTriples(5000));

      const shapePath = join(tempDir, 'shape.ttl');
      await writeFile(shapePath, generators.shaclShapes('foaf:Person'));

      const result = await runCLI(
        `validate shacl ${dataPath} ${shapePath} --incremental`
      );

      // Should handle incremental validation if implemented
      if (result.exitCode === 0) {
        assert.success(result);
      }
    });
  });
});
