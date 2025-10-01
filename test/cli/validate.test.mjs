/**
 * @fileoverview Tests for CLI validate command
 * Focus on SHACL validation workflows
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  execCLI,
  createCLITestContext,
  RDF_SAMPLES,
  assertSuccess,
  assertFailure,
  assertOutputContains
} from './test-helpers.mjs';
import { join } from 'node:path';
import { writeFile, readFile } from 'node:fs/promises';

describe('CLI: validate command', () => {
  let ctx;

  beforeEach(() => {
    ctx = createCLITestContext();
  });

  afterEach(async () => {
    await ctx.cleanup();
  });

  describe('SHACL validation (Critical Path)', () => {
    it('should validate conforming data successfully', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      const shapePath = join(dir, 'shape.ttl');

      await writeFile(dataPath, RDF_SAMPLES.simple);
      await writeFile(shapePath, RDF_SAMPLES.shaclShape);

      const result = await execCLI([
        'validate',
        dataPath,
        '--shape',
        shapePath
      ]);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'Validation passed');
    });

    it('should report violations for non-conforming data', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      const shapePath = join(dir, 'shape.ttl');

      // Data missing required properties
      const invalidData = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:person1 a foaf:Person .`;

      await writeFile(dataPath, invalidData);
      await writeFile(shapePath, RDF_SAMPLES.shaclShape);

      const result = await execCLI([
        'validate',
        dataPath,
        '--shape',
        shapePath
      ]);

      assertSuccess(result); // Command succeeds but validation fails
      assertOutputContains(result.stdout, 'Validation failed');
      assertOutputContains(result.stdout, 'violations');
    });

    it('should write validation report to output file', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      const shapePath = join(dir, 'shape.ttl');
      const outputPath = join(dir, 'report.json');

      await writeFile(dataPath, RDF_SAMPLES.simple);
      await writeFile(shapePath, RDF_SAMPLES.shaclShape);

      const result = await execCLI([
        'validate',
        dataPath,
        '--shape',
        shapePath,
        '--output',
        outputPath
      ]);

      assertSuccess(result);
      assertOutputContains(result.stdout, `Validation report written to ${outputPath}`);

      // Verify report structure
      const reportContent = await readFile(outputPath, 'utf-8');
      const report = JSON.parse(reportContent);
      expect(report).toHaveProperty('conforms');
    });
  });

  describe('Error handling', () => {
    it('should handle missing data file', async () => {
      const dir = await ctx.createTempDir();
      const shapePath = join(dir, 'shape.ttl');
      await writeFile(shapePath, RDF_SAMPLES.shaclShape);

      const result = await execCLI([
        'validate',
        '/nonexistent/data.ttl',
        '--shape',
        shapePath
      ]);

      assertFailure(result);
    });

    it('should handle missing shape file', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      await writeFile(dataPath, RDF_SAMPLES.simple);

      const result = await execCLI([
        'validate',
        dataPath,
        '--shape',
        '/nonexistent/shape.ttl'
      ]);

      assertFailure(result);
    });

    it('should handle invalid SHACL shapes', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      const shapePath = join(dir, 'shape.ttl');

      await writeFile(dataPath, RDF_SAMPLES.simple);
      await writeFile(shapePath, 'invalid shacl syntax @#$%');

      const result = await execCLI([
        'validate',
        dataPath,
        '--shape',
        shapePath
      ]);

      assertFailure(result);
    });
  });

  describe('Validation reporting', () => {
    it('should display violation count', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      const shapePath = join(dir, 'shape.ttl');

      const invalidData = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:person1 a foaf:Person .
ex:person2 a foaf:Person .`;

      await writeFile(dataPath, invalidData);
      await writeFile(shapePath, RDF_SAMPLES.shaclShape);

      const result = await execCLI([
        'validate',
        dataPath,
        '--shape',
        shapePath
      ]);

      assertSuccess(result);
      expect(result.stdout).toMatch(/Found \d+ violations/);
    });

    it('should display violation details', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      const shapePath = join(dir, 'shape.ttl');

      const invalidData = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:person1 a foaf:Person .`;

      await writeFile(dataPath, invalidData);
      await writeFile(shapePath, RDF_SAMPLES.shaclShape);

      const result = await execCLI([
        'validate',
        dataPath,
        '--shape',
        shapePath
      ]);

      assertSuccess(result);
      // Should show violation messages
      expect(result.stdout).toContain('violations:');
    });
  });

  describe('Performance', () => {
    it('should validate large datasets efficiently', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'large.ttl');
      const shapePath = join(dir, 'shape.ttl');

      // Generate large dataset
      let content = '@prefix ex: <http://example.org/> .\n';
      content += '@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n\n';
      for (let i = 0; i < 500; i++) {
        content += `ex:person${i} a foaf:Person ; foaf:name "Person${i}" ; foaf:age ${20 + (i % 50)} .\n`;
      }

      await writeFile(dataPath, content);
      await writeFile(shapePath, RDF_SAMPLES.shaclShape);

      const start = Date.now();
      const result = await execCLI([
        'validate',
        dataPath,
        '--shape',
        shapePath
      ]);
      const duration = Date.now() - start;

      assertSuccess(result);

      // Performance target: validate 500 entities in under 5 seconds
      expect(duration).toBeLessThan(5000);
    });
  });
});
