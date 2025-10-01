/**
 * @fileoverview Tests for CLI parse command
 * Focus on critical path: 20% of features that deliver 80% of value
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

describe('CLI: parse command', () => {
  let ctx;

  beforeEach(() => {
    ctx = createCLITestContext();
  });

  afterEach(async () => {
    await ctx.cleanup();
  });

  describe('Basic parsing (Critical Path)', () => {
    it('should parse valid Turtle file', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'input.ttl');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI(['parse', inputPath]);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'Parsing RDF data');
      assertOutputContains(result.stdout, 'successfully');
    });

    it('should parse and write output file', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'input.ttl');
      const outputPath = join(dir, 'output.ttl');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI(['parse', inputPath, '--output', outputPath]);

      assertSuccess(result);

      // Verify output file exists and contains data
      const outputContent = await readFile(outputPath, 'utf-8');
      expect(outputContent).toBeTruthy();
      expect(outputContent.length).toBeGreaterThan(0);
    });

    it('should report parsing errors for invalid RDF', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'invalid.ttl');
      await writeFile(inputPath, 'this is not valid turtle syntax @#$%');

      const result = await execCLI(['parse', inputPath]);

      assertFailure(result);
      expect(result.stderr).toContain('error');
    });

    it('should handle missing input file gracefully', async () => {
      const result = await execCLI(['parse', '/nonexistent/file.ttl']);

      assertFailure(result);
      assertOutputContains(result.stdout, 'File not found');
    });
  });

  describe('Performance (Critical Path)', () => {
    it('should parse large file efficiently', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'large.ttl');

      // Generate large RDF file (1000 triples)
      let largeContent = '@prefix ex: <http://example.org/> .\n';
      largeContent += '@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n\n';
      for (let i = 0; i < 1000; i++) {
        largeContent += `ex:person${i} a foaf:Person ; foaf:name "Person${i}" ; foaf:age ${20 + (i % 50)} .\n`;
      }

      await writeFile(inputPath, largeContent);

      const start = Date.now();
      const result = await execCLI(['parse', inputPath]);
      const duration = Date.now() - start;

      assertSuccess(result);
      assertOutputContains(result.stdout, '1000 triples');

      // Performance target: should parse 1000 triples in under 2 seconds
      expect(duration).toBeLessThan(2000);
    });
  });

  describe('Edge cases', () => {
    it('should handle empty RDF file', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'empty.ttl');
      await writeFile(inputPath, RDF_SAMPLES.empty);

      const result = await execCLI(['parse', inputPath]);

      assertSuccess(result);
      assertOutputContains(result.stdout, '0 triples');
    });

    it('should handle file with only prefixes', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'prefixes.ttl');
      await writeFile(inputPath, `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix schema: <https://schema.org/> .`);

      const result = await execCLI(['parse', inputPath]);

      assertSuccess(result);
      assertOutputContains(result.stdout, '0 triples');
    });
  });

  describe('Format detection', () => {
    it('should auto-detect Turtle format from extension', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.ttl');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI(['parse', inputPath]);

      assertSuccess(result);
    });

    it('should handle .turtle extension', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.turtle');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI(['parse', inputPath]);

      assertSuccess(result);
    });
  });

  describe('Output validation', () => {
    it('should display triple count', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'input.ttl');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI(['parse', inputPath]);

      assertSuccess(result);
      // Simple sample has multiple triples
      expect(result.stdout).toMatch(/\d+ triples/);
    });

    it('should display parsing duration', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'input.ttl');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI(['parse', inputPath]);

      assertSuccess(result);
      expect(result.stdout).toMatch(/\d+ms/);
    });
  });
});
