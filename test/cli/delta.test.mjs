/**
 * @fileoverview Tests for CLI delta command
 * Testing RDF dataset comparison and diff generation
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  execCLI,
  createCLITestContext,
  RDF_SAMPLES,
  assertSuccess,
  assertOutputContains,
  parseJSONOutput
} from './test-helpers.mjs';
import { join } from 'node:path';
import { writeFile, readFile } from 'node:fs/promises';

describe('CLI: delta command', () => {
  let ctx;

  beforeEach(() => {
    ctx = createCLITestContext();
  });

  afterEach(async () => {
    await ctx.cleanup();
  });

  describe('Dataset comparison (Critical Path)', () => {
    it('should detect added triples', async () => {
      const dir = await ctx.createTempDir();
      const sourcePath = join(dir, 'source.ttl');
      const targetPath = join(dir, 'target.ttl');

      await writeFile(sourcePath, RDF_SAMPLES.simple);
      await writeFile(targetPath, RDF_SAMPLES.extended);

      const result = await execCLI(['delta', sourcePath, targetPath]);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'Added triples:');
      assertOutputContains(result.stdout, 'Dataset Comparison:');
    });

    it('should detect removed triples', async () => {
      const dir = await ctx.createTempDir();
      const sourcePath = join(dir, 'source.ttl');
      const targetPath = join(dir, 'target.ttl');

      await writeFile(sourcePath, RDF_SAMPLES.extended);
      await writeFile(targetPath, RDF_SAMPLES.simple);

      const result = await execCLI(['delta', sourcePath, targetPath]);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'Removed triples:');
    });

    it('should detect no changes in identical datasets', async () => {
      const dir = await ctx.createTempDir();
      const sourcePath = join(dir, 'source.ttl');
      const targetPath = join(dir, 'target.ttl');

      await writeFile(sourcePath, RDF_SAMPLES.simple);
      await writeFile(targetPath, RDF_SAMPLES.simple);

      const result = await execCLI(['delta', sourcePath, targetPath]);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'Added triples: 0');
      assertOutputContains(result.stdout, 'Removed triples: 0');
    });
  });

  describe('Delta reporting', () => {
    it('should display summary statistics', async () => {
      const dir = await ctx.createTempDir();
      const sourcePath = join(dir, 'source.ttl');
      const targetPath = join(dir, 'target.ttl');

      await writeFile(sourcePath, RDF_SAMPLES.simple);
      await writeFile(targetPath, RDF_SAMPLES.extended);

      const result = await execCLI(['delta', sourcePath, targetPath]);

      assertSuccess(result);
      expect(result.stdout).toMatch(/Added triples: \d+/);
      expect(result.stdout).toMatch(/Removed triples: \d+/);
      expect(result.stdout).toMatch(/Unchanged triples: \d+/);
    });

    it('should limit displayed triples to first 5', async () => {
      const dir = await ctx.createTempDir();
      const sourcePath = join(dir, 'source.ttl');
      const targetPath = join(dir, 'target.ttl');

      // Create source with few triples
      await writeFile(sourcePath, RDF_SAMPLES.empty);

      // Create target with many triples
      let largeContent = '@prefix ex: <http://example.org/> .\n';
      largeContent += '@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n\n';
      for (let i = 0; i < 10; i++) {
        largeContent += `ex:person${i} a foaf:Person ; foaf:name "Person${i}" .\n`;
      }
      await writeFile(targetPath, largeContent);

      const result = await execCLI(['delta', sourcePath, targetPath]);

      assertSuccess(result);
      // Should show "... and X more" message
      expect(result.stdout).toMatch(/and \d+ more/);
    });
  });

  describe('Output to file', () => {
    it('should write delta report to JSON file', async () => {
      const dir = await ctx.createTempDir();
      const sourcePath = join(dir, 'source.ttl');
      const targetPath = join(dir, 'target.ttl');
      const outputPath = join(dir, 'delta.json');

      await writeFile(sourcePath, RDF_SAMPLES.simple);
      await writeFile(targetPath, RDF_SAMPLES.extended);

      const result = await execCLI([
        'delta',
        sourcePath,
        targetPath,
        '--output',
        outputPath
      ]);

      assertSuccess(result);
      assertOutputContains(result.stdout, `Delta report written to ${outputPath}`);

      // Verify report structure
      const reportContent = await readFile(outputPath, 'utf-8');
      const report = JSON.parse(reportContent);

      expect(report).toHaveProperty('summary');
      expect(report.summary).toHaveProperty('added');
      expect(report.summary).toHaveProperty('removed');
      expect(report.summary).toHaveProperty('unchanged');
      expect(report).toHaveProperty('added');
      expect(report).toHaveProperty('removed');
      expect(Array.isArray(report.added)).toBe(true);
      expect(Array.isArray(report.removed)).toBe(true);
    });

    it('should include triple details in report', async () => {
      const dir = await ctx.createTempDir();
      const sourcePath = join(dir, 'source.ttl');
      const targetPath = join(dir, 'target.ttl');
      const outputPath = join(dir, 'delta.json');

      await writeFile(sourcePath, RDF_SAMPLES.simple);
      await writeFile(targetPath, RDF_SAMPLES.extended);

      const result = await execCLI([
        'delta',
        sourcePath,
        targetPath,
        '--output',
        outputPath
      ]);

      assertSuccess(result);

      const reportContent = await readFile(outputPath, 'utf-8');
      const report = JSON.parse(reportContent);

      // Added triples should have subject, predicate, object
      if (report.added.length > 0) {
        const triple = report.added[0];
        expect(triple).toHaveProperty('subject');
        expect(triple).toHaveProperty('predicate');
        expect(triple).toHaveProperty('object');
      }
    });
  });

  describe('Edge cases', () => {
    it('should handle empty source dataset', async () => {
      const dir = await ctx.createTempDir();
      const sourcePath = join(dir, 'source.ttl');
      const targetPath = join(dir, 'target.ttl');

      await writeFile(sourcePath, RDF_SAMPLES.empty);
      await writeFile(targetPath, RDF_SAMPLES.simple);

      const result = await execCLI(['delta', sourcePath, targetPath]);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'Added triples:');
      assertOutputContains(result.stdout, 'Removed triples: 0');
    });

    it('should handle empty target dataset', async () => {
      const dir = await ctx.createTempDir();
      const sourcePath = join(dir, 'source.ttl');
      const targetPath = join(dir, 'target.ttl');

      await writeFile(sourcePath, RDF_SAMPLES.simple);
      await writeFile(targetPath, RDF_SAMPLES.empty);

      const result = await execCLI(['delta', sourcePath, targetPath]);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'Removed triples:');
      assertOutputContains(result.stdout, 'Added triples: 0');
    });

    it('should handle both datasets empty', async () => {
      const dir = await ctx.createTempDir();
      const sourcePath = join(dir, 'source.ttl');
      const targetPath = join(dir, 'target.ttl');

      await writeFile(sourcePath, RDF_SAMPLES.empty);
      await writeFile(targetPath, RDF_SAMPLES.empty);

      const result = await execCLI(['delta', sourcePath, targetPath]);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'Added triples: 0');
      assertOutputContains(result.stdout, 'Removed triples: 0');
    });
  });

  describe('Performance', () => {
    it('should compare large datasets efficiently', async () => {
      const dir = await ctx.createTempDir();
      const sourcePath = join(dir, 'source.ttl');
      const targetPath = join(dir, 'target.ttl');

      // Generate large source dataset
      let sourceContent = '@prefix ex: <http://example.org/> .\n';
      sourceContent += '@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n\n';
      for (let i = 0; i < 500; i++) {
        sourceContent += `ex:person${i} a foaf:Person ; foaf:name "Person${i}" .\n`;
      }

      // Generate large target dataset with changes
      let targetContent = '@prefix ex: <http://example.org/> .\n';
      targetContent += '@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n\n';
      for (let i = 0; i < 500; i++) {
        targetContent += `ex:person${i} a foaf:Person ; foaf:name "Person${i}" .\n`;
        if (i < 100) {
          targetContent += `ex:person${i} foaf:age ${20 + i} .\n`; // Add new triples
        }
      }

      await writeFile(sourcePath, sourceContent);
      await writeFile(targetPath, targetContent);

      const start = Date.now();
      const result = await execCLI(['delta', sourcePath, targetPath]);
      const duration = Date.now() - start;

      assertSuccess(result);

      // Performance target: compare 500+ entities in under 3 seconds
      expect(duration).toBeLessThan(3000);
    });
  });
});
