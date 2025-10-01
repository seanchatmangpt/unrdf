/**
 * @fileoverview Tests for CLI query command
 * Focus on SPARQL query execution and result formatting
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  execCLI,
  createCLITestContext,
  RDF_SAMPLES,
  SPARQL_QUERIES,
  assertSuccess,
  assertFailure,
  assertOutputContains,
  parseJSONOutput
} from './test-helpers.mjs';
import { join } from 'node:path';
import { writeFile, readFile } from 'node:fs/promises';

describe('CLI: query command', () => {
  let ctx;

  beforeEach(() => {
    ctx = createCLITestContext();
  });

  afterEach(async () => {
    await ctx.cleanup();
  });

  describe('SPARQL SELECT queries (Critical Path)', () => {
    it('should execute SELECT query with --query flag', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.ttl');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI([
        'query',
        inputPath,
        '--query',
        SPARQL_QUERIES.selectPeople
      ]);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'Alice');
      assertOutputContains(result.stdout, 'Bob');
    });

    it('should execute query from file with --query-file', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      const queryPath = join(dir, 'query.rq');

      await writeFile(dataPath, RDF_SAMPLES.simple);
      await writeFile(queryPath, SPARQL_QUERIES.selectPeople);

      const result = await execCLI([
        'query',
        dataPath,
        '--query-file',
        queryPath
      ]);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'Alice');
      assertOutputContains(result.stdout, 'Bob');
    });

    it('should fail when no query provided', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.ttl');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI(['query', inputPath]);

      assertFailure(result);
      assertOutputContains(result.stdout, 'Query required');
    });
  });

  describe('Output formats (Critical Path)', () => {
    it('should output results as JSON', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.ttl');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI([
        'query',
        inputPath,
        '--query',
        SPARQL_QUERIES.selectPeople,
        '--format',
        'json'
      ]);

      assertSuccess(result);

      // Verify valid JSON
      const json = parseJSONOutput(result.stdout);
      expect(Array.isArray(json)).toBe(true);
      expect(json.length).toBeGreaterThan(0);
    });

    it('should output results as CSV', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.ttl');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI([
        'query',
        inputPath,
        '--query',
        SPARQL_QUERIES.selectPeople,
        '--format',
        'csv'
      ]);

      assertSuccess(result);

      // Verify CSV format
      const lines = result.stdout.trim().split('\n');
      expect(lines.length).toBeGreaterThan(1); // Header + data
      expect(lines[0]).toContain(','); // CSV separator
    });

    it('should output results as table (default)', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.ttl');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI([
        'query',
        inputPath,
        '--query',
        SPARQL_QUERIES.selectPeople
      ]);

      assertSuccess(result);

      // Verify table format with column separators
      expect(result.stdout).toContain('|');
      expect(result.stdout).toContain('-');
    });
  });

  describe('Result persistence', () => {
    it('should write results to output file', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.ttl');
      const outputPath = join(dir, 'results.json');

      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI([
        'query',
        inputPath,
        '--query',
        SPARQL_QUERIES.selectPeople,
        '--format',
        'json',
        '--output',
        outputPath
      ]);

      assertSuccess(result);
      assertOutputContains(result.stdout, `Results written to ${outputPath}`);

      // Verify output file
      const outputContent = await readFile(outputPath, 'utf-8');
      const json = JSON.parse(outputContent);
      expect(Array.isArray(json)).toBe(true);
    });
  });

  describe('Query filtering', () => {
    it('should execute query with FILTER clause', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.ttl');
      await writeFile(inputPath, RDF_SAMPLES.extended);

      const result = await execCLI([
        'query',
        inputPath,
        '--query',
        SPARQL_QUERIES.selectByAge,
        '--format',
        'json'
      ]);

      assertSuccess(result);

      const json = parseJSONOutput(result.stdout);
      // Should only include people over 25
      expect(json.length).toBeGreaterThan(0);
      for (const row of json) {
        expect(parseInt(row.age)).toBeGreaterThan(25);
      }
    });
  });

  describe('Empty results', () => {
    it('should handle queries with no results', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.ttl');
      await writeFile(inputPath, RDF_SAMPLES.empty);

      const result = await execCLI([
        'query',
        inputPath,
        '--query',
        SPARQL_QUERIES.selectPeople,
        '--format',
        'table'
      ]);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'No results found');
    });

    it('should output empty array for JSON format', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.ttl');
      await writeFile(inputPath, RDF_SAMPLES.empty);

      const result = await execCLI([
        'query',
        inputPath,
        '--query',
        SPARQL_QUERIES.selectPeople,
        '--format',
        'json'
      ]);

      assertSuccess(result);

      const json = parseJSONOutput(result.stdout);
      expect(Array.isArray(json)).toBe(true);
      expect(json.length).toBe(0);
    });
  });

  describe('Error handling', () => {
    it('should handle invalid SPARQL syntax', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'data.ttl');
      await writeFile(inputPath, RDF_SAMPLES.simple);

      const result = await execCLI([
        'query',
        inputPath,
        '--query',
        'INVALID SPARQL QUERY'
      ]);

      assertFailure(result);
      expect(result.stderr).toContain('error');
    });

    it('should handle missing data file', async () => {
      const result = await execCLI([
        'query',
        '/nonexistent/data.ttl',
        '--query',
        SPARQL_QUERIES.selectPeople
      ]);

      assertFailure(result);
    });
  });

  describe('Performance', () => {
    it('should execute queries on large datasets efficiently', async () => {
      const dir = await ctx.createTempDir();
      const inputPath = join(dir, 'large.ttl');

      // Generate large dataset
      let content = '@prefix ex: <http://example.org/> .\n';
      content += '@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n\n';
      for (let i = 0; i < 1000; i++) {
        content += `ex:person${i} a foaf:Person ; foaf:name "Person${i}" ; foaf:age ${20 + (i % 50)} .\n`;
      }

      await writeFile(inputPath, content);

      const start = Date.now();
      const result = await execCLI([
        'query',
        inputPath,
        '--query',
        SPARQL_QUERIES.selectPeople,
        '--format',
        'json'
      ]);
      const duration = Date.now() - start;

      assertSuccess(result);

      const json = parseJSONOutput(result.stdout);
      expect(json.length).toBe(1000);

      // Performance target: query 1000 triples in under 3 seconds
      expect(duration).toBeLessThan(3000);
    });
  });
});
