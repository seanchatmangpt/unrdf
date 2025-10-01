/**
 * @fileoverview Integration tests for CLI workflows
 * Testing end-to-end scenarios combining multiple commands
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  execCLI,
  createCLITestContext,
  RDF_SAMPLES,
  SPARQL_QUERIES,
  assertSuccess,
  assertOutputContains
} from './test-helpers.mjs';
import { join } from 'node:path';
import { writeFile, readFile } from 'node:fs/promises';

describe('CLI: Integration workflows', () => {
  let ctx;

  beforeEach(() => {
    ctx = createCLITestContext();
  });

  afterEach(async () => {
    await ctx.cleanup();
  });

  describe('Parse-Query workflow (Critical Path)', () => {
    it('should parse RDF then query it', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      const parsedPath = join(dir, 'parsed.ttl');

      // Step 1: Parse RDF data
      await writeFile(dataPath, RDF_SAMPLES.simple);
      const parseResult = await execCLI(['parse', dataPath, '--output', parsedPath]);
      assertSuccess(parseResult);

      // Step 2: Query parsed data
      const queryResult = await execCLI([
        'query',
        parsedPath,
        '--query',
        SPARQL_QUERIES.selectPeople,
        '--format',
        'json'
      ]);
      assertSuccess(queryResult);

      const results = JSON.parse(queryResult.stdout);
      expect(results.length).toBeGreaterThan(0);
    });
  });

  describe('Parse-Validate workflow (Critical Path)', () => {
    it('should parse RDF then validate against SHACL', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      const shapePath = join(dir, 'shape.ttl');

      // Step 1: Parse RDF data
      await writeFile(dataPath, RDF_SAMPLES.simple);
      await writeFile(shapePath, RDF_SAMPLES.shaclShape);

      const parseResult = await execCLI(['parse', dataPath]);
      assertSuccess(parseResult);

      // Step 2: Validate data
      const validateResult = await execCLI([
        'validate',
        dataPath,
        '--shape',
        shapePath
      ]);
      assertSuccess(validateResult);
      assertOutputContains(validateResult.stdout, 'Validation passed');
    });
  });

  describe('Init-Parse-Query workflow (Critical Path)', () => {
    it('should initialize project, parse and query data', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'workflow-test';

      // Step 1: Initialize project
      const initResult = await execCLI(['init', projectName], { cwd: dir });
      assertSuccess(initResult);

      const projectDir = join(dir, projectName);
      const dataPath = join(projectDir, 'data.ttl');

      // Step 2: Parse the generated data
      const parseResult = await execCLI(['parse', dataPath]);
      assertSuccess(parseResult);

      // Step 3: Query the data
      const queryResult = await execCLI([
        'query',
        dataPath,
        '--query',
        SPARQL_QUERIES.selectPeople,
        '--format',
        'json'
      ]);
      assertSuccess(queryResult);

      const results = JSON.parse(queryResult.stdout);
      expect(results.length).toBeGreaterThan(0);
    });
  });

  describe('Delta workflow (Critical Path)', () => {
    it('should compare two versions of dataset', async () => {
      const dir = await ctx.createTempDir();
      const v1Path = join(dir, 'v1.ttl');
      const v2Path = join(dir, 'v2.ttl');
      const deltaPath = join(dir, 'delta.json');

      // Create version 1
      await writeFile(v1Path, RDF_SAMPLES.simple);

      // Create version 2 with changes
      await writeFile(v2Path, RDF_SAMPLES.extended);

      // Compare versions
      const deltaResult = await execCLI([
        'delta',
        v1Path,
        v2Path,
        '--output',
        deltaPath
      ]);
      assertSuccess(deltaResult);

      // Verify delta report
      const deltaContent = await readFile(deltaPath, 'utf-8');
      const delta = JSON.parse(deltaContent);

      expect(delta.summary.added).toBeGreaterThan(0);
      expect(delta).toHaveProperty('added');
      expect(delta).toHaveProperty('removed');
    });
  });

  describe('Prefix expansion workflow', () => {
    it('should expand CURIEs in query results', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');

      await writeFile(dataPath, RDF_SAMPLES.simple);

      // Query with CURIEs
      const queryResult = await execCLI([
        'query',
        dataPath,
        '--query',
        'SELECT ?s ?p ?o WHERE { ?s a ?o }',
        '--format',
        'json'
      ]);
      assertSuccess(queryResult);

      // Expand a CURIE
      const expandResult = await execCLI(['prefix', 'expand', 'foaf:Person']);
      assertSuccess(expandResult);
      assertOutputContains(expandResult.stdout, 'http://xmlns.com/foaf/0.1/Person');
    });
  });

  describe('Round-trip workflows', () => {
    it('should parse, serialize, and re-parse data', async () => {
      const dir = await ctx.createTempDir();
      const originalPath = join(dir, 'original.ttl');
      const serializedPath = join(dir, 'serialized.ttl');
      const reparsedPath = join(dir, 'reparsed.ttl');

      // Original parse
      await writeFile(originalPath, RDF_SAMPLES.simple);
      const parse1 = await execCLI(['parse', originalPath, '--output', serializedPath]);
      assertSuccess(parse1);

      // Re-parse serialized data
      const parse2 = await execCLI(['parse', serializedPath, '--output', reparsedPath]);
      assertSuccess(parse2);

      // Both should succeed
      assertOutputContains(parse1.stdout, 'successfully');
      assertOutputContains(parse2.stdout, 'successfully');
    });
  });

  describe('Error recovery workflows', () => {
    it('should handle validation failure gracefully', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      const shapePath = join(dir, 'shape.ttl');
      const reportPath = join(dir, 'report.json');

      // Invalid data
      const invalidData = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:person1 a foaf:Person .`;

      await writeFile(dataPath, invalidData);
      await writeFile(shapePath, RDF_SAMPLES.shaclShape);

      // Validate (will fail)
      const validateResult = await execCLI([
        'validate',
        dataPath,
        '--shape',
        shapePath,
        '--output',
        reportPath
      ]);

      assertSuccess(validateResult); // Command succeeds but validation fails
      assertOutputContains(validateResult.stdout, 'Validation failed');

      // Report should be created
      const report = JSON.parse(await readFile(reportPath, 'utf-8'));
      expect(report.conforms).toBe(false);
      expect(report.results.length).toBeGreaterThan(0);
    });
  });

  describe('Batch processing workflows', () => {
    it('should process multiple files in sequence', async () => {
      const dir = await ctx.createTempDir();

      // Create multiple data files
      const files = [];
      for (let i = 0; i < 3; i++) {
        const filePath = join(dir, `data${i}.ttl`);
        await writeFile(filePath, RDF_SAMPLES.simple);
        files.push(filePath);
      }

      // Parse each file
      for (const file of files) {
        const result = await execCLI(['parse', file]);
        assertSuccess(result);
      }
    });

    it('should query multiple datasets', async () => {
      const dir = await ctx.createTempDir();

      // Create datasets
      const simple = join(dir, 'simple.ttl');
      const extended = join(dir, 'extended.ttl');

      await writeFile(simple, RDF_SAMPLES.simple);
      await writeFile(extended, RDF_SAMPLES.extended);

      // Query both
      const query1 = await execCLI([
        'query',
        simple,
        '--query',
        SPARQL_QUERIES.selectPeople,
        '--format',
        'json'
      ]);

      const query2 = await execCLI([
        'query',
        extended,
        '--query',
        SPARQL_QUERIES.selectPeople,
        '--format',
        'json'
      ]);

      assertSuccess(query1);
      assertSuccess(query2);

      const results1 = JSON.parse(query1.stdout);
      const results2 = JSON.parse(query2.stdout);

      // Extended should have more results
      expect(results2.length).toBeGreaterThanOrEqual(results1.length);
    });
  });

  describe('Performance workflows', () => {
    it('should handle end-to-end workflow efficiently', async () => {
      const dir = await ctx.createTempDir();
      const dataPath = join(dir, 'data.ttl');
      const shapePath = join(dir, 'shape.ttl');
      const queryPath = join(dir, 'query.rq');
      const resultsPath = join(dir, 'results.json');

      // Setup
      await writeFile(dataPath, RDF_SAMPLES.extended);
      await writeFile(shapePath, RDF_SAMPLES.shaclShape);
      await writeFile(queryPath, SPARQL_QUERIES.selectPeople);

      const start = Date.now();

      // Parse
      const parseResult = await execCLI(['parse', dataPath]);
      assertSuccess(parseResult);

      // Validate
      const validateResult = await execCLI([
        'validate',
        dataPath,
        '--shape',
        shapePath
      ]);
      assertSuccess(validateResult);

      // Query
      const queryResult = await execCLI([
        'query',
        dataPath,
        '--query-file',
        queryPath,
        '--format',
        'json',
        '--output',
        resultsPath
      ]);
      assertSuccess(queryResult);

      const duration = Date.now() - start;

      // Performance target: complete workflow in under 2 seconds
      expect(duration).toBeLessThan(2000);
    });
  });
});
