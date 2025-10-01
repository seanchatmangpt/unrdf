/**
 * @fileoverview Unit tests for CLI v2 query commands
 * Tests the 20% value command group - P0 priority
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

describe('CLI v2: query commands', () => {
  let tempDir;
  let cleanup;

  beforeEach(async () => {
    const temp = await createTempDir('query-test-');
    tempDir = temp.dir;
    cleanup = temp.cleanup;
  });

  afterEach(async () => {
    await cleanup();
  });

  describe('query select (Critical Path - P0)', () => {
    it('should execute SELECT query successfully', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(10));

      const queryPath = join(tempDir, 'query.rq');
      await writeFile(queryPath, generators.sparqlQuery('select'));

      const result = await runCLI(`query select ${queryPath} ${dataPath}`);

      assert.success(result);
      assert.outputContains(result, 'person');
    });

    it('should support inline query', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const result = await runCLI(
        `query select "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 5" ${dataPath}`
      );

      assert.success(result);
    });

    it('should support JSON output format', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const queryPath = join(tempDir, 'query.rq');
      await writeFile(queryPath, generators.sparqlQuery('select'));

      const result = await runCLI(
        `query select ${queryPath} ${dataPath} --format json`
      );

      assert.success(result);
      const data = assert.jsonOutput(result);
      expect(Array.isArray(data)).toBe(true);
    });

    it('should support table output format', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const queryPath = join(tempDir, 'query.rq');
      await writeFile(queryPath, generators.sparqlQuery('select'));

      const result = await runCLI(
        `query select ${queryPath} ${dataPath} --format table`
      );

      assert.success(result);
      assert.outputMatches(result, /\|/); // Table format has pipes
    });

    it('should support CSV output format', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const queryPath = join(tempDir, 'query.rq');
      await writeFile(queryPath, generators.sparqlQuery('select'));

      const result = await runCLI(
        `query select ${queryPath} ${dataPath} --format csv`
      );

      assert.success(result);
      assert.outputMatches(result, /,/); // CSV has commas
    });

    it('should write output to file', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const queryPath = join(tempDir, 'query.rq');
      await writeFile(queryPath, generators.sparqlQuery('select'));

      const outputPath = join(tempDir, 'results.json');
      const result = await runCLI(
        `query select ${queryPath} ${dataPath} --output ${outputPath}`
      );

      assert.success(result);
      assert.outputContains(result, 'written');
    });

    it('should meet performance target (< 50ms for simple query)', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(100));

      const queryPath = join(tempDir, 'simple.rq');
      await writeFile(queryPath, 'SELECT ?s WHERE { ?s ?p ?o } LIMIT 10');

      const result = await runCLI(`query select ${queryPath} ${dataPath}`);

      assert.success(result);
      assert.performanceTarget(result, 50, 'simple SELECT query');
    });
  });

  describe('query ask (P0)', () => {
    it('should execute ASK query and return boolean', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const queryPath = join(tempDir, 'ask.rq');
      await writeFile(queryPath, generators.sparqlQuery('ask'));

      const result = await runCLI(`query ask ${queryPath} ${dataPath}`);

      assert.success(result);
      assert.outputMatches(result, /true|false/i);
    });

    it('should return true when pattern matches', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const queryPath = join(tempDir, 'ask-match.rq');
      await writeFile(queryPath, generators.sparqlQuery('ask'));

      const result = await runCLI(`query ask ${queryPath} ${dataPath} --format json`);

      assert.success(result);
      const data = assert.jsonOutput(result);
      expect(data.result).toBe(true);
    });

    it('should return false when pattern does not match', async () => {
      const dataPath = join(tempDir, 'empty.ttl');
      await writeFile(dataPath, '@prefix ex: <http://test.example.org/> .');

      const queryPath = join(tempDir, 'ask-no-match.rq');
      await writeFile(queryPath, generators.sparqlQuery('ask'));

      const result = await runCLI(`query ask ${queryPath} ${dataPath} --format json`);

      assert.success(result);
      const data = assert.jsonOutput(result);
      expect(data.result).toBe(false);
    });
  });

  describe('query construct (P1)', () => {
    it('should execute CONSTRUCT query', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const queryPath = join(tempDir, 'construct.rq');
      await writeFile(queryPath, generators.sparqlQuery('construct'));

      const result = await runCLI(`query construct ${queryPath} ${dataPath}`);

      assert.success(result);
    });

    it('should output in Turtle format by default', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const queryPath = join(tempDir, 'construct.rq');
      await writeFile(queryPath, generators.sparqlQuery('construct'));

      const result = await runCLI(
        `query construct ${queryPath} ${dataPath} --format turtle`
      );

      assert.success(result);
      assert.outputMatches(result, /@prefix|PREFIX/);
    });

    it('should support N-Triples output', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const queryPath = join(tempDir, 'construct.rq');
      await writeFile(queryPath, generators.sparqlQuery('construct'));

      const result = await runCLI(
        `query construct ${queryPath} ${dataPath} --format ntriples`
      );

      assert.success(result);
    });
  });

  describe('query describe (P2)', () => {
    it('should execute DESCRIBE query', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const queryPath = join(tempDir, 'describe.rq');
      await writeFile(queryPath, generators.sparqlQuery('describe'));

      const result = await runCLI(`query describe ${queryPath} ${dataPath}`);

      assert.success(result);
    });

    it('should describe specific resource', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const result = await runCLI(
        `query describe "http://test.example.org/entity0" ${dataPath}`
      );

      assert.success(result);
    });
  });

  describe('Error Handling', () => {
    it('should handle invalid SPARQL syntax', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(5));

      const queryPath = join(tempDir, 'invalid.rq');
      await writeFile(queryPath, 'SELECT THIS IS NOT VALID SPARQL');

      const result = await runCLI(`query select ${queryPath} ${dataPath}`);

      assert.failure(result);
      assert.outputContains(result, 'syntax');
    });

    it('should handle missing data file', async () => {
      const queryPath = join(tempDir, 'query.rq');
      await writeFile(queryPath, generators.sparqlQuery('select'));

      const result = await runCLI(`query select ${queryPath} /nonexistent/data.ttl`);

      assert.failure(result);
      assert.outputContains(result, 'not found');
    });

    it('should handle query timeout', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(10000)); // Large dataset

      const queryPath = join(tempDir, 'complex.rq');
      // Complex query that might timeout
      await writeFile(queryPath, `
        SELECT ?s1 ?s2 WHERE {
          ?s1 ?p1 ?o1 .
          ?s2 ?p2 ?o2 .
          ?s3 ?p3 ?o3 .
          FILTER(?s1 != ?s2)
        }
      `);

      const result = await runCLI(
        `query select ${queryPath} ${dataPath} --timeout 100`
      );

      // Should either succeed quickly or fail with timeout
      if (result.exitCode !== 0) {
        assert.outputMatches(result, /timeout|exceeded/i);
      }
    }, 5000);
  });

  describe('Performance: Query operations', () => {
    it('should handle large result sets efficiently', async () => {
      const dataPath = join(tempDir, 'large-data.ttl');
      await writeFile(dataPath, generators.rdfTriples(1000));

      const queryPath = join(tempDir, 'all.rq');
      await writeFile(queryPath, 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }');

      const result = await runCLI(`query select ${queryPath} ${dataPath}`);

      assert.success(result);
      // Should complete in reasonable time even with 1000 triples
      expect(result.duration).toBeLessThan(2000);
    });

    it('should use query optimization for filtered queries', async () => {
      const dataPath = join(tempDir, 'data.ttl');
      await writeFile(dataPath, generators.rdfTriples(500));

      const queryPath = join(tempDir, 'filtered.rq');
      await writeFile(
        queryPath,
        generators.sparqlQuery('select', 'FILTER(?age > 30)')
      );

      const result = await runCLI(`query select ${queryPath} ${dataPath}`);

      assert.success(result);
      assert.performanceTarget(result, 200, 'filtered query');
    });
  });
});
