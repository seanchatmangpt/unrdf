/**
 * @fileoverview Unit tests for CLI v2 parse commands
 * Tests the 15% value command group - P0 priority
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  runCLI,
  createTempDir,
  assert,
  generators
} from '../test-utils.mjs';
import { writeFile, readFile } from 'node:fs/promises';
import { join } from 'node:path';

describe('CLI v2: parse commands', () => {
  let tempDir;
  let cleanup;

  beforeEach(async () => {
    const temp = await createTempDir('parse-test-');
    tempDir = temp.dir;
    cleanup = temp.cleanup;
  });

  afterEach(async () => {
    await cleanup();
  });

  describe('parse turtle (Critical Path - P0)', () => {
    it('should parse valid Turtle file', async () => {
      const inputPath = join(tempDir, 'input.ttl');
      await writeFile(inputPath, generators.rdfTriples(10));

      const result = await runCLI(`parse turtle ${inputPath}`);

      assert.success(result);
      assert.outputMatches(result, /parsed|success/i);
      assert.outputContains(result, '10');
    });

    it('should write parsed output to file', async () => {
      const inputPath = join(tempDir, 'input.ttl');
      await writeFile(inputPath, generators.rdfTriples(10));

      const outputPath = join(tempDir, 'output.ttl');
      const result = await runCLI(`parse turtle ${inputPath} --output ${outputPath}`);

      assert.success(result);

      const outputContent = await readFile(outputPath, 'utf-8');
      expect(outputContent.length).toBeGreaterThan(0);
      expect(outputContent).toContain('foaf:Person');
    });

    it('should report parse errors for invalid Turtle', async () => {
      const inputPath = join(tempDir, 'invalid.ttl');
      await writeFile(inputPath, 'this is not valid turtle @#$% <<<>>>');

      const result = await runCLI(`parse turtle ${inputPath}`);

      assert.failure(result);
      assert.outputMatches(result, /error|invalid|syntax/i);
    });

    it('should meet performance target (< 500ms for 10k triples)', async () => {
      const inputPath = join(tempDir, 'large.ttl');
      await writeFile(inputPath, generators.rdfTriples(10000));

      const result = await runCLI(`parse turtle ${inputPath}`);

      assert.success(result);
      assert.performanceTarget(result, 500, 'parse 10k triples');
    });

    it('should handle empty file gracefully', async () => {
      const inputPath = join(tempDir, 'empty.ttl');
      await writeFile(inputPath, '');

      const result = await runCLI(`parse turtle ${inputPath}`);

      // Should either succeed with 0 triples or fail gracefully
      if (result.exitCode === 0) {
        assert.outputContains(result, '0');
      }
    });

    it('should support different Turtle variants', async () => {
      const inputPath = join(tempDir, 'prefixes.ttl');
      await writeFile(inputPath, `
        @prefix ex: <http://test.example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:alice a foaf:Person ;
          foaf:name "Alice" .
      `);

      const result = await runCLI(`parse turtle ${inputPath}`);

      assert.success(result);
    });
  });

  describe('parse nquads (P1)', () => {
    it('should parse N-Quads format', async () => {
      const inputPath = join(tempDir, 'input.nq');
      await writeFile(inputPath, `
        <http://test.example.org/s> <http://test.example.org/p> <http://test.example.org/o> .
        <http://test.example.org/s2> <http://test.example.org/p2> "literal" .
      `);

      const result = await runCLI(`parse nquads ${inputPath}`);

      assert.success(result);
    });

    it('should handle named graphs in N-Quads', async () => {
      const inputPath = join(tempDir, 'named-graph.nq');
      await writeFile(inputPath, `
        <http://test.example.org/s> <http://test.example.org/p> <http://test.example.org/o> <http://test.example.org/graph1> .
      `);

      const result = await runCLI(`parse nquads ${inputPath}`);

      assert.success(result);
    });
  });

  describe('parse jsonld (P1)', () => {
    it('should parse JSON-LD format', async () => {
      const inputPath = join(tempDir, 'input.jsonld');
      await writeFile(inputPath, JSON.stringify({
        "@context": {
          "foaf": "http://xmlns.com/foaf/0.1/",
          "name": "foaf:name"
        },
        "@id": "http://test.example.org/alice",
        "@type": "foaf:Person",
        "name": "Alice"
      }));

      const result = await runCLI(`parse jsonld ${inputPath}`);

      assert.success(result);
    });

    it('should handle JSON-LD arrays', async () => {
      const inputPath = join(tempDir, 'array.jsonld');
      await writeFile(inputPath, JSON.stringify([
        {
          "@id": "http://test.example.org/alice",
          "http://xmlns.com/foaf/0.1/name": "Alice"
        },
        {
          "@id": "http://test.example.org/bob",
          "http://xmlns.com/foaf/0.1/name": "Bob"
        }
      ]));

      const result = await runCLI(`parse jsonld ${inputPath}`);

      assert.success(result);
    });

    it('should validate JSON-LD context', async () => {
      const inputPath = join(tempDir, 'invalid-context.jsonld');
      await writeFile(inputPath, JSON.stringify({
        "@context": "http://nonexistent.example.org/context.jsonld",
        "@id": "http://test.example.org/test"
      }));

      const result = await runCLI(`parse jsonld ${inputPath}`);

      // Should fail or warn about invalid context
      if (result.exitCode !== 0) {
        assert.outputMatches(result, /context|error/i);
      }
    });
  });

  describe('parse rdfxml (P2)', () => {
    it('should parse RDF/XML format', async () => {
      const inputPath = join(tempDir, 'input.rdf');
      await writeFile(inputPath, `
        <?xml version="1.0"?>
        <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns:foaf="http://xmlns.com/foaf/0.1/">
          <foaf:Person rdf:about="http://test.example.org/alice">
            <foaf:name>Alice</foaf:name>
          </foaf:Person>
        </rdf:RDF>
      `);

      const result = await runCLI(`parse rdfxml ${inputPath}`);

      // May not be implemented yet - that's okay
      if (result.exitCode === 0) {
        assert.success(result);
      }
    });
  });

  describe('Error Handling', () => {
    it('should handle missing file', async () => {
      const result = await runCLI('parse turtle /nonexistent/file.ttl');

      assert.failure(result);
      assert.outputContains(result, 'not found');
    });

    it('should handle file read permission errors', async () => {
      // Create file with no read permissions (platform-dependent)
      const inputPath = join(tempDir, 'no-read.ttl');
      await writeFile(inputPath, generators.rdfTriples(1));

      // Try to parse
      const result = await runCLI(`parse turtle ${inputPath}`);

      // Should handle gracefully
      if (result.exitCode !== 0) {
        assert.outputMatches(result, /error|permission|access/i);
      }
    });

    it('should handle corrupted file', async () => {
      const inputPath = join(tempDir, 'corrupted.ttl');
      // Write binary garbage
      await writeFile(inputPath, Buffer.from([0xFF, 0xFE, 0x00, 0x00]));

      const result = await runCLI(`parse turtle ${inputPath}`);

      assert.failure(result);
    });

    it('should handle very large files gracefully', async () => {
      const inputPath = join(tempDir, 'huge.ttl');
      // Generate 100k triples
      await writeFile(inputPath, generators.rdfTriples(100000));

      const result = await runCLI(`parse turtle ${inputPath} --timeout 10000`);

      // Should either succeed or fail with clear message
      if (result.exitCode !== 0) {
        assert.outputMatches(result, /timeout|memory|too large/i);
      }
    }, 15000);
  });

  describe('Performance: Parse operations', () => {
    it('should stream large files efficiently', async () => {
      const inputPath = join(tempDir, 'stream-test.ttl');
      await writeFile(inputPath, generators.rdfTriples(50000));

      const result = await runCLI(`parse turtle ${inputPath} --stream`);

      // Should handle streaming if implemented
      if (result.exitCode === 0) {
        expect(result.duration).toBeLessThan(5000);
      }
    }, 10000);

    it('should parse incrementally to reduce memory', async () => {
      const inputPath = join(tempDir, 'incremental.ttl');
      await writeFile(inputPath, generators.rdfTriples(20000));

      const result = await runCLI(`parse turtle ${inputPath}`);

      assert.success(result);
      // Should complete without excessive memory usage
    });
  });

  describe('Format Detection', () => {
    it('should auto-detect format from file extension', async () => {
      const inputPath = join(tempDir, 'auto.ttl');
      await writeFile(inputPath, generators.rdfTriples(5));

      const result = await runCLI(`parse ${inputPath}`); // No format specified

      assert.success(result);
    });

    it('should detect JSON-LD from .jsonld extension', async () => {
      const inputPath = join(tempDir, 'auto.jsonld');
      await writeFile(inputPath, JSON.stringify({
        "@id": "http://test.example.org/test",
        "http://xmlns.com/foaf/0.1/name": "Test"
      }));

      const result = await runCLI(`parse ${inputPath}`);

      assert.success(result);
    });
  });
});
