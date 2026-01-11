/**
 * @file RDF Parsers Test Suite
 * @module @unrdf/core/test/rdf-parsers
 */

import { describe, it, expect } from 'vitest';
import { writeFileSync, unlinkSync, existsSync } from 'node:fs';
import {
  parseRdf,
  parseFile,
  parseToStore,
  parseFileToStore,
  parseWithRecovery,
  createParserStream,
  batchParse,
} from '../src/rdf/parsers.mjs';

describe('RDF Parsers', () => {
  describe('parseRdf', () => {
    it('should parse N-Triples format', async () => {
      const ntriples = `
        <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
        <http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .
      `;

      const quads = [];
      for await (const quad of parseRdf(ntriples, { format: 'ntriples' })) {
        quads.push(quad);
      }

      expect(quads).toHaveLength(2);
      expect(quads[0].subject.value).toBe('http://example.org/alice');
      expect(quads[0].object.value).toBe('Alice');
      expect(quads[1].subject.value).toBe('http://example.org/bob');
      expect(quads[1].object.value).toBe('Bob');
    });

    it('should parse Turtle format', async () => {
      const turtle = `
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix ex: <http://example.org/> .

        ex:alice foaf:name "Alice" .
      `;

      const quads = [];
      for await (const quad of parseRdf(turtle, { format: 'turtle' })) {
        quads.push(quad);
      }

      expect(quads.length).toBeGreaterThan(0);
      expect(quads[0].object.value).toBe('Alice');
    });

    it('should parse JSON-LD format', async () => {
      const jsonld = JSON.stringify({
        '@graph': [
          {
            '@id': 'http://example.org/alice',
            'http://xmlns.com/foaf/0.1/name': [{ '@value': 'Alice' }],
          },
        ],
      });

      const quads = [];
      for await (const quad of parseRdf(jsonld, { format: 'jsonld' })) {
        quads.push(quad);
      }

      expect(quads.length).toBeGreaterThan(0);
      expect(quads[0].subject.value).toBe('http://example.org/alice');
    });

    it('should handle empty input', async () => {
      const quads = [];
      for await (const quad of parseRdf('', { format: 'ntriples' })) {
        quads.push(quad);
      }

      expect(quads).toHaveLength(0);
    });

    it('should skip comments in N-Triples', async () => {
      const ntriples = `
        # This is a comment
        <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
        # Another comment
        <http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .
      `;

      const quads = [];
      for await (const quad of parseRdf(ntriples, { format: 'ntriples' })) {
        quads.push(quad);
      }

      expect(quads).toHaveLength(2);
    });

    it('should handle literals with language tags', async () => {
      const ntriples = `
        <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice"@en .
      `;

      const quads = [];
      for await (const quad of parseRdf(ntriples, { format: 'ntriples' })) {
        quads.push(quad);
      }

      expect(quads).toHaveLength(1);
      expect(quads[0].object.language).toBe('en');
    });

    it('should handle literals with datatypes', async () => {
      const ntriples = `
        <http://example.org/alice> <http://example.org/age> "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
      `;

      const quads = [];
      for await (const quad of parseRdf(ntriples, { format: 'ntriples' })) {
        quads.push(quad);
      }

      expect(quads).toHaveLength(1);
      expect(quads[0].object.datatype.value).toBe('http://www.w3.org/2001/XMLSchema#integer');
    });

    it('should throw error in strict mode for invalid input', async () => {
      const invalid = '<invalid> <malformed';

      const quads = [];
      try {
        for await (const quad of parseRdf(invalid, { format: 'ntriples', strict: true })) {
          quads.push(quad);
        }
        expect.fail('Should have thrown error');
      } catch (error) {
        expect(error).toBeDefined();
      }
    });

    it('should not throw in permissive mode for invalid input', async () => {
      const invalid = '<invalid> <malformed';

      const quads = [];
      for await (const quad of parseRdf(invalid, { format: 'ntriples', strict: false })) {
        quads.push(quad);
      }

      expect(quads).toBeDefined();
    });
  });

  describe('parseFile', () => {
    const testFile = '/tmp/test-parse.nt';

    it('should parse RDF from file', async () => {
      const content = `
        <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
        <http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .
      `;

      writeFileSync(testFile, content);

      const quads = [];
      for await (const quad of parseFile(testFile, { format: 'ntriples' })) {
        quads.push(quad);
      }

      expect(quads).toHaveLength(2);

      if (existsSync(testFile)) unlinkSync(testFile);
    });

    it('should throw error for invalid file path', async () => {
      try {
        for await (const _quad of parseFile('')) {
          // Should not reach here
        }
        expect.fail('Should have thrown error');
      } catch (error) {
        expect(error.message).toContain('filePath is required');
      }
    });
  });

  describe('parseToStore', () => {
    it('should parse RDF into Oxigraph store', async () => {
      const ntriples = `
        <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
        <http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .
      `;

      const store = await parseToStore(ntriples, { format: 'ntriples' });

      expect(store.size).toBe(2);
    });

    it('should handle empty input to store', async () => {
      const store = await parseToStore('', { format: 'ntriples' });

      expect(store.size).toBe(0);
    });
  });

  describe('parseFileToStore', () => {
    const testFile = '/tmp/test-parse-store.nt';

    it('should parse file into Oxigraph store', async () => {
      const content = `
        <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
        <http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .
      `;

      writeFileSync(testFile, content);

      const store = await parseFileToStore(testFile, { format: 'ntriples' });

      expect(store.size).toBe(2);

      if (existsSync(testFile)) unlinkSync(testFile);
    });
  });

  describe('parseWithRecovery', () => {
    it('should parse with error recovery', async () => {
      const mixed = `
        <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
        <invalid> <malformed
        <http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .
      `;

      const result = await parseWithRecovery(mixed, {
        format: 'ntriples',
        skipInvalid: true,
        collectErrors: true,
      });

      expect(result.quads.length).toBeGreaterThan(0);
      expect(result.errors).toBeDefined();
    });

    it('should collect errors during parsing', async () => {
      const invalid = '<invalid> <malformed';

      const result = await parseWithRecovery(invalid, {
        format: 'ntriples',
        skipInvalid: true,
        collectErrors: true,
      });

      expect(result.quads).toHaveLength(0);
      expect(result.errors).toBeDefined();
    });
  });

  describe('createParserStream', () => {
    it('should create transform stream for parsing', async () => {
      const parser = createParserStream({ format: 'ntriples' });

      const quads = [];
      parser.on('data', q => {
        quads.push(q);
      });

      const ntriples = '<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .\n';

      parser.write(ntriples);
      parser.end();

      await new Promise(resolve => parser.on('end', resolve));

      expect(quads.length).toBeGreaterThan(0);
    });
  });

  describe('batchParse', () => {
    it('should parse multiple RDF strings in batch', async () => {
      const inputs = [
        '<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .',
        '<http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .',
      ];

      const results = [];
      for await (const result of batchParse(inputs, { format: 'ntriples' })) {
        results.push(result);
      }

      expect(results.length).toBe(2);
      expect(results[0].index).toBe(0);
      expect(results[1].index).toBe(1);
    });

    it('should handle errors in batch with strict mode disabled', async () => {
      const inputs = [
        '<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .',
        '<invalid> <malformed',
        '<http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .',
      ];

      const results = [];
      for await (const result of batchParse(inputs, { format: 'ntriples', strict: false })) {
        results.push(result);
      }

      expect(results.length).toBeGreaterThan(0);
    });
  });
});
