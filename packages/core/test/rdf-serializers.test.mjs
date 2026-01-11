/**
 * @file RDF Serializers Test Suite
 * @module @unrdf/core/test/rdf-serializers
 */

import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  serializeNTriples,
  serializeTurtle,
  serializeJsonLd,
  serializeToFile,
  batchSerialize,
  createSerializerStream,
} from '../src/rdf/serializers.mjs';
import { unlinkSync, existsSync } from 'node:fs';

const { namedNode, literal, quad, defaultGraph } = dataFactory;

describe('RDF Serializers', () => {
  describe('serializeNTriples', () => {
    it('should serialize quads to N-Triples format', async () => {
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice'),
          defaultGraph()
        ),
      ];

      const chunks = [];
      for await (const chunk of serializeNTriples(quads)) {
        chunks.push(chunk);
      }

      const result = chunks.join('');
      expect(result).toContain('<http://example.org/alice>');
      expect(result).toContain('<http://xmlns.com/foaf/0.1/name>');
      expect(result).toContain('"Alice"');
    });

    it('should handle empty quad stream', async () => {
      const chunks = [];
      for await (const chunk of serializeNTriples([])) {
        chunks.push(chunk);
      }

      expect(chunks).toHaveLength(0);
    });

    it('should serialize multiple quads with chunking', async () => {
      const quads = [];
      for (let i = 0; i < 2500; i++) {
        quads.push(
          quad(
            namedNode(`http://example.org/s${i}`),
            namedNode('http://example.org/p'),
            literal(`value${i}`),
            defaultGraph()
          )
        );
      }

      const chunks = [];
      for await (const chunk of serializeNTriples(quads, { chunkSize: 1000 })) {
        chunks.push(chunk);
      }

      expect(chunks.length).toBeGreaterThan(1);
      const result = chunks.join('');
      expect(result).toContain('value0');
      expect(result).toContain('value2499');
    });

    it('should handle literals with language tags', async () => {
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice', 'en'),
          defaultGraph()
        ),
      ];

      const chunks = [];
      for await (const chunk of serializeNTriples(quads)) {
        chunks.push(chunk);
      }

      const result = chunks.join('');
      expect(result).toContain('"Alice"@en');
    });

    it('should handle literals with datatypes', async () => {
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://example.org/age'),
          literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer')),
          defaultGraph()
        ),
      ];

      const chunks = [];
      for await (const chunk of serializeNTriples(quads)) {
        chunks.push(chunk);
      }

      const result = chunks.join('');
      expect(result).toContain('^^<http://www.w3.org/2001/XMLSchema#integer>');
    });
  });

  describe('serializeTurtle', () => {
    it('should serialize quads to Turtle format', async () => {
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice'),
          defaultGraph()
        ),
      ];

      const chunks = [];
      for await (const chunk of serializeTurtle(quads)) {
        chunks.push(chunk);
      }

      const result = chunks.join('');
      expect(result).toContain('http://example.org/alice');
      expect(result).toContain('http://xmlns.com/foaf/0.1/name');
      expect(result).toContain('"Alice"');
    });

    it('should include prefixes in Turtle output', async () => {
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice'),
          defaultGraph()
        ),
      ];

      const prefixes = {
        foaf: 'http://xmlns.com/foaf/0.1/',
        ex: 'http://example.org/',
      };

      const chunks = [];
      for await (const chunk of serializeTurtle(quads, { prefixes })) {
        chunks.push(chunk);
      }

      const result = chunks.join('');
      expect(result).toContain('@prefix foaf:');
      expect(result).toContain('@prefix ex:');
    });

    it('should group quads by subject', async () => {
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice'),
          defaultGraph()
        ),
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/age'),
          literal('30'),
          defaultGraph()
        ),
      ];

      const chunks = [];
      for await (const chunk of serializeTurtle(quads, { chunkSize: 10 })) {
        chunks.push(chunk);
      }

      const result = chunks.join('');
      expect(result).toContain('http://example.org/alice');
      expect(result).toContain('http://xmlns.com/foaf/0.1/name');
      expect(result).toContain('http://xmlns.com/foaf/0.1/age');
    });
  });

  describe('serializeJsonLd', () => {
    it('should serialize quads to JSON-LD format', async () => {
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice'),
          defaultGraph()
        ),
      ];

      const chunks = [];
      for await (const chunk of serializeJsonLd(quads)) {
        chunks.push(chunk);
      }

      const result = chunks.join('');
      const doc = JSON.parse(result);

      expect(doc).toHaveProperty('@graph');
      expect(doc['@graph']).toBeInstanceOf(Array);
      expect(doc['@graph'].length).toBeGreaterThan(0);
    });

    it('should handle multiple predicates per subject', async () => {
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice'),
          defaultGraph()
        ),
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/age'),
          literal('30'),
          defaultGraph()
        ),
      ];

      const chunks = [];
      for await (const chunk of serializeJsonLd(quads)) {
        chunks.push(chunk);
      }

      const result = chunks.join('');
      const doc = JSON.parse(result);

      expect(doc['@graph'][0]).toHaveProperty('http://xmlns.com/foaf/0.1/name');
      expect(doc['@graph'][0]).toHaveProperty('http://xmlns.com/foaf/0.1/age');
    });
  });

  describe('serializeToFile', () => {
    const testFile = '/tmp/test-serialize.nt';

    it('should serialize store to file', async () => {
      const store = createStore();
      store.add(
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice'),
          defaultGraph()
        )
      );

      const stats = await serializeToFile(store, testFile, { format: 'ntriples' });

      expect(stats).toHaveProperty('bytesWritten');
      expect(stats).toHaveProperty('quadsWritten');
      expect(stats.bytesWritten).toBeGreaterThan(0);

      expect(existsSync(testFile)).toBe(true);

      if (existsSync(testFile)) unlinkSync(testFile);
    });

    it('should serialize with compression', async () => {
      const store = createStore();
      store.add(
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice'),
          defaultGraph()
        )
      );

      const compressedFile = testFile + '.gz';
      await serializeToFile(store, compressedFile, {
        format: 'ntriples',
        compress: true,
      });

      expect(existsSync(compressedFile)).toBe(true);

      if (existsSync(compressedFile)) unlinkSync(compressedFile);
    });

    it('should throw error for invalid store', async () => {
      await expect(serializeToFile(null, testFile)).rejects.toThrow('store is required');
    });

    it('should throw error for invalid outputPath', async () => {
      const store = createStore();
      await expect(serializeToFile(store, '')).rejects.toThrow('outputPath is required');
    });
  });

  describe('batchSerialize', () => {
    it('should serialize multiple stores in batch', async () => {
      const store1 = createStore();
      store1.add(
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice'),
          defaultGraph()
        )
      );

      const store2 = createStore();
      store2.add(
        quad(
          namedNode('http://example.org/bob'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Bob'),
          defaultGraph()
        )
      );

      const results = [];
      for await (const result of batchSerialize([store1, store2], { format: 'ntriples' })) {
        results.push(result);
      }

      expect(results.length).toBeGreaterThan(0);
      expect(results.some(r => r.storeIndex === 0)).toBe(true);
      expect(results.some(r => r.storeIndex === 1)).toBe(true);
    });
  });

  describe('createSerializerStream', () => {
    it('should create transform stream for serialization', async () => {
      const serializer = createSerializerStream({ format: 'ntriples', chunkSize: 1 });

      const results = [];
      serializer.on('data', chunk => {
        results.push(chunk);
      });

      const testQuad = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice'),
        defaultGraph()
      );

      serializer.write(testQuad);
      serializer.end();

      await new Promise(resolve => serializer.on('end', resolve));

      expect(results.length).toBeGreaterThan(0);
    });
  });
});
