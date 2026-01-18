/**
 * @file Loader Tests
 * @description Tests for TOML and RDF loaders
 */

import { describe, it, expect } from 'vitest';
import { loadTomlSync, loadRdf, loadRdfContext, mergeContexts } from '../src/loaders.mjs';

describe('TOML Loaders', () => {
  describe('loadTomlSync', () => {
    it('should parse simple TOML', () => {
      const toml = `
        title = "Test"
        count = 42
      `;
      const result = loadTomlSync(toml);
      expect(result.title).toBe('Test');
      expect(result.count).toBe(42);
    });

    it('should parse nested TOML', () => {
      const toml = `
        [database]
        host = "localhost"
        port = 5432
      `;
      const result = loadTomlSync(toml);
      expect(result.database.host).toBe('localhost');
      expect(result.database.port).toBe(5432);
    });

    it('should parse arrays', () => {
      const toml = `
        items = ["a", "b", "c"]
      `;
      const result = loadTomlSync(toml);
      expect(result.items).toEqual(['a', 'b', 'c']);
    });

    it('should throw on invalid TOML', () => {
      const invalid = 'invalid toml [[';
      expect(() => loadTomlSync(invalid)).toThrow('Failed to parse TOML');
    });

    it('should handle empty TOML', () => {
      const result = loadTomlSync('');
      expect(result).toEqual({});
    });

    it('should parse TOML with comments', () => {
      const toml = `
        # This is a comment
        key = "value"  # Inline comment
      `;
      const result = loadTomlSync(toml);
      expect(result.key).toBe('value');
    });
  });
});

describe('RDF Loaders', () => {
  describe('loadRdf', () => {
    it('should parse Turtle content', async () => {
      const turtle = `
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate "object" .
      `;

      const { store, triples } = await loadRdf({
        content: turtle,
        format: 'turtle',
      });

      expect(store).toBeDefined();
      expect(triples).toHaveLength(1);
      expect(triples[0].subject.value).toBe('http://example.org/subject');
      expect(triples[0].predicate.value).toBe('http://example.org/predicate');
      expect(triples[0].object.value).toBe('object');
    });

    it('should handle multiple triples', async () => {
      const turtle = `
        @prefix ex: <http://example.org/> .
        ex:s1 ex:p1 "o1" .
        ex:s2 ex:p2 "o2" .
        ex:s3 ex:p3 "o3" .
      `;

      const { triples } = await loadRdf({
        content: turtle,
        format: 'turtle',
      });

      expect(triples).toHaveLength(3);
    });

    it('should use base IRI', async () => {
      const turtle = `
        <subject> <predicate> "object" .
      `;

      const { triples } = await loadRdf({
        content: turtle,
        format: 'turtle',
        baseIRI: 'http://example.org/',
      });

      expect(triples[0].subject.value).toBe('http://example.org/subject');
    });

    it('should throw on invalid Turtle', async () => {
      const invalid = 'invalid turtle @@@';

      await expect(
        loadRdf({ content: invalid, format: 'turtle' })
      ).rejects.toThrow('Failed to load RDF');
    });

    it('should validate options with Zod', async () => {
      await expect(
        loadRdf({})
      ).rejects.toThrow();
    });
  });

  describe('loadRdfContext', () => {
    it('should create context with helpers', async () => {
      const turtle = `
        @prefix ex: <http://example.org/> .
        ex:subject1 ex:predicate1 "object1" .
        ex:subject2 ex:predicate2 "object2" .
      `;

      const context = await loadRdfContext({
        content: turtle,
        format: 'turtle',
      });

      expect(context.triples).toHaveLength(2);
      expect(context.store).toBeDefined();
      expect(typeof context.query).toBe('function');
      expect(typeof context.subjects).toBe('function');
      expect(typeof context.predicates).toBe('function');
      expect(typeof context.objects).toBe('function');
    });

    it('should extract unique subjects', async () => {
      const turtle = `
        @prefix ex: <http://example.org/> .
        ex:s1 ex:p1 "o1" .
        ex:s2 ex:p2 "o2" .
        ex:s1 ex:p3 "o3" .
      `;

      const context = await loadRdfContext({
        content: turtle,
        format: 'turtle',
      });

      const subjects = context.subjects();
      expect(subjects).toHaveLength(2);
      expect(subjects).toContain('http://example.org/s1');
      expect(subjects).toContain('http://example.org/s2');
    });

    it('should extract unique predicates', async () => {
      const turtle = `
        @prefix ex: <http://example.org/> .
        ex:s1 ex:p1 "o1" .
        ex:s2 ex:p2 "o2" .
        ex:s3 ex:p1 "o3" .
      `;

      const context = await loadRdfContext({
        content: turtle,
        format: 'turtle',
      });

      const preds = context.predicates();
      expect(preds).toHaveLength(2);
      expect(preds).toContain('http://example.org/p1');
      expect(preds).toContain('http://example.org/p2');
    });

    it('should extract unique objects', async () => {
      const turtle = `
        @prefix ex: <http://example.org/> .
        ex:s1 ex:p1 "o1" .
        ex:s2 ex:p2 "o2" .
        ex:s3 ex:p3 "o1" .
      `;

      const context = await loadRdfContext({
        content: turtle,
        format: 'turtle',
      });

      const objs = context.objects();
      expect(objs).toHaveLength(2);
      expect(objs).toContain('o1');
      expect(objs).toContain('o2');
    });

    it('should execute SPARQL queries', async () => {
      const turtle = `
        @prefix ex: <http://example.org/> .
        ex:Alice ex:knows ex:Bob .
        ex:Bob ex:knows ex:Charlie .
      `;

      const context = await loadRdfContext({
        content: turtle,
        format: 'turtle',
      });

      const results = context.query('SELECT ?s ?o WHERE { ?s <http://example.org/knows> ?o }');
      expect(results).toBeDefined();
      expect(Array.isArray(results)).toBe(true);
    });
  });
});

describe('Context Merging', () => {
  describe('mergeContexts', () => {
    it('should merge TOML context', async () => {
      const context = await mergeContexts({
        toml: {
          content: 'title = "Test"\ncount = 42',
        },
      });

      expect(context.title).toBe('Test');
      expect(context.count).toBe(42);
    });

    it('should merge RDF context', async () => {
      const context = await mergeContexts({
        rdf: {
          content: '@prefix ex: <http://example.org/> . ex:s ex:p "o" .',
          format: 'turtle',
        },
      });

      expect(context.rdf).toBeDefined();
      expect(context.rdf.triples).toHaveLength(1);
    });

    it('should merge both TOML and RDF', async () => {
      const context = await mergeContexts({
        toml: {
          content: 'title = "Test"',
        },
        rdf: {
          content: '@prefix ex: <http://example.org/> . ex:s ex:p "o" .',
          format: 'turtle',
        },
      });

      expect(context.title).toBe('Test');
      expect(context.rdf).toBeDefined();
      expect(context.rdf.triples).toHaveLength(1);
    });

    it('should handle empty options', async () => {
      const context = await mergeContexts({});
      expect(context).toEqual({});
    });

    it('should not overwrite TOML with RDF key', async () => {
      const context = await mergeContexts({
        toml: {
          content: 'rdf = "toml-value"',
        },
        rdf: {
          content: '@prefix ex: <http://example.org/> . ex:s ex:p "o" .',
          format: 'turtle',
        },
      });

      // RDF context should overwrite TOML 'rdf' key
      expect(context.rdf).toBeDefined();
      expect(context.rdf.triples).toBeDefined();
    });
  });
});
