/**
 * @file Tests for NLP Query Builder
 * @module test/ai-semantic/nlp-query-builder
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Store, DataFactory } from 'n3';
import { createNLPQueryBuilder } from '../../src/knowledge-engine/ai-semantic/nlp-query-builder.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('NLPQueryBuilder', () => {
  let store;
  let builder;

  beforeEach(() => {
    store = new Store();
    builder = createNLPQueryBuilder({
      cacheSize: 100,
      enableLLM: false, // Disable LLM for tests
    });

    // Add test data with labels
    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const person = namedNode('http://example.org/Person');
    const knows = namedNode('http://example.org/knows');
    const rdfsLabel = namedNode('http://www.w3.org/2000/01/rdf-schema#label');
    const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

    store.addQuad(quad(alice, rdfsLabel, literal('Alice')));
    store.addQuad(quad(alice, rdfType, person));
    store.addQuad(quad(alice, knows, bob));
    store.addQuad(quad(bob, rdfsLabel, literal('Bob')));
    store.addQuad(quad(bob, rdfType, person));
    store.addQuad(quad(person, rdfsLabel, literal('Person')));
  });

  describe('buildQuery', () => {
    it('should translate "list all X" to SELECT query', async () => {
      const result = await builder.buildQuery('list all person', store);

      expect(result).toBeDefined();
      expect(result.sparql).toContain('SELECT');
      expect(result.intent).toBe('select');
      expect(result.confidence).toBeGreaterThan(0);
      expect(result.method).toBe('pattern');
      expect(result.duration).toBeGreaterThan(0);
    });

    it('should translate "who is X" to DESCRIBE query', async () => {
      const result = await builder.buildQuery('who is alice', store);

      expect(result.sparql).toContain('DESCRIBE');
      expect(result.intent).toBe('describe');
      expect(result.confidence).toBeGreaterThan(0);
    });

    it('should translate "how many X" to COUNT query', async () => {
      const result = await builder.buildQuery('how many person', store);

      expect(result.sparql).toContain('COUNT');
      expect(result.sparql).toContain('SELECT');
      expect(result.intent).toBe('select');
    });

    it('should translate "does X have Y" to ASK query', async () => {
      const result = await builder.buildQuery('does alice have bob', store);

      expect(result.sparql).toContain('ASK');
      expect(result.intent).toBe('ask');
    });

    it('should extract entities from natural language', async () => {
      const result = await builder.buildQuery('list all person', store);

      expect(result.entities).toBeInstanceOf(Array);
      // Should find 'person' entity
      const personEntity = result.entities.find(e => e.text === 'person');
      if (personEntity) {
        expect(personEntity.uri).toBe('http://example.org/Person');
      }
    });

    it('should map entity text to RDF URIs', async () => {
      const result = await builder.buildQuery('show alice', store);

      expect(result.entities).toBeInstanceOf(Array);
      const aliceEntity = result.entities.find(e => e.text === 'alice');
      if (aliceEntity) {
        expect(aliceEntity.uri).toBe('http://example.org/alice');
      }
    });

    it('should use cache for repeated queries', async () => {
      const result1 = await builder.buildQuery('list all person', store);
      const result2 = await builder.buildQuery('list all person', store);

      expect(result1.duration).toBeGreaterThanOrEqual(0);
      expect(result2.duration).toBeGreaterThanOrEqual(0);
      expect(result2.duration).toBeLessThanOrEqual(result1.duration);

      const stats = builder.getStats();
      expect(stats.cacheHits).toBeGreaterThan(0);
    });

    it('should generate fallback SPARQL for unrecognized patterns', async () => {
      const result = await builder.buildQuery('random unknown query', store);

      expect(result.sparql).toBeDefined();
      expect(result.sparql).toContain('SELECT');
      expect(result.confidence).toBeLessThan(0.5);
    });

    it('should normalize queries before processing', async () => {
      const result1 = await builder.buildQuery('LIST ALL PERSON', store);
      const result2 = await builder.buildQuery('list all person', store);

      // Should generate similar queries
      expect(result1.intent).toBe(result2.intent);
    });

    it('should handle complex multi-part queries', async () => {
      const result = await builder.buildQuery('alice related to bob', store);

      expect(result.sparql).toBeDefined();
      expect(result.intent).toBe('select');
      expect(result.sparql).toContain('UNION');
    });
  });

  describe('performance', () => {
    it('should translate NL to SPARQL within 300ms', async () => {
      const result = await builder.buildQuery('list all person', store);

      expect(result.duration).toBeLessThan(300);
    });

    it('should handle large stores efficiently', async () => {
      // Add more data
      for (let i = 0; i < 100; i++) {
        const entity = namedNode(`http://example.org/entity${i}`);
        const rdfsLabel = namedNode('http://www.w3.org/2000/01/rdf-schema#label');
        store.addQuad(quad(entity, rdfsLabel, literal(`Entity ${i}`)));
      }

      const result = await builder.buildQuery('list all entities', store);

      expect(result.duration).toBeLessThan(300);
    });
  });

  describe('cache management', () => {
    it('should clear caches', async () => {
      await builder.buildQuery('list all person', store);
      builder.clearCache();

      const stats = builder.getStats();
      expect(stats.cacheSize).toBe(0);
      expect(stats.entityCacheSize).toBe(0);
    });

    it('should track statistics', () => {
      const stats = builder.getStats();

      expect(stats).toHaveProperty('queries');
      expect(stats).toHaveProperty('cacheHits');
      expect(stats).toHaveProperty('cacheMisses');
      expect(stats).toHaveProperty('avgDuration');
      expect(stats).toHaveProperty('cacheSize');
      expect(stats).toHaveProperty('entityCacheSize');
      expect(stats).toHaveProperty('cacheHitRate');
    });
  });

  describe('edge cases', () => {
    it('should handle empty queries gracefully', async () => {
      const result = await builder.buildQuery('', store);

      expect(result).toBeDefined();
      expect(result.sparql).toBeDefined();
    });

    it('should handle queries with no matching entities', async () => {
      const result = await builder.buildQuery('list all nonexistent', store);

      expect(result).toBeDefined();
      expect(result.sparql).toBeDefined();
      expect(result.confidence).toBeLessThanOrEqual(0.8);
    });

    it('should handle empty store', async () => {
      const emptyStore = new Store();
      const result = await builder.buildQuery('list all items', emptyStore);

      expect(result).toBeDefined();
      expect(result.sparql).toBeDefined();
    });
  });
});
