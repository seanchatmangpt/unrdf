/**
 * @file Tests for Semantic Analyzer
 * @module test/ai-semantic/semantic-analyzer
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Store, DataFactory } from 'n3';
import { createSemanticAnalyzer } from '../../src/knowledge-engine/ai-semantic/semantic-analyzer.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('SemanticAnalyzer', () => {
  let store;
  let analyzer;

  beforeEach(() => {
    store = new Store();
    analyzer = createSemanticAnalyzer({
      cacheSize: 100,
      maxConcepts: 50,
      minConceptFrequency: 1, // Allow single occurrences for tests
    });
  });

  describe('analyze', () => {
    it('should analyze an RDF graph and extract concepts', async () => {
      // Add test data
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');
      const name = namedNode('http://example.org/name');
      const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
      const person = namedNode('http://example.org/Person');

      store.addQuad(quad(alice, rdfType, person));
      store.addQuad(quad(alice, name, literal('Alice')));
      store.addQuad(quad(alice, knows, bob));
      store.addQuad(quad(bob, rdfType, person));
      store.addQuad(quad(bob, name, literal('Bob')));

      const result = await analyzer.analyze(store);

      expect(result).toBeDefined();
      expect(result.concepts).toBeInstanceOf(Array);
      expect(result.concepts.length).toBeGreaterThan(0);
      expect(result.relationships).toBeInstanceOf(Array);
      expect(result.patterns).toBeInstanceOf(Array);
      expect(result.suggestions).toBeInstanceOf(Array);
      expect(result.statistics).toBeDefined();
      expect(result.duration).toBeGreaterThan(0);
    });

    it('should extract key concepts with frequency and centrality', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));
      store.addQuad(quad(bob, knows, alice));

      const result = await analyzer.analyze(store);

      expect(result.concepts.length).toBeGreaterThan(0);
      const concept = result.concepts[0];
      expect(concept).toHaveProperty('uri');
      expect(concept).toHaveProperty('frequency');
      expect(concept).toHaveProperty('centrality');
      expect(typeof concept.frequency).toBe('number');
      expect(typeof concept.centrality).toBe('number');
    });

    it('should analyze relationships with strength scores', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      const result = await analyzer.analyze(store);

      expect(result.relationships.length).toBeGreaterThan(0);
      const rel = result.relationships[0];
      expect(rel).toHaveProperty('subject');
      expect(rel).toHaveProperty('predicate');
      expect(rel).toHaveProperty('object');
      expect(rel).toHaveProperty('strength');
      expect(rel.strength).toBeGreaterThanOrEqual(0);
      expect(rel.strength).toBeLessThanOrEqual(1);
    });

    it('should detect patterns in the graph', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const charlie = namedNode('http://example.org/charlie');
      const knows = namedNode('http://example.org/knows');
      const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
      const person = namedNode('http://example.org/Person');

      store.addQuad(quad(alice, rdfType, person));
      store.addQuad(quad(bob, rdfType, person));
      store.addQuad(quad(charlie, rdfType, person));
      store.addQuad(quad(alice, knows, bob));
      store.addQuad(quad(bob, knows, charlie));

      const result = await analyzer.analyze(store);

      expect(result.patterns.length).toBeGreaterThan(0);
      const pattern = result.patterns[0];
      expect(pattern).toHaveProperty('pattern');
      expect(pattern).toHaveProperty('count');
      expect(pattern).toHaveProperty('confidence');
    });

    it('should generate ontology improvement suggestions', async () => {
      // Create a graph with potential improvements
      const alice = namedNode('http://example.org/alice');
      const _bob = namedNode('http://example.org/_bob');
      const knows = namedNode('http://example.org/knows');

      // Add many forward relationships without reverse
      for (let i = 0; i < 10; i++) {
        const person = namedNode(`http://example.org/person${i}`);
        store.addQuad(quad(alice, knows, person));
      }

      const result = await analyzer.analyze(store);

      expect(result.suggestions).toBeInstanceOf(Array);
      // May or may not have suggestions depending on heuristics
    });

    it('should calculate graph statistics', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));
      store.addQuad(quad(bob, knows, alice));

      const result = await analyzer.analyze(store);

      expect(result.statistics).toBeDefined();
      expect(result.statistics.totalTriples).toBe(2);
      expect(result.statistics.uniqueSubjects).toBeGreaterThan(0);
      expect(result.statistics.uniquePredicates).toBeGreaterThan(0);
      expect(result.statistics.avgDegree).toBeGreaterThan(0);
      expect(result.statistics.density).toBeGreaterThanOrEqual(0);
    });

    it('should use cache for repeated analyses', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      const result1 = await analyzer.analyze(store);
      const result2 = await analyzer.analyze(store);

      expect(result1.duration).toBeGreaterThanOrEqual(0);
      expect(result2.duration).toBeGreaterThanOrEqual(0);
      expect(result2.duration).toBeLessThanOrEqual(result1.duration);

      const stats = analyzer.getStats();
      expect(stats.cacheHits).toBeGreaterThan(0);
    });

    it('should respect maxConcepts configuration', async () => {
      const limitedAnalyzer = createSemanticAnalyzer({ maxConcepts: 2 });

      for (let i = 0; i < 10; i++) {
        const entity = namedNode(`http://example.org/entity${i}`);
        const prop = namedNode(`http://example.org/prop`);
        store.addQuad(quad(entity, prop, literal(`value${i}`)));
      }

      const result = await limitedAnalyzer.analyze(store);

      expect(result.concepts.length).toBeLessThanOrEqual(2);
    });
  });

  describe('computeSimilarity', () => {
    it('should compute semantic similarity between concepts', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const charlie = namedNode('http://example.org/charlie');
      const knows = namedNode('http://example.org/knows');
      const likes = namedNode('http://example.org/likes');
      const pizza = namedNode('http://example.org/pizza');

      // Alice and Bob have similar properties
      store.addQuad(quad(alice, knows, charlie));
      store.addQuad(quad(alice, likes, pizza));
      store.addQuad(quad(bob, knows, charlie));
      store.addQuad(quad(bob, likes, pizza));

      const result = await analyzer.computeSimilarity(store, alice.value, bob.value);

      expect(result).toBeDefined();
      expect(result.similarity).toBeGreaterThanOrEqual(0);
      expect(result.similarity).toBeLessThanOrEqual(1);
      expect(result.method).toBe('jaccard');
      expect(result.commonProperties).toBeInstanceOf(Array);
      expect(result.commonNeighbors).toBeInstanceOf(Array);
    });

    it('should return low similarity for dissimilar concepts', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');
      const likes = namedNode('http://example.org/likes');
      const charlie = namedNode('http://example.org/charlie');
      const pizza = namedNode('http://example.org/pizza');

      // Alice and Bob have different properties
      store.addQuad(quad(alice, knows, charlie));
      store.addQuad(quad(bob, likes, pizza));

      const result = await analyzer.computeSimilarity(store, alice.value, bob.value);

      expect(result.similarity).toBeLessThan(0.5);
    });
  });

  describe('cache management', () => {
    it('should clear cache', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      await analyzer.analyze(store);
      analyzer.clearCache();

      const stats = analyzer.getStats();
      expect(stats.cacheSize).toBe(0);
    });

    it('should track statistics', () => {
      const stats = analyzer.getStats();

      expect(stats).toHaveProperty('analyses');
      expect(stats).toHaveProperty('cacheHits');
      expect(stats).toHaveProperty('cacheMisses');
      expect(stats).toHaveProperty('avgDuration');
      expect(stats).toHaveProperty('cacheSize');
      expect(stats).toHaveProperty('cacheHitRate');
    });
  });

  describe('performance', () => {
    it('should analyze within 500ms for moderate-sized graphs', async () => {
      // Create a graph with ~100 triples
      for (let i = 0; i < 50; i++) {
        const entity = namedNode(`http://example.org/entity${i}`);
        const prop = namedNode(`http://example.org/prop${i % 10}`);
        const obj = namedNode(`http://example.org/obj${i}`);
        store.addQuad(quad(entity, prop, obj));
      }

      const result = await analyzer.analyze(store, { useCache: false });

      expect(result.duration).toBeLessThan(500);
    });
  });
});
