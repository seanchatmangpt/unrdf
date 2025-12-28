/**
 * @file SPARQL Pattern Matcher Tests
 * @description Comprehensive tests for SPARQLPatternMatcher including
 * pattern matching, FILTER support, and performance benchmarks.
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { SPARQLPatternMatcher, createSPARQLPatternMatcher } from '../src/sparql-pattern-matcher.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

// Test constants
const EX = 'http://example.org/';
const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const FOAF = 'http://xmlns.com/foaf/0.1/';

/**
 * Helper to create a named node
 * @param {string} uri - URI string
 * @returns {Object} Named node
 */
function nn(uri) {
  return dataFactory.namedNode(uri);
}

/**
 * Helper to create a literal
 * @param {string} value - Literal value
 * @returns {Object} Literal
 */
function lit(value) {
  return dataFactory.literal(value);
}

describe('SPARQLPatternMatcher', () => {
  let store;
  let matcher;

  beforeEach(() => {
    store = createStore();
    matcher = new SPARQLPatternMatcher(store);
  });

  afterEach(() => {
    matcher.clearCache();
  });

  describe('Constructor', () => {
    it('should create instance with valid store', () => {
      expect(matcher).toBeInstanceOf(SPARQLPatternMatcher);
      expect(matcher.store).toBe(store);
    });

    it('should throw on invalid store', () => {
      expect(() => new SPARQLPatternMatcher(null)).toThrow(TypeError);
      expect(() => new SPARQLPatternMatcher({})).toThrow('must have a match() method');
    });

    it('should accept custom prefixes', () => {
      const custom = new SPARQLPatternMatcher(store, { myns: 'http://my.namespace/' });
      expect(custom.prefixes.myns).toBe('http://my.namespace/');
      expect(custom.prefixes.rdf).toBeDefined(); // default still present
    });
  });

  describe('createSPARQLPatternMatcher factory', () => {
    it('should create instance via factory function', () => {
      const m = createSPARQLPatternMatcher(store);
      expect(m).toBeInstanceOf(SPARQLPatternMatcher);
    });
  });

  describe('matchPattern - Simple Patterns', () => {
    beforeEach(() => {
      // Add test data: Alice knows Bob, Charlie
      store.add(dataFactory.triple(nn(EX + 'alice'), nn(FOAF + 'knows'), nn(EX + 'bob')));
      store.add(dataFactory.triple(nn(EX + 'alice'), nn(FOAF + 'knows'), nn(EX + 'charlie')));
      store.add(dataFactory.triple(nn(EX + 'alice'), nn(FOAF + 'name'), lit('Alice')));
      store.add(dataFactory.triple(nn(EX + 'bob'), nn(FOAF + 'name'), lit('Bob')));
      store.add(dataFactory.triple(nn(EX + 'alice'), nn(RDF + 'type'), nn(FOAF + 'Person')));
    });

    it('should match all triples with (?s ?p ?o)', async () => {
      const results = await matcher.matchPattern('?s', '?p', '?o');
      expect(results.length).toBe(5);
      expect(results[0]).toHaveProperty('s');
      expect(results[0]).toHaveProperty('p');
      expect(results[0]).toHaveProperty('o');
    });

    it('should match by named subject', async () => {
      const results = await matcher.matchPattern(`<${EX}alice>`, '?p', '?o');
      expect(results.length).toBe(4); // alice has 4 triples
      results.forEach(r => {
        expect(r.s).toBeUndefined(); // named subject not bound
        expect(r.p).toBeDefined();
        expect(r.o).toBeDefined();
      });
    });

    it('should match by named predicate (rdf:type)', async () => {
      const results = await matcher.matchPattern('?s', `<${RDF}type>`, '?o');
      expect(results.length).toBe(1);
      expect(results[0].s.value).toBe(EX + 'alice');
      expect(results[0].o.value).toBe(FOAF + 'Person');
    });

    it('should match by named object', async () => {
      const results = await matcher.matchPattern('?s', '?p', `<${EX}bob>`);
      expect(results.length).toBe(1);
      expect(results[0].s.value).toBe(EX + 'alice');
      expect(results[0].p.value).toBe(FOAF + 'knows');
    });

    it('should match using prefixed names', async () => {
      const results = await matcher.matchPattern('?s', 'rdf:type', '?o');
      expect(results.length).toBe(1);
    });

    it('should match using "a" as rdf:type', async () => {
      const results = await matcher.matchPattern('?s', 'a', '?o');
      expect(results.length).toBe(1);
    });

    it('should handle null pattern terms as wildcards', async () => {
      const results = await matcher.matchPattern(null, null, null);
      expect(results.length).toBe(5);
    });
  });

  describe('matchPattern - FILTER Support', () => {
    beforeEach(() => {
      store.add(dataFactory.triple(nn(EX + 'item1'), nn(EX + 'value'), lit('10')));
      store.add(dataFactory.triple(nn(EX + 'item2'), nn(EX + 'value'), lit('20')));
      store.add(dataFactory.triple(nn(EX + 'item3'), nn(EX + 'value'), lit('30')));
    });

    it('should apply equality filter', async () => {
      const filter = bindings => {
        const val = bindings.o;
        return val && val.value === '20';
      };
      const results = await matcher.matchPattern('?s', '?p', '?o', [filter]);
      expect(results.length).toBe(1);
      expect(results[0].o.value).toBe('20');
    });

    it('should apply comparison filter', async () => {
      const filter = bindings => {
        const val = bindings.o;
        return val && parseInt(val.value) > 15;
      };
      const results = await matcher.matchPattern('?s', '?p', '?o', [filter]);
      expect(results.length).toBe(2);
    });

    it('should apply multiple filters (AND)', async () => {
      const filter1 = b => b.o && parseInt(b.o.value) > 10;
      const filter2 = b => b.o && parseInt(b.o.value) < 30;
      const results = await matcher.matchPattern('?s', '?p', '?o', [filter1, filter2]);
      expect(results.length).toBe(1);
      expect(results[0].o.value).toBe('20');
    });
  });

  describe('executeQuery - SELECT Queries', () => {
    beforeEach(() => {
      store.add(dataFactory.triple(nn(EX + 'alice'), nn(FOAF + 'knows'), nn(EX + 'bob')));
      store.add(dataFactory.triple(nn(EX + 'alice'), nn(FOAF + 'knows'), nn(EX + 'charlie')));
      store.add(dataFactory.triple(nn(EX + 'bob'), nn(FOAF + 'knows'), nn(EX + 'dave')));
      store.add(dataFactory.triple(nn(EX + 'alice'), nn(FOAF + 'name'), lit('Alice')));
    });

    it('should execute simple SELECT query', async () => {
      const results = await matcher.executeQuery(`
        SELECT ?s ?o WHERE {
          ?s <${FOAF}knows> ?o .
        }
      `);
      expect(results.length).toBe(3);
      expect(results[0]).toHaveProperty('s');
      expect(results[0]).toHaveProperty('o');
    });

    it('should execute SELECT * query', async () => {
      const results = await matcher.executeQuery(`
        SELECT * WHERE {
          ?s ?p ?o .
        }
      `);
      expect(results.length).toBe(4);
    });

    it('should project only requested variables', async () => {
      const results = await matcher.executeQuery(`
        SELECT ?s WHERE {
          ?s <${FOAF}knows> ?o .
        }
      `);
      expect(results.length).toBe(3);
      expect(results[0]).toHaveProperty('s');
      expect(results[0]).not.toHaveProperty('o');
    });

    it('should handle FILTER in query', async () => {
      store.add(dataFactory.triple(nn(EX + 'x'), nn(EX + 'val'), lit('100')));
      store.add(dataFactory.triple(nn(EX + 'y'), nn(EX + 'val'), lit('200')));

      const results = await matcher.executeQuery(`
        SELECT ?s ?v WHERE {
          ?s <${EX}val> ?v .
          FILTER(?v = "100")
        }
      `);
      expect(results.length).toBe(1);
    });

    it('should throw on empty query', async () => {
      await expect(matcher.executeQuery('')).rejects.toThrow('must be a non-empty string');
    });

    it('should throw on invalid query syntax', async () => {
      await expect(matcher.executeQuery('NOT VALID SPARQL')).rejects.toThrow('Failed to parse');
    });

    it('should cache query results', async () => {
      const query = `SELECT ?s WHERE { ?s <${FOAF}knows> ?o }`;

      const results1 = await matcher.executeQuery(query);
      const results2 = await matcher.executeQuery(query);

      expect(results1.length).toBe(results2.length);
      expect(matcher.getCacheStats().size).toBeGreaterThan(0);
    });
  });

  describe('compileToBeamPattern', () => {
    it('should compile variable pattern', () => {
      const pattern = matcher.compileToBeamPattern('?s', '?p', '?o');
      expect(pattern).toBe('{Var_s, Var_p, Var_o}');
    });

    it('should compile URI pattern', () => {
      const pattern = matcher.compileToBeamPattern(`<${EX}subject>`, '?p', '?o');
      expect(pattern).toContain('uri');
      expect(pattern).toContain(EX + 'subject');
    });

    it('should compile mixed pattern', () => {
      const pattern = matcher.compileToBeamPattern('?s', `<${RDF}type>`, `<${FOAF}Person>`);
      expect(pattern).toContain('Var_s');
      expect(pattern).toContain('uri');
    });

    it('should compile null as wildcard', () => {
      const pattern = matcher.compileToBeamPattern(null, null, null);
      expect(pattern).toBe('{_, _, _}');
    });

    it('should compile prefixed names', () => {
      const pattern = matcher.compileToBeamPattern('?s', 'rdf:type', 'foaf:Person');
      expect(pattern).toContain(RDF + 'type');
      expect(pattern).toContain(FOAF + 'Person');
    });
  });

  describe('Cache Management', () => {
    it('should clear cache', () => {
      matcher._queryCache.set('test', { results: [], timestamp: Date.now() });
      expect(matcher.getCacheStats().size).toBe(1);

      matcher.clearCache();
      expect(matcher.getCacheStats().size).toBe(0);
    });

    it('should report cache stats', () => {
      const stats = matcher.getCacheStats();
      expect(stats).toHaveProperty('size');
      expect(stats).toHaveProperty('maxSize');
      expect(stats.maxSize).toBe(100);
    });
  });

  describe('Error Handling', () => {
    it('should handle empty store gracefully', async () => {
      const emptyStore = createStore();
      const emptyMatcher = new SPARQLPatternMatcher(emptyStore);

      const results = await emptyMatcher.matchPattern('?s', '?p', '?o');
      expect(results).toEqual([]);
    });

    it('should handle malformed pattern terms', async () => {
      // Should not throw, just return no matches or handle gracefully
      const results = await matcher.matchPattern('invalid', 'also-invalid', '???');
      expect(Array.isArray(results)).toBe(true);
    });
  });

  describe('Performance Benchmarks', () => {
    const TRIPLE_COUNT = 1000;

    beforeEach(() => {
      // Add 1000 triples
      for (let i = 0; i < TRIPLE_COUNT; i++) {
        store.add(dataFactory.triple(
          nn(EX + `subject${i}`),
          nn(EX + 'predicate'),
          lit(`value${i}`)
        ));
      }
    });

    it('should match all triples in <10ms', async () => {
      const start = performance.now();
      const results = await matcher.matchPattern('?s', '?p', '?o');
      const duration = performance.now() - start;

      expect(results.length).toBe(TRIPLE_COUNT);
      expect(duration).toBeLessThan(10);
      console.log(`Pattern match (${TRIPLE_COUNT} triples): ${duration.toFixed(2)}ms`);
    });

    it('should execute SELECT query in <10ms', async () => {
      const start = performance.now();
      const results = await matcher.executeQuery(`
        SELECT ?s ?o WHERE {
          ?s <${EX}predicate> ?o .
        }
      `);
      const duration = performance.now() - start;

      expect(results.length).toBe(TRIPLE_COUNT);
      expect(duration).toBeLessThan(10);
      console.log(`SELECT query (${TRIPLE_COUNT} triples): ${duration.toFixed(2)}ms`);
    });

    it('should filter results efficiently', async () => {
      const filter = b => b.o && b.o.value.includes('500');

      const start = performance.now();
      const results = await matcher.matchPattern('?s', '?p', '?o', [filter]);
      const duration = performance.now() - start;

      expect(results.length).toBe(1); // Only value500
      expect(duration).toBeLessThan(10);
      console.log(`Filtered match (${TRIPLE_COUNT} triples): ${duration.toFixed(2)}ms`);
    });

    it('should compile BEAM patterns quickly', () => {
      const iterations = 1000;
      const start = performance.now();

      for (let i = 0; i < iterations; i++) {
        matcher.compileToBeamPattern('?s', `<${EX}p${i}>`, '?o');
      }

      const duration = performance.now() - start;
      expect(duration).toBeLessThan(50); // 1000 compilations in <50ms
      console.log(`BEAM compile (${iterations} patterns): ${duration.toFixed(2)}ms`);
    });

    it('should handle cached queries faster', async () => {
      const query = `SELECT ?s ?o WHERE { ?s <${EX}predicate> ?o }`;

      // First query (cold cache)
      const start1 = performance.now();
      await matcher.executeQuery(query);
      const cold = performance.now() - start1;

      // Second query (warm cache)
      const start2 = performance.now();
      await matcher.executeQuery(query);
      const warm = performance.now() - start2;

      console.log(`Cold query: ${cold.toFixed(2)}ms, Warm query: ${warm.toFixed(2)}ms`);
      // Cached should be at least as fast (usually faster)
      expect(warm).toBeLessThanOrEqual(cold + 1); // +1ms tolerance
    });
  });

  describe('Complex Query Patterns', () => {
    beforeEach(() => {
      // Build a small social network
      store.add(dataFactory.triple(nn(EX + 'alice'), nn(RDF + 'type'), nn(FOAF + 'Person')));
      store.add(dataFactory.triple(nn(EX + 'bob'), nn(RDF + 'type'), nn(FOAF + 'Person')));
      store.add(dataFactory.triple(nn(EX + 'alice'), nn(FOAF + 'knows'), nn(EX + 'bob')));
      store.add(dataFactory.triple(nn(EX + 'alice'), nn(FOAF + 'name'), lit('Alice')));
      store.add(dataFactory.triple(nn(EX + 'bob'), nn(FOAF + 'name'), lit('Bob')));
      store.add(dataFactory.triple(nn(EX + 'alice'), nn(FOAF + 'age'), lit('30')));
      store.add(dataFactory.triple(nn(EX + 'bob'), nn(FOAF + 'age'), lit('25')));
    });

    it('should find all Person types', async () => {
      const results = await matcher.matchPattern('?person', 'a', 'foaf:Person');
      expect(results.length).toBe(2);
    });

    it('should find who Alice knows', async () => {
      const results = await matcher.executeQuery(`
        SELECT ?friend WHERE {
          <${EX}alice> <${FOAF}knows> ?friend .
        }
      `);
      expect(results.length).toBe(1);
      expect(results[0].friend.value).toBe(EX + 'bob');
    });

    it('should find names via SELECT', async () => {
      const results = await matcher.executeQuery(`
        SELECT ?person ?name WHERE {
          ?person <${FOAF}name> ?name .
        }
      `);
      expect(results.length).toBe(2);
      const names = results.map(r => r.name.value).sort();
      expect(names).toEqual(['Alice', 'Bob']);
    });
  });
});
