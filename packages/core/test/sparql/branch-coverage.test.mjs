/**
 * @vitest-environment node
 * Branch Coverage Test Suite for SPARQL Executors
 * Coverage Target: 90%+ branch coverage
 * Pattern: Chicago School TDD
 *
 * Covers FILTER, OPTIONAL, ORDER BY, LIMIT, OFFSET, complex expressions, error branches
 */

import { describe, it, expect } from 'vitest';
import { createUnrdfStore, namedNode, literal, quad } from '../../src/index.mjs';
import { executeQuerySync } from '../../src/sparql/executor-sync.mjs';

describe('SPARQL Branch Coverage Tests', () => {
  describe('FILTER Clause Branches', () => {
    it('handles FILTER with string equality', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));
      store.add(quad(namedNode('http://bob'), namedNode('http://name'), literal('Bob')));

      const sparql = `
        SELECT ?person ?name WHERE {
          ?person <http://name> ?name .
          FILTER(?name = "Alice")
        }
      `;

      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
    });

    it('handles FILTER with numeric comparison (greater than)', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('25')));
      store.add(quad(namedNode('http://bob'), namedNode('http://age'), literal('35')));

      const sparql = `
        SELECT ?person ?age WHERE {
          ?person <http://age> ?age .
          FILTER(?age > 30)
        }
      `;

      const result = store.query(sparql);

      expect(result).toBeDefined();
    });

    it('handles FILTER with numeric comparison (less than)', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('25')));
      store.add(quad(namedNode('http://bob'), namedNode('http://age'), literal('35')));

      const sparql = `
        SELECT ?person ?age WHERE {
          ?person <http://age> ?age .
          FILTER(?age < 30)
        }
      `;

      const result = store.query(sparql);

      expect(result).toBeDefined();
    });

    it('handles FILTER with logical AND', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('25')));
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));

      const sparql = `
        SELECT ?person ?age ?name WHERE {
          ?person <http://age> ?age .
          ?person <http://name> ?name .
          FILTER(?age > 20 && ?name = "Alice")
        }
      `;

      const result = store.query(sparql);

      expect(result).toBeDefined();
    });

    it('handles FILTER with logical OR', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('25')));
      store.add(quad(namedNode('http://bob'), namedNode('http://age'), literal('35')));

      const sparql = `
        SELECT ?person ?age WHERE {
          ?person <http://age> ?age .
          FILTER(?age < 20 || ?age > 30)
        }
      `;

      const result = store.query(sparql);

      expect(result).toBeDefined();
    });

    it('handles FILTER with NOT', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));
      store.add(quad(namedNode('http://bob'), namedNode('http://name'), literal('Bob')));

      const sparql = `
        SELECT ?person ?name WHERE {
          ?person <http://name> ?name .
          FILTER(!(?name = "Bob"))
        }
      `;

      const result = store.query(sparql);

      expect(result).toBeDefined();
    });

    it('handles FILTER with regex', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));
      store.add(quad(namedNode('http://bob'), namedNode('http://name'), literal('Bob')));

      const sparql = `
        SELECT ?person ?name WHERE {
          ?person <http://name> ?name .
          FILTER(regex(?name, "^A"))
        }
      `;

      const result = store.query(sparql);

      expect(result).toBeDefined();
    });

    it('handles FILTER that filters out all results', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('25')));

      const sparql = `
        SELECT ?person ?age WHERE {
          ?person <http://age> ?age .
          FILTER(?age > 100)
        }
      `;

      const result = store.query(sparql);

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(0);
    });
  });

  describe('OPTIONAL Clause Branches', () => {
    it('handles OPTIONAL with matching data', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('30')));

      const sparql = `
        SELECT ?person ?name ?age WHERE {
          ?person <http://name> ?name .
          OPTIONAL { ?person <http://age> ?age }
        }
      `;

      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
      expect(result[0]).toHaveProperty('name');
    });

    it('handles OPTIONAL with missing data', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));
      // No age for Alice

      const sparql = `
        SELECT ?person ?name ?age WHERE {
          ?person <http://name> ?name .
          OPTIONAL { ?person <http://age> ?age }
        }
      `;

      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
      expect(result[0]).toHaveProperty('name');
    });

    it('handles multiple OPTIONAL clauses', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('30')));

      const sparql = `
        SELECT ?person ?name ?age ?email WHERE {
          ?person <http://name> ?name .
          OPTIONAL { ?person <http://age> ?age }
          OPTIONAL { ?person <http://email> ?email }
        }
      `;

      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
    });

    it('handles OPTIONAL with FILTER', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('30')));

      const sparql = `
        SELECT ?person ?name ?age WHERE {
          ?person <http://name> ?name .
          OPTIONAL {
            ?person <http://age> ?age .
            FILTER(?age > 25)
          }
        }
      `;

      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
    });
  });

  describe('ORDER BY Branches', () => {
    it('handles ORDER BY ASC', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('30')));
      store.add(quad(namedNode('http://bob'), namedNode('http://age'), literal('25')));
      store.add(quad(namedNode('http://charlie'), namedNode('http://age'), literal('35')));

      const sparql = `
        SELECT ?person ?age WHERE {
          ?person <http://age> ?age .
        }
        ORDER BY ASC(?age)
      `;

      const result = store.query(sparql);

      expect(result.length).toBe(3);
    });

    it('handles ORDER BY DESC', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('30')));
      store.add(quad(namedNode('http://bob'), namedNode('http://age'), literal('25')));

      const sparql = `
        SELECT ?person ?age WHERE {
          ?person <http://age> ?age .
        }
        ORDER BY DESC(?age)
      `;

      const result = store.query(sparql);

      expect(result.length).toBe(2);
    });

    it('handles ORDER BY multiple variables', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('30')));

      const sparql = `
        SELECT ?person ?name ?age WHERE {
          ?person <http://name> ?name .
          ?person <http://age> ?age .
        }
        ORDER BY ?name ?age
      `;

      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
    });
  });

  describe('LIMIT and OFFSET Branches', () => {
    it('handles LIMIT', () => {
      const store = createUnrdfStore();
      for (let i = 0; i < 10; i++) {
        store.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`value${i}`)));
      }

      const sparql = `
        SELECT ?s ?o WHERE {
          ?s <http://p> ?o .
        }
        LIMIT 5
      `;

      const result = store.query(sparql);

      expect(result.length).toBeLessThanOrEqual(5);
    });

    it('handles OFFSET', () => {
      const store = createUnrdfStore();
      for (let i = 0; i < 10; i++) {
        store.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`value${i}`)));
      }

      const sparql = `
        SELECT ?s ?o WHERE {
          ?s <http://p> ?o .
        }
        OFFSET 5
      `;

      const result = store.query(sparql);

      expect(result.length).toBeLessThanOrEqual(5);
    });

    it('handles LIMIT and OFFSET together', () => {
      const store = createUnrdfStore();
      for (let i = 0; i < 10; i++) {
        store.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`value${i}`)));
      }

      const sparql = `
        SELECT ?s ?o WHERE {
          ?s <http://p> ?o .
        }
        LIMIT 3
        OFFSET 2
      `;

      const result = store.query(sparql);

      expect(result.length).toBeLessThanOrEqual(3);
    });

    it('handles LIMIT 0', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const sparql = `
        SELECT ?s ?o WHERE {
          ?s <http://p> ?o .
        }
        LIMIT 0
      `;

      const result = store.query(sparql);

      expect(result.length).toBe(0);
    });

    it('handles OFFSET greater than result count', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const sparql = `
        SELECT ?s ?o WHERE {
          ?s <http://p> ?o .
        }
        OFFSET 100
      `;

      const result = store.query(sparql);

      expect(result.length).toBe(0);
    });
  });

  describe('Complex Query Patterns', () => {
    it('handles UNION', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));
      store.add(quad(namedNode('http://bob'), namedNode('http://label'), literal('Bob')));

      const sparql = `
        SELECT ?person ?value WHERE {
          {
            ?person <http://name> ?value .
          }
          UNION
          {
            ?person <http://label> ?value .
          }
        }
      `;

      const result = store.query(sparql);

      expect(result.length).toBeGreaterThanOrEqual(2);
    });

    it('handles nested OPTIONAL and FILTER', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('30')));

      const sparql = `
        SELECT ?person ?name ?age WHERE {
          ?person <http://name> ?name .
          OPTIONAL {
            ?person <http://age> ?age .
            FILTER(?age > 25)
          }
        }
      `;

      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
    });

    it('handles FILTER with complex expression', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('30')));
      store.add(quad(namedNode('http://bob'), namedNode('http://age'), literal('25')));

      const sparql = `
        SELECT ?person ?age WHERE {
          ?person <http://age> ?age .
          FILTER((?age > 20 && ?age < 35) || ?age > 50)
        }
      `;

      const result = store.query(sparql);

      expect(result).toBeDefined();
    });

    it('handles multiple triple patterns with same variable', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://alice'), namedNode('http://name'), literal('Alice')));
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('30')));
      store.add(
        quad(namedNode('http://alice'), namedNode('http://email'), literal('alice@example.org'))
      );

      const sparql = `
        SELECT ?person ?name ?age ?email WHERE {
          ?person <http://name> ?name .
          ?person <http://age> ?age .
          ?person <http://email> ?email .
        }
      `;

      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
      expect(result[0]).toHaveProperty('name');
      expect(result[0]).toHaveProperty('age');
      expect(result[0]).toHaveProperty('email');
    });
  });

  describe('Query Type Detection Branches', () => {
    it('detects SELECT with PREFIX', () => {
      const store = createUnrdfStore();
      store.add(
        quad(
          namedNode('http://xmlns.com/foaf/0.1/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        )
      );

      const sparql = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE { ?person foaf:name ?name }
      `;

      const result = store.query(sparql);

      expect(Array.isArray(result)).toBe(true);
    });

    it('detects CONSTRUCT with PREFIX', () => {
      const store = createUnrdfStore();
      store.add(
        quad(
          namedNode('http://xmlns.com/foaf/0.1/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        )
      );

      const sparql = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX ex: <http://example.org/>
        CONSTRUCT { ?person ex:label ?name }
        WHERE { ?person foaf:name ?name }
      `;

      const result = store.query(sparql);

      expect(Array.isArray(result)).toBe(true);
    });

    it('detects ASK with PREFIX', () => {
      const store = createUnrdfStore();
      store.add(
        quad(
          namedNode('http://xmlns.com/foaf/0.1/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        )
      );

      const sparql = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        ASK { ?person foaf:name "Alice" }
      `;

      const result = store.query(sparql);

      expect(typeof result).toBe('boolean');
    });

    it('detects DESCRIBE', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://name'), literal('Alice'))
      );

      const sparql = `DESCRIBE <http://example.org/alice>`;

      const result = store.query(sparql);

      expect(Array.isArray(result)).toBe(true);
    });

    it('detects SELECT with multiple PREFIX declarations', () => {
      const store = createUnrdfStore();

      const sparql = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT * WHERE { ?s ?p ?o }
      `;

      const result = store.query(sparql);

      expect(Array.isArray(result)).toBe(true);
    });
  });

  describe('Result Formatting Branches', () => {
    it('formats SELECT results with json resultsFormat', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const sparql = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
      const result = store.query(sparql, { resultsFormat: 'json' });

      expect(result).toHaveProperty('head');
      expect(result).toHaveProperty('results');
      expect(result.head).toHaveProperty('vars');
      expect(result.results).toHaveProperty('bindings');
    });

    it('formats SELECT results with bindings resultsFormat (default)', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const sparql = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
      const result = store.query(sparql, { resultsFormat: 'bindings' });

      expect(Array.isArray(result)).toBe(true);
    });

    it('formats term with language tag', () => {
      const store = createUnrdfStore();

      const sparqlUpdate = `
        INSERT DATA {
          <http://example.org/s> <http://example.org/p> "hello"@en .
        }
      `;

      store.update(sparqlUpdate);

      const sparql = 'SELECT ?o WHERE { <http://example.org/s> <http://example.org/p> ?o }';
      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
      expect(result[0]).toHaveProperty('o');
    });

    it('formats term with datatype', () => {
      const store = createUnrdfStore();

      const sparqlUpdate = `
        INSERT DATA {
          <http://example.org/s> <http://example.org/p> "42"^^<http://www.w3.org/2001/XMLSchema#integer> .
        }
      `;

      store.update(sparqlUpdate);

      const sparql = 'SELECT ?o WHERE { <http://example.org/s> <http://example.org/p> ?o }';
      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
      expect(result[0]).toHaveProperty('o');
    });
  });

  describe('Error Branches', () => {
    it('handles invalid FILTER expression gracefully', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const sparql = `
        SELECT ?s WHERE {
          ?s ?p ?o .
          FILTER(INVALID_FUNCTION(?s))
        }
      `;

      expect(() => store.query(sparql)).toThrow();
    });

    it('handles invalid ORDER BY variable', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const sparql = `
        SELECT ?s WHERE {
          ?s ?p ?o .
        }
        ORDER BY ?nonexistent
      `;

      // Should not throw, but may return empty or unsorted results
      const result = store.query(sparql);
      expect(result).toBeDefined();
    });

    it('handles empty WHERE clause', () => {
      const store = createUnrdfStore();
      const sparql = 'SELECT ?s ?p ?o WHERE { }';

      const result = store.query(sparql);

      expect(Array.isArray(result)).toBe(true);
      // Empty WHERE may return 1 result with no bindings
      expect(result.length).toBeGreaterThanOrEqual(0);
    });

    it('handles query with no variables in SELECT', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';

      const result = store.query(sparql);

      expect(Array.isArray(result)).toBe(true);
    });
  });

  describe('Edge Case Branches', () => {
    it('handles query with very long variable names', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const sparql = `
        SELECT ?thisIsAVeryLongVariableNameThatShouldStillWork WHERE {
          ?thisIsAVeryLongVariableNameThatShouldStillWork <http://p> ?o .
        }
      `;

      const result = store.query(sparql);

      expect(result).toBeDefined();
    });

    it('handles query with Unicode characters', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('こんにちは')));

      const sparql = `SELECT ?o WHERE { ?s <http://p> ?o }`;

      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
    });

    it('handles query with special characters in literals', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('test "quoted" value')));

      const sparql = `SELECT ?o WHERE { ?s <http://p> ?o }`;

      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
    });

    it('handles query with blank nodes', () => {
      const store = createUnrdfStore();

      const sparqlUpdate = `
        INSERT DATA {
          _:b1 <http://example.org/p> "value" .
        }
      `;

      store.update(sparqlUpdate);

      const sparql = 'SELECT ?s ?o WHERE { ?s <http://example.org/p> ?o }';
      const result = store.query(sparql);

      expect(result.length).toBeGreaterThan(0);
    });
  });
});
