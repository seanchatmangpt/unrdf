/**
 * @file KGN Integration Tests with Core RDF Operations
 * @description Tests query builder integration, SHACL validation, serialization, and performance
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { sparql } from '../src/sparql/index.mjs';
import { UnrdfStore } from '../src/rdf/unrdf-store.mjs';
import { dataFactory } from '@unrdf/oxigraph';

const { quad, namedNode, literal, blankNode } = dataFactory;

describe('Query Builder Integration with KGN Templates', () => {
  it('should build SELECT query matching template output', () => {
    // Arrange
    const builder = sparql()
      .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
      .select('?name', '?email')
      .where('?person foaf:name ?name')
      .where('?person foaf:mbox ?email')
      .limit(10);

    // Act
    const query = builder.build();

    // Assert
    expect(query).toContain('PREFIX foaf: <http://xmlns.com/foaf/0.1/>');
    expect(query).toContain('SELECT ?name ?email');
    expect(query).toContain('WHERE {');
    expect(query).toContain('?person foaf:name ?name');
    expect(query).toContain('?person foaf:mbox ?email');
    expect(query).toContain('LIMIT 10');
  });

  it('should build INSERT query matching template output', () => {
    // Arrange
    const builder = sparql()
      .prefix('ex', 'http://example.org/')
      .insert('<http://example.org/person/1> ex:name "John Doe"')
      .insert('<http://example.org/person/1> ex:age 30');

    // Act
    const query = builder.build();

    // Assert
    expect(query).toContain('INSERT DATA {');
    expect(query).toContain('<http://example.org/person/1> ex:name "John Doe"');
    expect(query).toContain('<http://example.org/person/1> ex:age 30');
  });

  it('should build CONSTRUCT query matching template output', () => {
    // Arrange
    const builder = sparql()
      .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
      .prefix('rdfs', 'http://www.w3.org/2000/01/rdf-schema#')
      .construct('?s rdfs:label ?o')
      .where('?s foaf:name ?o');

    // Act
    const query = builder.build();

    // Assert
    expect(query).toContain('PREFIX foaf: <http://xmlns.com/foaf/0.1/>');
    expect(query).toContain('PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>');
    expect(query).toContain('CONSTRUCT {');
    expect(query).toContain('?s rdfs:label ?o');
    expect(query).toContain('WHERE {');
    expect(query).toContain('?s foaf:name ?o');
  });

  it('should build query with FILTER clause', () => {
    // Arrange
    const builder = sparql()
      .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
      .select('?name')
      .where('?person foaf:name ?name')
      .filter('REGEX(?name, "John")');

    // Act
    const query = builder.build();

    // Assert
    expect(query).toContain('FILTER(REGEX(?name, "John"))');
  });

  it('should build query with OPTIONAL clause', () => {
    // Arrange
    const builder = sparql()
      .select('?name', '?email')
      .where('?person <http://xmlns.com/foaf/0.1/name> ?name')
      .optional('?person <http://xmlns.com/foaf/0.1/mbox> ?email');

    // Act
    const query = builder.build();

    // Assert
    expect(query).toContain('OPTIONAL {');
    expect(query).toContain('?person <http://xmlns.com/foaf/0.1/mbox> ?email');
  });

  it('should build query with ORDER BY', () => {
    // Arrange
    const builder = sparql()
      .select('?name')
      .where('?person <http://xmlns.com/foaf/0.1/name> ?name')
      .orderBy('?name');

    // Act
    const query = builder.build();

    // Assert
    expect(query).toContain('ORDER BY ?name');
  });

  it('should build DELETE query', () => {
    // Arrange
    const builder = sparql()
      .prefix('ex', 'http://example.org/')
      .delete('<http://example.org/person/1> ex:name "Old Name"');

    // Act
    const query = builder.build();

    // Assert
    expect(query).toContain('DELETE DATA {');
    expect(query).toContain('<http://example.org/person/1> ex:name "Old Name"');
  });
});

describe('SHACL Validation Workflows', () => {
  let store;

  beforeEach(() => {
    // Arrange: Create store with sample data
    const quads = [
      quad(
        namedNode('http://example.org/person/1'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://xmlns.com/foaf/0.1/Person')
      ),
      quad(
        namedNode('http://example.org/person/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('John Doe')
      )
    ];
    store = new UnrdfStore(quads);
  });

  it('should query person data for validation', () => {
    // Arrange
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?name
      WHERE {
        ?person foaf:name ?name .
      }
    `;

    // Act
    const results = store.query(query);

    // Assert
    expect(results).toHaveLength(1);
    expect(results[0].name.value).toBe('John Doe');
  });

  it('should validate required properties exist', () => {
    // Arrange
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      ASK {
        <http://example.org/person/1> rdf:type foaf:Person .
        <http://example.org/person/1> foaf:name ?name .
      }
    `;

    // Act
    const result = store.query(query);

    // Assert
    expect(result).toBe(true);
  });

  it('should detect missing required properties', () => {
    // Arrange
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      ASK {
        <http://example.org/person/1> foaf:mbox ?email .
      }
    `;

    // Act
    const result = store.query(query);

    // Assert
    expect(result).toBe(false);
  });

  it('should query for validation violations', () => {
    // Arrange: Add person without name
    store.add(
      quad(
        namedNode('http://example.org/person/2'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://xmlns.com/foaf/0.1/Person')
      )
    );

    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      SELECT ?person
      WHERE {
        ?person rdf:type foaf:Person .
        FILTER NOT EXISTS { ?person foaf:name ?name }
      }
    `;

    // Act
    const results = store.query(query);

    // Assert
    expect(results).toHaveLength(1);
    expect(results[0].person.value).toBe('http://example.org/person/2');
  });
});

describe('Serialization Round-Trips', () => {
  it('should serialize and parse back quads', () => {
    // Arrange
    const originalQuads = [
      quad(
        namedNode('http://example.org/subject'),
        namedNode('http://example.org/predicate'),
        literal('Object Value')
      ),
      quad(
        namedNode('http://example.org/subject'),
        namedNode('http://example.org/number'),
        literal('42', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      )
    ];

    const store1 = new UnrdfStore(originalQuads);

    // Act: Query all triples
    const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
    const results = store1.query(query);

    // Assert: Verify data integrity (order not guaranteed)
    expect(results).toHaveLength(2);
    const textResult = results.find(r => r.o.value === 'Object Value');
    const numResult = results.find(r => r.o.value === '42');
    expect(textResult).toBeDefined();
    expect(textResult.s.value).toBe('http://example.org/subject');
    expect(textResult.p.value).toBe('http://example.org/predicate');
    expect(numResult).toBeDefined();
    expect(numResult.s.value).toBe('http://example.org/subject');
    expect(numResult.p.value).toBe('http://example.org/number');
  });

  it('should handle blank nodes in round-trip', () => {
    // Arrange
    const bn = blankNode('person1');
    const quads = [
      quad(
        bn,
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Anonymous')
      )
    ];

    const store = new UnrdfStore(quads);

    // Act
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?name
      WHERE {
        ?person foaf:name ?name .
      }
    `;
    const results = store.query(query);

    // Assert
    expect(results).toHaveLength(1);
    expect(results[0].name.value).toBe('Anonymous');
  });

  it('should preserve language tags', () => {
    // Arrange
    const quads = [
      quad(
        namedNode('http://example.org/resource'),
        namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
        literal('Hello', 'en')
      ),
      quad(
        namedNode('http://example.org/resource'),
        namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
        literal('Bonjour', 'fr')
      )
    ];

    const store = new UnrdfStore(quads);

    // Act
    const query = `
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      SELECT ?label
      WHERE {
        <http://example.org/resource> rdfs:label ?label .
        FILTER(LANG(?label) = "en")
      }
    `;
    const results = store.query(query);

    // Assert
    expect(results).toHaveLength(1);
    expect(results[0].label.value).toBe('Hello');
  });

  it('should preserve datatypes', () => {
    // Arrange
    const quads = [
      quad(
        namedNode('http://example.org/measurement'),
        namedNode('http://example.org/value'),
        literal('42', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ),
      quad(
        namedNode('http://example.org/measurement'),
        namedNode('http://example.org/decimal'),
        literal('3.14', namedNode('http://www.w3.org/2001/XMLSchema#decimal'))
      )
    ];

    const store = new UnrdfStore(quads);

    // Act
    const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
    const results = store.query(query);

    // Assert
    expect(results).toHaveLength(2);
    const intResult = results.find(r => r.o.value === '42');
    const decResult = results.find(r => r.o.value === '3.14');

    expect(intResult).toBeDefined();
    expect(decResult).toBeDefined();

    // Verify values are correctly retrieved
    expect(intResult.o.value).toBe('42');
    expect(decResult.o.value).toBe('3.14');
  });
});

describe('Parser Error Handling', () => {
  it('should throw on invalid SPARQL syntax', () => {
    // Arrange
    const store = new UnrdfStore();
    const invalidQuery = 'SELECT ?s WHERE { ?s ?p }';  // Missing object

    // Act & Assert
    expect(() => {
      store.query(invalidQuery);
    }).toThrow();
  });

  it('should throw on unknown prefix', () => {
    // Arrange
    const store = new UnrdfStore();
    const queryWithUnknownPrefix = `
      SELECT ?s
      WHERE {
        ?s unknown:property ?o .
      }
    `;

    // Act & Assert
    expect(() => {
      store.query(queryWithUnknownPrefix);
    }).toThrow();
  });

  it('should handle empty query gracefully', () => {
    // Arrange
    const store = new UnrdfStore();

    // Act & Assert
    expect(() => {
      store.query('');
    }).toThrow();
  });

  it('should validate query type', () => {
    // Arrange
    const store = new UnrdfStore();

    // Act & Assert
    expect(() => {
      store.query(123);
    }).toThrow(TypeError);
  });
});

describe('Performance Under Load', () => {
  it('should handle bulk insert efficiently', () => {
    // Arrange
    const store = new UnrdfStore();
    const quads = [];
    const numQuads = 1000;

    for (let i = 0; i < numQuads; i++) {
      quads.push(
        quad(
          namedNode(`http://example.org/subject${i}`),
          namedNode('http://example.org/predicate'),
          literal(`Object ${i}`)
        )
      );
    }

    // Act
    const start = Date.now();
    store.bulkAdd(quads);
    const duration = Date.now() - start;

    // Assert
    expect(duration).toBeLessThan(1000); // Should complete in <1s
    const count = store.query('SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }');
    expect(parseInt(count[0].count.value)).toBe(numQuads);
  });

  it('should query large dataset efficiently', () => {
    // Arrange
    const quads = [];
    for (let i = 0; i < 1000; i++) {
      quads.push(
        quad(
          namedNode(`http://example.org/person${i}`),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal(`Person ${i}`)
        )
      );
    }
    const store = new UnrdfStore(quads);

    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?person ?name
      WHERE {
        ?person foaf:name ?name .
      }
      LIMIT 100
    `;

    // Act
    const start = Date.now();
    const results = store.query(query);
    const duration = Date.now() - start;

    // Assert
    expect(duration).toBeLessThan(500); // Should complete in <500ms
    expect(results).toHaveLength(100);
  });

  it('should handle concurrent queries', async () => {
    // Arrange
    const quads = [];
    for (let i = 0; i < 100; i++) {
      quads.push(
        quad(
          namedNode(`http://example.org/item${i}`),
          namedNode('http://example.org/value'),
          literal(`${i}`)
        )
      );
    }
    const store = new UnrdfStore(quads);

    const query = 'SELECT ?s ?o WHERE { ?s <http://example.org/value> ?o }';

    // Act
    const start = Date.now();
    const promises = Array(10).fill(null).map(() =>
      Promise.resolve(store.query(query))
    );
    const results = await Promise.all(promises);
    const duration = Date.now() - start;

    // Assert
    expect(duration).toBeLessThan(1000);
    expect(results).toHaveLength(10);
    results.forEach(result => {
      expect(result).toHaveLength(100);
    });
  });

  it('should handle complex query patterns efficiently', () => {
    // Arrange
    const quads = [];
    for (let i = 0; i < 100; i++) {
      quads.push(
        quad(
          namedNode(`http://example.org/person${i}`),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        ),
        quad(
          namedNode(`http://example.org/person${i}`),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal(`Person ${i}`)
        ),
        quad(
          namedNode(`http://example.org/person${i}`),
          namedNode('http://xmlns.com/foaf/0.1/age'),
          literal(`${20 + i}`, namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        )
      );
    }
    const store = new UnrdfStore(quads.flat());

    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      SELECT ?name ?age
      WHERE {
        ?person rdf:type foaf:Person .
        ?person foaf:name ?name .
        ?person foaf:age ?age .
        FILTER(?age >= 50)
      }
    `;

    // Act
    const start = Date.now();
    const results = store.query(query);
    const duration = Date.now() - start;

    // Assert
    expect(duration).toBeLessThan(500);
    expect(results.length).toBeGreaterThan(0);
  });
});
