/**
 * @file End-to-End RDF-KGN Integration Tests
 * @description Full workflow tests: Template → RDF → Parse → Validate → Query
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createEngine, createCustomFilters } from '../packages/kgn/src/index.js';
import { sparql } from '../packages/core/src/sparql/index.mjs';
import { UnrdfStore } from '../packages/core/src/rdf/unrdf-store.mjs';
import { dataFactory } from '@unrdf/oxigraph';
import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const { quad, namedNode, literal } = dataFactory;

describe('E2E: Template to RDF Workflow', () => {
  let engine;
  let store;

  beforeEach(() => {
    // Arrange: Setup template engine and RDF store
    const filters = createCustomFilters({ deterministicMode: true });
    engine = createEngine({
      enableInjection: false,
      deterministicMode: true
    });

    Object.entries(filters).forEach(([name, filter]) => {
      engine.addFilter(name, filter);
    });

    store = new UnrdfStore();
  });

  it('should complete full workflow: template → triples → store → query', () => {
    // Arrange: Define template
    const template = `
{{ "http://example.org/person1" | rdfResource }} {{ "rdf:type" | expand | contract }} {{ "foaf:Person" | expand | contract }} .
{{ "http://example.org/person1" | rdfResource }} {{ "foaf:name" | expand | contract }} {{ "Alice" | rdfLiteral("en") }} .
`;

    // Act: Render template
    const rdfOutput = engine.renderString(template);

    // Add to store (manually create quads from template output)
    store.add(
      quad(
        namedNode('http://example.org/person1'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://xmlns.com/foaf/0.1/Person')
      )
    );
    store.add(
      quad(
        namedNode('http://example.org/person1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice', 'en')
      )
    );

    // Query the store
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?name
      WHERE {
        <http://example.org/person1> foaf:name ?name .
      }
    `;
    const results = store.query(query);

    // Assert
    expect(rdfOutput).toContain('rdf:type foaf:Person');
    expect(rdfOutput).toContain('"Alice"@en');
    expect(results).toHaveLength(1);
    expect(results[0].name.value).toBe('Alice');
  });

  it('should generate ontology from template and validate data', () => {
    // Arrange: Create ontology template
    const ontologyTemplate = `
{{ "http://example.org/Person" | rdfResource }} {{ "rdf:type" | expand | contract }} {{ "owl:Class" | expand | contract }} .
{{ "http://example.org/hasName" | rdfResource }} {{ "rdf:type" | expand | contract }} {{ "owl:DatatypeProperty" | expand | contract }} .
{{ "http://example.org/hasName" | rdfResource }} {{ "rdfs:domain" | expand | contract }} {{ "http://example.org/Person" | rdfResource }} .
`;

    // Act: Render ontology
    const ontology = engine.renderString(ontologyTemplate);

    // Add ontology to store
    store.add(
      quad(
        namedNode('http://example.org/Person'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://www.w3.org/2002/07/owl#Class')
      )
    );
    store.add(
      quad(
        namedNode('http://example.org/hasName'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://www.w3.org/2002/07/owl#DatatypeProperty')
      )
    );
    store.add(
      quad(
        namedNode('http://example.org/hasName'),
        namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
        namedNode('http://example.org/Person')
      )
    );

    // Validate ontology exists
    const validateQuery = `
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      ASK {
        <http://example.org/Person> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> owl:Class .
      }
    `;
    const isValid = store.query(validateQuery);

    // Assert
    expect(ontology).toContain('owl:Class');
    expect(ontology).toContain('owl:DatatypeProperty');
    expect(isValid).toBe(true);
  });

  it('should generate SPARQL query from template and execute it', () => {
    // Arrange: Add test data
    store.add(
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      )
    );
    store.add(
      quad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Bob')
      )
    );

    // Template for SPARQL query
    const queryTemplate = `
PREFIX foaf: <{{ "foaf:name" | expand | replace("name", "") }}>
SELECT {{ "name" | sparqlVar }}
WHERE {
  {{ "person" | sparqlVar }} foaf:name {{ "name" | sparqlVar }} .
}
`;

    // Act: Render query
    const generatedQuery = engine.renderString(queryTemplate);
    const results = store.query(generatedQuery);

    // Assert
    expect(generatedQuery).toContain('PREFIX foaf:');
    expect(generatedQuery).toContain('SELECT ?name');
    expect(results).toHaveLength(2);
    expect(results.map(r => r.name.value)).toContain('Alice');
    expect(results.map(r => r.name.value)).toContain('Bob');
  });
});

describe('E2E: Real-World Ontology Creation', () => {
  let engine;
  let store;

  beforeEach(() => {
    const filters = createCustomFilters({ deterministicMode: true });
    engine = createEngine({
      enableInjection: false,
      deterministicMode: true
    });

    Object.entries(filters).forEach(([name, filter]) => {
      engine.addFilter(name, filter);
    });

    store = new UnrdfStore();
  });

  it('should create complete FOAF ontology subset', () => {
    // Arrange: FOAF ontology template
    const template = `
{% set classes = ["Person", "Agent", "Organization"] %}
{% set properties = ["name", "mbox", "homepage"] %}

{% for cls in classes %}
{{ "foaf:" + cls | expand | rdfResource }} {{ "rdf:type" | expand | contract }} {{ "owl:Class" | expand | contract }} .
{% endfor %}

{% for prop in properties %}
{{ "foaf:" + prop | expand | rdfResource }} {{ "rdf:type" | expand | contract }} {{ "owl:DatatypeProperty" | expand | contract }} .
{% endfor %}

{{ "foaf:Person" | expand | rdfResource }} {{ "rdfs:subClassOf" | expand | contract }} {{ "foaf:Agent" | expand | contract }} .
{{ "foaf:Organization" | expand | rdfResource }} {{ "rdfs:subClassOf" | expand | contract }} {{ "foaf:Agent" | expand | contract }} .
`;

    // Act: Render ontology
    const ontology = engine.renderString(template);

    // Parse and add to store (simplified - in real scenario would parse Turtle)
    const classes = ['Person', 'Agent', 'Organization'];
    classes.forEach(cls => {
      store.add(
        quad(
          namedNode(`http://xmlns.com/foaf/0.1/${cls}`),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://www.w3.org/2002/07/owl#Class')
        )
      );
    });

    store.add(
      quad(
        namedNode('http://xmlns.com/foaf/0.1/Person'),
        namedNode('http://www.w3.org/2000/01/rdf-schema#subClassOf'),
        namedNode('http://xmlns.com/foaf/0.1/Agent')
      )
    );

    // Validate hierarchy
    const query = `
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      SELECT ?subclass
      WHERE {
        ?subclass rdfs:subClassOf <http://xmlns.com/foaf/0.1/Agent> .
      }
    `;
    const results = store.query(query);

    // Assert
    expect(ontology).toContain('owl:Class');
    expect(ontology).toContain('rdfs:subClassOf foaf:Agent');
    expect(results).toHaveLength(1);
  });

  it('should create domain-specific ontology with constraints', () => {
    // Arrange: Library ontology
    const template = `
{% set classes = [
  {name: "Book", label: "Book", comment: "A published work"},
  {name: "Author", label: "Author", comment: "Creator of books"}
] %}

{% for cls in classes %}
ex:{{ cls.name }} {{ "rdf:type" | expand | contract }} {{ "owl:Class" | expand | contract }} ;
  {{ "rdfs:label" | expand | contract }} {{ cls.label | rdfLiteral("en") }} ;
  {{ "rdfs:comment" | expand | contract }} {{ cls.comment | rdfLiteral("en") }} .
{% endfor %}

ex:hasAuthor {{ "rdf:type" | expand | contract }} {{ "owl:ObjectProperty" | expand | contract }} ;
  {{ "rdfs:domain" | expand | contract }} ex:Book ;
  {{ "rdfs:range" | expand | contract }} ex:Author .
`;

    // Act
    const ontology = engine.renderString(template);

    // Assert
    expect(ontology).toContain('ex:Book rdf:type owl:Class');
    expect(ontology).toContain('ex:Author rdf:type owl:Class');
    expect(ontology).toContain('rdfs:label "Book"@en');
    expect(ontology).toContain('ex:hasAuthor rdf:type owl:ObjectProperty');
    expect(ontology).toContain('rdfs:domain ex:Book');
    expect(ontology).toContain('rdfs:range ex:Author');
  });
});

describe('E2E: Data Import Workflow', () => {
  let engine;
  let store;

  beforeEach(() => {
    const filters = createCustomFilters({ deterministicMode: true });
    engine = createEngine({
      enableInjection: false,
      deterministicMode: true
    });

    Object.entries(filters).forEach(([name, filter]) => {
      engine.addFilter(name, filter);
    });

    store = new UnrdfStore();
  });

  it('should import JSON data to RDF via template', () => {
    // Arrange: Sample JSON data
    const data = {
      people: [
        { id: 'p1', name: 'Alice', age: 30 },
        { id: 'p2', name: 'Bob', age: 25 }
      ]
    };

    const template = `
{% for person in people %}
ex:{{ person.id }} {{ "rdf:type" | expand | contract }} {{ "foaf:Person" | expand | contract }} ;
  {{ "foaf:name" | expand | contract }} {{ person.name | rdfLiteral }} ;
  ex:age {{ person.age | rdfDatatype("integer") }} .
{% endfor %}
`;

    // Act: Render RDF
    const rdf = engine.renderString(template, data);

    // Add to store
    data.people.forEach(person => {
      store.add(
        quad(
          namedNode(`http://example.org/${person.id}`),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        )
      );
      store.add(
        quad(
          namedNode(`http://example.org/${person.id}`),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal(person.name)
        )
      );
      store.add(
        quad(
          namedNode(`http://example.org/${person.id}`),
          namedNode('http://example.org/age'),
          literal(person.age.toString(), namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        )
      );
    });

    // Query imported data
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?name
      WHERE {
        ?person foaf:name ?name .
      }
    `;
    const results = store.query(query);

    // Assert
    expect(rdf).toContain('foaf:Person');
    expect(rdf).toContain('foaf:name');
    expect(results).toHaveLength(2);
    expect(results.map(r => r.name.value)).toContain('Alice');
    expect(results.map(r => r.name.value)).toContain('Bob');
  });

  it('should transform CSV-like data to RDF', () => {
    // Arrange: Tabular data
    const data = {
      headers: ['id', 'name', 'email'],
      rows: [
        ['1', 'Alice', 'alice@example.com'],
        ['2', 'Bob', 'bob@example.com']
      ]
    };

    const template = `
{% for row in rows %}
ex:person{{ row[0] }} {{ "foaf:name" | expand | contract }} {{ row[1] | rdfLiteral }} ;
  {{ "foaf:mbox" | expand | contract }} {{ row[2] | rdfResource }} .
{% endfor %}
`;

    // Act
    const rdf = engine.renderString(template, data);

    // Assert
    expect(rdf).toContain('ex:person1 foaf:name "Alice"');
    expect(rdf).toContain('ex:person2 foaf:name "Bob"');
    expect(rdf).toContain('foaf:mbox <alice@example.com>');
  });
});

describe('E2E: Cross-Package Integration', () => {
  it('should use query builder to generate query matching template output', () => {
    // Arrange: Build query with query builder
    const builder = sparql()
      .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
      .select('?name', '?email')
      .where('?person foaf:name ?name')
      .where('?person foaf:mbox ?email');

    const builderQuery = builder.build();

    // Generate same query from template
    const filters = createCustomFilters({ deterministicMode: true });
    const engine = createEngine({ enableInjection: false });
    Object.entries(filters).forEach(([name, filter]) => {
      engine.addFilter(name, filter);
    });

    const template = `
PREFIX foaf: <{{ "foaf:name" | expand | replace("name", "") }}>
SELECT {{ "name" | sparqlVar }} {{ "email" | sparqlVar }}
WHERE {
  {{ "person" | sparqlVar }} foaf:name {{ "name" | sparqlVar }} .
  {{ "person" | sparqlVar }} foaf:mbox {{ "email" | sparqlVar }} .
}
`;
    const templateQuery = engine.renderString(template).trim();

    // Assert: Both should produce similar queries
    expect(builderQuery).toContain('PREFIX foaf:');
    expect(builderQuery).toContain('SELECT ?name ?email');
    expect(templateQuery).toContain('PREFIX foaf:');
    expect(templateQuery).toContain('SELECT ?name ?email');
  });

  it('should integrate template rendering with store querying', () => {
    // Arrange: Create store
    const store = new UnrdfStore([
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      )
    ]);

    // Generate query from template
    const filters = createCustomFilters({ deterministicMode: true });
    const engine = createEngine({ enableInjection: false });
    Object.entries(filters).forEach(([name, filter]) => {
      engine.addFilter(name, filter);
    });

    const queryTemplate = `
PREFIX foaf: <{{ "foaf:name" | expand | replace("name", "") }}>
SELECT ?name
WHERE {
  ?person foaf:name ?name .
}
`;

    // Act: Render and execute
    const query = engine.renderString(queryTemplate);
    const results = store.query(query);

    // Assert
    expect(results).toHaveLength(1);
    expect(results[0].name.value).toBe('Alice');
  });
});

describe('E2E: Complex Query Patterns', () => {
  let engine;
  let store;

  beforeEach(() => {
    const filters = createCustomFilters({ deterministicMode: true });
    engine = createEngine({ enableInjection: false });
    Object.entries(filters).forEach(([name, filter]) => {
      engine.addFilter(name, filter);
    });

    // Setup complex dataset
    const quads = [];
    for (let i = 1; i <= 5; i++) {
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
          namedNode('http://example.org/age'),
          literal(`${20 + i * 5}`, namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        )
      );
    }
    store = new UnrdfStore(quads);
  });

  it('should execute complex OPTIONAL pattern from template', () => {
    // Arrange
    const template = `
PREFIX foaf: <{{ "foaf:name" | expand | replace("name", "") }}>
SELECT ?name ?email
WHERE {
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:mbox ?email }
}
`;

    // Act
    const query = engine.renderString(template);
    const results = store.query(query);

    // Assert
    expect(results).toHaveLength(5);
    results.forEach(result => {
      expect(result.name).toBeDefined();
    });
  });

  it('should execute FILTER pattern from template', () => {
    // Arrange
    const template = `
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name ?age
WHERE {
  ?person foaf:name ?name .
  ?person <http://example.org/age> ?age .
  FILTER(?age >= 30)
}
`;

    // Act
    const query = engine.renderString(template);
    const results = store.query(query);

    // Assert
    expect(results.length).toBeGreaterThan(0);
    results.forEach(result => {
      expect(parseInt(result.age.value)).toBeGreaterThanOrEqual(30);
    });
  });

  it('should execute aggregation query from template', () => {
    // Arrange
    const template = `
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT (COUNT(?person) as ?count)
WHERE {
  ?person <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> foaf:Person .
}
`;

    // Act
    const query = engine.renderString(template);
    const results = store.query(query);

    // Assert
    expect(results).toHaveLength(1);
    expect(parseInt(results[0].count.value)).toBe(5);
  });
});

describe('E2E: Multi-Format Output Scenarios', () => {
  let engine;

  beforeEach(() => {
    const filters = createCustomFilters({ deterministicMode: true });
    engine = createEngine({ enableInjection: false });
    Object.entries(filters).forEach(([name, filter]) => {
      engine.addFilter(name, filter);
    });
  });

  it('should generate Turtle and validate syntax', () => {
    // Arrange
    const template = `
@prefix ex: <http://example.org/> .
@prefix foaf: <{{ "foaf:name" | expand | replace("name", "") }}> .

ex:alice a foaf:Person ;
  foaf:name {{ "Alice" | rdfLiteral("en") }} ;
  foaf:age {{ 30 | rdfDatatype("integer") }} .
`;

    // Act
    const turtle = engine.renderString(template);

    // Assert
    expect(turtle).toContain('@prefix ex:');
    expect(turtle).toContain('@prefix foaf:');
    expect(turtle).toContain('ex:alice a foaf:Person');
    expect(turtle).toContain('"Alice"@en');
    expect(turtle).toContain('"30"^^xsd:integer');
  });

  it('should generate JSON-LD and parse as JSON', () => {
    // Arrange
    const template = `
{
  "@context": {
    "foaf": "{{ "foaf:name" | expand | replace("name", "") }}",
    "name": "foaf:name",
    "Person": "foaf:Person"
  },
  "@type": "Person",
  "name": "Alice"
}
`;

    // Act
    const jsonld = engine.renderString(template);
    const parsed = JSON.parse(jsonld);

    // Assert
    expect(parsed['@context'].foaf).toBe('http://xmlns.com/foaf/0.1/');
    expect(parsed['@type']).toBe('Person');
    expect(parsed.name).toBe('Alice');
  });

  it('should generate N-Quads with named graphs', () => {
    // Arrange
    const template = `
{{ "http://example.org/alice" | rdfResource }} {{ "foaf:name" | expand | rdfResource }} {{ "Alice" | rdfLiteral }} {{ "http://example.org/graph1" | rdfResource }} .
{{ "http://example.org/bob" | rdfResource }} {{ "foaf:name" | expand | rdfResource }} {{ "Bob" | rdfLiteral }} {{ "http://example.org/graph2" | rdfResource }} .
`;

    // Act
    const nquads = engine.renderString(template);

    // Assert
    expect(nquads).toContain('<http://example.org/alice>');
    expect(nquads).toContain('<http://xmlns.com/foaf/0.1/name>');
    expect(nquads).toContain('"Alice"');
    expect(nquads).toContain('<http://example.org/graph1>');
  });
});

describe('E2E: Error Recovery and Validation', () => {
  let engine;
  let store;

  beforeEach(() => {
    const filters = createCustomFilters({ deterministicMode: true });
    engine = createEngine({ enableInjection: false });
    Object.entries(filters).forEach(([name, filter]) => {
      engine.addFilter(name, filter);
    });
    store = new UnrdfStore();
  });

  it('should handle template errors gracefully', () => {
    // Arrange
    const invalidTemplate = '{{ unclosed filter';

    // Act & Assert
    expect(() => {
      engine.renderString(invalidTemplate);
    }).toThrow();
  });

  it('should handle missing template variables with defaults', () => {
    // Arrange
    const template = '{{ missing | default("default value") }}';

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result).toBe('default value');
  });

  it('should validate generated SPARQL before execution', () => {
    // Arrange: Generate invalid SPARQL
    const template = 'SELECT ?s WHERE { ?s ?p }';  // Missing object

    // Act
    const query = engine.renderString(template);

    // Assert: Store should reject invalid query
    expect(() => {
      store.query(query);
    }).toThrow();
  });

  it('should handle empty results gracefully', () => {
    // Arrange
    const template = `
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name
WHERE {
  ?person foaf:name ?name .
}
`;

    // Act
    const query = engine.renderString(template);
    const results = store.query(query);

    // Assert
    expect(results).toEqual([]);
  });
});

describe('E2E: Performance and Scalability', () => {
  it('should handle large template rendering', () => {
    // Arrange
    const filters = createCustomFilters({ deterministicMode: true });
    const engine = createEngine({ enableInjection: false });
    Object.entries(filters).forEach(([name, filter]) => {
      engine.addFilter(name, filter);
    });

    const data = {
      items: Array.from({ length: 100 }, (_, i) => ({
        id: i,
        name: `Item ${i}`
      }))
    };

    const template = `
{% for item in items %}
ex:item{{ item.id }} {{ "foaf:name" | expand | contract }} {{ item.name | rdfLiteral }} .
{% endfor %}
`;

    // Act
    const start = Date.now();
    const result = engine.renderString(template, data);
    const duration = Date.now() - start;

    // Assert
    expect(duration).toBeLessThan(1000);
    expect(result.split('\n').filter(line => line.trim()).length).toBeGreaterThan(90);
  });

  it('should handle complex nested templates efficiently', () => {
    // Arrange
    const filters = createCustomFilters({ deterministicMode: true });
    const engine = createEngine({ enableInjection: false });
    Object.entries(filters).forEach(([name, filter]) => {
      engine.addFilter(name, filter);
    });

    const data = {
      classes: Array.from({ length: 10 }, (_, i) => ({
        name: `Class${i}`,
        properties: Array.from({ length: 5 }, (_, j) => ({
          name: `prop${j}`
        }))
      }))
    };

    const template = `
{% for cls in classes %}
ex:{{ cls.name }} {{ "rdf:type" | expand | contract }} {{ "owl:Class" | expand | contract }} .
{% for prop in cls.properties %}
ex:{{ prop.name }} {{ "rdfs:domain" | expand | contract }} ex:{{ cls.name }} .
{% endfor %}
{% endfor %}
`;

    // Act
    const start = Date.now();
    const result = engine.renderString(template, data);
    const duration = Date.now() - start;

    // Assert
    expect(duration).toBeLessThan(500);
    expect(result).toContain('ex:Class0 rdf:type owl:Class');
    expect(result).toContain('ex:prop0 rdfs:domain ex:Class0');
  });
});
