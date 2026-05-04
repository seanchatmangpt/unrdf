/**
 * @file RDF Integration Tests for KGN Templates
 * @description Tests template rendering with RDF data, custom filters, SPARQL generation, and multi-format output
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createEngine, createCustomFilters } from '../src/index.js';

describe('RDF Template Rendering', () => {
  let engine;

  beforeEach(() => {
    // Arrange: Create engine with RDF filters (filters auto-added in constructor)
    engine = createEngine({
      enableInjection: false,
      deterministicMode: true
    });
  });

  it('should render template with RDF prefix expansion', () => {
    // Arrange
    const template = '{{ "foaf:name" | expand }}';

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result.content.trim()).toBe('http://xmlns.com/foaf/0.1/name');
  });

  it('should render template with custom RDF filters', () => {
    // Arrange
    const template = '{{ "Alice" | rdfLiteral("en") }}';

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result.content.trim()).toBe('"Alice"@en');
  });

  it('should render RDF literal with datatype', () => {
    // Arrange
    const template = '{{ 42 | rdfDatatype("integer") }}';

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result.content.trim()).toBe('"42"^^xsd:integer');
  });

  it('should render SPARQL variables', () => {
    // Arrange
    const template = '{{ "subject" | sparqlVar }} {{ "predicate" | sparqlVar }}';

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result.content.trim()).toBe('?subject ?predicate');
  });

  it('should render RDF list', () => {
    // Arrange
    const items = ["item1", "item2", "item3"];
    const template = '{{ items | rdfList }}';

    // Act
    const result = engine.renderString(template, { items });

    // Assert
    expect(result.content.trim()).toBe('( item1 item2 item3 )');
  });

  it('should render blank nodes', () => {
    // Arrange
    const template = '{{ "person1" | blankNode }}';

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result.content.trim()).toMatch(/_:person1/);
  });

  it('should handle hash filter for deterministic IDs', () => {
    // Arrange
    const template = '{{ "content" | hash }}';

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result.content.trim()).toMatch(/^[a-f0-9]{64}$/);
  });

  it('should create short hash for identifiers', () => {
    // Arrange
    const template = '{{ "content" | shortHash(8) }}';

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result.content.trim()).toMatch(/^[a-f0-9]{8}$/);
  });

  it('should handle empty RDF list', () => {
    // Arrange
    const items = [];
    const template = '{{ items | rdfList }}';

    // Act
    const result = engine.renderString(template, { items });

    // Assert
    expect(result.content.trim()).toBe('rdf:nil');
  });

  it('should handle missing variables gracefully', () => {
    // Arrange
    const template = '{{ missingVar | default("fallback") }}';

    // Act - Disable validation
    const result = engine.renderString(template, {}, { validateVars: false });

    // Assert
    expect(result.success).toBe(true);
    expect(result.content.trim()).toBe('fallback');
  });
});

describe('SPARQL Generation from Templates', () => {
  let engine;

  beforeEach(() => {
    // Arrange: Engine auto-includes filters
    engine = createEngine({
      enableInjection: false,
      deterministicMode: true
    });
  });

  it('should generate SELECT query from template', () => {
    // Arrange
    const template = `
SELECT {{ "name" | sparqlVar }} {{ "email" | sparqlVar }}
WHERE {
  {{ "person" | sparqlVar }} {{ "foaf:name" | expand | contract }} {{ "name" | sparqlVar }} .
  {{ "person" | sparqlVar }} {{ "foaf:mbox" | expand | contract }} {{ "email" | sparqlVar }} .
}
`;

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result.content).toContain('SELECT ?name ?email');
    expect(result.content).toContain('WHERE {');
    expect(result.content).toContain('?person foaf:name ?name');
    expect(result.content).toContain('?person foaf:mbox ?email');
  });

  it('should generate INSERT query from template', () => {
    // Arrange
    const template = `
INSERT DATA {
  ex:person1 {{ "foaf:name" | expand | contract }} {{ "John Doe" | rdfLiteral("en") }} .
  ex:person1 ex:age {{ 30 | rdfDatatype("integer") }} .
}
`;

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result.content).toContain('INSERT DATA {');
    expect(result.content).toContain('"John Doe"@en');
    expect(result.content).toContain('"30"^^xsd:integer');
  });

  it('should generate parameterized query from template variables', () => {
    // Arrange
    const template = `
SELECT *
WHERE {
  {{ "s" | sparqlVar }} {{ "rdf:type" | expand | contract }} {{ typeUri | rdfResource }} .
}
LIMIT {{ limit }}
`;
    const context = {
      typeUri: 'http://xmlns.com/foaf/0.1/Person',
      limit: 10
    };

    // Act
    const result = engine.renderString(template, context);

    // Assert
    expect(result.content).toContain('?s rdf:type <http://xmlns.com/foaf/0.1/Person>');
    expect(result.content).toContain('LIMIT 10');
  });
});

describe('Ontology Generation via Templates', () => {
  let engine;

  beforeEach(() => {
    // Arrange: Engine auto-includes filters
    engine = createEngine({
      enableInjection: false,
      deterministicMode: true
    });
  });

  it('should generate OWL class definition', () => {
    // Arrange
    const template = `
ex:Person {{ "rdf:type" | expand | contract }} {{ "owl:Class" | expand | contract }} .
ex:Person {{ "rdfs:label" | expand | contract }} {{ "Person" | rdfLiteral("en") }} .
`;

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result.content).toContain('ex:Person rdf:type owl:Class');
    expect(result.content).toContain('rdfs:label "Person"@en');
  });

  it('should generate class hierarchy', () => {
    // Arrange
    const classes = ['Person', 'Student', 'Teacher'];
    const template = `
{% for cls in classes %}
ex:{{ cls }} {{ "rdf:type" | expand | contract }} {{ "owl:Class" | expand | contract }} .
{% endfor %}
ex:Student {{ "rdfs:subClassOf" | expand | contract }} ex:Person .
ex:Teacher {{ "rdfs:subClassOf" | expand | contract }} ex:Person .
`;

    // Act - Disable validation since cls is a loop variable
    const result = engine.renderString(template, { classes }, { validateVars: false });

    // Assert
    expect(result.success).toBe(true);
    expect(result.content).toBeDefined();
    expect(result.content).toContain('ex:Person rdf:type owl:Class');
    expect(result.content).toContain('ex:Student rdf:type owl:Class');
    expect(result.content).toContain('ex:Teacher rdf:type owl:Class');
    expect(result.content).toContain('ex:Student rdfs:subClassOf ex:Person');
    expect(result.content).toContain('ex:Teacher rdfs:subClassOf ex:Person');
  });
});

describe('Multi-Format Output', () => {
  let engine;

  beforeEach(() => {
    // Arrange: Engine auto-includes filters
    engine = createEngine({
      enableInjection: false,
      deterministicMode: true
    });
  });

  it('should generate Turtle format', () => {
    // Arrange
    const template = `
@prefix ex: <http://example.org/> .

ex:subject1 ex:predicate1 {{ "Object 1" | rdfLiteral }} .
ex:subject2 ex:predicate2 {{ 42 | rdfDatatype("integer") }} .
`;

    // Act
    const result = engine.renderString(template);

    // Assert
    expect(result.content).toContain('@prefix ex:');
    expect(result.content).toContain('ex:subject1 ex:predicate1 "Object 1"');
    expect(result.content).toContain('ex:subject2 ex:predicate2 "42"^^xsd:integer');
  });

  it('should generate JSON-LD context via template', () => {
    // Arrange
    const template = `
{
  "@context": {
    "name": "{{ "foaf:name" | expand }}",
    "Person": "{{ "foaf:Person" | expand }}"
  },
  "@type": "Person",
  "name": "John Doe"
}
`;

    // Act
    const result = engine.renderString(template);
    const parsed = JSON.parse(result.content);

    // Assert
    expect(parsed['@context'].name).toBe('http://xmlns.com/foaf/0.1/name');
    expect(parsed['@type']).toBe('Person');
  });
});

describe('Error Handling and Validation', () => {
  let engine;

  beforeEach(() => {
    // Arrange: Engine auto-includes filters
    engine = createEngine({
      enableInjection: false,
      deterministicMode: true
    });
  });

  it('should handle null values in RDF filters', () => {
    // Arrange
    const template = '{{ nullVal | rdfLiteral }}';

    // Act
    const result = engine.renderString(template, { nullVal: null });

    // Assert
    expect(result.content.trim()).toBe('""');
  });

  it('should validate SPARQL variable names', () => {
    // Arrange
    const template = '{{ varName | sparqlVar }}';

    // Act
    const result = engine.renderString(template, { varName: 'invalid-var-name' });

    // Assert
    expect(result.content.trim()).toBe('?invalid_var_name');
  });

  it('should handle template syntax errors gracefully', () => {
    // Arrange
    const invalidTemplate = '{{ unclosed filter';

    // Act
    const result = engine.renderString(invalidTemplate);

    // Assert - Engine returns error in result object instead of throwing
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });
});
