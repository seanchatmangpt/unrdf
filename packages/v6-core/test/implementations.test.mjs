/**
 * Test suite for new implementations (BLAKE3, SPARQL CONSTRUCT, ontology, templates)
 * @module @unrdf/v6-core/test/implementations
 */

import { describe, it, expect } from 'vitest';
import { blake3Hash, canonicalize } from '../src/receipt-pattern.mjs';
import {
  executeSparqlConstruct,
  loadOntology,
  generateDocFromClass,
  renderFromOntology,
  applyDocTemplate
} from '../src/docs/thesis-builder.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { writeFile, mkdir, rm } from 'node:fs/promises';
import { join } from 'node:path';

const { namedNode, literal, quad } = dataFactory;

describe('BLAKE3 Hash Implementation', () => {
  it('should hash a simple string', async () => {
    const hash = await blake3Hash('test data');
    expect(hash).toHaveLength(64);
    expect(hash).toMatch(/^[0-9a-f]{64}$/);
  });

  it('should produce deterministic hashes', async () => {
    const hash1 = await blake3Hash('deterministic test');
    const hash2 = await blake3Hash('deterministic test');
    expect(hash1).toBe(hash2);
  });

  it('should hash objects consistently', async () => {
    const obj = { name: 'test', value: 42 };
    const hash1 = await blake3Hash(obj);
    const hash2 = await blake3Hash(obj);
    expect(hash1).toBe(hash2);
  });

  it('should produce different hashes for different inputs', async () => {
    const hash1 = await blake3Hash('input1');
    const hash2 = await blake3Hash('input2');
    expect(hash1).not.toBe(hash2);
  });
});

describe('Canonicalize Function', () => {
  it('should sort object keys lexicographically', () => {
    const obj = { z: 3, a: 1, m: 2 };
    const canonical = canonicalize(obj);
    expect(canonical).toBe('{"a":1,"m":2,"z":3}');
  });

  it('should handle nested objects', () => {
    const obj = { outer: { z: 2, a: 1 } };
    const canonical = canonicalize(obj);
    expect(canonical).toBe('{"outer":{"a":1,"z":2}}');
  });

  it('should handle arrays', () => {
    const arr = [3, 1, 2];
    const canonical = canonicalize(arr);
    expect(canonical).toBe('[3,1,2]');
  });

  it('should remove undefined values', () => {
    const obj = { a: 1, b: undefined, c: 3 };
    const canonical = canonicalize(obj);
    expect(canonical).toBe('{"a":1,"c":3}');
  });
});

describe('SPARQL CONSTRUCT Implementation', () => {
  it('should execute simple CONSTRUCT query', async () => {
    const store = createStore();

    // Add test data
    const subject = namedNode('http://example.org/subject');
    const predicate = namedNode('http://example.org/predicate');
    const object = literal('value');
    store.add(quad(subject, predicate, object));

    // Execute CONSTRUCT query
    const query = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }';
    const result = await executeSparqlConstruct(store, query);

    expect(result).toBeTruthy();
    expect(typeof result).toBe('string');
    expect(result.length).toBeGreaterThan(0);
  });

  it('should handle prefixes in CONSTRUCT query', async () => {
    const store = createStore();

    const subject = namedNode('http://example.org/subject');
    const predicate = namedNode('http://example.org/predicate');
    const object = literal('value');
    store.add(quad(subject, predicate, object));

    const query = 'CONSTRUCT { ?s ex:predicate ?o } WHERE { ?s ex:predicate ?o }';
    const result = await executeSparqlConstruct(store, query, {
      prefixes: { ex: 'http://example.org/' }
    });

    expect(result).toBeTruthy();
  });

  it('should return empty string for no results', async () => {
    const store = createStore();

    const query = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }';
    const result = await executeSparqlConstruct(store, query);

    expect(result).toBe('');
  });

  it('should support different output formats', async () => {
    const store = createStore();

    const subject = namedNode('http://example.org/subject');
    const predicate = namedNode('http://example.org/predicate');
    const object = literal('value');
    store.add(quad(subject, predicate, object));

    const query = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }';

    // Turtle format (default)
    const turtleResult = await executeSparqlConstruct(store, query, {
      outputFormat: 'turtle'
    });
    expect(turtleResult).toBeTruthy();

    // N-Triples format
    const ntriplesResult = await executeSparqlConstruct(store, query, {
      outputFormat: 'ntriples'
    });
    expect(ntriplesResult).toBeTruthy();
  });
});

describe('Ontology Loading Implementation', () => {
  const testOntologyPath = '/tmp/test-ontology.ttl';

  it('should load Turtle ontology', async () => {
    const ontologyData = `
      @prefix ex: <http://example.org/> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

      ex:Person a rdfs:Class ;
        rdfs:label "Person" ;
        rdfs:comment "A human being" .
    `;

    await writeFile(testOntologyPath, ontologyData, 'utf-8');

    const store = await loadOntology(testOntologyPath);

    expect(store).toBeDefined();
    expect(store.size).toBeGreaterThan(0);

    // Clean up
    await rm(testOntologyPath);
  });

  it('should reject unsupported formats', async () => {
    const badPath = '/tmp/test.xyz';
    await writeFile(badPath, 'invalid', 'utf-8');

    await expect(loadOntology(badPath)).rejects.toThrow(/Unsupported ontology format/);

    // Clean up
    await rm(badPath);
  });

  it('should handle file read errors', async () => {
    await expect(loadOntology('/nonexistent/path.ttl')).rejects.toThrow();
  });
});

describe('Generate Documentation from Class', () => {
  it('should generate documentation for a class', async () => {
    const store = createStore();

    // Add class metadata
    const classUri = namedNode('http://example.org/Person');
    const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const rdfsClass = namedNode('http://www.w3.org/2000/01/rdf-schema#Class');
    const rdfsLabel = namedNode('http://www.w3.org/2000/01/rdf-schema#label');
    const rdfsComment = namedNode('http://www.w3.org/2000/01/rdf-schema#comment');

    store.add(quad(classUri, rdfType, rdfsClass));
    store.add(quad(classUri, rdfsLabel, literal('Person')));
    store.add(quad(classUri, rdfsComment, literal('A human being')));

    const doc = await generateDocFromClass(store, 'http://example.org/Person', 'reference');

    expect(doc).toBeDefined();
    expect(doc.path).toContain('reference/Person.md');
    expect(doc.type).toBe('reference');
    expect(doc.content).toContain('Person');
    expect(doc.content).toContain('A human being');
    expect(doc.content).toContain('http://example.org/Person');
  });

  it('should handle classes with properties', async () => {
    const store = createStore();

    const classUri = namedNode('http://example.org/Person');
    const rdfsLabel = namedNode('http://www.w3.org/2000/01/rdf-schema#label');
    const rdfsComment = namedNode('http://www.w3.org/2000/01/rdf-schema#comment');
    const rdfsDomain = namedNode('http://www.w3.org/2000/01/rdf-schema#domain');
    const property = namedNode('http://example.org/name');

    store.add(quad(classUri, rdfsLabel, literal('Person')));
    store.add(quad(property, rdfsDomain, classUri));
    store.add(quad(property, rdfsLabel, literal('name')));

    const doc = await generateDocFromClass(store, 'http://example.org/Person', 'reference');

    expect(doc.content).toContain('Properties');
    expect(doc.content).toContain('name');
    expect(doc.metadata.propertyCount).toBeGreaterThan(0);
  });
});

describe('Render from Ontology', () => {
  const testOutputDir = '/tmp/test-ontology-output';

  it('should render documentation from ontology', async () => {
    const ontologyPath = '/tmp/test-ontology-render.ttl';
    const ontologyData = `
      @prefix ex: <http://example.org/> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .

      ex:Person a owl:Class ;
        rdfs:label "Person" ;
        rdfs:comment "A human being" .

      ex:Animal a owl:Class ;
        rdfs:label "Animal" ;
        rdfs:comment "A living organism" .
    `;

    await writeFile(ontologyPath, ontologyData, 'utf-8');

    const result = await renderFromOntology(ontologyPath, testOutputDir, {
      generateDiataxis: true
    });

    expect(result.status).toBe('success');
    expect(result.generatedDocs.length).toBeGreaterThan(0);
    expect(result.stats.totalClasses).toBeGreaterThan(0);

    // Clean up
    await rm(ontologyPath);
    await rm(testOutputDir, { recursive: true });
  });

  it('should execute custom CONSTRUCT queries', async () => {
    const ontologyPath = '/tmp/test-ontology-custom.ttl';
    const ontologyData = `
      @prefix ex: <http://example.org/> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

      ex:Test rdfs:label "Test" .
    `;

    await writeFile(ontologyPath, ontologyData, 'utf-8');

    const result = await renderFromOntology(ontologyPath, testOutputDir, {
      generateDiataxis: false,
      queries: [
        {
          name: 'labels.ttl',
          query: 'CONSTRUCT { ?s rdfs:label ?o } WHERE { ?s rdfs:label ?o }',
          outputFormat: 'turtle'
        }
      ]
    });

    expect(result.generatedDocs.some(d => d.path === 'labels.ttl')).toBe(true);

    // Clean up
    await rm(ontologyPath);
    await rm(testOutputDir, { recursive: true });
  });
});

describe('Apply Documentation Template', () => {
  it('should apply template to data', async () => {
    const template = {
      type: 'reference',
      name: 'TestClass',
      template: '# {{name}}\n\n{{description}}'
    };

    const data = {
      name: 'Example Class',
      description: 'This is an example class for testing'
    };

    const result = await applyDocTemplate(template, data);

    expect(result).toContain('Example Class');
    expect(result).toContain('This is an example class for testing');
    expect(result).toContain('---'); // Frontmatter
    expect(result).toContain('type: reference');
  });

  it('should use default templates', async () => {
    const template = {
      type: 'reference',
      name: 'TestClass'
    };

    const data = {
      name: 'Example',
      description: 'Test description',
      uri: 'http://example.org/Example'
    };

    const result = await applyDocTemplate(template, data);

    expect(result).toContain('Example');
    expect(result).toContain('Test description');
    expect(result).toContain('http://example.org/Example');
  });

  it('should handle different doc types', async () => {
    const types = ['reference', 'tutorial', 'howto', 'explanation'];

    for (const type of types) {
      const template = {
        type,
        name: 'Test'
      };

      const data = {
        name: 'Test',
        description: 'Description'
      };

      const result = await applyDocTemplate(template, data);
      expect(result).toContain(`type: ${type}`);
    }
  });
});

describe('Integration Test: Full Workflow', () => {
  it('should complete end-to-end ontology to documentation workflow', async () => {
    const ontologyPath = '/tmp/test-full-workflow.ttl';
    const outputDir = '/tmp/test-full-workflow-output';

    // Create test ontology
    const ontologyData = `
      @prefix ex: <http://example.org/> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .

      ex:Person a owl:Class ;
        rdfs:label "Person" ;
        rdfs:comment "Represents a human being" .

      ex:name a owl:DatatypeProperty ;
        rdfs:domain ex:Person ;
        rdfs:label "name" ;
        rdfs:comment "The person's name" .
    `;

    await writeFile(ontologyPath, ontologyData, 'utf-8');

    // Load ontology
    const store = await loadOntology(ontologyPath);
    expect(store.size).toBeGreaterThan(0);

    // Generate documentation
    const doc = await generateDocFromClass(store, 'http://example.org/Person', 'reference');
    expect(doc.content).toContain('Person');
    expect(doc.content).toContain('Represents a human being');
    expect(doc.content).toContain('name');

    // Render full ontology
    const result = await renderFromOntology(ontologyPath, outputDir);
    expect(result.status).toBe('success');
    expect(result.generatedDocs.length).toBeGreaterThan(0);

    // Clean up
    await rm(ontologyPath);
    await rm(outputDir, { recursive: true });
  });
});
