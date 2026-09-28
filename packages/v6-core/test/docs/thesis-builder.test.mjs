/**
 * @file Tests for thesis-builder.mjs
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdir, writeFile, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { createStore } from '@unrdf/oxigraph';
import {
  executeSparqlConstruct,
  generateDocFromClass,
  loadOntology,
  renderFromOntology,
  applyDocTemplate
} from '../../src/docs/thesis-builder.mjs';

describe('SPARQL CONSTRUCT Execution', () => {
  let store;

  beforeEach(() => {
    store = createStore();
    // Add test data
    store.load(`
      @prefix ex: <http://example.org/> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

      ex:Person a rdfs:Class ;
        rdfs:label "Person" ;
        rdfs:comment "A person entity" .

      ex:name a rdf:Property ;
        rdfs:domain ex:Person ;
        rdfs:label "name" ;
        rdfs:comment "Person's name" .
    `, { format: 'text/turtle' });
  });

  it('should execute CONSTRUCT query and return turtle', async () => {
    const query = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }';
    const result = await executeSparqlConstruct(store, query, { outputFormat: 'turtle' });

    expect(result).toBeTruthy();
    expect(typeof result).toBe('string');
    expect(result.length).toBeGreaterThan(0);
  });

  it('should handle prefixes in CONSTRUCT query', async () => {
    const query = 'CONSTRUCT { ?s rdfs:label ?o } WHERE { ?s rdfs:label ?o }';
    const result = await executeSparqlConstruct(store, query, {
      prefixes: { rdfs: 'http://www.w3.org/2000/01/rdf-schema#' },
      outputFormat: 'turtle'
    });

    expect(result).toBeTruthy();
    expect(typeof result).toBe('string');
  });

  it('should throw error for invalid store', async () => {
    await expect(
      executeSparqlConstruct(null, 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }')
    ).rejects.toThrow();
  });
});

describe('generateDocFromClass', () => {
  let store;

  beforeEach(() => {
    store = createStore();
    store.load(`
      @prefix ex: <http://example.org/> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .

      ex:Person a owl:Class ;
        rdfs:label "Person" ;
        rdfs:comment "Represents a person" ;
        rdfs:subClassOf ex:Agent .

      ex:name a owl:DatatypeProperty ;
        rdfs:domain ex:Person ;
        rdfs:label "name" ;
        rdfs:comment "The person's name" ;
        rdfs:range xsd:string .
    `, { format: 'text/turtle' });
  });

  it('should generate reference documentation for class', async () => {
    const doc = await generateDocFromClass(store, 'http://example.org/Person', 'reference');

    expect(doc).toBeDefined();
    expect(doc.type).toBe('reference');
    expect(doc.content).toContain('Person');
    expect(doc.content).toContain('Represents a person');
    expect(doc.path).toContain('reference/Person.md');
  });

  it('should generate tutorial documentation for class', async () => {
    const doc = await generateDocFromClass(store, 'http://example.org/Person', 'tutorial');

    expect(doc).toBeDefined();
    expect(doc.type).toBe('tutorial');
    expect(doc.content).toBeTruthy();
  });

  it('should include class metadata in documentation', async () => {
    const doc = await generateDocFromClass(store, 'http://example.org/Person', 'reference');

    expect(doc.metadata).toBeDefined();
    expect(doc.metadata.classUri).toBe('http://example.org/Person');
    expect(doc.metadata.label).toBe('Person');
  });
});

describe('loadOntology', () => {
  let tempDir;
  let ontologyPath;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `test-ontology-${Date.now()}`);
    await mkdir(tempDir, { recursive: true });
    ontologyPath = join(tempDir, 'test.ttl');

    const ontologyContent = `
      @prefix ex: <http://example.org/> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .

      ex:Person a owl:Class ;
        rdfs:label "Person" ;
        rdfs:comment "A person" .

      ex:Organization a owl:Class ;
        rdfs:label "Organization" ;
        rdfs:comment "An organization" .
    `;

    await writeFile(ontologyPath, ontologyContent, 'utf8');
  });

  afterEach(async () => {
    await rm(tempDir, { recursive: true, force: true });
  });

  it('should load ontology from TTL file', async () => {
    const result = await loadOntology(ontologyPath);

    expect(result.loaded).toBe(true);
    expect(result.store).toBeDefined();
    expect(result.tripleCount).toBeGreaterThan(0);
    expect(result.path).toBe(ontologyPath);
  });

  it('should detect correct format from extension', async () => {
    const result = await loadOntology(ontologyPath);

    expect(result.format).toBe('text/turtle');
  });

  it('should throw error for non-existent file', async () => {
    await expect(loadOntology('/nonexistent/file.ttl')).rejects.toThrow();
  });

  it('should throw error for unsupported format', async () => {
    const invalidPath = join(tempDir, 'test.invalid');
    await writeFile(invalidPath, 'test', 'utf8');

    await expect(loadOntology(invalidPath)).rejects.toThrow(/Unsupported/);
  });
});

describe('renderFromOntology', () => {
  let tempDir;
  let ontologyPath;
  let outputDir;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `test-render-${Date.now()}`);
    await mkdir(tempDir, { recursive: true });

    ontologyPath = join(tempDir, 'schema.ttl');
    outputDir = join(tempDir, 'docs');

    const ontologyContent = `
      @prefix ex: <http://example.org/> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .

      ex:Person a owl:Class ;
        rdfs:label "Person" ;
        rdfs:comment "A person entity" .

      ex:Organization a owl:Class ;
        rdfs:label "Organization" ;
        rdfs:comment "An organization entity" .
    `;

    await writeFile(ontologyPath, ontologyContent, 'utf8');
  });

  afterEach(async () => {
    await rm(tempDir, { recursive: true, force: true });
  });

  it('should render documentation from ontology', async () => {
    const result = await renderFromOntology(ontologyPath, outputDir, {
      generateDiataxis: false
    });

    expect(result.status).toMatch(/success|partial/);
    expect(result.generatedDocs.length).toBeGreaterThan(0);
    expect(result.outputDir).toBe(outputDir);
  });

  it('should generate Diataxis structure when enabled', async () => {
    const result = await renderFromOntology(ontologyPath, outputDir, {
      generateDiataxis: true
    });

    expect(result.status).toMatch(/success|partial/);
    // Should have multiple doc types for each class
    expect(result.generatedDocs.length).toBeGreaterThan(2);
  });

  it('should execute custom CONSTRUCT queries', async () => {
    const result = await renderFromOntology(ontologyPath, outputDir, {
      generateDiataxis: false,
      queries: [{
        name: 'classes.ttl',
        query: 'CONSTRUCT { ?s a ?type } WHERE { ?s a ?type }',
        outputFormat: 'turtle'
      }]
    });

    expect(result.generatedDocs.some(d => d.path.includes('classes.ttl'))).toBe(true);
  });
});

describe('applyDocTemplate', () => {
  it('should apply template with data', async () => {
    const template = {
      type: 'reference',
      template: '# {{title}}\n\n{{description}}'
    };

    const data = {
      title: 'Test Class',
      description: 'This is a test class'
    };

    const result = await applyDocTemplate(template, data);

    expect(result).toContain('# Test Class');
    expect(result).toContain('This is a test class');
    expect(result).toContain('type: reference');
    expect(result).toContain('generated: true');
  });

  it('should use default template when none provided', async () => {
    const template = { type: 'reference' };
    const data = { title: 'Test', description: 'Desc' };

    const result = await applyDocTemplate(template, data);

    expect(result).toBeTruthy();
    expect(result).toContain('type: reference');
  });

  it('should handle custom frontmatter', async () => {
    const template = {
      type: 'tutorial',
      template: '# {{title}}',
      frontmatter: {
        author: 'Test Author',
        version: '1.0.0'
      }
    };

    const data = { title: 'Tutorial' };
    const result = await applyDocTemplate(template, data);

    expect(result).toContain('author: Test Author');
    expect(result).toContain('version: 1.0.0');
  });
});
