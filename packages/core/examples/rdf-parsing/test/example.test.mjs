/**
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  parseTurtle,
  parseNTriples,
  parseNQuads,
  mergeStores,
  getGraphs,
  canonicalize,
  validateRDF,
  convertFormat
} from '../src/index.mjs';
import { Store } from 'n3';

const SAMPLE_TURTLE = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice foaf:name "Alice" .
`;

const SAMPLE_NTRIPLES = `
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
`;

const SAMPLE_NQUADS = `
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" <http://example.org/graph1> .
`;

const MULTI_GRAPH_NQUADS = `
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" <http://example.org/graph1> .
<http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" <http://example.org/graph2> .
`;

describe('RDF Parsing Examples', () => {
  it('parses Turtle format', async () => {
    const store = await parseTurtle(SAMPLE_TURTLE);
    expect(store.size).toBe(1);
  });

  it('parses N-Triples format', async () => {
    const store = await parseNTriples(SAMPLE_NTRIPLES);
    expect(store.size).toBe(1);
  });

  it('parses N-Quads format with graphs', async () => {
    const store = await parseNQuads(SAMPLE_NQUADS);
    expect(store.size).toBe(1);

    const graphs = getGraphs(store);
    expect(graphs).toHaveLength(1);
    expect(graphs[0]).toBe('http://example.org/graph1');
  });

  it('merges multiple stores', async () => {
    const store1 = await parseTurtle(SAMPLE_TURTLE);
    const store2 = await parseNTriples(SAMPLE_NTRIPLES);

    const merged = mergeStores([store1, store2]);
    expect(merged.size).toBe(1); // Same triple, so deduplicated
  });

  it('gets unique graphs from store', async () => {
    const store = await parseNQuads(SAMPLE_NQUADS);
    const graphs = getGraphs(store);

    expect(graphs).toHaveLength(1);
    expect(graphs[0]).toContain('graph1');
  });

  it('canonicalizes RDF output', async () => {
    const store = await parseTurtle(SAMPLE_TURTLE);
    const canonical = canonicalize(store);

    expect(canonical).toContain('<http://example.org/alice>');
    expect(canonical).toContain('<http://xmlns.com/foaf/0.1/name>');
    expect(canonical).toContain('"Alice"');
  });

  it('validates correct RDF syntax', async () => {
    const result = await validateRDF(SAMPLE_TURTLE, 'turtle');
    expect(result.valid).toBe(true);
    expect(result.tripleCount).toBeGreaterThan(0);
  });

  it('detects invalid RDF syntax', async () => {
    const result = await validateRDF('invalid rdf', 'turtle');
    expect(result.valid).toBe(false);
    expect(result.errors.length).toBeGreaterThan(0);
  });

  it('converts between RDF formats', async () => {
    const converted = await convertFormat(SAMPLE_TURTLE, 'turtle', 'ntriples');
    expect(converted).toContain('<http://example.org/alice>');
    expect(converted).toContain('"Alice"');
  });

  it('handles parsing errors gracefully', async () => {
    await expect(parseTurtle('invalid turtle syntax'))
      .rejects.toThrow('Turtle parsing failed');
  });

  it('parses multiple graphs from N-Quads', async () => {
    const store = await parseNQuads(MULTI_GRAPH_NQUADS);
    expect(store.size).toBe(2);
    const graphs = getGraphs(store);
    expect(graphs).toHaveLength(2);
  });

  it('merges empty stores', () => {
    const store1 = new Store();
    const store2 = new Store();
    const merged = mergeStores([store1, store2]);
    expect(merged.size).toBe(0);
  });

  it('canonicalizes empty store', () => {
    const emptyStore = new Store();
    const canonical = canonicalize(emptyStore);
    expect(canonical).toBe('');
  });

  it('gets graphs from store with no graphs', async () => {
    const store = await parseTurtle(SAMPLE_TURTLE);
    const graphs = getGraphs(store);
    expect(graphs).toHaveLength(0);
  });

  it('validates N-Triples format', async () => {
    const result = await validateRDF(SAMPLE_NTRIPLES, 'ntriples');
    expect(result.valid).toBe(true);
    expect(result.tripleCount).toBe(1);
  });
});

describe('Error Handling and Edge Cases', () => {
  it('rejects invalid N-Triples parsing', async () => {
    await expect(parseNTriples('invalid ntriples'))
      .rejects.toThrow('N-Triples parsing failed');
  });

  it('rejects invalid N-Quads parsing', async () => {
    await expect(parseNQuads('invalid nquads'))
      .rejects.toThrow('N-Quads parsing failed');
  });

  it('rejects unsupported format conversion', async () => {
    await expect(convertFormat(SAMPLE_TURTLE, 'invalid', 'ntriples'))
      .rejects.toThrow('Unsupported input format');
  });

  it('validates N-Quads format', async () => {
    const result = await validateRDF(SAMPLE_NQUADS, 'nquads');
    expect(result.valid).toBe(true);
    expect(result.tripleCount).toBe(1);
  });

  it('rejects unsupported output format conversion', async () => {
    await expect(convertFormat(SAMPLE_TURTLE, 'turtle', 'unsupported'))
      .rejects.toThrow('Unsupported output format');
  });

  it('parses complex Turtle with multiple prefixes', async () => {
    const complexTurtle = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:alice foaf:name "Alice" ;
         foaf:age "30"^^xsd:integer .
`;
    const store = await parseTurtle(complexTurtle);
    expect(store.size).toBe(2);
  });

  it('canonicalizes store with typed literals', async () => {
    const turtleWithTypes = `
@prefix ex: <http://example.org/> .
ex:alice ex:age "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
`;
    const store = await parseTurtle(turtleWithTypes);
    const canonical = canonicalize(store);
    expect(canonical).toContain('^^<http://www.w3.org/2001/XMLSchema#integer>');
  });
});
