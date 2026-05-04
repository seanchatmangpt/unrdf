#!/usr/bin/env node
/**
 * Standalone Grammar Smoke Test Runner
 * Validates grammar subsystems without vitest dependency
 *
 * Usage: node grammar-smoke-standalone.mjs
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { streamingParse, streamingWrite } from '@unrdf/core/rdf/n3-justified-only.mjs';
import { KGenSHACLTemplates } from '../../../kgn/src/base/shacl-templates.js';

const tests = [];
const results = { passed: 0, failed: 0, errors: [] };

function test(name, fn) {
  tests.push({ name, fn });
}

async function runTests() {
  console.log('🔬 Running Grammar Smoke Tests...\n');
  const startTime = performance.now();

  for (const { name, fn } of tests) {
    try {
      const testStart = performance.now();
      await fn();
      const testDuration = (performance.now() - testStart).toFixed(2);
      console.log(`✅ ${name} (${testDuration}ms)`);
      results.passed++;
    } catch (error) {
      console.log(`❌ ${name}`);
      console.log(`   Error: ${error.message}`);
      results.failed++;
      results.errors.push({ test: name, error: error.message });
    }
  }

  const totalDuration = (performance.now() - startTime).toFixed(2);

  console.log(`\n📊 Results:`);
  console.log(`   Total: ${tests.length} tests`);
  console.log(`   Passed: ${results.passed}`);
  console.log(`   Failed: ${results.failed}`);
  console.log(`   Duration: ${totalDuration}ms`);
  console.log(`   Avg: ${(totalDuration / tests.length).toFixed(2)}ms per test`);

  if (results.failed > 0) {
    console.log(`\n❌ FAILED TESTS:`);
    results.errors.forEach(({ test, error }) => {
      console.log(`   - ${test}: ${error}`);
    });
    process.exit(1);
  } else {
    console.log(`\n✅ ALL TESTS PASSED`);
    process.exit(0);
  }
}

// ===== SPARQL Tests =====
test('SPARQL: Parse SELECT query', async () => {
  const store = createStore();
  store.add(
    dataFactory.quad(
      dataFactory.namedNode('http://example.org/s'),
      dataFactory.namedNode('http://example.org/p'),
      dataFactory.literal('value')
    )
  );
  const results = store.query('SELECT ?s WHERE { ?s ?p ?o }');
  if (!Array.isArray(results) || results.length === 0) {
    throw new Error('Expected non-empty array');
  }
});

test('SPARQL: Parse CONSTRUCT query', async () => {
  const store = createStore();
  store.add(
    dataFactory.quad(
      dataFactory.namedNode('http://example.org/s'),
      dataFactory.namedNode('http://example.org/p'),
      dataFactory.literal('value')
    )
  );
  const results = store.query('CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }');
  if (!Array.isArray(results)) {
    throw new Error('Expected array result');
  }
});

test('SPARQL: Parse ASK query', async () => {
  const store = createStore();
  store.add(
    dataFactory.quad(
      dataFactory.namedNode('http://example.org/s'),
      dataFactory.namedNode('http://example.org/p'),
      dataFactory.literal('value')
    )
  );
  const result = store.query('ASK { ?s ?p ?o }');
  if (typeof result !== 'boolean' || result !== true) {
    throw new Error('Expected boolean true');
  }
});

test('SPARQL: Handle FILTER clause', async () => {
  const store = createStore();
  store.add(
    dataFactory.quad(
      dataFactory.namedNode('http://example.org/s1'),
      dataFactory.namedNode('http://example.org/age'),
      dataFactory.literal('25', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  );
  const query = `
    SELECT ?s WHERE {
      ?s <http://example.org/age> ?age .
      FILTER(?age > 20)
    }
  `;
  const results = store.query(query);
  if (!Array.isArray(results)) {
    throw new Error('Expected array result');
  }
});

test('SPARQL: Parse query with PREFIX', async () => {
  const store = createStore();
  store.add(
    dataFactory.quad(
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/Person'),
      dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      dataFactory.namedNode('http://www.w3.org/2000/01/rdf-schema#Class')
    )
  );
  const query = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?s WHERE { ?s rdf:type ?o }
  `;
  const results = store.query(query);
  if (!Array.isArray(results)) {
    throw new Error('Expected array result');
  }
});

// ===== Turtle Parsing Tests =====
test('Turtle: Parse simple triple', async () => {
  const turtle = `
    @prefix ex: <http://example.org/> .
    ex:subject ex:predicate "value" .
  `;
  const quads = await streamingParse(turtle, { format: 'text/turtle' });
  if (!Array.isArray(quads) || quads.length !== 1 || quads[0].object.value !== 'value') {
    throw new Error('Expected 1 quad with value "value"');
  }
});

test('Turtle: Serialize quads', async () => {
  const quads = [
    dataFactory.quad(
      dataFactory.namedNode('http://example.org/s'),
      dataFactory.namedNode('http://example.org/p'),
      dataFactory.literal('value')
    ),
  ];
  const turtle = await streamingWrite(quads, { format: 'text/turtle' });
  if (typeof turtle !== 'string' || !turtle.includes('"value"')) {
    throw new Error('Expected serialized Turtle string');
  }
});

test('Turtle: Parse with prefixes', async () => {
  const turtle = `
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix ex: <http://example.org/> .
    ex:resource rdf:type ex:Thing .
  `;
  const quads = await streamingParse(turtle, { format: 'text/turtle' });
  if (quads.length !== 1 || !quads[0].predicate.value.includes('type')) {
    throw new Error('Expected 1 quad with rdf:type predicate');
  }
});

test('Turtle: Parse N-Triples', async () => {
  const ntriples = '<http://example.org/s> <http://example.org/p> "value" .';
  const quads = await streamingParse(ntriples, { format: 'application/n-triples' });
  if (quads.length !== 1 || quads[0].object.value !== 'value') {
    throw new Error('Expected 1 quad');
  }
});

test('Turtle: Handle typed literals', async () => {
  const turtle = `
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
    @prefix ex: <http://example.org/> .
    ex:resource ex:age "25"^^xsd:integer .
  `;
  const quads = await streamingParse(turtle, { format: 'text/turtle' });
  if (quads.length !== 1 || quads[0].object.value !== '25') {
    throw new Error('Expected typed literal');
  }
});

// ===== SHACL Template Tests =====
test('SHACL: Generate basic node shape', () => {
  const templates = new KGenSHACLTemplates({
    deterministicMode: true,
    staticBuildTime: '2024-01-01T00:00:00.000Z',
  });
  const shape = templates.generateShape('basic_node_shape', {
    shapeName: 'Person',
    targetClass: 'Person',
    properties: [{ path: 'name', datatype: 'xsd:string', minCount: 1 }],
  });
  if (shape.name !== 'PersonShape' || !shape.content.includes('sh:NodeShape')) {
    throw new Error('Expected PersonShape');
  }
});

test('SHACL: List available templates', () => {
  const templates = new KGenSHACLTemplates();
  const names = templates.getTemplateNames();
  if (!Array.isArray(names) || !names.includes('basic_node_shape')) {
    throw new Error('Expected template list');
  }
});

test('SHACL: Generate property shape', () => {
  const templates = new KGenSHACLTemplates({
    deterministicMode: true,
    staticBuildTime: '2024-01-01T00:00:00.000Z',
  });
  const shape = templates.generateShape('property_shape', {
    shapeName: 'Email',
    propertyPath: 'email',
    datatype: 'xsd:string',
    pattern: '^[\\w.-]+@[\\w.-]+\\.\\w+$',
  });
  if (!shape.content.includes('sh:PropertyShape')) {
    throw new Error('Expected PropertyShape');
  }
});

test('SHACL: Generate validation file', () => {
  const templates = new KGenSHACLTemplates({
    deterministicMode: true,
    staticBuildTime: '2024-01-01T00:00:00.000Z',
  });
  const file = templates.generateValidationFile([
    { template: 'basic_node_shape', context: { shapeName: 'Person', targetClass: 'Person' } },
    { template: 'property_shape', context: { shapeName: 'Email', propertyPath: 'email' } },
  ]);
  if (!file.content.includes('@prefix sh:') || file.metadata.shapeCount !== 2) {
    throw new Error('Expected validation file with 2 shapes');
  }
});

test('SHACL: Get template statistics', () => {
  const templates = new KGenSHACLTemplates();
  const stats = templates.getStats();
  if (stats.totalTemplates === 0 || !Array.isArray(stats.templates)) {
    throw new Error('Expected template stats');
  }
});

// ===== Quad Operations Tests =====
test('Quads: Create and add to store', () => {
  const store = createStore();
  const quad = dataFactory.quad(
    dataFactory.namedNode('http://example.org/s'),
    dataFactory.namedNode('http://example.org/p'),
    dataFactory.literal('value')
  );
  store.add(quad);
  if (store.size !== 1 || !store.has(quad)) {
    throw new Error('Expected quad in store');
  }
});

test('Quads: Match by pattern', () => {
  const store = createStore();
  store.add(
    dataFactory.quad(
      dataFactory.namedNode('http://example.org/s1'),
      dataFactory.namedNode('http://example.org/p'),
      dataFactory.literal('value1')
    )
  );
  store.add(
    dataFactory.quad(
      dataFactory.namedNode('http://example.org/s2'),
      dataFactory.namedNode('http://example.org/p'),
      dataFactory.literal('value2')
    )
  );
  const matches = store.match(null, dataFactory.namedNode('http://example.org/p'), null);
  if (matches.length !== 2) {
    throw new Error('Expected 2 matches');
  }
});

test('Quads: Delete from store', () => {
  const store = createStore();
  const quad = dataFactory.quad(
    dataFactory.namedNode('http://example.org/s'),
    dataFactory.namedNode('http://example.org/p'),
    dataFactory.literal('value')
  );
  store.add(quad);
  store.delete(quad);
  if (store.size !== 0) {
    throw new Error('Expected empty store');
  }
});

test('Quads: Handle blank nodes', () => {
  const store = createStore();
  const quad = dataFactory.quad(
    dataFactory.blankNode('b1'),
    dataFactory.namedNode('http://example.org/p'),
    dataFactory.literal('value')
  );
  store.add(quad);
  if (store.size !== 1 || quad.subject.termType !== 'BlankNode') {
    throw new Error('Expected blank node');
  }
});

test('Quads: Handle named graphs', () => {
  const store = createStore();
  const quad = dataFactory.quad(
    dataFactory.namedNode('http://example.org/s'),
    dataFactory.namedNode('http://example.org/p'),
    dataFactory.literal('value'),
    dataFactory.namedNode('http://example.org/graph1')
  );
  store.add(quad);
  const matches = store.match(null, null, null, dataFactory.namedNode('http://example.org/graph1'));
  if (matches.length !== 1) {
    throw new Error('Expected 1 match in graph');
  }
});

// ===== Term Creation Tests =====
test('Terms: Create named node', () => {
  const node = dataFactory.namedNode('http://example.org/resource');
  if (node.termType !== 'NamedNode' || node.value !== 'http://example.org/resource') {
    throw new Error('Expected NamedNode');
  }
});

test('Terms: Create literal', () => {
  const literal = dataFactory.literal('test value');
  if (literal.termType !== 'Literal' || literal.value !== 'test value') {
    throw new Error('Expected Literal');
  }
});

test('Terms: Create typed literal', () => {
  const literal = dataFactory.literal(
    '42',
    dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer')
  );
  if (literal.termType !== 'Literal' || literal.value !== '42' || !literal.datatype.value.includes('integer')) {
    throw new Error('Expected typed Literal');
  }
});

test('Terms: Create blank node', () => {
  const blank = dataFactory.blankNode('b1');
  if (blank.termType !== 'BlankNode' || blank.value !== 'b1') {
    throw new Error('Expected BlankNode');
  }
});

test('Terms: Create default graph', () => {
  const graph = dataFactory.defaultGraph();
  if (graph.termType !== 'DefaultGraph') {
    throw new Error('Expected DefaultGraph');
  }
});

// Run all tests
runTests().catch(console.error);
