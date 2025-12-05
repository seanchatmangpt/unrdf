import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, dataFactory } from '../src/index.mjs';

/**
 * Benchmark suite to measure Oxigraph performance against the current engine
 */
describe('Oxigraph Benchmarks', () => {
  let store;

  beforeEach(() => {
    store = createStore();
  });

  /**
   * Benchmark: Add operation performance
   * Measures throughput for adding triples to the store
   */
  it('should benchmark add operations', () => {
    const iterations = 1000;
    const ex = dataFactory.namedNode('http://example.com/');
    const predicate = dataFactory.namedNode('http://schema.org/knows');

    const startTime = performance.now();

    for (let i = 0; i < iterations; i++) {
      const node = dataFactory.namedNode(`http://example.com/person/${i}`);
      const triple = dataFactory.triple(ex, predicate, node);
      store.add(triple);
    }

    const duration = performance.now() - startTime;
    const throughput = iterations / (duration / 1000);

    console.log(`\n📊 Add Benchmark (Oxigraph):`);
    console.log(`   Iterations: ${iterations}`);
    console.log(`   Duration: ${duration.toFixed(2)}ms`);
    console.log(`   Throughput: ${throughput.toFixed(0)} ops/sec`);

    expect(store.size).toBe(iterations);
    expect(duration).toBeLessThan(30000); // Should complete in reasonable time
  });

  /**
   * Benchmark: Query performance
   * Measures time to execute SPARQL SELECT queries
   */
  it('should benchmark SELECT query performance', () => {
    // Populate store with test data
    const ex = dataFactory.namedNode('http://example.com/');
    const predicate = dataFactory.namedNode('http://schema.org/knows');

    for (let i = 0; i < 100; i++) {
      const node = dataFactory.namedNode(`http://example.com/person/${i}`);
      store.add(dataFactory.triple(ex, predicate, node));
    }

    const iterations = 100;
    const startTime = performance.now();

    for (let i = 0; i < iterations; i++) {
      const results = store.query('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
      expect(Array.isArray(results)).toBe(true);
    }

    const duration = performance.now() - startTime;
    const throughput = iterations / (duration / 1000);

    console.log(`\n📊 SELECT Query Benchmark (Oxigraph):`);
    console.log(`   Iterations: ${iterations}`);
    console.log(`   Duration: ${duration.toFixed(2)}ms`);
    console.log(`   Throughput: ${throughput.toFixed(0)} queries/sec`);

    expect(duration).toBeLessThan(30000);
  });

  /**
   * Benchmark: ASK query performance
   * Measures time for ASK queries (existence checks)
   */
  it('should benchmark ASK query performance', () => {
    const ex = dataFactory.namedNode('http://example.com/');
    const predicate = dataFactory.namedNode('http://schema.org/name');
    const value = dataFactory.literal('Test');

    store.add(dataFactory.triple(ex, predicate, value));

    const iterations = 1000;
    const startTime = performance.now();

    for (let i = 0; i < iterations; i++) {
      const result = store.query('ASK { ?s ?p ?o }');
      expect(typeof result).toBe('boolean');
    }

    const duration = performance.now() - startTime;
    const throughput = iterations / (duration / 1000);

    console.log(`\n📊 ASK Query Benchmark (Oxigraph):`);
    console.log(`   Iterations: ${iterations}`);
    console.log(`   Duration: ${duration.toFixed(2)}ms`);
    console.log(`   Throughput: ${throughput.toFixed(0)} ops/sec`);

    expect(duration).toBeLessThan(30000);
  });

  /**
   * Benchmark: CONSTRUCT query performance
   * Measures time to construct new graphs from query results
   */
  it('should benchmark CONSTRUCT query performance', () => {
    // Populate store
    const ex = dataFactory.namedNode('http://example.com/');
    const predicate = dataFactory.namedNode('http://schema.org/knows');

    for (let i = 0; i < 50; i++) {
      const node = dataFactory.namedNode(`http://example.com/person/${i}`);
      store.add(dataFactory.triple(ex, predicate, node));
    }

    const iterations = 100;
    const startTime = performance.now();

    for (let i = 0; i < iterations; i++) {
      const results = store.query('CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }');
      expect(Array.isArray(results)).toBe(true);
    }

    const duration = performance.now() - startTime;
    const throughput = iterations / (duration / 1000);

    console.log(`\n📊 CONSTRUCT Query Benchmark (Oxigraph):`);
    console.log(`   Iterations: ${iterations}`);
    console.log(`   Duration: ${duration.toFixed(2)}ms`);
    console.log(`   Throughput: ${throughput.toFixed(0)} queries/sec`);

    expect(duration).toBeLessThan(30000);
  });

  /**
   * Benchmark: Pattern matching performance
   * Measures throughput for match operations
   */
  it('should benchmark pattern matching performance', () => {
    const ex = dataFactory.namedNode('http://example.com/');
    const predicate = dataFactory.namedNode('http://schema.org/knows');

    for (let i = 0; i < 500; i++) {
      const node = dataFactory.namedNode(`http://example.com/person/${i}`);
      store.add(dataFactory.triple(ex, predicate, node));
    }

    const iterations = 1000;
    const startTime = performance.now();

    for (let i = 0; i < iterations; i++) {
      const results = store.match(ex, null, null);
      expect(Array.isArray(results)).toBe(true);
    }

    const duration = performance.now() - startTime;
    const throughput = iterations / (duration / 1000);

    console.log(`\n📊 Pattern Matching Benchmark (Oxigraph):`);
    console.log(`   Iterations: ${iterations}`);
    console.log(`   Duration: ${duration.toFixed(2)}ms`);
    console.log(`   Throughput: ${throughput.toFixed(0)} ops/sec`);

    expect(duration).toBeLessThan(30000);
  });

  /**
   * Benchmark: Delete operation performance
   * Measures throughput for removing triples
   */
  it('should benchmark delete operations', () => {
    const ex = dataFactory.namedNode('http://example.com/');
    const predicate = dataFactory.namedNode('http://schema.org/knows');
    const triples = [];

    // Add triples
    for (let i = 0; i < 500; i++) {
      const node = dataFactory.namedNode(`http://example.com/person/${i}`);
      const triple = dataFactory.triple(ex, predicate, node);
      store.add(triple);
      triples.push(triple);
    }

    const startTime = performance.now();

    // Delete triples
    triples.forEach(triple => store.delete(triple));

    const duration = performance.now() - startTime;
    const throughput = triples.length / (duration / 1000);

    console.log(`\n📊 Delete Benchmark (Oxigraph):`);
    console.log(`   Iterations: ${triples.length}`);
    console.log(`   Duration: ${duration.toFixed(2)}ms`);
    console.log(`   Throughput: ${throughput.toFixed(0)} ops/sec`);

    expect(store.size).toBe(0);
    expect(duration).toBeLessThan(30000);
  });

  /**
   * Benchmark: Bulk load performance
   * Measures throughput for loading RDF data in Turtle format
   */
  it('should benchmark bulk load performance', () => {
    const turtle = `
      @prefix ex: <http://example.com/> .
      @prefix schema: <http://schema.org/> .

      ex:person1 schema:name "Person 1" ;
                 schema:age 30 ;
                 schema:knows ex:person2 .

      ex:person2 schema:name "Person 2" ;
                 schema:age 25 ;
                 schema:knows ex:person3 .

      ex:person3 schema:name "Person 3" ;
                 schema:age 28 .
    `;

    const iterations = 100;
    const startTime = performance.now();

    for (let i = 0; i < iterations; i++) {
      const freshStore = createStore();
      freshStore.load(turtle, {
        format: 'text/turtle',
        base_iri: 'http://example.com/',
      });
    }

    const duration = performance.now() - startTime;
    const throughput = iterations / (duration / 1000);

    console.log(`\n📊 Bulk Load Benchmark (Oxigraph):`);
    console.log(`   Iterations: ${iterations}`);
    console.log(`   Duration: ${duration.toFixed(2)}ms`);
    console.log(`   Throughput: ${throughput.toFixed(0)} loads/sec`);

    expect(duration).toBeLessThan(30000);
  });

  /**
   * Benchmark: Dump operation performance
   * Measures throughput for exporting RDF data
   */
  it('should benchmark dump performance', () => {
    // Populate store
    const ex = dataFactory.namedNode('http://example.com/');
    const predicate = dataFactory.namedNode('http://schema.org/knows');

    for (let i = 0; i < 100; i++) {
      const node = dataFactory.namedNode(`http://example.com/person/${i}`);
      store.add(dataFactory.triple(ex, predicate, node));
    }

    const iterations = 100;
    const startTime = performance.now();

    for (let i = 0; i < iterations; i++) {
      const exported = store.dump({ format: 'application/n-quads' });
      expect(typeof exported).toBe('string');
    }

    const duration = performance.now() - startTime;
    const throughput = iterations / (duration / 1000);

    console.log(`\n📊 Dump Benchmark (Oxigraph):`);
    console.log(`   Iterations: ${iterations}`);
    console.log(`   Duration: ${duration.toFixed(2)}ms`);
    console.log(`   Throughput: ${throughput.toFixed(0)} dumps/sec`);

    expect(duration).toBeLessThan(30000);
  });
});
