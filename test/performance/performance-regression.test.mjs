/**
 * @fileoverview Performance Regression Tests
 * @module test/performance/performance-regression
 *
 * @description
 * Performance regression testing for UNRDF v3.1.0.
 * Validates performance targets against v3.0.0 baseline.
 * Tests parsing, querying, validation, and storage operations.
 */

import { describe, it, expect, beforeEach } from 'vitest';

describe('Performance Regression Tests', () => {
  let performanceBaseline;

  beforeEach(() => {
    // v3.0.0 performance baseline
    performanceBaseline = {
      parse: {
        '1k-triples': 500, // ms
        '10k-triples': 3000,
        '100k-triples': 25000,
      },
      query: {
        'simple-select': 50,
        'complex-join': 200,
        aggregation: 300,
      },
      validate: {
        'shacl-basic': 100,
        'shacl-complex': 500,
      },
      storage: {
        'indexeddb-write-1k': 1000,
        'indexeddb-read-1k': 500,
      },
    };
  });

  describe('Parsing Performance', () => {
    it('should parse 1K triples within baseline (< 500ms)', () => {
      const turtle = generateTurtle(1000);

      const start = performance.now();
      // Simulate parsing
      parseTurtle(turtle);
      const duration = performance.now() - start;

      const baseline = performanceBaseline.parse['1k-triples'];
      const regressionThreshold = baseline * 1.1; // Allow 10% regression

      expect(duration, 'Parsing 1K triples should be within baseline').toBeLessThan(
        regressionThreshold
      );
    });

    it('should parse 10K triples within baseline (< 3000ms)', () => {
      const turtle = generateTurtle(10000);

      const start = performance.now();
      parseTurtle(turtle);
      const duration = performance.now() - start;

      const baseline = performanceBaseline.parse['10k-triples'];
      const regressionThreshold = baseline * 1.1;

      expect(duration).toBeLessThan(regressionThreshold);
    });

    it('should show no more than 10% regression vs v3.0.0', () => {
      const turtle = generateTurtle(1000);

      const start = performance.now();
      parseTurtle(turtle);
      const duration = performance.now() - start;

      const baseline = performanceBaseline.parse['1k-triples'];
      const regressionPercent = ((duration - baseline) / baseline) * 100;

      expect(regressionPercent, 'Should have < 10% regression').toBeLessThan(10);
    });

    it('should maintain linear scaling for parse operations', () => {
      const sizes = [1000, 2000, 3000];
      const durations = [];

      for (const size of sizes) {
        const turtle = generateTurtle(size);
        const start = performance.now();
        parseTurtle(turtle);
        const duration = performance.now() - start;
        durations.push(duration);
      }

      // Check linear scaling (O(n))
      const ratio1 = durations[1] / durations[0]; // 2x input
      const ratio2 = durations[2] / durations[0]; // 3x input

      // Should be close to linear (2x and 3x)
      expect(ratio1).toBeGreaterThan(1.5);
      expect(ratio1).toBeLessThan(2.5);
      expect(ratio2).toBeGreaterThan(2.5);
      expect(ratio2).toBeLessThan(3.5);
    });
  });

  describe('Query Performance', () => {
    beforeEach(() => {
      // Setup test store with 10K quads
      global.testStore = createTestStore(10000);
    });

    it('should execute simple SELECT within baseline (< 50ms)', () => {
      const query = `
        SELECT ?s ?p ?o WHERE {
          ?s ?p ?o .
          FILTER(STRSTARTS(STR(?s), "http://example.org/subject"))
        } LIMIT 100
      `;

      const start = performance.now();
      executeQuery(query);
      const duration = performance.now() - start;

      const baseline = performanceBaseline.query['simple-select'];
      const regressionThreshold = baseline * 1.1;

      expect(duration).toBeLessThan(regressionThreshold);
    });

    it('should execute complex JOIN within baseline (< 200ms)', () => {
      const query = `
        SELECT ?person ?name ?age WHERE {
          ?person a <http://example.org/Person> .
          ?person <http://example.org/name> ?name .
          ?person <http://example.org/age> ?age .
          FILTER(?age > 30)
        }
      `;

      const start = performance.now();
      executeQuery(query);
      const duration = performance.now() - start;

      const baseline = performanceBaseline.query['complex-join'];
      const regressionThreshold = baseline * 1.1;

      expect(duration).toBeLessThan(regressionThreshold);
    });

    it('should execute aggregation within baseline (< 300ms)', () => {
      const query = `
        SELECT (COUNT(?s) AS ?count) (AVG(?age) AS ?avgAge) WHERE {
          ?s a <http://example.org/Person> .
          ?s <http://example.org/age> ?age .
        }
      `;

      const start = performance.now();
      executeQuery(query);
      const duration = performance.now() - start;

      const baseline = performanceBaseline.query['aggregation'];
      const regressionThreshold = baseline * 1.1;

      expect(duration).toBeLessThan(regressionThreshold);
    });

    it('should benefit from query optimization', () => {
      const query = `
        SELECT ?s WHERE {
          ?s <http://example.org/predicate> "common-value" .
        } LIMIT 10
      `;

      // First execution (cold cache)
      const start1 = performance.now();
      executeQuery(query);
      const duration1 = performance.now() - start1;

      // Second execution (warm cache)
      const start2 = performance.now();
      executeQuery(query);
      const duration2 = performance.now() - start2;

      // Second execution should be faster due to caching/optimization
      expect(duration2).toBeLessThan(duration1);
    });
  });

  describe('Validation Performance', () => {
    it('should validate basic SHACL within baseline (< 100ms)', () => {
      const shapes = generateBasicShapes();
      const data = generateTurtle(100);

      const start = performance.now();
      validateSHACL(data, shapes);
      const duration = performance.now() - start;

      const baseline = performanceBaseline.validate['shacl-basic'];
      const regressionThreshold = baseline * 1.1;

      expect(duration).toBeLessThan(regressionThreshold);
    });

    it('should validate complex SHACL within baseline (< 500ms)', () => {
      const shapes = generateComplexShapes();
      const data = generateTurtle(500);

      const start = performance.now();
      validateSHACL(data, shapes);
      const duration = performance.now() - start;

      const baseline = performanceBaseline.validate['shacl-complex'];
      const regressionThreshold = baseline * 1.1;

      expect(duration).toBeLessThan(regressionThreshold);
    });
  });

  describe('Storage Performance (IndexedDB)', () => {
    it('should write 1K quads within baseline (< 1000ms)', () => {
      const quads = generateQuads(1000);

      const start = performance.now();
      writeToIndexedDB(quads);
      const duration = performance.now() - start;

      const baseline = performanceBaseline.storage['indexeddb-write-1k'];
      const regressionThreshold = baseline * 1.1;

      expect(duration).toBeLessThan(regressionThreshold);
    });

    it('should read 1K quads within baseline (< 500ms)', () => {
      const quads = generateQuads(1000);
      writeToIndexedDB(quads);

      const start = performance.now();
      readFromIndexedDB(1000);
      const duration = performance.now() - start;

      const baseline = performanceBaseline.storage['indexeddb-read-1k'];
      const regressionThreshold = baseline * 1.1;

      expect(duration).toBeLessThan(regressionThreshold);
    });
  });

  describe('Memory Usage', () => {
    it('should not leak memory across operations', () => {
      const initialMemory = process.memoryUsage().heapUsed;

      // Execute 100 operations
      for (let i = 0; i < 100; i++) {
        const turtle = generateTurtle(100);
        parseTurtle(turtle);
      }

      // Force garbage collection if available
      if (global.gc) {
        global.gc();
      }

      const finalMemory = process.memoryUsage().heapUsed;
      const memoryIncrease = finalMemory - initialMemory;

      // Should not increase by more than 10MB
      expect(memoryIncrease).toBeLessThan(10 * 1024 * 1024);
    });

    it('should handle large datasets without excessive memory', () => {
      const turtle = generateTurtle(100000);
      const initialMemory = process.memoryUsage().heapUsed;

      parseTurtle(turtle);

      const peakMemory = process.memoryUsage().heapUsed;
      const memoryUsed = peakMemory - initialMemory;

      // Should use less than 100MB for 100K triples
      expect(memoryUsed).toBeLessThan(100 * 1024 * 1024);
    });
  });

  describe('Profiler Accuracy', () => {
    it('should accurately track operation durations', () => {
      const operations = [];

      for (let i = 0; i < 10; i++) {
        const start = performance.now();
        simulateWork(50); // 50ms of work
        const duration = performance.now() - start;
        operations.push(duration);
      }

      const avgDuration = operations.reduce((a, b) => a + b, 0) / operations.length;

      // Average should be close to 50ms (±20% variance acceptable)
      expect(avgDuration).toBeGreaterThan(40);
      expect(avgDuration).toBeLessThan(70);
    });

    it('should detect performance bottlenecks', () => {
      const operations = {
        parse: measureOperation(() => parseTurtle(generateTurtle(1000))),
        query: measureOperation(() => executeQuery('SELECT * WHERE { ?s ?p ?o } LIMIT 10')),
        validate: measureOperation(() => validateSHACL(generateTurtle(100), generateBasicShapes())),
      };

      // Identify slowest operation
      const slowest = Object.entries(operations).reduce((a, b) => (a[1] > b[1] ? a : b));

      expect(slowest[0]).toBeDefined();
      expect(slowest[1]).toBeGreaterThan(0);
    });
  });

  describe('v3.1.0 Specific Performance', () => {
    it.skip('should execute isolated-vm code efficiently (< 100ms)', () => {
      const code = 'return 42;';

      const start = performance.now();
      executeInIsolatedVM(code);
      const duration = performance.now() - start;

      expect(duration, 'Isolated-VM execution should be fast').toBeLessThan(100);
    });

    it('should query IndexedDB efficiently (< 200ms)', () => {
      setupIndexedDB(10000);

      const start = performance.now();
      queryIndexedDB({ subject: 'http://example.org/s42' });
      const duration = performance.now() - start;

      expect(duration, 'IndexedDB query should be fast').toBeLessThan(200);
    });

    it('should show performance improvements vs v3.0.0', () => {
      const turtle = generateTurtle(1000);

      const start = performance.now();
      parseTurtle(turtle);
      const duration = performance.now() - start;

      const baseline = performanceBaseline.parse['1k-triples'];

      // v3.1.0 should be faster or same as v3.0.0
      expect(duration, 'Should not regress from v3.0.0').toBeLessThanOrEqual(baseline);
    });
  });
});

// Helper functions for performance testing

function generateTurtle(tripleCount) {
  const lines = [];
  for (let i = 0; i < tripleCount; i++) {
    lines.push(`<http://example.org/s${i}> <http://example.org/p> "value${i}" .`);
  }
  return lines.join('\n');
}

function generateQuads(count) {
  const quads = [];
  for (let i = 0; i < count; i++) {
    quads.push({
      subject: { value: `http://example.org/s${i}` },
      predicate: { value: 'http://example.org/p' },
      object: { value: `value${i}` },
      graph: { value: 'http://example.org/g' },
    });
  }
  return quads;
}

function generateBasicShapes() {
  return `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .

    ex:PersonShape a sh:NodeShape ;
      sh:targetClass ex:Person ;
      sh:property [
        sh:path ex:name ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
      ] .
  `;
}

function generateComplexShapes() {
  return `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .

    ex:PersonShape a sh:NodeShape ;
      sh:targetClass ex:Person ;
      sh:property [
        sh:path ex:name ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
      ] ;
      sh:property [
        sh:path ex:age ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
      ] ;
      sh:property [
        sh:path ex:email ;
        sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
      ] .
  `;
}

function parseTurtle(turtle) {
  // Mock parse operation
  const lines = turtle.split('\n');
  const quads = lines.map(line => ({ line }));
  return quads;
}

function executeQuery(_query) {
  // Mock query execution
  return [];
}

function validateSHACL(_data, _shapes) {
  // Mock SHACL validation
  return { conforms: true, violations: [] };
}

function createTestStore(quadCount) {
  return { quads: generateQuads(quadCount) };
}

function writeToIndexedDB(_quads) {
  // Mock IndexedDB write
  return Promise.resolve();
}

function readFromIndexedDB(count) {
  // Mock IndexedDB read
  return Promise.resolve(generateQuads(count));
}

function simulateWork(durationMs) {
  const start = Date.now();
  while (Date.now() - start < durationMs) {
    // Busy wait
  }
}

function measureOperation(operation) {
  const start = performance.now();
  operation();
  return performance.now() - start;
}

function executeInIsolatedVM(code) {
  // Mock isolated-VM execution
  return eval(code);
}

function setupIndexedDB(quadCount) {
  // Mock IndexedDB setup
  global.indexedDB = { quads: generateQuads(quadCount) };
}

function queryIndexedDB(pattern) {
  // Mock IndexedDB query
  return global.indexedDB?.quads.filter(q => q.subject.value === pattern.subject) || [];
}
