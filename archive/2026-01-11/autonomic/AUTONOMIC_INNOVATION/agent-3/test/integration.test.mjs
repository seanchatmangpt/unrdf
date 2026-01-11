/**
 * @fileoverview Integration tests for end-to-end lens functionality
 */

import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { createStore } from '@unrdf/oxigraph';
import { defineLens, compileLens, executeLensToGraph, executeLensFromGraph } from '../src/index.mjs';

describe('End-to-End Bidirectional', () => {
  test('1. Customer API → Graph → API', () => {
    // Define lens
    const lens = defineLens('customer-v1', {
      namespace: 'https://example.org/',
      prefixes: {
        schema: 'http://schema.org/',
        xsd: 'http://www.w3.org/2001/XMLSchema#'
      },
      conventions: { idField: 'id' }
    }, {
      Customer: {
        subject: { pattern: '{namespace}Customer/{id}', keys: ['id'] },
        type: 'schema:Customer',
        predicates: {
          name: { iri: 'schema:name', required: true },
          email: { iri: 'schema:email', required: true },
          phone: { iri: 'schema:telephone' }
        }
      }
    });

    // Compile
    const compiled = compileLens(lens);

    // Execute: API → Graph
    const store = createStore();
    const initialPayload = {
      id: 'cust-001',
      name: 'Alice Johnson',
      email: 'alice@example.com',
      phone: '+1-555-0100'
    };

    const quads = executeLensToGraph(initialPayload, compiled, store, 'Customer');
    assert.ok(quads.length >= 4);

    // Execute: Graph → API
    const finalPayload = executeLensFromGraph(quads[0].subject.value, compiled, store, 'Customer');

    // ❓ Complete round-trip lossless?
    assert.deepEqual(finalPayload, initialPayload);
  });

  test('2. Multiple entities in store', () => {
    const lens = defineLens('customer-v1', {
      namespace: 'https://example.org/',
      prefixes: { schema: 'http://schema.org/' },
      conventions: { idField: 'id' }
    }, {
      Customer: {
        subject: { pattern: '{namespace}Customer/{id}', keys: ['id'] },
        type: 'schema:Customer',
        predicates: {
          name: { iri: 'schema:name', required: true },
          email: { iri: 'schema:email', required: true }
        }
      }
    });

    const compiled = compileLens(lens);
    const store = createStore();

    // Add customer 1
    const customer1 = { id: 'c1', name: 'Bob Smith', email: 'bob@example.com' };
    executeLensToGraph(customer1, compiled, store, 'Customer');

    // Add customer 2
    const customer2 = { id: 'c2', name: 'Carol White', email: 'carol@example.com' };
    executeLensToGraph(customer2, compiled, store, 'Customer');

    // ❓ Can extract correct entity from multi-entity store?
    const extractedC1 = executeLensFromGraph('https://example.org/Customer/c1', compiled, store, 'Customer');
    const extractedC2 = executeLensFromGraph('https://example.org/Customer/c2', compiled, store, 'Customer');

    assert.deepEqual(extractedC1, customer1);
    assert.deepEqual(extractedC2, customer2);
  });

  test('3. Lens compilation determinism', () => {
    const createLens = () => defineLens('test-v1', {
      namespace: 'https://test.org/',
      prefixes: { schema: 'http://schema.org/' },
      conventions: { idField: 'id' }
    }, {
      Test: {
        subject: { pattern: '{namespace}Test/{id}', keys: ['id'] },
        predicates: {
          field1: { iri: 'schema:field1' },
          field2: { iri: 'schema:field2' }
        }
      }
    });

    const lens1 = createLens();
    const lens2 = createLens();

    const compiled1 = compileLens(lens1);
    const compiled2 = compileLens(lens2);

    // ❓ Compile same lens twice → identical result?
    assert.equal(compiled1.canonicalHash, compiled2.canonicalHash);
    assert.deepEqual(compiled1.compiledMappings, compiled2.compiledMappings);
  });

  test('4. JSON serialization (no closures)', () => {
    const lens = defineLens('serialization-test', {
      namespace: 'https://example.org/',
      prefixes: { schema: 'http://schema.org/' },
      conventions: { idField: 'id' }
    }, {
      Entity: {
        subject: { pattern: '{namespace}Entity/{id}', keys: ['id'] },
        predicates: {
          field: { iri: 'schema:field' }
        }
      }
    });

    const compiled = compileLens(lens);

    // ❓ Can be serialized to JSON?
    let json;
    assert.doesNotThrow(() => {
      json = JSON.stringify(compiled);
    });

    // ❓ Can be parsed back?
    let parsed;
    assert.doesNotThrow(() => {
      parsed = JSON.parse(json);
    });

    // ❓ No functions in JSON?
    assert.ok(!json.includes('function'));
    assert.ok(!json.includes('=>'));

    // ❓ Deep equality after round-trip?
    assert.deepEqual(parsed, compiled);
  });

  test('5. Large payload (50 fields)', () => {
    // Create lens with 50 fields
    const predicates = {};
    for (let i = 0; i < 50; i++) {
      predicates[`field${i}`] = { iri: `http://schema.org/field${i}` };
    }

    const lens = defineLens('large-v1', {
      namespace: 'https://example.org/',
      prefixes: {},
      conventions: { idField: 'id' }
    }, {
      Large: {
        subject: { pattern: '{namespace}Large/{id}', keys: ['id'] },
        predicates
      }
    });

    const compiled = compileLens(lens);

    // Create payload with 50 fields
    const payload = { id: 'large-001' };
    for (let i = 0; i < 50; i++) {
      payload[`field${i}`] = `value${i}`;
    }

    const store = createStore();

    // ❓ Performance acceptable (<100ms)?
    const start = Date.now();
    const quads = executeLensToGraph(payload, compiled, store, 'Large');
    const end = Date.now();

    const duration = end - start;
    console.log(`Large payload execution time: ${duration}ms`);

    // ❓ All fields mapped?
    assert.ok(quads.length >= 50);

    // ❓ Round-trip works?
    const reconstructed = executeLensFromGraph(quads[0].subject.value, compiled, store, 'Large');
    assert.deepEqual(reconstructed, payload);
  });

  test('6. Deterministic IRI generation across executions', () => {
    const lens = defineLens('determinism-v1', {
      namespace: 'https://example.org/',
      prefixes: { schema: 'http://schema.org/' },
      conventions: { idField: 'id' }
    }, {
      Test: {
        subject: { pattern: '{namespace}Test/{id}', keys: ['id'] },
        type: 'schema:Test',
        predicates: {
          value: { iri: 'schema:value', required: true }
        }
      }
    });

    const compiled = compileLens(lens);
    const payload = { id: 'test-123', value: 'test-value' };

    // Execute multiple times
    const iris = [];
    for (let i = 0; i < 10; i++) {
      const store = createStore();
      const quads = executeLensToGraph(payload, compiled, store, 'Test');
      iris.push(quads[0].subject.value);
    }

    // ❓ All IRIs identical?
    const firstIRI = iris[0];
    assert.ok(iris.every(iri => iri === firstIRI));
  });
});
