/**
 * @fileoverview Tests for lens execution (bidirectional)
 */

import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { createStore } from '@unrdf/oxigraph';
import { defineLens } from '../src/lens.mjs';
import { compileLens } from '../src/compiler.mjs';
import { executeLensToGraph, executeLensFromGraph, verifyRoundTrip } from '../src/execute.mjs';

describe('Lens Execution', () => {
  // Helper to create test lens
  function createTestLens() {
    const profile = {
      namespace: 'https://example.org/',
      prefixes: {
        schema: 'http://schema.org/',
        xsd: 'http://www.w3.org/2001/XMLSchema#'
      },
      conventions: { idField: 'id' }
    };

    const mappings = {
      Customer: {
        subject: {
          pattern: '{namespace}Customer/{id}',
          keys: ['id']
        },
        type: 'schema:Customer',
        predicates: {
          name: { iri: 'schema:name', required: true, datatype: 'xsd:string' },
          email: { iri: 'schema:email', required: true, datatype: 'xsd:string' },
          age: { iri: 'schema:age', datatype: 'xsd:integer' }
        }
      }
    };

    return compileLens(defineLens('customer-v1', profile, mappings));
  }

  test('1. Transform Customer payload → quads', () => {
    const lens = createTestLens();
    const store = createStore();
    const payload = {
      id: 'cust-123',
      name: 'Alice Smith',
      email: 'alice@example.com',
      age: 30
    };

    const quads = executeLensToGraph(payload, lens, store, 'Customer');

    // ❓ Are all properties mapped to quads?
    assert.ok(quads.length >= 4); // type + 3 properties minimum

    // ❓ Are quads sorted canonically?
    // Verify predicates are in sorted order
    const predicates = quads.slice(1).map(q => q.predicate.value);
    const sorted = [...predicates].sort();
    assert.deepEqual(predicates, sorted);

    // ❓ Is type triple present?
    const typeQuad = quads.find(q =>
      q.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
    );
    assert.ok(typeQuad);
    assert.equal(typeQuad.object.value, 'http://schema.org/Customer');
  });

  test('2. Reverse: quads → payload (lossless)', () => {
    const lens = createTestLens();
    const store = createStore();
    const originalPayload = {
      id: 'cust-456',
      name: 'Bob Jones',
      email: 'bob@example.com',
      age: 25
    };

    // Transform to graph
    const quads = executeLensToGraph(originalPayload, lens, store, 'Customer');

    // Transform back to payload
    const reconstructed = executeLensFromGraph(quads[0].subject.value, lens, store, 'Customer');

    // ❓ Is round-trip lossless?
    assert.deepEqual(originalPayload, reconstructed);
  });

  test('3. Determinism: same payload → same quads', () => {
    const lens = createTestLens();
    const payload = {
      id: 'cust-789',
      name: 'Charlie Brown',
      email: 'charlie@example.com'
    };

    const store1 = createStore();
    const store2 = createStore();

    const quads1 = executeLensToGraph(payload, lens, store1, 'Customer');
    const quads2 = executeLensToGraph(payload, lens, store2, 'Customer');

    // ❓ Execute twice → identical quads?
    assert.equal(quads1.length, quads2.length);

    for (let i = 0; i < quads1.length; i++) {
      assert.equal(quads1[i].subject.value, quads2[i].subject.value);
      assert.equal(quads1[i].predicate.value, quads2[i].predicate.value);
      assert.equal(quads1[i].object.value, quads2[i].object.value);
    }
  });

  test('4. Handle missing optional fields', () => {
    const lens = createTestLens();
    const store = createStore();
    const payload = {
      id: 'cust-111',
      name: 'Dave Smith',
      email: 'dave@example.com'
      // age is optional, not provided
    };

    const quads = executeLensToGraph(payload, lens, store, 'Customer');

    // ❓ Does execution skip undefined properties?
    // Should have type + 2 required fields
    assert.equal(quads.length, 3);

    // Verify age is not present
    const ageQuad = quads.find(q => q.predicate.value === 'http://schema.org/age');
    assert.equal(ageQuad, undefined);
  });

  test('5. Error on missing required field', () => {
    const lens = createTestLens();
    const store = createStore();
    const payload = {
      id: 'cust-222',
      name: 'Eve Johnson'
      // email is required, not provided
    };

    // ❓ Does execution fail on missing required?
    assert.throws(() => {
      executeLensToGraph(payload, lens, store, 'Customer');
    }, /Missing required field: email/);
  });

  test('6. Handle different data types', () => {
    const lens = createTestLens();
    const store = createStore();
    const payload = {
      id: 'cust-333',
      name: 'Frank Miller',
      email: 'frank@example.com',
      age: 42 // Number
    };

    const quads = executeLensToGraph(payload, lens, store, 'Customer');

    // Find age quad
    const ageQuad = quads.find(q => q.predicate.value === 'http://schema.org/age');

    // ❓ Is integer datatype preserved?
    assert.ok(ageQuad);
    assert.equal(ageQuad.object.value, '42');
    assert.equal(ageQuad.object.datatype.value, 'http://www.w3.org/2001/XMLSchema#integer');
  });

  test('7. Stable IRI generation', () => {
    const lens = createTestLens();
    const store = createStore();
    const payload = {
      id: '123',
      name: 'Grace Hopper',
      email: 'grace@example.com'
    };

    const quads = executeLensToGraph(payload, lens, store, 'Customer');

    // ❓ Same ID → same IRI?
    assert.equal(quads[0].subject.value, 'https://example.org/Customer/123');
  });

  test('8. Verify round-trip helper function', () => {
    const lens = createTestLens();
    const store = createStore();
    const payload = {
      id: 'cust-999',
      name: 'Hannah Davis',
      email: 'hannah@example.com',
      age: 28
    };

    // ❓ Does verifyRoundTrip return true for lossless?
    const isLossless = verifyRoundTrip(payload, lens, store, 'Customer');
    assert.equal(isLossless, true);
  });
});
