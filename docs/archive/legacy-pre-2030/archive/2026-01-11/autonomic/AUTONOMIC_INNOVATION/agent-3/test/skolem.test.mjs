/**
 * @fileoverview Tests for stable IRI and skolem generation
 */

import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { createStableIRI, createSkolemID, extractFromIRI } from '../src/skolem.mjs';

describe('Stable IRI & Skolem', () => {
  test('1. Pattern substitution: {namespace}{type}/{id}', () => {
    const entity = { id: '123', name: 'Alice' };
    const rule = {
      pattern: '{namespace}Customer/{id}',
      keys: ['id']
    };
    const namespace = 'https://example.org/';

    const iri = createStableIRI(entity, 'customer-v1', rule, namespace, 'Customer');

    // ❓ Are placeholders replaced correctly?
    assert.equal(iri, 'https://example.org/Customer/123');
  });

  test('2. Hash fallback when ID missing', () => {
    const entity = { name: 'Bob', email: 'bob@example.com' };
    const rule = {
      pattern: '{namespace}Customer/{hash}',
      keys: ['id'] // ID not in entity
    };
    const namespace = 'https://example.org/';

    const iri = createStableIRI(entity, 'customer-v1', rule, namespace, 'Customer');

    // ❓ Is content hash used when no ID?
    assert.ok(iri.includes('sha256:'));
    assert.ok(iri.startsWith('https://example.org/Customer/'));
  });

  test('3. Deterministic hash (same data → same hash)', () => {
    const entity1 = { name: 'Charlie', age: 30 };
    const entity2 = { name: 'Charlie', age: 30 };

    const rule = {
      pattern: '{namespace}Person/{hash}',
      keys: ['id']
    };
    const namespace = 'https://example.org/';

    const iri1 = createStableIRI(entity1, 'person-v1', rule, namespace, 'Person');
    const iri2 = createStableIRI(entity2, 'person-v1', rule, namespace, 'Person');

    // ❓ Same entity → same hash?
    assert.equal(iri1, iri2);
  });

  test('4. Skolem ID from content', () => {
    const values = { street: 'Main St', city: 'NYC' };
    const pattern = 'urn:skolem:address:{hash}';

    const skolemID = createSkolemID(pattern, values);

    // ❓ Is skolem IRI deterministic?
    assert.ok(skolemID.startsWith('urn:skolem:address:'));
    assert.ok(skolemID.includes('sha256:'));

    // ❓ Same values → same skolem?
    const skolemID2 = createSkolemID(pattern, { street: 'Main St', city: 'NYC' });
    assert.equal(skolemID, skolemID2);
  });

  test('5. IRI encoding (special characters)', () => {
    const entity = { id: 'Name With Spaces', type: 'Test' };
    const rule = {
      pattern: '{namespace}Customer/{id}',
      keys: ['id']
    };
    const namespace = 'https://example.org/';

    const iri = createStableIRI(entity, 'test-v1', rule, namespace, 'Customer');

    // ❓ Are spaces/special chars encoded?
    assert.ok(iri.includes('Name%20With%20Spaces'));
    assert.ok(!iri.includes(' '));
  });

  test('6. Extract ID from IRI (reverse operation)', () => {
    const iri = 'https://example.org/Customer/123';
    const pattern = '{namespace}Customer/{id}';
    const namespace = 'https://example.org/';

    const extracted = extractFromIRI(iri, pattern, namespace);

    // ❓ Is ID extracted correctly?
    assert.equal(extracted.id, '123');
  });

  test('7. Multiple keys in pattern', () => {
    const entity = { type: 'Order', localId: '456' };
    const rule = {
      pattern: '{namespace}{type}/{localId}',
      keys: ['type', 'localId']
    };
    const namespace = 'https://example.org/';

    const iri = createStableIRI(entity, 'order-v1', rule, namespace, 'Order');

    // ❓ Are multiple placeholders replaced?
    assert.equal(iri, 'https://example.org/Order/456');
  });

  test('8. Skolem pattern without prefix adds urn:skolem', () => {
    const values = { field: 'value' };
    const pattern = 'custom-id:{hash}'; // No urn:skolem prefix

    const skolemID = createSkolemID(pattern, values);

    // ❓ Is urn:skolem prefix added?
    assert.ok(skolemID.startsWith('urn:skolem:'));
  });
});
