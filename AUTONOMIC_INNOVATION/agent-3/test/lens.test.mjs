/**
 * @fileoverview Tests for lens definition and validation
 */

import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { defineLens } from '../src/lens.mjs';

describe('Lens Definition', () => {
  test('1. Define valid lens with all fields', () => {
    const profile = {
      namespace: 'https://example.org/',
      prefixes: {
        schema: 'http://schema.org/',
        xsd: 'http://www.w3.org/2001/XMLSchema#'
      },
      conventions: {
        idField: 'id',
        case: 'camelCase'
      }
    };

    const mappings = {
      Customer: {
        subject: {
          pattern: '{namespace}Customer/{id}',
          keys: ['id']
        },
        type: 'schema:Customer',
        predicates: {
          name: { iri: 'schema:name', required: true },
          email: { iri: 'schema:email', required: true }
        }
      }
    };

    const lens = defineLens('customer-v1', profile, mappings);

    // ❓ Does defineLens accept valid schema?
    assert.ok(lens);
    assert.equal(lens.id, 'customer-v1');
    assert.equal(lens.version, '1.0.0');

    // ❓ Are all fields preserved?
    assert.equal(lens.profile.namespace, 'https://example.org/');
    assert.ok(lens.mappings.Customer);

    // ❓ Are prefixed IRIs expanded?
    assert.equal(lens.mappings.Customer.type, 'http://schema.org/Customer');
    assert.equal(lens.mappings.Customer.predicates.name.iri, 'http://schema.org/name');
  });

  test('2. Reject invalid IRI pattern', () => {
    const profile = {
      namespace: 'https://example.org/',
      prefixes: {},
      conventions: { idField: 'id' }
    };

    const mappings = {
      Customer: {
        subject: {
          pattern: 'invalid-pattern-no-placeholder', // ❌ No placeholder
          keys: ['id']
        },
        predicates: {}
      }
    };

    // ❓ Does validation catch malformed patterns?
    assert.throws(() => {
      defineLens('test', profile, mappings);
    }, /Invalid IRI pattern/);
  });

  test('3. Normalize prefixed IRIs', () => {
    const profile = {
      namespace: 'https://example.org/',
      prefixes: {
        schema: 'http://schema.org/',
        foaf: 'http://xmlns.com/foaf/0.1/'
      },
      conventions: { idField: 'id' }
    };

    const mappings = {
      Person: {
        subject: {
          pattern: '{namespace}Person/{id}',
          keys: ['id']
        },
        type: 'foaf:Person',
        predicates: {
          name: { iri: 'foaf:name' },
          email: { iri: 'schema:email' }
        }
      }
    };

    const lens = defineLens('person-v1', profile, mappings);

    // ❓ Are prefixes expanded correctly?
    assert.equal(lens.mappings.Person.type, 'http://xmlns.com/foaf/0.1/Person');
    assert.equal(lens.mappings.Person.predicates.name.iri, 'http://xmlns.com/foaf/0.1/name');
    assert.equal(lens.mappings.Person.predicates.email.iri, 'http://schema.org/email');
  });

  test('4. Validate required fields', () => {
    const profile = {
      namespace: 'https://example.org/',
      prefixes: {},
      conventions: { idField: 'id' }
    };

    // ❓ Missing mappings
    assert.throws(() => {
      defineLens('test', profile, null);
    }, /mappings/);

    // ❓ Invalid version format
    const invalidProfile = {
      namespace: 'not-a-url',
      prefixes: {},
      conventions: { idField: 'id' }
    };

    assert.throws(() => {
      defineLens('test', invalidProfile, {});
    }, /url/);
  });

  test('5. Freeze lens object (immutable)', () => {
    const profile = {
      namespace: 'https://example.org/',
      prefixes: { schema: 'http://schema.org/' },
      conventions: { idField: 'id' }
    };

    const mappings = {
      Customer: {
        subject: {
          pattern: '{namespace}Customer/{id}',
          keys: ['id']
        },
        predicates: {
          name: { iri: 'schema:name' }
        }
      }
    };

    const lens = defineLens('test', profile, mappings);

    // ❓ Can lens be modified after creation?
    assert.equal(Object.isFrozen(lens), true);

    // ❓ Attempting to modify throws
    assert.throws(() => {
      lens.id = 'modified';
    });
  });

  test('6. Sort entity types and predicates canonically', () => {
    const profile = {
      namespace: 'https://example.org/',
      prefixes: { schema: 'http://schema.org/' },
      conventions: { idField: 'id' }
    };

    const mappings = {
      ZCustomer: {
        subject: { pattern: '{namespace}Customer/{id}', keys: ['id'] },
        predicates: {
          zfield: { iri: 'schema:zfield' },
          afield: { iri: 'schema:afield' },
          mfield: { iri: 'schema:mfield' }
        }
      },
      ACustomer: {
        subject: { pattern: '{namespace}Customer/{id}', keys: ['id'] },
        predicates: {}
      }
    };

    const lens = defineLens('test', profile, mappings);

    // ❓ Are entity types sorted?
    const entityTypes = Object.keys(lens.mappings);
    assert.deepEqual(entityTypes, ['ACustomer', 'ZCustomer']);

    // ❓ Are predicates sorted?
    const predicates = Object.keys(lens.mappings.ZCustomer.predicates);
    assert.deepEqual(predicates, ['afield', 'mfield', 'zfield']);
  });
});
