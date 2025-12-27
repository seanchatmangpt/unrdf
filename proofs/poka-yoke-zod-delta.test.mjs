import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { guardSerializedDeltaValid } from '../packages/kgc-4d/src/schemas/delta-schema.mjs';

describe('Poka-Yoke: Zod Delta Validation', () => {
  it('should accept valid serialized delta (NamedNode)', () => {
    const delta = {
      type: 'add',
      subject: 'http://example.org/resource',
      subjectType: 'NamedNode',
      predicate: 'http://example.org/predicate',
      object: {
        value: 'literal value',
        type: 'Literal',
      },
    };

    assert.doesNotThrow(() => guardSerializedDeltaValid(delta));
  });

  it('should reject delta with invalid type', () => {
    const delta = {
      type: 'modify',
      subject: 'http://example.org/resource',
      subjectType: 'NamedNode',
      predicate: 'http://example.org/predicate',
      object: { value: 'test', type: 'Literal' },
    };

    assert.throws(
      () => guardSerializedDeltaValid(delta),
      /Invalid enum value/
    );
  });

  it('should reject delta with missing object.value', () => {
    const delta = {
      type: 'add',
      subject: 'http://example.org/resource',
      subjectType: 'NamedNode',
      predicate: 'http://example.org/predicate',
      object: {
        type: 'Literal',
      },
    };

    assert.throws(
      () => guardSerializedDeltaValid(delta),
      /Required/
    );
  });

  it('should reject delta with invalid predicate (not URL)', () => {
    const delta = {
      type: 'add',
      subject: 'http://example.org/resource',
      subjectType: 'NamedNode',
      predicate: 'not-a-url',
      object: { value: 'test', type: 'Literal' },
    };

    assert.throws(
      () => guardSerializedDeltaValid(delta),
      /Invalid url/
    );
  });

  it('should accept delta with optional datatype', () => {
    const delta = {
      type: 'add',
      subject: 'http://example.org/resource',
      subjectType: 'NamedNode',
      predicate: 'http://example.org/predicate',
      object: {
        value: '42',
        type: 'Literal',
        datatype: 'http://www.w3.org/2001/XMLSchema#integer',
      },
    };

    assert.doesNotThrow(() => guardSerializedDeltaValid(delta));
  });

  it('should prevent malformed delta from causing TypeError', () => {
    const malformedDelta = {
      type: 'add',
      subject: 'http://example.org/resource',
      subjectType: 'NamedNode',
      predicate: 'http://example.org/predicate',
      object: null,
    };

    assert.throws(
      () => guardSerializedDeltaValid(malformedDelta),
      /Expected object/
    );
  });
});
