/**
 * @file Core RDF Diff Engine Tests (ULTRA-FAST)
 * @description 3 essential tests for diff operations - <50ms total
 * Uses pure functions and mocks, no external dependencies
 */

import { describe, it, expect } from 'vitest';

/**
 * Core diff functions (minimal implementation)
 */
function quadToTriple(quad) {
  return {
    subject: quad.subject?.value || quad.subject,
    predicate: quad.predicate?.value || quad.predicate,
    object: quad.object?.value || quad.object,
  };
}

function tripleKey(triple) {
  return `${triple.subject}|${triple.predicate}|${triple.object}`;
}

function diffGraphs(before, after) {
  const beforeKeys = new Set(before.map(tripleKey));
  const afterKeys = new Set(after.map(tripleKey));

  const added = after.filter(t => !beforeKeys.has(tripleKey(t)));
  const removed = before.filter(t => !afterKeys.has(tripleKey(t)));

  return { added, removed };
}

describe('RDF Diff Engine (ULTRA-FAST)', () => {
  it('should convert quad to triple', () => {
    const quad = {
      subject: { value: 'http://ex.org/s' },
      predicate: { value: 'http://ex.org/p' },
      object: { value: 'http://ex.org/o' },
    };

    const triple = quadToTriple(quad);

    expect(triple.subject).toBe('http://ex.org/s');
    expect(triple.predicate).toBe('http://ex.org/p');
    expect(triple.object).toBe('http://ex.org/o');
  });

  it('should create unique keys from triples', () => {
    const t1 = {
      subject: 'http://ex.org/alice',
      predicate: 'http://ex.org/knows',
      object: 'http://ex.org/bob',
    };
    const t2 = {
      subject: 'http://ex.org/alice',
      predicate: 'http://ex.org/knows',
      object: 'http://ex.org/charlie',
    };

    const k1 = tripleKey(t1);
    const k2 = tripleKey(t2);

    expect(k1).not.toBe(k2);
    expect(typeof k1).toBe('string');
    expect(typeof k2).toBe('string');
  });

  it('should detect added and removed triples', () => {
    const before = [
      { subject: 'http://ex.org/alice', predicate: 'http://ex.org/name', object: 'Alice' },
      { subject: 'http://ex.org/alice', predicate: 'http://ex.org/oldProp', object: 'oldValue' },
    ];
    const after = [
      { subject: 'http://ex.org/alice', predicate: 'http://ex.org/name', object: 'Alice' },
      { subject: 'http://ex.org/alice', predicate: 'http://ex.org/newProp', object: 'newValue' },
    ];

    const diff = diffGraphs(before, after);

    expect(diff.added).toHaveLength(1);
    expect(diff.removed).toHaveLength(1);
    expect(diff.added[0].predicate).toBe('http://ex.org/newProp');
    expect(diff.removed[0].predicate).toBe('http://ex.org/oldProp');
  });
});
