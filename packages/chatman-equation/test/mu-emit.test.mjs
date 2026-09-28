/**
 * @file The Turtle emitted by mu() must be parseable and must not be injectable via state values.
 */

import { describe, it, expect } from 'vitest';
import { randomUUID } from 'crypto';
import { createStore } from '@unrdf/oxigraph';
import { mu } from '../src/mu-transform.mjs';

function run(state, ops, operatorName = 'test_operator') {
  const obs = { id: randomUUID(), timestamp: new Date().toISOString(), domain: 'market', state };
  const delta = { id: randomUUID(), timestamp: new Date().toISOString(), domain: 'market', operations: ops };
  const operator = {
    type: 'merge',
    name: operatorName,
    domain: 'market',
    idempotent: true,
    deterministic: true,
    rules: [],
  };
  return mu(obs, delta, operator).artifact.proof.content;
}

function parse(turtle) {
  const store = createStore();
  store.load(turtle, { format: 'text/turtle' });
  return store;
}

describe('mu() Turtle emission', () => {
  it('emits Turtle that parses for plain numeric and string state', () => {
    const turtle = run({ customers: 100, label: 'a' }, [
      { op: 'update', field: 'customers', value: 150 },
      { op: 'update', field: 'label', value: 'plain' },
    ]);
    expect(parse(turtle).size).toBeGreaterThan(5);
  });

  it('emits parseable Turtle when a state value contains quotes, backslashes and newlines', () => {
    const tricky = 'he said "hi"\\ and\nleft';
    const turtle = run({ note: '' }, [{ op: 'update', field: 'note', value: tricky }]);
    const store = parse(turtle);
    const objects = store.match().map(q => q.object.value);
    expect(objects).toContain(tricky);
  });

  it('a malicious value cannot inject extra triples', () => {
    const evil = '" ; ex:injected "yes';
    const turtle = run({ note: '' }, [{ op: 'update', field: 'note', value: evil }]);
    const store = parse(turtle);
    const predicates = store.match().map(q => q.predicate.value);
    expect(predicates.some(p => p.endsWith('injected'))).toBe(false);
  });

  it('operator names are escaped as literals', () => {
    const turtle = run({ a: 1 }, [{ op: 'update', field: 'a', value: 2 }], 'op"; ex:pwn "1');
    const predicates = parse(turtle).match().map(q => q.predicate.value);
    expect(predicates.some(p => p.endsWith('pwn'))).toBe(false);
  });
});
