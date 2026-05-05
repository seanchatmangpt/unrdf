import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { composeGuards, composeGuardsAccumulate } from '../packages/kgc-4d/src/guards/compose.mjs';

describe('Poka-Yoke: Guard Composition', () => {
  it('should run all guards in order', () => {
    let order = [];
    const guard1 = () => { order.push(1); };
    const guard2 = () => { order.push(2); };
    const guard3 = () => { order.push(3); };

    const composed = composeGuards(guard1, guard2, guard3);
    composed();

    assert.deepEqual(order, [1, 2, 3]);
  });

  it('should short-circuit on first failure', () => {
    let order = [];
    const guard1 = () => { order.push(1); };
    const guard2 = () => { order.push(2); throw new Error('Guard 2 failed'); };
    const guard3 = () => { order.push(3); };

    const composed = composeGuards(guard1, guard2, guard3);
    
    assert.throws(() => composed(), /Guard 2 failed/);
    assert.deepEqual(order, [1, 2]);
  });

  it('should accumulate errors in soft-fail mode', () => {
    const guard1 = () => { throw new Error('Error 1'); };
    const guard2 = () => { throw new Error('Error 2'); };
    const guard3 = () => { /* passes */ };

    const composed = composeGuardsAccumulate(guard1, guard2, guard3);
    const result = composed();

    assert.equal(result.passed, false);
    assert.equal(result.errors.length, 2);
    assert.ok(result.errors.includes('Error 1'));
    assert.ok(result.errors.includes('Error 2'));
  });

  it('should pass arguments to all guards', () => {
    let received = [];
    const guard1 = (a, b) => { received.push([a, b]); };
    const guard2 = (a, b) => { received.push([a, b]); };

    const composed = composeGuards(guard1, guard2);
    composed(42, 'test');

    assert.deepEqual(received, [[42, 'test'], [42, 'test']]);
  });

  it('should reject non-function guards when composed function is called', () => {
    const composed = composeGuards('not a function', () => {});
    assert.throws(
      () => composed(),
      /All guards must be functions/
    );
  });

  it('should require at least one guard', () => {
    assert.throws(
      () => composeGuards(),
      /At least one guard required/
    );
  });
});
