import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { UniverseStateMachine } from '../packages/kgc-4d/src/state-machine.mjs';

describe('Poka-Yoke: Sealed Universe State Machine', () => {
  it('should allow appendEvent in MUTABLE state', () => {
    const sm = new UniverseStateMachine();
    assert.doesNotThrow(() => sm.guardMutableOperation('appendEvent'));
  });

  it('should reject appendEvent in FROZEN state', () => {
    const sm = new UniverseStateMachine();
    sm.freeze();
    assert.throws(
      () => sm.guardMutableOperation('appendEvent'),
      /Universe is FROZEN/
    );
  });

  it('should reject appendEvent in SEALED state', () => {
    const sm = new UniverseStateMachine();
    sm.freeze();
    sm.seal();
    assert.throws(
      () => sm.guardMutableOperation('appendEvent'),
      /Universe is SEALED/
    );
  });

  it('should prevent freeze to mutable transition', () => {
    const sm = new UniverseStateMachine();
    sm.freeze();
    assert.equal(sm.state, 'FROZEN');
  });

  it('should prevent double freeze', () => {
    const sm = new UniverseStateMachine();
    sm.freeze();
    assert.throws(() => sm.freeze(), /already FROZEN/);
  });

  it('should prevent seal without freeze', () => {
    const sm = new UniverseStateMachine();
    assert.throws(() => sm.seal(), /Must freeze universe first/);
  });

  it('should allow freeze to seal transition', () => {
    const sm = new UniverseStateMachine();
    sm.freeze();
    assert.doesNotThrow(() => sm.seal());
    assert.equal(sm.state, 'SEALED');
    assert.equal(sm.isTerminal(), true);
  });
});
