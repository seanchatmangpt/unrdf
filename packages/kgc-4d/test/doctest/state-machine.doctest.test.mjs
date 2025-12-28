import { describe, test, expect } from 'vitest';

import { UniverseStateMachine } from '../../src/state-machine.mjs'

describe('Doctests: state-machine.mjs', () => {
  test('UniverseStateSchema example 1 (line 1)', async () => {
    const sm = new UniverseStateMachine();
sm.guardMutableOperation('appendEvent'); // OK in MUTABLE
sm.freeze(); // Transition to FROZEN
sm.guardMutableOperation('appendEvent'); // Throws: cannot mutate FROZEN universe
  });
});
