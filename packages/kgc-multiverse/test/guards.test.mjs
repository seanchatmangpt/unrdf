/**
 * KGC Multiverse - Guards Tests
 * Tests state machine guards and poka-yoke prevention
 */

import { describe, it, expect } from 'vitest';
import {
  guardStateTransition,
  guardMorphismApplication,
  guardMergePreconditions,
  guardFreezePreconditions,
  guardDeleteSafety,
  guardCanFork,
  guardNoUnfreeze,
  guardUniverseState,
  guardQStarID,
  guardUniverseHash,
} from '../src/guards.mjs';

describe('State Machine Guards', () => {
  describe('guardStateTransition', () => {
    it('allows valid GENESIS → ACTIVE transition', () => {
      expect(() => {
        guardStateTransition('GENESIS', 'ACTIVE');
      }).not.toThrow();
    });

    it('allows valid ACTIVE → FORKED transition', () => {
      expect(() => {
        guardStateTransition('ACTIVE', 'FORKED');
      }).not.toThrow();
    });

    it('allows valid ACTIVE → FROZEN transition', () => {
      expect(() => {
        guardStateTransition('ACTIVE', 'FROZEN');
      }).not.toThrow();
    });

    it('blocks FROZEN → ACTIVE transition (GR6)', () => {
      expect(() => {
        guardStateTransition('FROZEN', 'ACTIVE');
      }).toThrow(/Invalid transition FROZEN → ACTIVE/);
    });

    it('blocks FROZEN → FORKED transition (GR5)', () => {
      expect(() => {
        guardStateTransition('FROZEN', 'FORKED');
      }).toThrow(/Invalid transition/);
    });

    it('blocks transitions from DISCARDED (terminal state)', () => {
      expect(() => {
        guardStateTransition('DISCARDED', 'ACTIVE');
      }).toThrow(/terminal state/);
    });

    it('blocks MERGED → FORKED transition (GR9)', () => {
      expect(() => {
        guardStateTransition('MERGED', 'FORKED');
      }).toThrow(/Invalid transition/);
    });
  });

  describe('guardMorphismApplication', () => {
    it('allows morphism on ACTIVE universe', () => {
      expect(() => {
        guardMorphismApplication('ACTIVE');
      }).not.toThrow();
    });

    it('blocks morphism on FROZEN universe (GR3)', () => {
      expect(() => {
        guardMorphismApplication('FROZEN');
      }).toThrow(/FROZEN universe is immutable/);
    });

    it('blocks morphism on DISCARDED universe', () => {
      expect(() => {
        guardMorphismApplication('DISCARDED');
      }).toThrow(/Cannot apply morphism to DISCARDED/);
    });
  });

  describe('guardMergePreconditions', () => {
    it('allows merge of FORKED universe with no conflicts', () => {
      const fork = {
        state: 'FORKED',
        parent: 'Q*_0123456789abcdef',
      };

      expect(() => {
        guardMergePreconditions(fork, [], null);
      }).not.toThrow();
    });

    it('blocks merge without parent reference (GR8)', () => {
      const fork = {
        state: 'FORKED',
        parent: null,
      };

      expect(() => {
        guardMergePreconditions(fork, [], null);
      }).toThrow(/missing parent reference/);
    });

    it('blocks merge with unresolved conflicts (GR2)', () => {
      const fork = {
        state: 'FORKED',
        parent: 'Q*_0123456789abcdef',
      };
      const conflicts = [{ quad: 'conflict1' }, { quad: 'conflict2' }];

      expect(() => {
        guardMergePreconditions(fork, conflicts, null);
      }).toThrow(/requires conflict resolution.*2 conflicts/);
    });

    it('blocks merge of non-FORKED universe', () => {
      const universe = {
        state: 'ACTIVE',
        parent: 'Q*_0123456789abcdef',
      };

      expect(() => {
        guardMergePreconditions(universe, [], null);
      }).toThrow(/must be FORKED/);
    });
  });

  describe('guardFreezePreconditions', () => {
    it('allows freeze of ACTIVE universe with events', () => {
      const universe = {
        state: 'ACTIVE',
        eventCount: 10,
      };

      expect(() => {
        guardFreezePreconditions(universe);
      }).not.toThrow();
    });

    it('blocks freeze of FORKED universe (GR1)', () => {
      const universe = {
        state: 'FORKED',
        eventCount: 5,
      };

      expect(() => {
        guardFreezePreconditions(universe);
      }).toThrow(/Cannot freeze FORKED/);
    });

    it('blocks freeze of empty GENESIS (GR10)', () => {
      const universe = {
        state: 'GENESIS',
        eventCount: 0,
      };

      expect(() => {
        guardFreezePreconditions(universe);
      }).toThrow(/Cannot freeze empty GENESIS/);
    });

    it('blocks freeze of DISCARDED universe', () => {
      const universe = {
        state: 'DISCARDED',
        eventCount: 5,
      };

      expect(() => {
        guardFreezePreconditions(universe);
      }).toThrow(/Cannot freeze DISCARDED/);
    });
  });

  describe('guardDeleteSafety', () => {
    it('allows delete of FROZEN universe', () => {
      const universe = { state: 'FROZEN' };

      expect(() => {
        guardDeleteSafety(universe);
      }).not.toThrow();
    });

    it('blocks delete of ACTIVE without force (GR4)', () => {
      const universe = { state: 'ACTIVE' };

      expect(() => {
        guardDeleteSafety(universe);
      }).toThrow(/force=true/);
    });

    it('allows delete of ACTIVE with force flag', () => {
      const universe = { state: 'ACTIVE' };

      expect(() => {
        guardDeleteSafety(universe, { force: true });
      }).not.toThrow();
    });

    it('blocks delete of DISCARDED (idempotency)', () => {
      const universe = { state: 'DISCARDED' };

      expect(() => {
        guardDeleteSafety(universe);
      }).toThrow(/already DISCARDED/);
    });
  });

  describe('guardCanFork', () => {
    it('allows fork from ACTIVE', () => {
      expect(() => {
        guardCanFork('ACTIVE');
      }).not.toThrow();
    });

    it('allows fork from GENESIS', () => {
      expect(() => {
        guardCanFork('GENESIS');
      }).not.toThrow();
    });

    it('blocks fork from FROZEN (GR5)', () => {
      expect(() => {
        guardCanFork('FROZEN');
      }).toThrow(/Cannot fork FROZEN/);
    });

    it('blocks fork from MERGED (GR9)', () => {
      expect(() => {
        guardCanFork('MERGED');
      }).toThrow(/MERGED is transient/);
    });

    it('blocks fork from DISCARDED', () => {
      expect(() => {
        guardCanFork('DISCARDED');
      }).toThrow(/Cannot fork DISCARDED/);
    });
  });

  describe('guardQStarID', () => {
    it('validates correct Q* ID format', () => {
      expect(() => {
        guardQStarID('Q*_0123456789abcdef');
      }).not.toThrow();
    });

    it('rejects invalid format', () => {
      expect(() => {
        guardQStarID('invalid-id');
      }).toThrow(/Invalid Q\* ID format/);
    });

    it('rejects wrong prefix', () => {
      expect(() => {
        guardQStarID('R*_0123456789abcdef');
      }).toThrow(/Invalid Q\* ID format/);
    });
  });

  describe('guardUniverseHash', () => {
    it('validates 64-char hex hash', () => {
      expect(() => {
        guardUniverseHash('a'.repeat(64));
      }).not.toThrow();
    });

    it('rejects wrong length', () => {
      expect(() => {
        guardUniverseHash('a'.repeat(32));
      }).toThrow(/expected 64 hex chars/);
    });

    it('rejects non-hex characters', () => {
      expect(() => {
        guardUniverseHash('g'.repeat(64));
      }).toThrow(/Invalid hash format/);
    });
  });

  describe('guardUniverseState', () => {
    it('allows state in allowed list', () => {
      expect(() => {
        guardUniverseState('ACTIVE', ['ACTIVE', 'GENESIS']);
      }).not.toThrow();
    });

    it('blocks state not in allowed list', () => {
      expect(() => {
        guardUniverseState('FROZEN', ['ACTIVE', 'GENESIS']);
      }).toThrow(/not allowed.*expected: ACTIVE, GENESIS/);
    });
  });
});
