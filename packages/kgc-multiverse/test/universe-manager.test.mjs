/**
 * KGC Multiverse - Universe Manager Tests
 * Tests state machine transitions, Q* ID generation, CRUD operations
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  UniverseManager,
  UniverseState,
  generateQStarID,
} from '../src/universe-manager.mjs';

describe('UniverseManager', () => {
  let manager;

  beforeEach(() => {
    manager = new UniverseManager();
  });

  describe('Q* ID Generation', () => {
    it('generates valid Q* identifier', async () => {
      const qid = await generateQStarID({ createdBy: 'test' });

      expect(qid.Q_ID).toMatch(/^Q\*_[a-f0-9]{16}$/);
      expect(qid.Q_RDF).toMatch(/^http:\/\/kgc\.io\/multiverse\/[a-f0-9]{16}$/);
      expect(qid.Q_PROV).toHaveProperty('createdAt');
      expect(typeof qid.Q_PROV.createdAt).toBe('bigint');
    });

    it('includes parent reference when forking', async () => {
      const parentID = 'Q*_0123456789abcdef';
      const qid = await generateQStarID({
        parentID,
        forkPoint: 123456n,
      });

      expect(qid.Q_PROV.parentID).toBe(parentID);
      expect(qid.Q_PROV.forkPoint).toBe(123456n);
    });

    it('generates unique IDs', async () => {
      const qid1 = await generateQStarID();
      const qid2 = await generateQStarID();

      expect(qid1.Q_ID).not.toBe(qid2.Q_ID);
    });
  });

  describe('Universe Creation', () => {
    it('creates universe in GENESIS state', async () => {
      const universe = await manager.createUniverse({ createdBy: 'alice' });

      expect(universe.state).toBe(UniverseState.GENESIS);
      expect(universe.eventCount).toBe(0);
      expect(universe.id.Q_ID).toMatch(/^Q\*_/);
    });

    it('stores created universe', async () => {
      const universe = await manager.createUniverse();
      const retrieved = manager.getUniverse(universe.id.Q_ID);

      expect(retrieved).not.toBeNull();
      expect(retrieved.id.Q_ID).toBe(universe.id.Q_ID);
    });

    it('returns immutable universe object', async () => {
      const universe = await manager.createUniverse();

      expect(() => {
        universe.state = 'ACTIVE';
      }).toThrow();
    });
  });

  describe('State Transitions', () => {
    it('transitions GENESIS → ACTIVE', async () => {
      const universe = await manager.createUniverse();
      const updated = manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);

      expect(updated.state).toBe(UniverseState.ACTIVE);
    });

    it('transitions ACTIVE → FORKED', async () => {
      const universe = await manager.createUniverse();
      manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);
      const updated = manager.transitionState(universe.id.Q_ID, UniverseState.FORKED);

      expect(updated.state).toBe(UniverseState.FORKED);
    });

    it('rejects FROZEN → ACTIVE transition', async () => {
      const universe = await manager.createUniverse();
      manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);
      const hash = 'a'.repeat(64);
      manager.freezeUniverse(universe.id.Q_ID, hash);

      expect(() => {
        manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);
      }).toThrow(/Invalid state transition/);
    });

    it('rejects DISCARDED → any transition', async () => {
      const universe = await manager.createUniverse();
      manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);
      manager.deleteUniverse(universe.id.Q_ID, { force: true });

      expect(() => {
        manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);
      }).toThrow();
    });
  });

  describe('Universe Forking', () => {
    it('creates fork from ACTIVE universe', async () => {
      const parent = await manager.createUniverse();
      manager.transitionState(parent.id.Q_ID, UniverseState.ACTIVE);

      const fork = await manager.forkUniverse(parent.id.Q_ID, { createdBy: 'bob' });

      expect(fork.state).toBe(UniverseState.FORKED);
      expect(fork.parent).toBe(parent.id.Q_ID);
      expect(fork.id.Q_PROV.parentID).toBe(parent.id.Q_ID);
    });

    it('auto-transitions GENESIS parent to ACTIVE before forking', async () => {
      const parent = await manager.createUniverse();
      expect(parent.state).toBe(UniverseState.GENESIS);

      const fork = await manager.forkUniverse(parent.id.Q_ID);

      const parentAfter = manager.getUniverse(parent.id.Q_ID);
      expect(parentAfter.state).toBe(UniverseState.ACTIVE);
      expect(fork.state).toBe(UniverseState.FORKED);
    });

    it('throws error if parent not found', async () => {
      await expect(
        manager.forkUniverse('Q*_nonexistent1234')
      ).rejects.toThrow(/not found/);
    });
  });

  describe('Universe Freezing', () => {
    it('freezes ACTIVE universe with hash', async () => {
      const universe = await manager.createUniverse();
      manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);

      const hash = 'a'.repeat(64);
      const frozen = manager.freezeUniverse(universe.id.Q_ID, hash);

      expect(frozen.state).toBe(UniverseState.FROZEN);
      expect(frozen.universeHash).toBe(hash);
    });

    it('rejects invalid hash format', async () => {
      const universe = await manager.createUniverse();
      manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);

      expect(() => {
        manager.freezeUniverse(universe.id.Q_ID, 'invalid-hash');
      }).toThrow(/64-char hex string/);
    });

    it('makes frozen universe immutable', async () => {
      const universe = await manager.createUniverse();
      manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);

      const hash = 'b'.repeat(64);
      manager.freezeUniverse(universe.id.Q_ID, hash);

      const frozen = manager.getUniverse(universe.id.Q_ID);

      // Frozen object should be immutable
      expect(Object.isFrozen(frozen)).toBe(true);
    });
  });

  describe('Universe Deletion', () => {
    it('deletes FROZEN universe', async () => {
      const universe = await manager.createUniverse();
      manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);
      const hash = 'c'.repeat(64);
      manager.freezeUniverse(universe.id.Q_ID, hash);

      const deleted = manager.deleteUniverse(universe.id.Q_ID);

      expect(deleted).toBe(true);
      const retrieved = manager.getUniverse(universe.id.Q_ID);
      expect(retrieved.state).toBe(UniverseState.DISCARDED);
    });

    it('requires force flag for ACTIVE universe', async () => {
      const universe = await manager.createUniverse();
      manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);

      expect(() => {
        manager.deleteUniverse(universe.id.Q_ID);
      }).toThrow(/force=true/);

      // With force flag, should succeed
      const deleted = manager.deleteUniverse(universe.id.Q_ID, { force: true });
      expect(deleted).toBe(true);
    });

    it('rejects delete of DISCARDED universe', async () => {
      const universe = await manager.createUniverse();
      manager.transitionState(universe.id.Q_ID, UniverseState.ACTIVE);
      manager.deleteUniverse(universe.id.Q_ID, { force: true });

      expect(() => {
        manager.deleteUniverse(universe.id.Q_ID);
      }).toThrow(/already DISCARDED/);
    });
  });

  describe('Universe Listing', () => {
    it('lists all universes', async () => {
      await manager.createUniverse();
      await manager.createUniverse();
      await manager.createUniverse();

      const all = manager.listUniverses();
      expect(all.length).toBe(3);
    });

    it('filters by state', async () => {
      const u1 = await manager.createUniverse();
      const u2 = await manager.createUniverse();
      manager.transitionState(u2.id.Q_ID, UniverseState.ACTIVE);

      const genesis = manager.listUniverses({ state: UniverseState.GENESIS });
      const active = manager.listUniverses({ state: UniverseState.ACTIVE });

      expect(genesis.length).toBe(1);
      expect(active.length).toBe(1);
    });

    it('filters by parent', async () => {
      const parent = await manager.createUniverse();
      manager.transitionState(parent.id.Q_ID, UniverseState.ACTIVE);

      await manager.forkUniverse(parent.id.Q_ID);
      await manager.forkUniverse(parent.id.Q_ID);

      const children = manager.listUniverses({ parent: parent.id.Q_ID });
      expect(children.length).toBe(2);
      expect(children.every(c => c.parent === parent.id.Q_ID)).toBe(true);
    });
  });

  describe('Universe Count', () => {
    it('returns correct count', async () => {
      expect(manager.count()).toBe(0);

      await manager.createUniverse();
      expect(manager.count()).toBe(1);

      await manager.createUniverse();
      await manager.createUniverse();
      expect(manager.count()).toBe(3);
    });
  });
});
