/**
 * KGC 4D Integration Tests - Real Git & Real RDF
 * Uses single shared Git repo initialized once for speed
 * Target: <5s total execution (ARD-mandated isomorphic-git)
 * NO Git CLI - pure JavaScript implementation
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { tmpdir } from 'os';
import { join } from 'path';
import { rmSync, mkdirSync, existsSync } from 'fs';

import { KGCStore, GitBackbone, freezeUniverse, now } from '../src/index.mjs';
import { dataFactory } from '@unrdf/oxigraph';
import { GRAPHS, EVENT_TYPES } from '../src/constants.mjs';

// Single shared test directory - initialized ONCE via isomorphic-git
const TEST_DIR = join(tmpdir(), `kgc-integration-${Date.now()}`);
let sharedGit;

describe('KGC 4D Integration - Real Git & RDF (<5s)', () => {
  beforeAll(() => {
    // Single Git init for ALL tests - GitBackbone uses isomorphic-git (ARD-mandated)
    mkdirSync(TEST_DIR, { recursive: true });
    sharedGit = new GitBackbone(TEST_DIR);
    // Note: _ensureInit() is called automatically on first operation
  });

  afterAll(() => {
    try {
      rmSync(TEST_DIR, { recursive: true, force: true });
    } catch {}
  });

  it('IT1: Real Git commit and retrieval', async () => {
    const nquads = '<http://test.org/a> <http://test.org/p> "v1" <http://kgc.io/Universe> .';
    const hash = await sharedGit.commitSnapshot(nquads, 'Test snapshot');

    expect(hash).toHaveLength(40);

    const retrieved = await sharedGit.readSnapshot(hash);
    expect(retrieved).toBe(nquads);
  });

  it('IT2: Real RDF persistence in Universe graph', async () => {
    const store = new KGCStore();

    await store.appendEvent(
      { type: EVENT_TYPES.CREATE, payload: {} },
      [{
        type: 'add',
        subject: dataFactory.namedNode('http://example.org/Alice'),
        predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
        object: dataFactory.literal('Alice'),
      }]
    );

    const quads = [...store.match(
      dataFactory.namedNode('http://example.org/Alice'),
      null, null,
      dataFactory.namedNode(GRAPHS.UNIVERSE)
    )];

    expect(quads.length).toBe(1);
    expect(quads[0].object.value).toBe('Alice');
  });

  it('IT3: Real freeze to Git with BLAKE3 hash', async () => {
    const store = new KGCStore();

    await store.appendEvent(
      { type: EVENT_TYPES.CREATE, payload: {} },
      [{
        type: 'add',
        subject: dataFactory.namedNode('http://example.org/Data'),
        predicate: dataFactory.namedNode('http://example.org/value'),
        object: dataFactory.literal('test'),
      }]
    );

    const freeze = await freezeUniverse(store, sharedGit);

    expect(freeze.git_ref).toHaveLength(40);
    expect(freeze.universe_hash).toHaveLength(64);
    expect(freeze.universe_hash).toMatch(/^[a-f0-9]{64}$/);
  });

  it('IT4: Snapshot retrieval from real Git', async () => {
    const store = new KGCStore();

    await store.appendEvent(
      { type: EVENT_TYPES.CREATE, payload: {} },
      [{
        type: 'add',
        subject: dataFactory.namedNode('http://example.org/Entity'),
        predicate: dataFactory.namedNode('http://example.org/status'),
        object: dataFactory.literal('active'),
      }]
    );

    const freeze = await freezeUniverse(store, sharedGit);
    const snapshot = await sharedGit.readSnapshot(freeze.git_ref);

    expect(snapshot).toContain('http://example.org/Entity');
    expect(snapshot).toContain('active');
  });

  it('IT5: Event log integrity with monotonic ordering', async () => {
    const store = new KGCStore();
    const timestamps = [];

    for (let i = 0; i < 20; i++) {
      const { receipt } = await store.appendEvent(
        { type: EVENT_TYPES.CREATE, payload: { seq: i } },
        []
      );
      timestamps.push(BigInt(receipt.t_ns));
    }

    let violations = 0;
    for (let i = 1; i < timestamps.length; i++) {
      if (timestamps[i] <= timestamps[i - 1]) violations++;
    }

    expect(violations).toBe(0);
  });

  it('IT6: Multi-tenant isolation', async () => {
    const storeA = new KGCStore();
    const storeB = new KGCStore();

    await storeA.appendEvent(
      { type: EVENT_TYPES.CREATE, payload: { tenant: 'A' } },
      [{
        type: 'add',
        subject: dataFactory.namedNode('http://acme.org/Data'),
        predicate: dataFactory.namedNode('http://example.org/name'),
        object: dataFactory.literal('ACME'),
      }]
    );

    await storeB.appendEvent(
      { type: EVENT_TYPES.CREATE, payload: { tenant: 'B' } },
      [{
        type: 'add',
        subject: dataFactory.namedNode('http://widgets.org/Data'),
        predicate: dataFactory.namedNode('http://example.org/name'),
        object: dataFactory.literal('Widgets'),
      }]
    );

    const bInA = [...storeA.match(
      dataFactory.namedNode('http://widgets.org/Data'),
      null, null, dataFactory.namedNode(GRAPHS.UNIVERSE)
    )];

    expect(bInA.length).toBe(0);
  });

  it('IT7: Delete operations', async () => {
    const store = new KGCStore();
    const triple = {
      type: 'add',
      subject: dataFactory.namedNode('http://example.org/ToDelete'),
      predicate: dataFactory.namedNode('http://example.org/status'),
      object: dataFactory.literal('temporary'),
    };

    await store.appendEvent({ type: EVENT_TYPES.CREATE, payload: {} }, [triple]);

    let quads = [...store.match(
      dataFactory.namedNode('http://example.org/ToDelete'),
      null, null, dataFactory.namedNode(GRAPHS.UNIVERSE)
    )];
    expect(quads.length).toBe(1);

    await store.appendEvent(
      { type: EVENT_TYPES.DELETE, payload: {} },
      [{ ...triple, type: 'delete' }]
    );

    quads = [...store.match(
      dataFactory.namedNode('http://example.org/ToDelete'),
      null, null, dataFactory.namedNode(GRAPHS.UNIVERSE)
    )];
    expect(quads.length).toBe(0);
  });

  it('IT8: BLAKE3 determinism', async () => {
    const store = new KGCStore();

    await store.appendEvent(
      { type: EVENT_TYPES.CREATE, payload: {} },
      [{
        type: 'add',
        subject: dataFactory.namedNode('http://example.org/Deterministic'),
        predicate: dataFactory.namedNode('http://example.org/value'),
        object: dataFactory.literal('fixed'),
      }]
    );

    const freeze1 = await freezeUniverse(store, sharedGit);
    const freeze2 = await freezeUniverse(store, sharedGit);

    // Same data = same BLAKE3 hash (Git refs differ due to timestamp)
    expect(freeze1.universe_hash).toBe(freeze2.universe_hash);
  });
});
