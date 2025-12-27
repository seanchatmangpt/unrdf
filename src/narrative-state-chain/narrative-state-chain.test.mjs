/**
 * @fileoverview Tests for narrative-state-chain
 *
 * **Coverage**:
 * - UniverseStore and SceneStore
 * - Reconciliation engine
 * - Guard enforcement
 * - Receipt generation and verification
 * - Bridge system
 *
 * @module narrative-state-chain/tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  UniverseStore,
  SceneStore,
  reconcile,
  checkInvariants,
  checkMinimality,
  createIdentityReconcile,
  evaluateGuard,
  evaluateAllGuards,
  createAllowAllGuard,
  createDenyAllGuard,
  createAgentWhitelistGuard,
  generateReceipt,
  hashReceipt,
  verifyReceiptChain,
  Bridge,
  crossUniverseCall,
  createBidirectionalBridge,
} from './index.mjs';

describe('UniverseStore', () => {
  let store;

  beforeEach(() => {
    store = new UniverseStore();
  });

  it('should create a universe', async () => {
    const universe = await store.create({
      schema: 'http://example.org/schema#',
      reconcile: createIdentityReconcile(),
      metadata: { name: 'TestUniverse' },
    });

    expect(universe).toBeDefined();
    expect(universe.id).toBeDefined();
    expect(universe.schema).toBe('http://example.org/schema#');
    expect(universe.metadata.name).toBe('TestUniverse');
  });

  it('should retrieve a universe by ID', async () => {
    const created = await store.create({
      schema: 'http://example.org/schema#',
      reconcile: createIdentityReconcile(),
      metadata: { name: 'TestUniverse' },
    });

    const retrieved = store.get(created.id);
    expect(retrieved).toEqual(created);
  });

  it('should list all universe IDs', async () => {
    await store.create({
      schema: 'http://example.org/schema1#',
      reconcile: createIdentityReconcile(),
      metadata: { name: 'Universe1' },
    });

    await store.create({
      schema: 'http://example.org/schema2#',
      reconcile: createIdentityReconcile(),
      metadata: { name: 'Universe2' },
    });

    const ids = store.list();
    expect(ids).toHaveLength(2);
  });

  it('should delete a universe', async () => {
    const created = await store.create({
      schema: 'http://example.org/schema#',
      reconcile: createIdentityReconcile(),
      metadata: { name: 'TestUniverse' },
    });

    const deleted = store.delete(created.id);
    expect(deleted).toBe(true);

    const retrieved = store.get(created.id);
    expect(retrieved).toBeNull();
  });
});

describe('SceneStore', () => {
  let universeStore;
  let sceneStore;
  let universe;

  beforeEach(async () => {
    universeStore = new UniverseStore();
    sceneStore = new SceneStore(universeStore);

    universe = await universeStore.create({
      schema: 'http://example.org/schema#',
      reconcile: createIdentityReconcile(),
      guards: [createAllowAllGuard()],
      metadata: { name: 'TestUniverse' },
    });
  });

  it('should add a scene to universe', async () => {
    const scene = await sceneStore.add(
      universe.id,
      [{ type: 'observation', value: 42 }],
      { count: 1 },
      { agent: 'test@example.com' }
    );

    expect(scene).toBeDefined();
    expect(scene.id).toBeDefined();
    expect(scene.universeId).toBe(universe.id);
    expect(scene.observations).toHaveLength(1);
    expect(scene.delta).toEqual({ count: 1 });
    expect(scene.receipts).toHaveLength(1);
  });

  it('should fail when universe does not exist', async () => {
    await expect(
      sceneStore.add(
        'nonexistent-universe-id',
        [],
        {},
        { agent: 'test@example.com' }
      )
    ).rejects.toThrow('Universe not found');
  });

  it('should reject scene when guards fail', async () => {
    const restrictedUniverse = await universeStore.create({
      schema: 'http://example.org/schema#',
      reconcile: createIdentityReconcile(),
      guards: [createDenyAllGuard()],
      metadata: { name: 'RestrictedUniverse' },
    });

    await expect(
      sceneStore.add(
        restrictedUniverse.id,
        [],
        { count: 1 },
        { agent: 'test@example.com' }
      )
    ).rejects.toThrow('Scene not admissible');
  });

  it('should retrieve scene by ID', async () => {
    const created = await sceneStore.add(
      universe.id,
      [{ type: 'observation' }],
      { count: 1 },
      { agent: 'test@example.com' }
    );

    const retrieved = sceneStore.get(created.id);
    expect(retrieved).toEqual(created);
  });

  it('should maintain scene history', async () => {
    await sceneStore.add(universe.id, [], { a: 1 }, { agent: 'test@example.com' });
    await sceneStore.add(universe.id, [], { b: 2 }, { agent: 'test@example.com' });
    await sceneStore.add(universe.id, [], { c: 3 }, { agent: 'test@example.com' });

    const history = sceneStore.getHistory(universe.id);
    expect(history).toHaveLength(3);
  });

  it('should replay scene history', async () => {
    const scene1 = await sceneStore.add(universe.id, [], { a: 1 }, { agent: 'test@example.com' });
    const scene2 = await sceneStore.add(universe.id, [], { b: 2 }, { agent: 'test@example.com' });
    const scene3 = await sceneStore.add(universe.id, [], { c: 3 }, { agent: 'test@example.com' });

    const finalState = await sceneStore.replay(universe.id, scene1.id, scene3.id);

    expect(finalState).toMatchObject({
      a: 1,
      b: 2,
      c: 3,
    });
  });
});

describe('Reconciliation Engine', () => {
  it('should execute reconciliation', async () => {
    const universe = {
      reconcile: async (state, observations) => ({
        consequences: observations.map(o => ({ ...o, processed: true })),
        artifacts: { processedCount: observations.length },
        errors: [],
      }),
      invariants: [],
    };

    const result = await reconcile(universe, {}, [{ value: 42 }]);

    expect(result.consequences).toHaveLength(1);
    expect(result.consequences[0].processed).toBe(true);
    expect(result.artifacts.processedCount).toBe(1);
    expect(result.errors).toHaveLength(0);
  });

  it('should check invariants', async () => {
    const universe = {
      invariants: [
        {
          id: 'positive-balance',
          name: 'Balance must be positive',
          predicate: (state) => state.balance >= 0,
        },
      ],
    };

    const violations1 = await checkInvariants(universe, { balance: 10 });
    expect(violations1).toHaveLength(0);

    const violations2 = await checkInvariants(universe, { balance: -5 });
    expect(violations2).toHaveLength(1);
    expect(violations2[0]).toContain('positive-balance');
  });

  it('should check minimality', async () => {
    const delta = { a: 2, b: 3 };
    const previousState = { a: 1, c: 4 };

    const result = await checkMinimality(delta, previousState);

    expect(result.minimal).toBe(true);
    expect(result.proof).toBeDefined();
  });

  it('should detect non-minimal delta', async () => {
    const delta = { a: 1, b: 3 };
    const previousState = { a: 1, c: 4 };

    const result = await checkMinimality(delta, previousState);

    expect(result.minimal).toBe(false);
    expect(result.redundantKeys).toContain('a');
  });
});

describe('Guard Enforcement', () => {
  it('should evaluate allow-all guard', async () => {
    const guard = createAllowAllGuard();

    const result = await evaluateGuard(
      guard,
      'test@example.com',
      'write',
      { resource: 'doc1' }
    );

    expect(result.passed).toBe(true);
    expect(result.guardId).toBe('allow-all');
    expect(result.proof).toBeDefined();
  });

  it('should evaluate deny-all guard', async () => {
    const guard = createDenyAllGuard();

    const result = await evaluateGuard(
      guard,
      'test@example.com',
      'write',
      { resource: 'doc1' }
    );

    expect(result.passed).toBe(false);
    expect(result.reason).toContain('denied');
  });

  it('should evaluate whitelist guard', async () => {
    const guard = createAgentWhitelistGuard(
      ['alice@example.com', 'bob@example.com'],
      'whitelist',
      'Whitelist Guard'
    );

    const result1 = await evaluateGuard(guard, 'alice@example.com', 'write', {});
    expect(result1.passed).toBe(true);

    const result2 = await evaluateGuard(guard, 'charlie@example.com', 'write', {});
    expect(result2.passed).toBe(false);
  });

  it('should evaluate all guards', async () => {
    const universe = {
      guards: [
        createAllowAllGuard('guard1', 'Guard 1'),
        createAllowAllGuard('guard2', 'Guard 2'),
      ],
    };

    const results = await evaluateAllGuards(universe, 'test@example.com', {
      observations: [],
      delta: {},
    });

    expect(results).toHaveLength(2);
    expect(results.every(r => r.passed)).toBe(true);
  });
});

describe('Receipt Generation', () => {
  it('should generate receipt', async () => {
    const receipt = await generateReceipt({
      sceneId: 'scene-123',
      universeId: 'universe-456',
      admissibilityChecks: [
        {
          guardId: 'guard1',
          passed: true,
          timestamp: new Date(),
        },
      ],
      delta: { count: 1 },
    });

    expect(receipt).toBeDefined();
    expect(receipt.sceneId).toBe('scene-123');
    expect(receipt.universeId).toBe('universe-456');
    expect(receipt.receiptHash).toBeDefined();
    expect(receipt.minimalityProof).toBeDefined();
  });

  it('should verify receipt chain', async () => {
    const receipt1 = await generateReceipt({
      sceneId: 'scene-1',
      universeId: 'universe-1',
      admissibilityChecks: [],
      delta: { a: 1 },
    });

    const receipt2 = await generateReceipt({
      sceneId: 'scene-2',
      universeId: 'universe-1',
      admissibilityChecks: [],
      delta: { b: 2 },
      previousReceipt: receipt1,
    });

    const result = await verifyReceiptChain([receipt1, receipt2]);

    expect(result.valid).toBe(true);
    expect(result.errors).toHaveLength(0);
  });

  it('should detect broken receipt chain', async () => {
    const receipt1 = await generateReceipt({
      sceneId: 'scene-1',
      universeId: 'universe-1',
      admissibilityChecks: [],
      delta: { a: 1 },
    });

    const receipt2 = await generateReceipt({
      sceneId: 'scene-2',
      universeId: 'universe-1',
      admissibilityChecks: [],
      delta: { b: 2 },
    });

    // Tamper with chain
    receipt2.previousReceiptHash = 'invalid-hash';

    const result = await verifyReceiptChain([receipt1, receipt2]);

    expect(result.valid).toBe(false);
    expect(result.errors.length).toBeGreaterThan(0);
  });
});

describe('Bridge System', () => {
  let universeA;
  let universeB;

  beforeEach(async () => {
    const store = new UniverseStore();

    universeA = await store.create({
      schema: 'http://example.org/schemaA#',
      reconcile: createIdentityReconcile(),
      metadata: { name: 'UniverseA' },
    });

    universeB = await store.create({
      schema: 'http://example.org/schemaB#',
      reconcile: createIdentityReconcile(),
      metadata: { name: 'UniverseB' },
    });
  });

  it('should create a bridge', async () => {
    const bridge = await Bridge.define(
      universeA,
      universeB,
      (value) => ({ ...value, transformed: true }),
      async (value) => value.count >= 0,
      { name: 'A to B Bridge' }
    );

    expect(bridge).toBeDefined();
    expect(bridge.id).toBeDefined();
    expect(bridge.sourceUniverseId).toBe(universeA.id);
    expect(bridge.targetUniverseId).toBe(universeB.id);
  });

  it('should verify bridge', async () => {
    const bridge = await Bridge.define(
      universeA,
      universeB,
      (value) => ({ ...value, namespace: 'B' }),
      async () => true,
      { name: 'Test Bridge' }
    );

    const result = await Bridge.verify(bridge);

    expect(result.valid).toBe(true);
    expect(result.typePreserving).toBe(true);
    expect(result.invariantsHold).toBe(true);
  });

  it('should execute cross-universe call', async () => {
    const bridge = await Bridge.define(
      universeA,
      universeB,
      (value) => ({ ...value, count: value.count * 2 }),
      async (value) => value.count >= 0,
      { name: 'Doubler Bridge' }
    );

    // Grant access
    Bridge.grantAccess(bridge, 'test@example.com', 'execute');

    const result = await crossUniverseCall(
      bridge,
      { count: 5 },
      'test@example.com'
    );

    expect(result.success).toBe(true);
    expect(result.result.count).toBe(10);
  });

  it('should reject call without permission', async () => {
    const bridge = await Bridge.define(
      universeA,
      universeB,
      (value) => value,
      async () => true,
      { name: 'Test Bridge' }
    );

    const result = await crossUniverseCall(
      bridge,
      { count: 5 },
      'unauthorized@example.com'
    );

    expect(result.success).toBe(false);
    expect(result.error).toContain('permission');
  });

  it('should create bidirectional bridge', async () => {
    const { forward, reverse } = await createBidirectionalBridge(
      universeA,
      universeB,
      (v) => ({ ...v, context: 'B' }),
      (v) => ({ ...v, context: 'A' }),
      async () => true,
      { name: 'Bidirectional Bridge' }
    );

    expect(forward.sourceUniverseId).toBe(universeA.id);
    expect(forward.targetUniverseId).toBe(universeB.id);
    expect(reverse.sourceUniverseId).toBe(universeB.id);
    expect(reverse.targetUniverseId).toBe(universeA.id);
  });
});
