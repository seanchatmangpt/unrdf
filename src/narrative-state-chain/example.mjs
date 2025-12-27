/**
 * @fileoverview Smoke test example for narrative-state-chain
 *
 * **Purpose**: Verify all modules can be imported and basic operations work
 *
 * @module narrative-state-chain/example
 */

import {
  UniverseStore,
  SceneStore,
  createIdentityReconcile,
  createAllowAllGuard,
  generateReceipt,
  Bridge,
} from './index.mjs';

/**
 * Run smoke test
 */
async function runSmokeTest() {
  console.log('🚀 Starting narrative-state-chain smoke test...\n');

  // 1. Create stores
  console.log('1. Creating UniverseStore and SceneStore...');
  const universeStore = new UniverseStore();
  const sceneStore = new SceneStore(universeStore);
  console.log('   ✅ Stores created\n');

  // 2. Create a universe
  console.log('2. Creating Universe...');
  const universe = await universeStore.create({
    schema: 'http://example.org/narrative-schema#',
    reconcile: createIdentityReconcile(),
    guards: [createAllowAllGuard('allow-all', 'Allow All Guard')],
    invariants: [
      {
        id: 'positive-count',
        name: 'Count must be non-negative',
        predicate: (state) => !state.count || state.count >= 0,
      },
    ],
    metadata: {
      name: 'Example Universe',
      description: 'A simple universe for demonstration',
      version: '1.0.0',
    },
  });
  console.log(`   ✅ Universe created: ${universe.id}`);
  console.log(`   📝 Schema: ${universe.schema}`);
  console.log(`   📋 Guards: ${universe.guards.length}`);
  console.log(`   📏 Invariants: ${universe.invariants.length}\n`);

  // 3. Add first scene
  console.log('3. Adding first Scene...');
  const scene1 = await sceneStore.add(
    universe.id,
    [{ type: 'observation', event: 'user_registered', userId: 'alice' }],
    { count: 1, users: ['alice'] },
    { agent: 'system@example.com' }
  );
  console.log(`   ✅ Scene 1 created: ${scene1.id}`);
  console.log(`   📊 Delta: ${JSON.stringify(scene1.delta)}`);
  console.log(`   🧾 Receipt hash: ${scene1.receipts[0].receiptHash.substring(0, 16)}...\n`);

  // 4. Add second scene
  console.log('4. Adding second Scene...');
  const scene2 = await sceneStore.add(
    universe.id,
    [{ type: 'observation', event: 'user_registered', userId: 'bob' }],
    { count: 2, users: ['alice', 'bob'] },
    { agent: 'system@example.com' }
  );
  console.log(`   ✅ Scene 2 created: ${scene2.id}`);
  console.log(`   📊 Delta: ${JSON.stringify(scene2.delta)}`);
  console.log(`   🔗 Previous scene: ${scene2.previousSceneId}\n`);

  // 5. Verify receipt chain
  console.log('5. Verifying receipt chain...');
  const { verifyReceiptChain } = await import('./receipts.mjs');
  const receipts = [scene1.receipts[0], scene2.receipts[0]];
  const chainVerification = await verifyReceiptChain(receipts);
  console.log(`   ✅ Chain valid: ${chainVerification.valid}`);
  console.log(`   ❌ Errors: ${chainVerification.errors.length}\n`);

  // 6. Replay history
  console.log('6. Replaying scene history...');
  const finalState = await sceneStore.replay(universe.id);
  console.log(`   ✅ Final state: ${JSON.stringify(finalState)}\n`);

  // 7. Create a bridge
  console.log('7. Creating a Bridge...');
  const universeB = await universeStore.create({
    schema: 'http://example.org/target-schema#',
    reconcile: createIdentityReconcile(),
    metadata: { name: 'Target Universe' },
  });

  const bridge = await Bridge.define(
    universe,
    universeB,
    (value) => ({ ...value, transformed: true, namespace: 'target' }),
    async (value) => value.count >= 0,
    {
      name: 'Example to Target Bridge',
      description: 'Transforms data between universes',
    }
  );
  console.log(`   ✅ Bridge created: ${bridge.id}`);
  console.log(`   🌉 ${bridge.sourceUniverseId} → ${bridge.targetUniverseId}\n`);

  // 8. Test bridge
  console.log('8. Testing cross-universe call...');
  Bridge.grantAccess(bridge, 'system@example.com', 'execute');

  const { crossUniverseCall } = await import('./bridges.mjs');
  const bridgeResult = await crossUniverseCall(
    bridge,
    { count: 5, data: 'example' },
    'system@example.com'
  );
  console.log(`   ✅ Bridge call success: ${bridgeResult.success}`);
  console.log(`   🔄 Transformed: ${JSON.stringify(bridgeResult.result)}\n`);

  // Summary
  console.log('📋 Summary:');
  console.log(`   Universes created: ${universeStore.list().length}`);
  console.log(`   Scenes created: ${sceneStore.getHistory(universe.id).length}`);
  console.log(`   Receipt chain valid: ${chainVerification.valid}`);
  console.log(`   Bridge operational: ${bridgeResult.success}`);
  console.log('\n✅ All smoke tests passed!\n');
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runSmokeTest().catch((error) => {
    console.error('❌ Smoke test failed:', error);
    process.exit(1);
  });
}

export { runSmokeTest };
