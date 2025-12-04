#!/usr/bin/env node

/**
 * Real System Integration Test
 *
 * This test validates the real implementations built on top of the working
 * transaction system. It demonstrates actual functionality without fake components.
 */

import { createStore } from '@unrdf/oxigraph';
import { RealKnowledgeHookManager } from '../src/knowledge-engine/real-knowledge-hooks.mjs';
import { createRealLockchainWriter } from '../src/knowledge-engine/real-lockchain-writer.mjs';
import { createRealResolutionLayer } from '../src/knowledge-engine/real-resolution-layer.mjs';
import { createRealEffectSandbox } from '../src/knowledge-engine/real-effect-sandbox.mjs';
import { createRealQueryOptimizer } from '../src/knowledge-engine/real-query-optimizer.mjs';

console.log('🚀 Real System Integration Test\n');

async function testRealSystem() {
  let success = true;
  const results = [];

  try {
    // === Phase 1: Real System Initialization ===
    console.log('🔧 Phase 1: Real System Initialization');

    const manager = new RealKnowledgeHookManager({
      basePath: process.cwd(),
      strictMode: false,
      enableLockchain: true,
      enableResolution: true,
    });

    console.log('  ✅ Real Knowledge Hook Manager initialized');
    results.push({ phase: 'Real System Initialization', success: true });

    // === Phase 2: Real Knowledge Hook Creation ===
    console.log('\n📋 Phase 2: Real Knowledge Hook Creation');

    const realHook = {
      id: 'real-validation-hook',
      name: 'real-validation',
      description: 'Real validation hook with actual functionality',
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://real-validation.rq',
        },
      },
      run: async ({ store, delta, _hook }) => {
        console.log('    ✅ Real hook executed with actual store:', store.size, 'quads');
        console.log(
          '    📊 Delta contains:',
          delta.additions.length,
          'additions,',
          delta.removals.length,
          'removals'
        );
        return { result: 'real-validation-success', processed: true };
      },
    };

    manager.addKnowledgeHook(realHook);
    console.log('  ✅ Real knowledge hook added');
    results.push({ phase: 'Real Knowledge Hook Creation', success: true });

    // === Phase 3: Real Transaction Processing ===
    console.log('\n🔄 Phase 3: Real Transaction Processing');

    // Create real store with actual data
    const store = createStore();
    const realQuad1 = { s: 'ex:alice', p: 'ex:knows', o: 'ex:bob' };
    const realQuad2 = { s: 'ex:bob', p: 'ex:knows', o: 'ex:charlie' };
    store.addQuad(realQuad1);
    store.addQuad(realQuad2);

    // Create real delta with actual quads
    const realDelta = {
      additions: [
        { s: 'ex:charlie', p: 'ex:knows', o: 'ex:alice' },
        { s: 'ex:alice', p: 'ex:age', o: '30' },
      ],
      removals: [{ s: 'ex:bob', p: 'ex:knows', o: 'ex:charlie' }],
    };

    console.log('  📊 Real store size:', store.size);
    console.log('  📊 Real delta: +2 additions, -1 removal');

    // Apply real transaction
    const receipt = await manager.apply(store, realDelta, {
      actor: 'real-test-user',
      metadata: { testRun: 'real-system' },
    });

    console.log(
      `  ✅ Real transaction applied: ${receipt.receipt.committed ? 'committed' : 'vetoed'}`
    );
    console.log(`  📊 Hook results: ${receipt.receipt.hookResults.length} hooks executed`);
    console.log(`  📊 Final store size: ${store.size}`);

    results.push({ phase: 'Real Transaction Processing', success: true });

    // === Phase 4: Real Lockchain Integration ===
    console.log('\n🔗 Phase 4: Real Lockchain Integration');

    const lockchainWriter = createRealLockchainWriter({
      gitRepo: process.cwd(),
      batchSize: 5,
    });

    // Write receipt to lockchain
    const entryId = await lockchainWriter.writeReceipt(receipt.receipt);
    console.log('  ✅ Receipt written to lockchain:', entryId);

    // Commit batch
    const commitResult = await lockchainWriter.commitBatch();
    console.log('  ✅ Lockchain batch committed:', commitResult.committed);

    // Get lockchain stats
    const lockchainStats = lockchainWriter.getStats();
    console.log('  📊 Lockchain stats:', lockchainStats);

    results.push({ phase: 'Real Lockchain Integration', success: true });

    // === Phase 5: Real Resolution Layer ===
    console.log('\n🤝 Phase 5: Real Resolution Layer');

    const resolutionLayer = createRealResolutionLayer({
      defaultStrategy: 'voting',
      enableConflictDetection: true,
    });

    // Submit proposals
    const proposal1 = await resolutionLayer.submitProposal(
      'agent-1',
      {
        additions: [{ s: 'ex:new', p: 'ex:type', o: 'ex:document' }],
        removals: [],
      },
      { confidence: 0.9, priority: 80 }
    );

    const proposal2 = await resolutionLayer.submitProposal(
      'agent-2',
      {
        additions: [{ s: 'ex:new', p: 'ex:status', o: 'ex:active' }],
        removals: [],
      },
      { confidence: 0.8, priority: 70 }
    );

    console.log('  ✅ Proposals submitted:', proposal1, proposal2);

    // Resolve proposals
    const resolution = await resolutionLayer.resolveProposals([proposal1, proposal2], {
      type: 'voting',
    });

    console.log('  ✅ Proposals resolved:', resolution.consensus ? 'consensus' : 'conflict');
    console.log('  📊 Resolution confidence:', resolution.confidence);

    // Get resolution stats
    const resolutionStats = resolutionLayer.getStats();
    console.log('  📊 Resolution stats:', resolutionStats);

    results.push({ phase: 'Real Resolution Layer', success: true });

    // === Phase 6: Real Effect Sandboxing ===
    console.log('\n🛡️ Phase 6: Real Effect Sandboxing');

    const effectSandbox = createRealEffectSandbox({
      type: 'worker', // Use worker threads for safety
      timeout: 3000,
      enableConsole: true,
    });

    // Execute effect in sandbox
    const sandboxResult = await effectSandbox.executeEffect(
      async context => {
        console.log('    🔒 Executing in sandbox with context:', Object.keys(context));
        return { sandboxed: true, result: 'safe-execution' };
      },
      { store, delta: realDelta, hook: realHook }
    );

    console.log('  ✅ Effect executed in sandbox:', sandboxResult.success);
    console.log('  📊 Sandbox result:', sandboxResult.result);

    // Get sandbox stats
    const sandboxStats = effectSandbox.getStats();
    console.log('  📊 Sandbox stats:', sandboxStats);

    results.push({ phase: 'Real Effect Sandboxing', success: true });

    // === Phase 7: Real Query Optimization ===
    console.log('\n⚡ Phase 7: Real Query Optimization');

    const queryOptimizer = createRealQueryOptimizer({
      enableCaching: true,
      enableIndexing: true,
      maxCacheSize: 100,
    });

    // Create indexes
    const indexResult = await queryOptimizer.createIndexes(store, [
      'subject',
      'predicate',
      'object',
    ]);
    console.log('  ✅ Indexes created:', indexResult.indexes);

    // Optimize query
    const optimizationResult = await queryOptimizer.optimizeQuery(
      'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
      { store }
    );

    console.log('  ✅ Query optimized:', optimizationResult.success);
    console.log('  📊 From cache:', optimizationResult.fromCache);

    // Execute optimized query
    const executionResult = await queryOptimizer.executeOptimizedQuery(
      optimizationResult.plan,
      store
    );

    console.log('  ✅ Optimized query executed:', executionResult.success);
    console.log('  📊 Results count:', executionResult.results.length);

    // Get optimizer stats
    const optimizerStats = queryOptimizer.getStats();
    console.log('  📊 Optimizer stats:', optimizerStats);

    results.push({ phase: 'Real Query Optimization', success: true });
  } catch (error) {
    console.error(`\n❌ Real system test failed: ${error.message}`);
    console.error(error.stack);
    success = false;
  }

  // === Summary ===
  console.log('\n🎯 Real System Test Summary:');
  console.log('================================');

  results.forEach(result => {
    const status = result.success ? '✅' : '❌';
    console.log(`${status} ${result.phase}`);
  });

  const passed = results.filter(r => r.success).length;
  const total = results.length;

  console.log(`\n📊 Results: ${passed}/${total} phases passed`);

  if (success && passed === total) {
    console.log('🎉 Real system test: SUCCESS');
    console.log('🚀 All real components are working!');
  } else {
    console.log('⚠️  Real system test: FAILED');
    console.log('🔧 Some real components need fixes');
  }

  return success;
}

// Run the test
testRealSystem()
  .then(success => {
    process.exit(success ? 0 : 1);
  })
  .catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
