#!/usr/bin/env node

/**
 * @file Validation Examples for New UNRDF Features
 * @description
 * Comprehensive validation of all new features including:
 * - LockchainWriter with Git anchoring
 * - PolicyPack management
 * - Effect sandboxing
 * - Resolution layer for multi-agent coordination
 * - Query optimization
 * - Test utilities
 */

import { createStore } from '@unrdf/oxigraph';
import { KnowledgeHookManager } from '../packages/knowledge-engine/src/knowledge-hook-manager.mjs';
import {
  PolicyPackManager,
  createPolicyPackManifest,
} from '../packages/knowledge-engine/src/policy-pack.mjs';
import { createLockchainWriter } from '../packages/knowledge-engine/src/lockchain-writer.mjs';
import { createEffectSandbox } from '../packages/knowledge-engine/src/effect-sandbox.mjs';
import { createResolutionLayer } from '../packages/knowledge-engine/src/resolution-layer.mjs';
import { createQueryOptimizer } from '../packages/knowledge-engine/src/query-optimizer.mjs';
import { scenario, _expect, createTestContext, TestHelpers } from '../packages/test-utils/index.mjs';

console.log('🚀 UNRDF New Features Validation\n');

async function validateLockchainWriter() {
  console.log('📋 Testing LockchainWriter...');

  try {
    const lockchain = createLockchainWriter({
      gitRepo: process.cwd(),
      refName: 'refs/notes/test-lockchain',
      batchSize: 5,
    });

    // Create a mock receipt
    const mockReceipt = {
      id: 'test-receipt-123',
      committed: true,
      delta: {
        additions: [
          {
            subject: { value: 'ex:alice' },
            predicate: { value: 'ex:knows' },
            object: { value: 'ex:bob' },
          },
        ],
        removals: [],
      },
      hookResults: [],
      beforeHash: { sha3: 'abc123', blake3: 'def456' },
      afterHash: { sha3: 'ghi789', blake3: 'jkl012' },
      timestamp: Date.now(),
      duration: 150,
    };

    // Write receipt to lockchain
    const entry = await lockchain.writeReceipt(mockReceipt);
    console.log(`  ✅ Receipt written to lockchain: ${entry.id}`);

    // Commit batch
    const commitResult = await lockchain.commitBatch();
    console.log(`  ✅ Batch committed: ${commitResult.committed ? 'success' : 'failed'}`);

    // Get stats
    const stats = lockchain.getStats();
    console.log(`  📊 Lockchain stats: ${stats.pendingEntries} pending, Git: ${stats.gitEnabled}`);

    return true;
  } catch (error) {
    console.log(`  ❌ LockchainWriter failed: ${error.message}`);
    return false;
  }
}

async function validatePolicyPacks() {
  console.log('\n📦 Testing PolicyPack Management...');

  try {
    const manager = new PolicyPackManager();

    // Create a test policy pack manifest
    const manifest = createPolicyPackManifest('test-compliance', [], {
      name: 'test-compliance',
      description: 'Test compliance policy pack',
      author: 'test-user',
      version: 'latest',
    });

    console.log(
      `  ✅ Policy pack manifest created: ${manifest.meta.name} v${manifest.meta.version}`
    );

    // Test compatibility check
    const compatibility = {
      version: 'latest',
      environment: 'development',
      features: ['sparql', 'shacl'],
    };

    const pack = new (await import('../src/knowledge-engine/policy-pack.mjs')).PolicyPack(manifest);
    const compatResult = pack.checkCompatibility(compatibility);
    console.log(
      `  ✅ Compatibility check: ${compatResult.compatible ? 'compatible' : 'incompatible'}`
    );

    // Get stats
    const stats = manager.getStats();
    console.log(
      `  📊 Policy pack manager stats: ${stats.totalPacks} packs, ${stats.activePacks} active`
    );

    return true;
  } catch (error) {
    console.log(`  ❌ PolicyPack failed: ${error.message}`);
    return false;
  }
}

async function validateEffectSandbox() {
  console.log('\n🔒 Testing Effect Sandbox...');

  try {
    const sandbox = createEffectSandbox({
      type: 'worker',
      timeout: 5000,
      memoryLimit: 32 * 1024 * 1024, // 32MB
      allowedGlobals: ['console', 'Date', 'Math'],
    });

    // Create a safe test function
    const testEffect = async _context => {
      console.log('Sandboxed function executing...');
      return {
        result: 'success',
        timestamp: Date.now(),
        message: 'Hello from sandbox!',
      };
    };

    // Execute in sandbox
    const result = await sandbox.executeEffect(testEffect, {
      event: { name: 'test-event' },
      store: createStore(),
      delta: { additions: [], removals: [] },
    });

    console.log(`  ✅ Sandbox execution: ${result.success ? 'success' : 'failed'}`);
    if (result.success) {
      console.log(`  📄 Result: ${JSON.stringify(result.result)}`);
    }

    // Get stats
    const stats = sandbox.getStats();
    console.log(
      `  📊 Sandbox stats: ${stats.totalExecutions} executions, ${stats.activeWorkers} workers`
    );

    // Cleanup
    await sandbox.terminate();

    return true;
  } catch (error) {
    console.log(`  ❌ Effect Sandbox failed: ${error.message}`);
    return false;
  }
}

async function validateResolutionLayer() {
  console.log('\n🤝 Testing Resolution Layer...');

  try {
    const resolution = createResolutionLayer({
      defaultStrategy: 'voting',
      maxProposals: 50,
      enableConflictDetection: true,
    });

    // Register test agents
    resolution.registerAgent('agent-1', { type: 'validator' });
    resolution.registerAgent('agent-2', { type: 'enforcer' });
    resolution.registerAgent('agent-3', { type: 'monitor' });

    // Submit proposals
    const delta1 = {
      additions: [
        {
          subject: { value: 'ex:alice' },
          predicate: { value: 'ex:hasRole' },
          object: { value: 'ex:admin' },
        },
      ],
      removals: [],
    };

    const delta2 = {
      additions: [
        {
          subject: { value: 'ex:bob' },
          predicate: { value: 'ex:hasRole' },
          object: { value: 'ex:user' },
        },
      ],
      removals: [],
    };

    const proposal1 = await resolution.submitProposal('agent-1', delta1, {
      confidence: 0.8,
      priority: 70,
    });
    const proposal2 = await resolution.submitProposal('agent-2', delta2, {
      confidence: 0.9,
      priority: 80,
    });

    console.log(`  ✅ Proposals submitted: ${proposal1}, ${proposal2}`);

    // Resolve proposals
    const resolutionResult = await resolution.resolveProposals([proposal1, proposal2], {
      type: 'voting',
      quorum: 0.5,
    });

    console.log(
      `  ✅ Resolution completed: strategy=${resolutionResult.strategy}, consensus=${resolutionResult.consensus}`
    );
    console.log(
      `  📊 Confidence: ${resolutionResult.confidence.toFixed(2)}, conflicts: ${resolutionResult.conflicts?.length || 0}`
    );

    // Get stats
    const stats = resolution.getStats();
    console.log(
      `  📊 Resolution stats: ${stats.proposals.total} proposals, ${stats.agents.total} agents`
    );

    return true;
  } catch (error) {
    console.log(`  ❌ Resolution Layer failed: ${error.message}`);
    return false;
  }
}

async function validateQueryOptimizer() {
  console.log('\n⚡ Testing Query Optimizer...');

  try {
    const optimizer = createQueryOptimizer({
      enableCaching: true,
      enableIndexing: true,
      enableDeltaAware: true,
      maxCacheSize: 100,
    });

    // Create a test store
    const store = createStore();
    store.addQuad(
      { value: 'ex:alice', termType: 'NamedNode' },
      { value: 'ex:knows', termType: 'NamedNode' },
      { value: 'ex:bob', termType: 'NamedNode' }
    );

    // Create indexes
    const indexes = await optimizer.createIndexes(store);
    console.log(`  ✅ Indexes created: ${indexes.length} indexes`);

    // Test query optimization
    const testQuery = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o . }';
    const plan = await optimizer.optimizeQuery(testQuery, 'sparql-select', store);
    console.log(`  ✅ Query plan created: ${plan.id}, cost: ${plan.plan.estimatedCost}`);

    // Test delta-aware optimization
    const delta = {
      additions: [
        {
          subject: { value: 'ex:charlie' },
          predicate: { value: 'ex:knows' },
          object: { value: 'ex:dave' },
        },
      ],
      removals: [],
    };

    const deltaPlan = await optimizer.optimizeQuery(testQuery, 'sparql-select', store, delta);
    console.log(`  ✅ Delta-aware plan created: ${deltaPlan.id}`);

    // Update indexes
    await optimizer.updateIndexes(delta);
    console.log(`  ✅ Indexes updated with delta`);

    // Get stats
    const stats = optimizer.getStats();
    console.log(
      `  📊 Optimizer stats: cache hit rate ${(stats.cache.hitRate * 100).toFixed(1)}%, ${stats.indexes.count} indexes`
    );

    return true;
  } catch (error) {
    console.log(`  ❌ Query Optimizer failed: ${error.message}`);
    return false;
  }
}

async function validateTestUtils() {
  console.log('\n🧪 Testing Test Utilities...');

  try {
    // Create a test scenario
    const testResult = await scenario('Feature Validation Test')
      .setupScenario(async () => {
        return createTestContext()
          .withStore(createStore())
          .withMetadata({ testRun: 'validation' })
          .build();
      })
      .step('Initialize store', async context => {
        const quad = TestHelpers.createQuad('ex:test', 'ex:hasValue', 'ex:success');
        context.store.addQuad(quad);
        return { quadsAdded: 1 };
      })
      .step('Validate store contents', async context => {
        const quads = context.store.getQuads();
        return { quadCount: quads.length };
      })
      .execute();

    console.log(`  ✅ Test scenario executed: ${testResult.success ? 'success' : 'failed'}`);
    console.log(
      `  📊 Steps completed: ${testResult.steps.length}, duration: ${testResult.duration}ms`
    );

    if (!testResult.success) {
      console.log(
        `  ❌ Errors: ${testResult.errors ? testResult.errors.join(', ') : 'Unknown error'}`
      );
    }

    return testResult.success;
  } catch (error) {
    console.log(`  ❌ Test Utilities failed: ${error.message}`);
    return false;
  }
}

async function validateIntegratedFeatures() {
  console.log('\n🔗 Testing Integrated Features...');

  try {
    // Create a knowledge hook manager with all features enabled
    const manager = new KnowledgeHookManager({
      basePath: process.cwd(),
      strictMode: false,
      enableLockchain: true,
      lockchainConfig: {
        gitRepo: process.cwd(),
        refName: 'refs/notes/integration-test',
        batchSize: 3,
      },
      enableResolution: true,
      resolutionConfig: {
        defaultStrategy: 'voting',
        maxProposals: 20,
      },
    });

    console.log(`  ✅ KnowledgeHookManager created with integrated features`);

    // Create a test hook
    const testHook = {
      meta: {
        name: 'integration-test-hook',
        description: 'Test hook for integration validation',
        version: 'latest',
      },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://test.rq',
          sha256: 'test-hash',
          mediaType: 'application/sparql-query',
        },
      },
      run: async _event => {
        console.log('Integration test hook executed');
        return { success: true, message: 'Integration test passed' };
      },
    };

    // Add the hook
    manager.addKnowledgeHook(testHook);
    console.log(`  ✅ Test hook added: ${testHook.meta.name}`);

    // Create a test store and delta
    const store = createStore();
    const delta = {
      additions: [
        {
          subject: { value: 'ex:integration' },
          predicate: { value: 'ex:test' },
          object: { value: 'ex:success' },
        },
      ],
      removals: [],
    };

    // Apply transaction with all features
    const result = await manager.apply(store, delta, {
      actor: 'integration-test',
    });
    console.log(`  ✅ Transaction applied: ${result.receipt.committed ? 'committed' : 'failed'}`);
    console.log(
      `  📊 Hook results: ${result.receipt.knowledgeHookResults?.length || 0} hooks executed`
    );

    // Get comprehensive stats
    const stats = manager.getStats();
    console.log(
      `  📊 Manager stats: ${stats.totalHooks} hooks, lockchain: ${stats.lockchainEnabled}, resolution: ${stats.resolution?.enabled || false}`
    );

    return true;
  } catch (error) {
    console.log(`  ❌ Integrated Features failed: ${error.message}`);
    return false;
  }
}

// Main validation function
async function main() {
  const results = [];

  results.push(await validateLockchainWriter());
  results.push(await validatePolicyPacks());
  results.push(await validateEffectSandbox());
  results.push(await validateResolutionLayer());
  results.push(await validateQueryOptimizer());
  results.push(await validateTestUtils());
  results.push(await validateIntegratedFeatures());

  const passed = results.filter(r => r).length;
  const total = results.length;

  console.log(`\n🎯 Validation Summary:`);
  console.log(`  ✅ Passed: ${passed}/${total}`);
  console.log(`  ❌ Failed: ${total - passed}/${total}`);

  if (passed === total) {
    console.log(`\n🎉 All features validated successfully!`);
    process.exit(0);
  } else {
    console.log(`\n⚠️  Some features failed validation. Check the logs above.`);
    process.exit(1);
  }
}

// Run validation
main().catch(error => {
  console.error('💥 Validation failed with error:', error);
  process.exit(1);
});
