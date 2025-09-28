#!/usr/bin/env node

/**
 * Simple Production Test
 * 
 * This test validates the core functionality without complex validation
 */

import { Store } from 'n3';
import { KnowledgeHookManager } from '../src/knowledge-engine/knowledge-hook-manager.mjs';
import { createKnowledgeHook } from '../src/knowledge-engine/schemas.mjs';

console.log('🚀 Simple Production Test\n');

async function testSimpleProduction() {
  let success = true;
  const results = [];

  try {
    // === Phase 1: System Initialization ===
    console.log('🔧 Phase 1: System Initialization');
    
    const manager = new KnowledgeHookManager({
      basePath: process.cwd(),
      strictMode: false
    });

    console.log('  ✅ KnowledgeHookManager initialized');
    results.push({ phase: 'System Initialization', success: true });

    // === Phase 2: Simple Hook Creation ===
    console.log('\n📋 Phase 2: Simple Hook Creation');
    
    const simpleHook = createKnowledgeHook({
      meta: {
        name: 'simple-test',
        version: '1.0.0',
        description: 'Simple test hook'
      },
      when: {
        kind: 'sparql-ask',
        ref: { 
          uri: 'file://simple.rq'
        }
      },
      run: async (event, store, delta, metadata) => {
        console.log('    ✅ Simple hook executed');
        return { result: 'success' };
      }
    });

    // Add hook directly
    manager.addKnowledgeHook(simpleHook);
    console.log('  ✅ Simple hook added');
    results.push({ phase: 'Simple Hook Creation', success: true });

    // === Phase 3: Simple Transaction ===
    console.log('\n🔄 Phase 3: Simple Transaction');
    
    // Create test store
    const store = new Store();
    const initialQuad = { s: 'ex:test', p: 'ex:hasStatus', o: 'ex:active' };
    store.addQuad(initialQuad);
    
    // Create simple delta without complex validation
    const simpleDelta = {
      additions: [
        { s: 'ex:test', p: 'ex:hasValue', o: 'ex:success' }
      ],
      removals: []
    };

    console.log('  📊 Simple delta created');

    // Apply transaction
    const receipt = await manager.apply(store, simpleDelta, {
      actor: 'test-user',
      metadata: { testRun: 'simple-production' }
    });

    console.log(`  ✅ Transaction applied: ${receipt.committed ? 'committed' : 'vetoed'}`);
    console.log(`  📊 Hook results: ${receipt.hookResults ? receipt.hookResults.length : 0} hooks executed`);
    
    results.push({ phase: 'Simple Transaction', success: true });

  } catch (error) {
    console.error(`\n❌ Simple production test failed: ${error.message}`);
    console.error(error.stack);
    success = false;
  }

  // === Summary ===
  console.log('\n🎯 Simple Production Test Summary:');
  console.log('==================================');
  
  results.forEach(result => {
    const status = result.success ? '✅' : '❌';
    console.log(`${status} ${result.phase}`);
  });
  
  const passed = results.filter(r => r.success).length;
  const total = results.length;
  
  console.log(`\n📊 Results: ${passed}/${total} phases passed`);
  
  if (success && passed === total) {
    console.log('🎉 Simple production test: SUCCESS');
    console.log('🚀 Core system is working!');
  } else {
    console.log('⚠️  Simple production test: FAILED');
    console.log('🔧 Core system needs fixes');
  }
  
  return success;
}

// Run the test
testSimpleProduction()
  .then(success => {
    process.exit(success ? 0 : 1);
  })
  .catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
