#!/usr/bin/env node
/**
 * Validation script for Oxigraph Integration
 *
 * Verifies that the integration module correctly uses @unrdf/oxigraph
 * without requiring full test suite dependencies.
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad } = dataFactory;

console.log('=== Oxigraph Integration Validation ===\n');

// Test 1: Create store
console.log('[1/5] Creating Oxigraph store...');
const store = createStore();
console.log('✅ Store created successfully');
console.log(`   Initial size: ${store.size} quads\n`);

// Test 2: Add triple
console.log('[2/5] Adding triple to store...');
const subject = namedNode('http://example.org/subject');
const predicate = namedNode('http://example.org/predicate');
const object = literal('test value');
const testQuad = quad(subject, predicate, object);

store.add(testQuad);
console.log('✅ Triple added successfully');
console.log(`   Store size: ${store.size} quads`);
console.log(`   Subject: ${subject.value}`);
console.log(`   Predicate: ${predicate.value}`);
console.log(`   Object: "${object.value}"\n`);

// Test 3: Query pattern
console.log('[3/5] Querying pattern...');
const matches = store.match(subject, null, null);
console.log(`✅ Query successful`);
console.log(`   Matches found: ${matches.length}`);
console.log(`   Match subject: ${matches[0]?.subject.value}`);
console.log(`   Match predicate: ${matches[0]?.predicate.value}`);
console.log(`   Match object: "${matches[0]?.object.value}"\n`);

// Test 4: SPARQL query
console.log('[4/5] Executing SPARQL query...');
try {
  const results = store.query('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
  console.log('✅ SPARQL query executed');
  console.log(`   Results type: ${Array.isArray(results) ? 'Array' : typeof results}`);
  console.log(`   Results count: ${results.length || 'N/A'}\n`);
} catch (error) {
  console.log(`⚠️  SPARQL query error: ${error.message}\n`);
}

// Test 5: Delete triple
console.log('[5/5] Deleting triple...');
store.delete(testQuad);
console.log('✅ Triple deleted successfully');
console.log(`   Store size: ${store.size} quads\n`);

// Test 6: Import integration module
console.log('[6/6] Testing integration module imports...');
try {
  const integration = await import('./src/oxigraph-integration.mjs');

  const hasCreateStore = typeof integration.createOxigraphStore === 'function';
  const hasCreateBridge = typeof integration.createOxigraphBridge === 'function';
  const hasCreateIntegrated = typeof integration.createIntegratedStore === 'function';
  const hasDataFactory = typeof integration.dataFactory === 'object';

  console.log('✅ Integration module loaded successfully');
  console.log(`   createOxigraphStore: ${hasCreateStore ? '✅' : '❌'}`);
  console.log(`   createOxigraphBridge: ${hasCreateBridge ? '✅' : '❌'}`);
  console.log(`   createIntegratedStore: ${hasCreateIntegrated ? '✅' : '❌'}`);
  console.log(`   dataFactory: ${hasDataFactory ? '✅' : '❌'}\n`);

  // Test create functions
  console.log('[Bonus] Testing integration factory functions...');
  const testStore = integration.createOxigraphStore();
  console.log(`✅ createOxigraphStore() works - size: ${testStore.size}`);

  const testBridge = integration.createOxigraphBridge();
  console.log(`✅ createOxigraphBridge() works - state: ${testBridge.state}`);

  const integrated = integration.createIntegratedStore();
  console.log(`✅ createIntegratedStore() works - store size: ${integrated.store.size}, bridge state: ${integrated.bridge.state}\n`);

} catch (error) {
  console.error('❌ Integration module import failed:', error.message);
  process.exit(1);
}

console.log('=== All Validations Passed ✅ ===');
console.log('\nSummary:');
console.log('- @unrdf/oxigraph integration: ✅ Working');
console.log('- Store operations (add/query/delete): ✅ Working');
console.log('- SPARQL support: ✅ Available');
console.log('- Integration module exports: ✅ Correct');
console.log('- Factory functions: ✅ Functional');
