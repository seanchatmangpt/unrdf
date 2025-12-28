#!/usr/bin/env node
/**
 * WASM Roundtrip Demo: Oxigraph
 * 
 * Demonstrates JavaScript ↔ WASM ↔ JavaScript data flow with Oxigraph
 * Measures performance of each stage
 */

import { performance } from 'node:perf_hooks';
import { createStore, dataFactory } from '../packages/oxigraph/src/index.mjs';

console.log('=== WASM Roundtrip Demo: Oxigraph ===\n');

// 1. Initialize WASM store
console.log('1. Initializing WASM store...');
const startInit = performance.now();
const store = createStore();
const initTime = performance.now() - startInit;
console.log(`   ✅ WASM instantiation: ${initTime.toFixed(2)}ms\n`);

// 2. Create RDF data in JavaScript
console.log('2. Creating RDF triple in JavaScript...');
const { namedNode, literal } = dataFactory;
const triple = {
  subject: namedNode('http://example.org/alice'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
  object: literal('Alice')
};
console.log(`   Subject: ${triple.subject.value}`);
console.log(`   Predicate: ${triple.predicate.value}`);
console.log(`   Object: ${triple.object.value}\n`);

// 3. JavaScript → WASM (add triple)
console.log('3. JavaScript → WASM (add triple)...');
const startAdd = performance.now();
store.add(triple);
const addTime = performance.now() - startAdd;
console.log(`   ✅ JS → WASM (add): ${addTime.toFixed(3)}ms\n`);

// 4. WASM → JavaScript (query)
console.log('4. WASM → JavaScript (query all triples)...');
const startQuery = performance.now();
const results = Array.from(store.match(null, null, null));
const queryTime = performance.now() - startQuery;
console.log(`   ✅ WASM → JS (query): ${queryTime.toFixed(3)}ms\n`);

// 5. Verify roundtrip
console.log('5. Verify roundtrip integrity...');
console.log(`   📊 Results: ${results.length} triple(s)`);
if (results.length > 0) {
  console.log(`   Subject: ${results[0].subject.value}`);
  console.log(`   Predicate: ${results[0].predicate.value}`);
  console.log(`   Object: ${results[0].object.value}`);
  
  // Verify data integrity
  const match = 
    results[0].subject.value === triple.subject.value &&
    results[0].predicate.value === triple.predicate.value &&
    results[0].object.value === triple.object.value;
  
  console.log(`   ✅ Data integrity: ${match ? 'PASS' : 'FAIL'}\n`);
}

// 6. Performance summary
const totalTime = initTime + addTime + queryTime;
console.log('=== Performance Summary ===');
console.log(`Total roundtrip: ${totalTime.toFixed(2)}ms`);
console.log(`Breakdown:`);
console.log(`  - WASM init: ${initTime.toFixed(2)}ms (${(initTime/totalTime*100).toFixed(1)}%)`);
console.log(`  - JS→WASM:   ${addTime.toFixed(3)}ms (${(addTime/totalTime*100).toFixed(1)}%)`);
console.log(`  - WASM→JS:   ${queryTime.toFixed(3)}ms (${(queryTime/totalTime*100).toFixed(1)}%)`);

// 7. SLA check
const SLA_TARGET = 10; // 10ms target
const withinSLA = totalTime < SLA_TARGET;
console.log(`\nSLA Check (<${SLA_TARGET}ms): ${withinSLA ? '✅ PASS' : '❌ FAIL'}`);
if (!withinSLA) {
  console.log(`  Exceeded by: ${(totalTime - SLA_TARGET).toFixed(2)}ms`);
}

console.log('\n=== Demo Complete ===');
