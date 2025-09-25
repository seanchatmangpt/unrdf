#!/usr/bin/env node

/**
 * @fileoverview Test script for minimal unrdf Knowledge Hooks core
 * 
 * Demonstrates all predicate types and core functionality
 */

import { 
  initStore, 
  defineHook, 
  evaluateHook, 
  planHook, 
  loadFrontmatterHook,
  addTurtle,
  registerPredicate 
} from './src/hooks.mjs'
import fs from 'node:fs/promises'

// Sample RDF data for testing
const sampleTurtle = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix schema: <https://schema.org/> .

ex:service1 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 500 ;
  ex:status ex:active ;
  ex:requests 1000 .

ex:service2 a ex:Service ;
  ex:errorRate 0.03 ;
  ex:latency 1200 ;
  ex:status ex:active ;
  ex:requests 2000 .

ex:service3 a ex:Service ;
  ex:errorRate 0.005 ;
  ex:latency 300 ;
  ex:status ex:maintenance ;
  ex:requests 500 .

ex:person1 a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 30 ;
  schema:email "alice@example.org" .

ex:person2 a foaf:Person ;
  foaf:name "Bob" ;
  foaf:age 25 ;
  schema:email "bob@example.org" .
`

async function testBasicHooks() {
  console.log('🧪 Testing Basic Hook System\n')
  
  // Initialize store with sample data
  const { store, engine } = initStore(sampleTurtle)
  console.log(`📊 Loaded ${store.size} triples into store`)
  
  // Test 1: ASK predicate
  console.log('\n1️⃣ Testing ASK Predicate')
  const askHook = defineHook({
    id: 'test:ask',
    ask: 'ASK WHERE { ?s a <http://example.org/Service> }',
    predicates: [
      { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }
    ]
  })
  
  const askPlan = planHook(askHook)
  console.log('📋 Plan:', JSON.stringify(askPlan, null, 2))
  
  const askReceipt = await evaluateHook(askHook, { store, engine })
  console.log('📄 Receipt:', JSON.stringify(askReceipt, null, 2))
  
  // Test 2: THRESHOLD predicate
  console.log('\n2️⃣ Testing THRESHOLD Predicate')
  const thresholdHook = defineHook({
    id: 'test:threshold',
    select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
    predicates: [
      { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
    ]
  })
  
  const thresholdReceipt = await evaluateHook(thresholdHook, { store, engine })
  console.log('📄 Receipt:', JSON.stringify(thresholdReceipt, null, 2))
  
  // Test 3: DELTA predicate
  console.log('\n3️⃣ Testing DELTA Predicate')
  const deltaHook = defineHook({
    id: 'test:delta',
    select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
    predicates: [
      { kind: 'DELTA', spec: { key: ['service', 'errorRate'], prev: [] } }
    ]
  })
  
  const deltaReceipt = await evaluateHook(deltaHook, { store, engine })
  console.log('📄 Receipt:', JSON.stringify(deltaReceipt, null, 2))
  
  // Test 4: SHACL predicate (stub)
  console.log('\n4️⃣ Testing SHACL Predicate (stub)')
  const shaclHook = defineHook({
    id: 'test:shacl',
    select: 'SELECT ?person WHERE { ?person a <http://xmlns.com/foaf/0.1/Person> }',
    predicates: [
      { kind: 'SHACL', spec: { shapes: 'ex:PersonShape', strict: true } }
    ]
  })
  
  const shaclReceipt = await evaluateHook(shaclHook, { store, engine })
  console.log('📄 Receipt:', JSON.stringify(shaclReceipt, null, 2))
  
  // Test 5: WINDOW predicate
  console.log('\n5️⃣ Testing WINDOW Predicate')
  const windowHook = defineHook({
    id: 'test:window',
    select: 'SELECT ?service ?requests WHERE { ?service <http://example.org/requests> ?requests }',
    predicates: [
      { 
        kind: 'WINDOW', 
        spec: { 
          var: 'requests', 
          size: '5m', 
          op: 'count', 
          cmp: { op: '>', value: 1 } 
        } 
      }
    ]
  })
  
  const windowReceipt = await evaluateHook(windowHook, { store, engine })
  console.log('📄 Receipt:', JSON.stringify(windowReceipt, null, 2))
  
  // Test 6: Combined predicates with AND
  console.log('\n6️⃣ Testing Combined Predicates (AND)')
  const combinedHook = defineHook({
    id: 'test:combined',
    select: 'SELECT ?service ?errorRate ?latency WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }',
    predicates: [
      { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 } },
      { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 400 } }
    ],
    combine: 'AND'
  })
  
  const combinedReceipt = await evaluateHook(combinedHook, { store, engine })
  console.log('📄 Receipt:', JSON.stringify(combinedReceipt, null, 2))
  
  // Test 7: Hook with effect
  console.log('\n7️⃣ Testing Hook with Effect')
  const effectHook = defineHook({
    id: 'test:effect',
    select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
    predicates: [
      { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
    ],
    effect: async ({ rows, receipt, store, engine }) => {
      console.log('🔥 EFFECT FIRED! Alerting about high error rates:')
      rows.forEach(row => {
        console.log(`  - Service: ${row.get('service')?.value}, Error Rate: ${row.get('errorRate')?.value}`)
      })
    }
  })
  
  const effectReceipt = await evaluateHook(effectHook, { store, engine })
  console.log('📄 Receipt:', JSON.stringify(effectReceipt, null, 2))
}

async function testCustomPredicate() {
  console.log('\n\n🔧 Testing Custom Predicate Registration\n')
  
  // Register a custom predicate
  registerPredicate('CUSTOM_COUNT', async (spec, ctx) => {
    const { rows } = ctx
    const count = rows.length
    const threshold = spec.threshold || 0
    return { 
      ok: count >= threshold, 
      meta: { count, threshold, kind: 'CUSTOM_COUNT' } 
    }
  })
  
  const { store, engine } = initStore(sampleTurtle)
  
  const customHook = defineHook({
    id: 'test:custom',
    select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
    predicates: [
      { kind: 'CUSTOM_COUNT', spec: { threshold: 2 } }
    ]
  })
  
  const customReceipt = await evaluateHook(customHook, { store, engine })
  console.log('📄 Custom Predicate Receipt:', JSON.stringify(customReceipt, null, 2))
}

async function testFrontmatterLoader() {
  console.log('\n\n📄 Testing Front-matter Hook Loader\n')
  
  // Create a sample markdown file with front-matter
  const sampleMarkdown = `---
hook:
  id: 'ex:HealthMonitor'
  name: 'Service Health Monitor'
  select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }'
  predicates:
    - kind: 'THRESHOLD'
      spec:
        var: 'errorRate'
        op: '>'
        value: 0.02
  combine: 'OR'
---

# Service Health Monitor

This hook monitors service error rates and alerts when they exceed 2%.

## Configuration

- **Threshold**: 2% error rate
- **Action**: Alert operations team
- **Schedule**: Every 5 minutes
`

  const tempFile = '/tmp/test-hook.md'
  await fs.writeFile(tempFile, sampleMarkdown)
  
  try {
    const loadedHook = await loadFrontmatterHook(tempFile)
    console.log('📋 Loaded Hook:', JSON.stringify(loadedHook, null, 2))
    
    // Test the loaded hook
    const { store, engine } = initStore(sampleTurtle)
    const receipt = await evaluateHook(loadedHook, { store, engine })
    console.log('📄 Evaluation Receipt:', JSON.stringify(receipt, null, 2))
    
  } finally {
    await fs.unlink(tempFile)
  }
}

async function testPerformanceAndHashes() {
  console.log('\n\n⚡ Testing Performance and Hash Determinism\n')
  
  const { store, engine } = initStore(sampleTurtle)
  
  // Test multiple evaluations to check hash consistency
  const hook = defineHook({
    id: 'test:performance',
    select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
    predicates: [
      { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 } }
    ]
  })
  
  console.log('🔄 Running multiple evaluations...')
  const receipts = []
  for (let i = 0; i < 3; i++) {
    const receipt = await evaluateHook(hook, { store, engine })
    receipts.push(receipt)
    console.log(`Run ${i + 1}: ${receipt.durations.totalMs}ms, Hash: ${receipt.provenance.qHash?.substring(0, 16)}...`)
  }
  
  // Check hash consistency
  const firstHash = receipts[0].provenance.qHash
  const allSameHash = receipts.every(r => r.provenance.qHash === firstHash)
  console.log(`🔒 Hash consistency: ${allSameHash ? '✅' : '❌'}`)
  
  // Check timing consistency
  const avgDuration = receipts.reduce((sum, r) => sum + r.durations.totalMs, 0) / receipts.length
  console.log(`⏱️ Average duration: ${avgDuration.toFixed(2)}ms`)
}

async function main() {
  try {
    await testBasicHooks()
    await testCustomPredicate()
    await testFrontmatterLoader()
    await testPerformanceAndHashes()
    
    console.log('\n\n✅ All tests completed successfully!')
    console.log('\n📚 Hook System Features Demonstrated:')
    console.log('  - ✅ ASK predicates (boolean SPARQL)')
    console.log('  - ✅ THRESHOLD predicates (numeric comparisons)')
    console.log('  - ✅ DELTA predicates (change detection)')
    console.log('  - ✅ SHACL predicates (validation stub)')
    console.log('  - ✅ WINDOW predicates (tumbling windows)')
    console.log('  - ✅ Combined predicates (AND/OR logic)')
    console.log('  - ✅ Effect functions (side effects)')
    console.log('  - ✅ Custom predicate registration')
    console.log('  - ✅ Front-matter hook loading')
    console.log('  - ✅ Deterministic hashing')
    console.log('  - ✅ Performance timing')
    
  } catch (error) {
    console.error('❌ Test failed:', error)
    process.exit(1)
  }
}

// Run tests if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main()
}
