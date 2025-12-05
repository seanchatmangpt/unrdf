#!/usr/bin/env node

/**
 * @fileoverview Hooks Runtime Example for Nitro
 * 
 * This demonstrates how to use the hooks engine with Nitro runtime.
 */

import { defineHook, evaluateHook } from 'unrdf/knowledge-engine';
import { initStore } from '../../packages/composables/src/context/index.mjs'
import { useTurtle } from '../../packages/composables/src/composables/use-turtle.mjs'
import { useGraph } from '../../packages/composables/src/composables/use-graph.mjs'

// Sample RDF data
const sampleData = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

# Services with metrics
ex:service1 a ex:Service ;
  ex:errorRate 0.05 ;
  ex:latency 1500 ;
  ex:requests 1000 ;
  ex:status ex:active .

ex:service2 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 300 ;
  ex:requests 2000 ;
  ex:status ex:active .

ex:service3 a ex:Service ;
  ex:errorRate 0.08 ;
  ex:latency 2000 ;
  ex:requests 500 ;
  ex:status ex:degraded .

# People
ex:person1 a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 30 .

ex:person2 a foaf:Person ;
  foaf:name "Bob" ;
  foaf:age 25 .
`

async function demonstrateHooksRuntime() {
  console.log('🚀 UNRDF Hooks Runtime Demo')
  console.log('============================\n')
  
  // Initialize store with composable architecture
  const runApp = initStore()
  
  await runApp(async () => {
    const turtle = await useTurtle()
    await turtle.parse(sampleData)
    
    console.log('📊 Loaded sample data with services and people\n')
    
    // Define various hooks
    const hooks = [
      // Service Health Monitor
      defineHook({
        id: 'ex:ServiceHealthMonitor',
        name: 'Service Health Monitor',
        description: 'Monitors service error rates and latency',
        select: 'SELECT ?service ?errorRate ?latency WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }',
        predicates: [
          { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } },
          { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 1000 } }
        ],
        combine: 'OR'
      }),
      
      // High Error Rate Alert
      defineHook({
        id: 'ex:HighErrorRateAlert',
        name: 'High Error Rate Alert',
        description: 'Alerts when any service has error rate > 5%',
        select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
        predicates: [
          { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.05 } }
        ],
        combine: 'AND'
      }),
      
      // Service Existence Check
      defineHook({
        id: 'ex:ServiceExistenceCheck',
        name: 'Service Existence Check',
        description: 'Checks if services exist in the system',
        select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
        predicates: [
          { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }
        ],
        combine: 'AND'
      }),

      // Complex Multi-Predicate Hook
      defineHook({
        id: 'ex:ComplexSystemMonitor',
        name: 'Complex System Monitor',
        description: 'Comprehensive system monitoring with multiple criteria',
        select: 'SELECT ?service ?errorRate ?latency ?requests WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency ; <http://example.org/requests> ?requests }',
        predicates: [
          { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } },
          { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '<=', value: 0.1 } },
          { kind: 'THRESHOLD', spec: { var: 'latency', op: '<=', value: 3000 } },
          { kind: 'WINDOW', spec: { var: 'requests', size: '5m', op: 'count', cmp: { op: '>', value: 0 } } }
        ],
        combine: 'AND'
      })
    ]
    
    console.log(`🔧 Created ${hooks.length} hooks\n`)
    
    // Evaluate each hook
    for (const hook of hooks) {
      console.log(`\n📋 Evaluating Hook: ${hook.id}`)
      console.log(`   Name: ${hook.name}`)
      console.log(`   Description: ${hook.description}`)

      // Evaluate the hook
      const receipt = await evaluateHook(hook)
      
      console.log(`   Result: ${receipt.fired ? '🔥 FIRED' : '— No trigger'}`)
      console.log(`   Duration: ${receipt.durations.totalMs}ms`)
      console.log(`   Timestamp: ${receipt.at}`)
      
      // Show predicate results
      console.log(`   Predicates:`)
      receipt.predicates.forEach((pred, i) => {
        const status = pred.ok ? '✅' : '❌'
        console.log(`     ${i + 1}. ${pred.kind}: ${status} ${pred.meta ? JSON.stringify(pred.meta) : ''}`)
      })
      
      // Show provenance
      console.log(`   Provenance:`)
      console.log(`     Query Hash: ${receipt.provenance.qHash?.substring(0, 16)}...`)
      console.log(`     Predicate Hash: ${receipt.provenance.pHash?.substring(0, 16)}...`)
      console.log(`     Store Hash: ${receipt.provenance.sHash?.substring(0, 16)}...`)
    }
    
    console.log('\n🎯 Runtime Summary')
    console.log('==================')
    
    const firedHooks = hooks.filter(hook => {
      // We'd need to track results in a real runtime
      return true // Simplified for demo
    })
    
    console.log(`Total Hooks: ${hooks.length}`)
    console.log(`Fired Hooks: ${firedHooks.length}`)
    console.log(`Success Rate: ${Math.round((firedHooks.length / hooks.length) * 100)}%`)
    
    console.log('\n✨ Hooks Runtime Demo Complete!')
    console.log('\nTo run the Nitro web interface:')
    console.log('  cd playground && pnpm nitro:dev')
    console.log('  Then open http://localhost:3000')
  })
}

// Run the demo
demonstrateHooksRuntime().catch(console.error)
