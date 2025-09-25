#!/usr/bin/env node

/**
 * @fileoverview Simple CLI for testing the minimal hooks system with composables
 */

import { 
  defineHook, 
  evaluateHook, 
  planHook, 
  loadFrontmatterHook
} from './src/hooks.mjs'
import { initStore } from './src/context/index.mjs'
import { useTurtle } from './src/composables/use-turtle.mjs'
import fs from 'node:fs/promises'
import { resolve } from 'node:path'

const sampleData = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:service1 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 500 ;
  ex:status ex:active .

ex:service2 a ex:Service ;
  ex:errorRate 0.03 ;
  ex:latency 1200 ;
  ex:status ex:active .

ex:person1 a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 30 .
`

async function evalHook(hookPath, dataPath) {
  console.log(`🔍 Evaluating hook: ${hookPath}`)
  
  try {
    // Load hook definition
    const hook = await loadFrontmatterHook(hookPath)
    console.log(`📋 Hook ID: ${hook.id}`)
    
    // Initialize store with composable architecture
    const runApp = initStore()
    
    await runApp(async () => {
      const turtle = await useTurtle()
      
      // Load data if provided
      if (dataPath) {
        const data = await fs.readFile(dataPath, 'utf-8')
        await turtle.parse(data)
        console.log(`📊 Loaded data from ${dataPath}`)
      } else {
        // Use sample data
        await turtle.parse(sampleData)
        console.log(`📊 Using sample data`)
      }
      
      // Show plan
      const plan = planHook(hook)
      console.log('\n📋 Execution Plan:')
      console.log(`  Query: ${plan.queryPlan}`)
      console.log(`  Predicates: ${plan.predicatePlan.length}`)
      console.log(`  Combine: ${plan.combine}`)
      
      // Evaluate hook
      console.log('\n⚡ Evaluating...')
      const receipt = await evaluateHook(hook)
      
      // Show results
      console.log('\n📄 Results:')
      console.log(`  Hook ID: ${receipt.id}`)
      console.log(`  Fired: ${receipt.fired ? '🔥 YES' : '— No'}`)
      console.log(`  Duration: ${receipt.durations.totalMs}ms`)
      console.log(`  Timestamp: ${receipt.at}`)
      
      console.log('\n🎯 Predicate Results:')
      receipt.predicates.forEach((pred, i) => {
        const status = pred.ok ? '✅' : '❌'
        console.log(`  ${i + 1}. ${pred.kind}: ${status} ${pred.meta ? JSON.stringify(pred.meta) : ''}`)
      })
      
      console.log('\n🔗 Provenance:')
      console.log(`  Query Hash: ${receipt.provenance.qHash?.substring(0, 16)}...`)
      console.log(`  Predicate Hash: ${receipt.provenance.pHash?.substring(0, 16)}...`)
      console.log(`  Store Hash: ${receipt.provenance.sHash?.substring(0, 16)}...`)
    })
    
  } catch (error) {
    console.error(`❌ Error: ${error.message}`)
    process.exit(1)
  }
}

async function createSampleHook(outputPath) {
  const sampleHook = `---
hook:
  id: 'ex:SampleHook'
  name: 'Sample Knowledge Hook'
  description: 'Demonstrates basic hook functionality'
  select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }'
  predicates:
    - kind: 'THRESHOLD'
      spec:
        var: 'errorRate'
        op: '>'
        value: 0.02
  combine: 'AND'
---

# Sample Knowledge Hook

This is a sample hook definition that demonstrates the front-matter format.

## What it does

- Queries for services with error rates
- Checks if error rate exceeds 2%
- Triggers if threshold is exceeded

## Usage

\`\`\`bash
node hooks-cli.mjs eval sample-hook.md
\`\`\`
`

  await fs.writeFile(outputPath, sampleHook)
  console.log(`✅ Created sample hook: ${outputPath}`)
}

async function main() {
  const command = process.argv[2]
  const arg1 = process.argv[3]
  const arg2 = process.argv[4]
  
  switch (command) {
    case 'eval':
      if (!arg1) {
        console.error('❌ Usage: node hooks-cli.mjs eval <hook-file> [data-file]')
        process.exit(1)
      }
      await evalHook(arg1, arg2)
      break
      
    case 'create':
      const outputPath = arg1 || 'sample-hook.md'
      await createSampleHook(outputPath)
      break
      
    case 'demo':
      console.log('🎬 Running demo...')
      await evalHook('examples/hooks/service-health.md')
      break
      
    default:
      console.log('🔧 Minimal Hooks CLI (Composable Architecture)')
      console.log('')
      console.log('Commands:')
      console.log('  eval <hook-file> [data-file]  - Evaluate a hook')
      console.log('  create [output-file]          - Create sample hook')
      console.log('  demo                          - Run demo with service-health hook')
      console.log('')
      console.log('Examples:')
      console.log('  node hooks-cli.mjs eval examples/hooks/service-health.md')
      console.log('  node hooks-cli.mjs create my-hook.md')
      console.log('  node hooks-cli.mjs demo')
      break
  }
}

main().catch(console.error)
