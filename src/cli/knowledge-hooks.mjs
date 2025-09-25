/**
 * @fileoverview Knowledge Hooks CLI - Command-line interface for hook evaluation
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { readFile, writeFile } from "node:fs/promises";
import { join, dirname } from "node:path";
import { initStore } from "../context/index.mjs";
import { useKnowledgeHooks, defineHook, evaluateHook } from "../hooks.mjs";
import { useTurtle } from "../composables/use-turtle.mjs";

/**
 * CLI command: Evaluate a Knowledge Hook
 * @param {Object} options - CLI options
 * @param {string} options.hook - Path to hook definition file
 * @param {string} options.graph - Path to graph directory
 * @param {boolean} [options.persist=true] - Persist receipts and baselines
 * @param {string} [options.output] - Output format (json, jsonld, nquads, turtle)
 * @param {string} [options.destination] - Output destination (stdout, file, webhook)
 */
export async function evalHook(options) {
  const { hook: hookPath, data: dataPath, persist = true, output, destination } = options;

  try {
    // Load hook definition
    const hookContent = await readFile(hookPath, 'utf-8');
    const hookConfig = JSON.parse(hookContent);
    const hook = defineHook(hookConfig);

    // Initialize store and load data
    const runApp = initStore();

    await runApp(async () => {
      const turtle = useTurtle();

      // Load data if provided
      if (dataPath) {
        if (dataPath.endsWith('.ttl') || dataPath.endsWith('.turtle')) {
          const dataContent = await readFile(dataPath, 'utf-8');
          const quads = await turtle.parse(dataContent);
          turtle.store.add(...quads);
          console.log(`📁 Loaded data from ${dataPath} (${quads.length} triples)`);
        } else {
          // Try to load as directory
          try {
            await turtle.loadAll(dataPath);
            console.log(`📁 Loaded data directory ${dataPath}`);
          } catch {
            console.log(`⚠️  Could not load data from ${dataPath}`);
          }
        }
      }

      // Evaluate hook
      console.log(`🔍 Evaluating hook: ${hook.id}`);
      const receipt = await evaluateHook(hook, { persist });

      // Output result
      if (receipt.fired) {
        console.log('🔥 Hook FIRED - Action required!');
      } else {
        console.log('— No change detected');
      }

      // Show receipt details
      console.log(`\n📊 Receipt Summary:`);
      console.log(`  Hook ID: ${receipt.hookId}`);
      console.log(`  Timestamp: ${receipt.timestamp}`);
      console.log(`  Fired: ${receipt.fired}`);
      console.log(`  Data Count: ${receipt.data.count}`);
      console.log(`  Total Duration: ${receipt.performance.totalDuration.toFixed(2)}ms`);

      console.log(`\n🎯 Predicate Results:`);
      receipt.predicates.forEach((pred, i) => {
        console.log(`  ${i + 1}. ${pred.kind}: ${pred.result ? '✅' : '❌'} - ${pred.reason}`);
      });

      console.log(`\n🔗 Provenance:`);
      console.log(`  Query Hash: ${receipt.provenance.queryHash.substring(0, 16)}...`);
      console.log(`  Graph Hash: ${receipt.provenance.graphHash.substring(0, 16)}...`);
      console.log(`  Hook Hash: ${receipt.provenance.hookHash.substring(0, 16)}...`);

      // Handle output destination
      if (destination === 'file' || output) {
        const outputPath = join(dirname(hookPath), `${hook.id.replace(/[^a-zA-Z0-9]/g, '_')}_receipt.json`);
        await writeFile(outputPath, JSON.stringify(receipt, null, 2));
        console.log(`\n💾 Receipt saved to: ${outputPath}`);
      }
    });

  } catch (error) {
    console.error(`❌ Hook evaluation failed: ${error.message}`);
    process.exit(1);
  }
}

/**
 * CLI command: Create a sample hook definition
 * @param {Object} options - CLI options
 * @param {string} options.output - Output file path
 * @param {string} [options.template] - Template type (health, compliance, drift)
 */
export async function createHook(options) {
  const { output, template = 'health' } = options;
  
  const templates = {
    health: {
      id: 'ex:ServiceHealth',
      name: 'Service Health Monitor',
      description: 'Monitors service error rates and detects anomalies',
      select: `SELECT ?service ?errorRate ?latency WHERE {
        ?service ex:errorRate ?errorRate .
        ?service ex:latency ?latency .
        ?service ex:status ex:active
      }`,
      predicates: [
        {
          kind: 'THRESHOLD',
          spec: {
            var: 'errorRate',
            op: '>',
            value: 0.02,
            aggregate: 'avg'
          }
        },
        {
          kind: 'THRESHOLD',
          spec: {
            var: 'latency',
            op: '>',
            value: 1000,
            aggregate: 'avg'
          }
        }
      ],
      combine: 'OR',
      output: {
        format: 'json',
        destination: 'stdout'
      }
    },
    
    compliance: {
      id: 'ex:ComplianceCheck',
      name: 'Compliance Validation',
      description: 'Validates data against SHACL shapes for compliance',
      select: `SELECT ?resource ?type WHERE {
        ?resource a ?type .
        ?resource ex:sensitive true
      }`,
      predicates: [
        {
          kind: 'SHACL',
          spec: {
            shape: 'ex:SensitiveDataShape',
            strict: true
          }
        }
      ],
      combine: 'AND',
      output: {
        format: 'json',
        destination: 'file'
      }
    },
    
    drift: {
      id: 'ex:ConfigDrift',
      name: 'Configuration Drift Detection',
      description: 'Detects changes in configuration parameters',
      select: `SELECT ?config ?value ?environment WHERE {
        ?config ex:value ?value .
        ?config ex:environment ?environment .
        ?config ex:monitored true
      }`,
      predicates: [
        {
          kind: 'DELTA',
          spec: {
            change: 'any',
            key: ['config', 'environment']
          }
        }
      ],
      combine: 'AND',
      output: {
        format: 'json',
        destination: 'webhook'
      }
    }
  };
  
  const templateConfig = templates[template];
  if (!templateConfig) {
    throw new Error(`Unknown template: ${template}. Available: ${Object.keys(templates).join(', ')}`);
  }
  
  await writeFile(output, JSON.stringify(templateConfig, null, 2));
  console.log(`✅ Created hook template: ${output}`);
  console.log(`📝 Template: ${templateConfig.name}`);
  console.log(`🔧 Edit the file to customize your hook definition`);
}

/**
 * CLI command: List available hook templates
 */
export function listTemplates() {
  console.log('📋 Available Hook Templates:\n');
  
  console.log('🏥 health - Service Health Monitor');
  console.log('   Monitors service error rates and latency thresholds');
  console.log('   Use case: Alert when services exceed error/latency limits\n');
  
  console.log('📋 compliance - Compliance Validation');
  console.log('   Validates data against SHACL shapes');
  console.log('   Use case: Ensure sensitive data follows compliance rules\n');
  
  console.log('📊 drift - Configuration Drift Detection');
  console.log('   Detects changes in configuration parameters');
  console.log('   Use case: Monitor config changes across environments\n');
  
  console.log('Usage:');
  console.log('  unrdf hook create --template health --output hooks/health.json');
  console.log('  unrdf hook create --template compliance --output hooks/compliance.json');
  console.log('  unrdf hook create --template drift --output hooks/drift.json');
}

/**
 * CLI command: Validate a hook definition
 * @param {Object} options - CLI options
 * @param {string} options.hook - Path to hook definition file
 */
export async function validateHook(options) {
  const { hook: hookPath } = options;
  
  try {
    const hookContent = await readFile(hookPath, 'utf-8');
    const hookConfig = JSON.parse(hookContent);
    const hook = defineHook(hookConfig);
    
    console.log('✅ Hook definition is valid');
    console.log(`📝 Hook ID: ${hook.id}`);
    console.log(`🎯 Predicates: ${hook.predicates.length}`);
    console.log(`🔗 Combine: ${hook.combine}`);
    
    // Show predicate details
    console.log('\n🎯 Predicate Details:');
    hook.predicates.forEach((pred, i) => {
      console.log(`  ${i + 1}. ${pred.kind}: ${JSON.stringify(pred.spec)}`);
    });
    
  } catch (error) {
    console.error(`❌ Hook validation failed: ${error.message}`);
    process.exit(1);
  }
}

/**
 * CLI command: Show hook statistics
 * @param {Object} options - CLI options
 * @param {string} options.receipts - Path to receipts directory
 */
export async function showStats(options) {
  const { receipts: receiptsPath } = options;
  
  try {
    // This would load and analyze receipt history
    console.log('📊 Hook Statistics (placeholder)');
    console.log('   Total Evaluations: 0');
    console.log('   Fired Hooks: 0');
    console.log('   Fire Rate: 0%');
    console.log('   Avg Duration: 0ms');
    
    console.log('\n🎯 Predicate Usage:');
    console.log('   THRESHOLD: 0');
    console.log('   SHACL: 0');
    console.log('   DELTA: 0');
    console.log('   ASK: 0');
    console.log('   WINDOW: 0');
    
  } catch (error) {
    console.error(`❌ Statistics failed: ${error.message}`);
    process.exit(1);
  }
}
