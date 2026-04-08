#!/usr/bin/env node
/**
 * @file Open-Ontologies Integration Test
 * @description Demonstrates open-ontologies CLI usage through UNRDF daemon MCP tools
 */

import { spawn } from 'node:child_process';

const ONTO_BINARY = process.env.HOME + '/.local/bin/open-ontologies';
const ONTO_DATA_DIR = process.env.HOME + '/.open-ontologies';
const PLAYGROUND_ONTO = './ontologies/disney-governed-universe.ttl';

/**
 * Run open-ontologies CLI command
 */
function runOntoCommand(args) {
  return new Promise((resolve, reject) => {
    const proc = spawn(ONTO_BINARY, args, {
      stdio: ['ignore', 'pipe', 'pipe'],
      env: { ...process.env, DATA_DIR: ONTO_DATA_DIR },
    });

    let stdout = '';
    let stderr = '';

    proc.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    proc.stderr.on('data', (data) => {
      stderr += data.toString();
    });

    proc.on('close', (code) => {
      if (code === 0) {
        try {
          resolve(JSON.parse(stdout));
        } catch {
          resolve(stdout);
        }
      } else {
        reject(new Error(`Command failed (${code}): ${stderr || stdout}`));
      }
    });

    proc.on('error', (error) => {
      reject(new Error(`Failed to spawn open-ontologies: ${error.message}`));
    });
  });
}

/**
 * Main test execution
 */
async function main() {
  console.log('🚀 Open-Ontologies Integration Test');
  console.log('');
  console.log(`Binary: ${ONTO_BINARY}`);
  console.log(`Data: ${ONTO_DATA_DIR}`);
  console.log('');

  // Check if binary exists
  const fs = await import('node:fs');
  const testDataFile = new URL('../ontologies/disney-governed-universe.ttl', import.meta.url).pathname;

  if (!fs.existsSync(ONTO_BINARY)) {
    console.log('⚠️  open-ontologies binary not found, skipping integration tests');
    console.log(`   Install with: cargo install --path ~/.local/bin open-ontologies`);
    process.exit(0);
  }

  if (!fs.existsSync(testDataFile)) {
    console.log('⚠️  Test data file not found, skipping integration tests');
    console.log(`   Expected: ${testDataFile}`);
    process.exit(0);
  }

  try {
    // Test 1: Check status
    console.log('📊 Test 1: Check open-ontologies status');
    const status = await runOntoCommand(['status', '--pretty']);
    console.log(JSON.stringify(status, null, 2));
    console.log('');

    // Test 2: Validate playground ontology
    console.log('📝 Test 2: Validate disney-governed-universe ontology');
    const validateResult = await runOntoCommand(['validate', PLAYGROUND_ONTO, '--pretty']);
    console.log(JSON.stringify(validateResult, null, 2));
    console.log('');

    // Test 3: Load playground ontology
    console.log('📥 Test 3: Load disney-governed-universe ontology into store');
    const loadResult = await runOntoCommand(['load', PLAYGROUND_ONTO, '--pretty']);
    console.log(JSON.stringify(loadResult, null, 2));
    console.log('');

    // Test 4: Get stats
    console.log('📈 Test 4: Get ontology statistics');
    const stats = await runOntoCommand(['stats', '--pretty']);
    console.log(JSON.stringify(stats, null, 2));
    console.log('');

    // Test 5: Run SPARQL query
    console.log('🔍 Test 5: Run SPARQL query');
    const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 5';
    const queryResult = await runOntoCommand(['query', query, '--pretty']);
    console.log(JSON.stringify(queryResult, null, 2));
    console.log('');

    // Test 6: Clear store
    console.log('🧹 Test 6: Clear ontology store');
    const clearResult = await runOntoCommand(['clear', '--pretty']);
    console.log(JSON.stringify(clearResult, null, 2));
    console.log('');

    console.log('✅ All tests passed!');
    console.log('');
    console.log('📝 Summary:');
    console.log('   • open-ontologies CLI: Working ✅');
    console.log('   • Status endpoint: Working ✅');
    console.log('   • Validate command: Working ✅');
    console.log('   • Load command: Working ✅');
    console.log('   • Stats command: Working ✅');
    console.log('   • Query command: Working ✅');
    console.log('   • Clear command: Working ✅');
    console.log('');
    console.log('🎯 Next: Test UNRDF daemon MCP tools with open-ontologies');
    console.log('   Run: cd packages/daemon && pnpm mcp:start');
    console.log('   Then call tools like onto_validate, onto_stats, onto_query');

  } catch (error) {
    console.error('❌ Test failed:', error.message);
    process.exit(1);
  }
}

main().catch(error => {
  console.error('Unhandled error:', error);
  process.exit(1);
});
