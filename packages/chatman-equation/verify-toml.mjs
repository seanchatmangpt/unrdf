#!/usr/bin/env node
/**
 * Verify TOML configuration files
 * Simple verification script to show TOML loading works
 */

import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';
import TOML from '@iarna/toml';

const __dirname = dirname(fileURLToPath(import.meta.url));

console.log('Chatman Equation TOML Verification\n');

// Load equation schema
try {
  const schemaPath = resolve(__dirname, 'config/equation.schema.toml');
  const schemaContent = readFileSync(schemaPath, 'utf-8');
  const schema = TOML.parse(schemaContent);
  
  console.log('✓ Loaded equation.schema.toml');
  console.log(`  Version: ${schema.metadata.version}`);
  console.log(`  Components: ${Object.keys(schema).length}`);
  console.log(`  Domains: ${Object.keys(schema.unification || {}).length}`);
} catch (error) {
  console.error('✗ Failed to load equation.schema.toml:', error.message);
}

// Load examples
try {
  const examplesPath = resolve(__dirname, 'config/examples.toml');
  const examplesContent = readFileSync(examplesPath, 'utf-8');
  const examples = TOML.parse(examplesContent);
  
  console.log('\n✓ Loaded examples.toml');
  
  const exampleNames = Object.keys(examples);
  console.log(`  Examples: ${exampleNames.length}`);
  
  for (const name of exampleNames) {
    const example = examples[name];
    console.log(`\n  ${name}:`);
    console.log(`    Description: ${example.description}`);
    if (example.observation) {
      console.log(`    Observation: ${example.observation.id} (${example.observation.domain})`);
    }
    if (example.delta) {
      console.log(`    Delta: ${example.delta.id} (${example.delta.operations.length} operations)`);
    }
    if (example.closure_operator) {
      console.log(`    Operator: ${example.closure_operator.name} (${example.closure_operator.type})`);
    }
    if (example.expected_artifact) {
      console.log(`    Artifact: ${example.expected_artifact.id}`);
    }
  }
} catch (error) {
  console.error('✗ Failed to load examples.toml:', error.message);
}

console.log('\n✓ TOML verification complete\n');
