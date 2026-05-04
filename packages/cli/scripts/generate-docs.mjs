#!/usr/bin/env node

/**
 * Generate documentation from CLI ontology
 */

import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import { runSync } from '../dist/cli/commands/sync/orchestrator.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const configPath = resolve(__dirname, '../unrdf-docs.toml');

console.log('Generating CLI documentation from ontology...\n');

runSync({
  config: configPath,
  dryRun: false,
  verbose: true,
  force: true,
})
  .then(result => {
    if (result.success) {
      console.log('\n✓ Documentation generated successfully!');
      process.exit(0);
    } else {
      console.log('\n✗ Documentation generation failed');
      process.exit(1);
    }
  })
  .catch(err => {
    console.error('\n✗ Error:', err.message);
    process.exit(1);
  });
