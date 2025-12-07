#!/usr/bin/env node

/**
 * CLI entry point for validation suite
 * 
 * Usage: node src/validation-suite-cli.mjs <moduleName>
 */

import { ValidationSuite } from './validation-suite.mjs';

const moduleName = process.argv[2];

if (!moduleName) {
  console.error('Usage: node src/validation-suite-cli.mjs <moduleName>');
  console.error('Example: node src/validation-suite-cli.mjs hello_world');
  process.exit(1);
}

const suite = new ValidationSuite({
  log: (message) => console.log(`[LOG] ${message}`),
  onResult: (result) => {
    const status = result.status === 'pass' ? '✅' : result.status === 'fail' ? '❌' : result.status === 'running' ? '🔄' : '⏳';
    console.log(`${status} ${result.name}: ${result.message || ''}`);
  }
});

(async () => {
  try {
    console.log(`Running validation suite for module: ${moduleName}\n`);
    const results = await suite.runAll(moduleName);
    
    console.log('\n--- Validation Summary ---');
    const passed = results.filter(r => r.status === 'pass').length;
    const failed = results.filter(r => r.status === 'fail').length;
    console.log(`Passed: ${passed}`);
    console.log(`Failed: ${failed}`);
    console.log(`Total: ${results.length}`);
    
    if (failed > 0) {
      console.log('\nFailed validations:');
      results.filter(r => r.status === 'fail').forEach(r => {
        console.log(`  - ${r.name}: ${r.message}`);
        if (r.error) {
          console.log(`    Error: ${r.error.message}`);
        }
      });
      process.exit(1);
    } else {
      console.log('\n✅ All validations passed!');
      process.exit(0);
    }
  } catch (error) {
    console.error(`\n❌ Validation suite error: ${error.message}`);
    process.exit(1);
  }
})();

