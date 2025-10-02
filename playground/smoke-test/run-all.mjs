/**
 * Run all README smoke tests
 */

import { spawn } from 'child_process';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const tests = [
  { name: 'Quick Start', file: '01-quick-start.mjs', lines: '64-100' },
  { name: 'Simple Knowledge Graph', file: '02-simple-knowledge-graph.mjs', lines: '332-377' },
  { name: 'Policy-Driven Validation', file: '03-policy-driven-validation.mjs', lines: '379-423' },
  { name: 'Cryptographic Audit Trail', file: '04-cryptographic-audit-trail.mjs', lines: '426-460' },
  { name: 'Dark Matter 80/20 Optimization', file: '05-dark-matter-optimization.mjs', lines: '268-283' },
  { name: 'OpenTelemetry Observability', file: '06-opentelemetry-observability.mjs', lines: '285-302' }
];

const results = [];

console.log('🚀 Running README Code Examples Validation\n');
console.log('=' .repeat(60));

for (const test of tests) {
  console.log(`\n📝 ${test.name} (lines ${test.lines})`);
  console.log('-'.repeat(60));

  const testPath = join(__dirname, test.file);

  const passed = await new Promise((resolve) => {
    const child = spawn('node', [testPath], {
      stdio: 'inherit',
      cwd: __dirname
    });

    child.on('close', (code) => {
      resolve(code === 0);
    });

    child.on('error', (err) => {
      console.error('Failed to run test:', err);
      resolve(false);
    });
  });

  results.push({ ...test, passed });
}

console.log('\n' + '='.repeat(60));
console.log('📊 SUMMARY\n');

const passCount = results.filter(r => r.passed).length;
const totalCount = results.length;

console.log('AGENT: Code Examples Validator');
console.log(`STATUS: ${passCount === totalCount ? 'PASS' : 'FAIL'}\n`);

console.log('EXAMPLE RESULTS:');
results.forEach((r, i) => {
  console.log(`${i + 1}. ${r.name} (lines ${r.lines}):`);
  console.log(`   ${r.passed ? '✅' : '❌'} ${r.passed ? 'Runs without errors' : 'Failed'}`);
  console.log(`   ${r.passed ? '✅' : '❌'} ${r.passed ? 'Produces expected output' : 'Did not produce expected output'}`);
});

console.log(`\nPASS RATE: ${passCount}/${totalCount} examples work`);

if (passCount < totalCount) {
  console.log('\nCRITICAL ISSUES:');
  results.filter(r => !r.passed).forEach(r => {
    console.log(`- ${r.name} (lines ${r.lines}) FAILED`);
  });

  console.log('\nRECOMMENDATIONS:');
  console.log('- Review failed examples and fix implementation');
  console.log('- Update README if examples are incorrect');
  console.log('- Re-run validation after fixes');
}

console.log('\n' + '='.repeat(60));

process.exit(passCount === totalCount ? 0 : 1);
