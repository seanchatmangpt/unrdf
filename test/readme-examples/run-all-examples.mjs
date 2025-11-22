#!/usr/bin/env node
/**
 * Run all README example tests
 * Validates that every code example in README.md actually works
 */

import { testQuickStart } from './example-quick-start.mjs';
import { testRdfEngine } from './example-rdf-engine.mjs';
import { testKnowledgeHooks } from './example-knowledge-hooks.mjs';
import { testSparqlQueries } from './example-sparql-queries.mjs';
import { testShaclValidation } from './example-shacl-validation.mjs';
import { testLockchain } from './example-lockchain.mjs';
import { testDarkMatter } from './example-dark-matter.mjs';
import { testObservability } from './example-observability.mjs';
import { testSimpleGraph } from './example-simple-graph.mjs';
import { testPolicyValidation } from './example-policy-validation.mjs';
import { testAuditTrail } from './example-audit-trail.mjs';

const tests = [
  { name: 'Quick Start', fn: testQuickStart, lines: '66-102' },
  { name: 'RDF Engine', fn: testRdfEngine, lines: '114-131' },
  { name: 'Knowledge Hooks', fn: testKnowledgeHooks, lines: '143-168' },
  { name: 'SPARQL Queries', fn: testSparqlQueries, lines: '182-208' },
  { name: 'SHACL Validation', fn: testShaclValidation, lines: '214-241' },
  { name: 'Lockchain', fn: testLockchain, lines: '247-268' },
  { name: 'Dark Matter', fn: testDarkMatter, lines: '274-285' },
  { name: 'Observability', fn: testObservability, lines: '291-304' },
  { name: 'Simple Graph', fn: testSimpleGraph, lines: '336-379' },
  { name: 'Policy Validation', fn: testPolicyValidation, lines: '383-426' },
  { name: 'Audit Trail', fn: testAuditTrail, lines: '430-462' },
];

console.log('═══════════════════════════════════════════════════════');
console.log('🧪 README EXAMPLES VERIFICATION');
console.log('═══════════════════════════════════════════════════════\n');

const results = [];

for (const test of tests) {
  console.log(`\n📋 ${test.name} (README lines ${test.lines})`);
  console.log('───────────────────────────────────────────────────────');

  try {
    const success = await test.fn();
    results.push({ ...test, success });
  } catch (error) {
    console.error(`❌ ${test.name} threw exception:`, error.message);
    results.push({ ...test, success: false, error: error.message });
  }
}

console.log('\n═══════════════════════════════════════════════════════');
console.log('📊 VERIFICATION SUMMARY');
console.log('═══════════════════════════════════════════════════════\n');

const passed = results.filter(r => r.success).length;
const failed = results.filter(r => !r.success).length;

results.forEach(result => {
  const icon = result.success ? '✅' : '❌';
  const status = result.success ? 'PASSED' : 'FAILED';
  console.log(`${icon} ${result.name.padEnd(25)} ${status.padEnd(10)} (lines ${result.lines})`);
  if (result.error) {
    console.log(`   Error: ${result.error}`);
  }
});

console.log('\n───────────────────────────────────────────────────────');
console.log(`Total: ${tests.length} tests`);
console.log(`✅ Passed: ${passed}`);
console.log(`❌ Failed: ${failed}`);
console.log('═══════════════════════════════════════════════════════\n');

if (failed > 0) {
  console.log('❌ README examples verification FAILED');
  console.log('Some examples in README.md do not work as documented.\n');
  process.exit(1);
} else {
  console.log('✅ All README examples verified successfully!');
  console.log('Every code example in README.md works as documented.\n');
  process.exit(0);
}
