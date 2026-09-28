#!/usr/bin/env node
/**
 * @fileoverview Guard Denial Demonstration
 *
 * Proves that guards enforce security policies and emit denial receipts.
 *
 * Run: node examples/guard-denial-demo.mjs
 */

import { GuardManager } from '../src/guards.mjs';
import { createOrchestrator } from '../src/orchestrator.mjs';
import { createObservation } from '../src/observation.mjs';

console.log('=== KGC Probe Guard Denial Demo ===\n');

// Create orchestrator with guards
const orchestrator = createOrchestrator({
  out: '/tmp/guard-demo-output',
  root: ['/tmp/allowed'],
  netAllow: ['api.example.com'],
  budgetMs: 5000,
  samples: 10
});

const guards = orchestrator.getGuardManager();

// Test 1: Secret pattern denial
console.log('Test 1: Attempting to read SECRET_KEY file...');
const result1 = guards.guardFileRead('/tmp/allowed/SECRET_KEY', 'demo-agent');
console.log(`  ❌ Denied: ${result1.denial ? 'YES' : 'NO'}`);
if (result1.denial) {
  console.log(`  Guard: ${result1.denial.data.guardName}`);
  console.log(`  Reason: ${result1.denial.data.reason}`);
}
console.log();

// Test 2: Path outside allowed roots
console.log('Test 2: Attempting to read /etc/passwd...');
const result2 = guards.guardFileRead('/etc/passwd', 'demo-agent');
console.log(`  ❌ Denied: ${result2.denial ? 'YES' : 'NO'}`);
if (result2.denial) {
  console.log(`  Guard: ${result2.denial.data.guardName}`);
  console.log(`  Reason: ${result2.denial.data.reason}`);
}
console.log();

// Test 3: Network access to non-allowlisted host
console.log('Test 3: Attempting to access evil.com...');
const result3 = guards.guardNetworkAccess('evil.com', 'demo-agent');
console.log(`  ❌ Denied: ${result3.denial ? 'YES' : 'NO'}`);
if (result3.denial) {
  console.log(`  Guard: ${result3.denial.data.guardName}`);
  console.log(`  Reason: ${result3.denial.data.reason}`);
}
console.log();

// Test 4: Allowed file access
console.log('Test 4: Attempting to read allowed file...');
const result4 = guards.guardFileRead('/tmp/allowed/package.json', 'demo-agent');
console.log(`  ✅ Allowed: ${!result4.denial ? 'YES' : 'NO'}`);
console.log();

// Test 5: Allowed network access
console.log('Test 5: Attempting to access api.example.com...');
const result5 = guards.guardNetworkAccess('api.example.com', 'demo-agent');
console.log(`  ✅ Allowed: ${!result5.denial ? 'YES' : 'NO'}`);
console.log();

// Show guard statistics
const stats = guards.getStats();
console.log('=== Guard Statistics ===');
console.log(`Total denials: ${stats.totalDenials}`);
console.log('By guard:');
for (const [guard, count] of Object.entries(stats.byGuard)) {
  console.log(`  ${guard}: ${count}`);
}
console.log();

// Register a normal observation
orchestrator.registerAgentOutput('demo-agent', [
  createObservation({
    category: 'file',
    severity: 'info',
    message: 'Demo observation',
    data: { demo: true },
    metadata: {
      agentId: 'demo-agent',
      probeVersion: '1.0.0',
      budgetMs: 5000,
      actualMs: 10,
      timestamp: new Date().toISOString()
    }
  })
]);

// Write outputs (includes guard denials)
console.log('Writing outputs with guard denials...');
const output = await orchestrator.writeOutputs();

console.log('\n=== Output Summary ===');
console.log(`Observations: ${output.observationCount} (${output.observationCount - stats.totalDenials} normal + ${stats.totalDenials} denials)`);
console.log(`Receipts: ${output.receiptCount}`);
console.log(`Manifest hash: ${output.manifestHash}`);
console.log(`Guard denials: ${output.guardDenials}`);
console.log('\nOutput directory: /tmp/guard-demo-output');

console.log('\n✅ Guard denial demo complete!');
console.log('Denials are recorded in observations.json and observable via receipts.');
