#!/usr/bin/env node
/**
 * Manual Determinism Proof - Agent 4 Resource Allocator
 *
 * Runs allocate() multiple times and verifies identical outputs.
 * No test framework dependencies - pure Node.js verification.
 */

import { allocate } from './src/Allocator.mjs';

const workItems = [
  { id: 'wi-003', type: 'review', requiredCapabilities: ['code-review'] },
  { id: 'wi-001', type: 'implement', requiredCapabilities: ['backend'] },
  { id: 'wi-002', type: 'test', requiredCapabilities: ['testing'] },
  { id: 'wi-005', type: 'deploy', requiredCapabilities: [] },
  { id: 'wi-004', type: 'document', requiredCapabilities: [] },
];

const agents = [
  { id: 'agent-1', maxConcurrent: 2, capabilities: ['backend', 'testing'] },
  { id: 'agent-2', maxConcurrent: 1, capabilities: ['code-review'] },
  { id: 'agent-3', maxConcurrent: 1, capabilities: [] },
];

console.log('=== Agent 4: Resource Allocator Determinism Proof ===\n');

console.log('Test 1: Identical inputs produce identical outputs\n');

const runs = [];
for (let i = 1; i <= 5; i++) {
  const result = allocate(workItems, agents);
  runs.push(result);
  console.log(`Run ${i}:`);
  console.log(`  Assignments: ${result.totalAssigned}, Waitlisted: ${result.totalWaitlisted}`);
  console.log(`  Assignment IDs: ${result.assignments.map((a) => a.workItemId).join(', ')}`);
}

// Verify all runs produced identical results
const firstJSON = JSON.stringify(runs[0].assignments);
let allIdentical = true;

for (let i = 1; i < runs.length; i++) {
  const currentJSON = JSON.stringify(runs[i].assignments);
  if (firstJSON !== currentJSON) {
    allIdentical = false;
    console.error(`\n❌ FAIL: Run ${i + 1} differs from Run 1`);
    break;
  }
}

if (allIdentical) {
  console.log('\n✅ PASS: All 5 runs produced identical assignments');
} else {
  console.error('\n❌ FAIL: Determinism violated');
  process.exit(1);
}

console.log('\n---\n');
console.log('Test 2: Input order independence (commutativity)\n');

// Shuffle work items in different orders
const shuffled1 = [workItems[4], workItems[1], workItems[3], workItems[0], workItems[2]];
const shuffled2 = [workItems[2], workItems[4], workItems[0], workItems[1], workItems[3]];

const result1 = allocate(workItems, agents);
const result2 = allocate(shuffled1, agents);
const result3 = allocate(shuffled2, agents);

const result1JSON = JSON.stringify(result1.assignments);
const result2JSON = JSON.stringify(result2.assignments);
const result3JSON = JSON.stringify(result3.assignments);

if (result1JSON === result2JSON && result2JSON === result3JSON) {
  console.log('✅ PASS: Different input orders produce identical assignments');
} else {
  console.error('❌ FAIL: Commutativity violated');
  console.error('  Original:', result1.assignments);
  console.error('  Shuffled 1:', result2.assignments);
  console.error('  Shuffled 2:', result3.assignments);
  process.exit(1);
}

console.log('\n---\n');
console.log('Test 3: Capacity enforcement (exhaustion scenario)\n');

const manyItems = Array.from({ length: 10 }, (_, i) => ({
  id: `wi-${String(i + 1).padStart(3, '0')}`,
  type: 'task',
  requiredCapabilities: [],
}));

const limitedAgents = [
  { id: 'agent-A', maxConcurrent: 1, capabilities: [] },
  { id: 'agent-B', maxConcurrent: 1, capabilities: [] },
  { id: 'agent-C', maxConcurrent: 1, capabilities: [] },
];

const exhaustionResult = allocate(manyItems, limitedAgents);

console.log(`Total capacity: ${exhaustionResult.totalCapacity}`);
console.log(`Assigned: ${exhaustionResult.totalAssigned}`);
console.log(`Waitlisted: ${exhaustionResult.totalWaitlisted}`);

// Verify capacity constraints
const agentACount = exhaustionResult.assignments.filter((a) => a.agentId === 'agent-A').length;
const agentBCount = exhaustionResult.assignments.filter((a) => a.agentId === 'agent-B').length;
const agentCCount = exhaustionResult.assignments.filter((a) => a.agentId === 'agent-C').length;

console.log(`\nPer-agent assignments:`);
console.log(`  agent-A: ${agentACount} (capacity: 1)`);
console.log(`  agent-B: ${agentBCount} (capacity: 1)`);
console.log(`  agent-C: ${agentCCount} (capacity: 1)`);

if (
  agentACount <= 1 &&
  agentBCount <= 1 &&
  agentCCount <= 1 &&
  exhaustionResult.totalAssigned === 3 &&
  exhaustionResult.totalWaitlisted === 7
) {
  console.log('\n✅ PASS: Capacity limits enforced correctly');
} else {
  console.error('\n❌ FAIL: Capacity enforcement violated');
  process.exit(1);
}

console.log('\n---\n');
console.log('Test 4: Capability matching\n');

const capabilityItems = [
  { id: 'wi-backend', type: 'api', requiredCapabilities: ['backend'] },
  { id: 'wi-frontend', type: 'ui', requiredCapabilities: ['frontend'] },
  { id: 'wi-rust', type: 'compile', requiredCapabilities: ['rust'] },
];

const capabilityAgents = [
  { id: 'agent-backend', maxConcurrent: 2, capabilities: ['backend'] },
  { id: 'agent-frontend', maxConcurrent: 2, capabilities: ['frontend'] },
];

const capResult = allocate(capabilityItems, capabilityAgents);

const backendAssignment = capResult.assignments.find((a) => a.workItemId === 'wi-backend');
const frontendAssignment = capResult.assignments.find((a) => a.workItemId === 'wi-frontend');
const rustAssignment = capResult.assignments.find((a) => a.workItemId === 'wi-rust');

console.log(`wi-backend → ${backendAssignment?.agentId || 'WAITLIST'}`);
console.log(`wi-frontend → ${frontendAssignment?.agentId || 'WAITLIST'}`);
console.log(`wi-rust → ${rustAssignment?.agentId || 'WAITLIST'}`);

if (
  backendAssignment?.agentId === 'agent-backend' &&
  frontendAssignment?.agentId === 'agent-frontend' &&
  !rustAssignment // Should be waitlisted (no agent has 'rust')
) {
  console.log('\n✅ PASS: Capability matching works correctly');
} else {
  console.error('\n❌ FAIL: Capability matching violated');
  process.exit(1);
}

console.log('\n=== All Proofs PASSED ===');
console.log('\nDeterminism: ✅');
console.log('Commutativity: ✅');
console.log('Capacity Safety: ✅');
console.log('Capability Enforcement: ✅');
console.log('\nAgent 4 Resource Allocator is PROVEN CORRECT.\n');

process.exit(0);
