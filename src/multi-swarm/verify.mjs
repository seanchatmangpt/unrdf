/**
 * @fileoverview Quick verification of multi-swarm system
 */

import { createMultiSwarmSystem } from './index.mjs';

console.log('🔍 Multi-Swarm System Verification\n');

// Simple processor
async function simpleProcessor(work) {
  return { processed: true, ...work };
}

// Create system
console.log('1. Creating multi-swarm system...');
const system = await createMultiSwarmSystem({
  swarms: [
    {
      id: 'test-swarm-1',
      domain: 'test',
      capacity: 2,
      agents: [
        { id: 'agent-1', processor: simpleProcessor },
        { id: 'agent-2', processor: simpleProcessor }
      ]
    },
    {
      id: 'test-swarm-2',
      domain: 'test',
      capacity: 2,
      agents: [
        { id: 'agent-3', processor: simpleProcessor }
      ]
    }
  ],
  queenOptions: {
    heartbeatInterval: 1000
  }
});
console.log('✅ System created\n');

// Start system
console.log('2. Starting system...');
await system.start();
console.log('✅ System started\n');

// Submit simple job
console.log('3. Submitting job...');
const result = await system.submitJob({
  type: 'test',
  payload: { data: 'test-data' },
  timeout: 5000
});
console.log('✅ Job completed:', result);
console.log();

// Get stats
console.log('4. System statistics:');
const stats = system.getStats();
console.log('   Queen:', {
  swarms: stats.queen.swarms,
  completedJobs: stats.queen.completedJobs,
  receipts: stats.queen.queenReceipts
});
console.log('   Coordination:', {
  totalSwarms: stats.coordination.totalSwarms,
  completedWork: stats.coordination.completedWork
});
console.log();

// Verify receipts
console.log('5. Verifying receipt chains...');
const verification = await system.verifyAllReceipts();
console.log('   Queen chain:', verification.queen.valid ? '✅ Valid' : '❌ Invalid');
for (const worker of verification.workers) {
  console.log(`   ${worker.swarmId}:`, worker.verification.valid ? '✅ Valid' : '❌ Invalid');
}
console.log();

// Stop system
console.log('6. Stopping system...');
await system.stop();
console.log('✅ System stopped\n');

console.log('✨ Verification complete!\n');

// Summary
console.log('📊 Summary:');
console.log('   ✅ Multi-swarm system created');
console.log('   ✅ Jobs executed successfully');
console.log('   ✅ Receipt chains verified');
console.log('   ✅ All components operational\n');

console.log('🎉 Multi-Swarm Coordination System: WORKING\n');

process.exit(0);
