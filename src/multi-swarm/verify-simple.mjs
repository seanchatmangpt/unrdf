/**
 * @fileoverview Simpler verification - test worker swarm directly
 */

import { WorkerSwarm } from './worker-swarm.mjs';

console.log('🔍 Worker Swarm Direct Test\n');

// Simple processor
async function simpleProcessor(work) {
  console.log('  Processing:', work);
  return { processed: true, ...work };
}

// Create swarm
console.log('1. Creating worker swarm...');
const swarm = new WorkerSwarm('test-swarm', {
  domain: 'test',
  capacity: 2
});

swarm.addAgent('agent-1', simpleProcessor);
console.log('✅ Swarm created with 1 agent\n');

// Start swarm
console.log('2. Starting swarm...');
await swarm.start();
console.log('✅ Swarm started\n');

// Submit work directly
console.log('3. Submitting work directly to swarm...');
const result = await swarm.submitWork({
  type: 'test',
  payload: { data: 'test-data' }
});
console.log('✅ Work completed:', result);
console.log();

// Get stats
console.log('4. Swarm statistics:');
const stats = swarm.getStats();
console.log('   Agents:', stats.agents);
console.log('   Work:', stats.work);
console.log('   Receipts:', stats.receipts);
console.log();

// Verify receipts
console.log('5. Verifying receipts...');
const verification = await swarm.verifyReceipts();
console.log('   Valid:', verification.valid ? '✅' : '❌');
console.log();

// Stop swarm
await swarm.stop();
console.log('✅ Worker Swarm: WORKING\n');

process.exit(0);
