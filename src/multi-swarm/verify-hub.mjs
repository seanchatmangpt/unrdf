/**
 * @fileoverview Test hub coordination
 */

import { CoordinationHub } from './coordination.mjs';
import { WorkerSwarm } from './worker-swarm.mjs';

console.log('🔍 Hub Coordination Test\n');

// Simple processor
async function simpleProcessor(work) {
  console.log('  Agent processing:', work);
  return { processed: true, ...work };
}

// Create hub
console.log('1. Creating coordination hub...');
const hub = new CoordinationHub();
console.log('✅ Hub created\n');

// Create swarm
console.log('2. Creating worker swarm...');
const swarm = new WorkerSwarm('test-swarm', {
  domain: 'test',
  capacity: 2
});
swarm.addAgent('agent-1', simpleProcessor);
console.log('✅ Swarm created\n');

// Connect swarm to hub
console.log('3. Connecting swarm to hub...');
swarm.connectToHub(hub);
console.log('✅ Connected\n');

// Start hub and swarm
console.log('4. Starting hub and swarm...');
hub.start();
await swarm.start();
console.log('✅ Started\n');

// Distribute work via hub
console.log('5. Distributing work via hub...');
const workId = hub.distributeWork({
  type: 'test',
  payload: { data: 'test-data' }
});
console.log('   Work distributed with ID:', workId);
console.log('   Active work:', hub.activeWork.size);
console.log();

// Wait a bit for processing
console.log('6. Waiting for processing...');
await new Promise(resolve => setTimeout(resolve, 1000));

// Check results
console.log('7. Checking results...');
console.log('   Active work:', hub.activeWork.size);
console.log('   Results:', hub.results.size);

if (hub.results.size > 0) {
  const workIds = Array.from(hub.results.keys());
  for (const id of workIds) {
    const result = hub.results.get(id);
    console.log('   Result:', result);
  }
} else {
  console.log('   ❌ No results yet - checking messages...');
  console.log('   Queue size:', hub.messageQueue.size);

  // Manually trigger message processing
  console.log('\n8. Manually processing messages...');
  await swarm.processMessages();

  await new Promise(resolve => setTimeout(resolve, 500));

  console.log('   Active work:', hub.activeWork.size);
  console.log('   Results:', hub.results.size);
}

// Stop
await swarm.stop();
hub.stop();

console.log('\n✅ Test complete\n');

process.exit(0);
