/**
 * @fileoverview Multi-Swarm Example: Fault Isolation
 *
 * **Use Case**: Demonstrate fault isolation - failures in one swarm don't cascade
 *
 * **Scenario**:
 * - Swarm A: Reliable workers (success rate: 100%)
 * - Swarm B: Unreliable workers (success rate: 50%)
 * - Queen: Routes work and handles failures gracefully
 *
 * @example
 * node examples/fault-isolation.mjs
 */

import { createMultiSwarmSystem } from '../index.mjs';

/**
 * Reliable processor (always succeeds)
 *
 * @param {Object} work - Work payload
 * @returns {Promise<Object>}
 */
async function reliableProcessor(work) {
  await new Promise(resolve => setTimeout(resolve, 50));
  return {
    status: 'success',
    input: work,
    processedBy: 'reliable-swarm',
    timestamp: new Date().toISOString()
  };
}

/**
 * Unreliable processor (50% failure rate)
 *
 * @param {Object} work - Work payload
 * @returns {Promise<Object>}
 */
async function unreliableProcessor(work) {
  await new Promise(resolve => setTimeout(resolve, 50));

  // 50% chance of failure
  if (Math.random() < 0.5) {
    throw new Error('Simulated failure in unreliable swarm');
  }

  return {
    status: 'success',
    input: work,
    processedBy: 'unreliable-swarm',
    timestamp: new Date().toISOString()
  };
}

/**
 * Run fault isolation demo
 */
async function runFaultIsolationDemo() {
  console.log('🚀 Starting Multi-Swarm Fault Isolation Demo\n');

  // Create system with reliable and unreliable swarms
  const system = await createMultiSwarmSystem({
    swarms: [
      {
        id: 'reliable-swarm',
        domain: 'reliable',
        capacity: 3,
        agents: [
          { id: 'reliable-1', processor: reliableProcessor },
          { id: 'reliable-2', processor: reliableProcessor },
          { id: 'reliable-3', processor: reliableProcessor }
        ]
      },
      {
        id: 'unreliable-swarm',
        domain: 'unreliable',
        capacity: 3,
        agents: [
          { id: 'unreliable-1', processor: unreliableProcessor },
          { id: 'unreliable-2', processor: unreliableProcessor },
          { id: 'unreliable-3', processor: unreliableProcessor }
        ]
      }
    ],
    queenOptions: {
      heartbeatInterval: 1000
    }
  });

  await system.start();
  console.log('✅ Multi-swarm system started\n');

  // Submit work to both swarms
  const testData = Array.from({ length: 20 }, (_, i) => ({
    id: `item-${i}`,
    value: i
  }));

  console.log('📊 Fault Isolation Test:\n');

  // Test 1: Reliable swarm (should succeed 100%)
  console.log('Test 1: Reliable Swarm');
  const reliableResults = [];
  for (const item of testData.slice(0, 10)) {
    try {
      const result = await system.submitJob({
        type: 'process',
        domain: 'reliable',
        payload: item,
        partitionStrategy: 'domain'
      });
      reliableResults.push({ success: true, result });
    } catch (error) {
      reliableResults.push({ success: false, error: error.message });
    }
  }

  const reliableSuccess = reliableResults.filter(r => r.success).length;
  console.log(`   ✅ Success: ${reliableSuccess}/${reliableResults.length}`);
  console.log(`   ❌ Failures: ${reliableResults.length - reliableSuccess}\n`);

  // Test 2: Unreliable swarm (should fail ~50%)
  console.log('Test 2: Unreliable Swarm (with retries)');
  const unreliableResults = [];
  for (const item of testData.slice(10, 20)) {
    try {
      const result = await system.submitJob({
        type: 'process',
        domain: 'unreliable',
        payload: item,
        partitionStrategy: 'domain'
      });
      unreliableResults.push({ success: true, result });
    } catch (error) {
      unreliableResults.push({ success: false, error: error.message });
    }
  }

  const unreliableSuccess = unreliableResults.filter(r => r.success).length;
  console.log(`   ✅ Success: ${unreliableSuccess}/${unreliableResults.length}`);
  console.log(`   ❌ Failures: ${unreliableResults.length - unreliableSuccess}\n`);

  // Get swarm-specific statistics
  const stats = system.getStats();
  console.log('📈 Swarm Statistics:\n');

  for (const swarmStat of stats.swarms) {
    console.log(`${swarmStat.id}:`);
    console.log(`   Status: ${swarmStat.status}`);
    console.log(`   Utilization: ${swarmStat.utilization}`);
    console.log(`   Completed: ${swarmStat.work.completed}`);
    console.log(`   Success Rate: ${swarmStat.performance.successRate}\n`);
  }

  // Verify isolation: Check if queen is still operational
  console.log('🔒 Fault Isolation Verification:');
  console.log(`   Queen Status: ${stats.queen.running ? '✅ Running' : '❌ Failed'}`);
  console.log(`   Queen Receipts: ${stats.queen.queenReceipts}`);
  console.log(`   Total Jobs: ${stats.queen.completedJobs}\n`);

  // Key insight: Failures in unreliable swarm don't affect reliable swarm or queen
  console.log('💡 Key Insight:');
  console.log('   Failures in unreliable swarm are isolated.');
  console.log('   Reliable swarm continues to operate normally.');
  console.log('   Queen orchestrator remains operational.\n');

  // Verify receipt chains (both should be valid despite unreliable swarm failures)
  const verification = await system.verifyAllReceipts();
  console.log('🔐 Receipt Chain Verification:');
  console.log(`   Queen: ${verification.queen.valid ? '✅ Valid' : '❌ Invalid'}`);
  for (const worker of verification.workers) {
    console.log(`   ${worker.swarmId}: ${worker.verification.valid ? '✅ Valid' : '❌ Invalid'}`);
  }

  await system.stop();
  console.log('\n🛑 System stopped');
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runFaultIsolationDemo().catch(console.error);
}

export { runFaultIsolationDemo };
