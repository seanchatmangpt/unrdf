/**
 * @fileoverview Multi-Swarm Example: Compression + Validation Pipeline
 *
 * **Use Case**: Process large dataset through compression and validation swarms
 *
 * **Architecture**:
 * - Compression Swarm: Compresses data chunks
 * - Validation Swarm: Validates compressed data
 * - Queen: Orchestrates pipeline
 *
 * @example
 * node examples/compression-validation.mjs
 */

import { createMultiSwarmSystem } from '../index.mjs';
import { blake3 } from 'hash-wasm';

/**
 * Compress data (simulated)
 *
 * @param {Object} work - Work payload
 * @returns {Promise<Object>} Compressed data
 */
async function compressData(work) {
  const { data } = work;

  // Simulate compression
  await new Promise(resolve => setTimeout(resolve, 100));

  const compressed = {
    original: data,
    compressed: data.map(x => x % 256),
    ratio: 0.5,
    algorithm: 'simulated-lz4'
  };

  return compressed;
}

/**
 * Validate compressed data
 *
 * @param {Object} work - Work payload
 * @returns {Promise<Object>} Validation result
 */
async function validateData(work) {
  const { compressed } = work;

  // Simulate validation
  await new Promise(resolve => setTimeout(resolve, 50));

  const hash = await blake3(JSON.stringify(compressed));

  return {
    valid: true,
    hash,
    checks: ['ratio', 'algorithm', 'integrity'],
    timestamp: new Date().toISOString()
  };
}

/**
 * Run compression + validation pipeline
 */
async function runPipeline() {
  console.log('🚀 Starting Multi-Swarm Compression + Validation Pipeline\n');

  // Create multi-swarm system
  const system = await createMultiSwarmSystem({
    swarms: [
      {
        id: 'compression-swarm',
        domain: 'compression',
        capacity: 3,
        agents: [
          { id: 'compressor-1', processor: compressData },
          { id: 'compressor-2', processor: compressData },
          { id: 'compressor-3', processor: compressData }
        ]
      },
      {
        id: 'validation-swarm',
        domain: 'validation',
        capacity: 2,
        agents: [
          { id: 'validator-1', processor: validateData },
          { id: 'validator-2', processor: validateData }
        ]
      }
    ],
    queenOptions: {
      heartbeatInterval: 2000
    }
  });

  // Start system
  await system.start();
  console.log('✅ Multi-swarm system started\n');

  // Generate test data
  const testData = Array.from({ length: 10 }, (_, i) => ({
    id: `chunk-${i}`,
    data: Array.from({ length: 100 }, () => Math.floor(Math.random() * 1000))
  }));

  console.log(`📦 Processing ${testData.length} data chunks\n`);

  // Phase 1: Compress data
  console.log('Phase 1: Compression');
  const compressionJob = await system.submitJob({
    type: 'compress',
    payload: testData,
    partitionStrategy: 'domain',
    aggregationStrategy: 'concat'
  });

  console.log(`✅ Compression complete: ${compressionJob.length} chunks\n`);

  // Phase 2: Validate compressed data
  console.log('Phase 2: Validation');
  const validationJob = await system.submitJob({
    type: 'validate',
    payload: compressionJob,
    partitionStrategy: 'domain',
    aggregationStrategy: 'concat'
  });

  console.log(`✅ Validation complete: ${validationJob.length} results\n`);

  // Check results
  const allValid = validationJob.every(v => v.valid);
  console.log(`📊 Validation Status: ${allValid ? '✅ All valid' : '❌ Some invalid'}\n`);

  // Get statistics
  const stats = system.getStats();
  console.log('📈 System Statistics:');
  console.log(`   Queen: ${stats.queen.completedJobs} jobs completed`);
  console.log(`   Queen Receipts: ${stats.queen.queenReceipts}`);
  console.log(`   Swarms: ${stats.queen.swarms}`);
  console.log(`   Total Work: ${stats.coordination.completedWork} completed, ${stats.coordination.failedWork} failed`);
  console.log(`   Success Rate: ${stats.performance.successRate}\n`);

  // Verify receipt chains
  console.log('🔐 Verifying receipt chains...');
  const verification = await system.verifyAllReceipts();
  console.log(`   Queen chain: ${verification.queen.valid ? '✅ Valid' : '❌ Invalid'}`);
  for (const worker of verification.workers) {
    console.log(`   ${worker.swarmId}: ${worker.verification.valid ? '✅ Valid' : '❌ Invalid'}`);
  }

  // Stop system
  await system.stop();
  console.log('\n🛑 System stopped');
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runPipeline().catch(console.error);
}

export { runPipeline };
