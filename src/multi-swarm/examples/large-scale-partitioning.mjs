/**
 * @fileoverview Multi-Swarm Example: Large-Scale Observable Partitioning
 *
 * **Use Case**: Process 1000+ observables by partitioning across multiple swarms
 *
 * **Strategy**:
 * - Partition dataset by domain
 * - Distribute to specialized swarms
 * - Aggregate results
 * - Verify with nested receipts
 *
 * @example
 * node examples/large-scale-partitioning.mjs
 */

import { createMultiSwarmSystem } from '../index.mjs';
import { blake3 } from 'hash-wasm';

/**
 * Transform observables (domain: transform)
 *
 * @param {Object} work - Work payload
 * @returns {Promise<Object>}
 */
async function transformObservables(work) {
  const { observables } = work;

  await new Promise(resolve => setTimeout(resolve, 10));

  return observables.map(obs => ({
    ...obs,
    transformed: true,
    value: obs.value * 2,
    timestamp: new Date().toISOString()
  }));
}

/**
 * Filter observables (domain: filter)
 *
 * @param {Object} work - Work payload
 * @returns {Promise<Object>}
 */
async function filterObservables(work) {
  const { observables, predicate } = work;

  await new Promise(resolve => setTimeout(resolve, 10));

  return observables.filter(predicate || (obs => obs.value > 50));
}

/**
 * Aggregate observables (domain: aggregate)
 *
 * @param {Object} work - Work payload
 * @returns {Promise<Object>}
 */
async function aggregateObservables(work) {
  const { observables } = work;

  await new Promise(resolve => setTimeout(resolve, 10));

  const hash = await blake3(JSON.stringify(observables));

  return {
    count: observables.length,
    sum: observables.reduce((acc, obs) => acc + obs.value, 0),
    avg: observables.reduce((acc, obs) => acc + obs.value, 0) / observables.length,
    min: Math.min(...observables.map(obs => obs.value)),
    max: Math.max(...observables.map(obs => obs.value)),
    hash
  };
}

/**
 * Run large-scale partitioning demo
 */
async function runLargeScaleDemo() {
  console.log('🚀 Starting Large-Scale Observable Partitioning\n');

  const OBSERVABLE_COUNT = 1000;
  const PARTITION_SIZE = 100;

  // Create multi-swarm system with specialized domains
  const system = await createMultiSwarmSystem({
    swarms: [
      {
        id: 'transform-swarm-1',
        domain: 'transform',
        capacity: 5,
        agents: Array.from({ length: 5 }, (_, i) => ({
          id: `transformer-${i + 1}`,
          processor: transformObservables
        }))
      },
      {
        id: 'transform-swarm-2',
        domain: 'transform',
        capacity: 5,
        agents: Array.from({ length: 5 }, (_, i) => ({
          id: `transformer-${i + 6}`,
          processor: transformObservables
        }))
      },
      {
        id: 'filter-swarm',
        domain: 'filter',
        capacity: 3,
        agents: Array.from({ length: 3 }, (_, i) => ({
          id: `filter-${i + 1}`,
          processor: filterObservables
        }))
      },
      {
        id: 'aggregate-swarm',
        domain: 'aggregate',
        capacity: 2,
        agents: Array.from({ length: 2 }, (_, i) => ({
          id: `aggregator-${i + 1}`,
          processor: aggregateObservables
        }))
      }
    ],
    queenOptions: {
      heartbeatInterval: 2000
    }
  });

  await system.start();
  console.log('✅ Multi-swarm system started');
  console.log(`   Swarms: 4 (2 transform, 1 filter, 1 aggregate)`);
  console.log(`   Total Agents: 15\n`);

  // Generate observables
  console.log(`📊 Generating ${OBSERVABLE_COUNT} observables...`);
  const observables = Array.from({ length: OBSERVABLE_COUNT }, (_, i) => ({
    id: `obs-${i}`,
    value: Math.floor(Math.random() * 100),
    timestamp: new Date().toISOString()
  }));
  console.log(`✅ Generated ${observables.length} observables\n`);

  // Phase 1: Transform
  console.log('Phase 1: Transform');
  const startTransform = Date.now();

  const transformedResults = await system.submitJob({
    type: 'transform',
    domain: 'transform',
    payload: observables,
    partitionStrategy: 'domain',
    aggregationStrategy: 'concat'
  });

  const transformDuration = Date.now() - startTransform;
  console.log(`✅ Transformed ${transformedResults.length} observables in ${transformDuration}ms`);
  console.log(`   Throughput: ${(transformedResults.length / transformDuration * 1000).toFixed(0)} obs/sec\n`);

  // Phase 2: Filter
  console.log('Phase 2: Filter');
  const startFilter = Date.now();

  const filteredResults = await system.submitJob({
    type: 'filter',
    domain: 'filter',
    payload: { observables: transformedResults, predicate: (obs) => obs.value > 100 },
    partitionStrategy: 'domain',
    aggregationStrategy: 'concat'
  });

  const filterDuration = Date.now() - startFilter;
  console.log(`✅ Filtered to ${filteredResults.length} observables in ${filterDuration}ms`);
  console.log(`   Reduction: ${((1 - filteredResults.length / transformedResults.length) * 100).toFixed(1)}%\n`);

  // Phase 3: Aggregate
  console.log('Phase 3: Aggregate');
  const startAggregate = Date.now();

  const aggregateResult = await system.submitJob({
    type: 'aggregate',
    domain: 'aggregate',
    payload: { observables: filteredResults },
    partitionStrategy: 'domain',
    aggregationStrategy: 'merge'
  });

  const aggregateDuration = Date.now() - startAggregate;
  console.log(`✅ Aggregated in ${aggregateDuration}ms`);
  console.log(`   Count: ${aggregateResult.count}`);
  console.log(`   Sum: ${aggregateResult.sum.toFixed(0)}`);
  console.log(`   Avg: ${aggregateResult.avg.toFixed(2)}`);
  console.log(`   Min: ${aggregateResult.min}`);
  console.log(`   Max: ${aggregateResult.max}`);
  console.log(`   Hash: ${aggregateResult.hash.substring(0, 16)}...\n`);

  // Total pipeline duration
  const totalDuration = transformDuration + filterDuration + aggregateDuration;
  console.log(`⏱️  Total Pipeline Duration: ${totalDuration}ms\n`);

  // Get system statistics
  const stats = system.getStats();
  console.log('📈 System Statistics:');
  console.log(`   Queen Jobs: ${stats.queen.completedJobs} completed`);
  console.log(`   Queen Receipts: ${stats.queen.queenReceipts}`);
  console.log(`   Total Work Items: ${stats.coordination.completedWork}`);
  console.log(`   Overall Success Rate: ${stats.performance.successRate}\n`);

  // Swarm-level statistics
  console.log('📊 Swarm Performance:');
  for (const swarmStat of stats.swarms) {
    console.log(`   ${swarmStat.id}:`);
    console.log(`      Utilization: ${swarmStat.utilization}`);
    console.log(`      Work: ${swarmStat.work.completed} completed, ${swarmStat.work.active} active`);
    console.log(`      Success Rate: ${swarmStat.performance.successRate}`);
  }
  console.log();

  // Verify nested receipt chains
  console.log('🔐 Verifying Nested Receipt Chains...');
  const verification = await system.verifyAllReceipts();
  console.log(`   Queen Chain: ${verification.queen.valid ? '✅ Valid' : '❌ Invalid'} (${stats.queen.queenReceipts} receipts)`);
  for (const worker of verification.workers) {
    const workerStat = stats.swarms.find(s => s.id === worker.swarmId);
    console.log(`   ${worker.swarmId}: ${worker.verification.valid ? '✅ Valid' : '❌ Invalid'} (${workerStat.receipts} receipts)`);
  }
  console.log();

  // Performance insights
  console.log('💡 Performance Insights:');
  console.log(`   Parallel Processing: ${stats.swarms.length} swarms, 15 agents`);
  console.log(`   Work Distribution: ${stats.coordination.totalSwarms} swarms active`);
  console.log(`   Load Balance: ${stats.coordination.averageUtilization} avg utilization`);
  console.log(`   Fault Tolerance: Isolated failure domains\n`);

  await system.stop();
  console.log('🛑 System stopped');
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runLargeScaleDemo().catch(console.error);
}

export { runLargeScaleDemo };
