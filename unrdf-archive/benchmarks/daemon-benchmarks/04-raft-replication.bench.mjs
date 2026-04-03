/**
 * @file Raft Replication Latency Benchmark
 * @module @unrdf/daemon/benchmarks/raft-replication
 * @description Simulates and measures Raft consensus replication latency
 * Tests operation propagation through cluster nodes
 */

import { Daemon } from '../src/daemon.mjs';
import { analyzeVariance, storeBenchmarkResult } from './suite.mjs';
import { randomUUID } from 'crypto';

/**
 * Generate a valid UUID for daemon ID
 * @returns {string} Valid UUID
 */
function generateDaemonId() {
  return randomUUID();
}

/**
 * Simulates network latency
 * @param {number} baseLatency - Base network latency in ms
 * @param {number} jitter - Jitter percentage
 * @returns {Promise<number>} Simulated latency duration
 */
async function simulateNetworkLatency(baseLatency = 5, jitter = 0.1) {
  const variation = baseLatency * jitter * (Math.random() - 0.5) * 2;
  const actualLatency = Math.max(0, baseLatency + variation);
  return new Promise(resolve => {
    setTimeout(() => resolve(actualLatency), actualLatency);
  });
}

/**
 * Benchmark: Single leader replication
 * @param {Object} options - Benchmark options
 * @param {number} [options.nodeCount=3] - Number of nodes in cluster
 * @param {number} [options.operationCount=100] - Operations to replicate
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Object} Benchmark result with replication metrics
 */
export async function benchmarkLeaderReplication(options = {}) {
  const { nodeCount = 3, operationCount = 100, runs = 5 } = options;
  const replicationLatencies = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const nodes = [];
    let leader = null;

    // Create nodes
    for (let i = 0; i < nodeCount; i++) {
      const node = new Daemon({
        daemonId: generateDaemonId(),
        name: `cluster-node-${i}`,
        nodeId: `node-${i}`,
        clusterId: `cluster-${runIdx}`,
      });
      nodes.push(node);

      if (i === 0) {
        leader = node;
        node.isLeader = true;
      }
    }

    // Start all nodes
    await Promise.all(nodes.map(n => n.start()));

    // Replicate operations
    for (let opIdx = 0; opIdx < operationCount; opIdx++) {
      const operationId = `op-${opIdx}`;
      const startTime = performance.now();

      // Leader schedules operation
      leader.schedule({
        id: operationId,
        handler: async () => {
          // Simulate replication to followers
          await Promise.all(
            nodes.slice(1).map(node =>
              simulateNetworkLatency(2, 0.15).then(() => {
                node.schedule({
                  id: `${operationId}-replica`,
                  handler: async () => ({ replicated: true }),
                });
              })
            )
          );
          return { replicated: true };
        },
      });

      // Execute on leader
      await leader.execute(operationId).catch(() => {});

      const endTime = performance.now();
      replicationLatencies.push(endTime - startTime);
    }

    // Stop all nodes
    await Promise.all(nodes.map(n => n.stop()));
  }

  const variance = analyzeVariance(replicationLatencies);

  return storeBenchmarkResult({
    name: 'raft-leader-replication',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: replicationLatencies.length,
    nodeCount,
    operationCount,
    runs,
  });
}

/**
 * Benchmark: Raft consensus commit latency
 * @param {Object} options - Benchmark options
 * @param {number} [options.quorumSize=3] - Quorum size
 * @param {number} [options.operationCount=100] - Operations to commit
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Object} Benchmark result with commit latency
 */
export async function benchmarkConsensusCommit(options = {}) {
  const { quorumSize = 3, operationCount = 100, runs = 5 } = options;
  const commitLatencies = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const nodes = [];

    for (let i = 0; i < quorumSize; i++) {
      const node = new Daemon({
        daemonId: generateDaemonId(),
        name: `consensus-node-${i}`,
        nodeId: `node-${i}`,
        clusterId: `consensus-${runIdx}`,
      });
      nodes.push(node);
    }

    nodes[0].isLeader = true;
    const leader = nodes[0];

    await Promise.all(nodes.map(n => n.start()));

    for (let opIdx = 0; opIdx < operationCount; opIdx++) {
      const operationId = `consensus-op-${opIdx}`;
      const startTime = performance.now();

      leader.schedule({
        id: operationId,
        handler: async () => {
          // Simulate Raft log replication
          const replicatePromises = nodes.slice(1).map(node =>
            simulateNetworkLatency(1, 0.1).then(latency => ({
              nodeId: node.nodeId,
              latency,
            }))
          );

          const replicationResults = await Promise.all(replicatePromises);

          // Wait for quorum acknowledgment
          const quorumAcks = replicationResults.slice(0, Math.ceil(quorumSize / 2) - 1);
          const maxAckLatency = Math.max(...quorumAcks.map(r => r.latency));

          return { committed: true, quorumLatency: maxAckLatency };
        },
      });

      await leader.execute(operationId).catch(() => {});

      const endTime = performance.now();
      commitLatencies.push(endTime - startTime);
    }

    await Promise.all(nodes.map(n => n.stop()));
  }

  const variance = analyzeVariance(commitLatencies);

  return storeBenchmarkResult({
    name: 'raft-consensus-commit',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: commitLatencies.length,
    quorumSize,
    operationCount,
    runs,
  });
}

/**
 * Benchmark: Log replication throughput
 * @param {Object} options - Benchmark options
 * @param {number} [options.nodeCount=3] - Nodes in cluster
 * @param {number} [options.duration=5000] - Test duration in ms
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with replication throughput
 */
export async function benchmarkReplicationThroughput(options = {}) {
  const { nodeCount = 3, duration = 5000, runs = 3 } = options;
  const throughputs = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const nodes = [];

    for (let i = 0; i < nodeCount; i++) {
      const node = new Daemon({
        daemonId: generateDaemonId(),
        name: `tput-node-${i}`,
        nodeId: `node-${i}`,
        clusterId: `tput-${runIdx}`,
      });
      nodes.push(node);
    }

    nodes[0].isLeader = true;
    const leader = nodes[0];

    await Promise.all(nodes.map(n => n.start()));

    let operationCounter = 0;
    const startTime = performance.now();

    const scheduler = setInterval(() => {
      if (performance.now() - startTime < duration) {
        const opId = `tput-op-${operationCounter}`;
        leader.schedule({
          id: opId,
          handler: async () => {
            await Promise.all(
              nodes.slice(1).map(() => simulateNetworkLatency(1, 0.1))
            );
            return { replicated: true };
          },
        });
        operationCounter += 1;
      }
    }, 5);

    await new Promise(resolve => {
      setTimeout(() => {
        clearInterval(scheduler);
        resolve();
      }, duration + 500);
    });

    const actualDuration = performance.now() - startTime;
    const throughput = (operationCounter / (actualDuration / 1000));
    throughputs.push(throughput);

    await Promise.all(nodes.map(n => n.stop()));
  }

  const variance = analyzeVariance(throughputs);

  return storeBenchmarkResult({
    name: 'raft-replication-throughput',
    type: 'throughput',
    unit: 'ops/sec',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: throughputs.length,
    nodeCount,
    duration,
    runs,
  });
}
