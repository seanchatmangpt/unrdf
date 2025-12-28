#!/usr/bin/env node
/**
 * @fileoverview Scalability Benchmark - Test system scaling characteristics
 * @module benchmarks/monitoring/scalability-bench
 *
 * @description
 * Benchmarks scalability from 1-100 agents:
 * - Linear scaling (ideal)
 * - Sub-linear scaling (realistic)
 * - Scaling efficiency
 * - Resource utilization vs agents
 */

import { performance } from 'node:perf_hooks';
import { suite, runBenchmark, formatMarkdownTable } from '../framework.mjs';
import { MetricsCollector } from '../../src/monitoring/metrics-collector.mjs';

/**
 * Simulate agent work
 * @param {number} agentId - Agent identifier
 * @param {number} workUnits - Work units to process
 * @returns {Promise<number>}
 */
async function simulateAgentWork(agentId, workUnits) {
  let result = 0;

  for (let i = 0; i < workUnits; i++) {
    // CPU-bound work
    result += Math.sqrt(i * agentId);

    // Occasional I/O
    if (i % 100 === 0) {
      await new Promise(resolve => setTimeout(resolve, 1));
    }
  }

  return result;
}

/**
 * Run scalability benchmarks
 */
export async function runScalabilityBenchmarks() {
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║         Scalability Benchmark Suite                      ║');
  console.log('╚═══════════════════════════════════════════════════════════╝\n');

  const collector = new MetricsCollector({
    sampleInterval: 100,
    bufferSize: 5000,
  });

  collector.start();

  const agentCounts = [1, 2, 5, 10, 20, 50, 100];
  const workPerAgent = 1000;

  const benchmarks = {};

  for (const agentCount of agentCounts) {
    benchmarks[`${agentCount} Agent${agentCount > 1 ? 's' : ''}`] = {
      fn: async function() {
        const start = performance.now();

        // Simulate parallel agent work
        const agents = Array(agentCount).fill(0).map((_, i) => ({
          id: `α${i + 1}`,
          work: workPerAgent,
        }));

        const results = await Promise.all(
          agents.map(agent => simulateAgentWork(agent.id, agent.work))
        );

        const latency = performance.now() - start;

        // Update agent metrics
        agents.forEach((agent, i) => {
          collector.updateAgentMetrics(agent.id, {
            tasksCompleted: 1,
            tasksQueued: 0,
            cpuTime: latency * 1000, // Convert to μs
            memoryUsed: process.memoryUsage().heapUsed / agentCount,
            avgLatency: latency,
          });
        });

        collector.recordOperation(latency);

        return results.reduce((sum, r) => sum + r, 0);
      },
      iterations: 50,
      warmup: 5,
    };
  }

  const suiteRunner = suite('Scalability Benchmarks', benchmarks);
  const results = await suiteRunner();

  collector.stop();

  // Scaling analysis
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║         Scaling Analysis                                  ║');
  console.log('╚═══════════════════════════════════════════════════════════╝\n');

  console.log('Agent Count  │  Throughput  │  Latency (P95)  │  Efficiency  │  Speedup');
  console.log('─────────────┼──────────────┼─────────────────┼──────────────┼──────────');

  const baselineThroughput = results.results[0]?.throughput || 1;
  const baselineLatency = results.results[0]?.latency.p95 || 1;

  results.results.forEach((result, i) => {
    const agentCount = agentCounts[i];
    const throughput = result.throughput;
    const latency = result.latency.p95;
    const speedup = throughput / baselineThroughput;
    const efficiency = (speedup / agentCount) * 100;

    console.log(
      `${String(agentCount).padStart(11)} │ ` +
      `${throughput.toFixed(2).padStart(11)} │ ` +
      `${latency.toFixed(2).padStart(14)} │ ` +
      `${efficiency.toFixed(1).padStart(11)}% │ ` +
      `${speedup.toFixed(2)}x`
    );
  });

  // Calculate scaling efficiency
  const lastResult = results.results[results.results.length - 1];
  const maxAgents = agentCounts[agentCounts.length - 1];
  const maxSpeedup = lastResult.throughput / baselineThroughput;
  const scalingEfficiency = (maxSpeedup / maxAgents) * 100;

  console.log(`\nScaling Characteristics:`);
  console.log(`  Max Agents:          ${maxAgents}`);
  console.log(`  Max Speedup:         ${maxSpeedup.toFixed(2)}x`);
  console.log(`  Scaling Efficiency:  ${scalingEfficiency.toFixed(1)}%`);

  if (scalingEfficiency > 90) {
    console.log(`  Classification:      Near-linear scaling ✓`);
  } else if (scalingEfficiency > 70) {
    console.log(`  Classification:      Sub-linear scaling (good)`);
  } else {
    console.log(`  Classification:      Sub-linear scaling (poor)`);
  }

  // Print markdown table
  console.log('\n' + formatMarkdownTable(results));

  return {
    results,
    metrics: collector.toJSON(),
    scaling: {
      maxAgents,
      maxSpeedup,
      scalingEfficiency,
    },
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runScalabilityBenchmarks()
    .then(() => process.exit(0))
    .catch(error => {
      console.error('Benchmark failed:', error);
      process.exit(1);
    });
}
