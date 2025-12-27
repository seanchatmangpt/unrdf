#!/usr/bin/env node
/**
 * @fileoverview Example - Real-time monitoring system demonstration
 * @module monitoring/example
 *
 * @description
 * Demonstrates the complete monitoring system:
 * - MetricsCollector with real-time collection
 * - Dashboard with live ASCII display
 * - AdaptiveOptimizer with auto-tuning
 * - PrometheusExporter for metrics export
 *
 * Usage:
 *   node src/monitoring/example.mjs
 *   node src/monitoring/example.mjs --no-dashboard  # Headless mode
 *   node src/monitoring/example.mjs --prometheus-port=9091
 */

import { MetricsCollector } from './metrics-collector.mjs';
import { Dashboard } from './dashboard.mjs';
import { AdaptiveOptimizer } from './adaptive-optimizer.mjs';
import { PrometheusExporter } from './prometheus-exporter.mjs';

/**
 * Simulate workload
 * @param {MetricsCollector} collector - Metrics collector
 */
async function simulateWorkload(collector) {
  const agentIds = ['α₁', 'α₂', 'α₃', 'α₄', 'α₅', 'α₆', 'α₇', 'α₈', 'α₉', 'α₁₀'];

  let operationCount = 0;
  let receiptCount = 0;

  while (true) {
    // Simulate operations with varying latency
    const numOps = Math.floor(Math.random() * 20) + 5;

    for (let i = 0; i < numOps; i++) {
      const agentId = agentIds[Math.floor(Math.random() * agentIds.length)];
      const latency = Math.random() * 50; // 0-50ms

      // Simulate operation
      await new Promise(resolve => setTimeout(resolve, latency));

      // Record operation
      collector.recordOperation(latency);
      operationCount++;

      // Update agent metrics
      collector.updateAgentMetrics(agentId, {
        tasksCompleted: operationCount,
        tasksQueued: Math.floor(Math.random() * 10),
        cpuTime: latency * 1000, // μs
        memoryUsed: process.memoryUsage().heapUsed / 10,
        avgLatency: latency,
      });

      // Occasionally record compression
      if (Math.random() < 0.1) {
        const ratio = 2 + Math.random() * 3; // 2-5x compression
        collector.recordCompression(ratio);
      }

      // Occasionally add receipt
      if (Math.random() < 0.05) {
        receiptCount++;
        collector.updateReceiptCount(receiptCount);
      }
    }

    // Small delay between batches
    await new Promise(resolve => setTimeout(resolve, 100));
  }
}

/**
 * Main function
 */
async function main() {
  const args = process.argv.slice(2);
  const noDashboard = args.includes('--no-dashboard');
  const prometheusPort = args.find(a => a.startsWith('--prometheus-port='))
    ?.split('=')[1] || 9090;

  console.log('╔═══════════════════════════════════════════════════════════╗');
  console.log('║         UNRDF Real-time Monitoring System                ║');
  console.log('╚═══════════════════════════════════════════════════════════╝');
  console.log('');
  console.log('Starting monitoring system...');
  console.log('');

  // Create components
  const collector = new MetricsCollector({
    sampleInterval: 1000,
    bufferSize: 3600,
  });

  const dashboard = new Dashboard(collector, {
    refreshRate: 1000,
    width: 120,
    height: 40,
  });

  const optimizer = new AdaptiveOptimizer(collector, {
    evaluationInterval: 10000,
    minCooldown: 30000,
  });

  const exporter = new PrometheusExporter(collector, {
    port: prometheusPort,
    host: '0.0.0.0',
    path: '/metrics',
    namespace: 'unrdf',
  });

  // Start components
  collector.start();
  optimizer.start();
  exporter.start();

  console.log(`✓ Metrics collector started (interval: 1s)`);
  console.log(`✓ Adaptive optimizer started (evaluation: 10s)`);
  console.log(`✓ Prometheus exporter started (http://0.0.0.0:${prometheusPort}/metrics)`);

  if (!noDashboard) {
    console.log(`✓ Dashboard starting...`);
    console.log('');
    await new Promise(resolve => setTimeout(resolve, 1000));
    dashboard.start();
  } else {
    console.log(`✓ Running in headless mode (--no-dashboard)`);
    console.log('');
    console.log('Monitor metrics at:');
    console.log(`  Prometheus: http://localhost:${prometheusPort}/metrics`);
    console.log('');
    console.log('Press Ctrl+C to exit');
    console.log('');
  }

  // Start workload simulation
  simulateWorkload(collector).catch(error => {
    console.error('Workload simulation error:', error);
  });

  // Graceful shutdown
  process.on('SIGINT', () => {
    console.log('\n\nShutting down...');
    dashboard.stop();
    optimizer.stop();
    exporter.stop();
    collector.stop();
    process.exit(0);
  });

  // In headless mode, show periodic stats
  if (noDashboard) {
    setInterval(() => {
      const snapshot = collector.getCurrentSnapshot();
      if (snapshot) {
        console.log('\n─── Metrics Update ───');
        console.log(`Time:        ${new Date().toISOString()}`);
        console.log(`Throughput:  ${snapshot.throughput.opsPerSec.toFixed(2)} ops/sec`);
        console.log(`Latency P95: ${snapshot.latency.p95.toFixed(2)} ms`);
        console.log(`CPU:         ${(snapshot.cpu.percentageUser + snapshot.cpu.percentageSystem).toFixed(1)}%`);
        console.log(`Memory:      ${(snapshot.memory.heapUsed / 1024 / 1024).toFixed(1)} MB`);

        const params = optimizer.getParameters();
        console.log(`\nOptimizer Parameters:`);
        console.log(`  Epsilon:     ${params.epsilon.toFixed(4)}`);
        console.log(`  Budget:      ${params.budget}`);
        console.log(`  Agents:      ${params.agentCount}`);
        console.log(`  Batch Size:  ${params.batchSize}`);
      }
    }, 5000);
  }
}

// Run
main().catch(error => {
  console.error('Fatal error:', error);
  process.exit(1);
});
