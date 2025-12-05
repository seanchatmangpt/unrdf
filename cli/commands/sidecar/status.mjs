/**
 * @file Sidecar Status Command - Show sidecar health and metrics
 * @module cli-v2/commands/sidecar/status
 */

import { defineCommand } from 'citty';

// Mock sidecar status (would connect to real sidecar in production)
const mockSidecarStatus = {
  version: '2.1.0',
  status: 'HEALTHY',
  address: 'http://localhost:50051',
  uptime_seconds: 345600, // 4 days
  connected: true,
  startTime: '2025-12-01T10:30:00Z',
  lastHealthCheck: '2025-12-05T16:05:00Z',
  metrics: {
    requests: 12450,
    successes: 12401,
    failures: 49,
    avgLatency_ms: 2.3,
    errorRate: 0.39
  },
  connections: {
    active: 5,
    total: 128
  },
  memory: {
    used_mb: 245,
    available_mb: 1024
  },
  grpc: {
    port: 50051,
    status: 'SERVING',
    reflectionEnabled: true
  }
};

export const statusCommand = defineCommand({
  meta: {
    name: 'status',
    description: 'Get sidecar connection status and metrics'
  },
  args: {
    verbose: {
      type: 'boolean',
      description: 'Show detailed metrics',
      default: false,
      alias: 'v'
    }
  },
  async run(ctx) {
    const { verbose } = ctx.args;
    const status = mockSidecarStatus;

    try {
      // Determine health color/emoji
      const healthIcon = status.status === 'HEALTHY' ? '✅' : '⚠️';
      const healthColor = status.status === 'HEALTHY' ? 'green' : 'yellow';

      // Display header
      console.log(`\n🔌 Sidecar Status`);
      console.log(`${'═'.repeat(50)}`);
      console.log(`${healthIcon} Status:       ${status.status}`);
      console.log(`Version:       ${status.version}`);
      console.log(`Address:       ${status.address}`);
      console.log(`Uptime:        ${(status.uptime_seconds / 3600 / 24).toFixed(1)} days`);
      console.log(`Started:       ${new Date(status.startTime).toLocaleString()}`);
      console.log(`Last Check:    ${new Date(status.lastHealthCheck).toLocaleString()}`);

      // Display connection info
      console.log(`\n🔗 Connections:`);
      console.log(`   Active:     ${status.connections.active}`);
      console.log(`   Total:      ${status.connections.total}`);

      // Display metrics
      console.log(`\n📊 Request Metrics:`);
      console.log(`   Total:      ${status.metrics.requests.toLocaleString()}`);
      console.log(`   Successful: ${status.metrics.successes.toLocaleString()} (${((status.metrics.successes / status.metrics.requests) * 100).toFixed(2)}%)`);
      console.log(`   Failed:     ${status.metrics.failures} (${status.metrics.errorRate.toFixed(2)}%)`);
      console.log(`   Avg Latency: ${status.metrics.avgLatency_ms.toFixed(1)}ms`);

      // Display resource usage
      if (verbose) {
        console.log(`\n💾 Resources:`);
        console.log(`   Memory:     ${status.memory.used_mb}MB / ${status.memory.available_mb}MB`);
        const memPercent = ((status.memory.used_mb / status.memory.available_mb) * 100).toFixed(1);
        console.log(`   Usage:      ${memPercent}%`);

        console.log(`\n🚀 gRPC Server:`);
        console.log(`   Port:       ${status.grpc.port}`);
        console.log(`   Status:     ${status.grpc.status === 'SERVING' ? '✅' : '❌'} ${status.grpc.status}`);
        console.log(`   Reflection: ${status.grpc.reflectionEnabled ? 'Enabled' : 'Disabled'}`);

        // Health recommendation
        console.log(`\n💡 Recommendations:`);
        if (status.metrics.errorRate > 1) {
          console.log(`   ⚠️  Error rate is elevated (${status.metrics.errorRate.toFixed(2)}%)`);
          console.log(`       Consider checking logs: unrdf sidecar logs`);
        }
        if (memPercent > 80) {
          console.log(`   ⚠️  Memory usage is high (${memPercent}%)`);
          console.log(`       Consider restarting: unrdf sidecar restart`);
        }
        if (status.status === 'HEALTHY') {
          console.log(`   ✅ Sidecar is healthy and responding normally`);
        }
      }

      console.log('');
    } catch (error) {
      console.error(`\n❌ Failed to get sidecar status: ${error.message}`);
      console.error(`\n📋 Check:`);
      console.error(`   • Is sidecar running? (unrdf sidecar health)`);
      console.error(`   • Is address correct? (check config)`);
      console.error(`   • Network connectivity? (can reach localhost:50051)`);
      console.error('');
      process.exit(1);
    }
  }
});

export default statusCommand;
