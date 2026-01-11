/**
 * @file Config Command
 * @module cli/commands/daemon/config
 */

import { defineCommand } from 'citty';
import { ConfigArgsSchema } from './schemas.mjs';
import { formatDuration } from './helpers.mjs';

export const configCommand = defineCommand({
  meta: {
    name: 'config',
    description: 'Display current daemon configuration',
  },
  args: {
    json: {
      type: 'boolean',
      description: 'Output as JSON',
    },
  },
  async run({ args }) {
    try {
      const validated = ConfigArgsSchema.parse(args);

      const config = {
        daemonId: 'default-daemon',
        port: 8080,
        nodeId: 'node-default',
        clusterId: 'default-cluster',
        maxConcurrent: 10,
        healthCheckInterval: 30000,
        features: {
          clustering: true,
          federation: true,
          streaming: true,
          observability: true,
        },
        operationTypes: ['cron', 'interval', 'reactive', 'event', 'idle'],
        retryPolicy: {
          maxAttempts: 3,
          backoffMs: 1000,
          backoffMultiplier: 2,
          maxBackoffMs: 30000,
          jitterFactor: 0.1,
        },
        logging: {
          level: 'info',
          format: 'json',
          destination: 'stdout',
        },
      };

      if (validated.json) {
        console.log(JSON.stringify({ config }, null, 2));
      } else {
        console.log('\n⚙️  Daemon Configuration');
        console.log('═'.repeat(50));
        console.log(`Daemon ID: ${config.daemonId}`);
        console.log(`Port: ${config.port}`);
        console.log(`Node ID: ${config.nodeId}`);
        console.log(`Cluster ID: ${config.clusterId}`);
        console.log(`Max Concurrent: ${config.maxConcurrent}`);
        console.log(`Health Check Interval: ${formatDuration(config.healthCheckInterval)}`);
        console.log('═'.repeat(50));

        console.log('\n🔧 Features');
        console.log('─'.repeat(50));
        Object.entries(config.features).forEach(([feature, enabled]) => {
          console.log(`${feature.padEnd(20)} ${enabled ? '✅ Enabled' : '❌ Disabled'}`);
        });

        console.log('\n⚙️  Supported Operation Types');
        console.log('─'.repeat(50));
        config.operationTypes.forEach(type => {
          console.log(`  • ${type}`);
        });

        console.log('\n🔄 Retry Policy');
        console.log('─'.repeat(50));
        console.log(`Max Attempts: ${config.retryPolicy.maxAttempts}`);
        console.log(`Initial Backoff: ${formatDuration(config.retryPolicy.backoffMs)}`);
        console.log(`Multiplier: ${config.retryPolicy.backoffMultiplier}x`);
        console.log(`Max Backoff: ${formatDuration(config.retryPolicy.maxBackoffMs)}`);
        console.log(`Jitter Factor: ${(config.retryPolicy.jitterFactor * 100).toFixed(0)}%`);

        console.log('\n📊 Logging');
        console.log('─'.repeat(50));
        console.log(`Level: ${config.logging.level}`);
        console.log(`Format: ${config.logging.format}`);
        console.log(`Destination: ${config.logging.destination}`);
        console.log('═'.repeat(50));
      }
    } catch (error) {
      console.error(`❌ Error retrieving configuration: ${error.message}`);
      process.exit(1);
    }
  },
});
