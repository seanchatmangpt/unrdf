/**
 * @file Sidecar Logs Command
 * @module cli-v2/commands/sidecar/logs
 *
 * @description
 * Stream logs and metrics from KGC sidecar using GetMetrics RPC.
 * Provides real-time monitoring of sidecar operations and performance.
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../formatters/index.mjs';
import { createSidecarClient } from '../../../sidecar/client.mjs';

/**
 * Format metric value for display
 * @param {Object} metricValue - Metric value from proto
 * @returns {string} Formatted value
 */
function formatMetricValue(metricValue) {
  if (metricValue.int_value !== undefined) {
    return `${metricValue.int_value} ${metricValue.unit || ''}`.trim();
  }
  if (metricValue.double_value !== undefined) {
    return `${metricValue.double_value.toFixed(2)} ${metricValue.unit || ''}`.trim();
  }
  if (metricValue.string_value !== undefined) {
    return metricValue.string_value;
  }
  return 'N/A';
}

/**
 * Sidecar logs command
 */
export const logsCommand = defineCommand({
  meta: {
    name: 'logs',
    description: 'Stream logs and metrics from KGC sidecar'
  },
  args: {
    follow: {
      type: 'boolean',
      description: 'Follow log output (stream mode)',
      default: false,
      alias: 'f'
    },
    tail: {
      type: 'string',
      description: 'Number of lines to show from the end',
      default: '100'
    },
    interval: {
      type: 'string',
      description: 'Poll interval in seconds (follow mode)',
      default: '2'
    },
    metrics: {
      type: 'string',
      description: 'Comma-separated list of metric names to fetch',
      alias: 'm'
    },
    since: {
      type: 'string',
      description: 'Show logs since timestamp (Unix timestamp)',
      alias: 's'
    },
    output: {
      type: 'string',
      description: 'Output format (json, yaml, table)',
      default: 'table'
    },
    address: {
      type: 'string',
      description: 'Sidecar address (overrides config)',
      alias: 'a'
    }
  },
  async run(ctx) {
    const client = createSidecarClient();
    let lastTimestamp = ctx.args.since ? parseInt(ctx.args.since, 10) : 0;

    try {
      // Connect to sidecar
      await client.connect(ctx.args.address);

      const fetchMetrics = async () => {
        try {
          // Parse metric names if provided
          const metricNames = ctx.args.metrics
            ? ctx.args.metrics.split(',').map(n => n.trim())
            : [];

          // Get metrics from sidecar
          const metricsResponse = await client.getMetrics({
            metricNames,
            sinceTimestamp: lastTimestamp
          });

          // Update last timestamp
          lastTimestamp = metricsResponse.timestamp;

          // Format metrics for display
          const metricsData = [];
          for (const [name, value] of Object.entries(metricsResponse.metrics || {})) {
            metricsData.push({
              timestamp: new Date(metricsResponse.timestamp).toISOString(),
              metric: name,
              value: formatMetricValue(value)
            });
          }

          // Limit to tail count if not following
          if (!ctx.args.follow) {
            const tailCount = parseInt(ctx.args.tail, 10);
            metricsData.splice(0, Math.max(0, metricsData.length - tailCount));
          }

          if (metricsData.length > 0) {
            const output = formatOutput(metricsData, ctx.args.output, {
              columns: ['timestamp', 'metric', 'value'],
              headers: ['TIMESTAMP', 'METRIC', 'VALUE']
            });

            console.log(output);
          } else {
            console.log('No new metrics available');
          }

          return metricsData.length > 0;
        } catch (error) {
          console.error(`Failed to fetch metrics: ${error.message}`);
          return false;
        }
      };

      // Initial fetch
      await fetchMetrics();

      // Follow mode - continuously poll for new metrics
      if (ctx.args.follow) {
        const intervalMs = parseInt(ctx.args.interval, 10) * 1000;

        console.log(`\n⏱️ Following logs every ${ctx.args.interval}s (Ctrl+C to stop)...\n`);

        const intervalId = setInterval(async () => {
          await fetchMetrics();
        }, intervalMs);

        // Handle Ctrl+C gracefully
        process.on('SIGINT', async () => {
          clearInterval(intervalId);
          console.log('\n🛑 Log streaming stopped');
          await client.disconnect();
          process.exit(0);
        });
      } else {
        // Disconnect after single fetch
        await client.disconnect();
      }
    } catch (error) {
      console.error(`Failed to connect to sidecar: ${error.message}`);
      await client.disconnect();
      process.exit(1);
    }
  }
});

export default logsCommand;
