/**
 * @file Sidecar Status Command
 * @module cli-v2/commands/sidecar/status
 *
 * @description
 * Get KGC sidecar connection status, version, and uptime using gRPC client.
 * Provides comprehensive diagnostics including circuit breaker state and metrics.
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../formatters/index.mjs';
import { createSidecarClient } from '../../../sidecar/client.mjs';

/**
 * Sidecar status command
 */
export const statusCommand = defineCommand({
  meta: {
    name: 'status',
    description: 'Get KGC sidecar connection status and metrics'
  },
  args: {
    output: {
      type: 'string',
      description: 'Output format (json, yaml, table)',
      default: 'table'
    },
    address: {
      type: 'string',
      description: 'Sidecar address (overrides config)',
      alias: 'a'
    },
    verbose: {
      type: 'boolean',
      description: 'Show detailed metrics',
      default: false,
      alias: 'v'
    }
  },
  async run(ctx) {
    const client = createSidecarClient();

    try {
      // Connect to sidecar
      await client.connect(ctx.args.address);

      // Perform health check
      const healthResponse = await client.healthCheck();

      // Get client metrics
      const clientMetrics = client.getClientMetrics();

      // Build status object
      const status = {
        connected: true,
        status: healthResponse.status,
        address: client.config.getAddress(),
        uptime_seconds: healthResponse.uptime_seconds,
        details: healthResponse.details || {},
        metrics: ctx.args.verbose ? clientMetrics : {
          requests: clientMetrics.requests,
          successes: clientMetrics.successes,
          failures: clientMetrics.failures
        }
      };

      // Format and output
      const output = formatOutput(status, ctx.args.output, {
        columns: ctx.args.verbose
          ? ['connected', 'status', 'address', 'uptime_seconds', 'metrics']
          : ['connected', 'status', 'address', 'uptime_seconds'],
        headers: ctx.args.verbose
          ? ['CONNECTED', 'STATUS', 'ADDRESS', 'UPTIME (s)', 'METRICS']
          : ['CONNECTED', 'STATUS', 'ADDRESS', 'UPTIME (s)']
      });

      console.log(output);

      // Disconnect
      await client.disconnect();
    } catch (error) {
      console.error(`Failed to get sidecar status: ${error.message}`);

      // Output error status
      const errorStatus = {
        connected: false,
        status: 'UNAVAILABLE',
        address: ctx.args.address || client.config.getAddress(),
        error: error.message
      };

      const output = formatOutput(errorStatus, ctx.args.output);
      console.log(output);

      process.exit(1);
    }
  }
});

export default statusCommand;
