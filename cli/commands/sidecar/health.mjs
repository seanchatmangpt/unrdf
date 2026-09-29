/**
 * @file Sidecar Health Command
 * @module cli-v2/commands/sidecar/health
 *
 * @description
 * Detailed health check for KGC sidecar with circuit breaker diagnostics,
 * connection pool status, retry metrics, and health monitoring state.
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../../src/domain/formatters/index.mjs';
import { createSidecarClient } from '../../../src/sidecar/client.mjs';

/**
 * Format health status with emoji indicators
 * @param {string} status - Health status
 * @returns {string} Formatted status
 */
function formatHealthStatus(status) {
  const statusMap = {
    SERVING: '✅ SERVING',
    NOT_SERVING: '❌ NOT_SERVING',
    SERVICE_UNKNOWN: '⚠️ SERVICE_UNKNOWN',
    UNKNOWN: '❓ UNKNOWN',
    HEALTHY: '✅ HEALTHY',
    UNHEALTHY: '❌ UNHEALTHY',
    DEGRADED: '⚠️ DEGRADED'
  };

  return statusMap[status] || `❓ ${status}`;
}

/**
 * Sidecar health command
 */
export const healthCommand = defineCommand({
  meta: {
    name: 'health',
    description: 'Detailed health check for KGC sidecar'
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
    watch: {
      type: 'boolean',
      description: 'Continuously monitor health',
      default: false,
      alias: 'w'
    },
    interval: {
      type: 'string',
      description: 'Watch interval in seconds',
      default: '5'
    }
  },
  async run(ctx) {
    const client = createSidecarClient({
      enableHealthCheck: true
    });

    const performHealthCheck = async () => {
      try {
        // Connect to sidecar
        if (!client.connected) {
          await client.connect(ctx.args.address);
        }

        // Get health check response
        const healthResponse = await client.healthCheck();

        // Get client metrics
        const clientMetrics = client.getClientMetrics();

        // Build comprehensive health report
        const health = {
          service: {
            status: formatHealthStatus(healthResponse.status),
            uptime_seconds: healthResponse.uptime_seconds,
            details: healthResponse.details || {}
          },
          connection: {
            address: client.config.getAddress(),
            connected: client.connected
          },
          circuit_breaker: {
            state: clientMetrics.circuitBreaker?.state || 'UNKNOWN',
            failures: clientMetrics.circuitBreaker?.failures || 0,
            successes: clientMetrics.circuitBreaker?.successes || 0,
            half_open_successes: clientMetrics.circuitBreaker?.halfOpenSuccesses || 0
          },
          connection_pool: {
            active_connections: clientMetrics.connectionPool?.active || 0,
            available_connections: clientMetrics.connectionPool?.available || 0,
            total_connections: clientMetrics.connectionPool?.total || 0,
            pending_requests: clientMetrics.connectionPool?.pending || 0
          },
          retry_strategy: {
            total_retries: clientMetrics.retryStrategy?.totalRetries || 0,
            retry_rate: clientMetrics.retryStrategy?.retryRate || 0
          },
          request_metrics: {
            total_requests: clientMetrics.requests,
            successful_requests: clientMetrics.successes,
            failed_requests: clientMetrics.failures,
            success_rate: clientMetrics.requests > 0
              ? ((clientMetrics.successes / clientMetrics.requests) * 100).toFixed(2) + '%'
              : '0%'
          },
          health_monitor: clientMetrics.health || {}
        };

        // Format and output
        const output = formatOutput(health, ctx.args.output);
        console.log(output);

        // Return true if healthy
        return healthResponse.status === 'SERVING';
      } catch (error) {
        console.error(`❌ Health check failed: ${error.message}`);

        // Output error details
        const errorHealth = {
          service: {
            status: '❌ UNAVAILABLE',
            error: error.message
          },
          connection: {
            address: ctx.args.address || client.config.getAddress(),
            connected: false
          }
        };

        const output = formatOutput(errorHealth, ctx.args.output);
        console.log(output);

        return false;
      }
    };

    // Initial health check
    const isHealthy = await performHealthCheck();

    // Watch mode - continuously monitor
    if (ctx.args.watch) {
      const intervalMs = parseInt(ctx.args.interval, 10) * 1000;

      console.log(`\n⏱️ Monitoring health every ${ctx.args.interval}s (Ctrl+C to stop)...\n`);

      const intervalId = setInterval(async () => {
        console.log('---');
        await performHealthCheck();
      }, intervalMs);

      // Handle Ctrl+C gracefully
      process.on('SIGINT', async () => {
        clearInterval(intervalId);
        console.log('\n🛑 Health monitoring stopped');
        await client.disconnect();
        process.exit(0);
      });
    } else {
      // Disconnect after single check
      await client.disconnect();

      // Exit with appropriate code
      process.exit(isHealthy ? 0 : 1);
    }
  }
});

export default healthCommand;
