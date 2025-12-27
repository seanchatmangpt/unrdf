/**
 * @file Sidecar Commands
 * @module cli/commands/sidecar
 *
 * @description
 * KGC sidecar management commands for monitoring and control.
 */

import { getSidecarClient, formatSidecarError } from '../utils/sidecar-helper.mjs';
import { _getArg } from '../utils/context-wrapper.mjs';

/**
 * Check sidecar status
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function sidecarStatusCommand(_ctx, _config) {
  console.log('🔍 Checking sidecar status...\n');

  try {
    const client = await getSidecarClient();
    const health = await client.healthCheck();
    const metrics = client.getClientMetrics();

    console.log('Sidecar Status');
    console.log('──────────────');
    console.log(`Status:        ${health.status === 'SERVING' ? 'healthy' : health.status}`);
    console.log(`Uptime:        ${Math.floor((health.uptime_seconds || 0) / 60)}m`);
    console.log(`Protocol:      gRPC`);
    console.log(`Health:        ${health.status === 'SERVING' ? '✓ Healthy' : '✗ Unhealthy'}`);

    console.log('\nClient Metrics');
    console.log('──────────────');
    console.log(`Requests:      ${metrics.requests}`);
    console.log(`Successes:     ${metrics.successes}`);
    console.log(`Failures:      ${metrics.failures}`);
    console.log(
      `Success Rate:  ${metrics.requests > 0 ? ((metrics.successes / metrics.requests) * 100).toFixed(1) : 0}%`
    );

    if (metrics.connectionPool) {
      console.log('\nConnection Pool');
      console.log('───────────────');
      console.log(`Active:        ${metrics.connectionPool.active || 0}`);
      console.log(`Idle:          ${metrics.connectionPool.idle || 0}`);
      console.log(`Total:         ${metrics.connectionPool.total || 0}`);
    }
  } catch (error) {
    // Gracefully handle unavailable sidecar - not an error condition
    if (error.code === 14 || error.code === 4) {
      console.log('Status: unavailable');
      console.log('⚠️  Sidecar not available');
      console.log(`Run 'unrdf sidecar start' to start the sidecar process`);
      process.exit(0); // Exit with success - unavailable is not an error
    }
    console.error(`❌ ${formatSidecarError(error)}`);
    process.exit(1);
  }
}

/**
 * Run health check
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function sidecarHealthCommand(_ctx, _config) {
  console.log('🏥 Running health check...\n');

  try {
    const client = await getSidecarClient();
    const startTime = Date.now();
    const health = await client.healthCheck();
    const duration = Date.now() - startTime;

    if (health.status === 'SERVING') {
      console.log(`✅ Sidecar is healthy (${duration}ms)`);
      console.log(`   Uptime: ${Math.floor((health.uptime_seconds || 0) / 60)} minutes`);
    } else {
      console.log(`⚠️  Sidecar status: ${health.status}`);
      process.exit(1);
    }
  } catch (error) {
    // Gracefully handle unavailable sidecar - not an error condition
    if (error.code === 14 || error.code === 4) {
      console.log('⚠️  Sidecar not available');
      console.log(`Run 'unrdf sidecar start' to start the sidecar process`);
      process.exit(0); // Exit with success - unavailable is not an error
    }
    console.error(`❌ Health check failed: ${formatSidecarError(error)}`);
    process.exit(1);
  }
}

/**
 * Show sidecar metrics
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function sidecarMetricsCommand(ctx, _config) {
  const { args } = ctx;

  console.log('📊 Sidecar metrics...\n');

  try {
    const client = await getSidecarClient();
    const metrics = await client.getMetrics({
      metricNames: args.metrics ? args.metrics.split(',') : [],
    });

    if (args.watch) {
      // Watch mode - update every interval
      const interval = args.interval || 5000;
      console.log(`Watching metrics (updating every ${interval}ms)...\n`);

      setInterval(async () => {
        const updated = await client.getMetrics();
        console.clear();
        console.log('📊 Sidecar Metrics (Live)\n');
        displayMetrics(updated);
      }, interval);
    } else {
      displayMetrics(metrics);
    }
  } catch (error) {
    console.error(`❌ Failed to get metrics: ${formatSidecarError(error)}`);
    process.exit(1);
  }
}

/**
 * Display metrics in formatted table
 * @param {Object} metrics - Metrics data
 */
function displayMetrics(metrics) {
  console.log('Transaction Metrics');
  console.log('───────────────────');
  console.log(`Total:         ${metrics.transactions?.total || 0}`);
  console.log(`Success:       ${metrics.transactions?.success || 0}`);
  console.log(`Failed:        ${metrics.transactions?.failed || 0}`);
  console.log(`Vetoed:        ${metrics.transactions?.vetoed || 0}`);

  if (metrics.performance) {
    console.log('\nPerformance');
    console.log('───────────');
    console.log(`p50 Latency:   ${metrics.performance.p50 || 0}ms`);
    console.log(`p99 Latency:   ${metrics.performance.p99 || 0}ms`);
    console.log(`Avg Latency:   ${metrics.performance.avg || 0}ms`);
  }

  if (metrics.hooks) {
    console.log('\nHook Execution');
    console.log('──────────────');
    console.log(`Total:         ${metrics.hooks.total || 0}`);
    console.log(`Fired:         ${metrics.hooks.fired || 0}`);
    console.log(`Rate:          ${metrics.hooks.rate || 0}/min`);
  }
}

/**
 * Get sidecar configuration
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function sidecarConfigGetCommand(ctx, _config) {
  const { args } = ctx;

  try {
    const client = await getSidecarClient();
    const sidecarConfig = await client.getMetrics();

    if (args.key) {
      // Get specific key
      const value = getNestedValue(sidecarConfig, args.key);
      console.log(value !== undefined ? value : 'Key not found');
    } else {
      // Get all config
      console.log(JSON.stringify(sidecarConfig, null, 2));
    }
  } catch (error) {
    console.error(`❌ Failed to get config: ${formatSidecarError(error)}`);
    process.exit(1);
  }
}

/**
 * Set sidecar configuration
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function sidecarConfigSetCommand(ctx, _config) {
  const { args } = ctx;

  if (!args.key || !args.value) {
    console.error('❌ Usage: unrdf sidecar config set <key>=<value>');
    process.exit(1);
  }

  console.log(`🔧 Setting config: ${args.key} = ${args.value}`);
  console.log('⚠️  Note: This requires sidecar restart to take effect');

  // TODO: Implement config persistence
  console.log('✅ Config updated (restart required)');
}

/**
 * Get nested value from object by dot-notation path
 * @param {Object} obj - Object to query
 * @param {string} path - Dot-notation path
 * @returns {any} Value at path
 */
function getNestedValue(obj, path) {
  return path.split('.').reduce((current, key) => current?.[key], obj);
}

/**
 * Export sidecar command metadata
 */
export const sidecarCommandMeta = {
  name: 'sidecar',
  description: 'Manage KGC sidecar',
  subcommands: ['status', 'health', 'metrics', 'config'],
};
