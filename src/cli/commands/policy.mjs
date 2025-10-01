/**
 * @file Policy Commands
 * @module cli/commands/policy
 *
 * @description
 * Policy pack management with OTEL instrumentation.
 */

import { readFile } from 'node:fs/promises';
import { trace } from '@opentelemetry/api';
import { PolicyPackManager } from '../../knowledge-engine/policy-pack.mjs';
import { validateRequiredArgs, getArg } from '../utils/context-wrapper.mjs';

// Get tracer for OTEL instrumentation
const tracer = trace.getTracer('unrdf-cli-policy');

/**
 * Apply policy pack
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function policyApplyCommand(ctx, config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['file']);

  const span = tracer.startSpan('policy.apply', {
    attributes: {
      'cli.command': 'policy apply',
      'file': args.file
    }
  });

  const startTime = Date.now();

  try {
    const manager = new PolicyPackManager();

    // Read and parse the policy file
    const fileContent = await readFile(args.file, 'utf-8');
    const policyData = JSON.parse(fileContent);

    // Detect format: full policy pack vs simple policy
    let pack;
    if (policyData.id && policyData.meta && policyData.config) {
      // Full PolicyPack format - use as-is
      pack = await manager.loadPolicyPack(args.file);
    } else if (policyData.policies || policyData.hooks) {
      // Simple format - wrap it into a policy pack
      const { randomUUID } = await import('crypto');
      const wrapped = {
        id: randomUUID(),
        meta: {
          name: policyData.name || 'applied-policy',
          version: policyData.version || '1.0.0',
          description: policyData.description || 'Applied policy'
        },
        config: {
          enabled: true,
          priority: 50,
          strictMode: false,
          timeout: 30000,
          retries: 1
        },
        hooks: policyData.hooks || [],
        conditions: policyData.conditions || [],
        resources: policyData.resources || []
      };

      // Write temporary manifest
      const { writeFile: writeFileSync } = await import('node:fs/promises');
      const tmpPath = `/tmp/policy-${Date.now()}.json`;
      await writeFileSync(tmpPath, JSON.stringify(wrapped));

      pack = await manager.loadPolicyPack(tmpPath);
    } else {
      throw new Error('Invalid policy format: must have id/meta/config or policies/hooks');
    }

    span.addEvent('policy.loaded', {
      name: pack.manifest.meta.name,
      version: pack.manifest.meta.version
    });

    // Activate policy pack
    manager.activatePolicyPack(pack.manifest.meta.name);
    span.addEvent('policy.activated');

    const stats = pack.getStats();
    // Simple output for CLI compatibility
    console.log(`✅ Policy pack applied: ${stats.name}`);

    const duration = Date.now() - startTime;
    span.setAttributes({
      'policy.name': stats.name,
      'policy.version': stats.version,
      'policy.hooks': stats.hooks.total,
      'policy.duration_ms': duration,
      'policy.success': true
    });

    span.end();
  } catch (error) {
    const duration = Date.now() - startTime;
    console.error(`❌ Failed to apply policy pack: ${error.message}`);

    span.setAttributes({
      'policy.success': false,
      'policy.duration_ms': duration,
      'error.message': error.message
    });
    span.recordException(error);
    span.end();

    if (args.verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

/**
 * List active policy packs
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function policyListCommand(ctx, config) {
  const { args } = ctx;

  const span = tracer.startSpan('policy.list', {
    attributes: {
      'cli.command': 'policy list'
    }
  });

  const startTime = Date.now();

  try {
    console.log('📋 Listing policy packs...');

    const manager = new PolicyPackManager();

    // Load all policy packs
    const packs = await manager.loadAllPolicyPacks();
    span.addEvent('policies.loaded', { count: packs.length });

    if (packs.length === 0) {
      console.log('No policy packs found.');
      span.end();
      return;
    }

    // Format output
    const format = getArg(args, 'format', 'table');
    if (format === 'json') {
      const packsData = packs.map(pack => pack.getStats());
      console.log(JSON.stringify(packsData, null, 2));
    } else {
      // Table format
      console.log('\nNAME                VERSION   HOOKS   ENABLED');
      console.log('──────────────────  ────────  ──────  ───────');
      for (const pack of packs) {
        const stats = pack.getStats();
        const name = stats.name.padEnd(18);
        const version = stats.version.padEnd(8);
        const hooks = `${stats.hooks.enabled}/${stats.hooks.total}`.padEnd(6);
        const enabled = (stats.config.enabled ? '✓' : '✗').padEnd(7);
        console.log(`${name}  ${version}  ${hooks}  ${enabled}`);
      }
    }

    const duration = Date.now() - startTime;
    span.setAttributes({
      'policy.count': packs.length,
      'policy.duration_ms': duration,
      'policy.success': true
    });

    span.end();
  } catch (error) {
    const duration = Date.now() - startTime;
    console.error(`❌ Failed to list policy packs: ${error.message}`);

    span.setAttributes({
      'policy.success': false,
      'policy.duration_ms': duration,
      'error.message': error.message
    });
    span.recordException(error);
    span.end();

    if (args.verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

/**
 * Get policy pack details
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function policyGetCommand(ctx, config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['id']);

  const span = tracer.startSpan('policy.get', {
    attributes: {
      'cli.command': 'policy get',
      'policy.id': args.id
    }
  });

  const startTime = Date.now();

  try {
    console.log(`📋 Getting policy pack: ${args.id}`);

    const manager = new PolicyPackManager();
    await manager.loadAllPolicyPacks();

    const pack = manager.getPolicyPack(args.id);

    if (!pack) {
      console.error(`❌ Policy pack not found: ${args.id}`);
      span.end();
      process.exit(1);
    }

    const stats = pack.getStats();
    console.log(JSON.stringify(stats, null, 2));

    const duration = Date.now() - startTime;
    span.setAttributes({
      'policy.id': args.id,
      'policy.found': true,
      'policy.duration_ms': duration,
      'policy.success': true
    });

    span.end();
  } catch (error) {
    const duration = Date.now() - startTime;
    console.error(`❌ Failed to get policy pack: ${error.message}`);

    span.setAttributes({
      'policy.success': false,
      'policy.duration_ms': duration,
      'error.message': error.message
    });
    span.recordException(error);
    span.end();

    if (args.verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

/**
 * Export policy command metadata
 */
export const policyCommandMeta = {
  name: 'policy',
  description: 'Manage policy packs',
  subcommands: ['apply', 'list', 'get']
};
