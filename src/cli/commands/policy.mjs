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
 * Validate current store against applied policy packs
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function policyValidateCommand(ctx, config) {
  const { args } = ctx;

  const span = tracer.startSpan('policy.validate', {
    attributes: {
      'cli.command': 'policy validate',
      'strict': args.strict || false
    }
  });

  const startTime = Date.now();

  try {
    const { validatePolicy, formatValidationReport } = await import('../utils/policy-validator.mjs');
    const { Store, Parser } = await import('n3');

    // Get policy pack name (required)
    const policyPackName = getArg(args, 'policy') || getArg(args, 'pack');
    if (!policyPackName) {
      console.error('❌ Error: --policy flag is required');
      console.log('Usage: unrdf policy validate --policy <pack-name> [--store <file>] [--strict] [--format json|text|markdown]');
      process.exit(1);
    }

    // Load RDF store
    const store = new Store();
    const storeFile = getArg(args, 'store');

    if (storeFile) {
      const { readFile: readFileAsync } = await import('node:fs/promises');
      const rdfData = await readFileAsync(storeFile, 'utf-8');
      const parser = new Parser();

      const quads = parser.parse(rdfData);
      store.addQuads(quads);

      span.addEvent('store.loaded', {
        file: storeFile,
        size: store.size
      });
    } else {
      console.warn('⚠️  No store file provided, validating against empty store');
    }

    span.addEvent('validation.starting', {
      policy: policyPackName,
      storeSize: store.size
    });

    // Run validation
    const result = await validatePolicy(store, policyPackName, {
      strict: args.strict || false,
      basePath: process.cwd()
    });

    span.addEvent('validation.completed', {
      passed: result.passed,
      violations: result.violations.length
    });

    // Format and output report
    const format = getArg(args, 'format', 'text');
    const report = formatValidationReport(result, format);
    console.log(report);

    // Exit with error code if validation failed
    if (!result.passed) {
      const duration = Date.now() - startTime;
      span.setAttributes({
        'validation.passed': false,
        'validation.violations': result.violations.length,
        'validation.duration_ms': duration,
        'validation.success': true
      });
      span.end();
      process.exit(args.strict ? 1 : 0);
    }

    const duration = Date.now() - startTime;
    span.setAttributes({
      'validation.passed': true,
      'validation.violations': 0,
      'validation.duration_ms': duration,
      'validation.success': true
    });

    span.end();
  } catch (error) {
    const duration = Date.now() - startTime;
    console.error(`❌ Validation failed: ${error.message}`);

    span.setAttributes({
      'validation.success': false,
      'validation.duration_ms': duration,
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
 * View policy violation audit log
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function policyAuditCommand(ctx, config) {
  const { args } = ctx;

  const span = tracer.startSpan('policy.audit', {
    attributes: {
      'cli.command': 'policy audit',
      'violations-only': args['violations-only'] || false
    }
  });

  const startTime = Date.now();

  try {
    console.log('📋 Policy Audit Log\n');

    // Check if audit log exists
    const { readFile: readFileAsync, access } = await import('node:fs/promises');
    const { constants } = await import('node:fs');
    const { join: joinPath } = await import('node:path');

    const auditLogPath = joinPath(process.cwd(), '.unrdf', 'policy-audit.log');

    try {
      await access(auditLogPath, constants.R_OK);
    } catch {
      console.log('No audit log found. Run policy validations to generate audit history.');
      span.end();
      return;
    }

    // Read and parse audit log
    const auditData = await readFileAsync(auditLogPath, 'utf-8');
    const auditEntries = auditData
      .trim()
      .split('\n')
      .filter(line => line.trim())
      .map(line => {
        try {
          return JSON.parse(line);
        } catch {
          return null;
        }
      })
      .filter(entry => entry !== null);

    span.addEvent('audit.loaded', {
      entries: auditEntries.length
    });

    if (auditEntries.length === 0) {
      console.log('No audit entries found.');
      span.end();
      return;
    }

    // Filter for violations only if requested
    let filteredEntries = auditEntries;
    if (args['violations-only']) {
      filteredEntries = auditEntries.filter(entry => !entry.passed);
      span.addEvent('audit.filtered', {
        violations: filteredEntries.length
      });
    }

    // Format output
    const format = getArg(args, 'format', 'table');

    if (format === 'json') {
      console.log(JSON.stringify(filteredEntries, null, 2));
    } else {
      // Table format
      console.log('TIMESTAMP            POLICY                RESULT   VIOLATIONS');
      console.log('───────────────────  ────────────────────  ───────  ──────────');

      for (const entry of filteredEntries.slice(-50)) { // Last 50 entries
        const timestamp = new Date(entry.timestamp).toLocaleString().padEnd(19);
        const policy = (entry.policyPack || 'unknown').padEnd(20).substring(0, 20);
        const result = (entry.passed ? '✅ PASS' : '❌ FAIL').padEnd(7);
        const violations = `${entry.violations?.length || 0}`.padStart(10);

        console.log(`${timestamp}  ${policy}  ${result}  ${violations}`);
      }

      console.log(`\nShowing ${Math.min(filteredEntries.length, 50)} of ${filteredEntries.length} entries`);
    }

    const duration = Date.now() - startTime;
    span.setAttributes({
      'audit.entries': auditEntries.length,
      'audit.displayed': filteredEntries.length,
      'audit.duration_ms': duration,
      'audit.success': true
    });

    span.end();
  } catch (error) {
    const duration = Date.now() - startTime;
    console.error(`❌ Failed to read audit log: ${error.message}`);

    span.setAttributes({
      'audit.success': false,
      'audit.duration_ms': duration,
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
  subcommands: ['apply', 'list', 'get', 'validate', 'audit']
};
