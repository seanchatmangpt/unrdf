/**
 * @file Hook Commands
 * @module cli/commands/hook
 *
 * @description
 * Knowledge hook lifecycle management commands with KGC sidecar integration.
 */

import { readFile, writeFile } from 'node:fs/promises';
import { KnowledgeHookManager } from '../../knowledge-engine/knowledge-hook-manager.mjs';
import { useStoreContext } from '../../context/index.mjs';
import { useTurtle } from '../../composables/index.mjs';
import { validateRequiredArgs, getArg } from '../utils/context-wrapper.mjs';
import { withSidecar, formatSidecarError } from '../utils/sidecar-helper.mjs';
import { evaluateHook } from '../utils/hook-evaluator.mjs';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { printTraceInfo } from '../utils/otel-tracer.mjs';

const tracer = trace.getTracer('unrdf-cli-hook');

/**
 * Evaluate knowledge hook
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function hookEvalCommand(ctx, config) {
  return await tracer.startActiveSpan('hook.eval', async (span) => {
    try {
      const { args } = ctx;
      validateRequiredArgs(args, ['hook']);

      span.setAttribute('hook.file', args.hook);
      console.log(`🔍 Evaluating hook: ${args.hook}`);

      // Load hook definition
      let hookDef;
      try {
        const hookJson = await readFile(args.hook, 'utf-8');
        hookDef = JSON.parse(hookJson);
        span.setAttribute('hook.name', hookDef.meta?.name || 'unnamed');
        span.setAttribute('hook.kind', hookDef.when?.kind || 'unknown');
      } catch (error) {
        throw new Error(`Failed to load hook: ${error.message}`);
      }

      // Load data if provided
      const store = useStoreContext();
      if (args.data) {
        const turtle = await useTurtle();
        const dataContent = await readFile(args.data, 'utf-8');
        const quads = await turtle.parse(dataContent);
        store.addQuads(quads);
        console.log(`📊 Loaded ${quads.length} triples from ${args.data}`);
        span.setAttribute('data.triples', quads.length);
      }

      span.setAttribute('store.size', store.size);

      // Use local hook evaluator with OTEL spans
      const result = await evaluateHook(hookDef, store, {
        verbose: args.verbose,
        basePath: process.cwd()
      });

      // Format output
      const format = getArg(args, 'format', 'table');
      if (format === 'json') {
        console.log(JSON.stringify(result, null, 2));
      } else {
        console.log(`\n🔥 Result: ${result.fired ? '✅ FIRED' : '❌ NOT FIRED'}`);
        console.log(`   Type: ${result.type || 'unknown'}`);
        console.log(`   Duration: ${result.executionTime || 0}ms`);

        if (result.type === 'threshold') {
          console.log(`   Value: ${result.value} ${result.operator} ${result.threshold}`);
        }

        if (result.type === 'shacl' && result.violations) {
          console.log(`   Violations: ${result.violations.length}`);
        }
      }

      // Write output if requested
      if (args.output) {
        await writeFile(args.output, JSON.stringify(result, null, 2));
        console.log(`\n📄 Result written to ${args.output}`);
      }

      span.setAttribute('hook.fired', result.fired);
      span.setAttribute('hook.executionTime', result.executionTime);
      span.setStatus({ code: SpanStatusCode.OK });

      // Print trace information
      printTraceInfo('hook eval');
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      console.error(`❌ Hook evaluation failed: ${error.message}`);
      if (ctx.args.verbose) {
        console.error(error.stack);
      }
      process.exit(1);
    } finally {
      span.end();
    }
  });
}

/**
 * List knowledge hooks
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function hookListCommand(ctx, config) {
  const { args } = ctx;

  console.log('📋 Listing knowledge hooks...');

  try {
    const hooks = await withSidecar(async (client) => {
      // Query for hooks from sidecar
      const response = await client.queryPolicy({
        policyPack: args.policyPack || 'default',
        queryType: 'hooks'
      });
      return response.hooks || [];
    });

    if (hooks.length === 0) {
      console.log('No hooks found.');
      return;
    }

    // Format output
    const format = getArg(args, 'format', 'table');
    if (format === 'json') {
      console.log(JSON.stringify(hooks, null, 2));
    } else {
      // Table format
      console.log('\nNAME                TYPE          ENABLED   PRIORITY');
      console.log('──────────────────  ────────────  ────────  ────────');
      for (const hook of hooks) {
        const name = (hook.name || 'unnamed').padEnd(18);
        const type = (hook.kind || 'unknown').padEnd(12);
        const enabled = (hook.enabled ? '✓' : '✗').padEnd(8);
        const priority = (hook.priority || 0).toString().padEnd(8);
        console.log(`${name}  ${type}  ${enabled}  ${priority}`);
      }
    }
  } catch (error) {
    console.error(`❌ Failed to list hooks: ${formatSidecarError(error)}`);
    process.exit(1);
  }
}

/**
 * Create knowledge hook from SPARQL query file or inline query
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function hookCreateCommand(ctx, config) {
  const { args } = ctx;

  // Extract positional arguments: name is first, type is second
  const positionalArgs = args._.slice(2); // Skip 'hook' and 'create'
  const name = positionalArgs[0] || args.name;
  const type = positionalArgs[1] || args.type;

  if (!name || !type) {
    console.error('❌ Usage: hook create <name> <type> [options]');
    console.error('   Example: hook create health-check sparql-ask --file=test.rq');
    console.error('   Available types: sparql-ask, threshold, shacl');
    process.exit(1);
  }

  console.log(`🔨 Creating hook: ${name} (${type})`);

  // Read SPARQL query from file or use inline query
  let queryContent;
  if (args.file) {
    try {
      queryContent = await readFile(args.file, 'utf-8');
      console.log(`📖 Loaded SPARQL query from: ${args.file}`);
    } catch (error) {
      console.error(`❌ Failed to read SPARQL file: ${error.message}`);
      process.exit(1);
    }
  } else if (args.query) {
    queryContent = args.query;
  } else {
    // Provide default queries based on type
    const defaultQueries = {
      'sparql-ask': 'ASK { ?s ?p ?o }',
      'threshold': 'SELECT (COUNT(?s) AS ?count) WHERE { ?s ?p ?o }'
    };
    queryContent = defaultQueries[type] || 'ASK { ?s ?p ?o }';
    console.warn(`⚠️  No --file or --query provided, using default query`);
  }

  // Generate hook definition based on type
  let hookDefinition;

  switch (type) {
    case 'sparql-ask':
      hookDefinition = {
        name: name,
        kind: args.phase || 'before',
        condition: {
          type: 'sparql-ask',
          query: queryContent
        },
        effects: []
      };
      break;

    case 'threshold':
      hookDefinition = {
        name: name,
        kind: args.phase || 'before',
        condition: {
          type: 'threshold',
          query: queryContent,
          threshold: args.threshold || 1000,
          operator: args.operator || 'gt'
        },
        effects: []
      };
      break;

    case 'shacl':
      hookDefinition = {
        name: name,
        kind: args.phase || 'before',
        condition: {
          type: 'shacl',
          shapesFile: args.shapes || args.file || 'shapes.ttl'
        },
        effects: []
      };
      break;

    default:
      console.error(`❌ Unknown hook type: ${type}`);
      console.log(`Available types: sparql-ask, threshold, shacl`);
      process.exit(1);
  }

  // Add description if provided
  if (args.description) {
    hookDefinition.description = args.description;
  }

  // Write hook file
  let outputPath = args.output || `hooks/${name}.json`;

  // If output is a directory, create subdirectory structure
  if (args.output) {
    const { stat } = await import('node:fs/promises');
    try {
      const stats = await stat(args.output);
      if (stats.isDirectory()) {
        // Create hooks subdirectory and name-specific directory
        const { join } = await import('node:path');
        const { mkdir } = await import('node:fs/promises');
        const hookDir = join(args.output, 'hooks', name);
        await mkdir(hookDir, { recursive: true });
        outputPath = join(hookDir, `${name}.json`);
      }
    } catch (error) {
      // If path doesn't exist, treat as file path
      if (error.code !== 'ENOENT') throw error;
    }
  }

  await writeFile(outputPath, JSON.stringify(hookDefinition, null, 2));
  console.log(`✅ Hook created: ${outputPath}`);

  // Display hook definition in verbose mode
  if (args.verbose) {
    console.log('\n📋 Hook Definition:');
    console.log(JSON.stringify(hookDefinition, null, 2));
  }
}

/**
 * Get hook details
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function hookGetCommand(ctx, config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['id']);

  try {
    const hook = await withSidecar(async (client) => {
      const response = await client.queryPolicy({
        policyPack: 'default',
        queryType: 'hook',
        filters: { hookId: args.id }
      });
      return response.hook;
    });

    if (!hook) {
      console.error(`❌ Hook not found: ${args.id}`);
      process.exit(1);
    }

    console.log(JSON.stringify(hook, null, 2));
  } catch (error) {
    console.error(`❌ Failed to get hook: ${formatSidecarError(error)}`);
    process.exit(1);
  }
}

/**
 * Show hook execution history
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function hookHistoryCommand(ctx, config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['id']);

  console.log(`📜 Hook execution history: ${args.id}`);

  try {
    const history = await withSidecar(async (client) => {
      const response = await client.queryPolicy({
        policyPack: 'default',
        queryType: 'history',
        filters: {
          hookId: args.id,
          limit: args.limit || 100,
          firedOnly: args.firedOnly || false
        }
      });
      return response.history || [];
    });

    if (history.length === 0) {
      console.log('No execution history found.');
      return;
    }

    // Table format
    console.log('\nTIMESTAMP            FIRED   DURATION   EFFECTS');
    console.log('───────────────────  ──────  ─────────  ───────');
    for (const entry of history) {
      const timestamp = new Date(entry.timestamp).toISOString().substring(0, 19);
      const fired = (entry.fired ? '✓' : '✗').padEnd(6);
      const duration = `${entry.duration || 0}ms`.padEnd(9);
      const effects = (entry.effects || 0).toString();
      console.log(`${timestamp}  ${fired}  ${duration}  ${effects}`);
    }
  } catch (error) {
    console.error(`❌ Failed to get history: ${formatSidecarError(error)}`);
    process.exit(1);
  }
}

/**
 * Export hook command metadata
 */
export const hookCommandMeta = {
  name: 'hook',
  description: 'Manage knowledge hooks',
  subcommands: ['eval', 'list', 'create', 'get', 'history']
};
