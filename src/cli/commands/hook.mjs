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

/**
 * Evaluate knowledge hook
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function hookEvalCommand(ctx, config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['hook']);

  console.log(`🔍 Evaluating hook: ${args.hook}`);

  try {
    // Load hook definition
    let hookDef;
    try {
      const hookJson = await readFile(args.hook, 'utf-8');
      hookDef = JSON.parse(hookJson);
    } catch (error) {
      throw new Error(`Failed to load hook: ${error.message}`);
    }

    // Load data if provided
    const store = useStoreContext();
    if (args.data) {
      const turtle = await useTurtle();
      const dataContent = await readFile(args.data, 'utf-8');
      const quads = await turtle.parse(dataContent);
      store.add(...quads);
    }

    // Try sidecar first, fallback to local
    let result;
    try {
      result = await withSidecar(async (client) => {
        return await client.evaluateHook({
          hookId: hookDef.name || 'cli-hook',
          hook: hookDef,
          event: {
            type: 'manual',
            timestamp: new Date().toISOString(),
            store: store
          }
        });
      });
    } catch (error) {
      // Fallback to local evaluation
      console.warn(`⚠️  Sidecar unavailable, using local evaluation`);

      const manager = new KnowledgeHookManager({
        observability: { enableTracing: false },
        performance: { enableProfiling: args.verbose }
      });

      result = await manager.evaluateHook(hookDef, {
        type: 'manual',
        timestamp: new Date().toISOString()
      });
    }

    // Format output
    const format = getArg(args, 'format', 'json');
    if (format === 'json') {
      console.log(JSON.stringify(result, null, 2));
    } else {
      console.log(`\n🔥 Result: ${result.fired ? 'FIRED' : 'No Change'}`);
      console.log(`⏱️  Duration: ${result.executionTime || 0}ms`);
      if (result.effects && result.effects.length > 0) {
        console.log(`✨ Effects executed: ${result.effects.length}`);
      }
    }

    // Write output if requested
    if (args.output) {
      await writeFile(args.output, JSON.stringify(result, null, 2));
      console.log(`📄 Result written to ${args.output}`);
    }
  } catch (error) {
    console.error(`❌ Hook evaluation failed: ${error.message}`);
    if (args.verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
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
 * Create knowledge hook from template
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function hookCreateCommand(ctx, config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['name', 'type']);

  console.log(`🔨 Creating hook: ${args.name} (${args.type})`);

  const templates = {
    'sparql-ask': {
      name: args.name,
      kind: 'before',
      condition: {
        type: 'sparql-ask',
        query: args.query || 'ASK { ?s ?p ?o }'
      },
      effects: []
    },
    'shacl': {
      name: args.name,
      kind: 'before',
      condition: {
        type: 'shacl',
        shapesFile: args.shapes || 'shapes.ttl'
      },
      effects: []
    },
    'threshold': {
      name: args.name,
      kind: 'before',
      condition: {
        type: 'threshold',
        query: args.query || 'SELECT (COUNT(?s) AS ?count) WHERE { ?s ?p ?o }',
        threshold: args.threshold || 1000,
        operator: args.operator || 'gt'
      },
      effects: []
    }
  };

  const template = templates[args.type];
  if (!template) {
    console.error(`❌ Unknown hook type: ${args.type}`);
    console.log(`Available types: ${Object.keys(templates).join(', ')}`);
    process.exit(1);
  }

  // Add description if provided
  if (args.description) {
    template.description = args.description;
  }

  // Write hook file
  const outputPath = args.output || `hooks/${args.name}.json`;
  await writeFile(outputPath, JSON.stringify(template, null, 2));
  console.log(`✅ Hook created: ${outputPath}`);
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
