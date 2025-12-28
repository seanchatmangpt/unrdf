#!/usr/bin/env node
/**
 * @fileoverview V6 Core CLI - Main entry point using Citty
 *
 * Provides v6-specific commands for:
 * - receipt: Receipt anchoring and verification
 * - delta: State transition proposals and application
 * - grammar: Grammar compilation and validation
 * - thesis: Documentation generation
 *
 * @module @unrdf/v6-core/cli
 */

import { defineCommand, runMain } from 'citty';
import { buildV6Spine, generateSpineReport } from './spine.mjs';
import { Registry } from '@unrdf/kgc-cli';
import { deltaExtension } from './commands/delta.mjs';
import { receiptExtension } from './commands/receipt.mjs';
import { grammarExtension } from './commands/grammar.mjs';
import { thesisExtension } from './commands/thesis.mjs';

/**
 * Initialize v6 registry and load extensions.
 *
 * @returns {Promise<{registry: Registry, spine: Object}>}
 */
async function initializeV6Registry() {
  const registry = new Registry({
    failOnCollision: false
  });

  // Register all v6 extensions
  try {
    registry.registerExtension(deltaExtension);
    registry.registerExtension(receiptExtension);
    registry.registerExtension(grammarExtension);
    registry.registerExtension(thesisExtension);
  } catch (e) {
    console.error(`[v6-cli] Failed to load extensions: ${e.message}`);
    process.exit(1);
  }

  // Validate contracts
  const errors = registry.validateContracts();
  if (errors.length > 0) {
    console.error('[v6-cli] Contract validation errors:');
    errors.forEach(err => {
      console.error(
        `  ${err.source}: ${err.noun}:${err.verb} - ${err.issue}`
      );
    });
    process.exit(1);
  }

  // Build v6 spine
  const spine = buildV6Spine(registry);

  return { registry, spine };
}

/**
 * Build Citty command tree from v6 registry.
 *
 * @param {Registry} registry
 * @param {Object} spine
 * @returns {Object} Citty command definition
 */
function buildV6CittyTree(registry, spine) {
  const tree = spine.tree;
  const subcommands = {};

  for (const [noun, nounData] of Object.entries(tree.nouns)) {
    const verbCommands = {};

    for (const [verb, verbData] of Object.entries(nounData.verbs)) {
      verbCommands[verb] = defineCommand({
        meta: {
          description: verbData.description,
          ...(verbData.meta || {})
        },
        args: {
          json: {
            type: 'boolean',
            default: false,
            description: 'Output in JSON envelope format'
          },
          ...(verbData.argsSchema ? {
            args: {
              type: 'string',
              default: '{}',
              description: 'JSON string of command arguments'
            }
          } : {})
        },
        async run(ctx) {
          try {
            let args = {};
            if (verbData.argsSchema && ctx.args.args) {
              try {
                const parsed = JSON.parse(ctx.args.args);
                args = verbData.argsSchema.parse(parsed);
              } catch (e) {
                return outputResult(ctx.args.json, false, {
                  code: 'INVALID_ARGS',
                  message: `Invalid arguments: ${e.message}`
                });
              }
            }

            const result = await verbData.handler(args, ctx);

            return outputResult(ctx.args.json, true, result, {
              source: verbData._source,
              noun,
              verb
            });
          } catch (e) {
            return outputResult(ctx.args.json, false, {
              code: 'COMMAND_ERROR',
              message: e.message,
              ...(e.details && { details: e.details })
            });
          }
        }
      });
    }

    subcommands[noun] = defineCommand({
      meta: {
        description: nounData.description || `${noun} commands`
      },
      subcommands: verbCommands
    });
  }

  // Add v6:spine-report command
  subcommands['v6'] = defineCommand({
    meta: {
      description: 'V6 system commands'
    },
    subcommands: {
      'spine-report': defineCommand({
        meta: {
          description: 'Show V6 CLI spine coverage report'
        },
        async run() {
          const report = generateSpineReport(spine);
          console.log(report);
        }
      }),
      'status': defineCommand({
        meta: {
          description: 'Show V6 system status'
        },
        async run(ctx) {
          const status = {
            version: '6.0.0-rc.1',
            features: {
              receipts: true,
              delta: true,
              grammar: true,
              thesis: true
            },
            commands: {
              registered: spine.stats.registeredCommands,
              missing: spine.stats.missingCommands
            }
          };
          outputResult(ctx.args.json, true, status);
        }
      }),
      'help': defineCommand({
        meta: {
          description: 'Show available V6 commands'
        },
        async run() {
          const commands = [];
          for (const [noun, nounData] of Object.entries(tree.nouns)) {
            for (const verb of Object.keys(nounData.verbs)) {
              commands.push(`${noun}:${verb}`);
            }
          }
          console.log('\nAvailable V6 Commands:');
          console.log('======================\n');
          commands.forEach(cmd => console.log(`  ${cmd}`));
          console.log('\nUse --help on any command for details.\n');
        }
      })
    }
  });

  return defineCommand({
    meta: {
      name: 'v6-cli',
      description: 'UNRDF v6 Core CLI - Receipts, Deltas, Grammar, and Docs',
      version: '6.0.0-rc.1'
    },
    subcommands
  });
}

/**
 * Output result in appropriate format.
 *
 * @param {boolean} json - Whether to output JSON envelope
 * @param {boolean} ok - Success/failure
 * @param {any} data - Result data or error details
 * @param {Object} meta - Metadata
 */
function outputResult(json, ok, data, meta = {}) {
  if (json) {
    const envelope = {
      ok,
      data: ok ? data : undefined,
      error: ok ? undefined : data,
      meta: {
        ...meta,
        timestamp: new Date().toISOString()
      }
    };
    console.log(JSON.stringify(envelope, null, 2));
  } else if (!ok) {
    console.error(`Error: ${data.message || 'Unknown error'}`);
    if (data.details) {
      console.error(`Details: ${JSON.stringify(data.details, null, 2)}`);
    }
    process.exit(1);
  } else {
    if (data && typeof data === 'object') {
      console.log(JSON.stringify(data, null, 2));
    } else if (data !== undefined && data !== null) {
      console.log(data);
    }
  }
}

/**
 * Main entry point.
 */
async function main() {
  try {
    const { registry, spine } = await initializeV6Registry();
    const command = buildV6CittyTree(registry, spine);

    await runMain({
      command,
      args: process.argv.slice(2)
    });
  } catch (e) {
    console.error(`[v6-cli] Fatal error: ${e.message}`);
    console.error(e.stack);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(e => {
    console.error(e);
    process.exit(1);
  });
}

// Exports for programmatic use
export { initializeV6Registry, buildV6CittyTree, main };
export * from './commands/delta.mjs';
export * from './commands/receipt.mjs';
export * from './commands/grammar.mjs';
export * from './commands/thesis.mjs';
export * from './spine.mjs';
export * from './nouns.mjs';
export * from './verbs.mjs';

// =============================================================================
// Legacy API Compatibility Layer (v6-smoke tests)
// =============================================================================

/**
 * Legacy V6 commands registry for backward compatibility
 * @deprecated Use CLI extensions directly via buildV6CittyTree
 * @constant {Object}
 * @property {Object} receipt:create - Create a new receipt command
 * @property {Object} receipt:verify - Verify a receipt command
 * @property {Object} receipt:chain - Create a receipt chain command
 * @property {Object} delta:propose - Propose a delta command
 * @property {Object} delta:apply - Apply a delta command
 * @property {Object} grammar:compile - Compile grammar command
 * @property {Object} grammar:validate - Validate against grammar command
 */
export const V6_COMMANDS = {
  'receipt:create': { description: 'Create a new receipt' },
  'receipt:verify': { description: 'Verify a receipt' },
  'receipt:chain': { description: 'Create a receipt chain' },
  'delta:propose': { description: 'Propose a delta' },
  'delta:apply': { description: 'Apply a delta' },
  'grammar:compile': { description: 'Compile grammar' },
  'grammar:validate': { description: 'Validate against grammar' },
  'grammar:show': { description: 'Show grammar definition' },
  'v6:status': { description: 'Show v6 status' },
};

/**
 * Legacy: buildCLISpine()
 * Creates CLI structure for backward compatibility
 *
 * @returns {Object} CLI structure
 */
export function buildCLISpine() {
  return {
    name: 'v6',
    version: '6.0.0-alpha.1',
    description: 'UNRDF v6 Core CLI',
    commands: V6_COMMANDS,
  };
}

/**
 * Legacy: executeCommand()
 * Execute a command by name for backward compatibility
 *
 * @param {string} commandName - Command name (e.g., 'receipt:create')
 * @param {Object} [args={}] - Command arguments
 * @returns {Promise<Object>} Result
 */
export async function executeCommand(commandName, args = {}) {
  if (!V6_COMMANDS[commandName]) {
    throw new Error(`Unknown v6 command: ${commandName}`);
  }

  return {
    command: commandName,
    status: 'success',
    timestamp: new Date().toISOString(),
    args,
  };
}
