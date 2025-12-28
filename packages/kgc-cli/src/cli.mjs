#!/usr/bin/env node
/**
 * @fileoverview KGC CLI - Main entry point using Citty for command tree building.
 *
 * Loads extension registry, builds noun/verb command tree, executes commands
 * with deterministic JSON envelopes in --json mode.
 *
 * Usage:
 *   kgc --help                    # Show nouns
 *   kgc <noun> --help             # Show verbs for noun
 *   kgc <noun> <verb> [args]      # Execute command
 *   kgc <noun> <verb> --json      # JSON envelope output
 */

import { defineCommand, runMain } from 'citty';
import { Registry, createEnvelope } from './lib/registry.mjs';
import { loadManifest } from './manifest/extensions.mjs';

/**
 * Initialize registry and load extensions.
 *
 * @returns {Promise<{registry: Registry, tree: Object}>}
 */
async function initializeRegistry() {
  const registry = new Registry({
    failOnCollision: true
  });

  try {
    await loadManifest(registry);
  } catch (e) {
    console.error(`[kgc-cli] Failed to load extensions: ${e.message}`);
    process.exit(1);
  }

  const errors = registry.validateContracts();
  if (errors.length > 0) {
    console.error('[kgc-cli] Contract validation errors:');
    errors.forEach(err => {
      console.error(
        `  ${err.source}: ${err.noun}:${err.verb} - ${err.issue}`
      );
    });
    process.exit(1);
  }

  const tree = registry.buildCommandTree();
  return { registry, tree };
}

/**
 * Build Citty command tree from registry.
 *
 * Returns a Citty command with subcommands for each noun,
 * and nested subcommands for each verb.
 *
 * @param {Registry} registry
 * @param {Object} tree
 * @returns {Object} Citty command definition
 */
function buildCittyTree(registry, tree) {
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
          // Common options
          json: {
            type: 'boolean',
            default: false,
            description: 'Output in JSON envelope format'
          },
          ...(verbData.argsSchema ? {
            // If command has argsSchema, add a generic args option
            args: {
              type: 'string',
              default: '{}',
              description: 'JSON string of command arguments'
            }
          } : {})
        },
        async run(ctx) {
          try {
            // Parse args if schema present
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

            // Execute handler
            const result = await verbData.handler(args, ctx);

            // Output result
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

    // Create noun subcommand with verb subcommands
    subcommands[noun] = defineCommand({
      meta: {
        description: nounData.description || `${noun} commands`
      },
      subcommands: verbCommands
    });
  }

  // Root command
  return defineCommand({
    meta: {
      name: 'kgc',
      description: 'KGC CLI - Deterministic extension registry for UNRDF workspace',
      version: '5.0.1'
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
    const envelope = createEnvelope(ok, data, meta);
    console.log(JSON.stringify(envelope, null, 2));
  } else if (!ok) {
    // Human-readable error
    console.error(`Error: ${data.message || 'Unknown error'}`);
    if (data.details) {
      console.error(`Details: ${JSON.stringify(data.details, null, 2)}`);
    }
    process.exit(1);
  } else {
    // Human-readable success (quiet by default for data)
    if (data && typeof data === 'object') {
      // Pretty-print objects
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
    const { registry, tree } = await initializeRegistry();
    const command = buildCittyTree(registry, tree);

    // Run Citty main loop
    await runMain({
      command,
      args: process.argv.slice(2)
    });
  } catch (e) {
    console.error(`[kgc-cli] Fatal error: ${e.message}`);
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

export { buildCittyTree, initializeRegistry };
