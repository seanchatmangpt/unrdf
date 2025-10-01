#!/usr/bin/env node

/**
 * @file UNRDF v2 CLI - Enterprise Noun-Verb Command Interface
 * @module cli-v2
 *
 * @description
 * Production-ready CLI implementing noun-verb pattern (like kubectl/docker)
 * with full KGC sidecar and knowledge-engine integration.
 */

import { defineCommand, runMain } from 'citty';
import { createRouter } from './core/router.mjs';
import { ContextManager } from './core/context.mjs';
import { PluginLoader } from './core/plugin-loader.mjs';
import { loadConfig } from './core/config.mjs';
import { authMiddleware } from './middleware/auth.mjs';
import { telemetryMiddleware } from './middleware/telemetry.mjs';
import { errorHandler } from './middleware/error-handler.mjs';

// Import resource commands
import * as graphCommands from './commands/graph/index.mjs';
import * as hookCommands from './commands/hook/index.mjs';
import * as policyCommands from './commands/policy/index.mjs';
import * as sidecarCommands from './commands/sidecar/index.mjs';
import * as storeCommands from './commands/store/index.mjs';
import * as contextCommands from './commands/context/index.mjs';

/**
 * Main CLI command with noun-verb routing
 */
const main = defineCommand({
  meta: {
    name: 'unrdf',
    version: '2.0.0',
    description: 'UNRDF v2 - Enterprise Knowledge Graph CLI with KGC Sidecar Integration'
  },

  async setup(ctx) {
    // Load configuration
    ctx.config = await loadConfig();

    // Initialize context manager
    ctx.contextManager = new ContextManager(ctx.config);

    // Initialize plugin loader
    ctx.pluginLoader = new PluginLoader(ctx.config);
    await ctx.pluginLoader.loadPlugins();

    // Apply middleware
    await authMiddleware(ctx);
    await telemetryMiddleware(ctx);
  },

  subCommands: {
    // Graph resource commands
    graph: defineCommand({
      meta: {
        name: 'graph',
        description: 'Manage RDF graphs'
      },
      subCommands: {
        list: graphCommands.listCommand,
        get: graphCommands.getCommand,
        create: graphCommands.createCommand,
        update: graphCommands.updateCommand,
        delete: graphCommands.deleteCommand,
        validate: graphCommands.validateCommand,
        export: graphCommands.exportCommand,
        describe: graphCommands.describeCommand
      }
    }),

    // Hook resource commands
    hook: defineCommand({
      meta: {
        name: 'hook',
        description: 'Manage knowledge hooks'
      },
      subCommands: {
        list: hookCommands.listCommand,
        get: hookCommands.getCommand,
        create: hookCommands.createCommand,
        update: hookCommands.updateCommand,
        delete: hookCommands.deleteCommand,
        eval: hookCommands.evalCommand,
        history: hookCommands.historyCommand,
        describe: hookCommands.describeCommand
      }
    }),

    // Policy resource commands
    policy: defineCommand({
      meta: {
        name: 'policy',
        description: 'Manage policy packs'
      },
      subCommands: {
        list: policyCommands.listCommand,
        get: policyCommands.getCommand,
        apply: policyCommands.applyCommand,
        validate: policyCommands.validateCommand,
        test: policyCommands.testCommand,
        describe: policyCommands.describeCommand
      }
    }),

    // Sidecar resource commands
    sidecar: defineCommand({
      meta: {
        name: 'sidecar',
        description: 'Manage KGC sidecar'
      },
      subCommands: {
        status: sidecarCommands.statusCommand,
        logs: sidecarCommands.logsCommand,
        config: sidecarCommands.configCommand,
        restart: sidecarCommands.restartCommand,
        health: sidecarCommands.healthCommand
      }
    }),

    // Store resource commands
    store: defineCommand({
      meta: {
        name: 'store',
        description: 'Manage RDF store'
      },
      subCommands: {
        import: storeCommands.importCommand,
        export: storeCommands.exportCommand,
        query: storeCommands.queryCommand,
        stats: storeCommands.statsCommand,
        backup: storeCommands.backupCommand,
        restore: storeCommands.restoreCommand
      }
    }),

    // Context resource commands
    context: defineCommand({
      meta: {
        name: 'context',
        description: 'Manage CLI contexts (like kubeconfig)'
      },
      subCommands: {
        list: contextCommands.listCommand,
        get: contextCommands.getCommand,
        use: contextCommands.useCommand,
        create: contextCommands.createCommand,
        delete: contextCommands.deleteCommand,
        current: contextCommands.currentCommand
      }
    }),

    // Completion command
    completion: defineCommand({
      meta: {
        name: 'completion',
        description: 'Generate shell completion scripts'
      },
      args: {
        shell: {
          type: 'positional',
          description: 'Shell type (bash, zsh, fish)',
          required: true
        }
      },
      async run(ctx) {
        const { generateCompletion } = await import('./core/completion.mjs');
        const completion = generateCompletion(ctx.args.shell);
        console.log(completion);
      }
    }),

    // Plugin management
    plugin: defineCommand({
      meta: {
        name: 'plugin',
        description: 'Manage CLI plugins'
      },
      subCommands: {
        list: defineCommand({
          meta: {
            name: 'list',
            description: 'List installed plugins'
          },
          async run(ctx) {
            const plugins = ctx.pluginLoader.getPlugins();
            console.log('Installed plugins:');
            for (const plugin of plugins) {
              console.log(`  - ${plugin.name} (${plugin.version})`);
            }
          }
        }),
        install: defineCommand({
          meta: {
            name: 'install',
            description: 'Install a plugin'
          },
          args: {
            name: {
              type: 'positional',
              description: 'Plugin name',
              required: true
            }
          },
          async run(ctx) {
            await ctx.pluginLoader.install(ctx.args.name);
            console.log(`✅ Plugin ${ctx.args.name} installed`);
          }
        })
      }
    })
  },

  async cleanup(ctx) {
    // Cleanup resources
    if (ctx.pluginLoader) {
      await ctx.pluginLoader.cleanup();
    }
  }
});

// Run CLI with error handling
runMain(main).catch(errorHandler);
