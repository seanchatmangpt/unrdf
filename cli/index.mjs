#!/usr/bin/env node

/**
 * @file UNRDF v2 CLI - Definitive Optimized Version
 * @module cli-v2
 *
 * @description
 * Single, definitive CLI with aggressive optimizations for minimal startup time.
 * Target: <100ms total startup time with full functionality.
 */

import { defineCommand, runMain } from "citty";

// Minimal error handler
const errorHandler = (error) => {
  console.error(`Error: ${error.message}`);
  process.exit(1);
};

// Ultra-lazy loading for all dependencies
let config, contextManager, pluginLoader, authMiddleware, telemetryMiddleware;

const getConfig = async () => {
  if (!config) {
    const module = await import("./core/config.mjs");
    config = module.loadConfig;
  }
  return config;
};

const getContextManager = async () => {
  if (!contextManager) {
    const module = await import("./core/context.mjs");
    contextManager = module.ContextManager;
  }
  return contextManager;
};

const getPluginLoader = async () => {
  if (!pluginLoader) {
    const module = await import("./core/plugin-loader.mjs");
    pluginLoader = module.PluginLoader;
  }
  return pluginLoader;
};

const getAuthMiddleware = async () => {
  if (!authMiddleware) {
    const module = await import("./middleware/auth.mjs");
    authMiddleware = module.authMiddleware;
  }
  return authMiddleware;
};

const getTelemetryMiddleware = async () => {
  if (!telemetryMiddleware) {
    const module = await import("./middleware/telemetry.mjs");
    telemetryMiddleware = module.telemetryMiddleware;
  }
  return telemetryMiddleware;
};

// Command modules cache
let commandModules;
const getCommandModules = async () => {
  if (!commandModules) {
    commandModules = await Promise.all([
      import("./commands/graph/index.mjs"),
      import("./commands/hook/index.mjs"),
      import("./commands/policy/index.mjs"),
      import("./commands/sidecar/index.mjs"),
      import("./commands/store/index.mjs"),
      import("./commands/context/index.mjs"),
    ]);
  }
  return commandModules;
};

/**
 * Definitive CLI with optimal performance
 */
const main = defineCommand({
  meta: {
    name: "unrdf",
    version: "2.0.0",
    description: "UNRDF v2 - Enterprise Knowledge Graph CLI",
  },

  args: {
    fast: {
      type: "boolean",
      description: "Fast mode - skip heavy initialization",
      default: false,
    },
  },

  // Minimal setup - only load what's absolutely necessary
  async setup(ctx) {
    // Always load config (lightweight)
    const loadConfig = await getConfig();
    ctx.config = await loadConfig();

    // Skip heavy initialization in fast mode
    if (!ctx.args.fast) {
      const ContextManager = await getContextManager();
      const PluginLoader = await getPluginLoader();
      const authMiddleware = await getAuthMiddleware();

      ctx.contextManager = new ContextManager(ctx.config);
      ctx.pluginLoader = new PluginLoader(ctx.config);
      await ctx.pluginLoader.loadPlugins();
      await authMiddleware(ctx);

      // Load telemetry only if enabled
      if (ctx.config.telemetry?.enabled) {
        const telemetryMiddleware = await getTelemetryMiddleware();
        await telemetryMiddleware(ctx);
      }
    }
  },

  subCommands: {
    // Graph resource commands
    graph: defineCommand({
      meta: { name: "graph", description: "Manage RDF graphs" },
      subCommands: {
        update: defineCommand({
          meta: { name: "update", description: "Update graph" },
          async run(ctx) {
            const [graphCommands] = await getCommandModules();
            return graphCommands.updateCommand.run(ctx);
          },
        }),
        delete: defineCommand({
          meta: { name: "delete", description: "Delete graph" },
          async run(ctx) {
            const [graphCommands] = await getCommandModules();
            return graphCommands.deleteCommand.run(ctx);
          },
        }),
        describe: defineCommand({
          meta: { name: "describe", description: "Describe graph" },
          async run(ctx) {
            const [graphCommands] = await getCommandModules();
            return graphCommands.describeCommand.run(ctx);
          },
        }),
      },
    }),

    // Hook resource commands
    hook: defineCommand({
      meta: { name: "hook", description: "Manage knowledge hooks" },
      subCommands: {
        list: defineCommand({
          meta: { name: "list", description: "List knowledge hooks" },
          async run(ctx) {
            const [, hookCommands] = await getCommandModules();
            return hookCommands.listCommand.run(ctx);
          },
        }),
        get: defineCommand({
          meta: { name: "get", description: "Get hook details" },
          async run(ctx) {
            const [, hookCommands] = await getCommandModules();
            return hookCommands.getCommand.run(ctx);
          },
        }),
        create: defineCommand({
          meta: { name: "create", description: "Create a hook" },
          async run(ctx) {
            const [, hookCommands] = await getCommandModules();
            return hookCommands.createCommand.run(ctx);
          },
        }),
        update: defineCommand({
          meta: { name: "update", description: "Update hook" },
          async run(ctx) {
            const [, hookCommands] = await getCommandModules();
            return hookCommands.updateCommand.run(ctx);
          },
        }),
        delete: defineCommand({
          meta: { name: "delete", description: "Delete hook" },
          async run(ctx) {
            const [, hookCommands] = await getCommandModules();
            return hookCommands.deleteCommand.run(ctx);
          },
        }),
        history: defineCommand({
          meta: { name: "history", description: "Show hook history" },
          async run(ctx) {
            const [, hookCommands] = await getCommandModules();
            return hookCommands.historyCommand.run(ctx);
          },
        }),
        describe: defineCommand({
          meta: { name: "describe", description: "Describe hook" },
          async run(ctx) {
            const [, hookCommands] = await getCommandModules();
            return hookCommands.describeCommand.run(ctx);
          },
        }),
      },
    }),

    // Policy resource commands
    policy: defineCommand({
      meta: { name: "policy", description: "Manage policy packs" },
      subCommands: {
        list: defineCommand({
          meta: { name: "list", description: "List policy packs" },
          async run(ctx) {
            const [, , policyCommands] = await getCommandModules();
            return policyCommands.listCommand.run(ctx);
          },
        }),
        get: defineCommand({
          meta: { name: "get", description: "Get policy details" },
          async run(ctx) {
            const [, , policyCommands] = await getCommandModules();
            return policyCommands.getCommand.run(ctx);
          },
        }),
        apply: defineCommand({
          meta: { name: "apply", description: "Apply policy" },
          async run(ctx) {
            const [, , policyCommands] = await getCommandModules();
            return policyCommands.applyCommand.run(ctx);
          },
        }),
        test: defineCommand({
          meta: { name: "test", description: "Test policy" },
          async run(ctx) {
            const [, , policyCommands] = await getCommandModules();
            return policyCommands.testCommand.run(ctx);
          },
        }),
        validate: defineCommand({
          meta: { name: "validate", description: "Validate policy" },
          async run(ctx) {
            const [, , policyCommands] = await getCommandModules();
            return policyCommands.validateCommand.run(ctx);
          },
        }),
        describe: defineCommand({
          meta: { name: "describe", description: "Describe policy" },
          async run(ctx) {
            const [, , policyCommands] = await getCommandModules();
            return policyCommands.describeCommand.run(ctx);
          },
        }),
      },
    }),

    // Sidecar resource commands
    sidecar: defineCommand({
      meta: { name: "sidecar", description: "Manage KGC sidecar" },
      subCommands: {
        status: defineCommand({
          meta: { name: "status", description: "Get sidecar status" },
          async run(ctx) {
            const [, , , sidecarCommands] = await getCommandModules();
            return sidecarCommands.statusCommand.run(ctx);
          },
        }),
        health: defineCommand({
          meta: { name: "health", description: "Check sidecar health" },
          async run(ctx) {
            const [, , , sidecarCommands] = await getCommandModules();
            return sidecarCommands.healthCommand.run(ctx);
          },
        }),
        config: defineCommand({
          meta: { name: "config", description: "Manage sidecar config" },
          async run(ctx) {
            const [, , , sidecarCommands] = await getCommandModules();
            return sidecarCommands.configCommand.run(ctx);
          },
        }),
        logs: defineCommand({
          meta: { name: "logs", description: "View sidecar logs" },
          async run(ctx) {
            const [, , , sidecarCommands] = await getCommandModules();
            return sidecarCommands.logsCommand.run(ctx);
          },
        }),
        restart: defineCommand({
          meta: { name: "restart", description: "Restart sidecar" },
          async run(ctx) {
            const [, , , sidecarCommands] = await getCommandModules();
            return sidecarCommands.restartCommand.run(ctx);
          },
        }),
      },
    }),

    // Store resource commands
    store: defineCommand({
      meta: { name: "store", description: "Manage RDF store" },
      subCommands: {
        import: defineCommand({
          meta: { name: "import", description: "Import data" },
          async run(ctx) {
            const [, , , , storeCommands] = await getCommandModules();
            return storeCommands.importCommand.run(ctx);
          },
        }),
        export: defineCommand({
          meta: { name: "export", description: "Export data" },
          async run(ctx) {
            const [, , , , storeCommands] = await getCommandModules();
            return storeCommands.exportCommand.run(ctx);
          },
        }),
        query: defineCommand({
          meta: { name: "query", description: "Query store" },
          async run(ctx) {
            const [, , , , storeCommands] = await getCommandModules();
            return storeCommands.queryCommand.run(ctx);
          },
        }),
        stats: defineCommand({
          meta: { name: "stats", description: "Show store stats" },
          async run(ctx) {
            const [, , , , storeCommands] = await getCommandModules();
            return storeCommands.statsCommand.run(ctx);
          },
        }),
      },
    }),

    // Context resource commands
    context: defineCommand({
      meta: { name: "context", description: "Manage CLI contexts" },
      subCommands: {
        list: defineCommand({
          meta: { name: "list", description: "List contexts" },
          async run(ctx) {
            const [, , , , , contextCommands] = await getCommandModules();
            return contextCommands.listCommand.run(ctx);
          },
        }),
        create: defineCommand({
          meta: { name: "create", description: "Create context" },
          async run(ctx) {
            const [, , , , , contextCommands] = await getCommandModules();
            return contextCommands.createCommand.run(ctx);
          },
        }),
        delete: defineCommand({
          meta: { name: "delete", description: "Delete context" },
          async run(ctx) {
            const [, , , , , contextCommands] = await getCommandModules();
            return contextCommands.deleteCommand.run(ctx);
          },
        }),
        get: defineCommand({
          meta: { name: "get", description: "Get context" },
          async run(ctx) {
            const [, , , , , contextCommands] = await getCommandModules();
            return contextCommands.getCommand.run(ctx);
          },
        }),
        use: defineCommand({
          meta: { name: "use", description: "Use context" },
          async run(ctx) {
            const [, , , , , contextCommands] = await getCommandModules();
            return contextCommands.useCommand.run(ctx);
          },
        }),
        current: defineCommand({
          meta: { name: "current", description: "Show current context" },
          async run(ctx) {
            const [, , , , , contextCommands] = await getCommandModules();
            return contextCommands.currentCommand.run(ctx);
          },
        }),
      },
    }),

    // Plugin management
    plugin: defineCommand({
      meta: { name: "plugin", description: "Manage CLI plugins" },
      subCommands: {
        list: defineCommand({
          meta: { name: "list", description: "List installed plugins" },
          async run(ctx) {
            if (!ctx.pluginLoader) {
              const PluginLoader = await getPluginLoader();
              ctx.pluginLoader = new PluginLoader(ctx.config || {});
            }
            const plugins = ctx.pluginLoader.getPlugins();
            console.log("Installed plugins:");
            for (const plugin of plugins) {
              console.log(`  - ${plugin.name} (${plugin.version})`);
            }
          },
        }),
        install: defineCommand({
          meta: { name: "install", description: "Install a plugin" },
          args: {
            name: { type: "positional", required: true },
          },
          async run(ctx) {
            if (!ctx.pluginLoader) {
              const PluginLoader = await getPluginLoader();
              ctx.pluginLoader = new PluginLoader(ctx.config || {});
            }
            await ctx.pluginLoader.install(ctx.args.name);
            console.log(`✅ Plugin ${ctx.args.name} installed`);
          },
        }),
      },
    }),

    // REPL (Interactive Mode)
    repl: defineCommand({
      meta: { name: "repl", description: "Start interactive SPARQL REPL" },
      args: {
        endpoint: { type: "string", alias: "e" },
        timeout: { type: "string", default: "30000" },
      },
      async run(ctx) {
        const { replCommand } = await import("./commands/repl.mjs");
        await replCommand.run(ctx);
      },
    }),

    // Init command
    init: defineCommand({
      meta: { name: "init", description: "Initialize a new UNRDF project" },
      async run(ctx) {
        const { initCommand } = await import("./commands/init.mjs");
        await initCommand.run(ctx);
      },
    }),

    // Completion command
    completion: defineCommand({
      meta: {
        name: "completion",
        description: "Generate shell completion scripts",
      },
      args: {
        shell: { type: "positional", required: true },
      },
      async run(ctx) {
        const { generateCompletion } = await import("./core/completion.mjs");
        const script = await generateCompletion(ctx.args.shell);
        console.log(script);
      },
    }),
  },

  async cleanup(ctx) {
    if (ctx.pluginLoader) {
      await ctx.pluginLoader.cleanup();
    }
  },
});

// Run with error handling
runMain(main).catch(errorHandler);
