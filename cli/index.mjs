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

// Lazy import helpers to avoid eager loading unrelated command trees
const importGraph = () => import("./commands/graph/index.mjs");
const importHook = () => import("./commands/hook/index.mjs");
const importPolicy = () => import("./commands/policy/index.mjs");
const importStore = () => import("./commands/store/index.mjs");
const importContext = () => import("./commands/context/index.mjs");

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
            const graphCommands = await importGraph();
            return graphCommands.updateCommand.run(ctx);
          },
        }),
        delete: defineCommand({
          meta: { name: "delete", description: "Delete graph" },
          async run(ctx) {
            const graphCommands = await importGraph();
            return graphCommands.deleteCommand.run(ctx);
          },
        }),
        describe: defineCommand({
          meta: { name: "describe", description: "Describe graph" },
          async run(ctx) {
            const graphCommands = await importGraph();
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
            const hookCommands = await importHook();
            return hookCommands.listCommand.run(ctx);
          },
        }),
        get: defineCommand({
          meta: { name: "get", description: "Get hook details" },
          async run(ctx) {
            const hookCommands = await importHook();
            return hookCommands.getCommand.run(ctx);
          },
        }),
        create: defineCommand({
          meta: { name: "create", description: "Create a hook" },
          async run(ctx) {
            const hookCommands = await importHook();
            return hookCommands.createCommand.run(ctx);
          },
        }),
        update: defineCommand({
          meta: { name: "update", description: "Update hook" },
          async run(ctx) {
            const hookCommands = await importHook();
            return hookCommands.updateCommand.run(ctx);
          },
        }),
        delete: defineCommand({
          meta: { name: "delete", description: "Delete hook" },
          async run(ctx) {
            const hookCommands = await importHook();
            return hookCommands.deleteCommand.run(ctx);
          },
        }),
        history: defineCommand({
          meta: { name: "history", description: "Show hook history" },
          async run(ctx) {
            const hookCommands = await importHook();
            return hookCommands.historyCommand.run(ctx);
          },
        }),
        describe: defineCommand({
          meta: { name: "describe", description: "Describe hook" },
          async run(ctx) {
            const hookCommands = await importHook();
            return hookCommands.describeCommand.run(ctx);
          },
        }),
        eval: defineCommand({
          meta: { name: "eval", description: "Evaluate a knowledge hook" },
          args: {
            hook: { type: "positional", required: true },
            data: { type: "string", alias: "d" },
            format: { type: "string", default: "table", alias: "f" },
            output: { type: "string", alias: "o" },
            verbose: { type: "boolean", alias: "v" },
          },
          async run(ctx) {
            const { evalCommand } = await import("../src/cli/commands/hook/eval.mjs");
            return evalCommand.run(ctx);
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
            const policyCommands = await importPolicy();
            return policyCommands.listCommand.run(ctx);
          },
        }),
        get: defineCommand({
          meta: { name: "get", description: "Get policy details" },
          async run(ctx) {
            const policyCommands = await importPolicy();
            return policyCommands.getCommand.run(ctx);
          },
        }),
        apply: defineCommand({
          meta: { name: "apply", description: "Apply policy" },
          async run(ctx) {
            const policyCommands = await importPolicy();
            return policyCommands.applyCommand.run(ctx);
          },
        }),
        test: defineCommand({
          meta: { name: "test", description: "Test policy" },
          async run(ctx) {
            const policyCommands = await importPolicy();
            return policyCommands.testCommand.run(ctx);
          },
        }),
        validate: defineCommand({
          meta: { name: "validate", description: "Validate policy" },
          async run(ctx) {
            const policyCommands = await importPolicy();
            return policyCommands.validateCommand.run(ctx);
          },
        }),
        describe: defineCommand({
          meta: { name: "describe", description: "Describe policy" },
          async run(ctx) {
            const policyCommands = await importPolicy();
            return policyCommands.describeCommand.run(ctx);
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
            const storeCommands = await importStore();
            return storeCommands.importCommand.run(ctx);
          },
        }),
        export: defineCommand({
          meta: { name: "export", description: "Export data" },
          async run(ctx) {
            const storeCommands = await importStore();
            return storeCommands.exportCommand.run(ctx);
          },
        }),
        query: defineCommand({
          meta: { name: "query", description: "Query store" },
          async run(ctx) {
            const storeCommands = await importStore();
            return storeCommands.queryCommand.run(ctx);
          },
        }),
        stats: defineCommand({
          meta: { name: "stats", description: "Show store stats" },
          async run(ctx) {
            const storeCommands = await importStore();
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
            const contextCommands = await importContext();
            return contextCommands.listCommand.run(ctx);
          },
        }),
        create: defineCommand({
          meta: { name: "create", description: "Create context" },
          async run(ctx) {
            const contextCommands = await importContext();
            return contextCommands.createCommand.run(ctx);
          },
        }),
        delete: defineCommand({
          meta: { name: "delete", description: "Delete context" },
          async run(ctx) {
            const contextCommands = await importContext();
            return contextCommands.deleteCommand.run(ctx);
          },
        }),
        get: defineCommand({
          meta: { name: "get", description: "Get context" },
          async run(ctx) {
            const contextCommands = await importContext();
            return contextCommands.getCommand.run(ctx);
          },
        }),
        use: defineCommand({
          meta: { name: "use", description: "Use context" },
          async run(ctx) {
            const contextCommands = await importContext();
            return contextCommands.useCommand.run(ctx);
          },
        }),
        current: defineCommand({
          meta: { name: "current", description: "Show current context" },
          async run(ctx) {
            const contextCommands = await importContext();
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
