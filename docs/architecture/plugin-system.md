# UNRDF CLI Plugin System

## Executive Summary

This document specifies the plugin architecture and SDK for UNRDF CLI, enabling third-party extensions for custom commands, formatters, middleware, and integrations.

## Plugin Architecture

### Plugin Lifecycle

```
┌─────────────────────────────────────────────────────────────┐
│                    Plugin Lifecycle                         │
│                                                             │
│  Discovery → Load → Validate → Initialize → Register       │
│                                                             │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐  │
│  │Discovery │→ │  Loader  │→ │Validator │→ │ Registry │  │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘  │
│                                                             │
│  Execution → Cleanup                                        │
│                                                             │
│  ┌──────────┐  ┌──────────┐                               │
│  │ Execute  │→ │ Cleanup  │                               │
│  └──────────┘  └──────────┘                               │
└─────────────────────────────────────────────────────────────┘
```

### Plugin Types

1. **Command Plugins**: Add new commands or extend existing ones
2. **Formatter Plugins**: Add output formatters
3. **Middleware Plugins**: Add request/response middleware
4. **Integration Plugins**: Integrate with external systems
5. **Hook Plugins**: Add custom hook types

## Plugin Interface

### Plugin Manifest Schema

```javascript
// plugin-manifest.mjs
import { z } from 'zod';

export const PluginManifestSchema = z.object({
  // Metadata
  name: z.string().min(3).max(50),
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  description: z.string().max(500),
  author: z.string().optional(),
  license: z.string().default('MIT'),
  homepage: z.string().url().optional(),
  repository: z.string().url().optional(),

  // Dependencies
  requires: z.object({
    unrdf: z.string(), // Semver range
    node: z.string().optional(),
    plugins: z.record(z.string()).optional()
  }),

  // Plugin type and capabilities
  type: z.enum(['command', 'formatter', 'middleware', 'integration', 'hook']),

  capabilities: z.object({
    commands: z.array(z.object({
      noun: z.string(),
      verb: z.string(),
      description: z.string(),
      handler: z.string() // Path to handler
    })).optional(),

    formatters: z.record(z.string()).optional(), // name → path

    middleware: z.array(z.object({
      name: z.string(),
      handler: z.string(),
      priority: z.number().default(50)
    })).optional(),

    hookTypes: z.array(z.string()).optional()
  }),

  // Lifecycle hooks
  hooks: z.object({
    init: z.string().optional(),
    register: z.string(),
    cleanup: z.string().optional(),
    upgrade: z.string().optional()
  }),

  // Configuration schema
  config: z.record(z.any()).optional(),

  // Permissions required
  permissions: z.array(z.enum([
    'read:graph',
    'write:graph',
    'read:hooks',
    'write:hooks',
    'read:policies',
    'write:policies',
    'sidecar:connect',
    'filesystem:read',
    'filesystem:write',
    'network:outbound'
  ])).optional()
});
```

### Plugin Base Class

```javascript
// src/cli/plugin-system/plugin-base.mjs
export class Plugin {
  constructor(manifest, config = {}) {
    this.manifest = PluginManifestSchema.parse(manifest);
    this.config = config;
    this.logger = createLogger(`plugin:${this.manifest.name}`);
  }

  // Lifecycle: Initialize plugin
  async init(cliContext) {
    this.logger.info('Initializing plugin');
    // Override in subclass
  }

  // Lifecycle: Register plugin capabilities
  async register(router, pluginManager) {
    this.logger.info('Registering plugin');

    // Register commands
    if (this.manifest.capabilities.commands) {
      for (const cmd of this.manifest.capabilities.commands) {
        await this.registerCommand(router, cmd);
      }
    }

    // Register formatters
    if (this.manifest.capabilities.formatters) {
      for (const [name, path] of Object.entries(this.manifest.capabilities.formatters)) {
        await this.registerFormatter(pluginManager, name, path);
      }
    }

    // Register middleware
    if (this.manifest.capabilities.middleware) {
      for (const mw of this.manifest.capabilities.middleware) {
        await this.registerMiddleware(router, mw);
      }
    }
  }

  // Lifecycle: Cleanup
  async cleanup() {
    this.logger.info('Cleaning up plugin');
    // Override in subclass
  }

  // Helper: Register command
  async registerCommand(router, command) {
    const handler = await this.loadHandler(command.handler);

    router.register(command.noun, command.verb, handler, {
      meta: {
        description: command.description,
        plugin: this.manifest.name
      }
    });

    this.logger.info(`Registered command: ${command.noun} ${command.verb}`);
  }

  // Helper: Register formatter
  async registerFormatter(pluginManager, name, path) {
    const formatter = await this.loadHandler(path);
    pluginManager.registerFormatter(name, formatter);
    this.logger.info(`Registered formatter: ${name}`);
  }

  // Helper: Register middleware
  async registerMiddleware(router, middleware) {
    const handler = await this.loadHandler(middleware.handler);

    router.use(handler, {
      name: middleware.name,
      priority: middleware.priority,
      plugin: this.manifest.name
    });

    this.logger.info(`Registered middleware: ${middleware.name}`);
  }

  // Helper: Load handler module
  async loadHandler(path) {
    const fullPath = `${this.getPluginPath()}/${path}`;
    const module = await import(fullPath);
    return module.default || module;
  }

  // Get plugin installation path
  getPluginPath() {
    return `~/.unrdf/plugins/${this.manifest.name}`;
  }
}
```

## Plugin SDK

### Command Plugin Example

```javascript
// unrdf-plugin-prometheus/index.mjs
import { Plugin } from 'unrdf/plugin-sdk';

export default class PrometheusPlugin extends Plugin {
  async init(cliContext) {
    this.client = new PrometheusClient(this.config.endpoint);
  }

  async register(router, pluginManager) {
    // Register 'metrics' noun
    router.register('metrics', 'export', this.exportMetrics.bind(this), {
      schema: z.object({
        format: z.enum(['prometheus', 'json']).default('prometheus'),
        output: z.string().optional()
      }),
      meta: {
        description: 'Export metrics in Prometheus format'
      }
    });

    router.register('metrics', 'push', this.pushMetrics.bind(this), {
      schema: z.object({
        gateway: z.string().url()
      }),
      meta: {
        description: 'Push metrics to Prometheus gateway'
      }
    });

    // Add middleware to track metrics
    router.use(this.metricsMiddleware.bind(this), {
      name: 'prometheus-metrics',
      priority: 10
    });
  }

  async exportMetrics(ctx, args, flags) {
    const metrics = await this.client.collectMetrics();

    if (flags.format === 'prometheus') {
      return this.formatPrometheus(metrics);
    } else {
      return metrics;
    }
  }

  async pushMetrics(ctx, args, flags) {
    const metrics = await this.client.collectMetrics();
    await this.client.push(flags.gateway, metrics);

    return {
      status: 'success',
      pushed: metrics.length
    };
  }

  async metricsMiddleware(ctx) {
    const startTime = Date.now();

    try {
      const result = await ctx.next();

      // Record success metric
      this.client.recordCommand({
        noun: ctx.noun,
        verb: ctx.verb,
        status: 'success',
        duration: Date.now() - startTime
      });

      return result;

    } catch (error) {
      // Record error metric
      this.client.recordCommand({
        noun: ctx.noun,
        verb: ctx.verb,
        status: 'error',
        duration: Date.now() - startTime
      });

      throw error;
    }
  }

  formatPrometheus(metrics) {
    // Convert to Prometheus exposition format
    const lines = [];

    for (const metric of metrics) {
      lines.push(`# HELP ${metric.name} ${metric.help}`);
      lines.push(`# TYPE ${metric.name} ${metric.type}`);

      for (const sample of metric.samples) {
        const labels = Object.entries(sample.labels)
          .map(([k, v]) => `${k}="${v}"`)
          .join(',');

        lines.push(`${metric.name}{${labels}} ${sample.value}`);
      }
    }

    return lines.join('\n');
  }
}
```

### Formatter Plugin Example

```javascript
// unrdf-plugin-graphviz/index.mjs
import { Plugin } from 'unrdf/plugin-sdk';
import graphviz from 'graphviz';

export default class GraphvizPlugin extends Plugin {
  async register(router, pluginManager) {
    // Register formatter
    pluginManager.registerFormatter('dot', this.formatDot.bind(this));
    pluginManager.registerFormatter('svg', this.formatSvg.bind(this));
    pluginManager.registerFormatter('png', this.formatPng.bind(this));
  }

  async formatDot(data) {
    const g = graphviz.digraph('G');

    // Convert RDF quads to graph
    for (const quad of data) {
      g.addNode(quad.subject.value);
      g.addNode(quad.object.value);
      g.addEdge(quad.subject.value, quad.object.value, {
        label: quad.predicate.value
      });
    }

    return g.to_dot();
  }

  async formatSvg(data) {
    const dot = await this.formatDot(data);
    return await graphviz.dot(dot, 'svg');
  }

  async formatPng(data) {
    const dot = await this.formatDot(data);
    return await graphviz.dot(dot, 'png');
  }
}
```

### Integration Plugin Example

```javascript
// unrdf-plugin-slack/index.mjs
import { Plugin } from 'unrdf/plugin-sdk';
import { WebClient } from '@slack/web-api';

export default class SlackPlugin extends Plugin {
  async init(cliContext) {
    this.slack = new WebClient(this.config.token);
  }

  async register(router, pluginManager) {
    // Add 'notify' command
    router.register('slack', 'notify', this.notify.bind(this), {
      schema: z.object({
        message: z.string(),
        channel: z.string().optional()
      })
    });

    // Add middleware for notifications
    router.use(this.notifyMiddleware.bind(this), {
      name: 'slack-notifications',
      priority: 20
    });
  }

  async notify(ctx, args, flags) {
    const channel = flags.channel || this.config.defaultChannel;

    const result = await this.slack.chat.postMessage({
      channel,
      text: flags.message
    });

    return {
      status: 'sent',
      channel,
      ts: result.ts
    };
  }

  async notifyMiddleware(ctx) {
    try {
      const result = await ctx.next();

      // Notify on success for important operations
      if (this.shouldNotify(ctx)) {
        await this.notify(ctx, [], {
          message: `✅ ${ctx.noun} ${ctx.verb} succeeded`
        });
      }

      return result;

    } catch (error) {
      // Notify on error
      await this.notify(ctx, [], {
        message: `❌ ${ctx.noun} ${ctx.verb} failed: ${error.message}`
      });

      throw error;
    }
  }

  shouldNotify(ctx) {
    const importantOps = [
      'graph:delete',
      'policy:apply',
      'store:clear'
    ];

    return importantOps.includes(`${ctx.noun}:${ctx.verb}`);
  }
}
```

## Plugin Discovery & Installation

### Plugin Discovery

```javascript
// src/cli/plugin-system/discovery.mjs
export class PluginDiscovery {
  constructor(config) {
    this.config = config;
    this.searchPaths = [
      '~/.unrdf/plugins',
      './node_modules',
      process.env.UNRDF_PLUGINS?.split(':') || []
    ].flat();
  }

  // Discover plugins
  async discover() {
    const plugins = [];

    for (const searchPath of this.searchPaths) {
      const found = await this.scanDirectory(searchPath);
      plugins.push(...found);
    }

    return plugins;
  }

  async scanDirectory(dir) {
    const plugins = [];

    try {
      const entries = await fs.readdir(dir);

      for (const entry of entries) {
        // Check for plugin pattern
        if (entry.startsWith('unrdf-plugin-')) {
          const pluginPath = `${dir}/${entry}`;
          const manifest = await this.loadManifest(pluginPath);

          if (manifest) {
            plugins.push({
              path: pluginPath,
              manifest
            });
          }
        }
      }
    } catch (error) {
      // Directory doesn't exist or not readable
    }

    return plugins;
  }

  async loadManifest(pluginPath) {
    try {
      const manifestPath = `${pluginPath}/plugin.json`;
      const content = await fs.readFile(manifestPath, 'utf-8');
      return JSON.parse(content);
    } catch (error) {
      return null;
    }
  }
}
```

### Plugin Installation

```javascript
// src/cli/plugin-system/installer.mjs
export class PluginInstaller {
  constructor(config) {
    this.config = config;
    this.pluginDir = '~/.unrdf/plugins';
  }

  // Install plugin from npm
  async install(packageName, options = {}) {
    console.log(`Installing plugin: ${packageName}...`);

    // Download from npm
    await this.downloadPackage(packageName, options.version);

    // Load and validate manifest
    const manifest = await this.loadManifest(packageName);
    PluginManifestSchema.parse(manifest);

    // Check dependencies
    await this.checkDependencies(manifest);

    // Run init hook if present
    if (manifest.hooks.init) {
      await this.runHook(packageName, manifest.hooks.init);
    }

    console.log(`✅ Plugin installed: ${packageName}`);

    return {
      name: packageName,
      version: manifest.version,
      status: 'installed'
    };
  }

  // Uninstall plugin
  async uninstall(pluginName) {
    const pluginPath = `${this.pluginDir}/${pluginName}`;

    // Load manifest
    const manifest = await this.loadManifest(pluginName);

    // Run cleanup hook
    if (manifest.hooks.cleanup) {
      await this.runHook(pluginName, manifest.hooks.cleanup);
    }

    // Remove plugin directory
    await fs.rm(pluginPath, { recursive: true });

    console.log(`✅ Plugin uninstalled: ${pluginName}`);
  }

  async downloadPackage(packageName, version) {
    // npm install to plugin directory
    const targetDir = `${this.pluginDir}/${packageName}`;
    await fs.mkdir(targetDir, { recursive: true });

    const versionSpec = version ? `@${version}` : '';
    await exec(`npm install ${packageName}${versionSpec}`, {
      cwd: targetDir
    });
  }

  async checkDependencies(manifest) {
    // Check UNRDF version
    const unrdfVersion = await this.getUnrdfVersion();
    if (!semver.satisfies(unrdfVersion, manifest.requires.unrdf)) {
      throw new Error(
        `Plugin requires UNRDF ${manifest.requires.unrdf}, found ${unrdfVersion}`
      );
    }

    // Check plugin dependencies
    if (manifest.requires.plugins) {
      for (const [plugin, version] of Object.entries(manifest.requires.plugins)) {
        const installed = await this.isPluginInstalled(plugin);
        if (!installed) {
          throw new Error(`Missing plugin dependency: ${plugin}`);
        }
      }
    }
  }

  async loadManifest(pluginName) {
    const manifestPath = `${this.pluginDir}/${pluginName}/plugin.json`;
    const content = await fs.readFile(manifestPath, 'utf-8');
    return JSON.parse(content);
  }

  async runHook(pluginName, hookPath) {
    const fullPath = `${this.pluginDir}/${pluginName}/${hookPath}`;
    const hook = await import(fullPath);
    await hook.default();
  }

  async getUnrdfVersion() {
    // Get UNRDF CLI version
    return '2.0.0';
  }

  async isPluginInstalled(pluginName) {
    const pluginPath = `${this.pluginDir}/${pluginName}`;
    try {
      await fs.access(pluginPath);
      return true;
    } catch {
      return false;
    }
  }
}
```

## Plugin CLI Commands

### Plugin Management

```bash
# List installed plugins
unrdf plugin list

# Search for plugins
unrdf plugin search <query>

# Install plugin
unrdf plugin install <package>

# Uninstall plugin
unrdf plugin uninstall <name>

# Update plugin
unrdf plugin update <name>

# Enable plugin
unrdf plugin enable <name>

# Disable plugin
unrdf plugin disable <name>

# Show plugin info
unrdf plugin info <name>
```

## Plugin Development

### Creating a Plugin

```bash
# Initialize plugin project
npx create-unrdf-plugin my-plugin

# Project structure
my-plugin/
├── package.json
├── plugin.json           # Plugin manifest
├── src/
│   ├── index.mjs        # Main plugin class
│   ├── commands/
│   ├── formatters/
│   └── middleware/
├── test/
│   └── plugin.test.mjs
└── README.md
```

### Plugin Manifest (`plugin.json`)

```json
{
  "name": "unrdf-plugin-my-plugin",
  "version": "1.0.0",
  "description": "My awesome UNRDF plugin",
  "author": "Your Name",
  "license": "MIT",
  "type": "command",

  "requires": {
    "unrdf": "^2.0.0",
    "node": ">=18.0.0"
  },

  "capabilities": {
    "commands": [
      {
        "noun": "myplugin",
        "verb": "execute",
        "description": "Execute my plugin",
        "handler": "src/commands/execute.mjs"
      }
    ]
  },

  "hooks": {
    "register": "src/index.mjs"
  },

  "permissions": [
    "read:graph",
    "sidecar:connect"
  ]
}
```

### Plugin Testing

```javascript
// test/plugin.test.mjs
import { describe, it, expect } from 'vitest';
import { createTestContext, executeCommand } from 'unrdf/plugin-test-utils';

describe('MyPlugin', () => {
  it('should execute command', async () => {
    const ctx = createTestContext({
      plugins: ['my-plugin']
    });

    const result = await executeCommand(ctx, 'myplugin execute');

    expect(result.status).toBe('success');
  });
});
```

### Publishing Plugin

```bash
# Test locally
npm link
unrdf plugin install $(pwd)

# Publish to npm
npm publish

# Install from npm
unrdf plugin install unrdf-plugin-my-plugin
```

## Security Model

### Permission System

```javascript
// Plugin permissions checked at runtime
const permissions = [
  'read:graph',      // Read graph data
  'write:graph',     // Modify graph data
  'read:hooks',      // Read hook definitions
  'write:hooks',     // Create/modify hooks
  'read:policies',   // Read policy packs
  'write:policies',  // Create/modify policies
  'sidecar:connect', // Connect to sidecar
  'filesystem:read', // Read files
  'filesystem:write',// Write files
  'network:outbound' // Make network requests
];
```

### Sandboxing

Plugins run in isolated contexts with:
- Limited filesystem access
- Network restrictions
- Memory limits
- CPU quotas
- Timeout enforcement

## Plugin Registry

### Official Plugins

1. **unrdf-plugin-prometheus** - Metrics export
2. **unrdf-plugin-graphviz** - Graph visualization
3. **unrdf-plugin-slack** - Slack notifications
4. **unrdf-plugin-github** - GitHub integration
5. **unrdf-plugin-jupyter** - Jupyter notebook support

### Community Plugins

Browse at: https://plugins.unrdf.org

## Conclusion

The UNRDF plugin system provides:
- **Extensibility**: Add custom commands and features
- **Type Safety**: Zod-validated manifests
- **Security**: Permission-based access control
- **Discoverability**: Automatic plugin discovery
- **Ecosystem**: npm-based plugin distribution

**Status**: ✅ PLUGIN SYSTEM COMPLETE
