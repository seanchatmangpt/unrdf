# UNRDF Enterprise CLI Architecture

## Executive Summary

This document specifies the technical architecture for UNRDF's enterprise-grade noun-verb CLI, designed to scale from development to production with comprehensive KGC sidecar integration, plugin extensibility, and industry-standard patterns.

## Architectural Overview

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         UNRDF CLI v2                            │
│                                                                 │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │
│  │ Command      │  │ Plugin       │  │ Context      │         │
│  │ Router       │  │ System       │  │ Manager      │         │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘         │
│         │                  │                  │                 │
│  ┌──────▼──────────────────▼──────────────────▼───────┐        │
│  │           Command Execution Pipeline               │        │
│  └──────┬──────────────────┬──────────────────┬───────┘        │
│         │                  │                  │                 │
│  ┌──────▼───────┐  ┌──────▼───────┐  ┌──────▼───────┐         │
│  │ Input        │  │ Output       │  │ Error        │         │
│  │ Validation   │  │ Formatting   │  │ Handling     │         │
│  └──────────────┘  └──────────────┘  └──────────────┘         │
└─────────────────────────────────────────────────────────────────┘
                            │
            ┌───────────────┼───────────────┐
            │               │               │
    ┌───────▼────┐  ┌───────▼────┐  ┌──────▼──────┐
    │ KGC        │  │ Knowledge  │  │ Store       │
    │ Sidecar    │  │ Engine     │  │ Backend     │
    └────────────┘  └────────────┘  └─────────────┘
```

## Core Architecture Components

### 1. Command Router

**Purpose**: Route commands to appropriate handlers based on noun-verb patterns

**Structure**:
```javascript
// src/cli/router.mjs
import { z } from 'zod';

const CommandRouteSchema = z.object({
  noun: z.enum(['graph', 'hook', 'policy', 'sidecar', 'store', 'context', 'transaction', 'lockchain', 'config']),
  verb: z.enum(['list', 'get', 'create', 'update', 'delete', 'apply', ...]),
  args: z.array(z.string()),
  flags: z.record(z.any())
});

export class CommandRouter {
  constructor(config) {
    this.routes = new Map();
    this.middlewares = [];
    this.config = config;
  }

  // Register route: noun × verb → handler
  register(noun, verb, handler, options = {}) {
    const key = `${noun}:${verb}`;
    this.routes.set(key, {
      handler,
      schema: options.schema,
      middleware: options.middleware || [],
      meta: options.meta || {}
    });
  }

  // Execute command
  async execute(command) {
    const validated = CommandRouteSchema.parse(command);
    const key = `${validated.noun}:${validated.verb}`;
    const route = this.routes.get(key);

    if (!route) {
      throw new CLIError(`Unknown command: ${noun} ${verb}`);
    }

    // Execute middleware chain
    const ctx = await this.executeMiddleware(route.middleware, validated);

    // Execute handler
    return await route.handler(ctx, validated.args, validated.flags);
  }

  // Middleware execution
  async executeMiddleware(middlewares, command) {
    let ctx = { command, config: this.config };
    for (const mw of middlewares) {
      ctx = await mw(ctx);
    }
    return ctx;
  }
}
```

**Key Features**:
- Type-safe routing with Zod validation
- Middleware chain execution
- Dynamic route registration
- Plugin-friendly architecture
- Error propagation

### 2. Plugin System

**Purpose**: Enable extensibility through third-party commands and features

**Plugin Interface**:
```javascript
// src/cli/plugin-system.mjs
export const PluginSchema = z.object({
  name: z.string(),
  version: z.string(),
  description: z.string(),
  author: z.string().optional(),

  // Plugin lifecycle hooks
  hooks: z.object({
    init: z.function().optional(),
    register: z.function(),
    cleanup: z.function().optional()
  }),

  // Plugin capabilities
  provides: z.object({
    commands: z.array(z.object({
      noun: z.string(),
      verb: z.string(),
      handler: z.function(),
      schema: z.any().optional()
    })).optional(),

    middleware: z.array(z.object({
      name: z.string(),
      handler: z.function()
    })).optional(),

    formatters: z.record(z.function()).optional(),

    completions: z.record(z.function()).optional()
  })
});

export class PluginManager {
  constructor(config) {
    this.plugins = new Map();
    this.config = config;
  }

  // Load plugin from package
  async load(pluginPath) {
    const plugin = await import(pluginPath);
    const validated = PluginSchema.parse(plugin.default);

    // Initialize plugin
    if (validated.hooks.init) {
      await validated.hooks.init(this.config);
    }

    this.plugins.set(validated.name, validated);
    return validated;
  }

  // Register plugin with router
  async register(router) {
    for (const [name, plugin] of this.plugins) {
      await plugin.hooks.register(router, this.config);
    }
  }

  // Cleanup plugins
  async cleanup() {
    for (const [name, plugin] of this.plugins) {
      if (plugin.hooks.cleanup) {
        await plugin.hooks.cleanup();
      }
    }
  }
}
```

**Plugin Discovery**:
```javascript
// Load plugins from:
// 1. ~/.unrdf/plugins/
// 2. ./node_modules/unrdf-plugin-*/
// 3. Environment: UNRDF_PLUGINS=/path/to/plugin
```

### 3. Context Manager

**Purpose**: Manage CLI contexts for multi-environment workflows (like kubeconfig)

**Context Structure**:
```javascript
// src/cli/context-manager.mjs
export const ContextSchema = z.object({
  name: z.string(),

  // Sidecar connection
  sidecar: z.object({
    host: z.string(),
    port: z.number(),
    protocol: z.enum(['http', 'grpc']).default('grpc'),
    tls: z.boolean().default(false),
    cert: z.string().optional()
  }).optional(),

  // Store configuration
  store: z.object({
    type: z.enum(['memory', 'file', 'oxigraph', 'graphdb']),
    connection: z.string().optional()
  }).optional(),

  // Default settings
  defaults: z.object({
    format: z.enum(['json', 'yaml', 'table', 'tree']).default('table'),
    verbose: z.boolean().default(false),
    timeout: z.number().default(30000)
  }).optional(),

  // Authentication
  auth: z.object({
    type: z.enum(['none', 'token', 'oauth']),
    credentials: z.record(z.string())
  }).optional()
});

export const ContextConfigSchema = z.object({
  contexts: z.array(ContextSchema),
  current: z.string()
});

export class ContextManager {
  constructor(configPath = '~/.unrdf/config') {
    this.configPath = configPath;
    this.config = null;
  }

  // Load configuration
  async load() {
    const data = await fs.readFile(this.configPath, 'utf-8');
    this.config = ContextConfigSchema.parse(JSON.parse(data));
    return this.config;
  }

  // Save configuration
  async save() {
    await fs.writeFile(
      this.configPath,
      JSON.stringify(this.config, null, 2),
      'utf-8'
    );
  }

  // Get current context
  getCurrent() {
    const ctx = this.config.contexts.find(
      c => c.name === this.config.current
    );
    if (!ctx) {
      throw new CLIError('No current context set');
    }
    return ctx;
  }

  // Switch context
  async use(name) {
    const ctx = this.config.contexts.find(c => c.name === name);
    if (!ctx) {
      throw new CLIError(`Context not found: ${name}`);
    }
    this.config.current = name;
    await this.save();
  }

  // Create context
  async create(name, settings) {
    const ctx = ContextSchema.parse({ name, ...settings });
    this.config.contexts.push(ctx);
    await this.save();
    return ctx;
  }

  // Delete context
  async delete(name) {
    this.config.contexts = this.config.contexts.filter(
      c => c.name !== name
    );
    if (this.config.current === name) {
      this.config.current = this.config.contexts[0]?.name;
    }
    await this.save();
  }
}
```

**Context File Format** (`~/.unrdf/config`):
```yaml
contexts:
  - name: development
    sidecar:
      host: localhost
      port: 50051
      protocol: grpc
    store:
      type: memory
    defaults:
      format: json
      verbose: true

  - name: production
    sidecar:
      host: kgc-sidecar.prod.svc.cluster.local
      port: 50051
      protocol: grpc
      tls: true
      cert: /etc/tls/ca.crt
    store:
      type: graphdb
      connection: http://graphdb:7200/repositories/prod
    defaults:
      format: table
      verbose: false
    auth:
      type: token
      credentials:
        token: ${UNRDF_TOKEN}

current: development
```

### 4. Configuration Hierarchy

**Purpose**: Manage configuration from multiple sources with precedence

**Hierarchy** (highest to lowest precedence):
1. **Command-line flags**: `--format=json`
2. **Environment variables**: `UNRDF_FORMAT=json`
3. **Project config**: `./unrdf.config.mjs`
4. **Context config**: `~/.unrdf/config`
5. **Global defaults**: Built-in defaults

**Implementation**:
```javascript
// src/cli/config-loader.mjs
export class ConfigLoader {
  constructor() {
    this.sources = [];
  }

  // Load configuration from all sources
  async load() {
    const sources = await Promise.all([
      this.loadDefaults(),
      this.loadGlobalConfig(),
      this.loadProjectConfig(),
      this.loadEnvironment(),
      this.loadCommandFlags()
    ]);

    // Merge with precedence
    return sources.reduce((acc, src) => ({
      ...acc,
      ...src
    }), {});
  }

  async loadDefaults() {
    return {
      format: 'table',
      verbose: false,
      timeout: 30000,
      maxConcurrency: 10
    };
  }

  async loadGlobalConfig() {
    const contextMgr = new ContextManager();
    await contextMgr.load();
    const ctx = contextMgr.getCurrent();
    return ctx.defaults || {};
  }

  async loadProjectConfig() {
    const projectConfig = './unrdf.config.mjs';
    if (await exists(projectConfig)) {
      return (await import(projectConfig)).default;
    }
    return {};
  }

  loadEnvironment() {
    return {
      format: process.env.UNRDF_FORMAT,
      verbose: process.env.UNRDF_VERBOSE === 'true',
      sidecarHost: process.env.UNRDF_SIDECAR_HOST,
      sidecarPort: parseInt(process.env.UNRDF_SIDECAR_PORT || '50051')
    };
  }

  loadCommandFlags() {
    // Loaded from CLI parser
    return this.flags || {};
  }
}
```

### 5. Output Format System

**Purpose**: Support multiple output formats with streaming

**Supported Formats**:
- `json` - JSON output
- `yaml` - YAML output
- `table` - ASCII table (default)
- `tree` - Tree structure
- `csv` - Comma-separated values
- `turtle` - RDF Turtle
- `nquads` - RDF N-Quads
- `jsonld` - JSON-LD

**Implementation**:
```javascript
// src/cli/formatters.mjs
export const Formatters = {
  json: (data, options = {}) => {
    return JSON.stringify(data, null, options.indent || 2);
  },

  yaml: async (data) => {
    const yaml = await import('yaml');
    return yaml.stringify(data);
  },

  table: (data, options = {}) => {
    if (!Array.isArray(data)) {
      data = [data];
    }

    const headers = options.columns || Object.keys(data[0] || {});
    const colWidths = headers.map(h =>
      Math.max(
        h.length,
        ...data.map(r => String(r[h] || '').length)
      )
    );

    const rows = [
      headers.map((h, i) => h.padEnd(colWidths[i])).join(' | '),
      headers.map((_, i) => '─'.repeat(colWidths[i])).join('─┼─'),
      ...data.map(row =>
        headers.map((h, i) =>
          String(row[h] || '').padEnd(colWidths[i])
        ).join(' | ')
      )
    ];

    return rows.join('\n');
  },

  tree: (data, options = {}) => {
    const indent = options.indent || 0;
    const prefix = '  '.repeat(indent);

    if (typeof data !== 'object') {
      return `${prefix}${data}`;
    }

    if (Array.isArray(data)) {
      return data.map((item, i) =>
        `${prefix}├─ [${i}]\n${Formatters.tree(item, {
          ...options,
          indent: indent + 1
        })}`
      ).join('\n');
    }

    return Object.entries(data).map(([key, val]) =>
      `${prefix}├─ ${key}: ${
        typeof val === 'object'
          ? '\n' + Formatters.tree(val, { ...options, indent: indent + 1 })
          : val
      }`
    ).join('\n');
  },

  csv: (data) => {
    if (!Array.isArray(data)) {
      data = [data];
    }

    const headers = Object.keys(data[0] || {});
    return [
      headers.join(','),
      ...data.map(row =>
        headers.map(h => {
          const val = row[h] || '';
          return typeof val === 'string' && val.includes(',')
            ? `"${val}"`
            : val;
        }).join(',')
      )
    ].join('\n');
  },

  turtle: async (quads) => {
    const { Writer } = await import('n3');
    return new Promise((resolve, reject) => {
      const writer = new Writer({ format: 'Turtle' });
      writer.addQuads(quads);
      writer.end((error, result) => {
        if (error) reject(error);
        else resolve(result);
      });
    });
  },

  nquads: async (quads) => {
    const { Writer } = await import('n3');
    return new Promise((resolve, reject) => {
      const writer = new Writer({ format: 'N-Quads' });
      writer.addQuads(quads);
      writer.end((error, result) => {
        if (error) reject(error);
        else resolve(result);
      });
    });
  },

  jsonld: async (quads) => {
    const jsonld = await import('jsonld');
    // Convert quads to JSON-LD
    // ... implementation
  }
};

export class OutputFormatter {
  constructor(format = 'table') {
    this.format = format;
  }

  async format(data, options = {}) {
    const formatter = Formatters[this.format];
    if (!formatter) {
      throw new Error(`Unknown format: ${this.format}`);
    }
    return await formatter(data, options);
  }

  // Stream large datasets
  async *stream(dataStream, options = {}) {
    for await (const chunk of dataStream) {
      yield await this.format(chunk, options);
    }
  }
}
```

### 6. Error Handling System

**Purpose**: Standardized error handling with exit codes

**Error Hierarchy**:
```javascript
// src/cli/errors.mjs
export class CLIError extends Error {
  constructor(message, exitCode = 1) {
    super(message);
    this.name = 'CLIError';
    this.exitCode = exitCode;
  }
}

export class ValidationError extends CLIError {
  constructor(message, errors) {
    super(message, 2);
    this.name = 'ValidationError';
    this.errors = errors;
  }
}

export class ConnectionError extends CLIError {
  constructor(message, target) {
    super(message, 3);
    this.name = 'ConnectionError';
    this.target = target;
  }
}

export class NotFoundError extends CLIError {
  constructor(resource, identifier) {
    super(`${resource} not found: ${identifier}`, 4);
    this.name = 'NotFoundError';
    this.resource = resource;
    this.identifier = identifier;
  }
}

export class PermissionError extends CLIError {
  constructor(message) {
    super(message, 5);
    this.name = 'PermissionError';
  }
}

// Exit codes
export const ExitCodes = {
  SUCCESS: 0,
  GENERIC_ERROR: 1,
  VALIDATION_ERROR: 2,
  CONNECTION_ERROR: 3,
  NOT_FOUND: 4,
  PERMISSION_DENIED: 5,
  TIMEOUT: 6,
  INTERRUPTED: 130
};

// Error handler
export class ErrorHandler {
  constructor(verbose = false) {
    this.verbose = verbose;
  }

  handle(error) {
    if (error instanceof CLIError) {
      console.error(`Error: ${error.message}`);

      if (this.verbose && error.stack) {
        console.error(error.stack);
      }

      if (error instanceof ValidationError && error.errors) {
        console.error('Validation errors:');
        error.errors.forEach(e => console.error(`  - ${e.message}`));
      }

      process.exit(error.exitCode);
    } else {
      console.error('Unexpected error:', error.message);
      if (this.verbose && error.stack) {
        console.error(error.stack);
      }
      process.exit(ExitCodes.GENERIC_ERROR);
    }
  }
}
```

### 7. Command Execution Pipeline

**Purpose**: Unified command execution with middleware support

**Pipeline Stages**:
1. **Parse**: Parse command-line arguments
2. **Validate**: Validate input with Zod schemas
3. **Auth**: Authenticate and authorize
4. **Context**: Load and apply context
5. **Execute**: Execute command handler
6. **Format**: Format output
7. **Error**: Handle errors

**Implementation**:
```javascript
// src/cli/pipeline.mjs
export class CommandPipeline {
  constructor(config) {
    this.config = config;
    this.stages = [
      this.parseStage,
      this.validateStage,
      this.authStage,
      this.contextStage,
      this.executeStage,
      this.formatStage
    ];
  }

  async run(argv) {
    let ctx = { argv, config: this.config };

    try {
      for (const stage of this.stages) {
        ctx = await stage.call(this, ctx);
      }

      // Output result
      console.log(ctx.output);
      process.exit(0);

    } catch (error) {
      const errorHandler = new ErrorHandler(ctx.verbose);
      errorHandler.handle(error);
    }
  }

  async parseStage(ctx) {
    // Parse command-line arguments
    const parsed = parseArgs(ctx.argv);
    return { ...ctx, ...parsed };
  }

  async validateStage(ctx) {
    // Validate with Zod schemas
    const route = this.router.getRoute(ctx.noun, ctx.verb);
    if (route.schema) {
      const validated = route.schema.parse(ctx.args);
      return { ...ctx, validated };
    }
    return ctx;
  }

  async authStage(ctx) {
    // Authenticate if required
    if (ctx.auth?.required) {
      const token = await this.authenticate(ctx);
      return { ...ctx, token };
    }
    return ctx;
  }

  async contextStage(ctx) {
    // Load and apply context
    const contextMgr = new ContextManager();
    await contextMgr.load();
    const context = contextMgr.getCurrent();
    return { ...ctx, context };
  }

  async executeStage(ctx) {
    // Execute command
    const result = await this.router.execute({
      noun: ctx.noun,
      verb: ctx.verb,
      args: ctx.args,
      flags: ctx.flags
    });
    return { ...ctx, result };
  }

  async formatStage(ctx) {
    // Format output
    const formatter = new OutputFormatter(ctx.format || 'table');
    const output = await formatter.format(ctx.result);
    return { ...ctx, output };
  }
}
```

## Module Structure

```
src/cli/
├── index.mjs                 # CLI entry point
├── router.mjs                # Command router
├── pipeline.mjs              # Execution pipeline
├── context-manager.mjs       # Context management
├── config-loader.mjs         # Configuration loader
├── plugin-system.mjs         # Plugin manager
├── formatters.mjs            # Output formatters
├── errors.mjs                # Error handling
│
├── commands/                 # Command handlers
│   ├── graph/
│   │   ├── list.mjs
│   │   ├── get.mjs
│   │   ├── create.mjs
│   │   └── ...
│   ├── hook/
│   ├── policy/
│   ├── sidecar/
│   ├── store/
│   ├── context/
│   ├── transaction/
│   └── lockchain/
│
├── middleware/               # Middleware functions
│   ├── auth.mjs
│   ├── logging.mjs
│   ├── telemetry.mjs
│   └── rate-limiting.mjs
│
├── utils/                    # Utility functions
│   ├── parser.mjs
│   ├── validator.mjs
│   ├── logger.mjs
│   └── spinner.mjs
│
└── plugins/                  # Built-in plugins
    ├── completion/
    ├── interactive/
    └── debug/
```

## Design Patterns

### 1. Command Handler Pattern
```javascript
// src/cli/commands/graph/list.mjs
export const GraphListHandler = {
  schema: z.object({
    filter: z.string().optional(),
    limit: z.number().default(100),
    offset: z.number().default(0)
  }),

  async execute(ctx, args, flags) {
    const { filter, limit, offset } = flags;

    // Get graphs from sidecar
    const client = ctx.sidecarClient;
    const graphs = await client.listGraphs({
      filter,
      limit,
      offset
    });

    return graphs;
  },

  meta: {
    description: 'List all knowledge graphs',
    examples: [
      'unrdf graph list',
      'unrdf graph list --filter=prod',
      'unrdf graph list --limit=10'
    ]
  }
};
```

### 2. Middleware Pattern
```javascript
// src/cli/middleware/auth.mjs
export const authMiddleware = async (ctx) => {
  if (!ctx.context.auth) {
    return ctx;
  }

  const { type, credentials } = ctx.context.auth;

  switch (type) {
    case 'token':
      const token = credentials.token || process.env.UNRDF_TOKEN;
      if (!token) {
        throw new PermissionError('No authentication token');
      }
      ctx.authToken = token;
      break;

    case 'oauth':
      // OAuth flow
      break;
  }

  return ctx;
};
```

### 3. Plugin Pattern
```javascript
// Example plugin: unrdf-plugin-prometheus
export default {
  name: 'prometheus',
  version: '1.0.0',
  description: 'Prometheus metrics export',

  hooks: {
    async register(router, config) {
      // Register new commands
      router.register('metrics', 'export', async (ctx) => {
        const metrics = await collectMetrics();
        return metrics;
      });

      // Add middleware
      router.use(async (ctx) => {
        // Record metrics
        await recordCommandMetric(ctx.noun, ctx.verb);
        return ctx;
      });
    }
  },

  provides: {
    commands: [
      {
        noun: 'metrics',
        verb: 'export',
        handler: metricsExportHandler
      }
    ],

    formatters: {
      prometheus: prometheusFormatter
    }
  }
};
```

## Technology Stack

### Core Dependencies
- **Commander/Citty**: Command-line parsing
- **Zod**: Input validation and schemas
- **unctx**: Context management
- **chalk**: Terminal styling
- **ora**: Loading spinners
- **inquirer**: Interactive prompts
- **table**: ASCII table formatting
- **yaml**: YAML parsing/serialization

### Optional Dependencies
- **@opentelemetry/api**: Telemetry
- **pino**: Structured logging
- **winston**: Alternative logging
- **grpc**: Sidecar communication

## Performance Targets

| Metric | Target | Rationale |
|--------|--------|-----------|
| Cold start | < 100ms | Responsive CLI |
| Warm start (cached) | < 50ms | Snappy UX |
| Command execution | < 2s | Interactive feel |
| Large output (10k rows) | < 500ms | Efficient display |
| Plugin load | < 200ms | Fast extensibility |

## Security Model

### 1. Credential Management
- Store credentials in `~/.unrdf/credentials` (encrypted)
- Support keychain integration (macOS/Linux)
- Environment variable fallback
- Never log credentials

### 2. Input Validation
- Validate all inputs with Zod schemas
- Sanitize file paths
- Prevent command injection
- Rate limiting on expensive operations

### 3. Least Privilege
- Read-only by default
- Destructive operations require confirmation
- Support for RBAC via sidecar
- Audit logging

## Observability

### 1. Logging
```javascript
// Structured logging with pino
const logger = pino({
  level: process.env.LOG_LEVEL || 'info',
  transport: {
    target: 'pino-pretty',
    options: { colorize: true }
  }
});

logger.info({ noun: 'graph', verb: 'list' }, 'Executing command');
```

### 2. Telemetry
```javascript
// OpenTelemetry integration
const tracer = trace.getTracer('unrdf-cli');

const span = tracer.startSpan('graph.list');
span.setAttribute('filter', filter);
try {
  const result = await execute();
  span.setStatus({ code: SpanStatusCode.OK });
  return result;
} catch (error) {
  span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
  throw error;
} finally {
  span.end();
}
```

### 3. Metrics
```javascript
// Prometheus metrics
const commandCounter = new Counter({
  name: 'unrdf_commands_total',
  help: 'Total number of commands executed',
  labelNames: ['noun', 'verb', 'status']
});

const commandDuration = new Histogram({
  name: 'unrdf_command_duration_seconds',
  help: 'Command execution duration',
  labelNames: ['noun', 'verb']
});
```

## Conclusion

This architecture provides a scalable, maintainable foundation for UNRDF's enterprise CLI with:
- **Extensibility**: Plugin system for custom commands
- **Flexibility**: Context management for multi-environment workflows
- **Performance**: Optimized execution pipeline
- **Security**: Comprehensive credential and input validation
- **Observability**: Full telemetry and logging integration

The design follows industry best practices while accommodating UNRDF's unique knowledge graph requirements.

**Status**: ✅ ARCHITECTURE COMPLETE - Ready for sidecar integration design
