# Enterprise Features Design

## Executive Summary

This document specifies enterprise-grade features for UNRDF CLI including RBAC, audit logging, batch operations, watch/monitor modes, and interactive shell capabilities for production deployments.

## 1. Role-Based Access Control (RBAC)

### RBAC Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      UNRDF CLI RBAC                         │
│                                                             │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐  │
│  │  User    │  │  Role    │  │Permission│  │Resource  │  │
│  │  Auth    │  │  Manager │  │ Checker  │  │  Policy  │  │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘  │
│       │             │              │             │         │
│  ┌────▼─────────────▼──────────────▼─────────────▼─────┐  │
│  │            RBAC Enforcement Layer                    │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### Role Definition Schema

```javascript
// src/cli/rbac/roles.mjs
import { z } from 'zod';

export const PermissionSchema = z.object({
  resource: z.enum(['graph', 'hook', 'policy', 'store', 'transaction', 'lockchain', '*']),
  actions: z.array(z.enum(['read', 'write', 'delete', 'execute', '*'])),
  conditions: z.object({
    graphFilter: z.string().optional(),
    timeWindow: z.object({
      start: z.string(),
      end: z.string()
    }).optional(),
    ipRestriction: z.array(z.string()).optional()
  }).optional()
});

export const RoleSchema = z.object({
  name: z.string(),
  description: z.string(),
  permissions: z.array(PermissionSchema),
  inherits: z.array(z.string()).optional(),
  metadata: z.record(z.string()).optional()
});

// Predefined roles
export const PredefinedRoles = {
  admin: {
    name: 'admin',
    description: 'Full system access',
    permissions: [
      {
        resource: '*',
        actions: ['*']
      }
    ]
  },

  operator: {
    name: 'operator',
    description: 'Read/write access to graphs and queries',
    permissions: [
      {
        resource: 'graph',
        actions: ['read', 'write']
      },
      {
        resource: 'store',
        actions: ['read', 'execute']
      },
      {
        resource: 'hook',
        actions: ['read', 'execute']
      }
    ]
  },

  analyst: {
    name: 'analyst',
    description: 'Read-only access to data and queries',
    permissions: [
      {
        resource: 'graph',
        actions: ['read']
      },
      {
        resource: 'store',
        actions: ['read', 'execute']
      },
      {
        resource: 'transaction',
        actions: ['read']
      }
    ]
  },

  developer: {
    name: 'developer',
    description: 'Hook and policy development',
    permissions: [
      {
        resource: 'hook',
        actions: ['read', 'write', 'execute']
      },
      {
        resource: 'policy',
        actions: ['read', 'write']
      },
      {
        resource: 'graph',
        actions: ['read']
      }
    ]
  },

  auditor: {
    name: 'auditor',
    description: 'Audit trail access',
    permissions: [
      {
        resource: 'transaction',
        actions: ['read']
      },
      {
        resource: 'lockchain',
        actions: ['read']
      }
    ]
  }
};
```

### RBAC Manager

```javascript
// src/cli/rbac/manager.mjs
export class RBACManager {
  constructor(config) {
    this.config = config;
    this.roles = new Map();
    this.userRoles = new Map();
    this.loadPredefinedRoles();
  }

  loadPredefinedRoles() {
    for (const [name, role] of Object.entries(PredefinedRoles)) {
      this.roles.set(name, RoleSchema.parse(role));
    }
  }

  // Check if user has permission
  async checkPermission(user, resource, action, context = {}) {
    const userRoles = this.userRoles.get(user) || [];

    for (const roleName of userRoles) {
      const role = this.roles.get(roleName);
      if (!role) continue;

      // Check inherited roles
      if (role.inherits) {
        for (const inheritedRole of role.inherits) {
          if (await this.checkRolePermission(inheritedRole, resource, action, context)) {
            return true;
          }
        }
      }

      // Check direct permissions
      if (await this.checkRolePermission(roleName, resource, action, context)) {
        return true;
      }
    }

    return false;
  }

  async checkRolePermission(roleName, resource, action, context) {
    const role = this.roles.get(roleName);
    if (!role) return false;

    for (const permission of role.permissions) {
      // Check resource match
      if (permission.resource !== '*' && permission.resource !== resource) {
        continue;
      }

      // Check action match
      if (!permission.actions.includes('*') && !permission.actions.includes(action)) {
        continue;
      }

      // Check conditions
      if (permission.conditions) {
        if (!this.checkConditions(permission.conditions, context)) {
          continue;
        }
      }

      return true;
    }

    return false;
  }

  checkConditions(conditions, context) {
    // Check graph filter
    if (conditions.graphFilter && context.graph) {
      const regex = new RegExp(conditions.graphFilter);
      if (!regex.test(context.graph)) {
        return false;
      }
    }

    // Check time window
    if (conditions.timeWindow) {
      const now = new Date();
      const start = new Date(conditions.timeWindow.start);
      const end = new Date(conditions.timeWindow.end);
      if (now < start || now > end) {
        return false;
      }
    }

    // Check IP restriction
    if (conditions.ipRestriction && context.ip) {
      if (!conditions.ipRestriction.includes(context.ip)) {
        return false;
      }
    }

    return true;
  }

  // Assign role to user
  assignRole(user, roleName) {
    if (!this.roles.has(roleName)) {
      throw new Error(`Unknown role: ${roleName}`);
    }

    const userRoles = this.userRoles.get(user) || [];
    if (!userRoles.includes(roleName)) {
      userRoles.push(roleName);
      this.userRoles.set(user, userRoles);
    }
  }

  // Revoke role from user
  revokeRole(user, roleName) {
    const userRoles = this.userRoles.get(user) || [];
    const filtered = userRoles.filter(r => r !== roleName);
    this.userRoles.set(user, filtered);
  }

  // Get user roles
  getUserRoles(user) {
    return this.userRoles.get(user) || [];
  }

  // Create custom role
  createRole(roleDefinition) {
    const role = RoleSchema.parse(roleDefinition);
    this.roles.set(role.name, role);
    return role;
  }
}
```

### Permission Middleware

```javascript
// src/cli/middleware/rbac.mjs
export const rbacMiddleware = (requiredPermission) => {
  return async (ctx) => {
    const { resource, action } = requiredPermission;
    const user = ctx.user || 'anonymous';

    const rbac = new RBACManager(ctx.config.rbac);

    const hasPermission = await rbac.checkPermission(
      user,
      resource,
      action,
      {
        graph: ctx.graph,
        ip: ctx.ip
      }
    );

    if (!hasPermission) {
      throw new PermissionError(
        `User ${user} does not have ${action} permission on ${resource}`
      );
    }

    return ctx;
  };
};
```

## 2. Audit Logging

### Audit Log Schema

```javascript
// src/cli/audit/schema.mjs
export const AuditLogSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.string().datetime(),
  user: z.string(),
  action: z.string(),
  resource: z.string(),
  resourceId: z.string().optional(),
  status: z.enum(['success', 'failure', 'error']),
  metadata: z.object({
    ip: z.string().optional(),
    userAgent: z.string().optional(),
    duration: z.number().optional(),
    error: z.string().optional()
  }).optional(),
  changes: z.object({
    before: z.any().optional(),
    after: z.any().optional()
  }).optional()
});
```

### Audit Logger

```javascript
// src/cli/audit/logger.mjs
import { appendFile } from 'node:fs/promises';
import { createHash } from 'node:crypto';

export class AuditLogger {
  constructor(config) {
    this.config = config;
    this.logPath = config.auditLogPath || '~/.unrdf/audit.log';
    this.buffer = [];
    this.flushInterval = config.flushInterval || 5000;
    this.startFlushTimer();
  }

  // Log audit event
  async log(event) {
    const auditLog = AuditLogSchema.parse({
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      ...event
    });

    this.buffer.push(auditLog);

    // Immediate flush for critical events
    if (event.status === 'error' || event.action.includes('delete')) {
      await this.flush();
    }
  }

  // Flush buffer to disk
  async flush() {
    if (this.buffer.length === 0) return;

    const logs = this.buffer.splice(0);
    const lines = logs.map(log =>
      JSON.stringify(log) + '\n'
    ).join('');

    await appendFile(this.logPath, lines);
  }

  startFlushTimer() {
    this.timer = setInterval(() => {
      this.flush().catch(console.error);
    }, this.flushInterval);
  }

  // Query audit logs
  async query(filter = {}) {
    const content = await fs.readFile(this.logPath, 'utf-8');
    const logs = content.split('\n')
      .filter(line => line.trim())
      .map(line => JSON.parse(line));

    return logs.filter(log => {
      if (filter.user && log.user !== filter.user) return false;
      if (filter.action && log.action !== filter.action) return false;
      if (filter.resource && log.resource !== filter.resource) return false;
      if (filter.status && log.status !== filter.status) return false;

      if (filter.startDate) {
        if (new Date(log.timestamp) < new Date(filter.startDate)) {
          return false;
        }
      }

      if (filter.endDate) {
        if (new Date(log.timestamp) > new Date(filter.endDate)) {
          return false;
        }
      }

      return true;
    });
  }

  // Generate audit report
  async generateReport(options = {}) {
    const logs = await this.query(options);

    const report = {
      period: {
        start: options.startDate || 'all time',
        end: options.endDate || new Date().toISOString()
      },
      summary: {
        totalEvents: logs.length,
        successfulEvents: logs.filter(l => l.status === 'success').length,
        failedEvents: logs.filter(l => l.status === 'failure').length,
        errorEvents: logs.filter(l => l.status === 'error').length,
        uniqueUsers: new Set(logs.map(l => l.user)).size
      },
      byUser: this.groupBy(logs, 'user'),
      byAction: this.groupBy(logs, 'action'),
      byResource: this.groupBy(logs, 'resource'),
      timeline: this.createTimeline(logs)
    };

    return report;
  }

  groupBy(logs, key) {
    const grouped = new Map();

    for (const log of logs) {
      const value = log[key];
      if (!grouped.has(value)) {
        grouped.set(value, {
          [key]: value,
          count: 0,
          success: 0,
          failure: 0,
          error: 0
        });
      }

      const stats = grouped.get(value);
      stats.count++;
      stats[log.status]++;
    }

    return Array.from(grouped.values());
  }

  createTimeline(logs) {
    const timeline = new Map();

    for (const log of logs) {
      const hour = new Date(log.timestamp).toISOString().slice(0, 13);

      if (!timeline.has(hour)) {
        timeline.set(hour, {
          timestamp: hour,
          events: 0,
          success: 0,
          failure: 0,
          error: 0
        });
      }

      const stats = timeline.get(hour);
      stats.events++;
      stats[log.status]++;
    }

    return Array.from(timeline.values()).sort((a, b) =>
      a.timestamp.localeCompare(b.timestamp)
    );
  }

  async cleanup() {
    if (this.timer) {
      clearInterval(this.timer);
    }
    await this.flush();
  }
}
```

### Audit Middleware

```javascript
// src/cli/middleware/audit.mjs
export const auditMiddleware = async (ctx) => {
  const auditLogger = ctx.auditLogger || new AuditLogger(ctx.config.audit);
  const startTime = Date.now();

  try {
    // Execute command
    const result = await ctx.next();

    // Log success
    await auditLogger.log({
      user: ctx.user || 'anonymous',
      action: `${ctx.noun}.${ctx.verb}`,
      resource: ctx.noun,
      resourceId: ctx.args[0],
      status: 'success',
      metadata: {
        ip: ctx.ip,
        duration: Date.now() - startTime
      }
    });

    return result;

  } catch (error) {
    // Log error
    await auditLogger.log({
      user: ctx.user || 'anonymous',
      action: `${ctx.noun}.${ctx.verb}`,
      resource: ctx.noun,
      resourceId: ctx.args[0],
      status: 'error',
      metadata: {
        ip: ctx.ip,
        duration: Date.now() - startTime,
        error: error.message
      }
    });

    throw error;
  }
};
```

## 3. Batch Operations

### Batch Processor

```javascript
// src/cli/batch/processor.mjs
export class BatchProcessor {
  constructor(config) {
    this.config = config;
    this.maxConcurrency = config.maxConcurrency || 10;
    this.retryAttempts = config.retryAttempts || 3;
  }

  // Process batch of operations
  async process(operations, options = {}) {
    const results = [];
    const errors = [];

    // Process in chunks
    for (let i = 0; i < operations.length; i += this.maxConcurrency) {
      const chunk = operations.slice(i, i + this.maxConcurrency);

      const chunkResults = await Promise.allSettled(
        chunk.map(op => this.processOperation(op, options))
      );

      for (let j = 0; j < chunkResults.length; j++) {
        const result = chunkResults[j];
        const operation = chunk[j];

        if (result.status === 'fulfilled') {
          results.push({
            operation,
            status: 'success',
            result: result.value
          });
        } else {
          errors.push({
            operation,
            status: 'error',
            error: result.reason.message
          });
        }
      }

      // Progress callback
      if (options.onProgress) {
        await options.onProgress({
          completed: Math.min(i + this.maxConcurrency, operations.length),
          total: operations.length,
          successful: results.length,
          failed: errors.length
        });
      }
    }

    return {
      total: operations.length,
      successful: results.length,
      failed: errors.length,
      results,
      errors
    };
  }

  async processOperation(operation, options) {
    let lastError;

    for (let attempt = 1; attempt <= this.retryAttempts; attempt++) {
      try {
        return await operation.execute();
      } catch (error) {
        lastError = error;

        if (attempt < this.retryAttempts) {
          const delay = Math.min(1000 * Math.pow(2, attempt - 1), 10000);
          await new Promise(resolve => setTimeout(resolve, delay));
        }
      }
    }

    throw lastError;
  }

  // Process from file
  async processFromFile(filePath, parser, options = {}) {
    const content = await fs.readFile(filePath, 'utf-8');
    const operations = await parser(content);
    return await this.process(operations, options);
  }
}
```

### Batch Commands

```bash
# Batch import multiple graphs
unrdf graph import --batch=import-list.json \
  --concurrency=5 \
  --on-error=continue

# Batch hook evaluation
unrdf hook eval --batch=hooks-list.txt \
  --data=./graphs/ \
  --parallel

# Batch policy application
unrdf policy apply --batch=policies/ \
  --validate \
  --rollback-on-error
```

## 4. Watch & Monitor Modes

### Watch Mode

```javascript
// src/cli/watch/watcher.mjs
import chokidar from 'chokidar';

export class ResourceWatcher {
  constructor(config) {
    this.config = config;
    this.watchers = new Map();
  }

  // Watch resource changes
  async watch(resource, options = {}) {
    const watcher = chokidar.watch(options.path || '.', {
      ignored: options.ignored || /node_modules/,
      persistent: true,
      ignoreInitial: options.ignoreInitial !== false
    });

    watcher
      .on('add', path => this.handleEvent('add', path, options))
      .on('change', path => this.handleEvent('change', path, options))
      .on('unlink', path => this.handleEvent('delete', path, options));

    this.watchers.set(resource, watcher);

    console.log(`👀 Watching ${resource}...`);
  }

  async handleEvent(event, path, options) {
    if (options.onChange) {
      await options.onChange({
        event,
        path,
        timestamp: new Date().toISOString()
      });
    }
  }

  stop(resource) {
    const watcher = this.watchers.get(resource);
    if (watcher) {
      watcher.close();
      this.watchers.delete(resource);
    }
  }

  stopAll() {
    for (const [resource, watcher] of this.watchers) {
      watcher.close();
    }
    this.watchers.clear();
  }
}
```

### Monitor Mode

```javascript
// src/cli/monitor/monitor.mjs
export class ResourceMonitor {
  constructor(config) {
    this.config = config;
    this.metrics = new Map();
    this.interval = config.interval || 5000;
  }

  // Monitor resource metrics
  async monitor(resource, options = {}) {
    const intervalId = setInterval(async () => {
      try {
        const metrics = await this.collectMetrics(resource, options);

        if (options.onChange) {
          await options.onChange(metrics);
        }

        this.displayMetrics(metrics, options);
      } catch (error) {
        console.error(`Monitor error: ${error.message}`);
      }
    }, this.interval);

    this.metrics.set(resource, {
      intervalId,
      startTime: Date.now()
    });

    console.log(`📊 Monitoring ${resource}...`);
  }

  async collectMetrics(resource, options) {
    switch (resource) {
      case 'sidecar':
        return await this.collectSidecarMetrics(options);
      case 'store':
        return await this.collectStoreMetrics(options);
      case 'hooks':
        return await this.collectHookMetrics(options);
      default:
        throw new Error(`Unknown resource: ${resource}`);
    }
  }

  async collectSidecarMetrics(options) {
    const client = options.sidecarClient;
    const metrics = await client.metrics();

    return {
      timestamp: new Date().toISOString(),
      status: metrics.status,
      uptime: metrics.uptime,
      memory: metrics.memory,
      cpu: metrics.cpu,
      requests: metrics.requests,
      errors: metrics.errors
    };
  }

  displayMetrics(metrics, options) {
    if (options.format === 'json') {
      console.log(JSON.stringify(metrics, null, 2));
    } else {
      // Table display
      console.clear();
      console.log(`🕐 ${metrics.timestamp}`);
      console.log(`Status: ${metrics.status}`);
      console.log(`Memory: ${metrics.memory}MB`);
      console.log(`CPU: ${metrics.cpu}%`);
      console.log(`Requests: ${metrics.requests}`);
      console.log(`Errors: ${metrics.errors}`);
    }
  }

  stop(resource) {
    const monitor = this.metrics.get(resource);
    if (monitor) {
      clearInterval(monitor.intervalId);
      this.metrics.delete(resource);
    }
  }

  stopAll() {
    for (const [resource, monitor] of this.metrics) {
      clearInterval(monitor.intervalId);
    }
    this.metrics.clear();
  }
}
```

## 5. Interactive Shell Mode

### Interactive Shell

```javascript
// src/cli/interactive/shell.mjs
import readline from 'node:readline';
import { CommandRouter } from '../router.mjs';

export class InteractiveShell {
  constructor(config) {
    this.config = config;
    this.router = new CommandRouter(config);
    this.history = [];
    this.prompt = 'unrdf> ';
  }

  async start() {
    console.log('UNRDF Interactive Shell v2.0');
    console.log('Type "help" for commands, "exit" to quit\n');

    const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      prompt: this.prompt,
      historySize: 1000
    });

    rl.on('line', async (line) => {
      const trimmed = line.trim();

      if (!trimmed) {
        rl.prompt();
        return;
      }

      if (trimmed === 'exit' || trimmed === 'quit') {
        console.log('Goodbye!');
        rl.close();
        process.exit(0);
      }

      try {
        await this.executeCommand(trimmed);
        this.history.push(trimmed);
      } catch (error) {
        console.error(`Error: ${error.message}`);
      }

      rl.prompt();
    });

    rl.on('close', () => {
      console.log('\nGoodbye!');
      process.exit(0);
    });

    rl.prompt();
  }

  async executeCommand(line) {
    const [noun, verb, ...args] = line.split(/\s+/);

    // Built-in commands
    if (noun === 'help') {
      this.showHelp();
      return;
    }

    if (noun === 'history') {
      this.showHistory();
      return;
    }

    if (noun === 'clear') {
      console.clear();
      return;
    }

    // Execute via router
    const result = await this.router.execute({
      noun,
      verb,
      args,
      flags: this.parseFlags(args)
    });

    // Display result
    console.log(result);
  }

  parseFlags(args) {
    const flags = {};
    for (const arg of args) {
      if (arg.startsWith('--')) {
        const [key, value] = arg.slice(2).split('=');
        flags[key] = value || true;
      }
    }
    return flags;
  }

  showHelp() {
    console.log(`
Available Commands:
  graph <verb>     - Graph operations
  hook <verb>      - Hook operations
  policy <verb>    - Policy operations
  store <verb>     - Store operations
  sidecar <verb>   - Sidecar operations

Shell Commands:
  help             - Show this help
  history          - Show command history
  clear            - Clear screen
  exit / quit      - Exit shell
    `);
  }

  showHistory() {
    this.history.forEach((cmd, i) => {
      console.log(`${i + 1}  ${cmd}`);
    });
  }
}
```

## Usage Examples

### RBAC
```bash
# Assign role
unrdf rbac assign-role alice operator

# Check permission
unrdf rbac check-permission alice graph write

# Create custom role
unrdf rbac create-role custom-role.json
```

### Audit
```bash
# Query audit logs
unrdf audit query --user=alice --action=graph.delete

# Generate report
unrdf audit report --start-date=2025-01-01 --export=report.json
```

### Batch
```bash
# Batch import
unrdf graph import --batch=graphs.json --concurrency=5

# Progress monitoring
unrdf policy apply --batch=policies/ --progress
```

### Watch
```bash
# Watch hooks
unrdf hook list --watch

# Watch graphs
unrdf graph list --watch --filter=prod
```

### Monitor
```bash
# Monitor sidecar
unrdf sidecar monitor --interval=5s

# Monitor store
unrdf store monitor --format=json
```

### Interactive
```bash
# Start interactive shell
unrdf shell

# Or
unrdf -i
```

## Conclusion

These enterprise features provide:
- **RBAC**: Fine-grained access control
- **Audit**: Comprehensive audit logging
- **Batch**: Efficient bulk operations
- **Watch**: Real-time change monitoring
- **Monitor**: Live metrics monitoring
- **Interactive**: Shell mode for exploration

**Status**: ✅ ENTERPRISE FEATURES COMPLETE
