# UNRDF Plugin Architecture

**Version**: 1.0.0
**Status**: Canonical Standard
**Last Updated**: 2025-12-25

## Philosophy

Plugins extend core functionality without modifying source code. Good plugin architecture is:
- **Simple**: Easy to create and understand
- **Composable**: Plugins work together
- **Safe**: Plugins can't break the system
- **Observable**: Plugin behavior is traceable

## Core Concepts

### Plugin Lifecycle

```
┌─────────────┐
│   CREATED   │ Plugin defined
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  REGISTERED │ Added to manager
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ INITIALIZED │ init() called
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   ACTIVE    │ Handling events
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  DESTROYED  │ cleanup() called
└─────────────┘
```

### Plugin Interface

Every plugin implements:

```javascript
const plugin = {
  // Required fields
  name: 'plugin-name',          // Unique identifier
  version: '1.0.0',             // Semantic version

  // Lifecycle hooks
  async init() { },             // Initialize plugin
  async cleanup() { },          // Cleanup resources

  // Event handlers
  on: {
    beforeQuery: async (event) => { },
    afterQuery: async (event) => { },
    onError: async (event) => { },
  },
};
```

## Plugin Manager

### Core Implementation

```javascript
/**
 * @file Plugin manager for UNRDF
 * @module @unrdf/core/plugins
 */

import { z } from 'zod';

/**
 * Plugin schema for validation
 */
const PluginSchema = z.object({
  name: z.string().min(1),
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  init: z.function().optional(),
  cleanup: z.function().optional(),
  on: z.record(z.function()).optional(),
});

/**
 * Create plugin manager
 *
 * @returns {PluginManager} Plugin manager instance
 * @example
 * const manager = createPluginManager();
 * manager.register(myPlugin);
 * await manager.init();
 */
export function createPluginManager() {
  const plugins = new Map();
  const eventHandlers = new Map();
  let initialized = false;

  return {
    /**
     * Register a plugin
     *
     * @param {Plugin} plugin - Plugin to register
     * @throws {TypeError} If plugin is invalid
     * @throws {Error} If plugin name already registered
     * @example
     * manager.register({
     *   name: 'logger',
     *   version: '1.0.0',
     *   async init() { console.log('Logger initialized'); },
     * });
     */
    register(plugin) {
      // Validate plugin
      try {
        PluginSchema.parse(plugin);
      } catch (error) {
        throw new TypeError(`Invalid plugin: ${error.message}`);
      }

      // Check for duplicates
      if (plugins.has(plugin.name)) {
        throw new Error(`Plugin "${plugin.name}" already registered`);
      }

      // Store plugin
      plugins.set(plugin.name, plugin);

      // Register event handlers
      if (plugin.on) {
        for (const [event, handler] of Object.entries(plugin.on)) {
          if (!eventHandlers.has(event)) {
            eventHandlers.set(event, []);
          }
          eventHandlers.get(event).push({ plugin: plugin.name, handler });
        }
      }
    },

    /**
     * Initialize all plugins
     *
     * Calls init() on each plugin in registration order.
     *
     * @throws {Error} If plugin initialization fails
     * @example
     * await manager.init();
     */
    async init() {
      if (initialized) {
        throw new Error('Plugin manager already initialized');
      }

      for (const plugin of plugins.values()) {
        if (plugin.init) {
          try {
            await plugin.init();
          } catch (error) {
            throw new Error(`Plugin "${plugin.name}" init failed: ${error.message}`);
          }
        }
      }

      initialized = true;
    },

    /**
     * Execute event handlers
     *
     * @param {string} event - Event name
     * @param {*} data - Event data
     * @returns {Promise<*>} Modified data (if any)
     * @example
     * const result = await manager.execute('beforeQuery', { query });
     */
    async execute(event, data) {
      const handlers = eventHandlers.get(event) || [];
      let result = data;

      for (const { plugin, handler } of handlers) {
        try {
          const output = await handler(result);
          if (output !== undefined) {
            result = output;
          }
        } catch (error) {
          console.error(`Plugin "${plugin}" handler for "${event}" failed:`, error);
          // Continue executing other handlers
        }
      }

      return result;
    },

    /**
     * Cleanup all plugins
     *
     * Calls cleanup() on each plugin in reverse order.
     *
     * @example
     * await manager.cleanup();
     */
    async cleanup() {
      const pluginArray = Array.from(plugins.values()).reverse();

      for (const plugin of pluginArray) {
        if (plugin.cleanup) {
          try {
            await plugin.cleanup();
          } catch (error) {
            console.error(`Plugin "${plugin.name}" cleanup failed:`, error);
          }
        }
      }

      plugins.clear();
      eventHandlers.clear();
      initialized = false;
    },

    /**
     * Get plugin by name
     *
     * @param {string} name - Plugin name
     * @returns {Plugin|undefined} Plugin instance
     * @example
     * const logger = manager.get('logger');
     */
    get(name) {
      return plugins.get(name);
    },

    /**
     * Check if plugin is registered
     *
     * @param {string} name - Plugin name
     * @returns {boolean} True if registered
     * @example
     * if (manager.has('logger')) { }
     */
    has(name) {
      return plugins.has(name);
    },

    /**
     * List all registered plugins
     *
     * @returns {string[]} Plugin names
     * @example
     * console.log(manager.list());
     */
    list() {
      return Array.from(plugins.keys());
    },
  };
}
```

## Plugin Examples

### Example 1: Logging Plugin

**Purpose**: Log all queries with timing

```javascript
/**
 * @file Query logging plugin
 */

export const queryLoggerPlugin = {
  name: 'query-logger',
  version: '1.0.0',

  async init() {
    console.log('[QueryLogger] Initialized');
  },

  on: {
    beforeQuery: async (event) => {
      event.startTime = Date.now();
      console.log(`[QueryLogger] Executing: ${event.query}`);
      return event;
    },

    afterQuery: async (event) => {
      const duration = Date.now() - event.startTime;
      console.log(`[QueryLogger] Completed in ${duration}ms`);
      return event;
    },

    onError: async (event) => {
      console.error(`[QueryLogger] Error: ${event.error.message}`);
      return event;
    },
  },

  async cleanup() {
    console.log('[QueryLogger] Cleaned up');
  },
};

// Usage:
import { createPluginManager } from '@unrdf/core/plugins';

const manager = createPluginManager();
manager.register(queryLoggerPlugin);
await manager.init();
```

### Example 2: OTEL Tracing Plugin

**Purpose**: Add OpenTelemetry tracing to operations

```javascript
/**
 * @file OpenTelemetry tracing plugin
 */

import { trace } from '@opentelemetry/api';

export const otelTracingPlugin = {
  name: 'otel-tracing',
  version: '1.0.0',

  tracer: null,

  async init() {
    this.tracer = trace.getTracer('unrdf-core', '5.0.0');
  },

  on: {
    beforeQuery: async (event) => {
      const span = this.tracer.startSpan('sparql.query', {
        attributes: {
          'query.type': event.queryType,
          'query.length': event.query.length,
        },
      });

      event.span = span;
      return event;
    },

    afterQuery: async (event) => {
      if (event.span) {
        event.span.setAttributes({
          'result.count': event.results.length,
        });
        event.span.end();
      }
      return event;
    },

    onError: async (event) => {
      if (event.span) {
        event.span.recordException(event.error);
        event.span.setStatus({ code: 2, message: event.error.message });
        event.span.end();
      }
      return event;
    },
  },
};
```

### Example 3: Caching Plugin

**Purpose**: Cache query results

```javascript
/**
 * @file Query result caching plugin
 */

export function createCachingPlugin(options = {}) {
  const { maxSize = 100, ttl = 60000 } = options;
  const cache = new Map();

  return {
    name: 'caching',
    version: '1.0.0',

    async init() {
      // Start cache cleanup interval
      this.cleanupInterval = setInterval(() => {
        const now = Date.now();
        for (const [key, value] of cache.entries()) {
          if (now - value.timestamp > ttl) {
            cache.delete(key);
          }
        }
      }, ttl);
    },

    on: {
      beforeQuery: async (event) => {
        const cacheKey = event.query;
        const cached = cache.get(cacheKey);

        if (cached && Date.now() - cached.timestamp < ttl) {
          event.results = cached.results;
          event.fromCache = true;
          event.skipExecution = true; // Signal to skip actual query
        }

        return event;
      },

      afterQuery: async (event) => {
        if (!event.fromCache) {
          const cacheKey = event.query;

          // Evict oldest if cache full
          if (cache.size >= maxSize) {
            const oldest = cache.keys().next().value;
            cache.delete(oldest);
          }

          cache.set(cacheKey, {
            results: event.results,
            timestamp: Date.now(),
          });
        }

        return event;
      },
    },

    async cleanup() {
      clearInterval(this.cleanupInterval);
      cache.clear();
    },
  };
}

// Usage:
const cachingPlugin = createCachingPlugin({ maxSize: 200, ttl: 120000 });
manager.register(cachingPlugin);
```

### Example 4: Validation Plugin

**Purpose**: Validate queries before execution

```javascript
/**
 * @file Query validation plugin
 */

import { z } from 'zod';

const QueryEventSchema = z.object({
  query: z.string().min(1),
  queryType: z.enum(['SELECT', 'CONSTRUCT', 'ASK', 'DESCRIBE']),
});

export const validationPlugin = {
  name: 'validation',
  version: '1.0.0',

  on: {
    beforeQuery: async (event) => {
      try {
        QueryEventSchema.parse(event);
      } catch (error) {
        throw new Error(`Invalid query event: ${error.message}`);
      }

      // Additional SPARQL validation
      if (event.query.includes('DELETE') || event.query.includes('INSERT')) {
        throw new Error('UPDATE queries not allowed in SELECT mode');
      }

      return event;
    },
  },
};
```

## Event Types

### Standard Events

| Event | When | Data |
|-------|------|------|
| `beforeQuery` | Before query execution | `{ query, queryType, options }` |
| `afterQuery` | After successful query | `{ query, results, duration }` |
| `onError` | On query error | `{ query, error }` |
| `beforeAdd` | Before adding quad | `{ quad, store }` |
| `afterAdd` | After adding quad | `{ quad, store }` |
| `beforeRemove` | Before removing quad | `{ quad, store }` |
| `afterRemove` | After removing quad | `{ quad, store }` |

### Custom Events

Packages can define custom events:

```javascript
// In your package
await manager.execute('beforeFederation', {
  peers: discoveredPeers,
  query: federatedQuery,
});
```

## Integration Patterns

### With Store

```javascript
import { createStore } from '@unrdf/core';
import { createPluginManager } from '@unrdf/core/plugins';

export function createObservableStore(plugins = []) {
  const manager = createPluginManager();
  const store = createStore();

  // Register plugins
  for (const plugin of plugins) {
    manager.register(plugin);
  }

  // Wrap store methods
  return {
    async addQuad(quad) {
      await manager.execute('beforeAdd', { quad, store });
      store.addQuad(quad);
      await manager.execute('afterAdd', { quad, store });
    },

    async query(sparql) {
      const event = await manager.execute('beforeQuery', { query: sparql });

      if (event.skipExecution) {
        return event.results;
      }

      try {
        const results = store.query(sparql);
        await manager.execute('afterQuery', { query: sparql, results });
        return results;
      } catch (error) {
        await manager.execute('onError', { query: sparql, error });
        throw error;
      }
    },

    async init() {
      await manager.init();
    },

    async cleanup() {
      await manager.cleanup();
    },
  };
}

// Usage:
const store = createObservableStore([
  queryLoggerPlugin,
  otelTracingPlugin,
  createCachingPlugin(),
]);

await store.init();
const results = await store.query('SELECT * WHERE { ?s ?p ?o }');
await store.cleanup();
```

### Plugin Composition

Plugins can depend on each other:

```javascript
const metricsPlugin = {
  name: 'metrics',
  version: '1.0.0',

  queryCount: 0,

  async init() {
    // Access another plugin
    const logger = this.manager.get('logger');
    if (!logger) {
      throw new Error('Metrics plugin requires logger plugin');
    }
  },

  on: {
    afterQuery: async (event) => {
      this.queryCount++;
      return event;
    },
  },
};
```

## Plugin Discovery

### Registry Pattern

```javascript
/**
 * Global plugin registry
 */
const registry = new Map();

/**
 * Register plugin in global registry
 */
export function registerPlugin(plugin) {
  registry.set(plugin.name, plugin);
}

/**
 * Get plugin from registry
 */
export function getPlugin(name) {
  return registry.get(name);
}

/**
 * List available plugins
 */
export function listPlugins() {
  return Array.from(registry.keys());
}

// In plugin file:
registerPlugin(myPlugin);

// In app:
import '@unrdf/plugin-logger';  // Auto-registers
const logger = getPlugin('logger');
manager.register(logger);
```

## Testing Plugins

### Plugin Test Template

```javascript
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createPluginManager } from '@unrdf/core/plugins';
import { myPlugin } from './my-plugin.mjs';

describe('myPlugin', () => {
  let manager;

  beforeEach(async () => {
    manager = createPluginManager();
    manager.register(myPlugin);
    await manager.init();
  });

  afterEach(async () => {
    await manager.cleanup();
  });

  it('handles beforeQuery event', async () => {
    const event = { query: 'SELECT * WHERE { ?s ?p ?o }' };
    const result = await manager.execute('beforeQuery', event);
    expect(result).toBeDefined();
  });

  it('throws on invalid input', async () => {
    const event = { query: '' };
    await expect(manager.execute('beforeQuery', event)).rejects.toThrow();
  });
});
```

## Best Practices

### DO

1. **Validate inputs**:
   ```javascript
   on: {
     beforeQuery: async (event) => {
       if (!event.query) throw new TypeError('Missing query');
       return event;
     },
   }
   ```

2. **Return event data**:
   ```javascript
   on: {
     beforeQuery: async (event) => {
       event.timestamp = Date.now();
       return event;  // ✅ Return modified event
     },
   }
   ```

3. **Handle cleanup**:
   ```javascript
   async cleanup() {
     clearInterval(this.interval);
     this.cache.clear();
   }
   ```

4. **Use unique names**:
   ```javascript
   name: '@myorg/my-plugin',  // ✅ Scoped
   ```

### DON'T

1. **Mutate without returning**:
   ```javascript
   // ❌ BAD
   on: {
     beforeQuery: async (event) => {
       event.timestamp = Date.now();
       // Missing return!
     },
   }
   ```

2. **Throw in cleanup**:
   ```javascript
   // ❌ BAD
   async cleanup() {
     throw new Error('Cleanup failed');
   }

   // ✅ GOOD
   async cleanup() {
     try {
       await this.resource.close();
     } catch (error) {
       console.error('Cleanup failed:', error);
     }
   }
   ```

3. **Block execution**:
   ```javascript
   // ❌ BAD
   on: {
     beforeQuery: async (event) => {
       await sleep(10000);  // Blocks all queries!
       return event;
     },
   }
   ```

## Plugin Template

```javascript
/**
 * @file My plugin
 */

export const myPlugin = {
  // Required
  name: 'my-plugin',
  version: '1.0.0',

  // State
  initialized: false,

  // Lifecycle
  async init() {
    // Initialize resources
    this.initialized = true;
  },

  async cleanup() {
    // Cleanup resources
    this.initialized = false;
  },

  // Event handlers
  on: {
    beforeQuery: async (event) => {
      // Handle event
      return event;
    },

    afterQuery: async (event) => {
      // Handle event
      return event;
    },

    onError: async (event) => {
      // Handle error
      return event;
    },
  },
};
```

## Related Documents

- [API Design Guidelines](./API-DESIGN.md) - Function patterns
- [Package Structure](./PACKAGE-STRUCTURE.md) - Directory organization
- [Configuration Patterns](../packages/core/src/config.mjs) - Config management

---

**Version History**:
- 1.0.0 (2025-12-25): Initial plugin architecture standard
