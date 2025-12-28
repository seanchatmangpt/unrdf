# KGC Plugin Development Guide

## Overview

The KGC Runtime plugin system allows you to extend core functionality with custom receipt types, validators, and tools while maintaining security and deterministic execution.

## Quick Start

### 1. Create Plugin Manifest

```javascript
// plugin.json
{
  "name": "my-plugin",
  "version": "1.0.0",
  "description": "My custom KGC plugin",
  "entryPoint": "./index.mjs",
  "capabilities": [
    "receipt:generate",
    "receipt:validate"
  ],
  "api_version": "5.0.1",
  "author": "Your Name",
  "license": "MIT"
}
```

### 2. Implement Plugin

```javascript
// index.mjs
import { PluginReceiptSchema } from '@unrdf/kgc-runtime/schemas';

/**
 * Plugin entry point
 * @param {Object} runtime - KGC Runtime API (whitelisted methods only)
 */
export default function plugin(runtime) {
  return {
    name: 'my-plugin',
    version: '1.0.0',

    /**
     * Initialize plugin
     */
    async initialize() {
      console.log('Plugin initialized');
    },

    /**
     * Generate custom receipt
     */
    async generateCustomReceipt(operation, inputs, outputs) {
      const receipt = await runtime.generateReceipt(operation, inputs, outputs);

      // Add custom metadata
      return PluginReceiptSchema.parse({
        ...receipt,
        pluginMetadata: {
          pluginName: 'my-plugin',
          pluginVersion: '1.0.0',
          receiptType: 'custom',
          customFields: {
            myData: 'example'
          }
        }
      });
    },

    /**
     * Cleanup on unload
     */
    async cleanup() {
      console.log('Plugin cleanup');
    }
  };
}
```

### 3. Register and Use

```javascript
import { PluginManager } from '@unrdf/kgc-runtime/plugin-manager';

const manager = new PluginManager();

// Register plugin
await manager.registerPlugin({
  name: 'my-plugin',
  version: '1.0.0',
  entryPoint: './plugin.mjs',
  capabilities: ['receipt:generate'],
  api_version: '5.0.1'
});

// Load and activate
await manager.loadPlugin('my-plugin@1.0.0');
await manager.activatePlugin('my-plugin@1.0.0');
```

## API Reference

### Available Runtime APIs (Whitelisted)

Plugins can only access these whitelisted APIs:

```javascript
runtime = {
  // Receipt operations
  generateReceipt(operation, inputs, outputs, parentHash),
  verifyReceiptHash(receipt),

  // Schema validation
  validateReceipt(receipt),
  validateWorkItem(workItem),
  validateBounds(bounds),

  // Tool registry (read-only)
  getTool(name),
  getToolsByCapability(capability),

  // Bounds checking
  checkBounds(current, limits)
}
```

### Blocked Capabilities

The following are NEVER allowed for security:

- `filesystem:write` - Direct file writes
- `filesystem:delete` - File deletion
- `network:http` - HTTP requests
- `network:socket` - Socket connections
- `process:spawn` - Process spawning
- `process:exit` - Process termination
- `eval:code` - Dynamic code evaluation

## Plugin Lifecycle

```
REGISTERED → LOADED → EXECUTING → UNLOADED
     ↓                    ↓
  FAILED ←──────────────────
```

### State Transitions

1. **REGISTERED**: Plugin manifest validated and registered
2. **LOADED**: Plugin code loaded into memory, `initialize()` called
3. **EXECUTING**: Plugin actively processing requests
4. **UNLOADED**: Plugin stopped, `cleanup()` called
5. **FAILED**: Error occurred, check audit log

## Custom Receipt Types

Define custom receipt schemas:

```javascript
import { z } from 'zod';
import { PluginReceiptSchema } from '@unrdf/kgc-runtime/schemas';

// Define custom receipt type
const MyCustomReceiptSchema = PluginReceiptSchema.extend({
  pluginMetadata: z.object({
    pluginName: z.literal('my-plugin'),
    pluginVersion: z.string(),
    receiptType: z.literal('performance'),
    customFields: z.object({
      executionTime: z.number(),
      memoryUsed: z.number(),
      optimizationApplied: z.boolean()
    })
  })
});

// Generate typed receipt
export async function generatePerformanceReceipt(operation, metrics) {
  const receipt = await runtime.generateReceipt(operation, {}, metrics);

  return MyCustomReceiptSchema.parse({
    ...receipt,
    pluginMetadata: {
      pluginName: 'my-plugin',
      pluginVersion: '1.0.0',
      receiptType: 'performance',
      customFields: {
        executionTime: metrics.duration,
        memoryUsed: metrics.memory,
        optimizationApplied: metrics.optimized
      }
    }
  });
}
```

## Security Best Practices

### 1. Capability Declaration

Always declare minimum required capabilities:

```javascript
{
  "capabilities": [
    "receipt:generate",    // Only what you need
    "receipt:validate"
  ]
}
```

### 2. Input Validation

Validate all inputs with Zod schemas:

```javascript
import { z } from 'zod';

const InputSchema = z.object({
  operation: z.string().min(1).max(100),
  data: z.record(z.any())
});

export async function processInput(input) {
  // Validate first
  const validated = InputSchema.parse(input);

  // Then process
  return validated;
}
```

### 3. Error Handling

Use proper error receipts:

```javascript
try {
  const result = await riskyOperation();
  return await runtime.generateReceipt('success', {}, { result });
} catch (error) {
  return await runtime.generateReceipt('error', {}, {
    success: false,
    error: error.message,
    stack: error.stack
  });
}
```

## Testing Plugins

### Unit Tests

```javascript
import { describe, it, expect } from 'vitest';
import myPlugin from './plugin.mjs';

describe('My Plugin', () => {
  it('should initialize correctly', async () => {
    const mockRuntime = {
      generateReceipt: async (op, inp, out) => ({
        id: 'test',
        operation: op
      })
    };

    const plugin = myPlugin(mockRuntime);
    await plugin.initialize();

    expect(plugin.name).toBe('my-plugin');
  });

  it('should generate custom receipts', async () => {
    const mockRuntime = {
      generateReceipt: async (op, inp, out) => ({
        id: 'test',
        timestamp: Date.now(),
        operation: op,
        inputs: inp,
        outputs: out,
        hash: 'abc123'
      })
    };

    const plugin = myPlugin(mockRuntime);
    const receipt = await plugin.generateCustomReceipt('test', {}, {});

    expect(receipt.pluginMetadata).toBeDefined();
    expect(receipt.pluginMetadata.pluginName).toBe('my-plugin');
  });
});
```

### Integration Tests

```javascript
import { PluginManager } from '@unrdf/kgc-runtime/plugin-manager';
import { PluginIsolation } from '@unrdf/kgc-runtime/plugin-isolation';

describe('Plugin Integration', () => {
  it('should load and execute with isolation', async () => {
    const manager = new PluginManager();
    const isolation = new PluginIsolation({
      whitelist: ['receipt:generate']
    });

    await manager.registerPlugin({
      name: 'test-plugin',
      version: '1.0.0',
      entryPoint: './test-plugin.mjs',
      capabilities: ['receipt:generate'],
      api_version: '5.0.1'
    });

    await manager.loadPlugin('test-plugin@1.0.0');

    const state = manager.getPluginState('test-plugin@1.0.0');
    expect(state).toBe('loaded');
  });
});
```

## Publishing Plugins

### 1. Prepare Package

```json
{
  "name": "@my-scope/kgc-plugin-name",
  "version": "1.0.0",
  "type": "module",
  "main": "./index.mjs",
  "exports": {
    ".": "./index.mjs"
  },
  "peerDependencies": {
    "@unrdf/kgc-runtime": "^1.0.0"
  }
}
```

### 2. Test Thoroughly

```bash
# Run all tests
npm test

# Check type safety
npm run typecheck

# Verify capabilities
npm run verify-plugin
```

### 3. Submit to Registry

Create a PR to the [KGC Plugin Registry](https://github.com/unrdf/kgc-plugins):

```json
{
  "name": "my-plugin",
  "version": "1.0.0",
  "description": "My custom plugin",
  "repository": "https://github.com/my-org/my-plugin",
  "verified": false
}
```

## Examples

See `templates/plugin-template/` for a complete example plugin with:
- Manifest configuration
- Receipt generation
- Input validation
- Error handling
- Comprehensive tests

## Support

- Documentation: https://unrdf.github.io/kgc-runtime
- Issues: https://github.com/unrdf/kgc-runtime/issues
- Discussions: https://github.com/unrdf/kgc-runtime/discussions
