# KnowledgeHookManager

Class-based interface for managing Knowledge Hooks in the UNRDF framework.

## Installation

```javascript
import { KnowledgeHookManager } from '@unrdf/hooks';
```

## Overview

`KnowledgeHookManager` provides a convenient class-based API for managing hooks, built on top of the functional hook management APIs. It wraps the registry system in an easy-to-use interface.

## Constructor

```javascript
const manager = new KnowledgeHookManager(options);
```

### Options

- `includeBuiltins` (boolean, default: false) - Automatically register built-in validation hooks

## Methods

### registerHook(hook)

Register a hook with the manager.

```javascript
manager.registerHook({
  id: 'custom-hook',
  name: 'custom-hook',
  description: 'Custom validation',
  version: '1.0.0',
  trigger: 'before-add',
  enabled: true,
  validate: (quad) => ({ valid: true })
});
```

### unregisterHook(name)

Remove a hook by name.

```javascript
manager.unregisterHook('custom-hook');
```

### hasHook(name)

Check if a hook is registered.

```javascript
const exists = manager.hasHook('custom-hook');
```

### getHook(name)

Get a hook by name.

```javascript
const hook = manager.getHook('custom-hook');
```

### listHooks(options)

List all registered hooks with optional filtering.

```javascript
const allHooks = manager.listHooks();
const enabledHooks = manager.listHooks({ enabled: true });
const beforeAddHooks = manager.listHooks({ trigger: 'before-add' });
```

### getHooksByTrigger(trigger)

Get all hooks for a specific trigger.

```javascript
const hooks = manager.getHooksByTrigger('before-add');
```

### executeHook(hookId, data, context)

Execute a specific hook by ID.

```javascript
const result = await manager.executeHook('custom-hook', quad, store);
```

### executeByTrigger(trigger, data, context)

Execute all hooks for a trigger.

```javascript
const result = await manager.executeByTrigger('before-add', quad, store);
```

### executeChain(hooks, data, context)

Execute a chain of hooks in sequence.

```javascript
const hooks = manager.getHooksByTrigger('before-add');
const result = await manager.executeChain(hooks, quad, store);
```

### wouldPass(trigger, data, context)

Check if data would pass validation (dry-run).

```javascript
const wouldPass = await manager.wouldPass('before-add', quad, store);
```

### clearHooks()

Remove all registered hooks.

```javascript
manager.clearHooks();
```

### getStats()

Get registry statistics.

```javascript
const stats = manager.getStats();
// { totalHooks: 10, byTrigger: { 'before-add': 10 } }
```

## Static Methods

### KnowledgeHookManager.getBuiltinHooks()

Get array of built-in hooks.

```javascript
const builtins = KnowledgeHookManager.getBuiltinHooks();
```

## Examples

### Basic Usage

```javascript
import { KnowledgeHookManager } from '@unrdf/hooks';

// Create manager with built-in hooks
const manager = new KnowledgeHookManager({ includeBuiltins: true });

console.log('Hooks loaded:', manager.listHooks().length);
```

### Custom Hook Registration

```javascript
const customHook = {
  id: 'length-validator',
  name: 'length-validator',
  description: 'Validate string length',
  version: '1.0.0',
  trigger: 'before-add',
  enabled: true,
  validate: (quad) => {
    if (quad.object.value.length > 100) {
      return { valid: false, errors: ['Value too long'] };
    }
    return { valid: true };
  }
};

manager.registerHook(customHook);
```

### Hook Execution

```javascript
import { DataFactory } from 'n3';
const { quad, namedNode, literal } = DataFactory;

const testQuad = quad(
  namedNode('http://example.org/subject'),
  namedNode('http://example.org/predicate'),
  literal('test value')
);

// Execute hooks
const result = await manager.executeByTrigger('before-add', testQuad);

if (result.valid) {
  console.log('✅ Validation passed');
} else {
  console.log('❌ Validation failed:', result.errors);
}
```

### Server Integration

```javascript
import { KnowledgeHookManager } from '@unrdf/hooks';
import { Store } from 'n3';

class KnowledgeServer {
  constructor() {
    this.store = new Store();
    this.hooks = new KnowledgeHookManager({ includeBuiltins: true });
  }

  async addQuad(quad) {
    // Validate with hooks before adding
    const result = await this.hooks.executeByTrigger('before-add', quad, this.store);

    if (!result.valid) {
      throw new Error(`Validation failed: ${result.errors.join(', ')}`);
    }

    // Add to store if validation passes
    this.store.addQuad(result.data);

    // Execute post-add hooks
    await this.hooks.executeByTrigger('after-add', quad, this.store);
  }
}
```

## Hook Triggers

Built-in hooks use these triggers:

- `before-add` - Execute before adding quads to store
- `after-add` - Execute after adding quads
- `before-remove` - Execute before removing quads
- `after-remove` - Execute after removing quads
- `before-query` - Execute before SPARQL query
- `after-query` - Execute after SPARQL query

## API Compatibility

The manager provides a class-based wrapper around the functional APIs:

- Uses `createHookRegistry()` internally
- Delegates to `registerHook()`, `executeHook()`, etc.
- Maintains same hook schemas and validation
- Full compatibility with functional API

## Performance

- Minimal overhead over functional API
- Single registry instance per manager
- Efficient hook lookup by trigger
- No unnecessary validation or copying

## See Also

- [Hook Definition](./define-hook.md) - Creating custom hooks
- [Hook Execution](./hook-executor.md) - Executing hooks
- [Built-in Hooks](./builtin-hooks.md) - Standard validation hooks
