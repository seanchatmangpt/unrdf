# UNRDF DX/UX Architecture Guide

**Version**: 1.0.0
**Status**: Canonical Integration Guide
**Last Updated**: 2025-12-25

## Overview

This document integrates all DX/UX architecture standards for UNRDF. Use this as the single source of truth for building consistent, developer-friendly packages.

## Architecture Stack

```
┌─────────────────────────────────────────┐
│         Developer Experience            │
│  (Consistent APIs, Clear Docs, Fast)    │
└────────────────┬────────────────────────┘
                 │
┌────────────────┴────────────────────────┐
│         Package Structure               │
│  (Predictable Layout, Standard Files)   │
└────────────────┬────────────────────────┘
                 │
┌────────────────┴────────────────────────┐
│         API Design                      │
│  (Naming, Parameters, Return Values)    │
└────────────────┬────────────────────────┘
                 │
┌────────────────┴────────────────────────┐
│         Configuration                   │
│  (Env Vars, Validation, Defaults)       │
└────────────────┬────────────────────────┘
                 │
┌────────────────┴────────────────────────┐
│         Plugin Architecture             │
│  (Extensibility, Lifecycle, Events)     │
└─────────────────────────────────────────┘
```

## Core Principles

### 1. Consistency Reduces Cognitive Load

**Problem**: Every package has different structure/APIs
**Solution**: Standard patterns across all packages

**Evidence**: 20+ packages in UNRDF follow same structure → <5min navigation time

### 2. Fail Fast with Clear Errors

**Problem**: Silent failures, cryptic error messages
**Solution**: Zod validation, early input checking, descriptive errors

**Pattern**:
```javascript
function createStore(options) {
  try {
    ConfigSchema.parse(options);  // Validate immediately
  } catch (error) {
    throw new TypeError(`Invalid options: ${error.message}`);
  }
  // Proceed with valid input
}
```

### 3. Sensible Defaults, Easy Configuration

**Problem**: Too many required parameters, complex setup
**Solution**: Defaults for 80% use cases, simple override

**Pattern**:
```javascript
const config = createConfig({
  // Override only what you need
  query: { timeout: 10000 }
  // Everything else uses defaults
});
```

### 4. Progressive Disclosure

**Problem**: Users overwhelmed by complexity upfront
**Solution**: Simple quick start → advanced features as needed

**Documentation Pattern**:
1. README: 5-minute quick start
2. QUICKSTART: 15-minute tutorial
3. docs/API.md: Complete reference
4. docs/GUIDE.md: Advanced patterns

## Quick Reference

### Package Structure Checklist

When creating/refactoring a package:

- [ ] Directory structure matches [PACKAGE-STRUCTURE.md](./PACKAGE-STRUCTURE.md)
- [ ] `package.json` has required fields + exports
- [ ] `src/index.mjs` exports public API with JSDoc
- [ ] Tests in `test/` mirror `src/` structure
- [ ] `README.md` has installation + quick start
- [ ] `vitest.config.mjs` uses standard config
- [ ] All exports have JSDoc documentation
- [ ] File sizes under 500 lines

**Verification**:
```bash
timeout 5s find src -name '*.mjs' -exec wc -l {} + | awk '$1 > 500'
```

### API Design Checklist

When designing an API:

- [ ] Function names follow verb patterns (`create*`, `execute*`, `get*`)
- [ ] Parameters ordered: required → optional → options object
- [ ] Options object has defaults in destructuring
- [ ] Return types consistent (not mixed sync/async)
- [ ] Errors validated with Zod
- [ ] All exports have JSDoc with `@param`, `@returns`, `@example`
- [ ] Async functions support `AbortSignal`
- [ ] Large datasets use iterators
- [ ] Tests cover happy path + edge cases + errors

**Example**:
```javascript
/**
 * Creates a new store
 *
 * @param {Quad[]} [initialQuads=[]] - Initial quads
 * @param {Object} [options={}] - Configuration
 * @param {number} [options.capacity=10000] - Capacity
 * @returns {Store} Store instance
 * @throws {TypeError} If initialQuads invalid
 * @example
 * const store = createStore([quad1, quad2]);
 */
export function createStore(initialQuads = [], options = {}) {
  // Implementation
}
```

### Configuration Checklist

When adding configuration:

- [ ] Zod schema defined for validation
- [ ] Environment variables parsed (UNRDF_*)
- [ ] Defaults provided for all options
- [ ] Precedence: env > explicit > defaults
- [ ] `createConfig()` merges and validates
- [ ] Documentation shows all options

**Usage**:
```javascript
import { createConfig } from '@unrdf/core/config';

const config = createConfig({
  query: { timeout: 10000 }  // Override default
});
```

### Plugin Checklist

When creating a plugin:

- [ ] Implements required fields (`name`, `version`)
- [ ] Has `init()` and `cleanup()` lifecycle hooks
- [ ] Event handlers return modified event data
- [ ] Validates inputs with Zod
- [ ] Handles errors gracefully
- [ ] Documentation includes example usage
- [ ] Tests cover lifecycle and events

**Template**:
```javascript
export const myPlugin = {
  name: 'my-plugin',
  version: '1.0.0',

  async init() {
    // Initialize
  },

  on: {
    beforeQuery: async (event) => {
      // Handle event
      return event;
    },
  },

  async cleanup() {
    // Cleanup
  },
};
```

## Integration Workflow

### Creating a New Package

**Step 1**: Copy template
```bash
cp -r docs/templates/package-template packages/new-package
cd packages/new-package
```

**Step 2**: Customize `package.json`
```json
{
  "name": "@unrdf/new-package",
  "description": "What this package does",
  // Update other fields
}
```

**Step 3**: Implement `src/` modules
```bash
# Follow API design guidelines
# Use factory functions (createX)
# Validate with Zod
# Document with JSDoc
```

**Step 4**: Write tests
```bash
# Mirror src/ structure in test/
# Cover happy path, edges, errors
pnpm test
```

**Step 5**: Document
```bash
# Update README.md with examples
# Update docs/API.md with full reference
# Create docs/GUIDE.md for advanced usage
```

**Step 6**: Validate
```bash
timeout 5s pnpm test
timeout 5s pnpm lint
```

### Adding Configuration to Package

**Step 1**: Define schema in `src/config.mjs`
```javascript
import { z } from 'zod';

export const PackageConfigSchema = z.object({
  option1: z.string().default('default'),
  option2: z.number().min(0).default(100),
});
```

**Step 2**: Use `createConfig` pattern
```javascript
import { createConfig } from '@unrdf/core/config';
import { PackageConfigSchema } from './config.mjs';

export function initializePackage(options = {}) {
  const config = createConfig(options, PackageConfigSchema);
  // Use config
}
```

**Step 3**: Document environment variables
```markdown
## Configuration

Environment variables:
- `UNRDF_PACKAGE_OPTION1` - Description (default: 'default')
- `UNRDF_PACKAGE_OPTION2` - Description (default: 100)
```

### Adding Plugin Support

**Step 1**: Create plugin manager
```javascript
import { createPluginManager } from '@unrdf/core/plugins';

const manager = createPluginManager();
```

**Step 2**: Define event points
```javascript
export async function executeQuery(query, options) {
  // Before query
  const beforeEvent = await manager.execute('beforeQuery', {
    query,
    options,
  });

  // Execute
  const results = runQuery(beforeEvent.query);

  // After query
  await manager.execute('afterQuery', {
    query,
    results,
  });

  return results;
}
```

**Step 3**: Document events
```markdown
## Plugin Events

- `beforeQuery({ query, options })` - Before query execution
- `afterQuery({ query, results })` - After successful query
- `onError({ query, error })` - On query error
```

## File Organization Patterns

### Small Package (< 5 modules)

```
package-name/
├── src/
│   ├── index.mjs        # Exports
│   ├── core.mjs         # Main implementation
│   ├── validation.mjs   # Zod schemas
│   └── constants.mjs    # Constants
└── test/
    └── core.test.mjs
```

### Medium Package (5-15 modules)

```
package-name/
├── src/
│   ├── index.mjs
│   ├── module-1.mjs
│   ├── module-2.mjs
│   ├── module-3.mjs
│   ├── validation.mjs
│   ├── constants.mjs
│   └── internal/
│       └── helpers.mjs
└── test/
    ├── module-1.test.mjs
    ├── module-2.test.mjs
    └── module-3.test.mjs
```

### Large Package (15+ modules)

```
package-name/
├── src/
│   ├── index.mjs
│   ├── feature-1/
│   │   ├── index.mjs
│   │   ├── module-a.mjs
│   │   └── module-b.mjs
│   ├── feature-2/
│   │   ├── index.mjs
│   │   ├── module-c.mjs
│   │   └── module-d.mjs
│   ├── validation/
│   │   └── index.mjs
│   └── internal/
│       └── shared.mjs
└── test/
    ├── feature-1/
    │   ├── module-a.test.mjs
    │   └── module-b.test.mjs
    └── feature-2/
        ├── module-c.test.mjs
        └── module-d.test.mjs
```

## Testing Patterns

### Unit Test Template

```javascript
import { describe, it, expect, beforeEach } from 'vitest';
import { createThing } from '../src/thing.mjs';

describe('createThing', () => {
  it('creates with defaults', () => {
    const thing = createThing();
    expect(thing).toBeDefined();
  });

  it('creates with options', () => {
    const thing = createThing({ option: 'value' });
    expect(thing.option).toBe('value');
  });

  it('validates input', () => {
    expect(() => createThing({ invalid: true })).toThrow(TypeError);
  });
});
```

### Integration Test Template

```javascript
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createStore } from '@unrdf/core';
import { createPlugin } from '../src/plugin.mjs';

describe('plugin integration', () => {
  let store;
  let manager;

  beforeEach(async () => {
    store = createStore();
    manager = createPluginManager();
    manager.register(createPlugin());
    await manager.init();
  });

  afterEach(async () => {
    await manager.cleanup();
  });

  it('handles query lifecycle', async () => {
    const results = await store.query('SELECT * WHERE { ?s ?p ?o }');
    expect(results).toBeDefined();
  });
});
```

## Documentation Patterns

### README.md Structure

```markdown
# @unrdf/package-name

> One-sentence description

## Installation

[Install command]

## Quick Start

[5-line example that works]

## Features

- Feature 1
- Feature 2
- Feature 3

## API

### Main Function

[Signature, params, returns, example]

## Advanced

[Link to detailed docs]

## License

MIT
```

### API.md Structure

```markdown
# API Reference

## Functions

### `functionName(params)`

Description.

**Parameters**: [List with types and defaults]

**Returns**: [Type and description]

**Throws**: [Error types and when]

**Example**: [Working code]

## Classes

[If any]

## Constants

[If any]
```

## Performance Patterns

### Use Iterators for Large Data

```javascript
// ✅ GOOD: Memory efficient
export function* iterateQuads(store) {
  for (const quad of store._quads) {
    yield quad;
  }
}

// ❌ BAD: Loads all into memory
export function getQuads(store) {
  return store._quads.slice();
}
```

### Lazy Initialization

```javascript
// ✅ GOOD: Initialize on first use
export function createStore() {
  let initialized = false;
  let index = null;

  return {
    query(sparql) {
      if (!initialized) {
        index = buildIndex(this._quads);
        initialized = true;
      }
      return executeQuery(sparql, index);
    },
  };
}
```

### Caching with TTL

```javascript
// ✅ GOOD: Cache with expiration
const cache = new Map();
const TTL = 60000;

function getCached(key) {
  const entry = cache.get(key);
  if (entry && Date.now() - entry.time < TTL) {
    return entry.value;
  }
  cache.delete(key);
  return null;
}
```

## Error Handling Patterns

### Input Validation

```javascript
import { z } from 'zod';

const OptionsSchema = z.object({
  timeout: z.number().min(0),
});

function doSomething(options) {
  try {
    OptionsSchema.parse(options);
  } catch (error) {
    throw new TypeError(`Invalid options: ${error.message}`);
  }
  // Proceed safely
}
```

### Async Error Handling

```javascript
async function fetchData(url, options = {}) {
  const { signal } = options;

  try {
    const response = await fetch(url, { signal });
    return await response.json();
  } catch (error) {
    if (error.name === 'AbortError') {
      throw new Error('Request cancelled');
    }
    throw new Error(`Fetch failed: ${error.message}`);
  }
}
```

## Complete Example: New Package

See `/docs/templates/package-template/` for a complete, working example that demonstrates:

- Package structure
- API design (factory functions, options objects)
- Configuration (Zod validation, defaults)
- Testing (unit tests with vitest)
- Documentation (README, JSDoc, API reference)

**To use**:
```bash
cp -r docs/templates/package-template packages/my-package
cd packages/my-package
# Customize and implement
```

## Related Documents

- [Package Structure Standard](./PACKAGE-STRUCTURE.md) - Directory layout and file organization
- [API Design Guidelines](./API-DESIGN.md) - Naming conventions and function signatures
- [Plugin Architecture](./PLUGIN-ARCHITECTURE.md) - Extensibility patterns
- [Configuration](../packages/core/src/config.mjs) - Config management implementation

## Quick Commands

```bash
# Create new package from template
cp -r docs/templates/package-template packages/new-package

# Validate package structure
timeout 5s find src -name '*.mjs' -exec wc -l {} + | awk '$1 > 500'

# Run tests
timeout 10s pnpm test

# Run linter
timeout 5s pnpm lint

# Check JSDoc coverage
timeout 5s pnpm lint 2>&1 | grep "missing-jsdoc"

# Check file counts
ls -1 src/*.mjs | wc -l
```

## Success Metrics

**Package Quality**:
- [ ] Test pass rate: 100%
- [ ] Test coverage: >80%
- [ ] Lint errors: 0
- [ ] JSDoc coverage: 100% of exports
- [ ] File sizes: All < 500 lines

**Developer Experience**:
- [ ] Time to first use: < 5 minutes
- [ ] Time to understand package: < 15 minutes
- [ ] Documentation completeness: README + API + Guide
- [ ] Example quality: All examples runnable

**Consistency**:
- [ ] Follows package structure standard
- [ ] Follows API design guidelines
- [ ] Follows naming conventions
- [ ] Uses standard configuration pattern

---

**Version History**:
- 1.0.0 (2025-12-25): Initial DX/UX architecture integration guide
