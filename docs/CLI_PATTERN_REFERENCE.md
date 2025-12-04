# CLI Pattern Reference - How to Build Working UNRDF Packages

**Source**: @unrdf/cli (19/19 tests passing)
**Status**: ✅ Production-ready reference implementation

---

## The Golden Rules

1. **Export Everything Tests Import** - No orphan functions
2. **JSDoc on All Public Functions** - Complete param and return types
3. **Zod for Validation** - Schema-based input validation
4. **Pure Functions** - No OTEL in implementation, no side effects
5. **Clear Error Messages** - Wrap errors with context
6. **Integration-Style Tests** - Test real behavior, not existence

---

## File Structure Pattern

```
packages/cli/
├── src/
│   ├── index.mjs              # ✅ Export aggregator (ALL exports here)
│   ├── cli.mjs                # CLI entry point
│   └── cli/
│       ├── main.mjs           # Command dispatcher
│       └── commands/
│           ├── graph.mjs      # Graph commands implementation
│           ├── query.mjs      # Query commands implementation
│           ├── context.mjs    # Context commands implementation
│           └── convert.mjs    # Conversion commands implementation
├── test/
│   ├── cli/
│   │   └── cli.test.mjs       # ✅ Comprehensive integration tests
│   └── adversarial.test.mjs   # ✅ Advertised feature validation
└── package.json
```

---

## Pattern 1: Export Aggregator (index.mjs)

### ✅ Correct Pattern
```javascript
/**
 * @unrdf/cli
 *
 * CLI - Command-line Tools for Graph Operations
 *
 * @module @unrdf/cli
 */

// Export all command functions
export {
  loadGraph,
  saveGraph,
  createCommand,
  deleteCommand,
  describeCommand,
  mergeCommand,
  graphCommand,
} from './cli/commands/graph.mjs';

export {
  queryCommand,
  queryFileCommand,
  formatTable,
  formatJSON,
  formatCSV,
} from './cli/commands/query.mjs';

export {
  contextCommand,
  showCommand,
  addPrefixCommand,
  removePrefixCommand,
  normalizeCommand,
} from './cli/commands/context.mjs';

export {
  convertCommand,
  toTurtleCommand,
  toNTriplesCommand,
  toJSONCommand,
} from './cli/commands/convert.mjs';
```

**Key Points**:
- ALL exported functions listed explicitly
- Organized by source module
- Module-level JSDoc comment
- No "export *" wildcards (explicit is better)

### ❌ Wrong Pattern
```javascript
// Missing exports
export { createCommand } from './cli/commands/graph.mjs';
// deleteCommand, describeCommand, mergeCommand NOT EXPORTED

// Or using wildcards
export * from './cli/commands/graph.mjs'; // ❌ Not explicit
```

---

## Pattern 2: Function Implementation

### ✅ Correct Pattern
```javascript
/**
 * Load RDF graph from file
 * @param {string} filePath - Path to RDF file
 * @param {string} [format] - RDF format (auto-detected if not provided)
 * @returns {Promise<Object>} N3 Store
 */
export async function loadGraph(filePath, format) {
  const content = await readFile(filePath, 'utf8');
  const actualFormat = format || detectFormat(filePath);

  return new Promise((resolve, reject) => {
    const store = createStore();
    const parser = new Parser({ format: actualFormat });

    parser.parse(content, (error, quad, prefixes) => {
      if (error) {
        reject(new Error(`Parse error: ${error.message}`));
      } else if (quad) {
        addQuad(store, quad);
      } else {
        resolve(store);
      }
    });
  });
}
```

**Key Points**:
- Complete JSDoc with @param and @returns
- Optional parameters marked with [brackets]
- Clear error messages with context
- Direct return (no defensive guards)
- Pure async pattern (no OTEL callbacks)

### ❌ Wrong Pattern
```javascript
// Missing JSDoc
export async function loadGraph(filePath, format) {
  try {
    // Defensive guard
    if (!filePath || typeof filePath !== 'string') {
      return null; // ❌ Should throw error
    }

    const content = await readFile(filePath, 'utf8');
    // Missing error handling for parse errors
    return parseContent(content);
  } catch (e) {
    // Generic error
    throw new Error('Failed'); // ❌ No context
  }
}
```

---

## Pattern 3: Validation with Zod

### ✅ Correct Pattern
```javascript
import { z } from 'zod';

/**
 * Validation schema for graph commands
 */
const graphPathSchema = z.string().min(1, 'Path is required');
const formatSchema = z.enum(['turtle', 'ntriples', 'nquads', 'trig']).default('turtle');

/**
 * Create command - Create new RDF graph file
 */
export const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Create a new RDF graph file',
  },
  args: {
    path: {
      type: 'positional',
      description: 'Path to new graph file',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Output format (turtle, ntriples, nquads, trig)',
      default: 'turtle',
    },
  },
  async run({ args }) {
    // Validate inputs
    const path = graphPathSchema.parse(args.path);
    const format = formatSchema.parse(args.format);

    const store = createStore();
    await saveGraph(store, path, format);

    console.log(`✅ Created graph: ${path}`);
    console.log(`   Format: ${format}`);
  },
});
```

**Key Points**:
- Zod schemas at top of file
- Clear error messages in schema
- Validation before use
- User-friendly output

### ❌ Wrong Pattern
```javascript
// No validation
export const createCommand = defineCommand({
  async run({ args }) {
    // Direct use without validation
    const store = createStore();
    await saveGraph(store, args.path, args.format); // ❌ No validation
  },
});
```

---

## Pattern 4: Error Handling

### ✅ Correct Pattern
```javascript
export async function saveGraph(store, filePath, format) {
  const actualFormat = format || detectFormat(filePath);
  const writer = new Writer({ format: actualFormat });

  const quads = getQuads(store);
  quads.forEach(q => writer.addQuad(q));

  return new Promise((resolve, reject) => {
    writer.end((error, result) => {
      if (error) {
        // Add context to error
        reject(new Error(`Write error: ${error.message}`));
      } else {
        writeFile(filePath, result, 'utf8')
          .then(() => resolve())
          .catch(reject); // Let file errors bubble
      }
    });
  });
}
```

**Key Points**:
- Wrap errors with context ("Write error: ...")
- Let system errors bubble (writeFile throws)
- Clear error flow (reject with Error object)

### ❌ Wrong Pattern
```javascript
export async function saveGraph(store, filePath, format) {
  try {
    const writer = new Writer({ format });
    // ... code ...
    writer.end((error, result) => {
      if (error) {
        throw error; // ❌ Generic error, no context
      }
      // Missing error handling for writeFile
      writeFile(filePath, result, 'utf8');
    });
  } catch (e) {
    // Silent failure
    console.error(e); // ❌ Don't log and swallow
    return null; // ❌ Should throw
  }
}
```

---

## Pattern 5: Test Structure

### ✅ Correct Pattern
```javascript
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdir, rm, writeFile, readFile } from 'node:fs/promises';
import path from 'node:path';
import { createStore, addQuad, namedNode, literal, quad } from '@unrdf/core';
import { loadGraph, saveGraph } from '../../src/index.mjs';

const TEST_DIR = path.join(
  process.cwd(),
  `test-data-cli-${Date.now()}-${Math.random().toString(36).slice(2)}`
);

describe('@unrdf/cli - Graph Commands', () => {
  beforeEach(async () => {
    await mkdir(TEST_DIR, { recursive: true });
  });

  afterEach(async () => {
    await rm(TEST_DIR, { recursive: true, force: true });
  });

  it('should load and save Turtle graph', async () => {
    // Setup: Create test file
    const turtleContent = `
      @prefix ex: <http://example.org/> .
      ex:subject ex:predicate "object" .
    `;

    const filePath = path.join(TEST_DIR, 'test.ttl');
    await writeFile(filePath, turtleContent, 'utf8');

    // Execute: Load graph
    const store = await loadGraph(filePath);

    // Verify: Check quads
    const quads = store.getQuads();
    expect(quads.length).toBeGreaterThan(0);
  });

  it('should handle missing file', async () => {
    const fakePath = path.join(TEST_DIR, 'nonexistent.ttl');
    await expect(loadGraph(fakePath)).rejects.toThrow();
  });
});
```

**Key Points**:
- Setup/teardown with unique test directories
- Test real behavior (file I/O, parsing)
- Clear setup-execute-verify structure
- Error cases tested
- No mocking (integration-style)

### ❌ Wrong Pattern
```javascript
describe('@unrdf/cli - Graph Commands', () => {
  it('should have loadGraph function', () => {
    expect(typeof loadGraph).toBe('function'); // ❌ Existence check only
  });

  it('should load graph', async () => {
    const result = await loadGraph('test.ttl'); // ❌ No file setup
    expect(result).toBeDefined(); // ❌ Weak assertion
  });
});
```

---

## Pattern 6: Package Configuration

### ✅ Correct Pattern
```json
{
  "name": "@unrdf/cli",
  "version": "5.0.0-alpha.0",
  "type": "module",
  "main": "src/index.mjs",
  "exports": {
    ".": "./src/index.mjs",
    "./commands": "./src/commands/index.mjs"
  },
  "dependencies": {
    "@unrdf/core": "workspace:*",
    "@unrdf/federation": "workspace:*",
    "@unrdf/hooks": "workspace:*",
    "@unrdf/streaming": "workspace:*",
    "citty": "^0.1.6",
    "table": "^6.9.0",
    "yaml": "^2.8.1"
  },
  "scripts": {
    "test": "vitest run --coverage",
    "test:watch": "vitest --coverage"
  }
}
```

**Key Points**:
- Main entry point is index.mjs
- Explicit exports map
- Workspace dependencies for monorepo
- Test scripts configured

---

## Anti-Patterns to Avoid

### ❌ 1. OTEL in Implementation
```javascript
// ❌ WRONG
export function addQuad(store, quad) {
  return tracer.startActiveSpan('addQuad', span => {
    store.addQuad(quad);
    span.end();
  }); // Returns undefined because callback doesn't return
}

// ✅ RIGHT
export function addQuad(store, quad) {
  store.addQuad(quad);
}
```

### ❌ 2. Defensive Guards
```javascript
// ❌ WRONG
export function addQuad(store, quad) {
  if (!store || typeof store.addQuad !== 'function') {
    return; // Silent failure
  }
  store.addQuad(quad);
}

// ✅ RIGHT
export function addQuad(store, quad) {
  store.addQuad(quad); // Let it throw if invalid
}
```

### ❌ 3. Missing Return Values
```javascript
// ❌ WRONG
export function executeHook(hook, context) {
  if (hook.validate) {
    hook.validate(context);
    // Missing return
  }
}

// ✅ RIGHT
export function executeHook(hook, context) {
  if (hook.validate) {
    const valid = hook.validate(context);
    return { success: valid, quad: context.quad };
  }
  return { success: true, quad: context.quad };
}
```

### ❌ 4. Incomplete Exports
```javascript
// ❌ WRONG
// Implementation file has 5 functions
export function createManager() { ... }
function registerItem() { ... }  // Not exported
function unregisterItem() { ... }  // Not exported

// index.mjs only exports factory
export { createManager } from './manager.mjs';

// ✅ RIGHT
// Export ALL functions
export function createManager() { ... }
export function registerItem() { ... }
export function unregisterItem() { ... }

// index.mjs exports everything
export {
  createManager,
  registerItem,
  unregisterItem,
} from './manager.mjs';
```

---

## Checklist for New Packages

Use this checklist when creating or fixing packages:

### Implementation
- [ ] All functions have complete JSDoc
- [ ] All parameters have type annotations
- [ ] Return types specified in JSDoc
- [ ] Zod schemas for validation
- [ ] Clear error messages with context
- [ ] No OTEL in implementation code
- [ ] No defensive guards (let errors throw)
- [ ] Pure functions (no side effects except I/O)

### Exports
- [ ] All public functions exported from implementation files
- [ ] All exports aggregated in index.mjs
- [ ] Exports organized by source module
- [ ] No orphan functions (implemented but not exported)
- [ ] package.json exports map configured

### Testing
- [ ] Integration-style tests (not existence checks)
- [ ] Setup/teardown with unique test dirs
- [ ] Test real behavior (file I/O, data processing)
- [ ] Error cases tested
- [ ] All exported functions tested
- [ ] Tests import from index.mjs (not implementation files)

### Quality
- [ ] No unused imports
- [ ] No unused variables
- [ ] Consistent naming conventions
- [ ] No hardcoded secrets
- [ ] No circular dependencies
- [ ] Test pass rate > 80%

---

## How to Use This Reference

1. **Read CLI source code** - See the patterns in action
2. **Copy structure** - Use CLI as template for new packages
3. **Run tests** - Verify patterns work (19/19 passing)
4. **Apply patterns** - Fix other packages using these patterns

---

**Reference Implementation**: packages/cli/src/
**Test Suite**: packages/cli/test/
**Pass Rate**: 19/19 (100%)
**Status**: ✅ Production-ready

Use this as your reference when fixing other packages!
