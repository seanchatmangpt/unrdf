# UNRDF Vitest Testing Standard

**Version:** 1.0.0
**Last Updated:** 2025-12-04
**Applies To:** All 26 UNRDF subprojects (11 packages + examples)

## 📋 Overview

This document defines the comprehensive Vitest testing standard for all UNRDF subprojects, ensuring consistency, quality, and maintainability across the monorepo.

## 🎯 Core Principles

1. **Consistency**: All subprojects follow the same patterns
2. **80/20 Rule**: Focus tests on critical functionality
3. **Environment-Aware**: Node.js vs Browser testing strategies
4. **Coverage-Driven**: Maintain 80%+ coverage minimum
5. **AI-Compatible**: Single-threaded execution for agent compatibility

## 📁 Standard Directory Structure

```
<package-or-example>/
├── src/
│   └── *.mjs (source files)
├── test/
│   ├── <feature>.test.mjs (test files)
│   ├── setup.mjs (optional setup)
│   ├── fixtures/ (optional test data)
│   └── utils/ (optional test utilities)
├── vitest.config.mjs (configuration)
└── package.json (with test scripts)
```

## 🔧 Configuration Templates

### Template 1: Node.js Environment (Default)

**Use for:** Core, Hooks, Federation, Streaming, CLI, Knowledge-Engine, Dark-Matter, Project-Engine

```javascript
/**
 * @fileoverview Vitest configuration for @unrdf/<package>
 * Node.js environment for RDF operations
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Environment
    environment: 'node',
    globals: true,

    // Single-threaded execution for AI agent compatibility
    pool: 'forks',
    poolOptions: {
      forks: {
        singleFork: true,
      },
    },
    concurrent: false,
    maxConcurrency: 1,

    // Timeouts
    testTimeout: 30_000,
    hookTimeout: 30_000,

    // Test patterns
    include: ['test/**/*.test.mjs'],
    exclude: [
      'node_modules/**',
      'dist/**',
      'test/fixtures/**',
      'test/utils/**',
    ],

    // Coverage
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: [
        'node_modules/**',
        'dist/**',
        'test/**',
        '**/*.config.mjs',
        'coverage/**',
      ],
      // Coverage thresholds
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 75,
        statements: 80,
      },
      all: true,
    },

    // Reporting
    reporter: ['verbose', 'json', 'html'],
    outputFile: {
      json: './coverage/test-results.json',
      html: './coverage/test-results.html',
    },

    // Configuration
    isolate: true,
    passWithNoTests: true,
    retry: 2,
    bail: 0,

    // Dependencies
    deps: {
      external: [],
      inline: [],
    },

    // Server
    server: {
      sourcemap: true,
    },
  },

  // Resolve
  resolve: {
    alias: {},
  },

  // Optimize
  optimizeDeps: {
    include: ['n3', 'zod', '@comunica/query-sparql'],
    exclude: [],
  },

  // Build
  build: {
    sourcemap: true,
    minify: false,
    target: 'node18',
  },
});
```

### Template 2: Browser Environment (jsdom)

**Use for:** Browser, Composables

```javascript
/**
 * @fileoverview Vitest configuration for @unrdf/<package>
 * jsdom environment for browser operations
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Browser environment
    environment: 'jsdom',
    globals: true,

    // Single-threaded execution
    pool: 'forks',
    poolOptions: {
      forks: {
        singleFork: true,
      },
    },
    concurrent: false,
    maxConcurrency: 1,

    // Timeouts
    testTimeout: 30_000,
    hookTimeout: 30_000,

    // Test patterns
    include: ['test/**/*.test.mjs'],
    exclude: [
      'node_modules/**',
      'dist/**',
      'test/fixtures/**',
      'test/utils/**',
    ],

    // Coverage
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: [
        'node_modules/**',
        'dist/**',
        'test/**',
        '**/*.config.mjs',
        'coverage/**',
      ],
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 75,
        statements: 80,
      },
      all: true,
    },

    // Reporting
    reporter: ['verbose', 'json', 'html'],
    outputFile: {
      json: './coverage/test-results.json',
      html: './coverage/test-results.html',
    },

    // Configuration
    isolate: true,
    passWithNoTests: true,
    retry: 2,
    bail: 0,

    // Dependencies
    deps: {
      external: [],
      inline: [],
    },

    // Server
    server: {
      sourcemap: true,
    },
  },

  // Resolve
  resolve: {
    alias: {},
  },

  // Optimize
  optimizeDeps: {
    include: ['n3', 'zod'],
    exclude: [],
  },

  // Build
  build: {
    sourcemap: true,
    minify: false,
    target: 'es2022',
  },
});
```

### Template 3: Example Projects

**Use for:** All example subprojects in packages/*/examples/*

```javascript
/**
 * @fileoverview Vitest configuration for <example-name>
 * Minimal configuration for example projects
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Environment (node or jsdom)
    environment: 'node', // or 'jsdom' for browser examples

    // Basic configuration
    globals: true,
    testTimeout: 30_000,

    // Test patterns
    include: ['test/**/*.test.mjs'],

    // Coverage
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json'],
      include: ['src/**/*.mjs', '*.mjs'],
      exclude: ['node_modules/**', 'dist/**', 'test/**'],
      thresholds: {
        lines: 70,    // Lower threshold for examples
        functions: 70,
        branches: 60,
        statements: 70,
      },
    },

    // Reporting
    reporter: ['verbose'],
  },
});
```

## 📝 Test File Standards

### File Naming Convention

- Test files: `<feature>.test.mjs`
- Setup files: `setup.mjs`
- Utility files: `utils/<utility>.mjs`
- Fixture files: `fixtures/<fixture>.json` or `.mjs`

### Test File Structure

```javascript
/**
 * @fileoverview <Package Name> - <Feature> Test Suite
 * <Brief description of what is being tested>
 *
 * @vitest-environment node (or jsdom for browser tests)
 */

import { describe, it, expect, beforeEach, afterEach, beforeAll, afterAll } from 'vitest';
import { functionUnderTest } from '../src/index.mjs';

// Global test data
const TEST_DATA = {
  // test fixtures
};

describe('<PackageName> - <Feature>', () => {
  // Lifecycle hooks
  beforeAll(() => {
    // Setup before all tests
  });

  afterAll(() => {
    // Cleanup after all tests
  });

  beforeEach(() => {
    // Setup before each test
  });

  afterEach(() => {
    // Cleanup after each test
  });

  // Test groups
  describe('<SubFeature>', () => {
    it('should <behavior>', () => {
      // Arrange
      const input = 'test';

      // Act
      const result = functionUnderTest(input);

      // Assert
      expect(result).toBe('expected');
    });

    it('should handle edge cases', () => {
      expect(functionUnderTest(null)).toBe(null);
      expect(functionUnderTest('')).toBe('');
    });

    it('should throw on invalid input', () => {
      expect(() => functionUnderTest(undefined)).toThrow(TypeError);
    });
  });

  describe('Integration', () => {
    it('should integrate with other components', async () => {
      // Integration test
    });
  });

  describe('Error Handling', () => {
    it('should handle errors gracefully', () => {
      // Error handling test
    });
  });
});
```

## 🎨 Test Categories

### 1. Unit Tests

**Purpose:** Test individual functions in isolation

```javascript
describe('Unit: <Function Name>', () => {
  it('should return expected output for valid input', () => {
    expect(add(1, 2)).toBe(3);
  });

  it('should handle edge cases', () => {
    expect(add(0, 0)).toBe(0);
    expect(add(-1, 1)).toBe(0);
  });

  it('should throw on invalid input', () => {
    expect(() => add('a', 'b')).toThrow(TypeError);
  });
});
```

### 2. Integration Tests

**Purpose:** Test component interactions

```javascript
describe('Integration: <Component Interaction>', () => {
  it('should integrate multiple components', () => {
    const store = createStore();
    const quad = createQuad('s', 'p', 'o');
    addQuad(store, quad);
    const result = getQuads(store);
    expect(result.length).toBe(1);
  });
});
```

### 3. Example Tests

**Purpose:** Demonstrate real-world usage

```javascript
describe('Example: <Use Case>', () => {
  it('should demonstrate typical workflow', async () => {
    // Setup
    const store = createStore();

    // Load data
    await parseAndLoad(store, turtleData);

    // Query
    const results = await executeQuery(store, sparqlQuery);

    // Verify
    expect(results.length).toBeGreaterThan(0);
  });
});
```

### 4. Error Handling Tests

**Purpose:** Verify error scenarios

```javascript
describe('Error Handling', () => {
  it('should reject null inputs', () => {
    expect(() => functionUnderTest(null)).toThrow(TypeError);
  });

  it('should handle missing resources', async () => {
    await expect(loadResource('nonexistent')).rejects.toThrow();
  });
});
```

## 🧪 Assertion Patterns

### Common Assertions

```javascript
// Equality
expect(value).toBe(expected);              // Strict equality
expect(value).toEqual(expected);            // Deep equality
expect(value).toStrictEqual(expected);      // Strict deep equality

// Truthiness
expect(value).toBeTruthy();
expect(value).toBeFalsy();
expect(value).toBeDefined();
expect(value).toBeUndefined();
expect(value).toBeNull();

// Numbers
expect(number).toBeGreaterThan(0);
expect(number).toBeGreaterThanOrEqual(0);
expect(number).toBeLessThan(100);
expect(number).toBeCloseTo(0.3, 5);

// Strings
expect(string).toContain('substring');
expect(string).toMatch(/regex/);

// Arrays
expect(array).toContain(item);
expect(array).toContainEqual(object);
expect(array).toHaveLength(3);

// Objects
expect(object).toHaveProperty('key');
expect(object).toHaveProperty('key', value);
expect(object).toMatchObject({ key: value });

// Errors
expect(() => fn()).toThrow();
expect(() => fn()).toThrow(TypeError);
expect(() => fn()).toThrow('error message');
await expect(asyncFn()).rejects.toThrow();

// Async
await expect(promise).resolves.toBe(value);
await expect(promise).rejects.toThrow();
```

### RDF-Specific Assertions

```javascript
// Store assertions
expect(store).toBeDefined();
expect(typeof store.getQuads).toBe('function');
expect(countQuads(store)).toBe(expectedCount);

// Quad assertions
expect(quad.subject.value).toBe('http://example.org/subject');
expect(quad.predicate.value).toBe('http://example.org/predicate');
expect(quad.object.termType).toBe('Literal');

// SPARQL assertions
expect(results).toBeDefined();
expect(Array.isArray(results)).toBe(true);
expect(results.length).toBeGreaterThan(0);
expect(results[0]).toHaveProperty('name');
```

## 📦 Package.json Scripts

### Standard Scripts for All Packages

```json
{
  "scripts": {
    "test": "vitest run --coverage",
    "test:fast": "vitest run",
    "test:watch": "vitest --coverage",
    "test:ui": "vitest --ui",
    "test:coverage": "vitest run --coverage",
    "test:coverage:ui": "vitest --ui --coverage"
  }
}
```

### Additional Scripts for Root Package

```json
{
  "scripts": {
    "test": "vitest run --coverage",
    "test:fast": "vitest run --no-coverage",
    "test:watch": "vitest --coverage",
    "test:packages": "pnpm -r --filter './packages/*' test",
    "test:examples": "pnpm -r --filter './packages/*/examples/*' test",
    "test:ci": "vitest run --coverage --reporter=json --reporter=html"
  }
}
```

## 📊 Coverage Standards

### Minimum Coverage Thresholds

| Package Type | Lines | Functions | Branches | Statements |
|-------------|-------|-----------|----------|------------|
| Core Packages | 80% | 80% | 75% | 80% |
| Browser Packages | 80% | 80% | 75% | 80% |
| Examples | 70% | 70% | 60% | 70% |
| Integration | 70% | 70% | 60% | 70% |

### Coverage Exclusions

**Always exclude:**
- `node_modules/**`
- `dist/**`
- `coverage/**`
- `test/**`
- `**/*.config.mjs`
- `**/*.config.js`

**Optional exclusions (case-by-case):**
- `fixtures/**`
- `utils/**` (if purely for testing)
- `examples/**` (if examples are in package root)

## 🔄 Special Handling by Package Type

### Core Packages (core, hooks, federation, streaming)

**Characteristics:**
- Pure Node.js environment
- Fast unit tests
- Mock external dependencies
- Focus on algorithm testing

**Example Test:**
```javascript
describe('@unrdf/core - Store Operations', () => {
  it('should add quads efficiently', () => {
    const store = createStore();
    const quad = createQuad('s', 'p', 'o');
    addQuad(store, quad);
    expect(countQuads(store)).toBe(1);
  });
});
```

### Browser Packages (browser, composables)

**Characteristics:**
- jsdom environment
- Browser globals available
- IndexedDB simulation
- Service worker mocking

**Example Test:**
```javascript
/**
 * @vitest-environment jsdom
 */
describe('@unrdf/browser - IndexedDB', () => {
  it('should store quads in IndexedDB', async () => {
    const store = createIndexedDBStore('test-db');
    await openIndexedDBStore(store);
    // Test IndexedDB operations
  });
});
```

### Advanced Packages (knowledge-engine, dark-matter)

**Characteristics:**
- Large dataset tests
- Performance assertions
- Inference validation
- Complex query testing

**Example Test:**
```javascript
describe('@unrdf/knowledge-engine - Inference', () => {
  it('should infer transitive relationships', async () => {
    const engine = createKnowledgeEngine();
    await engine.load(ontologyData);
    const inferred = await engine.infer(query);
    expect(inferred.length).toBeGreaterThan(0);
  });
});
```

### Integration Packages (full-stack examples)

**Characteristics:**
- Multi-environment tests
- Server + Client testing
- API integration tests
- End-to-end scenarios

**Example Test:**
```javascript
describe('Full-Stack Integration', () => {
  it('should handle client-server workflow', async () => {
    // Server test (Node.js)
    const server = await startServer();

    // Client test (would be in separate jsdom test)
    // const client = createClient();

    await server.close();
  });
});
```

## 🎯 Test Organization Best Practices

### 1. Arrange-Act-Assert (AAA) Pattern

```javascript
it('should multiply two numbers', () => {
  // Arrange
  const a = 2;
  const b = 3;

  // Act
  const result = multiply(a, b);

  // Assert
  expect(result).toBe(6);
});
```

### 2. One Assertion Per Test (Preferred)

```javascript
// ✅ Good: Single assertion
it('should return 6 when multiplying 2 and 3', () => {
  expect(multiply(2, 3)).toBe(6);
});

// ⚠️ Acceptable: Related assertions
it('should handle zero correctly', () => {
  expect(multiply(0, 5)).toBe(0);
  expect(multiply(5, 0)).toBe(0);
});

// ❌ Avoid: Unrelated assertions
it('should do multiple things', () => {
  expect(multiply(2, 3)).toBe(6);
  expect(add(2, 3)).toBe(5);  // Unrelated
});
```

### 3. Descriptive Test Names

```javascript
// ✅ Good: Describes behavior
it('should throw TypeError when input is null', () => {
  expect(() => process(null)).toThrow(TypeError);
});

// ❌ Bad: Vague description
it('should work', () => {
  expect(process('input')).toBeTruthy();
});
```

### 4. Test Data Organization

```javascript
// ✅ Good: Organized test data
const TEST_QUADS = {
  alice: {
    subject: namedNode('http://example.org/alice'),
    predicate: FOAF.name,
    object: literal('Alice'),
    graph: defaultGraph(),
  },
  bob: {
    subject: namedNode('http://example.org/bob'),
    predicate: FOAF.name,
    object: literal('Bob'),
    graph: defaultGraph(),
  },
};

describe('Store Operations', () => {
  it('should add Alice quad', () => {
    const store = createStore();
    addQuad(store, TEST_QUADS.alice);
    expect(countQuads(store)).toBe(1);
  });
});
```

## 🚫 Common Anti-Patterns to Avoid

### 1. Don't Use `test()` - Use `it()`

```javascript
// ❌ Avoid: test()
test('should work', () => {});

// ✅ Use: it()
it('should work', () => {});
```

### 2. Don't Mix Environments in Same File

```javascript
// ❌ Avoid: Mixing environments
describe('Browser tests', () => {
  // Uses window, document
});
describe('Node.js tests', () => {
  // Uses fs, path
});

// ✅ Use: Separate files
// browser.test.mjs (with @vitest-environment jsdom)
// node.test.mjs (with @vitest-environment node)
```

### 3. Don't Skip Tests Without Comments

```javascript
// ❌ Avoid: Silent skip
it.skip('should do something', () => {});

// ✅ Use: Documented skip
// TODO: Re-enable when feature X is implemented
it.skip('should do something', () => {});
```

### 4. Don't Use Magic Numbers

```javascript
// ❌ Avoid: Magic numbers
expect(results.length).toBe(42);

// ✅ Use: Named constants
const EXPECTED_RESULT_COUNT = 42;
expect(results.length).toBe(EXPECTED_RESULT_COUNT);
```

## 🔧 Setup and Cleanup Patterns

### beforeEach / afterEach

```javascript
describe('Store Operations', () => {
  let store;

  beforeEach(() => {
    store = createStore();
  });

  afterEach(() => {
    // Cleanup if needed
    if (store) {
      store.clear();
    }
  });

  it('should start with empty store', () => {
    expect(countQuads(store)).toBe(0);
  });
});
```

### beforeAll / afterAll

```javascript
describe('Database Tests', () => {
  let db;

  beforeAll(async () => {
    db = await connectDatabase();
  });

  afterAll(async () => {
    await db.close();
  });

  it('should query database', async () => {
    const results = await db.query('SELECT * FROM users');
    expect(results).toBeDefined();
  });
});
```

### Shared Setup Files

```javascript
// test/setup/cleanup-hooks.mjs
import { afterEach } from 'vitest';

afterEach(() => {
  // Global cleanup logic
  clearTestDatabases();
  resetMocks();
});
```

## 🎯 Package-Specific Standards

### @unrdf/core

```javascript
// vitest.config.mjs
export default defineConfig({
  test: {
    environment: 'node',
    include: ['test/**/*.test.mjs'],
    coverage: {
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 75,
        statements: 80,
      },
    },
  },
});
```

### @unrdf/browser

```javascript
// vitest.config.mjs
export default defineConfig({
  test: {
    environment: 'jsdom',
    include: ['test/**/*.test.mjs'],
    coverage: {
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 75,
        statements: 80,
      },
    },
  },
});
```

### @unrdf/knowledge-engine

```javascript
// vitest.config.mjs
export default defineConfig({
  test: {
    environment: 'node',
    testTimeout: 60_000, // Longer timeout for inference
    include: ['test/**/*.test.mjs'],
    coverage: {
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 75,
        statements: 80,
      },
    },
  },
});
```

## 📝 Test Documentation Standards

### File-Level Documentation

```javascript
/**
 * @fileoverview @unrdf/core - RDF Store Operations Test Suite
 *
 * Comprehensive tests for RDF store CRUD operations including:
 * - Store creation and initialization
 * - Quad addition and removal
 * - Query operations
 * - Canonicalization
 * - Error handling
 *
 * @vitest-environment node
 */
```

### Test-Level Documentation

```javascript
describe('Store Operations', () => {
  /**
   * Test: Adding quads to store
   * Verifies that quads can be added and retrieved correctly
   */
  it('should add quads to store', () => {
    // Test implementation
  });
});
```

## 🚀 Running Tests

### Local Development

```bash
# Run all tests
pnpm test

# Run specific package tests
pnpm --filter @unrdf/core test

# Run tests in watch mode
pnpm test:watch

# Run tests with coverage
pnpm test:coverage
```

### CI/CD

```bash
# Run all tests with coverage reporting
pnpm test:ci

# Run tests for changed packages only
pnpm test:changed
```

## 📊 Coverage Reporting

### Viewing Coverage Reports

```bash
# Generate and view HTML report
pnpm test:coverage
open coverage/index.html

# View text report
pnpm test --coverage

# Generate JSON report for CI
pnpm test --coverage --reporter=json
```

### Coverage Output Files

- `coverage/index.html` - Interactive HTML report
- `coverage/coverage-final.json` - JSON coverage data
- `coverage/test-results.json` - Test execution results
- `coverage/test-results.html` - HTML test results

## 🔍 Debugging Tests

### Using Vitest UI

```bash
pnpm test:ui
```

### Using Node.js Debugger

```json
{
  "scripts": {
    "test:debug": "node --inspect-brk ./node_modules/vitest/vitest.mjs run"
  }
}
```

### Console Logging

```javascript
it('should debug test', () => {
  const result = functionUnderTest(input);
  console.log('Debug output:', result);
  expect(result).toBe(expected);
});
```

## 📚 Additional Resources

- [Vitest Documentation](https://vitest.dev)
- [UNRDF Testing Guide](./TESTING.md)
- [UNRDF Contributing Guide](./CONTRIBUTING.md)
- [UNRDF Architecture](./ARCHITECTURE.md)

## 📋 Checklist: Implementing Tests for New Package

- [ ] Create `test/` directory
- [ ] Add `vitest.config.mjs` using appropriate template
- [ ] Add test scripts to `package.json`
- [ ] Create test files following naming convention
- [ ] Implement unit tests (80% coverage minimum)
- [ ] Implement integration tests
- [ ] Add error handling tests
- [ ] Document test files with JSDoc
- [ ] Run tests locally: `pnpm test`
- [ ] Verify coverage: `pnpm test:coverage`
- [ ] Add tests to CI/CD pipeline
- [ ] Review and refactor based on coverage report

## 🎓 Examples by Package Type

### Example 1: Core Package Test

```javascript
// packages/core/test/store.test.mjs
import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, addQuad, getQuads } from '../src/index.mjs';

describe('@unrdf/core - Store', () => {
  let store;

  beforeEach(() => {
    store = createStore();
  });

  it('should create empty store', () => {
    expect(store).toBeDefined();
    expect(getQuads(store).length).toBe(0);
  });

  it('should add quad', () => {
    const quad = { subject: 's', predicate: 'p', object: 'o' };
    addQuad(store, quad);
    expect(getQuads(store).length).toBe(1);
  });
});
```

### Example 2: Browser Package Test

```javascript
/**
 * @vitest-environment jsdom
 */
// packages/browser/test/indexeddb.test.mjs
import { describe, it, expect } from 'vitest';
import { createIndexedDBStore } from '../src/index.mjs';

describe('@unrdf/browser - IndexedDB', () => {
  it('should create IndexedDB store', () => {
    const store = createIndexedDBStore('test-db');
    expect(store.dbName).toBe('test-db');
  });
});
```

### Example 3: Example Project Test

```javascript
// packages/core/examples/basic-store/test/example.test.mjs
import { describe, it, expect } from 'vitest';
import { runExample } from '../index.mjs';

describe('Basic Store Example', () => {
  it('should demonstrate store operations', async () => {
    const result = await runExample();
    expect(result).toBeDefined();
  });
});
```

## 🔄 Migration Guide

### Migrating Existing Tests

1. **Update vitest.config.mjs** to match standard template
2. **Add environment directive** if using jsdom
3. **Update test scripts** in package.json
4. **Reorganize test files** to match structure
5. **Update imports** to use Vitest methods
6. **Add coverage thresholds**
7. **Run tests** and fix any failures
8. **Verify coverage** meets standards

### Migration Checklist

- [ ] Backup existing tests
- [ ] Update vitest.config.mjs
- [ ] Update package.json scripts
- [ ] Add environment directives where needed
- [ ] Update test imports
- [ ] Run tests and verify all pass
- [ ] Check coverage meets thresholds
- [ ] Update documentation
- [ ] Commit changes

---

**Last Updated:** 2025-12-04
**Version:** 1.0.0
**Maintained By:** UNRDF Architecture Team
