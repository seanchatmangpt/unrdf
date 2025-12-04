# Vitest Quick Reference for UNRDF

**For Full Documentation:** See [VITEST_STANDARD.md](./VITEST_STANDARD.md)

## 🚀 Quick Start

### 1. Choose Your Template

| Package Type | Environment | Coverage Threshold |
|-------------|-------------|-------------------|
| Core, Hooks, Federation, Streaming, CLI, Knowledge-Engine, Dark-Matter, Project-Engine | `node` | 80/80/75/80 |
| Browser, Composables | `jsdom` | 80/80/75/80 |
| Examples | `node` or `jsdom` | 70/70/60/70 |

### 2. Create vitest.config.mjs

**Node.js packages:**
```javascript
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'node',
    globals: true,
    pool: 'forks',
    poolOptions: { forks: { singleFork: true } },
    concurrent: false,
    maxConcurrency: 1,
    include: ['test/**/*.test.mjs'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      thresholds: { lines: 80, functions: 80, branches: 75, statements: 80 },
    },
  },
});
```

**Browser packages:**
```javascript
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'jsdom', // ← Only difference
    globals: true,
    pool: 'forks',
    poolOptions: { forks: { singleFork: true } },
    concurrent: false,
    maxConcurrency: 1,
    include: ['test/**/*.test.mjs'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      thresholds: { lines: 80, functions: 80, branches: 75, statements: 80 },
    },
  },
});
```

### 3. Add Test Scripts to package.json

```json
{
  "scripts": {
    "test": "vitest run --coverage",
    "test:fast": "vitest run",
    "test:watch": "vitest --coverage",
    "test:ui": "vitest --ui"
  }
}
```

### 4. Create Test File

```javascript
/**
 * @fileoverview <Package> - <Feature> Test Suite
 * @vitest-environment node (or jsdom)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { functionUnderTest } from '../src/index.mjs';

describe('<Package> - <Feature>', () => {
  beforeEach(() => {
    // Setup
  });

  it('should <behavior>', () => {
    // Arrange
    const input = 'test';

    // Act
    const result = functionUnderTest(input);

    // Assert
    expect(result).toBe('expected');
  });

  it('should handle errors', () => {
    expect(() => functionUnderTest(null)).toThrow(TypeError);
  });
});
```

## 📁 Directory Structure

```
<package>/
├── src/
│   └── *.mjs
├── test/
│   ├── <feature>.test.mjs
│   ├── setup.mjs (optional)
│   └── fixtures/ (optional)
├── vitest.config.mjs
└── package.json
```

## 🎯 Common Assertions

```javascript
// Equality
expect(value).toBe(expected);
expect(value).toEqual(expected);

// Truthiness
expect(value).toBeTruthy();
expect(value).toBeDefined();
expect(value).toBeNull();

// Numbers
expect(number).toBeGreaterThan(0);
expect(number).toBeLessThan(100);

// Strings
expect(string).toContain('substring');
expect(string).toMatch(/regex/);

// Arrays
expect(array).toContain(item);
expect(array).toHaveLength(3);

// Objects
expect(object).toHaveProperty('key');
expect(object).toMatchObject({ key: value });

// Errors
expect(() => fn()).toThrow();
expect(() => fn()).toThrow(TypeError);
await expect(asyncFn()).rejects.toThrow();

// Async
await expect(promise).resolves.toBe(value);
```

## 🏃 Running Tests

```bash
# Run all tests
pnpm test

# Run specific package
pnpm --filter @unrdf/core test

# Watch mode
pnpm test:watch

# With UI
pnpm test:ui

# Coverage only
pnpm test:coverage
```

## ✅ Coverage Thresholds

| Metric | Core/Browser Packages | Examples |
|--------|----------------------|----------|
| Lines | 80% | 70% |
| Functions | 80% | 70% |
| Branches | 75% | 60% |
| Statements | 80% | 70% |

## 🔍 Test Categories

1. **Unit Tests**: Test individual functions
2. **Integration Tests**: Test component interactions
3. **Example Tests**: Demonstrate real-world usage
4. **Error Handling**: Verify error scenarios

## 🚫 Common Mistakes to Avoid

- ❌ Don't use `test()` → Use `it()`
- ❌ Don't mix environments in same file
- ❌ Don't skip tests without comments
- ❌ Don't use magic numbers
- ❌ Don't add OTEL to test files

## 📊 Package-Specific Notes

### @unrdf/core
- Environment: `node`
- Focus: RDF operations, SPARQL queries
- Mocks: External dependencies

### @unrdf/browser
- Environment: `jsdom`
- Focus: IndexedDB, Service Workers
- Mocks: Browser APIs

### @unrdf/knowledge-engine
- Environment: `node`
- Timeout: 60s (for inference)
- Focus: Inference, reasoning

### Examples
- Lower coverage thresholds (70/70/60/70)
- Demonstrate use cases
- Simple assertions

## 🎓 AAA Pattern (Arrange-Act-Assert)

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

## 🔧 Lifecycle Hooks

```javascript
describe('Feature', () => {
  beforeAll(() => {
    // Setup once before all tests
  });

  afterAll(() => {
    // Cleanup once after all tests
  });

  beforeEach(() => {
    // Setup before each test
  });

  afterEach(() => {
    // Cleanup after each test
  });
});
```

## 📝 Test Documentation

```javascript
/**
 * @fileoverview @unrdf/core - Store Operations Test Suite
 *
 * Tests for RDF store CRUD operations including:
 * - Store creation and initialization
 * - Quad addition and removal
 * - Query operations
 *
 * @vitest-environment node
 */
```

## 🐛 Debugging

```bash
# Use Vitest UI
pnpm test:ui

# Use Node.js debugger
node --inspect-brk ./node_modules/vitest/vitest.mjs run

# Console logging
it('should debug', () => {
  console.log('Debug:', value);
  expect(value).toBe(expected);
});
```

## 📋 New Package Checklist

- [ ] Create `test/` directory
- [ ] Add `vitest.config.mjs`
- [ ] Add test scripts to `package.json`
- [ ] Create test files
- [ ] Implement unit tests (80%+ coverage)
- [ ] Add integration tests
- [ ] Add error handling tests
- [ ] Run `pnpm test`
- [ ] Verify coverage: `pnpm test:coverage`

## 📚 Resources

- [Full Standard](./VITEST_STANDARD.md)
- [Vitest Docs](https://vitest.dev)
- [UNRDF Testing Guide](./TESTING.md)

---

**Quick Reference Version 1.0.0**
