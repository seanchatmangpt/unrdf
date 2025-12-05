# UNRDF Testing Strategy

Comprehensive guide to testing in the UNRDF monorepo.

## Overview

- **Test Framework:** Vitest (fast, ESM-native)
- **Coverage Target:** 80%+ across all packages
- **Test Types:** Unit, Integration, E2E
- **Fixtures:** Shared RDF test data in `test/fixtures/`
- **Organization:** Tests mirror source structure

---

## Test Organization

### Directory Structure

```
packages/core/test/
├── api/                       # Public API tests
│   ├── knowledge-substrate.test.mjs
│   ├── store.test.mjs
│   └── query.test.mjs
├── internal/                  # Internal implementation tests
│   ├── parser.test.mjs
│   ├── executor.test.mjs
│   └── validator.test.mjs
├── integration/               # Cross-module integration tests
│   ├── sparql.test.mjs
│   ├── shacl.test.mjs
│   └── transactions.test.mjs
├── fixtures/                  # Test data
│   ├── sample-graph.ttl
│   ├── shapes.ttl
│   └── queries.sparql
└── setup.mjs                  # Vitest configuration
```

### File Naming

- **Test files:** `*.test.mjs` (always suffix)
- **Fixtures:** Domain-specific names (e.g., `sample-graph.ttl`)
- **Setup:** `setup.mjs` for configuration
- **Mirrors source:** `src/api/store.mjs` → `test/api/store.test.mjs`

---

## Running Tests

### All Tests

```bash
# Run all tests across all packages
pnpm test

# Run with coverage
pnpm test -- --coverage

# Watch mode (rerun on file changes)
pnpm run test:watch
```

### Single Package

```bash
# Test one package
pnpm --filter @unrdf/core test

# Test with watch
pnpm --filter @unrdf/core run test:watch
```

### Specific Tests

```bash
# Match test name pattern
pnpm test -- --testNamePattern="SPARQL"

# Run single test file
pnpm test -- packages/core/test/api/query.test.mjs

# Run tests matching glob pattern
pnpm test -- packages/core/test/api/**
```

---

## Writing Tests

### Basic Test Structure

```javascript
// test/api/store.test.mjs
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createStore } from '../../src/api/store.mjs';

describe('Store', () => {
  let store;

  beforeEach(() => {
    store = createStore();
  });

  afterEach(() => {
    store.clear();
  });

  it('creates an empty store', () => {
    expect(store.size).toBe(0);
  });

  it('adds triples to store', () => {
    store.addQuad(subject, predicate, object);
    expect(store.size).toBe(1);
  });
});
```

### Test Patterns

#### 1. Unit Tests (API)

Test individual public functions:

```javascript
describe('query()', () => {
  it('executes simple SELECT query', async () => {
    const store = createStore();
    store.addQuad(alice, knows, bob);

    const results = await query(store, `
      SELECT ?person WHERE { ?person ex:knows ?other }
    `);

    expect(results).toHaveLength(1);
    expect(results[0].get('person')).toEqual(alice);
  });

  it('returns empty results when no matches', async () => {
    const store = createStore();
    const results = await query(store, 'SELECT * WHERE { ?s ?p ?o }');
    expect(results).toHaveLength(0);
  });

  it('throws on invalid SPARQL syntax', async () => {
    const store = createStore();
    await expect(
      query(store, 'INVALID SPARQL')
    ).rejects.toThrow();
  });
});
```

#### 2. Integration Tests

Test how modules work together:

```javascript
describe('SPARQL with SHACL Validation', () => {
  it('validates results against shape constraints', async () => {
    const store = createStore();
    const shapeStore = createStore();

    // Load data
    store.addQuad(person, name, literal('Alice'));

    // Load shape
    shapeStore.addQuad(
      personShape, targets, person,
      personShape, requires, name
    );

    // Query and validate
    const results = await query(store, 'SELECT * WHERE { ?s ?p ?o }');
    const validation = await validateShacl(store, shapeStore);

    expect(validation.conforms).toBe(true);
  });
});
```

#### 3. Error Cases

Always test error conditions:

```javascript
describe('Error Handling', () => {
  it('handles malformed RDF input', () => {
    expect(() => {
      parseRdf('invalid @prefix syntax');
    }).toThrow();
  });

  it('handles missing required parameters', async () => {
    await expect(query(null, sparql)).rejects.toThrow();
    await expect(query(store, null)).rejects.toThrow();
  });

  it('handles concurrent access correctly', async () => {
    const store = createStore();
    const promises = [];

    for (let i = 0; i < 100; i++) {
      promises.push(
        store.addQuad(
          namedNode(`ex:${i}`),
          rdfType,
          namedNode('ex:Item')
        )
      );
    }

    await Promise.all(promises);
    expect(store.size).toBe(100);
  });
});
```

---

## Test Fixtures

### Using Fixtures

Store reusable test data in `test/fixtures/`:

```bash
# test/fixtures/sample-graph.ttl
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:Alice foaf:name "Alice" ;
         foaf:knows ex:Bob .

ex:Bob foaf:name "Bob" .
```

```bash
# test/fixtures/shapes.ttl
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path foaf:name ;
    sh:minCount 1
  ] .
```

### Loading Fixtures in Tests

```javascript
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

describe('With Fixtures', () => {
  let store;
  let sampleData;

  beforeEach(() => {
    store = createStore();
    sampleData = readFileSync(
      join(__dirname, 'fixtures/sample-graph.ttl'),
      'utf-8'
    );
    store.load(sampleData, 'text/turtle');
  });

  it('queries fixture data', async () => {
    const results = await query(store, `
      SELECT ?name WHERE {
        ?person foaf:name ?name .
      }
    `);

    expect(results).toContainEqual(
      expect.objectContaining({ name: 'Alice' })
    );
  });
});
```

---

## Cross-Package Testing

### Testing Dependencies

If package A depends on package B:

```bash
# 1. Ensure package B builds
pnpm --filter @unrdf/core run build

# 2. Test package A
pnpm --filter @unrdf/hooks test

# 3. Test integration
pnpm test -- packages/hooks/test/integration/
```

### Testing Monorepo Integration

Verify packages work together:

```javascript
// packages/hooks/test/integration/with-core.test.mjs
import { describe, it, expect } from 'vitest';
import { createKnowledgeSubstrateCore } from '@unrdf/core';
import { registerHook, defineHook } from '@unrdf/hooks';

describe('Hooks + Core Integration', () => {
  it('fires hooks when data changes in core', async () => {
    const core = await createKnowledgeSubstrateCore();
    const store = core.parseRdf(`
      @prefix ex: <http://example.org/> .
      ex:Alice ex:status "online" .
    `);

    let hookFired = false;

    defineHook({
      trigger: 'UPDATE',
      pattern: '?s ex:status ?o',
      run: () => { hookFired = true; }
    });

    registerHook(hook);

    // Modify data
    store.removeQuad(...);
    store.addQuad(...);

    expect(hookFired).toBe(true);
  });
});
```

---

## Coverage Requirements

### Minimum Coverage

- **80%** overall (all packages combined)
- **70%** per package (minimum)
- **100%** for public APIs (via integration tests)

### Measuring Coverage

```bash
# Generate coverage report
pnpm test -- --coverage

# View coverage report (in coverage/index.html)
open coverage/index.html
```

### Coverage Goals

| Category | Target | How |
|----------|--------|-----|
| **Statements** | 80% | Write tests for all code paths |
| **Branches** | 75% | Test if/else, try/catch paths |
| **Functions** | 80% | Test all exported functions |
| **Lines** | 80% | Cover all source lines |

---

## Mocking and Spies

### Mocking Modules

```javascript
import { vi } from 'vitest';

vi.mock('@unrdf/oxigraph', () => ({
  createStore: vi.fn(() => ({ size: 0 }))
}));
```

### Spying on Functions

```javascript
import { vi } from 'vitest';

const logSpy = vi.spyOn(console, 'log');

logSpy.mockImplementation(() => {});
// Test code...
expect(logSpy).toHaveBeenCalledWith('message');

logSpy.mockRestore();
```

---

## Async Testing

### Testing Promises

```javascript
describe('Async Operations', () => {
  it('handles async queries', async () => {
    const results = await query(store, sparql);
    expect(results).toHaveLength(2);
  });

  it('handles errors in async operations', async () => {
    await expect(
      query(invalidStore, sparql)
    ).rejects.toThrow('Invalid store');
  });
});
```

### Testing Streams

```javascript
import { Readable } from 'stream';

describe('Streaming', () => {
  it('processes stream data', async () => {
    const stream = createReadableStream(data);
    const results = [];

    for await (const chunk of stream) {
      results.push(chunk);
    }

    expect(results).toHaveLength(100);
  });
});
```

---

## Performance Testing

### Benchmarking

```javascript
import { performance } from 'perf_hooks';

describe('Performance', () => {
  it('queries 1M triples in < 100ms', async () => {
    const store = createLargeStore(1_000_000);

    const start = performance.now();
    const results = await query(store, complexQuery);
    const elapsed = performance.now() - start;

    expect(elapsed).toBeLessThan(100);
    expect(results).toHaveLength(expected);
  });
});
```

---

## Testing Strategy by Package Type

### Core Package (@unrdf/core)

1. **Unit tests:** Each public API function
2. **Integration tests:** SPARQL + SHACL together
3. **Regression tests:** Known bug cases
4. **Fixtures:** Various RDF formats, complex queries

### Feature Package (@unrdf/hooks, @unrdf/streaming)

1. **Unit tests:** Hook lifecycle, stream composition
2. **Integration tests:** With core package
3. **Error scenarios:** Invalid input, edge cases
4. **Performance:** Overhead measurements

### Integration Package (@unrdf/react, @unrdf/composables)

1. **Component tests:** React/Vue component rendering
2. **Hook tests:** React hook behavior
3. **State management:** Data binding, updates
4. **Integration:** With core package

### CLI Package (@unrdf/cli)

1. **Command tests:** Each CLI command
2. **Argument parsing:** Valid/invalid args
3. **File I/O:** Read/write operations
4. **Exit codes:** Correct error handling

---

## CI/CD Testing

### GitHub Actions Workflow

Tests run automatically on:
- Push to any branch
- Pull requests
- Before release

```bash
# Typical CI steps
pnpm install
pnpm run lint
pnpm run build
pnpm test -- --coverage
```

### Local Pre-commit Testing

```bash
# Before committing
pnpm test              # Run all tests
pnpm run lint          # Check linting
pnpm run build         # Build succeeds
```

---

## Troubleshooting Tests

### Issue: "Cannot find module"

**Solution:** Ensure file paths are correct:

```javascript
// ❌ Wrong
import { query } from '@unrdf/core/src/api/query.mjs';

// ✅ Correct (relative)
import { query } from '../../src/api/query.mjs';

// ✅ Also correct (published API)
import { query } from '@unrdf/core';
```

### Issue: Tests are hanging

**Solution:** Check for unresolved promises:

```javascript
it('completes async operation', async () => {
  const result = await asyncFunction();
  expect(result).toBeDefined();
  // Missing return - test might hang!
});

// Fix: ensure all async ops complete
it('completes async operation', async () => {
  const result = await asyncFunction();
  expect(result).toBeDefined();
});
```

### Issue: Fixtures not found

**Solution:** Use proper path resolution:

```javascript
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const fixturePath = join(__dirname, 'fixtures/sample.ttl');
```

### Issue: Tests pass locally but fail in CI

**Solution:** Check for:
- Environment variables
- File paths (Windows vs Linux)
- Timing issues (add delays if needed)
- Node version differences

---

## Test Checklist

Before committing code:

- [ ] All new functions have test coverage
- [ ] Error cases are tested
- [ ] Integration with dependencies tested
- [ ] No console.log statements left
- [ ] Tests pass: `pnpm test`
- [ ] Linting passes: `pnpm run lint`
- [ ] Build succeeds: `pnpm run build`
- [ ] Coverage is 80%+

---

## Next Steps

- **Learn Vitest:** https://vitest.dev/
- **Fixtures Guide:** [docs/test-fixtures](fixtures/)
- **Contributing:** [CONTRIBUTING.md](../CONTRIBUTING.md)

---

**Ready to write tests?** Start with a simple unit test, then expand to integration tests!
