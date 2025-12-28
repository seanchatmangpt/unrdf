# Testing Patterns Skill

Expert knowledge for writing high-quality tests in UNRDF.

## Activation

Use this skill when:
- Writing unit tests
- Writing integration tests
- Improving test coverage
- Debugging test failures

## Core Pattern: AAA

```javascript
import { describe, it, expect } from 'vitest';

describe('Feature', () => {
  it('should [expected behavior] when [condition]', () => {
    // Arrange - set up test data and dependencies
    const input = { value: 42 };
    const processor = new Processor();

    // Act - execute the code under test
    const result = processor.process(input);

    // Assert - verify the expected outcome
    expect(result).toBe(84);
  });
});
```

## Test Organization

### File Structure
```
packages/package-name/
├── src/
│   └── feature.mjs
└── test/
    ├── feature.test.mjs      # Unit tests
    ├── feature.int.test.mjs  # Integration tests
    └── fixtures/             # Test data
        └── sample-data.json
```

### Describe Blocks
```javascript
describe('ClassName', () => {
  describe('methodName()', () => {
    describe('when condition A', () => {
      it('should do X', () => {});
    });

    describe('when condition B', () => {
      it('should do Y', () => {});
    });
  });
});
```

## Assertion Patterns

### Basic Assertions
```javascript
expect(value).toBe(expected);           // Strict equality
expect(value).toEqual(expected);        // Deep equality
expect(value).toBeDefined();            // Not undefined
expect(value).toBeNull();               // Is null
expect(value).toBeTruthy();             // Truthy value
expect(value).toBeFalsy();              // Falsy value
```

### Object Assertions
```javascript
expect(obj).toEqual({ a: 1, b: 2 });           // Exact match
expect(obj).toMatchObject({ a: 1 });           // Partial match
expect(obj).toHaveProperty('key');             // Has property
expect(obj).toHaveProperty('key', 'value');    // Property with value
```

### Array Assertions
```javascript
expect(arr).toHaveLength(3);                   // Length check
expect(arr).toContain(item);                   // Contains item
expect(arr).toContainEqual({ id: 1 });         // Contains object
expect(arr).toEqual(expect.arrayContaining([1, 2]));  // Partial array
```

### Error Assertions
```javascript
expect(() => fn()).toThrow();                  // Throws any error
expect(() => fn()).toThrow(ErrorClass);        // Throws specific class
expect(() => fn()).toThrow('message');         // Throws with message
expect(() => fn()).toThrow(/pattern/);         // Throws matching pattern
```

### Async Assertions
```javascript
await expect(asyncFn()).resolves.toBe(value);  // Resolves to value
await expect(asyncFn()).rejects.toThrow();     // Rejects with error
```

## Mocking

### Function Mocks
```javascript
import { vi } from 'vitest';

const mockFn = vi.fn();
mockFn.mockReturnValue(42);
mockFn.mockReturnValueOnce(100);
mockFn.mockImplementation((x) => x * 2);

// Assertions
expect(mockFn).toHaveBeenCalled();
expect(mockFn).toHaveBeenCalledWith('arg');
expect(mockFn).toHaveBeenCalledTimes(3);
```

### Module Mocks
```javascript
import { vi } from 'vitest';

vi.mock('./dependency.mjs', () => ({
  someFunction: vi.fn().mockReturnValue('mocked')
}));
```

### Spy on Object Methods
```javascript
const spy = vi.spyOn(object, 'method');
spy.mockReturnValue('mocked');

// After test
spy.mockRestore();
```

## Test Fixtures

### Setup and Teardown
```javascript
describe('Feature', () => {
  let instance;

  beforeEach(() => {
    instance = new Feature();
  });

  afterEach(() => {
    instance.cleanup();
  });

  beforeAll(() => {
    // Run once before all tests
  });

  afterAll(() => {
    // Run once after all tests
  });
});
```

### Test Data Factories
```javascript
// test/fixtures/factories.mjs
export function createTriple(overrides = {}) {
  return {
    subject: 'http://example.org/subject',
    predicate: 'http://example.org/predicate',
    object: 'http://example.org/object',
    ...overrides
  };
}

// In test
const triple = createTriple({ subject: 'http://custom.org/s' });
```

## UNRDF-Specific Patterns

### Testing Receipts
```javascript
describe('Receipt Creation', () => {
  it('should generate unique IDs', () => {
    const r1 = createReceipt({ operation: 'create' });
    const r2 = createReceipt({ operation: 'create' });

    expect(r1.id).not.toBe(r2.id);
    expect(r1.id).toMatch(/^[a-f0-9-]{36}$/);
  });

  it('should include timestamp', () => {
    const before = Date.now();
    const receipt = createReceipt({ operation: 'create' });
    const after = Date.now();

    expect(receipt.timestamp).toBeGreaterThanOrEqual(before);
    expect(receipt.timestamp).toBeLessThanOrEqual(after);
  });
});
```

### Testing SPARQL
```javascript
describe('SPARQL Query', () => {
  let store;

  beforeEach(() => {
    store = createStore();
    store.add(testTriple);
  });

  it('should find matching triples', () => {
    const results = store.query(`
      SELECT ?s WHERE { ?s ?p ?o }
    `);

    expect(results).toHaveLength(1);
    expect(results[0].s.value).toBe(testTriple.subject);
  });
});
```

## Forbidden Patterns

```javascript
// FORBIDDEN - No skipped tests
it.skip('should work', () => {});

// FORBIDDEN - No empty tests
it('should work', () => {});

// FORBIDDEN - No console.log in tests
it('should work', () => {
  console.log(result);  // Remove before commit
});

// FORBIDDEN - No hardcoded timeouts
await new Promise(r => setTimeout(r, 1000));  // Use vi.useFakeTimers()
```

## Coverage Requirements

- Lines: 80% minimum
- Functions: 80% minimum
- Branches: 80% minimum
- Statements: 80% minimum
