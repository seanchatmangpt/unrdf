# Testing Standards

Mandatory testing requirements for UNRDF development.

## Test Framework

- **Framework**: Vitest 4.0.16
- **Assertion**: Vitest built-in (`expect`)
- **Mocking**: Vitest built-in (`vi.fn()`, `vi.mock()`)

## Test Organization

### File Structure
```
packages/
└── package-name/
    ├── src/
    │   └── feature.mjs
    └── test/
        └── feature.test.mjs
```

### Naming Convention
```javascript
// File: feature.test.mjs
describe('Feature', () => {
  describe('method()', () => {
    it('should [expected behavior] when [condition]', () => {});
  });
});
```

## AAA Pattern (Mandatory)

```javascript
it('should calculate total correctly', () => {
  // Arrange
  const items = [{ price: 10 }, { price: 20 }];
  const calculator = new PriceCalculator();

  // Act
  const total = calculator.calculate(items);

  // Assert
  expect(total).toBe(30);
});
```

## Coverage Requirements

| Metric | Minimum | Target |
|--------|---------|--------|
| Lines | 80% | 90% |
| Functions | 80% | 90% |
| Branches | 80% | 90% |
| Statements | 80% | 90% |

## Test Categories

### Unit Tests
- Isolate single units
- Fast execution (<100ms each)
- No external dependencies

```javascript
describe('Receipt', () => {
  it('should generate unique ID', () => {
    const r1 = createReceipt({ operation: 'create' });
    const r2 = createReceipt({ operation: 'create' });
    expect(r1.id).not.toBe(r2.id);
  });
});
```

### Integration Tests
- Test module interactions
- Medium execution (<1s each)
- May use test databases

```javascript
describe('DeltaGate Integration', () => {
  it('should process delta and create receipt', async () => {
    const gate = new DeltaGate({ receipts: true });
    const result = await gate.process(delta);

    expect(result.receipt).toBeDefined();
    expect(result.receipt.operation).toBe('insert');
  });
});
```

### E2E Tests
- Full workflow tests
- Longer execution (up to 30s)
- Test real scenarios

## Forbidden Patterns

### No Skipped Tests
```javascript
// FORBIDDEN - Will fail CI
it.skip('feature test', () => {});
describe.skip('Feature', () => {});
```

### No Incomplete Tests
```javascript
// FORBIDDEN
it('should work', () => {
  // TODO: implement
});

// FORBIDDEN
it('should work', () => {});  // Empty test body
```

### No Test-Only Exports
```javascript
// FORBIDDEN in src/
export const __TEST_ONLY__ = { internalFn };

// Use dependency injection instead
```

## Timeout Standards

```javascript
// Default: 5 seconds (global)
// Override only with justification
it('slow integration test', { timeout: 10000 }, async () => {
  // Must document why >5s needed
});
```

## Running Tests

```bash
# All tests
pnpm test

# Fast suite only (<30s)
pnpm test:fast

# With coverage
pnpm test:coverage

# Specific package
pnpm -C packages/v6-core test

# Watch mode
pnpm test:watch
```

## Before Committing

```bash
# Verify all pass
timeout 30s pnpm test:fast

# Check coverage
pnpm test:coverage

# Verify no skipped tests
grep -r "it.skip\|describe.skip" packages/*/test --include="*.test.mjs"
# Should return nothing
```

## CI/CD Integration

Tests run on:
- Every push
- Every PR
- Nightly (full suite)

Failures block merge.
