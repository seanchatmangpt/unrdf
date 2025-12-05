# Vitest Doctests - User Guide

## Quick Start

Doctests are executable examples embedded in JSDoc comments (`@example` tags) that are automatically extracted and run as Vitest tests. This keeps your documentation and tests in sync while increasing coverage.

### Running Doctests

```bash
# Generate and run doctests
npm run test:doctest

# Watch mode for doctests
npm run test:doctest:watch

# Run everything (existing tests + doctests)
npm run test:all

# Run tests (pretest hook auto-generates doctests)
npm test
```

### Writing Doctests

Add `@example` blocks to any JSDoc comment:

```javascript
/**
 * Parse ISO 8601 timestamp string into nanosecond BigInt
 *
 * @param {string} iso - ISO 8601 timestamp
 * @returns {bigint} Nanosecond-precision timestamp
 *
 * @example
 * import { fromISO } from './time.mjs';
 *
 * const time = fromISO('2025-01-15T10:30:00.123456789Z');
 * console.assert(typeof time === 'bigint', 'Returns BigInt');
 * console.assert(time > 0n, 'Positive timestamp');
 *
 * @example
 * import { fromISO } from './time.mjs';
 *
 * // Error handling example
 * try {
 *   fromISO('not-a-date');
 *   throw new Error('Should have thrown');
 * } catch (err) {
 *   console.assert(err.message.includes('Invalid'), 'Clear error message');
 * }
 */
export function fromISO(iso) {
  // ... implementation
}
```

## Best Practices

### ✅ DO

- **Include full imports**: Each example is isolated, so include all necessary imports
- **Use `console.assert()`**: Vitest runs them as assertions
- **One concept per example**: Keep examples <15 lines each
- **Handle both success and error cases**: Show what works AND what breaks
- **Use realistic data**: No `foo`, `bar`, or placeholder values
- **Test at API boundaries**: Focus on public API, not implementation details

### ❌ DON'T

- **Don't skip imports**: Examples run in isolation
- **Don't test implementation details**: Test behavior, not internals
- **Don't create huge examples**: Break into multiple smaller examples
- **Don't hardcode paths or secrets**: Use test fixtures or generated data
- **Don't assume globals**: Each example is independent

## Example Patterns

### Success Case

```javascript
/**
 * @example
 * import { multiply } from './math.mjs';
 *
 * const result = multiply(3, 4);
 * console.assert(result === 12, 'Returns correct product');
 */
```

### Error Handling

```javascript
/**
 * @example
 * import { parseJSON } from './parser.mjs';
 *
 * try {
 *   parseJSON('not valid json');
 *   throw new Error('Should have thrown SyntaxError');
 * } catch (err) {
 *   console.assert(err instanceof SyntaxError, 'Throws SyntaxError');
 * }
 */
```

### Edge Cases

```javascript
/**
 * @example
 * import { clamp } from './utils.mjs';
 *
 * // Normal value passes through
 * console.assert(clamp(5, 0, 10) === 5, 'Within range');
 *
 * // Boundary values clamped
 * console.assert(clamp(-5, 0, 10) === 0, 'Below min clamped to min');
 * console.assert(clamp(15, 0, 10) === 10, 'Above max clamped to max');
 */
```

### Async Functions

```javascript
/**
 * @example
 * import { fetchUser } from './api.mjs';
 *
 * const user = await fetchUser(123);
 * console.assert(user.id === 123, 'Returns correct user');
 * console.assert(typeof user.name === 'string', 'Has name field');
 */
```

## Generated Test Files

Generated doctests are placed in `test/doctest/` with names like:
- `time.doctest.test.mjs` - from `src/time.mjs`
- `store.doctest.test.mjs` - from `src/store.mjs`

These are auto-generated before each test run via the pretest hook. Don't edit them directly - modify the `@example` blocks in source files instead.

## How It Works

1. **Extraction**: `src/doctest/extractor.mjs` finds all `@example` blocks in source files
2. **Transformation**: `src/doctest/transformer.mjs` converts them to Vitest test cases
3. **Generation**: `scripts/generate-doctests.mjs` orchestrates the pipeline
4. **Integration**: `package.json` pretest hook ensures they're always fresh

The full pipeline runs in <5 seconds, ensuring fast feedback.

## Troubleshooting

### "No examples found"

Check that your `@example` blocks are inside a JSDoc comment:

```javascript
// ✅ CORRECT
/**
 * Function description
 * @example
 * code here
 */

// ❌ WRONG - Comment not JSDoc
// Example:
// code here
```

### "Cannot find module"

Imports in generated tests are rewritten to use `../../src/` paths. Make sure your source files are in `src/`:

```javascript
// ✅ CORRECT
import { funcName } from '../../src/module.mjs';

// ❌ WRONG - relative path not adjusted
import { funcName } from './module.mjs';
```

### Test timeout

Doctests have a 5-second timeout per test. If your example is slow:
- Break it into smaller examples
- Use lighter assertions
- Check for inefficient loops

### Assertion failures

Use `console.assert()` with a descriptive message:

```javascript
// ✅ GOOD
console.assert(value > 0, 'Value must be positive');

// ❌ VAGUE
console.assert(value);
```

## Coverage

Doctests contribute to overall code coverage. The `src/doctest/` directory is excluded from coverage calculations, but your source code exercised by doctests counts toward coverage metrics.

Current doctests: **11 examples** across 5 source files
