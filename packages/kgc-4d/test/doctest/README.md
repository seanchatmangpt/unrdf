# Generated Doctest Files

## What Are These Files?

These `.doctest.test.mjs` files are **automatically generated** from `@example` blocks in JSDoc comments. They should not be manually edited.

## How They're Generated

```
src/time.mjs (@example blocks)
    ↓
src/doctest/extractor.mjs (extracts code)
    ↓
src/doctest/transformer.mjs (converts to Vitest tests)
    ↓
test/doctest/time.doctest.test.mjs (generated test file)
```

## Example Generation

**Source File** (`src/time.mjs`):
```javascript
/**
 * Parse ISO 8601 timestamp string into nanosecond BigInt
 *
 * @example
 * import { fromISO } from './time.mjs';
 *
 * const time = fromISO('2025-01-15T10:30:00.123456789Z');
 * console.assert(typeof time === 'bigint', 'Returns BigInt');
 */
export function fromISO(iso) { ... }
```

**Generated Test** (`test/doctest/time.doctest.test.mjs`):
```javascript
import { describe, test, expect } from 'vitest';

import { fromISO } from '../../src/time.mjs'

describe('Doctests: time.mjs', () => {
  test('fromISO example 1 (line 45)', async () => {
    import { fromISO } from './time.mjs';

    const time = fromISO('2025-01-15T10:30:00.123456789Z');
    console.assert(typeof time === 'bigint', 'Returns BigInt');
  });
});
```

## Regenerating Files

Files are automatically regenerated before every test run via the pretest hook:

```bash
npm test  # Runs pretest hook automatically
```

Or manually:

```bash
node scripts/generate-doctests.mjs
```

## File List

- `time.doctest.test.mjs` - 3 examples from `src/time.mjs`
- `store.doctest.test.mjs` - 2 examples from `src/store.mjs`
- `git.doctest.test.mjs` - 1 example from `src/git.mjs`
- `freeze.doctest.test.mjs` - 1 example from `src/freeze.mjs`
- `guards.doctest.test.mjs` - 4 examples from `src/guards.mjs`

## Editing Examples

**DO NOT** edit these generated files. Instead:

1. Edit the `@example` block in the source file
2. Run `npm test` (or `node scripts/generate-doctests.mjs` manually)
3. Generated files will be updated automatically

Example workflow:
```
src/time.mjs:       Edit @example block
    ↓ (save)
npm test            Runs pretest hook
    ↓ (generates)
test/doctest/time.doctest.test.mjs  Updated with new example
```

## Running Doctests

```bash
# Run only doctests
npm run test:doctest

# Watch mode
npm run test:doctest:watch

# Run all tests (existing + doctests)
npm run test:all

# Or part of normal test run
npm test
```

## Why Auto-Generated?

1. **Keep docs in sync**: Examples always match source code
2. **No duplicates**: Single source of truth (the `@example` block)
3. **Consistency**: All examples formatted the same way
4. **CI integration**: Regenerate on every PR to catch stale examples
5. **No merge conflicts**: Generated files never conflict with hand-edits

## Technical Details

### Import Path Rewriting

Examples in source files use relative imports:
```javascript
import { now } from './time.mjs';
```

Generated tests adjust paths for their location in `test/doctest/`:
```javascript
import { now } from '../../src/time.mjs';
```

### Metadata in Test Names

Each generated test includes:
- **Function name**: The function being documented
- **Example number**: Nth example for that function
- **Line number**: Where in source file it appears

Example: `fromISO example 2 (line 64)`

This makes it easy to find the original `@example` in source when investigating failures.

## Troubleshooting

### Test failures after editing source

Some common mistakes:

1. **Import path wrong**
   ```javascript
   // ❌ WRONG
   import { func } from './module.mjs';

   // ✅ CORRECT - must include full path to source
   import { func } from './module.mjs';
   ```

2. **Missing imports in example**
   ```javascript
   // ❌ WRONG - assumes func is available
   const x = func(5);

   // ✅ CORRECT - include the import
   import { func } from './module.mjs';
   const x = func(5);
   ```

3. **Async function not awaited**
   ```javascript
   // ❌ WRONG
   const result = fetchData();

   // ✅ CORRECT
   const result = await fetchData();
   ```

### Files not updating

If edited source file but test file didn't update:

```bash
# Manually regenerate
node scripts/generate-doctests.mjs
```

Check that:
- `@example` block is inside a JSDoc comment (`/** ... */`)
- Source file is in `src/` directory
- File ends with `.mjs` extension
