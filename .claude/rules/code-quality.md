# Code Quality Rules

Enforced code quality standards for UNRDF development.

## File Rules

### ESM Only
- All files MUST use `.mjs` extension
- No CommonJS (`require`, `module.exports`)
- Use ES6 imports/exports exclusively

### File Size Limit
- Maximum **500 lines** per file
- If exceeding, split into modules
- Exception: Generated files (with justification)

### Naming Conventions
- Files: `kebab-case.mjs`
- Schemas: `*.schema.mjs`
- Tests: `*.test.mjs`
- Types: `*.types.mjs` (JSDoc only)

## Code Rules

### No Direct N3 Imports
```javascript
// FORBIDDEN
import { Store } from 'n3';

// REQUIRED
import { createStore } from '@unrdf/oxigraph';
```

### Zod Validation Required
- All public APIs MUST validate input with Zod
- Use `safeParse` for user input
- Use `parse` for internal/trusted data

```javascript
import { z } from 'zod';

const InputSchema = z.object({
  name: z.string().min(1),
  count: z.number().int().positive()
});

export function processInput(input) {
  const validated = InputSchema.parse(input);
  // ...
}
```

### JSDoc Required
- All exported functions MUST have JSDoc
- Include `@param`, `@returns`, `@throws`
- Example usage in `@example`

```javascript
/**
 * Creates a new receipt for the operation
 * @param {Object} options - Receipt options
 * @param {string} options.operation - Operation type
 * @param {string} options.entityType - Entity type
 * @returns {Receipt} The created receipt
 * @throws {ValidationError} If options are invalid
 * @example
 * const receipt = createReceipt({ operation: 'create', entityType: 'Triple' });
 */
export function createReceipt(options) { }
```

## Quality Metrics

### Lint Rules
- ZERO errors allowed
- ZERO warnings allowed (warnings = errors)
- ESLint config: `eslint.config.mjs`

### Test Requirements
- Minimum **80% coverage**
- All tests MUST pass (no `it.skip`)
- Test timeout: **5 seconds** default

### Performance
- Default timeout: **5 seconds**
- Justify any timeout >5s
- Profile slow operations

## Forbidden Patterns

### No TODOs in Production Code
```javascript
// FORBIDDEN
// TODO: implement later
function placeholder() {
  throw new Error('Not implemented');
}

// REQUIRED
// Either implement fully or don't commit
```

### No Disabled Tests
```javascript
// FORBIDDEN
it.skip('should work', () => {});
describe.skip('Feature', () => {});

// FORBIDDEN
// eslint-disable-next-line
```

### No Magic Numbers
```javascript
// FORBIDDEN
if (count > 100) { }

// REQUIRED
const MAX_ITEMS = 100;
if (count > MAX_ITEMS) { }
```

## Enforcement

These rules are enforced via:
1. ESLint (automated)
2. Pre-commit hooks
3. CI/CD pipeline
4. Code review

Violations block merge.
