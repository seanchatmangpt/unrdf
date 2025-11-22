# Coding Standards

## Purpose

This document defines coding standards to eliminate Mura (unevenness) and ensure consistency across the codebase.

## Style Standards

### Formatting
- **Tool**: Prettier (via `pnpm format`)
- **Requirement**: All files must pass `pnpm format --check`
- **CI**: Formatting check runs in CI pipeline

### Naming Conventions
- **Functions**: `camelCase`
- **Classes**: `PascalCase`
- **Constants**: `UPPER_SNAKE_CASE`
- **Unused variables**: Must use `_` prefix (e.g., `_context`, `_hook`)
- **Private methods**: Use `_` prefix (e.g., `_internalMethod`)

### Import Organization
- **Order**: 
  1. Standard library imports
  2. External dependencies
  3. Internal modules
- **Grouping**: Group by category with blank lines
- **Example**:
  ```javascript
  import { readFile } from 'fs/promises';
  import { z } from 'zod';
  
  import { Store } from 'n3';
  
  import { createHookExecutor } from './hook-executor.mjs';
  ```

## Pattern Standards

### Error Handling

#### Standard Pattern
Use `throw new Error()` for synchronous errors and try/catch for async errors:

```javascript
// ✅ Good: Standard error handling
async function processData(data) {
  if (!data) {
    throw new TypeError('processData: data is required');
  }
  
  try {
    const result = await doWork(data);
    return result;
  } catch (error) {
    throw new Error(`Failed to process data: ${error.message}`);
  }
}
```

#### Error Object Returns (Special Cases)
Only return error objects when necessary (e.g., hook results):

```javascript
// ✅ Good: Hook execution results
async function executeHook(hook, event) {
  try {
    const result = await _executeHookLifecycle(hook, event);
    return { success: true, result };
  } catch (error) {
    return { success: false, error: sanitizeError(error) };
  }
}
```

#### Reference Implementation
- **File**: `src/knowledge-engine/condition-evaluator.mjs`
- **Pattern**: Consistent `throw new Error()` with context

### Async/Await Patterns

#### Standard Pattern
Prefer `async/await` over Promise chains:

```javascript
// ✅ Good: async/await
async function fetchData(url) {
  try {
    const response = await fetch(url);
    const data = await response.json();
    return data;
  } catch (error) {
    throw new Error(`Failed to fetch: ${error.message}`);
  }
}

// ❌ Avoid: Promise chains
function fetchData(url) {
  return fetch(url)
    .then(response => response.json())
    .catch(error => {
      throw new Error(`Failed to fetch: ${error.message}`);
    });
}
```

#### Reference Implementation
- **File**: `src/knowledge-engine/hook-executor.mjs`
- **Pattern**: Consistent `async/await` with try/catch

### Timeout Patterns

#### Standard Pattern
Use consistent timeout pattern:

```javascript
// ✅ Good: Consistent timeout pattern
async function executeWithTimeout(fn, timeoutMs) {
  const timeoutPromise = new Promise((_, reject) => {
    setTimeout(() => reject(new Error(`Timeout after ${timeoutMs}ms`)), timeoutMs);
  });
  
  return Promise.race([fn(), timeoutPromise]);
}
```

## Quality Standards

### Test Coverage
- **Minimum**: 80% coverage for all modules
- **Measurement**: `pnpm test --coverage`
- **CI**: Coverage check runs in CI pipeline

### Error Handling
- **Requirement**: All fallible operations must handle errors
- **Pattern**: Use try/catch or return error objects as appropriate

### Documentation
- **Requirement**: All public APIs must have JSDoc
- **Format**:
  ```javascript
  /**
   * Brief description
   * @param {Type} param - Parameter description
   * @returns {Type} Return description
   * @throws {Error} When error occurs
   */
  ```

## Documentation Standards

### Public Functions
- **Requirement**: Must have JSDoc with `@param`, `@returns`, `@throws`
- **Example**:
  ```javascript
  /**
   * Process RDF data
   * @param {Store} store - RDF store
   * @param {Object} options - Processing options
   * @returns {Promise<Object>} Processing result
   * @throws {TypeError} If store is invalid
   */
  async function processRDF(store, options = {}) {
    // ...
  }
  ```

### Complex Logic
- **Requirement**: Must have inline comments explaining why
- **Example**:
  ```javascript
  // Use Promise.race to implement timeout
  // This ensures the operation fails fast if it takes too long
  const result = await Promise.race([operation(), timeoutPromise]);
  ```

### Examples
- **Requirement**: Public APIs should have usage examples
- **Location**: JSDoc `@example` tags or separate examples directory

## Reference Implementations

### Error Handling
- **File**: `src/knowledge-engine/condition-evaluator.mjs`
- **Why**: Consistent error handling, good error messages, proper context

### Async Patterns
- **File**: `src/knowledge-engine/hook-executor.mjs`
- **Why**: Consistent async/await, proper error handling, good structure

### Documentation
- **File**: `src/knowledge-engine/condition-evaluator.mjs`
- **Why**: Comprehensive JSDoc, clear parameter descriptions, good examples

## CI Checks

### Automated Enforcement
- **Formatting**: `pnpm format --check` (fails if not formatted)
- **Linting**: `pnpm lint` (fails if linting errors)
- **Tests**: `pnpm test` (fails if tests fail)
- **Coverage**: Coverage check (fails if coverage < 80%)

### Pre-commit Hooks
- Format on commit
- Lint on commit
- Run tests on commit (optional, can be slow)

## Code Review Checklist

When reviewing code, check:
- [ ] Code follows style standards (formatting, naming)
- [ ] Code uses standard patterns (error handling, async/await)
- [ ] Code meets quality standards (test coverage, error handling)
- [ ] Code has required documentation (JSDoc for public APIs)
- [ ] Code passes CI checks (format, lint, test)

## Maintenance

### Regular Audits
- **Frequency**: Weekly or monthly
- **Process**:
  1. Run consistency checks (`pnpm format --check`, `pnpm lint`)
  2. Identify new inconsistencies
  3. Apply standards
  4. Update controls if needed

### Updating Standards
- Standards should evolve with the codebase
- Changes should be documented
- Team should be notified of changes

