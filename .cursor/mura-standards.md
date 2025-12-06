# Mura Elimination Standards

## Standards Definition

### Style Standards

#### Unused Variables
- **Standard**: All unused variables/parameters must be prefixed with `_`
- **Examples**:
  - `function handler(context, hook)` → `function handler(_context, _hook)`
  - `const profile = ...` (unused) → `const _profile = ...`
  - `import { Store } from 'n3'` (unused) → Remove or use `_Store` if needed for type hints
- **Enforcement**: ESLint rule `no-unused-vars` with `argsIgnorePattern: '^_'`

#### Formatting
- **Standard**: Use Prettier (enforced via `pnpm format`)
- **Enforcement**: CI check: `pnpm format --check`

#### Import Organization
- **Standard**: Group imports in order:
  1. Node.js built-ins (`fs`, `path`, `crypto`)
  2. External packages (`zod`, `n3`, `@opentelemetry/api`)
  3. Internal modules (`./canonicalize.mjs`, `./observability.mjs`)
- **Enforcement**: Manual review, consider adding import-order plugin

### Pattern Standards

#### Error Handling
- **Standard**: Use consistent error types
  - **Validation errors**: Zod schemas with `.parse()` or `.safeParse()`
  - **Business logic errors**: Custom error classes extending `Error`
  - **System errors**: Use standard Error with context
- **Pattern**:
  ```javascript
  // Validation
  const result = Schema.safeParse(data);
  if (!result.success) {
    throw new ValidationError(result.error);
  }
  
  // Business logic
  try {
    // operation
  } catch (error) {
    throw new BusinessError('Operation failed', { cause: error });
  }
  ```

#### Testing
- **Standard**: Minimum 80% test coverage for all modules
- **Pattern**: Use Vitest with AAA pattern (Arrange, Act, Assert)
- **Enforcement**: CI coverage check

#### Documentation
- **Standard**: All public APIs must have JSDoc
- **Pattern**:
  ```javascript
  /**
   * Brief description
   * @param {Type} param - Description
   * @returns {Type} Description
   */
  export function publicFunction(param) { ... }
  ```
- **Enforcement**: ESLint `jsdoc/require-jsdoc` for public functions

### Quality Standards

#### Test Coverage
- **Minimum**: 80% for all modules
- **Target**: 95% for critical paths
- **Enforcement**: CI coverage check

#### Error Handling
- **Standard**: All fallible operations must handle errors
- **Pattern**: Use try/catch or Result types
- **Enforcement**: Code review

#### Code Complexity
- **Standard**: Cyclomatic complexity < 10 per function
- **Enforcement**: Code review, consider adding complexity checker

## Reference Implementations

### Error Handling
- **Pattern**: OpenAPI-compliant error responses with OTEL tracing

### Testing
- **Reference**: `test/knowledge-engine/` test files
- **Pattern**: Comprehensive test coverage with clear test names

### Documentation
- **Reference**: `src/knowledge-engine/transaction.mjs`
- **Pattern**: Comprehensive JSDoc with type definitions

## Application Priority

### Priority 1: Unused Variables (High Impact, Easy Fix)
- **158 warnings** across codebase
- **Fix**: Prefix unused variables with `_`
- **Files**: Focus on `src/` directory first, then `test/`, then `examples/`

### Priority 2: Error Handling (High Impact, Medium Effort)
- **Multiple patterns** in use
- **Fix**: Standardize on Zod + custom error classes
- **Files**: `src/knowledge-engine/`, `src/cli/`

### Priority 3: Import Organization (Medium Impact, Low Effort)
- **Inconsistent ordering**
- **Fix**: Group imports consistently
- **Files**: All source files

## Controls

### Automated Checks (CI)
- ✅ `pnpm format --check` - Formatting
- ✅ `pnpm lint` - Linting (includes unused vars)
- ⚠️ Coverage check - Need to add threshold enforcement
- ⚠️ Import order - Consider adding plugin

### Code Review Checklist
- [ ] No unused variables (or prefixed with `_`)
- [ ] Consistent error handling pattern
- [ ] Imports properly organized
- [ ] Documentation present for public APIs
- [ ] Tests meet coverage threshold

### Documentation
- This file: Standards reference
- `.cursor/mura-inventory.md`: Current inconsistencies
- Project README: Link to standards

