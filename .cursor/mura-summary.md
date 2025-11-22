# Mura Inventory - Codebase Inconsistencies

## Step 1: Identified Mura

### 1. Code Style Inconsistency

#### Unused Variables (158 warnings)
- **Issue**: Variables defined but never used should be prefixed with `_`
- **Examples**:
  - `context` parameter unused in multiple files
  - `hook` parameter unused in some functions
  - Imported modules not used (e.g., `Store`, `crypto`, `fs`, `path`)
- **Impact**: High - Creates noise in linting, indicates dead code
- **Files affected**: ~30+ files across codebase

#### Import Organization
- **Issue**: Import ordering not consistently enforced
- **Examples**:
  - Some files group std/external/local imports
  - Others mix import types
- **Impact**: Medium - Affects readability

### 2. Pattern Inconsistency

#### Error Handling Patterns
- **Issue**: Multiple error handling patterns used
- **Patterns found**:
  1. `throw new Error()` - Generic errors
  2. `try/catch` with custom error classes
  3. Zod validation errors
  4. Promise rejections
- **Impact**: High - Makes error handling unpredictable
- **Files affected**: Multiple files in `src/knowledge-engine/`, `src/cli/`

### 3. Quality Inconsistency

#### Test Coverage
- **Issue**: Uneven test coverage across modules
- **Impact**: Medium - Some modules well-tested, others not
- **Note**: Need to verify actual coverage numbers

#### Documentation
- **Issue**: Some files have comprehensive JSDoc, others minimal
- **Impact**: Medium - Affects maintainability

### 4. Complexity Inconsistency

#### Function Complexity
- **Issue**: Similar problems solved with different complexity levels
- **Impact**: Low-Medium - Affects maintainability

## Step 2: Variability Measurement

### Style Consistency
- **Unused variable warnings**: 158
- **Formatting violations**: 0 (after formatting)
- **Inconsistency score**: High (due to unused variables)

### Pattern Consistency
- **Error handling patterns**: 4+ different patterns
- **Inconsistency score**: High

### Quality Consistency
- **Test coverage**: Need to measure
- **Documentation**: Need to measure
- **Inconsistency score**: Unknown (needs measurement)

## Step 3: Standards Definition

### Style Standards
- **Unused variables**: Prefix with `_` (e.g., `_context`, `_hook`)
- **Formatting**: Use `pnpm format` (enforced via Prettier)
- **Imports**: Alphabetical, grouped by std/external/local

### Pattern Standards
- **Error handling**: Use consistent error types (Zod for validation, custom classes for business logic)
- **Testing**: Minimum 80% test coverage (per project standards)
- **Documentation**: All public APIs must have JSDoc

### Quality Standards
- **Test coverage**: Minimum 80% for all modules
- **Error handling**: All fallible operations must handle errors
- **Documentation**: All public functions documented

## Step 4: Apply Consistently

### Priority 1: Fix Unused Variables (High Impact)
- Fix 158 unused variable warnings
- Prefix unused parameters with `_`
- Remove unused imports

### Priority 2: Standardize Error Handling (High Impact)
- Audit error handling patterns
- Choose standard pattern
- Refactor to standard

### Priority 3: Import Organization (Medium Impact)
- Standardize import ordering
- Group imports consistently

## Step 5: Control

### Automated Checks
- ✅ `pnpm format --check` in CI
- ✅ `pnpm lint` in CI
- ⚠️ Add unused variable check enforcement
- ⚠️ Add import ordering check

### Code Review Checklist
- [ ] No unused variables (or prefixed with `_`)
- [ ] Consistent error handling
- [ ] Imports properly organized
- [ ] Documentation present for public APIs

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
- **Reference**: `sidecar/server/middleware/02.error-handler.mjs`

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
- **Reference**: `sidecar/server/middleware/02.error-handler.mjs`
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

