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

