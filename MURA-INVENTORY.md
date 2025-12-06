# Mura (Unevenness) Inventory

**Date**: 2025-01-23
**Analysis**: Eliminate Mura workflow execution

## Step 1: Identified Mura

### 1. Code Style Inconsistency

- [x] **520 files** need Prettier formatting (format:check shows warnings)
- [x] **156 lint warnings/errors** across codebase
- [x] Arrow function parameter formatting inconsistency:
  - Some use: `async (span) =>`
  - Some use: `async span =>` (Prettier standard with `arrowParens: "avoid"`)
- [x] Trailing comma inconsistency (some files have, some don't)
- [x] Quote style inconsistency (some double, some single - should be single)

**Impact**: High - Affects code readability and maintainability

### 2. Pattern Inconsistency

- [x] **Error handling patterns** - Multiple different approaches:
  - Try/catch blocks
  - Promise rejection patterns
  - Custom error classes (ValidationError, RDFError, HookError)
  - Legacy error handler utilities
- [x] **Async/await patterns** - Inconsistent usage:
  - Some use `.then()` chains
  - Some use async/await
  - Some mix both

**Impact**: Medium - Makes error handling unpredictable

### 3. Quality Inconsistency

- [x] **Test coverage variance**: 20% - 90% across modules
  - Core knowledge engine: ~85%
  - Integration tests: ~72%
  - E2E tests: ~68%
  - Knowledge hooks: ~45%
  - Performance tests: ~40%
- [x] **Test quality variance**:
  - Some modules have comprehensive tests
  - Some modules have minimal/no tests
- [x] **Documentation variance**:
  - Some modules well-documented
  - Some modules lack documentation

**Impact**: High - Uneven quality increases maintenance risk

### 4. Complexity Inconsistency

- [x] **Similar problems solved differently**:
  - Some simple problems have complex solutions
  - Some complex problems have simple solutions
- [x] **Abstraction levels vary**:
  - Some code over-abstracted
  - Some code under-abstracted

**Impact**: Medium - Makes codebase harder to understand

### 5. Documentation Inconsistency

- [x] **JSDoc enforcement** - ESLint enforces but not all files comply
- [x] **Documentation style** - Different documentation styles across modules
- [x] **Example code** - Some APIs have examples, others don't

**Impact**: Low - Affects developer experience

---

## Step 2: Variability Measurement

### Style Consistency

- Formatting violations: **520 files**
- Lint violations: **156 warnings/errors**
- Inconsistency score: **HIGH**

### Pattern Consistency

- Error handling patterns: **5+ different patterns**
- Async patterns: **3 different patterns**
- Inconsistency score: **MEDIUM**

### Quality Consistency

- Test coverage range: **20% - 90%**
- Coverage variance: **70%**
- Inconsistency score: **HIGH**

### Complexity Consistency

- Cyclomatic complexity: **Varies significantly**
- Inconsistency score: **MEDIUM**

---

## Step 3: Standards Definition

### Style Standards

- **Naming**: camelCase for functions, PascalCase for classes/types
- **Formatting**: Prettier with configured rules (single quotes, 2 spaces, trailing commas)
- **Imports**: Alphabetical, grouped by std/external/local
- **Arrow functions**: `arrowParens: "avoid"` (single param without parens)

### Pattern Standards

- **Error handling**: Use try/catch with custom error classes where appropriate
- **Async/await**: Prefer async/await over Promise chains
- **Validation**: Use Zod schemas consistently
- **Testing**: Minimum 80% test coverage for all modules

### Quality Standards

- **Test coverage**: Minimum 80% for all modules (currently 95% threshold in vitest.config.mjs)
- **Error handling**: All fallible operations should handle errors
- **Documentation**: All public APIs must have JSDoc comments

### Documentation Standards

- **Public functions**: Must have JSDoc with @param, @returns
- **Complex logic**: Must have inline comments explaining why
- **Examples**: Public APIs should have usage examples

---

## Step 4: Reference Implementations

### Error Handling
- Uses OpenAPI-compliant error format
- Proper error propagation
- Good error messages

### Test Patterns
- **Reference**: `test/knowledge-engine/parse.test.mjs`
- Comprehensive test coverage
- Good test organization
- Clear test names

### Code Style
- **Reference**: Files formatted by Prettier (after `pnpm format`)
- Consistent formatting
- Single quotes
- Trailing commas

---

## Step 5: Action Plan

### Immediate (P0)

1. **Apply Prettier formatting** to all files
   - Run `pnpm format` to fix all formatting issues
   - Verify with `pnpm format:check`

2. **Fix lint violations**
   - Address 156 lint warnings/errors
   - Focus on high-impact issues first

3. **Standardize arrow function formatting**
   - Ensure all single-param arrow functions use `param =>` (no parens)
   - Prettier will handle this automatically

### Short-term (P1)

1. **Standardize error handling**
   - Document error handling patterns
   - Refactor inconsistent error handling gradually

2. **Improve test coverage**
   - Bring low-coverage modules to 80%+
   - Focus on critical paths first

3. **Standardize async patterns**
   - Convert Promise chains to async/await
   - Document async/await best practices

### Medium-term (P2)

1. **Documentation standardization**
   - Ensure all public APIs have JSDoc
   - Add examples to key APIs

2. **Complexity reduction**
   - Refactor over-complex code
   - Simplify where possible

---

## Controls (Step 5)

### Automated Checks

- [x] Prettier formatting in CI (`pnpm format:check`)
- [x] ESLint in CI (`pnpm lint`)
- [x] Test coverage checks (95% threshold)
- [x] Pre-commit hooks (lint-staged)

### Code Review Checklist

- [ ] Code follows style standards (Prettier formatted)
- [ ] Code uses standard patterns
- [ ] Code meets quality standards (tests, error handling)
- [ ] Code has required documentation (JSDoc)

### Documentation

- [x] `.prettierrc` - Prettier configuration
- [x] `eslint.config.mjs` - ESLint configuration
- [x] `vitest.config.mjs` - Test configuration
- [ ] Error handling guide (to be created)
- [ ] Async/await best practices (to be created)

### Regular Audits

- **Frequency**: Weekly
- **Process**:
  1. Run `pnpm format:check`
  2. Run `pnpm lint`
  3. Run `pnpm test --coverage`
  4. Identify new inconsistencies
  5. Apply standards
  6. Update controls if needed
