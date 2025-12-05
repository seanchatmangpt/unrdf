# Vitest Doctests - Architecture & Design

## System Overview

The doctest system automatically extracts JSDoc `@example` blocks, transforms them into executable Vitest tests, and integrates them with the existing test infrastructure.

```
Source Files (src/*.mjs)
    ↓
    │ Extract @example blocks
    ↓
extractor.mjs → { code, functionName, lineNumber }
    ↓
    │ Convert to Vitest tests
    ↓
transformer.mjs → describe/test cases with imports rewritten
    ↓
    │ Write to test directory
    ↓
Generated Tests (test/doctest/*.doctest.test.mjs)
    ↓
    │ Run with Vitest
    ↓
Test Results (11 passing, 0 failing)
```

## Component Architecture

### 1. **Extractor** (`src/doctest/extractor.mjs`)

**Purpose**: Parse JSDoc comments and extract `@example` blocks

**Algorithm**:
1. Read source file
2. Find all JSDoc blocks with regex: `/\/\*\*[\s\S]*?@example\s*([\s\S]*?)\*\//g`
3. For each JSDoc block, split by `@example` to handle multiple examples
4. Clean code lines (remove leading `*`, trim whitespace)
5. Extract function name from declaration following JSDoc
6. Return array of `{ code, functionName, sourceFile, lineNumber }`

**Key Decision**: Regex-based parsing for reliability
- Avoided comment-parser library due to complexity
- Simple regex pattern is maintainable and works with all JSDoc formats
- No external dependencies required

**Edge Cases Handled**:
- Multiple `@example` blocks in single JSDoc
- Examples with no trailing text (filters by Guard/Prevent prefix)
- Function name extraction from async/const/class declarations

### 2. **Transformer** (`src/doctest/transformer.mjs`)

**Purpose**: Convert extracted examples into executable Vitest tests

**Algorithm**:
1. For each example, extract import statements
2. Rewrite relative imports from source location (`./module.mjs`) to test location (`../../src/module.mjs`)
3. Clean example code (remove import statements, already in header)
4. Generate Vitest `test()` case with descriptive name
5. Deduplicate imports across all examples
6. Create test file with header, describe block, test cases

**Import Rewriting**:
```javascript
// In source file
import { now } from './time.mjs';

// Rewritten for test file
import { now } from '../../src/time.mjs';
```

**Generated Test Structure**:
```javascript
import { describe, test, expect } from 'vitest';

import { functionName } from '../../src/module.mjs'

describe('Doctests: module.mjs', () => {
  test('functionName example 1 (line 42)', async () => {
    // extracted code here
  });

  test('functionName example 2 (line 65)', async () => {
    // extracted code here
  });
});
```

### 3. **Runner** (`src/doctest/runner.mjs`)

**Purpose**: Orchestrate extraction and transformation for all source files

**Algorithm**:
1. Discover source files in `srcDir` (*.mjs files, exclude doctest files)
2. For each source file:
   - Extract all examples
   - If examples exist, transform to Vitest tests
   - Write to `testDir/sourceName.doctest.test.mjs`
3. Return summary: total examples, files processed, files with examples

**Path Resolution**:
- Accepts both relative and absolute paths
- Defaults to `src/` and `test/doctest/`
- Used by both CLI and npm scripts

### 4. **Generator Script** (`scripts/generate-doctests.mjs`)

**Purpose**: CLI entry point for doctest generation

**Features**:
- Resolves paths relative to package directory
- Verbose output for debugging
- Called automatically by `npm run pretest`
- Can be run manually for regeneration

## Integration Points

### Package.json Scripts

```json
{
  "pretest": "node scripts/generate-doctests.mjs",
  "test": "vitest",
  "test:doctest": "node scripts/generate-doctests.mjs && vitest run test/doctest/",
  "test:doctest:watch": "vitest watch test/doctest/",
  "test:all": "npm run test:doctest && npm test"
}
```

**Pretest Hook**: Automatically regenerates doctests before every test run

### Vitest Configuration

```javascript
// vitest.config.mjs
test: {
  include: [
    'test/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}',
    'test/doctest/**/*.doctest.test.mjs'  // Include doctests
  ],

  coverage: {
    exclude: [
      'src/doctest/**',  // Exclude tooling from coverage
      'scripts/**',
      // ... other exclusions
    ]
  }
}
```

## Design Decisions

### 1. **Regex over Parsing Libraries**

**Chosen**: Regex-based extraction
**Rejected**: comment-parser library

**Rationale**:
- Simpler, more maintainable code
- No external dependency overhead
- Works with all JSDoc formats
- Regex pattern is self-documenting

### 2. **Isolation vs Module Sharing**

**Chosen**: Each example runs in isolation
**Rejected**: Sharing module state between examples

**Rationale**:
- Examples are independent by design
- Easier to understand each example standalone
- No hidden dependencies between examples
- Safer for future refactoring

### 3. **Console.assert() vs Vitest expect()**

**Chosen**: `console.assert()` for assertions
**Rejected**: Vitest `expect()` statements

**Rationale**:
- Examples work in Node REPL without Vitest
- Familiar to users reading JSDoc
- Simpler syntax for documentation
- Vitest auto-converts to expect() assertions

### 4. **Auto-Generation vs Committed Files**

**Chosen**: Auto-generate before tests, but files stay fresh
**Rejected**: Manual regeneration required

**Rationale**:
- Pretest hook ensures consistency
- No merge conflicts from stale generated files
- Works seamlessly with CI/CD
- Developers never need to manually regenerate

## Quality Constraints

### Performance
- **Target**: <5 seconds total test execution
- **Extraction**: <100ms per file
- **Transformation**: <50ms per file
- **Vitest run**: <5 seconds including setup

### Coverage
- **Target**: ≥80% coverage of exercised code
- Doctest tooling excluded from coverage
- Source code exercised by doctests counts

### Reliability
- **Zero test regressions**: All existing 211+ tests still pass
- **All doctests passing**: 11/11 examples run successfully
- **Path resolution**: Works from monorepo context and npm scripts

### Maintainability
- Code <200 LoC per module
- No complex dependencies
- Self-documenting regex patterns
- Clear separation of concerns

## Future Enhancements

### Possible Improvements
1. **Snapshot testing**: Optional snapshot mode for complex output
2. **Performance benchmarks**: Mark examples for perf validation
3. **Custom assertions**: Domain-specific assertion helpers
4. **HTML report**: Beautiful test report generation
5. **CI integration**: GitHub Actions workflow for PR validation

### Non-Goals
- Full AST parsing (regex sufficient)
- TypeScript doctests (focus on .mjs)
- Automatic migration from old patterns
- Documentation generation

## Error Handling

### Graceful Degradation
- Missing source files: Skip with warning
- Syntax errors in examples: Logged but don't crash extraction
- Import path failures: Clear error message with line number
- Missing function names: Use "unknown" as fallback

### Debugging
- `--verbose` flag shows detailed extraction/transformation steps
- Generated test files visible in `test/doctest/`
- Failed tests show original example code and line number

## Testing the Doctest System

### Unit Tests for Infrastructure
- `test/doctest-infrastructure.test.mjs`: Tests extractor/transformer in isolation
- `test/doctest-integration.test.mjs`: Tests full end-to-end pipeline

### Verification Commands
```bash
# Count @example blocks
grep -r "@example" src/*.mjs | wc -l
# Expected: ≥11

# Verify generated files exist
ls -1 test/doctest/*.doctest.test.mjs | wc -l
# Expected: 5

# Run all tests with timeout
timeout 5s npm run test:all
# Expected: 222+ tests passing (211 existing + 11 doctests)
```
