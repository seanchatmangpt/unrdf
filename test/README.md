# unrdf Test Suite

This directory contains the comprehensive test suite for the unrdf composable framework, organized using the London School of TDD methodology.

## Test Structure

```
test/
├── composables/           # Tests for each composable
│   ├── useStore.test.mjs
│   ├── useTerms.test.mjs
│   ├── usePrefixes.test.mjs
│   ├── useTurtle.test.mjs
│   ├── useGraph.test.mjs
│   ├── useValidator.test.mjs
│   ├── useReasoner.test.mjs
│   ├── useCanon.test.mjs
│   └── useZod.test.mjs
├── utils/                 # Tests for utility functions
│   ├── term-utils.test.mjs
│   └── utils.test.mjs
└── README.md             # This file
```

## Test Methodology

### London School of TDD

Each test file follows the London School of TDD approach:

1. **Start with the simplest failing test** - Write a test that fails for the right reason
2. **Write minimal code to make it pass** - Implement just enough to satisfy the test
3. **Refactor while keeping tests green** - Improve the code without breaking tests
4. **Add complexity incrementally** - Build up functionality test by test

### Test Organization

- **One test file per composable** - Each composable has its own dedicated test file
- **Self-contained tests** - Each test is independent and doesn't reference other files
- **Clear naming** - Test names describe the expected behavior
- **Arrange-Act-Assert pattern** - Each test follows the AAA pattern

## Running Tests

### All Tests
```bash
pnpm test:run          # Run all tests
pnpm test:coverage     # Run with coverage
pnpm test:watch        # Watch mode
```

### Specific Test Files
```bash
# Run specific composable tests
pnpm vitest test/composables/useStore.test.mjs
pnpm vitest test/composables/useGraph.test.mjs

# Run utility tests
pnpm vitest test/utils/term-utils.test.mjs
```

### Test Scripts

- `pnpm test:run` - Run all tests without coverage
- `pnpm test:coverage` - Run tests with coverage reporting
- `pnpm test:watch` - Watch mode for development
- `pnpm test:ui` - Interactive UI mode
- `pnpm test:bench` - Benchmark tests

## Test Configuration

The test suite is configured in `vitest.config.mjs` with:

- **Maximum concurrency** - Uses all available CPU cores
- **Parallel execution** - Tests run in parallel by default
- **Thread pool** - Optimized for performance
- **Coverage reporting** - Comprehensive coverage metrics
- **Timeout settings** - Generous timeouts for RDF operations

## Current Status

- **Total Tests**: 92
- **Passing**: 90
- **Failing**: 2 (expected with TDD approach)
- **Coverage**: Comprehensive across all composables

## Failing Tests

The 2 failing tests are intentional and follow the TDD methodology:

1. **useTerms literal datatype test** - Tests the expected behavior of literal creation
2. **term-utils literal test** - Tests the utility function for literal normalization

These tests will guide the implementation of the actual composable functions.

## Adding New Tests

When adding new tests:

1. **Create a new test file** in the appropriate directory
2. **Follow the naming convention** - `[composable].test.mjs`
3. **Use the London School of TDD** - Start with failing tests
4. **Keep tests self-contained** - Don't reference other files
5. **Use clear, descriptive names** - Test names should explain the expected behavior

## Test Dependencies

The test suite uses:

- **Vitest** - Test runner and framework
- **N3** - RDF library for test data
- **Zod** - Schema validation for testing
- **Node.js** - Runtime environment

## Contributing

When contributing to the test suite:

1. **Follow the existing patterns** - Use the same structure and naming
2. **Write failing tests first** - Follow the TDD approach
3. **Keep tests focused** - One test per behavior
4. **Use descriptive names** - Make test intent clear
5. **Maintain concurrency** - Don't break parallel execution
