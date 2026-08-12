# Integration Tests for process_mining_wasm

This directory contains comprehensive integration tests for the process_mining_wasm library in both Node.js and browser environments.

## Test Files

### nodejs.test.ts

Node.js integration tests covering:

- WASM module initialization
- XES file loading and analysis
- OCEL JSON loading and analysis
- Event statistics extraction
- Process discovery algorithms (DFG, Alpha++, DECLARE)
- State management (object counting, clearing)
- Complete end-to-end workflows
- Error handling and recovery
- Data integrity validation
- Result consistency

**Run with:**

```bash
npm run test:integration:nodejs
```

### browser.test.ts

Browser environment integration tests covering:

- WASM initialization in browser context
- Async operations and promises
- DOM element interaction
- File API and FileReader
- Memory management
- Error handling in browser context
- Data validation
- Browser-specific features (requestAnimationFrame, events)
- Security considerations (CORS, XSS prevention)
- Worker communication patterns

**Run with:**

```bash
npm run test:integration:browser
```

## Running Tests

```bash
# Run all integration tests
npm run test:integration

# Run Node.js tests only
npm run test:integration:nodejs

# Run browser tests only
npm run test:integration:browser

# Run with watch mode
npm run test:watch

# Run with coverage report
npm run test:coverage
```

## Test Data

Tests use sample data from `../fixtures/`:

- `sample.xes` - Standard XES event log
- `sample.json` - OCEL (Object-Centric Event Log) in JSON format

See `../fixtures/README.md` for details.

## Test Structure

Each test file follows this pattern:

1. **Setup** - Initialize WASM module and load test fixtures
2. **Test Suites** - Organized by functionality (loading, analysis, discovery, etc.)
3. **Individual Tests** - Specific test cases with clear assertions
4. **Cleanup** - Reset mocks and clear state after each test

## Key Test Suites

### Node.js Tests

1. **Initialization** - Module setup and version detection
2. **XES File Loading** - Load and validate XES files
3. **OCEL File Loading** - Load and validate OCEL files
4. **Event Statistics Analysis** - Extract event frequency and counts
5. **OCEL Statistics Analysis** - Object-centric analysis
6. **Process Discovery** - DFG, Alpha++, DECLARE algorithms
7. **Analysis Functions** - Case duration, dotted chart
8. **State Management** - Object tracking and cleanup
9. **Complete Workflows** - End-to-end scenarios
10. **Error Handling** - Invalid inputs and error recovery
11. **Data Integrity** - Format validation and consistency

### Browser Tests

1. **WASM Initialization** - Async loading and setup
2. **DOM Integration** - Element selection and manipulation
3. **File Input Handling** - File selection and reading
4. **Async Operations** - Promise handling and timing
5. **Memory Management** - Buffer tracking and cleanup
6. **Error Handling** - Browser-specific error scenarios
7. **Data Validation** - Input format checking
8. **Browser Features** - Events, storage, requestAnimationFrame
9. **Worker Scenarios** - postMessage communication
10. **Cross-Origin & Security** - CORS and XSS prevention

## Writing New Tests

### Basic Test Template

```typescript
describe('Feature Name', () => {
  beforeEach(() => {
    // Setup before each test
  });

  afterEach(() => {
    // Cleanup after each test
  });

  test('should do something specific', () => {
    // Arrange
    const input = 'test data';

    // Act
    const result = functionUnderTest(input);

    // Assert
    expect(result).toEqual('expected output');
  });
});
```

### Async Test Template

```typescript
test('should handle async operation', async () => {
  const result = await asyncFunction();
  expect(result).toBeDefined();
});
```

### Error Test Template

```typescript
test('should handle errors gracefully', () => {
  expect(() => {
    functionThatThrows();
  }).toThrow('Error message');
});
```

## Test Configuration

Tests are configured in:

- `jest.config.js` - Main Jest configuration
- `jest.setup.js` - Global setup and utilities

Key settings:

- **Timeout**: 30 seconds (configurable per test)
- **Environment**: Node.js or browser
- **Coverage**: Statements > 80%, Branches > 75%

## Fixtures

Test data files in `../fixtures/`:

**sample.xes** (XES Event Log)

- 3 traces
- 14 events
- 5 activities
- 3 resources

**sample.json** (OCEL)

- 10 events
- 7 objects
- 3 object types
- Event-object relationships

## Coverage

Current coverage goals:

- Statements: > 80%
- Branches: > 75%
- Functions: > 85%
- Lines: > 80%

View coverage report:

```bash
npm run test:coverage
```

## Debugging Tests

### Verbose Output

```bash
npm run test:integration -- --verbose
```

### Single Test

```bash
npm run test:integration -- --testNamePattern="test name"
```

### Debug Mode

```bash
node --inspect-brk node_modules/.bin/jest --runInBand
```

## Best Practices

1. **Isolation** - Each test should be independent
2. **Clarity** - Test names should describe what's tested
3. **Fixtures** - Use provided test data, don't hardcode
4. **Assertions** - Use specific, meaningful assertions
5. **Cleanup** - Always clean up after tests
6. **Performance** - Keep tests fast (< 1 second each)
7. **Coverage** - Test both success and failure paths

## Troubleshooting

### Tests Not Found

```bash
# Ensure test files exist
ls -la integration/*.test.ts

# Rebuild WASM
npm run build:all
```

### Timeout Errors

- Increase timeout: `jest.setTimeout(60000);`
- Check for hanging operations
- Verify async/await usage

### Module Not Found

- Install dependencies: `npm install`
- Build WASM: `npm run build`
- Check import paths

### Fixture Loading Fails

```bash
# Verify fixture files exist
ls -la ../fixtures/

# Check file permissions
file ../fixtures/sample.xes
```

## Performance

Average test execution times:

- Node.js tests: ~5 seconds
- Browser tests: ~10 seconds
- Total suite: ~15 seconds

Optimize by:

- Mocking expensive operations
- Using beforeAll for shared setup
- Parallel test execution (Jest default)

## CI/CD Integration

These tests run in CI pipelines:

```yaml
test:
  script:
    - npm install
    - npm run build:all
    - npm run test:integration
```

## Related Documentation

- `../fixtures/README.md` - Test data documentation
- `../../examples/` - Working examples
- `../../README.md` - Main project README
- `jest.config.js` - Jest configuration
- `jest.setup.js` - Setup and utilities
