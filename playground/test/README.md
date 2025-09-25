# UNRDF Playground Integration Tests

Comprehensive integration tests for the UNRDF Knowledge Hooks Runtime Playground, following the 80/20 rule to cover the most critical functionality.

## Overview

These tests validate the core functionality of the playground with a focus on:
- **Hook Management**: Creation, evaluation, planning, and deletion
- **Data Management**: Data source creation, querying, and management
- **Runtime Operations**: Status monitoring, commands, and error handling
- **Error Scenarios**: Robust error handling and edge cases
- **Performance**: Load testing and concurrent operations

## Test Structure

### Core Tests (`integration.test.mjs`)

- **Runtime Management**: Health checks, status monitoring, cleanup operations
- **Data Source Management**: CRUD operations, SPARQL querying (SELECT/ASK)
- **Hook Management**: Complete hook lifecycle testing
- **Error Handling**: Invalid requests, missing resources, malformed data
- **Performance Testing**: Concurrent evaluations, rapid operations
- **End-to-End Workflows**: Complete data-to-hook workflows

### Test Utilities (`test-utils.mjs`)

- **Test Data Generation**: Sample RDF data and hook fixtures
- **Performance Testing**: Execution time measurement and load testing
- **Helper Functions**: Resource tracking, retry logic, wait conditions

### Test Configuration

- **Vitest Setup**: Proper configuration for integration testing
- **Global Setup**: Server startup and teardown
- **Per-Test Setup**: Resource cleanup and state management

## Running Tests

### Prerequisites

Ensure the playground server is running:
```bash
cd playground
pnpm server
```

### Run Integration Tests

```bash
# Run all integration tests
pnpm test:integration

# Run with coverage
pnpm test:integration:coverage

# Run specific test file
pnpm vitest run test/integration.test.mjs

# Run with verbose output
pnpm vitest run test/integration.test.mjs --reporter=verbose
```

### Test Scripts

The following scripts are available in `package.json`:

```json
{
  "test:integration": "vitest run --config test/integration.config.mjs",
  "test:integration:watch": "vitest --config test/integration.config.mjs --watch",
  "test:integration:coverage": "vitest run --config test/integration.config.mjs --coverage",
  "test:integration:ui": "vitest --config test/integration.config.mjs --ui"
}
```

## Test Coverage

### 80/20 Rule Implementation

The tests focus on the 20% of functionality that provides 80% of the value:

#### High-Value Features (80%+)
- ✅ Hook creation and evaluation (core functionality)
- ✅ Data source querying (primary data access)
- ✅ Runtime status monitoring (system health)
- ✅ Error handling (robustness)
- ✅ Basic CRUD operations (essential workflows)

#### Medium-Value Features (15%)
- ⚠️ Concurrent operations (performance)
- ⚠️ Complex predicates (advanced features)
- ⚠️ Load testing (scalability)

#### Low-Value Features (5%)
- ⚠️ UI interactions (covered by manual testing)
- ⚠️ Advanced SPARQL features (edge cases)
- ⚠️ Custom predicate registration (specialized use)

### Coverage Targets

```
Global Coverage Goals:
├── Branches: 70%     ✅ (Critical paths covered)
├── Functions: 75%    ✅ (Core functions tested)
├── Lines: 80%        ✅ (Main logic covered)
└── Statements: 80%   ✅ (Primary statements tested)
```

## Test Data

### Sample Data

The tests use realistic sample data including:
- Service health metrics (error rates, latency, request counts)
- User profiles with scores and status
- Multiple RDF namespaces and data types

### Fixtures

Pre-defined test fixtures for common scenarios:
- Minimal hook configurations
- Complex multi-predicate hooks
- Various RDF data patterns
- Error condition data

## Performance Testing

### Load Testing

- Concurrent hook evaluations (up to 10 simultaneous)
- Rapid hook creation/deletion cycles
- Bulk data operations

### Performance Metrics

- Execution time measurement
- Memory usage tracking
- Request throughput analysis
- Error rate monitoring

## Error Scenarios

### Covered Error Cases

- ❌ Missing hooks/data sources (404 handling)
- ❌ Invalid hook creation (400 validation)
- ❌ Malformed queries (500 server errors)
- ❌ Unsupported SPARQL operations
- ❌ Concurrent resource conflicts
- ❌ Network timeouts and failures

### Edge Cases

- Empty result sets
- Large datasets
- Special characters in identifiers
- Resource cleanup during errors

## Integration Patterns

### End-to-End Workflows

1. **Data-to-Hook Pipeline**
   - Create data source
   - Create hook referencing data
   - Query data to verify
   - Evaluate hook with data
   - Clean up resources

2. **Complex Hook Testing**
   - Multi-predicate hooks
   - OR/AND logic combinations
   - Custom threshold values
   - Result validation

3. **Runtime Management**
   - Status monitoring
   - Resource cleanup
   - Health checks
   - Error recovery

## Debugging

### Debug Mode

```bash
# Run tests with debug output
DEBUG=1 pnpm test:integration

# Run single test with detailed logging
pnpm vitest run test/integration.test.mjs --reporter=verbose
```

### Common Issues

1. **Server Not Ready**
   - Ensure server is running on port 3000
   - Check for port conflicts
   - Verify server startup logs

2. **Test Timeouts**
   - Increase timeout for slow operations
   - Check network connectivity
   - Verify server responsiveness

3. **Resource Conflicts**
   - Tests clean up after themselves
   - Use unique identifiers for each test
   - Check for leftover resources

## Extending Tests

### Adding New Tests

1. Add test cases to `integration.test.mjs`
2. Create fixtures in `test-utils.mjs`
3. Update coverage thresholds if needed
4. Add documentation for new functionality

### Custom Test Utilities

```javascript
// Example: Custom matcher for hook results
import { expect } from 'vitest'

expect.extend({
  toHaveFiredHook(received, expected = true) {
    const pass = received.fired === expected
    return {
      message: () => `expected hook to ${expected ? 'have fired' : 'not have fired'}`,
      pass
    }
  }
})
```

## CI/CD Integration

### GitHub Actions Example

```yaml
- name: Run Integration Tests
  run: |
    cd playground
    pnpm install
    pnpm server &
    sleep 5
    pnpm test:integration
  env:
    CI: true
```

### Coverage Reporting

- HTML reports generated in `coverage/`
- JSON reports for CI processing
- Threshold enforcement in CI pipeline

## Best Practices

### Test Organization

- Group related tests in `describe` blocks
- Use descriptive test names
- Include timeout expectations for async operations

### Resource Management

- Track all created resources
- Clean up in `after` blocks
- Use unique identifiers to avoid conflicts

### Error Testing

- Test both success and failure cases
- Verify error messages are informative
- Test edge cases and boundary conditions

### Performance

- Use realistic data sizes
- Test concurrent operations
- Monitor resource usage
- Set appropriate timeouts
