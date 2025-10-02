# Nuxt Component Tests

## Overview

This directory contains London School TDD tests for the UNRDF Sidecar Nuxt application, focusing on the 20% of components that validate 80% of functionality.

## Test Files

### 1. **components/hooks/HooksList.test.mjs**
Tests the HooksList component with focus on:
- Rendering behavior with various hook data states
- User interaction events (evaluate, delete, edit, toggle)
- Empty state handling
- Error handling for malformed data
- Accessibility features

**Coverage**: 8 test suites, 15+ test cases

### 2. **composables/useKnowledgeHooks.test.mjs**
Tests the core hooks management composable:
- API fetch operations with success and error states
- Hook creation with Zod validation
- Hook update and deletion operations
- Hook evaluation with context
- State management and consistency
- Error recovery patterns

**Coverage**: 8 test suites, 20+ test cases

### 3. **components/runtime/StatusDashboard.test.mjs**
Integration tests for runtime monitoring:
- Status display and formatting
- Auto-refresh polling (5-second intervals)
- Loading and error states
- Manual refresh functionality
- Composable collaboration
- Accessibility features

**Coverage**: 7 test suites, 18+ test cases

## Running Tests

```bash
# Run all Nuxt component tests
pnpm test:nuxt

# Run specific test file
pnpm vitest test/nuxt/components/hooks/HooksList.test.mjs

# Run with coverage
pnpm vitest --coverage test/nuxt/

# Watch mode for development
pnpm vitest --watch test/nuxt/
```

## Test Patterns

### London School TDD Principles

All tests follow the mockist approach:

1. **Mock Dependencies**: All external collaborators are mocked
2. **Behavior Verification**: Focus on interactions, not state
3. **Contract Definition**: Tests define expected interfaces
4. **Event Testing**: Verify component collaborations

### Mock Examples

```javascript
// API mocking
vi.mock('#app', () => ({
  $fetch: vi.fn()
}))

// Composable mocking
vi.mock('~/composables/useRuntime.mjs', () => ({
  useRuntime: vi.fn()
}))
```

### Test Structure

```javascript
describe('Component/Composable Name', () => {
  describe('Behavior Category', () => {
    it('should do specific behavior', async () => {
      // Arrange: Setup mocks and component
      // Act: Trigger behavior
      // Assert: Verify interactions
    })
  })
})
```

## Coverage Goals

- **HooksList**: 90%+ coverage of user interactions
- **useKnowledgeHooks**: 95%+ coverage of API operations
- **StatusDashboard**: 85%+ coverage of integration patterns

## Key Testing Features

- ✅ Vitest framework with @nuxt/test-utils
- ✅ Mock-driven development (London School)
- ✅ Async component testing with `mountSuspended`
- ✅ Event emission verification
- ✅ Zod schema validation testing
- ✅ Error handling and recovery
- ✅ Accessibility testing
- ✅ Polling and timer testing with `vi.useFakeTimers()`

## Next Steps

1. Implement the actual components to match test contracts
2. Run tests to validate implementation
3. Add additional edge case tests as needed
4. Integrate with CI/CD pipeline
5. Monitor coverage reports

## Coordination

Tests follow Claude-Flow coordination protocol:

```bash
# Before testing
npx claude-flow@alpha hooks pre-task --description "Run component tests"

# After testing
npx claude-flow@alpha hooks post-task --task-id "tests-core"
```

## Notes

- All tests use absolute paths as required
- Tests define behavior contracts before implementation
- Mocks establish clear interfaces between components
- Focus on the 20% that validates 80% of functionality
