# Code Review Checklist

## Magic Numbers

- [ ] **No magic numbers for timeouts, delays, or intervals**
  - Use named constants with `_MS` suffix (e.g., `EXECUTION_TIMEOUT_MS`)
  - Constants defined at top of file with comments
  - See [Coding Patterns](./coding-patterns.md#named-constants-pattern) for details

- [ ] **No magic numbers for exit codes or status values**
  - Use named constants with `_CODE` suffix (e.g., `SUCCESS_EXIT_CODE`)
  - Constants grouped by category

- [ ] **No magic numbers for buffer sizes or retry counts**
  - Use named constants with descriptive suffixes
  - Constants have clear, self-documenting names

## Code Quality

- [ ] **Poka-Yoke design principles followed**
  - State machine prevents invalid operations
  - Validation prevents invalid inputs
  - Type guards ensure state consistency
  - See [Poka-Yoke Documentation](./poka-yoke-invariants.md) for details

- [ ] **Error handling is appropriate**
  - Errors are caught and handled gracefully
  - Error messages are clear and actionable
  - State transitions on error are correct

- [ ] **Code is self-documenting**
  - Named constants explain values
  - Function names describe behavior
  - Comments explain "why", not "what"

## Testing

- [ ] **Tests cover new functionality**
  - Unit tests for new functions
  - Integration tests for new features
  - Edge cases are tested

- [ ] **Tests pass**
  - All existing tests still pass
  - New tests are added for new code
  - No test failures or warnings

## Documentation

- [ ] **JSDoc comments are complete**
  - All public functions have JSDoc
  - Parameters and return types documented
  - Examples provided where helpful

- [ ] **README updated if needed**
  - New features documented
  - Breaking changes noted
  - Examples updated

## Performance

- [ ] **No unnecessary allocations**
  - Reuse objects where possible
  - Avoid creating objects in hot paths
  - Use references instead of clones

- [ ] **Timeouts are appropriate**
  - Not too short (causes false failures)
  - Not too long (causes slow feedback)
  - Named constants make values clear

## Consistency

- [ ] **Follows existing patterns**
  - Matches code style in file
  - Uses same error handling approach
  - Consistent naming conventions

- [ ] **No code duplication**
  - Extract repeated patterns to functions
  - Reuse existing utilities
  - DRY principle followed

## Security

- [ ] **Input validation**
  - All inputs validated
  - No trust of external data
  - Sanitization where needed

- [ ] **No security vulnerabilities**
  - No XSS risks
  - No injection vulnerabilities
  - Proper error handling (no information leakage)

