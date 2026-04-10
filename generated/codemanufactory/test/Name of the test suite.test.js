






## Signal Theory

**Signal**: `S=(linguistic, testing, verify, code, unit-test)`
- **Mode**: linguistic
- **Genre**: testing
- **Type**: verify
- **Format**: code
- **Structure**: unit-test

## Usage

```bash
# Generate unit test
unrdf template render \
  --template packages/cli/templates/testing/unit-test.njk \
  --test-name "Calculator" \
  --language "javascript" \
  --functions '[
    {
      "name": "add",
      "description": "adds two numbers",
      "inputs": {"a": 1, "b": 2},
      "assertions": [
        {"field": "result", "operator": "toBe", "expected": 3}
      ]
    }
  ]'

# Run tests
npm test          # JavaScript
pytest            # Python
go test ./...     # Go
cargo test        # Rust
```

## Chicago TDD Principles

All generated tests follow Chicago TDD:

1. **Red**: Write failing test first
2. **Green**: Implement minimal code to pass
3. **Refactor**: Clean code without changing behavior

## Test Characteristics

- **Fast**: Tests run in milliseconds
- **Independent**: No test depends on another
- **Repeatable**: Same result every time
- **Self-Checking**: Clear PASS/FAIL assertions
- **Timely**: Written at same time as implementation

## References

- [Chicago TDD](https://www.jamesshore.com/tdd/)
- [Jest Best Practices](https://jestjs.io/docs/getting-started)
- [Pytest Documentation](https://docs.pytest.org/)
