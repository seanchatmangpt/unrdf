






## Signal Theory

**Signal**: `S=(linguistic, testing, verify, code, integration-test)`
- **Mode**: linguistic
- **Genre**: testing
- **Type**: verify
- **Format**: code
- **Structure**: integration-test

## Usage

```bash
# Generate integration test
unrdf template render \
  --template packages/cli/templates/testing/integration-test.njk \
  --test-name "UserAPI" \
  --system-under-test "UserAPI" \
  --dependencies '["postgresql", "redis"]' \
  --language "javascript"

# Run integration tests
npm run test:integration
pytest tests/integration/

# With Docker Compose
docker-compose up -d test-db
npm run test:integration
docker-compose down
```

## Integration Test Guidelines

1. **Use Real Dependencies**: Connect to actual services
2. **Isolate Tests**: Each test should be independent
3. **Cleanup**: Always clean up after tests
4. **Timeouts**: Set reasonable timeouts for external calls
5. **Fixtures**: Use fixtures for consistent test data
6. **Error Handling**: Test both success and failure cases

## Test Dependencies

### PostgreSQL
```bash
--dependencies '["postgresql"]' \
--setup-code 'await db.connect("postgres://localhost:5432/test")'
```

### Redis
```bash
--dependencies '["redis"]' \
--setup-code 'await redis.connect("redis://localhost:6379")'
```

### External APIs
```bash
--dependencies '["external-api"]' \
--setup-code 'apiClient = new ApiClient("https://api.example.com")'
```

## References

- [Integration Testing Best Practices](https://martinfowler.com/bliki/IntegrationTest.html)
- [Jest Integration Tests](https://jestjs.io/docs/configuration)
- [Pytest Fixtures](https://docs.pytest.org/en/fixture.html)
