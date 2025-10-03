# Testing Guide

This guide covers the testing strategies, frameworks, and best practices used in the Knowd project.

## Testing Philosophy

Knowd follows a comprehensive testing approach with multiple layers:

1. **Unit Tests** - Test individual functions and methods
2. **Integration Tests** - Test component interactions
3. **End-to-End Tests** - Test complete workflows
4. **Performance Tests** - Validate performance characteristics
5. **Chaos Tests** - Test resilience under failure conditions

## Test Structure

### Test Organization

```
internal/
├── store/
│   ├── memory_test.go          # Unit tests for memory store
│   └── disk/                   # Disk store tests
├── sparql/
│   ├── parser_test.go          # SPARQL parser tests
│   ├── exec_test.go            # Query execution tests
│   └── plan_cache_test.go      # Plan cache tests
├── hooks/
│   └── hooks_test.go           # Hooks system tests
└── engine/
    └── engine_test.go          # Engine integration tests

integration_test.go             # End-to-end tests
```

### Test Naming Conventions

**Unit tests:**
```go
func TestComponent_Function_Scenario(t *testing.T)
func TestStore_AddQuad_ValidInput(t *testing.T)
func TestParser_Parse_InvalidSyntax(t *testing.T)
```

**Integration tests:**
```go
func TestIntegration_Feature_Workflow(t *testing.T)
func TestIntegration_QueryAndTransaction(t *testing.T)
```

**Benchmark tests:**
```go
func BenchmarkComponent_Function(b *testing.B)
func BenchmarkStore_AddQuad(b *testing.B)
```

## Running Tests

### All Tests

```bash
# Run all tests
bazel test //...

# Run with verbose output
bazel test //... --test_output=summary

# Run with race detection
go test -race ./...

# Run with coverage
go test -cover ./...
```

### End-to-End Tests

**Run complete end-to-end test suite:**

```bash
# Run e2e tests specifically
go test -run TestEndToEnd -v

# Run e2e tests with timeout
go test -run TestEndToEnd -timeout 5m -v

# Run e2e tests with detailed logging
go test -run TestEndToEnd -v -args -test.v
```

**Test the complete workflow manually:**

```bash
# 1. Start the server
./knowd -addr :8090 -data-dir ./test-data &

# 2. Wait for server to start
sleep 2

# 3. Submit a transaction
curl -X POST http://localhost:8090/v1/tx \
  -H "Content-Type: application/json" \
  -d '{
    "delta": {
      "add": [
        {
          "subject": "<http://example.org/test>",
          "predicate": "<http://example.org/predicate>",
          "object": "\"test-value\"",
          "graph": "default"
        }
      ]
    },
    "actor": "test@example.com"
  }'

# 4. Query the data
curl -X POST http://localhost:8090/v1/query \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
    "kind": "sparql-select"
  }'

# 5. Verify receipt
RECEIPT_ID=$(curl -s http://localhost:8090/v1/tx \
  -H "Content-Type: application/json" \
  -d '{"delta": {"add": []}, "actor": "test"}' | jq -r '.receiptId')

curl http://localhost:8090/v1/receipts/$RECEIPT_ID/verify

# 6. Test SHACL validation
curl -X POST http://localhost:8090/v1/validate \
  -H "Content-Type: application/json" \
  -d '{
    "data": "@prefix ex: <http://example.org/> . ex:test a ex:Type .",
    "shapes": "@prefix sh: <http://www.w3.org/ns/shacl#> . ex:TypeShape a sh:NodeShape ."
  }'

# 7. Test vector search (if enabled)
curl -X POST http://localhost:8090/v1/similar \
  -H "Content-Type: application/json" \
  -d '{"text": "test query", "topK": 5}'

# 8. Stop the server
pkill knowd
```

### Specific Test Packages

```bash
# Test storage layer
bazel test //internal/store:store_test

# Test SPARQL engine
bazel test //internal/sparql:sparql_test

# Test hooks system
bazel test //internal/hooks:hooks_test

# Test integration scenarios
go test -run TestIntegration
```

### Test Filtering

```bash
# Run tests matching pattern
go test -run TestEngine

# Run specific test
go test -run TestEngine_Query

# Skip integration tests
go test -short ./...
```

## Test Categories

### 1. Unit Tests

**Individual component testing:**

```go
func TestStore_AddQuad(t *testing.T) {
    store, _ := NewMemoryStore(Config{MaxQuads: 1000})

    quad := Quad{
        Subject:   "<http://example.org/s1>",
        Predicate: "<http://example.org/p1>",
        Object:    "<http://example.org/o1>",
        Graph:     "default",
    }

    err := store.AddQuad(context.Background(), quad)
    if err != nil {
        t.Errorf("AddQuad() error = %v", err)
    }

    count := store.GetQuadCount()
    if count != 1 {
        t.Errorf("GetQuadCount() = %d, want 1", count)
    }
}
```

**Mock dependencies:**
```go
func TestEngine_Query_WithMockStore(t *testing.T) {
    // Create mock store
    mockStore := &mockStore{
        quads: []Quad{testQuad1, testQuad2},
    }

    engine := &Engine{
        store: mockStore,
        // ... other dependencies
    }

    result, err := engine.Query(context.Background(), testQuery)
    // Assert results
}
```

### 2. Integration Tests

**Component interaction testing:**

```go
func TestIntegration_QueryAndHooks(t *testing.T) {
    // Set up complete system
    config := Config{
        Store: store.Config{MaxQuads: 1000},
        Hooks: hooks.Config{MaxHooks: 10},
    }

    engine, err := New(config)
    if err != nil {
        t.Fatalf("Failed to create engine: %v", err)
    }

    // Add test data
    tx := TxRequest{
        Actor: "test-actor",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{testQuad1, testQuad2},
        },
    }

    _, err = engine.Tx(context.Background(), tx)
    if err != nil {
        t.Fatalf("Transaction failed: %v", err)
    }

    // Query the data
    result, err := engine.Query(context.Background(), testQuery)
    if err != nil {
        t.Fatalf("Query failed: %v", err)
    }

    // Verify hooks were triggered
    if len(result.Rows) != 2 {
        t.Errorf("Expected 2 results, got %d", len(result.Rows))
    }
}
```

### 3. End-to-End Tests

**Complete workflow testing:**

```go
func TestEndToEnd_TransactionAndReceipt(t *testing.T) {
    // Start server
    server := New(":8090", "./test-data", "native://")
    go server.Start()
    defer server.Close()

    // Wait for server to start
    time.Sleep(100 * time.Millisecond)

    // Submit transaction
    txData := TxRequest{
        Actor: "test-user",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{testQuad},
        },
    }

    resp, err := http.Post("http://localhost:8090/v1/tx",
        "application/json", bytes.NewBuffer(jsonData))
    if err != nil {
        t.Fatalf("Transaction request failed: %v", err)
    }

    var result TxResult
    json.NewDecoder(resp.Body).Decode(&result)

    // Verify receipt
    receipt, err := http.Get(fmt.Sprintf("http://localhost:8090/v1/receipts/%s",
        result.ReceiptID))
    if err != nil {
        t.Fatalf("Receipt retrieval failed: %v", err)
    }

    // Verify receipt integrity
    verifyResp, err := http.Get(fmt.Sprintf("http://localhost:8090/v1/receipts/%s/verify",
        result.ReceiptID))
    if err != nil {
        t.Fatalf("Receipt verification failed: %v", err)
    }

    var verifyResult map[string]interface{}
    json.NewDecoder(verifyResp.Body).Decode(&verifyResult)

    if !verifyResult["ok"].(bool) {
        t.Error("Receipt verification failed")
    }
}
```

## Performance Testing

### Benchmarks

**Query performance:**
```go
func BenchmarkEngine_Query(b *testing.B) {
    engine, _ := New(defaultConfig)

    // Setup test data
    setupBenchmarkData(engine, b.N)

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        _, err := engine.Query(context.Background(), benchmarkQuery)
        if err != nil {
            b.Fatalf("Query failed: %v", err)
        }
    }
}
```

**Storage performance:**
```go
func BenchmarkStore_AddQuad(b *testing.B) {
    store, _ := NewMemoryStore(Config{MaxQuads: b.N + 1000})

    quad := Quad{
        Subject:   "<http://example.org/s1>",
        Predicate: "<http://example.org/p1>",
        Object:    fmt.Sprintf("\"object-%d\"", b.N),
        Graph:     "default",
    }

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        quad.Object = fmt.Sprintf("\"object-%d\"", i)
        store.AddQuad(context.Background(), quad)
    }
}
```

### Load Testing

**Concurrent load testing:**
```go
func TestLoad_ConcurrentQueries(t *testing.T) {
    engine, _ := New(defaultConfig)
    setupLoadTestData(engine)

    const numGoroutines = 100
    const queriesPerGoroutine = 10

    var wg sync.WaitGroup
    wg.Add(numGoroutines)

    start := time.Now()

    for i := 0; i < numGoroutines; i++ {
        go func(goroutineID int) {
            defer wg.Done()

            for j := 0; j < queriesPerGoroutine; j++ {
                query := QueryRequest{
                    Query: fmt.Sprintf("SELECT ?s WHERE { ?s ?p ?o } LIMIT %d", j+1),
                    Kind:  "sparql-select",
                }

                _, err := engine.Query(context.Background(), query)
                if err != nil {
                    t.Errorf("Goroutine %d, query %d failed: %v", goroutineID, j, err)
                }
            }
        }(i)
    }

    wg.Wait()
    duration := time.Since(start)

    totalQueries := numGoroutines * queriesPerGoroutine
    qps := float64(totalQueries) / duration.Seconds()

    t.Logf("Executed %d queries in %v (%.2f QPS)", totalQueries, duration, qps)

    // Assert minimum performance
    if qps < 100 {
        t.Errorf("Performance too low: %.2f QPS, want >= 100", qps)
    }
}
```

## Test Data Management

### Test Data Setup

**Reusable test data:**
```go
var testQuads = []Quad{
    {
        Subject:   "<http://example.org/alice>",
        Predicate: "<http://xmlns.com/foaf/0.1/name>",
        Object:    "\"Alice\"",
        Graph:     "default",
    },
    {
        Subject:   "<http://example.org/bob>",
        Predicate: "<http://xmlns.com/foaf/0.1/name>",
        Object:    "\"Bob\"",
        Graph:     "default",
    },
}

func setupTestData(store Interface) error {
    for _, quad := range testQuads {
        if err := store.AddQuad(context.Background(), quad); err != nil {
            return err
        }
    }
    return nil
}
```

### Test Cleanup

**Resource cleanup:**
```go
func TestWithCleanup(t *testing.T) {
    // Setup
    tempDir, err := ioutil.TempDir("", "knowd-test")
    if err != nil {
        t.Fatalf("Failed to create temp dir: %v", err)
    }
    defer os.RemoveAll(tempDir) // Cleanup

    config := Config{DataDir: tempDir}
    engine, err := New(config)
    if err != nil {
        t.Fatalf("Failed to create engine: %v", err)
    }
    defer engine.Close() // Cleanup

    // Test implementation
}
```

## Debugging Tests

### Verbose Test Output

```bash
# Run tests with verbose output
go test -v ./internal/store

# Run with detailed failure information
go test -v -args -test.v ./internal/engine
```

### Test Coverage

**Generate coverage reports:**
```bash
# Generate coverage profile
go test -coverprofile=coverage.out ./...

# View coverage report
go tool cover -html=coverage.out

# Coverage by function
go tool cover -func=coverage.out
```

**Coverage targets:**
- **Overall coverage**: > 80%
- **Critical paths**: > 95%
- **New features**: > 90%

## CI/CD Testing

### Automated Testing

**GitHub Actions configuration:**
```yaml
name: Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    - uses: actions/setup-go@v3
      with:
        go-version: '1.22'
    - name: Install Bazel
      run: |
        curl -fsSL https://bazel.build/bazel-release.pub.gpg | gpg --dearmor > bazel.gpg
        sudo mv bazel.gpg /etc/apt/trusted.gpg.d/
        echo "deb [arch=amd64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | sudo tee /etc/apt/sources.list.d/bazel.list
        sudo apt update && sudo apt install bazel
    - name: Run tests
      run: bazel test //...
    - name: Upload coverage
      uses: codecov/codecov-action@v3
```

### Test Environments

**Multiple test environments:**
- **Unit tests**: Minimal dependencies, fast execution
- **Integration tests**: Full component stack
- **Performance tests**: Dedicated hardware, controlled conditions
- **Chaos tests**: Network partitions, resource constraints

## Best Practices

### 1. Test Organization

**Keep tests focused:**
```go
// Good - focused on one behavior
func TestStore_AddQuad_SingleQuad(t *testing.T) {
    // Test single quad addition
}

// Avoid - testing multiple behaviors
func TestStore_AddRemoveQuery(t *testing.T) {
    // Tests addition, removal, and querying
}
```

**Use table-driven tests:**
```go
func TestParser_Parse_QueryTypes(t *testing.T) {
    tests := []struct {
        name     string
        query    string
        wantType string
        wantErr  bool
    }{
        {"SELECT query", "SELECT * WHERE { ?s ?p ?o }", "SELECT", false},
        {"ASK query", "ASK WHERE { ?s ?p ?o }", "ASK", false},
        {"invalid query", "INVALID SYNTAX", "", true},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            // Test implementation
        })
    }
}
```

### 2. Test Data Management

**Use realistic test data:**
```go
// Realistic test data
var realisticQuads = []Quad{
    {
        Subject:   "<https://schema.org/Person/alice-smith>",
        Predicate: "<https://schema.org/name>",
        Object:    "\"Alice Smith\"",
        Graph:     "people",
    },
    {
        Subject:   "<https://schema.org/Person/bob-jones>",
        Predicate: "<https://schema.org/knows>",
        Object:    "<https://schema.org/Person/alice-smith>",
        Graph:     "social",
    },
}
```

**Test edge cases:**
```go
func TestStore_AddQuad_EdgeCases(t *testing.T) {
    store, _ := NewMemoryStore(Config{MaxQuads: 10})

    tests := []struct {
        name string
        quad Quad
        wantErr bool
    }{
        {"empty subject", Quad{Predicate: "p", Object: "o"}, true},
        {"empty predicate", Quad{Subject: "s", Object: "o"}, true},
        {"empty object", Quad{Subject: "s", Predicate: "p"}, true},
        {"valid quad", Quad{Subject: "s", Predicate: "p", Object: "o"}, false},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            err := store.AddQuad(context.Background(), tt.quad)
            if (err != nil) != tt.wantErr {
                t.Errorf("AddQuad() error = %v, wantErr %v", err, tt.wantErr)
            }
        })
    }
}
```

### 3. Performance Testing

**Benchmark critical paths:**
```go
func BenchmarkStore_FindQuads(b *testing.B) {
    store, _ := NewMemoryStore(Config{MaxQuads: 10000})
    setupBenchmarkData(store, 10000)

    pattern := Quad{
        Predicate: "<http://example.org/predicate>",
    }

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        _, err := store.FindQuads(context.Background(), pattern)
        if err != nil {
            b.Fatalf("FindQuads failed: %v", err)
        }
    }
}
```

**Memory profiling:**
```go
func TestMemoryUsage_QueryExecution(t *testing.T) {
    if testing.Short() {
        t.Skip("Skipping memory test in short mode")
    }

    engine, _ := New(defaultConfig)

    var memBefore runtime.MemStats
    runtime.GC()
    runtime.ReadMemStats(&memBefore)

    // Execute many queries
    for i := 0; i < 1000; i++ {
        engine.Query(context.Background(), testQuery)
    }

    runtime.GC()
    var memAfter runtime.MemStats
    runtime.ReadMemStats(&memAfter)

    memUsed := memAfter.Alloc - memBefore.Alloc
    t.Logf("Memory used for 1000 queries: %d bytes", memUsed)

    // Assert reasonable memory usage
    if memUsed > 100*1024*1024 { // 100MB
        t.Errorf("Memory usage too high: %d bytes", memUsed)
    }
}
```

## Debugging Tests

### Common Test Issues

**Flaky tests:**
- Use deterministic test data
- Avoid time-based assertions
- Implement proper cleanup

**Slow tests:**
- Use `t.Parallel()` for independent tests
- Minimize setup/teardown overhead
- Use in-memory storage for fast tests

**Test isolation:**
- Each test should be independent
- Use separate test databases/namespaces
- Clean up resources between tests

### Test Debugging Tools

**Verbose test output:**
```bash
go test -v -run TestEngine_Query
```

**Test with coverage:**
```bash
go test -cover -coverprofile=coverage.out -run TestEngine
go tool cover -html=coverage.out
```

**Benchmark comparison:**
```bash
# Run benchmarks before and after changes
go test -bench=BenchmarkEngine -benchmem ./internal/engine

# Compare results
benchcmp old.txt new.txt
```

## Integration with CI/CD

### Test Requirements

**All PRs must:**
- Pass all unit tests
- Pass integration tests
- Maintain test coverage above 80%
- Include tests for new functionality

**Performance tests:**
- Run on main branch merges
- Alert on performance regressions
- Track performance trends over time

### Test Automation

**Automated testing pipeline:**
1. **Linting** - Code style and static analysis
2. **Unit tests** - Fast feedback for developers
3. **Integration tests** - Component interaction validation
4. **Performance tests** - Regression detection
5. **Security scanning** - Vulnerability detection

This comprehensive testing strategy ensures code quality, performance, and reliability across the entire Knowd codebase.
