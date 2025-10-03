# Development Setup

This guide explains how to set up a development environment for contributing to the Knowd project.

## Prerequisites

### Required Software

- **Go 1.22+** - Primary development language
- **Git** - Version control system
- **Make** - Build automation (optional, for convenience scripts)
- **Docker** - For containerized development (optional)
- **Bazel** - Build system (for advanced builds)

### Recommended Tools

- **GoLand** or **VS Code** with Go extension - IDE support
- **gofmt** - Go code formatting (included with Go)
- **golint** or **golangci-lint** - Code quality tools
- **delve** - Go debugger
- **protoc** - Protocol buffer compiler (for gRPC)

## Environment Setup

### 1. Clone the Repository

```bash
git clone https://github.com/unrdf/knowd.git
cd knowd
```

### 2. Install Dependencies

```bash
# Install Go dependencies
go mod download

# Verify installation
go version
go mod tidy
```

### 3. Set Up Environment Variables

Create a `.env` file or export environment variables:

```bash
# Basic configuration
export KNOWD_ADDR=:8090
export KNOWD_DATA_DIR=./data
export KNOWD_STORE=disk

# Development settings
export KNOWD_LOG_LEVEL=debug
export KNOWD_OTEL_EXPORTER=stdout
```

### 4. Initialize Submodules (if any)

```bash
git submodule update --init --recursive
```

## Development Workflow

### Building the Project

```bash
# Build the main binary
go build ./cmd/knowd

# Build with optimizations
go build -ldflags="-s -w" ./cmd/knowd

# Cross-compile for different platforms
GOOS=linux GOARCH=amd64 go build ./cmd/knowd
```

### Running Tests

```bash
# Run all tests
go test ./...

# Run tests with verbose output
go test -v ./...

# Run specific package tests
go test ./internal/sparql/...

# Run tests with race detection
go test -race ./...

# Run benchmarks
go test -bench=. ./...
```

### Code Quality Checks

```bash
# Format code
gofmt -w .

# Check formatting
gofmt -l .

# Run linter (if golangci-lint is installed)
golangci-lint run

# Check for unused dependencies
go mod tidy
go mod verify
```

## Development Tools

### Debugging

**Using Delve:**
```bash
# Start debugger
dlv debug ./cmd/knowd -- -addr=:8090

# Set breakpoints and step through code
```

**Using IDE Debuggers:**
- GoLand: Built-in debugger support
- VS Code: Install Go extension for debugging

### Profiling

**CPU Profiling:**
```bash
go test -cpuprofile=cpu.prof ./...
go tool pprof cpu.prof
```

**Memory Profiling:**
```bash
go test -memprofile=mem.prof ./...
go tool pprof mem.prof
```

### Tracing

**Built-in pprof endpoints:**
```bash
# Enable profiling endpoints
KNOWD_PPROF_ADDR=:6060 ./knowd

# Access profiles
curl http://localhost:6060/debug/pprof/profile?seconds=30 > cpu.prof
curl http://localhost:6060/debug/pprof/heap > heap.prof
```

## Project Structure

```
knowd/
├── cmd/knowd/           # Main application entrypoint
├── internal/            # Private application code
│   ├── auth/           # Authentication & authorization
│   ├── cluster/        # Clustering and replication
│   ├── engine/         # Core transaction engine
│   ├── hooks/          # Knowledge hooks system
│   ├── lockchain/      # Cryptographic provenance
│   ├── namespace/      # Multi-tenancy support
│   ├── policy/         # Policy pack management
│   ├── server/         # HTTP/gRPC servers
│   ├── shacl/          # SHACL validation engine
│   ├── sparql/         # SPARQL query engine
│   ├── store/          # RDF storage backends
│   ├── telemetry/      # Observability and metrics
│   ├── vec/           # Vector similarity search
│   └── wasm/          # WASM execution runtime
├── api/               # API definitions (OpenAPI, protobuf)
├── docs/              # Documentation
├── testdata/          # Test data files
└── rules_knowd/       # Bazel build rules
```

## Key Development Areas

### Adding New Features

1. **Identify the appropriate package** in `internal/`
2. **Follow existing patterns** for similar functionality
3. **Add comprehensive tests** with the new feature
4. **Update documentation** in the appropriate `docs/` section
5. **Consider backward compatibility** for API changes

### Working with SPARQL

The SPARQL engine supports advanced query constructs:

```go
// Example: Adding a new SPARQL operator
func (a *Algebra) executeCustomOp(ctx context.Context, executor *Executor, store store.Interface) (*QueryResponse, error) {
    // Implementation here
}
```

### Database Schema Changes

When modifying the RDF store schema:

1. **Update type definitions** in `internal/store/api.go`
2. **Modify serialization logic** in `internal/store/disk/codec.go`
3. **Add migration logic** for backward compatibility
4. **Update tests** to cover new schema features

## Testing Guidelines

### Unit Tests

- **Test individual functions** and methods
- **Use table-driven tests** for multiple scenarios
- **Mock external dependencies** where appropriate
- **Aim for 80%+ code coverage**

### Integration Tests

- **Test component interactions** across package boundaries
- **Use real dependencies** when possible
- **Include performance tests** for critical paths
- **Test error conditions** and edge cases

### End-to-End Tests

- **Test complete workflows** from API to storage
- **Include realistic data** and query patterns
- **Test clustering scenarios** when applicable
- **Validate performance requirements**

## Performance Optimization

### Profiling Workflow

1. **Establish baselines** with existing benchmarks
2. **Identify bottlenecks** using pprof and tracing
3. **Implement optimizations** with measurable improvements
4. **Add regression tests** to prevent performance degradation

### Common Optimization Areas

- **Query plan caching** - Reduce parsing overhead
- **Batch processing** - Group operations for efficiency
- **Memory pooling** - Reuse buffers and objects
- **Parallel execution** - Utilize multiple CPU cores

## Debugging Common Issues

### Build Issues

```bash
# Check for missing dependencies
go mod tidy

# Verify Go version compatibility
go version

# Check for import errors
go build ./...
```

### Runtime Issues

```bash
# Enable debug logging
KNOWD_LOG_LEVEL=debug ./knowd

# Check resource usage
top -p $(pgrep knowd)

# Monitor file descriptors
lsof -p $(pgrep knowd) | wc -l
```

### Test Issues

```bash
# Run tests with verbose output
go test -v -run TestSpecificFunction ./internal/package/

# Check test coverage
go test -cover ./...

# Debug failing tests
go test -v -run TestFailingFunction ./internal/package/ -args -test.v
```

## Contributing Workflow

### 1. Create Feature Branch

```bash
git checkout -b feature/new-sparql-operator
```

### 2. Implement Changes

- Follow the existing code style and patterns
- Add tests for new functionality
- Update documentation as needed
- Ensure all tests pass

### 3. Submit Pull Request

- Provide clear description of changes
- Reference related issues/tickets
- Include performance impact assessment
- Update CHANGELOG.md for significant changes

### 4. Code Review

- Address reviewer feedback
- Ensure CI/CD checks pass
- Update documentation based on review

## Development Best Practices

### Code Style

- **Follow Go conventions** - Use `gofmt` and `goimports`
- **Write clear comments** - Document public APIs and complex logic
- **Use descriptive names** - Variables and functions should be self-documenting
- **Keep functions focused** - Single responsibility principle

### Error Handling

- **Return errors, don't panic** - Allow callers to handle errors appropriately
- **Use error wrapping** - Provide context with `fmt.Errorf` and `%w`
- **Log errors appropriately** - Use structured logging with context
- **Handle edge cases** - Consider nil pointers, empty slices, etc.

### Testing

- **Test error conditions** - Ensure graceful failure handling
- **Use realistic test data** - Avoid artificial test scenarios
- **Test concurrency** - Use goroutines and race detection
- **Benchmark critical paths** - Identify performance regressions

### Performance

- **Profile before optimizing** - Measure actual bottlenecks
- **Use appropriate data structures** - Choose based on access patterns
- **Minimize allocations** - Reuse objects where possible
- **Consider memory layout** - Cache-friendly data structures

## Getting Help

### Documentation
- **API Reference**: `docs/user-guide/api-reference.md`
- **Architecture**: `docs/architecture/`
- **Troubleshooting**: `docs/troubleshooting/`

### Community
- **GitHub Issues**: Report bugs and request features
- **GitHub Discussions**: Ask questions and share ideas
- **Code Reviews**: Participate in the development process

### Development Tools
- **Makefile**: Build automation and common tasks
- **Docker**: Containerized development environment
- **CI/CD**: Automated testing and deployment

This setup guide provides everything needed to start developing on the Knowd project effectively and contribute high-quality code that follows the project's standards and practices.
