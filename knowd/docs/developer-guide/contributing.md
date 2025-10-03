# Contributing to Knowd

We welcome contributions to Knowd! This guide explains how to contribute effectively to the project.

## Ways to Contribute

### 1. Code Contributions
- **Bug fixes** - Fix existing issues
- **Features** - Implement new functionality
- **Performance improvements** - Optimize existing code
- **Documentation** - Improve documentation and examples

### 2. Non-Code Contributions
- **Issue reporting** - Report bugs and feature requests
- **Documentation** - Improve existing docs or write new ones
- **Testing** - Help expand test coverage
- **Community support** - Help other users on discussions

## Development Workflow

### 1. Fork and Clone

```bash
# Fork the repository on GitHub
# Then clone your fork
git clone https://github.com/your-username/knowd.git
cd knowd

# Add upstream remote
git remote add upstream https://github.com/unrdf/knowd.git
```

### 2. Set Up Development Environment

**Install dependencies:**
```bash
# Install Bazel (if not already installed)
# See installation guide for platform-specific instructions

# Install Go dependencies
go mod download

# Install development tools
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
go install golang.org/x/tools/cmd/goimports@latest
```

### 3. Create a Feature Branch

```bash
# Create a branch for your feature
git checkout -b feature/amazing-feature

# Or for bug fixes
git checkout -b fix/bug-description
```

### 4. Make Changes

**Follow coding standards:**
- Use `gofmt` for consistent formatting
- Run `goimports` to manage imports
- Follow the existing code style
- Add tests for new functionality
- Update documentation

**Code review checklist:**
- [ ] Code compiles without errors
- [ ] Tests pass (`bazel test //...`)
- [ ] Linting passes (`golangci-lint run`)
- [ ] Documentation updated
- [ ] CHANGELOG.md updated for user-facing changes

### 5. Test Your Changes

```bash
# Run all tests
bazel test //...

# Run specific tests
bazel test //internal/engine:engine_test

# Run integration tests
go test -run TestIntegration

# Check for race conditions
go test -race ./...

# Benchmark performance
go test -bench=. ./internal/store
```

### 6. Commit and Push

```bash
# Stage your changes
git add .

# Commit with descriptive message
git commit -m "feat: add amazing new feature

- Add new endpoint for data export
- Include comprehensive tests
- Update API documentation
- Closes #123"

# Push to your fork
git push origin feature/amazing-feature
```

### 7. Create a Pull Request

1. Go to GitHub and create a pull request
2. Fill out the PR template
3. Request review from maintainers
4. Address any feedback
5. Once approved, the PR will be merged

## Code Standards

### Go Code Style

**Imports:**
```go
import (
    "context"
    "fmt"

    "github.com/unrdf/knowd/internal/engine"
    "github.com/unrdf/knowd/internal/store"
)
```

**Error handling:**
```go
// Good - wrap errors with context
if err != nil {
    return fmt.Errorf("failed to process query: %w", err)
}

// Avoid - generic errors
if err != nil {
    return err
}
```

**Variable naming:**
```go
// Use descriptive names
func (e *Engine) executeComplexQuery(ctx context.Context, query *Query) (*Result, error) {
    // Implementation
}

// Avoid abbreviations
func (e *Engine) execQry(ctx context.Context, q *Q) (*R, error) {
    // Implementation
}
```

### Documentation

**Package documentation:**
```go
// Package engine provides core orchestration for knowd.
// It wraps store, runs queries, executes hooks, and coordinates responses.
//
// Example usage:
//   config := engine.Config{...}
//   eng, err := engine.New(config)
//   if err != nil {
//       return err
//   }
package engine
```

**Function documentation:**
```go
// New creates a new engine instance with the provided configuration.
// It initializes all subsystems including storage, query engine, and hooks.
//
// The config parameter must include valid store and hook configurations.
// Returns an error if any subsystem fails to initialize.
func New(config Config) (*Engine, error) {
    // Implementation
}
```

### Testing

**Test structure:**
```go
func TestEngine_New(t *testing.T) {
    config := Config{
        Store: store.Config{MaxQuads: 1000},
        Cache: sparql.CacheConfig{Capacity: 100},
    }

    engine, err := New(config)
    if err != nil {
        t.Fatalf("New() error = %v", err)
    }

    if engine == nil {
        t.Error("New() returned nil engine")
    }
}
```

**Table-driven tests:**
```go
func TestEngine_Query(t *testing.T) {
    tests := []struct {
        name    string
        query   string
        wantErr bool
    }{
        {"valid query", "SELECT * WHERE { ?s ?p ?o }", false},
        {"invalid query", "INVALID SYNTAX", true},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            // Test implementation
        })
    }
}
```

## Commit Message Guidelines

Follow [Conventional Commits](https://conventionalcommits.org/) format:

```
type(scope): description

[optional body]

[optional footer]
```

**Types:**
- `feat` - New feature
- `fix` - Bug fix
- `docs` - Documentation changes
- `style` - Code style changes (formatting, etc.)
- `refactor` - Code changes that neither fix bugs nor add features
- `test` - Adding or modifying tests
- `chore` - Maintenance tasks

**Examples:**
```bash
feat: add vector search support

Add HNSW-based vector similarity search for semantic queries.
Includes configuration options and API endpoints.

Closes #456

fix: resolve memory leak in query executor

The query executor was not properly releasing resources,
causing memory usage to grow over time.

docs: update API reference with new endpoints

Add documentation for recently added clustering endpoints
and update examples.

test: add integration tests for hooks system

Add comprehensive tests for hook registration, evaluation,
and error handling scenarios.
```

## Pull Request Process

### 1. PR Title and Description

**Title:** Brief, descriptive title following conventional commits

**Description:** Include:
- **What** - What changes are being made
- **Why** - Why these changes are needed
- **How** - How the changes work
- **Testing** - How changes were tested

### 2. Code Review

**What reviewers look for:**
- Code correctness and functionality
- Test coverage for new features
- Performance implications
- Security considerations
- Documentation completeness

**Responding to feedback:**
- Address all reviewer comments
- Explain design decisions
- Update code as requested
- Re-run tests after changes

### 3. Merging

**Before merge:**
- All CI checks pass
- At least one approval from maintainer
- No merge conflicts
- CHANGELOG.md updated for user-facing changes

## Development Tools

### Required Tools

**Core tools:**
- **Bazel** - Build system
- **Go 1.22+** - Programming language
- **Git** - Version control

**Development tools:**
- **golangci-lint** - Go linting
- **goimports** - Import formatting
- **gofmt** - Code formatting

**Testing tools:**
- **go test** - Unit testing
- **go test -race** - Race condition detection
- **go test -bench** - Performance benchmarking

### Optional Tools

**Performance profiling:**
- **pprof** - CPU and memory profiling
- **benchcmp** - Benchmark comparison

**Code quality:**
- **gosec** - Security scanning
- **ineffassign** - Inefficient assignment detection
- **staticcheck** - Static analysis

## Local Development Setup

### 1. Environment Setup

**Create development environment:**
```bash
# Create development directory
mkdir -p ~/dev/knowd
cd ~/dev/knowd

# Clone repository
git clone https://github.com/unrdf/knowd.git
cd knowd

# Set up Git hooks (optional)
ln -s ../../scripts/pre-commit .git/hooks/pre-commit
```

### 2. IDE Configuration

**VS Code:**
```json
{
    "go.toolsManagement.checkForUpdates": "local",
    "go.useLanguageServer": true,
    "go.formatTool": "gofmt",
    "go.lintTool": "golangci-lint",
    "go.testFlags": ["-v"],
    "go.buildFlags": ["-tags=debug"],
    "go.testTimeout": "30s"
}
```

**GoLand/IntelliJ:**
- Enable Go modules support
- Configure GOPATH appropriately
- Enable code inspections

### 3. Running Tests

```bash
# Run all tests
bazel test //...

# Run specific package tests
bazel test //internal/engine:engine_test

# Run with verbose output
bazel test //... --test_output=summary

# Run integration tests
go test -run TestIntegration

# Run benchmarks
go test -bench=. //internal/store

# Run with race detection
go test -race ./internal/...
```

### 4. Debugging

**Enable debug logging:**
```bash
KNOWD_LOG_LEVEL=debug ./knowd
```

**Profile performance:**
```bash
# Enable pprof
KNOWD_PPROF_ADDR=:6060 ./knowd

# Access profiling data
go tool pprof http://localhost:6060/debug/pprof/profile
```

## Best Practices

### 1. Code Organization

**Keep packages focused:**
- One responsibility per package
- Clear package boundaries
- Minimal public APIs

**Import organization:**
```go
import (
    // Standard library
    "context"
    "fmt"

    // External dependencies (alphabetical)
    "github.com/google/uuid"
    "golang.org/x/crypto/sha3"

    // Internal packages (alphabetical)
    "github.com/unrdf/knowd/internal/engine"
    "github.com/unrdf/knowd/internal/store"
)
```

### 2. Error Handling

**Use error wrapping:**
```go
if err != nil {
    return fmt.Errorf("failed to process query %s: %w", queryID, err)
}
```

**Handle context cancellation:**
```go
select {
case <-ctx.Done():
    return ctx.Err()
default:
    // Continue processing
}
```

### 3. Testing

**Write comprehensive tests:**
- Unit tests for individual functions
- Integration tests for component interaction
- End-to-end tests for complete workflows

**Test error conditions:**
```go
func TestEngine_Query_InvalidSyntax(t *testing.T) {
    engine, _ := New(defaultConfig)

    _, err := engine.Query(context.Background(), QueryRequest{
        Query: "INVALID SPARQL SYNTAX",
        Kind:  "sparql-select",
    })

    if err == nil {
        t.Error("Expected error for invalid syntax")
    }
}
```

### 4. Performance

**Profile before optimizing:**
```go
// Use benchmarking for performance testing
func BenchmarkEngine_Query(b *testing.B) {
    engine, _ := New(defaultConfig)

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        engine.Query(context.Background(), testQuery)
    }
}
```

**Consider memory allocation:**
- Use sync.Pool for frequent allocations
- Avoid unnecessary slice growth
- Profile memory usage with `-memprofile`

## Getting Help

### Asking Questions

**GitHub Discussions:**
- Use for questions, ideas, and general discussion
- Tag appropriately (question, idea, help wanted)

**Issues:**
- Use for bug reports and feature requests
- Provide detailed reproduction steps
- Include relevant logs and error messages

### Reporting Bugs

**Include in bug reports:**
- Steps to reproduce
- Expected vs actual behavior
- Environment details (OS, Go version, etc.)
- Relevant logs and error messages
- Minimal reproduction case

### Feature Requests

**Include in feature requests:**
- Problem description
- Proposed solution
- Use cases and benefits
- Implementation considerations

## Recognition

Contributors are recognized through:
- **GitHub contributor graph**
- **CHANGELOG.md** mentions for significant contributions
- **Release notes** for major features
- **Community shoutouts** in discussions

Thank you for contributing to Knowd! 🚀
