# Building and Bazel

This guide explains how to build the Knowd project using both Go's native build system and Bazel for more complex builds and dependency management.

## Quick Start

### Using Go (Recommended for Development)

```bash
# Build the main binary
go build ./cmd/knowd

# Run tests
go test ./...

# Install dependencies
go mod tidy
go mod download
```

### Using Bazel (Recommended for Production)

```bash
# Build everything
bazel build //...

# Run tests
bazel test //...

# Build specific target
bazel build //cmd/knowd:knowd
```

## Go Build System

### Building the Application

**Standard build:**
```bash
go build ./cmd/knowd
```

**Optimized build:**
```bash
go build -ldflags="-s -w" ./cmd/knowd  # Strip debug info and symbol table
```

**Cross-compilation:**
```bash
# Linux AMD64
GOOS=linux GOARCH=amd64 go build ./cmd/knowd

# Windows
GOOS=windows GOARCH=amd64 go build ./cmd/knowd

# macOS ARM64
GOOS=darwin GOARCH=arm64 go build ./cmd/knowd
```

**Build with version information:**
```bash
go build -ldflags="-X 'github.com/unrdf/knowd/internal/version.Version=v1.0.0'" ./cmd/knowd
```

### Dependency Management

**Update dependencies:**
```bash
go get -u ./...          # Update all dependencies
go get package@version   # Update specific package
go mod tidy             # Clean up unused dependencies
go mod verify           # Verify dependency integrity
```

**Vendor dependencies:**
```bash
go mod vendor           # Create vendor directory
go build -mod=vendor    # Build using vendored dependencies
```

## Bazel Build System

### Bazel Basics

**Install Bazel:**
```bash
# Using Bazelisk (recommended)
curl -L https://github.com/bazelbuild/bazelisk/releases/download/v1.19.0/bazelisk-linux-amd64 > /usr/local/bin/bazel
chmod +x /usr/local/bin/bazel

# Or using package manager
# Ubuntu/Debian
sudo apt install bazel

# macOS
brew install bazel
```

**Verify installation:**
```bash
bazel version
```

### Bazel Configuration

**WORKSPACE file:**
```python
# MODULE.bazel (Bazel 6+)
module(
    name = "knowd",
    version = "1.0.0",
)

bazel_dep(name = "rules_go", version = "0.41.0")
bazel_dep(name = "gazelle", version = "0.32.0")
```

**BUILD.bazel files:**
```python
# Root BUILD.bazel
load("@rules_knowd//knowd:defs.bzl", "knowd_binary")

knowd_binary(
    name = "knowd",
    srcs = ["cmd/knowd/main.go"],
    visibility = ["//visibility:public"],
)
```

### Bazel Commands

**Build targets:**
```bash
# Build everything
bazel build //...

# Build specific package
bazel build //internal/store/...

# Build main binary
bazel build //cmd/knowd:knowd

# Build with optimizations
bazel build --compilation_mode=opt //cmd/knowd:knowd
```

**Run tests:**
```bash
# Run all tests
bazel test //...

# Run specific tests
bazel test //internal/sparql:sparql_test

# Run with coverage
bazel coverage //...

# Run performance tests
bazel test --test_arg=-test.bench=. //internal/sparql:sparql_test
```

**Query dependencies:**
```bash
# Show dependencies of a target
bazel query "deps(//cmd/knowd:knowd)"

# Show reverse dependencies
bazel query "rdeps(//..., //internal/store:store)"

# Show test targets
bazel query "kind(test, //...)"
```

## Build Targets

### Main Application

**`//cmd/knowd:knowd`** - The main Knowd binary
```bash
bazel build //cmd/knowd:knowd
./bazel-bin/cmd/knowd/knowd --help
```

**`//cmd/knowd:knowd_test`** - Tests for the main application
```bash
bazel test //cmd/knowd:knowd_test
```

### Libraries and Components

**Core libraries:**
- `//internal/engine:engine` - Transaction engine
- `//internal/store:store` - Storage abstraction
- `//internal/sparql:sparql` - SPARQL query engine
- `//internal/hooks:hooks` - Hook system

**Build a library:**
```bash
bazel build //internal/sparql:sparql
```

### Tests and Benchmarks

**All tests:**
```bash
bazel test //...
```

**Specific test packages:**
```bash
bazel test //internal/sparql:sparql_test
bazel test //internal/hooks:hooks_test
bazel test //internal/store:store_test
```

**Benchmarks:**
```bash
bazel test --test_arg=-test.bench=. //internal/sparql:sparql_test
```

## Advanced Build Options

### Compilation Modes

**Debug build:**
```bash
bazel build --compilation_mode=dbg //cmd/knowd:knowd
```

**Optimized build:**
```bash
bazel build --compilation_mode=opt //cmd/knowd:knowd
```

**Fast build:**
```bash
bazel build --compilation_mode=fastbuild //cmd/knowd:knowd
```

### Platform Selection

**Linux:**
```bash
bazel build --platforms=@io_bazel_rules_go//go/toolchain:linux_amd64 //cmd/knowd:knowd
```

**macOS:**
```bash
bazel build --platforms=@io_bazel_rules_go//go/toolchain:darwin_amd64 //cmd/knowd:knowd
```

**Windows:**
```bash
bazel build --platforms=@io_bazel_rules_go//go/toolchain:windows_amd64 //cmd/knowd:knowd
```

### Custom Configurations

**Create `.bazelrc`:**
```ini
# Development configuration
build --compilation_mode=dbg
build --strip=never

# Release configuration
build:release --compilation_mode=opt
build:release --strip=always

# Test configuration
test --test_output=summary
test --test_timeout=300
```

**Use configurations:**
```bash
bazel build --config=release //cmd/knowd:knowd
bazel test --config=test //...
```

## Continuous Integration

### GitHub Actions Example

```yaml
name: CI
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
    - name: Build
      run: bazel build //...
    - name: Test
      run: bazel test //...
    - name: Upload coverage
      uses: codecov/codecov-action@v3
```

### Build Caching

**Local cache:**
```bash
bazel build --disk_cache=/tmp/bazel-cache //cmd/knowd:knowd
```

**Remote cache (Bazel Remote Execution):**
```bash
bazel build --remote_cache=http://cache.example.com:8080 //cmd/knowd:knowd
```

## Troubleshooting

### Common Build Issues

**"Module not found" errors:**
```bash
# Clean build cache
bazel clean --expunge

# Update dependencies
go mod tidy
bazel run //:gazelle
```

**Import path issues:**
```bash
# Check import paths
bazel query "deps(//cmd/knowd:knowd)"

# Fix import statements in BUILD files
bazel run //:gazelle -- update -go_prefix=github.com/unrdf/knowd
```

**Test failures:**
```bash
# Run tests with more verbose output
bazel test --test_output=errors //internal/sparql:sparql_test

# Debug failing test
bazel test --test_arg=-test.v //internal/sparql:sparql_test
```

### Performance Optimization

**Parallel builds:**
```bash
bazel build --jobs=8 //cmd/knowd:knowd
```

**Build without bytes:**
```bash
bazel build --nobuild_runfile_links //cmd/knowd:knowd
```

**Incremental builds:**
```bash
# Only rebuild changed files
bazel build //cmd/knowd:knowd
```

## Development Workflow

### 1. Code Changes

```bash
# Make changes to source files
vim internal/sparql/parser.go

# Run tests for affected package
bazel test //internal/sparql:sparql_test

# Build to verify compilation
bazel build //cmd/knowd:knowd
```

### 2. Adding Dependencies

**Go modules:**
```bash
go get github.com/new/dependency@v1.0.0
go mod tidy
```

**Bazel (update BUILD files):**
```bash
bazel run //:gazelle -- update
```

### 3. Release Builds

**Create release binary:**
```bash
bazel build --config=release //cmd/knowd:knowd

# Copy to release directory
cp bazel-bin/cmd/knowd/knowd ./releases/knowd-v1.0.0-linux-amd64
```

**Docker build:**
```dockerfile
FROM golang:1.22-alpine AS builder
WORKDIR /app
COPY . .
RUN go build -o knowd ./cmd/knowd

FROM alpine:latest
COPY --from=builder /app/knowd /usr/local/bin/knowd
CMD ["knowd"]
```

## Build Artifacts

### Generated Files

**Bazel generates:**
- `bazel-bin/` - Build outputs
- `bazel-out/` - Intermediate build files
- `bazel-testlogs/` - Test logs and outputs

**Go generates:**
- `go.sum` - Dependency checksums
- `vendor/` - Vendored dependencies (optional)

### Cleanup

**Clean build artifacts:**
```bash
# Clean Go build cache
go clean -cache

# Clean Bazel cache
bazel clean --expunge

# Clean all generated files
rm -rf bazel-* vendor/
```

This build system provides flexibility for both development (Go) and production (Bazel) workflows while maintaining consistency and reliability across different environments.
