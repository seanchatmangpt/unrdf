# knowd

**A lightweight HTTP server for knowledge graph transactions and queries.**

Building on Bazel with Go 1.22+, knowd provides a minimal HTTP surface with REST endpoints for transactions, SPARQL queries, and hook evaluation - all optimized for scalable builds.

## Quick Start

### Prerequisites

- [Bazel 7.0+](https://bazel.build/setup/install)
- Go 1.22+ (Bazel will automatically download via `rules_go`)

### Build & Run

```bash
# Clone and enter the directory
cd knowd

# Build the binary
bazel build //cmd/knowd:knowd

# Run the server
./bazel-bin/cmd/knowd/knowd_/knowd -addr :8090

# Or run directly through Bazel
bazel run //cmd/knowd:knowd -- -addr :8090
```

### Configuration

**Command-line flags:**
- `-addr` (default: `:8090`): HTTP server address
- `-data-dir` (default: `./data`): Data directory path  
- `-core-url` (default: `native://`): Core URL

**Environment variables (override defaults):**
- `KNOWD_ADDR`
- `KNOWD_DATA_DIR` 
- `KNOWD_CORE_URL`

**Example:**
```bash
KNOWD_ADDR=:9090 KNOWD_DATA_DIR=/var/lib/knowd ./bazel-bin/cmd/knowd/knowd_/knowd
```

## API Endpoints

### Health Check
```
GET /healthz
→ 200 ok
```

### Transaction Pipeline
```
POST /v1/tx
Content-Type: application/json

{ "arbitrary": "json payload" }
→ {"arbitrary": "json payload"}  # Echoed request body
```

### SPARQL Queries  
```
POST /v1/query
Content-Type: application/json

{ 
  "query": "SELECT * WHERE { ?s ?p ?o }",
  "kind": "sparql-select"  // sparql-select|sparql-ask|sparql-construct
}
→ {"json": []}  # Static stub response
```

### Hook Evaluation
```
POST /v1/hooks/evaluate
Content-Type: application/json

{ 
  "hook": { "rule": "...", "context": "..." },
  "persist": true
}
→ {"fired": true, "result": null}  # Static stub response
```

### Version Information
```
GET /version
→ Version: 0.1.0
  Commit: abc123...
```

## Bazel Targets

```bash
# Build the binary
bazel build //cmd/knowd:knowd

# Run with flags
bazel run //cmd/knowd:knowd -- -addr :9090 -data-dir /tmp/data

# Build all libraries
bazel build //...

# Run tests (when added)
bazel test //...

# Clean build artifacts
bazel clean
```

## Build Configuration

The project uses Bazel's modern module system (`MODULE.bazel`) with:
- `rules_go` for Go compilation
- `gazelle` for automatic dependency management
- Pure Go compilation (no cgo/external linking)

## Development

### Project Structure
```
knowd/
├── MODULE.bazel              # Bazel module configuration
├── WORKSPACE.bazel           # Workspace definition  
├── .bazelrc                  # Bazel flags and settings
├── go.mod                    # Go module definition
├── BUILD.bazel              # Root build definition
├── cmd/knowd/
│   ├── main.go              # CLI entrypoint with flag parsing
│   └── BUILD.bazel          # Binary build definition
├── internal/
│   ├── server/
│   │   ├── http.go          # HTTP mux and API handlers
│   │   └── BUILD.bazel      # Server library
│   └── version/
│       ├── version.go       # Build version/commit info
│       └── BUILD.bazel      # Version library
└── README.md                # This file
```

### Build Version Injection

The binary injects version information at build time using `-ldflags`:

```bash
# Inject version strings into binary
bazel build //cmd/knowd:knowd \
  --action_env=VERSION=0.1.0 \
  --action_env=COMMIT=$(git rev-parse HEAD)
```

## Status: Prototype (v0.1.0)

This is an initial prototype with:
- ✅ Minimal HTTP API with JSON endpoints
- ✅ Flag/env configuration  
- ✅ Graceful shutdown handling
- ✅ Bazel build system integration
- ✅ Server starts and responds to endpoints
- ⏳ No persistence or storage yet
- ⏳ SPARQL execution stubs
- ⏳ Hook evaluation stubs  
- ⏳ No input validation or error paths

## License

See [LICENSE](LICENSE) file.
