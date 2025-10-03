# Quick Start

Get Knowd up and running in just a few minutes.

## Prerequisites

- **Bazel 7.0+** - [Install Bazel](https://bazel.build/install)
- **Go 1.22+** - Bazel will automatically download via `rules_go`
- **Git** - For cloning the repository

## 1. Clone and Build

```bash
# Clone the repository
git clone https://github.com/unrdf/knowd.git
cd knowd

# Build the binary
bazel build //cmd/knowd:knowd

# Verify the build
ls -la bazel-bin/cmd/knowd/knowd_/knowd
```

## 2. Start the Server

```bash
# Start with default configuration
./bazel-bin/cmd/knowd/knowd_/knowd

# Or run directly through Bazel
bazel run //cmd/knowd:knowd
```

The server will start on `http://localhost:8090` by default.

## 3. Test the Installation

```bash
# Health check
curl http://localhost:8090/healthz

# Should return: ok

# Version info
curl http://localhost:8090/version

# Should return version information
```

## 4. Load Some Data

```bash
# Create a simple RDF dataset
cat > sample-data.ttl << 'EOF'
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .

ex:alice a foaf:Person ;
    foaf:name "Alice" ;
    foaf:knows ex:bob .

ex:bob a foaf:Person ;
    foaf:name "Bob" ;
    foaf:age 25 .
EOF

# Submit a transaction (this is a stub in the basic version)
curl -X POST http://localhost:8090/v1/tx \
  -H "Content-Type: application/json" \
  -d '{"delta": {"add": []}, "actor": "demo"}'
```

## 5. Query the Data

```bash
# Query with SPARQL (basic stub response)
curl -X POST http://localhost:8090/v1/query \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT ?name WHERE { ?person foaf:name ?name }",
    "kind": "sparql-select"
  }'
```

## What's Next?

Now that you have Knowd running:

1. **Explore the API** - See [API Reference](../user-guide/api-reference.md) for all available endpoints
2. **Configure for Production** - Check [Configuration Guide](./configuration.md) for production settings
3. **Set up Clustering** - Learn about [Clustering](../../architecture/clustering.md) for high availability
4. **Add SHACL Validation** - See [SHACL Validation](../user-guide/shacl-validation.md)
5. **Create Hooks** - Learn about [Event-driven Processing](../user-guide/hooks.md)

## Troubleshooting

If you encounter issues:

1. **Port already in use**: Change the port with `-addr :9090`
2. **Permission denied**: Ensure Bazel has proper permissions
3. **Build fails**: Check that Go 1.22+ and Bazel 7.0+ are installed

For more help, see the [Troubleshooting Guide](../../troubleshooting/common-issues.md).

## Example Output

```
$ curl http://localhost:8090/healthz
ok

$ curl http://localhost:8090/version
Version: v1.0.0
Commit: abc123def456...

$ ./bazel-bin/cmd/knowd/knowd_/knowd -addr :8090
INFO: Starting knowd server on :8090
INFO: Data directory: ./data
INFO: Core URL: native://
INFO: Server ready
```

## Next Steps

- **[Full Installation Guide](./installation.md)** - For more detailed installation options
- **[Configuration Guide](./configuration.md)** - Customize Knowd for your needs
- **[API Reference](../user-guide/api-reference.md)** - Explore all available endpoints
