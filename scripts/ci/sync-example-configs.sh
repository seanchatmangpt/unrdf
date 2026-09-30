#!/usr/bin/env bash
set -euo pipefail
echo "Searching for example configurations..."

# Look for example TOML configs
EXAMPLE_CONFIGS=$(find packages/cli/examples -name "*.toml" 2>/dev/null || true)

if [ -z "$EXAMPLE_CONFIGS" ]; then
  echo "No example configs found in packages/cli"
  echo "Creating test config for validation..."

  # Create minimal test config
  mkdir -p /tmp/sync-test
  cat > /tmp/sync-test/test.toml <<EOFCONFIG
[project]
name = "ci-test"
version = "1.0.0"

[ontology]
source = "/tmp/sync-test/test.ttl"
format = "turtle"

[generation]
output_dir = "/tmp/sync-test/out"

[[generation.rules]]
name = "test-rule"
query = "SELECT ?s WHERE { ?s ?p ?o } LIMIT 1"
template = "/tmp/sync-test/template.njk"
output_file = "test.mjs"
EOFCONFIG

  # Create minimal ontology
  cat > /tmp/sync-test/test.ttl <<EOFONT
@prefix ex: <http://example.org/> .
ex:test a ex:Class .
EOFONT

  # Create minimal template
  cat > /tmp/sync-test/template.njk <<EOFTMPL
---
to: test.mjs
---
export const TEST = true;
EOFTMPL

  # Test the config with dry-run
  echo "Testing generated config..."
  timeout 10s node packages/cli/src/cli/main.mjs sync --config /tmp/sync-test/test.toml --dry-run --verbose

  echo "Config validation passed"
else
  echo "Found example configs - $EXAMPLE_CONFIGS"

  # Test each example config
  for config in $EXAMPLE_CONFIGS; do
    echo "Testing config - $config"
    timeout 10s node packages/cli/src/cli/main.mjs sync --config "$config" --dry-run || {
      echo "Config failed - $config"
      exit 1
    }
  done

  echo "All example configs validated"
fi
