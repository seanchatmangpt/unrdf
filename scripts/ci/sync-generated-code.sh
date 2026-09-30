#!/usr/bin/env bash
set -euo pipefail
echo "Testing code generation and ESM compilation..."

# Create comprehensive test setup
TEST_DIR=$(mktemp -d)
echo "Test directory - $TEST_DIR"

# Create test ontology with multiple entities and properties
cat > "$TEST_DIR/schema.ttl" <<EOFSCHEMA
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix test: <http://test.example/schema#> .

test:User a owl:Class ;
    rdfs:label "User" ;
    rdfs:comment "User entity" .

test:Post a owl:Class ;
    rdfs:label "Post" ;
    rdfs:comment "Post entity" .

test:username a owl:DatatypeProperty ;
    rdfs:domain test:User ;
    rdfs:range xsd:string ;
    rdfs:label "username" .

test:title a owl:DatatypeProperty ;
    rdfs:domain test:Post ;
    rdfs:range xsd:string ;
    rdfs:label "title" .
EOFSCHEMA

# Create template directory
mkdir -p "$TEST_DIR/templates"

# Create entities template (using printf to avoid heredoc issues)
printf '%s\n' '---' 'to: entities.mjs' 'description: Generated entity constants' '---' '/**' ' * @file Generated Entity Constants' ' */' '' '{% for row in sparql_results %}' 'export const {{ row.label | upper }}_URI = '"'"'{{ row.entity }}'"'"';' '{% endfor %}' '' 'export const ALL_ENTITIES = [' '{% for row in sparql_results %}' '  {{ row.label | upper }}_URI,' '{% endfor %}' '];' > "$TEST_DIR/templates/entities.njk"

# Create properties template
printf '%s\n' '---' 'to: properties.mjs' '---' '/**' ' * @file Generated Property Definitions' ' */' '' '{% for row in sparql_results %}' 'export const PROP_{{ row.label | upper }} = {' '  uri: '"'"'{{ row.prop }}'"'"',' '  domain: '"'"'{{ row.domain }}'"'"',' '  range: '"'"'{{ row.range }}'"'"',' '};' '{% endfor %}' > "$TEST_DIR/templates/properties.njk"

# Create comprehensive config
cat > "$TEST_DIR/unrdf.toml" <<EOFCONFIG
[project]
name = "validation-test"
version = "1.0.0"
description = "CI validation test project"

[ontology]
source = "$TEST_DIR/schema.ttl"
format = "turtle"

[generation]
output_dir = "$TEST_DIR/src"

[[generation.rules]]
name = "entities"
description = "Generate entity constants"
query = """
SELECT ?entity ?label ?comment
WHERE {
  ?entity a owl:Class .
  OPTIONAL { ?entity rdfs:label ?label }
  OPTIONAL { ?entity rdfs:comment ?comment }
}
ORDER BY ?label
"""
template = "$TEST_DIR/templates/entities.njk"
output_file = "entities.mjs"
enabled = true

[[generation.rules]]
name = "properties"
description = "Generate property definitions"
query = """
SELECT ?prop ?label ?domain ?range
WHERE {
  ?prop a owl:DatatypeProperty .
  OPTIONAL { ?prop rdfs:label ?label }
  OPTIONAL { ?prop rdfs:domain ?domain }
  OPTIONAL { ?prop rdfs:range ?range }
}
ORDER BY ?label
"""
template = "$TEST_DIR/templates/properties.njk"
output_file = "properties.mjs"
enabled = true
EOFCONFIG

# Run sync command with verbose output
echo "Executing sync command..."
timeout 15s node packages/cli/src/cli/main.mjs sync \
  --config "$TEST_DIR/unrdf.toml" \
  --verbose \
  --output text

# Verify generated files exist
echo "Verifying generated files..."
test -f "$TEST_DIR/src/entities.mjs" || {
  echo "❌ entities.mjs not created"
  exit 1
}
test -f "$TEST_DIR/src/properties.mjs" || {
  echo "❌ properties.mjs not created"
  exit 1
}

# Verify ESM syntax is valid
echo "Checking ESM syntax..."
timeout 5s node -c "$TEST_DIR/src/entities.mjs" || {
  echo "❌ entities.mjs has syntax errors"
  cat "$TEST_DIR/src/entities.mjs"
  exit 1
}
timeout 5s node -c "$TEST_DIR/src/properties.mjs" || {
  echo "❌ properties.mjs has syntax errors"
  cat "$TEST_DIR/src/properties.mjs"
  exit 1
}

# Test that files can be imported
echo "Testing ESM imports..."
timeout 5s node --input-type=module -e "
  import('$TEST_DIR/src/entities.mjs').then(m => {
    console.log('✅ entities.mjs imports successfully');
    console.log('  Exports:', Object.keys(m).length, 'symbols');
    if (!m.ALL_ENTITIES || m.ALL_ENTITIES.length === 0) {
      console.error('❌ ALL_ENTITIES is empty');
      process.exit(1);
    }
  });
" || exit 1

timeout 5s node --input-type=module -e "
  import('$TEST_DIR/src/properties.mjs').then(m => {
    console.log('✅ properties.mjs imports successfully');
    console.log('  Exports:', Object.keys(m).length, 'symbols');
  });
" || exit 1

# Verify content quality
echo "Verifying output content..."

# Check entities.mjs contains expected content
grep -q "USER_URI" "$TEST_DIR/src/entities.mjs" || {
  echo "❌ entities.mjs missing expected exports"
  exit 1
}
grep -q "POST_URI" "$TEST_DIR/src/entities.mjs" || {
  echo "❌ entities.mjs missing POST entity"
  exit 1
}
grep -q "ALL_ENTITIES" "$TEST_DIR/src/entities.mjs" || {
  echo "❌ entities.mjs missing ALL_ENTITIES export"
  exit 1
}

# Check properties.mjs contains expected content
grep -q "PROP_USERNAME" "$TEST_DIR/src/properties.mjs" || {
  echo "❌ properties.mjs missing expected properties"
  exit 1
}

# Display output files for debugging
echo "Output entities.mjs:"
cat "$TEST_DIR/src/entities.mjs"
echo ""
echo "Output properties.mjs:"
cat "$TEST_DIR/src/properties.mjs"

# Cleanup
rm -rf "$TEST_DIR"

echo "✅ All generated code validated successfully"
