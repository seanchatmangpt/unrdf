# UNRDF CLI Usage Examples

This document provides comprehensive examples of using the UNRDF CLI for various RDF operations and workflows.

## Getting Started

### Installation and Setup

```bash
# Install UNRDF globally
pnpm install -g unrdf

# Or use with npx (no global install needed)
npx unrdf --help

# Initialize a new project
unrdf init my-knowledge-graph
cd my-knowledge-graph
pnpm install
```

### Basic Operations

```bash
# Check CLI version and help
unrdf --version
unrdf --help

# Get help for specific commands
unrdf help parse
unrdf help query
```

## Data Parsing Examples

### Basic Parsing

```bash
# Parse a Turtle file
unrdf parse person-data.ttl

# Parse with detailed statistics
unrdf parse person-data.ttl --stats

# Parse and save to file
unrdf parse person-data.ttl --output cleaned-data.ttl
```

### Different Input Formats

```bash
# Parse JSON-LD
unrdf parse schema-org-data.jsonld --format json-ld

# Parse N-Quads
unrdf parse quad-data.nq --format nquads

# Parse N3/Turtle with custom base
unrdf parse ontology.n3 --format n3
```

### Sample Data Files

**person-data.ttl:**
```turtle
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/people/> .

ex:alice a foaf:Person ;
    foaf:name "Alice Johnson" ;
    foaf:age 32 ;
    foaf:email "alice@example.com" ;
    foaf:knows ex:bob .

ex:bob a foaf:Person ;
    foaf:name "Bob Smith" ;
    foaf:age 28 ;
    foaf:email "bob@example.com" .
```

## SPARQL Query Examples

### Basic Queries

```bash
# Simple SELECT query
unrdf query person-data.ttl \
  --query "SELECT ?name WHERE { ?person foaf:name ?name }"

# Count all triples
unrdf query person-data.ttl \
  --query "SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }"

# Find all people and their ages
unrdf query person-data.ttl \
  --query "SELECT ?name ?age WHERE { 
    ?person foaf:name ?name ; 
    foaf:age ?age 
  } ORDER BY ?age"
```

### Query Output Formats

```bash
# JSON output for programmatic processing
unrdf query person-data.ttl \
  --query "SELECT * WHERE { ?s ?p ?o } LIMIT 5" \
  --format json

# CSV output for spreadsheet import
unrdf query person-data.ttl \
  --query "SELECT ?name ?email WHERE { 
    ?person foaf:name ?name ; 
    foaf:email ?email 
  }" \
  --format csv

# Table format for human reading (default)
unrdf query person-data.ttl \
  --query "SELECT ?name ?age WHERE { 
    ?person foaf:name ?name ; 
    foaf:age ?age 
  }" \
  --format table
```

### Query Files

**queries/people-analytics.sparql:**
```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?name ?age ?email
WHERE {
  ?person a foaf:Person ;
          foaf:name ?name ;
          foaf:age ?age ;
          foaf:email ?email .
  FILTER(?age > 25)
}
ORDER BY DESC(?age)
```

```bash
# Execute query from file
unrdf query person-data.ttl \
  --query-file queries/people-analytics.sparql \
  --format json
```

### Complex Queries

```bash
# Social network analysis
unrdf query person-data.ttl \
  --query "SELECT ?person1 ?person2 WHERE { 
    ?person1 foaf:knows ?person2 
  }"

# Aggregation queries
unrdf query person-data.ttl \
  --query "SELECT (AVG(?age) as ?avgAge) (COUNT(*) as ?total) WHERE { 
    ?person foaf:age ?age 
  }"

# CONSTRUCT queries to generate new RDF
unrdf query person-data.ttl \
  --query "CONSTRUCT { ?person ex:isAdult true } WHERE { 
    ?person foaf:age ?age . 
    FILTER(?age >= 18) 
  }" \
  --format turtle
```

## Validation Examples

### SHACL Validation

**shapes/person-shape.ttl:**
```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

[] a sh:NodeShape ;
   sh:targetClass foaf:Person ;
   sh:property [
     sh:path foaf:name ;
     sh:datatype xsd:string ;
     sh:minCount 1 ;
     sh:maxCount 1 ;
   ] ;
   sh:property [
     sh:path foaf:age ;
     sh:datatype xsd:integer ;
     sh:minInclusive 0 ;
     sh:maxInclusive 150 ;
   ] ;
   sh:property [
     sh:path foaf:email ;
     sh:datatype xsd:string ;
     sh:pattern "^[\\w\\.-]+@[\\w\\.-]+\\.[a-zA-Z]{2,}$" ;
   ] .
```

```bash
# Basic validation
unrdf validate person-data.ttl --shape shapes/person-shape.ttl

# Save validation report
unrdf validate person-data.ttl \
  --shape shapes/person-shape.ttl \
  --output validation-report.json

# Multiple shape files
unrdf validate person-data.ttl --shape shapes/person-shape.ttl
unrdf validate person-data.ttl --shape shapes/organization-shape.ttl
```

### Validation Workflow

```bash
# Validate before processing
unrdf validate raw-data.ttl --shape schemas/data-shape.ttl

# Only proceed if validation passes
if [ $? -eq 0 ]; then
  echo "✅ Data is valid, proceeding with processing..."
  unrdf convert raw-data.ttl --to json-ld --output processed.jsonld
else
  echo "❌ Data validation failed, check the errors above"
  exit 1
fi
```

## Format Conversion Examples

### RDF to RDF Conversion

```bash
# Turtle to JSON-LD
unrdf convert person-data.ttl --to json-ld --output person-data.jsonld

# JSON-LD to N-Quads
unrdf convert person-data.jsonld --from json-ld --to nquads --output person-data.nq

# N-Quads to Turtle
unrdf convert person-data.nq --from nquads --to turtle --output person-data-clean.ttl
```

### Structured Data Conversion

**schemas/person-schema.mjs:**
```javascript
import { z } from 'zod';

export default z.object({
  name: z.string(),
  age: z.number().min(0).max(150),
  email: z.string().email(),
  friends: z.array(z.string()).optional()
});
```

```bash
# JSON to RDF with schema validation
unrdf convert people.json \
  --from json \
  --to turtle \
  --schema schemas/person-schema.mjs \
  --output people.ttl

# RDF to JSON with schema validation
unrdf convert people.ttl \
  --from turtle \
  --to json \
  --schema schemas/person-schema.mjs \
  --output validated-people.json
```

## Reasoning Examples

### OWL Reasoning

**ontologies/family-ontology.ttl:**
```turtle
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix family: <http://example.org/family#> .

family:Parent rdfs:subClassOf family:Person .
family:Child rdfs:subClassOf family:Person .

family:hasParent owl:inverseOf family:hasChild .
family:hasChild a owl:ObjectProperty .
family:hasParent a owl:ObjectProperty .
```

```bash
# Apply OWL reasoning
unrdf reason family-data.ttl \
  --builtin owl \
  --output family-inferred.ttl

# Custom reasoning rules
unrdf reason family-data.ttl \
  --rules rules/family-rules.n3 \
  --output family-reasoned.ttl

# Multiple rule sets
unrdf reason ontology.ttl \
  --builtin owl \
  --builtin rdfs \
  --output fully-reasoned.ttl
```

### Custom N3 Rules

**rules/family-rules.n3:**
```n3
@prefix family: <http://example.org/family#> .

# If X is a parent of Y, then Y is a child of X
{ ?x family:hasChild ?y } => { ?y family:hasParent ?x } .

# Grandparent relationship
{ ?x family:hasChild ?y . ?y family:hasChild ?z } => { ?x family:hasGrandchild ?z } .
```

## Advanced Workflows

### Data Processing Pipeline

```bash
#!/bin/bash
# Complete data processing pipeline

echo "🔄 Starting data processing pipeline..."

# 1. Validate input data
echo "Step 1: Validating input data..."
unrdf validate raw-data.ttl --shape schemas/input-shape.ttl
if [ $? -ne 0 ]; then
  echo "❌ Input validation failed"
  exit 1
fi

# 2. Parse and analyze
echo "Step 2: Parsing and analyzing data..."
unrdf parse raw-data.ttl --stats > parse-report.txt

# 3. Apply reasoning
echo "Step 3: Applying reasoning rules..."
unrdf reason raw-data.ttl \
  --builtin owl \
  --rules rules/domain-rules.n3 \
  --output enriched-data.ttl

# 4. Validate enriched data
echo "Step 4: Validating enriched data..."
unrdf validate enriched-data.ttl --shape schemas/output-shape.ttl

# 5. Convert to target formats
echo "Step 5: Converting to target formats..."
unrdf convert enriched-data.ttl --to json-ld --output final-data.jsonld
unrdf convert enriched-data.ttl --to nquads --output final-data.nq

# 6. Generate analytics
echo "Step 6: Generating analytics..."
unrdf metrics final-data.jsonld --detailed > analytics-report.txt

# 7. Query for insights
echo "Step 7: Extracting insights..."
unrdf query final-data.jsonld \
  --query-file queries/insights.sparql \
  --format json > insights.json

echo "✅ Pipeline completed successfully!"
```

### Data Quality Assurance

```bash
#!/bin/bash
# Comprehensive data quality checks

DATA_FILE="dataset.ttl"
SHAPES_DIR="shapes"
REPORTS_DIR="quality-reports"

mkdir -p $REPORTS_DIR

echo "🔍 Running data quality assurance..."

# Basic metrics
unrdf metrics $DATA_FILE --detailed > $REPORTS_DIR/basic-metrics.txt

# Canonicalization check
unrdf canon check $DATA_FILE > $REPORTS_DIR/canon-check.txt

# Generate canonical hash for integrity
HASH=$(unrdf canon hash $DATA_FILE)
echo "Dataset hash: $HASH" > $REPORTS_DIR/integrity-hash.txt

# Validate against all shape files
for shape in $SHAPES_DIR/*.ttl; do
  shape_name=$(basename "$shape" .ttl)
  echo "Validating against $shape_name..."
  
  unrdf validate $DATA_FILE \
    --shape "$shape" \
    --output "$REPORTS_DIR/validation-$shape_name.json"
done

# Check for common issues
unrdf query $DATA_FILE \
  --query "SELECT ?s WHERE { ?s ?p ?o . FILTER(!isIRI(?s)) }" \
  --format json > $REPORTS_DIR/blank-node-subjects.json

unrdf query $DATA_FILE \
  --query "SELECT ?p (COUNT(*) as ?count) WHERE { ?s ?p ?o } GROUP BY ?p ORDER BY DESC(?count)" \
  --format json > $REPORTS_DIR/predicate-usage.json

echo "✅ Quality assurance completed. Reports in $REPORTS_DIR/"
```

### Dataset Comparison and Versioning

```bash
#!/bin/bash
# Compare dataset versions

OLD_VERSION="dataset-v1.ttl"
NEW_VERSION="dataset-v2.ttl"
REPORTS_DIR="version-reports"

mkdir -p $REPORTS_DIR

echo "📊 Comparing dataset versions..."

# Generate delta report
unrdf delta $OLD_VERSION $NEW_VERSION \
  --output $REPORTS_DIR/changes.json

# Metrics comparison
unrdf metrics $OLD_VERSION --detailed > $REPORTS_DIR/v1-metrics.txt
unrdf metrics $NEW_VERSION --detailed > $REPORTS_DIR/v2-metrics.txt

# Quality comparison
unrdf validate $OLD_VERSION --shape shapes/quality-shape.ttl \
  --output $REPORTS_DIR/v1-quality.json
unrdf validate $NEW_VERSION --shape shapes/quality-shape.ttl \
  --output $REPORTS_DIR/v2-quality.json

# Generate comparison summary
cat > $REPORTS_DIR/summary.md << EOF
# Dataset Version Comparison

## Changes Summary
$(jq '.summary' $REPORTS_DIR/changes.json)

## Quality Metrics
- V1: $(jq '.violations' $REPORTS_DIR/v1-quality.json) violations
- V2: $(jq '.violations' $REPORTS_DIR/v2-quality.json) violations

## Detailed Reports
- Changes: changes.json
- V1 Metrics: v1-metrics.txt
- V2 Metrics: v2-metrics.txt
EOF

echo "✅ Version comparison completed. Reports in $REPORTS_DIR/"
```

## Utility Examples

### Namespace and Prefix Management

```bash
# List all known prefixes
unrdf prefix list

# Expand CURIEs
unrdf prefix expand foaf:Person
unrdf prefix expand schema:Organization

# Shrink full IRIs
unrdf prefix shrink "http://xmlns.com/foaf/0.1/Person"
unrdf prefix shrink "https://schema.org/Organization"
```

### ID Generation

```bash
# Generate UUIDs for new resources
unrdf id uuid
unrdf id uuid --count 10

# Generate hash-based IDs for deterministic identifiers
unrdf id hash "Alice Johnson"
unrdf id hash "Bob Smith"

# Generate prefixed IDs
unrdf id generate --prefix person
unrdf id generate --prefix org
```

### Cache and Performance Management

```bash
# Clear cache when needed
unrdf cache clear

# Check cache statistics
unrdf cache stats

# Monitor performance with timing
time unrdf parse large-dataset.ttl --stats
time unrdf query large-dataset.ttl --query-file complex-query.sparql
```

## Integration Examples

### Package.json Scripts

```json
{
  "name": "my-semantic-app",
  "scripts": {
    "data:validate": "unrdf validate data/*.ttl --shape shapes/*.ttl",
    "data:convert": "unrdf convert data/source.ttl --to json-ld --output data/processed.jsonld",
    "data:analyze": "unrdf metrics data/processed.jsonld --detailed > reports/analysis.txt",
    "data:query": "unrdf query data/processed.jsonld --query-file queries/insights.sparql --format json > results/insights.json",
    "data:process": "npm run data:validate && npm run data:convert && npm run data:analyze && npm run data:query",
    "data:clean": "unrdf cache clear"
  }
}
```

### GitHub Actions CI/CD

```yaml
name: RDF Data Quality

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Setup Node.js
      uses: actions/setup-node@v3
      with:
        node-version: '18'
        
    - name: Install UNRDF
      run: npm install -g unrdf
      
    - name: Validate RDF Data
      run: |
        unrdf validate data/*.ttl --shape schemas/*.ttl
        
    - name: Generate Quality Report
      run: |
        unrdf metrics data/combined.ttl --detailed > quality-report.txt
        
    - name: Upload Quality Report
      uses: actions/upload-artifact@v3
      with:
        name: quality-report
        path: quality-report.txt
```

### Docker Integration

```dockerfile
FROM node:18-alpine

# Install UNRDF globally
RUN npm install -g unrdf

# Copy data and configuration
COPY data/ /app/data/
COPY shapes/ /app/shapes/
COPY queries/ /app/queries/
COPY unrdf.config.mjs /app/

WORKDIR /app

# Run data processing pipeline
CMD ["sh", "-c", "unrdf validate data/*.ttl --shape shapes/*.ttl && unrdf convert data/source.ttl --to json-ld --output processed.jsonld"]
```

### Makefile Integration

```makefile
.PHONY: validate convert analyze clean

DATA_DIR := data
SHAPES_DIR := shapes
OUTPUT_DIR := output

validate:
	@echo "Validating RDF data..."
	@for file in $(DATA_DIR)/*.ttl; do \
		unrdf validate "$$file" --shape $(SHAPES_DIR)/schema.ttl; \
	done

convert:
	@echo "Converting data formats..."
	@mkdir -p $(OUTPUT_DIR)
	@unrdf convert $(DATA_DIR)/source.ttl --to json-ld --output $(OUTPUT_DIR)/data.jsonld
	@unrdf convert $(DATA_DIR)/source.ttl --to nquads --output $(OUTPUT_DIR)/data.nq

analyze:
	@echo "Analyzing data quality..."
	@unrdf metrics $(DATA_DIR)/source.ttl --detailed > $(OUTPUT_DIR)/metrics.txt

clean:
	@echo "Cleaning cache and temporary files..."
	@unrdf cache clear
	@rm -rf $(OUTPUT_DIR)

process: validate convert analyze
	@echo "Data processing pipeline completed!"
```

## Troubleshooting Examples

### Common Error Scenarios

```bash
# Debug parsing issues
UNRDF_DEBUG=true unrdf parse problematic-data.ttl

# Handle large files with streaming
unrdf parse huge-dataset.ttl --stats | head -20

# Validate with detailed error reporting
unrdf validate data.ttl --shape strict-shape.ttl --output errors.json
cat errors.json | jq '.results[] | {focus: .focusNode, message: .message}'

# Query timeout handling
timeout 30s unrdf query large-dataset.ttl --query "SELECT * WHERE { ?s ?p ?o }" --limit 1000
```

### Performance Optimization

```bash
# Profile memory usage
/usr/bin/time -v unrdf parse large-dataset.ttl

# Optimize queries with LIMIT
unrdf query dataset.ttl --query "SELECT * WHERE { ?s ?p ?o }" --limit 1000

# Batch processing for large datasets
split -l 10000 huge-dataset.ttl chunk_
for chunk in chunk_*; do
  unrdf parse "$chunk" --output "processed_$chunk"
done
```

This comprehensive guide covers the most common and advanced usage patterns for the UNRDF CLI. Each example is production-ready and follows best practices for RDF data processing workflows.
