# CLI Package Design - @unrdf/cli

**Status:** 📋 Design Document (Implementation: Q2 2026)
**Last Updated:** March 15, 2026

---

## Overview

`@unrdf/cli` will provide a command-line interface for UNRDF, decoupled from the core library as a standalone package.

**Planned Release:** Q2 2026 (v1.0.0)

---

## Motivation

### Why Separate Package?

**v3.0.0 removed CLI** from core to:
1. **Reduce bundle size** - Core library is 45% smaller
2. **Independent evolution** - CLI and core can evolve separately
3. **Optional dependency** - Users who don't need CLI save install time
4. **Better separation of concerns** - Library vs tooling

### Use Cases

- **Interactive exploration** - Quick RDF data inspection
- **CI/CD pipelines** - Validation, transformation
- **Data migration** - Import/export between formats
- **Development tools** - Scaffolding, code generation
- **Testing** - Query testing, validation

---

## Architecture

### Package Structure

```
@unrdf/cli/
├── bin/
│   └── unrdf.mjs              # Entry point
├── src/
│   ├── commands/
│   │   ├── parse.mjs          # RDF parsing
│   │   ├── query.mjs          # SPARQL queries
│   │   ├── validate.mjs       # SHACL validation
│   │   ├── convert.mjs        # Format conversion
│   │   ├── server.mjs         # Dev server
│   │   ├── init.mjs           # Project scaffolding
│   │   └── hooks/
│   │       ├── create.mjs     # Create hook
│   │       ├── list.mjs       # List hooks
│   │       └── test.mjs       # Test hook
│   ├── cli.mjs                # CLI framework (citty)
│   ├── config.mjs             # Configuration loading
│   └── utils/
│       ├── formatter.mjs      # Output formatting
│       ├── spinner.mjs        # Progress indicators
│       └── logger.mjs         # Logging
├── templates/                 # Project templates
│   ├── basic/
│   ├── hooks/
│   └── full/
├── completions/              # Shell completions
│   ├── bash.sh
│   ├── zsh.sh
│   └── fish.fish
├── package.json
└── README.md
```

### Dependencies

```json
{
  "dependencies": {
    "unrdf": "^3.1.0",
    "citty": "^0.1.6",
    "consola": "^3.2.3",
    "chalk": "^5.3.0",
    "ora": "^8.0.1",
    "inquirer": "^9.2.12",
    "table": "^6.8.1"
  }
}
```

---

## Commands

### 1. `unrdf parse`

**Purpose:** Parse RDF file and display/save in different format

**Usage:**
```bash
# Parse Turtle file
unrdf parse data.ttl

# Convert Turtle to JSON-LD
unrdf parse data.ttl --format jsonld --output data.jsonld

# Parse from stdin
cat data.ttl | unrdf parse --format nquads

# Pretty-print with colors
unrdf parse data.ttl --pretty

# Count triples
unrdf parse data.ttl --count
```

**Options:**
```
-f, --format <format>   Output format (turtle, nquads, jsonld)
-o, --output <file>     Output file (default: stdout)
-p, --pretty            Pretty-print output
-c, --count             Count triples
-b, --base <iri>        Base IRI for relative URIs
-v, --validate          Validate RDF syntax
```

**Examples:**
```bash
# Convert Turtle to JSON-LD
unrdf parse foaf.ttl -f jsonld -o foaf.jsonld

# Validate RDF syntax
unrdf parse data.ttl --validate

# Count triples in file
unrdf parse large-dataset.nq --count
# Output: 1,234,567 triples
```

### 2. `unrdf query`

**Purpose:** Execute SPARQL query against RDF data

**Usage:**
```bash
# Query from file
unrdf query data.ttl --sparql query.rq

# Inline query
unrdf query data.ttl -q "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# Output as JSON
unrdf query data.ttl -q "SELECT * WHERE { ?s ?p ?o }" --json

# Output as table
unrdf query data.ttl -q "SELECT * WHERE { ?s ?p ?o }" --table

# ASK query
unrdf query data.ttl -q "ASK { ?s a foaf:Person }"
```

**Options:**
```
-q, --query <sparql>       SPARQL query string
-f, --file <file>          SPARQL query file
-o, --output <file>        Output file
--json                     Output as JSON
--table                    Output as table
--csv                      Output as CSV
--explain                  Show query plan
--timeout <ms>             Query timeout
```

**Examples:**
```bash
# Find all people
unrdf query foaf.ttl -q "
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
" --table

# Output:
# ┌─────────┐
# │ name    │
# ├─────────┤
# │ Alice   │
# │ Bob     │
# │ Charlie │
# └─────────┘

# Export to CSV
unrdf query data.ttl --file complex-query.rq --csv -o results.csv

# Explain query plan
unrdf query data.ttl -q "SELECT * WHERE { ?s ?p ?o }" --explain
```

### 3. `unrdf validate`

**Purpose:** Validate RDF data against SHACL shapes

**Usage:**
```bash
# Validate against shapes
unrdf validate data.ttl --shapes shapes.ttl

# Show violations
unrdf validate data.ttl --shapes shapes.ttl --verbose

# Exit code 1 if invalid
unrdf validate data.ttl --shapes shapes.ttl || echo "Validation failed"
```

**Options:**
```
-s, --shapes <file>      SHACL shapes file
-v, --verbose            Show detailed violations
-o, --output <file>      Output validation report
--json                   Output as JSON
--summary                Show summary only
```

**Examples:**
```bash
# Validate with detailed report
unrdf validate persons.ttl --shapes person-shape.ttl --verbose

# Output:
# ❌ Validation Failed
#
# Violations:
# 1. ex:alice
#    Property: foaf:age
#    Constraint: sh:datatype xsd:integer
#    Message: Value "thirty" is not an integer
#
# 2. ex:bob
#    Property: foaf:name
#    Constraint: sh:minCount 1
#    Message: Missing required property foaf:name
#
# Summary: 2 violations, 0 conforming

# Export report as JSON
unrdf validate data.ttl --shapes shapes.ttl --json -o report.json
```

### 4. `unrdf convert`

**Purpose:** Convert between RDF formats

**Usage:**
```bash
# Convert Turtle to N-Quads
unrdf convert data.ttl data.nq

# Convert with auto-detect
unrdf convert foaf.ttl foaf.jsonld  # Auto-detects formats from extensions

# Specify formats explicitly
unrdf convert input.rdf output.ttl --from rdfxml --to turtle

# Batch convert
unrdf convert *.ttl --to jsonld --output-dir ./jsonld/
```

**Options:**
```
--from <format>          Input format (auto-detect by default)
--to <format>            Output format (required)
-o, --output <file>      Output file
--output-dir <dir>       Output directory (batch mode)
--pretty                 Pretty-print output
```

**Supported formats:**
- `turtle` (`.ttl`)
- `ntriples` (`.nt`)
- `nquads` (`.nq`)
- `jsonld` (`.jsonld`)
- `rdfxml` (`.rdf`, `.owl`)
- `trig` (`.trig`)

### 5. `unrdf server`

**Purpose:** Start development server with web UI

**Usage:**
```bash
# Start server
unrdf server

# Custom port
unrdf server --port 8080

# Load data
unrdf server --data data.ttl

# Watch for changes
unrdf server --data data.ttl --watch
```

**Options:**
```
-p, --port <port>        Port (default: 3000)
-d, --data <file>        RDF data file to load
-w, --watch              Watch data file for changes
--cors                   Enable CORS
--auth <token>           Require authentication
```

**Features:**
- Web UI for browsing RDF data
- Interactive SPARQL query editor
- SHACL validation
- Real-time updates (--watch mode)
- REST API for querying

**Example:**
```bash
unrdf server --data foaf.ttl --watch --port 8080
# Output:
# 🚀 UNRDF Server running on http://localhost:8080
# 📊 Loaded 1,234 triples from foaf.ttl
# 👀 Watching foaf.ttl for changes...

# Open http://localhost:8080 in browser
```

### 6. `unrdf init`

**Purpose:** Initialize new UNRDF project

**Usage:**
```bash
# Interactive init
unrdf init

# With template
unrdf init my-project --template hooks

# Skip prompts
unrdf init my-project --template basic --yes
```

**Options:**
```
-t, --template <name>    Template (basic, hooks, full)
-y, --yes                Skip prompts, use defaults
```

**Templates:**

**basic:** Minimal setup
```
my-project/
├── data/
│   └── example.ttl
├── queries/
│   └── select-all.rq
├── package.json
└── README.md
```

**hooks:** Knowledge Hooks setup
```
my-project/
├── data/
│   └── example.ttl
├── hooks/
│   ├── validation.mjs
│   └── transform.mjs
├── shapes/
│   └── schema.ttl
├── package.json
└── unrdf.config.mjs
```

**full:** Complete project
```
my-project/
├── data/
│   ├── ontology.ttl
│   └── instances.ttl
├── hooks/
│   ├── pre-commit.mjs
│   └── post-commit.mjs
├── shapes/
│   └── constraints.ttl
├── queries/
│   ├── reports.rq
│   └── analytics.rq
├── test/
│   └── hooks.test.mjs
├── package.json
├── unrdf.config.mjs
└── README.md
```

**Interactive prompts:**
```bash
$ unrdf init

? Project name: my-rdf-project
? Description: My semantic web project
? Template: hooks
? Install dependencies? Yes

✔ Created project in ./my-rdf-project
✔ Installed dependencies
✔ Initialized git repository

Next steps:
  cd my-rdf-project
  npm run dev
```

### 7. `unrdf hooks`

**Purpose:** Manage Knowledge Hooks

#### `unrdf hooks create`

```bash
# Create hook interactively
unrdf hooks create

# With options
unrdf hooks create validation --when sparql-ask --description "Validate age"
```

**Interactive prompts:**
```bash
$ unrdf hooks create

? Hook name: age-validation
? Description: Ensure age is >= 18
? Trigger type: sparql-ask
? SPARQL query:
  ASK {
    ?person ex:age ?age .
    FILTER (?age < 18)
  }
? Effect code:
  if (event.result) {
    throw new Error('Age must be >= 18');
  }

✔ Created hook at ./hooks/age-validation.mjs
✔ Registered in unrdf.config.mjs
```

#### `unrdf hooks list`

```bash
# List all hooks
unrdf hooks list

# Output:
# Registered Hooks:
# 1. age-validation
#    Description: Ensure age is >= 18
#    Trigger: sparql-ask
#    File: hooks/age-validation.mjs
#
# 2. transform-data
#    Description: Transform incoming data
#    Trigger: delta
#    File: hooks/transform-data.mjs
```

#### `unrdf hooks test`

```bash
# Test hook
unrdf hooks test age-validation --data test-data.ttl

# Output:
# Testing hook: age-validation
# ✔ Hook executed successfully
# ✔ Validation passed
```

---

## Configuration

### Config File: `unrdf.config.mjs`

```javascript
// unrdf.config.mjs
export default {
  // Data sources
  data: {
    files: ['./data/**/*.ttl'],
    baseIRI: 'http://example.org/'
  },

  // Hooks
  hooks: {
    dir: './hooks',
    autoRegister: true  // Auto-register hooks in hooks/ dir
  },

  // SHACL shapes
  shapes: {
    files: ['./shapes/**/*.ttl']
  },

  // Query defaults
  query: {
    timeout: 5000,
    defaultFormat: 'table'
  },

  // Server
  server: {
    port: 3000,
    cors: true,
    watch: true
  },

  // Profiling
  profiling: {
    enabled: true,
    slowQueryThreshold: 100
  }
};
```

### Environment Variables

```bash
# Override config
UNRDF_PORT=8080
UNRDF_LOG_LEVEL=debug
UNRDF_PROFILE=true
UNRDF_TIMEOUT=10000

# Run with env vars
UNRDF_PORT=8080 unrdf server
```

---

## Shell Completions

### Installation

```bash
# Bash
unrdf completions bash >> ~/.bashrc

# Zsh
unrdf completions zsh >> ~/.zshrc

# Fish
unrdf completions fish > ~/.config/fish/completions/unrdf.fish
```

### Auto-complete Examples

```bash
# Tab completion for commands
unrdf <TAB>
# parse  query  validate  convert  server  init  hooks

# Tab completion for options
unrdf query --<TAB>
# --query  --file  --output  --json  --table  --explain

# Tab completion for formats
unrdf convert data.ttl output.<TAB>
# .ttl  .nt  .nq  .jsonld  .rdf
```

---

## Output Formatting

### Table Format (Default)

```bash
$ unrdf query data.ttl -q "SELECT ?name ?age WHERE { ?p foaf:name ?name ; ex:age ?age }"

┌─────────┬─────┐
│ name    │ age │
├─────────┼─────┤
│ Alice   │ 30  │
│ Bob     │ 25  │
│ Charlie │ 35  │
└─────────┴─────┘
```

### JSON Format

```bash
$ unrdf query data.ttl -q "SELECT ?name WHERE { ?p foaf:name ?name }" --json

[
  { "name": "Alice" },
  { "name": "Bob" },
  { "name": "Charlie" }
]
```

### CSV Format

```bash
$ unrdf query data.ttl -q "SELECT ?name ?age WHERE { ?p foaf:name ?name ; ex:age ?age }" --csv

name,age
Alice,30
Bob,25
Charlie,35
```

### Pretty Turtle

```bash
$ unrdf parse data.nq --format turtle --pretty

@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
         foaf:name "Alice" ;
         ex:age 30 .

ex:bob a foaf:Person ;
       foaf:name "Bob" ;
       ex:age 25 .
```

---

## Error Handling

### Graceful Errors

```bash
$ unrdf parse invalid.ttl

❌ Parse Error
   File: invalid.ttl
   Line: 15
   Column: 23
   Message: Expected '.' but found ';'

   14 | ex:alice foaf:name "Alice" ;
   15 |          ex:age 30;
      |                   ^ Expected '.'
   16 |

Use --verbose for full stack trace
```

### Debug Mode

```bash
$ unrdf query data.ttl -q "..." --debug

🔍 Debug Mode Enabled

Loaded data:
  File: data.ttl
  Triples: 1,234
  Parse time: 45ms

Query:
  Type: SELECT
  Variables: ?name, ?age
  Patterns: 2

Execution:
  Duration: 12ms
  Results: 3
  Cache: miss

[Results...]
```

---

## CI/CD Integration

### GitHub Actions

```yaml
# .github/workflows/validate-rdf.yml
name: Validate RDF Data

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install UNRDF CLI
        run: npm install -g @unrdf/cli

      - name: Validate RDF data
        run: unrdf validate data/**/*.ttl --shapes shapes/**/*.ttl

      - name: Run test queries
        run: |
          for query in queries/*.rq; do
            unrdf query data/all.ttl --file "$query"
          done
```

### GitLab CI

```yaml
# .gitlab-ci.yml
validate-rdf:
  image: node:18
  before_script:
    - npm install -g @unrdf/cli
  script:
    - unrdf validate data/**/*.ttl --shapes shapes/**/*.ttl
    - unrdf query data/all.ttl --file queries/critical.rq --json > results.json
  artifacts:
    paths:
      - results.json
```

---

## Roadmap

### v1.0.0 (Q2 2026)
- Core commands (parse, query, validate, convert)
- Interactive init
- Shell completions
- Basic web server

### v1.1.0 (Q3 2026)
- Hook management commands
- Advanced server features (auth, WebSocket)
- Plugin system
- Performance profiling commands

### v1.2.0 (Q4 2026)
- LSP server for RDF/SPARQL
- VS Code extension
- Data migration tools
- Automated testing framework

---

## Contributing

See main [CONTRIBUTING.md](../CONTRIBUTING.md)

**CLI-specific:**
- Use citty for CLI framework
- Follow Commander.js naming conventions
- Add tests for all commands
- Update completions when adding commands

---

## Summary

`@unrdf/cli` will provide:

✅ **Full RDF toolchain** - parse, query, validate, convert
✅ **Developer experience** - init, server, hooks management
✅ **CI/CD integration** - Exit codes, JSON output
✅ **Shell completions** - Bash, Zsh, Fish
✅ **Beautiful output** - Tables, colors, progress

**Status:** Design complete, implementation planned for Q2 2026

---

**Questions?** Open an issue: https://github.com/unrdf/unrdf/issues
