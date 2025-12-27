# Agent 7: UNRDF Substrate Exploration CLI

Exploration of CLI conventions and citty integration in UNRDF. Demonstrates noun-verb command pattern for RDF operations.

## Overview

This exploration CLI showcases:

- **Citty** - CLI framework used by @unrdf/cli and @unrdf/kgc-cli
- **Noun-verb pattern** - Commands organized as `noun verb [args]`
- **Argument parsing** - Type validation with positional and named arguments
- **Error handling** - Graceful error messages with JSON mode support
- **Extensibility** - Easy to add new nouns and verbs

## Commands

### RDF Noun - Graph Operations

#### `rdf load <file>`

Load RDF file from disk.

**Options:**

- `--format, -f` - RDF format (ttl, nt, jsonld, rdfxml). Auto-detected if omitted.
- `--base-iri, -b` - Base IRI for resolving relative IRIs (default: http://example.org/)
- `--dry-run, -d` - Show what would be loaded without loading
- `--json` - Output in JSON format

**Examples:**

```bash
# Load Turtle file with auto-format detection
node index.mjs rdf load ./data.ttl

# Load with explicit format and base IRI
node index.mjs rdf load ./data.jsonld --format jsonld --base-iri http://myapp.org/

# Dry-run mode
node index.mjs rdf load ./data.ttl --dry-run

# JSON output
node index.mjs rdf load ./data.ttl --json
```

#### `rdf query`

Execute SPARQL query against loaded RDF.

**Options:**

- `--sparql, -s` - SPARQL query string (SELECT, ASK, CONSTRUCT, DESCRIBE)
- `--format, -f` - Output format (table, json, csv, xml). Default: table
- `--limit, -l` - Limit results (applies to SELECT). Default: 100
- `--json` - Output in JSON format

**Examples:**

```bash
# Simple SELECT query
node index.mjs rdf query --sparql "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"

# CONSTRUCT query with JSON output
node index.mjs rdf query --sparql "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }" --json

# With result limit
node index.mjs rdf query --sparql "SELECT ?name WHERE { ?x foaf:name ?name }" --limit 50 --format csv
```

#### `rdf validate`

Validate RDF graph against SHACL shapes or basic constraints.

**Options:**

- `--shapes, -s` - Path to SHACL shapes file (TTL format)
- `--strict` - Enable strict validation (fail on warnings)
- `--report, -r` - Output validation report to file
- `--json` - Output in JSON format

**Examples:**

```bash
# Basic validation
node index.mjs rdf validate

# With SHACL shapes
node index.mjs rdf validate --shapes ./shapes.ttl

# Strict mode with report
node index.mjs rdf validate --shapes ./shapes.ttl --strict --report ./report.json

# JSON output
node index.mjs rdf validate --json
```

#### `rdf canonicalize`

Canonicalize RDF graph (normalize IRIs, sort triples).

**Options:**

- `--output, -o` - Output file (default: stdout)
- `--format, -f` - Output format (ttl, nt, jsonld, rdfxml). Default: ttl
- `--algorithm, -a` - Canonicalization algorithm (rdfc10, rdfc14). Default: rdfc10
- `--json` - Output in JSON format

**Examples:**

```bash
# Canonicalize to stdout
node index.mjs rdf canonicalize

# Output to file with format conversion
node index.mjs rdf canonicalize --output ./canonical.ttl --format ttl

# Use different algorithm
node index.mjs rdf canonicalize --algorithm rdfc14 --output ./canonical.nt --format nt

# JSON status output
node index.mjs rdf canonicalize --json
```

### POLICY Noun - Enforcement Operations

#### `policy apply <name>`

Apply policy hook to RDF graph.

**Options:**

- `--params, -p` - Policy parameters as JSON string
- `--dry-run, -d` - Show what would be applied without applying
- `--json` - Output in JSON format

**Valid Policies:**

- `encrypt-sensitive` - Encrypt sensitive RDF values
- `redact-pii` - Redact personally identifiable information
- `mask-emails` - Mask email addresses
- `sign-graph` - Digitally sign the RDF graph

**Examples:**

```bash
# Apply basic policy
node index.mjs policy apply redact-pii

# With parameters
node index.mjs policy apply redact-pii --params '{"fields":["email","phone"]}'

# Dry-run mode
node index.mjs policy apply encrypt-sensitive --dry-run

# JSON output
node index.mjs policy apply sign-graph --json
```

## CLI Architecture

### Command Structure

```
root (exploration-cli)
├── rdf (noun)
│   ├── load (verb)
│   ├── query (verb)
│   ├── validate (verb)
│   └── canonicalize (verb)
└── policy (noun)
    └── apply (verb)
```

### Citty Integration Points

- **defineCommand()** - Creates command objects with meta, args, subcommands, run handler
- **runMain()** - Executes the command tree with argument parsing
- **Args validation** - Type checking with default values, aliases, descriptions
- **Subcommands** - Nested command trees for noun-verb pattern

### Design Patterns

1. **Noun-Verb Separation**
   - Nouns group related operations (rdf, policy)
   - Verbs specify the action (load, query, validate)
   - Enables intuitive discovery via --help at each level

2. **Consistent Error Handling**
   - All commands catch errors and output human-readable messages
   - Optional JSON mode for machine-readable output
   - Non-zero exit codes on failure

3. **Type Validation**
   - Positional arguments (file, policy name)
   - Named string options (--sparql, --params)
   - Boolean flags (--dry-run, --json)
   - Automatic type coercion and defaults

4. **JSON Envelope Output**
   - Commands support --json flag
   - Success/error information with metadata
   - Machine-readable result structures

## Help System

### View all available nouns

```bash
node index.mjs --help
```

### View verbs for a noun

```bash
node index.mjs rdf --help
```

### View help for a specific command

```bash
node index.mjs rdf load --help
```

## Testing

Test all commands:

```bash
# Create a sample RDF file
cat > sample.ttl << 'EOF'
@prefix ex: <http://example.org/> .
ex:alice ex:knows ex:bob ;
         ex:name "Alice" .
EOF

# Load command
node index.mjs rdf load sample.ttl

# Query command
node index.mjs rdf query --sparql "SELECT ?name WHERE { ?x <http://example.org/name> ?name }"

# Validate command
node index.mjs rdf validate

# Canonicalize command
node index.mjs rdf canonicalize --output canonical.ttl

# Policy command
node index.mjs policy apply redact-pii --dry-run
```

## Integration Points

This exploration CLI can integrate with:

1. **Agent 1** - Capability mapping (package roles and APIs)
2. **Agent 2** - RDF parsing/serialization (I/O operations)
3. **Agent 3** - SPARQL execution (query verb)
4. **Agent 4** - Store operations (graph loading/storage)
5. **Agent 6** - Policy hooks and enforcement (policy operations)

## Files

- `index.mjs` - Main CLI implementation (runnable)
- `README.md` - Command reference (this file)
- `notes.md` - Citty API, patterns, and UNRDF integration details

## Running

```bash
# With node directly
node /home/user/unrdf/exploration/agents/agent-7/index.mjs --help

# Make executable and run
chmod +x /home/user/unrdf/exploration/agents/agent-7/index.mjs
/home/user/unrdf/exploration/agents/agent-7/index.mjs rdf load --help
```

## Dependencies

- `citty` - CLI framework (^0.1.6)
- Built-in Node.js modules: fs, path

## Version

- Version: 1.0.0
- Agent: 7 (UNRDF Substrate Exploration Swarm)
- Status: Exploration/Reference Implementation
