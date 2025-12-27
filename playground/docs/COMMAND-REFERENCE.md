# Command Reference

> Hyper-Reference for all Playground CLI commands

```json-ld
{
  "@context": {
    "cmd": "urn:playground:commands:",
    "xsd": "http://www.w3.org/2001/XMLSchema#"
  },
  "@id": "urn:playground:command-reference:v1.0.0",
  "@type": "cmd:CommandReference"
}
```

---

## Global Options

Available on all commands:

| Flag | Alias | Type | Default | Description |
|------|-------|------|---------|-------------|
| `--quiet` | `-q` | boolean | false | Suppress non-essential output |
| `--format` | `-f` | enum | table | Output format (json, yaml, table, latex) |
| `--output` | `-o` | string | - | Output file path |
| `--verbose` | `-v` | boolean | false | Enable verbose output |
| `--config` | `-c` | string | - | Path to config file |

---

## papers generate

**URI:** `cmd:papers-generate`
**Version:** 1.0.0

Generate research paper from template using specified family structure.

### Syntax

```bash
playground papers generate [family] --title <title> --author <author> [options]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `family` | positional | No | imrad | Paper family (imrad, dsr, argument, contribution) |
| `--title` | string | Yes | - | Paper title |
| `--author` | string | Yes | - | Author name |
| `--affiliation` | string | No | - | Author affiliation |
| `--abstract` | string | No | - | Paper abstract |
| `--sections` | string | No | - | Custom sections as JSON array |
| `--output` | string | No | - | Output file path |
| `--format` | enum | No | latex | Output format (latex, json) |

### SPARQL Precondition

```sparql
ASK {
  FILTER(STRLEN(?title) > 0) .
  FILTER(STRLEN(?author) > 0) .
}
```

### Success Output

```json
{
  "id": "paper-1700000000000",
  "family": "imrad",
  "title": "My Research Paper",
  "authors": [{"name": "Alice", "affiliation": "MIT"}],
  "sections": [
    {"heading": "Introduction", "order": 1, "content": ""},
    {"heading": "Methods", "order": 2, "content": ""},
    {"heading": "Results", "order": 3, "content": ""},
    {"heading": "Discussion", "order": 4, "content": ""},
    {"heading": "Conclusion", "order": 5, "content": ""}
  ],
  "createdAt": "2025-11-22T00:00:00.000Z"
}
```

### Error Cases

| Code | Message | Recovery |
|------|---------|----------|
| E_VALIDATION | Title is required | Add --title flag |
| E_VALIDATION | Author is required | Add --author flag |
| E_UNKNOWN_FAMILY | Unknown paper family | Run `playground papers list` |

### Performance

| Metric | Value |
|--------|-------|
| p50 | 120ms |
| p95 | 320ms |
| p99 | 450ms |
| max | 800ms |

### Examples

```bash
# Basic IMRAD paper
playground papers generate --title "My Paper" --author "Alice"

# DSR paper with affiliation
playground papers generate dsr -t "Design Science" -a "Bob" --affiliation "Stanford"

# Argument paper with abstract, JSON output
playground papers generate argument --title "Thesis" --author "Carol" \
  --abstract "This paper argues..." --format json

# Custom sections
playground papers generate contribution --title "Novel Approach" --author "Dave" \
  --sections '[{"heading":"Custom","content":""}]' -o ./output/paper.tex
```

---

## papers list

**URI:** `cmd:papers-list`
**Version:** 1.0.0

List all available paper families.

### Syntax

```bash
playground papers list [--verbose] [--format]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `--verbose` | boolean | No | false | Show detailed information |
| `--format` | enum | No | table | Output format (table, json, yaml) |

### Success Output (JSON)

```json
[
  {
    "id": "imrad",
    "name": "IMRAD",
    "description": "Introduction, Methods, Results, and Discussion",
    "sections": ["Introduction", "Methods", "Results", "Discussion", "Conclusion"]
  },
  {
    "id": "dsr",
    "name": "Design Science Research",
    "description": "Design Science Research structure",
    "sections": ["Problem", "Objectives", "Design", "Demonstration", "Evaluation", "Communication"]
  }
]
```

### Performance

| Metric | Value |
|--------|-------|
| p50 | 5ms |
| p99 | 15ms |

---

## papers validate

**URI:** `cmd:papers-validate`
**Version:** 1.0.0

Validate paper structure against family schema.

### Syntax

```bash
playground papers validate <path> [--strict] [--format]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `path` | positional | Yes | - | Path to paper file |
| `--strict` | boolean | No | false | Enable strict validation |
| `--format` | enum | No | table | Output format (table, json) |

### Success Output

```json
{
  "path": "./paper.tex",
  "valid": true,
  "errors": [],
  "warnings": [],
  "info": {
    "family": "imrad",
    "sections": 5,
    "wordCount": 1500
  }
}
```

### Error Cases

| Code | Message | Recovery |
|------|---------|----------|
| E_FILE_NOT_FOUND | File not found | Check file path |
| E_PARSE_ERROR | Cannot parse file | Check file format |

### Performance

| Metric | Value |
|--------|-------|
| p50 | 80ms |
| p99 | 250ms |

---

## thesis generate

**URI:** `cmd:thesis-generate`
**Version:** 1.0.0

Generate thesis document structure.

### Syntax

```bash
playground thesis generate [type] --title <title> --author <author> [options]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `type` | positional | No | monograph | Thesis type |
| `--title` | string | Yes | - | Thesis title |
| `--author` | string | Yes | - | Author name |
| `--institution` | string | No | - | Institution name |
| `--department` | string | No | - | Department name |
| `--supervisor` | string | No | - | Supervisor name |
| `--degree` | enum | No | PhD | Degree type (PhD, Master, Bachelor) |
| `--output` | string | No | - | Output file path |
| `--format` | enum | No | latex | Output format (latex, json) |

### Performance

| Metric | Value |
|--------|-------|
| p50 | 150ms |
| p99 | 520ms |

### Examples

```bash
# Basic monograph thesis
playground thesis generate --title "My PhD Thesis" --author "Alice"

# Narrative thesis with supervisor
playground thesis generate narrative -t "Qualitative Study" -a "Bob" \
  --supervisor "Dr. Smith" --institution "Harvard"

# Contribution-based PhD
playground thesis generate contribution --title "Publications Collection" \
  --author "Carol" --degree PhD --department "Computer Science"
```

---

## thesis list

**URI:** `cmd:thesis-list`
**Version:** 1.0.0

List all available thesis types.

### Syntax

```bash
playground thesis list [--verbose] [--format]
```

### Performance

| Metric | Value |
|--------|-------|
| p50 | 5ms |
| p99 | 15ms |

---

## thesis schedule list

**URI:** `cmd:thesis-schedule-list`
**Version:** 1.0.0

Display current thesis schedule and milestones.

### Syntax

```bash
playground thesis schedule list [--format]
```

### Success Output

```json
{
  "defenseDate": "2025-06-15",
  "milestones": [
    {"name": "Thesis Proposal", "date": "2024-09-01", "status": "completed"},
    {"name": "Literature Review", "date": "2024-12-01", "status": "completed"},
    {"name": "Complete Draft", "date": "2025-03-01", "status": "in-progress"},
    {"name": "Final Review", "date": "2025-05-15", "status": "pending"},
    {"name": "Thesis Defense", "date": "2025-06-15", "status": "pending"}
  ]
}
```

---

## thesis schedule set

**URI:** `cmd:thesis-schedule-set`
**Version:** 1.0.0

Configure thesis schedule with defense date or milestones.

### Syntax

```bash
playground thesis schedule set [--defense <date>] [--milestone <json>]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `--defense` | string | No | - | Defense date (YYYY-MM-DD) |
| `--milestone` | string | No | - | Milestone JSON |

### Examples

```bash
# Set defense date
playground thesis schedule set --defense 2025-06-15

# Add milestone
playground thesis schedule set --milestone '{"name": "Draft Complete", "date": "2025-03-01"}'
```

---

## config set

**URI:** `cmd:config-set`
**Version:** 1.0.0

Set a configuration value.

### Syntax

```bash
playground config set <key> <value>
```

### Arguments

| Argument | Type | Required | Pattern | Description |
|----------|------|----------|---------|-------------|
| `key` | positional | Yes | `^[a-z]+(\.[a-z]+)*$` | Configuration key |
| `value` | positional | Yes | - | Configuration value |

### Valid Keys

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `author.name` | string | "Unknown Author" | Default author name |
| `author.email` | string | "" | Author email |
| `author.affiliation` | string | "" | Author affiliation |
| `output.format` | string | "latex" | Default output format |
| `output.directory` | string | "./output" | Default output directory |
| `cli.verbose` | string | "false" | Verbose mode |
| `cli.color` | string | "true" | Color output |
| `templates.directory` | string | "./templates" | Templates directory |
| `ontology.path` | string | "./ontologies/papers-thesis.ttl" | Ontology file path |

### Examples

```bash
playground config set author.name "Alice Smith"
playground config set output.format json
```

---

## config get

**URI:** `cmd:config-get`
**Version:** 1.0.0

Get a configuration value.

### Syntax

```bash
playground config get <key> [--format]
```

### Examples

```bash
playground config get author.name
playground config get output.format --format json
```

---

## config list

**URI:** `cmd:config-list`
**Version:** 1.0.0

List all configuration values.

### Syntax

```bash
playground config list [--format] [--defaults]
```

---

## config reset

**URI:** `cmd:config-reset`
**Version:** 1.0.0

Reset configuration to defaults.

### Syntax

```bash
playground config reset [--confirm] [--key <key>]
```

### Examples

```bash
# Reset specific key
playground config reset --key author.name

# Reset all (requires confirmation)
playground config reset --confirm
```

---

## meta introspect

**URI:** `cmd:meta-introspect`
**Version:** 1.0.0

Machine-grade introspection of CLI capabilities.

### Syntax

```bash
playground meta introspect [--format] [--section]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `--format` | enum | No | json | Output format (json, yaml) |
| `--section` | enum | No | all | Section filter |

### Sections

- `all` - Complete registry
- `commands` - Command definitions only
- `families` - Paper families only
- `types` - Thesis types only
- `global` - Global args only

---

## meta sparql

**URI:** `cmd:meta-sparql`
**Version:** 1.0.0

Execute SPARQL query against loaded ontologies.

### Syntax

```bash
playground meta sparql [query] [--file] [--format] [--limit]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `query` | positional | No* | - | SPARQL query string |
| `--file` | string | No* | - | Path to query file |
| `--format` | enum | No | table | Output format (table, json, csv) |
| `--limit` | string | No | 100 | Result limit |

*Either query or --file must be provided

### Examples

```bash
# Inline query
playground meta sparql "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"

# Query from file
playground meta sparql --file ./queries/find-papers.sparql

# JSON output
playground meta sparql "SELECT * WHERE { ?s a paper:Paper }" --format json
```

### Performance

| Metric | Value (10 results) | Value (100 results) | Value (1000 results) |
|--------|-------------------|---------------------|---------------------|
| p50 | 50ms | 85ms | 250ms |
| p99 | 150ms | 350ms | 800ms |

---

## meta completions

**URI:** `cmd:meta-completions`
**Version:** 1.0.0

Generate shell completions.

### Syntax

```bash
playground meta completions <shell>
```

### Arguments

| Argument | Type | Required | Values | Description |
|----------|------|----------|--------|-------------|
| `shell` | positional | Yes | bash, zsh, fish, powershell | Target shell |

### Examples

```bash
# Bash
playground meta completions bash > ~/.bash_completion.d/playground

# Zsh
playground meta completions zsh > ~/.zsh/completions/_playground

# Fish
playground meta completions fish > ~/.config/fish/completions/playground.fish
```

---

## meta middleware list

**URI:** `cmd:meta-middleware-list`
**Version:** 1.0.0

List active middleware components.

### Syntax

```bash
playground meta middleware list [--format]
```

---

## meta telemetry

**URI:** `cmd:meta-telemetry`
**Version:** 1.0.0

Export execution metrics.

### Syntax

```bash
playground meta telemetry [format] [--output] [--timeframe]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `format` | positional | No | json | Export format (json, prometheus, otlp) |
| `--output` | string | No | - | Output file path |
| `--timeframe` | enum | No | 24h | Timeframe (1h, 24h, 7d) |

### Examples

```bash
# JSON to stdout
playground meta telemetry

# Prometheus format
playground meta telemetry prometheus

# OTLP to file
playground meta telemetry otlp -o ./metrics.otlp
```
