# unrdf sync CLI Reference

> **Document Type**: Reference
> **Audience**: CLI users and automation scripts
> **Package**: @unrdf/cli
> **Source**: `/home/user/unrdf/packages/cli/src/cli/commands/sync.mjs`

---

## Synopsis

```
unrdf sync [options]
```

Generates synchronized code artifacts from RDF ontology based on rules defined in a `ggen.toml` configuration file.

---

## Options

| Option | Alias | Type | Default | Description |
|--------|-------|------|---------|-------------|
| `--config` | - | string | `ggen.toml` | Path to ggen.toml configuration file |
| `--dry-run` | - | boolean | `false` | Preview changes without writing files |
| `--verbose` | `-v` | boolean | `false` | Enable verbose output |
| `--watch` | `-w` | boolean | `false` | Watch ontology and template files for changes |
| `--force` | `-f` | boolean | `false` | Overwrite existing files without prompting |
| `--rule` | - | string | - | Run only the specified rule by name |
| `--output` | - | string | `text` | Output format: `text` or `json` |

### Option Details

#### `--config`

Specifies the path to the configuration file. Relative paths are resolved from the current working directory.

```bash
unrdf sync --config ./config/ggen.toml
unrdf sync --config /absolute/path/ggen.toml
```

#### `--dry-run`

Previews the files that would be generated without writing them to disk. Outputs `[DRY RUN]` prefix for each file.

```bash
unrdf sync --dry-run
```

#### `--verbose`

Enables detailed output including:
- Configuration path and project name
- SPARQL query result counts
- Watch paths (when combined with `--watch`)
- Full stack traces on errors

```bash
unrdf sync --verbose
unrdf sync -v
```

#### `--watch`

Continuously monitors ontology files and templates for changes. Automatically re-runs generation when modifications are detected.

- Debounce interval: 300ms
- Monitors: config file, ontology source, template files
- Exit: `Ctrl+C` (SIGINT)

```bash
unrdf sync --watch
unrdf sync -w
```

#### `--force`

Overwrites existing files without prompting. By default, the command respects the `mode` setting in each rule.

```bash
unrdf sync --force
unrdf sync -f
```

#### `--rule`

Executes only the rule with the specified name. All other rules are skipped regardless of their `enabled` status.

```bash
unrdf sync --rule js-classes
unrdf sync --rule zod-schemas
```

#### `--output`

Controls the output format.

| Value | Description |
|-------|-------------|
| `text` | Human-readable colored output (default) |
| `json` | Machine-readable JSON output |

```bash
unrdf sync --output json
unrdf sync --output text
```

JSON output structure:

```json
{
  "success": true,
  "results": [
    {
      "rule": "js-classes",
      "path": "/path/to/output.mjs",
      "status": "success",
      "duration": 12.5,
      "bytes": 2048
    }
  ],
  "totalDuration": 156.2,
  "metrics": {
    "rulesProcessed": 3,
    "filesGenerated": 3,
    "filesSkipped": 0,
    "errors": 0,
    "totalTriples": 1250,
    "totalBytes": 8192
  }
}
```

---

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success - all rules executed without errors |
| 1 | Error - configuration not found, parse error, SPARQL error, template error, or rule execution failure |

---

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `NO_COLOR` | Disables colored output when set to any value | - |
| `ONTOLOGY_PATH` | Override ontology source path (via config substitution) | - |
| `OUTPUT_DIR` | Override output directory (via config substitution) | - |

### Color Behavior

Colors are automatically disabled when:
- `NO_COLOR` environment variable is set
- Standard output is not a TTY (e.g., piped to file)

---

## Execution Phases

The sync command executes in three phases:

### Phase 1: Loading Configuration

Parses and validates `ggen.toml`. Exits with code 1 if:
- Configuration file not found
- TOML parse error
- Schema validation failure

### Phase 2: Loading Ontology

Loads RDF ontology into an in-memory Oxigraph store. Reports triple count on success.

Supported formats:
- `turtle` (`.ttl`)
- `ntriples` (`.nt`)
- `nquads` (`.nq`)
- `jsonld` (`.jsonld`, `.json`)
- `rdfxml` (`.rdf`, `.xml`, `.owl`)
- `trig` (`.trig`)

### Phase 3: Processing Rules

For each enabled rule:
1. Execute SPARQL query against loaded ontology
2. Render Handlebars template with query results
3. Write output file (unless `--dry-run`)

---

## Examples

### Basic Usage

```bash
# Run with default configuration (ggen.toml in current directory)
unrdf sync

# Specify custom configuration file
unrdf sync --config ./config/api-gen.toml
```

### Dry Run

```bash
# Preview what would be generated
unrdf sync --dry-run

# Preview with verbose output
unrdf sync --dry-run --verbose
```

### Single Rule Execution

```bash
# Run only the 'js-classes' rule
unrdf sync --rule js-classes

# Run single rule with verbose output
unrdf sync --rule zod-schemas -v
```

### Watch Mode

```bash
# Start watch mode
unrdf sync --watch

# Watch with verbose output (shows file change events)
unrdf sync -w -v
```

### JSON Output

```bash
# Output as JSON for scripting
unrdf sync --output json > result.json

# Parse result with jq
unrdf sync --output json | jq '.metrics.filesGenerated'
```

### Combined Options

```bash
# Force overwrite, verbose, specific rule
unrdf sync --force --verbose --rule ts-types

# Short form
unrdf sync -f -v --rule ts-types
```

---

## Metrics Output

On completion, the command reports:

| Metric | Description |
|--------|-------------|
| Rules processed | Number of rules executed |
| Files generated | Number of files written to disk |
| Errors | Number of rules that failed |
| Duration | Total execution time |

Example output:

```
Sync complete!
   Rules processed: 4
   Files generated: 4
   Duration: 156.23ms
```

---

## Error Messages

| Error | Cause | Resolution |
|-------|-------|------------|
| `Configuration file not found: <path>` | ggen.toml does not exist at specified path | Create configuration file or specify correct path |
| `TOML parse error` | Invalid TOML syntax | Check ggen.toml syntax |
| `Ontology file not found` | Source file in `[ontology]` section missing | Verify ontology path |
| `SPARQL parse error` | Invalid SPARQL query in rule | Check query syntax |
| `Template not found` | Template file missing | Verify template path |

---

## See Also

- [ggen.toml Configuration Reference](/home/user/unrdf/docs/diataxis/reference/sync-config.md)
- [CLI Commands Reference](/home/user/unrdf/docs/diataxis/reference/cli-commands.md)
