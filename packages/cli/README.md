# @unrdf/cli

RDF ontology to code generation. Transform your RDF knowledge graphs into typed code artifacts—Zod schemas, OpenAPI specs, JSDoc types, GraphQL schemas—using SPARQL queries and Nunjucks templates.

**Version**: [VERSION] | **Node.js**: >=[VERSION]

## Installation

### Global Install (recommended for CLI usage)

```bash
# Using pnpm (recommended)
pnpm add -g @unrdf/cli

# Using npm
npm install -g @unrdf/cli
```

Verify installation:

```bash
unrdf --version
# Output: [VERSION]
```

---

## Quick Start (5 Minutes)

1. **Initialize Project**
```bash
unrdf sync init
```

2. **Run Generation**
```bash
unrdf sync --config unrdf.toml
```

For a detailed walkthrough, see [Getting Started Guide](docs/GETTING_STARTED.md).

---

## Key Features

### 🚀 RDF-Driven Generation
Query your ontology with SPARQL and use the results directly in Nunjucks templates. Access URI local names, namespaces, and datatypes automatically.

### 🧩 Hygen Parity
Full support for Hygen-style frontmatter directives for surgical file modifications:
- **`inject`**: Enable line-based modification.
- **`before` / `after`**: Inject content relative to string or regex anchors.
- **`at_line`**: Insert at specific line numbers.
- **`skip_if`**: Prevent duplication with regex-based existence checks.
- **`chmod`**: Set file permissions (e.g., `755` for scripts).
- **`sh`**: Run post-generation shell commands.

### 🔄 Smart Injection
Automatic detection of per-row vs. summary rendering. Use `inject: true` to automatically iterate over SPARQL results and update a single file (like an `index.ts` registry).

---

## CLI Usage

### Sync (Project-wide Generation)
```bash
unrdf sync                 # Run all rules in unrdf.toml
unrdf sync --watch         # Regenerate on ontology changes
unrdf sync --dry-run       # Preview changes without writing
```

### Template (Ad-hoc Generation)
```bash
unrdf template generate data.ttl --template service.njk --batch --class-uri owl:Class
```

### WASM Runtimes (AtomVM)
```bash
unrdf atomvm doctor        # Check for erlc and packbeam dependencies
unrdf atomvm build <mod>   # Compile .erl to .avm bytecode
unrdf atomvm execute <avm> # Run bytecode on WASM runtime
```

### Diagnostics
```bash
unrdf doctor               # Health check and auto-fix
```

---

## Documentation

- **[Getting Started](docs/GETTING_STARTED.md)**: 5-minute quickstart.
- **[Sync Command Guide](docs/sync-command.md)**: Configuration reference for `unrdf.toml`.
- **[Template Command Reference](docs/template-command.md)**: Full list of directives and Nunjucks filters.
- **[Migration Guide](docs/MIGRATION.md)**: Upgrading from v5 and adopting from other engines.
- **[API Reference](docs/API.md)**: Stable JSON schemas and configuration specifications.
- **[Doctor Command](docs/doctor-command.md)**: Diagnostic and health checks.
- **[Examples](examples/)**: Complete pipeline and configuration examples.

---

## API Stability

UNRDF is committed to stable interfaces for automated workflows:
*   **Versioned JSON**: All machine-readable outputs include a `version` field for compatibility tracking.
*   **Schema Consistency**: The `unrdf.toml` schema follows Semantic Versioning; no breaking changes in minor/patch releases.
*   **Backward Compatibility**: v6 fully supports legacy `ggen.toml` configurations.
