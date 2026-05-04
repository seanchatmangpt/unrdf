# UNRDF Migration Guide

This guide covers the transition between major versions of the UNRDF CLI and migration paths from other template engines.

## v5 → v6 Upgrade Path

Version 6 introduces the `unrdf sync` command as the primary project-wide generation engine, replacing the legacy `ggen` command.

### 1. Configuration File
*   **Rename**: `ggen.toml` is now officially `unrdf.toml`. Both are supported, but `unrdf.toml` is preferred.
*   **Rule Schema**: The rules array has moved under the generation section.
    *   *Old*: `[[rules]]`
    *   *New*: `[[generation.rules]]`
*   **Default Output**: The default output directory has changed from current directory to `lib/`. Use `output_dir = "."` to maintain old behavior.

### 2. SPARQL Variable Prefixing
To keep the Nunjucks context clean, v6 recommends prefixing internal-only SPARQL variables with an underscore (e.g., `SELECT ?_internal ?label`). Variables starting with `_` are excluded from the top-level template context but remain available in `sparql_results`.

### 3. Template Directives
*   `skipIf` has been aliased to `skip_if` (snake_case) for Hygen parity.
*   `lineAt` has been aliased to `at_line`.

---

## Template Adoption Guide

If you are coming from other templating environments, here is how UNRDF concepts map:

### For Hygen Users
UNRDF is "Hygen for RDF". All your favorite directives (`inject`, `before`, `after`, `skip_if`) work exactly the same way. The key difference is that instead of a shell-based generator, you use SPARQL to pull data from your knowledge graph.

### For Nunjucks Users
UNRDF uses standard Nunjucks. You can use all built-in tags (`if`, `for`, `set`, `include`, `import`) and blocks. UNRDF adds specialized filters like `localName`, `pascalCase`, and `zodType`.

### For EJS/ERB Users
Nunjucks uses `{% ... %}` for logic and `{{ ... }}` for variables, which is more readable than `<% ... %>`. Most EJS logic can be ported 1:1 to Nunjucks tags.

---

## Feature Comparison Matrix

| Feature | v5 (Legacy) | v6 (Stable) | Notes |
| :--- | :--- | :--- | :--- |
| **Project-wide Sync** | ⚠️ Partial | ✅ Full | Orchestrator handles dependencies |
| **Hygen Parity** | ❌ No | ✅ Full | `inject`, `chmod`, `sh` supported |
| **SPARQL Engine** | N3.js | Oxigraph (WASM) | 100x performance increase |
| **RDF-star Support** | ❌ No | ✅ Yes | Quoted triples and annotations |
| **Watch Mode** | ❌ No | ✅ Yes | Automatic re-generation on changes |
| **MCP Integration** | ❌ No | ✅ Yes | Integrated AI Agent tools |

---

## Deprecated Features

*   **`ggen` binary**: Use `unrdf sync` instead. The `ggen` binary will be removed in v7.
*   **Top-level `[[rules]]`**: Use `[[generation.rules]]`.
*   **`turtleData` frontmatter**: Use `rdf:` or `turtle:` for clarity.
