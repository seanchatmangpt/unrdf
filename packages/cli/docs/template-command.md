# Template Command Reference

The `unrdf template` command provides powerful ontology-driven code generation. It combines the structured data of RDF/SPARQL with the flexibility of Nunjucks templates and Hygen-style file modification directives.

## Subcommands

*   **`generate`**: Main generation command (RDF + .njk -> file).
*   **`list`**: Discover available templates in your project or catalog.
*   **`query`**: Run a SPARQL SELECT and see the context data that would be passed to a template.
*   **`extract`**: Extract all properties for a specific subject URI as JSON (useful for debugging).
*   **`validate`**: Check a template's frontmatter against the schema.
*   **`catalog`**: Show information about the global and project-local template catalogs.

---

## Frontmatter Directives

Templates use YAML frontmatter to control output behavior. These directives are compatible with [Hygen](https://hygen.io).

| Directive | Type | Description |
| :--- | :--- | :--- |
| `to` | `string` | **Required.** The output file path. Supports Nunjucks interpolation. |
| `inject` | `boolean` | Enable injection mode (partial file modification). |
| `before` | `string` | Regex or string anchor. Insert content *before* the match. Requires `inject: true`. |
| `after` | `string` | Regex or string anchor. Insert content *after* the match. Requires `inject: true`. |
| `append` | `boolean` | Append content to the end of the file. |
| `prepend` | `boolean` | Prepend content to the start of the file. |
| `at_line` | `number` | Insert content at a specific line number. Requires `inject: true`. |
| `skip_if` | `string` | Skip if pattern exists in file or if expression is true. Supports regex `/pattern/`. |
| `unless_exists`| `boolean` | Skip generation if the output file already exists. |
| `force` | `boolean` | Overwrite file even if `unless_exists` or `skip_if` would skip it. |
| `chmod` | `string` | Set file permissions (e.g., `"755"` for executables). |
| `sh` | `string` | Shell command to execute after writing the file. |
| `eof_last` | `boolean` | Ensure (true) or prevent (false) a trailing newline at the end of the file. |
| `rdf` | `string` | Path to RDF file to use for this template (relative to template). |
| `sparql` | `string` | SPARQL query to execute to fetch template data. |

---

## SPARQL Context Generation

When a template is rendered, the SPARQL query results are injected into the Nunjucks context.

### Template Variables

*   **`results`** or **`sparql_results`**: An array of objects, one for each row in the SPARQL result set.
*   **`subject`**: The URI of the current subject (in batch mode or when `--subject` is passed).
*   **`now`**: A Javascript `Date` object for the current time.

### Row Structure
Each row in the `results` array contains:
*   Variable names from your `SELECT` clause (e.g., `{{ row.label }}`).
*   **`_localName`** suffix: Automatically provided for URIs (e.g., `{{ row.entity_localName }}`).
*   **`_meta`**: Metadata about the RDF term (datatype, language, termType).

Example SPARQL: `SELECT ?entity ?label WHERE { ?entity rdfs:label ?label }`
Example Context Access: `{{ results[0].label }}` or `{{ results[0].entity_localName }}`

---

## Nunjucks Filters

UNRDF provides several custom filters for common RDF and code generation tasks:

| Filter | Description | Example |
| :--- | :--- | :--- |
| `camelCase` | Convert string to camelCase | `{{ "user-name" \| camelCase }}` -> `userName` |
| `pascalCase`| Convert string to PascalCase | `{{ "user-name" \| pascalCase }}` -> `UserName` |
| `snakeCase` | Convert string to snake_case | `{{ "UserName" \| snakeCase }}` -> `user_name` |
| `kebabCase` | Convert string to kebab-case | `{{ "UserName" \| kebabCase }}` -> `user-name` |
| `localName` | Extract local part of a URI | `{{ "http://ex.org/User" \| localName }}` -> `User` |
| `namespace` | Extract namespace part of URI | `{{ "http://ex.org/User" \| namespace }}` -> `http://ex.org/` |
| `zodType` | Map RDF datatype to Zod type | `{{ row.prop_meta.datatype \| zodType }}` |
| `date` | Format dates using moment-style | `{{ now \| date("YYYY") }}` |

---

## Batch Mode

Use `--batch` to generate multiple files from a single template—one file for each instance of a class.

### How it works:
1.  Provide a `--class-uri` (e.g., `owl:Class`).
2.  Your SPARQL query should include a `?subject` placeholder.
3.  UNRDF finds all instances of the class and executes the query once per instance, replacing `?subject` with the actual instance URI.
4.  The `to:` directive in your frontmatter should use a dynamic path (e.g., `to: "src/{{ label \| pascalCase }}.ts"`).

---

## Advanced Examples

### Multiple Output Files (Dynamic Path)
```yaml
---
to: src/services/{{ label | pascalCase }}Service.ts
---
export class {{ label | pascalCase }}Service {}
```

### Injecting into an Entry Point
```yaml
---
to: src/index.ts
inject: true
after: "// AUTO-GENERATED EXPORTS"
skip_if: "export * from './{{ label }}'"
---
export * from './{{ label }}';
```

### Conditional Generation
```yaml
---
to: docs/{{ label }}.md
skip_if: isDraft
---
# {{ label }}
This file is only generated if isDraft is false.
```

## Error Handling

*   **Zero results**: If a SPARQL query returns no results, the command will notify you. In batch mode, no files will be created for that specific instance.
*   **Invalid Frontmatter**: Frontmatter is strictly validated. Use `unrdf template validate <path>` to check your syntax.
*   **Nunjucks Errors**: Detailed line and column information is provided for template syntax errors, along with suggested fixes.
