# API & Schema Reference

This document defines the stable data structures used by the UNRDF CLI for configuration, JSON outputs, and error responses.

## JSON Output Schemas

Commands that support `--format json` (e.g., `template query`, `template extract`) return a standardized JSON object.

### Query Result Schema
```typescript
interface QueryResponse {
  version: string;        // Package version (e.g. "26.5.4")
  timestamp: string;      // ISO 8601 timestamp of execution
  sparql_results: Array<Record<string, RdfTerm>>; // Array of result rows
  $rdf: {
    raw: Array<BindingRow>; // Raw bindings with metadata
  };
}

interface RdfTerm {
  type: "uri" | "literal" | "bnode";
  value: string;
  datatype?: string;      // For literals
  language?: string;      // For language-tagged literals
}
```

### Extract Result Schema
```typescript
interface ExtractResponse {
  version: string;
  timestamp: string;
  [predicate: string]: string | number | boolean | Array<any>; // Coerced RDF properties
}
```

---

## Configuration Schema (`unrdf.toml`)

The configuration file follows a strict schema validated by Zod.

### Rule Mode Options
*   **`overwrite`** (Default): Always recreate the file.
*   **`append`**: Add content to the end of existing file.
*   **`skip_existing`**: Do nothing if the file already exists.
*   **`prepend`**: Add content to the start of existing file.

### Metadata Directives
Directives in template frontmatter can override global settings:
*   `force: true`: Always overwrite, ignoring `unless_exists` or `skip_if`.
*   `inject: true`: Enable surgical line-based editing.

---

## Error Response Format

When an error occurs and `--format json` is used, the CLI returns a structured error object.

```json
{
  "status": "error",
  "error": {
    "code": "PARSER_ERROR",
    "message": "Invalid Turtle syntax at line 5",
    "details": {
      "line": 5,
      "column": 12,
      "file": "schema.ttl"
    },
    "fix": "Check for missing period (.) at line end."
  }
}
```

### Common Exit Codes
| Code | Meaning |
| :--- | :--- |
| **0** | Success |
| **1** | General Error (Config, Parser, Template) |
| **2** | No results found (Optional/Contextual) |
| **127** | Command not found |

---

## Stability Promise

*   **Config Stability**: `unrdf.toml` schema will not have breaking changes within the v6.x major version.
*   **JSON Stability**: The top-level `version` and `sparql_results` fields in JSON output are guaranteed for all v6 releases.
*   **Filter Stability**: Core filters (`pascalCase`, `localName`, etc.) will not be removed or renamed without a deprecation cycle.
