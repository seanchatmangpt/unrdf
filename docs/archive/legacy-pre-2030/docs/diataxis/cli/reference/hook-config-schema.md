# Hook Config Schema Reference

The hooks config file passed to `hooks execute` and `hooks define` must be a JSON array where every element conforms to `KnowledgeHookSchema` from `@unrdf/hooks`.

## Top-Level Structure

```json
[
  {
    /* hook definition */
  },
  {
    /* hook definition */
  }
]
```

The file must be a JSON array even when it contains only one hook.

## KnowledgeHookSchema Fields

### Required Fields

| Field       | Type          | Default | Description                                                                                             |
| ----------- | ------------- | ------- | ------------------------------------------------------------------------------------------------------- |
| `id`        | string (UUID) | —       | Unique identifier. Must be a valid UUID v4. Generate with `node -e "console.log(crypto.randomUUID())"`. |
| `meta`      | object        | —       | Hook metadata (see below)                                                                               |
| `condition` | object        | —       | Condition that triggers the hook (see below)                                                            |
| `effect`    | object        | —       | Action to take when the condition is satisfied (see below)                                              |
| `priority`  | integer 0–100 | `50`    | Execution order relative to other hooks. Lower numbers run first.                                       |
| `enabled`   | boolean       | `true`  | If `false`, the hook is skipped entirely during `hooks execute`.                                        |

### `meta` Object

| Field         | Type     | Required | Description                                                                             |
| ------------- | -------- | -------- | --------------------------------------------------------------------------------------- |
| `name`        | string   | yes      | Human-readable hook name (1–100 characters)                                             |
| `version`     | string   | yes      | Semantic version string, e.g. `"1.0.0"` (must match `\d+\.\d+\.\d+`)                    |
| `description` | string   | no       | Free-text description (max 1000 characters)                                             |
| `author`      | string   | no       | Author name or identifier                                                               |
| `license`     | string   | no       | License identifier, e.g. `"MIT"`                                                        |
| `tags`        | string[] | no       | Up to 20 tag strings, each 1–50 characters. Used for grouping in `hooks define` output. |

### `condition` Object

| Field   | Type               | Required    | Description                                                               |
| ------- | ------------------ | ----------- | ------------------------------------------------------------------------- |
| `kind`  | string (enum)      | yes         | Condition type — see valid values below                                   |
| `query` | string             | conditional | SPARQL query string (required for `sparql-ask`, `sparql-select`, `count`) |
| `ref`   | object             | conditional | Reference to external resource, e.g. a SHACL shapes file                  |
| `spec`  | object             | conditional | Kind-specific configuration (used by `delta`, `threshold`, `window`)      |
| `rules` | string \| string[] | conditional | Rules text (used by `n3`, `datalog`)                                      |
| `goal`  | string             | conditional | Datalog query goal (used by `datalog`)                                    |

#### Valid `condition.kind` Values

| Kind            | Returns   | Description                                               |
| --------------- | --------- | --------------------------------------------------------- |
| `sparql-ask`    | boolean   | SPARQL ASK query; `true` if the pattern matches           |
| `sparql-select` | binding[] | SPARQL SELECT query; non-empty result set means satisfied |
| `shacl`         | boolean   | SHACL shape validation against a shapes file              |
| `delta`         | boolean   | Detects RDF additions or deletions since last snapshot    |
| `threshold`     | boolean   | Numeric comparison against a property value               |
| `count`         | boolean   | Pattern count comparison                                  |
| `window`        | boolean   | Time-window evaluation                                    |
| `n3`            | boolean   | N3 logic rules evaluation                                 |
| `datalog`       | boolean   | Datalog rule evaluation                                   |

### `effect` Object

| Field   | Type          | Required    | Description                                              |
| ------- | ------------- | ----------- | -------------------------------------------------------- |
| `kind`  | string (enum) | yes         | Effect type — see valid values below                     |
| `query` | string        | conditional | SPARQL CONSTRUCT query (required for `sparql-construct`) |

#### Valid `effect.kind` Values

| Kind               | Description                                                             |
| ------------------ | ----------------------------------------------------------------------- |
| `sparql-construct` | Runs a SPARQL CONSTRUCT query and adds the resulting quads to the store |

Only `sparql-construct` is applied by `hooks execute`. Other effect kinds may be stored in the config for external processors.

### Optional Top-Level Fields

| Field       | Type             | Default | Description                                                                                                  |
| ----------- | ---------------- | ------- | ------------------------------------------------------------------------------------------------------------ |
| `dependsOn` | string[] (UUIDs) | —       | List of hook IDs this hook depends on. Recorded for external orchestration; not enforced by `hooks execute`. |

## Minimal Example

The smallest valid hook definition — only the required fields with defaults used for `priority` and `enabled`:

```json
[
  {
    "id": "f47ac10b-58cc-4372-a567-0e02b2c3d479",
    "meta": {
      "name": "any-triple-exists",
      "version": "1.0.0"
    },
    "condition": {
      "kind": "sparql-ask",
      "query": "ASK { ?s ?p ?o }"
    },
    "effect": {
      "kind": "sparql-construct",
      "query": "CONSTRUCT { <urn:audit:ran> <urn:audit:at> ?now } WHERE { BIND(NOW() AS ?now) }"
    },
    "priority": 50,
    "enabled": true
  }
]
```

## Complete Example

```json
[
  {
    "id": "a1b2c3d4-e5f6-7890-abcd-ef1234567890",
    "meta": {
      "name": "tag-compliant-trades",
      "version": "1.0.0",
      "description": "Adds a compliance timestamp to trades that have a status",
      "tags": ["compliance", "fibo"]
    },
    "condition": {
      "kind": "sparql-ask",
      "query": "PREFIX fibo: <https://spec.edmcouncil.org/fibo/ontology/> ASK { ?t a fibo:Trade ; fibo:hasComplianceStatus ?s }"
    },
    "effect": {
      "kind": "sparql-construct",
      "query": "PREFIX fibo: <https://spec.edmcouncil.org/fibo/ontology/> CONSTRUCT { ?t fibo:verifiedAt ?now } WHERE { ?t a fibo:Trade . BIND(NOW() AS ?now) }"
    },
    "priority": 10,
    "enabled": true
  }
]
```

## Validation

Run `unrdf hooks define --config <file>` to validate all hook definitions and print a table showing which fields failed Zod parsing.

Errors are reported in `field.path: message` format, for example:

```
id: Invalid uuid
meta.version: Required
condition.kind: Invalid enum value
```
