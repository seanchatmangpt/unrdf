# Protocol Specifications

> State machines and message flows for Playground CLI operations

```json-ld
{
  "@context": {
    "proto": "urn:playground:protocols:",
    "rdfs": "http://www.w3.org/2000/01/rdf-schema#"
  },
  "@id": "urn:playground:protocols:v1.0.0",
  "@type": "proto:ProtocolSpecification"
}
```

## 1. Papers Generation Protocol

### State Machine

```
                    +--------+
                    |  INIT  |
                    +----+---+
                         |
                         v
              +----------+-----------+
              |   PARSE_CLI_ARGS     |
              +----------+-----------+
                         |
            +------------+------------+
            |                         |
            v                         v
    +-------+-------+         +-------+-------+
    | VALID_ARGS    |         | INVALID_ARGS  |
    +-------+-------+         +-------+-------+
            |                         |
            v                         v
    +-------+-------+         +-------+-------+
    | VALIDATE_ZOD  |         |    ERROR      |<----+
    +-------+-------+         +---------------+     |
            |                                       |
            +-------------------+                   |
            |                   |                   |
            v                   v                   |
    +-------+-------+   +-------+-------+          |
    |  ZOD_VALID    |   |  ZOD_INVALID  |----------+
    +-------+-------+   +---------------+          |
            |                                       |
            v                                       |
    +-------+-------+                               |
    | RESOLVE_FAMILY|                               |
    +-------+-------+                               |
            |                                       |
            +-------------------+                   |
            |                   |                   |
            v                   v                   |
    +-------+-------+   +-------+-------+          |
    | FAMILY_FOUND  |   | FAMILY_UNKNOWN|----------+
    +-------+-------+   +---------------+
            |
            v
    +-------+-------+
    | BUILD_CONTEXT |
    +-------+-------+
            |
            v
    +-------+-------+
    | RENDER_TEMPLATE|
    +-------+-------+
            |
            +-------------------+
            |                   |
            v                   v
    +-------+-------+   +-------+-------+
    | RENDER_SUCCESS|   | RENDER_ERROR  |----------> ERROR
    +-------+-------+   +---------------+
            |
            v
    +-------+-------+
    | OUTPUT_RESULT |
    +-------+-------+
            |
            +-------------------+
            |                   |
            v                   v
    +-------+-------+   +-------+-------+
    | WRITE_FILE    |   |  STDOUT       |
    +-------+-------+   +-------+-------+
            |                   |
            v                   |
    +-------+-------+          |
    | FILE_WRITTEN  |          |
    +-------+-------+          |
            |                   |
            +--------+----------+
                     |
                     v
              +------+------+
              |    DONE     |
              +-------------+
```

### State Definitions

| State | Description | Entry Condition | Exit Condition |
|-------|-------------|-----------------|----------------|
| INIT | Protocol entry point | CLI invoked | Args parsed |
| PARSE_CLI_ARGS | Parse citty arguments | INIT complete | Args available |
| VALIDATE_ZOD | Zod schema validation | Args parsed | Validation result |
| RESOLVE_FAMILY | Lookup paper family | Valid input | Family config |
| BUILD_CONTEXT | Build Nunjucks context | Family resolved | Context object |
| RENDER_TEMPLATE | Execute Nunjucks render | Context ready | Rendered output |
| OUTPUT_RESULT | Route to file or stdout | Render complete | Output written |
| DONE | Terminal success state | Output complete | - |
| ERROR | Terminal error state | Any error | - |

### Message Types

```json
{
  "ParsedArgs": {
    "family": "string | undefined",
    "title": "string",
    "author": "string",
    "affiliation": "string | undefined",
    "abstract": "string | undefined",
    "output": "string | undefined",
    "format": "latex | json"
  },
  "ValidatedInput": {
    "family": "imrad | dsr | argument | contribution",
    "title": "string (non-empty)",
    "author": "string (non-empty)",
    "affiliation": "string | undefined",
    "abstract": "string | undefined"
  },
  "FamilyConfig": {
    "name": "string",
    "description": "string",
    "sections": "string[]"
  },
  "TemplateContext": {
    "paper": "ValidatedInput",
    "family": "FamilyConfig",
    "timestamp": "ISO8601"
  },
  "PaperOutput": {
    "id": "string",
    "family": "string",
    "title": "string",
    "authors": "Author[]",
    "sections": "Section[]",
    "createdAt": "ISO8601"
  }
}
```

---

## 2. SPARQL Query Protocol

### State Machine

```
              +--------+
              |  INIT  |
              +----+---+
                   |
                   v
        +----------+-----------+
        |   RESOLVE_QUERY      |
        +----------+-----------+
                   |
      +------------+------------+
      |                         |
      v                         v
+-----+-----+           +-------+-------+
| FROM_ARG  |           |  FROM_FILE    |
+-----+-----+           +-------+-------+
      |                         |
      +-----------+-------------+
                  |
                  v
        +---------+---------+
        |   VALIDATE_SYNTAX |
        +---------+---------+
                  |
      +-----------+-----------+
      |                       |
      v                       v
+-----+-----+         +-------+-------+
| VALID_SPARQL|       | SYNTAX_ERROR  |----> ERROR
+-----+-----+         +---------------+
      |
      v
+-----+-----+
| LOAD_ONTOLOGY |
+-----+-----+
      |
      +-----------+-----------+
      |                       |
      v                       v
+-----+-----+         +-------+-------+
| ONTOLOGY_LOADED |   | LOAD_ERROR    |----> ERROR
+-----+-----+         +---------------+
      |
      v
+-----+-----+
| EXECUTE_QUERY |
+-----+-----+
      |
      +-----------+-----------+
      |                       |
      v                       v
+-----+-----+         +-------+-------+
| RESULTS_READY |     | EXEC_ERROR    |----> ERROR
+-----+-----+         +---------------+
      |
      v
+-----+-----+
| FORMAT_RESULTS |
+-----+-----+
      |
      +-------+-------+-------+
      |       |       |       |
      v       v       v       v
  +---+---+ +-+-+ +---+---+ +-+--+
  | TABLE | |JSON| | CSV | |TURTLE|
  +---+---+ +-+-+ +---+---+ +----+
      |       |       |       |
      +-----------+---+-------+
                  |
                  v
           +------+------+
           |    DONE     |
           +-------------+
```

### Query Execution Protocol

1. **Query Resolution**
   - Check positional argument for inline query
   - Check --file flag for query file path
   - Error if neither provided

2. **Syntax Validation**
   - Parse SPARQL using parser
   - Validate query type (SELECT, ASK, CONSTRUCT, DESCRIBE)
   - Check for prohibited operations (DELETE, INSERT)

3. **Ontology Loading**
   - Load from config `ontology.path`
   - Parse Turtle format
   - Build in-memory store

4. **Query Execution**
   - Execute against store
   - Apply LIMIT from args
   - Collect bindings

5. **Result Formatting**
   - Table: Tabular ASCII output
   - JSON: Array of binding objects
   - CSV: Header + rows
   - Turtle: RDF serialization (CONSTRUCT only)

---

## 3. Template Rendering Protocol

### State Machine

```
         +--------+
         |  INIT  |
         +----+---+
              |
              v
    +---------+---------+
    |  RESOLVE_TEMPLATE |
    +---------+---------+
              |
    +---------+---------+
    |                   |
    v                   v
+---+---+         +-----+-----+
| FOUND |         | NOT_FOUND |----> ERROR
+---+---+         +-----------+
    |
    v
+---+---+
| LOAD_TEMPLATE |
+---+---+
    |
    v
+---+---+
| COMPILE_NUNJUCKS |
+---+---+
    |
    +---------+---------+
    |                   |
    v                   v
+---+---+         +-----+-----+
| COMPILED|       | COMPILE_ERR|----> ERROR
+---+---+         +-----------+
    |
    v
+---+---+
| BUILD_CONTEXT |
+---+---+
    |
    v
+---+---+
| APPLY_FILTERS |
+---+---+
    |
    v
+---+---+
| RENDER |
+---+---+
    |
    +---------+---------+
    |                   |
    v                   v
+---+---+         +-----+-----+
| OUTPUT |        | RENDER_ERR |----> ERROR
+---+---+         +-----------+
    |
    v
+---+---+
| DONE  |
+-------+
```

### Nunjucks Filter Catalog

| Filter | Input | Output | Description |
|--------|-------|--------|-------------|
| `latex_escape` | string | string | Escape LaTeX special chars |
| `section_number` | index | string | Format section number |
| `date_format` | ISO8601 | string | Format date for document |
| `author_list` | Author[] | string | Format author list |
| `bibtex_key` | Paper | string | Generate BibTeX key |

---

## 4. Configuration Protocol

### State Machine

```
         +--------+
         |  INIT  |
         +----+---+
              |
              v
    +---------+---------+
    |  RESOLVE_CONFIG   |
    +---------+---------+
              |
    +---------+---------+
    |                   |
    v                   v
+---+---+         +-----+-----+
| FILE  |         |  DEFAULT  |
| EXISTS|         |  CONFIG   |
+---+---+         +-----+-----+
    |                   |
    v                   |
+---+---+              |
| LOAD  |              |
| FILE  |              |
+---+---+              |
    |                   |
    +--------+----------+
             |
             v
    +--------+--------+
    |  MERGE_WITH_DEFAULTS |
    +--------+--------+
             |
             v
    +--------+--------+
    |  CONFIG_READY   |
    +--------+--------+
             |
    +--------+--------+--------+
    |        |        |        |
    v        v        v        v
+---+---+ +--+--+ +---+---+ +--+--+
|  GET  | | SET | | LIST  | |RESET|
+---+---+ +--+--+ +---+---+ +--+--+
    |        |        |        |
    v        v        v        v
+---+---+ +--+--+ +---+---+ +--+--+
|RETURN | |WRITE| |DISPLAY| |CLEAR|
|VALUE  | |FILE | | ALL   | |FILE |
+---+---+ +--+--+ +---+---+ +--+--+
    |        |        |        |
    +--------+--------+--------+
             |
             v
      +------+------+
      |    DONE     |
      +-------------+
```

### Atomicity Guarantees

| Operation | Atomicity | Isolation | Durability |
|-----------|-----------|-----------|------------|
| config get | Atomic read | Snapshot | N/A |
| config set | Copy-on-write | Read committed | Sync write |
| config list | Atomic read | Snapshot | N/A |
| config reset | Transaction | Serializable | Sync write |

### File Format

```json
{
  "$schema": "urn:playground:config:v1.0.0",
  "author": {
    "name": "string",
    "email": "string",
    "affiliation": "string"
  },
  "output": {
    "format": "latex | json",
    "directory": "string"
  },
  "cli": {
    "verbose": "boolean",
    "color": "boolean"
  },
  "templates": {
    "directory": "string"
  },
  "ontology": {
    "path": "string"
  }
}
```

---

## 5. Error Recovery Protocol

### State Machine

```
         +--------+
         | ERROR  |
         +----+---+
              |
              v
    +---------+---------+
    |  CLASSIFY_ERROR   |
    +---------+---------+
              |
    +---------+---------+---------+
    |         |         |         |
    v         v         v         v
+---+---+ +---+---+ +---+---+ +---+---+
|RECOVERABLE|FATAL |USER_ERR|SYS_ERR|
+---+---+ +---+---+ +---+---+ +---+---+
    |         |         |         |
    v         v         v         v
+---+---+ +---+---+ +---+---+ +---+---+
|SUGGEST | |LOG &  | |DISPLAY| |LOG &  |
|RECOVERY| |EXIT 1 | |HELP   | |EXIT 2 |
+---+---+ +---+---+ +---+---+ +---+---+
    |                   |
    v                   v
+---+---+         +-----+-----+
| RETRY |         | PROMPT    |
| FLOW  |         | USER      |
+-------+         +-----------+
```

### Error Categories

| Category | Exit Code | Recovery | Example |
|----------|-----------|----------|---------|
| USER_INPUT | 1 | Show help | Missing --title |
| VALIDATION | 1 | Show constraints | Invalid date format |
| NOT_FOUND | 1 | Suggest command | Unknown family |
| IO_ERROR | 2 | Check permissions | File write failed |
| SYSTEM | 2 | Check logs | Memory exhausted |

### Recovery Actions

```json
{
  "E_VALIDATION": {
    "action": "showUsage",
    "command": "--help"
  },
  "E_UNKNOWN_FAMILY": {
    "action": "listAlternatives",
    "command": "playground papers list"
  },
  "E_FILE_NOT_FOUND": {
    "action": "suggestCreate",
    "message": "Check path or create file"
  },
  "E_CONFIG_NOT_FOUND": {
    "action": "suggestSet",
    "command": "playground config set <key> <value>"
  }
}
```

---

## Protocol Verification Queries

### Verify State Transitions

```sparql
PREFIX proto: <urn:playground:protocols:>

ASK {
  proto:INIT proto:transitionsTo proto:PARSE_CLI_ARGS .
  proto:VALIDATE_ZOD proto:transitionsTo proto:RESOLVE_FAMILY .
  proto:RENDER_TEMPLATE proto:transitionsTo proto:OUTPUT_RESULT .
}
```

### Verify Error Handling

```sparql
PREFIX proto: <urn:playground:protocols:>

SELECT ?state ?errorTransition
WHERE {
  ?state a proto:State ;
         proto:onError ?errorTransition .
}
```
