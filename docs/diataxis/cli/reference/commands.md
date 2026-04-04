# Command Reference

All commands are invoked as `unrdf <group> <subcommand> [flags]`.

---

## graph

Manage in-memory named RDF graphs. All subcommands accept `--graph <name>`.

| Subcommand     | Synopsis                                            | Notes                                                          |
| -------------- | --------------------------------------------------- | -------------------------------------------------------------- |
| `graph create` | `unrdf graph create --name <name>`                  | Creates an empty named graph                                   |
| `graph load`   | `unrdf graph load --graph <name> --file <path>`     | Formats: Turtle, N-Triples, N-Quads, JSON-LD, RDF/XML, TriG    |
| `graph query`  | `unrdf graph query --graph <name> --query <sparql>` | Supports SELECT, CONSTRUCT, ASK, DESCRIBE; stdout output       |
| `graph export` | `unrdf graph export --graph <name> --format <fmt>`  | Formats: `turtle` `ntriples` `nquads` `jsonld` `rdfxml` `trig` |

---

## context

Manage RDF named graphs as named contexts.

| Subcommand       | Synopsis                             |
| ---------------- | ------------------------------------ |
| `context create` | `unrdf context create --name <name>` |
| `context list`   | `unrdf context list`                 |
| `context delete` | `unrdf context delete --name <name>` |

---

## sparql

```
unrdf sparql --query <sparql> [--file <rdf-file>]
```

`--file` (optional): RDF file to load before querying.

---

## hooks

Knowledge hook management. All subcommands are under `unrdf hooks <subcommand>`.

### hooks execute

Load an RDF store, evaluate each hook's condition, and apply effects for satisfied hooks.

```
unrdf hooks execute --store <file> --config <file> [--output <file>] [--show-effects]
```

| Flag             | Type    | Required | Default | Description                                                           |
| ---------------- | ------- | -------- | ------- | --------------------------------------------------------------------- |
| `--store`        | string  | yes      | —       | RDF store file (Turtle `.ttl`, N-Quads `.nq`, N-Triples `.nt`)        |
| `--config`       | string  | yes      | —       | Hooks config file (JSON array of hook definitions)                    |
| `--output`       | string  | no       | —       | Save execution results to a JSON file (required for `hooks receipts`) |
| `--show-effects` | boolean | no       | `false` | Print per-hook effect details (quads added per hook)                  |

Hooks with `"enabled": false` are skipped. Only `sparql-construct` effects are applied by this command.

### hooks define

Validate a hooks config file against `KnowledgeHookSchema`.

```
unrdf hooks define --config <file> [--validate] [--output <file>]
```

`--config` (required). `--validate` for validation-only mode. `--output` saves metadata JSON.

### hooks template

Generate a starter JSON config for one or more hooks.

```
unrdf hooks template [--type <type>] [--output <file>]
```

`--type` choices: `generic` (default), `fibo`, `security`, `compliance`. `--output` writes to file; omit to print to stdout.

### hooks evaluate-condition

Evaluate a single condition against an RDF store without a full hook config.

```
unrdf hooks evaluate-condition --store <file> --condition <kind> --config <json>
```

| Flag          | Type   | Required | Description                                   |
| ------------- | ------ | -------- | --------------------------------------------- |
| `--store`     | string | yes      | RDF store file                                |
| `--condition` | string | yes      | Condition kind (see `hooks list-conditions`)  |
| `--config`    | string | yes      | Condition parameters as an inline JSON string |

### hooks list-conditions

Print all available condition kinds with descriptions and example configs.

```
unrdf hooks list-conditions
```

No flags. Supported kinds: `sparql-ask`, `sparql-select`, `shacl`, `delta`, `threshold`, `count`, `window`, `n3`, `datalog`.

### hooks receipts

Display the receipt chain from an execution results file.

```
unrdf hooks receipts --file <file> [--format <format>] [--verify]
```

| Flag       | Type    | Required | Default | Description                                            |
| ---------- | ------- | -------- | ------- | ------------------------------------------------------ |
| `--file`   | string  | yes      | —       | JSON results file produced by `hooks execute --output` |
| `--format` | string  | no       | `table` | Output format: `table` or `json`                       |
| `--verify` | boolean | no       | `false` | Verify receipt chain hash integrity                    |

---

## sync

Generate typed code artifacts from an RDF ontology using `.unrdf.toml`.

```
unrdf sync [--config <path>] [--rule <name>] [--dry-run] [--verbose] [--watch]
```

All flags optional. `--config` defaults to `.unrdf.toml`. `--rule` runs a single named generation rule. `--dry-run` previews without writing. `--watch` re-generates on file change.

---

## template

Ad-hoc code generation from a single RDF file and a Nunjucks template.

| Subcommand          | Synopsis                                                                      |
| ------------------- | ----------------------------------------------------------------------------- |
| `template generate` | `unrdf template generate [--template <path>] [--sparql <query>] [<rdf-file>]` |
| `template list`     | `unrdf template list` — list templates under `templates/sync`                 |
| `template query`    | `unrdf template query [<rdf-file>]` — run SELECT and print template context   |

---

## Environment Variables

| Variable               | Description                                       |
| ---------------------- | ------------------------------------------------- |
| `UNRDF_DEFAULT_GRAPH`  | Default graph name used when `--graph` is omitted |
| `UNRDF_DEFAULT_FORMAT` | Default export format                             |
| `UNRDF_CONFIG_PATH`    | Default path to `.unrdf.toml`                     |
