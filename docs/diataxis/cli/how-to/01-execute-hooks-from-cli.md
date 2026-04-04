# How to Execute Hooks from the CLI

This guide shows the complete three-step workflow: generate a starter hook config with `hooks template`, validate it with `hooks define`, then execute it against an RDF store with `hooks execute`.

## Prerequisites

- `unrdf` installed
- A Turtle (`.ttl`), N-Quads (`.nq`), or N-Triples (`.nt`) file to use as the RDF store

## Step 1 — Generate a Starter Config

```bash
unrdf hooks template --type generic --output my-hooks.json
```

This writes a `my-hooks.json` file containing one or more example hook definitions. Available `--type` values: `generic`, `fibo`, `security`, `compliance`.

If you omit `--output`, the JSON is printed to stdout.

## Step 2 — Edit the Config

Open `my-hooks.json` and replace the generated IDs and queries with your own. A minimal valid hook looks like this:

```json
[
  {
    "id": "a1b2c3d4-e5f6-7890-abcd-ef1234567890",
    "meta": {
      "name": "check-persons-exist",
      "version": "1.0.0",
      "description": "Fires when the graph contains at least one foaf:Person"
    },
    "condition": {
      "kind": "sparql-ask",
      "query": "PREFIX foaf: <http://xmlns.com/foaf/0.1/> ASK { ?s a foaf:Person }"
    },
    "effect": {
      "kind": "sparql-construct",
      "query": "PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX ex: <http://example.org/> CONSTRUCT { ex:audit ex:ran true } WHERE { ?s a foaf:Person }"
    },
    "priority": 10,
    "enabled": true
  }
]
```

Required fields: `id` (UUID string), `meta.name`, `meta.version`, `condition.kind`, `effect.kind`, `priority` (integer), `enabled` (boolean). See [reference/hook-config-schema.md](../reference/hook-config-schema.md) for all fields.

## Step 3 — Validate the Config

Before executing, confirm the JSON conforms to `KnowledgeHookSchema`:

```bash
unrdf hooks define --config my-hooks.json
```

The CLI parses each hook definition and prints a validation table:

```
Hook Definitions
══════════════════════════════════════════════════
Total Hooks:    1
Valid:          1
Invalid:        0
══════════════════════════════════════════════════

 # | ID            | Name                 | Valid | Conditions | Effect Type
---+---------------+----------------------+-------+------------+--------------
 1 | a1b2c3d4-...  | check-persons-exist  | ✅    | sparql-ask | sparql-construct
```

If any hook is invalid, the error message shows the exact field path that failed Zod validation. Fix the JSON and re-run `hooks define` before proceeding.

## Step 4 — Execute Against an RDF Store

```bash
unrdf hooks execute --store people.ttl --config my-hooks.json
```

To also see per-hook effect details:

```bash
unrdf hooks execute --store people.ttl --config my-hooks.json --show-effects
```

To save results to a JSON file (required for `hooks receipts`):

```bash
unrdf hooks execute --store people.ttl --config my-hooks.json --output results.json
```

The CLI prints a summary:

```
Hook Execution Summary
══════════════════════════════════════════════════
Total Hooks:    1
Satisfied:      1
Not Satisfied:  0
Quads Added:    1
══════════════════════════════════════════════════
```

## Step 5 — Inspect Receipts (Optional)

If you saved output with `--output results.json`, view the receipt chain:

```bash
unrdf hooks receipts --file results.json
unrdf hooks receipts --file results.json --format json
unrdf hooks receipts --file results.json --verify
```

## Troubleshooting

- **Config must be an array** — Wrap your hook object in `[...]`. The config file must be a JSON array even for a single hook.
- **Store file not found** — Use the absolute path or run `unrdf` from the directory containing the file.
- **Schema validation error at `id`** — The `id` field must be a UUID string (e.g. output of `node -e "console.log(crypto.randomUUID())"`).
