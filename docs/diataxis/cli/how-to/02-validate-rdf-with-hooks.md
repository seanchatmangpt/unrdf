# How to Validate RDF with Hooks Evaluate-Condition

`unrdf hooks evaluate-condition` lets you test a single condition against an RDF store without writing a full hook config file. This is useful for debugging queries before embedding them in a hook definition.

## Prerequisites

- `unrdf` installed
- An RDF store file (Turtle, N-Quads, or N-Triples)

## Use Case

You have a Turtle file and want to check whether it contains any `foaf:Person` nodes before building a hook around that fact.

## Step 1 — Identify Your Store File and Condition

Suppose your store file is `people.ttl`:

```turtle
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex:   <http://example.org/> .

ex:alice a foaf:Person ; foaf:name "Alice Smith" .
```

The condition you want to test is a SPARQL ASK query.

## Step 2 — Run evaluate-condition

The `--config` flag takes a JSON string (inline) that provides the condition parameters. The `--condition` flag names the condition kind.

```bash
unrdf hooks evaluate-condition \
  --store people.ttl \
  --condition sparql-ask \
  --config '{"query":"PREFIX foaf: <http://xmlns.com/foaf/0.1/> ASK { ?s a foaf:Person }"}'
```

Expected output:

```
Loading store...
✅ Loaded 2 quads
Parsing condition config...
Evaluating condition (sparql-ask)...

Evaluation Result
══════════════════════════════════════════════════
Condition Type: sparql-ask
Store Quads:    2
Result Type:    boolean
══════════════════════════════════════════════════
Result:         ✅ TRUE
```

## Step 3 — Try Other Condition Kinds

To list all supported condition kinds and their example configs:

```bash
unrdf hooks list-conditions
```

Testing a `sparql-select` condition returns the matching rows instead of a boolean:

```bash
unrdf hooks evaluate-condition \
  --store people.ttl \
  --condition sparql-select \
  --config '{"query":"PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name WHERE { ?s foaf:name ?name }"}'
```

```
Result Count:   1 items
  1. { "name": { "value": "Alice Smith", ... } }
```

## Step 4 — Use the Confirmed Query in a Hook Config

Once you have verified the query returns the expected result, copy the query text into a hook config:

```json
{
  "condition": {
    "kind": "sparql-ask",
    "query": "PREFIX foaf: <http://xmlns.com/foaf/0.1/> ASK { ?s a foaf:Person }"
  }
}
```

Then proceed with `hooks define` and `hooks execute` as described in [how-to/01-execute-hooks-from-cli.md](01-execute-hooks-from-cli.md).

## Notes

- The `--config` value must be valid JSON. Use single quotes around the JSON string in bash to avoid escaping issues.
- On Windows, use double quotes and escape inner quotes: `--config "{\"query\":\"ASK { ?s ?p ?o }\"}"`.
- The condition is evaluated read-only. No quads are added to the store by `evaluate-condition`.
