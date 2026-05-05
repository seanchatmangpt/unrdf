# Tutorial 01: Query an RDF Graph

In this tutorial you will load a Turtle file into a named graph, run two SPARQL queries against it, and export the results to JSON-LD. By the end you will have a working end-to-end data pipeline using `unrdf`.

Estimated time: 10 minutes.

## What You Need

- `unrdf` installed globally (`pnpm add -g @unrdf/cli`)
- A text editor

## Step 1 — Create a Turtle File

Create a file called `people.ttl` with this content:

```turtle
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex:   <http://example.org/> .

ex:alice a foaf:Person ;
    foaf:name "Alice Smith" ;
    foaf:age  30 .

ex:bob a foaf:Person ;
    foaf:name "Bob Jones" ;
    foaf:age  25 .
```

## Step 2 — Create a Named Graph

```bash
unrdf graph create --name people
```

The CLI creates an in-memory named graph called `people`. Output:

```
Graph 'people' created successfully.
```

## Step 3 — Load the Turtle File

```bash
unrdf graph load --graph people --file people.ttl
```

The CLI parses `people.ttl` and loads every triple into the `people` graph. Output:

```
Loaded people.ttl into graph 'people' (6 triples).
```

## Step 4 — Run a SELECT Query

```bash
unrdf graph query --graph people --query "SELECT ?name ?age WHERE { ?p foaf:name ?name ; foaf:age ?age } ORDER BY ?age"
```

Expected output (table):

```
name          age
Bob Jones     25
Alice Smith   30
```

## Step 5 — Run a CONSTRUCT Query

CONSTRUCT queries return Turtle. Redirect to a file:

```bash
unrdf graph query --graph people \
  --query "CONSTRUCT { ?p <http://schema.org/name> ?n } WHERE { ?p foaf:name ?n }" \
  > schema-names.ttl
```

`schema-names.ttl` now contains the same names mapped to the `schema:name` predicate.

## Step 6 — Export to JSON-LD

```bash
unrdf graph export --graph people --format jsonld > people.jsonld
```

Open `people.jsonld` to see the full graph in JSON-LD format. Supported formats: `turtle`, `ntriples`, `nquads`, `jsonld`, `rdfxml`, `trig`.

## What You Accomplished

- Created a named graph with `unrdf graph create`
- Loaded a Turtle file with `unrdf graph load`
- Ran SELECT and CONSTRUCT queries with `unrdf graph query`
- Exported data with `unrdf graph export`

## Next Steps

- See [how-to/01-execute-hooks-from-cli.md](../how-to/01-execute-hooks-from-cli.md) to automate knowledge-graph policies with hooks.
- See [reference/commands.md](../reference/commands.md) for all flags.
