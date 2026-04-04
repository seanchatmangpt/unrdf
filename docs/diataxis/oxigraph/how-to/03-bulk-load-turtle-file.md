# How to Bulk-Load a Turtle File

This guide shows how to read one or more RDF files from disk and load them into an Oxigraph store using `store.load()`, including loading into named graphs and combining multiple formats.

## When to Use `store.load()` vs `store.add()`

| Situation                                                   | Recommended approach                      |
| ----------------------------------------------------------- | ----------------------------------------- |
| Loading an existing `.ttl`, `.nt`, `.nq`, or `.jsonld` file | `store.load(data, { format })`            |
| Adding individual quads programmatically                    | `store.add(quad)` or `store.addQuad(...)` |
| Bulk-inserting a large array of pre-parsed quads            | Pass to `createStore(quads)` constructor  |

`store.load()` parses the serialised string and inserts all triples in one call. It is faster than calling `store.add()` in a loop because parsing is handled inside the WASM module.

## Step 1 — Read the File and Load

```javascript
import { readFileSync } from 'node:fs';
import { createStore } from '@unrdf/oxigraph';

const store = createStore();

const turtleData = readFileSync('people.ttl', 'utf8');

store.load(turtleData, {
  format: 'text/turtle',
});

console.log(`Loaded ${store.size} triples`);
```

The `format` option is required. See [Supported Format Strings](#supported-format-strings) below for valid values.

## Step 2 — Load Into a Named Graph

By default, triples are loaded into the default graph. To load into a named graph, pass `toNamedGraph`:

```javascript
import { dataFactory } from '@unrdf/oxigraph';

const graphUri = dataFactory.namedNode('http://example.org/graphs/people');

store.load(turtleData, {
  format: 'text/turtle',
  toNamedGraph: graphUri,
});

// Query only that graph
const results = store.query(`
  SELECT ?s ?p ?o
  WHERE {
    GRAPH <http://example.org/graphs/people> {
      ?s ?p ?o
    }
  }
`);

console.log(`Triples in named graph: ${results.length}`);
```

## Step 3 — Load Multiple Files

To combine data from several files, call `store.load()` once per file on the same store:

```javascript
import { readFileSync } from 'node:fs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const store = createStore();
const graph = name => dataFactory.namedNode(`http://example.org/graphs/${name}`);

const files = [
  { path: 'people.ttl', format: 'text/turtle', graphName: 'people' },
  { path: 'organisations.ttl', format: 'text/turtle', graphName: 'orgs' },
  { path: 'links.nq', format: 'application/n-quads', graphName: null }, // nq has graph info embedded
];

for (const { path, format, graphName } of files) {
  const data = readFileSync(path, 'utf8');
  const options = { format };
  if (graphName) options.toNamedGraph = graph(graphName);
  store.load(data, options);
  console.log(`Loaded ${path}`);
}

console.log(`Total triples: ${store.size}`);
```

Note: N-Quads files already carry graph names in each line. Passing `toNamedGraph` when loading N-Quads overrides those graph names, which is usually not what you want — omit it for N-Quads.

## Step 4 — Resolve Relative IRIs with a Base IRI

Turtle files often contain relative IRIs that require a base to resolve correctly. Pass `baseIri` (note: the underlying oxigraph package uses `base_iri` — `@unrdf/oxigraph` accepts both):

```javascript
store.load(turtleData, {
  format: 'text/turtle',
  baseIri: 'http://example.org/',
});
```

Without a base IRI, relative IRIs in the Turtle source will cause a parse error.

## Step 5 — Skip Validation for Trusted Data

For large, pre-validated datasets where parse speed matters more than data integrity checks:

```javascript
store.load(turtleData, {
  format: 'text/turtle',
  unchecked: true, // disable triple-level validation
  noTransaction: true, // disable transactional guarantees for the load
});
```

Use `unchecked` and `noTransaction` only when you trust the source data and need maximum throughput. Invalid triples may silently produce incorrect results.

## Step 6 — Export Back to a File

After manipulation, write the store back to disk:

```javascript
import { writeFileSync } from 'node:fs';

const turtle = store.dump({ format: 'text/turtle' });
writeFileSync('output.ttl', turtle);

const nquads = store.dump({ format: 'application/n-quads' });
writeFileSync('output.nq', nquads);
```

To export only one named graph:

```javascript
const graphData = store.dump({
  format: 'text/turtle',
  fromNamedGraph: dataFactory.namedNode('http://example.org/graphs/people'),
});
writeFileSync('people-export.ttl', graphData);
```

## Supported Format Strings

| Format    | MIME type               | Short aliases |
| --------- | ----------------------- | ------------- |
| Turtle    | `text/turtle`           | `ttl`         |
| TriG      | `application/trig`      | `trig`        |
| N-Triples | `application/n-triples` | `nt`          |
| N-Quads   | `application/n-quads`   | `nq`          |
| JSON-LD   | `application/ld+json`   | `jsonld`      |
| RDF/XML   | `application/rdf+xml`   | `rdf`         |

Both the MIME type string and the short alias are accepted by `store.load()` and `store.dump()`.

## Error Handling

`store.load()` throws if:

- `data` is empty or not a string: `Error: Data must be a non-empty string`
- `options.format` is missing: `Error: Format option is required`
- The data does not parse: `Error: Load operation failed: <parse error>`

Wrap in a try/catch for production use:

```javascript
try {
  store.load(data, { format: 'text/turtle' });
} catch (err) {
  console.error('Failed to load RDF data:', err.message);
  // inspect the source file and format string
}
```

## See Also

- [reference/oxigraph-store-api.md — load() and dump()](../reference/oxigraph-store-api.md#load)
- [reference/configuration-options.md — LoadOptions and DumpOptions](../reference/configuration-options.md#loadoptions)
