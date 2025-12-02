# How-To: Parse RDF Formats

**Problem**: You need to convert between different RDF serialization formats (Turtle, N-Triples, JSON-LD, RDF/XML) or load RDF data from files.

## Solution

UNRDF provides unified parsing and serialization through format-specific functions. All formats convert to/from N3 Store as the common interchange format.

### Parse Turtle/TriG

Turtle is the most human-readable RDF format:

```javascript
import { parseTurtle, toTurtle } from 'unrdf';

const ttl = `
@prefix ex: <http://example.org/> .
@prefix schema: <http://schema.org/> .

ex:alice a schema:Person ;
  schema:name "Alice" ;
  schema:knows ex:bob .

ex:bob a schema:Person ;
  schema:name "Bob" .
`;

// Parse to Store
const store = parseTurtle(ttl);
console.log(`Parsed ${store.size} quads`);

// Serialize back to Turtle
const output = toTurtle(store);
console.log(output);
```

### Parse N-Quads/N-Triples

N-Quads is line-based, ideal for streaming:

```javascript
import { toNQuads } from 'unrdf';

// Serialize to N-Quads
const nquads = toNQuads(store);
console.log(nquads);
// Output:
// <http://example.org/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
// <http://example.org/alice> <http://schema.org/name> "Alice" .
// ...

// Parse N-Quads (use parseTurtle - it handles all N3 formats)
const store2 = parseTurtle(nquads);
```

### Parse JSON-LD

JSON-LD embeds RDF in JSON:

```javascript
import { parseJsonLd, toJsonLd } from 'unrdf';

const jsonLd = {
  "@context": "http://schema.org/",
  "@type": "Person",
  "@id": "http://example.org/alice",
  "name": "Alice",
  "knows": {
    "@id": "http://example.org/bob"
  }
};

// Parse to Store
const store = await parseJsonLd(jsonLd);

// Serialize back to JSON-LD
const output = await toJsonLd(store);
console.log(JSON.stringify(output, null, 2));
```

### Load from Files

Use I/O utilities for file-based workflows:

```javascript
import { readTurtleFile, writeTurtleFile } from 'unrdf/utils';
import { readNQuadsFile, readJsonLdFile } from 'unrdf/utils';

// Read Turtle file
const store = await readTurtleFile('./data.ttl');

// Read N-Quads file
const store2 = await readNQuadsFile('./data.nq');

// Read JSON-LD file
const store3 = await readJsonLdFile('./data.jsonld');

// Write Turtle file
await writeTurtleFile(store, './output.ttl');
```

### Format Conversion Pipeline

Convert between formats:

```javascript
import { readTurtleFile, toJsonLd, writeJsonLdFile } from 'unrdf';

// Turtle → JSON-LD
const store = await readTurtleFile('./input.ttl');
const jsonLd = await toJsonLd(store);
await writeJsonLdFile(jsonLd, './output.jsonld');

// JSON-LD → N-Quads
const store2 = await parseJsonLd(jsonLd);
const nquads = toNQuads(store2);
await fs.writeFile('./output.nq', nquads, 'utf-8');
```

### Composable Parsing

Use composables for parsing within contexts:

```javascript
import { initStore, useTurtle } from 'unrdf';

await initStore();
const turtle = useTurtle();

// Parse Turtle
const store = turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:alice a ex:Person .
`);

// Serialize with options
const output = turtle.serialize(store, {
  prefixes: {
    ex: 'http://example.org/',
    schema: 'http://schema.org/'
  }
});
```

### Stream Large Files

For large RDF files, use streaming parsers:

```javascript
import { Parser } from 'unrdf/knowledge-engine';
import { createReadStream } from 'fs';

const parser = new Parser({ format: 'Turtle' });
const store = new Store();

await new Promise((resolve, reject) => {
  createReadStream('./large-file.ttl')
    .pipe(parser)
    .on('data', quad => store.addQuad(quad))
    .on('end', resolve)
    .on('error', reject);
});

console.log(`Streamed ${store.size} quads`);
```

## Variations

### Prefixed Names

Use namespace managers for compact IRIs:

```javascript
import { createNamespaceManager, expandPrefix, compactIRI } from 'unrdf/utils';

const ns = createNamespaceManager();
ns.register('ex', 'http://example.org/');
ns.register('schema', 'http://schema.org/');

// Expand prefixed name
const fullIRI = ns.expand('ex:alice');
// → http://example.org/alice

// Compact IRI
const prefixed = ns.compact('http://example.org/alice');
// → ex:alice

// Use in Turtle serialization
const ttl = toTurtle(store, {
  prefixes: ns.getPrefixes()
});
```

### Auto-Detect Format

Parse based on file extension:

```javascript
import { readFile } from 'fs/promises';
import { parseTurtle, parseJsonLd } from 'unrdf';

async function parseRDFFile(path) {
  const content = await readFile(path, 'utf-8');

  if (path.endsWith('.ttl') || path.endsWith('.n3')) {
    return parseTurtle(content);
  } else if (path.endsWith('.jsonld') || path.endsWith('.json')) {
    return parseJsonLd(JSON.parse(content));
  } else if (path.endsWith('.nq') || path.endsWith('.nt')) {
    return parseTurtle(content);  // parseTurtle handles N-Quads
  }

  throw new Error(`Unknown format: ${path}`);
}
```

### Batch Processing

Process multiple files in parallel:

```javascript
import { glob } from 'glob';
import { readTurtleFile } from 'unrdf/utils';
import { mergeStores } from 'unrdf/utils';

// Find all Turtle files
const files = await glob('./data/**/*.ttl');

// Parse in parallel
const stores = await Promise.all(
  files.map(file => readTurtleFile(file))
);

// Merge into single store
const merged = mergeStores(...stores);
console.log(`Merged ${merged.size} quads from ${files.length} files`);
```

### Error Handling

Handle parsing errors gracefully:

```javascript
import { parseTurtle } from 'unrdf';

function safeParseTurtle(ttl, fallback = null) {
  try {
    return parseTurtle(ttl);
  } catch (err) {
    console.error('Parse error:', err.message);
    return fallback;
  }
}

// Validate before parsing
function validateTurtleSyntax(ttl) {
  const requiredPrefixes = ['@prefix', '@base'];
  const hasPrefix = requiredPrefixes.some(p => ttl.includes(p));

  if (!hasPrefix && !ttl.includes('<')) {
    return { valid: false, error: 'No IRIs or prefixes found' };
  }

  return { valid: true };
}
```

## Related Guides

- [How-To: Manage Namespaces](./manage-namespaces.md) - Namespace utilities and prefixes
- [How-To: Use Composables](./use-composables.md) - High-level parsing APIs
- [How-To: Query with SPARQL](./query-with-sparql.md) - Query parsed data
- [Tutorial: Getting Started](../tutorials/01-getting-started.md) - Basic parsing workflow
