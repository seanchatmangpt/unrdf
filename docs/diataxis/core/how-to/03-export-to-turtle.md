# How-To: Export the Store (and Load It Back)

## Dump to N-Quads

N-Quads is the most portable format for full round-trips because it preserves named graphs.

```javascript
import { createUnrdfStore, namedNode, literal, quad } from '@unrdf/core';

const store = createUnrdfStore();
store.add(
  quad(namedNode('http://example.org/s'), namedNode('http://example.org/p'), literal('hello'))
);

const nquads = store.dump({ format: 'application/n-quads' });
// '<http://example.org/s> <http://example.org/p> "hello" .\n'
console.log(nquads);
```

## Dump to TriG

TriG preserves named graph structure with human-readable graph blocks:

```javascript
const trig = store.dump({ format: 'application/trig' });
console.log(trig);
```

## Load from N-Quads

```javascript
const store2 = createUnrdfStore();
store2.load(nquads, { format: 'application/n-quads' });
console.log('Loaded:', store2.size()); // 1
```

## Load from Turtle

Turtle describes triples in the default graph. Named graphs are not supported by Turtle itself;
use TriG for that.

```javascript
const turtleData = `
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  <http://example.org/alice>
    a foaf:Person ;
    foaf:name "Alice" ;
    foaf:age 30 .
`;

const store = createUnrdfStore();
store.load(turtleData, { format: 'text/turtle' });
console.log('Loaded:', store.size()); // 3
```

## Load from N-Triples

```javascript
const ntriples = '<http://example.org/s> <http://example.org/p> "o" .';
store.load(ntriples, { format: 'application/n-triples' });
```

## Round-trip a store

```javascript
const original = createUnrdfStore();
original.add(quad(namedNode('http://s'), namedNode('http://p'), literal('v')));

const serialized = original.dump({ format: 'application/n-quads' });

const copy = createUnrdfStore();
copy.load(serialized, { format: 'application/n-quads' });

console.log(copy.size() === original.size()); // true
```

## Convert quads to N-Triples (canonicalization path)

The `toNTriples` utility serializes an array of quads to the N-Triples text format. It strips
the graph component from each quad.

```javascript
import { createStore, addQuad, namedNode, literal, FOAF, getQuads, toNTriples } from '@unrdf/core';

const store = createStore();
addQuad(store, {
  subject: namedNode('http://example.org/alice'),
  predicate: FOAF.name,
  object: literal('Alice'),
  graph: { value: '', termType: 'DefaultGraph' },
});

const quads = getQuads(store);
const ntriples = await toNTriples(quads);
console.log(ntriples);
// <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
```

## Canonicalize to URDNA2015

Canonicalization produces a stable, hash-safe N-Quads string suitable for cryptographic
signing or comparison:

```javascript
import { createStore, addQuad, namedNode, literal, FOAF, canonicalize } from '@unrdf/core';

const store = createStore();
addQuad(store, {
  subject: namedNode('http://example.org/alice'),
  predicate: FOAF.name,
  object: literal('Alice'),
  graph: { value: '', termType: 'DefaultGraph' },
});

const canonical = await canonicalize(store);
console.log(canonical);
```

## Check whether two stores contain the same data

`isIsomorphic` canonicalises both stores and compares the strings. Blank node identifiers are
normalised out, so two stores are isomorphic even if their blank node names differ.

```javascript
import { createStore, addQuad, namedNode, literal, FOAF, isIsomorphic } from '@unrdf/core';

const s1 = createStore();
const s2 = createStore();

const q = {
  subject: namedNode('http://example.org/alice'),
  predicate: FOAF.name,
  object: literal('Alice'),
  graph: { value: '', termType: 'DefaultGraph' },
};

addQuad(s1, q);
addQuad(s2, q);

console.log(await isIsomorphic(s1, s2)); // true
```

## Supported format strings

| Format string             | Description                      |
| ------------------------- | -------------------------------- |
| `'application/n-quads'`   | N-Quads (default for dump/load)  |
| `'application/n-triples'` | N-Triples                        |
| `'text/turtle'`           | Turtle (load only in most cases) |
| `'application/trig'`      | TriG                             |

Check the Oxigraph documentation for the full list of supported formats.

## See also

- Reference: [`UnrdfStore` API](../reference/store-api.md) — `load`, `dump`, `size`.
- Explanation: [Storage backends](../explanation/02-storage-backends.md) — why Oxigraph handles
  serialization rather than N3.js.
