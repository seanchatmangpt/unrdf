# How-To: Bulk-Add Quads

Use `bulkAdd` when you need to insert many quads in one call, or `transaction` when all
inserts must succeed atomically.

## Bulk-add with `bulkAdd`

`bulkAdd` inserts an array of quads and increments the store version exactly once, regardless
of how many quads are in the array.

```javascript
import { createUnrdfStore, namedNode, literal, quad } from '@unrdf/core';

const store = createUnrdfStore();

// Build an array of quads from your data source
const quads = data.map(row =>
  quad(
    namedNode(`http://example.org/person/${row.id}`),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal(row.name)
  )
);

store.bulkAdd(quads);
console.log('Inserted:', store.size());
```

`bulkAdd` throws `TypeError: bulkAdd: quads must be an array` if the argument is not an array.

## Remove many quads with `bulkRemove`

```javascript
store.bulkRemove(quads);
console.log('After removal:', store.size()); // 0
```

`bulkRemove` silently skips quads that are not in the store.

## Atomic operations with `transaction`

`transaction` calls your function with the store as argument. If the function throws, the
store is rolled back to the state it had before `transaction` was called.

```javascript
const q1 = quad(
  namedNode('http://example.org/s1'),
  namedNode('http://example.org/p'),
  literal('a')
);
const q2 = quad(
  namedNode('http://example.org/s2'),
  namedNode('http://example.org/p'),
  literal('b')
);

try {
  store.transaction(tx => {
    tx.add(q1);
    tx.add(q2);
    // If this throws, q1 and q2 are rolled back
    if (someConditionFails) {
      throw new Error('condition not met');
    }
  });
} catch (err) {
  // err.message is 'Transaction failed: condition not met'
  // store is back to its pre-transaction state
  console.error(err.message);
}
```

The rollback restores by re-inserting the full snapshot taken at the start of `transaction`.
For very large stores (millions of quads) that snapshot cost is measurable; prefer
`bulkAdd` plus application-level error handling in those cases.

## Insert via SPARQL UPDATE

For programmatic insert when the data is already expressed as IRI strings:

```javascript
store.update(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  INSERT DATA {
    <http://example.org/carol> foaf:name "Carol" .
    <http://example.org/carol> a foaf:Person .
  }
`);
```

SPARQL UPDATE accepts a `baseIri` option for resolving relative IRIs:

```javascript
store.update(`INSERT DATA { <alice> <http://xmlns.com/foaf/0.1/name> "Alice" . }`, {
  baseIri: 'http://example.org/',
});
// Inserts <http://example.org/alice> foaf:name "Alice"
```

## Initialize the store with data at construction time

Pass an array of quads directly to `createUnrdfStore` to skip the initial `bulkAdd`:

```javascript
const initial = [
  quad(namedNode('http://example.org/s'), namedNode('http://example.org/p'), literal('o')),
];
const store = createUnrdfStore(initial);
console.log(store.size()); // 1
```

## See also

- Reference: [`UnrdfStore` API](../reference/store-api.md) — complete method signatures.
- How-To: [Run SPARQL queries](./02-run-sparql-queries.md).
