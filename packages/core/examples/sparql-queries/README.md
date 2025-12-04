# SPARQL Query Examples

This example demonstrates how to query RDF data using SPARQL-like patterns with UNRDF.

## What You'll Learn

- Executing SELECT queries to retrieve data
- Using ASK queries to test conditions
- Building new graphs with CONSTRUCT queries
- Filtering results by criteria
- Formatting query results

## Running the Example

```bash
# Install dependencies
pnpm install

# Run the example
pnpm start

# Run tests
pnpm test
```

## Query Types

### SELECT Queries

Retrieve specific data from the store:

```javascript
// Get all books by an author
const books = getBooksByAuthor(store, 'Alice Smith');

// Get books in a price range
const books = getBooksByPriceRange(store, 30, 45);
```

### ASK Queries

Test if a condition exists:

```javascript
// Check if any books are in stock
const hasStock = hasBooksInStock(store);
// Returns: true or false
```

### CONSTRUCT Queries

Build new graphs from existing data:

```javascript
// Create book summaries
const summaries = constructBookSummaries(store);
// Returns: New store with summary triples
```

## Query Patterns

### Simple Pattern Match

```javascript
const quads = store.getQuads(
  namedNode('http://example.org/book1'),  // specific subject
  null,                                    // any predicate
  null,                                    // any object
  null                                     // any graph
);
```

### Filter by Predicate

```javascript
const titles = store.getQuads(
  null,                                    // any subject
  namedNode('http://purl.org/dc/elements/1.1/title'),
  null,
  null
);
```

### Filter by Object Value

```javascript
const authorBooks = store.getQuads(
  null,
  namedNode('http://purl.org/dc/elements/1.1/creator'),
  literal('Alice Smith'),                  // specific author
  null
);
```

## Example Queries

### Get All Book Titles

```javascript
const titleQuads = store.getQuads(
  null,
  namedNode('http://purl.org/dc/elements/1.1/title'),
  null,
  null
);

const titles = titleQuads.map(q => q.object.value);
```

### Complex Filtering

```javascript
// Get in-stock books under $40
const affordable = allBooks
  .filter(book => {
    const stockQuads = store.getQuads(
      namedNode(book.uri),
      namedNode('http://example.org/vocab/inStock'),
      literal('true', namedNode('http://www.w3.org/2001/XMLSchema#boolean')),
      null
    );
    return stockQuads.length > 0 && book.price < 40;
  });
```

## Result Formatting

```javascript
const results = [
  { title: 'Book 1', price: 29.99 },
  { title: 'Book 2', price: 39.99 }
];

console.log(formatResults(results));
// Output:
// title  | price
// -------|------
// Book 1 | 29.99
// Book 2 | 39.99
```

## Key Concepts

### Query Variables

In SPARQL, `?variable` is used for matching. In N3 store queries, use `null` as a wildcard.

### Triple Patterns

SPARQL patterns like:
```sparql
?book dc:title ?title .
?book dc:creator "Alice Smith" .
```

Translate to:
```javascript
store.getQuads(null, namedNode('dc:title'), null, null);
store.getQuads(null, namedNode('dc:creator'), literal('Alice Smith'), null);
```

### Result Sets

SPARQL result sets map to arrays of objects:
```javascript
[
  { uri: '...', title: '...', author: '...' },
  { uri: '...', title: '...', author: '...' }
]
```

## Next Steps

- Explore RDF parsing in the `rdf-parsing` example
- Learn about advanced SPARQL features in UNRDF documentation
- Try building complex multi-pattern queries


## Testing

Run the test suite:

```bash
pnpm test
```

Run tests in watch mode:

```bash
pnpm test:watch
```

Generate coverage report:

```bash
pnpm test:coverage
```

Test coverage: 80%+ (minimum requirement)
