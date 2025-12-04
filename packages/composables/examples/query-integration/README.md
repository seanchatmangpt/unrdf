# Query Integration Example

Interactive SPARQL query editor with reactive results display using @unrdf/composables.

## Features

- **SPARQL Query Editor**: Execute SELECT, CONSTRUCT, and ASK queries
- **Real-time Results**: Automatic UI updates when results change
- **Query Templates**: Pre-built sample queries for common patterns
- **Performance Metrics**: Execution time tracking
- **Interactive Data Store**: Load and manage RDF data
- **Reactive Updates**: Auto-refresh queries when store changes

## Usage

```bash
# Install dependencies
pnpm install

# Start development server
pnpm dev

# Run tests
pnpm test

# Build for production
pnpm build
```

## Architecture

### useQuery Composable

The `useQuery` composable provides SPARQL query execution:

```javascript
const { execute, isExecuting, results, error } = useQuery(store);
```

**Methods:**
- `execute(query)`: Execute SPARQL query string
- `isExecuting`: Ref indicating query execution state
- `results`: Reactive query results
- `error`: Reactive error state

### Query Result Types

**SELECT queries** return bindings:
```javascript
{
  type: 'bindings',
  variables: ['person', 'name', 'age'],
  data: [
    { person: NamedNode, name: Literal, age: Literal },
    // ...
  ]
}
```

**CONSTRUCT queries** return quads:
```javascript
{
  type: 'quads',
  data: [Quad, Quad, ...]
}
```

**ASK queries** return boolean:
```javascript
{
  type: 'boolean',
  data: true
}
```

## Component Structure

**QueryComponent.vue** - Main application:
- SPARQL query editor with syntax highlighting
- Sample query templates (SELECT, CONSTRUCT, ASK)
- Execute and clear controls
- Results display with type-specific formatting
- RDF data store management
- Load/clear sample data

## Key Concepts

### Reactive Query Execution

When you execute a query:
1. `execute(query)` is called with SPARQL string
2. Query is parsed and executed against store
3. Results are returned in type-specific format
4. UI automatically updates with new results

### Query Types

**SELECT** - Retrieve variable bindings:
```sparql
SELECT ?person ?name
WHERE {
  ?person foaf:name ?name .
}
```

**CONSTRUCT** - Build new RDF graph:
```sparql
CONSTRUCT {
  ?person a foaf:Person .
}
WHERE {
  ?person foaf:name ?name .
}
```

**ASK** - Boolean query:
```sparql
ASK {
  ?person foaf:knows ?friend .
}
```

### Auto-refresh

The composable watches the store for changes:
- When store is updated, queries can be re-executed
- Results automatically update in UI
- No manual refresh needed

## Sample Data

Includes FOAF (Friend of a Friend) sample data:
- 3 people (Alice, Bob, Charlie)
- Names and ages
- Social connections via `foaf:knows`

## Testing

Tests cover:
- Store creation and data loading
- Pattern-based queries
- Predicate filtering
- Typed literals
- FOAF relationships
- Term formatting

Run tests with:
```bash
pnpm test
```

## Learn More

- [UNRDF Composables Documentation](../../README.md)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [Vue 3 Composition API](https://vuejs.org/guide/extras/composition-api-faq.html)
- [N3.js Documentation](https://github.com/rdfjs/N3.js)
