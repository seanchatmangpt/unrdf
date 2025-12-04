# Reactive Graphs Example

Interactive Vue 3 application demonstrating reactive RDF graph management with @unrdf/composables.

## Features

- **Reactive Store**: Real-time updates when quads are added/removed
- **Interactive UI**: Add, remove, and clear triples through a web interface
- **Graph Statistics**: Live display of quad count, subjects, and predicates
- **Sample Data**: Load example FOAF data with one click
- **Type Safety**: Full TypeScript support via JSDoc

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

### useGraph Composable

The `useGraph` composable provides reactive access to an N3 Store:

```javascript
const { store, quads, quadCount, subjects, predicates, objects } = useGraph();
```

**Reactive Properties:**
- `store`: Ref to N3 Store instance
- `quads`: Computed array of all quads in store
- `quadCount`: Computed total number of quads
- `subjects`: Computed array of unique subjects
- `predicates`: Computed array of unique predicates
- `objects`: Computed array of unique objects

### Component Structure

**App.vue** - Main application component:
- Add triple form with subject/predicate/object inputs
- Graph statistics display
- Interactive table of all quads
- Remove individual quads
- Clear all quads
- Load sample FOAF data

## Key Concepts

### Reactive Updates

When you call `store.value.addQuad(quad)`, Vue's reactivity system automatically:
1. Detects the store mutation
2. Updates `quads` computed property
3. Re-renders the UI with new data

### Term Formatting

RDF terms are formatted for display:
- **Named Nodes**: Full URI (e.g., `http://example.org/alice`)
- **Literals**: Quoted with optional datatype (e.g., `"42"^^xsd:integer`)
- **Blank Nodes**: Node identifier (e.g., `_:b0`)

### Sample Data

The example includes FOAF (Friend of a Friend) sample data:
- Person entities with names and ages
- Social connections via `foaf:knows`
- Typed literals (strings, integers)

## Testing

Tests cover:
- Store creation and initialization
- Adding/removing quads
- Querying subjects, predicates, objects
- Term formatting and display
- Reactive updates

Run tests with:
```bash
pnpm test
```

## Learn More

- [UNRDF Composables Documentation](../../README.md)
- [Vue 3 Composition API](https://vuejs.org/guide/extras/composition-api-faq.html)
- [N3.js Documentation](https://github.com/rdfjs/N3.js)
- [RDF 1.1 Concepts](https://www.w3.org/TR/rdf11-concepts/)
