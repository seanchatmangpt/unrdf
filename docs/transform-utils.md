# Transform Utils

RDF data transformation and format conversion utilities.

## Overview

The `transform-utils` module provides utilities for transforming RDF data between different formats and structures. It includes functions for format conversion, data flattening, denormalization, normalization, and various data transformations.

## Format Conversion Functions

### `toJSONLD(store, options?)`

Converts an RDF store to JSON-LD format.

```javascript
import { toJSONLD } from 'unrdf/utils';

const jsonld = toJSONLD(store, {
  context: {
    '@vocab': 'http://example.org/',
    'foaf': 'http://xmlns.com/foaf/0.1/'
  },
  compact: true
});
```

**Parameters:**
- `store` (Store) - N3 store to convert
- `options` (Object, optional) - Conversion options

**Returns:** Object

### `fromJSONLD(jsonld, options?)`

Converts JSON-LD to an RDF store.

```javascript
import { fromJSONLD } from 'unrdf/utils';

const store = fromJSONLD(jsonld, {
  baseIRI: 'http://example.org/',
  expandContext: true
});
```

**Parameters:**
- `jsonld` (Object) - JSON-LD data
- `options` (Object, optional) - Conversion options

**Returns:** Store

### `toNTriples(store)`

Converts an RDF store to N-Triples format.

```javascript
import { toNTriples } from 'unrdf/utils';

const ntriples = toNTriples(store);
```

**Parameters:**
- `store` (Store) - N3 store to convert

**Returns:** string

### `fromNTriples(ntriples)`

Converts N-Triples to an RDF store.

```javascript
import { fromNTriples } from 'unrdf/utils';

const store = fromNTriples(ntriples);
```

**Parameters:**
- `ntriples` (string) - N-Triples data

**Returns:** Store

### `toCSV(store, options?)`

Converts an RDF store to CSV format.

```javascript
import { toCSV } from 'unrdf/utils';

const csv = toCSV(store, {
  subjects: ['http://example.org/person1'],
  predicates: ['http://xmlns.com/foaf/0.1/name'],
  includeHeaders: true
});
```

**Parameters:**
- `store` (Store) - N3 store to convert
- `options` (Object, optional) - Conversion options

**Returns:** string

### `fromCSV(csv, options?)`

Converts CSV to an RDF store.

```javascript
import { fromCSV } from 'unrdf/utils';

const store = fromCSV(csv, {
  subjectColumn: 'id',
  predicateColumn: 'property',
  objectColumn: 'value',
  baseIRI: 'http://example.org/'
});
```

**Parameters:**
- `csv` (string) - CSV data
- `options` (Object, optional) - Conversion options

**Returns:** Store

## Data Transformation Functions

### `flatten(store, options?)`

Flattens nested RDF structures.

```javascript
import { flatten } from 'unrdf/utils';

const flattened = flatten(store, {
  maxDepth: 3,
  preserveBlankNodes: true
});
```

**Parameters:**
- `store` (Store) - N3 store to flatten
- `options` (Object, optional) - Flattening options

**Returns:** Store

### `denormalize(store, options?)`

Denormalizes RDF data by expanding references.

```javascript
import { denormalize } from 'unrdf/utils';

const denormalized = denormalize(store, {
  expandReferences: true,
  includeInverse: true
});
```

**Parameters:**
- `store` (Store) - N3 store to denormalize
- `options` (Object, optional) - Denormalization options

**Returns:** Store

### `normalize(store, options?)`

Normalizes RDF data by consolidating duplicates.

```javascript
import { normalize } from 'unrdf/utils';

const normalized = normalize(store, {
  mergeDuplicates: true,
  canonicalizeIRIs: true
});
```

**Parameters:**
- `store` (Store) - N3 store to normalize
- `options` (Object, optional) - Normalization options

**Returns:** Store

### `transform(store, transformer)`

Applies a custom transformation function to a store.

```javascript
import { transform } from 'unrdf/utils';

const transformed = transform(store, (quad) => {
  // Custom transformation logic
  return modifiedQuad;
});
```

**Parameters:**
- `store` (Store) - N3 store to transform
- `transformer` (Function) - Transformation function

**Returns:** Store

### `map(store, mapper)`

Maps quads in a store using a mapping function.

```javascript
import { map } from 'unrdf/utils';

const mapped = map(store, (quad) => {
  // Map each quad
  return newQuad;
});
```

**Parameters:**
- `store` (Store) - N3 store to map
- `mapper` (Function) - Mapping function

**Returns:** Store

### `filter(store, predicate)`

Filters quads in a store using a predicate function.

```javascript
import { filter } from 'unrdf/utils';

const filtered = filter(store, (quad) => {
  // Filter condition
  return quad.predicate.value.includes('foaf');
});
```

**Parameters:**
- `store` (Store) - N3 store to filter
- `predicate` (Function) - Filter predicate

**Returns:** Store

### `reduce(store, reducer, initialValue)`

Reduces a store to a single value.

```javascript
import { reduce } from 'unrdf/utils';

const result = reduce(store, (acc, quad) => {
  // Reduction logic
  return acc + 1;
}, 0);
```

**Parameters:**
- `store` (Store) - N3 store to reduce
- `reducer` (Function) - Reduction function
- `initialValue` (any) - Initial value

**Returns:** any

### `groupBy(store, keyFunction)`

Groups quads by a key function.

```javascript
import { groupBy } from 'unrdf/utils';

const grouped = groupBy(store, (quad) => quad.predicate.value);
```

**Parameters:**
- `store` (Store) - N3 store to group
- `keyFunction` (Function) - Key function

**Returns:** Object

### `sort(store, compareFunction)`

Sorts quads in a store.

```javascript
import { sort } from 'unrdf/utils';

const sorted = sort(store, (a, b) => {
  return a.subject.value.localeCompare(b.subject.value);
});
```

**Parameters:**
- `store` (Store) - N3 store to sort
- `compareFunction` (Function) - Comparison function

**Returns:** Store

### `unique(store, keyFunction?)`

Removes duplicate quads from a store.

```javascript
import { unique } from 'unrdf/utils';

const uniqueStore = unique(store, (quad) => quad.toString());
```

**Parameters:**
- `store` (Store) - N3 store to deduplicate
- `keyFunction` (Function, optional) - Key function for uniqueness

**Returns:** Store

### `chunk(store, size)`

Splits a store into chunks of specified size.

```javascript
import { chunk } from 'unrdf/utils';

const chunks = chunk(store, 100);
```

**Parameters:**
- `store` (Store) - N3 store to chunk
- `size` (number) - Chunk size

**Returns:** Array<Store>

### `batch(store, batchSize, processor)`

Processes a store in batches.

```javascript
import { batch } from 'unrdf/utils';

const results = batch(store, 100, (batch) => {
  // Process each batch
  return processBatch(batch);
});
```

**Parameters:**
- `store` (Store) - N3 store to batch
- `batchSize` (number) - Batch size
- `processor` (Function) - Batch processor

**Returns:** Array<any>

## Examples

### Basic Format Conversion

```javascript
import { 
  toJSONLD, fromJSONLD, toNTriples, fromNTriples,
  toCSV, fromCSV 
} from 'unrdf/utils';

// Convert to JSON-LD
const jsonld = toJSONLD(store, {
  context: {
    '@vocab': 'http://example.org/',
    'foaf': 'http://xmlns.com/foaf/0.1/'
  }
});

// Convert from JSON-LD
const storeFromJsonld = fromJSONLD(jsonld);

// Convert to N-Triples
const ntriples = toNTriples(store);

// Convert from N-Triples
const storeFromNtriples = fromNTriples(ntriples);

// Convert to CSV
const csv = toCSV(store, {
  includeHeaders: true
});

// Convert from CSV
const storeFromCsv = fromCSV(csv, {
  subjectColumn: 'id',
  predicateColumn: 'property',
  objectColumn: 'value'
});
```

### Data Flattening

```javascript
import { flatten } from 'unrdf/utils';

// Flatten nested structures
const flattened = flatten(store, {
  maxDepth: 3,
  preserveBlankNodes: true,
  expandCollections: true
});

console.log('Original store size:', store.size);
console.log('Flattened store size:', flattened.size);
```

### Data Denormalization

```javascript
import { denormalize } from 'unrdf/utils';

// Denormalize by expanding references
const denormalized = denormalize(store, {
  expandReferences: true,
  includeInverse: true,
  maxDepth: 2
});

console.log('Denormalized store size:', denormalized.size);
```

### Data Normalization

```javascript
import { normalize } from 'unrdf/utils';

// Normalize by consolidating duplicates
const normalized = normalize(store, {
  mergeDuplicates: true,
  canonicalizeIRIs: true,
  removeRedundant: true
});

console.log('Normalized store size:', normalized.size);
```

### Custom Transformations

```javascript
import { transform, map, filter } from 'unrdf/utils';

// Custom transformation
const transformed = transform(store, (quad) => {
  // Add timestamp to all quads
  const timestamp = new Date().toISOString();
  return quad(quad.subject, quad.predicate, quad.object, 
    namedNode(`http://example.org/graph/${timestamp}`));
});

// Map quads
const mapped = map(store, (quad) => {
  // Convert all IRIs to lowercase
  if (quad.subject.termType === 'NamedNode') {
    return quad(
      namedNode(quad.subject.value.toLowerCase()),
      quad.predicate,
      quad.object,
      quad.graph
    );
  }
  return quad;
});

// Filter quads
const filtered = filter(store, (quad) => {
  // Keep only foaf properties
  return quad.predicate.value.includes('foaf');
});
```

### Data Reduction and Aggregation

```javascript
import { reduce, groupBy, unique } from 'unrdf/utils';

// Count quads
const count = reduce(store, (acc, quad) => acc + 1, 0);

// Group by predicate
const grouped = groupBy(store, (quad) => quad.predicate.value);

// Remove duplicates
const uniqueStore = unique(store, (quad) => 
  `${quad.subject.value}-${quad.predicate.value}-${quad.object.value}`
);

console.log('Total quads:', count);
console.log('Grouped by predicate:', Object.keys(grouped));
console.log('Unique quads:', uniqueStore.size);
```

### Batch Processing

```javascript
import { chunk, batch } from 'unrdf/utils';

// Split into chunks
const chunks = chunk(store, 100);
console.log('Number of chunks:', chunks.length);

// Process in batches
const results = batch(store, 100, (batch) => {
  // Process each batch
  console.log('Processing batch of size:', batch.size);
  return batch.size;
});

console.log('Batch results:', results);
```

### Complex Data Transformation Pipeline

```javascript
import { 
  transform, map, filter, groupBy, 
  flatten, normalize, unique 
} from 'unrdf/utils';

// Create a transformation pipeline
function transformPipeline(store) {
  // Step 1: Filter relevant quads
  const filtered = filter(store, (quad) => 
    quad.predicate.value.includes('foaf') || 
    quad.predicate.value.includes('schema')
  );
  
  // Step 2: Map to standardize IRIs
  const mapped = map(filtered, (quad) => {
    if (quad.predicate.value.includes('foaf')) {
      return quad(
        quad.subject,
        namedNode(quad.predicate.value.replace('foaf', 'schema')),
        quad.object,
        quad.graph
      );
    }
    return quad;
  });
  
  // Step 3: Flatten nested structures
  const flattened = flatten(mapped, { maxDepth: 2 });
  
  // Step 4: Normalize duplicates
  const normalized = normalize(flattened, { mergeDuplicates: true });
  
  // Step 5: Remove remaining duplicates
  const unique = unique(normalized);
  
  return unique;
}

// Apply pipeline
const transformed = transformPipeline(store);
console.log('Transformed store size:', transformed.size);
```

### Format-Specific Transformations

```javascript
import { toJSONLD, toCSV, toNTriples } from 'unrdf/utils';

// Transform to different formats
const jsonld = toJSONLD(store, {
  context: {
    '@vocab': 'http://example.org/',
    'foaf': 'http://xmlns.com/foaf/0.1/',
    'schema': 'http://schema.org/'
  },
  compact: true,
  flatten: true
});

const csv = toCSV(store, {
  subjects: ['http://example.org/person1', 'http://example.org/person2'],
  predicates: ['http://xmlns.com/foaf/0.1/name', 'http://xmlns.com/foaf/0.1/email'],
  includeHeaders: true,
  format: 'wide'
});

const ntriples = toNTriples(store);

console.log('JSON-LD:', jsonld);
console.log('CSV:', csv);
console.log('N-Triples:', ntriples);
```

### Error Handling and Validation

```javascript
import { transform, validateStore } from 'unrdf/utils';

function safeTransform(store, transformer) {
  try {
    // Validate input
    if (!store || store.size === 0) {
      throw new Error('Empty or invalid store');
    }
    
    // Apply transformation
    const transformed = transform(store, transformer);
    
    // Validate output
    if (!transformed || transformed.size === 0) {
      throw new Error('Transformation resulted in empty store');
    }
    
    return transformed;
  } catch (error) {
    console.error('Transformation failed:', error.message);
    return store; // Return original store on error
  }
}

// Use safe transformation
const safeTransformed = safeTransform(store, (quad) => {
  // Transformation logic
  return quad;
});
```

## Performance Notes

- **Format Conversion**: Optimized for large datasets
- **Memory Usage**: Efficient streaming for large stores
- **Batch Processing**: Configurable batch sizes for memory management
- **Caching**: Built-in caching for repeated transformations

## Best Practices

### Data Transformation
- Use appropriate transformation functions for your use case
- Validate input and output data
- Handle errors gracefully
- Consider memory usage for large datasets

### Format Conversion
- Choose appropriate output formats
- Use consistent namespace prefixes
- Validate converted data
- Consider data loss during conversion

### Performance Optimization
- Use batch processing for large datasets
- Implement efficient transformation functions
- Monitor memory usage
- Use streaming for very large datasets

## Related Modules

- [IO Utils](./io-utils.md) - File I/O operations
- [Merge Utils](./merge-utils.md) - Store merging operations
- [Quality Utils](./quality-utils.md) - Data quality assessment
