# Merge Utils

RDF store merging and set operations utilities.

## Overview

The `merge-utils` module provides utilities for merging RDF stores and performing set operations. It includes functions for union, intersection, difference, symmetric difference, deduplication, and merge statistics.

## Set Operations

### `union(store1, store2, options?)`

Performs union of two stores.

```javascript
import { union } from 'unrdf/utils';

const merged = union(store1, store2, {
  mergeGraphs: true,
  preserveMetadata: true
});
```

**Parameters:**
- `store1` (Store) - First store
- `store2` (Store) - Second store
- `options` (Object, optional) - Merge options

**Returns:** Store

### `intersection(store1, store2, options?)`

Performs intersection of two stores.

```javascript
import { intersection } from 'unrdf/utils';

const common = intersection(store1, store2, {
  matchBy: 'quad',
  includeMetadata: true
});
```

**Parameters:**
- `store1` (Store) - First store
- `store2` (Store) - Second store
- `options` (Object, optional) - Intersection options

**Returns:** Store

### `difference(store1, store2, options?)`

Performs difference of two stores (store1 - store2).

```javascript
import { difference } from 'unrdf/utils';

const diff = difference(store1, store2, {
  matchBy: 'quad',
  preserveOrder: true
});
```

**Parameters:**
- `store1` (Store) - First store
- `store2` (Store) - Second store
- `options` (Object, optional) - Difference options

**Returns:** Store

### `symmetricDifference(store1, store2, options?)`

Performs symmetric difference of two stores.

```javascript
import { symmetricDifference } from 'unrdf/utils';

const symDiff = symmetricDifference(store1, store2, {
  matchBy: 'quad',
  includeMetadata: true
});
```

**Parameters:**
- `store1` (Store) - First store
- `store2` (Store) - Second store
- `options` (Object, optional) - Symmetric difference options

**Returns:** Store

### `complement(store, universe, options?)`

Performs complement of a store relative to a universe.

```javascript
import { complement } from 'unrdf/utils';

const complement = complement(store, universe, {
  matchBy: 'quad',
  preserveOrder: true
});
```

**Parameters:**
- `store` (Store) - Store to complement
- `universe` (Store) - Universe store
- `options` (Object, optional) - Complement options

**Returns:** Store

## Deduplication Functions

### `deduplicate(store, options?)`

Removes duplicate quads from a store.

```javascript
import { deduplicate } from 'unrdf/utils';

const deduplicated = deduplicate(store, {
  matchBy: 'quad',
  preserveOrder: true,
  keepFirst: true
});
```

**Parameters:**
- `store` (Store) - Store to deduplicate
- `options` (Object, optional) - Deduplication options

**Returns:** Store

### `deduplicateBySubject(store, options?)`

Removes duplicate quads by subject.

```javascript
import { deduplicateBySubject } from 'unrdf/utils';

const deduplicated = deduplicateBySubject(store, {
  preserveOrder: true,
  keepFirst: true
});
```

**Parameters:**
- `store` (Store) - Store to deduplicate
- `options` (Object, optional) - Deduplication options

**Returns:** Store

### `deduplicateByPredicate(store, options?)`

Removes duplicate quads by predicate.

```javascript
import { deduplicateByPredicate } from 'unrdf/utils';

const deduplicated = deduplicateByPredicate(store, {
  preserveOrder: true,
  keepFirst: true
});
```

**Parameters:**
- `store` (Store) - Store to deduplicate
- `options` (Object, optional) - Deduplication options

**Returns:** Store

### `deduplicateByObject(store, options?)`

Removes duplicate quads by object.

```javascript
import { deduplicateByObject } from 'unrdf/utils';

const deduplicated = deduplicateByObject(store, {
  preserveOrder: true,
  keepFirst: true
});
```

**Parameters:**
- `store` (Store) - Store to deduplicate
- `options` (Object, optional) - Deduplication options

**Returns:** Store

## Merge Statistics

### `getMergeStats(store1, store2, options?)`

Gets statistics about merging two stores.

```javascript
import { getMergeStats } from 'unrdf/utils';

const stats = getMergeStats(store1, store2, {
  includeDetails: true,
  matchBy: 'quad'
});

console.log(stats);
// {
//   store1Size: 100,
//   store2Size: 150,
//   unionSize: 200,
//   intersectionSize: 50,
//   differenceSize: 50,
//   symmetricDifferenceSize: 100,
//   overlapPercentage: 33.33,
//   details: { ... }
// }
```

**Parameters:**
- `store1` (Store) - First store
- `store2` (Store) - Second store
- `options` (Object, optional) - Statistics options

**Returns:** Object

### `getDeduplicationStats(store, options?)`

Gets statistics about deduplicating a store.

```javascript
import { getDeduplicationStats } from 'unrdf/utils';

const stats = getDeduplicationStats(store, {
  includeDetails: true,
  matchBy: 'quad'
});

console.log(stats);
// {
//   originalSize: 200,
//   deduplicatedSize: 150,
//   duplicatesRemoved: 50,
//   duplicatePercentage: 25.0,
//   details: { ... }
// }
```

**Parameters:**
- `store` (Store) - Store to analyze
- `options` (Object, optional) - Statistics options

**Returns:** Object

## Advanced Merge Operations

### `mergeWithStrategy(store1, store2, strategy, options?)`

Merges stores using a custom strategy.

```javascript
import { mergeWithStrategy } from 'unrdf/utils';

const merged = mergeWithStrategy(store1, store2, (quad1, quad2) => {
  // Custom merge strategy
  return quad1; // or quad2, or a new quad
}, {
  matchBy: 'quad',
  preserveOrder: true
});
```

**Parameters:**
- `store1` (Store) - First store
- `store2` (Store) - Second store
- `strategy` (Function) - Merge strategy function
- `options` (Object, optional) - Merge options

**Returns:** Store

### `mergeByGraph(store1, store2, options?)`

Merges stores by graph.

```javascript
import { mergeByGraph } from 'unrdf/utils';

const merged = mergeByGraph(store1, store2, {
  mergeGraphs: true,
  preserveMetadata: true
});
```

**Parameters:**
- `store1` (Store) - First store
- `store2` (Store) - Second store
- `options` (Object, optional) - Merge options

**Returns:** Store

### `mergeBySubject(store1, store2, options?)`

Merges stores by subject.

```javascript
import { mergeBySubject } from 'unrdf/utils';

const merged = mergeBySubject(store1, store2, {
  mergeGraphs: true,
  preserveMetadata: true
});
```

**Parameters:**
- `store1` (Store) - First store
- `store2` (Store) - Second store
- `options` (Object, optional) - Merge options

**Returns:** Store

### `mergeByPredicate(store1, store2, options?)`

Merges stores by predicate.

```javascript
import { mergeByPredicate } from 'unrdf/utils';

const merged = mergeByPredicate(store1, store2, {
  mergeGraphs: true,
  preserveMetadata: true
});
```

**Parameters:**
- `store1` (Store) - First store
- `store2` (Store) - Second store
- `options` (Object, optional) - Merge options

**Returns:** Store

## Examples

### Basic Set Operations

```javascript
import { 
  union, intersection, difference, 
  symmetricDifference, complement 
} from 'unrdf/utils';

// Union of two stores
const merged = union(store1, store2);
console.log('Union size:', merged.size);

// Intersection of two stores
const common = intersection(store1, store2);
console.log('Intersection size:', common.size);

// Difference (store1 - store2)
const diff = difference(store1, store2);
console.log('Difference size:', diff.size);

// Symmetric difference
const symDiff = symmetricDifference(store1, store2);
console.log('Symmetric difference size:', symDiff.size);

// Complement relative to universe
const complement = complement(store, universe);
console.log('Complement size:', complement.size);
```

### Deduplication Operations

```javascript
import { 
  deduplicate, deduplicateBySubject, 
  deduplicateByPredicate, deduplicateByObject 
} from 'unrdf/utils';

// Remove all duplicates
const deduplicated = deduplicate(store, {
  matchBy: 'quad',
  preserveOrder: true,
  keepFirst: true
});

// Remove duplicates by subject
const bySubject = deduplicateBySubject(store, {
  preserveOrder: true,
  keepFirst: true
});

// Remove duplicates by predicate
const byPredicate = deduplicateByPredicate(store, {
  preserveOrder: true,
  keepFirst: true
});

// Remove duplicates by object
const byObject = deduplicateByObject(store, {
  preserveOrder: true,
  keepFirst: true
});

console.log('Original size:', store.size);
console.log('Deduplicated size:', deduplicated.size);
console.log('By subject size:', bySubject.size);
console.log('By predicate size:', byPredicate.size);
console.log('By object size:', byObject.size);
```

### Merge Statistics

```javascript
import { getMergeStats, getDeduplicationStats } from 'unrdf/utils';

// Get merge statistics
const mergeStats = getMergeStats(store1, store2, {
  includeDetails: true,
  matchBy: 'quad'
});

console.log('Merge Statistics:', mergeStats);
console.log('Store 1 size:', mergeStats.store1Size);
console.log('Store 2 size:', mergeStats.store2Size);
console.log('Union size:', mergeStats.unionSize);
console.log('Intersection size:', mergeStats.intersectionSize);
console.log('Overlap percentage:', mergeStats.overlapPercentage);

// Get deduplication statistics
const dedupStats = getDeduplicationStats(store, {
  includeDetails: true,
  matchBy: 'quad'
});

console.log('Deduplication Statistics:', dedupStats);
console.log('Original size:', dedupStats.originalSize);
console.log('Deduplicated size:', dedupStats.deduplicatedSize);
console.log('Duplicates removed:', dedupStats.duplicatesRemoved);
console.log('Duplicate percentage:', dedupStats.duplicatePercentage);
```

### Advanced Merge Operations

```javascript
import { 
  mergeWithStrategy, mergeByGraph, 
  mergeBySubject, mergeByPredicate 
} from 'unrdf/utils';

// Custom merge strategy
const customMerged = mergeWithStrategy(store1, store2, (quad1, quad2) => {
  // Prefer quad1 if it has more metadata
  if (quad1.graph && !quad2.graph) {
    return quad1;
  }
  // Prefer quad2 if it's more recent
  if (quad2.graph && quad2.graph.value.includes('2024')) {
    return quad2;
  }
  // Default to quad1
  return quad1;
}, {
  matchBy: 'quad',
  preserveOrder: true
});

// Merge by graph
const graphMerged = mergeByGraph(store1, store2, {
  mergeGraphs: true,
  preserveMetadata: true
});

// Merge by subject
const subjectMerged = mergeBySubject(store1, store2, {
  mergeGraphs: true,
  preserveMetadata: true
});

// Merge by predicate
const predicateMerged = mergeByPredicate(store1, store2, {
  mergeGraphs: true,
  preserveMetadata: true
});

console.log('Custom merged size:', customMerged.size);
console.log('Graph merged size:', graphMerged.size);
console.log('Subject merged size:', subjectMerged.size);
console.log('Predicate merged size:', predicateMerged.size);
```

### Batch Merge Operations

```javascript
import { union, intersection, difference } from 'unrdf/utils';

// Merge multiple stores
function mergeMultipleStores(stores) {
  if (stores.length === 0) return new Store();
  if (stores.length === 1) return stores[0];
  
  let result = stores[0];
  for (let i = 1; i < stores.length; i++) {
    result = union(result, stores[i]);
  }
  
  return result;
}

// Find common quads across multiple stores
function findCommonQuads(stores) {
  if (stores.length === 0) return new Store();
  if (stores.length === 1) return stores[0];
  
  let result = stores[0];
  for (let i = 1; i < stores.length; i++) {
    result = intersection(result, stores[i]);
  }
  
  return result;
}

// Find unique quads in each store
function findUniqueQuads(stores) {
  const unique = [];
  
  for (let i = 0; i < stores.length; i++) {
    let uniqueStore = stores[i];
    
    for (let j = 0; j < stores.length; j++) {
      if (i !== j) {
        uniqueStore = difference(uniqueStore, stores[j]);
      }
    }
    
    unique.push(uniqueStore);
  }
  
  return unique;
}

// Test with sample stores
const stores = [store1, store2, store3];
const merged = mergeMultipleStores(stores);
const common = findCommonQuads(stores);
const unique = findUniqueQuads(stores);

console.log('Merged size:', merged.size);
console.log('Common size:', common.size);
console.log('Unique sizes:', unique.map(s => s.size));
```

### Error Handling and Validation

```javascript
import { union, intersection, difference } from 'unrdf/utils';

function safeMerge(store1, store2, operation) {
  try {
    // Validate inputs
    if (!store1 || !store2) {
      throw new Error('Invalid store inputs');
    }
    
    if (store1.size === 0 && store2.size === 0) {
      return new Store();
    }
    
    // Perform operation
    let result;
    switch (operation) {
      case 'union':
        result = union(store1, store2);
        break;
      case 'intersection':
        result = intersection(store1, store2);
        break;
      case 'difference':
        result = difference(store1, store2);
        break;
      default:
        throw new Error(`Unknown operation: ${operation}`);
    }
    
    // Validate result
    if (!result) {
      throw new Error('Merge operation failed');
    }
    
    return result;
  } catch (error) {
    console.error('Merge operation failed:', error.message);
    return new Store(); // Return empty store on error
  }
}

// Use safe merge
const safeMerged = safeMerge(store1, store2, 'union');
console.log('Safe merge result size:', safeMerged.size);
```

### Performance Optimization

```javascript
import { union, deduplicate } from 'unrdf/utils';

// Optimize merge operations for large datasets
function optimizedMerge(store1, store2) {
  // For large stores, consider chunking
  if (store1.size > 10000 || store2.size > 10000) {
    console.log('Large stores detected, using optimized merge');
    
    // Deduplicate first to reduce size
    const dedup1 = deduplicate(store1);
    const dedup2 = deduplicate(store2);
    
    // Then merge
    const merged = union(dedup1, dedup2);
    
    // Final deduplication
    return deduplicate(merged);
  }
  
  // For small stores, use standard merge
  return union(store1, store2);
}

// Use optimized merge
const optimized = optimizedMerge(store1, store2);
console.log('Optimized merge size:', optimized.size);
```

## Performance Notes

- **Set Operations**: Optimized for large datasets
- **Memory Usage**: Efficient memory management for merge operations
- **Deduplication**: Fast duplicate detection and removal
- **Statistics**: Lightweight statistical calculations

## Best Practices

### Merge Operations
- Choose appropriate merge strategies
- Validate input stores
- Handle errors gracefully
- Consider memory usage for large datasets

### Deduplication
- Use appropriate deduplication methods
- Consider performance implications
- Validate deduplication results
- Document deduplication strategies

### Performance Optimization
- Use batch operations for multiple stores
- Implement efficient merge algorithms
- Monitor memory usage
- Consider streaming for very large datasets

## Related Modules

- [Transform Utils](./transform-utils.md) - Data transformation operations
- [Quality Utils](./quality-utils.md) - Data quality assessment
- [Graph Utils](./graph-utils.md) - RDF graph operations
