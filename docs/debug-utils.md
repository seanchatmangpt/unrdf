# Debug Utils

Introspection, logging, and performance measurement utilities.

## Overview

The `debug-utils` module provides quick introspection and logging utilities for RDF data, including store statistics, performance measurement, and debugging tools.

## Functions

### `previewQuads(store, n?)`

Pretty prints the first N quads from a store.

```javascript
import { previewQuads } from 'unrdf/utils';

const preview = previewQuads(store, 5);
// Returns: Array of formatted quad strings
console.log(preview);
```

**Parameters:**
- `store` (Store) - RDF store to preview
- `n` (number, optional) - Number of quads to show (default: 5)

**Returns:** Array<string>

### `dumpTurtle(store, engine, prefixes?)`

Console.log Turtle representation for inspection.

```javascript
import { dumpTurtle } from 'unrdf/utils';

await dumpTurtle(store, engine, { ex: 'http://example.org/' });
// Outputs Turtle to console
```

**Parameters:**
- `store` (Store) - RDF store to dump
- `engine` (RdfEngine) - RDF engine for serialization
- `prefixes` (Object, optional) - Prefix mappings

**Returns:** Promise<void>

### `getStoreStats(store)`

Gets comprehensive statistics about a store.

```javascript
import { getStoreStats } from 'unrdf/utils';

const stats = getStoreStats(store);
// Returns: { quadCount, subjectCount, predicateCount, objectCount, graphCount, blankNodeCount, literalCount, namedNodeCount }
```

**Parameters:**
- `store` (Store) - RDF store to analyze

**Returns:** Object with statistics

### `printStoreStats(store)`

Prints store statistics to console.

```javascript
import { printStoreStats } from 'unrdf/utils';

printStoreStats(store);
// Outputs statistics to console
```

**Parameters:**
- `store` (Store) - RDF store to analyze

**Returns:** void

### `deepInspect(obj, options?)`

Deep inspection of objects with circular reference handling.

```javascript
import { deepInspect } from 'unrdf/utils';

const inspection = deepInspect(complexObject, { maxDepth: 3 });
console.log(inspection);
```

**Parameters:**
- `obj` (any) - Object to inspect
- `options` (Object, optional) - Inspection options
  - `maxDepth` (number) - Maximum depth to traverse
  - `showFunctions` (boolean) - Include function definitions

**Returns:** string

### `logDeep(obj, options?)`

Logs deep object inspection to console.

```javascript
import { logDeep } from 'unrdf/utils';

logDeep(complexObject, { maxDepth: 2 });
// Outputs deep inspection to console
```

**Parameters:**
- `obj` (any) - Object to inspect
- `options` (Object, optional) - Inspection options

**Returns:** void

### `timeExecution(fn, label?)`

Times the execution of a function.

```javascript
import { timeExecution } from 'unrdf/utils';

const result = await timeExecution(async () => {
  return await expensiveOperation();
}, 'Expensive Operation');
// Returns: { result: any, duration: number }
```

**Parameters:**
- `fn` (Function) - Function to time
- `label` (string, optional) - Label for timing output

**Returns:** Promise<{result: any, duration: number}>

### `createTimer(label?)`

Creates a performance timer.

```javascript
import { createTimer } from 'unrdf/utils';

const timer = createTimer('My Operation');
// ... do work ...
const duration = timer.end();
console.log(`Operation took ${duration}ms`);
```

**Parameters:**
- `label` (string, optional) - Timer label

**Returns:** Timer object

### `createDebugLogger(level?)`

Creates a debug logger with levels.

```javascript
import { createDebugLogger } from 'unrdf/utils';

const logger = createDebugLogger('info');
logger.info('Processing started');
logger.warn('Warning message');
logger.error('Error occurred');
```

**Parameters:**
- `level` (string, optional) - Log level ('debug', 'info', 'warn', 'error')

**Returns:** Logger object

### `createProgressTracker(total, label?)`

Creates a progress tracker.

```javascript
import { createProgressTracker } from 'unrdf/utils';

const tracker = createProgressTracker(100, 'Processing items');
for (let i = 0; i < 100; i++) {
  // ... do work ...
  tracker.update(i + 1);
}
tracker.complete();
```

**Parameters:**
- `total` (number) - Total number of items
- `label` (string, optional) - Progress label

**Returns:** ProgressTracker object

### `prettyPrintJSON(obj, indent?)`

Pretty prints JSON with custom indentation.

```javascript
import { prettyPrintJSON } from 'unrdf/utils';

const formatted = prettyPrintJSON(complexObject, 2);
console.log(formatted);
```

**Parameters:**
- `obj` (any) - Object to format
- `indent` (number, optional) - Indentation spaces (default: 2)

**Returns:** string

## Timer API

### `createTimer(label?)`

Creates a timer with the following methods:

```javascript
const timer = createTimer('My Operation');

// Start timing
timer.start();

// Get elapsed time without stopping
const elapsed = timer.elapsed();

// End timing and get duration
const duration = timer.end();

// Reset timer
timer.reset();
```

## Logger API

### `createDebugLogger(level?)`

Creates a logger with the following methods:

```javascript
const logger = createDebugLogger('info');

// Log messages
logger.debug('Debug message');
logger.info('Info message');
logger.warn('Warning message');
logger.error('Error message');

// Set log level
logger.setLevel('warn');

// Check if level is enabled
if (logger.isEnabled('debug')) {
  logger.debug('This will only log if debug is enabled');
}
```

## Progress Tracker API

### `createProgressTracker(total, label?)`

Creates a progress tracker with the following methods:

```javascript
const tracker = createProgressTracker(100, 'Processing');

// Update progress
tracker.update(50); // 50% complete

// Get current progress
const progress = tracker.getProgress(); // { current: 50, total: 100, percentage: 50 }

// Complete tracking
tracker.complete();

// Reset tracker
tracker.reset();
```

## Examples

### Store Inspection

```javascript
import { 
  previewQuads, getStoreStats, printStoreStats, dumpTurtle 
} from 'unrdf/utils';

// Preview first 10 quads
const preview = previewQuads(store, 10);
console.log('First 10 quads:');
preview.forEach((quad, index) => {
  console.log(`${index + 1}: ${quad}`);
});

// Get detailed statistics
const stats = getStoreStats(store);
console.log('Store Statistics:');
console.log(`- Total quads: ${stats.quadCount}`);
console.log(`- Unique subjects: ${stats.subjectCount}`);
console.log(`- Unique predicates: ${stats.predicateCount}`);
console.log(`- Unique objects: ${stats.objectCount}`);
console.log(`- Blank nodes: ${stats.blankNodeCount}`);
console.log(`- Literals: ${stats.literalCount}`);

// Print statistics to console
printStoreStats(store);

// Dump as Turtle
await dumpTurtle(store, engine, { 
  ex: 'http://example.org/',
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
});
```

### Performance Measurement

```javascript
import { timeExecution, createTimer } from 'unrdf/utils';

// Time a single operation
const { result, duration } = await timeExecution(async () => {
  return await processStore(store);
}, 'Store Processing');

console.log(`Processing took ${duration}ms`);

// Use timer for multiple operations
const timer = createTimer('Batch Operations');

timer.start();
await operation1();
console.log(`Operation 1: ${timer.elapsed()}ms`);

await operation2();
console.log(`Operation 2: ${timer.elapsed()}ms`);

const totalDuration = timer.end();
console.log(`Total time: ${totalDuration}ms`);
```

### Debug Logging

```javascript
import { createDebugLogger } from 'unrdf/utils';

const logger = createDebugLogger('debug');

async function processRDFData(store) {
  logger.info('Starting RDF data processing');
  
  const stats = getStoreStats(store);
  logger.debug(`Processing ${stats.quadCount} quads`);
  
  try {
    // Process data
    const result = await processStore(store);
    logger.info('Processing completed successfully');
    return result;
  } catch (error) {
    logger.error('Processing failed:', error);
    throw error;
  }
}
```

### Progress Tracking

```javascript
import { createProgressTracker } from 'unrdf/utils';

async function processLargeDataset(items) {
  const tracker = createProgressTracker(items.length, 'Processing items');
  
  for (let i = 0; i < items.length; i++) {
    // Process item
    await processItem(items[i]);
    
    // Update progress
    tracker.update(i + 1);
    
    // Log progress every 10%
    if ((i + 1) % Math.ceil(items.length / 10) === 0) {
      const progress = tracker.getProgress();
      console.log(`Progress: ${progress.percentage}% (${progress.current}/${progress.total})`);
    }
  }
  
  tracker.complete();
  console.log('All items processed!');
}
```

### Object Inspection

```javascript
import { deepInspect, logDeep, prettyPrintJSON } from 'unrdf/utils';

// Deep inspect complex object
const complexObject = {
  store: store,
  metadata: { version: '1.0', created: new Date() },
  circular: null
};
complexObject.circular = complexObject; // Create circular reference

// Safe deep inspection
const inspection = deepInspect(complexObject, { maxDepth: 3 });
console.log(inspection);

// Log deep inspection
logDeep(complexObject, { maxDepth: 2 });

// Pretty print JSON
const jsonOutput = prettyPrintJSON(complexObject, 4);
console.log(jsonOutput);
```

### Memory and Performance Monitoring

```javascript
import { timeExecution, createTimer } from 'unrdf/utils';

// Monitor memory usage
function getMemoryUsage() {
  const usage = process.memoryUsage();
  return {
    rss: Math.round(usage.rss / 1024 / 1024) + ' MB',
    heapTotal: Math.round(usage.heapTotal / 1024 / 1024) + ' MB',
    heapUsed: Math.round(usage.heapUsed / 1024 / 1024) + ' MB',
    external: Math.round(usage.external / 1024 / 1024) + ' MB'
  };
}

// Time operations with memory monitoring
const timer = createTimer('Memory Intensive Operation');

console.log('Memory before:', getMemoryUsage());
timer.start();

// Perform memory-intensive operation
const result = await processLargeStore(store);

const duration = timer.end();
console.log('Memory after:', getMemoryUsage());
console.log(`Operation completed in ${duration}ms`);
```

## Error Handling

- **Safe Inspection**: Deep inspection handles circular references safely
- **Timer Errors**: Timers handle errors gracefully
- **Logger Levels**: Loggers filter messages based on level
- **Progress Tracking**: Progress trackers handle invalid updates

## Performance Notes

- **Minimal Overhead**: Debug utilities have minimal performance impact
- **Lazy Evaluation**: Inspection is performed only when needed
- **Memory Efficient**: Progress tracking uses minimal memory
- **Non-blocking**: Logging and timing don't block main operations

## Related Modules

- [Graph Utils](./graph-utils.md) - Store operations
- [Validation Utils](./validation-utils.md) - Data validation
- [I/O Utils](./io-utils.md) - File operations
