# API Reference: @unrdf/dark-matter

Complete reference for all functions and methods.

---

## Functions


### optimizeQuery()

optimizeQuery is a core function handling optimizequery operations.

```javascript
optimizeQuery(options?: Options): Promise<Result>
```

**Parameters:**
- `options.aggressiveness` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await optimizeQuery({
  aggressiveness: true
});
console.log('Result:', result);
```


### createOptimizer()

createOptimizer is a core function handling createoptimizer operations.

```javascript
createOptimizer(options?: Options): Promise<Result>
```

**Parameters:**
- `options.aggressiveness` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await createOptimizer({
  aggressiveness: true
});
console.log('Result:', result);
```


### analyzePerformance()

analyzePerformance is a core function handling analyzeperformance operations.

```javascript
analyzePerformance(options?: Options): Promise<Result>
```

**Parameters:**
- `options.aggressiveness` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await analyzePerformance({
  aggressiveness: true
});
console.log('Result:', result);
```


### suggestIndices()

suggestIndices is a core function handling suggestindices operations.

```javascript
suggestIndices(options?: Options): Promise<Result>
```

**Parameters:**
- `options.aggressiveness` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await suggestIndices({
  aggressiveness: true
});
console.log('Result:', result);
```


### benchmarkQuery()

benchmarkQuery is a core function handling benchmarkquery operations.

```javascript
benchmarkQuery(options?: Options): Promise<Result>
```

**Parameters:**
- `options.aggressiveness` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await benchmarkQuery({
  aggressiveness: true
});
console.log('Result:', result);
```


---

## Module Exports

```javascript
export {
  optimizeQuery,
  createOptimizer,
  analyzePerformance,
  suggestIndices,
  benchmarkQuery
};
```

---

## Error Handling

All functions use consistent error handling:

```javascript
try {
  const result = await optimizeQuery();
} catch (error) {
  if (error.code === 'TIMEOUT') {
    console.error('Operation timed out');
  } else if (error.code === 'CONFIG_ERROR') {
    console.error('Invalid configuration');
  }
}
```

---

## Performance Notes

- Functions are optimized for both speed and memory efficiency
- Streaming mode available for large datasets
- Caching can significantly improve performance

---

## See Also

- [02-types.md](02-types.md) - Type definitions
- [03-configuration.md](03-configuration.md) - Configuration options
- [../how-to/](../how-to/) - Practical usage guides
