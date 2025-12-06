# API Reference: @unrdf/engine-gateway

Complete reference for all functions and methods.

---

## Functions


### createGateway()

createGateway is a core function handling creategateway operations.

```javascript
createGateway(options?: Options): Promise<Result>
```

**Parameters:**
- `options.engines` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await createGateway({
  engines: true
});
console.log('Result:', result);
```


### addEngine()

addEngine is a core function handling addengine operations.

```javascript
addEngine(options?: Options): Promise<Result>
```

**Parameters:**
- `options.engines` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await addEngine({
  engines: true
});
console.log('Result:', result);
```


### removeEngine()

removeEngine is a core function handling removeengine operations.

```javascript
removeEngine(options?: Options): Promise<Result>
```

**Parameters:**
- `options.engines` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await removeEngine({
  engines: true
});
console.log('Result:', result);
```


### federatedQuery()

federatedQuery is a core function handling federatedquery operations.

```javascript
federatedQuery(options?: Options): Promise<Result>
```

**Parameters:**
- `options.engines` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await federatedQuery({
  engines: true
});
console.log('Result:', result);
```


### mergeResults()

mergeResults is a core function handling mergeresults operations.

```javascript
mergeResults(options?: Options): Promise<Result>
```

**Parameters:**
- `options.engines` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await mergeResults({
  engines: true
});
console.log('Result:', result);
```


---

## Module Exports

```javascript
export {
  createGateway,
  addEngine,
  removeEngine,
  federatedQuery,
  mergeResults
};
```

---

## Error Handling

All functions use consistent error handling:

```javascript
try {
  const result = await createGateway();
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
