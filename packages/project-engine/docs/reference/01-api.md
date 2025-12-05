# API Reference: @unrdf/project-engine

Complete reference for all functions and methods.

---

## Functions


### createProject()

createProject is a core function handling createproject operations.

```javascript
createProject(options?: Options): Promise<Result>
```

**Parameters:**
- `options.storageBackend` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await createProject({
  storageBackend: true
});
console.log('Result:', result);
```


### addGraph()

addGraph is a core function handling addgraph operations.

```javascript
addGraph(options?: Options): Promise<Result>
```

**Parameters:**
- `options.storageBackend` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await addGraph({
  storageBackend: true
});
console.log('Result:', result);
```


### defineWorkflow()

defineWorkflow is a core function handling defineworkflow operations.

```javascript
defineWorkflow(options?: Options): Promise<Result>
```

**Parameters:**
- `options.storageBackend` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await defineWorkflow({
  storageBackend: true
});
console.log('Result:', result);
```


### executeWorkflow()

executeWorkflow is a core function handling executeworkflow operations.

```javascript
executeWorkflow(options?: Options): Promise<Result>
```

**Parameters:**
- `options.storageBackend` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await executeWorkflow({
  storageBackend: true
});
console.log('Result:', result);
```


### validateProject()

validateProject is a core function handling validateproject operations.

```javascript
validateProject(options?: Options): Promise<Result>
```

**Parameters:**
- `options.storageBackend` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await validateProject({
  storageBackend: true
});
console.log('Result:', result);
```


---

## Module Exports

```javascript
export {
  createProject,
  addGraph,
  defineWorkflow,
  executeWorkflow,
  validateProject
};
```

---

## Error Handling

All functions use consistent error handling:

```javascript
try {
  const result = await createProject();
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
