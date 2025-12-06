# API Reference: @unrdf/composables

Complete reference for all functions and methods.

---

## Functions


### useRdfStore()

useRdfStore is a core function handling userdfstore operations.

```javascript
useRdfStore(options?: Options): Promise<Result>
```

**Parameters:**
- `options.autoSync` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await useRdfStore({
  autoSync: true
});
console.log('Result:', result);
```


### useQuery()

useQuery is a core function handling usequery operations.

```javascript
useQuery(options?: Options): Promise<Result>
```

**Parameters:**
- `options.autoSync` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await useQuery({
  autoSync: true
});
console.log('Result:', result);
```


### useMutation()

useMutation is a core function handling usemutation operations.

```javascript
useMutation(options?: Options): Promise<Result>
```

**Parameters:**
- `options.autoSync` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await useMutation({
  autoSync: true
});
console.log('Result:', result);
```


### useSubscription()

useSubscription is a core function handling usesubscription operations.

```javascript
useSubscription(options?: Options): Promise<Result>
```

**Parameters:**
- `options.autoSync` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await useSubscription({
  autoSync: true
});
console.log('Result:', result);
```


### useIndexing()

useIndexing is a core function handling useindexing operations.

```javascript
useIndexing(options?: Options): Promise<Result>
```

**Parameters:**
- `options.autoSync` - Configuration option (optional)
- `options.timeout` - Timeout in milliseconds (default: 30000)
- `options.retries` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
```javascript
const result = await useIndexing({
  autoSync: true
});
console.log('Result:', result);
```


---

## Module Exports

```javascript
export {
  useRdfStore,
  useQuery,
  useMutation,
  useSubscription,
  useIndexing
};
```

---

## Error Handling

All functions use consistent error handling:

```javascript
try {
  const result = await useRdfStore();
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
