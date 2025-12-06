# Configuration: @unrdf/composables

Complete reference for all configuration options.

---

## Configuration Options

Each option controls specific behavior:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| autoSync | string | default | Controls autosync behavior |
| cacheSize | string | default | Controls cachesize behavior |
| updateInterval | string | default | Controls updateinterval behavior |
| debounceMs | string | default | Controls debouncems behavior |
| batchSize | string | default | Controls batchsize behavior |

---

## Option Groups

### Performance Options

Options affecting performance characteristics:

- `autoSync` - Enable feature
- `cacheSize` - Cache size
- `updateInterval` - Update interval

### Reliability Options

Options affecting reliability:

- Retry count - Number of retries
- Timeout - Operation timeout
- Health checks - Monitor intervals

### Integration Options

Options for system integration:

- Storage backend - Where data is stored
- Cache backend - Cache implementation
- Logging - Log level

---

## Configuration Profiles

### Development

```javascript
{
  autoSync: false,
  cacheSize: 100,
  verbose: true
}
```

### Production

```javascript
{
  autoSync: true,
  cacheSize: 10000,
  verbose: false
}
```

---

## See Also

- [01-api.md](01-api.md) - API functions
- [03-errors.md](03-errors.md) - Error handling
- [../how-to/](../how-to/) - Configuration patterns
