# Configuration: @unrdf/project-engine

Complete reference for all configuration options.

---

## Configuration Options

Each option controls specific behavior:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| storageBackend | string | default | Controls storagebackend behavior |
| persistenceLevel | string | default | Controls persistencelevel behavior |
| validationLevel | string | default | Controls validationlevel behavior |
| workflowTimeout | string | default | Controls workflowtimeout behavior |
| maxGraphs | string | default | Controls maxgraphs behavior |

---

## Option Groups

### Performance Options

Options affecting performance characteristics:

- `storageBackend` - Enable feature
- `persistenceLevel` - Cache size
- `validationLevel` - Update interval

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
  storageBackend: false,
  persistenceLevel: 100,
  verbose: true
}
```

### Production

```javascript
{
  storageBackend: true,
  persistenceLevel: 10000,
  verbose: false
}
```

---

## See Also

- [01-api.md](01-api.md) - API functions
- [03-errors.md](03-errors.md) - Error handling
- [../how-to/](../how-to/) - Configuration patterns
