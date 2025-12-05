# Configuration: @unrdf/dark-matter

Complete reference for all configuration options.

---

## Configuration Options

Each option controls specific behavior:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| aggressiveness | string | default | Controls aggressiveness behavior |
| cacheTTL | string | default | Controls cachettl behavior |
| maxMemory | string | default | Controls maxmemory behavior |
| parallelism | string | default | Controls parallelism behavior |
| timeout | string | default | Controls timeout behavior |

---

## Option Groups

### Performance Options

Options affecting performance characteristics:

- `aggressiveness` - Enable feature
- `cacheTTL` - Cache size
- `maxMemory` - Update interval

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
  aggressiveness: false,
  cacheTTL: 100,
  verbose: true
}
```

### Production

```javascript
{
  aggressiveness: true,
  cacheTTL: 10000,
  verbose: false
}
```

---

## See Also

- [01-api.md](01-api.md) - API functions
- [03-errors.md](03-errors.md) - Error handling
- [../how-to/](../how-to/) - Configuration patterns
