# Configuration: @unrdf/engine-gateway

Complete reference for all configuration options.

---

## Configuration Options

Each option controls specific behavior:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| engines | string | default | Controls engines behavior |
| timeout | string | default | Controls timeout behavior |
| resultMerging | string | default | Controls resultmerging behavior |
| queryRouting | string | default | Controls queryrouting behavior |
| loadBalancing | string | default | Controls loadbalancing behavior |

---

## Option Groups

### Performance Options

Options affecting performance characteristics:

- `engines` - Enable feature
- `timeout` - Cache size
- `resultMerging` - Update interval

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
  engines: false,
  timeout: 100,
  verbose: true
}
```

### Production

```javascript
{
  engines: true,
  timeout: 10000,
  verbose: false
}
```

---

## See Also

- [01-api.md](01-api.md) - API functions
- [03-errors.md](03-errors.md) - Error handling
- [../how-to/](../how-to/) - Configuration patterns
