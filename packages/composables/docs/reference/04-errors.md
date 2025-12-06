# Errors: @unrdf/composables

Complete reference for error types and handling.

---

## Common Error Codes

| Code | Message | Cause | Solution |
|------|---------|-------|----------|
| CONFIG_ERROR | Invalid configuration | Bad option value | Check configuration options |
| TIMEOUT | Operation timeout | Exceeds timeout limit | Increase timeout or optimize |
| RESOURCE | Resource unavailable | Out of memory/disk | Free resources |
| OPERATION | Operation failed | Unexpected error | Enable diagnostics |

---

## Error Handling Patterns

### Try-Catch Pattern

```javascript
try {
  const result = await instance.useRdfStore();
} catch (error) {
  console.error('Error:', error.message);
}
```

### Promise Rejection Pattern

```javascript
instance.useRdfStore()
  .catch(error => {
    console.error('Error:', error);
  });
```

---

## Troubleshooting Guide

### Problem: Timeout Errors
- **Cause:** Operation takes too long
- **Solution:** Increase timeout or optimize operation

### Problem: Out of Memory
- **Cause:** Too much data in memory
- **Solution:** Use streaming or pagination

### Problem: Configuration Errors
- **Cause:** Invalid option values
- **Solution:** Review configuration options

---

## See Also

- [03-configuration.md](03-configuration.md) - Configuration options
- [../how-to/](../how-to/) - Problem solving guides
- [01-api.md](01-api.md) - API reference
