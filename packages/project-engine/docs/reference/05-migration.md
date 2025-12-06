# Migration Guide: @unrdf/project-engine

Guide for upgrading to new versions.

---

## Version History

### Version 2.0.0

New features and improvements in v2:

- Enhanced performance
- Better error messages
- Improved configuration options
- Streaming support

---

## Upgrade Steps

1. **Review breaking changes** - Check what changed
2. **Update package** - Install new version
3. **Update code** - Modify for new API
4. **Test** - Run full test suite
5. **Deploy** - Push to production

---

## Common Migration Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| Import errors | API moved | Update imports |
| Config errors | Option renamed | Update configuration |
| Type errors | Signature changed | Update function calls |

---

## Compatibility Mode

Some versions support compatibility mode for gradual migration:

```javascript
const instance = await createProject({
  compatibilityMode: true
});
```

---

## See Also

- Release notes for detailed changes
- [01-api.md](01-api.md) - New API signatures
- [../how-to/](../how-to/) - Usage examples
