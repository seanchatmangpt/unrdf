# Example KGC Plugin Template

This is a complete template for creating KGC Runtime plugins.

## Structure

```
plugin-template/
├── plugin.json       # Plugin manifest
├── index.mjs         # Plugin implementation
├── README.md         # This file
└── test.mjs          # Plugin tests
```

## Quick Start

1. **Copy Template**
   ```bash
   cp -r templates/plugin-template my-plugin
   cd my-plugin
   ```

2. **Update Manifest** (`plugin.json`)
   - Change `name` to your plugin name
   - Update `version`, `author`, `description`
   - Declare required `capabilities`

3. **Implement Plugin** (`index.mjs`)
   - Modify `initialize()` for setup
   - Add your custom methods
   - Update `cleanup()` for teardown

4. **Test Plugin** (`test.mjs`)
   - Write comprehensive tests
   - Test all capabilities
   - Verify error handling

5. **Register and Use**
   ```javascript
   import { PluginManager } from '@unrdf/kgc-runtime/plugin-manager';

   const manager = new PluginManager();
   await manager.registerPlugin(manifest);
   await manager.loadPlugin('my-plugin@1.0.0');
   ```

## Best Practices

1. **Validate All Inputs** - Use Zod schemas
2. **Handle Errors** - Return error receipts
3. **Declare Minimum Capabilities** - Only what you need
4. **Document Your API** - JSDoc comments
5. **Write Tests** - Aim for 100% coverage

## See Also

- [Plugin Development Guide](../../docs/extensions/plugin-development.md)
- [API Stability Policy](../../docs/api-stability.md)
