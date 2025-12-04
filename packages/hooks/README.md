# @unrdf/hooks

**Knowledge Hooks - Policy Definition and Execution Framework**

The policy enforcement layer for UNRDF. Define and execute hooks to enforce rules, validate data, and transform quads.

## Installation

```bash
pnpm add @unrdf/hooks
```

## 📚 Examples

See these examples that demonstrate @unrdf/hooks:

- **[basic-knowledge-hook.mjs](../../examples/basic-knowledge-hook.mjs)** - Hook basics: validation and transformation (15 min)
- **[define-hook-example.mjs](../../examples/define-hook-example.mjs)** - Advanced hook composition patterns (20 min)
- **[production-hook-test.mjs](../../examples/production-hook-test.mjs)** - Production-ready hook testing
- **[hook-lifecycle-test.mjs](../../examples/hook-lifecycle-test.mjs)** - Complete hook lifecycle
- **[knowledge-hooks-events.mjs](../../examples/knowledge-hooks-events.mjs)** - Event-driven hooks

**Want to learn hooks?** Follow the [hooks learning path](../../examples/README.md#-knowledge-hooks).

## Quick Start

```javascript
import { defineHook, executeHook } from '@unrdf/hooks'

// Define a validation hook
defineHook('validate-pii', {
  type: 'validate-before-write',
  async check(quad) {
    // Reject PII without consent
    if (isPII(quad) && !hasConsent(quad.subject)) {
      return false
    }
    return true
  }
})

// Execute hooks
const isValid = await executeHook('validate-pii', quad)
```

## Features

- ✅ Define custom hooks for any RDF operation
- ✅ Pre-write validation (block invalid data)
- ✅ Post-write transformation (modify data)
- ✅ Hook composition (chain multiple hooks)
- ✅ Type-safe hook definitions
- ✅ Async/await support

## Use Cases

- **Validation**: Check data before writing
- **Transformation**: Modify quads on the fly
- **Enforcement**: Implement security policies
- **Audit**: Track and log changes
- **Compliance**: Enforce regulatory requirements

## Documentation

- **[API Reference](./docs/API.md)** - Complete API documentation
- **[User Guide](./docs/GUIDE.md)** - How to use hooks
- **[Examples](./examples/)** - Code examples
- **[Contributing](./docs/CONTRIBUTING.md)** - How to contribute

## Depends On

- `@unrdf/core` - RDF substrate

## VOC Usage

- VOC-1: Autonomous Knowledge Agent (policy enforcement)
- VOC-3: ML Agent (apply learned patterns)
- VOC-4: Audit Agent (compliance monitoring)
- VOC-5: Data Engineer (validation during ingestion)

## License

MIT
