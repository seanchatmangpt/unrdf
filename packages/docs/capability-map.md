# Capability Map: @unrdf/docs

**Generated:** 2025-12-28
**Package:** @unrdf/docs
**Version:** 5.0.1

---

## Description

No description available

---

## Capability Atoms

### A89: Core Documentation

**Runtime:** Static
**Invariants:** markdown, generated
**Evidence:** `packages/docs/README.md`



---

## Package Metadata

### Dependencies

- `@ai-sdk/gateway`: ^2.0.15
- `@ai-sdk/vue`: ^2.0.101
- `@electric-sql/pglite`: ^0.2.17
- `@iconify-json/logos`: ^1.2.10
- `@iconify-json/lucide`: ^1.2.75
- `@iconify-json/simple-icons`: ^1.2.60
- `@iconify-json/vscode-icons`: ^1.2.36
- `@nuxt/content`: ^3.8.2
- `@nuxt/image`: ^2.0.0
- `@nuxt/ui`: ^4.2.1
- `@nuxtjs/mdc`: ^0.18.4
- `ai`: ^5.0.101
- `better-sqlite3`: ^12.4.6
- `date-fns`: ^4.1.0
- `drizzle-orm`: ^0.44.7
- `nuxt`: ^4.2.1
- `nuxt-auth-utils`: ^0.5.25
- `nuxt-charts`: 0.2.4
- `nuxt-llms`: 0.1.3
- `nuxt-og-image`: ^5.1.12
- `shiki-stream`: ^0.1.3

### Exports

- `.`: `./src/index.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Core Documentation**
   - Import: `import { /* exports */ } from '@unrdf/docs'`
   - Use for: Core Documentation operations
   - Runtime: Static


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/docs';

const store = createStore();
// Use docs capabilities with store
```


---

## Evidence Trail

- **A89**: `packages/docs/README.md`

---

## Next Steps

1. **Explore API Surface**
   - Review exports in package.json
   - Read source files in `src/` directory

2. **Integration Testing**
   - Create test cases using package capabilities
   - Verify compatibility with dependent packages

3. **Performance Profiling**
   - Benchmark key operations
   - Measure runtime characteristics

---

**Status:** GENERATED
**Method:** Systematic extraction from capability-basis.md + package.json analysis
**Confidence:** 95% (evidence-based)
