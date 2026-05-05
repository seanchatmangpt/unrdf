# Capability Map: @unrdf/docs

**Generated:** 2025-12-28
**Package:** @unrdf/docs
**Version:** [VERSION]

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

- `@ai-sdk/gateway`: ^[VERSION]
- `@ai-sdk/vue`: ^[VERSION]
- `@electric-sql/pglite`: ^[VERSION]
- `@iconify-json/logos`: ^[VERSION]
- `@iconify-json/lucide`: ^[VERSION]
- `@iconify-json/simple-icons`: ^[VERSION]
- `@iconify-json/vscode-icons`: ^[VERSION]
- `@nuxt/content`: ^[VERSION]
- `@nuxt/image`: ^[VERSION]
- `@nuxt/ui`: ^[VERSION]
- `@nuxtjs/mdc`: ^[VERSION]
- `ai`: ^[VERSION]
- `better-sqlite3`: ^1[VERSION]
- `date-fns`: ^[VERSION]
- `drizzle-orm`: ^[VERSION]
- `nuxt`: ^[VERSION]
- `nuxt-auth-utils`: ^[VERSION]
- `nuxt-charts`: [VERSION]
- `nuxt-llms`: [VERSION]
- `nuxt-og-image`: ^[VERSION]
- `shiki-stream`: ^[VERSION]

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
