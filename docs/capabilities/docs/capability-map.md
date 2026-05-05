# Capability Map: @unrdf/docs

**Generated:** 2025-12-28
**Package:** @unrdf/docs
**Version:** latest

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

- `@ai-sdk/gateway`: ^latest
- `@ai-sdk/vue`: ^latest
- `@electric-sql/pglite`: ^latest
- `@iconify-json/logos`: ^latest
- `@iconify-json/lucide`: ^latest
- `@iconify-json/simple-icons`: ^latest
- `@iconify-json/vscode-icons`: ^latest
- `@nuxt/content`: ^latest
- `@nuxt/image`: ^latest
- `@nuxt/ui`: ^latest
- `@nuxtjs/mdc`: ^latest
- `ai`: ^latest
- `better-sqlite3`: ^1latest
- `date-fns`: ^latest
- `drizzle-orm`: ^latest
- `nuxt`: ^latest
- `nuxt-auth-utils`: ^latest
- `nuxt-charts`: latest
- `nuxt-llms`: latest
- `nuxt-og-image`: ^latest
- `shiki-stream`: ^latest

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
