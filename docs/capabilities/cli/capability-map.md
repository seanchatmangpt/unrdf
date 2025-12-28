# Capability Map: @unrdf/cli

**Generated:** 2025-12-28
**Package:** @unrdf/cli
**Version:** 5.0.1

---

## Description

UNRDF CLI - Command-line Tools for Graph Operations and Context Management

---

## Capability Atoms

### A80: UNRDF CLI

**Runtime:** Node.js
**Invariants:** command-line, package-management
**Evidence:** `packages/cli/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/core`: workspace:5.0.0-beta.1
- `@unrdf/decision-fabric`: workspace:5.0.0-beta.1
- `@unrdf/federation`: workspace:5.0.0-beta.1
- `@unrdf/hooks`: workspace:5.0.0-beta.1
- `@unrdf/streaming`: workspace:5.0.0-beta.1
- `citty`: ^0.1.6
- `table`: ^6.9.0
- `yaml`: ^2.8.2

### Exports

- `.`: `./src/index.mjs`
- `./commands`: `./src/commands/index.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **UNRDF CLI**
   - Import: `import { /* exports */ } from '@unrdf/cli'`
   - Use for: UNRDF CLI operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/cli';

const store = createStore();
// Use cli capabilities with store
```


---

## Evidence Trail

- **A80**: `packages/cli/src/index.mjs`

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
