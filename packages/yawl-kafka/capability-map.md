# Capability Map: @unrdf/yawl-kafka

**Generated:** 2025-12-28
**Package:** @unrdf/yawl-kafka
**Version:** 1.0.0

---

## Description

Apache Kafka event streaming integration for YAWL workflows with Avro serialization

---

## Capability Atoms

### A66: Kafka Event Integration

**Runtime:** Node.js
**Invariants:** event-driven, exactly-once
**Evidence:** `packages/yawl-kafka/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/core`: workspace:*
- `avsc`: ^5.7.7
- `kafkajs`: ^2.2.4
- `zod`: ^3.24.1

### Exports

- `.`: `./src/index.mjs`
- `./producer`: `./src/producer.mjs`
- `./consumer`: `./src/consumer.mjs`
- `./schemas`: `./src/schemas.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Kafka Event Integration**
   - Import: `import { /* exports */ } from '@unrdf/yawl-kafka'`
   - Use for: Kafka Event Integration operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/yawl-kafka';

const store = createStore();
// Use yawl-kafka capabilities with store
```


---

## Evidence Trail

- **A66**: `packages/yawl-kafka/src/index.mjs`

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
