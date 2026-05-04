# Phase 3: CLI & Template Hardening Plan

This plan outlines the steps to strengthen the `@unrdf/cli` hardening protocols and ensure that Nunjucks templates correctly implement L4/L5 compliance gates as specified in `OSTAR-UNRDF-BRIDGE-COMPLIANCE-1000.md`.

## 1. CLI Robustness & Flag Propagation

### Current State
The `--harden` flag exists in `SyncArgsSchema` and is partially implemented in `sync` and `template generate` commands. It currently enforces **Gate 1** (Constitutional Admissibility) by checking for `shacl:conforms` in the source ontology.

### Proposed Enhancements
- **Unified Harden Context**: Refactor flag handling into a shared utility within `@unrdf/cli` to ensure consistent behavior across all projection commands.
- **Environment Globals**: Update `createNunjucksEnvironment` in `template-renderer.mjs` to automatically inject `harden_enabled` and `harden_mode` into the global Nunjucks context.
- **Strict Validation**: If `--harden` is active, the CLI must fail if any template in the pipeline lacks the `harden-compliant` frontmatter tag, ensuring no "soft" artifacts are projected in a hardened environment.

## 2. Injection Logic for Hardened Primitives

Templates (specifically `projected-artifact.njk` or its functional equivalents like `mcp-handlers.njk`) must be updated to inject the following logic when `harden` is enabled.

### VectorClock (Gate 2)
Every state-mutating handler must initialize a context-bound `VectorClock` to ensure causal lineage.

```njk
{% if harden %}
import { VectorClock } from '@unrdf/kgc-4d';
// ...
const vc = new VectorClock(context.nodeId || '{{ project.name | slug }}-node');
vc.increment(); // Mark the start of the decision cycle
{% endif %}
```

### HardenedAtomVM (Gate 1 Loopback)
Execution logic must be wrapped in the `HardenedAtomVM` facade to verify the artifact's own cryptographic receipt before running.

```njk
{% if harden %}
import { HardenedAtomVM } from '@unrdf/atomvm';
// ...
return HardenedAtomVM.execute(async () => {
  {{ handler_logic | indent(2) }}
}, { 
  vectorClock: vc, 
  artifact: '{{ templatePath | basename }}',
  receipt: '{{ receipt_id }}' 
});
{% else %}
{{ handler_logic }}
{% endif %}
```

## 3. Implementation of Gates 2 and 3

### Gate 2: Causal Lineage
- **Requirement**: $O(1)$ causal ordering via `initialize_causal_context(nodeId)`.
- **Plan**:
  - Implement a Nunjucks macro `{% macro causal_init(ctx) %}` that generates the `VectorClock` boilerplate.
  - Update the `mcp-tool` and `powl` template suites to call this macro at the entry point of every generated tool.

### Gate 3: JIT SHACL Enforcement
- **Requirement**: Block invalid transitions at the edge via `@unrdf/shacl` validation.
- **Plan**:
  - **Runtime Injection**: Inject a `shacl_validate(delta)` call before every `store.appendEvent()` or `store.update()` call in generated artifacts.
  - **Schema Baking**: If `harden` is true, the CLI will "bake" the relevant SHACL shapes directly into the generated artifact as a static constant to avoid external lookups at runtime.
  - **Validation Loop**:
    ```njk
    {% if harden %}
    const validation = await validateShacl(eventDelta, BOKED_SHAPES);
    if (!validation.conforms) throw new HardeningError('JIT-SHACL violation', validation.results);
    {% endif %}
    await store.appendEvent(eventDelta);
    ```

---

## Adversarial Review

### "What if the template injection makes generated code unreadable?"
**Critique**: Wrapping handlers in multiple layers of `if (harden)` and `HardenedAtomVM.execute` increases cognitive load for developers auditing the generated code and complicates debugging.
**Mitigation**:
- Use **Template Composition**: Move the hardening boilerplate into a base template or a "Hardening Wrapper" template that uses Nunjucks blocks.
- **Source Maps**: Ensure that the code generator produces accurate source maps that point back to the original RDF/SPARQL source, bypassing the hardening noise during debug sessions.

### "Is Gate 3 (JIT SHACL) too expensive for edge devices?"
**Critique**: Running a full SHACL engine at the edge for every state mutation is computationally expensive and may introduce latency that violates real-time manufacturing constraints.
**Mitigation**:
- **Compiled Guards**: Instead of a generic SHACL interpreter, Phase 3 will investigate **Compiled SHACL Guards**—where shapes are compiled into specialized JavaScript validation functions at build time.
- **Selective Hardening**: Allow users to specify "Critical Shapes" that require JIT enforcement, while leaving non-critical validation to asynchronous audit loops.

## Deliverables
- [ ] Updated `template-renderer.mjs` with hardening globals.
- [ ] Refactored `mcp-handlers.njk` with `HardenedAtomVM` wrappers.
- [ ] New `Gate 2` and `Gate 3` test suites in `packages/cli/test/harden-gate-deep.test.mjs`.
