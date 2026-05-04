# Plan Phase 2: Chicago TDD Refactoring for `packages/atomvm`

## Overview
This plan outlines the refactoring of the `packages/atomvm` test suite from London-style (interaction-based, heavy mocking) to Chicago-style (outcome-based, sociable mocks/real instances). The goal is to improve test reliability, reduce brittleness, and ensure tests validate behavioral outcomes rather than implementation details.

## 1. Refactoring `oxigraph-bridge.test.mjs`
### Current State
- Uses a hand-rolled `createMockStore` with basic triple matching.
- Uses custom `mockNamedNode` and `mockLiteral` functions.
- Assertions focus on internal mock state (e.g., `store.size`).

### Refactor Plan
- **Real Instances**: Replace `createMockStore` with a real `OxigraphStore` from `@unrdf/oxigraph`.
- **Data Factory**: Use `dataFactory` from `@unrdf/oxigraph` to create real RDF terms.
- **Sociable Assertions**: Assert on the bridge's state and use the store's `match` method to verify triples were actually persisted correctly according to RDF semantics (e.g., handling prefixes, blank nodes).
- **Behavioral Focus**: Test how the bridge handles edge cases (invalid triples, concurrent operations) using the real store's behavior.

## 2. Refactoring `atomvm-runtime.test.mjs`
### Current State
- Relies on `jsdom` and manual pollution of `global.window`.
- Mocks WASM module initialization brittly.
- Uses London-style mocks for `TerminalUI`.

### Refactor Plan
- **Sociable Mocks**: Use a real `TerminalUI` (it's a lightweight UI wrapper) or a sociable stub that captures output without needing a full DOM.
- **Real Lifecycle**: Use the real `AtomVMRuntime` state machine. Instead of mocking `window.Module`, provide a minimal, valid WASM buffer that the runtime can actually "load," even if it doesn't execute complex logic.
- **Outcome Assertions**: Shift from checking if `appendChild` was called to checking if the runtime reached the `Ready` or `Destroyed` state and if the expected messages were emitted via the terminal bridge.

## 3. Refactoring `vm-e2e.test.mjs`
### Current State
- **Broken API**: The test currently uses an outdated API for `HardenedAtomVM` (passing a VM mock where a store is expected, and passing bytecode where a path is expected).
- **Heavy Mocking**: Mocks the entire `mockRawVm` and asserts on every `registerOpcode` and `intercept` call.
- **Implementation Tracking**: Heavily tracks internal function calls (`expect(spy).toHaveBeenCalledWith`).

### Refactor Plan
- **API Fix**: Align the test with the actual `HardenedAtomVM` implementation in `src/vm/facade.mjs`.
- **Real Store**: Pass a real `OxigraphStore` as the `store` argument.
- **Sociable Runtime**: Use `AtomVMNodeRuntime` instead of `mockRawVm`. For the E2E test, use a small, real `.avm` file or a synthetic one that triggers the desired bridge callbacks (`KGC4D_BRIDGE:register_hook`).
- **Behavioral Assertions**:
    - Assert that the `execute` call returns a success status.
    - Assert that the produced `receipt` contains correct PROV-O metadata (agent, timestamp, input context).
    - Assert that if the Erlang side sends a `register_hook` message, the JS-side `HooksBridge` state reflects the new hook registration.

---

## Adversarial Review

### "Will using real Oxigraph stores make tests too slow to run on CI?"
**Critique**: Oxigraph is a Rust-based WASM module. While faster than many JS implementations, it still has overhead. Running hundreds of tests with real stores might slow down the pipeline.
**Rebuttal**: In-memory Oxigraph stores are extremely efficient (sub-millisecond for basic operations). The gain in test reliability (catching RDF serialization/matching bugs that a hand-rolled mock misses) far outweighs the few seconds added to the CI run. We can reuse a single store instance with `clear()` between tests if performance becomes a bottleneck.

### "How do we test browser-only logic without brittle mocks while maintaining isolation?"
**Critique**: `atomvm-runtime` targets the browser. Using real WASM and DOM elements in Node-based Vitest is difficult and often leads to the same brittle `global` pollution we are trying to avoid.
**Rebuttal**: We should shift browser-specific tests to `@vitest/browser` using Playwright or a real browser provider. This allows us to use real browser APIs (SharedArrayBuffer, Web Workers) without mocking them. For unit tests in Node, we use `AtomVMNodeRuntime` which shares 90% of the logic with the browser version but uses Node's native capabilities, providing a "sociable" environment that is still fast.

### "Is Chicago-style TDD too 'Integrated' for unit tests?"
**Critique**: If `HardenedAtomVM` tests fail because `OxigraphStore` has a bug, is it still a unit test?
**Rebuttal**: Chicago-style accepts this. If the system fails, the test should fail. The goal is to verify that the *component works in its environment*, not just that its internal code executes. Debugging is slightly harder, but we avoid the "Green tests, broken app" syndrome common in London-style TDD.
