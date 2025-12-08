# Poka-Yoke Design: KGC-4D Integration with JS/Erlang Layer

## Overview

This document defines the poka-yoke (error-proofing) design for integrating KGC-4D with the JavaScript/Erlang layer via the AtomVM bridge. The goal is to make invalid states unrepresentable and prevent errors at compile time and runtime.

## Error Modes Inventory

### Bridge Initialization Errors

- [ ] **Bridge not initialized before use**
  - Error: Calling `emitEvent()`, `registerHook()`, or `processIntent()` before bridge is initialized
  - Current: Bridge is created lazily via `getBridge()`, but no state tracking
  - Prevention: State machine for bridge lifecycle

- [ ] **Store initialization failure**
  - Error: `KGCStore` constructor throws (e.g., invalid nodeId)
  - Current: Error propagates, bridge may be in invalid state
  - Prevention: Validate store initialization, track state

- [ ] **Hook registry initialization failure**
  - Error: `createHookRegistry()` fails
  - Current: Error propagates, bridge may be in invalid state
  - Prevention: Validate registry initialization, track state

### Event Emission Errors

- [ ] **Invalid event type**
  - Error: `type` is null, undefined, or not a string
  - Current: Defaults to `EVENT_TYPES.CREATE`, but no validation
  - Prevention: Validate event type at entry point

- [ ] **Invalid payload format**
  - Error: `payload` is not an object or exceeds size limits
  - Current: `KGCStore.appendEvent()` validates payload size, but bridge doesn't pre-validate
  - Prevention: Validate payload before calling store

- [ ] **Payload size exceeds limit**
  - Error: Payload > 1MB (KGC-4D store limit)
  - Current: Store throws error, but bridge doesn't pre-check
  - Prevention: Validate payload size before store call

- [ ] **Store append failure**
  - Error: `store.appendEvent()` throws (e.g., transaction failure)
  - Current: Error caught and returned, but roundtrip tracking may be inconsistent
  - Prevention: Ensure roundtrip tracking is consistent on error

### Hook Registration Errors

- [ ] **Invalid hook name**
  - Error: `hookConfig.name` is null, undefined, or empty string
  - Current: `defineHook()` may throw, but bridge doesn't pre-validate
  - Prevention: Validate hook config at entry point

- [ ] **Invalid hook trigger**
  - Error: `hookConfig.trigger` is not a valid `HookTrigger`
  - Current: `defineHook()` validates, but bridge doesn't pre-validate
  - Prevention: Validate trigger before calling `defineHook()`

- [ ] **Hook already registered**
  - Error: Hook with same name already exists in registry
  - Current: `registerHook()` throws, but bridge doesn't check first
  - Prevention: Check for existing hook before registration

- [ ] **Invalid hook functions**
  - Error: `validate` or `transform` are not functions (if provided)
  - Current: `defineHook()` may throw, but bridge doesn't pre-validate
  - Prevention: Validate functions before calling `defineHook()`

### Intent Processing Errors

- [ ] **Invalid intent ID**
  - Error: `intentId` is null, undefined, or empty string
  - Current: No validation, may cause issues in intent storage
  - Prevention: Validate intent ID at entry point

- [ ] **Invalid intent payload**
  - Error: `intent` is not an object or missing required fields
  - Current: No validation, may cause issues in processing
  - Prevention: Validate intent structure at entry point

- [ ] **Intent event emission failure**
  - Error: `emitEvent('INTENT', ...)` fails
  - Current: Returns error, but intent processing continues
  - Prevention: Fail fast if intent event emission fails

### Bridge Command Parsing Errors

- [ ] **Invalid command format**
  - Error: Bridge command doesn't match expected format (e.g., `KGC4D_BRIDGE:command:args`)
  - Current: Command ignored, but no error reported
  - Prevention: Validate command format, report errors

- [ ] **Missing command arguments**
  - Error: Command requires arguments but none provided (e.g., `emit_event` without type)
  - Current: May cause undefined behavior
  - Prevention: Validate required arguments for each command

- [ ] **Invalid JSON in payload**
  - Error: Payload string is not valid JSON
  - Current: Falls back to `{ raw: payloadStr }`, but may cause issues
  - Prevention: Validate JSON, report parsing errors

### Roundtrip Tracking Errors

- [ ] **Roundtrip ID mismatch**
  - Error: `endRoundtrip()` called with non-existent roundtrip ID
  - Current: Warning logged, but no error thrown
  - Prevention: Validate roundtrip ID exists before ending

- [ ] **Roundtrip not started**
  - Error: `endRoundtrip()` called without corresponding `startRoundtrip()`
  - Current: Warning logged, but no error thrown
  - Prevention: Ensure roundtrip is started before ending

- [ ] **SLA violation not prevented**
  - Error: Operation starts even when error rate exceeds threshold
  - Current: `canStartRoundtrip()` checks, but may not be called
  - Prevention: Always check `canStartRoundtrip()` before starting

### OTEL Span Errors

- [ ] **Tracer not initialized**
  - Error: `trace.getTracer()` returns `NoopTracer` (provider not registered)
  - Current: Spans created but not collected
  - Prevention: Ensure provider is initialized before bridge operations

- [ ] **Span attributes invalid**
  - Error: Span attributes are null, undefined, or wrong type
  - Current: May cause OTEL SDK errors
  - Prevention: Validate span attributes before setting

### Interceptor Errors

- [ ] **Module.print/printErr not available**
  - Error: `Module.print` or `Module.printErr` is undefined
  - Current: May cause interceptor installation to fail silently
  - Prevention: Validate Module object before intercepting

- [ ] **Interceptor installed multiple times**
  - Error: `interceptAtomVMOutput()` called multiple times
  - Current: May wrap functions multiple times, causing issues
  - Prevention: Track interceptor state, prevent re-installation

## Prevention Strategy

### Type-Level Prevention

1. **State Machine Pattern**: Use explicit states for bridge lifecycle (Uninitialized, Initializing, Ready, Error, Destroyed)
2. **Validation Types**: 
   - `NonEmptyString` for event types, hook names, intent IDs
   - `EventPayloadSchema` for event payloads (Zod schema)
   - `HookConfigSchema` for hook configurations (Zod schema)
   - `IntentSchema` for intent payloads (Zod schema)
3. **Type Guards**: Functions to check bridge state before operations
4. **Compile-Time Checks**: JSDoc types and validation functions

### Runtime Prevention

1. **Constructor Validation**: Validate all required inputs at construction
2. **State Checks**: Check bridge state before all operations
3. **Input Validation**: Validate event types, payloads, hook configs, intents at entry points
4. **Pre-flight Checks**: Validate payload size, hook existence, roundtrip state before operations
5. **Error Handling**: Ensure roundtrip tracking is consistent on errors

## Implementation Plan

### Phase 1: Error Mode Identification
- [x] Document all error modes (this document)
- [ ] Review existing code for error handling gaps
- [ ] Prioritize error modes by severity and frequency

### Phase 2: Type-Level Prevention
- [ ] Add state machine to `KGC4DBridge` class
- [ ] Create validation schemas (Zod) for event payloads, hook configs, intents
- [ ] Add type guards for bridge state
- [ ] Add JSDoc types for all public APIs

### Phase 3: Runtime Prevention
- [ ] Add input validation at all entry points
- [ ] Add pre-flight checks (payload size, hook existence, etc.)
- [ ] Ensure roundtrip tracking consistency
- [ ] Add interceptor state tracking

### Phase 4: Verification
- [ ] Add tests for all error modes
- [ ] Verify poka-yoke prevents invalid operations
- [ ] Measure error prevention effectiveness

### Phase 5: Documentation
- [ ] Document state machine transitions
- [ ] Document type invariants
- [ ] Document usage patterns
- [ ] Update API documentation

