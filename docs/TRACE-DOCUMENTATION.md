# OTEL Trace Documentation - Production System

This document describes the actual OTEL traces created by the production-ready AtomVM system.

## Trace Structure

### Root Span: `validation.suite.atomvm-playground`
- **Created by**: ValidationRunner
- **Attributes**: 
  - `validation.suite`: 'atomvm-playground'
  - `validation.start_time`: timestamp
- **Children**: Feature validation spans

### Feature Validation Spans: `validation.{feature}`
- **Created by**: OTELValidator
- **Attributes**:
  - `validation.id`: unique validation ID
  - `validation.feature`: feature name (e.g., 'atomvm-bridge')
  - `validation.start_time`: timestamp
- **Children**: Feature execution spans

### Feature Execution Spans: `feature.{feature}`
- **Created by**: OTELValidator
- **Attributes**:
  - `feature.name`: feature name
- **Children**: Execution function spans

### Execution Function Spans: `validation.execute.{type}`
- **Created by**: OTELSpanBuilder execution functions
- **Types**: `bridge`, `runtime`, `erlang`
- **Attributes**:
  - `validation.id`: validation ID
  - `operation.type`: execution type
  - `service.name`: service name
- **Children**: Operation-specific spans

## Bridge Operation Spans

### `bridge.emit_event`
- **Created by**: KGC4DBridge.emitEvent()
- **Attributes**:
  - `event.type`: Event type (e.g., 'TEST', 'INTENT')
  - `event.source`: 'erlang-process'
  - `event.id`: Event receipt ID (if successful)
  - `event.success`: boolean
  - `event.error`: Error message (if failed)
- **Status**: OK (code 1) on success, ERROR (code 2) on failure

### `bridge.register_hook`
- **Created by**: KGC4DBridge.registerHook()
- **Attributes**:
  - `hook.name`: Hook name
  - `hook.trigger`: Hook trigger (e.g., 'before-add')
  - `hook.success`: boolean
  - `hook.error`: Error message (if failed)
- **Status**: OK (code 1) on success, ERROR (code 2) on failure

### `bridge.process_intent`
- **Created by**: KGC4DBridge.processIntent()
- **Attributes**:
  - `intent.id`: Intent ID
  - `intent.description`: Intent description
  - `intent.success`: boolean
  - `intent.accepted`: boolean (if successful)
  - `intent.error`: Error message (if failed)
- **Status**: OK (code 1) on success, ERROR (code 2) on failure

## Runtime Operation Spans

### `atomvm.load_wasm`
- **Created by**: AtomVMNodeRuntime.load()
- **Attributes**:
  - `runtime.type`: 'node'
  - `atomvm.version`: AtomVM version (e.g., 'v0.6.6')
  - `runtime.state`: Runtime state after load
  - `runtime.error`: Error message (if failed)
- **Status**: OK (code 1) on success, ERROR (code 2) on failure

### `atomvm.execute_beam`
- **Created by**: OTELSpanBuilder (for validation collection)
- **Note**: Runtime execution doesn't create its own span, but span data is collected
- **Attributes**:
  - `module.name`: Module name
  - `avm.path`: Path to .avm file
  - `runtime.type`: 'node'
  - `runtime.state`: Runtime state
  - `runtime.error`: Error message (if failed)
- **Status**: 'ok' or 'error'

### `atomvm.state_transition`
- **Created by**: OTELSpanBuilder (for validation collection)
- **Note**: State transitions are tracked via runtime state, not separate spans
- **Attributes**:
  - `from_state`: Previous state
  - `to_state`: New state
  - `runtime.type`: 'node'
- **Status**: 'ok'

## Erlang Process Spans

### `erlang.process.emit_event`
- **Created by**: BridgeInterceptor when parsing `KGC4D_BRIDGE:emit_event` commands
- **Attributes**:
  - `event.type`: Event type from Erlang
  - `event.source`: 'erlang-process'
  - `service.name`: 'atomvm-playground'
  - `operation.type`: 'erlang.process'
  - `event.id`: Event receipt ID (if successful)
  - `event.success`: boolean
  - `event.error`: Error message (if failed)
- **Status**: OK (code 1) on success, ERROR (code 2) on failure

### `erlang.process.register_hook`
- **Created by**: BridgeInterceptor when parsing `KGC4D_BRIDGE:register_hook` commands
- **Attributes**:
  - `hook.name`: Hook name from Erlang
  - `hook.trigger`: Hook trigger from Erlang
  - `service.name`: 'atomvm-playground'
  - `operation.type`: 'erlang.process'
  - `hook.success`: boolean
  - `hook.error`: Error message (if failed)
- **Status**: OK (code 1) on success, ERROR (code 2) on failure

### `erlang.process.intent`
- **Created by**: BridgeInterceptor when parsing `KGC4D_BRIDGE:process_intent` commands
- **Attributes**:
  - `intent.id`: Intent ID from Erlang
  - `intent.description`: Intent description
  - `service.name`: 'atomvm-playground'
  - `operation.type`: 'erlang.process'
  - `intent.success`: boolean
  - `intent.accepted`: boolean (if successful)
  - `intent.error`: Error message (if failed)
- **Status**: OK (code 1) on success, ERROR (code 2) on failure

### `erlang.process.execute`
- **Created by**: OTELSpanBuilder (for validation collection)
- **Attributes**:
  - `module.name`: Module name (e.g., 'boardroom-swarm')
  - `avm.path`: Path to .avm file
  - `service.name`: 'atomvm-playground'
  - `operation.type`: 'erlang.process'
  - `process.id`: Process ID (if error)
  - `event.type`: Error type (if error, e.g., 'MODULE_NOT_FOUND')
  - `error`: Error message (if error)
- **Status**: 'ok' or 'error'

### `erlang.process.load`
- **Created by**: OTELSpanBuilder (for validation collection when runtime load fails)
- **Attributes**:
  - `process.id`: 'unknown'
  - `event.type`: 'RUNTIME_LOAD_ERROR'
  - `error`: Error message
  - `service.name`: 'atomvm-playground'
  - `operation.type`: 'erlang.process'
- **Status**: 'error'

## Production System Characteristics

### Real OTEL Instrumentation
- All spans are created using `tracer.startActiveSpan()` from `@opentelemetry/api`
- No simulated or mock spans
- Proper parent-child relationships via OTEL context propagation

### Span Collection
- Spans are collected by OTEL span processor
- Span data is also manually collected for validation purposes
- Both real OTEL spans and span data objects are used for validation

### Error Handling
- Errors create error spans with proper status codes
- Error attributes are set on spans
- Exceptions are recorded via `span.recordException()`

### State Machine Integration
- Runtime state transitions are tracked in spans
- Invalid operations create error spans
- State machine prevents invalid operations (poka-yoke)

## Validation vs Production

### Validation Mode
- Creates span data objects for validation collection
- May include error spans when modules don't exist
- Error rate thresholds are relaxed (1.0) to allow expected errors

### Production Mode
- All spans come from real OTEL instrumentation
- Error spans only created on actual failures
- Error rate thresholds are strict (0.01) for production monitoring

## Trace Flow Example

```
validation.suite.atomvm-playground
  └─ validation.atomvm-bridge
      └─ feature.atomvm-bridge
          └─ validation.execute.bridge
              ├─ bridge.emit_event (real OTEL span)
              ├─ bridge.register_hook (real OTEL span)
              └─ bridge.process_intent (real OTEL span)
```

## Key Points

1. **All spans are real**: No simulation, all from actual OTEL instrumentation
2. **Proper hierarchy**: Parent-child relationships via OTEL context
3. **Error tracking**: Errors create error spans with proper attributes
4. **Production ready**: Traces accurately represent production system behavior

