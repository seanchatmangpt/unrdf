# Hook Primitives Protocol

**Kernel Architecture**: `hook_primitives.erl` is the kernel. JavaScript is a pluggable execution engine. This document defines the protocol between them.

## Overview

The protocol enables Erlang (kernel) to:
- Register hooks with JavaScript (execution engine)
- Execute hook chains via JavaScript
- Receive execution results from JavaScript

JavaScript (execution engine) cannot:
- Invent hook names or triggers
- Register hooks independently
- Modify hook definitions

## Erlang → JavaScript Protocol

All commands are sent via `io:format` with the prefix `HOOK_PRIMITIVE:`.

### Command Format

```
HOOK_PRIMITIVE:<command>:<arg1>:<arg2>:...
```

### Commands

#### 1. Register Hook Metadata

**Command**: `HOOK_PRIMITIVE:register:<HookDefJSON>`

**Purpose**: Register hook metadata with JavaScript execution engine.

**HookDefJSON Schema**:
```json
{
  "name": "validate_iri",
  "trigger": "before_add",
  "hasValidate": true,
  "hasTransform": false
}
```

**Fields**:
- `name` (string): Hook name (atom converted to string)
- `trigger` (string): Hook trigger (atom converted to string, e.g., "before_add")
- `hasValidate` (boolean): Whether hook has validation function
- `hasTransform` (boolean): Whether hook has transformation function

**Example**:
```
HOOK_PRIMITIVE:register:{"name":"validate_iri","trigger":"before_add","hasValidate":true,"hasTransform":false}
```

**JavaScript Response**: None (fire-and-forget registration)

---

#### 2. Execute Hook Chain

**Command**: `HOOK_PRIMITIVE:execute:<Trigger>:<DataJSON>:<ChainKey>`

**Purpose**: Execute all hooks for a trigger via JavaScript execution engine.

**Arguments**:
- `Trigger` (string): Hook trigger (atom converted to string)
- `DataJSON` (string): Data to process (JSON-encoded)
- `ChainKey` (string): Chain key identifying which hooks to execute (e.g., "hook1|hook2|hook3")

**Example**:
```
HOOK_PRIMITIVE:execute:before_add:{"type":"CREATE","resource":"contract"}:validate_iri|normalize_event
```

**JavaScript Response**: Must send `{hook_primitive_result, ChainKey, Result}` message (see JS → Erlang Protocol)

---

#### 3. Pre-compile Hook Chain

**Command**: `HOOK_PRIMITIVE:chain:<ChainKey>:<HookNamesJSON>`

**Purpose**: Request JavaScript to pre-compile a hook chain for performance.

**Arguments**:
- `ChainKey` (string): Chain key (e.g., "hook1|hook2|hook3")
- `HookNamesJSON` (string): JSON array of hook names

**HookNamesJSON Schema**:
```json
["validate_iri", "normalize_event"]
```

**Example**:
```
HOOK_PRIMITIVE:chain:validate_iri|normalize_event:["validate_iri","normalize_event"]
```

**JavaScript Response**: None (fire-and-forget compilation)

---

#### 4. Execute Compiled Chain

**Command**: `HOOK_PRIMITIVE:execute_chain:<ChainKey>:<DataJSON>`

**Purpose**: Execute a pre-compiled hook chain.

**Arguments**:
- `ChainKey` (string): Chain key identifying the compiled chain
- `DataJSON` (string): Data to process (JSON-encoded)

**Example**:
```
HOOK_PRIMITIVE:execute_chain:validate_iri|normalize_event:{"type":"CREATE","resource":"contract"}
```

**JavaScript Response**: Must send `{hook_primitive_result, ChainKey, Result}` message (see JS → Erlang Protocol)

---

## JavaScript → Erlang Protocol

JavaScript sends results back to Erlang via process messages.

### Message Format

```erlang
{hook_primitive_result, ChainKey, Result}
```

**Where**:
- `ChainKey` (string): Chain key from the execute command
- `Result` (map): Result map with the following shape

### Result Map Schema

```erlang
#{
    valid  => boolean(),    % Required: true if validation/execution succeeded
    data   => term(),        % Required: Processed data (often same shape as input)
    errors => term()         % Optional: Error details if valid = false
}
```

### Result Examples

**Success**:
```erlang
{hook_primitive_result, "validate_iri|normalize_event", #{
    valid => true,
    data => #{type => <<"CREATE">>, resource => <<"contract">>, normalized => true}
}}
```

**Validation Failure**:
```erlang
{hook_primitive_result, "validate_iri|normalize_event", #{
    valid => false,
    data => #{type => <<"CREATE">>, resource => <<"contract">>},
    errors => <<"Invalid IRI format">>
}}
```

**Error**:
```erlang
{hook_primitive_result, "validate_iri|normalize_event", #{
    valid => false,
    data => #{type => <<"CREATE">>, resource => <<"contract">>},
    errors => <<"Hook execution failed: timeout">>
}}
```

### Sending Messages to Erlang

**Node.js**: Use AtomVM runtime message passing (implementation-specific)

**Browser**: Use AtomVM WASM message passing (implementation-specific)

**Key Requirement**: The message must be delivered to the Erlang process that called `execute/2` or `execute_chain/2`.

---

## Error Handling Protocol

### Timeout

If JavaScript does not respond within 1000ms, Erlang returns:
```erlang
{error, timeout}
```

### Invalid Result Shape

If JavaScript sends an invalid result shape, Erlang returns:
```erlang
{invalid, {unexpected_result, Result}}
```

### Encoding Errors

If data encoding fails, Erlang returns:
```erlang
{error, {encoding_failed, Error, Reason}}
```

---

## Trigger Mapping

Erlang triggers (atoms) are converted to strings for JavaScript:
- `before_add` → `"before_add"`
- `after_add` → `"after_add"`
- `before_query` → `"before_query"`
- etc.

JavaScript must map these back to hook trigger strings when executing hooks.

---

## Chain Key Format

Chain keys are generated from hook names:
- Hook names: `[validate_iri, normalize_event]`
- Chain key: `"validate_iri|normalize_event"`

The pipe (`|`) character is used as a separator. Hook names are atoms converted to strings.

---

## Implementation Notes

1. **Fire-and-Forget Commands**: `register` and `chain` commands are fire-and-forget. No response expected.

2. **Synchronous Commands**: `execute` and `execute_chain` commands require a response. Erlang waits up to 1000ms.

3. **Result Normalization**: JavaScript results are normalized by `handle_result/2` in `hook_primitives.erl` to ensure consistent outcome shapes.

4. **Error Recovery**: If JavaScript fails to respond, Erlang returns `{error, timeout}`. The caller can decide how to handle this.

5. **Data Encoding**: Data is JSON-encoded for transmission. Complex Erlang terms may be base64-encoded.

---

## Example Flow

1. **Erlang registers hook**:
   ```
   HOOK_PRIMITIVE:register:{"name":"validate_iri","trigger":"before_add","hasValidate":true,"hasTransform":false}
   ```

2. **Erlang executes hook**:
   ```
   HOOK_PRIMITIVE:execute:before_add:{"type":"CREATE","resource":"contract"}:validate_iri
   ```

3. **JavaScript executes hook** (internal):
   - Looks up hook `validate_iri` for trigger `before_add`
   - Executes validation
   - Prepares result

4. **JavaScript sends result**:
   ```erlang
   {hook_primitive_result, "validate_iri", #{
       valid => true,
       data => #{type => <<"CREATE">>, resource => <<"contract">>}
   }}
   ```

5. **Erlang receives result**:
   - `execute/2` receives message
   - `handle_result/2` normalizes to `{valid, Data}`
   - Returns to caller

---

## See Also

- `hook_primitives.erl` - Kernel implementation
- `hook-primitive-bridge.mjs` - JavaScript execution engine
- `bridge-interceptor.mjs` - Protocol handler

