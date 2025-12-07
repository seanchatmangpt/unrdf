# Production Scenarios Reference

**Information-oriented**: Specifications for production validation scenarios.

## Process Lifecycle Scenario

**Module:** `process-test.erl`

**Validates:**
- Process spawn
- Message passing
- Crash handling
- Process isolation

**Test Flow:**
1. Coordinator spawns worker processes
2. Workers receive messages
3. Workers respond to messages
4. Worker crashes (test scenario)
5. Verify crash is handled

## Supervision Tree Scenario

**Module:** `supervision-test.erl`

**Validates:**
- Supervisor creation
- Child process supervision
- Automatic restart on crash
- State recovery

**Test Flow:**
1. Supervisor spawns worker
2. Worker processes task
3. Worker crashes (test scenario)
4. Supervisor restarts worker
5. Verify worker state is recovered

## KGC-4D Event Integration Scenario

**Module:** `kgc4d-test.erl`

**Validates:**
- Event emission
- Event structure
- Event ordering
- Event persistence

**Test Flow:**
1. Event emitter process starts
2. Process emits structured events
3. Events are validated for structure
4. Events are stored (simulated KGC-4D)

**Event Structure:**
```javascript
{
  type: string,        // Event type (e.g., "PROCESS_STARTED")
  timestamp: number,   // Timestamp in milliseconds
  payload: object,     // Event payload
  module: string       // Module name
}
```

## State Machine Scenario

**Validates:**
- Valid state transitions
- Invalid operations are prevented
- Type guards work correctly

**Test Flow:**
1. Create runtime in Uninitialized state
2. Attempt invalid operation (should fail)
3. Verify type guards work
4. Verify state transitions are valid

## Dual Runtime Scenario

**Validates:**
- Browser runtime works
- Node.js runtime works
- Both runtimes work identically

**Test Flow:**
1. Create browser runtime
2. Create Node.js runtime
3. Verify both have same state machine structure
4. Verify both can load modules

## See Also

- [Validation API](./validation-api.md)
- [How-To Guides](../how-to/)

