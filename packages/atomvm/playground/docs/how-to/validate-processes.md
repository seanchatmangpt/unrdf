# How-To: Validate Process Lifecycle

**Task-oriented**: Validate that Erlang processes work correctly in AtomVM.

## Problem

You need to verify that:
- Processes can be spawned
- Messages can be sent/received
- Crashes are handled
- Processes are restarted by supervisors

## Solution

### 1. Build Process Test Module

```bash
cd packages/atomvm
pnpm run build:erlang process-test
```

### 2. Run Validation

In the playground:
1. Enter module name: `process-test`
2. Click "Run Validation Suite"
3. Check "Process Lifecycle" result

### 3. Verify Results

The validation checks:
- ✅ Module file exists
- ✅ Process spawn works
- ✅ Message passing works
- ✅ Crash handling works
- ✅ Restart works

## Production Module

The `process-test.erl` module demonstrates:
- Coordinator process spawning workers
- Message passing between processes
- Crash handling
- Process isolation

## See Also

- [Validate Supervision Trees](./validate-supervision.md)
- [Validate KGC-4D Integration](./validate-kgc4d.md)
- [Validation API Reference](../reference/validation-api.md)

