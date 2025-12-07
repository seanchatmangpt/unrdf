# How-To: Validate Supervision Trees

**Task-oriented**: Validate that supervisor restart behavior works correctly.

## Problem

You need to verify that:
- Supervisors can be created
- Child processes are supervised
- Crashes trigger restarts
- State is recovered after restart

## Solution

### 1. Build Supervision Test Module

```bash
cd packages/atomvm
pnpm run build:erlang supervision-test
```

### 2. Run Validation

In the playground:
1. Enter module name: `supervision-test`
2. Click "Run Validation Suite"
3. Check "Supervision Tree" result

### 3. Verify Results

The validation checks:
- ✅ Supervisor creation works
- ✅ Child process restart works
- ✅ State recovery works

## Production Module

The `supervision-test.erl` module demonstrates:
- Supervisor process managing children
- Link-based supervision
- Automatic restart on crash
- State recovery

## See Also

- [Validate Processes](./validate-processes.md)
- [Validate KGC-4D Integration](./validate-kgc4d.md)
- [Validation API Reference](../reference/validation-api.md)

