# How-To: Validate KGC-4D Integration

**Task-oriented**: Validate that events emitted from Erlang processes reach KGC-4D.

## Problem

You need to verify that:
- Processes can emit events
- Events are structured correctly
- Events reach KGC-4D
- Events are queryable in knowledge graph

## Solution

### 1. Build KGC-4D Test Module

```bash
cd packages/atomvm
pnpm run build:erlang kgc4d-test
```

### 2. Run Validation

In the playground:
1. Enter module name: `kgc4d-test`
2. Click "Run Validation Suite"
3. Check "KGC-4D Event Integration" result

### 3. Verify Results

The validation checks:
- ✅ Event emission works
- ✅ Event structure is valid
- ✅ Events reach KGC-4D (simulated)

## Production Module

The `kgc4d-test.erl` module demonstrates:
- Event emitter process
- Structured event format (type, timestamp, payload)
- Event ordering
- Event persistence

## Event Structure

Events must have:
- `type` - Event type (string)
- `timestamp` - Timestamp (number)
- `payload` - Event payload (object)
- `module` - Module name (string)

## See Also

- [Validate Processes](./validate-processes.md)
- [Validate Supervision Trees](./validate-supervision.md)
- [Validation API Reference](../reference/validation-api.md)

