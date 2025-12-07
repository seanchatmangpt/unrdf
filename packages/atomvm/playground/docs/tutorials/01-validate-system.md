# Tutorial: Validate System Works

**Learning-oriented**: Step-by-step guide to validate that AtomVM works in production scenarios.

## Goal

Validate that AtomVM cannot fail in production scenarios:
- Process lifecycle works
- Supervision trees work
- KGC-4D integration works
- Both browser and Node.js runtimes work

## Prerequisites

- Erlang toolchain installed (`erlc`, `packbeam`)
- Node.js 18+
- pnpm installed

## Steps

### 1. Build a Validation Module

Build the process test module:

```bash
cd packages/atomvm
pnpm run build:erlang process-test
```

This creates `public/process-test.avm`.

### 2. Start Playground

```bash
cd playground
pnpm dev
```

### 3. Open Browser

Navigate to `http://localhost:3001`

### 4. Run Validation

1. Enter module name: `process-test`
2. Click "Run Validation Suite"
3. Watch validation results

### 5. Verify Results

All validations should pass:
- ✅ Process Lifecycle
- ✅ State Machine Integrity
- ✅ KGC-4D Event Integration
- ✅ Dual Runtime (Browser + Node.js)
- ✅ Supervision Tree

## What Happens

The validation suite:
1. Checks that module files exist
2. Validates state machine prevents invalid operations
3. Verifies event structure for KGC-4D
4. Tests both browser and Node.js runtimes
5. Validates supervision behavior

## Next Steps

- Learn how to validate specific scenarios: [How-To Guides](../how-to/)
- Understand the validation API: [Reference](../reference/)
- Learn about the methodology: [Big Bang 80/20](../explanation/big-bang-80-20.md)

