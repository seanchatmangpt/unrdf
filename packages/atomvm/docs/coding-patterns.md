# Coding Patterns and Standards

## Named Constants Pattern

### Overview

Extract magic numbers to named constants for clarity, maintainability, and self-documentation.

### Pattern

Use named constants instead of magic numbers for:
- Timeouts and delays (milliseconds)
- Exit codes
- Retry attempts and intervals
- Buffer sizes
- Configuration values
- Repeated literals

### Naming Convention

Constants use descriptive names with units:
- `*_MS` suffix for milliseconds
- `*_SIZE` suffix for sizes
- `*_CODE` suffix for exit codes
- `*_ATTEMPTS` suffix for retry counts
- `*_INTERVAL_MS` suffix for polling intervals

### Examples

#### Before (Magic Numbers)

```javascript
setTimeout(() => {
  checkStatus();
}, 1000);

if (code === 0) {
  resolve({ status: 'ok', exitCode: 0 });
}
```

#### After (Named Constants)

```javascript
/** @constant {number} Delay (ms) before checking status */
const STATUS_CHECK_DELAY_MS = 1000;

/** @constant {number} Exit code indicating successful execution */
const SUCCESS_EXIT_CODE = 0;

setTimeout(() => {
  checkStatus();
}, STATUS_CHECK_DELAY_MS);

if (code === SUCCESS_EXIT_CODE) {
  resolve({ status: 'ok', exitCode: SUCCESS_EXIT_CODE });
}
```

### Benefits

1. **Clarity**: Constant names explain what the value represents
2. **Maintainability**: Change value in one place instead of searching for magic numbers
3. **Self-documentation**: Code reads more clearly without comments
4. **Type safety**: Easier to validate and type-check constants
5. **Consistency**: Same values used consistently across codebase

### Implementation

Constants are defined at the top of the file, grouped by category:

```javascript
/** @constant {number} Delay before checking if Module is ready after script load */
const MODULE_LOAD_CHECK_DELAY_MS = 100;

/** @constant {number} Interval (ms) for polling Module initialization status */
const MODULE_INIT_CHECK_INTERVAL_MS = 50;

/** @constant {number} Maximum attempts to check Module initialization (10s total at 50ms intervals) */
const MODULE_INIT_MAX_ATTEMPTS = 200;

/** @constant {number} Delay (ms) before checking execution status */
const EXECUTION_STATUS_CHECK_DELAY_MS = 1000;

/** @constant {number} Maximum time (ms) to wait for execution to complete */
const EXECUTION_TIMEOUT_MS = 30000;

/** @constant {number} Exit code indicating successful execution */
const SUCCESS_EXIT_CODE = 0;
```

### When to Apply

Apply this pattern when:
- ✅ Value is used multiple times
- ✅ Value represents a configuration or timeout
- ✅ Value may need to change in the future
- ✅ Value is not immediately obvious (e.g., `30000` vs `EXECUTION_TIMEOUT_MS`)
- ✅ Value is part of an API contract (e.g., exit codes)

Don't apply when:
- ❌ Value is a simple counter (e.g., `i < 10` in a loop)
- ❌ Value is part of a mathematical formula (e.g., `Math.PI * 2`)
- ❌ Value is a well-known constant (e.g., `0`, `1`, `-1` in simple contexts)

### Code Review Checklist

- [ ] No magic numbers for timeouts, delays, or intervals
- [ ] No magic numbers for exit codes or status values
- [ ] Constants are defined at the top of the file
- [ ] Constants have descriptive names with units
- [ ] Constants have comments explaining their purpose
- [ ] Constants are grouped by category

### Measurement

**Baseline** (before improvement): 15 magic numbers across source files
**After improvement**: 0 magic numbers (all extracted to named constants)
**Improvement**: 100% elimination of magic numbers

### Files Updated

- `src/atomvm-runtime.mjs`: 6 constants (timeouts, intervals, exit codes)
- `src/service-worker-manager.mjs`: 6 constants (timeouts, delays, sizes)
- `src/index.mjs`: 3 constants (delays, timeouts)
- `src/node-runtime.mjs`: 1 constant (exit code)

### Related Patterns

- **Poka-Yoke Design**: Named constants prevent errors from using wrong values
- **Self-Documenting Code**: Constants make code readable without comments

