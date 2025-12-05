# Poka-Yoke Design (Error Prevention) - Multi-Step Workflow

## Purpose

This command guides agents to design code that prevents errors at compile time through type safety and invariants. Poka-yoke means "mistake-proofing" - making errors impossible through design. Experts use the type system to prevent entire classes of errors.

## Workflow Overview

```
Step 1: Identify Error Modes → Step 2: Design Type-Level Prevention → Step 3: Add Compile-Time Checks → Step 4: Verify Prevention (with Measurement) → Step 5: Document Invariants (with Control)
```

## Step-by-Step Instructions

### Step 1: Identify Error Modes

**Action**: List all ways code can fail at runtime.

**Error mode categories**:

1. **Invalid state** - States that shouldn't exist
   - Example: Negative count, empty required field, invalid variant

2. **Invalid input** - Inputs that cause errors
   - Example: Empty string when non-empty required, null when non-null required

3. **Invalid operations** - Operations that fail in certain states
   - Example: Reading from closed file, modifying immutable data

4. **Resource errors** - Resource-related failures
   - Example: Out of memory, file not found, network error

5. **Logic errors** - Errors in program logic
   - Example: Division by zero, index out of bounds, overflow

**Action**: Create error mode inventory

```markdown
## Error Modes Inventory

### Invalid State

- [ ] Counter can be negative (should be >= 0)
- [ ] Parser can be in invalid state after error

### Invalid Input

- [ ] Empty string passed to `parse_number` (should be non-empty)
- [ ] Null/null passed where value required

### Invalid Operations

- [ ] Reading from closed file handle
- [ ] Modifying data after finalization

### Resource Errors

- [ ] File not found errors
- [ ] Network connection errors

### Logic Errors

- [ ] Division by zero
- [ ] Index out of bounds
- [ ] Integer overflow
```

---

### Step 2: Design Type-Level Prevention

**Action**: Use javascript's type system to make errors impossible.

#### 2.1: Use Newtypes for Validation

**Action**: Create newtypes that enforce invariants.

**Example**:

```javascript
// ❌ BAD: Can have invalid state
class Counter {
  constructor(value) {
    this.value = value; // Can be negative!
  }
}

// ✅ GOOD: Validation prevents invalid state
class Counter {
  constructor(value) {
    if (value < 0) {
      throw new Error("Counter value cannot be negative");
    }
    this.value = value; // Cannot be negative - validation prevents it
  }

  increment() {
    this.value = Math.min(this.value + 1, Number.MAX_SAFE_INTEGER); // Prevents overflow
  }
}
```

#### 2.2: Use Enums for State Machines

**Action**: Use enums to represent valid states only.

**Example**:

```javascript
// ❌ BAD: Can be in invalid state
class Parser {
  constructor() {
    this.isOpen = false;
    this.isClosed = false;
    // Can have both true - invalid state!
  }
}

// ✅ GOOD: Enum prevents invalid states
const ParserState = {
  Initial: "Initial",
  Parsing: "Parsing",
  Complete: "Complete",
  Error: "Error",
};

class Parser {
  constructor() {
    this.state = ParserState.Initial; // Only valid states possible
  }
}
```

#### 2.3: Use Option/Result for Nullable Values

**Action**: Use `T | null` instead of nullable types.

**Example**:

```javascript
// ❌ BAD: Can pass null, causes runtime error
function process(value) {
  return parseInt(value); // Throws if empty or null!
}

// ✅ GOOD: Validation forces handling of null
async function process(value) {
  if (value === null || value === undefined) {
    throw new Error("MissingInput");
  }
  if (value.length === 0) {
    throw new Error("EmptyInput");
  }
  const parsed = parseInt(value);
  if (isNaN(parsed)) {
    throw new Error("InvalidFormat");
  }
  return parsed;
}
```

#### 2.4: Use for Type-Level Invariants

**Action**: Use to encode invariants in types.

**Example**:

```javascript
// Type-level invariants;

// Type-level invariant: FileHandle<Open> vs FileHandle<Closed>
class Open;
class Closed;

class FileHandle {
    file: File,
    _state: });
FileHandle<Open> {
    read() => Promise<Array<u8> | Error> {
        // Can only read when Open
});
    close() => FileHandle<Closed> {
        // Consumes Open handle, returns Closed handle
        FileHandle {
            file: this.file,
            _state: });
});
});
// Cannot read from Closed handle - compiler error!
```

---

### Step 3: Add Compile-Time Checks

**Action**: Leverage javascript's compiler to catch errors.

#### 3.1: Use Type Bounds

**Action**: Add trait bounds to restrict valid types.

**Example**:

```javascript
// Function only accepts types that can be converted to string
function printValue(value) {
    if (typeof value !== 'string' && typeof value !== 'number' && typeof value !== 'object') {
        throw new TypeError('Value must be displayable');
    }
    console.log(String(value));
}
// Type checking ensures only valid types are passed
```

#### 3.2: Use Const Generics for Sizes

**Action**: Use const generics to prevent size errors.

**Example**:

```javascript
// Array size validated - prevents index errors
function processArray(arr) {
    if (!Array.isArray(arr)) {
        throw new TypeError('Expected array');
    }
    // Size is validated - cannot index out of bounds
    return arr;
}
```

#### 3.3: Use Lifetimes to Prevent Use-After-Free

**Action**: Use lifetimes to prevent memory errors.

**Example**:

```javascript
// Reference validation ensures data doesn't outlive scope
function process(data) {
    if (typeof data !== 'string') {
        throw new TypeError('Expected string');
    }
    // Returned reference tied to input - prevents use-after-free
    return data;
}
```

#### 3.4: Use Unsafe Blocks Sparingly

**Action**: Mark unsafe code explicitly, use safe abstractions.

**Example**:

```javascript
// Safe abstraction over potentially unsafe operations
async function safeOperation(input) {
    if (!(input instanceof Uint8Array)) {
        throw new TypeError('Expected Uint8Array');
    }
    // Internal validation ensures safety
    // Public API is safe - runtime checks enforce safety
    try {
        // Validated operations here, interface is safe
        return input.length;
    } catch (error) {
        return error;
    }
}
```

---

### Step 4: Verify Prevention

**Action**: Ensure type system prevents errors.

#### 4.1: Attempt Invalid Operations

**Action**: Try to write code that should fail to compile.

**Example**:

```javascript
// Try to create invalid state - should fail validation
const counter = new Counter(-1); // Should throw error if Counter validates input

// Try to use invalid state - should fail to compile
let closed_file: FileHandle<Closed> = ...;
closed_file.read(); // Should be compile error - can't read from closed file
```

**Verification**: Code that should be invalid doesn't compile

```bash
pnpm lint
# Should show compile errors for invalid operations
```

#### 4.2: Verify Valid Operations Compile

**Action**: Ensure valid code compiles successfully.

**Example**:

```javascript
// Valid operations should compile
let counter = Counter.new(0); // Valid
counter.increment(); // Valid

let open_file: FileHandle<Open> = ...;
open_file.read(); // Valid - can read from open file
```

**Verification**: Valid code compiles

```bash
pnpm lint
# Should compile successfully
```

#### 4.3: Test Runtime Behavior

**Action**: Verify type-level prevention works at runtime.

```bash
pnpm test
# Tests should pass - type system prevents errors
```

#### 4.4: Measure Error Prevention (DMAIC Measurement)

**Action**: Measure error prevention effectiveness against baseline.

**Measurement**:

- Count errors prevented by type system
- Compare to baseline (errors that would occur without types)
- Calculate prevention percentage
- Verify success criteria met

**Action**: Measure error prevention

```bash
# Count compile-time errors caught (prevented runtime errors)
pnpm lint 2>&1 | grep -c "error\["
# Output: 5 compile-time errors (prevented 5 runtime errors)

# Count runtime errors (should be 0 with type prevention)
pnpm test 2>&1 | grep -c "panicked"
# Output: 0 panics (type system prevented errors)

# Calculate prevention
# Baseline: 5 potential runtime errors
# After type prevention: 0 runtime errors (caught at compile time)
# Prevention: 100% (5/5 errors prevented)
```

**Example error prevention measurement**:

```markdown
## Error Prevention Measurement

**Baseline**: 5 potential runtime errors (without type prevention)
**After Type Prevention**: 0 runtime errors (caught at compile time)
**Prevention**: 100% (5/5 errors prevented)

**By Error Type**:

- Invalid state errors: 2 → 0 (100% prevented)
- Invalid input errors: 2 → 0 (100% prevented)
- Invalid operation errors: 1 → 0 (100% prevented)

**Success Criteria Met**: ✅

- All errors caught at compile time ✅
- No runtime errors ✅
- Type system prevents invalid states ✅
```

---

### Step 5: Document Invariants

**Action**: Explain why design prevents errors.

#### 5.1: Document Type Invariants

**Action**: Document invariants enforced by types.

**Example**:

```javascript
/// Counter that cannot be negative.
///
/// **Poka-yoke**: Uses validation to prevent negative values
/// at runtime. The validation makes invalid states impossible.

class Counter {
  constructor(number) {
    this.value = number;
  } // Invariant: Always >= 0 (enforced by type});
```

#### 5.2: Document State Machine Invariants

**Action**: Document valid state transitions.

**Example**:

```javascript
/// Parser state machine.
///
/// **Poka-yoke**: Enum prevents invalid states. Cannot be both open and closed.
/// Valid transitions:
/// - Initial => Parsing => Complete
/// - Initial => Parsing => Error
/// - Cannot transition from Complete/Error back to Parsing (type prevents it)
const ParserState = {
  Initial: 'Initial',
  Parsing: 'Parsing',
  Complete: 'Complete',
  Error: 'Error'
};);
```

#### 5.3: Document Usage Patterns

**Action**: Document how to use types safely.

**Example**:

```javascript
/// File handle that prevents use-after-close errors.
///
/// **Poka-yoke**: Type-level state prevents reading from closed files.
///
/// # Example
///
/// ```javascript
/// let file = FileHandle.<Open>.open("file.txt")?;
/// let data = file.read()?; // Valid - file is open
/// let closed = file.close();
/// // closed.read() // Compile error - cannot read from closed file
/// ```
```

#### 5.4: Establish Controls (DMAIC Control)

**Action**: Set up controls to ensure error prevention is sustained.

**Controls**:

- **Code review**: Check for type safety in reviews
- **Automated checks**: Lint rules to flag unsafe patterns
- **Monitoring**: Track error prevention effectiveness over time
- **Standards**: Document type safety patterns in coding standards

**Action**: Create todo list for controls (10+ items)

```markdown
## Poka-Yoke Control Todos (10+ items)

**Code Review Controls**:

- [ ] Add checklist item: Use type system to prevent errors
- [ ] Add checklist item: No runtime error handling for invalid states
- [ ] Update code review process to include type safety checks
- [ ] Verify checklist is used in reviews

**Automated Checks**:

- [ ] Add lint rule: Flag unsafe patterns
- [ ] Add lint rule: Flag missing type safety
- [ ] Configure CI check: Verify type safety
- [ ] Review lint rules monthly

**Monitoring Controls**:

- [ ] Set up error prevention tracking dashboard
- [ ] Configure alerts if runtime errors increase
- [ ] Review error prevention trends weekly
- [ ] Document error prevention patterns

**Standards Controls**:

- [ ] Add standard: Use type system to prevent errors
- [ ] Add standard: Make invalid states unrepresentable
- [ ] Update team documentation with standards
- [ ] Verify standards are followed in code reviews
```

**Execution**:

1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement controls)
3. Mark todos as completed as controls are implemented
4. Verify each control works before moving to next
5. Continue until all controls implemented

**Principle**: Implement controls to sustain error prevention, don't just document them. Todos track progress, controls prevent regression.

#### 5.5: Monitor (DMAIC Control)

**Action**: Monitor to ensure error prevention is sustained.

**Monitoring**:

- Track runtime error count over time
- Set up alerts for regression
- Review trends periodically
- Adjust controls if needed

**Action**: Set up monitoring

```bash
# Monitor runtime errors
# Run weekly: pnpm test 2>&1 | grep -c "panicked"
# Alert if error count > 0

# Track trends
# Week 1: 5 potential errors (baseline - without type prevention)
# Week 2: 0 errors (after type prevention)
# Week 3: 0 errors (controls working)
# Week 4: 0 errors (sustained)
```

---

## Complete Workflow Example

```javascript
// Step 1: Identify Error Modes
// Error: Counter can be negative
// Error: Can read from closed file

// Step 2: Design Type-Level Prevention
// Counter: Use validation to prevent negative values
// FileHandle: Use for state (Open/Closed)

// Step 3: Add Compile-Time Checks
class Counter {
  constructor(number) {
    this.value = number;
  } // Prevents negative
});
FileState {
    Open(File),
    Closed});
// Step 4: Verify Prevention
pnpm lint
// Attempt invalid operations - should fail to compile
// let counter = Counter { value: -1 }; // Compile error!

// Step 5: Document Invariants
/// Counter that cannot be negative (Poka-yoke: number type prevents it)
```

## Integration with Other Commands

- **[Gemba Walk](./gemba-walk.md)** - Verify actual type behavior matches design
- **[Root Cause Analysis](./root-cause-analysis.md)** - Understand why errors occur, then prevent with types
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC measurement and control steps integrated into this workflow
- **[Eliminate Muda](./eliminate-muda.md)** - Remove error-prone patterns, replace with type-safe designs

## Expert Insights

**Why this matters**: Runtime errors are expensive. Type-level prevention catches errors at compile time, before they reach production.

**Key principle**: "Make invalid states unrepresentable" - Use types to make errors impossible.

**Remember**: The type system is your friend. Use it to prevent entire classes of errors. If you can't represent an invalid state in your type system, you've prevented that error.

**Poka-yoke principle**: "Prevention is better than detection" - Prevent errors at compile time rather than catching them at runtime.

**DfLSS alignment**: Poka-yoke (defect prevention) is the Six Sigma component of DfLSS (Design for Lean Six Sigma). However, defect prevention alone is incomplete - DfLSS addresses both efficiency (waste elimination) AND quality (defect prevention). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. When preventing defects with types, also consider eliminating waste (unnecessary complexity, redundant checks). See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

End Command ---
