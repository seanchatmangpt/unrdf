# Expert-Level Testing Patterns - Multi-Step Workflow

## Purpose

This command guides agents through implementing expert-level testing patterns that catch 80% of production bugs. It breaks down complex testing scenarios into clear, sequential steps with examples and validation checkpoints.

## Workflow Overview

```
Step 1: Identify Test Type → Step 2: Choose Pattern → Step 3: Implement Test → Step 4: Verify Coverage → Step 5: Validate Quality
```

## Documentation Reference

For complete testing documentation, see:

- **[Getting Started Guide](../../docs/getting-started.md)** - Quick start with verified examples
- **[User Guide](../../docs/USER_GUIDE.md)** - Comprehensive testing guide with patterns
- **[API Reference](../../docs/api/)** - Complete API documentation
- **[Architecture](../../docs/ARCHITECTURE.md)** - Design principles

## Core Principle: 80/20 Rule

**Expert testing focuses on the 20% of test cases that catch 80% of bugs**:

- Error paths (not just happy path)
- Boundary conditions (not just normal values)
- Resource cleanup (not just normal execution)
- Concurrency (not just single-threaded)
- Real dependencies (not just mocks)

## Step-by-Step Pattern Implementation

### Pattern 1: Error Path Testing (Critical - 80% of bugs)

#### Step 1.1: Identify Error Scenarios

**Action**: List all possible error conditions for the function/feature.

**Questions to ask**:

- What inputs cause errors?
- What error variants exist?
- Can errors be recovered from?
- Are errors properly propagated?

**Example**: For `parseNumber(input: string) => Promise<number | Error>`

- Empty input → `ParseError.EmptyInput`
- Invalid format → `ParseError.InvalidFormat`
- Overflow → `ParseError.Overflow`
- Edge cases: `"-0"`, `" 42 "`, etc.

#### Step 1.2: Create Test Cases

**Action**: Create test cases for each error scenario.

```javascript
import { expect, test } from 'vitest';

test('test_parse_number_all_error_paths', () => {
    // Arrange: Test all error variants
    const testCases = [
        ['', 'EmptyInput'],
        ['abc', 'InvalidFormat'],
        ['999999999999999999999', 'Overflow'],
        ['-0', 'InvalidFormat'], // Edge case
        [' 42 ', 'InvalidFormat'], // Whitespace
    ];

    // Act & Assert: Verify each error path
    for (const [input, expectedError] of testCases) {
        const result = parseNumber(input);
        expect(result).rejects.toThrow();
        try {
            await result;
            throw new Error(`Expected error for input: ${input}`);
        } catch (error) {
            expect(error.message).toContain(expectedError);
        }
    }
});
```

#### Step 1.3: Test Error Recovery

**Action**: Verify system can recover from errors.

```javascript
test("test_error_recovery", async () => {
  // Arrange: Create parser
  const parser = new NumberParser();

  // Act: Cause error
  await expect(parser.parse("invalid")).rejects.toThrow();

  // Assert: Parser should still be usable after error
  const result = await parser.parse("42");
  expect(result).toBe(42);
});
```

#### Step 1.4: Verify Coverage

**Checklist**:

- [ ] All error variants tested
- [ ] Error messages verified
- [ ] Error recovery tested
- [ ] Edge cases covered

**Reference**: See [User Guide - Best Practices](../../docs/USER_GUIDE.md#best-practices)

---

### Pattern 2: Boundary Condition Testing

#### Step 2.1: Identify Boundaries

**Action**: List all boundary conditions.

**Common boundaries**:

- Empty collections
- Single item
- Maximum size
- Zero values
- Negative values (if applicable)
- Minimum/maximum ranges

#### Step 2.2: Create Boundary Tests

**Action**: Test each boundary condition.

```javascript
import { expect, test } from 'vitest';

test('test_collection_boundaries', () => {
    // Arrange: Test empty collection
    const empty = [];
    expect(processCollection(empty)).toBe(0);

    // Arrange: Test single item
    const single = [42];
    expect(processCollection(single)).toBe(42);

    // Arrange: Test max capacity (avoid OOM in test)
    const maxSize = new Array(10000).fill(0);
    const result = processCollection(maxSize);
    expect(result).toBeDefined();

    // Arrange: Test zero values
    const zeros = new Array(100).fill(0);
    expect(processCollection(zeros)).toBe(0);

    // Arrange: Test negative values (if applicable)
    const negatives = [-1, -2, -3];
    try {
        const result = processCollection(negatives);
        expect(result).toBeDefined();
    } catch (error) {
        expect(error.message).toContain('NegativeNotAllowed');
    }
});
```

#### Step 2.3: Verify Coverage

**Checklist**:

- [ ] Empty collection tested
- [ ] Single item tested
- [ ] Maximum size tested (safely)
- [ ] Zero values tested
- [ ] Negative values tested (if applicable)

---

### Pattern 3: Resource Cleanup Testing

#### Step 3.1: Identify Resources

**Action**: List all resources that need cleanup.

**Common resources**:

- File handles
- Network connections
- Database connections
- Memory allocations
- Locks/mutexes

#### Step 3.2: Test Normal Cleanup

**Action**: Verify resources are cleaned up in normal execution.

```javascript
import { expect, test } from 'vitest';

let dropCount = 0;

class TestResource {
    constructor(id) {
        this.id = id;
    }

    [Symbol.dispose]() {
        dropCount++;
    }
}

test('test_resource_cleanup_normal_path', () => {
    // Arrange: Reset counter
    dropCount = 0;

    // Act: Create and dispose resource
    {
        using resource = new TestResource(1);
        // Resource should dispose here
    }

    // Assert: Verify cleanup
    expect(dropCount).toBe(1);
});
```

#### Step 3.3: Test Error Path Cleanup

**Action**: Verify resources are cleaned up even when errors occur.

```javascript
test('test_resource_cleanup_error_path', async () => {
    // Arrange: Reset counter
    dropCount = 0;

    // Act: Create resource, then error
    try {
        using resource = new TestResource(2);
        throw new Error('error'); // Error path
        // Resource should still dispose
    } catch (error) {
        expect(error.message).toBe('error');
    }

    // Assert: Verify cleanup happened
    expect(dropCount).toBe(1);
});
```

#### Step 3.4: Test Panic Safety

**Action**: Verify resources are cleaned up even on panic.

```javascript
test('test_resource_cleanup_exception_safety', async () => {
    // Arrange: Reset counter
    dropCount = 0;

    // Act: Create resource, then throw exception
    try {
        using resource = new TestResource(3);
        throw new Error('test exception');
        // Resource should still dispose
    } catch (error) {
        expect(error.message).toBe('test exception');
    }

    // Assert: Verify cleanup happened
    expect(dropCount).toBe(1);
});
```

#### Step 3.5: Verify Coverage

**Checklist**:

- [ ] Normal cleanup tested
- [ ] Error path cleanup tested
- [ ] Exception safety tested
- [ ] Double-dispose safety verified (if applicable)

---

### Pattern 4: Concurrency Testing

#### Step 4.1: Identify Concurrency Scenarios

**Action**: List concurrent access patterns.

**Common scenarios**:

- Multiple async operations accessing shared state
- Race conditions
- Deadlocks
- Thread safety

#### Step 4.2: Test Concurrent Access

**Action**: Create concurrent test.

```javascript
import { expect, test } from 'vitest';

test('test_concurrent_access', async () => {
    // Arrange: Shared state
    let counter = 0;
    const mutex = { locked: false };
    const lock = () => {
        while (mutex.locked) {
            // Wait for lock
        }
        mutex.locked = true;
    };
    const unlock = () => {
        mutex.locked = false;
    };

    // Act: Spawn multiple async operations
    const promises = [];
    for (let i = 0; i < 10; i++) {
        promises.push(
            (async () => {
                for (let j = 0; j < 100; j++) {
                    lock();
                    counter++;
                    unlock();
                }
            })()
        );
    }

    // Wait for all operations
    await Promise.all(promises);

    // Assert: Verify final state
    expect(counter).toBe(1000);
});
```

#### Step 4.3: Test Thread Safety

**Action**: Verify concurrency safety.

```javascript
test('test_thread_safety', () => {
    // Arrange: Shared state with proper synchronization
    const sharedState = { value: 0 };
    const lock = new Mutex();

    // Assert: Verify thread-safe access
    async function incrementSafely() {
        await lock.acquire();
        try {
            sharedState.value++;
        } finally {
            lock.release();
        }
    }

    expect(typeof incrementSafely).toBe('function');
});
```

#### Step 4.4: Verify Coverage

**Checklist**:

- [ ] Concurrent access tested
- [ ] Race conditions tested
- [ ] Thread safety verified
- [ ] Deadlock prevention tested (if applicable)

---

### Pattern 5: Property-Based Testing

#### Step 5.1: Identify Properties

**Action**: List invariants that should always hold.

**Common properties**:

- Reversibility: `reverse(reverse(x)) == x`
- Idempotency: `f(f(x)) == f(x)`
- Commutativity: `f(a, b) == f(b, a)`
- Associativity: `f(f(a, b), c) == f(a, f(b, c))`

#### Step 5.2: Implement Property Test

**Action**: Create property test function.

```javascript
import { expect, test } from 'vitest';

function propertyAllReversesCorrectly(generator, iterations) {
    for (let i = 0; i < iterations; i++) {
        const data = generator.generateTestData();
        // Test property: reverse(reverse(x)) == x
        const reversedOnce = new Map();
        for (const [k, v] of data.entries()) {
            reversedOnce.set(v, k);
        }
        const reversedTwice = new Map();
        for (const [k, v] of reversedOnce.entries()) {
            reversedTwice.set(v, k);
        }

        // Compare maps
        if (data.size !== reversedTwice.size) {
            return false; // Property violated
        }
        for (const [k, v] of data.entries()) {
            if (reversedTwice.get(k) !== v) {
                return false; // Property violated
            }
        }
    }
    return true; // Property holds for all tested inputs
}

test('test_reverse_property', () => {
    // Arrange: Create generator
    const generator = new PropertyTestGenerator(42);

    // Act & Assert: Test property
    expect(
        propertyAllReversesCorrectly(generator, 1000)
    ).toBe(true);
});
```

#### Step 5.3: Verify Coverage

**Checklist**:

- [ ] Property function implemented
- [ ] Generator configured with seed
- [ ] Sufficient iterations (100+)
- [ ] Property violation detection works

**Reference**: See [User Guide - Property-Based Testing](../../docs/USER_GUIDE.md#property-based-testing)

---

### Pattern 6: Integration Testing with Real Collaborators

#### Step 6.1: Identify Dependencies

**Action**: List external dependencies.

**Common dependencies**:

- Databases
- APIs
- File systems
- Network services

#### Step 6.2: Set Up Real Dependencies

**Action**: Use testcontainers or real test services.

```javascript
import { expect, test } from 'vitest';
import { GenericContainer } from 'testcontainers';

test('test_integration_real_database', async () => {
    // Arrange: Use real test database (not mock)
    const container = await new GenericContainer('postgres', 'latest')
        .withExposedPorts(5432)
        .start();

    const port = container.getMappedPort(5432);

    // Act: Execute real operations
    const db = await connectToDatabase(port);
    const user = await createUser(db, 'test_user');
    const retrieved = await getUser(db, user.id);

    // Assert: Verify real state changes
    expect(retrieved.id).toBe(user.id);
    expect(retrieved.name).toBe('test_user');

    // Verify: Database actually persisted data
    const count = await countUsers(db);
    expect(count).toBe(1);

    // Cleanup: Automatic via container stop
    await container.stop();
});
```

#### Step 6.3: Verify Coverage

**Checklist**:

- [ ] Real dependencies used (not mocks)
- [ ] State changes verified
- [ ] Cleanup verified
- [ ] Integration points tested

**Reference**: See [User Guide - Testcontainers Integration](../../docs/USER_GUIDE.md#testcontainers-integration)

---

## Complete Expert Test Suite Template

### Step 7: Create Complete Test Suite

**Action**: Combine all patterns into comprehensive test suite.

```javascript
import { expect, test, describe } from 'vitest';

describe('expert_tests', () => {
    // 1. Error path testing
    test('test_all_error_variants', () => {
        // Implement error path tests
    });

    // 2. Boundary conditions
    test('test_boundary_conditions', () => {
        // Implement boundary tests
    });

    // 3. Resource cleanup
    test('test_resource_cleanup_all_paths', () => {
        // Implement cleanup tests
    });

    // 4. Concurrency
    test('test_concurrent_access', () => {
        // Implement concurrency tests
    });

    // 5. Property-based
    test('test_property_invariants', () => {
        // Implement property tests
    });

    // 6. Regression
    test('test_regression_bug_123', () => {
        // Implement regression tests
    });

    // 7. Integration
    test('test_integration_real_deps', () => {
        // Implement integration tests
    });

    // 8. Exception safety
    test('test_exception_safety', () => {
        // Implement exception safety tests
    });
});
```

### Step 8: Verify Expert Coverage

**Checklist**: Before marking tests complete, verify expert-level coverage:

- [ ] **Error paths**: All error variants tested (not just happy path)
- [ ] **Boundary conditions**: Empty, single, max, zero, negative tested
- [ ] **Resource cleanup**: Cleanup tested in error paths and exception paths
- [ ] **Concurrency**: Concurrent access patterns tested (if applicable)
- [ ] **Memory safety**: No leaks, proper resource disposal (use memory profilers)
- [ ] **Property-based**: Key invariants tested with random inputs
- [ ] **Regression**: Previously fixed bugs have regression tests
- [ ] **Integration**: Real dependencies tested, not just mocks
- [ ] **Exception safety**: Exceptions don't corrupt state
- [ ] **Thread safety**: Concurrent access patterns verified
- [ ] **Dispose behavior**: Resource disposal implementations tested
- [ ] **Reference safety**: References don't outlive data

---

## Common Junior Mistakes to Avoid

See [User Guide - Anti-patterns](../../docs/USER_GUIDE.md#anti-patterns) for complete list.

1. ❌ **Only testing happy path** - Most bugs are in error paths (80% of bugs)
2. ❌ **Not testing boundary conditions** - Edge cases cause production bugs
3. ❌ **Not testing resource cleanup** - Leaks accumulate over time
4. ❌ **Not testing concurrency** - Race conditions are hard to reproduce
5. ❌ **Not testing with real dependencies** - Mocks hide integration issues
6. ❌ **Not testing exception safety** - Exceptions can corrupt state
7. ❌ **Not using property-based testing** - Fixed inputs miss edge cases
8. ❌ **Not testing regressions** - Bugs come back without regression tests

---

## Tools for Expert Testing

- **Memory profilers**: Node.js memory profiling tools for leak detection
- **Concurrency testing**: Vitest with async/await patterns
- **Property-based**: Property testing libraries for JavaScript (see [User Guide](../../docs/USER_GUIDE.md#property-based-testing))
- **Fuzzing**: Random input testing with property-based testing
- **Sanitizers**: AddressSanitizer, ThreadSanitizer (if using native modules)
- **Testcontainers**: Real Docker containers (see [User Guide](../../docs/USER_GUIDE.md#testcontainers-integration))

---

## Summary

Expert-level testing focuses on the **80/20 rule**: Test the 20% of cases that cause 80% of bugs:

- Error paths (not just happy path)
- Boundary conditions (not just normal values)
- Resource cleanup (not just normal execution)
- Concurrency (not just single-threaded)
- Real dependencies (not just mocks)

**Remember**: "Never trust the text, only trust test results" - especially for error paths and edge cases.

## Documentation

- **[Getting Started Guide](../../docs/getting-started.md)** - Quick start with verified examples
- **[User Guide](../../docs/USER_GUIDE.md)** - Complete testing guide with patterns
- **[API Reference](../../docs/api/)** - Complete API documentation
- **[Architecture](../../docs/ARCHITECTURE.md)** - Design principles

End Command ---
