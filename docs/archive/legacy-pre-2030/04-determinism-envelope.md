# Pattern 4: Determinism Envelope

**Version**: v6.0.0
**Maturity Target**: L5 (Stable contracts, deterministic, adversarial-safe, composable)
**Copy-Exact Template**: Yes - Use AS-IS for all P0+P1 packages

---

## Overview

**Problem**: Non-deterministic code produces different results on repeated execution, breaking receipt verification and reproducibility.

**Solution**: Determinism Envelope = Wrapper that injects ALL external dependencies (time, randomness, UUIDs, env vars).

**Invariant**: Same inputs → Same outputs → Same receipts.

**Test**: Can run twice with identical inputs and get identical receipts (hash-to-hash match).

---

## Core Principle

**Banned**: All sources of non-determinism
- `Date.now()` - System clock (changes every call)
- `Math.random()` - Random numbers (changes every call)
- `crypto.randomUUID()` - Random UUIDs (changes every call)
- `process.env.*` - Environment variables (external state)
- Network calls without content hashing
- File system reads without content hashing
- Database reads without deterministic ordering

**Allowed**: All deterministic operations
- Pure functions (e.g., `(x, y) => x + y`)
- Injected dependencies (provided via parameters)
- Deterministic serialization (sorted keys)
- Content-based hashing (BLAKE3)

---

## Copy-Exact Templates

### 1. Time Provider (Injected Time)

```javascript
/**
 * Time Provider - Deterministic time source
 *
 * NEVER use Date.now() directly. ALWAYS inject time provider.
 */

/**
 * @typedef {Object} TimeProvider
 * @property {() => bigint} now - Get current nanosecond timestamp
 * @property {(ns: bigint) => string} toISO - Convert to ISO 8601 string
 */

/**
 * Create Real Time Provider - Uses system clock
 *
 * @returns {TimeProvider}
 */
export function createRealTimeProvider() {
  return {
    now() {
      // Use process.hrtime.bigint() for nanosecond precision
      return process.hrtime.bigint();
    },
    toISO(ns) {
      const ms = Number(ns / 1_000_000n);
      return new Date(ms).toISOString();
    },
  };
}

/**
 * Create Fake Time Provider - For testing (deterministic)
 *
 * @param {bigint} [initialTime] - Starting timestamp
 * @returns {TimeProvider}
 */
export function createFakeTimeProvider(initialTime = 1_000_000_000_000_000_000n) {
  let currentTime = initialTime;

  return {
    now() {
      return currentTime;
    },
    toISO(ns) {
      const ms = Number(ns / 1_000_000n);
      return new Date(ms).toISOString();
    },
    // Test helper: advance time
    advance(ns) {
      currentTime += ns;
    },
  };
}

/**
 * Example Usage
 */
// ❌ WRONG - Non-deterministic
function operation() {
  return { timestamp: Date.now() };
}

// ✅ CORRECT - Deterministic with injection
function operation(timeProvider) {
  return { timestamp: timeProvider.now() };
}

// Production
const realTime = createRealTimeProvider();
const result = operation(realTime);

// Testing (deterministic)
const fakeTime = createFakeTimeProvider();
const testResult1 = operation(fakeTime);
const testResult2 = operation(fakeTime);
console.assert(testResult1.timestamp === testResult2.timestamp); // ✅ Same
```

### 2. Random Provider (Injected Randomness)

```javascript
/**
 * Random Provider - Deterministic random source
 *
 * NEVER use Math.random() directly. ALWAYS inject random provider.
 */

/**
 * @typedef {Object} RandomProvider
 * @property {() => number} random - Get random number [0, 1)
 * @property {() => string} uuid - Get random UUID
 * @property {(min: number, max: number) => number} int - Get random integer
 */

/**
 * Create Real Random Provider - Uses crypto
 *
 * @returns {RandomProvider}
 */
export function createRealRandomProvider() {
  return {
    random() {
      return Math.random(); // OK in real provider
    },
    uuid() {
      return crypto.randomUUID();
    },
    int(min, max) {
      return Math.floor(Math.random() * (max - min + 1)) + min;
    },
  };
}

/**
 * Create Seeded Random Provider - For testing (deterministic)
 *
 * Uses seeded PRNG (pseudorandom number generator).
 *
 * @param {number} seed - Random seed
 * @returns {RandomProvider}
 */
export function createSeededRandomProvider(seed) {
  let state = seed;

  // Linear congruential generator (LCG)
  function next() {
    state = (state * 1103515245 + 12345) & 0x7fffffff;
    return state / 0x7fffffff;
  }

  return {
    random() {
      return next();
    },
    uuid() {
      // Generate deterministic UUID from seeded random
      const hex = '0123456789abcdef';
      let uuid = '';
      for (let i = 0; i < 36; i++) {
        if (i === 8 || i === 13 || i === 18 || i === 23) {
          uuid += '-';
        } else if (i === 14) {
          uuid += '4'; // Version 4
        } else if (i === 19) {
          uuid += hex[(Math.floor(next() * 4) + 8)]; // Variant bits
        } else {
          uuid += hex[Math.floor(next() * 16)];
        }
      }
      return uuid;
    },
    int(min, max) {
      return Math.floor(next() * (max - min + 1)) + min;
    },
  };
}

/**
 * Example Usage
 */
// ❌ WRONG - Non-deterministic
function operation() {
  return { random: Math.random(), id: crypto.randomUUID() };
}

// ✅ CORRECT - Deterministic with injection
function operation(randomProvider) {
  return { random: randomProvider.random(), id: randomProvider.uuid() };
}

// Production
const realRandom = createRealRandomProvider();
const result = operation(realRandom);

// Testing (deterministic)
const seededRandom = createSeededRandomProvider(42);
const testResult1 = operation(seededRandom);

const seededRandom2 = createSeededRandomProvider(42); // Same seed
const testResult2 = operation(seededRandom2);

console.assert(testResult1.random === testResult2.random); // ✅ Same
console.assert(testResult1.id === testResult2.id); // ✅ Same UUID
```

### 3. Environment Provider (Injected Config)

```javascript
/**
 * Environment Provider - Deterministic config source
 *
 * NEVER use process.env directly. ALWAYS inject environment provider.
 */

/**
 * @typedef {Object} EnvironmentProvider
 * @property {(key: string, defaultValue?: string) => string} get - Get environment variable
 * @property {(key: string) => boolean} has - Check if variable exists
 */

/**
 * Create Real Environment Provider - Uses process.env
 *
 * @returns {EnvironmentProvider}
 */
export function createRealEnvironmentProvider() {
  return {
    get(key, defaultValue = '') {
      return process.env[key] || defaultValue;
    },
    has(key) {
      return key in process.env;
    },
  };
}

/**
 * Create Fake Environment Provider - For testing (deterministic)
 *
 * @param {Record<string, string>} env - Environment variables
 * @returns {EnvironmentProvider}
 */
export function createFakeEnvironmentProvider(env = {}) {
  return {
    get(key, defaultValue = '') {
      return env[key] || defaultValue;
    },
    has(key) {
      return key in env;
    },
  };
}

/**
 * Example Usage
 */
// ❌ WRONG - Non-deterministic
function operation() {
  return { apiKey: process.env.API_KEY };
}

// ✅ CORRECT - Deterministic with injection
function operation(envProvider) {
  return { apiKey: envProvider.get('API_KEY', 'default-key') };
}

// Production
const realEnv = createRealEnvironmentProvider();
const result = operation(realEnv);

// Testing (deterministic)
const fakeEnv = createFakeEnvironmentProvider({ API_KEY: 'test-key' });
const testResult = operation(fakeEnv);
console.assert(testResult.apiKey === 'test-key'); // ✅ Deterministic
```

### 4. Determinism Context (All-in-One)

```javascript
/**
 * Determinism Context - Injects ALL dependencies
 *
 * @typedef {Object} DeterminismContext
 * @property {TimeProvider} time - Time provider
 * @property {RandomProvider} random - Random provider
 * @property {EnvironmentProvider} env - Environment provider
 */

/**
 * Create Real Context - For production
 *
 * @returns {DeterminismContext}
 */
export function createRealContext() {
  return {
    time: createRealTimeProvider(),
    random: createRealRandomProvider(),
    env: createRealEnvironmentProvider(),
  };
}

/**
 * Create Fake Context - For testing (deterministic)
 *
 * @param {Object} [options]
 * @param {bigint} [options.initialTime]
 * @param {number} [options.randomSeed]
 * @param {Record<string, string>} [options.env]
 * @returns {DeterminismContext}
 */
export function createFakeContext(options = {}) {
  const {
    initialTime = 1_000_000_000_000_000_000n,
    randomSeed = 42,
    env = {},
  } = options;

  return {
    time: createFakeTimeProvider(initialTime),
    random: createSeededRandomProvider(randomSeed),
    env: createFakeEnvironmentProvider(env),
  };
}

/**
 * Example Usage - Public Function with Context
 */
/**
 * Create User - With determinism context
 *
 * @param {Object} input
 * @param {string} input.name
 * @param {number} input.age
 * @param {DeterminismContext} context - Determinism context
 * @returns {User}
 */
export function createUser(input, context) {
  return {
    id: context.random.uuid(),
    name: input.name,
    age: input.age,
    role: 'user',
    createdAt: context.time.now(),
    apiKey: context.env.get('DEFAULT_API_KEY', 'none'),
  };
}

// Production
const realContext = createRealContext();
const user1 = createUser({ name: 'Alice', age: 30 }, realContext);

// Testing (deterministic)
const fakeContext = createFakeContext({ randomSeed: 42, initialTime: 1000n });
const testUser1 = createUser({ name: 'Alice', age: 30 }, fakeContext);

const fakeContext2 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
const testUser2 = createUser({ name: 'Alice', age: 30 }, fakeContext2);

console.assert(testUser1.id === testUser2.id); // ✅ Same UUID (deterministic)
console.assert(testUser1.createdAt === testUser2.createdAt); // ✅ Same timestamp
```

---

## Deterministic Serialization

### 1. Key Sorting (Deterministic JSON)

```javascript
/**
 * Deterministic Serialize - Serialize with sorted keys
 *
 * Ensures same object → same JSON string (always).
 *
 * @param {any} obj - Object to serialize
 * @returns {string} Deterministic JSON string
 */
export function deterministicSerialize(obj) {
  if (obj === null || obj === undefined) {
    return JSON.stringify(null);
  }

  if (typeof obj === 'bigint') {
    return obj.toString();
  }

  if (typeof obj !== 'object') {
    return JSON.stringify(obj);
  }

  if (Array.isArray(obj)) {
    const items = obj.map((item) => deterministicSerialize(item));
    return `[${items.join(',')}]`;
  }

  // Sort keys alphabetically for deterministic ordering
  const sortedKeys = Object.keys(obj).sort();
  const pairs = sortedKeys.map((key) => {
    const value = obj[key];
    const serializedValue = deterministicSerialize(value);
    return `${JSON.stringify(key)}:${serializedValue}`;
  });

  return `{${pairs.join(',')}}`;
}

/**
 * Test Determinism
 */
const obj1 = { b: 2, a: 1, c: 3 };
const obj2 = { c: 3, a: 1, b: 2 };

console.assert(
  deterministicSerialize(obj1) === deterministicSerialize(obj2)
); // ✅ Same (keys sorted)
```

### 2. Content Hashing (BLAKE3)

```javascript
import { blake3 } from 'hash-wasm';

/**
 * Compute BLAKE3 Hash - Deterministic hash
 *
 * @param {any} data - Data to hash
 * @returns {Promise<string>} 64-character hex hash
 */
export async function computeBlake3(data) {
  const serialized = typeof data === 'string' ? data : deterministicSerialize(data);
  return blake3(serialized);
}

/**
 * Test Hash Determinism
 */
const data1 = { b: 2, a: 1 };
const data2 = { a: 1, b: 2 };

const hash1 = await computeBlake3(data1);
const hash2 = await computeBlake3(data2);

console.assert(hash1 === hash2); // ✅ Same hash (deterministic)
```

---

## External Dependencies (Network, DB, FS)

### 1. Network Calls (Content Hashing)

```javascript
/**
 * Deterministic Fetch - Hash content, not response metadata
 *
 * @param {string} url
 * @param {DeterminismContext} context
 * @returns {Promise<{data: any, contentHash: string}>}
 */
export async function deterministicFetch(url, context) {
  const response = await fetch(url);
  const data = await response.json();

  // Hash ONLY the content (not status, headers, timestamps)
  const contentHash = await computeBlake3(data);

  return {
    data,
    contentHash, // Deterministic proof of content
  };
}
```

### 2. Database Reads (Deterministic Ordering)

```javascript
/**
 * Deterministic Query - Always order results
 *
 * @param {Object} db
 * @param {string} sql
 * @param {Array} params
 * @returns {Promise<Array>}
 */
export async function deterministicQuery(db, sql, params) {
  // ALWAYS add ORDER BY for determinism
  const orderedSQL = sql.includes('ORDER BY')
    ? sql
    : `${sql} ORDER BY id ASC`;

  return await db.query(orderedSQL, params);
}
```

### 3. File Reads (Content Hashing)

```javascript
import { readFile } from 'fs/promises';

/**
 * Deterministic Read File - Hash content
 *
 * @param {string} path
 * @returns {Promise<{content: string, contentHash: string}>}
 */
export async function deterministicReadFile(path) {
  const content = await readFile(path, 'utf-8');
  const contentHash = await computeBlake3(content);

  return {
    content,
    contentHash,
  };
}
```

---

## Testing Determinism

### 1. Determinism Test Template

```javascript
import { describe, it, expect } from 'vitest';

describe('Determinism Envelope', () => {
  it('produces same output with same context', () => {
    const context1 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
    const result1 = operation(input, context1);

    const context2 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
    const result2 = operation(input, context2);

    expect(result1).toEqual(result2); // ✅ Same output
  });

  it('produces same hash with same context', async () => {
    const context1 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
    const {receipt: r1} = await withReceipt(operation, context1, input);

    const context2 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
    const {receipt: r2} = await withReceipt(operation, context2, input);

    expect(r1.receiptHash).toBe(r2.receiptHash); // ✅ Same receipt hash
  });

  it('is NOT deterministic without context injection', () => {
    // This test SHOULD fail (proves non-determinism without injection)
    const result1 = badOperation(); // Uses Date.now(), Math.random()
    const result2 = badOperation();

    expect(result1).not.toEqual(result2); // ❌ Different (non-deterministic)
  });
});
```

---

## Common Pitfalls

### Pitfall 1: Hidden Non-Determinism

```javascript
// ❌ WRONG - Hidden Date.now() in nested function
function operation(context) {
  const user = createUser({ name: 'Alice' });
  // createUser() uses Date.now() internally → non-deterministic!
  return user;
}

// ✅ CORRECT - Pass context down
function operation(context) {
  const user = createUser({ name: 'Alice' }, context);
  return user;
}
```

### Pitfall 2: Object Key Ordering

```javascript
// ❌ WRONG - Assumes key order
function hash(obj) {
  return JSON.stringify(obj); // Key order not guaranteed!
}

// ✅ CORRECT - Sort keys
function hash(obj) {
  return deterministicSerialize(obj); // Keys sorted
}
```

### Pitfall 3: Floating Point Non-Determinism

```javascript
// ❌ WRONG - Floating point operations may differ across platforms
function operation() {
  return 0.1 + 0.2; // May not equal 0.3 exactly
}

// ✅ CORRECT - Use integers or fixed precision
function operation() {
  return Math.floor((0.1 + 0.2) * 1000) / 1000; // Fixed precision
}
```

### Pitfall 4: Async Race Conditions

```javascript
// ❌ WRONG - Parallel async operations (non-deterministic order)
async function operation() {
  const [a, b] = await Promise.all([fetchA(), fetchB()]);
  return { a, b }; // Order of completion is non-deterministic
}

// ✅ CORRECT - Sequential async operations
async function operation() {
  const a = await fetchA();
  const b = await fetchB();
  return { a, b }; // Deterministic order
}
```

---

## Enforcement (ESLint Rules Sketch)

```javascript
// .eslintrc.cjs
module.exports = {
  rules: {
    'no-restricted-globals': ['error', {
      name: 'Date',
      message: 'Use injected TimeProvider instead of Date.now()',
    }],
    'no-restricted-syntax': ['error', {
      selector: 'CallExpression[callee.object.name="Math"][callee.property.name="random"]',
      message: 'Use injected RandomProvider instead of Math.random()',
    }, {
      selector: 'CallExpression[callee.object.name="crypto"][callee.property.name="randomUUID"]',
      message: 'Use injected RandomProvider.uuid() instead of crypto.randomUUID()',
    }, {
      selector: 'MemberExpression[object.object.name="process"][object.property.name="env"]',
      message: 'Use injected EnvironmentProvider instead of process.env',
    }],
  },
};
```

---

## Decision Matrix: When to Inject

| Dependency | Inject? | Provider | Rationale |
|------------|---------|----------|-----------|
| Time (Date.now) | ✅ YES | TimeProvider | Non-deterministic clock |
| Random (Math.random) | ✅ YES | RandomProvider | Non-deterministic RNG |
| UUID (crypto.randomUUID) | ✅ YES | RandomProvider | Non-deterministic UUID |
| Environment (process.env) | ✅ YES | EnvironmentProvider | External state |
| Network (fetch) | ⚠️ HASH | Hash response content | Response varies |
| Database (query) | ⚠️ ORDER | Order results | Row order varies |
| File System (readFile) | ⚠️ HASH | Hash file content | Content may change |
| Pure function (x + y) | ❌ NO | None | Already deterministic |

---

## Copy-Paste Checklist

- [ ] Copy `determinism-context.mjs` with providers
- [ ] Copy `deterministic-serialize.mjs` for key sorting
- [ ] Add `createFakeContext` for testing
- [ ] Replace ALL `Date.now()` with `context.time.now()`
- [ ] Replace ALL `Math.random()` with `context.random.random()`
- [ ] Replace ALL `crypto.randomUUID()` with `context.random.uuid()`
- [ ] Replace ALL `process.env.*` with `context.env.get('*')`
- [ ] Add determinism tests (same context → same output)
- [ ] Enable ESLint rules to ban non-deterministic operations

---

## References

- Existing Implementations:
  - `/home/user/unrdf/packages/yawl/src/receipt-core.mjs` (deterministicSerialize)
  - `/home/user/unrdf/packages/v6-core/src/receipts/base-receipt.mjs` (computeBlake3)
- Time Provider: `@unrdf/kgc-4d` (`now()`, `toISO()`)
- Hashing: `hash-wasm` (BLAKE3)
