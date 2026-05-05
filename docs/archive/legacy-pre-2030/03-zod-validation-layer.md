# Pattern 3: Zod Validation Layer

**Version**: v6.0.0
**Maturity Target**: L5 (Stable contracts, deterministic, adversarial-safe, composable)
**Copy-Exact Template**: Yes - Use AS-IS for all P0+P1 packages

---

## Overview

**Problem**: Runtime safety requires validation at boundaries. JSDoc alone doesn't prevent invalid data.

**Solution**: Zod schemas for ALL public functions (input + output validation) with runtime guarantees.

**Invariant**: No unvalidated data crosses module boundaries.

**Philosophy**: Parse, don't validate. Transform untrusted input → trusted types.

---

## Core Principles

### 1. Validation Depth Strategy

```
API Boundary (Public Functions):  ✅ ALWAYS validate input + output
Internal Functions (Private):      ⚠️  OPTIONAL (assumes pre-validated data)
Leaf Functions (Pure logic):       ❌ NO validation (performance)
```

**Rule**: Validate at the BOUNDARY, trust internally.

### 2. Parse, Don't Validate

```javascript
// ❌ WRONG - Validation returns boolean
function validateUser(data) {
  return typeof data.name === 'string' && typeof data.age === 'number';
}

// ✅ CORRECT - Parse returns typed object or throws
function parseUser(data) {
  return UserSchema.parse(data); // Throws if invalid, returns User if valid
}
```

### 3. Three-Layer Architecture

```
Layer 1: Zod Schema Definition (types + constraints)
Layer 2: Parse Functions (runtime validation)
Layer 3: JSDoc Type Annotations (IDE hints)
```

---

## Copy-Exact Templates

### 1. Basic Schema Template

```javascript
import { z } from 'zod';

/**
 * [Entity] Schema - Zod validation schema
 *
 * Defines runtime constraints for [Entity].
 *
 * @example
 * const user = UserSchema.parse({ name: 'Alice', age: 30 }); // ✅ Valid
 * const bad = UserSchema.parse({ name: 123 }); // ❌ Throws ZodError
 */
export const UserSchema = z.object({
  // Required fields
  id: z.string().uuid(),
  name: z.string().min(1).max(100),
  age: z.number().int().nonnegative().max(150),

  // Optional fields
  email: z.string().email().optional(),
  bio: z.string().max(500).optional(),

  // Enums
  role: z.enum(['admin', 'user', 'guest']),

  // Nested objects
  address: z.object({
    street: z.string(),
    city: z.string(),
    zip: z.string().regex(/^\d{5}$/),
  }).optional(),

  // Arrays
  tags: z.array(z.string()).default([]),

  // Timestamps (bigint for nanosecond precision)
  createdAt: z.bigint(),
  updatedAt: z.bigint().optional(),
});

/**
 * @typedef {z.infer<typeof UserSchema>} User
 */
```

### 2. Parse Function Template

```javascript
/**
 * Parse [Entity] - Validate and parse data
 *
 * @param {unknown} data - Untrusted data to parse
 * @returns {User} Validated user object
 * @throws {ZodError} If data is invalid
 *
 * @example
 * try {
 *   const user = parseUser(req.body);
 *   // user is now typed and validated
 * } catch (error) {
 *   console.error('Validation failed:', error.errors);
 * }
 */
export function parseUser(data) {
  return UserSchema.parse(data);
}

/**
 * Safe Parse [Entity] - Validate without throwing
 *
 * @param {unknown} data - Untrusted data to parse
 * @returns {{success: true, data: User} | {success: false, error: ZodError}}
 *
 * @example
 * const result = safeParseUser(req.body);
 * if (result.success) {
 *   console.log('Valid:', result.data);
 * } else {
 *   console.error('Invalid:', result.error.errors);
 * }
 */
export function safeParseUser(data) {
  return UserSchema.safeParse(data);
}
```

### 3. Public Function Template (Input + Output Validation)

```javascript
/**
 * Create User - Public API function with validation
 *
 * VALIDATION:
 * - Input: CreateUserInput schema
 * - Output: User schema
 *
 * @param {Object} input - User creation data
 * @param {string} input.name - User name
 * @param {number} input.age - User age
 * @param {string} [input.email] - User email
 * @returns {User} Created user
 * @throws {ZodError} If input or output is invalid
 *
 * @example
 * const user = createUser({ name: 'Alice', age: 30, email: 'alice@example.com' });
 * console.assert(user.id); // UUID generated
 */
export function createUser(input) {
  // 1. Validate input
  const validatedInput = CreateUserInputSchema.parse(input);

  // 2. Execute business logic
  const user = {
    id: crypto.randomUUID(),
    name: validatedInput.name,
    age: validatedInput.age,
    email: validatedInput.email,
    role: 'user',
    tags: [],
    createdAt: BigInt(Date.now()) * 1_000_000n,
  };

  // 3. Validate output
  const validatedUser = UserSchema.parse(user);

  return validatedUser;
}

/**
 * CreateUserInput Schema
 */
const CreateUserInputSchema = z.object({
  name: z.string().min(1).max(100),
  age: z.number().int().nonnegative().max(150),
  email: z.string().email().optional(),
});
```

### 4. Async Function Template

```javascript
/**
 * Fetch User - Async function with validation
 *
 * VALIDATION:
 * - Input: userId (string UUID)
 * - Output: User schema
 *
 * @param {string} userId - User UUID
 * @returns {Promise<User>} User object
 * @throws {ZodError} If validation fails
 * @throws {Error} If user not found
 *
 * @example
 * const user = await fetchUser('550e8400-e29b-41d4-a716-446655440000');
 */
export async function fetchUser(userId) {
  // 1. Validate input
  const validatedUserId = z.string().uuid().parse(userId);

  // 2. Execute async logic
  const rawUser = await db.query('SELECT * FROM users WHERE id = ?', [validatedUserId]);

  if (!rawUser) {
    throw new Error(`User not found: ${validatedUserId}`);
  }

  // 3. Validate output
  const validatedUser = UserSchema.parse(rawUser);

  return validatedUser;
}
```

---

## Advanced Patterns

### 1. Discriminated Unions (Type-Safe Enums)

```javascript
/**
 * Event Schema - Discriminated union by type
 */
export const EventSchema = z.discriminatedUnion('type', [
  z.object({
    type: z.literal('user_created'),
    userId: z.string().uuid(),
    timestamp: z.bigint(),
  }),
  z.object({
    type: z.literal('user_updated'),
    userId: z.string().uuid(),
    changes: z.record(z.string(), z.any()),
    timestamp: z.bigint(),
  }),
  z.object({
    type: z.literal('user_deleted'),
    userId: z.string().uuid(),
    timestamp: z.bigint(),
  }),
]);

/**
 * @typedef {z.infer<typeof EventSchema>} Event
 */

/**
 * Handle Event - Type-safe event handler
 *
 * @param {Event} event
 * @returns {void}
 */
export function handleEvent(event) {
  const validatedEvent = EventSchema.parse(event);

  switch (validatedEvent.type) {
    case 'user_created':
      console.log('Created:', validatedEvent.userId);
      break;
    case 'user_updated':
      console.log('Updated:', validatedEvent.userId, validatedEvent.changes);
      break;
    case 'user_deleted':
      console.log('Deleted:', validatedEvent.userId);
      break;
  }
}
```

### 2. Schema Composition (Reuse)

```javascript
/**
 * Base Entity Schema - Reusable base
 */
const BaseEntitySchema = z.object({
  id: z.string().uuid(),
  createdAt: z.bigint(),
  updatedAt: z.bigint().optional(),
});

/**
 * User Schema - Extends base
 */
export const UserSchema = BaseEntitySchema.extend({
  name: z.string().min(1).max(100),
  age: z.number().int().nonnegative(),
  role: z.enum(['admin', 'user', 'guest']),
});

/**
 * Post Schema - Extends base
 */
export const PostSchema = BaseEntitySchema.extend({
  title: z.string().min(1).max(200),
  content: z.string().max(5000),
  authorId: z.string().uuid(),
  published: z.boolean(),
});
```

### 3. Transform Schemas (Parsing + Transformation)

```javascript
/**
 * Timestamp Schema - Parse ISO string to bigint
 */
const TimestampSchema = z.string().datetime().transform((iso) => {
  return BigInt(new Date(iso).getTime()) * 1_000_000n;
});

/**
 * User Input Schema - With transformations
 */
const UserInputSchema = z.object({
  name: z.string().trim().min(1).max(100),
  age: z.string().transform((s) => parseInt(s, 10)).pipe(z.number().int().nonnegative()),
  createdAt: TimestampSchema,
});

/**
 * Parse User Input
 *
 * @example
 * const input = { name: '  Alice  ', age: '30', createdAt: '2024-01-01T00:00:00Z' };
 * const parsed = UserInputSchema.parse(input);
 * // { name: 'Alice', age: 30, createdAt: 1704067200000000000n }
 */
```

### 4. Recursive Schemas (Trees)

```javascript
/**
 * Tree Node Schema - Recursive structure
 */
const TreeNodeSchema = z.lazy(() =>
  z.object({
    value: z.string(),
    children: z.array(TreeNodeSchema).optional(),
  })
);

/**
 * @typedef {z.infer<typeof TreeNodeSchema>} TreeNode
 */

/**
 * @example
 * const tree = TreeNodeSchema.parse({
 *   value: 'root',
 *   children: [
 *     { value: 'child1' },
 *     { value: 'child2', children: [{ value: 'grandchild' }] }
 *   ]
 * });
 */
```

---

## Validation Depth Decision Matrix

| Function Type | Validate Input? | Validate Output? | Rationale |
|---------------|-----------------|------------------|-----------|
| Public API (exported) | ✅ YES | ✅ YES | Boundary protection |
| Internal (non-exported) | ⚠️ OPTIONAL | ⚠️ OPTIONAL | Pre-validated data assumed |
| Pure leaf function | ❌ NO | ❌ NO | Performance (no side effects) |
| Async (DB, network) | ✅ YES | ✅ YES | External data untrusted |
| Constructor/Factory | ✅ YES | ✅ YES | Object creation boundary |

**Rule of Thumb**: If a function is `export`ed, it MUST validate input + output.

---

## Common Pitfalls

### Pitfall 1: Over-Validation (Performance Cost)

```javascript
// ❌ WRONG - Validating in hot loop
function processItems(items) {
  return items.map(item => ItemSchema.parse(item)); // Validates EVERY item!
}

// ✅ CORRECT - Validate once at boundary
function processItems(items) {
  const validatedItems = z.array(ItemSchema).parse(items); // Single validation
  return validatedItems.map(item => transformItem(item)); // No validation
}
```

### Pitfall 2: Under-Validation (Security Risk)

```javascript
// ❌ WRONG - No validation at API boundary
export function updateUser(userId, data) {
  return db.update('users', userId, data); // data is untrusted!
}

// ✅ CORRECT - Validate at boundary
export function updateUser(userId, data) {
  const validatedUserId = z.string().uuid().parse(userId);
  const validatedData = UpdateUserSchema.parse(data);
  return db.update('users', validatedUserId, validatedData);
}
```

### Pitfall 3: Forgetting Output Validation

```javascript
// ❌ WRONG - Only validates input
export async function fetchUser(userId) {
  const validatedUserId = z.string().uuid().parse(userId);
  return await db.query('SELECT * FROM users WHERE id = ?', [validatedUserId]);
  // What if DB returns invalid data?
}

// ✅ CORRECT - Validates input + output
export async function fetchUser(userId) {
  const validatedUserId = z.string().uuid().parse(userId);
  const rawUser = await db.query('SELECT * FROM users WHERE id = ?', [validatedUserId]);
  return UserSchema.parse(rawUser); // Validates DB response
}
```

### Pitfall 4: Using `.optional()` Instead of `.nullable()`

```javascript
// ❌ WRONG - Optional doesn't handle null
const schema = z.object({
  name: z.string().optional(), // Allows undefined, NOT null
});

schema.parse({ name: null }); // ❌ Throws error

// ✅ CORRECT - Nullable handles null
const schema = z.object({
  name: z.string().nullable(), // Allows null
  // OR
  name: z.string().optional().nullable(), // Allows undefined OR null
});

schema.parse({ name: null }); // ✅ Valid
```

---

## Error Handling

### 1. Graceful Error Handling

```javascript
/**
 * Create User - With error handling
 *
 * @param {unknown} input
 * @returns {{success: true, user: User} | {success: false, errors: Array}}
 */
export function createUserSafe(input) {
  const result = CreateUserInputSchema.safeParse(input);

  if (!result.success) {
    return {
      success: false,
      errors: result.error.errors.map(err => ({
        path: err.path.join('.'),
        message: err.message,
      })),
    };
  }

  const user = {
    id: crypto.randomUUID(),
    ...result.data,
    role: 'user',
    tags: [],
    createdAt: BigInt(Date.now()) * 1_000_000n,
  };

  const validationResult = UserSchema.safeParse(user);

  if (!validationResult.success) {
    return {
      success: false,
      errors: validationResult.error.errors,
    };
  }

  return {
    success: true,
    user: validationResult.data,
  };
}
```

### 2. Custom Error Messages

```javascript
const UserSchema = z.object({
  name: z.string().min(1, 'Name is required').max(100, 'Name too long'),
  age: z.number({ required_error: 'Age is required' })
    .int('Age must be an integer')
    .nonnegative('Age must be positive')
    .max(150, 'Age must be ≤ 150'),
  email: z.string().email('Invalid email format').optional(),
});
```

---

## Testing Template

```javascript
import { describe, it, expect } from 'vitest';
import { UserSchema, parseUser } from './user-schema.mjs';

describe('Zod Validation Layer', () => {
  it('parses valid user', () => {
    const validUser = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      name: 'Alice',
      age: 30,
      role: 'user',
      tags: [],
      createdAt: 1234567890000000000n,
    };

    const parsed = parseUser(validUser);
    expect(parsed).toEqual(validUser);
  });

  it('rejects invalid user (missing name)', () => {
    const invalidUser = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      age: 30,
      role: 'user',
      tags: [],
      createdAt: 1234567890000000000n,
    };

    expect(() => parseUser(invalidUser)).toThrow();
  });

  it('rejects invalid user (invalid UUID)', () => {
    const invalidUser = {
      id: 'not-a-uuid',
      name: 'Alice',
      age: 30,
      role: 'user',
      tags: [],
      createdAt: 1234567890000000000n,
    };

    expect(() => parseUser(invalidUser)).toThrow();
  });

  it('safe parse returns error for invalid data', () => {
    const invalidUser = { name: 123 };
    const result = UserSchema.safeParse(invalidUser);

    expect(result.success).toBe(false);
    if (!result.success) {
      expect(result.error.errors).toHaveLength(1);
    }
  });
});
```

---

## Performance Optimization

### 1. Schema Caching (Precompile)

```javascript
// ❌ WRONG - Creates new schema every call
function validateUser(data) {
  const schema = z.object({ name: z.string() }); // Recreated!
  return schema.parse(data);
}

// ✅ CORRECT - Cached schema
const UserSchema = z.object({ name: z.string() }); // Created once

function validateUser(data) {
  return UserSchema.parse(data);
}
```

### 2. Partial Validation (When Needed)

```javascript
// Full schema
const UserSchema = z.object({
  id: z.string().uuid(),
  name: z.string(),
  age: z.number(),
  email: z.string().email(),
});

// Partial schema (for updates)
const UpdateUserSchema = UserSchema.partial(); // All fields optional

/**
 * Update User - Partial validation
 */
export function updateUser(userId, updates) {
  const validatedUserId = z.string().uuid().parse(userId);
  const validatedUpdates = UpdateUserSchema.parse(updates);
  // Only provided fields are validated
  return db.update('users', validatedUserId, validatedUpdates);
}
```

---

## JSDoc Integration

```javascript
/**
 * User type (inferred from Zod schema)
 * @typedef {z.infer<typeof UserSchema>} User
 */

/**
 * Create user function with JSDoc types
 *
 * @param {Object} input - User creation input
 * @param {string} input.name - User name
 * @param {number} input.age - User age
 * @returns {User} Created user
 */
export function createUser(input) {
  return UserSchema.parse({ ...input, id: crypto.randomUUID(), createdAt: now() });
}
```

---

## Copy-Paste Checklist

- [ ] Copy schema template for your domain entity
- [ ] Define `[Entity]Schema` with Zod
- [ ] Create `parse[Entity]` and `safeParse[Entity]` functions
- [ ] Add JSDoc `@typedef {z.infer<typeof Schema>}` for type hints
- [ ] Validate input + output for ALL public functions
- [ ] Write tests for valid + invalid cases
- [ ] Document validation errors in README

---

## References

- Zod Documentation: https://zod.dev
- Existing Schemas: `/home/user/unrdf/packages/yawl/src/receipt-core.mjs`
- Delta Schema: `/home/user/unrdf/packages/v6-core/src/delta/schema.mjs`
- Base Receipt: `/home/user/unrdf/packages/v6-core/src/receipts/base-receipt.mjs`
