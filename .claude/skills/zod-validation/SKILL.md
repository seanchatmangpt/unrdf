# Zod Validation Skill

Expert knowledge for runtime validation with Zod in UNRDF.

## Activation

Use this skill when:
- Adding input validation to functions
- Creating API schemas
- Validating configuration
- Building type-safe data pipelines

## Core Patterns

### Basic Schema Definition
```javascript
import { z } from 'zod';

// Primitive types
const StringSchema = z.string();
const NumberSchema = z.number();
const BooleanSchema = z.boolean();

// Object schema
const UserSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(100),
  email: z.string().email(),
  age: z.number().int().positive().optional(),
  role: z.enum(['admin', 'user', 'guest']),
  createdAt: z.date()
});

// Type inference
/** @typedef {z.infer<typeof UserSchema>} User */
```

### Safe vs Strict Parsing
```javascript
// STRICT - throws on invalid (use for trusted data)
const user = UserSchema.parse(data);

// SAFE - returns result object (use for user input)
const result = UserSchema.safeParse(data);
if (result.success) {
  const user = result.data;
} else {
  console.error(result.error.issues);
}
```

### Custom Validation
```javascript
const PathSchema = z.string().refine(
  path => !path.includes('..'),
  { message: 'Path traversal not allowed' }
);

const PositiveEvenSchema = z.number()
  .positive()
  .refine(n => n % 2 === 0, 'Must be even');
```

### Transform Data
```javascript
const TrimmedStringSchema = z.string().transform(s => s.trim());

const DateStringSchema = z.string().transform(s => new Date(s));

const ParsedNumberSchema = z.string().transform(Number).pipe(z.number());
```

## UNRDF-Specific Schemas

### Triple Schema
```javascript
const TripleSchema = z.object({
  subject: z.string().url(),
  predicate: z.string().url(),
  object: z.union([
    z.string().url(),  // URI
    z.object({         // Literal
      value: z.string(),
      datatype: z.string().url().optional(),
      language: z.string().length(2).optional()
    })
  ])
});
```

### Receipt Schema
```javascript
const ReceiptSchema = z.object({
  id: z.string().uuid(),
  operation: z.enum(['create', 'update', 'delete', 'query']),
  entityType: z.string(),
  timestamp: z.number().int().positive(),
  agent: z.string(),
  data: z.record(z.unknown()).optional(),
  hash: z.string().optional()
});
```

### Delta Schema
```javascript
const DeltaSchema = z.object({
  id: z.string().uuid(),
  type: z.enum(['insert', 'delete', 'update']),
  payload: z.array(TripleSchema),
  timestamp: z.number(),
  source: z.string().optional()
});
```

## Error Handling

### Formatting Errors
```javascript
import { z } from 'zod';

const result = schema.safeParse(data);
if (!result.success) {
  const formatted = result.error.format();
  // { fieldName: { _errors: ['error message'] } }

  // Or flatten
  const flat = result.error.flatten();
  // { formErrors: [], fieldErrors: { field: ['msg'] } }
}
```

### Custom Error Messages
```javascript
const Schema = z.object({
  email: z.string({
    required_error: 'Email is required',
    invalid_type_error: 'Email must be a string'
  }).email({ message: 'Invalid email format' })
});
```

## Best Practices

1. **Schema Location**: Co-locate with implementation (`.schema.mjs`)
2. **Reuse Schemas**: Export and import common schemas
3. **Safe for User Input**: Always use `safeParse` for external data
4. **Type Inference**: Use `z.infer<>` for JSDoc types
5. **Validation Boundaries**: Validate at system edges only

## Common Mistakes

### Don't Over-Validate
```javascript
// BAD - validates internal data unnecessarily
function internalHelper(data) {
  const validated = Schema.parse(data); // Already validated upstream
}

// GOOD - validate at boundaries only
export function publicApi(input) {
  const validated = Schema.parse(input);
  return internalHelper(validated);
}
```

### Don't Ignore Errors
```javascript
// BAD
const result = schema.safeParse(data);
const value = result.data; // Could be undefined!

// GOOD
const result = schema.safeParse(data);
if (!result.success) {
  throw new ValidationError(result.error);
}
const value = result.data;
```
