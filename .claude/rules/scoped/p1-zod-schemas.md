# Scoped Rule: Zod Schema Conventions

**Scope**: All Zod schema definitions and validation code

## Zod v4 Migration

### Removed Methods

Do NOT use `.args()` or `.returns()` (removed in Zod v4):

```javascript
// Wrong - args() removed in v4
const schema = z.function().args(z.string()).returns(z.number());

// Correct - bare z.function()
const schema = z.function();
```

### Validate-Only Pattern

Zod `parse()` returns plain objects (prototype methods lost). Use for validation only:

```javascript
// Correct - validate then use original
const isValid = schema.safeParse(input);
if (isValid.success) {
  // Use input directly, not isValid.data
  processInput(input);
}

// Wrong - expecting prototype methods on parsed data
const parsed = schema.parse(input);
parsed.someMethod(); // Error: undefined
```

## Schema Definitions

### Type Mapping Filters

In templates, use `zodType` filter for XSD-to-Zod mapping:

```nunjucks
{{ property.xsdType|zodType }}
{# Renders: z.string(), z.number().int(), z.boolean() #}
```

### Export Patterns

```javascript
// Correct - named export
export const entitySchema = z.object({
  name: z.string(),
  count: z.number().int()
});

// Correct - default export for single schema
export default z.object({ /* ... */ });
```

## Error Handling

```javascript
// Correct - check success before accessing
const result = schema.safeParse(input);
if (!result.success) {
  console.error(result.error.errors);
  return;
}
// Use result.data

// Wrong - accessing .data without check
const result = schema.safeParse(input);
process(result.data); // May be undefined
```

## Files Using Zod

- `packages/*/src/**/*.schema.mjs` - Schema definitions
- `packages/cli/src/lib/frontmatter-parser.mjs` - Config validation
- Any validation layer with `.parse()` or `.safeParse()`
