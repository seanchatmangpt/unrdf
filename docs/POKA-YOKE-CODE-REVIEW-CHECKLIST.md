# Poka-Yoke Code Review Checklist

## Purpose

This checklist ensures code reviews verify poka-yoke (error prevention) patterns are used correctly. Poka-yoke prevents errors at compile time through type safety and invariants.

## Checklist Items

### Input Validation

- [ ] All function inputs are validated (non-null, non-empty, correct type)
- [ ] Validation errors throw descriptive errors (not silent failures)
- [ ] Invalid inputs are rejected before processing (fail fast)

### Type Guards

- [ ] Type guards check for null/undefined before operations
- [ ] Type guards verify object structure before property access
- [ ] Type guards ensure arrays/collections exist before iteration

### State Machine

- [ ] State transitions are explicit and validated
- [ ] Invalid state transitions are prevented (compile-time if possible)
- [ ] State is checked before operations (e.g., "ready" before "execute")

### Defensive Programming

- [ ] Defensive checks prevent data loss (e.g., create collector if missing)
- [ ] Error handling doesn't silently swallow errors
- [ ] Edge cases are handled explicitly (not assumed to not occur)

### Type Safety

- [ ] JSDoc types are accurate and complete
- [ ] Type invariants are documented in comments
- [ ] Invalid states are unrepresentable in the type system

## Review Process

1. Review code for poka-yoke patterns
2. Check each checklist item
3. Flag missing poka-yoke patterns
4. Request fixes before approval

## Examples

### ✅ Good: Input Validation

```javascript
function process(value) {
  if (typeof value !== 'string' || value.length === 0) {
    throw new Error('value must be a non-empty string');
  }
  // Process value - guaranteed to be valid
}
```

### ❌ Bad: No Validation

```javascript
function process(value) {
  // value could be null/undefined/empty - runtime error!
  return value.toUpperCase();
}
```

### ✅ Good: Type Guard

```javascript
function addSpan(validationId, spanData) {
  if (!this.spanCollector.has(validationId)) {
    this.spanCollector.set(validationId, []);
  }
  // Collector guaranteed to exist
  this.spanCollector.get(validationId).push(spanData);
}
```

### ❌ Bad: No Type Guard

```javascript
function addSpan(validationId, spanData) {
  // spanCollector.get(validationId) could be undefined - runtime error!
  this.spanCollector.get(validationId).push(spanData);
}
```

