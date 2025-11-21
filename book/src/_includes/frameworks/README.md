# Abstraction Frameworks

Production-ready frameworks for eliminating boilerplate and building scalable applications.

## Purpose

**80/20 Frameworks:**
- Eliminate repetitive code patterns
- Enforce best practices automatically
- Provide type-safe abstractions
- Improve developer experience

## Available Frameworks

### [React Hooks Abstraction](./react-hooks-abstraction.md)

Composable, type-safe React hooks architecture.

**Eliminates:**
- Loading state boilerplate
- Error handling patterns
- Caching logic
- Retry mechanisms
- Type validation

**Provides:**
- `createHook()` - Base hook factory with Zod validation
- `composeHooks()` - Combine multiple hooks
- `createKnowledgeHook()` - Type-safe Knowledge Hooks
- `createOptimisticHook()` - Optimistic updates with rollback
- `createCachedHook()` - Automatic caching
- Testing utilities

**Impact:** 70% less hook boilerplate, 100% type safety

## Design Philosophy

### 1. Composability
Build complex behavior from simple pieces:
```typescript
const useDashboard = composeHooks({
  hooks: { user: useUser, orders: useOrders },
  combiner: (results) => ({ ...results })
});
```

### 2. Type Safety
Zod schemas provide runtime + compile-time safety:
```typescript
const useProduct = createHook({
  inputSchema: z.object({ id: z.string() }),
  outputSchema: ProductSchema,
  execute: fetchProduct
});
```

### 3. Best Practices Built-in
No need to remember patterns:
- Automatic loading states
- Error boundaries
- Cleanup on unmount
- Retry logic
- Cache invalidation

### 4. Progressive Enhancement
Start simple, add complexity as needed:
```typescript
// Simple
const useSimple = createHook({ ... });

// With caching
const useCached = createCachedHook({ hook: useSimple });

// With debouncing
const useDebounced = createDebouncedHook({ hook: useCached });

// With optimistic updates
const useOptimistic = createOptimisticHook({ ... });
```

## Usage Patterns

### Pattern 1: Data Fetching
```typescript
const useUsers = createHook({
  name: 'useUsers',
  inputSchema: z.object({ page: z.number() }),
  outputSchema: z.array(UserSchema),
  execute: async ({ page }) => {
    const res = await fetch(`/api/users?page=${page}`);
    return res.json();
  }
});
```

### Pattern 2: Real-time Updates
```typescript
const useLiveData = createKnowledgeHook({
  hookId: 'data-updates',
  schema: DataSchema
});
```

### Pattern 3: Complex Composition
```typescript
const useComplexFlow = composeHooks({
  hooks: {
    step1: useStep1,
    step2: useStep2,
    step3: useStep3
  },
  combiner: ({ step1, step2, step3 }) => ({
    ...step1.data,
    ...step2.data,
    ...step3.data
  })
});
```

## Performance Considerations

### Automatic Optimizations
- **Debouncing**: Prevent excessive API calls
- **Caching**: Reduce duplicate requests
- **Batching**: Combine multiple operations
- **Memoization**: Prevent unnecessary re-renders

### Performance Patterns
```typescript
// Selective re-rendering
const useOptimized = createSelectiveHook({
  hook: useExpensive,
  selector: (data) => data.relevantPart
});

// Request batching
const useBatched = createBatchHook({
  hook: batchedFetch,
  batchWindowMs: 100
});
```

## Testing

### Built-in Test Utilities
```typescript
import { testHook } from 'unrdf/testing';

it('loads data', async () => {
  const result = await testHook(
    () => useMyHook({ id: '123' }),
    {
      initialState: { loading: true },
      finalState: { loading: false, data: expect.any(Object) }
    }
  );

  expect(result.data).toBeDefined();
});
```

## Migration Path

### From Manual Hooks
```typescript
// Before: 50+ lines of boilerplate
function useManual(id: string) {
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  // ... 40 more lines of error handling, caching, etc.
}

// After: 10 lines with framework
const useManual = createHook({
  inputSchema: z.object({ id: z.string() }),
  outputSchema: DataSchema,
  execute: fetchData
});
```

### Benefits
- ✅ 70% less code
- ✅ 100% type safety
- ✅ Best practices automatic
- ✅ Better testing
- ✅ Improved performance

## Next Steps

1. Review [React Hooks Abstraction](./react-hooks-abstraction.md)
2. Study [Real-World Examples](./react-hooks-abstraction.md#real-world-examples)
3. Check [Testing Framework](./react-hooks-abstraction.md#testing-framework)
4. See [Performance Patterns](./react-hooks-abstraction.md#performance-patterns)

---

> **🎯 Framework Philosophy:** Abstract the 80% boilerplate, expose the 20% that matters.
