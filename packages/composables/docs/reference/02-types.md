# Types: @unrdf/composables

Complete type definitions for @unrdf/composables.

---

## Main Interfaces

### Options

Configuration object for functions.

```typescript
interface Options {
  autosync: string | number | boolean;
  cachesize: string | number | boolean;
  updateinterval: string | number | boolean;
  debouncems: string | number | boolean;
  batchsize: string | number | boolean;
  timeout?: number;
  retries?: number;
}
```

### Result

Return value from operations.

```typescript
interface Result {
  success: boolean;
  data?: any;
  error?: string;
  timestamp: Date;
  duration: number;
}
```

---

## Common Types

### Status Type

```typescript
type Status = 'pending' | 'running' | 'completed' | 'failed' | 'cancelled';
```

### Error Type

```typescript
interface CustomError extends Error {
  code: string;
  details?: Record<string, any>;
  timestamp: Date;
}
```

---

## See Also

- [01-api.md](01-api.md) - API functions
- [03-configuration.md](03-configuration.md) - Configuration options
