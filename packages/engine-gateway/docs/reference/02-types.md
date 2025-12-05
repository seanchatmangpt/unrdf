# Types: @unrdf/engine-gateway

Complete type definitions for @unrdf/engine-gateway.

---

## Main Interfaces

### Options

Configuration object for functions.

```typescript
interface Options {
  engines: string | number | boolean;
  timeout: string | number | boolean;
  resultmerging: string | number | boolean;
  queryrouting: string | number | boolean;
  loadbalancing: string | number | boolean;
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
