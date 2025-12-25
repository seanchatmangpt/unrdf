# UNRDF API Design Guidelines

**Version**: 1.0.0
**Status**: Canonical Standard
**Last Updated**: 2025-12-25

## Philosophy

Good API design is invisible. Users should understand functions instantly without reading documentation.

**Core Principles**:
1. **Predictability**: Similar operations have similar signatures
2. **Simplicity**: Required first, options last
3. **Consistency**: Same patterns across all packages
4. **Safety**: Fail fast with clear errors
5. **Composability**: Small functions combine easily

## Function Naming Conventions

### Verb Patterns

| Pattern | Usage | Example |
|---------|-------|---------|
| `create*` | Factory functions returning new instances | `createStore()`, `createQuad()` |
| `execute*` | Run operations (queries, commands) | `executeQuery()`, `executeSelect()` |
| `get*` | Retrieve existing data (sync) | `getQuads()`, `getNamedNode()` |
| `fetch*` | Retrieve data (async, I/O) | `fetchRemoteGraph()` |
| `add*` | Insert new data | `addQuad()`, `addHook()` |
| `remove*` | Delete data | `removeQuad()`, `removeHook()` |
| `validate*` | Check correctness, return boolean | `validateQuad()`, `validateStore()` |
| `is*` | Boolean predicates | `isQuad()`, `isLiteral()` |
| `has*` | Check existence | `hasQuad()`, `hasNamespace()` |
| `iterate*` | Generator/iterator functions | `iterateQuads()`, `iterateBindings()` |
| `canonicalize*` | Normalize to standard form | `canonicalize()`, `canonicalizeGraph()` |
| `parse*` | Convert from string/buffer | `parseTriples()`, `parseTurtle()` |
| `serialize*` | Convert to string/buffer | `serializeQuads()`, `serializeJSON()` |

### Naming Rules

1. **Use full words** (not abbreviations)
   ```javascript
   // ✅ GOOD
   function executeQuery(query, options)

   // ❌ BAD
   function execQry(q, opts)
   ```

2. **Action + Noun pattern**
   ```javascript
   // ✅ GOOD
   createStore()
   addQuad()
   removeHook()

   // ❌ BAD
   storeCreate()
   quadAdd()
   ```

3. **Boolean predicates start with is/has/can**
   ```javascript
   // ✅ GOOD
   isQuad(value)
   hasQuads(store)
   canExecute(query)

   // ❌ BAD
   quad(value)         // Ambiguous
   quadsExist(store)   // Verbose
   ```

4. **Async functions use async suffix when ambiguous**
   ```javascript
   // ✅ GOOD
   function fetchGraph(url)           // Clearly I/O (async implied)
   function getQuads(pattern)          // Clearly sync
   function processAsync(data)         // When both versions exist

   // ❌ BAD
   async function getQuads()           // Confusing if sync version exists
   ```

## Parameter Ordering

### Standard Order

**Required parameters → Optional parameters → Options object → Callback**

```javascript
/**
 * Standard parameter pattern
 */
function operation(
  required1,           // 1. Required positional args
  required2,
  optional1 = null,    // 2. Optional positional (with defaults)
  options = {}         // 3. Options object (always last)
) {
  // implementation
}
```

### Examples

```javascript
// ✅ GOOD: Required first, options last
function createStore(initialQuads, options = {}) { }
function executeQuery(query, store, options = {}) { }
function addQuad(store, subject, predicate, object, options = {}) { }

// ❌ BAD: Options in middle
function executeQuery(query, options = {}, store) { }

// ❌ BAD: Too many required positional args
function addQuad(store, subject, predicate, object, graph, context, metadata) { }
// Better:
function addQuad(store, quad, options = {}) { }
```

### Options Object Pattern

**Always use object destructuring with defaults**:

```javascript
/**
 * @param {Object} [options] - Configuration options
 * @param {number} [options.limit=100] - Max results
 * @param {number} [options.offset=0] - Skip first N results
 * @param {AbortSignal} [options.signal] - Abort signal
 */
function executeQuery(query, store, options = {}) {
  const {
    limit = 100,
    offset = 0,
    signal,
  } = options;

  // Use limit, offset, signal
}
```

**Options object conventions**:
- Always optional (default `= {}`)
- Always documented in JSDoc
- Always destructured with defaults
- Max 5-7 options (if more, split function)

## Return Value Patterns

### Sync Functions

| Pattern | Return Type | Example |
|---------|-------------|---------|
| Factory | Instance | `createStore()` → `Store` |
| Query | Array/Iterator | `getQuads()` → `Quad[]` |
| Predicate | Boolean | `isQuad()` → `boolean` |
| Transform | New value | `canonicalize()` → `Quad[]` |
| Mutation | Modified input (or void) | `addQuad()` → `void` |

```javascript
// ✅ GOOD: Clear return types
function createStore(): Store
function getQuads(pattern): Quad[]
function isQuad(value): boolean
function canonicalize(quads): Quad[]
function addQuad(store, quad): void

// ❌ BAD: Ambiguous returns
function processStore(store): boolean | Store  // Which is it?
```

### Async Functions

**Always return Promise, never mix**:

```javascript
// ✅ GOOD: Consistent async
async function fetchGraph(url): Promise<Quad[]>
async function saveStore(store, path): Promise<void>

// ❌ BAD: Sometimes sync, sometimes async
function loadGraph(input) {
  if (typeof input === 'string') return parseTriples(input);  // Sync
  return fetchGraph(input);  // Async - INCONSISTENT!
}
```

### Chaining Pattern

**Methods that mutate should return `this` for chaining**:

```javascript
class QueryBuilder {
  select(...vars) {
    this.vars = vars;
    return this;  // ✅ Enable chaining
  }

  where(pattern) {
    this.patterns.push(pattern);
    return this;
  }
}

// Usage:
const results = new QueryBuilder()
  .select('?s', '?p', '?o')
  .where({ subject: '?s', predicate: '?p', object: '?o' })
  .execute(store);
```

## Error Handling Patterns

### Input Validation

**Validate early, fail fast**:

```javascript
import { z } from 'zod';

const QuadSchema = z.object({
  subject: z.object({ value: z.string() }),
  predicate: z.object({ value: z.string() }),
  object: z.object({ value: z.string() }),
});

function addQuad(store, quad) {
  // Validate inputs immediately
  try {
    QuadSchema.parse(quad);
  } catch (error) {
    throw new TypeError(`Invalid quad: ${error.message}`);
  }

  // Proceed with valid input
  store._quads.push(quad);
}
```

### Error Types

Use standard error types:

```javascript
// Input validation
throw new TypeError('Expected string, got number');

// Resource not found
throw new ReferenceError('Namespace "ex" not defined');

// Operation failed
throw new Error('SPARQL query execution failed: syntax error at line 5');

// Abort/timeout
throw new AbortError('Query timeout after 5000ms');
```

### Try-Catch Pattern

**Keep try blocks minimal**:

```javascript
// ✅ GOOD: Narrow try scope
function parseTriples(input) {
  let quads;
  try {
    quads = parser.parse(input);  // Only risky operation
  } catch (error) {
    throw new Error(`Parse failed: ${error.message}`);
  }

  return canonicalize(quads);  // Outside try - easier to debug
}

// ❌ BAD: Everything in try
function parseTriples(input) {
  try {
    const quads = parser.parse(input);
    const canonical = canonicalize(quads);
    const validated = validate(canonical);
    return process(validated);
  } catch (error) {
    // Which operation failed?
    throw new Error(`Something failed: ${error.message}`);
  }
}
```

## Async/Await Best Practices

### Prefer Async/Await over Promises

```javascript
// ✅ GOOD: Async/await
async function loadAndMerge(url1, url2) {
  const graph1 = await fetchGraph(url1);
  const graph2 = await fetchGraph(url2);
  return mergeGraphs(graph1, graph2);
}

// ❌ BAD: Promise chains (harder to read)
function loadAndMerge(url1, url2) {
  return fetchGraph(url1)
    .then(graph1 => fetchGraph(url2)
      .then(graph2 => mergeGraphs(graph1, graph2))
    );
}
```

### Parallel vs Sequential

```javascript
// Sequential (when operations depend on each other)
async function sequential() {
  const store = await createStore();
  const result = await executeQuery(query, store);  // Needs store
  return result;
}

// Parallel (when operations are independent)
async function parallel() {
  const [graph1, graph2, graph3] = await Promise.all([
    fetchGraph(url1),
    fetchGraph(url2),
    fetchGraph(url3),
  ]);
  return mergeGraphs(graph1, graph2, graph3);
}
```

### AbortSignal Support

**All async operations should support cancellation**:

```javascript
/**
 * @param {Object} [options]
 * @param {AbortSignal} [options.signal] - Abort signal
 */
async function fetchGraph(url, options = {}) {
  const { signal } = options;

  if (signal?.aborted) {
    throw new AbortError('Operation aborted before start');
  }

  const response = await fetch(url, { signal });

  return parseResponse(response);
}

// Usage:
const controller = new AbortController();
setTimeout(() => controller.abort(), 5000);

try {
  const graph = await fetchGraph(url, { signal: controller.signal });
} catch (error) {
  if (error.name === 'AbortError') {
    console.log('Request timed out');
  }
}
```

## Factory Pattern

**Use factory functions, not constructors**:

```javascript
// ✅ GOOD: Factory function
export function createStore(options = {}) {
  const { capacity = 1000 } = options;

  return {
    quads: [],
    capacity,
    addQuad(quad) { this.quads.push(quad); },
    getQuads(pattern) { return this.quads.filter(matches(pattern)); },
  };
}

// ❌ BAD: Class constructor (requires 'new', exports class)
export class Store {
  constructor(options = {}) {
    this.quads = [];
    this.capacity = options.capacity ?? 1000;
  }
}
```

**Rationale**:
- No `new` keyword required
- Easy to add initialization logic
- Can return different implementations
- Better for functional composition

## Fluent Interface Pattern

**When building complex configurations**:

```javascript
export function createQueryBuilder() {
  const state = {
    selectVars: [],
    wherePatterns: [],
    limit: null,
  };

  return {
    select(...vars) {
      state.selectVars.push(...vars);
      return this;
    },

    where(pattern) {
      state.wherePatterns.push(pattern);
      return this;
    },

    limitTo(n) {
      state.limit = n;
      return this;
    },

    build() {
      return buildQuery(state);
    },
  };
}

// Usage:
const query = createQueryBuilder()
  .select('?s', '?p', '?o')
  .where({ subject: '?s', predicate: RDF.type, object: '?o' })
  .limitTo(100)
  .build();
```

## Configuration API Pattern

**For packages with configuration**:

```javascript
import { z } from 'zod';

const ConfigSchema = z.object({
  endpoint: z.string().url(),
  timeout: z.number().min(0).default(5000),
  retries: z.number().int().min(0).default(3),
  cache: z.boolean().default(true),
});

export function createClient(config) {
  // Validate and fill defaults
  const validatedConfig = ConfigSchema.parse(config);

  return {
    query: (sparql) => executeQuery(sparql, validatedConfig),
    update: (sparql) => executeUpdate(sparql, validatedConfig),
  };
}

// Usage:
const client = createClient({
  endpoint: 'https://example.com/sparql',
  timeout: 10000,
  // retries and cache use defaults
});
```

## Iteration Patterns

### Synchronous Iteration

**Prefer iterators over arrays for large datasets**:

```javascript
// ✅ GOOD: Iterator (memory efficient)
export function* iterateQuads(store, pattern) {
  for (const quad of store._quads) {
    if (matches(quad, pattern)) {
      yield quad;
    }
  }
}

// Usage:
for (const quad of iterateQuads(store, { predicate: RDF.type })) {
  console.log(quad);
}

// ❌ BAD: Array (loads all into memory)
export function getQuads(store, pattern) {
  return store._quads.filter(quad => matches(quad, pattern));
}
```

### Async Iteration

**For streaming data**:

```javascript
export async function* streamQuads(url) {
  const response = await fetch(url);
  const reader = response.body.getReader();
  const parser = createStreamingParser();

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    for (const quad of parser.parse(value)) {
      yield quad;
    }
  }
}

// Usage:
for await (const quad of streamQuads(url)) {
  store.addQuad(quad);
}
```

## Plugin/Extension Pattern

**For extensible systems**:

```javascript
export function createPluginManager() {
  const plugins = [];

  return {
    register(plugin) {
      // Validate plugin interface
      if (!plugin.name || !plugin.init) {
        throw new TypeError('Invalid plugin: missing name or init');
      }
      plugins.push(plugin);
    },

    async init() {
      for (const plugin of plugins) {
        await plugin.init();
      }
    },

    async execute(event) {
      for (const plugin of plugins) {
        if (plugin.on?.(event.type)) {
          await plugin.on(event.type)(event);
        }
      }
    },
  };
}

// Plugin interface:
const myPlugin = {
  name: 'my-plugin',
  async init() {
    console.log('Plugin initialized');
  },
  on: (type) => {
    if (type === 'beforeQuery') {
      return async (event) => {
        console.log('Before query:', event.query);
      };
    }
  },
};
```

## JSDoc Standards

**Every exported function requires JSDoc**:

```javascript
/**
 * Creates a new RDF store with optional initial quads
 *
 * The store provides in-memory RDF triple/quad storage with SPARQL query
 * support. All operations are synchronous for maximum performance.
 *
 * @param {Quad[]} [initialQuads=[]] - Initial quads to populate store
 * @param {Object} [options] - Configuration options
 * @param {number} [options.capacity=10000] - Initial capacity hint
 * @param {boolean} [options.validate=true] - Validate quads on insert
 * @returns {Store} RDF store instance
 * @throws {TypeError} If initialQuads contains invalid quads
 * @example
 * const store = createStore();
 * store.addQuad(quad(namedNode('ex:s'), namedNode('ex:p'), literal('o')));
 *
 * @example
 * const store = createStore([quad1, quad2], { capacity: 5000 });
 */
export function createStore(initialQuads = [], options = {}) {
  // implementation
}
```

**Required tags**:
- `@param` for each parameter (type + description)
- `@returns` with type and description
- `@throws` for any errors thrown
- `@example` for non-trivial functions (at least one)

**Optional but recommended**:
- `@see` for related functions
- `@deprecated` for deprecated APIs
- `@since` for version added

## Deprecation Pattern

**When retiring APIs**:

```javascript
/**
 * @deprecated Use createStore() instead. Will be removed in v6.0.0.
 * @see createStore
 */
export function newStore(options) {
  console.warn('newStore() is deprecated. Use createStore() instead.');
  return createStore(options);
}
```

## API Versioning

**Semantic versioning for breaking changes**:

- **Patch (1.0.X)**: Bug fixes, no API changes
- **Minor (1.X.0)**: New features, backward compatible
- **Major (X.0.0)**: Breaking changes

**Breaking changes**:
- Removing exported functions
- Changing function signatures
- Changing return types
- Changing error behavior

**NOT breaking**:
- Adding new optional parameters (with defaults)
- Adding new exports
- Adding new properties to returned objects
- Performance improvements

## Testing API Design

**Every API should have tests demonstrating**:

1. **Happy path**:
   ```javascript
   it('creates store with initial quads', () => {
     const store = createStore([quad1, quad2]);
     expect(store.size).toBe(2);
   });
   ```

2. **Edge cases**:
   ```javascript
   it('handles empty input', () => {
     const store = createStore([]);
     expect(store.size).toBe(0);
   });
   ```

3. **Error cases**:
   ```javascript
   it('throws on invalid quad', () => {
     expect(() => createStore([null])).toThrow(TypeError);
   });
   ```

4. **Examples from docs work**:
   ```javascript
   it('example from JSDoc works', () => {
     // Copy example from JSDoc
     const store = createStore();
     store.addQuad(quad(namedNode('ex:s'), namedNode('ex:p'), literal('o')));
     expect(store.size).toBe(1);
   });
   ```

## API Design Checklist

Before publishing an API:

- [ ] Function names follow verb patterns
- [ ] Parameters ordered: required → optional → options
- [ ] Options object has defaults and JSDoc
- [ ] Return type is consistent (not mixed sync/async)
- [ ] Errors validated with Zod schemas
- [ ] All exports have JSDoc with `@param`, `@returns`, `@example`
- [ ] Async functions support AbortSignal
- [ ] Large data uses iterators, not arrays
- [ ] Tests cover happy path, edge cases, errors
- [ ] Examples in JSDoc actually work

## Anti-Patterns

### DO NOT

1. **Mix sync and async**:
   ```javascript
   // ❌ BAD
   function load(input) {
     if (typeof input === 'string') return parseSync(input);
     return fetchAsync(input);
   }
   ```

2. **Return different types**:
   ```javascript
   // ❌ BAD
   function getQuad(id) {
     const quad = store.get(id);
     return quad ?? false;  // Quad | boolean
   }
   ```

3. **Mutate parameters silently**:
   ```javascript
   // ❌ BAD
   function addPrefix(store, prefix, uri) {
     store.prefixes[prefix] = uri;  // Mutates input
   }

   // ✅ GOOD: Explicit
   function addPrefix(store, prefix, uri) {
     store.prefixes[prefix] = uri;
     return store;  // Clear mutation
   }
   ```

4. **Callback hell**:
   ```javascript
   // ❌ BAD
   function loadAll(urls, callback) {
     load(urls[0], (graph1) => {
       load(urls[1], (graph2) => {
         callback(merge(graph1, graph2));
       });
     });
   }

   // ✅ GOOD: async/await
   async function loadAll(urls) {
     const graphs = await Promise.all(urls.map(load));
     return merge(...graphs);
   }
   ```

5. **Boolean trap**:
   ```javascript
   // ❌ BAD
   function query(sparql, true, false)  // What do these mean?

   // ✅ GOOD
   function query(sparql, { validate: true, cache: false })
   ```

## Related Documents

- [Package Structure](./PACKAGE-STRUCTURE.md) - Directory organization
- [Plugin Architecture](./PLUGIN-ARCHITECTURE.md) - Extensibility patterns
- [Configuration Patterns](../packages/core/src/config.mjs) - Config management

## References

**API design inspirations**:
- [Stripe API](https://stripe.com/docs/api) - Clear parameters, options objects
- [Node.js API](https://nodejs.org/docs/latest/api/) - Consistent patterns
- [React API](https://react.dev/reference/react) - Factory functions, hooks pattern

---

**Version History**:
- 1.0.0 (2025-12-25): Initial standard based on @unrdf/core, @unrdf/streaming patterns
