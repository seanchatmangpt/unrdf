# Agent 6 Exploration Notes

## API Locations (Verified)

### Core Hook Files

#### /home/user/unrdf/packages/hooks/src/index.mjs

**Main API Export Point**

Exports all public APIs:

```javascript
// Hook definition
export { defineHook, isValidHook, getHookMetadata, hasValidation, hasTransformation, ... }
export { HookTriggerSchema, HookConfigSchema, HookSchema }

// Hook execution
export { executeHook, executeHookChain, executeHooksByTrigger, wouldPassHooks, ... }
export { HookResultSchema, ChainResultSchema }

// Batch operations
export { executeBatch, validateBatch, transformBatch }

// Cache management
export { clearHookCaches, prewarmHookCache }

// Chain compilation (JIT)
export { compileHookChain, compileValidationOnlyChain, ... }

// Quad pooling
export { QuadPool, quadPool, createPooledTransform, isPooledQuad }

// Hook management
export { createHookRegistry, registerHook, unregisterHook, getHook, listHooks, ... }

// Built-in hooks
export { builtinHooks, validateSubjectIRI, validatePredicateIRI, ... }

// Hook manager (class-based)
export { KnowledgeHookManager }

// Hook scheduler
export { HookScheduler, createHookScheduler, ScheduleConfigSchema }

// Quality metrics
export { QualityMetricsCollector, createQualityHooks, ... }
```

#### /home/user/unrdf/packages/hooks/src/hooks/define-hook.mjs

**Hook Definition API**

- `defineHook(config)` - Create a hook
  - Input: `{ name, trigger, validate?, transform?, metadata? }`
  - Output: Hook object with `_validated`, `_hasValidation`, `_hasTransformation` flags

- `isValidHook(hook)` - Validate a hook object

- `getHookMetadata(hook, key)` - Access hook metadata

- `hasValidation(hook)` - Check if hook has validation (fast path, uses \_validated flag)

- `hasTransformation(hook)` - Check if hook has transform (fast path)

**Hook Trigger Types** (from HookTriggerSchema):

```javascript
z.enum([
  'before-add',
  'after-add',
  'before-query',
  'after-query',
  'before-remove',
  'after-remove',
  'before-commit',
  'after-commit',
  'before-rollback',
  'after-rollback',
  'on-error',
  'on-validation-fail',
  'on-transform',
  'on-timeout',
  'on-circuit-open',
  'before-fetch',
  'after-fetch',
  'before-sync',
  'after-sync',
  'before-import',
  'after-import',
  'on-schedule',
  'on-interval',
  'on-idle',
  'on-startup',
  'quality-gate',
  'defect-detection',
  'continuous-improvement',
  'spc-control',
  'capability-analysis',
  'root-cause',
  'kaizen-event',
  'audit-trail',
]);
```

#### /home/user/unrdf/packages/hooks/src/hooks/hook-executor.mjs

**Hook Execution API**

- `executeHook(hook, quad, options?)` - Execute single hook
  - Returns: `{ valid, quad, error, hookName, warning?, errorDetails? }`
  - POKA-YOKE guards: Non-boolean returns, pooled quad warnings

- `executeHookChain(hooks, quad)` - Execute sequential hooks
  - Returns: `{ valid, quad, results, error }`
  - Behavior: Fail-fast (stops on first rejection), chains transforms

- `executeHooksByTrigger(hooks, trigger, quad)` - Filter by trigger and execute

- `wouldPassHooks(hooks, quad)` - Dry-run validation

- `validateOnly(hooks, quad)` - Validation-only, skip transforms (faster)

- `executeBatch(hooks, quads)` - Bulk operations

- `validateBatch(hooks, quads)` - Bulk validation

- `transformBatch(hooks, quads)` - Bulk transformation

- `clearHookCaches()` - Clear compiled chain cache

- `prewarmHookCache(hooks)` - Pre-compile chains

#### /home/user/unrdf/packages/hooks/src/hooks/policy-pack.mjs

**Policy Pack Management**

```javascript
class PolicyPack {
  constructor(manifest, basePath?)

  // Load from filesystem
  async load()

  // Access hooks/conditions/resources
  getHooks()
  getHook(name)
  getConditions()
  getCondition(name)
  getResources()
  getResource(name)

  // Compatibility check
  checkCompatibility(environment?)

  // Statistics
  getStats()
}

class PolicyPackManager {
  constructor(basePath?)

  // Load packs
  async loadPolicyPack(manifestPath)
  async loadAllPolicyPacks(packsDir?)

  // Lifecycle
  activatePolicyPack(packName)
  deactivatePolicyPack(packName)

  // Query
  getPolicyPack(name)
  getAllPolicyPacks()
  getActivePolicyPacks()
  getActiveHooks()

  // Statistics
  getStats()
}

// Manifest structure
PolicyPackManifestSchema = z.object({
  id: z.string().uuid(),
  meta: {
    name: z.string(),
    version: z.string(),  // semantic version
    description: z.string().optional(),
    author: z.string().optional(),
    tags: z.array(z.string()).optional(),
    ontology: z.array(z.string()).optional(),
    dependencies: z.array({ name, version, required }).optional(),
  },
  config: {
    enabled: z.boolean(),
    priority: z.number().min(0).max(100),
    strictMode: z.boolean(),
    timeout: z.number().max(300000),
    retries: z.number().max(5),
    conditions: { environment, version, features },
  },
  hooks: z.array({
    name: z.string(),
    file: z.string(),
    enabled: z.boolean(),
    priority: z.number(),
  }),
  conditions?: z.array({
    name, file,
    type: enum(['sparql-ask', 'sparql-select', 'shacl'])
  }),
  resources?: z.array({
    name, file,
    type: enum(['ontology', 'vocabulary', 'data', 'other'])
  }),
})
```

#### /home/user/unrdf/packages/hooks/src/hooks/builtin-hooks.mjs

**Pre-built Hook Patterns**

Validation Hooks:

- `validateSubjectIRI` - Subject is NamedNode
- `validatePredicateIRI` - Predicate is NamedNode
- `validateObjectLiteral` - Object is Literal
- `validateIRIFormat` - IRIs are well-formed URLs
- `validateLanguageTag` - Literals have language tags
- `rejectBlankNodes` - Reject blank node subjects

Transformation Hooks:

- `normalizeNamespace` - Normalize namespace prefixes
- `normalizeLanguageTag` - Lowercase language tags
- `trimLiterals` - Trim whitespace

Pooled variants (zero-allocation):

- `normalizeLanguageTagPooled`
- `trimLiteralsPooled`

Combined:

- `standardValidation` - All validation hooks together

#### /home/user/unrdf/packages/hooks/src/hooks/hook-management.mjs

**Hook Registry API**

```javascript
// Create isolated registry
createHookRegistry();

// Lifecycle
registerHook(registry, hook);
unregisterHook(registry, hookName);

// Query
getHook(registry, hookName);
listHooks(registry);
getHooksByTrigger(registry, trigger);
hasHook(registry, hookName);

// Maintenance
clearHooks(registry);
getRegistryStats(registry);
```

#### /home/user/unrdf/packages/hooks/src/hooks/hook-chain-compiler.mjs

**JIT Compilation for Performance**

```javascript
// Compile hook chain to optimized function
compileHookChain(hooks);

// Validation-only variant (faster)
compileValidationOnlyChain(hooks);

// Cache management
clearCompiledChainCache();
getCompilerStats();
isJitAvailable();
getChainKey(hooks);
```

#### /home/user/unrdf/packages/hooks/src/hooks/quad-pool.mjs

**Object Pooling for Zero-Allocation Transforms**

```javascript
class QuadPool {
  // Reuse Quad objects
  acquire()
  release(quad)
  reset()
}

// Global pool instance
quadPool

// Create pooled transform
createPooledTransform(transformFn)

// Check if quad is pooled
isPooledQuad(quad)
```

#### /home/user/unrdf/packages/hooks/src/hooks/hook-scheduler.mjs

**Cron/Interval-based Hooks**

```javascript
class HookScheduler {
  // Schedule hooks for time-based execution
  constructor()

  addScheduledHook(hook, schedule)
  removeScheduledHook(hookName)
  start()
  stop()
  pause()
  resume()
}

createHookScheduler(options)

// Schedule config
ScheduleConfigSchema = z.object({
  type: enum(['cron', 'interval']),
  pattern: z.string(),          // cron: '0 0 * * *'
  interval: z.number(),         // ms
  startTime: z.date().optional(),
  endTime: z.date().optional(),
  timezone: z.string().optional(),
})
```

#### /home/user/unrdf/packages/hooks/src/hooks/quality-metrics.mjs

**Lean Six Sigma Support**

```javascript
class QualityMetricsCollector {
  // Statistical Process Control hooks
  recordDataPoint(metric, value)

  // Defect tracking
  recordDefect(quad, type, severity)

  // Continuous improvement
  recordKaizen(event)

  // Reports
  getControlChartData()
  getDefectAnalysis()
  getCapabilityIndex()
}

createQualityHooks(config)

// Quality gate schema
QualityGateSchema = z.object({
  name: z.string(),
  threshold: z.number(),
  metric: z.string(),
  action: enum(['allow', 'reject', 'escalate']),
})

// SPC data point schema
SPCDataPointSchema = z.object({
  timestamp: z.date(),
  value: z.number(),
  subgroup: z.number().optional(),
})
```

#### /home/user/unrdf/packages/hooks/src/hooks/knowledge-hook-manager.mjs

**Class-based Hook Management**

```javascript
class KnowledgeHookManager {
  constructor(options?)

  // Define and register
  define(name, config)
  register(hook)

  // Execute
  execute(hookName, quad)
  executeChain(hookNames, quad)

  // Query
  getHook(name)
  listHooks()

  // Batch operations
  batch(hookNames, quads)

  // Statistics
  getStats()
}
```

## API Limitations

### 1. Filesystem-based Policy Packs

**Limitation**: Policy packs must be loaded from directory structure

```
policy-packs/
  my-policy/
    manifest.json
    hooks/
      validate-something.mjs
      transform-something.mjs
    conditions/
      query.sparql
    resources/
      vocabulary.ttl
```

**Impact**: Can't load policy packs from databases or remote sources without custom loader

**Workaround**: Create custom `PolicyPackManager` that overrides `loadPolicyPack()` method

### 2. Synchronous Execution Only

**Limitation**: All hook execution is synchronous (`executeHook`, `executeHookChain`, etc.)

**Impact**: Can't call async validation (e.g., database lookups, API calls)

**Workaround**: Pre-load async data before hook execution, or wrap in Promise

### 3. No Automatic Receipt Generation

**Limitation**: Receipts must be built manually from hook execution results

**Impact**: Receipt generation logic must be implemented by user

**Workaround**: Wrap `executeHook()` to automatically generate receipts

```javascript
function executeHookWithReceipt(hook, quad) {
  const result = executeHook(hook, quad);
  return {
    receipt: new Receipt(quad, hook.name, result.valid ? 'allow' : 'reject', result.error),
    hookResult: result,
  };
}
```

### 4. No Async Transform Support

**Limitation**: Transform functions cannot be async

**Impact**: Can't normalize values using external services (translation APIs, geocoding, etc.)

**Workaround**: Pre-process data or use after-commit hooks with separate async processing

### 5. Manual Policy Conflict Resolution

**Limitation**: No automatic detection of conflicting policies (e.g., two policies with opposite validations)

**Impact**: User responsible for policy design validation

**Workaround**: Create policy composition test suite

### 6. Non-boolean Validation Returns Coerced

**Limitation**: If `validate()` returns non-boolean, it's coerced (with warning)

```javascript
// This logs warning and coerces to boolean
validate: quad => quad.subject; // Returns quad, not boolean!
```

**Impact**: Bugs may be hidden by automatic coercion

**Workaround**: Always return boolean from validate(), use POKA-YOKE guards

### 7. Transform Must Return Valid Quad

**Limitation**: `transform()` must return an object with subject, predicate, object properties

**Impact**: Runtime errors if transform returns null or invalid Quad

**Workaround**: Use TypeScript or JSDoc with Zod validation

### 8. Policy Pack Manifests Are JSON Only

**Limitation**: Manifests must be JSON (no YAML, TOML support)

**Impact**: Verbose manifest files

**Workaround**: Use YAML loader if needed (custom implementation)

### 9. No Hook Dependency Management

**Limitation**: Hooks don't declare dependencies on other hooks

**Impact**: Hard to compose complex policy chains

**Workaround**: Use metadata field to document dependencies

### 10. Registry is Mutable

**Limitation**: Hooks can be registered/unregistered at runtime, no immutability guarantees

**Impact**: Hooks could change during execution, causing race conditions

**Workaround**: Create hooks registry once, don't modify after initialization

## Performance Characteristics

### Sub-1μs Execution (Hot Path)

From code comments:

```javascript
// Pre-computed flags for sub-1μs execution (skip Zod in hot path)
hook._validated = true;
hook._hasValidation = true;
hook._hasTransformation = true;
```

Benefits:

- Hook creation validated at definition time
- No Zod parsing in execution loop
- Fast path checks via flags

### Batch Operations

```javascript
// High-performance bulk operations
executeBatch(hooks, quads); // Zod-free hot path
validateBatch(hooks, quads);
transformBatch(hooks, quads);
```

### Object Pooling

```javascript
// Zero-allocation transforms via pooling
const pooledTransform = createPooledTransform(quad => {
  // Reuse Quad objects from pool
  return quad;
});
```

### JIT Compilation

```javascript
// Compile hook chains for optimization
const compiled = compileHookChain(hooks);
const result = compiled(quad); // Fast path
```

## Error Handling

### POKA-YOKE Guards

From hook-executor.mjs:

1. **Non-boolean validation** - Coerces, logs warning
2. **Invalid transform return** - Throws TypeError with context
3. **Missing quad properties** - Detects and reports
4. **Pooled quad return** - Warns about memory issues
5. **Stack trace preservation** - Maintains error context in `errorDetails`

### Error Result Structure

```javascript
HookResult = {
  valid: false,
  error: 'error message',
  errorDetails: {
    hookName: 'hook-name',
    hookTrigger: 'before-add',
    stack: 'stack trace',
    originalError: Error,
    rawError: mixed, // if not Error instance
  },
};
```

## Testing

Test file: `/home/user/unrdf/packages/hooks/test/hooks.test.mjs`

Key test patterns:

```javascript
// Hook definition testing
it('should define a validation hook', () => {
  const hook = defineHook({
    name: 'test',
    trigger: 'before-add',
    validate: q => q.subject.termType === 'NamedNode',
  });
  expect(hook.name).toBe('test');
});

// Execution testing
it('should execute hook chain', () => {
  const result = executeHookChain([hook1, hook2], quad);
  expect(result.valid).toBe(true);
});
```

## Production Patterns

### Pattern 1: Receipt with Audit Trail

```javascript
async function insertQuadWithAudit(quad, hooks) {
  const receipts = [];

  for (const hook of hooks) {
    const result = executeHook(hook, quad);
    receipts.push({
      timestamp: new Date().toISOString(),
      hook: hook.name,
      quad: quad,
      valid: result.valid,
      reason: result.error || 'Valid',
    });

    if (!result.valid) break;
  }

  // Persist receipts to audit log
  await auditLog.insert(receipts);

  // If all valid, insert quad
  if (receipts[receipts.length - 1].valid) {
    store.insert(quad);
  }

  return receipts;
}
```

### Pattern 2: Policy Composition

```javascript
class DataQualityPolicy {
  constructor() {
    this.hooks = [
      defineHook({
        name: 'validate-iri',
        trigger: 'before-add',
        validate: q => q.subject.termType === 'NamedNode',
      }),
      defineHook({
        name: 'validate-predicate',
        trigger: 'before-add',
        validate: q => this.allowedPredicates.has(q.predicate.value),
      }),
      defineHook({
        name: 'normalize-language',
        trigger: 'before-add',
        transform: q => ({
          ...q,
          object: q.object.language
            ? { ...q.object, language: q.object.language.toLowerCase() }
            : q.object,
        }),
      }),
    ];
  }

  apply(quad) {
    return executeHookChain(this.hooks, quad);
  }
}
```

## Summary Table

| Aspect            | Status           | Location                  |
| ----------------- | ---------------- | ------------------------- |
| Hook Definition   | ✅ Complete      | `define-hook.mjs`         |
| Hook Execution    | ✅ Complete      | `hook-executor.mjs`       |
| Hook Chaining     | ✅ Complete      | `hook-executor.mjs`       |
| Receipt Structure | ❌ Manual        | Not built-in              |
| Policy Packs      | ✅ Complete      | `policy-pack.mjs`         |
| Built-in Hooks    | ✅ Complete      | `builtin-hooks.mjs`       |
| Hook Management   | ✅ Complete      | `hook-management.mjs`     |
| Async Transforms  | ❌ Not supported | -                         |
| Async Validation  | ❌ Not supported | -                         |
| JIT Compilation   | ✅ Available     | `hook-chain-compiler.mjs` |
| Batch Operations  | ✅ Available     | `hook-executor.mjs`       |
| Object Pooling    | ✅ Available     | `quad-pool.mjs`           |
| Hook Scheduler    | ✅ Available     | `hook-scheduler.mjs`      |
| Quality Metrics   | ✅ Available     | `quality-metrics.mjs`     |
