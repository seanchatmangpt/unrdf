# How to Hot-Reload Validation Policies

This guide shows how to use AtomVM's hot code loading to update validation policies **without downtime**.

## Overview

Hot-reloading policies enables:

- **Zero-downtime updates**: Change validation rules while system runs
- **Fast reload**: <100ms policy swap time
- **Atomic transitions**: Old policy → new policy with no intermediate state
- **Callback hooks**: Execute code before/after reload

## Quick Start

```javascript
import { HotCodeLoader } from '@unrdf/atomvm';
import { RDFValidator } from '@unrdf/atomvm';

// 1. Create policy manager
const loader = new HotCodeLoader(runtime);

// 2. Load initial policy
const policy = new RDFValidator();
policy.registerShape('User', [
  { property: 'email', required: true, datatype: 'xsd:string' },
  { property: 'age', required: true, datatype: 'xsd:integer', minValue: 18 }
]);

// 3. Register hot-swap callback
loader.registerHotSwap('user-policy', {
  beforeSwap: async () => console.log('Updating policy...'),
  afterSwap: async ({ version }) => console.log(`Policy v${version} active`),
});

// 4. Hot-reload new policy
await loader.reloadModule('user-policy');
```

## Creating a Policy Pack

A **policy pack** is a module containing validation rules that can be hot-reloaded.

### Basic Policy Pack

```javascript
/**
 * Policy Pack - Encapsulates validation rules
 */
class PolicyPack {
  constructor(name, version = 1) {
    this.name = name;
    this.version = version;
    this.validator = new RDFValidator();
    this.rules = [];
  }

  /**
   * Register validation rule (SHACL-like shape)
   */
  registerRule(shapeName, rules, targetClass) {
    this.validator.registerShape(shapeName, rules, targetClass);
    this.rules.push({ shapeName, rules, targetClass });
    return this;
  }

  /**
   * Validate triples using current policy
   */
  async validate(triples) {
    const shapeNames = this.rules.map(r => r.shapeName);
    return this.validator.validateGraph(triples, { shapes: shapeNames });
  }

  /**
   * Get policy metadata
   */
  getMetadata() {
    return {
      name: this.name,
      version: this.version,
      ruleCount: this.rules.length,
      shapes: this.rules.map(r => r.shapeName),
    };
  }
}
```

### Example Policy

```javascript
const userPolicy = new PolicyPack('user-validation', 1);

userPolicy.registerRule('schema:Person', [
  {
    property: 'schema:name',
    required: true,
    datatype: 'xsd:string',
    minLength: 2,
    maxLength: 100,
  },
  {
    property: 'schema:email',
    required: true,
    datatype: 'xsd:string',
    pattern: /^[^\s@]+@[^\s@]+\.[^\s@]+$/,
  },
  {
    property: 'schema:birthDate',
    required: false,
    datatype: 'xsd:date',
  },
  {
    property: 'schema:url',
    required: false,
    nodeKind: 'IRI',
  },
], 'schema:Person');
```

## Hot-Reloading Policies

### Step 1: Load Initial Policy

```javascript
import { HotCodeLoader } from '@unrdf/atomvm';

const loader = new HotCodeLoader(runtime);

// Load initial policy module
const result = await loader.loadModule('/policies/user-policy.beam');
console.log(`Loaded: ${result.moduleName} (${result.signature})`);
```

### Step 2: Register Reload Callbacks

```javascript
loader.registerHotSwap('user-policy', {
  /**
   * Called BEFORE policy swap
   * Use for: logging, validation, backup
   */
  beforeSwap: async ({ moduleName, timestamp }) => {
    console.log(`[${timestamp}] Preparing to reload ${moduleName}`);
    // Optional: backup current policy
  },

  /**
   * Called AFTER successful swap
   * Use for: cache invalidation, notifications
   */
  afterSwap: async ({ moduleName, version, signature, timestamp }) => {
    console.log(`[${timestamp}] Loaded ${moduleName} v${version}`);
    console.log(`  Signature: ${signature}`);
    // Optional: invalidate validation cache
  },

  /**
   * Called if reload fails
   * Use for: error handling, rollback
   */
  onError: async ({ moduleName, error }) => {
    console.error(`Failed to reload ${moduleName}: ${error.message}`);
    // Optional: alert monitoring system
  },
});
```

### Step 3: Execute Hot Reload

```javascript
// Update policy definition
const updatedPolicy = new PolicyPack('user-policy', 2);
updatedPolicy.registerRule('schema:Person', [
  {
    property: 'schema:name',
    required: true,
    datatype: 'xsd:string',
    minLength: 5, // STRICTER: was 2
    maxLength: 100,
  },
  {
    property: 'schema:email',
    required: true,
    datatype: 'xsd:string',
    pattern: /^[^\s@]+@[^\s@]+\.[^\s@]+$/,
  },
  {
    property: 'schema:verified', // NEW RULE
    required: true,
    datatype: 'xsd:boolean',
  },
], 'schema:Person');

// Hot-reload (zero downtime)
const reloadResult = await loader.reloadModule('user-policy');

console.log(`Reload complete in ${reloadResult.duration}ms`);
console.log(`New version: ${reloadResult.version}`);
```

## Performance Guarantees

### Zero-Downtime Target: <100ms

Hot-reload operations must complete in **<100ms** to ensure zero perceived downtime.

```javascript
const startTime = performance.now();
const result = await loader.reloadModule('policy');
const duration = performance.now() - startTime;

if (duration > 100) {
  console.warn(`⚠️  Reload took ${duration}ms (target: <100ms)`);
} else {
  console.log(`✅ Zero-downtime: ${duration}ms`);
}
```

### Measuring Reload Performance

```javascript
async function measureReload(loader, moduleName) {
  const metrics = {
    reloadTime: 0,
    signatureComputeTime: 0,
    callbackTime: 0,
  };

  let callbackStart = 0;

  loader.registerHotSwap(moduleName, {
    beforeSwap: async () => {
      callbackStart = performance.now();
    },
    afterSwap: async () => {
      metrics.callbackTime = performance.now() - callbackStart;
    },
  });

  const start = performance.now();
  const result = await loader.reloadModule(moduleName);
  metrics.reloadTime = performance.now() - start;

  return {
    ...metrics,
    success: result.success,
    version: result.version,
  };
}

const metrics = await measureReload(loader, 'user-policy');
console.log('Reload metrics:', metrics);
// Example output:
// {
//   reloadTime: 42,
//   callbackTime: 3,
//   success: true,
//   version: 2
// }
```

## Validation Examples

### Before Reload (Permissive Policy)

```javascript
const policy1 = new PolicyPack('validation', 1);
policy1.registerRule('test:Document', [
  { property: 'test:title', required: true, datatype: 'xsd:string' },
]);

const triples = [
  {
    subject: 'http://example.org/doc1',
    predicate: 'rdf:type',
    value: 'test:Document',
  },
  {
    subject: 'http://example.org/doc1',
    predicate: 'test:title',
    value: 'Hi', // Short title - OK
    datatype: 'xsd:string',
  },
];

const result = await policy1.validate(triples);
console.log(result.valid); // true
```

### After Reload (Stricter Policy)

```javascript
const policy2 = new PolicyPack('validation', 2);
policy2.registerRule('test:Document', [
  {
    property: 'test:title',
    required: true,
    datatype: 'xsd:string',
    minLength: 10, // NEW: minimum length
  },
]);

await loader.reloadModule('validation');

const result = await policy2.validate(triples);
console.log(result.valid); // false
console.log(result.errors[0].type); // 'MIN_LENGTH_VIOLATION'
```

## Best Practices

### 1. Version Your Policies

```javascript
const policy = new PolicyPack('user-policy', 2);
//                                           ^ version number
```

- Increment version on every change
- Use semantic versioning for breaking changes
- Track policy versions in logs

### 2. Test Before Reload

```javascript
async function safeReload(loader, newPolicy, testTriples) {
  // Validate new policy against test data
  const testResult = await newPolicy.validate(testTriples);

  if (!testResult.valid && testTriples.shouldPass) {
    throw new Error('New policy fails validation test');
  }

  // Proceed with reload
  return loader.reloadModule(newPolicy.name);
}
```

### 3. Handle Reload Failures

```javascript
try {
  const result = await loader.reloadModule('policy');
  if (!result.success) {
    console.error('Reload failed:', result.error);
    // Old policy remains active - system continues working
  }
} catch (error) {
  console.error('Reload exception:', error);
  // Implement rollback or alert
}
```

### 4. Monitor Reload Performance

```javascript
loader.registerHotSwap('policy', {
  afterSwap: async ({ version, timestamp }) => {
    metrics.recordPolicyReload({
      version,
      timestamp,
      duration: Date.now() - timestamp,
    });

    if (duration > 100) {
      alerts.warn('Policy reload exceeded 100ms threshold');
    }
  },
});
```

### 5. Queue Concurrent Reloads

HotCodeLoader automatically queues concurrent reloads:

```javascript
// These will be queued and executed sequentially
const reload1 = loader.reloadModule('policy-1');
const reload2 = loader.reloadModule('policy-2');
const reload3 = loader.reloadModule('policy-3');

await Promise.all([reload1, reload2, reload3]);
```

## Guarantees

### What Hot Reload Guarantees

✅ **Atomicity**: Old policy → new policy in single operation
✅ **No intermediate state**: Never in "half-updated" state
✅ **Rollback on failure**: Old policy remains if reload fails
✅ **Callback execution**: beforeSwap/afterSwap always called
✅ **Signature verification**: Module integrity checked

### What Hot Reload Does NOT Guarantee

❌ **In-flight requests**: Requests during reload may use old or new policy
❌ **Backward compatibility**: New policy may reject previously valid data
❌ **Zero data loss**: Validation cache may be cleared

## Troubleshooting

### Reload Takes >100ms

**Symptom**: `duration > 100` in reload result

**Causes**:
- Large policy module (>1MB)
- Complex signature computation
- Slow filesystem/network

**Solutions**:
```javascript
// 1. Optimize policy size
const policy = new PolicyPack('optimized', 1);
// Use fewer, more general rules instead of many specific ones

// 2. Increase reload timeout (if necessary)
const loader = new HotCodeLoader(runtime, {
  reloadTimeout: 60000, // 60 seconds
});

// 3. Pre-compile policy modules
// Generate .beam files ahead of time
```

### Callbacks Not Executing

**Symptom**: `beforeSwap`/`afterSwap` not called

**Causes**:
- Callback registered after reload started
- Exception in callback code

**Solutions**:
```javascript
// Register callback BEFORE first reload
loader.registerHotSwap('policy', {
  beforeSwap: async () => {
    try {
      // Your code
    } catch (error) {
      console.error('Callback error:', error);
      // Don't throw - allows reload to continue
    }
  },
});
```

### Old Policy Still Active

**Symptom**: New rules not applied after reload

**Causes**:
- Reload failed silently
- Policy cache not invalidated
- Wrong module reloaded

**Solutions**:
```javascript
const result = await loader.reloadModule('policy');

// Check result
if (!result.success) {
  console.error('Reload failed:', result.error);
  // Old policy still active - expected behavior
}

// Verify version updated
const moduleInfo = loader.activeModules.get('policy');
console.log(`Active version: ${moduleInfo.version}`);

// Clear validation cache after reload
loader.registerHotSwap('policy', {
  afterSwap: async () => {
    validationCache.clear();
  },
});
```

## Complete Example

See [`packages/atomvm/examples/policy-hot-reload-demo.mjs`](../../examples/policy-hot-reload-demo.mjs) for a complete working example demonstrating:

- Initial policy load
- Hot reload with timing measurement
- Validation before/after reload
- Callback execution
- Zero-downtime verification

Run the demo:

```bash
node packages/atomvm/examples/policy-hot-reload-demo.mjs
```

Expected output:

```
═══════════════════════════════════════════════════════════════════
  Policy Hot Reload Demo
  Zero-Downtime Validation Policy Updates
═══════════════════════════════════════════════════════════════════

📦 Loading policy: validation-policy v1
   ✅ Loaded in 42.35ms
   📋 Rules: 2

🔥 HOT RELOAD: validation-policy v2
   ⏸️  Before swap: validation-policy at 1234567890123
   ▶️  After swap: validation-policy v2 at 1234567890124
   ✅ Reloaded in 38.72ms
   🚀 ZERO-DOWNTIME: 38.72ms < 100ms target

✅ SUCCESS: Hot reload verified
   ✓ Reload timing < 100ms
   ✓ Callbacks executed
   ✓ New policy active
   ✓ Old policy no longer applies
```

## References

- [HotCodeLoader API](../api/hot-code-loader.md)
- [RDFValidator API](../api/rdf-validator.md)
- [SHACL Shapes](https://www.w3.org/TR/shacl/)
- [OTP Hot Code Loading](https://www.erlang.org/doc/reference_manual/code_loading.html)
