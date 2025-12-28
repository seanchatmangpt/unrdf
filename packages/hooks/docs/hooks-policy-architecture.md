## Hook & Policy Architecture

### Hook Execution Model

#### Hook Definition

Hooks are defined via the `defineHook()` function with the following structure:

```javascript
const hook = defineHook({
  name: 'string',           // Required: Hook identifier
  trigger: 'before-add',    // Required: Trigger event type
  validate: (quad) => boolean,    // Optional: Validation function
  transform: (quad) => quad,      // Optional: Transformation function
  metadata: { ... }         // Optional: Additional metadata
});
```

**Supported Trigger Types** (33 total):

- **Core CRUD** (6): `before-add`, `after-add`, `before-query`, `after-query`, `before-remove`, `after-remove`
- **Transaction** (4): `before-commit`, `after-commit`, `before-rollback`, `after-rollback`
- **Error/Event** (5): `on-error`, `on-validation-fail`, `on-transform`, `on-timeout`, `on-circuit-open`
- **Async/IO** (6): `before-fetch`, `after-fetch`, `before-sync`, `after-sync`, `before-import`, `after-import`
- **Cron/Time** (4): `on-schedule`, `on-interval`, `on-idle`, `on-startup`
- **Quality** (8): `quality-gate`, `defect-detection`, `continuous-improvement`, `spc-control`, `capability-analysis`, `root-cause`, `kaizen-event`, `audit-trail`

#### Hook Registration

Hooks are registered in a registry:

```javascript
import { createHookRegistry, registerHook } from '@unrdf/hooks';

const registry = createHookRegistry();
registerHook(registry, myHook);
```

#### Hook Execution

Hooks are triggered by events and executed sequentially:

```javascript
import { executeHooksByTrigger } from '@unrdf/hooks';

const result = executeHooksByTrigger(hooks, 'before-add', quad);
// result: { valid: boolean, quad: Quad, results: [], error?: string }
```

**Execution Flow**:

1. Filter hooks by trigger type
2. Execute validation hooks first (return boolean)
3. If validation passes, execute transformation hooks (return quad)
4. Chain transformations (output of hook N becomes input of hook N+1)
5. Stop on first validation failure (fail-fast)

#### Sandbox Execution

Hooks can execute in isolated sandboxes:

- **Sandbox Type**: Worker threads via `effect-sandbox.mjs` and `effect-sandbox-worker.mjs`
- **Isolation**: Separate JavaScript context per hook
- **Security**: Restricted module access, no file system by default
- **Performance**: Pooled workers for reduced overhead

### Policy Pack Structure

#### Manifest Format

Policy packs are defined in `manifest.json`:

```json
{
  "id": "uuid",
  "meta": {
    "name": "my-policy-pack",
    "version": "1.0.0",
    "description": "Access control policies",
    "author": "Organization",
    "license": "MIT",
    "tags": ["security", "access-control"],
    "ontology": ["http://example.org/ontology"],
    "dependencies": []
  },
  "config": {
    "enabled": true,
    "priority": 50,
    "strictMode": false,
    "timeout": 30000,
    "retries": 1,
    "conditions": {
      "environment": ["production"],
      "version": "1.0.0",
      "features": ["audit"]
    }
  },
  "hooks": [
    {
      "name": "acl-policy",
      "file": "acl-policy.mjs",
      "enabled": true,
      "priority": 100
    }
  ],
  "conditions": [
    {
      "name": "is-reviewer",
      "file": "is-reviewer.sparql",
      "type": "sparql-ask"
    }
  ],
  "resources": [
    {
      "name": "security-ontology",
      "file": "security.ttl",
      "type": "ontology"
    }
  ]
}
```

#### Loading Policy Packs

```javascript
import { PolicyPackManager } from '@unrdf/hooks';

const manager = new PolicyPackManager('/path/to/packs');

// Load single pack
const pack = await manager.loadPolicyPack('/path/to/manifest.json');

// Load all packs in directory
const packs = await manager.loadAllPolicyPacks();

// Activate pack
manager.activatePolicyPack('my-policy-pack');

// Get active hooks
const hooks = manager.getActiveHooks();
```

### Policy Evaluation Model

#### Condition Types

##### 1. SPARQL-ASK (Boolean Predicates)

```javascript
const condition = {
  kind: 'sparql-ask',
  ref: {
    uri: 'file://policies/is-admin.sparql',
    sha256: 'hash...',
  },
  // OR inline:
  query: `
    ASK {
      ?user a :Admin .
      ?user :hasRole :AdminRole .
    }
  `,
};
```

**Returns**: `boolean` - true if pattern matches, false otherwise

##### 2. SPARQL-SELECT (Data Retrieval)

```javascript
const condition = {
  kind: 'sparql-select',
  query: `
    SELECT ?role WHERE {
      ?user :hasRole ?role .
    }
  `,
};
```

**Returns**: `Array<{ [var]: RDFTerm }>` - bindings for each result

##### 3. SHACL (Shape Validation)

```javascript
const condition = {
  kind: 'shacl',
  ref: {
    uri: 'file://shapes/user-shape.ttl',
    sha256: 'hash...',
  },
};
```

**Returns**: `{ conforms: boolean, results: ValidationResult[] }`

##### 4. DELTA (Change Detection)

```javascript
const condition = {
  kind: 'delta',
  spec: {
    change: 'increase', // 'any', 'increase', 'decrease', 'modify'
    threshold: 0.1, // 10% change threshold
    baseline: 'ref://baseline-graph',
  },
};
```

##### 5. THRESHOLD (Numeric Conditions)

```javascript
const condition = {
  kind: 'threshold',
  spec: {
    var: 'count',
    op: '>', // '>', '>=', '<', '<=', '==', '!='
    value: 100,
    aggregate: 'sum', // 'sum', 'avg', 'min', 'max', 'count'
  },
};
```

##### 6. COUNT (Graph Size)

```javascript
const condition = {
  kind: 'count',
  spec: {
    op: '>=',
    value: 1000,
    query: 'SELECT ?s WHERE { ?s a :Entity }', // Optional
  },
};
```

##### 7. WINDOW (Sliding Window Aggregates)

```javascript
const condition = {
  kind: 'window',
  spec: {
    size: 100, // Window size
    slide: 50, // Slide amount
    aggregate: 'avg',
    query: 'SELECT ?val WHERE { ... }',
  },
};
```

#### Evaluating Conditions

```javascript
import { createConditionEvaluator } from '@unrdf/hooks';

const evaluator = createConditionEvaluator({
  basePath: process.cwd(),
  enableCache: true,
  cacheMaxAge: 60000, // 1 minute
  strictMode: false,
});

// Evaluate single condition
const result = await evaluator.evaluate(condition, graph, env);

// Check if satisfied (boolean)
const satisfied = await evaluator.isSatisfied(condition, graph, env);

// Evaluate multiple conditions in parallel
const results = await evaluator.evaluateAll(conditions, graph, env);
```

### Policy-Controlled Workflow

#### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Policy Engine                                               │
│  - registerPolicy(hookName, predicate)                      │
│  - isAllowed(hookName, context) → boolean                   │
│  - filterHooks(hooks, context) → { allowed, denied }        │
└─────────────────────────────────────────────────────────────┘
                          ▼
         ┌────────────────────────────────┐
         │ Policy Evaluation              │
         │  - Actor-based (RBAC)          │
         │  - Time-based                  │
         │  - Resource-based (ABAC)       │
         │  - Graph state predicates      │
         └────────────────────────────────┘
                          ▼
    ┌───────────────┬────────────────┐
    │ Allowed Hooks │ Denied Hooks   │
    └───────────────┴────────────────┘
            ▼                  ▼
    executeHookChain      log denial
```

#### Implementation Example

See: `/home/user/unrdf/packages/hooks/proofs/policy-controlled-workflow-standalone.mjs`

**Key Features**:

1. **Fail-Closed Security**: Policy evaluation errors result in denial
2. **Audit Trail**: All policy decisions logged with timestamp, actor, decision
3. **Separation of Concerns**: Policy evaluation separate from hook execution
4. **Flexibility**: Predicates are pure functions (context) => boolean

### Proof: Policy-Controlled Workflow

**Command**:

```bash
node /home/user/unrdf/packages/hooks/proofs/policy-controlled-workflow-standalone.mjs
```

**Output**:

```
======================================================================
POLICY-CONTROLLED WORKFLOW PROOF
======================================================================

TEST 1: actor="user" (unauthorized)
----------------------------------------------------------------------
Hooks allowed: audit-logger
Hooks denied:  data-mutator

[AUDIT] Logging quad: http://example.org/alice
Execution result: SUCCESS
Final quad value: Alice Smith

Expected: audit-logger executes, data-mutator blocked
Actual:   PASS ✅

======================================================================
TEST 2: actor="reviewer" (authorized)
----------------------------------------------------------------------
Hooks allowed: audit-logger, data-mutator
Hooks denied:

[AUDIT] Logging quad: http://example.org/alice
Execution result: SUCCESS
Final quad value: ALICE SMITH

Expected: both hooks execute, value transformed to uppercase
Actual:   PASS ✅

======================================================================
AUDIT LOG
======================================================================

[1] audit-logger
    Actor: user
    Decision: allow (no policy)
[2] data-mutator
    Actor: user
    Decision: deny
[3] audit-logger
    Actor: reviewer
    Decision: allow (no policy)
[4] data-mutator
    Actor: reviewer
    Decision: allow

======================================================================
PROOF SUMMARY
======================================================================

Policy Engine demonstrates:
  1. Ungated hooks always execute (audit-logger)
  2. Policy-gated hooks require actor authorization
  3. Fail-closed security (policy error = deny)
  4. Clean separation: policy evaluation ≠ hook execution
  5. Full audit trail of policy decisions

Architecture:
  - PolicyEngine.registerPolicy(hookName, predicate)
  - PolicyEngine.isAllowed(hookName, context) → boolean
  - PolicyEngine.filterHooks(hooks, context) → { allowed, denied }
  - PolicyEngine.getAuditLog() → audit trail

✅ Policy-controlled workflow PROVEN
```

### Error Handling

#### Policy Evaluation Errors

**Strict Mode (strictMode: true)**:

- Throws error on policy evaluation failure
- Use for critical security policies

**Non-Strict Mode (strictMode: false)**:

- Returns safe defaults (false for ASK, [] for SELECT, {conforms: false} for SHACL)
- Logs warning
- Use for non-critical policies

#### Hook Execution Errors

**Validation Failures**:

```javascript
{
  valid: false,
  error: "Validation failed for hook: hook-name",
  hookName: "hook-name"
}
```

**Transformation Errors**:

- TypeError if transform doesn't return Quad
- Caught and returned in result.error
- Full stack trace in result.errorDetails

### Performance Notes

#### Hook Execution Latency

- **Cold start**: ~100μs (Zod validation overhead)
- **Hot path**: <1μs (pre-validated hooks, skip Zod)
- **Batch operations**: Sub-1μs per quad via `executeBatch()`

#### Policy Evaluation Cost

- **SPARQL-ASK**: ~1-10ms (depends on query complexity)
- **SPARQL-SELECT**: ~5-50ms (depends on result size)
- **SHACL**: ~10-100ms (depends on shape complexity)
- **Cache hit**: <1ms (in-memory lookup)

#### Optimization Strategies

1. **Pre-validate hooks** at startup: `prewarmHookCache(hooks)`
2. **Enable condition caching**: `enableCache: true, cacheMaxAge: 60000`
3. **Use compiled chains**: `compileHookChain(hooks)` for JIT optimization
4. **Batch operations**: `executeBatch()` for bulk quad processing
5. **Object pooling**: Use `QuadPool` for zero-allocation transforms

### Integration Patterns

#### Pattern 1: Pre-Hook Policy Gate

```javascript
// Evaluate policy before hook execution
const satisfied = await policyEngine.isSatisfied(condition, graph, { actor });
if (satisfied) {
  const result = executeHook(hook, quad);
}
```

#### Pattern 2: Hook Metadata Policy

```javascript
// Policy stored in hook metadata
const hook = defineHook({
  name: 'sensitive-op',
  trigger: 'before-add',
  transform: quad => quad,
  metadata: {
    policy: 'requires-admin',
    minRole: 'admin',
  },
});

// Engine checks metadata
const requiresAdmin = hook.metadata.policy === 'requires-admin';
if (requiresAdmin && context.role !== 'admin') {
  throw new Error('Unauthorized');
}
```

#### Pattern 3: Condition-Based Hook Activation

```javascript
// Hook only activates if condition satisfied
const hook = defineHook({
  name: 'conditional-hook',
  trigger: 'before-add',
  transform: async quad => {
    const shouldRun = await evaluator.isSatisfied(condition, graph);
    if (!shouldRun) return quad;

    // Transform logic
    return transformedQuad;
  },
});
```

### Real-World Use Cases

#### 1. RBAC (Role-Based Access Control)

```javascript
policyEngine.registerPolicy('delete-entity', context => {
  return context.roles.includes('admin') || context.roles.includes('moderator');
});
```

#### 2. Time-Based Access

```javascript
policyEngine.registerPolicy('after-hours-mutation', context => {
  const hour = new Date().getHours();
  return hour >= 9 && hour <= 17; // Business hours only
});
```

#### 3. Resource Quotas

```javascript
policyEngine.registerPolicy('quota-check', async context => {
  const usage = await getResourceUsage(context.user);
  return usage < context.user.quota;
});
```

#### 4. Data Classification

```javascript
policyEngine.registerPolicy('pii-access', context => {
  return context.clearanceLevel >= 3 && context.training.includes('pii-handling');
});
```

### Testing Strategies

#### Unit Tests

```javascript
import { PolicyEngine } from './policy-controlled-workflow-standalone.mjs';

test('policy allows authorized actor', () => {
  const engine = new PolicyEngine();
  engine.registerPolicy('restricted-hook', ctx => ctx.actor === 'admin');

  expect(engine.isAllowed('restricted-hook', { actor: 'admin' })).toBe(true);
  expect(engine.isAllowed('restricted-hook', { actor: 'user' })).toBe(false);
});
```

#### Integration Tests

```javascript
test('policy-controlled workflow end-to-end', async () => {
  const hooks = [auditHook, restrictedHook];
  const { allowed, denied } = policyEngine.filterHooks(hooks, { actor: 'user' });

  expect(allowed).toHaveLength(1);
  expect(denied).toHaveLength(1);

  const result = executeHookChain(allowed, quad);
  expect(result.valid).toBe(true);
});
```

### Future Enhancements

1. **Policy DSL**: Declarative policy language (e.g., Rego, Cedar)
2. **Policy Versioning**: Semantic versioning for policy packs
3. **Policy Composition**: AND/OR/NOT composition of policies
4. **Dynamic Policy Loading**: Hot-reload policies without restart
5. **Policy Analytics**: Track policy decision metrics (allow rate, deny rate, latency)
6. **Conflict Resolution**: Handle multiple policies for same hook (precedence rules)

### References

- Hook Definition: `/home/user/unrdf/packages/hooks/src/hooks/define-hook.mjs`
- Hook Executor: `/home/user/unrdf/packages/hooks/src/hooks/hook-executor.mjs`
- Condition Evaluator: `/home/user/unrdf/packages/hooks/src/hooks/condition-evaluator.mjs`
- Policy Pack Manager: `/home/user/unrdf/packages/hooks/src/hooks/policy-pack.mjs`
- Proof Implementation: `/home/user/unrdf/packages/hooks/proofs/policy-controlled-workflow-standalone.mjs`
- Examples: `/home/user/unrdf/packages/hooks/examples/policy-hooks/`
