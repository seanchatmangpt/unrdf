# @unrdf/hooks Examples

Complete working examples for all 9 condition kinds, effects, SHACL enforcement modes, receipt chaining, CLI, FIBO, and AtomVM integration.

## Condition Kind Examples

### 1. SPARQL ASK (Boolean Query)

```javascript
import { KnowledgeHookEngine, createKnowledgeHook } from '@unrdf/hooks';
import { createStore } from '@unrdf/oxigraph';
import { createContext } from '@unrdf/v6-core/receipt-pattern';

const store = createStore();
const engine = new KnowledgeHookEngine(store);

// Add sample data
store.insert(
  [namedNode('ex:alice'), namedNode('a'), namedNode('ex:Person')],
  namedNode('ex:default')
);

const hook = createKnowledgeHook({
  name: 'check-any-person',
  condition: {
    kind: 'sparql-ask',
    query: 'ASK { ?s a ex:Person }'
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?s ex:verified true .
      }
      WHERE {
        ?s a ex:Person .
      }
    `
  }]
});

const ctx = createContext({ nodeId: 'test-app', t_ns: BigInt(Date.now() * 1000000) });
const result = await engine.execute(ctx, [hook]);

console.log('Condition matched:', result.successful > 0);
console.log('Receipt:', result.receipt.receiptHash);
```

**When to Use**: Quick boolean checks without needing result bindings.

---

### 2. SPARQL SELECT (Result Set)

```javascript
import { namedNode, literal } from '@unrdf/oxigraph';

const hook = createKnowledgeHook({
  name: 'find-active-trades',
  condition: {
    kind: 'sparql-select',
    query: `
      SELECT ?trade WHERE {
        ?trade a ex:Trade ;
               ex:status ex:Active .
      }
    `
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?trade ex:lastChecked ?now .
      }
      WHERE {
        ?trade a ex:Trade ;
               ex:status ex:Active .
        BIND (NOW() as ?now)
      }
    `
  }]
});

const result = await engine.execute(ctx, [hook]);
console.log('Active trades found:', result.successful > 0);
```

**When to Use**: When condition result bindings matter for downstream logic.

---

### 3. SHACL Validation - Block Mode

```javascript
// Strict enforcement: fail if shape violations exist
const shaclBlockHook = createKnowledgeHook({
  name: 'strict-person-validation',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///shapes/person-shape.ttl' },
    enforcementMode: 'block'  // Fail on violations
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?s ex:conforms true .
      }
      WHERE {
        ?s a ex:Person .
      }
    `
  }]
});

try {
  const result = await engine.execute(ctx, [shaclBlockHook]);
  console.log('Shape valid, effects applied');
} catch (err) {
  console.log('Shape violations:', err.message);
}
```

**SHACL Shape** (`person-shape.ttl`):

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.com/> .

ex:PersonShape
  a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path ex:name ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string ;
  ] .
```

**When to Use**: Regulatory compliance, immutable audit trails, strict governance.

---

### 4. SHACL Validation - Annotate Mode

```javascript
// Soft-fail: execute but add violations as RDF triples
const shaclAnnotateHook = createKnowledgeHook({
  name: 'audit-person-validation',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///shapes/person-shape.ttl' },
    enforcementMode: 'annotate'  // Log violations, don't block
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?s ex:auditedAt ?now .
      }
      WHERE {
        ?s a ex:Person .
        BIND (NOW() as ?now)
      }
    `
  }]
});

const result = await engine.execute(ctx, [shaclAnnotateHook]);
// result.violations contains SHACL report as RDF quads
console.log('Violations as RDF:', result.violations);
// Store contains violations as RDF triples (ex:ValidationResult quads)
```

**Result Structure**:

```javascript
{
  successful: 1,
  violations: [
    {
      focusNode: 'ex:bob',
      resultPath: 'ex:name',
      resultMessage: 'Minimum count violation',
      severity: 'sh:Violation'
    }
  ]
}
```

**When to Use**: Risk management, warning systems, audit trails with soft-fail.

---

### 5. SHACL Validation - Repair Mode

```javascript
// Self-healing: auto-fix violations via SPARQL CONSTRUCT
const shaclRepairHook = createKnowledgeHook({
  name: 'auto-repair-person',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///shapes/person-shape.ttl' },
    enforcementMode: 'repair',
    repairConstruct: `
      CONSTRUCT {
        ?person ex:name 'Unknown' ;
                ex:repaired true ;
                ex:repairedAt ?now .
      }
      WHERE {
        ?person a ex:Person .
        FILTER NOT EXISTS { ?person ex:name ?name }
        BIND (NOW() as ?now)
      }
    `
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?s ex:repairComplete true .
      }
      WHERE {
        ?s ex:repaired true .
      }
    `
  }]
});

const result = await engine.execute(ctx, [shaclRepairHook]);
console.log('Auto-repairs applied:', result.repairs);
```

**When to Use**: Data quality improvement, auto-remediation, self-healing systems.

---

### 6. Delta (Change Detection)

```javascript
const deltaHook = createKnowledgeHook({
  name: 'react-to-new-trades',
  condition: {
    kind: 'delta',
    adds: [
      {
        subject: '?trade',
        predicate: 'rdf:type',
        object: 'ex:Trade'
      }
    ],
    deletes: []
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?trade ex:createdAt ?now ;
               ex:status ex:Active .
      }
      WHERE {
        ?trade a ex:Trade .
        BIND (NOW() as ?now)
      }
    `
  }]
});

// Trigger only if new Trade instances were added
const result = await engine.execute(ctx, [deltaHook]);
console.log('Delta matched:', result.deltaMatched);
```

**When to Use**: Reactive governance, change-driven triggers, event sourcing.

---

### 7. Threshold (Numeric Comparison)

```javascript
const thresholdHook = createKnowledgeHook({
  name: 'check-trade-volume',
  condition: {
    kind: 'threshold',
    query: `
      SELECT (COUNT(?trade) as ?count) WHERE {
        ?trade a ex:Trade ;
               ex:status ex:Active .
      }
    `,
    operator: 'greaterThan',
    value: 100
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?trade ex:requiresReview true .
      }
      WHERE {
        ?trade a ex:Trade ;
               ex:status ex:Active .
      }
    `
  }]
});

const result = await engine.execute(ctx, [thresholdHook]);
console.log('Volume exceeds threshold:', result.successful > 0);
```

**Operators**: `greaterThan`, `lessThan`, `equal`, `greaterThanOrEqual`, `lessThanOrEqual`

**When to Use**: Quantitative governance, risk limits, capacity management.

---

### 8. Count (Pattern Aggregation)

```javascript
const countHook = createKnowledgeHook({
  name: 'enforce-active-trade-limit',
  condition: {
    kind: 'count',
    pattern: {
      subject: '?trade',
      predicate: 'ex:status',
      object: 'ex:Active'
    },
    expected: 50  // Fail if not exactly 50 active trades
  },
  effects: []
});

const result = await engine.execute(ctx, [countHook]);
console.log('Active trade count matches expected:', result.successful > 0);
```

**When to Use**: Cardinality enforcement, batch validation.

---

### 9. Window (Time Range Evaluation)

```javascript
const windowHook = createKnowledgeHook({
  name: 'rate-limit-trades',
  condition: {
    kind: 'window',
    windowMs: 60000,  // 1 minute window
    maxMatches: 10,   // Max 10 trades per minute
    query: `
      SELECT ?timestamp WHERE {
        ?trade a ex:Trade ;
               ex:createdAt ?timestamp .
      }
    `
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?trade ex:rateLimited true .
      }
      WHERE {
        ?trade a ex:Trade .
        BIND (NOW() as ?now)
      }
    `
  }]
});

const result = await engine.execute(ctx, [windowHook]);
console.log('Within rate limit:', result.successful > 0);
```

**When to Use**: Rate limiting, temporal constraints, throughput control.

---

### 10. N3 Forward-Chaining Inference ⭐

```javascript
const n3Hook = createKnowledgeHook({
  name: 'apply-compliance-rules',
  condition: {
    kind: 'n3',
    rules: `
      # If restricted class, mark as requiring approval
      { ?x a :RestrictedClass } => { ?x :requiresApproval true } .
      
      # If high-risk score, mark for review
      { ?x :riskScore ?score . ?score > 50 } => { ?x :requiresReview true } .
      
      # If approved and high-value, require additional documentation
      { ?x :approved true ; :value ?val . ?val > 1000000 } => { 
        ?x :requiresDocumentation true 
      } .
    `,
    askQuery: 'ASK { ?s :requiresApproval true }'
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?s ex:rulesApplied true ;
           ex:appliedAt ?now .
      }
      WHERE {
        ?s a ?type .
        BIND (NOW() as ?now)
      }
    `
  }]
});

const result = await engine.execute(ctx, [n3Hook]);
console.log('N3 rules matched approval requirement:', result.successful > 0);
```

**When to Use**: Declarative inference without imperative code, complex rule evaluation.

---

### 11. Datalog Logic Programming ⭐

```javascript
const datalogHook = createKnowledgeHook({
  name: 'evaluate-access-control',
  condition: {
    kind: 'datalog',
    facts: [
      'user(alice)',
      'user(bob)',
      'admin(alice)',
      'group(admins)',
      'member(alice, admins)',
      'role(editor)',
      'canEdit(alice)',
      'resource(document1)',
      'owner(alice, document1)'
    ],
    rules: [
      // Admin users can edit anything
      'allowed(X, edit) :- admin(X)',
      // Users in admin group can edit
      'allowed(X, edit) :- member(X, admins)',
      // Editors can edit
      'allowed(X, edit) :- role(X)',
      // Owners can edit their resources
      'allowed(X, edit) :- owner(X, R)',
      // Transitive: admins are super-users
      'superuser(X) :- admin(X)',
      'superuser(X) :- member(X, admins), admin(Y), member(X, G), member(Y, G)'
    ],
    goal: 'allowed(alice, edit)'  // Check if alice can edit
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?user ex:accessGranted true ;
              ex:grantedAt ?now .
      }
      WHERE {
        ?user a ex:User .
        BIND (NOW() as ?now)
      }
    `
  }]
});

const result = await engine.execute(ctx, [datalogHook]);
console.log('Access allowed via Datalog:', result.successful > 0);
```

**Use Case**: Access control lists, permission evaluation, constraint satisfaction.

---

## Effect Examples

### SPARQL CONSTRUCT Effect

```javascript
const constructEffect = {
  kind: 'sparql-construct',
  query: `
    CONSTRUCT {
      ?trade ex:processed true ;
             ex:processedAt ?now ;
             ex:processor ?nodeId .
    }
    WHERE {
      ?trade a ex:Trade ;
             ex:status ex:Pending .
      BIND (NOW() as ?now)
      BIND ('my-node' as ?nodeId)
    }
  `
};

// Use in hook
const hook = createKnowledgeHook({
  name: 'process-trades',
  condition: { kind: 'sparql-ask', query: 'ASK { ?s a ex:Trade }' },
  effects: [constructEffect]
});
```

**Advantages**:
- Pure RDF transformation
- No JavaScript execution risk
- Deterministic output
- Composable with other SPARQL operations

### Function Effect (Legacy)

```javascript
const functionEffect = {
  kind: 'function',
  inline: async (store, quad) => {
    // Custom transformation logic
    console.log('Processing quad:', quad);
    return { 
      success: true,
      modified: 1
    };
  },
  timeout: 30000,
  retries: 1,
  sandbox: false
};
```

---

## Receipt Chaining Example

```javascript
import { createContext } from '@unrdf/v6-core/receipt-pattern';

const store = createStore();
const engine = new KnowledgeHookEngine(store);

// Execute first batch of hooks
const ctx1 = createContext({
  nodeId: 'my-app',
  t_ns: BigInt(Date.now() * 1000000)
});

const hook1 = createKnowledgeHook({
  name: 'first-batch',
  condition: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?s ex:batch ex:one .
      }
      WHERE {
        ?s ?p ?o .
      }
    `
  }]
});

const result1 = await engine.execute(ctx1, [hook1]);
console.log('Receipt 1:', result1.receipt.receiptHash);

// Chain to next batch using previous receipt hash
const ctx2 = createContext({
  nodeId: 'my-app',
  t_ns: BigInt(Date.now() * 1000000),
  previousReceiptHash: result1.receipt.receiptHash  // Link to previous
});

const hook2 = createKnowledgeHook({
  name: 'second-batch',
  condition: { kind: 'sparql-ask', query: 'ASK { ?s ex:batch ex:one }' },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?s ex:batch ex:two .
      }
      WHERE {
        ?s ex:batch ex:one .
      }
    `
  }]
});

const result2 = await engine.execute(ctx2, [hook2]);

// Verify chain
console.log('Receipt 1:', result1.receipt.receiptHash);
console.log('Receipt 2:', result2.receipt.receiptHash);
console.log('Previous Hash Match:', 
  result2.receipt.previousReceiptHash === result1.receipt.receiptHash
);

// Get full chain
const chain = engine.getReceiptChain();
console.log('Chain length:', chain.length);
console.log('Chain verified:', chain.every((r, i) => {
  if (i === 0) return true;
  return r.previousReceiptHash === chain[i-1].receiptHash;
}));
```

**Output**:

```
Receipt 1: a3f7d9e2c8f4b6a1e9c7d5f3a1b9e8c6
Receipt 2: b9e2c4d1f7a3e8c9b5d3f1a7e9c6b4a2
Previous Hash Match: true
Chain length: 2
Chain verified: true
```

---

## CLI Examples

### 1. List All Condition Kinds

```bash
unrdf hooks list-conditions

# Output:
# 1. sparql-ask - Boolean SPARQL queries
# 2. sparql-select - SPARQL SELECT with result binding
# 3. shacl - Shape validation (block/annotate/repair)
# 4. delta - Change detection (adds/deletes)
# 5. threshold - Numeric comparison
# 6. count - Pattern aggregation
# 7. window - Time range evaluation
# 8. n3 - Forward-chaining inference
# 9. datalog - Logic programming
```

### 2. Execute Hooks with Receipt Chain

```bash
unrdf hooks execute \
  --store data.nq \
  --config hooks.json \
  --show-receipts \
  --output results.json

# Config file (hooks.json):
# [
#   {
#     "name": "validate-trades",
#     "condition": { "kind": "sparql-ask", "query": "ASK { ?s a ex:Trade }" },
#     "effects": [{ "kind": "sparql-construct", "query": "..." }]
#   }
# ]
```

### 3. Validate Hook Configuration

```bash
unrdf hooks define --config hooks.json --validate

# Output:
# ✅ hooks.json is valid
# - 3 hooks defined
# - 5 conditions validated
# - All schemas conform to Zod
```

### 4. Test Single Condition

```bash
unrdf hooks evaluate-condition \
  --store data.nq \
  --condition n3 \
  --config '{
    "rules": "{ ?x a :Person } => { ?x :verified true } .",
    "askQuery": "ASK { ?s :verified true }"
  }'

# Output:
# Condition result: true
# Rules applied: 2
# Inference time: 23ms
```

### 5. Display and Verify Receipt Chain

```bash
unrdf hooks receipts --file results.json --verify

# Output:
# Receipt Chain Verification
# ========================
# Total receipts: 3
# Chain valid: ✅ true
# 
# Receipt 1: a3f7... (timestamp: 2026-04-03T12:00:00Z)
# Receipt 2: b9e2... (previous: a3f7... ✅)
# Receipt 3: c4d1... (previous: b9e2... ✅)
```

### 6. Generate Hooks from FIBO Template

```bash
unrdf hooks template --type fibo --output fibo-hooks.json

# Generates:
# - SHACL shapes for FIBO compliance
# - N3 rules for regulatory inference
# - Datalog facts for counterparty assessment
```

---

## FIBO Case Study

Financial regulatory compliance using all 6 priorities.

**File**: `examples/fibo-jtbd-governance.mjs`

```javascript
import { KnowledgeHookEngine, createKnowledgeHook } from '@unrdf/hooks';
import { createStore } from '@unrdf/oxigraph';
import { createContext } from '@unrdf/v6-core/receipt-pattern';

const store = createStore();
const engine = new KnowledgeHookEngine(store);

// JTBD 1: Verify Regulatory Compliance
const complianceHook = createKnowledgeHook({
  name: 'fibo-compliance-check',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///fibo/shapes/regulatory-compliance.ttl' },
    enforcementMode: 'block'  // Strict: fail on violations
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?trade fibo:compliantWithRegulation true ;
               fibo:complianceCheckDate ?now .
      }
      WHERE {
        ?trade a fibo:Trade .
        BIND (NOW() as ?now)
      }
    `
  }]
});

// JTBD 2: Assess Counterparty Risk (N3 Rules)
const riskAssessmentHook = createKnowledgeHook({
  name: 'fibo-counterparty-risk',
  condition: {
    kind: 'n3',
    rules: `
      # High credit rating = low risk
      { ?party fibo:creditRating 'AAA' } => { ?party fibo:riskLevel 'Low' } .
      { ?party fibo:creditRating 'AA' } => { ?party fibo:riskLevel 'Low' } .
      { ?party fibo:creditRating 'A' } => { ?party fibo:riskLevel 'Medium' } .
      
      # Recent defaults = high risk
      { ?party fibo:defaultHistory ?defaults . ?defaults > 0 } => { 
        ?party fibo:riskLevel 'High' 
      } .
      
      # Large exposures amplify risk
      { ?party fibo:riskLevel ?level ; fibo:exposure ?exp . 
        ?exp > 1000000 } => { 
        ?party fibo:requiresEscalation true 
      } .
    `,
    askQuery: 'ASK { ?s fibo:riskLevel ?level }'
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?party fibo:riskAssessmentDate ?now .
      }
      WHERE {
        ?party a fibo:Counterparty .
        BIND (NOW() as ?now)
      }
    `
  }]
});

// JTBD 3: Manage Liquidity Positions (Datalog Logic)
const liquidityHook = createKnowledgeHook({
  name: 'fibo-liquidity-management',
  condition: {
    kind: 'datalog',
    facts: [
      'position(trade1, 10000000)',  // Trade 1: 10M exposure
      'position(trade2, 5000000)',   // Trade 2: 5M exposure
      'position(trade3, 2000000)',   // Trade 3: 2M exposure
      'limit(portfolio, 50000000)',  // Portfolio limit
      'cash_available(25000000)'     // Available cash
    ],
    rules: [
      'total_exposure(X) :- position(_, X)',
      'within_limit(portfolio) :- total_exposure(X), limit(portfolio, L), X =< L',
      'adequate_liquidity :- cash_available(C), total_exposure(E), C >= E * 0.2'
    ],
    goal: 'within_limit(portfolio)'
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?portfolio fibo:liquidityStatus fibo:Adequate ;
                   fibo:liquidityCheckDate ?now .
      }
      WHERE {
        ?portfolio a fibo:Portfolio .
        BIND (NOW() as ?now)
      }
    `
  }]
});

// JTBD 4: Maintain Audit Trail (Receipt Chaining)
const ctx = createContext({
  nodeId: 'fibo-compliance-engine',
  t_ns: BigInt(Date.now() * 1000000)
});

const result1 = await engine.execute(ctx, [complianceHook]);
console.log('Compliance receipt:', result1.receipt.receiptHash);

// JTBD 5: Auto-Repair Violations (SHACL Repair Mode)
const repairHook = createKnowledgeHook({
  name: 'fibo-auto-repair',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///fibo/shapes/auto-repair.ttl' },
    enforcementMode: 'repair',
    repairConstruct: `
      CONSTRUCT {
        ?trade fibo:repaired true ;
               fibo:repairedAt ?now ;
               fibo:status fibo:Corrected .
      }
      WHERE {
        ?trade a fibo:Trade ;
               fibo:status fibo:Malformed .
        BIND (NOW() as ?now)
      }
    `
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?trade fibo:repairComplete true .
      }
      WHERE {
        ?trade fibo:repaired true .
      }
    `
  }]
});

// Full workflow with chaining
const ctx2 = createContext({
  ...ctx,
  previousReceiptHash: result1.receipt.receiptHash
});

const result2 = await engine.execute(ctx2, [
  riskAssessmentHook,
  liquidityHook,
  repairHook
]);

console.log('All JTBD completed');
console.log('Receipt chain:', [
  result1.receipt.receiptHash,
  result2.receipt.receiptHash
]);
```

Run:

```bash
node examples/fibo-jtbd-governance.mjs
```

---

## AtomVM Integration Example

**File**: `examples/atomvm-fibo-hooks-demo.mjs`

```javascript
import { HooksBridge } from '@unrdf/hooks/atomvm';
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
const bridge = new HooksBridge(store);

// From Erlang process: Register hook
const hookId = await bridge.registerHook({
  name: 'erlang-trade-validation',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///fibo/shapes/trade.ttl' },
    enforcementMode: 'annotate'
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?trade fibo:erl_validated true ;
               fibo:erl_validator ?validator .
      }
      WHERE {
        ?trade a fibo:Trade .
        BIND ('erlang_process' as ?validator)
      }
    `
  }]
});

console.log('Registered hook:', hookId);

// From Erlang process: Evaluate condition
const condition = {
  kind: 'datalog',
  facts: [
    'trade(t1)',
    'counterparty(cp1)',
    'rating(cp1, AAA)',
    'approved(cp1)'
  ],
  rules: [
    'eligible(X) :- approved(X), rating(X, AAA)',
    'can_trade(T) :- trade(T), counterparty(C), eligible(C)'
  ],
  goal: 'can_trade(t1)'
};

const conditionResult = await bridge.evaluateCondition(condition);
console.log('Condition satisfied:', conditionResult);

// Full workflow with receipt chaining
const ctx = {
  nodeId: 'erlang-vm',
  t_ns: BigInt(Date.now() * 1000000)
};

const hook = await bridge.getHook(hookId);
const result = await bridge.executeHooks(ctx, [hook]);

console.log('Execution result:');
console.log('  Successful:', result.successful);
console.log('  Receipt:', result.receipt.receiptHash);
console.log('  Previous receipt:', result.receipt.previousReceiptHash);
```

---

## Summary Table

| Condition Kind | Use Case | Complexity | Latency |
|---|---|---|---|
| sparql-ask | Boolean checks | Low | 1-5ms |
| sparql-select | Result binding | Low | 2-8ms |
| shacl | Shape validation | Medium | 5-15ms |
| delta | Change detection | Low | <1ms |
| threshold | Numeric limits | Low | 2-8ms |
| count | Cardinality | Low | 1-5ms |
| window | Time ranges | Medium | 5-20ms |
| n3 | Rule inference | High | 10-100ms |
| datalog | Logic programming | High | 1-30ms |

| Effect Type | Safety | Performance | Use Case |
|---|---|---|---|
| sparql-construct | High | Fast | RDF transformation |
| function | Medium | Variable | Custom logic (legacy) |

| SHACL Mode | Governance | Compliance | Self-Healing |
|---|---|---|---|
| block | Strict | High | Manual |
| annotate | Soft-fail | Medium | Via logging |
| repair | Self-healing | High | Automatic |
