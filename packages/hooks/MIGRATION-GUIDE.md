# Migration Guide - UNRDF Hooks v26.4.5

**Current Version:** v26.4.4  
**Target Version:** v26.4.5+  
**Breaking Changes:** NONE (backward compatible)  
**New Features:** BLAKE3 hashing, SHACL enforcement modes, Window condition, Delta decrease detection

---

## Quick Start

If you're upgrading from v26.4.0-v26.4.4:

```bash
npm install @unrdf/hooks@26.4.5
# No code changes required — all APIs remain the same
# But read the sections below for new capabilities
```

---

## What's New in v26.4.5

### 1. BLAKE3 Cryptographic Hashing (Security Fix)

**Status:** AUTOMATIC - No code changes needed

**Before (v26.4.0-v26.4.3):**

```javascript
// Receipts used weak hashing
const receipt = await engine.execute(hooks, store);
// receipt.receiptHash was non-cryptographic
// ❌ Could not verify audit trail integrity
```

**After (v26.4.5+):**

```javascript
// Receipts now use BLAKE3 (256-bit cryptographic hash)
const receipt = await engine.execute(hooks, store);
// receipt.receiptHash uses BLAKE3 algorithm
// ✅ Audit trail integrity is verified
// ✅ Receipts can be cryptographically verified
```

**Migration:**

- No code changes required
- Old receipts from v26.4.0-v26.4.3 should be marked as legacy
- New receipts will automatically use BLAKE3
- See [SECURITY-DISCLOSURE.md](SECURITY-DISCLOSURE.md) for detailed security migration

---

### 2. SHACL Repair Mode - Now Fully Functional

**Status:** Major feature fix

**Before (v26.4.3 and earlier):**

```javascript
// Repair mode was a stub—it didn't actually repair
const hook = defineHook({
  id: 'fix-invalid-data',
  condition: {
    kind: 'shacl',
    ref: { uri: 'shapes.ttl' },
    enforcementMode: 'repair',
    repairConstruct: `
      CONSTRUCT { ?s ?p ?o }
      WHERE { ?s ?p ?o . FILTER(!ISBLANK(?o)) }
    `,
  },
  run: async store => {
    // Repair mode silently failed—nothing happened
    console.log('Repair complete'); // ❌ But nothing was actually repaired
  },
});
```

**After (v26.4.5+):**

```javascript
// Repair mode actually applies repairs and re-validates
const hook = defineHook({
  id: 'fix-invalid-data',
  condition: {
    kind: 'shacl',
    ref: { uri: 'shapes.ttl' },
    enforcementMode: 'repair',
    repairConstruct: `
      CONSTRUCT { ?s ?p ?o }
      WHERE { ?s ?p ?o . FILTER(!ISBLANK(?o)) }
    `,
  },
  run: async store => {
    // Repair mode now:
    // 1. Evaluates SHACL shapes
    // 2. Executes repairConstruct if invalid
    // 3. Applies repair quads to store
    // 4. Re-validates with updated graph
    // 5. Returns boolean success/failure
    console.log('Repair complete'); // ✅ Repairs actually applied
  },
});
```

**Migration Checklist:**

- [ ] Review all hooks using `enforcementMode: 'repair'`
- [ ] Test repair code paths (they now work for real)
- [ ] Verify `repairConstruct` queries are correct
- [ ] Update unit tests to check actual repairs
- [ ] No code changes needed—just verify it works

**Expected Behavior After Migration:**

```javascript
// Before: Repair hook would silently fail
const result1 = await engine.execute([repairHook], store);
// result1.successful === true (but nothing actually repaired)

// After: Repair hook actually repairs
const result2 = await engine.execute([repairHook], store);
// result2.successful === true && repairs applied
// Store contents are actually modified if validation failed
```

---

### 3. SHACL Annotate Mode - Now Generates Valid RDF

**Status:** Major feature fix

**Before (v26.4.3 and earlier):**

```javascript
// Annotate mode generated invalid RDF
const hook = defineHook({
  id: 'annotate-violations',
  condition: {
    kind: 'shacl',
    ref: { uri: 'shapes.ttl' },
    enforcementMode: 'annotate', // ❌ Generated invalid RDF
  },
  run: async store => {
    // Annotate mode would add SHACL violation quads
    // But the RDF structure was malformed and couldn't be stored
  },
});

const result = await engine.execute([hook], store);
// Error: Invalid RDF in SHACL report (couldn't be stored)
```

**After (v26.4.5+):**

```javascript
// Annotate mode generates valid RDF that can be stored
const hook = defineHook({
  id: 'annotate-violations',
  condition: {
    kind: 'shacl',
    ref: { uri: 'shapes.ttl' },
    enforcementMode: 'annotate', // ✅ Generates valid RDF
  },
  run: async store => {
    // Annotate mode now:
    // 1. Evaluates SHACL shapes
    // 2. Serializes violations as RDF triples
    // 3. Adds violation quads to store
    // 4. Returns true (allows write with annotation)
  },
});

const result = await engine.execute([hook], store);
// result.successful === true
// Violation quads added to store
// RDF is valid and can be queried
```

**Migration Checklist:**

- [ ] Review all hooks using `enforcementMode: 'annotate'`
- [ ] Test that violations are stored as valid RDF
- [ ] Query stored violations with SPARQL
- [ ] Update monitoring to track violation quads
- [ ] No code changes needed—just verify stored RDF is valid

**Querying Stored Violations:**

```javascript
// New capability: Query stored SHACL violation RDF
const query = `
  PREFIX sh: <http://www.w3.org/ns/shacl#>
  SELECT ?result ?focusNode ?severity WHERE {
    ?result sh:focusNode ?focusNode ;
            sh:resultSeverity ?severity .
  }
`;

const violations = await core.query(store, query);
for (const violation of violations) {
  console.log(`Node ${violation.focusNode} has ${violation.severity}`);
}
```

---

### 4. Delta Decrease Condition - Now Fully Functional

**Status:** Major feature fix

**Before (v26.4.3 and earlier):**

```javascript
// Decrease detection never worked
const hook = defineHook({
  id: 'monitor-data-loss',
  condition: {
    kind: 'delta',
    property: 'triples',
    direction: 'decrease', // ❌ Never actually detected decreases
    threshold: 10,
  },
  run: async store => {
    console.log('Data loss detected!');
  },
});

// Even if triples decreased, condition would return false
const result = await engine.execute([hook], store);
// ❌ Decrease never triggered
```

**After (v26.4.5+):**

```javascript
// Decrease detection now works correctly
const hook = defineHook({
  id: 'monitor-data-loss',
  condition: {
    kind: 'delta',
    property: 'triples',
    direction: 'decrease', // ✅ Now detects decreases
    threshold: 10,
  },
  run: async store => {
    console.log('Data loss detected!');
  },
});

// Decrease is now properly detected
const result = await engine.execute([hook], store);
// ✅ Condition triggers when triples decrease by >= 10
```

**All Delta Directions Now Supported:**

| Direction  | Trigger Condition      | Example                |
| ---------- | ---------------------- | ---------------------- |
| `increase` | Delta > threshold      | New quads added        |
| `decrease` | Delta < -threshold     | Quads removed          |
| `both`     | Abs(delta) > threshold | Any significant change |

**Migration Checklist:**

- [ ] Review all hooks using `direction: 'decrease'`
- [ ] Update tests—decrease monitoring now actually works
- [ ] Enable decrease monitoring if previously disabled
- [ ] Set appropriate `threshold` values
- [ ] No code changes needed

**Testing Decrease Detection:**

```javascript
// New capability: Test decrease detection
const monitorHook = defineHook({
  id: 'detect-loss',
  condition: {
    kind: 'delta',
    property: 'triples',
    direction: 'decrease',
    threshold: 5
  },
  run: async () => ({ success: true })
});

// Initially 20 triples
store.add(createQuad(...)); // × 20

// Execute hook (baseline established)
await engine.execute([monitorHook], store);

// Remove 6 triples (> 5 threshold)
for (let i = 0; i < 6; i++) {
  store.delete(quads[i]);
}

// Execute again (decrease detected!)
const result = await engine.execute([monitorHook], store);
console.assert(result.successful === true, 'Decrease should be detected');
```

---

### 5. Window Condition - Now Fully Implemented

**Status:** Major feature implementation

**Before (v26.4.3 and earlier):**

```javascript
// Window condition was a stub—only basic structure
const hook = defineHook({
  id: 'periodic-check',
  condition: {
    kind: 'window',
    type: 'time',
    duration: 3600000, // ❌ Duration ignored
    unit: 'ms', // ❌ Unit ignored
  },
  run: async () => {
    /* never executed on schedule */
  },
});

// Window monitoring didn't actually work
```

**After (v26.4.5+):**

```javascript
// Window condition now fully implemented
const hook = defineHook({
  id: 'periodic-check',
  condition: {
    kind: 'window',
    type: 'time',
    duration: 3600000, // ✅ 1 hour window
    unit: 'ms', // ✅ milliseconds
  },
  run: async () => {
    /* executes every hour */
  },
});

// Window monitoring works correctly
```

**Window Types Now Supported:**

| Type     | Duration         | Use Case           | Example                    |
| -------- | ---------------- | ------------------ | -------------------------- |
| `time`   | Duration in ms   | Periodic execution | Every 1 hour               |
| `count`  | Number of events | Batch processing   | Every 100 events           |
| `hybrid` | Time + count     | Flexible batching  | Every 1 hour OR 100 events |

**Window Time Units:**

- `ms` - Milliseconds
- `s` - Seconds
- `m` - Minutes
- `h` - Hours
- `d` - Days

**Migration Examples:**

```javascript
// Example 1: Execute every hour
defineHook({
  id: 'hourly-sync',
  condition: {
    kind: 'window',
    type: 'time',
    duration: 1,
    unit: 'h',
  },
  run: async store => {
    await synchronizeWithExternalSystem(store);
  },
});

// Example 2: Batch process every 100 events
defineHook({
  id: 'batch-events',
  condition: {
    kind: 'window',
    type: 'count',
    duration: 100,
  },
  run: async (store, events) => {
    await processBatch(store, events);
  },
});

// Example 3: Flexible: every 5 minutes OR 50 events
defineHook({
  id: 'hybrid-trigger',
  condition: {
    kind: 'window',
    type: 'hybrid',
    duration: 5,
    unit: 'm',
    count: 50,
  },
  run: async store => {
    await handleWindowEvent(store);
  },
});
```

**Migration Checklist:**

- [ ] Review all hooks using `kind: 'window'`
- [ ] Test window timing with actual execution
- [ ] Adjust `duration` and `unit` values based on needs
- [ ] Monitor for execution frequency
- [ ] No code changes needed—existing definitions now work

---

### 6. Knowledge-Engine Restoration - N3 Reasoning Now Available

**Status:** Major feature restoration

**Before (v26.4.3 and earlier):**

```javascript
// knowledge-engine was removed, N3 reasoning unavailable
// @unrdf/knowledge-engine: removed (broken imports, only used by experimental packages)
// No way to use forward-chaining inference
```

**After (v26.4.5+):**

```javascript
// N3 reasoning now available via n3 condition kind
const hook = defineHook({
  id: 'infer-relationships',
  condition: {
    kind: 'n3',
    rules: `
      { ?person foaf:knows ?other . } => { ?other foaf:knows ?person } .
    `,
    askQuery: 'ASK { ?x foaf:knows ?y . }',
  },
  run: async store => {
    console.log('Bidirectional relationships exist');
  },
});

// Advanced reasoning patterns
const complexHook = defineHook({
  id: 'ontology-reasoning',
  condition: {
    kind: 'n3',
    rules: `
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      
      { ?x a ?class . ?class rdfs:subClassOf ?parent . } 
      => 
      { ?x a ?parent . } .
      
      { ?a rdfs:subClassOf ?b . ?b rdfs:subClassOf ?c . }
      =>
      { ?a rdfs:subClassOf ?c . } .
    `,
    askQuery: 'ASK { ?x a ?parentClass . }',
  },
  run: async store => {
    console.log('Ontology closure computed');
  },
});
```

**N3 Condition Features:**

| Feature          | Description                    | Status          |
| ---------------- | ------------------------------ | --------------- |
| **rules**        | N3 forward-chaining rules      | ✅ Full support |
| **askQuery**     | SPARQL ASK over inferred graph | ✅ Full support |
| **EYE Reasoner** | Foundation (v10+)              | ✅ Included     |
| **Entailment**   | RDF semantics inference        | ✅ Supported    |
| **Recursion**    | Fixpoint closure computation   | ✅ Supported    |

**Migration Examples:**

```javascript
// Example 1: Simple transitive rule
defineHook({
  id: 'transitive-knows',
  condition: {
    kind: 'n3',
    rules: `
      { ?a foaf:knows ?b . ?b foaf:knows ?c . } 
      => 
      { ?a foaf:transitiveKnows ?c . } .
    `,
    askQuery: 'ASK { ?x foaf:transitiveKnows ?y . }',
  },
  run: async () => {
    console.log('Transitive relationships exist');
  },
});

// Example 2: Class hierarchy inference
defineHook({
  id: 'class-hierarchy',
  condition: {
    kind: 'n3',
    rules: `
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
      
      { ?x ?p ?y . ?p rdfs:range ?t . } => { ?y a ?t . } .
      { ?x ?p ?y . ?p rdfs:domain ?t . } => { ?x a ?t . } .
    `,
    askQuery: 'ASK { ?x a rdfs:Class . }',
  },
  run: async store => {
    // Typed inference complete
  },
});

// Example 3: Complex multi-step reasoning
defineHook({
  id: 'complex-inference',
  condition: {
    kind: 'n3',
    rules: `
      @prefix ex: <http://example.org/> .
      
      # Step 1: Inheritance
      { ?child ex:parentOf ?parent . ?grandchild ex:parentOf ?child . }
      =>
      { ?grandchild ex:grandchildOf ?parent . } .
      
      # Step 2: Closure
      { ?x ex:knows ?y . ?y ex:knows ?z . ?z ex:knows ?w . }
      =>
      { ?x ex:friendOfFriend ?w . } .
    `,
    askQuery: 'ASK { ?x ex:grandchildOf ?y . ?a ex:friendOfFriend ?b . }',
  },
  run: async store => {
    // Advanced reasoning complete
  },
});
```

**Migration Checklist:**

- [ ] Review code that was working around missing N3 support
- [ ] Migrate workarounds to use `kind: 'n3'` conditions
- [ ] Test rule definitions thoroughly
- [ ] Verify `askQuery` patterns are correct
- [ ] Benchmark performance vs. previous workarounds
- [ ] No code changes needed—use new features

**Datalog Logic Programming Also Available:**

```javascript
// Bonus: Datalog is also available for logic programming
defineHook({
  id: 'datalog-query',
  condition: {
    kind: 'datalog',
    facts: ['parent(alice, bob)', 'parent(bob, charlie)', 'parent(charlie, david)'],
    goal: 'ancestor(X, Y)', // Find all ancestor relationships
  },
  run: async () => {
    console.log('Datalog inference complete');
  },
});
```

---

## Upgrade Steps

### Step 1: Update Package Version

```bash
# Using npm
npm install @unrdf/hooks@26.4.5

# Using pnpm (recommended)
pnpm add @unrdf/hooks@26.4.5

# Using yarn
yarn add @unrdf/hooks@26.4.5
```

### Step 2: Verify Dependencies

```bash
# Check that @noble/hashes is installed (for BLAKE3)
npm ls @noble/hashes

# Check that eyereasoner is available (for N3)
npm ls eyereasoner

# Should show:
# @noble/hashes@2.0.1
# eyereasoner@10.x.x
```

### Step 3: Run Test Suite

```bash
# Run tests to ensure everything works
npm test

# Or with pnpm
pnpm test

# Or with yarn
yarn test
```

### Step 4: Update Hook Definitions (Optional)

If you want to use new features:

```javascript
// Option A: Add SHACL repair mode (now works!)
defineHook({
  condition: { enforcementMode: 'repair', repairConstruct: '...' },
  // ...
});

// Option B: Enable N3 reasoning
defineHook({
  condition: { kind: 'n3', rules: '...', askQuery: 'ASK { ... }' },
  // ...
});

// Option C: Monitor data loss with decrease detection
defineHook({
  condition: { kind: 'delta', direction: 'decrease', threshold: 10 },
  // ...
});

// Option D: Use window conditions for scheduling
defineHook({
  condition: { kind: 'window', type: 'time', duration: 1, unit: 'h' },
  // ...
});
```

### Step 5: Update Tests

```javascript
// Test that new features work
describe('v26.4.5 Features', () => {
  it('should apply SHACL repairs', async () => {
    const result = await engine.execute([repairHook], store);
    expect(result.successful).toBe(true);
    // Verify store was actually modified
  });

  it('should detect delta decreases', async () => {
    // Remove some quads
    const result = await engine.execute([decreaseHook], store);
    expect(result.successful).toBe(true);
  });

  it('should use N3 reasoning', async () => {
    const result = await engine.execute([n3Hook], store);
    expect(result.successful).toBe(true);
  });

  it('should execute on window schedule', async () => {
    const result = await engine.execute([windowHook], store);
    // Window should trigger on schedule
  });
});
```

### Step 6: Deploy

```bash
# Build your application
npm run build

# Deploy to your environment
npm run deploy

# Monitor for any issues
npm run monitor
```

---

## Breaking Changes

**NONE.** All changes are backward compatible.

- Existing APIs remain unchanged
- Existing hook definitions continue to work
- New features are opt-in
- Receipt format is compatible (only hash algorithm changed)

---

## Deprecated Features

None. All features continue to be supported.

---

## Known Issues and Workarounds

### Window Condition Timing

**Issue:** Window timing might be off by a few milliseconds due to JS event loop scheduling.

**Workaround:** If precision is critical, add a small buffer to your duration.

```javascript
// Instead of exactly 1 hour
duration: 3600000,

// Use slightly longer to account for scheduling variance
duration: 3600050,  // 1 hour + 50ms buffer
```

### N3 Rule Complexity

**Issue:** Very large rule sets (100+ rules) may have performance impact.

**Workaround:** Break rules into multiple hooks with different conditions.

```javascript
// ❌ One hook with 100 rules
defineHook({ condition: { kind: 'n3', rules: '/* 100 rules */' } });

// ✅ Multiple hooks with 20-30 rules each
defineHook({ condition: { kind: 'n3', rules: '/* rules 1-30 */' } });
defineHook({ condition: { kind: 'n3', rules: '/* rules 31-60 */' } });
defineHook({ condition: { kind: 'n3', rules: '/* rules 61-100 */' } });
```

---

## Support & Questions

- **Documentation:** [README.md](./README.md)
- **API Reference:** [API-REFERENCE.md](./API-REFERENCE.md)
- **Security Issues:** [SECURITY-DISCLOSURE.md](./SECURITY-DISCLOSURE.md)
- **GitHub Issues:** https://github.com/unrdf/unrdf/issues
- **Community Chat:** https://github.com/unrdf/unrdf/discussions

---

## Rollback Guide

If you need to downgrade:

```bash
# Go back to v26.4.4 (still recommended to upgrade)
npm install @unrdf/hooks@26.4.4

# Go back to v26.4.3 (re-introduces security issue)
npm install @unrdf/hooks@26.4.3  # ⚠️ SECURITY ISSUE
```

**Note:** Downgrading reintroduces the weak hashing vulnerability. Only downgrade if absolutely necessary and for temporary troubleshooting.

---

## Migration Checklist

Complete this checklist before deploying to production:

- [ ] Upgraded to @unrdf/hooks@26.4.5
- [ ] Verified @noble/hashes@2.0.1+ is installed
- [ ] Ran full test suite successfully
- [ ] Old receipts from v26.4.0-v26.4.3 archived or deleted
- [ ] New receipts verified to use BLAKE3
- [ ] All SHACL repair hooks tested (now they actually work)
- [ ] All SHACL annotate hooks tested (now they generate valid RDF)
- [ ] All delta decrease conditions tested (now they work)
- [ ] Window conditions verified for timing
- [ ] N3 reasoning rules validated if using new feature
- [ ] Documentation updated with new features
- [ ] Team trained on changes
- [ ] Compliance audit procedures updated (if using receipts)
- [ ] Monitoring updated for new features
- [ ] Deployment complete
- [ ] Post-deployment verification successful

---

## Version History

| Version | Date       | Status     | Notes                                     |
| ------- | ---------- | ---------- | ----------------------------------------- |
| 26.4.5  | 2026-04-04 | CURRENT    | Security fix (BLAKE3) + 5 major features  |
| 26.4.4  | 2026-04-03 | Previous   | Vision 2030 Phase 1 work                  |
| 26.4.3  | 2026-03-15 | Deprecated | Had weak hashing vulnerability            |
| 26.4.2  | 2026-03-01 | Deprecated | Earlier maturity work                     |
| 26.4.0  | 2026-01-15 | Deprecated | Initial v26.4 release (had vulnerability) |

---

**Last Updated:** 2026-04-04  
**Next Review:** 2026-07-04 (quarterly)
