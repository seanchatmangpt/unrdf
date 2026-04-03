# API Reference: O* Innovations 4-6

---

## Innovation 4: FederationQuorum

### Class: `FederationQuorum`

M-of-N federation trust voting with BLAKE3 receipt chaining.

#### Constructor

```javascript
const quorum = new FederationQuorum({
  validators: [
    { id: 'alice', weight: 1 },
    { id: 'bob', weight: 1 },
  ],
  requiredVotes: 2,
});
```

#### Method: `decide(proposalId, approvals)`

**Returns**: `{ proposalId, approved, receipt: { receiptHash, inputHash, outputHash, previousReceiptHash } }`

```javascript
const decision = await quorum.decide('proposal-123', {
  'alice': true,
  'bob': true,
});

console.log(decision.receipt.receiptHash);  // BLAKE3 proof
```

---

## Innovation 5: HooksMarketplace

### Class: `HooksMarketplace`

RDF-based hook composition with dependency resolution.

#### Method: `admit(hook)`

**Returns**: `{ admitted, hookIRI, violations, resolvedDependencies }`

```javascript
const result = await marketplace.admit({
  name: 'validator',
  conditions: [{ kind: 'shacl', ref: '...' }],
  effects: [{ kind: 'sparql-construct', query: '...' }],
});

// admitted=true even with violations (soft-fail)
if (result.violations.length > 0) {
  console.log('Audit trail captured as RDF');
}
```

#### Method: `query(sparql)`

Query marketplace store via SPARQL.

```javascript
const hooks = await marketplace.query(`
  PREFIX hook: <http://ostar.org/hook/>
  SELECT ?name WHERE { ?hook hook:name ?name }
`);
```

---

## Innovation 6: StreamingAdmission

### Class: `StreamingAdmission`

Streaming delta admission with BLAKE3 receipt chaining.

#### Method: `admit(deltas, condition)`

**Returns**: `{ admitted, receipt: { receiptHash, inputHash, outputHash, deltaHash, previousReceiptHash } }`

```javascript
const { admitted, receipt } = await stream.admit(
  [{ type: 'add', quad: ... }],
  { kind: 'delta', checksum: 'blake3' }
);

console.log(receipt.inputHash);    // Before
console.log(receipt.outputHash);   // After
console.log(receipt.deltaHash);    // Delta proof
```

#### Method: `getReceiptChain(targetHash, limit)`

Retrieve receipt chain history (proof lineage).

```javascript
const chain = stream.getReceiptChain(someHash);
// Can verify complete update lineage
```

---

## Schemas

```javascript
import {
  ValidatorSchema,
  QuorumConfigSchema,
  HookDefinitionSchema,
  DeltaConditionSchema,
} from '../src/lib/*.mjs';

// All support parse() and safeParse() via Zod
ValidatorSchema.parse({ id: 'v1', weight: 2 });
```

---

**Version**: 26.4.3 | **Date**: April 3, 2026
