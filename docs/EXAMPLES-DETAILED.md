# Detailed Examples: O\* Innovations 4-6

---

## Innovation 4: Federation Quorum Examples

### Example 1: Simple M-of-N Voting

```javascript
import { FederationQuorum } from '../src/lib/federate.mjs';

// Setup: 3-of-5 validator network
const quorum = new FederationQuorum({
  validators: [
    { id: 'ethereum-validator', weight: 1 },
    { id: 'solana-validator', weight: 1 },
    { id: 'cosmos-validator', weight: 1 },
    { id: 'avalanche-validator', weight: 1 },
    { id: 'polygon-validator', weight: 1 },
  ],
  requiredVotes: 3, // Need 3 of 5 to approve
});

// Governance decision: Upgrade core protocol
const proposal = 'upgrade-protocol-v2.1';

const approvals = {
  'ethereum-validator': true,
  'solana-validator': true,
  'cosmos-validator': true,
  'avalanche-validator': false,
  'polygon-validator': false,
};

const decision = await quorum.decide(proposal, approvals);

console.log('Decision:', decision.approved); // true (3 votes)
console.log('Approvers:', decision.approvingValidators);
// ['ethereum-validator', 'solana-validator', 'cosmos-validator']
console.log('Receipt Hash:', decision.receipt.receiptHash);
// Proof that this decision is legitimate
```

### Example 2: Weighted Voting

```javascript
// Some validators have more power (e.g., larger stake)
const quorum = new FederationQuorum({
  validators: [
    { id: 'large-validator', weight: 5 }, // 50% voting power
    { id: 'medium-validator-1', weight: 3 }, // 30%
    { id: 'medium-validator-2', weight: 2 }, // 20%
  ],
  requiredWeight: 6, // Need 6+ weight (51%)
});

const decision = await quorum.decide('proposal-456', {
  'large-validator': true,
  'medium-validator-1': false,
  'medium-validator-2': true,
});

console.log(decision.approved); // true (5 + 2 = 7 weight)
console.log(decision.approvingWeight); // 7
```

### Example 3: Receipt Chaining

```javascript
// First decision
const decision1 = await quorum.decide('proposal-1', approvals1);
console.log('Decision 1 Receipt:', decision1.receipt.receiptHash);

// Second decision (references first)
const quorum2 = new FederationQuorum({
  validators: [...],
  requiredVotes: 2,
  context: {
    previousReceiptHash: decision1.receipt.receiptHash,
  },
});

const decision2 = await quorum2.decide('proposal-2', approvals2);
console.log('Decision 2 Receipt:', decision2.receipt.receiptHash);
console.log('Links to Decision 1:', decision2.receipt.previousReceiptHash);

// Later: Verify unbroken chain of decisions
const chain = [
  decision1.receipt,
  decision2.receipt,
  // ... more decisions
];

// Each receipt cryptographically proves the previous
```

---

## Innovation 5: Hooks Marketplace Examples

### Example 1: Simple Hook Admission

```javascript
import { HooksMarketplace } from '../src/lib/admit-hook.mjs';

const marketplace = new HooksMarketplace();

// Define a payment validator hook
const paymentHook = {
  name: 'payment-validator',
  version: '1.0.0',
  description: 'Validates payment transactions',
  conditions: [
    {
      kind: 'shacl',
      ref: 'http://example.org/shapes/payment-shape',
    },
  ],
  effects: [
    {
      kind: 'sparql-construct',
      query: `
        CONSTRUCT { ?tx ex:validated true }
        WHERE { ?tx ex:amount ?amount . FILTER(?amount > 0) }
      `,
    },
  ],
};

const result = await marketplace.admit(paymentHook);

console.log('Admitted:', result.admitted); // true
console.log('Hook IRI:', result.hookIRI); // http://example.org/hook/...
console.log('Violations:', result.violations); // [] if valid
```

### Example 2: Dependency Resolution

```javascript
// Hook B depends on Hook A
const hookB = {
  name: 'advanced-validator',
  dependencies: ['basic-validator'],
  conditions: [...],
  effects: [...],
};

const result = await marketplace.admit(hookB);

console.log('Resolved Dependencies:');
// Marketplace automatically resolves:
// - basic-validator (direct)
// - Any hooks that basic-validator depends on (transitive)
for (const dep of result.resolvedDependencies) {
  console.log(`  - ${dep}`);
}
```

### Example 3: Soft-Fail with Audit Trail

```javascript
// This hook has SHACL violations (missing metadata)
const incompleteHook = {
  name: 'incomplete-hook',
  // Missing version field (SHACL will complain)
  conditions: [{ kind: 'shacl', ref: 'http://...' }],
};

const result = await marketplace.admit(incompleteHook);

// With soft-fail (annotate mode):
console.log('Admitted:', result.admitted); // true!
console.log('Violations:', result.violations.length); // > 0

// Violations are RDF triples (SHACL report)
// Example triple:
// [
//   sh:focusNode = <http://example.org/hook/incomplete-hook>,
//   sh:resultPath = ex:version,
//   sh:resultMessage = "Missing required property 'version'"
// ]

// Admin can query marketplace for problematic hooks:
const query = `
  PREFIX sh: <http://www.w3.org/ns/shacl#>
  SELECT ?hook ?message WHERE {
    ?hook a hook:Hook .
    ?violation sh:focusNode ?hook ;
               sh:resultMessage ?message .
  }
`;

const problems = await marketplace.query(query);
console.log('Problematic hooks:', problems);
```

### Example 4: Circular Dependency Detection

```javascript
// Hook A depends on B, B depends on C, C depends on A (cycle!)
const hookA = {
  name: 'hook-a',
  dependencies: ['hook-b'],
};

try {
  const result = await marketplace.admit(hookA);
} catch (error) {
  if (error.code === 'CIRCULAR_DEPENDENCY') {
    console.log('Detected cycle:', error.message);
    // Marketplace automatically rejects cyclical hooks
  }
}
```

### Example 5: Querying the Marketplace

```javascript
const marketplace = new HooksMarketplace();

// Admit several hooks
await marketplace.admit(hook1);
await marketplace.admit(hook2);
await marketplace.admit(hook3);

// Now query what's in the marketplace
const allHooks = await marketplace.query(`
  PREFIX hook: <http://ostar.org/hook/>
  SELECT ?name ?version WHERE {
    ?hook a hook:Hook ;
          hook:name ?name ;
          hook:version ?version .
  }
`);

console.log('Marketplace contains:', allHooks);

// Query hooks with no violations
const healthyHooks = await marketplace.query(`
  PREFIX hook: <http://ostar.org/hook/>
  PREFIX sh: <http://www.w3.org/ns/shacl#>
  SELECT ?name WHERE {
    ?hook a hook:Hook ; hook:name ?name .
    FILTER NOT EXISTS { ?hook sh:violation ?v }
  }
`);

console.log('Healthy hooks:', healthyHooks);
```

---

## Innovation 6: Streaming Admission Examples

### Example 1: Basic Delta Admission

```javascript
import { StreamingAdmission, createStreamingAdmission } from '../src/lib/stream-admit.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const store = createStore();
const stream = new StreamingAdmission(store);

// Admit a batch of deltas
const deltas = [
  {
    type: 'add',
    quad: dataFactory.quad(
      dataFactory.namedNode('http://example.org/alice'),
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
      dataFactory.literal('Alice')
    ),
  },
  {
    type: 'add',
    quad: dataFactory.quad(
      dataFactory.namedNode('http://example.org/bob'),
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
      dataFactory.literal('Bob')
    ),
  },
];

const { admitted, receipt } = await stream.admit(deltas);

console.log('Deltas Admitted:', admitted); // true
console.log('Input Hash:', receipt.inputHash); // Store before
console.log('Output Hash:', receipt.outputHash); // Store after
console.log('Delta Hash:', receipt.deltaHash); // Proof of delta
```

### Example 2: Determinism Verification

```javascript
// Same delta, run twice
const delta = { type: 'add', quad: aliceNameQuad };

const result1 = await stream.admit([delta]);
const result2 = await stream.admit([delta]);

// Determinism guarantee: identical hashes
console.log('Run 1 - Input Hash:', result1.receipt.inputHash);
console.log('Run 2 - Input Hash:', result2.receipt.inputHash);
console.assert(result1.receipt.inputHash === result2.receipt.inputHash, 'Input hashes must match!');

console.log('Run 1 - Delta Hash:', result1.receipt.deltaHash);
console.log('Run 2 - Delta Hash:', result2.receipt.deltaHash);
console.assert(result1.receipt.deltaHash === result2.receipt.deltaHash, 'Delta hashes must match!');

// Even output hash is deterministic (same store state)
console.assert(
  result1.receipt.outputHash === result2.receipt.outputHash,
  'Output hashes must match!'
);
```

### Example 3: Receipt Chaining (Proof Lineage)

```javascript
// Admit 10 updates in sequence
const stream = new StreamingAdmission(store);

for (let i = 0; i < 10; i++) {
  const delta = {
    type: 'add',
    quad: dataFactory.quad(
      dataFactory.namedNode(`http://example.org/entity-${i}`),
      dataFactory.namedNode('http://example.org/value'),
      dataFactory.literal(String(i))
    ),
  };

  const { receipt } = await stream.admit([delta]);
  console.log(`Update ${i}: ${receipt.receiptHash}`);
}

// Later: Get receipt chain to verify history
const finalReceipt = /* some receipt hash */;
const chain = stream.getReceiptChain(finalReceipt);

console.log('Update Chain:');
for (let i = chain.length - 1; i >= 0; i--) {
  const receipt = chain[i];
  const previous = chain[i - 1];
  console.log(`  ${receipt.receiptHash}`);
  if (previous) {
    console.assert(
      receipt.previousReceiptHash === previous.receiptHash,
      'Chain is broken!'
    );
  }
}

// Unbroken chain proves complete history
```

### Example 4: Conditional Admission (With Hash Verification)

```javascript
const stream = new StreamingAdmission(store);

// Admit delta, verifying output hash matches expected
const expectedOutputHash = 'abc123...'; // Pre-computed

const { admitted, receipt } = await stream.admit([delta1, delta2], {
  kind: 'delta',
  hash: expectedOutputHash,
  checksum: 'blake3',
});

if (!admitted) {
  console.log('Delta rejected! Reason:', receipt.reason);
  console.log('Store automatically rolled back');
  // This is useful for:
  // - Verifying determinism
  // - Rejecting malformed or malicious deltas
}
```

### Example 5: Stream Processing (Multiple Deltas Over Time)

```javascript
const stream = new StreamingAdmission(store);
let currentReceiptHash = null;

// Simulate streaming data pipeline
async function processUpdate(delta) {
  const { admitted, receipt } = await stream.admit([delta]);

  if (admitted) {
    currentReceiptHash = receipt.receiptHash;
    console.log(`Update admitted. Receipt: ${currentReceiptHash}`);
  } else {
    console.log(`Update REJECTED: ${receipt.reason}`);
    // Previous state is unchanged (automatic rollback)
  }

  return receipt;
}

// Process 100 updates from streaming source
const receipts = [];
for (const incomingDelta of incomingDataStream) {
  const receipt = await processUpdate(incomingDelta);
  receipts.push(receipt);
}

console.log(`Processed ${receipts.length} updates`);
console.log(`Final Receipt: ${currentReceiptHash}`);

// Verify complete chain
const chain = stream.getReceiptChain(currentReceiptHash);
console.log(`Chain length: ${chain.length}`);
console.assert(chain.length === receipts.length, 'Missing receipts!');
```

---

## Combined Example: All 3 Innovations

```javascript
import { FederationQuorum } from '../src/lib/federate.mjs';
import { HooksMarketplace } from '../src/lib/admit-hook.mjs';
import { StreamingAdmission } from '../src/lib/stream-admit.mjs';

// 1. Federation votes to admit a new hook to marketplace
const quorum = new FederationQuorum({ validators: [...], requiredVotes: 3 });
const voteResult = await quorum.decide('admit-payment-hook', approvals);

// 2. Hook admitted to marketplace (with SHACL soft-fail)
const marketplace = new HooksMarketplace();
const hookResult = await marketplace.admit(paymentHook);

// 3. Incoming data stream uses the hook + records updates
const stream = new StreamingAdmission(store);
for (const delta of paymentUpdates) {
  const streamResult = await stream.admit([delta]);
}

// Complete governance trail:
console.log('Federation Receipt:', voteResult.receipt.receiptHash);
console.log('Marketplace Receipt:', hookResult.receiptHash);
console.log('Stream Receipt Chain:', stream.getReceiptChain(...));

// All decisions linked via receipt chaining
```

---

**Version**: 26.4.4 | **Date**: April 3, 2026
