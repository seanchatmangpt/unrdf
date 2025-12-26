# Explanation 02: Proof-Based Admission vs Editing

**Objective:** Understand UNRDF's governance philosophy: "admit with proof" rather than "edit freely".

**Audience:** Architects and policy designers

**Estimated Reading Time:** 30 minutes

---

## Introduction

UNRDF introduces a paradigm shift in knowledge graph interaction: instead of freely editing data, you **admit new data with cryptographic proof** of its validity and provenance. This article explores why this model exists and its implications.

---

## Traditional Editing Model

**[Placeholder - Content to be filled]**

### How Traditional RDF Works

```javascript
// Traditional approach
store.add(triple);      // Add freely
store.remove(triple);   // Remove freely
// No audit trail
// No proof required
// No verification
```

### Problems

1. **No Accountability**
   - Who added this triple?
   - When was it added?
   - Why was it added?
   - Was it authorized?

2. **No Verification**
   - Is the data valid?
   - Does it meet policies?
   - Has it been tampered with?

3. **No Audit Trail**
   - Manual logging required
   - Easy to miss operations
   - No immutable history

**Evidence:** Comparison analysis at `/home/user/unrdf/docs/analysis/editing-models.md`

---

## Proof-Based Admission Model

**[Placeholder - Proof-based model explanation]**

### How UNRDF Works

```javascript
// UNRDF approach
await admitWithProof({
  data: triple,
  proof: {
    signature: '...',
    timestamp: '...',
    policy: 'approved-by-admin'
  },
  receipt: {
    anchor: 'git-notes'
  }
});
```

### Key Principles

1. **Every Change Requires Proof**
   - Who: Actor identity
   - What: Data being added
   - Why: Justification/policy
   - When: Timestamp
   - How: Receipt/signature

2. **Admission Over Editing**
   - Data is "admitted" if proof valid
   - Data is "rejected" if proof invalid
   - No direct editing
   - Immutable audit trail

3. **Policy Gates**
   - Hooks validate proof
   - Policies define requirements
   - Automatic enforcement
   - Veto mechanism

**Evidence:** Admission model at `/home/user/unrdf/packages/kgc-4d/src/admission.mjs`

---

## Philosophical Foundations

**[Placeholder - Philosophy deep dive]**

### Inspiration Sources

1. **Git Model**
   - Commit = admission with proof
   - Signed commits = verified provenance
   - Immutable history

2. **Blockchain Model**
   - Transaction = admission
   - Consensus = proof validation
   - Chain = audit trail

3. **Zero-Trust Security**
   - Verify everything
   - Trust nothing
   - Explicit authorization

**Evidence:** References in `/home/user/unrdf/docs/BIBLIOGRAPHY.bib`

---

## Design Trade-offs

**[Placeholder - Trade-off analysis]**

### Advantages of Proof-Based Model

- ✅ Complete audit trail
- ✅ Cryptographic verification
- ✅ Policy enforcement built-in
- ✅ Tamper evident
- ✅ Regulatory compliance friendly

### Disadvantages

- ❌ More complex API
- ❌ Performance overhead (proof generation)
- ❌ Learning curve
- ❌ Not suitable for all use cases

**Evidence:** Trade-off analysis at `/home/user/unrdf/docs/architecture/admission-tradeoffs.md`

---

## When to Use Each Model

**[Placeholder - Decision guide]**

### Use Proof-Based Admission When:

- Regulatory compliance required
- Multiple actors with different trust levels
- Audit trail essential
- Data provenance critical
- Security paramount

### Use Traditional Editing When:

- Single trusted actor
- No compliance requirements
- Performance critical
- Simple use cases
- Prototyping

**Evidence:** Decision tree at `/home/user/unrdf/docs/guides/choosing-model.md`

---

## Implementation Patterns

**[Placeholder - Implementation examples]**

### Pattern 1: Approval Workflow

**Evidence:** Pattern at `/home/user/unrdf/examples/patterns/approval-workflow.mjs`

---

### Pattern 2: Multi-Signature Admission

**Evidence:** Pattern at `/home/user/unrdf/examples/patterns/multi-sig.mjs`

---

### Pattern 3: Time-Locked Admission

**Evidence:** Pattern at `/home/user/unrdf/examples/patterns/time-locked.mjs`

---

## Comparison with Other Systems

**[Placeholder - System comparison]**

| System | Model | Audit Trail | Proof Required | Immutable |
|--------|-------|-------------|----------------|-----------|
| Traditional RDF | Edit | ❌ No | ❌ No | ❌ No |
| Git | Admit (commit) | ✅ Yes | ⚠️ Optional | ✅ Yes |
| Blockchain | Admit (tx) | ✅ Yes | ✅ Yes | ✅ Yes |
| UNRDF | Admit (proof) | ✅ Yes | ✅ Yes | ✅ Yes |

---

## Real-World Examples

**[Placeholder - Real-world use cases]**

### Example 1: Healthcare Records

**Evidence:** Case study at `/home/user/unrdf/docs/case-studies/healthcare.md`

---

### Example 2: Supply Chain

**Evidence:** Case study at `/home/user/unrdf/docs/case-studies/supply-chain.md`

---

### Example 3: Academic Publishing

**Evidence:** Case study at `/home/user/unrdf/docs/case-studies/academic.md`

---

## Performance Considerations

**[Placeholder - Performance analysis]**

**Evidence:** Benchmarks at `/home/user/unrdf/benchmarks/admission-performance.mjs`

---

## Conclusion

**[Placeholder - Summary]**

Proof-based admission represents a fundamental shift from "trust and edit" to "verify and admit". This model:
- Ensures accountability
- Provides cryptographic verification
- Creates immutable audit trails
- Enforces policies automatically

The overhead is justified for systems requiring strong governance, compliance, or multi-actor trust.

---

## Related Reading

- **[Explanation 01: Why Partitioned Universes](./why-partitioned-universes.md)** - Universe model
- **[Tutorial 03: Generate and Verify Receipts](../tutorials/03-generate-and-verify-receipts.md)** - Receipts in practice
- **[How-To 01: Validate Policy Packs](../how-to/01-validate-policy-packs.md)** - Policy implementation

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
