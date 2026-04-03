# FIBO JTBD Governance Case Study

## Overview

This case study demonstrates a comprehensive **Jobs to Be Done (JTBD)** framework applied to **Financial Industry Business Ontology (FIBO)** regulatory compliance. It showcases all **6 priorities** of the UNRDF knowledge hooks platform working together in a real-world financial governance scenario.

**Files in this case study:**
- `fibo-jtbd-governance.mjs` - Main executable example with all 5 JTBDs
- `fibo-ontology-snippet.nt` - FIBO ontology in N-Triples format
- `fibo-hooks-config.yaml` - Hook configuration for governance
- `FIBO-JTBD-GOVERNANCE-README.md` - This documentation

## What is JTBD?

**Jobs to Be Done (JTBD)** is an outcomes-focused framework that shifts perspective from:
- What customers are (traders, risk officers, compliance teams)
- What they do (buying, trading, reporting)

To:
- **What they want to accomplish** (specific outcomes they're hiring tools to achieve)

In financial services, key JTBDs include:
- "Help me comply with regulatory requirements without manual review"
- "Help me understand counterparty risk across my portfolio"
- "Help me manage liquidity positions within constraints"
- "Help me maintain an immutable audit trail for regulators"
- "Help me automatically fix compliance violations"

## The 6 Priorities

### Priority 1: withReceipt Integration

**What:** Cryptographic audit trail for every operation using receipt chaining.

**How:** Each hook execution produces a BLAKE3 hash that includes:
- Hash of input state
- Hash of output state
- Hash of previous receipt (creating a chain)

**Why:** Regulatory compliance requires proof that operations happened and can be audited.

**Example:**
```javascript
receipt: {
  enabled: true,
  algorithm: 'blake3',
  anchor: 'git-notes',
  chain: true,  // Each receipt references the previous
  includePayload: true
}
```

### Priority 2: SPARQL CONSTRUCT Effects

**What:** RDF graph transformations that capture state changes as semantic facts.

**How:** After a hook condition is satisfied, SPARQL CONSTRUCT queries add new facts to the RDF store.

**Why:** All compliance decisions must be recorded as RDF facts that can be queried and audited.

**Example - Add Compliance Status:**
```sparql
CONSTRUCT {
  ?trade fibo:hasComplianceStatus fibo:Compliant ;
         fibo:auditedAt ?now ;
         fibo:auditedBy ?auditor .
}
WHERE {
  ?trade fibo:txType ?type ;
         fibo:value ?value .
  BIND (NOW() as ?now)
  BIND (iri(concat("urn:auditor:", "sec-bot")) as ?auditor)
}
```

### Priority 3: SHACL Enforcement Modes

**What:** Semantic constraints with three enforcement strategies:
- `annotate` - Log violations, continue (soft-fail)
- `block` - Stop if violations found (fail-fast)
- `repair` - Auto-fix violations (self-healing)

**How:** SHACL shapes define constraints. The enforcement mode determines what happens when violated.

**Why:** Different scenarios need different enforcement:
- Compliance verification: Log issues but continue
- Audit trail: Strict (block on any violation)
- Auto-repair: Self-heal and log what was fixed

**Example - Block Mode (Strict):**
```yaml
validation:
  kind: shacl
  shape: "fibo:AuditTrailShape"
  enforcementMode: block  # NO EXCEPTIONS
  constraints:
    - "sh:minCount 1"  # Every audit entry required
    - "sh:pattern '^[a-f0-9]{64}$'"  # Valid hash format
```

### Priority 4: Input/Output Hash Receipts

**What:** Cryptographic proof that state changed from input→output.

**How:** Both input and output states are hashed independently, then the receipt contains:
- `inputHash` - Hash of state before operation
- `outputHash` - Hash of state after operation
- `receiptHash` - Hash of the receipt itself
- `previousReceiptHash` - Link to previous receipt (chain)

**Why:** Regulators require proof that:
1. We processed specific input data
2. We produced specific output
3. The transformation is auditable

**Example:**
```javascript
// Receipt structure
{
  inputHash: "6b86b273f403eb80779c5e0b0eda74007d1d376336d7999c28cc2753ed012100",
  outputHash: "d4735fea8e8e74aeb290ffc1c81d03fcac79d7eb9b2760cbfdb3e82d0f8e1d5e",
  receiptHash: "3cc8e37da141aa6e92c0eb6e63eb9d38f1b9da1b7a8c8d8f4e5c5d5d6d7d8d8",
  previousReceiptHash: "2c26b46911185131006145dd0c1540629a73f7c6c5417f63c306cc4de8d131bd",
  timestamp: "2026-04-03T15:30:00.000Z"
}
```

### Priority 5: N3 Forward-Chaining Rules

**What:** Regulatory rules expressed in N3 (Notation 3) for forward-chaining inference.

**How:** N3 rules define implications: `{antecedent} => {consequent}`.

**Why:** Financial regulations are naturally expressed as rules:
- "If credit rating is AAA, then risk level is Low"
- "If default history > 0, then risk level is High"
- "If regulatory status is Restricted, then risk level is High"

**Example - N3 Risk Rules:**
```n3
# High-quality credit ratings → Low risk
{
  ?cp fibo:creditRating ?rating .
  (?rating) list:member ( fibo:AAA fibo:AA fibo:A ) .
} => {
  ?cp fibo:riskLevel fibo:Low ;
      fibo:riskReason "Credit rating indicates low default probability" .
} .

# Any default history → High risk (conservative approach)
{
  ?cp fibo:defaultHistory ?count .
  ?count math:greaterThan 0 .
} => {
  ?cp fibo:riskLevel fibo:High ;
      fibo:riskReason "Counterparty has previous defaults" .
} .
```

**Why N3 is ideal for regulations:**
- Expresses "if-then" naturally
- Supports negation ("if NOT in watchlist")
- Human-readable (auditors can verify)
- Supports complex logic (chains of rules)
- Produces auditable inference traces

### Priority 6: Datalog Logic Programming

**What:** Constraint satisfaction using logic programming with facts and rules.

**How:** Datalog uses:
1. **Facts** - Initial assertions about the world
2. **Rules** - Logical implications derived from facts
3. **Goal** - Query to verify or find solutions

**Why:** Liquidity management requires constraint satisfaction:
- "Available ≥ Reserved + Threshold" (hard constraint)
- "Compliant if usable capital ≥ minimum threshold" (derived fact)
- "Find all non-compliant accounts" (goal query)

**Example - Liquidity Constraints:**
```datalog
% Facts
account(acc-001)
available(acc-001, 1000000)
reserved(acc-001, 250000)
threshold(acc-001, 100000)
riskLevel(acc-001, low)

% Rules
usable(A, X) :- available(A, X), riskLevel(A, low)
compliant(A) :- usable(A, U), threshold(A, T), U >= T, freed(A, F), F >= 0

% Goal
?- compliant(acc-001)
% Result: TRUE (1M available >= 350K required)
```

**Why Datalog for liquidity:**
- Deterministic (same facts → same conclusions always)
- Auditable (can trace conclusion back to specific facts)
- Handles fixed-point computation (iterative constraint solving)
- Detects contradictions (over-allocation)

## The 5 JTBDs Explained

### JTBD 1: Verify Regulatory Compliance

**Customer Job:** "Help me ensure all trades comply with SEC regulations before settlement."

**Governance Pattern:**
- Condition: SHACL shape (fibo:TradeComplianceShape)
- Enforcement: `annotate` mode (soft-fail - log violations, continue)
- Effects: SPARQL CONSTRUCT adds compliance status to RDF
- Receipt: Receipt chain for audit trail

**Priorities Used:** 1 (withReceipt) + 2 (sparql-construct) + 3 (SHACL/annotate)

**Process:**
1. Trade arrives in system
2. SHACL validates against compliance shape (soft-fail)
3. SPARQL CONSTRUCT adds `fibo:hasComplianceStatus fibo:Compliant`
4. Receipt generated with blake3 hash chain
5. Result: Trade marked compliant with audit proof

### JTBD 2: Assess Counterparty Risk

**Customer Job:** "Help me determine if this counterparty is safe to trade with based on their credit history and regulatory status."

**Governance Pattern:**
- Condition: N3 forward-chaining rules
- Effects: SPARQL CONSTRUCT records risk assessment
- Receipt: Merkle tree of all assessments

**Priorities Used:** 5 (N3 Rules) + 2 (sparql-construct) + 1 (withReceipt)

**Rules Applied:**
1. Credit Rating AAA/AA/A → Low Risk
2. Credit Rating BBB/BB → Medium Risk
3. Credit Rating B/CCC → High Risk
4. Any default history > 0 → High Risk (override)
5. Regulatory restriction → High Risk
6. All good + approved status → Low Risk (override)

**Process:**
1. Counterparty fact added to RDF store
2. N3 rules fire forward-chaining inference
3. Risk level derived (AAA credit → Low risk)
4. SPARQL CONSTRUCT records assessment
5. Receipt proves assessment with hash chain

### JTBD 3: Manage Liquidity Positions

**Customer Job:** "Help me calculate available liquidity while meeting regulatory minimums and ensuring we don't over-reserve."

**Governance Pattern:**
- Condition: Datalog constraint satisfaction
- Validation: SHACL shape with `block` mode (strict)
- Effects: SPARQL CONSTRUCT records liquidity status
- Receipt: Receipt chain for audit

**Priorities Used:** 6 (Datalog) + 3 (SHACL/block) + 1 (withReceipt)

**Constraints:**
1. Usable = Available - Reserved
2. Compliant if Usable ≥ Threshold
3. Freed = Available - Reserved (must be ≥ 0)
4. All priority accounts must be compliant

**Process:**
1. Account facts loaded (available, reserved, threshold)
2. Datalog rules derive usable and compliant
3. SHACL validates in BLOCK mode (strict)
4. SPARQL CONSTRUCT records liquidity status
5. Result: Gate opens only if all accounts compliant

### JTBD 4: Maintain Compliance Audit Trail

**Customer Job:** "Help me create an immutable, cryptographically-proven audit trail that regulators can verify."

**Governance Pattern:**
- Condition: SHACL shape with `block` mode (strict)
- Effects: SPARQL CONSTRUCT creates audit entries with hashes
- Receipt: Full receipt chaining with input/output hashes
- Verification: Chain integrity proof

**Priorities Used:** 3 (SHACL/block) + 1 (withReceipt) + 4 (Hash Receipts)

**Audit Entry Fields:**
- auditId - Unique identifier
- timestamp - When audit happened
- inputHash - State before operation
- outputHash - State after operation
- receiptHash - Proof of audit entry
- previousReceiptHash - Link to previous (chain integrity)

**Process:**
1. Transaction occurs
2. SHACL validates audit shape (strict mode)
3. SPARQL CONSTRUCT creates audit entry with all hashes
4. Receipt generated with blake3
5. Receipt includes hash of previous receipt (chain)
6. Result: Tamper-evident chain regulators can verify

**Verification Example:**
```
Receipt N: {hash: ABC123..., previous: 789DEF...}
  ↓ verify previous hash matches
Receipt N-1: {hash: 789DEF..., previous: 456GHI...}
  ↓ verify previous hash matches
Receipt N-2: {hash: 456GHI..., previous: 123JKL...}
  ↓ chain integrity proven
```

### JTBD 5: Auto-Repair Compliance Violations

**Customer Job:** "Help me automatically fix compliance violations without manual intervention or escalation."

**Governance Pattern:**
- Condition: SHACL shape with `repair` mode (auto-fix)
- Repairs:
  - Missing type → Assign default
  - Negative value → Set to zero
  - Invalid status → Normalize to Pending
- Effects: SPARQL CONSTRUCT logs remediation
- Receipt: Receipt proof of repair

**Priorities Used:** 3 (SHACL/repair) + 2 (sparql-construct) + 1 (withReceipt)

**Repair Examples:**
- Trade missing type → Auto-assign as `fibo:PendingTrade`
- Trade with negative value → Auto-correct to 0
- Trade with invalid status → Auto-normalize to `fibo:Pending`

**Process:**
1. Violation detected via SHACL
2. Repair rule applied automatically
3. SPARQL CONSTRUCT logs repair (method, timestamp, reason)
4. Entity marked as `fibo:FullyCompliant`
5. Receipt generated proving repair
6. Result: Violation fixed without manual review

## Running the Case Study

### Basic Execution

```bash
node examples/fibo-jtbd-governance.mjs
```

**Output:**
```
================================================================================
FIBO JTBD GOVERNANCE CASE STUDY
Financial Regulatory Compliance with Knowledge Hooks
================================================================================

📦 Step 1: Initialize FIBO Knowledge Store
   ✓ Created Oxigraph store with FIBO ontology

📥 Step 2: Load Trade Data
   ✓ Validated 3 trades

👥 Step 3: Load Counterparty Risk Data
   ✓ Validated 3 counterparties

💰 Step 4: Load Liquidity Positions
   ✓ Validated 2 accounts

⚡ Step 5: Execute JTBDs with Governance

📊 CASE STUDY RESULTS

JTBD 1: Verify Regulatory Compliance
  Priority: 1 (withReceipt) + 2 (sparql-construct) + 3 (SHACL/annotate)
  Status: PASS
  Trades Verified: 3
  Receipts Generated: 3
  Audit Proof: Receipt chain with blake3 hashing

JTBD 2: Assess Counterparty Risk
  Priority: 5 (N3 Rules) + 2 (sparql-construct) + 1 (withReceipt)
  Status: PASS
  Counterparties Assessed: 3
  Risk Breakdown: Low=2, Medium=1, High=0
  N3 Rules Applied: 6
  Receipts Generated: 3

JTBD 3: Manage Liquidity Positions
  Priority: 6 (Datalog) + 3 (SHACL/block) + 1 (withReceipt) + 2 (sparql-construct)
  Status: PASS
  Accounts Evaluated: 2
  Compliance: Compliant=2, Non-Compliant=0
  Datalog Rules: 7
  Enforcement Mode: block (strict)

JTBD 4: Maintain Compliance Audit Trail
  Priority: 3 (SHACL/block) + 1 (withReceipt) + 4 (Hash Receipts)
  Status: PASS
  Audit Entries: 3
  SHACL Mode: block (no exceptions)
  Hash Algorithm: BLAKE3
  Receipt Chain Integrity: true

JTBD 5: Auto-Repair Compliance Violations
  Priority: 3 (SHACL/repair) + 2 (sparql-construct) + 1 (withReceipt)
  Status: PASS
  Violations Detected: 0
  Repairs Applied: 0
  SHACL Mode: repair (auto-fix)

================================================================================
CASE STUDY SUMMARY
================================================================================

All 6 Priorities Successfully Demonstrated in Financial Governance:

Priority 1 (withReceipt):
  - Cryptographic audit trail for all JTBDs with blake3 hashing

Priority 2 (sparql-construct):
  - RDF state transformations in compliance, risk, and audit JTBDs

Priority 3 (SHACL):
  - Three enforcement modes: annotate (soft), block (strict), repair (auto-fix)

Priority 4 (Hash Receipts):
  - Input/output hashes prove state transitions in audit trail

Priority 5 (N3 Rules):
  - Forward-chaining regulatory inference for risk assessment

Priority 6 (Datalog):
  - Logic programming constraints for liquidity compliance

================================================================================
```

## Key Insights

### Why This Matters for Finance

1. **Regulatory Compliance:**
   - SEC requires proof of compliance decisions
   - Audit trails must be tamper-evident
   - All decisions must be reproducible

2. **Risk Management:**
   - Counterparty risk must be continuously assessed
   - Risk rules evolve; governance must adapt
   - N3 rules are easy for risk officers to verify

3. **Liquidity Governance:**
   - Constraints are strict (hard limits)
   - Logic programming ensures consistency
   - No manual workarounds possible

4. **Audit Requirements:**
   - Every state change must be logged
   - Hashes prove integrity
   - Chain proves completeness

### The JTBD Framework Benefits

**Traditional approach:** "Build a compliance tool"
- Focuses on features (checklist verification)
- Misses what outcomes customers need
- Tools get used partially or incorrectly

**JTBD approach:** "Help regulators stay compliant without manual review"
- Focuses on job (automate compliance)
- Aligns technology with outcomes
- Tools are indispensable

### How the 6 Priorities Enable JTBDs

| JTBD | Priority | Enables |
|------|----------|---------|
| Compliance Verification | 1+2+3 | Soft-fail governance + audit proof |
| Risk Assessment | 5+2+1 | Rule-driven inference + receipt chain |
| Liquidity Management | 6+3+1 | Constraint logic + strict gates |
| Audit Trail | 3+1+4 | Tamper-evident receipts with hashes |
| Auto-Repair | 3+2+1 | Self-healing + proof of repair |

## Files Reference

### fibo-jtbd-governance.mjs
- **Lines:** ~1,200
- **Content:** Complete working implementation
- **Exports:** All 5 JTBD definitions + runFIBOComplianceGovernance()
- **Execution:** `node examples/fibo-jtbd-governance.mjs`

### fibo-ontology-snippet.nt
- **Format:** N-Triples (RDF standard)
- **Namespaces:** FIBO from OMG
- **Content:**
  - Class definitions (Trade, Counterparty, LiquidityAccount, etc.)
  - Property definitions (creditRating, riskLevel, etc.)
  - Enumeration values (AAA, BB, Low, High, etc.)
  - Instance examples (sample trades, counterparties)

### fibo-hooks-config.yaml
- **Format:** YAML configuration
- **Content:**
  - Global settings (receipts, performance, observability)
  - Per-hook configuration
  - Rules and facts file references
  - SHACL shape definitions
  - Monitoring and alerting

## Testing and Validation

The case study includes:
- Input validation using Zod schemas
- SHACL constraint validation
- Receipt integrity verification
- N3 rule inference verification
- Datalog constraint satisfaction

All operations timeout at 5 seconds to detect hangs (Andon principle).

## Advanced Topics

### Receipt Chain Verification

To verify a receipt chain:
1. Take the first receipt (previousReceiptHash = null)
2. Compute its hash using blake3
3. Verify it matches the receiptHash
4. Move to next receipt
5. Verify its previousReceiptHash matches previous receipt's receiptHash
6. Repeat until last receipt

### N3 Rule Debugging

To debug N3 rules:
1. Check if antecedent matches (print matching facts)
2. Verify consequent is correctly formed
3. Use ASK query to check if inference succeeded
4. Inspect final RDF store to verify facts were added

### Datalog Constraint Solving

To debug Datalog constraints:
1. Print all facts at start
2. Trace rule application (which rules fired)
3. Print derived facts after each rule
4. Check goal query result
5. If goal fails, identify unsatisfied constraint

## Extending the Case Study

To add new JTBDs:

1. **Define the customer's job** - What outcome do they need?
2. **Choose priorities** - Which UNRDF features enable this job?
3. **Define validation** - SHACL shape or other constraint
4. **Define effects** - SPARQL, N3, or Datalog consequences
5. **Enable receipts** - Configure receipt chaining
6. **Add to config** - Update fibo-hooks-config.yaml
7. **Test** - Run with sample data

Example:
```javascript
export const myCustomJTBD = {
  meta: {
    id: 'fibo:my-custom-jtbd',
    name: 'My Custom Job',
    description: 'What outcome does the customer need?',
  },
  // validation, effects, receipt config...
};
```

## References

- **FIBO (Financial Industry Business Ontology):** https://www.omg.org/spec/FIBO/
- **JTBD Framework:** Jobs to Be Done approach for product development
- **SHACL (Shapes Constraint Language):** https://www.w3.org/TR/shacl/
- **N3 (Notation 3):** https://www.w3.org/TeamSubmission/n3/
- **Datalog:** Logic programming for constraint satisfaction
- **BLAKE3:** Fast cryptographic hash function

## License

This case study is part of UNRDF and follows the same license terms.

---

**Created:** 2026-04-03  
**Last Updated:** 2026-04-03  
**Version:** 1.0.0
