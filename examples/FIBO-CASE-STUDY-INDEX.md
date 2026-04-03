# FIBO JTBD Governance Case Study - File Index

## Quick Start

```bash
# Run the complete case study
node examples/fibo-jtbd-governance.mjs
```

**Expected output:** Shows all 5 JTBDs passing, demonstrating all 6 priorities.

## Files in This Case Study

### 1. fibo-jtbd-governance.mjs (1,179 lines, 37 KB)

**Purpose:** Main executable example with all 5 Jobs to Be Done implementations.

**Content:**
- Complete FIBO ontology namespace definitions
- Zod validation schemas for trades, counterparties, liquidity accounts
- 5 export JTBD definitions:
  1. `complianceVerificationJTBD` - SHACL + SPARQL CONSTRUCT + withReceipt
  2. `counterpartyRiskJTBD` - N3 rules + SPARQL CONSTRUCT + withReceipt
  3. `liquidityManagementJTBD` - Datalog + SHACL + withReceipt + SPARQL CONSTRUCT
  4. `auditTrailJTBD` - SHACL block mode + withReceipt + hash receipts
  5. `regulatoryRepairJTBD` - SHACL repair mode + SPARQL CONSTRUCT + withReceipt
- `runFIBOComplianceGovernance()` function - Executes all JTBDs with sample data

**Demonstrates:**
- Priority 1: withReceipt integration (cryptographic audit trail)
- Priority 2: sparql-construct effects (RDF transformations)
- Priority 3: SHACL enforcement modes (annotate/block/repair)
- Priority 4: Input/output hash receipts (state proof)
- Priority 5: N3 forward-chaining rules (regulatory inference)
- Priority 6: Datalog logic programming (constraint satisfaction)

**Run:** `node examples/fibo-jtbd-governance.mjs`

---

### 2. fibo-ontology-snippet.nt (189 lines, 16 KB)

**Purpose:** FIBO ontology definitions in N-Triples format.

**Format:** RDF N-Triples (one statement per line, machine-readable)

**Content:**
- Class definitions (Trade, FinancialTransaction, Counterparty, LiquidityAccount, ComplianceAuditEntry)
- Property definitions (txType, value, counterparty, status, creditRating, riskLevel, etc.)
- Enumeration values (AAA, BB, Low, High, Pending, Settled, Compliant, etc.)
- Instance examples (sample trades, counterparties, liquidity accounts)

**Usage:**
- Load into RDF store before executing hooks
- Referenced by the case study for ontology context
- Can be extended with additional rules and constraints

**Format Details:**
- Triple format: `<subject> <predicate> <object> .`
- Each statement ends with period
- IRIs enclosed in angle brackets
- Literals with datatype: `"value"^^<http://www.w3.org/2001/XMLSchema#string>`

**Load Example:**
```bash
# Load N-Triples into RDF store
rdf load examples/fibo-ontology-snippet.nt
```

---

### 3. fibo-hooks-config.yaml (447 lines, 12 KB)

**Purpose:** YAML configuration file for knowledge hooks governance.

**Format:** YAML (human-readable configuration)

**Content:**
- Global configuration (receipts, performance, observability)
- Per-JTBD hook configuration with:
  - Validation (SHACL shapes and enforcement modes)
  - Inference rules (N3, Datalog)
  - Effects (SPARQL CONSTRUCT queries)
  - Receipts (blake3, git-notes, chaining)
  - Channel routing (graph selection, view mode)
- Referenced rule and fact files
- SHACL shape definitions
- Monitoring and alerting configuration
- Testing suites configuration

**Structure:**
```yaml
version: "1.0.0"
global:
  receipts:
    enabled: true
    algorithm: blake3
    anchor: git-notes
    chain: true

hooks:
  - id: "fibo:verify-regulatory-compliance"
    name: "Verify Regulatory Compliance"
    validation: { ... }
    effects: [ ... ]
    receipt: { ... }
```

**Usage:**
- Can be loaded by a hook engine to configure governance
- Separates configuration from code
- Enables environment-specific tuning
- Documents expected behavior

---

### 4. FIBO-JTBD-GOVERNANCE-README.md (592 lines, 19 KB)

**Purpose:** Comprehensive documentation of the entire case study.

**Content:**
- **Overview:** What JTBD is, why it matters for finance
- **The 6 Priorities:**
  1. withReceipt - Cryptographic audit trails
  2. SPARQL CONSTRUCT - RDF transformations
  3. SHACL - Three enforcement modes explained
  4. Hash Receipts - Input/output proof
  5. N3 Rules - Forward-chaining inference
  6. Datalog - Logic programming constraints
- **5 JTBDs Explained:**
  - Each JTBD's customer job
  - Governance pattern
  - Priorities used
  - Process flow
  - Example rules/queries
- **Running the Case Study:** Step-by-step execution
- **Key Insights:** Why this matters for finance
- **Files Reference:** What each file contains
- **Testing and Validation:** How to verify correctness
- **Advanced Topics:** Receipt verification, rule debugging, constraint solving
- **Extending the Case Study:** How to add new JTBDs
- **References:** Links to FIBO, SHACL, N3, Datalog

**Read this for:** Understanding the entire framework, why each priority matters, how to extend the case study.

---

### 5. FIBO-CASE-STUDY-INDEX.md (This File)

**Purpose:** Quick reference guide to all files in the case study.

**Content:** File-by-file breakdown with quick navigation.

---

## The 6 Priorities at a Glance

| Priority | Demonstrates | Used In | Key Feature |
|----------|--------------|---------|------------|
| 1 | withReceipt | All JTBDs | BLAKE3 hash chains |
| 2 | sparql-construct | JTBD 1,2,3,5 | RDF state transformation |
| 3 | SHACL | JTBD 1,3,4,5 | annotate/block/repair modes |
| 4 | Hash Receipts | JTBD 4 | Input→Output proof |
| 5 | N3 Rules | JTBD 2 | Forward-chaining inference |
| 6 | Datalog | JTBD 3 | Constraint satisfaction |

## The 5 JTBDs at a Glance

| JTBD | Job | Customer | Outcome |
|------|-----|----------|---------|
| 1 | Verify Regulatory Compliance | Compliance Officer | Trade marked compliant with audit proof |
| 2 | Assess Counterparty Risk | Risk Officer | Risk level assigned via inference rules |
| 3 | Manage Liquidity Positions | Treasury Manager | Liquidity status calculated with constraints |
| 4 | Maintain Compliance Audit Trail | Regulator/Auditor | Tamper-evident receipt chain created |
| 5 | Auto-Repair Compliance Violations | Compliance Bot | Violations auto-fixed with proof |

## Key Insights

### Why JTBD Matters

**Traditional:** "Build a compliance tool"
- Focuses on features
- Results in unused functionality

**JTBD:** "Help compliance officers stay compliant without manual review"
- Focuses on outcomes
- Results in indispensable tools

### Why the 6 Priorities Work Together

- **Priority 1** (withReceipt) provides audit trail infrastructure
- **Priority 2** (SPARQL) captures governance decisions as RDF facts
- **Priority 3** (SHACL) enforces constraints with appropriate strictness
- **Priority 4** (Hash Receipts) proves state transitions
- **Priority 5** (N3) expresses regulatory rules naturally
- **Priority 6** (Datalog) solves compliance constraints deterministically

Together: **Complete financial governance platform with cryptographic proof**.

## Testing the Case Study

### Verify the Code Runs

```bash
timeout 10s node examples/fibo-jtbd-governance.mjs
```

Expected output: All 5 JTBDs pass with status = PASS

### Verify the Schemas

All data is validated with Zod:
- Trades (id, type, value, counterparty, status, timestamp)
- Counterparties (id, name, creditRating, defaultHistory, regulatoryStatus)
- Liquidity Accounts (accountId, available, reserved, threshold, riskLevel)

### Verify the Rules Work

- **N3 Rules:** 6 rules for counterparty risk assessment
- **Datalog Rules:** 7 rules for liquidity compliance
- **SHACL Shapes:** 4 shapes with different enforcement modes

## Extending the Case Study

### Add a New JTBD

1. Decide customer job (outcome they need)
2. Choose which priorities to use
3. Define validation (SHACL or other)
4. Define effects (SPARQL, N3, or Datalog)
5. Configure receipts
6. Test with sample data

### Example: "Detect Fraud"

```javascript
export const fraudDetectionJTBD = {
  meta: {
    id: 'fibo:detect-fraud',
    name: 'Detect Fraud Patterns',
  },
  // N3 rules to detect anomalies
  inference: { kind: 'n3', rules: '...' },
  // SPARQL to flag suspicious trades
  effects: [{ kind: 'sparql-construct', query: '...' }],
  // Receipt proof of detection
  receipt: { enabled: true, algorithm: 'blake3', chain: true }
};
```

## References

- **FIBO:** https://www.omg.org/spec/FIBO/
- **JTBD Framework:** Jobs to Be Done approach
- **SHACL:** https://www.w3.org/TR/shacl/
- **N3:** https://www.w3.org/TeamSubmission/n3/
- **Datalog:** Logic programming
- **BLAKE3:** https://blake3.io/

---

**Version:** 1.0.0  
**Created:** 2026-04-03  
**Status:** Production-ready case study  
