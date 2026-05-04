# Phase 3: Non-Negotiable 'Mark' Admissibility Plan

## 1. Objective
Transition the `AdmissionEngine` from a "negotiable" gatekeeper (with `strictMode` and `allowWarnings` toggles) to a non-negotiable "Mark" standard admissibility gate. This ensures that all changes to the RDF store ($O$) are either 100% conformant to the current $O^*$ (the compiled world state) or are rejected immediately.

## 2. Removing Negotiable Logic from `AdmissionEngine`

### 2.1 Schema Simplification
- **File:** `src/admission/admission-engine.mjs`
- **Action:** Remove `strictMode` and `allowWarnings` from `AdmissionConfigSchema`.
- **New Schema:**
```javascript
export const AdmissionConfigSchema = z.object({
  // All invariants are now mandatory
  customBounds: z.record(z.number()).optional(),
  customInvariants: z.array(z.function()).optional(),
  auditLog: z.boolean().default(true)
});
```

### 2.2 Decision Logic Consolidation
- **Action:** Remove all `if (!this.config.strictMode)` branches.
- **Outcome:** The engine will treat any invariant failure or guard violation as a terminal `DENY`.
- **Refactoring:**
    - `admitCapsule` will no longer filter failures by strictness.
    - Any failure in `invariantResults.failures` will immediately trigger a denial.

## 3. Refactoring `Verdict` to Single Compiled Bit

### 3.1 Verdict Bit Integration
- **Concept:** The outcome of an admission request is a single bit ($b \in \{0, 1\}$) where $1 = \text{ADMIT}$ and $0 = \text{DENY}$.
- **Action:**
    - Add `verdict: z.number().int().min(0).max(1)` to `DecisionResultSchema`.
    - The `verdict` bit must be derived from the $O^*$ transition validation.

### 3.2 Result Structure Refinement
```javascript
export const DecisionResultSchema = z.object({
  verdict: z.number().int().min(0).max(1), // 1 for ALLOW, 0 for DENY
  receiptHash: z.string(), // Cryptographic link to O*
  timestamp: z.string().datetime(),
  // Full details remain in the receipt/audit log, but the 'verdict' is the primary signal
  details: z.record(z.any()).optional()
});
```

## 4. Migrating `SecuritySandbox` to Capability-Based URI Predicates

### 4.1 From Strings to URIs
- **File:** `packages/atomvm/src/vm/sandbox.mjs`
- **Action:** Replace the `UNSAFE_OPCODES` string set with a URI-based capability map.
- **Mapping Example:**
    - `SYS_EXEC` $\rightarrow$ `https://unrdf.io/capabilities/system/execute`
    - `FS_WRITE_RAW` $\rightarrow$ `https://unrdf.io/capabilities/fs/write-raw`
    - `NET_LISTEN` $\rightarrow$ `https://unrdf.io/capabilities/net/listen`

### 4.2 Predicate Evaluation Refactor
- **Action:** Update `interceptOpcodes` to resolve opcodes to their capability URIs.
- **Predicate Check:**
```javascript
// New evaluation logic
const capabilityUri = OP_TO_CAPABILITY_MAP[op.name];
if (!this._evaluateCapability(capabilityUri)) {
  throw new ConstitutionalViolationError(capabilityUri);
}
```

## 5. Adversarial Review

### 5.1 Critique: "Is a single compiled bit too reductive for complex enterprise debugging?"
**Defense:** The single bit is the **operational interface** for automated systems (CI, runtime, orchestration). It eliminates ambiguity in automated decision paths. For human debugging, the **Receipt** (linked via `receiptHash`) contains the full trace, failed invariants, and state diffs. We are separating the *decision signal* from the *diagnostic evidence*.

### 5.2 Critique: "How do we handle legitimate but 'nonconformant' emergency overrides without 'talking chicken'?"
**Defense:** Under the 'Mark' standard, there is no "negotiation" with the engine. An emergency override is not a "lenient" flag; it is a **Policy Update Transaction**. If a developer needs to perform a forbidden operation in an emergency, they must first commit a signed Delta that updates the policy to grant that capability (potentially with a TTL). The `AdmissionEngine` remains non-negotiable, but the *rules* it enforces are dynamic and auditable. "Talking chicken" is replaced by "Explicit Governance."

## 6. Migration Steps
1.  **Draft URIs:** Define the standard capability URI set for all AtomVM opcodes.
2.  **Engine Update:** Remove `strictMode` and `allowWarnings` from `AdmissionEngine` and `MonorepoAdmissionEngine`.
3.  **Verdict Update:** Implement the 0/1 verdict bit in all admission return paths.
4.  **Sandbox Update:** Refactor `SecuritySandbox` to use URI predicates.
5.  **Validation:** Run `monorepo-admission.test.mjs` to ensure all previous "lenient" tests now fail as expected.
