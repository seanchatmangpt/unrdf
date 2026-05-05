# Supplementary Documentation: Gap Closure Modules

## 1. Governance & Protocol Compliance
This document serves as the regulatory overlay for the Gap Closure Plan, ensuring that moving from "research" to "production" adheres to the project's strict architectural mandates.

### Protocol: "Research-to-Production" (R2P)
All modules originating in `~/speckit-ralph` or `/experiments` that target integration into the production `@unrdf` monorepo must undergo the **R2P Certification Process**:

1.  **Contractual Definition**: The component must expose a public interface defined in a TDD-contractual style (Chicago School).
2.  **Zero-Marker Verification**: All internal `TODO`s must be resolved into formal GitHub issues and linked in the documentation.
3.  **Holographic Integration**: The module must integrate with `otel` for observability and `kgc-4d` for state persistence.
4.  **Regression Baseline**: A dedicated regression test suite must be established in the target module's `test/` directory.

## 2. Distributed State Reconciliation (DSR) Protocol
For the Ralph Loop agents (`BusinessOS`, `Statistical Engine`, etc.) to interface with `kgc-4d`, the following DSR protocol must be enforced:

- **Vector Clock Propagation**: All incoming payloads from external agents must include a `VectorClock` header to ensure causal ordering within the L5 graph.
- **Atomic Mutation Buffer**: All mutations must be wrapped in `store.appendEvent(eventData, deltas)` to guarantee ACID compliance.
- **Snapshot Alignment**: Agents requesting time-travel queries must use the `reconstructState` engine, ensuring the target time aligns with an indexed temporal snapshot.

## 3. Automation Metadata Standard
To replace the prohibited `TODO` marker in `.claude/` automation scripts, we adopt the following standard:

- **Prohibited**: `// TODO: ...`
- **Mandated**: `// DEFERRED_ACTION(issue_id): ...`
  - *Example*: `// DEFERRED_ACTION(#123): Implement OAuth2 callback handling.`
- **Tooling**: CI/CD pipelines and the project linter are configured to ignore `DEFERRED_ACTION` tags, preventing them from flagging these as technical debt, while ensuring they remain discoverable for future sprints.

## 4. Verification Procedures
To verify gap closure success, the following commands are mandated for the `RALPH_LOOP_SYNC` job:
```bash
# Verify no production technical debt
grep -rE "(TODO|FIXME|HACK)" packages/ --include="*.mjs" --exclude-dir=node_modules

# Verify automation integrity
grep -rE "TODO" .claude/ --include="*.md"
```
EOF
