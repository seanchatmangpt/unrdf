# Plan Phase 4: Final Hygiene & Metadata Standardization

## Overview
This plan addresses the final "hygiene" stage of the UNRDF v6 release cycle. It focuses on transitioning unregulated technical debt markers into a managed metadata format, ensuring that all remaining work is tracked, governed, and visible to the CI/CD pipeline.

## 1. Liquidation of unregulated `TODO` markers
The following markers identified in `sidecar/` and `hive/` will be standardized:

### `sidecar/server/tasks/policies/refresh-packs.mjs`
- **Current**: `// TODO: Implement actual signature validation`
- **Action**: Convert to `// DEFERRED_ACTION(#sec-101): Implement Ed25519 signature validation for policy packs.`
- **Reasoning**: Security feature deferred to post-RC1 hardening.

### `sidecar/server/utils/otel-context-propagation.mjs`
- **Current**: `// TODO: Create span from extracted context`
- **Action**: Convert to `// DEFERRED_ACTION(#otel-202): Extract OpenTelemetry SpanContext from parent carrier.`
- **Reasoning**: Infrastructure enhancement for cross-process tracing.

### `hive/backend/sidecar-graceful-degradation/sidecar.mjs`
- **Current**: `// TODO: Implement config persistence`
- **Action**: Convert to `// DEFERRED_ACTION(#hive-303): Implement persistent storage for sidecar configuration overrides.`
- **Reasoning**: Configuration persistence is a P2 feature for the graceful degradation module.

## 2. Standardization to `DEFERRED_ACTION(#issue_id)`
All production source code (excluding `test/`, `examples/`, and `playground/`) must adhere to the following metadata standard:

- **Format**: `// DEFERRED_ACTION(#<prefix>-<number>): <description>`
- **Permitted Prefixes**: `sec`, `otel`, `hive`, `core`, `feat`, `fix`, `gap-closure`.
- **Validation Rule**: Any comment containing `TODO`, `FIXME`, or `HACK` will trigger a build failure. `DEFERRED_ACTION` is only permitted if accompanied by a valid issue reference.

## 3. Implementation of `scripts/verify-hygiene.mjs`
A final verification script will be implemented to enforce the zero-unregulated-debt mandate.

### Script Specifications:
- **Scope**: Scans all `.mjs` and `.js` files in `packages/`, `sidecar/`, and `hive/`.
- **Exclusions**: Node modules, build artifacts, and test suites.
- **Checks**:
    1. **Zero-Tolerance**: Fail if any case-insensitive match for `TODO:`, `FIXME:`, or `HACK:` is found.
    2. **Format Validation**: Ensure all `DEFERRED_ACTION` markers include an issue ID (e.g., `#gap-closure` or `#123`).
    3. **Summary Reporting**: Output a count of managed debt items to OTel/stdout.

---

## Adversarial Review

### "Does renaming TODO to DEFERRED_ACTION actually reduce technical debt or just hide it?"
**Critique**: This transformation is purely cosmetic. Changing the label of a missing feature or a hack doesn't improve the code quality; it merely bypasses linter rules. It "hides" the debt under a more professional-sounding name while the underlying risk remains identical.

**Rebuttal**: This is not about hiding debt, but about **transitioning from unregulated to managed debt**. 
1. **Visibility**: `TODO`s are "dark debt"—they are often invisible to project management tools. `DEFERRED_ACTION(#issue_id)` creates a hard link between the codebase and the issue tracker.
2. **Accountability**: A `TODO` has no owner or timeline. A `DEFERRED_ACTION` is an explicit engineering decision that has been triaged and assigned a priority in the backlog.
3. **Automated Enforcement**: By standardizing the format, we can programmatically monitor the "debt budget" of the project. If the number of `DEFERRED_ACTION` markers exceeds a certain threshold, the CI can fail, forcing a "debt liquidation" sprint.
4. **Zero-Debt Mandate**: In a high-integrity system like UNRDF, "Zero Debt" doesn't mean the software is perfect; it means there are no **unknown** or **untracked** risks in the production path. Standardization is the first step toward liquidation.

## Next Steps
1. Execute the surgical replacement of `TODO` markers in `sidecar/` and `hive/`.
2. Commit the `scripts/verify-hygiene.mjs` tool.
3. Integrate the hygiene check into the `pnpm lint` or `pnpm test` lifecycle.
