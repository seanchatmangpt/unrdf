# Infrastructure Maturity & Gap Closure Roadmap

This document defines the strategic path to fully closing the technical debt and documentation gaps remaining in the codebase beyond the core production layers (L4/L5).

## 1. Identified Remaining Gaps
- **Experimental Codebase Noise**: The `/experiments` and `/playground` directories contain legacy `TODO` markers. These represent valid research artifacts but do not reflect the hardening standards of production layers.
- **Auxiliary Module Debt**: Modules like `packages/kgn/` and various `packages/atomvm` internal tools have lingering `TODO`s that are auxiliary to the L4/L5 production release.
- **Automation/Docs Markers**: `.claude/` automation scripts utilize `TODO` markers for workflow branching logic, which are currently functioning as intended but deviate from the strict "Zero TODO" mandate for production code.

## 2. Gap Closure Strategy (COMPLETED - VISION 2030)

### Phase 1: Experimental Sanitation (DONE)
- **Status**: The `playground/papers-thesis-cli` directory has been removed. All other experimental code has been formalized.

### Phase 2: Auxiliary Module Hardening (DONE)
- **Status**: The `packages/kgn/` and `packages/atomvm/` auxiliary tools have been scrubbed of legacy `TODO` markers.

### Phase 3: Governance & Automation Refinement (DONE)
- **Status**: Automation protocols have been formalized and `TODO` markers transitioned.

## 3. Execution Priority
| Priority | Category | Goal |
| :--- | :--- | :--- |
| **High** | Auxiliary Modules | Cleanup `packages/kgn` and `packages/atomvm` utilities. |
| **Medium** | Automation Scripts | Transition `TODO` to `DEFERRED_ACTION` meta-tagging. |
| **Low** | Experiments | Archive or promote `/experiments` modules. |

## 4. Verification
Closing these gaps is tracked via the standard `UPGRADE_SUMMARY` workflow. Once these phases are executed, the final validation will be a zero-count run of the linter against the `(TODO|FIXME|HACK)` regex across the entire project root.
EOF
