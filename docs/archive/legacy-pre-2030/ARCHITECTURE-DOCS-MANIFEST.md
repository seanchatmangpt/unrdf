# Architecture & Infrastructure Documentation: @unrdf/atomvm and @unrdf/kgc-4d

## Overview
This document outlines the architectural principles, testing methodologies, and production-ready guarantees for the `@unrdf/atomvm` (L4) and `@unrdf/kgc-4d` (L5) layers.

## 1. Architectural Principles
- **Chicago School (Outside-In) TDD**: All development is driven by public-facing behavioral contracts. Implementation details are kept internal to promote encapsulation and maintainability.
- **ACID Atomicity**: Event sourcing in `kgc-4d` ensures that every state transition is atomic. Mutations to the universe state are only committed if the associated EventLog entry is successfully persisted.
- **Holographic Verification**: We utilize formal property verification for critical paths (e.g., receipt validation, Merkle proof structures) to prove correctness beyond standard unit testing.

## 2. Testing Infrastructure
- **Structural Mandate**: All test suites must reside in `test/` directories and utilize `vitest`.
- **Chicago Style**: Tests must define `describe` blocks mapping to system behaviors and `it` blocks testing outcomes against public APIs.
- **Regression Suites**: 
  - **CFE-IEC Stress Test**: A dedicated regression test (`cfe-iec-loop.test.mjs`) simulates distributed packet traversal across the hierarchical KGC infrastructure.
  - **Temporal Rehydration**: Time-travel reconstruction is validated for snapshot selection, delete operations, and large-scale replay.

## 3. Production Readiness & Maintenance
- **Placeholder Hygiene**: Production code is strictly free of `TODO`, `FIXME`, or generic placeholder comments. Any technical debt or deferred tasks must be tracked as formal GitHub issues.
- **Doctest Integrity**: Documentation examples in `README.md` and JSDoc blocks are treated as integration tests. They must be kept functional and synchronized with the source code implementation.
- **Deployment**: Verify the system against the latest `UPGRADE_SUMMARY_2026-04-26.md` prior to any production roll-out.

## 4. Operational Guidelines
- **Adding Functionality**:
  1. Define the behavioral contract in a new `it` block.
  2. Implement the minimum logic required for the test to pass.
  3. Refactor internally while maintaining test parity.
- **Debugging**: Use the `kgc-4d` rehydration engine and EventLog statistics to inspect state at specific temporal points.
