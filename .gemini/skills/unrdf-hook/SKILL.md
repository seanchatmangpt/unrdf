---
name: unrdf-hook
description: Governs the Knowledge Hook execution engine. Use for registering, validating, and debugging hooks, or applying semantic bridge conditions.
---
# UNRDF Knowledge Hooks

## Overview
The Hook Engine is the autonomic nervous system of UNRDF.

## Hook Development
- **Define**: Use `manager.registerHook({...})`.
- **Eval**: Use `condition-evaluator.mjs` to register `semantic-inference` conditions.
- **Test**: See `packages/hooks/test/` for E2E integration patterns.

## Semantic Hooks
- Fused with Open Ontologies engine.
- Every hook execution evaluates SPARQL queries via the `@unrdf/core/utils/semantic-bridge`.
- Must return `valid: boolean`.
