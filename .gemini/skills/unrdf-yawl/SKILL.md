---
name: unrdf-yawl
description: Orchestrates YAWL workflow definitions, instance patterns, and task validation. Use for multiple-instance patterns (WP12-14) or checking YAWL-specific constraints.
---
# YAWL Workflow Orchestration

## Overview
YAWL handles complex process state machines.

## Core Workflows
- **Multiple Instance Patterns**: Implemented in `packages/yawl/src/multiple-instance/`.
- **Validation**: Every task transition is subject to `TaskDefinitionSchema` constraints.

## Best Practices
- When defining a `semanticCondition` in a `TaskDefinition`, use the SPARQL bridge.
- Always verify `TaskInstance` status via `yawl-receipts.mjs`.
