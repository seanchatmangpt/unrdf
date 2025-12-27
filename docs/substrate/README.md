# KGC-Claude Substrate Documentation

**@unrdf/kgc-claude** - Deterministic run objects, universal checkpoints, bounded autonomy, and multi-agent concurrency for Claude integration.

## Overview

The KGC-Claude substrate turns Claude into a replaceable actuator and moves correctness, continuity, and scale into the substrate itself. This package provides the foundational primitives for integrating Claude AI with the Knowledge Graph Construction (KGC) framework in a deterministic, auditable, and scalable way.

## Core Innovations

### 1. Deterministic Run Objects (Δ_run capsules)

Every Claude run becomes a first-class capsule with normalized tool traces, artifacts, and admission control via `preserve(Q) ∧ Δ_run ∉ H`.

### 2. Universal Checkpointing

Surface-agnostic freeze/thaw with receipts: `freeze: O_t → ⟨snapshot, receipt_t, hash(μ(O_t))⟩`. Portable across CLI/IDE/MCP surfaces with rollback independent of Claude features.

### 3. Bounded Autonomy

Explicit budget/capacity per epoch (C_τ) with hard stops and denial receipts. Guards enforce limits regardless of "autonomy mode."

### 4. Multi-Agent Concurrency

Shard deltas and merge deterministically: `μ(O ⊔ Δ₁ ⊔ Δ₂) = μ(O ⊔ Δ₁) ⊔ μ(Δ₂)`. Conflicts resolved by law (Λ), not negotiation.

### 5. Async Workflow Primitives

WorkItem nodes in O with completion as state transition. Receipts for every step.

### 6. Surface Projections

Single source of truth, multiple views: `UI = Π_ui(μ(O))`, `CLI = Π_cli(μ(O))`. Missing features become missing projections.

## Documentation Structure (Diataxis)

This documentation follows the [Diataxis](https://diataxis.fr/) framework for systematic technical documentation:

### Learning-Oriented: [Tutorial](./tutorial.md)

**Getting Started with KGC-Claude Substrate** - Step-by-step guide for first-time users to create their first run capsule, checkpoint, and multi-agent workflow.

### Task-Oriented: [How-To Guides](./how-to/)

Practical guides for specific tasks:

- [How to Create and Persist Run Capsules](./how-to/run-capsules.md)
- [How to Use Checkpoints for State Management](./how-to/checkpoints.md)
- [How to Configure Autonomy Guards](./how-to/autonomy-guards.md)
- [How to Implement Multi-Agent Concurrency](./how-to/multi-agent.md)
- [How to Build Async Workflows](./how-to/async-workflows.md)
- [How to Create Surface Projections](./how-to/projections.md)
- [How to Verify Receipt Chains](./how-to/verify-receipts.md)

### Information-Oriented: [Reference](./reference.md)

**API Reference** - Complete type definitions, function signatures, schemas, and return values for all exported modules.

### Understanding-Oriented: [Explanation](./explanation.md)

**Conceptual Documentation** - Deep dives into architectural decisions:

- Why Immutable Logs
- Why Deterministic Hashing
- Why Receipt Chains
- Why Bounded Autonomy
- Why Multi-Agent Sharding

## Quick Start

```bash
# Install
pnpm add @unrdf/kgc-claude

# Basic usage
import { createSubstrate } from '@unrdf/kgc-claude';
import { KGCStore, GitBackbone } from '@unrdf/kgc-4d';

const store = new KGCStore();
const git = new GitBackbone('/path/to/repo');
const substrate = createSubstrate(store, git);

// Create a run with autonomy checking
const run = substrate.createRun();
run.addToolCall({ name: 'Read', input: { file: 'foo.txt' } });

const check = await substrate.guard.check(run.getMetrics());
if (check.allowed) {
  const capsule = await run.seal();
  await substrate.persist(capsule);
}
```

## Package Information

- **Version**: 5.0.0
- **License**: MIT
- **Repository**: [@unrdf/kgc-claude](https://github.com/seanchatmangpt/unrdf/tree/main/packages/kgc-claude)
- **Dependencies**: @unrdf/core, @unrdf/oxigraph, @unrdf/kgc-4d, @unrdf/yawl

## Key Concepts

- **Run Capsule**: Immutable record of a single Claude execution with tool traces and artifacts
- **Checkpoint**: Snapshot of universe state with cryptographic receipt for rollback
- **Autonomy Guard**: Budget-based rate limiter for safe autonomous operation
- **Shard**: Isolated scope for concurrent agent operations
- **WorkItem**: Async task representation in the knowledge graph
- **Projection**: Transformation of universe state for specific surfaces (CLI, UI, API)

## Related Documentation

- [KGC-4D Core Documentation](../README.md)
- [KGC Markdown Specification](../KGC-MARKDOWN-SPECIFICATION.md)
- [Architecture Overview](../ARCHITECTURE.md)

## Need Help?

- See the [Tutorial](./tutorial.md) for a hands-on introduction
- Check [How-To Guides](./how-to/) for specific tasks
- Consult the [API Reference](./reference.md) for detailed function documentation
- Read [Explanations](./explanation.md) to understand design decisions
