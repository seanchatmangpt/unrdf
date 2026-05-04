# Phase 4: Cognitive Breed Morphology Plan

## 1. Overview
Introduce "Breeds" to the UNRDF ecosystem to allow specialization of generated artifacts based on the target environment's cognitive and resource constraints. This prevents "one-size-fits-all" bloat and optimizes for specific runtime profiles (e.g., AtomVM vs. Distributed Swarms).

## 2. Morphological Profiles

### 2.1 The 'Dachshund' Breed
*   **Target**: Embedded systems, AtomVM, resource-constrained environments.
*   **Core Philosophy**: "Low profile, high tenacity." Minimal footprint, direct execution.
*   **Key Features**:
    *   Single-node optimization.
    *   Synchronous execution paths to reduce context switching.
    *   L4 Hardening (Constitutional Admissibility) enabled by default.
    *   Omit heavy observability libraries; use direct logging.
*   **Runtime**: AtomVM / Bare Metal.

### 2.2 The 'Shepherd' Breed
*   **Target**: High-coordination environments, multi-agent swarms, distributed clusters.
*   **Core Philosophy**: "Vigilant coordination, collective intelligence."
*   **Key Features**:
    *   Multi-agent coordination protocols (Vector Clocks, Gossip).
    *   Consensus-aware state mutations (Paxos/Raft placeholders).
    *   L5 Hardening (Causal Consistency) enforced.
    *   Full OTel (OpenTelemetry) tracing and OCEL event logging.
*   **Runtime**: Node.js / Deno / Distributed Clusters.

## 3. Implementation Plan

### 3.1 Schema Updates (`packages/cli/src/cli/commands/sync/schemas.mjs`)
*   Update `ProjectConfigSchema` to include an optional `breed` field.
*   Update `SyncArgsSchema` to include a `--breed` parameter.
```javascript
export const ProjectConfigSchema = z.object({
  // ... existing fields
  breed: z.enum(['dachshund', 'shepherd']).default('dachshund'),
}).strict();

export const SyncArgsSchema = z.object({
  // ... existing fields
  breed: z.enum(['dachshund', 'shepherd']).optional(),
}).strict();
```

### 3.2 CLI Updates (`packages/cli/src/cli/commands/sync.mjs`)
*   Register the `--breed` argument in `defineCommand`.
*   Implement priority logic: CLI Argument > `unrdf.toml` Config > Default ('dachshund').

### 3.3 Orchestrator Updates (`packages/cli/src/cli/commands/sync/orchestrator.mjs`)
*   Inject the resolved `breed` value into the `renderWithOptions` context.
*   This ensures all templates have access to the `breed` variable.

### 3.4 Template Updates (`ostar/templates/projected-artifact.njk`)
*   Add conditional logic to toggle features.
*   **Example (MCP Tool Artifact):**
```jinja2
{% if breed == 'shepherd' %}
import { VectorClock, DistributedConsensus } from '@unrdf/kgc-4d';
{% endif %}

// ...

async function handler(input, context) {
  {% if breed == 'shepherd' %}
  const vc = new VectorClock(context.nodeId);
  // ... coordination logic
  {% else %}
  // Dachshund: Direct execution
  return await executeDirect(input);
  {% endif %}
}
```

## 4. Adversarial Review

### 4.1 Critique: "Does breed specialization create incompatible sub-ecosystems?"
**Response**: No. The breed specialization is a *projection-time* decision, not an *ontology-time* decision. Both breeds operate on the same RDF schemas and SHACL shapes. A 'Dachshund' node on an ESP32 can communicate with a 'Shepherd' node in the cloud because they share the same semantic DNA. The breed only dictates the *scaffolding* around the logic.

### 4.2 Critique: "Is 'Shepherd' coordination too heavy for current AtomVM limitations?"
**Response**: Yes, and that is intentional. The current AtomVM implementation (as found in Explore Gap 9) struggles with high-concurrency event loops and heavy memory overhead from large libraries like OTel. By formalizing the 'Dachshund' breed, we provide a safe path for AtomVM users that strips away these heavy layers while keeping the core semantic integrity. 'Shepherd' is explicitly aimed at environments where coordination overhead is a feature, not a bug.

## 5. Deliverables
1.  Modified `schemas.mjs` with `breed` support.
2.  Modified `sync.mjs` with `--breed` CLI option.
3.  Updated `projected-artifact.njk` with conditional breed logic.
4.  Documentation update in `DEVELOPER_GUIDE.md` explaining breed selection.
