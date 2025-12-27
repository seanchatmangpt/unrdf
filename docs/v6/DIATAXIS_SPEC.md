# UNRDF V6 Diataxis Documentation Specification

**Version:** 6.0.0-alpha.1  
**Last Updated:** 2025-12-27  
**Status:** Draft

## Overview

This specification defines the structure, conventions, and requirements for UNRDF package documentation following the **Diataxis framework**.

### Diataxis Framework

Every package MUST provide documentation in four categories:

1. **Tutorials** - Learning-oriented, hands-on introduction
2. **How-to Guides** - Task-oriented, problem-solving guides
3. **Reference** - Information-oriented, technical specifications
4. **Explanation** - Understanding-oriented, conceptual deep dives

---

## Directory Structure

Each package documentation MUST follow this structure:

```
packages/{package-name}/
├── README.md                    # Package overview + quick start
├── docs/
│   ├── index.md                # Documentation hub
│   ├── tutorials/
│   │   ├── README.md           # Tutorial index
│   │   ├── 01-getting-started.md
│   │   ├── 02-basic-usage.md
│   │   └── ...
│   ├── how-to/
│   │   ├── README.md           # How-to index
│   │   ├── 01-task-one.md
│   │   ├── 02-task-two.md
│   │   └── ...
│   ├── reference/
│   │   ├── README.md           # Reference index
│   │   ├── api.md              # API documentation
│   │   ├── configuration.md    # Config options
│   │   └── ...
│   └── explanation/
│       ├── README.md           # Explanation index
│       ├── architecture.md     # System design
│       ├── concepts.md         # Core concepts
│       └── ...
```

---

## Required Sections by Category

### 1. Tutorials

**Purpose:** Guide learners through completing a project

**Required Elements:**
- Title (action-oriented: "Build...", "Create...", "Learn...")
- Learning objectives (what user will be able to do)
- Prerequisites checklist
- Step-by-step instructions (numbered)
- Complete working example
- Expected output
- "What you learned" summary
- Next steps (links to related content)
- Troubleshooting section

**Frontmatter:**
```yaml
---
title: "Tutorial: [Action-Oriented Title]"
category: "tutorials"
audience: "beginner|intermediate|advanced"
version: "6.0.0-alpha.1"
estimated_time: "XX minutes"
prerequisites:
  - "Prerequisite 1"
  - "Prerequisite 2"
proof:
  source: ["path/to/code.mjs", "path/to/test.mjs"]
  hash: "sha256-hash-of-content"
  confidence: 0.95
---
```

**Example:**
```markdown
# Tutorial: Create Your First Receipt-Driven Store

> Time Required: 15 minutes  
> Difficulty: Beginner

## What You'll Build

A KGC-4D store that generates receipts for all operations.

## What You'll Learn

1. Initialize a receipt-driven store
2. Add RDF data with proof tracking
3. Generate and verify Merkle receipts
4. Freeze and audit store state

[...detailed steps...]
```

---

### 2. How-to Guides

**Purpose:** Solve specific problems quickly

**Required Elements:**
- Problem statement (clear and specific)
- Use cases ("Use this when you need to...")
- Prerequisites
- Solution steps (concise, actionable)
- Complete working example
- Variations (common alternatives)
- Troubleshooting
- Best practices
- Links to related guides and reference

**Frontmatter:**
```yaml
---
title: "How to [Solve Specific Problem]"
category: "how-to"
audience: "intermediate"
version: "6.0.0-alpha.1"
estimated_time: "X minutes"
proof:
  source: ["path/to/implementation.mjs"]
  hash: "sha256-hash"
  confidence: 0.90
---
```

**Example:**
```markdown
# How to Validate Merkle Receipts

## Problem

You need to verify that a receipt authentically represents a store operation.

## Use Cases

- Audit trail verification
- Compliance checking
- Third-party validation

## Solution

[...concise steps with code...]

## Variations

### With External Validator
[...alternative approach...]
```

---

### 3. Reference

**Purpose:** Provide technical specifications and API details

**Required Elements:**
- Module overview
- Import paths
- Function signatures (JSDoc format)
- Parameter tables
- Return value specifications
- Error conditions
- Type definitions
- Constants
- Usage examples (minimal, focused)
- Links to how-to guides and explanations

**Frontmatter:**
```yaml
---
title: "[Module/Function] Reference"
category: "reference"
audience: "all"
version: "6.0.0-alpha.1"
source: "src/path/to/module.mjs"
proof:
  source: ["src/receipts/index.mjs"]
  hash: "sha256-hash"
  confidence: 1.00
---
```

**Example:**
```markdown
# Receipt API Reference

Brief one-line description.

## Import

```javascript
import { generateReceipt } from '@unrdf/v6-core/receipts';
```

## API

### generateReceipt()

Generates a Merkle receipt for a store operation.

**Signature:**

```javascript
/**
 * @param {Object} operation - Store operation
 * @param {Store} operation.store - KGC store
 * @param {string} operation.type - Operation type
 * @returns {Promise<Receipt>}
 * @throws {ValidationError}
 */
async function generateReceipt(operation) {}
```

[...parameter tables, examples...]
```

---

### 4. Explanation

**Purpose:** Deepen understanding of concepts and design decisions

**Required Elements:**
- Overview (what and why it matters)
- Background (problem context)
- Core concepts
- Design principles
- Architecture (diagrams encouraged)
- Design decisions and rationale
- Trade-offs table
- Comparisons with alternatives
- When to use / not use
- Common misconceptions
- Advanced considerations
- Links to tutorials and reference

**Frontmatter:**
```yaml
---
title: "[Concept/Topic]"
category: "explanation"
audience: "intermediate"
version: "6.0.0-alpha.1"
proof:
  source: ["docs/adr/NNNN-decision.md"]
  hash: "sha256-hash"
  confidence: 0.85
---
```

**Example:**
```markdown
# Receipt-Driven Architecture

> "Every operation leaves an auditable proof."

## Overview

Receipt-driven architecture ensures all RDF operations generate cryptographic receipts...

## Background

### The Problem

Traditional RDF stores lack built-in audit trails...

### UNRDF Approach

We use Merkle proofs to...

[...detailed explanation with diagrams...]
```

---

## Cross-Linking Conventions

### Hierarchical Navigation

- **Tutorials → How-to Guides:** "Now try solving this specific problem"
- **How-to Guides → Reference:** "See API documentation for details"
- **Explanation → Tutorials:** "Try it hands-on"
- **Reference → Explanation:** "Understand why this works"

### Link Format

```markdown
<!-- Internal links (relative) -->
[Tutorial: Getting Started](../tutorials/01-getting-started.md)
[How to Validate](../how-to/01-validate.md)
[Receipt API](../reference/api.md#generateReceipt)
[Receipt Architecture](../explanation/architecture.md)

<!-- Cross-package links (absolute) -->
[KGC-4D Tutorial](/packages/kgc-4d/docs/tutorials/01-getting-started.md)
```

---

## Proof System

Every documentation file MUST include a `proof` section in frontmatter:

```yaml
proof:
  source:           # File paths that prove this content
    - "src/receipts/index.mjs"
    - "test/receipts.test.mjs"
  hash: "sha256-..."  # Content fingerprint
  confidence: 0.95    # 0.0-1.0 (1.0 = code-generated)
```

**Confidence Levels:**
- `1.00` - Generated directly from code (reference docs)
- `0.95` - Verified against tests + code
- `0.90` - Manual review against code
- `0.85` - Conceptual (explanation)
- `0.80` - External sources

---

## Example: @unrdf/yawl Documentation

```
packages/yawl/
├── README.md
├── docs/
│   ├── index.md
│   ├── tutorials/
│   │   ├── README.md
│   │   ├── 01-first-workflow.md
│   │   ├── 02-async-tasks.md
│   │   └── 03-error-handling.md
│   ├── how-to/
│   │   ├── README.md
│   │   ├── 01-timeout-tasks.md
│   │   ├── 02-parallel-execution.md
│   │   └── 03-custom-validators.md
│   ├── reference/
│   │   ├── README.md
│   │   ├── api.md
│   │   ├── task-schema.md
│   │   └── errors.md
│   └── explanation/
│       ├── README.md
│       ├── architecture.md
│       ├── task-lifecycle.md
│       └── vs-other-engines.md
```

### Sample Tutorial: First Workflow

```markdown
---
title: "Tutorial: Create Your First YAWL Workflow"
category: "tutorials"
audience: "beginner"
version: "4.0.0"
estimated_time: "10 minutes"
prerequisites:
  - "Node.js 18+"
proof:
  source:
    - "src/task.mjs"
    - "examples/basic-workflow.mjs"
  hash: "a1b2c3d4..."
  confidence: 0.95
---

# Tutorial: Create Your First YAWL Workflow

> Time: 10 minutes | Difficulty: Beginner

## What You'll Build

A simple workflow that fetches data, transforms it, and saves results.

## What You'll Learn

1. Define tasks with zod schemas
2. Chain tasks together
3. Handle errors gracefully
4. Validate outputs

## Prerequisites

- [x] Node.js 18+
- [x] Basic JavaScript knowledge

## Step 1: Define Your First Task

[...detailed instructions...]
```

---

## LaTeX Generation Pipeline

### Overview

The documentation pipeline MUST support:

1. **Collection:** Gather docs from all packages
2. **CONSTRUCT:** Generate docs from ontology via SPARQL CONSTRUCT
3. **LaTeX:** Convert markdown → LaTeX
4. **PDF:** Compile LaTeX → PDF (deterministic)

### Receipt Points

Each pipeline stage emits a receipt:

```javascript
{
  stage: "collect|construct|latex|pdf",
  input: { ... },
  output: { ... },
  receipt: {
    hash: "sha256-...",
    timestamp: "2025-12-27T...",
    merkleRoot: "sha256-..."
  }
}
```

### Lockfile Format

```json
{
  "version": "6.0.0-alpha.1",
  "generatedAt": "2025-12-27T12:00:00Z",
  "packages": {
    "@unrdf/yawl": {
      "version": "4.0.0",
      "tutorials": ["01-first-workflow.md"],
      "howtos": ["01-timeout-tasks.md"],
      "reference": ["api.md"],
      "explanation": ["architecture.md"],
      "hash": "sha256-..."
    }
  },
  "latex": {
    "compiler": "latexmk",
    "version": "4.77",
    "hash": "sha256-..."
  },
  "merkleRoot": "sha256-..."
}
```

---

## Quality Gates

All documentation MUST pass:

1. **Structure Check:** All 4 categories present
2. **Frontmatter Validation:** Required fields present
3. **Link Validation:** All internal links resolve
4. **Proof Verification:** Source files exist + match hash
5. **Code Examples:** All code blocks are syntactically valid
6. **Determinism:** Re-run produces identical output

---

## CLI Integration

```bash
# Collect package docs
kgc thesis collect --packages="@unrdf/*"

# CONSTRUCT docs from ontology
kgc thesis render --ontology=docs.ttl

# Build thesis (full pipeline)
kgc thesis build --output=thesis.pdf

# Export to format
kgc thesis export --format=pdf|latex|html

# Validate structure
kgc thesis validate --comprehensive
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 6.0.0-alpha.1 | 2025-12-27 | Initial V6 specification |

---

## References

- [Diataxis Framework](https://diataxis.fr/)
- [UNRDF V6 Architecture](/docs/v6/architecture.md)
- [Receipt Specification](/docs/v6/receipts.md)

