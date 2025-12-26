# UNRDF Learning Paths (Diataxis Framework)

**Navigate UNRDF documentation by your learning goal.**

---

## Quick Navigation by User Type

### New User? Start with Tutorials
**Goal:** Learn by doing, hands-on practice

Start here if you're new to UNRDF and want to build working examples step-by-step.

→ **[Go to Tutorials](./tutorials/)** | **Recommended first tutorial:** [Create and Freeze a Knowledge Universe](./tutorials/01-create-and-freeze-universe.md)

---

### Solving a Problem? Go to How-To Guides
**Goal:** Solve specific problems you're facing

Use these task-oriented guides when you know what you want to accomplish but need to know how.

→ **[Go to How-To Guides](./how-to/)** | **Popular guides:** [Validate Policy Packs](./how-to/01-validate-policy-packs.md), [Audit Decision Trail](./how-to/02-audit-decision-trail.md)

---

### Looking Up API? See Reference
**Goal:** Find exact syntax, parameters, and schemas

Reference documentation for APIs, schemas, configuration, and technical specifications.

→ **[Go to Reference](./reference/)** | **Most viewed:** [Receipt Schema](./reference/receipt-schema.md), [Hook API](./reference/hook-api.md)

---

### Understanding Design? Read Explanation
**Goal:** Deep dive into architecture and design rationale

Conceptual articles explaining the "why" behind UNRDF's architecture and design decisions.

→ **[Go to Explanation](./explanation/)** | **Start with:** [Why Partitioned Universes](./explanation/why-partitioned-universes.md)

---

## Interactive Decision Tree

```
Start Here
    │
    ├─ I'm new to UNRDF → TUTORIALS
    │   ├─ I want to create my first knowledge graph → Tutorial 01
    │   ├─ I need to parse RDF in the browser → Tutorial 02
    │   ├─ I want to generate receipts → Tutorial 03
    │   └─ I want to implement policy gates → Tutorial 04
    │
    ├─ I have a specific problem to solve → HOW-TO
    │   ├─ How do I validate policy packs? → How-To 01
    │   ├─ How do I audit decisions? → How-To 02
    │   ├─ How do I measure performance? → How-To 03
    │   └─ How do I integrate with existing graphs? → How-To 04
    │
    ├─ I need to look up exact syntax → REFERENCE
    │   ├─ What are the receipt schema fields? → Reference: Receipt Schema
    │   ├─ What's the policy predicate syntax? → Reference: Policy Predicate Syntax
    │   ├─ What hooks are available? → Reference: Hook API
    │   └─ What packages can I import? → Reference: Package Exports
    │
    └─ I want to understand the architecture → EXPLANATION
        ├─ Why partitioned universes? → Explanation 01
        ├─ What's the governance model? → Explanation 02
        ├─ How does cross-runtime work? → Explanation 03
        └─ What are the performance tradeoffs? → Explanation 04
```

---

## Learning Path Recommendations

### Path 1: Quick Start (30 minutes)
**For developers who want to get up and running fast**

1. [Tutorial 01: Create and Freeze Universe](./tutorials/01-create-and-freeze-universe.md) - 10 min
2. [How-To 01: Validate Policy Packs](./how-to/01-validate-policy-packs.md) - 10 min
3. [Reference: Hook API](./reference/hook-api.md) - 10 min
4. **Result:** Working knowledge graph with policy validation

---

### Path 2: Production Ready (2 hours)
**For teams deploying UNRDF in production**

1. [Tutorial 03: Generate and Verify Receipts](./tutorials/03-generate-and-verify-receipts.md) - 20 min
2. [How-To 02: Audit Decision Trail](./how-to/02-audit-decision-trail.md) - 20 min
3. [How-To 03: Measure Query Performance](./how-to/03-measure-query-performance.md) - 20 min
4. [Explanation 01: Why Partitioned Universes](./explanation/why-partitioned-universes.md) - 30 min
5. [Explanation 04: Performance Tradeoffs](./explanation/performance-tradeoffs.md) - 30 min
6. **Result:** Production-ready deployment with monitoring

---

### Path 3: Advanced Integration (3 hours)
**For architects integrating UNRDF into existing systems**

1. [Tutorial 02: Parse RDF in Browser](./tutorials/02-parse-rdf-in-browser.md) - 20 min
2. [Tutorial 04: Implement Policy Gates](./tutorials/04-implement-policy-gates.md) - 30 min
3. [How-To 04: Integrate with Existing Graphs](./how-to/04-integrate-with-existing-graphs.md) - 40 min
4. [Explanation 02: Proof-Based Admission vs Editing](./explanation/proof-based-admission-vs-editing.md) - 30 min
5. [Explanation 03: Cross-Runtime Bridging](./explanation/cross-runtime-bridging.md) - 40 min
6. [Reference: RDF Format Notes](./reference/rdf-format-notes.md) - 20 min
7. **Result:** Deep integration with existing architecture

---

## Documentation by Audience

### Beginners (New to UNRDF or RDF)
**Recommended:** Tutorials → How-To → Reference (in that order)

- Start with hands-on tutorials to build confidence
- Move to how-to guides when you have specific tasks
- Use reference for looking up syntax as needed

---

### Intermediate (Familiar with RDF, new to UNRDF)
**Recommended:** How-To → Reference → Explanation

- Jump straight to how-to guides for your use case
- Use reference documentation for API details
- Read explanations to understand UNRDF's unique approach

---

### Architects (Designing systems with UNRDF)
**Recommended:** Explanation → Reference → How-To → Tutorials

- Start with conceptual explanations to understand design rationale
- Study reference for complete API surface
- Use how-to guides for implementation patterns
- Review tutorials for proof-of-concepts

---

## Documentation Quality Standards

All Diataxis documentation follows these principles:

| Category | Focus | Tone | Success Metric |
|----------|-------|------|----------------|
| **Tutorials** | Learning-oriented | Encouraging, supportive | Reader completes successfully |
| **How-To** | Problem-oriented | Direct, practical | Reader solves their problem |
| **Reference** | Information-oriented | Neutral, precise | Reader finds exact answer |
| **Explanation** | Understanding-oriented | Discursive, analytical | Reader gains deep insight |

---

## Cross-Links and Prerequisites

Each document includes:
- **Prerequisites:** Capability atoms needed (from /home/user/unrdf/docs/capabilities/)
- **Evidence:** Source code pointers (file:line)
- **Examples:** Links to proof artifacts
- **Next Steps:** Related docs across all 4 categories

---

## Contributing to Diataxis Docs

When adding new documentation:

1. **Classify correctly:** Is it a tutorial, how-to, reference, or explanation?
2. **Follow the template:** Each category has a specific structure
3. **Link extensively:** Cross-link to related docs in all 4 categories
4. **Provide evidence:** Link to actual code, not just descriptions
5. **Test examples:** All code examples must run successfully

See [/home/user/unrdf/docs/CONTRIBUTING.md](/home/user/unrdf/docs/CONTRIBUTING.md) for full guidelines.

---

## Status and Roadmap

| Category | Files Created | Status |
|----------|---------------|--------|
| Tutorials | 4 skeleton files | 🟡 Structure complete, content in progress |
| How-To | 4 skeleton files | 🟡 Structure complete, content in progress |
| Reference | 6 skeleton files | 🟡 Structure complete, content in progress |
| Explanation | 4 skeleton files | 🟡 Structure complete, content in progress |

**Last Updated:** 2025-12-26

---

## Quick Links

- [Main Docs Hub](/home/user/unrdf/docs/README.md)
- [Architecture Overview](/home/user/unrdf/docs/ARCHITECTURE.md)
- [API Reference](/home/user/unrdf/docs/API-REFERENCE.md)
- [Quick Start](/home/user/unrdf/docs/QUICK-START.md)
- [All Packages](/home/user/unrdf/docs/PACKAGES.md)

---

**Questions?** Check [Troubleshooting](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
