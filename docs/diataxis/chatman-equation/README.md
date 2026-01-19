# Chatman Equation Documentation

Complete Diataxis documentation suite for the Chatman Equation: **S(t) = ⟨O, t_ns, V, G⟩**

## Documentation Structure

This documentation follows the [Diataxis framework](https://diataxis.fr/), organizing content into four categories:

### 📚 [Tutorials](./tutorials/)

Learning-oriented guides that take you from beginner to expert:
- [Understanding the Chatman Equation](./tutorials/01-understanding-chatman-equation.md)
- [Implementing 4D State in Your Application](./tutorials/02-implementing-4d-state.md)
- [Advanced Time-Travel Queries](./tutorials/03-time-travel-queries.md)

### 🛠️ [How-To Guides](./how-to/)

Task-oriented guides for specific operations:
- [Freeze and Verify State](./how-to/freeze-and-verify.md)
- [Reconstruct State from Git](./how-to/reconstruct-state.md)
- [Resolve Merge Conflicts](./how-to/resolve-conflicts.md)
- [Optimize Time-Travel Performance](./how-to/optimize-time-travel.md)
- [Implement Temporal Audit Trails](./how-to/temporal-audit-trails.md)
- [Set Up Distributed KGC Nodes](./how-to/distributed-setup.md)

### 📖 [Reference](./reference/)

Technical API documentation:
- [KGCStore API](./reference/kgc-store-api.md)
- [Receipt Schema](./reference/receipt-schema.md)
- [Vector Clock API](./reference/vector-clock-api.md)
- [State4D Type](./reference/state-4d-type.md)

### 💡 [Explanation](./explanation/)

Conceptual deep-dives and theory:
- [Theoretical Foundations](./explanation/theoretical-foundations.md)
- [Performance and Scaling](./explanation/performance-scaling.md)
- [Security Model](./explanation/security-model.md)

## The Chatman Equation

```math
S(t) = \langle O, t_{ns}, \vec{V}, G \rangle
```

Where:
- **O**: Observable RDF state
- **t_ns**: Nanosecond timestamp
- **V**: Vector clock (causality)
- **G**: Git commit SHA (cryptographic proof)

## Quick Start

1. **New to KGC-4D?** Start with [Understanding the Chatman Equation](./tutorials/01-understanding-chatman-equation.md)
2. **Ready to build?** Follow [Implementing 4D State](./tutorials/02-implementing-4d-state.md)
3. **Need specific task?** Check [How-To Guides](./how-to/)
4. **API reference?** See [Reference](./reference/)
5. **Deep theory?** Read [Explanations](./explanation/)

---

**Generated from**: TOML configs + Tera templates
**Last updated**: 2026-01-18
