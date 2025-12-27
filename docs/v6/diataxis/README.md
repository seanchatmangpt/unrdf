# UNRDF v6 - Complete Diataxis Documentation

**Version**: 6.0.0-alpha.1
**Last Updated**: 2025-12-27
**Framework**: [Diataxis](https://diataxis.fr/)

---

## 📚 Documentation Structure

This documentation follows the **Diataxis framework**, organizing content into 4 quadrants based on user needs:

```
                Learning-Oriented              Understanding-Oriented
                     ↓                                 ↓
            ┌─────────────────┐          ┌─────────────────────┐
            │   TUTORIALS     │          │   EXPLANATION       │
            │  (Learning)     │          │  (Understanding)    │
            │                 │          │                     │
            │ Getting Started │          │ Why ΔGate?          │
            │ First Receipt   │          │ Receipt Theory      │
            │ First Delta     │          │ L1-L5 Maturity      │
            └─────────────────┘          └─────────────────────┘
                     ↑                                 ↑
                Practical                         Theoretical

                     ↓                                 ↓
            ┌─────────────────┐          ┌─────────────────────┐
            │   HOW-TO        │          │   REFERENCE         │
            │  (Problem)      │          │  (Information)      │
            │                 │          │                     │
            │ Migrate v5→v6   │          │ CLI Commands        │
            │ Compose Deltas  │          │ API Reference       │
            │ Verify Receipts │          │ Receipt Schema      │
            └─────────────────┘          └─────────────────────┘
                     ↑                                 ↑
                Practical                         Theoretical
```

---

## 🎓 Tutorials (Learning-Oriented)

**Goal**: Learn by doing, step-by-step guidance

### Available Tutorials

1. **[Getting Started with UNRDF v6](./tutorials/01-getting-started-v6.md)** ⭐ START HERE
   - Install v6
   - Create your first receipt
   - Run your first delta command
   - Understand v6 fundamentals
   - **Time**: ~15 minutes

**Coming Soon:**
- Tutorial: Build a Receipt-Driven Application
- Tutorial: Create Your First L5 Package
- Tutorial: Deploy v6 to Production

---

## 🛠️ How-To Guides (Problem-Solving)

**Goal**: Solve specific problems, achieve specific goals

### Available How-To Guides

1. **[Migrate a v5 Package to v6](./how-to/01-migrate-v5-to-v6.md)**
   - Address all 7 breaking changes
   - Use v6-compat layer
   - Verify migration success
   - **Time**: ~2-4 hours per package

2. **[Compose Cross-Package Deltas](./how-to/02-compose-deltas.md)**
   - Understand delta composition
   - Compose deltas programmatically
   - Verify composed deltas
   - **Time**: ~30 minutes

3. **[Verify Receipt Chain Integrity](./how-to/03-verify-receipt-chain.md)**
   - Generate receipt chains
   - Verify individual receipts
   - Detect tampering
   - Anchor to blockchain
   - **Time**: ~20 minutes

4. **[Implement L5 Maturity](./how-to/04-implement-l5-maturity.md)**
   - Achieve production-grade quality
   - Progress from L1 → L5
   - Generate maturity certificate
   - **Time**: ~5-7 days

5. **[Integrate Cross-Package Workflows](./how-to/05-cross-package-integration.md)**
   - YAWL + Hooks integration
   - KGC-4D + Blockchain anchoring
   - Federation + Streaming
   - **Time**: ~45 minutes

**Coming Soon:**
- How-To: Debug Receipt Verification Failures
- How-To: Optimize Delta Composition Performance
- How-To: Implement Custom Receipt Anchoring
- How-To: Create Adversarial Tests

---

## 📖 Reference (Information)

**Goal**: Find precise information quickly

### Available References

1. **[CLI Command Matrix](./reference/01-cli-command-matrix.md)** ⭐ COMPREHENSIVE
   - 10 canonical nouns × 25 canonical verbs
   - Complete command syntax
   - Examples for every command
   - Exit codes and environment variables

**Coming Soon:**
2. **API Reference**
   - v6-core API (receipts, deltas, ΔGate)
   - Oxigraph API (store operations)
   - KGC-4D API (universe, eventlog)
   - Hooks API (policy enforcement)
   - Federation API (distributed queries)
   - Streaming API (change feeds)

3. **Receipt Schema Reference**
   - Receipt structure (JSON schema)
   - Proof format
   - Merkle tree structure
   - Signature algorithms

4. **Zod Schema Reference**
   - All v6 schemas
   - Validation rules
   - Error messages

---

## 💡 Explanation (Understanding)

**Goal**: Understand concepts and theory

### Coming Soon

1. **Why ΔGate Architecture?**
   - Theoretical foundations
   - Mathematical properties
   - Comparison to alternatives
   - **Time**: ~10 minutes

2. **Receipt Chain Verification Security**
   - Cryptographic guarantees
   - Attack vectors
   - Blockchain anchoring theory
   - **Time**: ~15 minutes

3. **L1-L5 Maturity Model Explained**
   - Philosophy of maturity levels
   - Why 5 levels?
   - Quality metrics
   - **Time**: ~12 minutes

4. **V6 Architecture Overview**
   - Control plane design
   - Receipt-driven operations
   - Cross-package composition
   - **Time**: ~20 minutes

---

## 🎯 Quick Start by Role

### New to UNRDF v6?
1. Read [Getting Started Tutorial](./tutorials/01-getting-started-v6.md)
2. Try [First Delta Example](./examples/01-first-delta.md)
3. Explore [CLI Command Matrix](./reference/01-cli-command-matrix.md)

### Migrating from v5?
1. Read [Migration Plan](/docs/v6/MIGRATION_PLAN.md)
2. Follow [Migration How-To](./how-to/01-migrate-v5-to-v6.md)
3. Use [v6-compat adapter](/packages/v6-compat/README.md)
4. Check [Maturity Ladder](/docs/v6/MATURITY_LADDER.md)

### Building a Package?
1. Study [L5 Maturity How-To](./how-to/04-implement-l5-maturity.md)
2. Review [Cross-Package Integration](./how-to/05-cross-package-integration.md)
3. Implement [Receipt Generation](./tutorials/01-getting-started-v6.md#your-first-receipt)

### Auditing/Compliance?
1. Read [Receipt Verification](./how-to/03-verify-receipt-chain.md)
2. Use [CLI to Export Chains](./reference/01-cli-command-matrix.md#compliance-audit)
3. Understand [Blockchain Anchoring](./how-to/03-verify-receipt-chain.md#step-5-anchor-receipts-to-blockchain)

---

## 🔍 Documentation Index by Topic

### Receipts
- **Tutorial**: [Your First Receipt](./tutorials/01-getting-started-v6.md#your-first-receipt)
- **How-To**: [Verify Receipt Chain](./how-to/03-verify-receipt-chain.md)
- **Reference**: (Coming) Receipt Schema Reference
- **Explanation**: (Coming) Receipt Theory

### Deltas
- **Tutorial**: [Your First Delta](./tutorials/01-getting-started-v6.md#your-first-delta-command)
- **How-To**: [Compose Deltas](./how-to/02-compose-deltas.md)
- **Reference**: [CLI Delta Commands](./reference/01-cli-command-matrix.md#10-delta-state-transitions)
- **Explanation**: (Coming) ΔGate Architecture

### Migration
- **How-To**: [Migrate v5→v6](./how-to/01-migrate-v5-to-v6.md)
- **Reference**: [Breaking Changes](/docs/v6/MIGRATION_PLAN.md#breaking-changes-summary)

### Maturity Levels
- **How-To**: [Achieve L5](./how-to/04-implement-l5-maturity.md)
- **Reference**: [Maturity Ladder](/docs/v6/MATURITY_LADDER.md)
- **Explanation**: (Coming) L1-L5 Philosophy

### CLI
- **Tutorial**: [First CLI Commands](./tutorials/01-getting-started-v6.md#your-first-delta-command)
- **Reference**: [Command Matrix](./reference/01-cli-command-matrix.md) ⭐
- **Examples**: [CLI Examples](./examples/)

### Cross-Package Integration
- **How-To**: [Cross-Package Workflows](./how-to/05-cross-package-integration.md)
- **Examples**: [YAWL + Hooks](./examples/02-yawl-hooks-integration.md)

---

## 🏷️ Tags for Search

**Core Concepts**: #receipts #deltas #ΔGate #control-plane #maturity-ladder #L1-L5

**Packages**: #v6-core #oxigraph #kgc-4d #hooks #yawl #federation #streaming

**Operations**: #freeze #verify #compose #anchor #apply #replay

**Topics**: #migration #integration #testing #security #performance #compliance

**Roles**: #developer #auditor #contributor #user #package-author

---

## 📊 Documentation Coverage Status

| Quadrant | Complete | In Progress | Planned | Total |
|----------|----------|-------------|---------|-------|
| **Tutorials** | 1 | 0 | 3 | 4 |
| **How-To** | 5 | 0 | 4 | 9 |
| **Reference** | 1 | 0 | 3 | 4 |
| **Explanation** | 0 | 0 | 4 | 4 |
| **Examples** | 0 | 0 | 5 | 5 |
| **TOTAL** | 7 | 0 | 19 | 26 |

**Completion**: 27% (7/26)
**Critical Path Complete**: ✅ (Tutorial + 5 How-Tos + CLI Reference)

---

## 🤝 Contributing to Documentation

See [CONTRIBUTING.md](/docs/CONTRIBUTING.md) for:
- Documentation style guide
- How to add new Diataxis content
- Review process
- Testing documentation examples

---

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/seanchatmangpt/unrdf/issues)
- **Discussions**: [GitHub Discussions](https://github.com/seanchatmangpt/unrdf/discussions)
- **Chat**: (Coming Soon) Discord/Slack

---

## 📝 Change Log

### 2025-12-27
- ✅ Initial Diataxis structure created
- ✅ Tutorial: Getting Started with v6
- ✅ How-To: Migrate v5→v6
- ✅ How-To: Compose Deltas
- ✅ How-To: Verify Receipt Chain
- ✅ How-To: Implement L5 Maturity
- ✅ How-To: Cross-Package Integration
- ✅ Reference: CLI Command Matrix

---

**Questions?** See [Migration Plan](/docs/v6/MIGRATION_PLAN.md) or [open an issue](https://github.com/seanchatmangpt/unrdf/issues/new).
