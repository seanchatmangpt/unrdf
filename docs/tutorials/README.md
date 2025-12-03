# UNRDF Tutorials

**Start with #1. Stop when you have what you need.**

---

## Beginner Path (30 minutes)

### 1. [Quick Start Tutorial](./01-quick-start.md) ← START HERE

Parse RDF, execute SPARQL queries, serialize data.

**Time:** 15 minutes
**What you'll learn:**
- Installing UNRDF
- The 11-line pit of success
- Basic SPARQL queries

### 2. [SHACL Validation](./02-shacl-validation.md)

Validate your data with SHACL shapes.

**Time:** 15 minutes
**What you'll learn:**
- Defining shape constraints
- Validating data
- Handling validation errors

---

## STOP HERE if you just need to parse, query, and validate.

Most users don't need anything beyond this point.

---

## Intermediate Path (1 hour) - Only If You Need Automation

### 3. [Building Your First Knowledge Hook](./02-first-knowledge-hook.md)

Create hooks that react to data changes automatically.

**Time:** 30 minutes
**When you need it:** Autonomous validation, transformation, logging
**Skip if:** You can validate/transform manually in your code

### 4. [Transactions with Hooks](./03-transactions-with-hooks.md)

Hook-driven atomic transactions.

**Time:** 20 minutes
**When you need it:** Multiple hooks coordinated on changes
**Skip if:** You don't need Knowledge Hooks

---

## Advanced Path (2+ hours) - Specialized Use Cases Only

### 5. [Browser Integration](./03-browser-integration.md)

Client-side RDF with IndexedDB.

**Time:** 45 minutes
**When you need it:** Browser-based applications
**Skip if:** Node.js only

### 6. [Policy Packs and Governance](./04-policy-packs.md)

Declarative access control and validation policies.

**Time:** 40 minutes
**When you need it:** Enterprise governance requirements
**Skip if:** Basic SHACL is enough

### 7. [Real-time Streaming](./05-real-time-streaming.md)

Change feeds, windowing, reactive pipelines.

**Time:** 50 minutes
**When you need it:** Real-time data processing
**Skip if:** Batch processing is sufficient

### 8. [Distributed Federation](./06-distributed-federation.md)

Multi-node queries, consensus protocols.

**Time:** 60 minutes
**When you need it:** Distributed systems
**Skip if:** Single-node is sufficient

### 9. [Production Deployment](./08-production-deployment.md)

Docker, Kubernetes, observability.

**Time:** 90 minutes
**When you need it:** Production infrastructure
**Skip if:** Development only

---

## Tutorial Selection Guide

| Your Goal | Tutorials to Complete |
|-----------|----------------------|
| Parse and query RDF | 1 only |
| Add validation | 1, 2 |
| React to data changes | 1, 2, 3 |
| Browser application | 1, 2, 5 |
| Production deployment | 1, 2, 9 |
| Everything | All of them (but why?) |

---

## Next Steps

- **[How-To Guides](../how-to/README.md)** - Task-specific recipes
- **[Reference](../reference/README.md)** - API documentation
- **[Which Features?](../WHICH-FEATURES.md)** - Decision trees

---

## Need Help?

- [GitHub Discussions](https://github.com/seanchatmangpt/unrdf/discussions)
- [Troubleshooting Guide](../TROUBLESHOOTING.md)
