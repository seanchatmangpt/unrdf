# UNRDF Tutorials

**Start with #1. Stop when you have what you need.**

---

## Beginner Path (30 minutes)

### 1. [Quick Start: First Knowledge Hook](./01-first-knowledge-hook.md) ← START HERE

Build your first knowledge hook with RDF, SPARQL, and reactive behaviors.

**Time:** 15 minutes
**What you'll learn:**
- Installing UNRDF
- Creating knowledge hooks
- Basic SPARQL queries

### 2. [RDF Validation with SHACL](./validation.md)

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

### 3. [Advanced Knowledge Hooks](./04-advanced-hooks.md)

Create hooks that react to data changes automatically.

**Time:** 30 minutes
**When you need it:** Autonomous validation, transformation, logging
**Skip if:** You can validate/transform manually in your code

### 4. [RDF Operations & Data Management](./02-rdf-operations.md)

Work with RDF quads, stores, and data operations.

**Time:** 20 minutes
**When you need it:** Complex data transformations and management
**Skip if:** Basic SPARQL queries are sufficient

---

## Advanced Path (2+ hours) - Specialized Use Cases Only

### 5. [SPARQL Query Patterns](./sparql.md)

Master SPARQL queries and advanced query patterns.

**Time:** 45 minutes
**When you need it:** Complex queries and data retrieval
**Skip if:** Basic SELECT queries are sufficient

### 6. [Composables & Context Management](./03-composables-context.md)

Use Vue-style composables for reactive RDF data.

**Time:** 40 minutes
**When you need it:** Building reactive applications
**Skip if:** Not using composables pattern

### 7. [Creating RDF Documents](./creating-rdf-documents.md)

Programmatically build RDF documents and knowledge graphs.

**Time:** 50 minutes
**When you need it:** Generating RDF from application data
**Skip if:** Working with existing RDF data only

### 8. Browser Integration (Coming Soon)

Client-side RDF with IndexedDB. See [How-To: React Integration](../how-to/use-hooks-in-react.md) for current options.

### 9. Production Deployment (Coming Soon)

Docker, Kubernetes, observability. See [Reference Documentation](../reference/README.md) for API details.

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
