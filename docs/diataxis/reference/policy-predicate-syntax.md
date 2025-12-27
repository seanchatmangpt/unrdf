# Policy Predicate Syntax Reference

**Purpose:** Complete reference for writing policy predicates in UNRDF.

**Audience:** Developers implementing policy systems

**Version:** 5.0.1

---

## Overview

Policy predicates are SPARQL queries that define conditions for knowledge graph operations. UNRDF supports:
- SPARQL ASK queries (boolean conditions)
- SPARQL SELECT queries (filtered results)
- SHACL shapes (structural validation)

---

## SPARQL ASK Predicates

### Basic Syntax

**[Placeholder - Content to be filled]**

```sparql
# Basic ASK predicate
ASK {
  ?operation :hasApproval ?approval .
  ?approval :approvedBy :admin .
}
```

**Evidence:** ASK predicate parser at `/home/user/unrdf/packages/validation/src/sparql-ask.mjs`

---

### Advanced Patterns

**[Placeholder - Advanced SPARQL patterns]**

```sparql
# Temporal constraint
ASK {
  ?operation :timestamp ?ts .
  FILTER(?ts > "2025-01-01T00:00:00Z"^^xsd:dateTime)
}

# Multi-hop constraint
ASK {
  ?user :hasRole ?role .
  ?role :hasPermission :write .
  ?operation :requestedBy ?user .
}
```

**Evidence:** Pattern library at `/home/user/unrdf/packages/validation/src/sparql-patterns.mjs`

---

## SPARQL SELECT Predicates

**[Placeholder - SELECT predicate syntax]**

**Evidence:** SELECT predicates at `/home/user/unrdf/packages/validation/src/sparql-select.mjs`

---

## SHACL Shape Predicates

**[Placeholder - SHACL syntax]**

**Evidence:** SHACL predicates at `/home/user/unrdf/packages/validation/src/shacl-predicates.mjs`

---

## Built-in Functions

**[Placeholder - Available SPARQL functions]**

- String functions
- Numeric functions
- Date/time functions
- Hash functions
- Custom UNRDF functions

**Evidence:** Functions at `/home/user/unrdf/packages/validation/src/functions.mjs`

---

## Variables and Bindings

**[Placeholder - Variable reference]**

**Evidence:** Variable handling at `/home/user/unrdf/packages/hooks/src/variable-binding.mjs`

---

## Examples by Use Case

### Example 1: Approval Required

**[Placeholder - Complete example]**

**Evidence:** Examples at `/home/user/unrdf/examples/policy-predicates/`

---

### Example 2: Time-Based Access

**[Placeholder - Time-based example]**

---

### Example 3: Role-Based Access Control

**[Placeholder - RBAC example]**

---

## Performance Considerations

**[Placeholder - Performance tips]**

- Predicate complexity
- Index usage
- Caching strategies

**Evidence:** Performance guide at `/home/user/unrdf/packages/validation/src/performance-guide.md`

---

## Common Patterns

**[Placeholder - Pattern library]**

**Evidence:** Patterns at `/home/user/unrdf/packages/validation/src/common-patterns.mjs`

---

## Error Messages

**[Placeholder - Error reference]**

**Evidence:** Errors at `/home/user/unrdf/packages/validation/src/predicate-errors.mjs`

---

## Related References

- **[Hook API Reference](./hook-api.md)** - Using predicates in hooks
- **[Receipt Schema](./receipt-schema.md)** - Policy decision receipts
- **[How-To 01: Validate Policy Packs](../how-to/01-validate-policy-packs.md)** - Validation guide

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
