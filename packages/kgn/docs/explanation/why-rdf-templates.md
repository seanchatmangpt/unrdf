# Why RDF Templates?

This document explains the design philosophy behind @unrdf/kgn and why combining RDF with templates is powerful.

## The Problem

Modern applications need to work with semantic data (RDF, knowledge graphs) but generating code that correctly handles RDF is tedious and error-prone:

1. **Manual URI management**: Hand-writing URIs leads to typos
2. **Prefix inconsistency**: Different parts of code use different prefix conventions
3. **Type mismatches**: RDF datatypes don't map cleanly to TypeScript/JavaScript
4. **Boilerplate code**: Repeated patterns for SPARQL queries, serialization, etc.
5. **Schema drift**: Code and RDF schema get out of sync

## The Solution: Template-Driven Code Generation

@unrdf/kgn combines **RDF semantics** with **Nunjucks templates** to generate correct, consistent code from semantic schemas.

### Key Insight

RDF data is **structural and regular**. Classes have properties, properties have types, vocabularies follow patterns. This regularity makes RDF perfect for code generation.

## Architecture

```
┌─────────────────────┐
│  RDF Vocabulary     │  ← Source of truth (ontology, schema)
│  (YAML frontmatter) │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Template Engine    │  ← @unrdf/kgn
│  + RDF Filters      │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Generated Code     │  ← TypeScript, JSON-LD, SPARQL, etc.
│  (deterministic)    │
└─────────────────────┘
```

### Components

1. **Frontmatter Parser**: Extracts RDF schema from YAML
2. **RDF Filters**: Transform RDF concepts (URIs, literals, CURIEs)
3. **Template Engine**: Nunjucks with deterministic rendering
4. **Output**: Type-safe, consistent code

## Why This Works

### 1. Single Source of Truth

RDF schema lives in frontmatter → all generated code stays in sync.

```yaml
---
classes:
  - uri: "ex:Person"
    properties:
      - "ex:name"
      - "ex:email"
---
```

One change updates TypeScript types, Zod validators, JSON Schema, and documentation.

### 2. Semantic Correctness

RDF filters ensure semantically correct output:

```nunjucks
{# WRONG: Manual URI construction #}
URI: http://example.org/{{ entity.name }}

{# RIGHT: Semantic expansion #}
URI: {{ entity.uri | expand(prefixes) }}
```

### 3. Deterministic Output

Same input → same output (always). Critical for:
- **Version control**: Meaningful diffs
- **CI/CD**: Reproducible builds
- **Testing**: Predictable behavior

### 4. Flexibility

Templates adapt to your needs:
- Generate TypeScript interfaces
- Generate Zod validators
- Generate JSON-LD contexts
- Generate SPARQL queries
- Generate documentation

Same RDF schema, multiple outputs.

## Design Principles

### Principle 1: Frontmatter as Schema

YAML frontmatter is:
- **Human-readable**: Easy to edit
- **Version-control friendly**: Clean diffs
- **Nested structure**: Matches RDF graphs
- **Tool-independent**: Not tied to specific RDF serialization

### Principle 2: Filters as RDF Operations

Each filter maps to an RDF concept:
- `expand` → CURIE expansion
- `rdfLiteral` → Literal serialization
- `contract` → URI shortening
- `sparqlVar` → SPARQL variable syntax

This makes templates **self-documenting**.

### Principle 3: Deterministic by Default

No randomness, no dates, no UUIDs (unless explicitly requested). Same template + same data = same output.

Why?
- **Testable**: Assert exact output
- **Cacheable**: Skip regeneration if input unchanged
- **Debuggable**: Output doesn't change between runs

### Principle 4: Composable

Small templates combine into larger systems:

```
vocabulary.njk       → types.ts
  + validators.njk   → validators.ts
  + queries.njk      → queries.ts
  + docs.njk         → README.md
```

Each template focuses on one concern.

## Comparison with Alternatives

### vs. Manual Code

**Manual:**
```typescript
// Easy to make mistakes
interface Person {
  uri: string;
  name: string;  // Missing RDF metadata
}
```

**Generated:**
```typescript
// Correct RDF mapping
interface Person {
  '@id': string;
  '@type': 'http://example.org/Person';
  'http://example.org/name': string;
}
```

### vs. JSON Schema

JSON Schema doesn't understand RDF semantics:
- No URI expansion
- No prefix management
- No SPARQL integration

### vs. OWL Reasoners

OWL reasoners validate schemas but don't generate code. @unrdf/kgn bridges the gap:

```
OWL Ontology → YAML → Templates → Code
```

## When to Use @unrdf/kgn

### Good Fit

✅ **Generating code from RDF vocabularies**
✅ **Multiple output formats from one schema**
✅ **Ensuring RDF correctness in generated code**
✅ **Deterministic, reproducible builds**
✅ **Documentation generation from RDF**

### Not a Good Fit

❌ **Runtime RDF processing** (use @unrdf/oxigraph instead)
❌ **Complex OWL reasoning** (use dedicated reasoner)
❌ **Dynamic template rendering** (frontmatter is static)

## Example Use Cases

### 1. Type-Safe RDF Clients

Generate TypeScript from SHACL shapes:

```
SHACL Shape → @unrdf/kgn → TypeScript Types + Validators
```

### 2. API Documentation

Generate OpenAPI specs from RDF vocabulary:

```
RDF Vocabulary → @unrdf/kgn → openapi.yaml
```

### 3. GraphQL Schemas

Generate GraphQL from RDF classes:

```
RDF Classes → @unrdf/kgn → schema.graphql
```

### 4. Test Data

Generate test fixtures from RDF examples:

```
RDF Examples → @unrdf/kgn → test-data.json
```

## Future Directions

### 1. SHACL Integration

Direct SHACL shape parsing → templates

### 2. Live Validation

Real-time feedback as you edit templates

### 3. Template Marketplace

Share RDF templates for common vocabularies (schema.org, FOAF, etc.)

## Conclusion

@unrdf/kgn combines the **semantic power of RDF** with the **flexibility of templates** to generate correct, consistent code from knowledge graphs.

Key takeaway: **Treat your RDF schema as the source of truth, use templates to project it into different representations.**

## See Also

- [Explanation: Deterministic Rendering](./deterministic-rendering.md)
- [Explanation: Type Mapping Strategies](./type-mapping.md)
- [Tutorial: Getting Started](../tutorial/01-getting-started.md)
