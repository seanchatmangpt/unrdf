# Explanation: How RDF-to-Code Generation Works

**Objective:** Understand the conceptual model and architecture behind UNRDF's synchronized code generation from RDF ontologies.

**Audience:** Architects, developers adopting the sync workflow, and anyone seeking to understand the "why" behind this approach.

**Estimated Reading Time:** 25 minutes

---

## Introduction

UNRDF's sync command transforms RDF ontologies into code artifacts through a declarative pipeline. This document explains the mental model behind this approach, why RDF serves as an ideal source of truth, and the trade-offs inherent in template-driven generation.

Rather than telling you *how* to use the sync command (see the [How-To guides](../how-to/) for that), this document helps you understand *why* the architecture works this way and when it is the right choice for your project.

---

## The Problem: Contract Drift

Software systems communicate through contracts: API schemas, type definitions, validation rules, database models. In a typical project, these contracts exist in multiple places:

```
┌─────────────────────────────────────────────────────────────┐
│                    Contract Drift Problem                   │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   TypeScript Types    ───┐                                  │
│                          │                                  │
│   Zod Schemas        ───┼──→  Diverge Over Time  ←─ Bug!   │
│                          │                                  │
│   OpenAPI Spec       ───┘                                  │
│                                                             │
│   Documentation      ─────→  Stale                ←─ Debt! │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Why Manual Synchronization Fails

**1. Cognitive Load**

Developers must remember to update three files when adding a field to an entity. They often forget at least one. The resulting inconsistency might not surface until runtime, or worse, in production.

**2. No Single Source of Truth**

Without a canonical definition, which file is "correct"? When the TypeScript type says `string` but the Zod schema says `z.string().email()`, which wins? This ambiguity breeds subtle bugs.

**3. Format-Specific Knowledge**

Keeping TypeScript, OpenAPI, and JSON Schema in sync requires expertise in all three formats. Most developers are proficient in one or two, leading to copy-paste errors.

**4. Review Burden**

Code reviewers must mentally verify that changes are consistent across all contract locations. This is tedious, error-prone, and rarely done thoroughly.

---

## The Solution: Single Source of Truth

The sync architecture solves contract drift by establishing a single canonical definition: an RDF ontology. All other artifacts are *derived* from this source, never authored directly.

```
                    ┌─────────────────┐
                    │  RDF Ontology   │
                    │  (Source of     │
                    │   Truth)        │
                    └────────┬────────┘
                             │
                    ┌────────▼────────┐
                    │  SPARQL Query   │
                    │  (Extract)      │
                    └────────┬────────┘
                             │
                    ┌────────▼────────┐
                    │  Query Results  │
                    │  (Structured    │
                    │   Data)         │
                    └────────┬────────┘
                             │
                    ┌────────▼────────┐
                    │   Templates     │
                    │  (Transform)    │
                    └────────┬────────┘
                             │
        ┌────────────────────┼────────────────────┐
        ▼                    ▼                    ▼
┌───────────────┐   ┌───────────────┐   ┌───────────────┐
│  TypeScript   │   │  Zod Schemas  │   │  OpenAPI      │
│  Types        │   │               │   │  Spec         │
└───────────────┘   └───────────────┘   └───────────────┘
```

### The Key Insight

**Derivation guarantees consistency.** If all outputs come from the same source through deterministic transformations, they cannot diverge. The problem of synchronization is eliminated by design.

---

## The Data Flow in Detail

The sync command orchestrates four distinct phases, each with a clear responsibility:

### Phase 1: Load Ontology

The ontology loader reads RDF data (typically in Turtle format) into an in-memory graph store. This store becomes the queryable representation of your domain model.

```
Input:    ontology/schema.ttl
          ↓
Process:  Parse → Validate → Index
          ↓
Output:   In-memory RDF Store (Oxigraph)
```

The loader extracts prefix declarations from the source file, making them available for both queries and templates. This allows you to write `schema:name` in your SPARQL queries rather than the full URI.

### Phase 2: Execute SPARQL

SPARQL queries extract structured data from the ontology. Each generation rule includes a query that selects exactly the information needed for that output.

```
Input:    SELECT ?class ?property ?datatype WHERE { ... }
          ↓
Process:  Query Planning → Execution → Result Binding
          ↓
Output:   [{ class: "User", property: "email", datatype: "string" }, ...]
```

SPARQL serves as a powerful extraction language because it:
- Handles the graph structure naturally
- Supports filtering, grouping, and ordering
- Allows optional matches for incomplete data
- Works with any RDF vocabulary

### Phase 3: Transform Results

Query results feed into templates that produce the final output. The template renderer provides domain-specific filters for common transformations.

```
Input:    Query results + Template
          ↓
Process:  Variable Binding → Filter Application → Rendering
          ↓
Output:   Generated code string
```

Templates see the query results as an array of objects, making iteration straightforward:

```nunjucks
{% for row in results %}
export type {{ row.class | pascalCase }} = {
  {{ row.property | camelCase }}: {{ row.datatype | jsdocType }};
};
{% endfor %}
```

### Phase 4: Write Output

The orchestrator writes generated content to the filesystem, respecting configuration for output directories and file modes.

---

## Why RDF?

RDF (Resource Description Framework) might seem like an unusual choice for defining domain models when JSON Schema, TypeScript interfaces, or Protobuf are more common. Here is why RDF excels in this role:

### Semantic Precision

RDF forces you to make meaning explicit. When you define a property, you declare its domain (what type of thing has it), range (what type of value it holds), and relationship to other concepts.

```turtle
schema:email a owl:DatatypeProperty ;
    rdfs:domain schema:Person ;
    rdfs:range xsd:string ;
    rdfs:comment "Electronic mail address" ;
    schema:pattern "^[^@]+@[^@]+$" .
```

This precision eliminates ambiguity. There is no question about whether `email` belongs to `Person` or what type it should be. The ontology is the definition, not a representation of it.

### Query Flexibility

SPARQL allows you to extract data in any shape needed for a particular output. The same ontology can produce:
- A flat list of properties for a validation schema
- A hierarchical structure for a class diagram
- A graph of relationships for a documentation page

Different queries, same source.

### Extensibility

RDF's open-world assumption means you can extend an ontology without breaking existing consumers. Adding a new class or property does not invalidate existing ones. This makes RDF ideal for evolving systems where requirements change over time.

### Standards-Based

RDF, RDFS, and OWL are W3C standards with decades of tooling, documentation, and community support. Your ontology can be validated with SHACL, visualized with standard tools, and exchanged with any system that speaks RDF.

---

## Why Templates?

The transformation from query results to code could be done with custom code, but templates offer distinct advantages:

### Separation of Concerns

Templates isolate the "what to generate" from the "how to extract". Query writers focus on data selection; template authors focus on output format. This separation makes both easier to reason about and test.

### Format Agnosticism

The same query results can feed into templates for TypeScript, Python, Java, or any other language. The data model remains constant; only the presentation layer changes.

### Easy Customization

Non-developers can modify templates without understanding SPARQL or the RDF data model. A technical writer can fix a typo in generated documentation without learning graph databases.

### Version Control Friendly

Templates are plain text files that diff cleanly and can be reviewed in pull requests. Changes to generation logic are visible and auditable, unlike code-based generators that hide logic in function implementations.

### Domain-Specific Filters

The template renderer provides filters tailored to code generation:

| Filter | Purpose | Example |
|--------|---------|---------|
| `camelCase` | Property names | `user_name` -> `userName` |
| `pascalCase` | Type names | `user_profile` -> `UserProfile` |
| `localName` | Extract URI suffix | `http://...#Person` -> `Person` |
| `zodType` | XSD to Zod mapping | `xsd:string` -> `z.string()` |
| `groupBy` | Organize results | Group properties by class |

---

## The Mental Model

Think of the sync architecture as a compiler for your domain model:

```
┌─────────────────────────────────────────────────────────────┐
│                    Compiler Analogy                         │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   Source Code  ≈  RDF Ontology                              │
│   Lexer/Parser ≈  Ontology Loader                           │
│   AST          ≈  In-Memory Graph Store                     │
│   Optimizer    ≈  SPARQL Queries (select relevant data)     │
│   Code Gen     ≈  Templates                                 │
│   Output       ≈  Generated Artifacts                       │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

Just as a compiler transforms source code through multiple phases, the sync pipeline transforms your domain definition through loading, querying, and templating. Each phase has a single responsibility and produces a well-defined output for the next.

---

## Trade-offs

No architecture is without trade-offs. Understanding these helps you decide when sync is appropriate for your project.

### Learning Curve for RDF/SPARQL

**Challenge:** Teams unfamiliar with RDF must learn new concepts: triples, URIs, prefixes, SPARQL syntax. This upfront investment can slow initial adoption.

**Mitigation:** Start with simple ontologies and queries. The sync documentation includes templates for common patterns. Most teams find that a few hours of learning unlocks the full system.

### Template Maintenance

**Challenge:** As output formats evolve (new TypeScript syntax, updated Zod APIs), templates must be updated. This ongoing maintenance is unavoidable.

**Mitigation:** Templates are plain text and easy to update. Changes affect all generated files at once, ensuring consistency. The alternative (manual updates to multiple files) is far more burdensome.

### Query Complexity for Deep Hierarchies

**Challenge:** Ontologies with deep inheritance or complex relationships require sophisticated SPARQL queries. Property paths and subqueries can become hard to read.

**Mitigation:** Break complex queries into multiple rules. Use SPARQL's modularity (subqueries, common table expressions) to manage complexity. Document query intent clearly.

### Limited to Structured Data

**Challenge:** The sync pipeline works best when output structure maps cleanly to query results. Free-form prose or highly contextual content is harder to generate.

**Mitigation:** Use templates for structured code (schemas, types, validation) and author narrative documentation manually. Hybrid approaches are valid.

---

## When NOT to Use Sync

The sync architecture is powerful but not universal. Consider alternatives when:

### One-Off Generation

If you need to generate code once and then maintain it manually, a full ontology pipeline is overkill. Copy-paste or a simple script suffices.

### Rapidly Prototyping

During early exploration, editing generated code directly is faster than the ontology-query-template cycle. Adopt sync once the domain model stabilizes.

### Non-Schema Outputs

For outputs that do not derive from a domain model (configuration files, environment scripts, deployment manifests), other tools are more appropriate.

### Tiny Projects

Projects with two or three types rarely suffer from contract drift. The overhead of maintaining an ontology is not justified.

---

## The Ecosystem Connection

The sync command does not exist in isolation. It connects to UNRDF's broader architecture:

```
┌─────────────────────────────────────────────────────────────┐
│                    UNRDF Ecosystem                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  @unrdf/oxigraph  ─→  High-performance SPARQL engine        │
│                        (powers query execution)             │
│                                                             │
│  @unrdf/core      ─→  RDF primitives, prefix management     │
│                        (powers ontology loading)            │
│                                                             │
│  @unrdf/hooks     ─→  Policy enforcement                    │
│                        (validate generated artifacts)       │
│                                                             │
│  @unrdf/v6-core   ─→  Receipt generation                    │
│                        (audit trail for generation)         │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

The sync pipeline benefits from UNRDF's investment in performant RDF tooling. Oxigraph provides sub-millisecond query execution; the core library handles format parsing and prefix expansion.

---

## Summary

The sync architecture solves contract drift by establishing RDF as the single source of truth for your domain model. All outputs are derived through a deterministic pipeline:

1. **Ontology** defines the domain model with semantic precision
2. **SPARQL** extracts structured data for each output format
3. **Templates** transform data into language-specific artifacts
4. **Orchestrator** coordinates the pipeline and writes results

This approach trades upfront learning (RDF, SPARQL) for long-term consistency and maintainability. Teams that invest in the ontology-first workflow find that contract drift becomes impossible by design.

---

## Related Reading

**Conceptual:**
- [Template-Driven RDF Generation](./template-driven-rdf-generation.md) - Generating RDF from templates (the reverse direction)
- [Sync Template Patterns](./sync-template-patterns.md) - Common template design patterns

**Practical:**
- [How-To: Configure Sync Rules](../how-to/sync-configuration.md) - Set up your first generation pipeline
- [Reference: Template Filters](../reference/sync-template-filters.md) - Complete filter documentation

**Implementation:**
- Source: `/home/user/unrdf/packages/cli/src/cli/commands/sync/`
- Tests: `/home/user/unrdf/packages/cli/test/sync/`

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
