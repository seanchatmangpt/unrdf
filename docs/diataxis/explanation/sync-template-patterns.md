# Template Design Patterns for Code Generation

Understanding the principles behind effective template design for RDF-to-code generation.

---

## Overview

Code generation from RDF vocabularies presents unique challenges. Unlike traditional code generation from schemas or IDL files, RDF data is inherently graph-structured, loosely typed, and often incomplete. The patterns described here address these challenges by establishing conventions that produce consistent, maintainable output.

This document explains **why** these patterns exist, not how to implement them. For implementation details, see the how-to guides.

---

## 1. Frontmatter Convention

### Why YAML Frontmatter?

Templates serve two distinct purposes: they define **what** to generate and **where** to put it. Mixing these concerns within template logic creates coupling that makes templates harder to reuse and reason about.

```
┌─────────────────────────────────────────────┐
│            Template File                    │
├─────────────────────────────────────────────┤
│  ┌───────────────────────────────────────┐  │
│  │         YAML Frontmatter              │  │
│  │  - Output location                    │  │
│  │  - Template metadata                  │  │
│  │  - Configuration hints                │  │
│  └───────────────────────────────────────┘  │
│  ┌───────────────────────────────────────┐  │
│  │         Template Body                 │  │
│  │  - Content generation logic           │  │
│  │  - Data transformation                │  │
│  │  - Output structure                   │  │
│  └───────────────────────────────────────┘  │
└─────────────────────────────────────────────┘
```

YAML frontmatter provides a **declarative** boundary between orchestration concerns (where files go) and generation concerns (what files contain). This separation enables:

- **Tooling introspection**: Build systems can determine output structure without parsing templates
- **Dry-run capabilities**: Output paths can be computed without executing templates
- **Validation**: Output path patterns can be validated against project structure

### Output Path Templating

Output paths often depend on the data being processed. A template generating TypeScript interfaces needs the class name to determine the filename. Rather than embedding this logic in the template body:

```
BAD: Template computes its own filename
     ↓ Requires special output handling
     ↓ Cannot predict outputs before execution
     ↓ Breaks when template logic changes
```

The frontmatter declares the **pattern** while the runtime resolves it:

```yaml
output: "{{ className | kebabCase }}.ts"
```

This inversion of control keeps templates focused on content generation while delegating file organization to the orchestration layer.

### Metadata Fields

Beyond output paths, frontmatter communicates intent to both humans and tools:

| Field | Purpose |
|-------|---------|
| `output` | Declares where generated content belongs |
| `description` | Documents template purpose for maintenance |
| `requires` | Declares data dependencies for validation |
| `partials` | Lists partial templates for dependency tracking |

Metadata serves as a **contract** between template authors and the generation system.

---

## 2. SPARQL Query Design

### The Fundamental Tension

SPARQL operates on graphs. Templates operate on trees. This impedance mismatch is the central challenge of RDF-based code generation.

```
RDF Graph Structure              Template Data Structure

    ┌───────┐                    {
    │ Class │                      "class": {
    └───┬───┘                        "name": "Person",
        │ hasProperty                "properties": [
   ┌────┴────┐                         { "name": "age" },
   ▼         ▼                         { "name": "name" }
┌──────┐ ┌──────┐                    ]
│ Prop │ │ Prop │                  }
└──────┘ └──────┘                }
```

SPARQL queries must bridge this gap by projecting graph patterns into tabular results that can be transformed into hierarchical structures.

### Denormalized vs Normalized Results

Consider a class with multiple properties. Two query strategies exist:

**Normalized** (one row per property):
```
| class   | property |
|---------|----------|
| Person  | age      |
| Person  | name     |
```

**Denormalized** (repeating class data):
```
| class  | classLabel | property | propertyType |
|--------|------------|----------|--------------|
| Person | Person     | age      | integer      |
| Person | Person     | name     | string       |
```

Normalized results require post-processing to reconstruct the hierarchy. Denormalized results contain redundancy but map directly to grouping operations.

**The pattern**: Prefer denormalized results because:

1. Templates can group directly without intermediate processing
2. All data for an entity is co-located in results
3. Missing optional fields appear as nulls rather than absent rows

The redundancy cost is minimal for typical vocabulary sizes, and the simplification of template logic is substantial.

### OPTIONAL for Nullable Fields

RDF's open-world assumption means any property might be absent. SPARQL's `OPTIONAL` clause handles this gracefully:

```
Without OPTIONAL          With OPTIONAL
─────────────────        ──────────────────
Query requires           Query returns row
property to exist        with null for
                         missing property
        │                        │
        ▼                        ▼
Missing property         Template handles
= No result row          null explicitly
```

Using `OPTIONAL` for nullable fields ensures:

- Results reflect **available** data, not just **complete** data
- Templates can apply default values uniformly
- Generation produces output even from partial vocabularies

### ORDER BY for Consistent Output

Code generation must be **deterministic**. The same input vocabulary must produce byte-identical output. SPARQL result ordering is undefined by default.

```
Run 1: age, name, email
Run 2: name, age, email    ← Different order
Run 3: email, age, name    ← Different order again
```

This breaks:
- Version control (spurious diffs)
- Caching (false cache misses)
- Testing (flaky comparisons)

`ORDER BY` establishes deterministic ordering. The specific order matters less than its consistency. Alphabetical by local name is conventional.

### FILTER Clauses for Scoping

Not everything in an RDF graph belongs in generated code. `FILTER` clauses scope queries to relevant data:

```sparql
FILTER(STRSTARTS(STR(?class), STR(schema:)))
```

This pattern:
- Excludes imported vocabulary terms
- Focuses on the target namespace
- Prevents accidental generation from dependencies

---

## 3. Template Organization Patterns

### One Template Per Output File

The relationship between templates and outputs should be **predictable**:

```
PREDICTABLE                    UNPREDICTABLE
────────────                   ─────────────
template A → file A            template A → file A
template B → file B                      → file B
template C → file C                      → file C

Easy to trace                  Hard to debug
Easy to modify                 Risk of conflicts
Easy to test                   Complex dependencies
```

One-to-one mapping means:
- Finding the template for any output is trivial
- Changing one output cannot affect others
- Templates can be tested in isolation

When a template needs to generate multiple files (e.g., one per class), the template still produces **one conceptual output**. The iteration happens at the orchestration layer.

### Partials for Reusable Fragments

Code generation often repeats patterns. JSDoc blocks, import statements, and type annotations appear across many outputs. Duplication creates maintenance burden.

```
┌──────────────────┐     ┌──────────────────┐
│  class.njk       │     │  interface.njk   │
├──────────────────┤     ├──────────────────┤
│ {% include       │     │ {% include       │
│   "jsdoc.njk" %} │     │   "jsdoc.njk" %} │
│                  │     │                  │
│ class {{ name }} │     │ interface        │
│ { ... }          │     │ {{ name }} {...} │
└──────────────────┘     └──────────────────┘
         │                        │
         └────────┬───────────────┘
                  ▼
         ┌──────────────┐
         │  jsdoc.njk   │  ← Single source of truth
         │  (partial)   │
         └──────────────┘
```

Partials should be:
- **Focused**: One partial, one purpose
- **Parameterized**: Accept data, don't assume context
- **Documented**: Declare expected inputs

### Index Templates for Barrel Exports

Generated code often needs aggregation. A module exporting all generated types requires knowledge of all types. Rather than hard-coding this list:

```javascript
// Hard-coded (brittle)
export { Person } from './person.js';
export { Organization } from './organization.js';
// Must update when types change
```

Index templates query the same vocabulary:

```
┌─────────────────────────────────────┐
│          RDF Vocabulary             │
└──────────────┬──────────────────────┘
               │
       ┌───────┴───────┐
       ▼               ▼
┌────────────┐  ┌────────────┐
│ type.njk   │  │ index.njk  │
│ (per type) │  │ (all types)│
└────────────┘  └────────────┘
       │               │
       ▼               ▼
  person.ts       index.ts
  org.ts          (re-exports all)
```

Both templates derive from the same source, ensuring consistency.

---

## 4. Grouping Pattern

### When Grouping Is Necessary

SPARQL returns flat tabular data. Generated code is hierarchical. The grouping pattern bridges this gap.

Consider generating a class with properties:

```
SPARQL Results (flat)
┌─────────┬──────────┬──────────────┐
│ class   │ property │ propertyType │
├─────────┼──────────┼──────────────┤
│ Person  │ name     │ string       │
│ Person  │ age      │ integer      │
│ Person  │ email    │ string       │
└─────────┴──────────┴──────────────┘

Desired Structure (hierarchical)
{
  "Person": {
    "properties": [
      { "name": "name", "type": "string" },
      { "name": "age", "type": "integer" },
      { "name": "email", "type": "string" }
    ]
  }
}
```

### The groupBy Filter

Grouping transforms a flat array into a nested structure by a key field:

```
Before groupBy('class')           After groupBy('class')
─────────────────────            ──────────────────────
[                                {
  { class: 'A', prop: 1 },         'A': [
  { class: 'A', prop: 2 },           { class: 'A', prop: 1 },
  { class: 'B', prop: 3 }            { class: 'A', prop: 2 }
]                                  ],
                                   'B': [
                                     { class: 'B', prop: 3 }
                                   ]
                                 }
```

Use groupBy when:
- Multiple result rows represent parts of one entity
- Output structure requires nesting
- You need to iterate "for each X, all its Y"

### Entity-Property Aggregation

The most common grouping scenario aggregates properties under their owning entity:

```
Query returns: class × property × details
              (Cartesian product-like)

Group by class → Each class with all its properties
               → Natural fit for code generation
```

This pattern appears in:
- Class/interface generation (properties under class)
- Module generation (exports under module)
- Documentation generation (methods under type)

### Nested Output Generation

Sometimes two levels of grouping are needed:

```
┌──────────────────────────────────────────┐
│ namespace1/                              │
│   ├── ClassA.ts                          │
│   └── ClassB.ts                          │
│ namespace2/                              │
│   └── ClassC.ts                          │
└──────────────────────────────────────────┘
```

This requires:
1. Group by namespace (determines directory)
2. Group by class within namespace (determines file)

Deeply nested grouping suggests reconsidering template boundaries. Often, separate templates at each level produce cleaner results.

---

## 5. Type Mapping Pattern

### The Type Impedance Problem

RDF types (XSD datatypes, custom types) don't correspond directly to programming language types:

```
XSD                      TypeScript
───                      ──────────
xsd:string         →     string
xsd:integer        →     number
xsd:decimal        →     number      (same!)
xsd:dateTime       →     Date
xsd:anyURI         →     string      (lossy!)
custom:Money       →     ???
```

Type mapping resolves this impedance mismatch.

### Centralized Type Conversion

Scattering type conversion logic throughout templates creates inconsistency:

```
BAD: Distributed type logic
─────────────────────────
Template A: xsd:integer → "number"
Template B: xsd:integer → "Number"  ← Inconsistent!
Template C: forgot to handle xsd:integer
```

Centralizing type mapping in a single function or lookup table ensures:
- Consistent type names across all outputs
- Single place to add new type mappings
- Explicit handling of edge cases

```
GOOD: Centralized type map
────────────────────────
┌─────────────────────┐
│    Type Map         │
│ ─────────────────── │
│ xsd:string → string │
│ xsd:integer → number│
│ ...                 │
└─────────┬───────────┘
          │
    ┌─────┴─────┐
    ▼           ▼
Template A  Template B
(uses map)  (uses map)
```

### Handling Unknown Types

Not all RDF types can be mapped. The strategy for unknowns depends on context:

| Strategy | When to Use |
|----------|-------------|
| Fail loudly | Strict generation, catch issues early |
| Fall back to `any`/`unknown` | Permissive generation, runtime handles |
| Preserve IRI as comment | Documentation, traceability |

The choice depends on whether you prefer **build-time safety** or **runtime flexibility**.

### Custom Type Extensions

Vocabularies often define domain-specific types. The type mapping system should be extensible:

```
┌──────────────────────────────────────┐
│           Type Resolution            │
│ ──────────────────────────────────── │
│  1. Check custom type map            │
│  2. Check XSD built-in map           │
│  3. Apply unknown type strategy      │
└──────────────────────────────────────┘
```

Custom mappings take precedence, allowing domain-specific types to map to domain-specific implementations (e.g., `schema:DateTime` → `Temporal.ZonedDateTime`).

---

## 6. Error Prevention

### Required vs Optional Fields

Templates must distinguish between fields that **must exist** and fields that **might exist**:

```
Required Field Missing           Optional Field Missing
─────────────────────           ──────────────────────
Generation should FAIL          Generation should SUCCEED
                                with sensible default

Error message identifies        Output may contain
missing data                    placeholder or omit
```

This distinction should be:
- Declared in template comments/frontmatter
- Enforced by validation before template execution
- Reflected in SPARQL query structure (non-OPTIONAL vs OPTIONAL)

### Safe Navigation in Templates

Template engines differ in handling undefined values:

```
Unsafe Navigation             Safe Navigation
─────────────────            ────────────────
{{ entity.type.name }}       {{ entity.type.name | default('unknown') }}
       │                            │
       ▼                            ▼
Throws if type                Returns 'unknown' if
is undefined                  type is undefined
```

Defensive patterns:
- **Default filters**: Provide fallback values
- **Conditional blocks**: Check existence before access
- **Null coalescing**: Use `??` or equivalent operators

### Default Values

When optional data is absent, defaults should be:

| Characteristic | Why It Matters |
|----------------|----------------|
| Explicit | Reader knows it's a default, not real data |
| Safe | Won't cause runtime errors |
| Documented | Comment explains the default's purpose |

```javascript
// GOOD: Explicit, documented default
/** @type {string} Default indicates no RDF description provided */
description: '(No description available)'

// BAD: Silent, potentially confusing default
description: ''
```

---

## Summary

These patterns emerge from the fundamental challenges of RDF-to-code generation:

1. **Frontmatter** separates orchestration from generation
2. **SPARQL design** bridges graph-to-tree impedance
3. **Template organization** maintains predictable structure
4. **Grouping** reconstructs hierarchy from flat results
5. **Type mapping** handles type system differences
6. **Error prevention** builds robust templates

The underlying principle: **make the implicit explicit**. Template behavior should be predictable from its declaration, not discovered through execution.

---

## Related Documentation

- **How-to**: [Writing Custom Templates](/docs/diataxis/how-to/sync-custom-templates.md)
- **Tutorial**: [Generating TypeScript from RDF](/docs/diataxis/tutorials/sync-typescript-tutorial.md)
- **Reference**: [Template Function Reference](/docs/diataxis/reference/sync-template-functions.md)
