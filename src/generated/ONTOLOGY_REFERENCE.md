# UNRDF Domain Ontology Reference

> Auto-generated from `schema/domain.ttl` via ggen

**Version**: 6.0.0-rc.1
**Generated**: 2025-01-01T00:00:00Z
**Namespace**: `https://unrdf.io/ns#`

---

## Classes

### Package

**URI**: `https://unrdf.io/ns#Package`

A software package in the UNRDF monorepo

**Properties**:
- `packageName` (string) - The name of the package
- `packageVersion` (string) - The semantic version of the package
- `packageDescription` (string) - Description of the package's purpose and features
- `hasTier` (Tier) - The package's tier classification
- `hasDependency` (Package[]) - Package dependency relationships

**Examples**:
- `@unrdf/core` (Essential tier)
- `@unrdf/oxigraph` (Essential tier)
- `@unrdf/federation` (Extended tier)

---

### Tier

**URI**: `https://unrdf.io/ns#Tier`

Classification tier for packages (Essential, Extended, Optional)

**Properties**:
- `tierName` (string) - Name of the tier (Essential, Extended, Optional, Internal)

**Valid Values**:
- Essential - Core packages always needed for UNRDF operation
- Extended - Commonly used packages for typical use cases
- Optional - Performance and optional feature packages
- Internal - Internal testing and validation packages

---

### Module

**URI**: `https://unrdf.io/ns#Module`

A JavaScript ESM module (.mjs file)

**Properties**:
- `moduleName` (string) - The name of the module file
- `moduleExports` (string) - Comma-separated list of exported symbols

---

### KnowledgeGraph

**URI**: `https://unrdf.io/ns#KnowledgeGraph`

An RDF knowledge graph instance

**Properties**:
- `graphTripleCount` (integer) - Number of RDF triples in the graph

---

### Hook

**URI**: `https://unrdf.io/ns#Hook`

A policy hook for event-driven execution

**Properties**:
- `hookId` (string) - Hook identifier
- `hookName` (string) - Hook name
- `hookTrigger` (string) - Hook trigger conditions
- `hookAction` (string) - Hook action

---

### Receipt

**URI**: `https://unrdf.io/ns#Receipt`

Cryptographic receipt for transaction verification

**Properties**:
- `receiptHash` (string) - Cryptographic hash of the receipt
- `receiptTimestamp` (datetime) - When the receipt was created
- `merklePath` (string[]) - Merkle tree path for verification

---

### Delta

**URI**: `https://unrdf.io/ns#Delta`

A change delta in the knowledge graph

**Properties**:
- `deltaTimestamp` (datetime) - When the delta was created
- `addedTriples` (Triple[]) - Triples added in this delta
- `removedTriples` (Triple[]) - Triples removed in this delta
- `deltaHash` (string) - Cryptographic hash of the delta

---

## Properties

| URI | Domain | Range | Comment |
|-----|--------|-------|---------|
| `packageName` | Package | xsd:string | The name of the package |
| `packageVersion` | Package | xsd:string | The semantic version of the package |
| `packageDescription` | Package | xsd:string | Description of the package |
| `hasTier` | Package | Tier | The package's tier classification |
| `hasDependency` | Package | Package | Package dependency relationship |
| `moduleName` | Module | xsd:string | The name of the module file |
| `moduleExports` | Module | xsd:string | Comma-separated exported symbols |
| `graphTripleCount` | KnowledgeGraph | xsd:integer | Number of RDF triples in the graph |
| `receiptHash` | Receipt | xsd:string | Cryptographic hash of the receipt |
| `deltaTimestamp` | Delta | xsd:dateTime | When the delta was created |

---

## Tier Instances

### Essential Tier

Core packages always needed for UNRDF operation.

**Packages**:
- @unrdf/core
- @unrdf/oxigraph
- @unrdf/kgc-4d
- @unrdf/hooks
- @unrdf/streaming
- @unrdf/v6-core
- @unrdf/yawl

### Extended Tier

Commonly used packages for typical use cases.

**Packages**:
- @unrdf/federation
- @unrdf/knowledge-engine
- @unrdf/cli
- @unrdf/kgc-runtime
- @unrdf/kgc-substrate
- @unrdf/receipts
- @unrdf/consensus
- @unrdf/v6-compat

### Optional Tier

Performance and optional feature packages.

### Internal Tier

Internal testing and validation packages.

---

## Package Instances

### @unrdf/core

- **Tier**: Essential
- **Version**: 6.0.0-rc.1
- **Description**: RDF Graph Operations, SPARQL, Foundational Substrate
- **Dependencies**: None

### @unrdf/oxigraph

- **Tier**: Essential
- **Version**: 6.0.0-rc.1
- **Description**: Oxigraph SPARQL engine binding (10-100x faster than N3)
- **Dependencies**: @unrdf/core

### @unrdf/federation

- **Tier**: Extended
- **Version**: 6.0.0-rc.1
- **Description**: Distributed RDF Query with RAFT Consensus
- **Dependencies**: @unrdf/core

### @unrdf/knowledge-engine

- **Tier**: Extended
- **Version**: 6.0.0-rc.1
- **Description**: Rule Engine, Inference, Pattern Matching
- **Dependencies**: @unrdf/core, @unrdf/oxigraph

---

## Relationships Map

```
@unrdf/core
  ├─→ @unrdf/oxigraph
  ├─→ @unrdf/kgc-4d
  ├─→ @unrdf/hooks
  ├─→ @unrdf/streaming
  ├─→ @unrdf/v6-core
  ├─→ @unrdf/yawl
  ├─→ @unrdf/federation
  │    └─→ @unrdf/consensus
  ├─→ @unrdf/knowledge-engine
  ├─→ @unrdf/cli
  └─→ @unrdf/receipts
```

---

## SPARQL Queries

### Find all packages in a tier

```sparql
PREFIX unrdf: <https://unrdf.io/ns#>

SELECT ?package ?name ?description
WHERE {
  ?tier unrdf:tierName "Essential" .
  ?package unrdf:hasTier ?tier ;
           unrdf:packageName ?name ;
           unrdf:packageDescription ?description .
}
ORDER BY ?name
```

### Find packages with dependencies

```sparql
PREFIX unrdf: <https://unrdf.io/ns#>

SELECT ?package ?name ?dependency ?depName
WHERE {
  ?package unrdf:packageName ?name ;
           unrdf:hasDependency ?dependency .
  ?dependency unrdf:packageName ?depName .
}
ORDER BY ?package
```

### Count triples in knowledge graphs

```sparql
PREFIX unrdf: <https://unrdf.io/ns#>

SELECT ?graph (SUM(?count) as ?totalTriples)
WHERE {
  ?graph a unrdf:KnowledgeGraph ;
         unrdf:graphTripleCount ?count .
}
GROUP BY ?graph
```

---

## Statistics

- **Total Classes**: 7
- **Total Properties**: 42
- **Total Instances**: 15+
- **Namespaces**: 6
- **RDF Triples**: 87

---

Generated by [ggen](https://github.com/seanchatmangpt/ggen) v5.0.2
