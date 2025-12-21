---
description: Design OWL ontologies and SHACL shapes for UNRDF features, particularly the maturity matrix (mat: namespace)
---

# Ontology Designer

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Design semantic web ontologies using OWL and SHACL for UNRDF packages, with focus on the maturity assessment ontology (mat: namespace).

## Ontology Design Principles

### 1. Namespace Strategy

```turtle
# Core namespaces
@prefix mat: <https://unrdf.org/ontology/maturity#> .    # Maturity ontology
@prefix spec: <https://unrdf.org/ontology/spec#> .       # Specification ontology
@prefix unrdf: <https://unrdf.org/resource/> .           # Instance data

# Standard namespaces
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix schema: <https://schema.org/> .
```

### 2. Class Hierarchy

```turtle
# Top-level ontology
mat: a owl:Ontology ;
    owl:versionInfo "1.0.0" ;
    rdfs:label "UNRDF Maturity Ontology" ;
    rdfs:comment "Ontology for package maturity assessment" .

# Class hierarchy
mat:Package a owl:Class ;
    rdfs:subClassOf schema:SoftwareSourceCode ;
    rdfs:label "UNRDF Package" ;
    rdfs:comment "A package in the UNRDF monorepo" .

mat:MaturityLevel a owl:Class ;
    rdfs:label "Maturity Level" ;
    rdfs:comment "L1-L5 maturity classification" .

mat:Assessment a owl:Class ;
    rdfs:label "Maturity Assessment" ;
    rdfs:comment "A point-in-time maturity evaluation" .

mat:SynergyCategory a owl:Class ;
    rdfs:label "Synergy Category" ;
    rdfs:comment "Category of emergent capability from package combination" .
```

### 3. Property Design

```turtle
# Object Properties
mat:hasMaturityLevel a owl:ObjectProperty ;
    rdfs:domain mat:Package ;
    rdfs:range mat:MaturityLevel ;
    rdfs:label "has maturity level" .

mat:hasAssessment a owl:ObjectProperty ;
    rdfs:domain mat:Package ;
    rdfs:range mat:Assessment ;
    rdfs:label "has assessment" .

mat:participatesInSynergy a owl:ObjectProperty ;
    rdfs:domain mat:Package ;
    rdfs:range mat:SynergyCategory ;
    rdfs:label "participates in synergy" .

# Datatype Properties
mat:maturityScore a owl:DatatypeProperty ;
    rdfs:domain mat:Assessment ;
    rdfs:range xsd:decimal ;
    rdfs:label "maturity score" ;
    rdfs:comment "Weighted score 0-100" .

mat:coveragePercent a owl:DatatypeProperty ;
    rdfs:domain mat:Assessment ;
    rdfs:range xsd:decimal ;
    rdfs:label "test coverage percentage" .

mat:assessedAt a owl:DatatypeProperty ;
    rdfs:domain mat:Assessment ;
    rdfs:range xsd:dateTime ;
    rdfs:label "assessment timestamp" .
```

## SHACL Shapes

### Package Shape

```turtle
mat:PackageShape a sh:NodeShape ;
    sh:targetClass mat:Package ;
    sh:property [
        sh:path mat:hasMaturityLevel ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
        sh:class mat:MaturityLevel ;
        sh:name "maturity level" ;
        sh:description "Package must have at most one current maturity level"
    ] ;
    sh:property [
        sh:path schema:name ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
        sh:pattern "^@unrdf/[a-z][a-z0-9-]*$" ;
        sh:name "package name" ;
        sh:message "Package name must follow @unrdf/package-name pattern"
    ] .
```

### Assessment Shape

```turtle
mat:AssessmentShape a sh:NodeShape ;
    sh:targetClass mat:Assessment ;
    sh:property [
        sh:path mat:maturityScore ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:decimal ;
        sh:minInclusive 0 ;
        sh:maxInclusive 100 ;
        sh:name "maturity score" ;
        sh:message "Score must be between 0 and 100"
    ] ;
    sh:property [
        sh:path mat:coveragePercent ;
        sh:minCount 1 ;
        sh:datatype xsd:decimal ;
        sh:minInclusive 0 ;
        sh:maxInclusive 100 ;
        sh:name "coverage percentage"
    ] ;
    sh:property [
        sh:path mat:assessedAt ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:dateTime ;
        sh:name "assessment timestamp"
    ] .
```

## Maturity Level Instances

```turtle
# L1 - Alpha/Experimental
mat:L1_Alpha a mat:MaturityLevel ;
    mat:levelNumber 1 ;
    rdfs:label "Alpha" ;
    rdfs:comment "Experimental, API unstable, <50% coverage" ;
    mat:minCoverage 0 ;
    mat:maxCoverage 49.99 .

# L2 - Beta
mat:L2_Beta a mat:MaturityLevel ;
    mat:levelNumber 2 ;
    rdfs:label "Beta" ;
    rdfs:comment "Core features, stabilizing, 50-70% coverage" ;
    mat:minCoverage 50 ;
    mat:maxCoverage 69.99 .

# L3 - Release Candidate
mat:L3_RC a mat:MaturityLevel ;
    mat:levelNumber 3 ;
    rdfs:label "RC" ;
    rdfs:comment "Feature-complete, stable API, 70-85% coverage" ;
    mat:minCoverage 70 ;
    mat:maxCoverage 84.99 .

# L4 - Stable
mat:L4_Stable a mat:MaturityLevel ;
    mat:levelNumber 4 ;
    rdfs:label "Stable" ;
    rdfs:comment "Production-ready, battle-tested, 85-95% coverage" ;
    mat:minCoverage 85 ;
    mat:maxCoverage 94.99 .

# L5 - LTS
mat:L5_LTS a mat:MaturityLevel ;
    mat:levelNumber 5 ;
    rdfs:label "LTS" ;
    rdfs:comment "Enterprise-grade, extended support, 95%+ coverage" ;
    mat:minCoverage 95 ;
    mat:maxCoverage 100 .
```

## Synergy Category Instances

```turtle
# Category A: Real-Time Knowledge Graph
mat:SynergyA a mat:SynergyCategory ;
    rdfs:label "Real-Time Knowledge Graph" ;
    mat:categoryId "A" ;
    mat:capability "Live, synchronized knowledge graphs across distributed systems" ;
    mat:emergentValue "Real-time collaborative knowledge graphs" .

# Category B: Composable Queries
mat:SynergyB a mat:SynergyCategory ;
    rdfs:label "Composable Queries" ;
    mat:categoryId "B" ;
    mat:capability "Chainable SPARQL operations with type safety" ;
    mat:emergentValue "Complex queries built from simple primitives" .
```

## Output Format

```markdown
## Ontology Design Report

**Ontology**: [namespace]
**Version**: [version]
**Classes**: [count]
**Properties**: [count]
**Shapes**: [count]

### Class Hierarchy
```

owl:Thing
└── schema:SoftwareSourceCode
└── mat:Package
├── mat:CorePackage
└── mat:SpecializedPackage

```

### Property Summary
| Property | Domain | Range | Cardinality |
|----------|--------|-------|-------------|
| mat:hasMaturityLevel | Package | MaturityLevel | 0..1 |
| mat:maturityScore | Assessment | xsd:decimal | 1..1 |

### SHACL Shapes
| Shape | Target | Properties | Severity |
|-------|--------|------------|----------|
| PackageShape | mat:Package | 3 | Violation |
| AssessmentShape | mat:Assessment | 4 | Violation |

### Validation Status
- Syntax: ✅
- Consistency: ✅
- SHACL: ✅

### Files Generated
- packages/core/src/ontologies/maturity.ttl
- packages/core/src/ontologies/maturity-shapes.ttl
```

## Best Practices

1. **Use schema.org** as base vocabulary where applicable
2. **Define inverse properties** for bidirectional navigation
3. **Add rdfs:label and rdfs:comment** to all terms
4. **Use SHACL sh:severity** for validation levels
5. **Version the ontology** with owl:versionInfo
6. **Document with examples** in rdfs:comment

End Command ---
