# Data Model: Package Maturity Matrix

**Feature**: 006-maturity-matrix
**Date**: 2025-12-20
**Phase**: 1 - Design

---

## 1. Entity Definitions

### 1.1 Package

Represents an UNRDF monorepo package with maturity assessment.

| Field                  | Type              | Required | Description                                         |
| ---------------------- | ----------------- | -------- | --------------------------------------------------- |
| `id`                   | IRI               | Yes      | `unrdf:package/{name}` (e.g., `unrdf:package/core`) |
| `name`                 | string            | Yes      | Package name (e.g., "core", "streaming")            |
| `description`          | string            | Yes      | Package description from README                     |
| `version`              | string            | Yes      | Current semver version                              |
| `maturityLevel`        | MaturityLevel     | Yes      | Assessed maturity level (L1-L5)                     |
| `maturityScore`        | decimal           | Yes      | Weighted score (0-100)                              |
| `coverage`             | CoverageMetrics   | Yes      | Test coverage data                                  |
| `apiStability`         | APIStability      | Yes      | API stability classification                        |
| `documentationQuality` | DocQuality        | Yes      | Documentation assessment                            |
| `testMaturity`         | TestMaturity      | Yes      | Test suite maturity                                 |
| `securityStatus`       | SecurityStatus    | Yes      | Security review status                              |
| `performanceStatus`    | PerfStatus        | Yes      | Performance benchmarking status                     |
| `synergies`            | SynergyCategory[] | Yes      | Synergy categories package participates in          |

### 1.2 MaturityLevel (Enumeration)

| Value       | Score | Label            | Criteria                        |
| ----------- | ----- | ---------------- | ------------------------------- |
| `L1_Alpha`  | 1     | Experimental     | <50% coverage, API unstable     |
| `L2_Beta`   | 2     | Development      | 50-70% coverage, basic docs     |
| `L3_RC`     | 3     | Pre-Production   | 70-85% coverage, stable API     |
| `L4_Stable` | 4     | Production-Ready | 85-95% coverage, semver-strict  |
| `L5_LTS`    | 5     | Enterprise       | 95%+ coverage, 24-month support |

### 1.3 CoverageMetrics

| Field         | Type     | Required | Description                      |
| ------------- | -------- | -------- | -------------------------------- |
| `lines`       | decimal  | Yes      | Line coverage percentage (0-100) |
| `branches`    | decimal  | Yes      | Branch coverage percentage       |
| `functions`   | decimal  | Yes      | Function coverage percentage     |
| `statements`  | decimal  | Yes      | Statement coverage percentage    |
| `collectedAt` | dateTime | Yes      | When coverage was collected      |

### 1.4 SynergyCategory

| Field           | Type      | Required | Description                                      |
| --------------- | --------- | -------- | ------------------------------------------------ |
| `id`            | IRI       | Yes      | `unrdf:synergy/{code}` (e.g., `unrdf:synergy/A`) |
| `code`          | string    | Yes      | Category code (A-K)                              |
| `name`          | string    | Yes      | Human-readable name                              |
| `packages`      | Package[] | Yes      | Packages in this synergy                         |
| `capability`    | string    | Yes      | Emergent capability description                  |
| `emergentValue` | string    | Yes      | What's enabled by combination                    |
| `useCases`      | string[]  | Yes      | Example use cases                                |

### 1.5 Assessment Criteria Enumerations

**APIStability**:

- `unstable` - Breaking changes frequent
- `stabilizing` - Core stable, edges changing
- `stable` - Rare breaking changes
- `very_stable` - Backward compatible
- `frozen` - No API changes

**DocQuality**:

- `minimal` - Basic README only
- `basic` - README + inline docs
- `good` - API reference included
- `excellent` - Guides + examples
- `professional` - Full docs site

**TestMaturity**:

- `unit_only` - Unit tests only
- `unit_integration` - + Integration tests
- `unit_integration_e2e` - + E2E tests
- `unit_integration_e2e_regression` - + Regression
- `comprehensive` - + Chaos/fuzz testing

**SecurityStatus**:

- `not_reviewed` - No security review
- `basic_scan` - Automated scan only
- `audit` - Manual audit completed
- `penetration_tested` - Pen test passed
- `hardened` - Security-hardened

**PerfStatus**:

- `untested` - No benchmarks
- `measured` - Basic metrics
- `optimized` - Performance tuned
- `benchmarked` - Regression suite
- `sla_bound` - SLA monitored

---

## 2. RDF Ontology (Turtle)

```turtle
@prefix mat: <https://unrdf.org/ontology/maturity#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix schema: <https://schema.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

# Ontology Metadata
<https://unrdf.org/ontology/maturity> a owl:Ontology ;
    rdfs:label "UNRDF Package Maturity Ontology" ;
    rdfs:comment "Ontology for assessing and tracking package maturity in UNRDF monorepo" ;
    owl:versionInfo "1.0.0" .

# ============ CLASSES ============

mat:Package a owl:Class ;
    rdfs:subClassOf schema:SoftwareSourceCode ;
    rdfs:label "UNRDF Package"@en ;
    rdfs:comment "A package in the UNRDF monorepo with maturity assessment"@en .

mat:MaturityLevel a owl:Class ;
    rdfs:label "Maturity Level"@en ;
    rdfs:comment "Classification of package readiness (L1-L5)"@en .

mat:SynergyCategory a owl:Class ;
    rdfs:label "Synergy Category"@en ;
    rdfs:comment "A grouping of packages that create emergent capabilities"@en .

mat:CoverageMetrics a owl:Class ;
    rdfs:label "Coverage Metrics"@en ;
    rdfs:comment "Test coverage data for a package"@en .

# ============ MATURITY LEVEL INSTANCES ============

mat:L1_Alpha a mat:MaturityLevel ;
    mat:levelScore 1 ;
    rdfs:label "Experimental (Alpha)"@en ;
    mat:coverageThreshold 50 .

mat:L2_Beta a mat:MaturityLevel ;
    mat:levelScore 2 ;
    rdfs:label "Development (Beta)"@en ;
    mat:coverageThreshold 70 .

mat:L3_RC a mat:MaturityLevel ;
    mat:levelScore 3 ;
    rdfs:label "Pre-Production (RC)"@en ;
    mat:coverageThreshold 85 .

mat:L4_Stable a mat:MaturityLevel ;
    mat:levelScore 4 ;
    rdfs:label "Production-Ready (Stable)"@en ;
    mat:coverageThreshold 95 .

mat:L5_LTS a mat:MaturityLevel ;
    mat:levelScore 5 ;
    rdfs:label "Enterprise (LTS)"@en ;
    mat:coverageThreshold 95 ;
    mat:supportWindow "P24M"^^xsd:duration .

# ============ OBJECT PROPERTIES ============

mat:hasMaturityLevel a owl:ObjectProperty ;
    rdfs:domain mat:Package ;
    rdfs:range mat:MaturityLevel ;
    rdfs:label "has maturity level"@en .

mat:participatesInSynergy a owl:ObjectProperty ;
    rdfs:domain mat:Package ;
    rdfs:range mat:SynergyCategory ;
    rdfs:label "participates in synergy"@en .

mat:hasCoverage a owl:ObjectProperty ;
    rdfs:domain mat:Package ;
    rdfs:range mat:CoverageMetrics ;
    rdfs:label "has coverage"@en .

mat:includesPackage a owl:ObjectProperty ;
    rdfs:domain mat:SynergyCategory ;
    rdfs:range mat:Package ;
    owl:inverseOf mat:participatesInSynergy ;
    rdfs:label "includes package"@en .

# ============ DATATYPE PROPERTIES ============

mat:name a owl:DatatypeProperty ;
    rdfs:domain mat:Package ;
    rdfs:range xsd:string .

mat:version a owl:DatatypeProperty ;
    rdfs:domain mat:Package ;
    rdfs:range xsd:string .

mat:maturityScore a owl:DatatypeProperty ;
    rdfs:domain mat:Package ;
    rdfs:range xsd:decimal ;
    rdfs:comment "Weighted maturity score (0-100)"@en .

mat:lineCoverage a owl:DatatypeProperty ;
    rdfs:domain mat:CoverageMetrics ;
    rdfs:range xsd:decimal .

mat:branchCoverage a owl:DatatypeProperty ;
    rdfs:domain mat:CoverageMetrics ;
    rdfs:range xsd:decimal .

mat:functionCoverage a owl:DatatypeProperty ;
    rdfs:domain mat:CoverageMetrics ;
    rdfs:range xsd:decimal .

mat:collectedAt a owl:DatatypeProperty ;
    rdfs:domain mat:CoverageMetrics ;
    rdfs:range xsd:dateTime .

mat:levelScore a owl:DatatypeProperty ;
    rdfs:domain mat:MaturityLevel ;
    rdfs:range xsd:integer .

mat:coverageThreshold a owl:DatatypeProperty ;
    rdfs:domain mat:MaturityLevel ;
    rdfs:range xsd:decimal .

mat:apiStability a owl:DatatypeProperty ;
    rdfs:domain mat:Package ;
    rdfs:range xsd:string .

mat:documentationQuality a owl:DatatypeProperty ;
    rdfs:domain mat:Package ;
    rdfs:range xsd:string .

mat:capability a owl:DatatypeProperty ;
    rdfs:domain mat:SynergyCategory ;
    rdfs:range xsd:string .

mat:emergentValue a owl:DatatypeProperty ;
    rdfs:domain mat:SynergyCategory ;
    rdfs:range xsd:string .
```

---

## 3. SHACL Shapes for Validation

```turtle
@prefix mat: <https://unrdf.org/ontology/maturity#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Package Shape
mat:PackageShape a sh:NodeShape ;
    sh:targetClass mat:Package ;
    sh:property [
        sh:path mat:name ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:pattern "^[a-z][a-z0-9-]*$" ;
        sh:message "Package name must be lowercase with hyphens"
    ] ;
    sh:property [
        sh:path mat:hasMaturityLevel ;
        sh:class mat:MaturityLevel ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:message "Package must have exactly one maturity level"
    ] ;
    sh:property [
        sh:path mat:maturityScore ;
        sh:datatype xsd:decimal ;
        sh:minInclusive 0 ;
        sh:maxInclusive 100 ;
        sh:message "Maturity score must be between 0 and 100"
    ] ;
    sh:property [
        sh:path mat:participatesInSynergy ;
        sh:class mat:SynergyCategory ;
        sh:minCount 1 ;
        sh:message "Package must participate in at least one synergy"
    ] .

# Coverage Metrics Shape
mat:CoverageMetricsShape a sh:NodeShape ;
    sh:targetClass mat:CoverageMetrics ;
    sh:property [
        sh:path mat:lineCoverage ;
        sh:datatype xsd:decimal ;
        sh:minInclusive 0 ;
        sh:maxInclusive 100
    ] ;
    sh:property [
        sh:path mat:branchCoverage ;
        sh:datatype xsd:decimal ;
        sh:minInclusive 0 ;
        sh:maxInclusive 100
    ] ;
    sh:property [
        sh:path mat:collectedAt ;
        sh:datatype xsd:dateTime ;
        sh:minCount 1
    ] .

# Synergy Category Shape
mat:SynergyCategoryShape a sh:NodeShape ;
    sh:targetClass mat:SynergyCategory ;
    sh:property [
        sh:path mat:includesPackage ;
        sh:class mat:Package ;
        sh:minCount 2 ;
        sh:message "Synergy must include at least 2 packages"
    ] ;
    sh:property [
        sh:path mat:capability ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:message "Synergy must define an emergent capability"
    ] .
```

---

## 4. Example Instance Data (Turtle)

```turtle
@prefix mat: <https://unrdf.org/ontology/maturity#> .
@prefix unrdf: <https://unrdf.org/package/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Package: core
unrdf:core a mat:Package ;
    mat:name "core" ;
    mat:version "5.0.0-beta.1" ;
    mat:hasMaturityLevel mat:L4_Stable ;
    mat:maturityScore 87.5 ;
    mat:apiStability "stable" ;
    mat:documentationQuality "excellent" ;
    mat:hasCoverage unrdf:core-coverage ;
    mat:participatesInSynergy mat:SynergyA, mat:SynergyB, mat:SynergyC,
        mat:SynergyD, mat:SynergyF, mat:SynergyG, mat:SynergyH, mat:SynergyI, mat:SynergyJ .

unrdf:core-coverage a mat:CoverageMetrics ;
    mat:lineCoverage 92.3 ;
    mat:branchCoverage 85.1 ;
    mat:functionCoverage 94.7 ;
    mat:collectedAt "2025-12-20T10:00:00Z"^^xsd:dateTime .

# Synergy Category A
mat:SynergyA a mat:SynergyCategory ;
    mat:name "Real-Time Knowledge Graph" ;
    mat:capability "Live, synchronized knowledge graphs across distributed systems" ;
    mat:emergentValue "Real-time collaborative knowledge graphs (impossible individually)" ;
    mat:includesPackage unrdf:core, unrdf:streaming, unrdf:federation, unrdf:domain .
```

---

## 5. State Transitions

### Package Maturity Progression

```
L1_Alpha → L2_Beta → L3_RC → L4_Stable → L5_LTS

Transition Criteria:
- L1→L2: Coverage ≥50%, basic docs, tests present
- L2→L3: Coverage ≥70%, API stable for 2 releases
- L3→L4: Coverage ≥85%, semver adherence, security audit
- L4→L5: Coverage ≥95%, 6+ months stable, commercial support
```

### Assessment Lifecycle

```
Pending → InProgress → Completed → Published

States:
- Pending: Assessment scheduled
- InProgress: Data collection active
- Completed: Scores calculated
- Published: Available in CLI/docs
```

---

## 6. Validation Rules

1. **Package Name**: Must match existing package in `packages/` directory
2. **Coverage Values**: Must be 0-100 decimal
3. **Maturity Level**: Must match one of L1-L5 instances
4. **Synergy Participation**: Every package must be in ≥1 synergy category
5. **Assessment Freshness**: Coverage data must be <30 days old for L4+
6. **Score Calculation**: Weighted sum must match individual criteria scores

---

**Status**: Data Model Complete
**Next**: Generate contracts/maturity-cli.json, quickstart.md
