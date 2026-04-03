---
description: Write specifications in RDF/Turtle format following UNRDF constitutional principles and maturity ontology
---

# Spec Writer

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Create specifications that conform to UNRDF's RDF-first approach, expressing requirements as semantic triples with proper ontology alignment.

## Constitutional Equation

```
spec.md = μ(feature.ttl)
```

Where `μ` is the transformation from RDF to Markdown. All specifications must be expressible as RDF triples.

## Specification Structure

### 1. RDF Ontology Header

```turtle
@prefix mat: <https://unrdf.org/ontology/maturity#> .
@prefix spec: <https://unrdf.org/ontology/spec#> .
@prefix schema: <https://schema.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Feature specification
<https://unrdf.org/specs/006-maturity-matrix>
    a spec:Feature ;
    spec:featureId "006-maturity-matrix" ;
    spec:status "active" ;
    spec:priority 1 .
```

### 2. User Stories as RDF

```turtle
# User Story 1: Maturity Assessment
spec:US1 a spec:UserStory ;
    spec:title "Maturity Assessment Framework" ;
    spec:actor spec:Developer ;
    spec:goal "Assess package maturity levels" ;
    spec:benefit "Know which packages are production-ready" ;
    spec:acceptanceCriteria [
        spec:criterion "CLI shows L1-L5 level" ;
        spec:criterion "Score calculated from 7 criteria" ;
        spec:criterion "Output in JSON/TTL/Table format"
    ] .
```

### 3. Requirements as SHACL Shapes

```turtle
# Requirement: Package must have coverage
spec:CoverageRequirement a sh:NodeShape ;
    sh:targetClass mat:Package ;
    sh:property [
        sh:path mat:coveragePercent ;
        sh:minCount 1 ;
        sh:datatype xsd:decimal ;
        sh:minInclusive 0 ;
        sh:maxInclusive 100 ;
    ] .
```

## Execution Steps

### 1. Initialize Spec Context

```bash
# Check for existing feature directory
FEATURE_ID="${ARGUMENTS:-new-feature}"
FEATURE_DIR="specs/$FEATURE_ID"
mkdir -p "$FEATURE_DIR"
```

### 2. Create RDF Spec File

```bash
# Create feature.ttl
cat > "$FEATURE_DIR/feature.ttl" << 'EOF'
@prefix spec: <https://unrdf.org/ontology/spec#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<https://unrdf.org/specs/${FEATURE_ID}>
    a spec:Feature ;
    spec:featureId "${FEATURE_ID}" ;
    spec:createdAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)"^^xsd:dateTime ;
    spec:status "draft" .
EOF
```

### 3. Validate RDF Syntax

```bash
# Use Node.js for validation
node -e "
import { createStore } from '@unrdf/oxigraph';
import { readFileSync } from 'fs';
const store = createStore();
const ttl = readFileSync('$FEATURE_DIR/feature.ttl', 'utf8');
store.load(ttl, { format: 'text/turtle' });
console.log('✅ Valid Turtle syntax');
"
```

### 4. Generate Markdown from RDF

Transform RDF to spec.md using this template:

```markdown
# Specification: [Feature Title]

**Feature ID**: [from spec:featureId]
**Status**: [from spec:status]
**Created**: [from spec:createdAt]

## Overview

[from spec:description]

## User Stories

### US1: [from spec:title]

**As a** [spec:actor]
**I want to** [spec:goal]
**So that** [spec:benefit]

**Acceptance Criteria:**

- [from spec:acceptanceCriteria]

## Requirements

### Functional Requirements

[from SHACL shapes with sh:severity sh:Violation]

### Non-Functional Requirements

[from SHACL shapes tagged with spec:nfr true]

## Data Model

[from OWL class definitions]

## Edge Cases

[from spec:edgeCase instances]
```

## Output Format

```markdown
## Spec Writer Report

**Feature**: [feature ID]
**Output**: [file path]

### Created Artifacts

- feature.ttl (RDF specification)
- spec.md (Markdown documentation)
- shapes.ttl (SHACL validation)

### Validation

- Turtle Syntax: ✅/❌
- SHACL Shapes: ✅/❌
- Constitution Alignment: ✅/❌

### Next Steps

1. Run /speckit.plan to create implementation plan
2. Run /speckit.tasks to generate task list
```

## Template: User Story RDF

```turtle
spec:US${N} a spec:UserStory ;
    spec:title "${TITLE}" ;
    spec:priority ${PRIORITY} ;
    spec:actor spec:${ACTOR} ;
    spec:goal "${GOAL}" ;
    spec:benefit "${BENEFIT}" ;
    spec:acceptanceCriteria [
        spec:criterion "${CRITERION_1}" ;
        spec:criterion "${CRITERION_2}"
    ] ;
    spec:testable true .
```

## Template: Requirement SHACL

```turtle
spec:${REQ_ID} a sh:NodeShape ;
    sh:targetClass ${TARGET_CLASS} ;
    sh:severity sh:${SEVERITY} ;
    spec:nfr ${IS_NFR} ;
    sh:property [
        sh:path ${PROPERTY_PATH} ;
        sh:minCount ${MIN} ;
        sh:maxCount ${MAX} ;
        sh:datatype ${DATATYPE}
    ] ;
    sh:message "${ERROR_MESSAGE}" .
```

End Command ---
