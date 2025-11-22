# RDF Ontology Guide

> Understanding the Papers-Thesis RDF schema

```json-ld
{
  "@context": {
    "owl": "http://www.w3.org/2002/07/owl#",
    "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
    "paper": "http://example.org/ontology/paper#",
    "thesis": "http://example.org/ontology/thesis#"
  },
  "@id": "urn:playground:ontology-guide:v1.0.0",
  "@type": "owl:Ontology"
}
```

---

## Ontology Metadata

| Property | Value |
|----------|-------|
| URI | `http://example.org/ontology/papers-thesis` |
| Version | 2.0.0 |
| Created | 2025-01-01 |
| Modified | 2025-11-21 |
| License | CC BY 4.0 |
| Imports | FOAF |

---

## Class Hierarchy Visualization

```
                    owl:Thing
                        |
              +---------+---------+
              |                   |
        paper:AcademicWork    foaf:Person
              |                   |
    +---------+---------+   +-----+-----+
    |                   |   |           |
paper:Paper      thesis:Thesis  paper:Author  thesis:CommitteeMember
    |                   |                           |
    +---+---+---+---+   +---+---+---+       +-------+-------+
    |   |   |   |   |   |   |   |           |               |
  IMRAD DSR Arg Cont Mon| Nar Cont    thesis:Advisor  thesis:ExternalExaminer
                      |
            +----+----+----+
            |    |    |    |
          PhD Master Bachelor

Supporting Classes:
  paper:Section
    |
    +-- paper:Chapter
    +-- paper:Subsection

  paper:Publisher
    |
    +-- paper:Conference
    +-- paper:Journal

  thesis:Schedule
  thesis:Milestone
  thesis:DefenseEvent
```

---

## Paper Domain Classes

### paper:AcademicWork

**Base class for all academic documents**

| Property | Domain | Range | Cardinality |
|----------|--------|-------|-------------|
| paper:hasTitle | AcademicWork | xsd:string | 1..1 |
| paper:hasAbstract | AcademicWork | xsd:string | 0..1 |
| paper:hasAuthor | AcademicWork | paper:Author | 1..* |
| paper:hasSection | AcademicWork | paper:Section | 0..* |
| paper:publicationYear | AcademicWork | xsd:gYear | 0..1 |
| paper:hasKeyword | AcademicWork | xsd:string | 0..* |
| paper:hasDOI | AcademicWork | xsd:string | 0..1 |

### paper:Paper

**Research paper (subclass of AcademicWork)**

| Property | Domain | Range | Description |
|----------|--------|-------|-------------|
| paper:paperFamily | Paper | xsd:string | Family: imrad, dsr, argument, contribution |
| paper:hasPublisher | Paper | paper:Publisher | Publication venue |
| paper:citationCount | Paper | xsd:nonNegativeInteger | Citation count |
| paper:cites | Paper | paper:AcademicWork | References |

### Paper Family Subclasses

| Class | Description | Required Sections |
|-------|-------------|-------------------|
| paper:IMRADPaper | Introduction, Methods, Results, Discussion | 4 minimum |
| paper:DSRPaper | Design Science Research | Problem, Objectives, Design, Demo, Eval, Comm |
| paper:ArgumentPaper | Argument-based | Thesis, Premises, Arguments, Counter, Conclusion |
| paper:ContributionPaper | Contribution-focused | Motivation, Background, Contribution, Validation |
| paper:MonographPaper | Single-topic study | Variable |
| paper:NarrativePaper | Narrative structure | Prologue, Background, Story, Analysis, Epilogue |

---

## Thesis Domain Classes

### thesis:Thesis

**Thesis document (subclass of AcademicWork)**

| Property | Domain | Range | Description |
|----------|--------|-------|-------------|
| thesis:thesisType | Thesis | xsd:string | Type: monograph, narrative, contribution |
| thesis:hasAdvisor | Thesis | thesis:Advisor | Primary advisor |
| thesis:institution | Thesis | xsd:string | Degree-granting institution |
| thesis:department | Thesis | xsd:string | Academic department |
| thesis:degree | Thesis | xsd:string | PhD, Masters, Bachelor |
| thesis:hasSchedule | Thesis | thesis:Schedule | Timeline |
| thesis:hasDefense | Thesis | thesis:DefenseEvent | Defense event |

### Thesis Type Subclasses

| Class | Description | Typical Chapters |
|-------|-------------|------------------|
| thesis:PhDThesis | Doctoral dissertation | 6-8 chapters |
| thesis:MastersThesis | Master's thesis | 5-6 chapters |
| thesis:BachelorsThesis | Undergraduate thesis | 4-5 chapters |

### thesis:DefenseEvent

| Property | Domain | Range | Description |
|----------|--------|-------|-------------|
| thesis:defenseDate | DefenseEvent | xsd:date | Defense date |
| thesis:defenseLocation | DefenseEvent | xsd:string | Location |
| thesis:defenseOutcome | DefenseEvent | xsd:string | pass, revisions, fail |
| thesis:hasCommitteeMember | DefenseEvent | thesis:CommitteeMember | Committee members (3+ required) |

### thesis:Schedule & thesis:Milestone

| Property | Domain | Range | Description |
|----------|--------|-------|-------------|
| thesis:hasMilestone | Schedule | Milestone | Milestones |
| thesis:milestoneName | Milestone | xsd:string | Milestone name |
| thesis:milestoneDate | Milestone | xsd:date | Target date |
| thesis:milestoneStatus | Milestone | xsd:string | pending, in-progress, completed, overdue |

---

## Property Relationships

### Object Properties

```turtle
paper:hasAuthor
    rdfs:domain paper:AcademicWork ;
    rdfs:range paper:Author ;
    owl:inverseOf paper:authorOf .

paper:hasSection
    rdfs:domain paper:AcademicWork ;
    rdfs:range paper:Section .

paper:hasParentSection
    rdfs:domain paper:Section ;
    rdfs:range paper:Section ;
    owl:inverseOf paper:hasSubsection .

paper:cites
    rdfs:domain paper:AcademicWork ;
    rdfs:range paper:AcademicWork ;
    owl:inverseOf paper:isCitedBy .

thesis:hasAdvisor
    rdfs:domain thesis:Thesis ;
    rdfs:range thesis:Advisor .

thesis:hasDefense
    rdfs:domain thesis:Thesis ;
    rdfs:range thesis:DefenseEvent .
```

### Datatype Properties

```turtle
paper:hasTitle
    rdfs:domain paper:AcademicWork ;
    rdfs:range xsd:string .

paper:sectionOrder
    rdfs:domain paper:Section ;
    rdfs:range xsd:integer .

thesis:defenseDate
    rdfs:domain thesis:DefenseEvent ;
    rdfs:range xsd:date .
```

---

## Instance Examples

### Paper Instance

```turtle
@prefix paper: <http://example.org/ontology/paper#> .
@prefix ex: <http://example.org/instances#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:paper-001 a paper:IMRADPaper ;
    paper:hasTitle "Machine Learning for Academic Writing" ;
    paper:hasAbstract "This paper presents..." ;
    paper:paperFamily "imrad" ;
    paper:publicationYear "2025"^^xsd:gYear ;
    paper:hasAuthor ex:alice ;
    paper:hasSection ex:paper-001-intro, ex:paper-001-methods,
                     ex:paper-001-results, ex:paper-001-discussion .

ex:alice a paper:Author ;
    foaf:name "Alice Smith" ;
    foaf:mbox <mailto:alice@example.org> ;
    paper:hasORCID "0000-0001-2345-6789" .

ex:paper-001-intro a paper:Section ;
    paper:sectionHeading "Introduction" ;
    paper:sectionOrder 1 ;
    paper:sectionLevel 1 .
```

### Thesis Instance

```turtle
@prefix thesis: <http://example.org/ontology/thesis#> .
@prefix paper: <http://example.org/ontology/paper#> .
@prefix ex: <http://example.org/instances#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:thesis-001 a thesis:PhDThesis ;
    paper:hasTitle "Autonomous Knowledge Systems" ;
    paper:hasAuthor ex:bob ;
    thesis:thesisType "monograph" ;
    thesis:degree "PhD" ;
    thesis:institution "MIT" ;
    thesis:department "Computer Science" ;
    thesis:hasAdvisor ex:dr-smith ;
    thesis:hasSchedule ex:thesis-001-schedule ;
    thesis:hasDefense ex:thesis-001-defense .

ex:thesis-001-schedule a thesis:Schedule ;
    thesis:hasMilestone ex:milestone-proposal, ex:milestone-defense .

ex:milestone-proposal a thesis:Milestone ;
    thesis:milestoneName "Thesis Proposal" ;
    thesis:milestoneDate "2024-09-01"^^xsd:date ;
    thesis:milestoneStatus "completed" .

ex:thesis-001-defense a thesis:DefenseEvent ;
    thesis:defenseDate "2025-06-15"^^xsd:date ;
    thesis:defenseLocation "Room 32-123" ;
    thesis:hasCommitteeMember ex:dr-smith, ex:dr-jones, ex:dr-external .
```

---

## SHACL Shape Catalog

### paper:PaperShape

```turtle
paper:PaperShape a sh:NodeShape ;
    sh:targetClass paper:Paper ;
    sh:property [
        sh:path paper:hasAuthor ;
        sh:minCount 1 ;
        sh:message "A paper must have at least one author."
    ] ;
    sh:property [
        sh:path paper:hasTitle ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:minLength 1 ;
        sh:datatype xsd:string ;
        sh:message "A paper must have exactly one non-empty title."
    ] .
```

### paper:IMRADPaperShape

```turtle
paper:IMRADPaperShape a sh:NodeShape ;
    sh:targetClass paper:IMRADPaper ;
    sh:property [
        sh:path paper:hasSection ;
        sh:minCount 4 ;
        sh:message "IMRAD paper must have at least Introduction, Methods, Results, Discussion."
    ] .
```

### thesis:ThesisShape

```turtle
thesis:ThesisShape a sh:NodeShape ;
    sh:targetClass thesis:Thesis ;
    sh:property [
        sh:path thesis:hasAdvisor ;
        sh:minCount 1 ;
        sh:message "A thesis must have at least one advisor."
    ] ;
    sh:property [
        sh:path thesis:institution ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
        sh:message "A thesis must specify an institution."
    ] .
```

### thesis:DefenseEventShape

```turtle
thesis:DefenseEventShape a sh:NodeShape ;
    sh:targetClass thesis:DefenseEvent ;
    sh:property [
        sh:path thesis:defenseDate ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:date ;
        sh:message "A defense must have exactly one defense date."
    ] ;
    sh:property [
        sh:path thesis:hasCommitteeMember ;
        sh:minCount 3 ;
        sh:message "A defense must have at least 3 committee members."
    ] .
```

---

## Constraint Validation

### Validation Query

```sparql
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?focusNode ?resultMessage ?resultPath
WHERE {
  ?report a sh:ValidationReport ;
          sh:result ?result .
  ?result sh:focusNode ?focusNode ;
          sh:resultMessage ?resultMessage .
  OPTIONAL { ?result sh:resultPath ?resultPath }
}
```

### Validation via CLI

```bash
# Validate paper structure
playground papers validate ./paper.ttl --strict

# Output includes SHACL violations
```

---

## Semantic Rules for Reasoning

### Subclass Inference

```n3
# All IMRADPapers are Papers
{ ?x a paper:IMRADPaper } => { ?x a paper:Paper } .

# All Papers are AcademicWorks
{ ?x a paper:Paper } => { ?x a paper:AcademicWork } .

# All PhDTheses are Theses
{ ?x a thesis:PhDThesis } => { ?x a thesis:Thesis } .
```

### Property Inference

```n3
# Inverse property inference
{ ?paper paper:hasAuthor ?author } => { ?author paper:authorOf ?paper } .
{ ?paper paper:cites ?cited } => { ?cited paper:isCitedBy ?paper } .
```

### Degree Inference

```n3
# PhDThesis has PhD degree
{ ?t a thesis:PhDThesis } => { ?t thesis:degree "PhD" } .

# MastersThesis has Masters degree
{ ?t a thesis:MastersThesis } => { ?t thesis:degree "Masters" } .
```

---

## Disjoint Classes

```turtle
# Paper types are mutually exclusive
paper:IMRADPaper owl:disjointWith
    paper:ArgumentPaper, paper:ContributionPaper,
    paper:DSRPaper, paper:NarrativePaper .

# Thesis types are mutually exclusive
thesis:PhDThesis owl:disjointWith
    thesis:MastersThesis, thesis:BachelorsThesis .
```

---

## Dublin Core Mappings

| DC Property | Ontology Mapping |
|-------------|------------------|
| dc:title | paper:hasTitle |
| dc:creator | paper:hasAuthor |
| dc:description | paper:hasAbstract |
| dc:subject | paper:hasKeyword |
| dc:date | paper:publicationYear |
| dc:publisher | paper:hasPublisher |
| dc:identifier | paper:hasDOI |

---

## Ontology Versioning

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-01-01 | Initial release |
| 2.0.0 | 2025-11-21 | Added SHACL shapes, thesis schedule, defense events |
