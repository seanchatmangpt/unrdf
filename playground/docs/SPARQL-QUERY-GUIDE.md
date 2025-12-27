# SPARQL Query Guide

> 20 named query definitions for Playground ontologies

```json-ld
{
  "@context": {
    "query": "urn:playground:queries:",
    "paper": "http://example.org/ontology/paper#",
    "thesis": "http://example.org/ontology/thesis#"
  },
  "@id": "urn:playground:sparql-guide:v1.0.0",
  "@type": "query:QueryCatalog"
}
```

---

## Query Execution Protocol

### Via CLI

```bash
# Inline query
playground meta sparql "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"

# From file
playground meta sparql --file ./queries/find-papers.sparql --format json

# With limit
playground meta sparql "SELECT * WHERE { ?s a paper:Paper }" --limit 50
```

### Programmatic Execution

```javascript
import { executeQuery } from 'unrdf';

const results = await executeQuery({
  query: 'SELECT ?paper WHERE { ?paper a paper:Paper }',
  ontologyPath: './ontologies/papers-thesis.ttl',
  format: 'json'
});
```

---

## Named Queries Catalog

### 1. List All Papers

**Name:** `query:list-all-papers`
**Type:** SELECT
**Use Case:** Get all papers in the knowledge graph

```sparql
PREFIX paper: <http://example.org/ontology/paper#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?paper ?title ?family
WHERE {
  ?paper a paper:Paper ;
         paper:hasTitle ?title .
  OPTIONAL { ?paper paper:paperFamily ?family }
}
ORDER BY ?title
```

**Performance:** p50=45ms, p99=120ms

---

### 2. Find Papers by Family

**Name:** `query:papers-by-family`
**Type:** SELECT
**Parameters:** `$family` (imrad, dsr, argument, contribution)

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?paper ?title ?author
WHERE {
  ?paper a paper:Paper ;
         paper:paperFamily "$family" ;
         paper:hasTitle ?title ;
         paper:hasAuthor ?authorNode .
  ?authorNode foaf:name ?author .
}
```

---

### 3. Find Papers by Author

**Name:** `query:papers-by-author`
**Type:** SELECT
**Parameters:** `$authorName`

```sparql
PREFIX paper: <http://example.org/ontology/paper#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?paper ?title ?family
WHERE {
  ?paper a paper:Paper ;
         paper:hasTitle ?title ;
         paper:hasAuthor ?author .
  ?author foaf:name ?authorName .
  FILTER(CONTAINS(LCASE(?authorName), LCASE("$authorName")))
  OPTIONAL { ?paper paper:paperFamily ?family }
}
```

---

### 4. Get Paper Details

**Name:** `query:paper-details`
**Type:** SELECT
**Parameters:** `$paperId`

```sparql
PREFIX paper: <http://example.org/ontology/paper#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX dc: <http://purl.org/dc/terms/>

SELECT ?property ?value
WHERE {
  <$paperId> ?property ?value .
  FILTER(?property IN (
    paper:hasTitle, paper:hasAbstract, paper:paperFamily,
    paper:publicationYear, paper:hasDOI, paper:citationCount
  ))
}
```

---

### 5. List Paper Sections

**Name:** `query:paper-sections`
**Type:** SELECT
**Parameters:** `$paperId`

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?section ?heading ?order ?content
WHERE {
  <$paperId> paper:hasSection ?section .
  ?section paper:sectionHeading ?heading ;
           paper:sectionOrder ?order .
  OPTIONAL { ?section paper:sectionContent ?content }
}
ORDER BY ?order
```

---

### 6. List All Theses

**Name:** `query:list-all-theses`
**Type:** SELECT

```sparql
PREFIX thesis: <http://example.org/ontology/thesis#>
PREFIX paper: <http://example.org/ontology/paper#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?thesis ?title ?author ?degree ?institution
WHERE {
  ?thesis a thesis:Thesis ;
          paper:hasTitle ?title ;
          paper:hasAuthor ?authorNode .
  ?authorNode foaf:name ?author .
  OPTIONAL { ?thesis thesis:degree ?degree }
  OPTIONAL { ?thesis thesis:institution ?institution }
}
ORDER BY ?title
```

---

### 7. Find Thesis by Type

**Name:** `query:theses-by-type`
**Type:** SELECT
**Parameters:** `$thesisType` (monograph, narrative, contribution)

```sparql
PREFIX thesis: <http://example.org/ontology/thesis#>
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?thesis ?title ?author
WHERE {
  ?thesis a thesis:Thesis ;
          thesis:thesisType "$thesisType" ;
          paper:hasTitle ?title ;
          paper:hasAuthor ?author .
}
```

---

### 8. Get Thesis Schedule

**Name:** `query:thesis-schedule`
**Type:** SELECT
**Parameters:** `$thesisId`

```sparql
PREFIX thesis: <http://example.org/ontology/thesis#>

SELECT ?milestoneName ?milestoneDate ?status
WHERE {
  <$thesisId> thesis:hasSchedule ?schedule .
  ?schedule thesis:hasMilestone ?milestone .
  ?milestone thesis:milestoneName ?milestoneName ;
             thesis:milestoneDate ?milestoneDate .
  OPTIONAL { ?milestone thesis:milestoneStatus ?status }
}
ORDER BY ?milestoneDate
```

---

### 9. Find Upcoming Defenses

**Name:** `query:upcoming-defenses`
**Type:** SELECT

```sparql
PREFIX thesis: <http://example.org/ontology/thesis#>
PREFIX paper: <http://example.org/ontology/paper#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?thesis ?title ?defenseDate ?location
WHERE {
  ?thesis a thesis:Thesis ;
          paper:hasTitle ?title ;
          thesis:hasDefense ?defense .
  ?defense thesis:defenseDate ?defenseDate .
  OPTIONAL { ?defense thesis:defenseLocation ?location }
  FILTER(?defenseDate >= xsd:date(NOW()))
}
ORDER BY ?defenseDate
```

---

### 10. List Committee Members

**Name:** `query:committee-members`
**Type:** SELECT
**Parameters:** `$thesisId`

```sparql
PREFIX thesis: <http://example.org/ontology/thesis#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?name ?role ?affiliation
WHERE {
  <$thesisId> thesis:hasDefense ?defense .
  ?defense thesis:hasCommitteeMember ?member .
  ?member foaf:name ?name ;
          thesis:committeeRole ?role .
  OPTIONAL { ?member foaf:organization ?affiliation }
}
```

---

### 11. Count Papers by Family

**Name:** `query:count-by-family`
**Type:** SELECT

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?family (COUNT(?paper) AS ?count)
WHERE {
  ?paper a paper:Paper ;
         paper:paperFamily ?family .
}
GROUP BY ?family
ORDER BY DESC(?count)
```

---

### 12. Find IMRAD Papers

**Name:** `query:imrad-papers`
**Type:** SELECT

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?paper ?title ?author
WHERE {
  ?paper a paper:IMRADPaper ;
         paper:hasTitle ?title ;
         paper:hasAuthor ?authorNode .
  ?authorNode foaf:name ?author .
}
```

---

### 13. Find DSR Papers

**Name:** `query:dsr-papers`
**Type:** SELECT

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?paper ?title ?author
WHERE {
  ?paper a paper:DSRPaper ;
         paper:hasTitle ?title ;
         paper:hasAuthor ?authorNode .
  ?authorNode foaf:name ?author .
}
```

---

### 14. Validate Paper Has Author

**Name:** `query:validate-paper-author`
**Type:** ASK
**Parameters:** `$paperId`

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

ASK {
  <$paperId> a paper:Paper ;
             paper:hasAuthor ?author .
}
```

---

### 15. Validate Paper Has Title

**Name:** `query:validate-paper-title`
**Type:** ASK
**Parameters:** `$paperId`

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

ASK {
  <$paperId> a paper:Paper ;
             paper:hasTitle ?title .
  FILTER(STRLEN(?title) > 0)
}
```

---

### 16. Get All Authors

**Name:** `query:all-authors`
**Type:** SELECT

```sparql
PREFIX paper: <http://example.org/ontology/paper#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT DISTINCT ?author ?name ?email ?orcid
WHERE {
  ?author a paper:Author .
  ?author foaf:name ?name .
  OPTIONAL { ?author foaf:mbox ?email }
  OPTIONAL { ?author paper:hasORCID ?orcid }
}
ORDER BY ?name
```

---

### 17. Find Citations

**Name:** `query:paper-citations`
**Type:** SELECT
**Parameters:** `$paperId`

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?citedPaper ?title
WHERE {
  <$paperId> paper:cites ?citedPaper .
  ?citedPaper paper:hasTitle ?title .
}
```

---

### 18. Find Citing Papers

**Name:** `query:citing-papers`
**Type:** SELECT
**Parameters:** `$paperId`

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?citingPaper ?title
WHERE {
  ?citingPaper paper:cites <$paperId> ;
               paper:hasTitle ?title .
}
```

---

### 19. Construct Paper Summary

**Name:** `query:construct-paper-summary`
**Type:** CONSTRUCT
**Parameters:** `$paperId`

```sparql
PREFIX paper: <http://example.org/ontology/paper#>
PREFIX summary: <http://example.org/summary#>

CONSTRUCT {
  ?paper summary:title ?title ;
         summary:family ?family ;
         summary:authorCount ?authorCount ;
         summary:sectionCount ?sectionCount .
}
WHERE {
  BIND(<$paperId> AS ?paper)
  ?paper paper:hasTitle ?title .
  OPTIONAL { ?paper paper:paperFamily ?family }
  {
    SELECT (COUNT(?author) AS ?authorCount)
    WHERE { <$paperId> paper:hasAuthor ?author }
  }
  {
    SELECT (COUNT(?section) AS ?sectionCount)
    WHERE { <$paperId> paper:hasSection ?section }
  }
}
```

---

### 20. Describe Paper

**Name:** `query:describe-paper`
**Type:** DESCRIBE
**Parameters:** `$paperId`

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

DESCRIBE <$paperId>
```

---

## Result Transformation Rules

### JSON Binding Format

```json
{
  "head": {"vars": ["paper", "title", "author"]},
  "results": {
    "bindings": [
      {
        "paper": {"type": "uri", "value": "http://example.org/instances#paper-1"},
        "title": {"type": "literal", "value": "My Paper"},
        "author": {"type": "literal", "value": "Alice"}
      }
    ]
  }
}
```

### Table Format

```
paper                                    title        author
-----------------------------------------------------------------------
http://example.org/instances#paper-1     My Paper     Alice
```

### CSV Format

```csv
paper,title,author
http://example.org/instances#paper-1,My Paper,Alice
```

---

## Error Handling

### Syntax Errors

```json
{
  "error": "E_SYNTAX_ERROR",
  "message": "SPARQL parse error at line 3",
  "line": 3,
  "column": 15,
  "recovery": "Check SPARQL syntax"
}
```

### Execution Errors

```json
{
  "error": "E_EXECUTION_ERROR",
  "message": "Unknown prefix: paper",
  "recovery": "Add PREFIX declaration"
}
```

---

## Caching Strategies

| Query Type | Cache TTL | Invalidation |
|------------|-----------|--------------|
| SELECT (read) | 5 minutes | On write |
| ASK (validation) | 1 minute | On write |
| CONSTRUCT | No cache | - |
| DESCRIBE | 5 minutes | On write |

---

## Performance Tuning

### Index Recommendations

```sparql
# Create index on paper:hasTitle for faster title lookups
# Create index on paper:paperFamily for family filtering
# Create index on thesis:defenseDate for date range queries
```

### Query Optimization Tips

1. Use FILTER early in WHERE clause
2. Avoid OPTIONAL when not needed
3. Use LIMIT for large result sets
4. Use ASK instead of SELECT for existence checks
5. Prefer specific types over generic patterns
