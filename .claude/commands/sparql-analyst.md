---
description: Write and analyze SPARQL queries for UNRDF RDF data, particularly maturity assessments and package relationships
---

# SPARQL Analyst

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Write, optimize, and analyze SPARQL queries for extracting insights from UNRDF RDF data stores.

## Standard Prefixes

```sparql
PREFIX mat: <https://unrdf.org/ontology/maturity#>
PREFIX spec: <https://unrdf.org/ontology/spec#>
PREFIX unrdf: <https://unrdf.org/resource/>
PREFIX schema: <https://schema.org/>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
```

## Common Query Patterns

### 1. List All Packages with Maturity Levels

```sparql
PREFIX mat: <https://unrdf.org/ontology/maturity#>
PREFIX schema: <https://schema.org/>

SELECT ?package ?name ?level ?score
WHERE {
  ?package a mat:Package ;
           schema:name ?name .
  OPTIONAL {
    ?package mat:hasMaturityLevel ?levelNode .
    ?levelNode rdfs:label ?level .
  }
  OPTIONAL {
    ?package mat:hasAssessment ?assessment .
    ?assessment mat:maturityScore ?score .
  }
}
ORDER BY DESC(?score)
```

### 2. Find Packages by Minimum Coverage

```sparql
PREFIX mat: <https://unrdf.org/ontology/maturity#>

SELECT ?package ?coverage
WHERE {
  ?package a mat:Package ;
           mat:hasAssessment ?assessment .
  ?assessment mat:coveragePercent ?coverage .
  FILTER(?coverage >= 80)
}
ORDER BY DESC(?coverage)
```

### 3. Synergy Category Analysis

```sparql
PREFIX mat: <https://unrdf.org/ontology/maturity#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?category ?label ?capability (COUNT(?package) AS ?packageCount)
WHERE {
  ?category a mat:SynergyCategory ;
            rdfs:label ?label ;
            mat:capability ?capability .
  ?package mat:participatesInSynergy ?category .
}
GROUP BY ?category ?label ?capability
ORDER BY DESC(?packageCount)
```

### 4. Level Distribution

```sparql
PREFIX mat: <https://unrdf.org/ontology/maturity#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?level ?levelLabel (COUNT(?package) AS ?count)
WHERE {
  ?package a mat:Package ;
           mat:hasMaturityLevel ?level .
  ?level rdfs:label ?levelLabel .
}
GROUP BY ?level ?levelLabel
ORDER BY ?level
```

### 5. Package Comparison

```sparql
PREFIX mat: <https://unrdf.org/ontology/maturity#>
PREFIX schema: <https://schema.org/>

SELECT ?name ?coverage ?apiStability ?docQuality ?score
WHERE {
  VALUES ?name { "core" "streaming" "federation" }

  ?package a mat:Package ;
           schema:name ?name ;
           mat:hasAssessment ?assessment .

  ?assessment mat:coveragePercent ?coverage ;
              mat:apiStability ?apiStability ;
              mat:docQuality ?docQuality ;
              mat:maturityScore ?score .
}
ORDER BY DESC(?score)
```

### 6. Recent Assessments

```sparql
PREFIX mat: <https://unrdf.org/ontology/maturity#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?package ?assessedAt ?score
WHERE {
  ?package a mat:Package ;
           mat:hasAssessment ?assessment .
  ?assessment mat:assessedAt ?assessedAt ;
              mat:maturityScore ?score .
  FILTER(?assessedAt > "2025-01-01T00:00:00Z"^^xsd:dateTime)
}
ORDER BY DESC(?assessedAt)
```

## Query Execution

### Using Oxigraph

```javascript
import { createStore } from '@unrdf/oxigraph';
import { readFileSync } from 'fs';

const store = createStore();

// Load data
store.load(readFileSync('maturity-data.ttl', 'utf8'), {
  format: 'text/turtle',
});

// Execute query
const query = `
  PREFIX mat: <https://unrdf.org/ontology/maturity#>
  SELECT ?package ?score
  WHERE {
    ?package mat:hasAssessment ?a .
    ?a mat:maturityScore ?score .
  }
`;

const results = store.query(query);
for (const binding of results) {
  console.log(binding.get('package').value, binding.get('score').value);
}
```

### Using CLI

```bash
# Execute SPARQL via CLI
unrdf query --file query.sparql --format json

# Inline query
unrdf query --sparql "SELECT * WHERE { ?s ?p ?o } LIMIT 10"
```

## Query Optimization

### 1. Use FILTER Placement

```sparql
# ❌ Slow - filter after join
SELECT * WHERE {
  ?package a mat:Package .
  ?package mat:hasAssessment ?a .
  ?a mat:maturityScore ?score .
  FILTER(?score > 80)
}

# ✅ Fast - filter early
SELECT * WHERE {
  ?package a mat:Package ;
           mat:hasAssessment ?a .
  ?a mat:maturityScore ?score .
  FILTER(?score > 80)
}
```

### 2. Use VALUES for Lists

```sparql
# ✅ Efficient with VALUES
SELECT * WHERE {
  VALUES ?pkg { unrdf:core unrdf:streaming unrdf:federation }
  ?pkg mat:hasMaturityLevel ?level .
}
```

### 3. Avoid SELECT \*

```sparql
# ❌ Inefficient
SELECT * WHERE { ?s ?p ?o }

# ✅ Efficient - select only needed
SELECT ?package ?score WHERE {
  ?package mat:hasAssessment/mat:maturityScore ?score .
}
```

## Output Format

````markdown
## SPARQL Analysis Report

**Query Purpose**: [description]
**Execution Time**: [ms]
**Results**: [count]

### Query

```sparql
[formatted query]
```
````

### Results

| Variable | Type    | Sample Values               |
| -------- | ------- | --------------------------- |
| ?package | IRI     | unrdf:core, unrdf:streaming |
| ?score   | Literal | 87.5, 72.3                  |

### Result Data

[table or JSON output]

### Optimization Notes

- [suggestions for performance]

### Integration

```javascript
// Code to execute this query
```

```

## Common Errors

### 1. Missing Prefix
```

Error: Unknown prefix 'mat'
Fix: Add PREFIX mat: <https://unrdf.org/ontology/maturity#>

```

### 2. Type Mismatch
```

Error: Cannot compare IRI to literal
Fix: Use STR(?var) or proper datatype casting

```

### 3. Unbound Variable
```

Error: Variable ?x is not bound
Fix: Ensure variable appears in WHERE clause

```

End Command ---
```
