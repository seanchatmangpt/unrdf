# SPARQL Concepts: @unrdf/core

Understanding SPARQL fundamentals.

---

## What is SPARQL?

SPARQL = Simple Protocol and RDF Query Language

It's a query language for RDF, like SQL for databases.

```sparql
SELECT ?name WHERE {
  ?person foaf:name ?name .
}
```

Translates to: "Find all values for ?name where someone's foaf:name property has that value"

---

## Basic Pattern Matching

### The Triple Pattern

```sparql
?subject ?predicate ?object .
```

Matches RDF quads where:
- `?subject` = anything (variable, must be S or O, not P)
- `?predicate` = anything (usually concrete)
- `?object` = anything (could be any term)

### Variables

Start with `?`:
```sparql
?person   # Variable
?x        # Variable
```

Never bound, only in queries.

### Named Nodes (IRIs)

In angle brackets:
```sparql
<http://example.com/alice>
```

Or with prefix:
```sparql
foaf:name   # Expands to full IRI
```

### Literals

Quoted strings:
```sparql
"Alice"           # String
"30"              # String (no type)
"30"^^xsd:integer # Typed literal
```

---

## Graph Patterns

### Basic AND (comma-separated)

```sparql
WHERE {
  ?person foaf:name ?name .
  ?person foaf:age ?age .
}
```

Means: Find person where BOTH patterns match.

### Optional (OPTIONAL)

```sparql
WHERE {
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:age ?age . }
}
```

Means: Get name (required), age if available.

### Alternative (UNION)

```sparql
WHERE {
  { ?person foaf:email ?email . }
  UNION
  { ?person foaf:phone ?phone . }
}
```

Means: Get email OR phone (or both).

---

## Solution Modifiers

### SELECT Variables

```sparql
SELECT ?name        # One variable
SELECT ?name ?age   # Multiple variables
SELECT *            # All variables
SELECT DISTINCT ?name  # Remove duplicates
```

### FILTER Conditions

```sparql
FILTER (?age > 30)                    # Comparison
FILTER (CONTAINS(?name, "John"))      # String function
FILTER (?age > 30 && ?age < 65)       # Boolean logic
```

### ORDER BY Sorting

```sparql
ORDER BY ?name              # Ascending
ORDER BY DESC(?age)         # Descending
ORDER BY ?age ?name         # Multiple keys
```

### LIMIT and OFFSET

```sparql
LIMIT 10              # Return first 10
OFFSET 20 LIMIT 10    # Return 10 starting from position 20 (pagination)
```

---

## Aggregate Functions

### COUNT

```sparql
SELECT (COUNT(?person) AS ?total)
WHERE { ?person foaf:name ?name . }
```

Returns number of matches.

### AVG, SUM, MIN, MAX

```sparql
SELECT (AVG(?age) AS ?average)
       (MAX(?age) AS ?oldest)
WHERE { ?person foaf:age ?age . }
```

### GROUP_CONCAT

```sparql
SELECT ?company
       (GROUP_CONCAT(?name; separator=", ") AS ?employees)
WHERE { ?person workAt ?company ; foaf:name ?name . }
GROUP BY ?company
```

Collect all values into one string.

---

## GROUP BY

Groups results and applies aggregates:

```sparql
SELECT ?company (COUNT(?person) AS ?count)
WHERE { ?person workAt ?company . }
GROUP BY ?company
```

Groups by company, counts people in each.

---

## Query Types

### SELECT

Returns variable bindings:
```sparql
SELECT ?name WHERE { ?x foaf:name ?name }
# Result: [{ name: "Alice" }, { name: "Bob" }]
```

### ASK

Returns true/false:
```sparql
ASK { <http://example.com/alice> foaf:name "Alice" }
# Result: true
```

### CONSTRUCT

Returns RDF quads:
```sparql
CONSTRUCT { ?x foaf:knows ?y }
WHERE { ?x foaf:knows ?y }
# Result: New quads created from pattern
```

### DESCRIBE

Returns all quads about resource:
```sparql
DESCRIBE <http://example.com/alice>
# Result: All quads with alice as subject or object
```

---

## Value Binding

After a query matches, variables get values:

```sparql
SELECT ?name WHERE {
  ?person foaf:name ?name .
}
```

If store has:
```
<http://example.com/alice> foaf:name "Alice" .
```

Result binding:
```javascript
{
  name: Literal("Alice")
}
```

---

## Set Operations

### UNION

Combine multiple patterns:
```sparql
WHERE {
  { ?x rdf:type foaf:Person . }
  UNION
  { ?x rdf:type foaf:Organization . }
}
```

### MINUS

Remove matches:
```sparql
WHERE {
  ?person foaf:name ?name .
  MINUS { ?person foaf:age ?age . }
}
```

(Get all people WITHOUT an age)

---

## Functions

### String Functions

```sparql
CONTAINS(?str, "substring")    # Boolean
STARTS WITH(?str, "prefix")    # Boolean
STRLEN(?str)                   # Length
CONCAT(?s1, ?s2)               # Combine
SUBSTR(?str, 1, 5)             # Substring
```

### Numeric Functions

```sparql
?age > 30                       # Comparison
?age + 5                        # Arithmetic
ABS(?number)                    # Absolute value
ROUND(?decimal)                 # Round
```

### Type Functions

```sparql
DATATYPE(?literal)              # Get type
LANG(?literal)                  # Get language
STR(?term)                      # Convert to string
```

---

## Blank Nodes in Queries

```sparql
SELECT ?name WHERE {
  ?person foaf:knows _:friend .
  _:friend foaf:name ?name .
}
```

`_:friend` = blank node (specific instance, not variable)

Rarely used in queries, more common in CONSTRUCT.

---

## Common Patterns

### Find all instances of a type

```sparql
?x rdf:type foaf:Person .
```

### Find properties of resource

```sparql
<http://example.com/alice> ?p ?o .
```

### Find all relationships

```sparql
?s ?p ?o .
```

### Find people and their friends

```sparql
?person foaf:knows ?friend .
?friend foaf:name ?friendName .
```

### Count by group

```sparql
SELECT ?type (COUNT(?x) AS ?count)
WHERE { ?x rdf:type ?type . }
GROUP BY ?type
```

---

## Key Semantics

**Variables match anything:** `?x` matches any term

**Predicates usually concrete:** `foaf:name` (not `?p`) in most queries

**No variable in predicates:** Can't do `?x ?p ?y` usually (unless needed)

**Multiple results:** One binding per solution (could have 0, 1, or many)

**Duplicates returned:** Unless DISTINCT used

---

## Learning Path

1. **Basic patterns:** `?x ?p ?o` with concrete predicates
2. **Filters:** Add FILTER to constrain results
3. **Joins:** Multiple patterns connected by variables
4. **Aggregates:** COUNT, GROUP BY
5. **Advanced:** UNION, MINUS, OPTIONAL, nested queries

---

## Next Reading

- **query-execution** (Explanation) - How queries actually execute
- **architecture** (Explanation) - System design
- **optimize-sparql-queries** (How-To) - Make queries faster
- **troubleshooting** (How-To) - Debug queries
