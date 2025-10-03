# SPARQL Queries

Knowd supports a comprehensive subset of SPARQL 1.1, including SELECT, ASK, CONSTRUCT queries with advanced features like aggregation, subqueries, and property paths.

## Query Types

### SELECT Queries

**Basic SELECT:**
```sparql
SELECT ?name ?age
WHERE {
  ?person foaf:name ?name .
  ?person foaf:age ?age .
}
```

**SELECT with LIMIT and OFFSET:**
```sparql
SELECT ?person ?name
WHERE {
  ?person foaf:name ?name .
}
LIMIT 10
OFFSET 20
```

**SELECT with ORDER BY:**
```sparql
SELECT ?person ?age
WHERE {
  ?person foaf:age ?age .
}
ORDER BY DESC(?age)
```

**SELECT with GROUP BY and HAVING:**
```sparql
SELECT ?department (COUNT(?person) AS ?count)
WHERE {
  ?person ex:department ?department .
}
GROUP BY ?department
HAVING (COUNT(?person) > 5)
```

### ASK Queries

**Basic ASK:**
```sparql
ASK WHERE {
  <http://example.org/alice> foaf:knows <http://example.org/bob> .
}
```

**ASK with complex patterns:**
```sparql
ASK WHERE {
  ?person a foaf:Person .
  ?person foaf:name ?name .
  FILTER(?name = "Alice")
}
```

### CONSTRUCT Queries

**Basic CONSTRUCT:**
```sparql
CONSTRUCT {
  ?person a ex:ImportantPerson .
}
WHERE {
  ?person ex:riskLevel "high" .
}
```

**CONSTRUCT with property paths:**
```sparql
CONSTRUCT {
  ?person ex:knowsIndirectly ?other .
}
WHERE {
  ?person ex:knows+ ?other .
}
```

## Advanced SPARQL Features

### Property Paths

**One or more (`+`):**
```sparql
# Find people Alice knows directly or indirectly
?person ex:knows+ <http://example.org/alice> .
```

**Zero or more (`*`):**
```sparql
# Find all people in Alice's social network (including Alice)
?person ex:knows* <http://example.org/alice> .
```

**Zero or one (`?`):**
```sparql
# Find people who may or may not have a middle name
?person foaf:firstName ?first .
?person foaf:middleName? ?middle .
?person foaf:lastName ?last .
```

**Alternatives (`|`):**
```sparql
# Find people connected by either "knows" or "worksWith"
?person (ex:knows | ex:worksWith) ?other .
```

**Negation (`!`):**
```sparql
# Find people not connected through any path
?person !(ex:knows | ex:worksWith)+ ?other .
```

### Subqueries

**Subquery in WHERE clause:**
```sparql
SELECT ?person ?name
WHERE {
  ?person foaf:name ?name .
  FILTER(?person NOT IN (
    SELECT ?blocked
    WHERE {
      ?blocked a ex:BlockedUser .
    }
  ))
}
```

**Subquery in SELECT:**
```sparql
SELECT ?person (COUNT(?friend) AS ?friendCount)
WHERE {
  ?person foaf:name ?name .
  {
    SELECT ?person (COUNT(?other) AS ?friend)
    WHERE {
      ?person ex:knows ?other .
    }
    GROUP BY ?person
  }
}
```

### Aggregation Functions

**COUNT:**
```sparql
SELECT (COUNT(?person) AS ?totalPersons)
WHERE {
  ?person a foaf:Person .
}
```

**SUM, AVG, MIN, MAX:**
```sparql
SELECT (SUM(?age) AS ?totalAge) (AVG(?age) AS ?averageAge)
WHERE {
  ?person foaf:age ?age .
}
```

**GROUP_CONCAT:**
```sparql
SELECT ?person (GROUP_CONCAT(?skill; SEPARATOR=", ") AS ?skills)
WHERE {
  ?person ex:hasSkill ?skill .
}
GROUP BY ?person
```

### UNION, OPTIONAL, and MINUS

**UNION:**
```sparql
SELECT ?person ?name
WHERE {
  { ?person foaf:name ?name . ?person ex:type "employee" }
  UNION
  { ?person foaf:name ?name . ?person ex:type "contractor" }
}
```

**OPTIONAL:**
```sparql
SELECT ?person ?name ?email
WHERE {
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:mbox ?email }
}
```

**MINUS:**
```sparql
SELECT ?person
WHERE {
  ?person ex:worksFor ?company .
  MINUS {
    ?company ex:industry "tech"
  }
}
```

### BIND and VALUES

**BIND:**
```sparql
SELECT ?person ?fullName
WHERE {
  ?person foaf:firstName ?first .
  ?person foaf:lastName ?last .
  BIND(CONCAT(?first, " ", ?last) AS ?fullName)
}
```

**VALUES:**
```sparql
SELECT ?person ?status
WHERE {
  ?person ex:department ?dept .
  VALUES ?dept {
    "engineering"
    "sales"
    "marketing"
  }
}
```

### FILTER Expressions

**String operations:**
```sparql
FILTER(STRSTARTS(?name, "A"))
FILTER(REGEX(?description, "important", "i"))
FILTER(STRENDS(?email, "@company.com"))
```

**Numeric operations:**
```sparql
FILTER(?age > 25)
FILTER(?salary >= 50000 && ?salary <= 100000)
FILTER(?score IN (90, 95, 100))
```

**Date operations:**
```sparql
FILTER(?created > "2023-01-01"^^xsd:date)
FILTER(NOW() - ?created < "P30D"^^xsd:duration)
```

## Query Optimization

### Cost-Based Optimization (CBO)

Knowd uses cost-based query optimization when statistics are available:

```bash
# Enable CBO (default: true)
KNOWD_PLANNER_CBO=true ./knowd

# Analyze data to generate statistics
curl -X POST http://localhost:8090/v1/admin/analyze \
  -H "Content-Type: application/json" \
  -d '{"namespace": "production"}'
```

### Index Selection

The query planner automatically selects the best indexes based on:
- Cardinality estimates
- Selectivity of predicates
- Join order optimization

### Plan Caching

Query plans are cached to improve performance for repeated queries:

```bash
# Configure plan cache size
KNOWD_PLAN_CACHE_SIZE=512 ./knowd

# Enable plan cache persistence
KNOWD_PLAN_CACHE_PERSIST=true ./knowd
```

## Best Practices

### 1. Use Specific Patterns

**Good:**
```sparql
# Specific subject-predicate-object pattern
<http://example.org/alice> foaf:knows ?person .
```

**Better:**
```sparql
# Use variables for better flexibility
?person foaf:knows <http://example.org/bob> .
```

### 2. Minimize FILTER Usage

**Avoid:**
```sparql
# Inefficient - filters after pattern matching
?s ?p ?o .
FILTER(?p = foaf:name)
```

**Prefer:**
```sparql
# More efficient - specific predicate
?s foaf:name ?o .
```

### 3. Use LIMIT for Large Results

```sparql
# Good for pagination
SELECT ?person ?name
WHERE {
  ?person foaf:name ?name .
}
ORDER BY ?name
LIMIT 100
OFFSET 200
```

### 4. Optimize JOINs

```sparql
# Explicit JOIN order can help
SELECT ?person ?dept ?project
WHERE {
  ?person ex:worksIn ?dept .
  ?dept ex:hasProject ?project .
}
```

### 5. Use Property Paths Judiciously

**Simple cases:**
```sparql
# Use + for one-or-more (efficient)
?person ex:knows+ ?friend .
```

**Complex cases:**
```sparql
# Use specific patterns when possible
?person ex:knows ?direct .
?direct ex:knows ?indirect .
```

## Performance Tuning

### Query Statistics

```bash
# Get query performance stats
curl http://localhost:8090/v1/store/stats

# Analyze query patterns
curl -X POST http://localhost:8090/v1/admin/analyze \
  -d '{"namespace": "production", "includeHistograms": true}'
```

### Common Performance Issues

**Slow queries:**
- Enable CBO: `KNOWD_PLANNER_CBO=true`
- Increase plan cache: `KNOWD_PLAN_CACHE_SIZE=1024`
- Analyze data: Use `/v1/admin/analyze` endpoint

**High memory usage:**
- Use disk storage: `KNOWD_STORE=disk`
- Reduce cache sizes
- Enable compaction: `KNOWD_COMPACT_INTERVAL_SEC=300`

**Network bottlenecks:**
- Use streaming for large results
- Compress responses when possible
- Tune replication settings for clusters

## Error Handling

### Common Query Errors

**Syntax errors:**
```json
{
  "error": "QuerySyntaxError",
  "message": "Invalid SPARQL syntax near 'WHERE'",
  "details": {
    "position": 45,
    "expected": "closing brace"
  }
}
```

**Type errors:**
```json
{
  "error": "TypeError",
  "message": "Cannot compare string and number",
  "details": {
    "expression": "?age > \"25\"",
    "suggestion": "Use numeric comparison: ?age > 25"
  }
}
```

**Runtime errors:**
```json
{
  "error": "QueryExecutionError",
  "message": "Index not available for predicate",
  "details": {
    "predicate": "<http://example.org/unknownPredicate>",
    "suggestion": "Ensure data exists or create appropriate indexes"
  }
}
```

## Examples

### Complete Application Queries

**Find people with specific skills:**
```sparql
SELECT ?person ?skill ?level
WHERE {
  ?person ex:hasSkill ?skill .
  ?person ex:skillLevel ?skill ?level .
  FILTER(?level >= 8)
}
ORDER BY DESC(?level)
```

**Find organizational hierarchy:**
```sparql
SELECT ?employee ?manager ?title
WHERE {
  ?employee ex:reportsTo+ ?manager .
  ?employee ex:title ?title .
}
ORDER BY ?manager
```

**Count relationships:**
```sparql
SELECT ?person (COUNT(?connection) AS ?connections)
WHERE {
  ?person (ex:knows | ex:worksWith) ?connection .
}
GROUP BY ?person
HAVING (COUNT(?connection) > 5)
ORDER BY DESC(COUNT(?connection))
```

### Time-Travel Queries

**Query data at a specific point in time:**
```sparql
SELECT ?person ?name
WHERE {
  ?person foaf:name ?name .
}
AT "2023-12-01T10:00:00Z"
```

**Query changes between time points:**
```sparql
SELECT ?person ?name
WHERE {
  ?person foaf:name ?name .
}
BETWEEN "2023-12-01T00:00:00Z" AND "2023-12-31T23:59:59Z"
```

## Integration with Other Features

### With SHACL Validation

```sparql
# Query only validated data
SELECT ?person ?name
WHERE {
  ?person a ex:ValidatedPerson .
  ?person foaf:name ?name .
}
```

### With Hooks

```sparql
# Query data that triggers notifications
SELECT ?person ?alertLevel
WHERE {
  ?person ex:riskScore ?score .
  FILTER(?score > 0.8)
}
# This query could trigger a hook for high-risk alerts
```

### With Vector Search

```sparql
# Combine structured queries with semantic search
SELECT ?person ?name ?similarity
WHERE {
  ?person foaf:name ?name .
  # This could be combined with vector similarity results
}
```

## Troubleshooting

### Query Debugging

**Enable detailed logging:**
```bash
KNOWD_OTEL_EXPORTER=stdout ./knowd
```

**Check query plans:**
```bash
# View execution plan (when implemented)
curl -X POST http://localhost:8090/v1/query \
  -H "X-Debug-Plan: true" \
  -d '{"query": "SELECT ...", "kind": "sparql-select"}'
```

### Performance Analysis

**Monitor slow queries:**
```bash
# Get query statistics
curl http://localhost:8090/v1/store/stats

# Analyze query patterns
curl -X POST http://localhost:8090/v1/admin/analyze \
  -d '{"namespace": "production"}'
```

### Common Issues

**"Query timeout":**
- Reduce result set size with LIMIT
- Use more specific patterns
- Check if indexes are being used effectively

**"Out of memory":**
- Use disk storage for large datasets
- Reduce plan cache size
- Enable streaming for large results

**"Invalid syntax":**
- Check SPARQL syntax using online validators
- Use simpler patterns first
- Check for missing prefixes

For more help, see the [Troubleshooting Guide](../troubleshooting/debugging.md).
