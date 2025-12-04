# SPARQL Rules Example

This example demonstrates advanced reasoning using SPARQL CONSTRUCT queries for complex rule patterns.

## Features

- **Friend-of-Friend Inference**: Transitive relationship discovery
- **Shared Interest Detection**: Connect people with common interests
- **Pattern Matching**: SPARQL-based rule definitions
- **Conflict Resolution**: Handle multiple derivation paths
- **Rule Statistics**: Track inference execution and results

## Running the Example

```bash
# From this directory
npm start

# Run tests
npm test
```

## What It Does

1. **Defines Social Network**:
   - Friendship chain: Alice → Bob → Charlie → Diana
   - Shared interests: Alice and Bob both interested in RDF

2. **Defines SPARQL Rules**:
   - **Friend-of-Friend**: If A knows B and B knows C, infer A has friend-of-friend C
   - **Shared Interest**: If A and B share interest X, infer A has common interest with B

3. **Executes Reasoning**:
   - Infers `ex:Alice ex:friendOfFriend ex:Charlie` (via Bob)
   - Infers `ex:Bob ex:friendOfFriend ex:Diana` (via Charlie)
   - Infers `ex:Alice ex:hasCommonInterestWith ex:Bob` (RDF interest)

4. **Analyzes Conflicts**:
   - Detects multiple derivation paths
   - Reports relationship multiplicities

## Key Concepts

### SPARQL CONSTRUCT Rules

Rules are defined as SPARQL CONSTRUCT queries that pattern-match the graph and generate new triples:

```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/>

CONSTRUCT {
  ?person ex:friendOfFriend ?foaf .
}
WHERE {
  ?person foaf:knows ?friend .
  ?friend foaf:knows ?foaf .
  FILTER(?person != ?foaf)
}
```

### Pattern Matching

The WHERE clause matches patterns in the existing graph:
- Finds transitive paths through the knowledge graph
- Uses FILTER to exclude self-references
- Supports complex join patterns and conditions

### Conflict Resolution

When multiple derivation paths exist (e.g., A knows B via C and D), the engine:
- Deduplicates inferred triples automatically
- Tracks provenance if needed
- Handles diamond patterns gracefully

### Rule Chaining

Rules can trigger other rules in sequence:
1. First pass infers friend-of-friend relationships
2. Second pass uses these to infer extended network
3. Process continues until fixed point

## SPARQL Rule Patterns

### Transitive Property

```sparql
CONSTRUCT { ?a ex:ancestorOf ?c }
WHERE {
  ?a ex:parentOf ?b .
  ?b ex:parentOf ?c .
}
```

### Inverse Property

```sparql
CONSTRUCT { ?y ex:childOf ?x }
WHERE {
  ?x ex:parentOf ?y .
}
```

### Property Chain

```sparql
CONSTRUCT { ?person ex:colleague ?other }
WHERE {
  ?person ex:worksFor ?org .
  ?other ex:worksFor ?org .
  FILTER(?person != ?other)
}
```

### Conditional Inference

```sparql
CONSTRUCT { ?person ex:expert ?topic }
WHERE {
  ?person ex:published ?paper .
  ?paper ex:about ?topic .
  {
    SELECT ?person ?topic (COUNT(?paper) AS ?count)
    WHERE {
      ?person ex:published ?paper .
      ?paper ex:about ?topic .
    }
    GROUP BY ?person ?topic
    HAVING (COUNT(?paper) > 5)
  }
}
```

## Testing

The test suite validates:
- Friend-of-friend inference
- Shared interest detection
- Transitive relationship chains
- Filter application (no self-references)
- Multiple derivation path handling
- Rule execution statistics

All tests use `@vitest-environment node` for N3 Store compatibility.

## Integration with UNRDF

This example demonstrates:
- `@unrdf/knowledge-engine` for SPARQL-based reasoning
- `@unrdf/core` for RDF data structures
- FOAF vocabulary for social network semantics
- SPARQL CONSTRUCT for rule definitions

The SPARQL rules integrate with:
- Query federation for distributed reasoning
- Streaming for incremental inference
- SHACL for validation after inference

## Performance Considerations

- **Rule Complexity**: More complex patterns increase execution time
- **Graph Size**: Larger graphs require more memory and CPU
- **Rule Chaining**: Multiple passes for transitive closure
- **Deduplication**: Triple store handles duplicate elimination

For production use, consider:
- Materialized views for frequently-used inferences
- Incremental reasoning for updates
- Rule optimization and reordering
- Parallel execution of independent rules
