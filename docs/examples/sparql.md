# SPARQL Examples

This guide provides comprehensive examples of SPARQL queries using UNRDF, covering various query patterns and use cases.

## Table of Contents

- [Basic SELECT Queries](#basic-select-queries)
- [CONSTRUCT Queries](#construct-queries)
- [ASK Queries](#ask-queries)
- [DESCRIBE Queries](#describe-queries)
- [Advanced Patterns](#advanced-patterns)
- [Performance Optimization](#performance-optimization)

## Basic SELECT Queries

### Simple Property Queries

```javascript
import { useStore, useGraph, useTurtle } from 'unrdf';

const store = useStoreContext();
const graph = useGraph(store);
const turtle = useTurtle();

// Load sample data
const data = `
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix ex: <http://example.org/> .
  
  ex:john a foaf:Person ;
    foaf:name "John Doe" ;
    foaf:age 30 ;
    foaf:email "john@example.com" .
  
  ex:jane a foaf:Person ;
    foaf:name "Jane Smith" ;
    foaf:age 28 ;
    foaf:email "jane@example.com" .
`;

const quads = await turtle.parse(data);
await graph.addQuads(quads);

// Query for all names
const nameQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>
  
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`;

const nameResults = await graph.query(nameQuery);
for await (const binding of nameResults) {
  console.log(binding.get('name').value);
}
```

### Multi-Variable Queries

```javascript
// Query for multiple properties
const multiQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?person ?name ?age ?email WHERE {
    ?person a foaf:Person ;
      foaf:name ?name ;
      foaf:age ?age ;
      foaf:email ?email .
  }
`;

const multiResults = await graph.query(multiQuery);
for await (const binding of multiResults) {
  console.log(`${binding.get('name').value} (${binding.get('age').value}) - ${binding.get('email').value}`);
}
```

### Filtered Queries

```javascript
// Query for people over 25
const filteredQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?name ?age WHERE {
    ?person a foaf:Person ;
      foaf:name ?name ;
      foaf:age ?age .
    FILTER(?age > 25)
  }
`;

const filteredResults = await graph.query(filteredQuery);
for await (const binding of filteredResults) {
  console.log(`${binding.get('name').value} is ${binding.get('age').value} years old`);
}
```

### Pattern Matching

```javascript
// Query for people with names containing "John"
const patternQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?name WHERE {
    ?person a foaf:Person ;
      foaf:name ?name .
    FILTER(CONTAINS(?name, "John"))
  }
`;

const patternResults = await graph.query(patternQuery);
for await (const binding of patternResults) {
  console.log(`Found: ${binding.get('name').value}`);
}
```

## CONSTRUCT Queries

### Creating New Graphs

```javascript
// Construct a new graph with simplified relationships
const constructQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  CONSTRUCT {
    ?person ex:hasName ?name .
    ?person ex:hasAge ?age .
  } WHERE {
    ?person a foaf:Person ;
      foaf:name ?name ;
      foaf:age ?age .
  }
`;

const constructResults = await graph.query(constructQuery);
const newQuads = [];

for await (const quad of constructResults) {
  newQuads.push(quad);
}

console.log(`Constructed ${newQuads.length} new triples`);

// Serialize the new graph
const newGraphTurtle = await turtle.serialize(newQuads);
console.log('New graph:');
console.log(newGraphTurtle);
```

### Transforming Data

```javascript
// Transform person data to employee data
const transformQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  CONSTRUCT {
    ?person a ex:Employee ;
      ex:employeeName ?name ;
      ex:employeeAge ?age .
  } WHERE {
    ?person a foaf:Person ;
      foaf:name ?name ;
      foaf:age ?age .
  }
`;

const transformResults = await graph.query(transformQuery);
const employeeQuads = [];

for await (const quad of transformResults) {
  employeeQuads.push(quad);
}

console.log(`Transformed ${employeeQuads.length} person triples to employee triples`);
```

## ASK Queries

### Boolean Checks

```javascript
// Check if any people exist
const askQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  
  ASK {
    ?person a foaf:Person .
  }
`;

const hasPeople = await graph.query(askQuery);
console.log(`People exist: ${hasPeople}`);

// Check if specific person exists
const specificAskQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  ASK {
    ex:john a foaf:Person .
  }
`;

const hasJohn = await graph.query(specificAskQuery);
console.log(`John exists: ${hasJohn}`);
```

### Conditional Logic

```javascript
// Check if all people have names
const allHaveNamesQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  
  ASK {
    ?person a foaf:Person .
    FILTER NOT EXISTS {
      ?person foaf:name ?name .
    }
  }
`;

const allHaveNames = await graph.query(allHaveNamesQuery);
console.log(`All people have names: ${!allHaveNames}`);
```

## DESCRIBE Queries

### Resource Descriptions

```javascript
// Describe a specific person
const describeQuery = `
  PREFIX ex: <http://example.org/
  
  DESCRIBE ex:john
`;

const describeResults = await graph.query(describeQuery);
const descriptionQuads = [];

for await (const quad of describeResults) {
  descriptionQuads.push(quad);
}

console.log(`Description of ex:john contains ${descriptionQuads.length} triples`);

// Serialize the description
const descriptionTurtle = await turtle.serialize(descriptionQuads);
console.log('Description:');
console.log(descriptionTurtle);
```

### Pattern-Based Descriptions

```javascript
// Describe all people
const describeAllQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  
  DESCRIBE ?person WHERE {
    ?person a foaf:Person .
  }
`;

const describeAllResults = await graph.query(describeAllQuery);
const allDescriptions = [];

for await (const quad of describeAllResults) {
  allDescriptions.push(quad);
}

console.log(`All person descriptions contain ${allDescriptions.length} triples`);
```

## Advanced Patterns

### Optional Properties

```javascript
// Query with optional properties
const optionalQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?name ?age ?email WHERE {
    ?person a foaf:Person ;
      foaf:name ?name .
    OPTIONAL { ?person foaf:age ?age }
    OPTIONAL { ?person foaf:email ?email }
  }
`;

const optionalResults = await graph.query(optionalQuery);
for await (const binding of optionalResults) {
  const name = binding.get('name').value;
  const age = binding.get('age')?.value || 'unknown';
  const email = binding.get('email')?.value || 'no email';
  
  console.log(`${name} (age: ${age}, email: ${email})`);
}
```

### Union Queries

```javascript
// Query for people or organizations
const unionQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?entity ?name WHERE {
    {
      ?entity a foaf:Person ;
        foaf:name ?name .
    } UNION {
      ?entity a ex:Organization ;
        ex:name ?name .
    }
  }
`;

const unionResults = await graph.query(unionQuery);
for await (const binding of unionResults) {
  console.log(`${binding.get('entity').value} is named ${binding.get('name').value}`);
}
```

### Subqueries

```javascript
// Find people who are older than the average age
const subquery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?name ?age WHERE {
    ?person a foaf:Person ;
      foaf:name ?name ;
      foaf:age ?age .
    {
      SELECT (AVG(?avgAge) as ?averageAge) WHERE {
        ?p a foaf:Person ;
          foaf:age ?avgAge .
      }
    }
    FILTER(?age > ?averageAge)
  }
`;

const subqueryResults = await graph.query(subquery);
for await (const binding of subqueryResults) {
  console.log(`${binding.get('name').value} (${binding.get('age').value}) is older than average`);
}
```

### Property Paths

```javascript
// Query using property paths
const pathQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?person ?friend WHERE {
    ?person foaf:knows+ ?friend .
  }
`;

const pathResults = await graph.query(pathQuery);
for await (const binding of pathResults) {
  console.log(`${binding.get('person').value} knows ${binding.get('friend').value}`);
}
```

### Aggregation

```javascript
// Count people by age
const aggregationQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?age (COUNT(?person) as ?count) WHERE {
    ?person a foaf:Person ;
      foaf:age ?age .
  }
  GROUP BY ?age
  ORDER BY ?age
`;

const aggregationResults = await graph.query(aggregationQuery);
for await (const binding of aggregationResults) {
  console.log(`Age ${binding.get('age').value}: ${binding.get('count').value} people`);
}
```

### Window Functions

```javascript
// Rank people by age
const windowQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?name ?age ?rank WHERE {
    ?person a foaf:Person ;
      foaf:name ?name ;
      foaf:age ?age .
  }
  ORDER BY ?age
`;

const windowResults = await graph.query(windowQuery);
let rank = 1;
for await (const binding of windowResults) {
  console.log(`Rank ${rank}: ${binding.get('name').value} (age: ${binding.get('age').value})`);
  rank++;
}
```

## Performance Optimization

### Efficient Patterns

```javascript
// Use specific patterns instead of wildcards
const efficientQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?name WHERE {
    ?person a foaf:Person ;
      foaf:name ?name .
  }
`;

// Avoid inefficient patterns
const inefficientQuery = `
  SELECT ?s ?p ?o WHERE {
    ?s ?p ?o .
    ?s a foaf:Person .
  }
`;
```

### Limit Results

```javascript
// Limit results for large datasets
const limitedQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?name WHERE {
    ?person a foaf:Person ;
      foaf:name ?name .
  }
  LIMIT 10
`;

const limitedResults = await graph.query(limitedQuery);
for await (const binding of limitedResults) {
  console.log(binding.get('name').value);
}
```

### Use Indexes

```javascript
// Query patterns that can use indexes
const indexedQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?name WHERE {
    ex:john foaf:name ?name .
  }
`;

// This query can use an index on the subject
const indexedResults = await graph.query(indexedQuery);
```

## Complex Examples

### Social Network Analysis

```javascript
// Find mutual connections
const mutualQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?person1 ?person2 ?mutual WHERE {
    ?person1 foaf:knows ?mutual .
    ?person2 foaf:knows ?mutual .
    FILTER(?person1 != ?person2)
  }
`;

const mutualResults = await graph.query(mutualQuery);
for await (const binding of mutualResults) {
  console.log(`${binding.get('person1').value} and ${binding.get('person2').value} both know ${binding.get('mutual').value}`);
}
```

### Organizational Hierarchy

```javascript
// Find all employees of a company
const hierarchyQuery = `
  PREFIX ex: <http://example.org/
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  
  SELECT ?employee ?name WHERE {
    ?company a ex:Company ;
      ex:name "Tech Corp" .
    ?employee ex:worksAt ?company ;
      foaf:name ?name .
  }
`;

const hierarchyResults = await graph.query(hierarchyQuery);
for await (const binding of hierarchyResults) {
  console.log(`Employee: ${binding.get('name').value}`);
}
```

### Temporal Queries

```javascript
// Query for recent events
const temporalQuery = `
  PREFIX ex: <http://example.org/
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  
  SELECT ?event ?date WHERE {
    ?event a ex:Event ;
      ex:date ?date .
    FILTER(?date > "2023-01-01"^^xsd:date)
  }
  ORDER BY ?date
`;

const temporalResults = await graph.query(temporalQuery);
for await (const binding of temporalResults) {
  console.log(`Event on ${binding.get('date').value}`);
}
```

## Best Practices

### 1. Use Meaningful Variable Names

```javascript
// Good
const query = `
  SELECT ?personName ?personAge WHERE {
    ?person foaf:name ?personName ;
      foaf:age ?personAge .
  }
`;

// Avoid
const badQuery = `
  SELECT ?a ?b WHERE {
    ?c foaf:name ?a ;
      foaf:age ?b .
  }
`;
```

### 2. Use Appropriate Filters

```javascript
// Good - specific filters
const query = `
  SELECT ?name WHERE {
    ?person foaf:name ?name ;
      foaf:age ?age .
    FILTER(?age > 18 && ?age < 65)
  }
`;

// Avoid - overly broad filters
const badQuery = `
  SELECT ?name WHERE {
    ?person foaf:name ?name .
    FILTER(?name != "")
  }
`;
```

### 3. Optimize Query Structure

```javascript
// Good - efficient structure
const query = `
  SELECT ?name WHERE {
    ?person a foaf:Person ;
      foaf:name ?name .
  }
`;

// Avoid - inefficient structure
const badQuery = `
  SELECT ?name WHERE {
    ?person ?p ?o .
    ?person a foaf:Person .
    ?person foaf:name ?name .
  }
`;
```

### 4. Handle Large Result Sets

```javascript
// Use pagination for large results
const paginatedQuery = `
  SELECT ?name WHERE {
    ?person a foaf:Person ;
      foaf:name ?name .
  }
  ORDER BY ?name
  LIMIT 100
  OFFSET 0
`;

// Process results in batches
const processBatch = async (offset) => {
  const query = `
    SELECT ?name WHERE {
      ?person a foaf:Person ;
        foaf:name ?name .
    }
    ORDER BY ?name
    LIMIT 100
    OFFSET ${offset}
  `;
  
  const results = await graph.query(query);
  const batch = [];
  
  for await (const binding of results) {
    batch.push(binding.get('name').value);
  }
  
  return batch;
};
```

## Next Steps

- Explore [Validation Examples](./validation.md) for data quality
- Learn about [Reasoning Examples](./reasoning.md) for inference
- Check out [CLI Examples](../cli/examples.md) for command-line usage
- Try the [playground examples](../../playground/) for interactive learning
