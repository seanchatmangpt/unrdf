# Basic Usage Examples

This guide provides practical examples of using UNRDF for common RDF operations.

## Table of Contents

- [Creating a Knowledge Graph](#creating-a-knowledge-graph)
- [Querying Data](#querying-data)
- [Working with Different Formats](#working-with-different-formats)
- [Error Handling](#error-handling)
- [Performance Tips](#performance-tips)

## Creating a Knowledge Graph

### Simple Person Data

```javascript
import { useStore, useGraph, useTurtle } from 'unrdf';

// Initialize components
const store = useStoreContext();
const graph = useGraph(store);
const turtle = useTurtle();

// Define person data in Turtle
const personData = `
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

// Parse and add to graph
const quads = await turtle.parse(personData);
await graph.addQuads(quads);

console.log(`Added ${quads.length} triples to the knowledge graph`);
```

### Adding Relationships

```javascript
// Add relationships between people
const relationshipData = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  ex:john foaf:knows ex:jane .
  ex:jane foaf:knows ex:john .
  
  ex:john ex:worksAt ex:company1 .
  ex:jane ex:worksAt ex:company2 .
  
  ex:company1 a ex:Company ;
    ex:name "Tech Corp" .
  
  ex:company2 a ex:Company ;
    ex:name "Data Inc" .
`;

const relationshipQuads = await turtle.parse(relationshipData);
await graph.addQuads(relationshipQuads);

console.log(`Added ${relationshipQuads.length} relationship triples`);
```

## Querying Data

### Basic SELECT Queries

```javascript
// Query for all people
const peopleQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>
  
  SELECT ?person ?name ?age WHERE {
    ?person a foaf:Person ;
      foaf:name ?name ;
      foaf:age ?age .
  }
`;

const peopleResults = await graph.query(peopleQuery);
console.log('People in the knowledge graph:');

for await (const binding of peopleResults) {
  console.log(`- ${binding.get('name').value} (age: ${binding.get('age').value})`);
}
```

### Filtered Queries

```javascript
// Query for people over 25
const adultsQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/
  
  SELECT ?name ?age WHERE {
    ?person a foaf:Person ;
      foaf:name ?name ;
      foaf:age ?age .
    FILTER(?age > 25)
  }
`;

const adultsResults = await graph.query(adultsQuery);
console.log('Adults (over 25):');

for await (const binding of adultsResults) {
  console.log(`- ${binding.get('name').value} (age: ${binding.get('age').value})`);
}
```

### Relationship Queries

```javascript
// Query for people who know each other
const knowsQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>
  
  SELECT ?person1 ?person2 WHERE {
    ?person1 foaf:knows ?person2 .
  }
`;

const knowsResults = await graph.query(knowsQuery);
console.log('People who know each other:');

for await (const binding of knowsResults) {
  console.log(`${binding.get('person1').value} knows ${binding.get('person2').value}`);
}
```

### CONSTRUCT Queries

```javascript
// Create a new graph with simplified relationships
const constructQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>
  
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
```

## Working with Different Formats

### Loading from Files

```javascript
import { readFile } from 'fs/promises';

// Load Turtle data from file
const turtleData = await readFile('data.ttl', 'utf-8');
const quads = await turtle.parse(turtleData);
await graph.addQuads(quads);

console.log(`Loaded ${quads.length} triples from file`);
```

### Serializing to Different Formats

```javascript
// Get all quads and serialize to Turtle
const allQuads = graph.getQuads();
const turtleOutput = await turtle.serialize(allQuads);

console.log('Turtle representation:');
console.log(turtleOutput);
```

### Working with JSON-LD

```javascript
// Convert to JSON-LD format
const jsonLdData = {
  "@context": {
    "foaf": "http://xmlns.com/foaf/0.1/",
    "ex": "http://example.org/"
  },
  "@id": "ex:john",
  "@type": "foaf:Person",
  "foaf:name": "John Doe",
  "foaf:age": 30
};

// Convert JSON-LD to RDF quads
const jsonLdQuads = await turtle.parse(JSON.stringify(jsonLdData));
await graph.addQuads(jsonLdQuads);
```

## Error Handling

### Parse Error Handling

```javascript
try {
  const invalidTurtle = `
    @prefix ex: <http://example.org/> .
    ex:john a ex:Person ;  # Missing closing bracket
  `;
  
  const quads = await turtle.parse(invalidTurtle);
} catch (error) {
  if (error.name === 'ParseError') {
    console.error('Parse error:', error.message);
    console.error('Line:', error.line);
    console.error('Column:', error.column);
  } else {
    console.error('Unexpected error:', error);
  }
}
```

### Query Error Handling

```javascript
try {
  const invalidQuery = `
    SELECT ?name WHERE {
      ?person foaf:name ?name
      # Missing closing brace
  `;
  
  const results = await graph.query(invalidQuery);
} catch (error) {
  if (error.name === 'QueryError') {
    console.error('Query error:', error.message);
  } else {
    console.error('Unexpected error:', error);
  }
}
```

### Validation Error Handling

```javascript
import { useValidator } from 'unrdf';

const validator = useValidator();

try {
  const result = await validator.validate(dataQuads, shapeQuads);
  
  if (!result.conforms) {
    console.log('Validation failed:');
    for (const violation of result.results) {
      console.log(`- ${violation.resultMessage[0].value}`);
    }
  } else {
    console.log('Validation passed');
  }
} catch (error) {
  console.error('Validation error:', error.message);
}
```

## Performance Tips

### Streaming Large Datasets

```javascript
import { createReadStream } from 'fs';
import { pipeline } from 'stream/promises';

// Stream processing for large files
const processLargeFile = async (filePath) => {
  const stream = createReadStream(filePath, { encoding: 'utf8' });
  let buffer = '';
  
  for await (const chunk of stream) {
    buffer += chunk;
    
    // Process complete lines
    const lines = buffer.split('\n');
    buffer = lines.pop(); // Keep incomplete line
    
    for (const line of lines) {
      if (line.trim()) {
        try {
          const quads = await turtle.parse(line);
          await graph.addQuads(quads);
        } catch (error) {
          console.error('Error processing line:', line, error.message);
        }
      }
    }
  }
  
  // Process remaining buffer
  if (buffer.trim()) {
    const quads = await turtle.parse(buffer);
    await graph.addQuads(quads);
  }
};
```

### Batch Processing

```javascript
// Process data in batches
const processInBatches = async (data, batchSize = 1000) => {
  for (let i = 0; i < data.length; i += batchSize) {
    const batch = data.slice(i, i + batchSize);
    
    try {
      const quads = await turtle.parse(batch.join('\n'));
      await graph.addQuads(quads);
      
      console.log(`Processed batch ${Math.floor(i / batchSize) + 1}`);
    } catch (error) {
      console.error(`Error processing batch ${Math.floor(i / batchSize) + 1}:`, error.message);
    }
  }
};
```

### Memory Management

```javascript
// Clear store when done
const processData = async (data) => {
  try {
    const quads = await turtle.parse(data);
    await graph.addQuads(quads);
    
    // Process the data
    const results = await graph.query('SELECT * WHERE { ?s ?p ?o }');
    
    // Clear when done
    store.clear();
    
    return results;
  } catch (error) {
    // Always clear on error
    store.clear();
    throw error;
  }
};
```

## Advanced Examples

### Working with Blank Nodes

```javascript
// Turtle with blank nodes
const blankNodeData = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  ex:john a foaf:Person ;
    foaf:name "John Doe" ;
    ex:address [
      ex:street "123 Main St" ;
      ex:city "San Francisco" ;
      ex:state "CA"
    ] .
`;

const blankNodeQuads = await turtle.parse(blankNodeData);
await graph.addQuads(blankNodeQuads);

// Query for addresses
const addressQuery = `
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  
  SELECT ?name ?street ?city WHERE {
    ?person foaf:name ?name ;
      ex:address ?address .
    ?address ex:street ?street ;
      ex:city ?city .
  }
`;

const addressResults = await graph.query(addressQuery);
for await (const binding of addressResults) {
  console.log(`${binding.get('name').value} lives at ${binding.get('street').value}, ${binding.get('city').value}`);
}
```

### Working with Collections

```javascript
// Turtle with collections (RDF lists)
const collectionData = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  ex:team a ex:Team ;
    ex:name "Development Team" ;
    ex:members ( ex:alice ex:bob ex:charlie ) .
`;

const collectionQuads = await turtle.parse(collectionData);
await graph.addQuads(collectionQuads);

// Query for team members
const teamQuery = `
  PREFIX ex: <http://example.org/>
  
  SELECT ?member WHERE {
    ?team ex:members ?list .
    ?list rdf:rest*/rdf:first ?member .
  }
`;

const teamResults = await graph.query(teamQuery);
for await (const binding of teamResults) {
  console.log(`Team member: ${binding.get('member').value}`);
}
```

### Working with Datatypes

```javascript
// Turtle with different datatypes
const datatypeData = `
  @prefix ex: <http://example.org/> .
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
  
  ex:product1 a ex:Product ;
    ex:name "Laptop" ;
    ex:price "999.99"^^xsd:decimal ;
    ex:weight "2.5"^^xsd:float ;
    ex:inStock "true"^^xsd:boolean ;
    ex:releaseDate "2023-01-15"^^xsd:date .
`;

const datatypeQuads = await turtle.parse(datatypeData);
await graph.addQuads(datatypeQuads);

// Query for products with price > 500
const priceQuery = `
  PREFIX ex: <http://example.org/>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  
  SELECT ?name ?price WHERE {
    ?product a ex:Product ;
      ex:name ?name ;
      ex:price ?price .
    FILTER(?price > "500"^^xsd:decimal)
  }
`;

const priceResults = await graph.query(priceQuery);
for await (const binding of priceResults) {
  console.log(`${binding.get('name').value}: $${binding.get('price').value}`);
}
```

## Best Practices

### 1. Always Handle Errors

```javascript
try {
  const quads = await turtle.parse(data);
  await graph.addQuads(quads);
} catch (error) {
  console.error('Error processing data:', error.message);
  // Handle error appropriately
}
```

### 2. Use Meaningful Variable Names

```javascript
// Good
const personQuery = `
  SELECT ?personName ?personAge WHERE {
    ?person foaf:name ?personName ;
      foaf:age ?personAge .
  }
`;

// Avoid
const query = `
  SELECT ?a ?b WHERE {
    ?c foaf:name ?a ;
      foaf:age ?b .
  }
`;
```

### 3. Validate Data Before Processing

```javascript
// Always validate RDF data
const result = await validator.validate(dataQuads, shapeQuads);
if (!result.conforms) {
  console.log('Data validation failed');
  return;
}

// Process validated data
await graph.addQuads(dataQuads);
```

### 4. Use Appropriate Data Types

```javascript
// Good - use appropriate datatypes
const data = `
  ex:person ex:age "30"^^xsd:integer ;
    ex:height "5.9"^^xsd:float ;
    ex:active "true"^^xsd:boolean .
`;

// Avoid - everything as strings
const badData = `
  ex:person ex:age "30" ;
    ex:height "5.9" ;
    ex:active "true" .
`;
```

### 5. Organize with Namespaces

```javascript
// Good - use prefixes
const data = `
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix ex: <http://example.org/> .
  
  ex:john foaf:name "John Doe" .
`;

// Avoid - full URIs everywhere
const badData = `
  <http://example.org/john> <http://xmlns.com/foaf/0.1/name> "John Doe" .
`;
```

## Next Steps

- Explore [SPARQL Examples](./sparql.md) for advanced query patterns
- Learn about [Validation Examples](./validation.md) for data quality
- Check out [Reasoning Examples](./reasoning.md) for inference
- Try the [playground examples](../../playground/) for interactive learning
