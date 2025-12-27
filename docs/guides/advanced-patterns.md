# Advanced Patterns and Best Practices

This guide covers advanced usage patterns, performance optimization, and best practices for building robust RDF applications with unrdf.

## Performance Optimization

### Store Management

#### Efficient Quad Addition
When adding many quads, batch operations are more efficient:

```javascript
import { useStore, useTerms } from 'unrdf';

const store = useStore();
const terms = useTerms();

// ✅ Good: Batch quad creation
const quads = [];
for (let i = 0; i < 1000; i++) {
  quads.push(terms.quad(
    terms.iri(`person${i}`),
    terms.iri("name"),
    terms.lit(`Person ${i}`)
  ));
}

// Add all quads at once
quads.forEach(quad => store.add(quad));

// ❌ Avoid: Adding quads one by one in a loop
for (let i = 0; i < 1000; i++) {
  const quad = terms.quad(
    terms.iri(`person${i}`),
    terms.iri("name"),
    terms.lit(`Person ${i}`)
  );
  store.add(quad); // Less efficient
}
```

#### Memory Management
For large datasets, consider using streaming or chunked processing:

```javascript
import { useNQuads } from 'unrdf';
import { createReadStream } from 'fs';

const nquads = useNQuads();

// Process large files in chunks
async function processLargeFile(filePath) {
  const stream = createReadStream(filePath, { encoding: 'utf8' });
  let buffer = '';
  
  for await (const chunk of stream) {
    buffer += chunk;
    const lines = buffer.split('\n');
    buffer = lines.pop(); // Keep incomplete line
    
    // Process complete lines
    for (const line of lines) {
      if (line.trim()) {
        const store = nquads.parse(line);
        // Process store...
      }
    }
  }
}
```

### Query Optimization

#### Efficient SPARQL Queries
Structure queries to minimize data transfer and processing:

```javascript
import { useGraph } from 'unrdf';

const graph = useGraph(store);

// ✅ Good: Specific queries with LIMIT
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name ?email WHERE {
    ?person foaf:name ?name ;
            foaf:mbox ?email .
  }
  LIMIT 100
`);

// ❌ Avoid: Unbounded queries
const allResults = await graph.select(`
  SELECT * WHERE {
    ?s ?p ?o .
  }
`);
```

#### Query Caching
Cache frequently used queries:

```javascript
import { useCache } from 'unrdf';

const cache = useCache({ ttl: 300000 }); // 5 minutes

async function getCachedQuery(query) {
  const cacheKey = `query:${hashQuery(query)}`;
  
  let results = cache.get(cacheKey);
  if (!results) {
    results = await graph.select(query);
    cache.set(cacheKey, results);
  }
  
  return results;
}

function hashQuery(query) {
  // Simple hash function for query strings
  return query.split('').reduce((a, b) => {
    a = ((a << 5) - a) + b.charCodeAt(0);
    return a & a;
  }, 0);
}
```

## Advanced Data Patterns

### Complex Graph Traversal

#### Multi-hop Traversal
Use Clownface for complex graph navigation:

```javascript
import { usePointer } from 'unrdf';

const pointer = usePointer(store);

// Find all friends of friends
function findFriendsOfFriends(personIRI) {
  const person = pointer.node(personIRI);
  
  const friends = person.out("foaf:knows").toArray();
  const friendsOfFriends = [];
  
  for (const friend of friends) {
    const friendFriends = friend.out("foaf:knows").toArray();
    friendsOfFriends.push(...friendFriends);
  }
  
  return friendsOfFriends;
}

// Recursive traversal with depth limit
function traverseWithDepth(startIRI, maxDepth = 3) {
  const visited = new Set();
  const results = [];
  
  function traverse(node, depth) {
    if (depth > maxDepth || visited.has(node.value)) {
      return;
    }
    
    visited.add(node.value);
    results.push(node);
    
    const neighbors = node.out().toArray();
    for (const neighbor of neighbors) {
      traverse(neighbor, depth + 1);
    }
  }
  
  traverse(pointer.node(startIRI), 0);
  return results;
}
```

#### Path Finding
Implement path finding algorithms:

```javascript
function findShortestPath(startIRI, endIRI) {
  const queue = [{ node: startIRI, path: [startIRI] }];
  const visited = new Set();
  
  while (queue.length > 0) {
    const { node, path } = queue.shift();
    
    if (node === endIRI) {
      return path;
    }
    
    if (visited.has(node)) {
      continue;
    }
    
    visited.add(node);
    
    const neighbors = pointer.node(node).out().toArray();
    for (const neighbor of neighbors) {
      if (!visited.has(neighbor.value)) {
        queue.push({
          node: neighbor.value,
          path: [...path, neighbor.value]
        });
      }
    }
  }
  
  return null; // No path found
}
```

### Data Transformation Patterns

#### Graph Merging
Merge multiple graphs with conflict resolution:

```javascript
import { useStore } from 'unrdf';

function mergeGraphsWithResolution(graphs, conflictResolver) {
  const merged = useStore();
  const conflicts = [];
  
  for (const graph of graphs) {
    for (const quad of graph) {
      if (merged.has(quad)) {
        // Conflict detected
        const existing = Array.from(merged.match(
          quad.subject,
          quad.predicate,
          null,
          quad.graph
        ))[0];
        
        const resolution = conflictResolver(existing, quad);
        if (resolution) {
          merged.remove(existing);
          merged.add(resolution);
        }
        conflicts.push({ existing, incoming: quad });
      } else {
        merged.add(quad);
      }
    }
  }
  
  return { merged, conflicts };
}

// Example conflict resolver
function preferNewer(existing, incoming) {
  // Simple example: prefer the newer quad
  return incoming;
}
```

#### Data Normalization
Normalize data to a standard format:

```javascript
import { useTerms } from 'unrdf';

function normalizePersonData(store) {
  const terms = useTerms();
  const normalized = useStore();
  
  // Find all person nodes
  const persons = getSubjectsByType(store, "foaf:Person");
  
  for (const person of persons) {
    // Normalize name (capitalize first letter)
    const name = getOne(store, person, "foaf:name");
    if (name && name.termType === "Literal") {
      const normalizedName = name.value.charAt(0).toUpperCase() + 
                           name.value.slice(1).toLowerCase();
      
      normalized.add(terms.quad(
        person,
        terms.iri("foaf:name"),
        terms.lit(normalizedName)
      ));
    }
    
    // Normalize age (ensure it's an integer)
    const age = getOne(store, person, "foaf:age");
    if (age && age.termType === "Literal") {
      const ageValue = parseInt(age.value);
      if (!isNaN(ageValue)) {
        normalized.add(terms.quad(
          person,
          terms.iri("foaf:age"),
          terms.lit(ageValue, "http://www.w3.org/2001/XMLSchema#integer")
        ));
      }
    }
  }
  
  return normalized;
}
```

## Error Handling and Resilience

### Comprehensive Error Handling

#### Graceful Degradation
Handle errors gracefully with fallbacks:

```javascript
async function robustQuery(query, fallbackQuery) {
  try {
    return await graph.select(query);
  } catch (error) {
    console.warn("Primary query failed, trying fallback:", error.message);
    
    try {
      return await graph.select(fallbackQuery);
    } catch (fallbackError) {
      console.error("Both queries failed:", fallbackError.message);
      return []; // Return empty results
    }
  }
}

// Usage
const results = await robustQuery(
  `SELECT ?name WHERE { ?person foaf:name ?name . }`,
  `SELECT ?name WHERE { ?person rdfs:label ?name . }`
);
```

#### Retry Logic
Implement retry logic for transient failures:

```javascript
async function withRetry(operation, maxRetries = 3, delay = 1000) {
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      return await operation();
    } catch (error) {
      if (attempt === maxRetries) {
        throw error;
      }
      
      console.warn(`Attempt ${attempt} failed, retrying in ${delay}ms:`, error.message);
      await new Promise(resolve => setTimeout(resolve, delay));
      delay *= 2; // Exponential backoff
    }
  }
}

// Usage
const results = await withRetry(async () => {
  return await graph.select(query);
});
```

### Data Validation Patterns

#### Schema Validation Pipeline
Create a validation pipeline for data quality:

```javascript
import { useValidator, useZod } from 'unrdf';
import { z } from 'zod';

async function validateDataPipeline(store) {
  const validator = useValidator();
  const zod = useZod();
  
  const results = {
    shacl: null,
    zod: null,
    quality: null
  };
  
  // 1. SHACL validation
  try {
    results.shacl = await validator.validate(store, shapesStore);
  } catch (error) {
    results.shacl = { error: error.message };
  }
  
  // 2. Zod validation
  try {
    const queryResults = await graph.select(`
      SELECT ?name ?age ?email WHERE {
        ?person foaf:name ?name ;
                foaf:age ?age .
        OPTIONAL { ?person foaf:mbox ?email }
      }
    `);
    
    const PersonSchema = z.object({
      name: z.string().min(1),
      age: z.number().min(0).max(150),
      email: z.string().email().optional()
    });
    
    results.zod = await zod.validateResults(queryResults, PersonSchema);
  } catch (error) {
    results.zod = { error: error.message };
  }
  
  // 3. Data quality assessment
  results.quality = assessDataQuality(store);
  
  return results;
}
```

#### Incremental Validation
Validate data as it's added:

```javascript
class ValidatingStore {
  constructor(shapesStore, zodSchema) {
    this.store = useStore();
    this.validator = useValidator();
    this.zod = useZod();
    this.shapesStore = shapesStore;
    this.zodSchema = zodSchema;
    this.validationCache = useCache();
  }
  
  async add(quad) {
    // Add the quad
    this.store.add(quad);
    
    // Validate incrementally
    const validationKey = `quad:${quadHash(quad)}`;
    if (!this.validationCache.has(validationKey)) {
      try {
        await this.validateQuad(quad);
        this.validationCache.set(validationKey, true);
      } catch (error) {
        // Remove invalid quad
        this.store.remove(quad);
        throw error;
      }
    }
  }
  
  async validateQuad(quad) {
    // Create a temporary store with just this quad
    const tempStore = useStore();
    tempStore.add(quad);
    
    // Validate against shapes
    const shaclReport = await this.validator.validate(tempStore, this.shapesStore);
    if (!shaclReport.conforms) {
      throw new Error(`SHACL validation failed: ${shaclReport.results}`);
    }
    
    // Validate with Zod if applicable
    if (this.zodSchema) {
      const queryResults = await graph.select(`
        SELECT ?s ?p ?o WHERE {
          ?s ?p ?o .
        }
      `);
      
      const zodResult = await this.zod.validateResults(queryResults, this.zodSchema);
      if (!zodResult.valid) {
        throw new Error(`Zod validation failed: ${zodResult.errors}`);
      }
    }
  }
}
```

## Advanced Serialization Patterns

### Custom Serialization

#### Context-Aware Serialization
Serialize data with appropriate context:

```javascript
async function serializeWithContext(store, context) {
  const jsonld = useJsonLd();
  
  // Add context to the store
  const contextQuad = terms.quad(
    terms.iri(""),
    terms.iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
    terms.iri("http://www.w3.org/ns/json-ld#Context")
  );
  
  const contextStore = useStore();
  contextStore.add(contextQuad);
  
  // Merge with original store
  const mergedStore = useStore();
  for (const quad of store) {
    mergedStore.add(quad);
  }
  for (const quad of contextStore) {
    mergedStore.add(quad);
  }
  
  return await jsonld.toJSONLD(mergedStore, context);
}
```

#### Streaming Serialization
Serialize large datasets efficiently:

```javascript
import { Transform } from 'stream';

class RDFStreamSerializer extends Transform {
  constructor(format = 'nquads') {
    super({ objectMode: true });
    this.format = format;
    this.nquads = useNQuads();
  }
  
  _transform(quad, encoding, callback) {
    try {
      const store = useStore();
      store.add(quad);
      
      let serialized;
      switch (this.format) {
        case 'nquads':
          serialized = this.nquads.serialize(store);
          break;
        case 'turtle':
          serialized = turtle.serialize(store);
          break;
        default:
          throw new Error(`Unsupported format: ${this.format}`);
      }
      
      this.push(serialized);
      callback();
    } catch (error) {
      callback(error);
    }
  }
}

// Usage
const serializer = new RDFStreamSerializer('nquads');
const output = createWriteStream('output.nq');

serializer.pipe(output);

// Stream quads to serializer
for (const quad of store) {
  serializer.write(quad);
}
serializer.end();
```

## Monitoring and Observability

### Metrics Collection

#### Performance Metrics
Track performance metrics for operations:

```javascript
import { useMetrics } from 'unrdf';

const metrics = useMetrics();

class InstrumentedGraph {
  constructor(store) {
    this.graph = useGraph(store);
  }
  
  async select(query) {
    metrics.startTimer('sparql.select');
    metrics.increment('sparql.queries');
    
    try {
      const results = await this.graph.select(query);
      metrics.increment('sparql.success');
      metrics.set('sparql.last_result_count', results.length);
      return results;
    } catch (error) {
      metrics.increment('sparql.errors');
      throw error;
    } finally {
      const duration = metrics.endTimer('sparql.select');
      metrics.set('sparql.last_duration', duration);
    }
  }
}

// Usage
const instrumentedGraph = new InstrumentedGraph(store);
const results = await instrumentedGraph.select(query);

// Get metrics
const allMetrics = metrics.getAll();
console.log('SPARQL Metrics:', allMetrics);
```

#### Data Quality Metrics
Monitor data quality over time:

```javascript
function collectDataQualityMetrics(store) {
  const metrics = useMetrics();
  
  // Basic statistics
  metrics.set('data.quads', store.size());
  metrics.set('data.subjects', new Set(Array.from(store).map(q => q.subject.value)).size);
  metrics.set('data.predicates', new Set(Array.from(store).map(q => q.predicate.value)).size);
  metrics.set('data.objects', new Set(Array.from(store).map(q => q.object.value)).size);
  
  // Quality indicators
  const persons = getSubjectsByType(store, "foaf:Person");
  const personsWithNames = persons.filter(p => getOne(store, p, "foaf:name"));
  const nameCompleteness = personsWithNames.length / persons.length;
  
  metrics.set('quality.name_completeness', nameCompleteness);
  
  // Check for common issues
  const issues = findDataQualityIssues(store);
  metrics.set('quality.issues_count', issues.length);
  
  return metrics.getAll();
}
```

### Logging and Debugging

#### Structured Logging
Implement structured logging for RDF operations:

```javascript
class RDFLogger {
  constructor() {
    this.logs = [];
  }
  
  log(level, message, context = {}) {
    const logEntry = {
      timestamp: new Date().toISOString(),
      level,
      message,
      context: {
        ...context,
        storeSize: context.store?.size() || 0
      }
    };
    
    this.logs.push(logEntry);
    
    // Also log to console in development
    if (process.env.NODE_ENV === 'development') {
      console.log(`[${level.toUpperCase()}] ${message}`, context);
    }
  }
  
  info(message, context) {
    this.log('info', message, context);
  }
  
  warn(message, context) {
    this.log('warn', message, context);
  }
  
  error(message, context) {
    this.log('error', message, context);
  }
  
  getLogs() {
    return this.logs;
  }
}

// Usage
const logger = new RDFLogger();

async function loggedQuery(query) {
  logger.info('Executing SPARQL query', { query });
  
  try {
    const results = await graph.select(query);
    logger.info('Query executed successfully', { 
      resultCount: results.length 
    });
    return results;
  } catch (error) {
    logger.error('Query execution failed', { 
      query, 
      error: error.message 
    });
    throw error;
  }
}
```

## Testing Patterns

### Comprehensive Testing

#### Store Testing Utilities
Create utilities for testing RDF stores:

```javascript
function createTestStore() {
  const store = useStore();
  const terms = useTerms({ baseIRI: "http://test.example.org/" });
  
  // Add test data
  const person1 = terms.iri("person1");
  const person2 = terms.iri("person2");
  
  store.add(terms.quad(person1, terms.iri("name"), terms.lit("Alice")));
  store.add(terms.quad(person1, terms.iri("age"), terms.lit(30)));
  store.add(terms.quad(person2, terms.iri("name"), terms.lit("Bob")));
  store.add(terms.quad(person2, terms.iri("age"), terms.lit(25)));
  
  return { store, terms };
}

function assertStoreContains(store, expectedQuads) {
  for (const expectedQuad of expectedQuads) {
    if (!store.has(expectedQuad)) {
      throw new Error(`Store does not contain expected quad: ${quadToString(expectedQuad)}`);
    }
  }
}

function assertStoreSize(store, expectedSize) {
  if (store.size() !== expectedSize) {
    throw new Error(`Expected store size ${expectedSize}, got ${store.size()}`);
  }
}
```

#### Integration Testing
Test complete workflows:

```javascript
import { describe, it, expect, beforeEach } from 'vitest';

describe('Person Management Workflow', () => {
  let store, terms, graph;
  
  beforeEach(() => {
    const testData = createTestStore();
    store = testData.store;
    terms = testData.terms;
    graph = useGraph(store);
  });
  
  it('should create, query, and update person data', async () => {
    // Create a new person
    const newPerson = terms.iri("person3");
    store.add(terms.quad(newPerson, terms.iri("name"), terms.lit("Charlie")));
    store.add(terms.quad(newPerson, terms.iri("age"), terms.lit(35)));
    
    // Query for all persons
    const results = await graph.select(`
      SELECT ?name ?age WHERE {
        ?person <http://test.example.org/name> ?name ;
                <http://test.example.org/age> ?age .
      }
    `);
    
    expect(results).toHaveLength(3);
    
    // Update person data
    const ageQuad = terms.quad(newPerson, terms.iri("age"), terms.lit(36));
    store.remove(terms.quad(newPerson, terms.iri("age"), terms.lit(35)));
    store.add(ageQuad);
    
    // Verify update
    const updatedResults = await graph.select(`
      SELECT ?age WHERE {
        <http://test.example.org/person3> <http://test.example.org/age> ?age .
      }
    `);
    
    expect(updatedResults[0].age).toBe(36);
  });
});
```

## Conclusion

These advanced patterns will help you build robust, performant, and maintainable RDF applications with unrdf. Remember to:

1. **Profile your code** to identify performance bottlenecks
2. **Implement comprehensive error handling** for production resilience
3. **Use structured logging** for better observability
4. **Write thorough tests** to ensure reliability
5. **Monitor data quality** to maintain high standards

For more specific use cases and examples, check out the [Examples](../examples/) section.
