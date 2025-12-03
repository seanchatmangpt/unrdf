# API Reference

Complete technical documentation for all unrdf APIs.

## Core Composables

### useStore

Creates and manages an N3.Store instance.

```javascript
import { useStore } from 'unrdf';

const store = useStore(initialQuads, options);
```

**Parameters:**
- `initialQuads` (Array<Quad>, optional): Initial quads to add to the store
- `options` (Object, optional): Configuration options

**Returns:** Store instance with methods:
- `add(quads)`: Add quads to the store
- `remove(quads)`: Remove quads from the store
- `clear()`: Clear all quads from the store
- `getQuads(s, p, o, g)`: Get quads matching pattern
- `countQuads(s, p, o, g)`: Count quads matching pattern
- `has(quad)`: Check if quad exists
- `stats()`: Get store statistics
- `serialize(options)`: Serialize store to string
- `getStore()`: Get underlying N3.Store
- `getEngine()`: Get RdfEngine instance

### useGraph

Provides high-level graph operations.

```javascript
import { useGraph } from 'unrdf';

const graph = useGraph(store);
```

**Parameters:**
- `store` (Store): N3.Store instance

**Returns:** Graph instance with methods:
- `select(sparql, options)`: Execute SELECT query
- `ask(sparql, options)`: Execute ASK query
- `construct(sparql, options)`: Execute CONSTRUCT query
- `update(sparql, options)`: Execute UPDATE query
- `validate(shapes, options)`: Validate against SHACL shapes
- `validateOrThrow(shapes, options)`: Validate and throw on failure
- `serialize(options)`: Serialize graph
- `stats()`: Get graph statistics
- `getStore()`: Get underlying store
- `getEngine()`: Get RdfEngine instance

### useTerms

Creates RDF terms using N3.DataFactory.

```javascript
import { useTerms } from 'unrdf';

const terms = useTerms(options);
```

**Parameters:**
- `options` (Object, optional): Configuration options
  - `baseIRI` (string): Base IRI for relative IRIs
  - `defaultDatatype` (string): Default datatype for literals

**Returns:** Terms instance with methods:
- `iri(iri)`: Create NamedNode
- `lit(value, datatype, language)`: Create Literal
- `bnode(id)`: Create BlankNode
- `quad(s, p, o, g)`: Create Quad
- `defaultGraph()`: Get DefaultGraph
- `getBaseIRI()`: Get base IRI
- `getDefaultDatatype()`: Get default datatype

### usePrefixes

Manages prefix mappings.

```javascript
import { usePrefixes } from 'unrdf';

const prefixes = usePrefixes(initialPrefixes, options);
```

**Parameters:**
- `initialPrefixes` (Object, optional): Initial prefix mappings
- `options` (Object, optional): Configuration options
  - `caseSensitive` (boolean): Case sensitivity for prefix matching

**Returns:** Prefixes instance with methods:
- `register(prefixes)`: Register prefix mappings
- `expand(curie)`: Expand CURIE to full IRI
- `shrink(iri)`: Shrink IRI to CURIE
- `list()`: Get all prefix mappings
- `get(prefix)`: Get IRI for prefix
- `has(prefix)`: Check if prefix exists
- `clear()`: Clear all prefixes
- `size()`: Get number of prefixes

## I/O Composables

### useTurtle

Handles Turtle parsing and serialization.

```javascript
import { useTurtle } from 'unrdf';

const turtle = useTurtle();
```

**Returns:** Turtle instance with methods:
- `parse(ttl, options)`: Parse Turtle string to store
- `write(store, options)`: Write store to Turtle string
- `getEngine()`: Get RdfEngine instance

### useTurtleFS

Manages Turtle files on filesystem.

```javascript
import { useTurtleFS } from 'unrdf';

const turtleFS = useTurtleFS(graphDir, options);
```

**Parameters:**
- `graphDir` (string): Directory for Turtle files
- `options` (Object, optional): Configuration options

**Returns:** TurtleFS instance with methods:
- `read(filename)`: Read Turtle file to store
- `write(filename, store)`: Write store to Turtle file
- `list()`: List all Turtle files
- `exists(filename)`: Check if file exists
- `delete(filename)`: Delete Turtle file
- `getGraphDir()`: Get graph directory
- `getEngine()`: Get RdfEngine instance

### useNQuads

Handles N-Quads parsing and serialization.

```javascript
import { useNQuads } from 'unrdf';

const nquads = useNQuads(options);
```

**Parameters:**
- `options` (Object, optional): Configuration options

**Returns:** NQuads instance with methods:
- `parse(nquads, options)`: Parse N-Quads string to store
- `write(store, options)`: Write store to N-Quads string
- `getEngine()`: Get RdfEngine instance

### useJsonLd

Handles JSON-LD conversion.

```javascript
import { useJsonLd } from 'unrdf';

const jsonld = useJsonLd(options);
```

**Parameters:**
- `options` (Object, optional): Configuration options
  - `baseIRI` (string): Base IRI for JSON-LD operations
  - `strict` (boolean): Strict mode for validation

**Returns:** JsonLd instance with methods:
- `toJSONLD(store, options)`: Convert store to JSON-LD
- `fromJSONLD(jsonld, options)`: Convert JSON-LD to store
- `compact(jsonld, context, options)`: Compact JSON-LD
- `expand(jsonld, options)`: Expand JSON-LD
- `frame(jsonld, frame, options)`: Frame JSON-LD
- `flatten(jsonld, options)`: Flatten JSON-LD
- `validate(jsonld)`: Validate JSON-LD
- `getStats(jsonld)`: Get JSON-LD statistics
- `toTurtle(jsonld, options)`: Convert JSON-LD to Turtle
- `fromTurtle(turtle, options)`: Convert Turtle to JSON-LD
- `getBaseIRI()`: Get base IRI
- `isValid(content)`: Check if content is valid JSON-LD

## Query and Traversal

### usePointer

Provides Clownface-based graph traversal.

```javascript
import { usePointer } from 'unrdf';

const pointer = usePointer(store, options);
```

**Parameters:**
- `store` (Store): N3.Store instance
- `options` (Object, optional): Configuration options
  - `baseIRI` (string): Base IRI for traversal

**Returns:** Pointer instance with methods:
- `node(term)`: Get pointer to node
- `blankNode(id)`: Create blank node pointer
- `namedNode(iri)`: Create named node pointer
- `literal(value, datatype, language)`: Create literal pointer
- `ofType(type)`: Get nodes of type
- `withValue(value)`: Get nodes with value
- `withProperty(property)`: Get nodes with property
- `getClownface()`: Get Clownface instance
- `getEngine()`: Get RdfEngine instance
- `getBaseIRI()`: Get base IRI
- `withBaseIRI(baseIRI)`: Create pointer with new base IRI
- `getStats()`: Get traversal statistics
- `isEmpty()`: Check if pointer is empty
- `size()`: Get pointer size

## Validation and Reasoning

### useValidator

Performs SHACL validation.

```javascript
import { useValidator } from 'unrdf';

const validator = useValidator(options);
```

**Parameters:**
- `options` (Object, optional): Configuration options

**Returns:** Validator instance with methods:
- `validate(data, shapes, options)`: Validate data against shapes
- `validateOrThrow(data, shapes, options)`: Validate and throw on failure
- `getEngine()`: Get RdfEngine instance

### useReasoner

Applies N3 rules for reasoning.

```javascript
import { useReasoner } from 'unrdf';

const reasoner = useReasoner(options);
```

**Parameters:**
- `options` (Object, optional): Configuration options

**Returns:** Reasoner instance with methods:
- `reason(data, rules, options)`: Apply rules to data
- `getEngine()`: Get RdfEngine instance

### useCanon

Provides RDF canonicalization.

```javascript
import { useCanon } from 'unrdf';

const canon = useCanon(options);
```

**Parameters:**
- `options` (Object, optional): Configuration options

**Returns:** Canon instance with methods:
- `canonicalize(store, options)`: Canonicalize store
- `isomorphic(storeA, storeB, options)`: Check if stores are isomorphic
- `getEngine()`: Get RdfEngine instance

## Type Safety and Validation

### useZod

Integrates Zod for runtime validation.

```javascript
import { useZod } from 'unrdf';

const zod = useZod(options);
```

**Parameters:**
- `options` (Object, optional): Configuration options

**Returns:** Zod instance with methods:
- `validate(schema, data)`: Validate data against schema
- `validateOrThrow(schema, data)`: Validate and throw on failure
- `getEngine()`: Get RdfEngine instance

## Change and Provenance

### useDelta

Calculates and applies graph differences.

```javascript
import { useDelta } from 'unrdf';

const delta = useDelta(options);
```

**Parameters:**
- `options` (Object, optional): Configuration options

**Returns:** Delta instance with methods:
- `calculate(storeA, storeB, options)`: Calculate difference between stores
- `apply(store, changes, options)`: Apply changes to store
- `invert(changes, options)`: Invert changes
- `merge(changesA, changesB, options)`: Merge change sets
- `getEngine()`: Get RdfEngine instance

## Performance and Caching

### useMetrics

Collects performance metrics.

```javascript
import { useMetrics } from 'unrdf';

const metrics = useMetrics(options);
```

**Parameters:**
- `options` (Object, optional): Configuration options
  - `enabled` (boolean): Enable metrics collection
  - `maxHistory` (number): Maximum history size

**Returns:** Metrics instance with methods:
- `wrap(label, fn)`: Wrap function with metrics
- `timer(label)`: Create performance timer
- `record(label, value, metadata)`: Record metric
- `get(label)`: Get metric by label
- `getAll()`: Get all metrics
- `timeline()`: Get metrics timeline
- `clear()`: Clear all metrics
- `isEnabled()`: Check if metrics enabled
- `enable()`: Enable metrics
- `disable()`: Disable metrics

### useCache

Implements caching strategies.

```javascript
import { useCache } from 'unrdf';

const cache = useCache(options);
```

**Parameters:**
- `options` (Object, optional): Configuration options
  - `maxSize` (number): Maximum cache size
  - `defaultTTL` (number): Default time-to-live
  - `enabled` (boolean): Enable caching

**Returns:** Cache instance with methods:
- `get(key, fn, options)`: Get or compute value
- `set(key, value, options)`: Set value
- `has(key)`: Check if key exists
- `delete(key)`: Delete key
- `clear()`: Clear all entries
- `size()`: Get cache size
- `keys()`: Get all keys
- `values()`: Get all values
- `entries()`: Get all entries
- `isEnabled()`: Check if cache enabled
- `enable()`: Enable cache
- `disable()`: Disable cache
- `getStats()`: Get cache statistics

## Utilities

### Term Utilities

```javascript
import { 
  asNamedNode, asLiteral, quadToJSON, 
  jsonToQuad, validateQuadJSON 
} from 'unrdf/utils';
```

- `asNamedNode(iri)`: Convert to NamedNode
- `asLiteral(value, datatype)`: Convert to Literal
- `quadToJSON(quad)`: Convert quad to JSON
- `jsonToQuad(json)`: Convert JSON to quad
- `validateQuadJSON(obj)`: Validate quad JSON

### Graph Utilities

```javascript
import { 
  pluck, indexByPredicate, debugTurtle 
} from 'unrdf/utils';
```

- `pluck(quads, predicate)`: Extract values by predicate
- `indexByPredicate(quads)`: Index quads by predicate
- `debugTurtle(store, options)`: Debug Turtle output

### ID Utilities

```javascript
import { makeBNodeGenerator } from 'unrdf/utils';
```

- `makeBNodeGenerator(prefix)`: Create blank node generator

## Engines

### RdfEngine

Internal engine class (not directly used).

```javascript
import { RdfEngine } from 'unrdf/engines';

const engine = new RdfEngine(options);
```

**Parameters:**
- `options` (Object, optional): Configuration options

**Returns:** RdfEngine instance with methods:
- `getStore()`: Get N3.Store instance
- `getQueryEngine()`: Get Comunica query engine
- `getValidator()`: Get SHACL validator
- `getReasoner()`: Get EYE reasoner
- `getCanonicalizer()`: Get RDF canonicalizer
- `getJsonLdProcessor()`: Get JSON-LD processor
- `getClownface(store)`: Get Clownface instance
- `toJSONLD(store, options)`: Convert store to JSON-LD
- `fromJSONLD(jsonld, options)`: Convert JSON-LD to store

## Error Handling

All composables throw errors with descriptive messages:

- `[useStore] Quads must be an array`
- `[useGraph] Store is required`
- `[useTerms] IRI must be a string`
- `[useValidator] Data is required`
- `[useReasoner] Rules are required`
- `[useCanon] Store is required`
- `[useZod] Schema is required`
- `[useDelta] Changes must be an array`
- `[useMetrics] Function is required`
- `[useCache] Key is required`

## Configuration

### Global Options

All composables accept an `options` parameter for configuration:

```javascript
const options = {
  baseIRI: 'http://example.org/',
  strict: true,
  enabled: true,
  maxSize: 1000,
  defaultTTL: 300000
};
```

### Environment Variables

- `UNRDF_BASE_IRI`: Default base IRI
- `UNRDF_STRICT_MODE`: Enable strict mode
- `UNRDF_CACHE_SIZE`: Default cache size
- `UNRDF_CACHE_TTL`: Default cache TTL

## See Also

- [Getting Started](./getting-started.md) - Basic usage guide
- [Core Concepts](./core-concepts.md) - Understanding unrdf's philosophy
- [Composables](./composables/) - Detailed composable documentation
- [Examples](./examples.md) - Usage examples and patterns