# Core RDF API Reference

**Version**: v4.1.1
**Package**: `unrdf`
**Module**: `knowledge-engine`

This reference documents all core RDF operations including parsing, serialization, querying, validation, reasoning, and canonicalization.

---

## Table of Contents

- [Parsing & Serialization](#parsing--serialization)
- [SPARQL Queries](#sparql-queries)
- [SHACL Validation](#shacl-validation)
- [N3 Reasoning](#n3-reasoning)
- [Canonicalization](#canonicalization)
- [N3.js Re-exports](#n3js-re-exports)

---

## Parsing & Serialization

### parseTurtle

**Signature**: `parseTurtle(ttl: string, baseIRI?: string): Promise<Store>`

**Description**: Parses a Turtle/TriG string into an N3 Store.

**Parameters**:
- `ttl` (string) - The Turtle string to parse
- `baseIRI` (string, optional) - Base IRI for resolving relative URIs (default: `"http://example.org/"`)

**Returns**: Promise resolving to an N3 Store containing the parsed quads

**Throws**:
- `TypeError` if ttl or baseIRI are not strings
- `Error` if parsing fails

**Example**:
```javascript
import { parseTurtle } from 'unrdf';

const ttl = `
  @prefix ex: <http://example.org/> .
  ex:alice ex:knows ex:bob .
`;

const store = await parseTurtle(ttl, 'http://example.org/');
console.log('Parsed', store.size, 'quads');
```

**See Also**: [toTurtle](#toturtle), [parseJsonLd](#parsejsonld)

**Since**: v4.1.1

---

### toTurtle

**Signature**: `toTurtle(store: Store, options?: { prefixes?: Object, baseIRI?: string }): Promise<string>`

**Description**: Serializes an N3 Store to Turtle format.

**Parameters**:
- `store` (Store) - The N3 Store to serialize
- `options` (Object, optional) - Serialization options
  - `prefixes` (Object, optional) - Prefix mappings (e.g., `{ ex: 'http://example.org/' }`)
  - `baseIRI` (string, optional) - Base IRI for the output

**Returns**: Promise resolving to the Turtle string

**Throws**:
- `TypeError` if store is not a valid Store instance
- `Error` if serialization fails

**Example**:
```javascript
import { toTurtle, Store } from 'unrdf';

const store = new Store();
// ... add quads

const turtle = await toTurtle(store, {
  prefixes: { ex: 'http://example.org/' },
  baseIRI: 'http://example.org/'
});

console.log(turtle);
```

**See Also**: [parseTurtle](#parseturtle), [toNQuads](#tonquads)

**Since**: v4.1.1

---

### toNQuads

**Signature**: `toNQuads(store: Store, options?: Object): Promise<string>`

**Description**: Serializes an N3 Store to canonical N-Quads format.

**Parameters**:
- `store` (Store) - The N3 Store to serialize
- `options` (Object, optional) - Serialization options

**Returns**: Promise resolving to the N-Quads string

**Throws**:
- `TypeError` if store is not a valid Store instance
- `Error` if serialization fails

**Example**:
```javascript
import { toNQuads } from 'unrdf';

const nquads = await toNQuads(store);
console.log(nquads);
```

**See Also**: [toTurtle](#toturtle), [parseTurtle](#parseturtle)

**Since**: v4.1.1

---

### parseJsonLd

**Signature**: `parseJsonLd(jsonld: string | Object, options?: Object): Promise<Store>`

**Description**: Parses a JSON-LD string or object into an N3 Store.

**Parameters**:
- `jsonld` (string | Object) - The JSON-LD to parse
- `options` (Object, optional) - Parsing options
  - `baseIRI` (string, optional) - Base IRI for resolving relative URIs

**Returns**: Promise resolving to an N3 Store containing the parsed quads

**Throws**:
- `TypeError` if jsonld is not a string or object
- `Error` if parsing fails or JSON-LD structure is invalid

**Example**:
```javascript
import { parseJsonLd } from 'unrdf';

const jsonld = {
  "@context": { "ex": "http://example.org/" },
  "@id": "ex:alice",
  "ex:knows": { "@id": "ex:bob" }
};

const store = await parseJsonLd(jsonld);
console.log('Parsed', store.size, 'quads');
```

**See Also**: [toJsonLd](#tojsonld), [parseTurtle](#parseturtle)

**Since**: v4.1.1

---

### toJsonLd

**Signature**: `toJsonLd(store: Store, options?: { context?: Object, baseIRI?: string }): Promise<Object>`

**Description**: Serializes an N3 Store to JSON-LD format.

**Parameters**:
- `store` (Store) - The N3 Store to serialize
- `options` (Object, optional) - Serialization options
  - `context` (Object, optional) - JSON-LD context
  - `baseIRI` (string, optional) - Base IRI for the output

**Returns**: Promise resolving to the JSON-LD object (not a string)

**Throws**:
- `TypeError` if store is not a valid Store instance
- `Error` if serialization fails

**Example**:
```javascript
import { toJsonLd } from 'unrdf';

const jsonld = await toJsonLd(store, {
  context: { ex: 'http://example.org/' }
});

console.log(JSON.stringify(jsonld, null, 2));
```

**See Also**: [parseJsonLd](#parsejsonld), [toTurtle](#toturtle)

**Since**: v4.1.1

---

## SPARQL Queries

### query

**Signature**: `query(store: Store, sparql: string, options?: { limit?: number, signal?: AbortSignal, deterministic?: boolean }): Promise<any>`

**Description**: Universal SPARQL query executor. Supports SELECT, ASK, CONSTRUCT, DESCRIBE, and UPDATE queries.

**Parameters**:
- `store` (Store) - The N3 Store to query against
- `sparql` (string) - The SPARQL query string
- `options` (Object, optional) - Query options
  - `limit` (number, optional) - Maximum number of results
  - `signal` (AbortSignal, optional) - Abort signal for cancellation
  - `deterministic` (boolean, optional) - Enable deterministic results

**Returns**: Promise resolving to query results:
- SELECT: Array of binding objects
- ASK: Boolean
- CONSTRUCT/DESCRIBE: N3 Store
- UPDATE: Updated store

**Throws**:
- `TypeError` if store or sparql are invalid
- `Error` if query execution fails

**Example**:
```javascript
import { query, Store } from 'unrdf';

const store = new Store();
// ... add quads

const results = await query(store, `
  SELECT ?s ?o WHERE {
    ?s <http://example.org/knows> ?o
  }
`);

console.log(results); // [{ s: '...', o: '...' }, ...]
```

**See Also**: [select](#select), [ask](#ask), [construct](#construct), [describe](#describe), [update](#update)

**Since**: v4.1.1

---

### select

**Signature**: `select(store: Store, sparql: string, options?: Object): Promise<Array<Object>>`

**Description**: Execute a SPARQL SELECT query and return bindings.

**Parameters**:
- `store` (Store) - The N3 Store to query against
- `sparql` (string) - The SPARQL SELECT query
- `options` (Object, optional) - Query options

**Returns**: Promise resolving to array of binding objects

**Throws**:
- `TypeError` if store or sparql are invalid
- `Error` if query execution fails or query is not a SELECT

**Example**:
```javascript
import { select } from 'unrdf';

const bindings = await select(store, `
  SELECT ?name ?age WHERE {
    ?person <http://example.org/name> ?name ;
             <http://example.org/age> ?age .
  }
`);

console.log(bindings);
// [{ name: 'Alice', age: '30' }, { name: 'Bob', age: '25' }]
```

**See Also**: [query](#query), [ask](#ask)

**Since**: v4.1.1

---

### ask

**Signature**: `ask(store: Store, sparql: string, options?: Object): Promise<boolean>`

**Description**: Execute a SPARQL ASK query and return boolean result.

**Parameters**:
- `store` (Store) - The N3 Store to query against
- `sparql` (string) - The SPARQL ASK query
- `options` (Object, optional) - Query options

**Returns**: Promise resolving to boolean result

**Throws**:
- `TypeError` if store or sparql are invalid
- `Error` if query execution fails or query is not an ASK

**Example**:
```javascript
import { ask } from 'unrdf';

const hasData = await ask(store, `
  ASK WHERE {
    ?s ?p ?o .
  }
`);

console.log('Store has data:', hasData);
```

**See Also**: [query](#query), [select](#select)

**Since**: v4.1.1

---

### construct

**Signature**: `construct(store: Store, sparql: string, options?: Object): Promise<Store>`

**Description**: Execute a SPARQL CONSTRUCT query and return a new store with constructed quads.

**Parameters**:
- `store` (Store) - The N3 Store to query against
- `sparql` (string) - The SPARQL CONSTRUCT query
- `options` (Object, optional) - Query options

**Returns**: Promise resolving to a new N3 Store with constructed quads

**Throws**:
- `TypeError` if store or sparql are invalid
- `Error` if query execution fails or query is not a CONSTRUCT

**Example**:
```javascript
import { construct } from 'unrdf';

const constructed = await construct(store, `
  CONSTRUCT {
    ?person <http://example.org/type> <http://example.org/Person> .
  } WHERE {
    ?person <http://example.org/name> ?name .
  }
`);

console.log('Constructed', constructed.size, 'quads');
```

**See Also**: [query](#query), [describe](#describe)

**Since**: v4.1.1

---

### describe

**Signature**: `describe(store: Store, sparql: string, options?: Object): Promise<Store>`

**Description**: Execute a SPARQL DESCRIBE query and return a new store with described quads.

**Parameters**:
- `store` (Store) - The N3 Store to query against
- `sparql` (string) - The SPARQL DESCRIBE query
- `options` (Object, optional) - Query options

**Returns**: Promise resolving to a new N3 Store with described quads

**Throws**:
- `TypeError` if store or sparql are invalid
- `Error` if query execution fails or query is not a DESCRIBE

**Example**:
```javascript
import { describe } from 'unrdf';

const described = await describe(store, `
  DESCRIBE <http://example.org/alice>
`);

console.log('Described', described.size, 'quads');
```

**See Also**: [query](#query), [construct](#construct)

**Since**: v4.1.1

---

### update

**Signature**: `update(store: Store, sparql: string, options?: Object): Promise<Store>`

**Description**: Execute a SPARQL UPDATE operation (INSERT, DELETE, etc.) and return the updated store.

**Parameters**:
- `store` (Store) - The N3 Store to update
- `sparql` (string) - The SPARQL UPDATE query
- `options` (Object, optional) - Update options

**Returns**: Promise resolving to the updated N3 Store

**Throws**:
- `TypeError` if store or sparql are invalid
- `Error` if update execution fails

**Example**:
```javascript
import { update } from 'unrdf';

const updated = await update(store, `
  INSERT DATA {
    <http://example.org/alice> <http://example.org/age> "31" .
  }
`);

console.log('Updated store has', updated.size, 'quads');
```

**See Also**: [query](#query)

**Since**: v4.1.1

---

## SHACL Validation

### validateShacl

**Signature**: `validateShacl(store: Store, shapes: Store | string, options?: { strict?: boolean, includeDetails?: boolean }): { conforms: boolean, results: Array<object> }`

**Description**: Validates an N3 Store against SHACL shapes.

**Parameters**:
- `store` (Store) - The N3 Store containing data to validate
- `shapes` (Store | string) - The N3 Store or Turtle string containing SHACL shapes
- `options` (Object, optional) - Validation options
  - `strict` (boolean, optional) - Enable strict validation mode
  - `includeDetails` (boolean, optional) - Include detailed validation results

**Returns**: Validation report object with:
- `conforms` (boolean) - Whether the data conforms to shapes
- `results` (Array) - Array of validation result objects

**Throws**:
- `TypeError` if store or shapes are invalid
- `Error` if validation fails

**Example**:
```javascript
import { validateShacl, Store } from 'unrdf';

const dataStore = new Store();
// ... add data quads

const shapesTtl = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:minCount 1 ;
      sh:maxCount 1
    ] .
`;

const report = validateShacl(dataStore, shapesTtl);
console.log('Conforms:', report.conforms);
console.log('Results:', report.results);
```

**See Also**: [validateShaclMultiple](#validateshaclmultiple), [formatValidationReport](#formatvalidationreport)

**Since**: v4.1.1

---

### validateShaclMultiple

**Signature**: `validateShaclMultiple(store: Store, shapesList: Array<Store | string>, options?: Object): { conforms: boolean, results: Array<object>, shapeResults: Array<object> }`

**Description**: Validates an N3 Store against multiple SHACL shape sets.

**Parameters**:
- `store` (Store) - The N3 Store containing data to validate
- `shapesList` (Array) - Array of N3 Stores or Turtle strings containing SHACL shapes
- `options` (Object, optional) - Validation options

**Returns**: Combined validation report object with:
- `conforms` (boolean) - Overall conformance
- `results` (Array) - All validation results
- `shapeResults` (Array) - Per-shape-set results
- `totalShapes` (number) - Number of shape sets
- `totalResults` (number) - Total number of results

**Throws**:
- `TypeError` if store or shapesList are invalid
- `Error` if validation fails

**Example**:
```javascript
import { validateShaclMultiple } from 'unrdf';

const shapesList = [
  personShapesTtl,
  organizationShapesTtl,
  contactShapesTtl
];

const report = validateShaclMultiple(store, shapesList);
console.log('Overall conforms:', report.conforms);
console.log('Shape-specific results:', report.shapeResults);
```

**See Also**: [validateShacl](#validateshacl)

**Since**: v4.1.1

---

### formatValidationReport

**Signature**: `formatValidationReport(validationResult: Object, options?: { includeSummary?: boolean, groupBySeverity?: boolean }): Object`

**Description**: Formats a SHACL validation report in a structured format.

**Parameters**:
- `validationResult` (Object) - Result from validateShacl
- `options` (Object, optional) - Formatting options
  - `includeSummary` (boolean, optional) - Include summary statistics
  - `groupBySeverity` (boolean, optional) - Group results by severity

**Returns**: Formatted validation report object

**Throws**:
- `TypeError` if validationResult is not an object

**Example**:
```javascript
import { validateShacl, formatValidationReport } from 'unrdf';

const report = validateShacl(store, shapes);
const formatted = formatValidationReport(report, {
  includeSummary: true,
  groupBySeverity: true
});

console.log(formatted);
```

**See Also**: [validateShacl](#validateshacl), [hasValidationErrors](#hasvalidationerrors)

**Since**: v4.1.1

---

### hasValidationErrors

**Signature**: `hasValidationErrors(validationResult: Object): boolean`

**Description**: Checks if a validation result contains any errors (violations).

**Parameters**:
- `validationResult` (Object) - Result from validateShacl

**Returns**: True if there are validation errors

**Example**:
```javascript
import { validateShacl, hasValidationErrors } from 'unrdf';

const report = validateShacl(store, shapes);
if (hasValidationErrors(report)) {
  console.log('Validation failed with errors');
}
```

**See Also**: [validateShacl](#validateshacl), [getValidationErrors](#getvalidationerrors)

**Since**: v4.1.1

---

### getValidationErrors

**Signature**: `getValidationErrors(validationResult: Object): Array<Object>`

**Description**: Extracts validation errors (violations) from a validation result.

**Parameters**:
- `validationResult` (Object) - Result from validateShacl

**Returns**: Array of validation error objects

**Example**:
```javascript
import { validateShacl, getValidationErrors } from 'unrdf';

const report = validateShacl(store, shapes);
const errors = getValidationErrors(report);

errors.forEach(error => {
  console.log(`Error: ${error.message} at ${error.focusNode}`);
});
```

**See Also**: [validateShacl](#validateshacl), [getValidationWarnings](#getvalidationwarnings)

**Since**: v4.1.1

---

### getValidationWarnings

**Signature**: `getValidationWarnings(validationResult: Object): Array<Object>`

**Description**: Extracts validation warnings from a validation result.

**Parameters**:
- `validationResult` (Object) - Result from validateShacl

**Returns**: Array of validation warning objects

**Example**:
```javascript
import { validateShacl, getValidationWarnings } from 'unrdf';

const report = validateShacl(store, shapes);
const warnings = getValidationWarnings(report);

warnings.forEach(warning => {
  console.log(`Warning: ${warning.message} at ${warning.focusNode}`);
});
```

**See Also**: [validateShacl](#validateshacl), [getValidationErrors](#getvalidationerrors)

**Since**: v4.1.1

---

## N3 Reasoning

### reason

**Signature**: `reason(store: Store, rules: Store | string, options?: { includeOriginal?: boolean, maxIterations?: number, debug?: boolean }): Promise<Store>`

**Description**: Executes forward-chaining reasoning using N3 rules.

**Parameters**:
- `store` (Store) - The N3 Store containing data to reason over
- `rules` (Store | string) - The N3 Store or Turtle string containing N3 rules
- `options` (Object, optional) - Reasoning options
  - `includeOriginal` (boolean, optional) - Include original data in result (default: true)
  - `maxIterations` (number, optional) - Maximum reasoning iterations (default: 100)
  - `debug` (boolean, optional) - Enable debug output (default: false)

**Returns**: Promise resolving to a new N3 Store containing original and inferred quads

**Throws**:
- `TypeError` if store or rules are invalid
- `Error` if reasoning fails

**Example**:
```javascript
import { reason, Store } from 'unrdf';

const dataStore = new Store();
// ... add data quads

const rulesTtl = `
  @prefix ex: <http://example.org/> .
  @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

  { ?x ex:parent ?y } => { ?x rdfs:subClassOf ?y } .
`;

const reasonedStore = await reason(dataStore, rulesTtl);
console.log('Original quads:', dataStore.size);
console.log('Reasoned quads:', reasonedStore.size);
```

**See Also**: [reasonMultiple](#reasonmultiple), [extractInferred](#extractinferred)

**Since**: v4.1.1

---

### reasonMultiple

**Signature**: `reasonMultiple(store: Store, rulesList: Array<Store | string>, options?: Object): Promise<Store>`

**Description**: Executes reasoning with multiple rule sets sequentially.

**Parameters**:
- `store` (Store) - The N3 Store containing data to reason over
- `rulesList` (Array) - Array of N3 Stores or Turtle strings containing N3 rules
- `options` (Object, optional) - Reasoning options

**Returns**: Promise resolving to a new N3 Store with all reasoning results

**Throws**:
- `TypeError` if store or rulesList are invalid
- `Error` if reasoning fails

**Example**:
```javascript
import { reasonMultiple } from 'unrdf';

const rulesList = [
  rdfsRulesTtl,
  owlRulesTtl,
  customRulesTtl
];

const reasonedStore = await reasonMultiple(store, rulesList);
console.log('Applied', rulesList.length, 'rule sets');
```

**See Also**: [reason](#reason)

**Since**: v4.1.1

---

### extractInferred

**Signature**: `extractInferred(original: Store, reasoned: Store): Store`

**Description**: Extracts only the inferred quads (not present in original store).

**Parameters**:
- `original` (Store) - The original N3 Store before reasoning
- `reasoned` (Store) - The reasoned N3 Store after reasoning

**Returns**: New N3 Store containing only inferred quads

**Example**:
```javascript
import { reason, extractInferred } from 'unrdf';

const original = new Store();
// ... add data

const reasoned = await reason(original, rules);
const inferred = extractInferred(original, reasoned);

console.log('Inferred', inferred.size, 'new quads');
```

**See Also**: [reason](#reason)

**Since**: v4.1.1

---

### getReasoningStats

**Signature**: `getReasoningStats(reasoned: Store): Object`

**Description**: Retrieves statistics about a reasoned store.

**Parameters**:
- `reasoned` (Store) - The reasoned N3 Store

**Returns**: Statistics object with reasoning metrics

**Example**:
```javascript
import { reason, getReasoningStats } from 'unrdf';

const reasoned = await reason(store, rules);
const stats = getReasoningStats(reasoned);

console.log('Reasoning stats:', stats);
```

**See Also**: [reason](#reason)

**Since**: v4.1.1

---

### validateRules

**Signature**: `validateRules(rulesTtl: string): boolean`

**Description**: Validates N3 rule syntax before execution.

**Parameters**:
- `rulesTtl` (string) - The N3 rules in Turtle format

**Returns**: True if rules are valid

**Throws**:
- `Error` if rules are invalid

**Example**:
```javascript
import { validateRules } from 'unrdf';

const rulesTtl = `
  @prefix ex: <http://example.org/> .
  { ?x ex:parent ?y } => { ?y ex:child ?x } .
`;

const valid = validateRules(rulesTtl);
console.log('Rules valid:', valid);
```

**See Also**: [reason](#reason)

**Since**: v4.1.1

---

### createReasoningSession

**Signature**: `createReasoningSession(): ReasoningSession`

**Description**: Creates a stateful reasoning session for incremental reasoning.

**Returns**: ReasoningSession object

**Example**:
```javascript
import { createReasoningSession } from 'unrdf';

const session = createReasoningSession();
session.addRules(rulesTtl);
const result = await session.reason(store);
```

**See Also**: [reason](#reason)

**Since**: v4.1.1

---

## Canonicalization

### canonicalize

**Signature**: `canonicalize(store: Store, options?: { algorithm?: string, produceGeneralizedRdf?: boolean, timeoutMs?: number }): Promise<string>`

**Description**: Canonicalizes an N3 Store using URDNA2015 algorithm into canonical N-Quads format.

**Parameters**:
- `store` (Store) - The N3 Store to canonicalize
- `options` (Object, optional) - Canonicalization options
  - `algorithm` (string, optional) - Algorithm name (default: 'URDNA2015')
  - `produceGeneralizedRdf` (boolean, optional) - Produce generalized RDF (default: false)
  - `timeoutMs` (number, optional) - Timeout in milliseconds (default: 30000)

**Returns**: Promise resolving to canonical N-Quads string

**Throws**:
- `TypeError` if store is invalid
- `Error` if canonicalization fails or times out

**Example**:
```javascript
import { canonicalize, Store } from 'unrdf';

const store = new Store();
// ... add quads with blank nodes

const canonical = await canonicalize(store);
console.log('Canonical N-Quads:', canonical);
```

**See Also**: [isIsomorphic](#isisomorphic), [getCanonicalHash](#getcanonicalhash)

**Since**: v4.1.1

---

### isIsomorphic

**Signature**: `isIsomorphic(storeA: Store, storeB: Store, options?: { algorithm?: string, timeoutMs?: number }): Promise<boolean>`

**Description**: Checks if two N3 Stores are isomorphic (logically equivalent).

**Parameters**:
- `storeA` (Store) - First N3 Store to compare
- `storeB` (Store) - Second N3 Store to compare
- `options` (Object, optional) - Comparison options
  - `algorithm` (string, optional) - Canonicalization algorithm (default: 'URDNA2015')
  - `timeoutMs` (number, optional) - Timeout in milliseconds (default: 30000)

**Returns**: Promise resolving to true if stores are isomorphic

**Throws**:
- `TypeError` if stores are invalid
- `Error` if comparison fails

**Example**:
```javascript
import { isIsomorphic, Store } from 'unrdf';

const store1 = new Store();
const store2 = new Store();
// ... add logically equivalent quads with different blank node labels

const isomorphic = await isIsomorphic(store1, store2);
console.log('Stores are isomorphic:', isomorphic);
```

**See Also**: [canonicalize](#canonicalize)

**Since**: v4.1.1

---

### getCanonicalHash

**Signature**: `getCanonicalHash(store: Store, options?: { hashAlgorithm?: string, algorithm?: string }): Promise<string>`

**Description**: Generates a canonical hash of an N3 Store.

**Parameters**:
- `store` (Store) - The N3 Store to hash
- `options` (Object, optional) - Hashing options
  - `hashAlgorithm` (string, optional) - Hash algorithm (default: 'SHA-256')
  - `algorithm` (string, optional) - Canonicalization algorithm (default: 'URDNA2015')

**Returns**: Promise resolving to hexadecimal hash string

**Throws**:
- `TypeError` if store is invalid
- `Error` if hashing fails

**Example**:
```javascript
import { getCanonicalHash } from 'unrdf';

const hash = await getCanonicalHash(store);
console.log('Canonical hash:', hash);
```

**See Also**: [canonicalize](#canonicalize), [isIsomorphic](#isisomorphic)

**Since**: v4.1.1

---

### groupByIsomorphism

**Signature**: `groupByIsomorphism(stores: Array<Store>, options?: Object): Promise<Array<Array<Store>>>`

**Description**: Groups multiple stores by isomorphism.

**Parameters**:
- `stores` (Array) - Array of N3 Stores to group
- `options` (Object, optional) - Grouping options

**Returns**: Promise resolving to array of store groups (isomorphic stores in same group)

**Example**:
```javascript
import { groupByIsomorphism } from 'unrdf';

const stores = [store1, store2, store3, store4];
const groups = await groupByIsomorphism(stores);

console.log('Found', groups.length, 'isomorphism classes');
```

**See Also**: [isIsomorphic](#isisomorphic), [findDuplicates](#findduplicates)

**Since**: v4.1.1

---

### findDuplicates

**Signature**: `findDuplicates(stores: Array<Store>, options?: Object): Promise<Array<Array<Store>>>`

**Description**: Finds duplicate (isomorphic) stores in an array.

**Parameters**:
- `stores` (Array) - Array of N3 Stores to check
- `options` (Object, optional) - Search options

**Returns**: Promise resolving to array of duplicate store groups

**Example**:
```javascript
import { findDuplicates } from 'unrdf';

const stores = [store1, store2, store3];
const duplicates = await findDuplicates(stores);

console.log('Found', duplicates.length, 'sets of duplicates');
```

**See Also**: [isIsomorphic](#isisomorphic), [groupByIsomorphism](#groupbyisomorphism)

**Since**: v4.1.1

---

### getCanonicalizationStats

**Signature**: `getCanonicalizationStats(store: Store): Object`

**Description**: Retrieves performance metrics for canonicalization.

**Parameters**:
- `store` (Store) - The N3 Store to analyze

**Returns**: Statistics object with canonicalization metrics

**Example**:
```javascript
import { getCanonicalizationStats } from 'unrdf';

const stats = getCanonicalizationStats(store);
console.log('Canonicalization stats:', stats);
```

**See Also**: [canonicalize](#canonicalize)

**Since**: v4.1.1

---

### createCanonicalizationSession

**Signature**: `createCanonicalizationSession(): CanonicalizationSession`

**Description**: Creates a stateful canonicalization session for batch operations.

**Returns**: CanonicalizationSession object

**Example**:
```javascript
import { createCanonicalizationSession } from 'unrdf';

const session = createCanonicalizationSession();
const canonical1 = await session.canonicalize(store1);
const canonical2 = await session.canonicalize(store2);
```

**See Also**: [canonicalize](#canonicalize)

**Since**: v4.1.1

---

## N3.js Re-exports

UNRDF re-exports core N3.js classes for convenience:

### Store

**Description**: N3 quad store implementation from N3.js.

**Example**:
```javascript
import { Store } from 'unrdf';

const store = new Store();
```

**External Documentation**: [N3.js Store](https://github.com/rdfjs/N3.js#store)

**Since**: v4.1.1

---

### Parser

**Description**: N3 RDF parser from N3.js.

**Example**:
```javascript
import { Parser } from 'unrdf';

const parser = new Parser();
```

**External Documentation**: [N3.js Parser](https://github.com/rdfjs/N3.js#parsing)

**Since**: v4.1.1

---

### Writer

**Description**: N3 RDF writer from N3.js.

**Example**:
```javascript
import { Writer } from 'unrdf';

const writer = new Writer({ format: 'Turtle' });
```

**External Documentation**: [N3.js Writer](https://github.com/rdfjs/N3.js#writing)

**Since**: v4.1.1

---

### DataFactory

**Description**: RDF/JS DataFactory implementation from N3.js.

**Example**:
```javascript
import { DataFactory } from 'unrdf';

const { namedNode, literal, blankNode, quad } = DataFactory;
const triple = quad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/knows'),
  namedNode('http://example.org/bob')
);
```

**External Documentation**: [RDF/JS DataFactory](https://rdf.js.org/data-model-spec/#datafactory-interface)

**Since**: v4.1.1

---

## Related Documentation

- [Knowledge Hooks API Reference](./knowledge-hooks-api.md) - Hook definition and management
- [Composables API Reference](./composables-api.md) - High-level composable functions
- [Utilities API Reference](./utilities-api.md) - Utility functions
- [Schemas Reference](./schemas.md) - Zod validation schemas
