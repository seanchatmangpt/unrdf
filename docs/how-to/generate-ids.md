# How-To: Generate IDs

**Problem**: You need to generate unique identifiers for RDF resources - either UUIDs, content-based hashes, or deterministic IDs.

## Solution

UNRDF provides multiple ID generation strategies via `id-utils`. Choose based on your use case: random (UUID), content-based (hash), or deterministic (seed-based).

### Generate UUIDs

Use RFC4122 UUIDs for unique blank nodes and IRIs:

```javascript
import { generateUUID, generateBlankNodeId } from 'unrdf/utils';
import { DataFactory } from 'unrdf/knowledge-engine';

// Generate UUID (v4)
const uuid = generateUUID();
console.log(uuid);
// → '550e8400-e29b-41d4-a716-446655440000'

// Generate blank node with UUID
const bnodeId = generateBlankNodeId();
console.log(bnodeId);
// → '_:b550e8400e29b41d4a716446655440000'

// Create blank node
const bnode = DataFactory.blankNode(bnodeId);

// Create IRI with UUID
const personIRI = `http://example.org/person/${uuid}`;
const person = DataFactory.namedNode(personIRI);
```

### Generate IRIs

Create IRIs with base + local name:

```javascript
import { generateIRI, isValidIRI } from 'unrdf/utils';

// Generate IRI
const base = 'http://example.org/';
const id = 'alice';
const iri = generateIRI(base, id);

console.log(iri);
// → 'http://example.org/alice'

// Validate before use
if (isValidIRI(iri)) {
  const node = DataFactory.namedNode(iri);
}

// Generate multiple IRIs
const ids = ['alice', 'bob', 'charlie'];
const iris = ids.map(id => generateIRI(base, id));
// → ['http://example.org/alice', 'http://example.org/bob', ...]
```

### Content-Based IDs (Hashing)

Generate deterministic IDs from content:

```javascript
import { getCanonicalHash } from 'unrdf';
import crypto from 'crypto';

// Hash RDF content
const store = parseTurtle(`
  @prefix ex: <http://example.org/> .
  ex:alice a ex:Person ;
    ex:name "Alice" .
`);

// Generate content hash
const hash = await getCanonicalHash(store);
console.log(`Content hash: ${hash}`);
// → SHA-256 hash of canonical form

// Use hash as IRI
const contentIRI = `http://example.org/graph/${hash}`;

// Hash arbitrary data
function hashData(data) {
  return crypto
    .createHash('sha256')
    .update(JSON.stringify(data))
    .digest('hex');
}

const dataHash = hashData({ name: 'Alice', age: 30 });
const dataIRI = `http://example.org/entity/${dataHash}`;
```

### Deterministic IDs (Seeded)

Generate reproducible IDs with seeds:

```javascript
import crypto from 'crypto';

class DeterministicIDGenerator {
  constructor(seed) {
    this.seed = seed;
    this.counter = 0;
  }

  generate(prefix = 'http://example.org/') {
    const input = `${this.seed}:${this.counter++}`;
    const hash = crypto
      .createHash('sha256')
      .update(input)
      .digest('hex')
      .substring(0, 16);

    return `${prefix}${hash}`;
  }

  reset() {
    this.counter = 0;
  }
}

// Create generator with seed
const idGen = new DeterministicIDGenerator('my-seed-123');

// Generate IDs
const id1 = idGen.generate();
const id2 = idGen.generate();

// Reset and generate again - same sequence
idGen.reset();
const id3 = idGen.generate();

console.log(id1 === id3);  // true (deterministic)
```

### Namespace-Based IDs

Combine namespaces with ID generation:

```javascript
import { createNamespaceManager, generateIRI, generateUUID } from 'unrdf/utils';

const ns = createNamespaceManager();
ns.register('ex', 'http://example.org/');
ns.register('person', 'http://example.org/person/');

// Generate namespaced IRI
const personBase = ns.getNamespaceURI('person');
const personId = generateUUID();
const personIRI = generateIRI(personBase, personId);

console.log(personIRI);
// → http://example.org/person/550e8400-e29b-41d4-a716-446655440000

// Compact back to prefixed form
const prefixed = ns.compact(personIRI);
// → person:550e8400-e29b-41d4-a716-446655440000
```

### Blank Node Management

Create scoped blank nodes:

```javascript
class BlankNodeScope {
  constructor(prefix = '_:bn') {
    this.prefix = prefix;
    this.counter = 0;
    this.mapping = new Map();
  }

  create(label = null) {
    if (label && this.mapping.has(label)) {
      return this.mapping.get(label);
    }

    const id = `${this.prefix}${this.counter++}`;
    const bnode = DataFactory.blankNode(id);

    if (label) {
      this.mapping.set(label, bnode);
    }

    return bnode;
  }

  get(label) {
    return this.mapping.get(label);
  }
}

// Use scoped blank nodes
const scope = new BlankNodeScope('_:myapp');

const person = scope.create('person');
const address = scope.create('address');

// Retrieve by label
const samePerson = scope.get('person');
console.log(person === samePerson);  // true
```

### IRI Validation

Validate generated IRIs:

```javascript
import { isValidIRI } from 'unrdf/utils';

function safeGenerateIRI(base, localName) {
  // Sanitize local name
  const sanitized = localName
    .replace(/[^a-zA-Z0-9_-]/g, '_')
    .replace(/^_+/, '')
    .replace(/_+$/, '');

  const iri = generateIRI(base, sanitized);

  if (!isValidIRI(iri)) {
    throw new Error(`Generated invalid IRI: ${iri}`);
  }

  return iri;
}

// Safe IRI generation
const iri1 = safeGenerateIRI('http://example.org/', 'alice@example.com');
// → http://example.org/alice_example_com

const iri2 = safeGenerateIRI('http://example.org/', 'user/123');
// → http://example.org/user_123
```

## Variations

### Slug-Based IDs

Generate human-readable slugs:

```javascript
function slugify(text) {
  return text
    .toLowerCase()
    .replace(/[^\w\s-]/g, '')
    .replace(/\s+/g, '-')
    .replace(/-+/g, '-')
    .trim();
}

function generateSlugIRI(base, text) {
  const slug = slugify(text);
  return generateIRI(base, slug);
}

// Human-readable IRIs
const iri = generateSlugIRI('http://example.org/article/', 'How to Use UNRDF');
// → http://example.org/article/how-to-use-unrdf
```

### Collision Detection

Detect and handle ID collisions:

```javascript
class UniqueIDGenerator {
  constructor(base) {
    this.base = base;
    this.used = new Set();
  }

  generate(preferred) {
    let candidate = generateIRI(this.base, preferred);
    let counter = 1;

    while (this.used.has(candidate)) {
      candidate = generateIRI(this.base, `${preferred}-${counter++}`);
    }

    this.used.add(candidate);
    return candidate;
  }
}

const gen = new UniqueIDGenerator('http://example.org/');
const id1 = gen.generate('alice');  // http://example.org/alice
const id2 = gen.generate('alice');  // http://example.org/alice-1
const id3 = gen.generate('alice');  // http://example.org/alice-2
```

### Hierarchical IDs

Create hierarchical identifier structures:

```javascript
class HierarchicalIDGenerator {
  constructor(base) {
    this.base = base;
  }

  generate(...segments) {
    const path = segments.map(s => encodeURIComponent(s)).join('/');
    return `${this.base}${path}`;
  }
}

const gen = new HierarchicalIDGenerator('http://example.org/');

const orgIRI = gen.generate('org', 'acme');
// → http://example.org/org/acme

const personIRI = gen.generate('org', 'acme', 'employee', 'alice');
// → http://example.org/org/acme/employee/alice
```

### Versioned IDs

Add versioning to IRIs:

```javascript
function generateVersionedIRI(base, id, version) {
  return `${generateIRI(base, id)}?version=${version}`;
}

// Create versioned resources
const v1 = generateVersionedIRI('http://example.org/', 'alice', 1);
// → http://example.org/alice?version=1

const v2 = generateVersionedIRI('http://example.org/', 'alice', 2);
// → http://example.org/alice?version=2

// Parse version from IRI
function getVersionFromIRI(iri) {
  const url = new URL(iri);
  return parseInt(url.searchParams.get('version')) || 1;
}
```

### Batch ID Generation

Generate IDs efficiently in bulk:

```javascript
function batchGenerateIRIs(base, count) {
  const ids = [];

  for (let i = 0; i < count; i++) {
    const uuid = generateUUID();
    ids.push(generateIRI(base, uuid));
  }

  return ids;
}

// Generate 1000 IRIs
const iris = batchGenerateIRIs('http://example.org/entity/', 1000);

// Use in bulk creation
iris.forEach(iri => {
  const entity = DataFactory.namedNode(iri);
  store.addQuad(entity, rdf.type, schema.Thing);
});
```

## Related Guides

- [How-To: Manage Namespaces](./manage-namespaces.md) - Namespace utilities
- [How-To: Parse RDF Formats](./parse-rdf-formats.md) - Create terms with IDs
- [Reference: Utilities](../reference/api/utilities.md) - Full ID utilities API
- [Explanation: RDF Terms](../explanation/concepts/rdf-terms.md) - Term types and identification
