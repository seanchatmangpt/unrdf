# How To: Work With Different RDF Formats

**Time estimate:** 5-7 hours
**Difficulty:** Intermediate
**Context:** You have RDF data in different formats and need to work with them

---

## Overview

RDF data exists in many formats. This guide covers:
1. Understanding different formats
2. Converting between formats
3. Parsing RDF from different sources
4. Serializing data to different formats
5. Choosing the right format

---

## Common RDF Formats

| Format | Extension | Use Case | Readability |
|--------|-----------|----------|-------------|
| **Turtle** | .ttl | Human-readable, development | Excellent |
| **N-Triples** | .nt | Simple, one statement per line | Good |
| **N-Quads** | .nq | Named graphs, one statement per line | Good |
| **RDF/XML** | .rdf | Legacy systems, compatibility | Poor |
| **JSON-LD** | .jsonld | Web APIs, JavaScript | Good |
| **TriG** | .trig | Turtle + named graphs | Excellent |

### Format Characteristics

**Turtle** - Best for most cases
```turtle
@prefix ex: <http://example.com/> .

ex:alice a foaf:Person ;
  foaf:name "Alice" ;
  foaf:knows ex:bob .
```

**N-Triples** - Simple, one triple per line
```
<http://example.com/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
<http://example.com/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.com/bob> .
```

**N-Quads** - Includes named graphs
```
<http://example.com/alice> <http://xmlns.com/foaf/0.1/name> "Alice" <http://example.com/graph1> .
```

**JSON-LD** - Linked Data in JSON
```json
{
  "@context": "http://xmlns.com/foaf/0.1/",
  "@id": "http://example.com/alice",
  "@type": "Person",
  "name": "Alice",
  "knows": { "@id": "http://example.com/bob" }
}
```

---

## Loading Data From Different Formats

### From Turtle Files

```javascript
import { createUnrdfStore } from '@unrdf/core';
import fs from 'fs';

// Read Turtle file
const turtleText = fs.readFileSync('data.ttl', 'utf-8');

// Parse and load
const store = createUnrdfStore();
// (Parsing requires a parser library)
// quads would come from parsing
quads.forEach(quad => store.addQuad(quad));

console.log(`Loaded ${store.size} triples`);
```

### From N-Triples

N-Triples is simple - one triple per line:

```javascript
function parseNTriples(text) {
  const quads = [];

  text.split('\n').forEach(line => {
    if (!line.trim()) return;  // Skip empty lines

    // Match: <subject> <predicate> <object> .
    const match = line.match(
      /^<(.+?)>\s+<(.+?)>\s+(.+?)\s+\.$/)
    ;

    if (match) {
      const [, subject, predicate, object] = match;

      quads.push({
        subject: { type: 'NamedNode', value: subject },
        predicate: { type: 'NamedNode', value: predicate },
        object: parseObject(object)
      });
    }
  });

  return quads;
}

function parseObject(str) {
  if (str.startsWith('<') && str.endsWith('>')) {
    return { type: 'NamedNode', value: str.slice(1, -1) };
  }
  if (str.startsWith('"')) {
    // Literal
    return { type: 'Literal', value: str.slice(1, -1) };
  }
  return null;
}

// Usage
const nTriplesText = `
<http://example.com/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
<http://example.com/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .
`;

const quads = parseNTriples(nTriplesText);
store.addQuads(quads);
```

### From JSON-LD

JSON-LD requires expansion to RDF:

```javascript
// JSON-LD document
const jsonld = {
  "@context": {
    "foaf": "http://xmlns.com/foaf/0.1/",
    "name": "foaf:name"
  },
  "@id": "http://example.com/alice",
  "@type": "foaf:Person",
  "name": "Alice"
};

// Convert to RDF (use jsonld library)
// const quads = await JsonLD.toRDF(jsonld);
// store.addQuads(quads);
```

---

## Converting Between Formats

### Using @unrdf/core Functions

```javascript
import { toNTriples, canonicalize } from '@unrdf/core';

// Get all quads from store
const quads = Array.from(store.match());

// Convert to N-Triples string
const nTriplesStr = toNTriples(quads);
console.log(nTriplesStr);

// Canonicalize (standardize) quads
const canonical = canonicalize(quads);
```

### Manual Conversion

**RDF to N-Triples:**

```javascript
function toNTriples(quads) {
  return quads.map(quad => {
    const subject = termToString(quad.subject);
    const predicate = termToString(quad.predicate);
    const object = termToString(quad.object);

    return `${subject} ${predicate} ${object} .`;
  }).join('\n');
}

function termToString(term) {
  if (term.type === 'NamedNode') {
    return `<${term.value}>`;
  }
  if (term.type === 'Literal') {
    return `"${term.value}"`;
  }
  if (term.type === 'BlankNode') {
    return `_:${term.value}`;
  }
  return '';
}

// Usage
const nTriplesString = toNTriples(quads);
fs.writeFileSync('output.nt', nTriplesString);
```

**N-Triples to Turtle (with prefixes):**

```javascript
function toTurtle(quads) {
  // Find all namespaces
  const namespaces = new Map();

  quads.forEach(quad => {
    extractNamespace(quad.subject, namespaces);
    extractNamespace(quad.predicate, namespaces);
    extractNamespace(quad.object, namespaces);
  });

  // Generate prefix declarations
  let ttl = '';
  for (const [prefix, ns] of namespaces) {
    ttl += `@prefix ${prefix}: <${ns}> .\n`;
  }
  ttl += '\n';

  // Generate triples with abbreviated URIs
  quads.forEach(quad => {
    ttl += `${abbreviate(quad.subject, namespaces)} `;
    ttl += `${abbreviate(quad.predicate, namespaces)} `;
    ttl += `${abbreviate(quad.object, namespaces)} .\n`;
  });

  return ttl;
}

function extractNamespace(term, map) {
  if (term.type !== 'NamedNode') return;

  const value = term.value;
  const lastHash = value.lastIndexOf('#');
  const lastSlash = value.lastIndexOf('/');
  const splitPoint = Math.max(lastHash, lastSlash);

  if (splitPoint > 0) {
    const ns = value.substring(0, splitPoint + 1);
    const local = value.substring(splitPoint + 1);

    const prefix = ns.includes('foaf') ? 'foaf' :
                   ns.includes('example') ? 'ex' :
                   `ns${map.size}`;

    if (!map.has(prefix)) {
      map.set(prefix, ns);
    }
  }
}

function abbreviate(term, namespaces) {
  if (term.type === 'Literal') {
    return `"${term.value}"`;
  }
  if (term.type !== 'NamedNode') {
    return termToString(term);
  }

  for (const [prefix, ns] of namespaces) {
    if (term.value.startsWith(ns)) {
      const local = term.value.substring(ns.length);
      return `${prefix}:${local}`;
    }
  }

  return `<${term.value}>`;
}
```

---

## Choosing the Right Format

### Use Turtle For
- Human-readable development
- Configuration files
- Sharing with non-technical users
- General-purpose storage

### Use N-Triples For
- Simple, line-by-line processing
- Large files (simpler parsing)
- Archival (one triple per line)
- Streaming

### Use N-Quads For
- Multiple named graphs
- Federated data
- Provenance tracking
- Linked data with metadata

### Use JSON-LD For
- Web APIs
- JavaScript applications
- Embedding in JSON
- REST endpoints

### Use RDF/XML For
- Legacy systems
- When required by standards
- Publishing (some platforms require it)

---

## Working With Large Files

### Streaming Large Files

For multi-gigabyte files, process line-by-line:

```javascript
import { createReadStream } from 'fs';
import { createUnrdfStore } from '@unrdf/core';

const store = createUnrdfStore();
let lineCount = 0;

createReadStream('large.nt')
  .on('data', chunk => {
    const lines = chunk.toString().split('\n');

    lines.forEach(line => {
      if (!line.trim()) return;

      const quad = parseNTriplesLine(line);
      if (quad) {
        store.addQuad(quad);
        lineCount++;

        // Log progress
        if (lineCount % 100000 === 0) {
          console.log(`Loaded ${lineCount} triples...`);
        }
      }
    });
  })
  .on('end', () => {
    console.log(`Finished loading ${lineCount} triples`);
  });
```

### Batch Processing

```javascript
function processBatches(quads, batchSize = 1000) {
  for (let i = 0; i < quads.length; i += batchSize) {
    const batch = quads.slice(i, i + batchSize);
    processBatch(batch);

    console.log(`Processed ${Math.min(i + batchSize, quads.length)} quads`);
  }
}

function processBatch(quads) {
  quads.forEach(quad => {
    // Process each quad
    store.addQuad(quad);
  });
}
```

---

## Format Detection

Automatically detect format from content:

```javascript
function detectFormat(text) {
  text = text.trim();

  // Turtle/TriG: starts with @prefix or @base
  if (text.startsWith('@')) return 'turtle';

  // JSON-LD: valid JSON starting with {
  if (text.startsWith('{')) {
    try {
      JSON.parse(text);
      return 'jsonld';
    } catch (e) {
      // Not JSON
    }
  }

  // RDF/XML: starts with <?xml
  if (text.startsWith('<?xml')) return 'rdfxml';

  // N-Triples/N-Quads: lines with <subject> <predicate> <object> .
  const lines = text.split('\n');
  if (lines[0]?.match(/^<.*>\s+<.*>\s+/)) {
    return lines[0]?.split('"').length > 3 ? 'nquads' : 'ntriples';
  }

  return 'unknown';
}

// Usage
const format = detectFormat(fileContent);
console.log(`Detected format: ${format}`);
```

---

## Validating RDF

Check for valid RDF before loading:

```javascript
function validateRDF(quads) {
  const errors = [];

  quads.forEach((quad, index) => {
    // Subject must be NamedNode or BlankNode
    if (!['NamedNode', 'BlankNode'].includes(quad.subject.type)) {
      errors.push(`Quad ${index}: Invalid subject type`);
    }

    // Predicate must be NamedNode
    if (quad.predicate.type !== 'NamedNode') {
      errors.push(`Quad ${index}: Predicate must be NamedNode`);
    }

    // Object can be any term
    // Graph can be NamedNode, BlankNode, or DefaultGraph

    // Validate IRIs
    if (quad.subject.type === 'NamedNode') {
      if (!isValidIRI(quad.subject.value)) {
        errors.push(`Quad ${index}: Invalid subject IRI`);
      }
    }

    if (!isValidIRI(quad.predicate.value)) {
      errors.push(`Quad ${index}: Invalid predicate IRI`);
    }
  });

  return errors;
}

function isValidIRI(iri) {
  try {
    new URL(iri);
    return true;
  } catch (e) {
    return false;
  }
}

// Usage
const errors = validateRDF(quads);
if (errors.length) {
  console.error('RDF validation errors:');
  errors.forEach(err => console.error(`  - ${err}`));
} else {
  console.log('RDF is valid');
}
```

---

## Summary

Key techniques:
1. **Choose format by use case** - Turtle for humans, N-Triples for processing
2. **Parse carefully** - Escape sequences, data types, IRIs
3. **Convert systematically** - Map between format representations
4. **Stream large files** - Don't load everything in memory
5. **Validate before loading** - Check for RDF conformance

---

## Next Reading

- **CONFIGURATION** (Reference) - How to configure format handling
- **working-with-formats** (How-To) - Work with APIs that expect specific formats
- **architecture** (Explanation) - Understand RDF format design
