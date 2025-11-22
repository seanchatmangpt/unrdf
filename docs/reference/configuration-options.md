# Configuration Options

Complete reference for UNRDF configuration.

## Knowledge Engine Configuration

### createKnowledgeEngine Options

```javascript
import { createKnowledgeEngine } from 'unrdf/knowledge-engine';

const engine = createKnowledgeEngine({
  baseIRI: 'http://example.org/',
  strictMode: true,
  maxHooks: 100
});
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `baseIRI` | `string` | `'http://example.org/'` | Base IRI for relative IRIs during parsing |
| `strictMode` | `boolean` | `false` | Enable strict validation mode |
| `maxHooks` | `number` | `100` | Maximum number of transaction hooks |

---

## Parsing Options

### parseTurtle Options

```javascript
const store = await parseTurtle(data, 'http://base.example.org/');
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `baseIRI` | `string` | `''` | Base IRI for relative IRIs |

### parseJsonLd Options

```javascript
const store = await parseJsonLd(doc, {
  baseIRI: 'http://example.org/',
  expandContext: context
});
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `baseIRI` | `string` | `''` | Base IRI for document |
| `expandContext` | `object` | `undefined` | Context for expansion |

---

## Serialization Options

### toTurtle Options

```javascript
const turtle = await toTurtle(store, {
  prefixes: {
    foaf: 'http://xmlns.com/foaf/0.1/',
    ex: 'http://example.org/'
  }
});
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `prefixes` | `object` | `{}` | Prefix mappings for compact output |

### toJsonLd Options

```javascript
const jsonld = await toJsonLd(store, {
  context: {
    foaf: 'http://xmlns.com/foaf/0.1/',
    name: 'foaf:name'
  },
  compact: true
});
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `context` | `object` | `{}` | JSON-LD context for compaction |
| `compact` | `boolean` | `true` | Compact output using context |

---

## Query Options

### Query Execution Options

```javascript
const results = await query(store, sparql, {
  timeout: 30000,
  baseIRI: 'http://example.org/'
});
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `timeout` | `number` | `60000` | Query timeout in milliseconds |
| `baseIRI` | `string` | `''` | Base IRI for query |

---

## Validation Options

### validateShacl Options

```javascript
const report = await validateShacl(store, shapes, {
  maxErrors: 100
});
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `maxErrors` | `number` | `Infinity` | Maximum errors to report |

---

## Reasoning Options

### reason Options

```javascript
const reasoned = await reason(store, rules, {
  maxIterations: 100
});
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `maxIterations` | `number` | `100` | Maximum reasoning iterations |

---

## Canonicalization Options

### canonicalize Options

```javascript
const canonical = await canonicalize(store, {
  algorithm: 'URDNA2015'
});
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `algorithm` | `string` | `'URDNA2015'` | Canonicalization algorithm |

---

## Knowledge Hook Configuration

### Hook Definition Options

```javascript
const hook = defineHook({
  meta: {
    name: 'namespace:hook-name',
    description: 'Hook description',
    ontology: ['foaf', 'prov']
  },
  channel: {
    graphs: ['urn:graph:production'],
    view: 'delta'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/condition.rq',
      sha256: '...',
      mediaType: 'application/sparql-query'
    }
  },
  determinism: { seed: 42 },
  receipt: { anchor: 'git-notes' }
});
```

### meta

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `name` | `string` | Yes | Unique hook identifier (namespace:name) |
| `description` | `string` | No | Human-readable description |
| `ontology` | `string[]` | No | Related ontology prefixes |

### channel

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `graphs` | `string[]` | `[]` | Named graphs to observe |
| `view` | `string` | `'after'` | Graph state to evaluate (`'before'`, `'after'`, `'delta'`) |

### when

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `kind` | `string` | Yes | Condition type (`'sparql-ask'`, `'sparql-select'`, `'shacl'`) |
| `ref.uri` | `string` | Yes | Condition file URI |
| `ref.sha256` | `string` | Yes | SHA-256 hash of file contents |
| `ref.mediaType` | `string` | Yes | MIME type of file |

### determinism

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `seed` | `number` | `42` | Seed for deterministic operations |

### receipt

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `anchor` | `string` | `'none'` | Receipt anchoring strategy (`'git-notes'`, `'none'`) |

---

## React Hooks Configuration

### useKnowledgeEngine Options

```javascript
const { store } = useKnowledgeEngine({
  initialData: turtleString,
  baseIRI: 'http://example.org/'
});
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `initialData` | `string` | `''` | Initial Turtle data |
| `baseIRI` | `string` | `''` | Base IRI for parsing |

### useChangeFeed Options

```javascript
const changes = useChangeFeed(store, {
  debounce: 100,
  maxHistory: 50
});
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `debounce` | `number` | `100` | Debounce interval (ms) |
| `maxHistory` | `number` | `100` | Maximum changes to track |

---

## Environment Variables

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `UNRDF_BASE_IRI` | `string` | `''` | Default base IRI |
| `UNRDF_STRICT_MODE` | `boolean` | `false` | Enable strict mode globally |
| `UNRDF_LOG_LEVEL` | `string` | `'info'` | Log level (debug, info, warn, error) |
| `DEBUG` | `string` | `''` | Debug namespaces (e.g., `'unrdf:*'`) |

---

## Configuration Files

### unrdf.config.mjs

```javascript
// unrdf.config.mjs
export default {
  engine: {
    baseIRI: 'http://example.org/',
    strictMode: process.env.NODE_ENV === 'production'
  },
  query: {
    timeout: 30000
  },
  validation: {
    maxErrors: 100
  }
};
```

### .unrdfrc.json

```json
{
  "engine": {
    "baseIRI": "http://example.org/",
    "strictMode": false
  },
  "query": {
    "timeout": 30000
  },
  "validation": {
    "maxErrors": 100
  }
}
```

---

## Related

- [API Reference](./api-reference.md) - Full API documentation
- [CLI Reference](./cli-reference.md) - Command-line options
- [Getting Started](../GETTING_STARTED.md) - Quick setup guide
