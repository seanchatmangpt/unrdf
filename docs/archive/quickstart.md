# UNRDF v3 Quickstart Guide

Get started with UNRDF in 5 minutes. Learn the basics of Knowledge Hooks, CLI usage, and RDF operations.

## Installation

```bash
# Using pnpm (recommended)
pnpm add unrdf

# Using npm
npm install unrdf

# Using yarn
yarn add unrdf
```

### System Requirements

- **Node.js**: >=18.0.0
- **Package Manager**: pnpm >=7.0.0 (required)
- **Git**: For lockchain audit trails (optional)

## Quick Start (5 Minutes)

### 1. Initialize a New Project

```bash
# Create project directory
mkdir my-unrdf-app
cd my-unrdf-app

# Initialize package.json
pnpm init

# Install UNRDF
pnpm add unrdf

# Create config file
cat > unrdf.config.mjs << 'EOF'
export default {
  baseIRI: 'http://example.org/',
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
};
EOF
```

### 2. Create Your First Knowledge Hook

Create `hooks/health-check.mjs`:

```javascript
import { defineHook } from 'unrdf';

export default defineHook({
  meta: {
    name: 'system:health-check',
    description: 'Monitor system health'
  },
  channel: {
    graphs: ['urn:graph:system'],
    view: 'delta'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/health-check.ask.rq',
      sha256: 'abc123...',
      mediaType: 'application/sparql-query'
    }
  },

  async run({ payload, context }) {
    console.log('Health check triggered!');
    return {
      result: { status: 'healthy', timestamp: Date.now() }
    };
  }
});
```

Create `hooks/health-check.ask.rq`:

```sparql
PREFIX ex: <http://example.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

ASK {
  ?service rdf:type ex:Service .
  ?service ex:status ?status .
  FILTER(?status != "healthy")
}
```

### 3. Use the CLI

```bash
# Parse RDF data
unrdf parse data.ttl

# Query with SPARQL
unrdf query select "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# Validate against SHACL shapes
unrdf validate --shapes shapes.ttl data.ttl

# Evaluate a knowledge hook
unrdf hook eval hooks/health-check.mjs --data data.ttl
```

### 4. Programmatic Usage

Create `app.mjs`:

```javascript
import { initStore, useGraph, defineHook } from 'unrdf';

// Initialize store context
const runApp = initStore();

await runApp(async () => {
  // Use graph composable
  const graph = useGraph();

  // Add triples
  await graph.update(`
    PREFIX ex: <http://example.org/>
    INSERT DATA {
      ex:Alice a ex:Person ;
        ex:name "Alice Smith" ;
        ex:email "alice@example.org" .
    }
  `);

  // Query data
  const results = graph.select(`
    PREFIX ex: <http://example.org/>
    SELECT ?name ?email WHERE {
      ?person a ex:Person ;
        ex:name ?name ;
        ex:email ?email .
    }
  `);

  console.log('Results:', results);

  // Validate with SHACL
  const validation = graph.validate(`
    PREFIX ex: <http://example.org/>
    PREFIX sh: <http://www.w3.org/ns/shacl#>

    ex:PersonShape a sh:NodeShape ;
      sh:targetClass ex:Person ;
      sh:property [
        sh:path ex:name ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
      ] .
  `);

  console.log('Valid:', validation.conforms);
});
```

Run it:

```bash
node app.mjs
```

## Core Concepts

### Knowledge Hooks

Knowledge Hooks are reactive intelligence for your RDF graphs. They trigger autonomically when conditions are met.

**Six Hook Types:**

1. **ASK** - Boolean queries (is condition true?)
2. **SHACL** - Shape validation (does data conform?)
3. **DELTA** - Change detection (what changed?)
4. **THRESHOLD** - Numeric limits (exceed threshold?)
5. **COUNT** - Quantity checks (how many?)
6. **WINDOW** - Time-based patterns (within timeframe?)

### Policy Packs

Policy packs are versioned governance units that bundle hooks, shapes, and rules.

```javascript
import { createPolicyPackManifest } from 'unrdf';

const policyPack = createPolicyPackManifest({
  name: 'compliance-v1',
  version: '1.0.0',
  description: 'Enterprise compliance policies',
  hooks: [
    './hooks/large-transaction.mjs',
    './hooks/data-quality.mjs'
  ],
  shapes: [
    './shapes/transaction.ttl',
    './shapes/customer.ttl'
  ]
});
```

### Lockchain Audit Trails

UNRDF provides cryptographic audit trails for all transactions:

```javascript
import { KnowledgeHookManager } from 'unrdf';

const manager = new KnowledgeHookManager({
  receipt: { anchor: 'git-notes' }  // Git-anchored receipts
});

// Apply transaction with audit trail
const result = await manager.apply(store, {
  additions: [/* quads */],
  removals: [/* quads */]
});

console.log('Receipt:', result.receipt);
// { committed: true, hash: 'sha256:...', timestamp: ... }
```

## Common Use Cases

### 1. Data Quality Monitoring

```javascript
defineHook({
  meta: { name: 'quality:check-completeness' },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/incomplete-records.ask.rq',
      sha256: '...',
      mediaType: 'application/sparql-query'
    }
  },
  async run({ payload }) {
    // Alert on incomplete records
    console.warn('Found incomplete records!');
    return { result: { action: 'alert-sent' } };
  }
});
```

### 2. Compliance Enforcement

```javascript
defineHook({
  meta: { name: 'compliance:gdpr' },
  when: {
    kind: 'shacl',
    ref: {
      uri: 'file://shapes/gdpr-compliance.ttl',
      sha256: '...',
      mediaType: 'text/shacl'
    }
  },
  async before({ payload }) {
    // Gate transaction on GDPR compliance
    if (!payload.hasConsent) {
      return { cancel: true, reason: 'Missing consent' };
    }
  }
});
```

### 3. Real-Time Analytics

```javascript
defineHook({
  meta: { name: 'analytics:revenue' },
  channel: { view: 'delta' },
  when: {
    kind: 'sparql-select',
    ref: {
      uri: 'file://queries/revenue-changes.rq',
      sha256: '...',
      mediaType: 'application/sparql-query'
    }
  },
  async run({ payload }) {
    // Calculate revenue impact
    const impact = payload.results.reduce((sum, r) =>
      sum + parseFloat(r.amount.value), 0
    );
    return { result: { revenueImpact: impact } };
  }
});
```

## CLI Reference (Quick)

```bash
# Graph Operations
unrdf graph validate data.ttl --shapes shapes.ttl
unrdf graph export data.ttl --format jsonld
unrdf graph import data.jsonld

# Hook Management
unrdf hook list
unrdf hook eval health-check.mjs
unrdf hook test compliance.mjs --dry-run

# Policy Packs
unrdf policy apply compliance-v1.json
unrdf policy list --active
unrdf policy rollback compliance-v1


**Kubernetes Deployment:**

```yaml
apiVersion: v1
kind: Pod
spec:
  containers:
  - name: app
    image: my-app:latest
    env:
      value: "localhost:50051"
  - name: knowledge-engine
    image: unrdf/knowledge-engine:latest
    ports:
    - containerPort: 50051
```

## Troubleshooting

### Hook Not Triggering

**Problem**: Knowledge hook doesn't execute

**Solutions**:
1. Check condition file exists: `ls -la hooks/health-check.ask.rq`
2. Verify SHA256 hash: `sha256sum hooks/health-check.ask.rq`
3. Enable debug mode: `DEBUG=unrdf:* node app.mjs`

### SPARQL Query Fails

**Problem**: Query returns empty results

**Solutions**:
1. Check store has data: `graph.size > 0`
2. Verify prefixes match: `graph.stats()`
3. Test query syntax: `unrdf query ask "ASK { ?s ?p ?o }"`

### Validation Errors

**Problem**: SHACL validation fails unexpectedly

**Solutions**:
1. Check shape syntax: `unrdf parse shapes.ttl`
2. Inspect validation report: `validation.results`
3. Use strict mode: `graph.validateOrThrow(shapes)`

### Performance Issues

**Problem**: Slow hook execution

**Solutions**:
1. Check hook metrics: `manager.getStats()`
2. Enable query optimization: `enableQueryOptimization: true`
3. Use indexed queries: Add `FILTER` early in query

## Next Steps

1. **Read Full API Documentation**: See `docs/api/` for complete API reference
2. **Explore Examples**: Check `examples/` for production patterns
4. **Join Community**: GitHub issues for support and feature requests

## Resources

- **Documentation**: [/docs](/docs)
- **Examples**: [/examples](/examples)
- **API Reference**: [/docs/api](/docs/api)
- **GitHub**: https://github.com/unrdf/unrdf
- **Issues**: https://github.com/unrdf/unrdf/issues

---

**Got questions?** Open an issue on GitHub or check the [FAQ](/docs/FAQ.md).

**Ready for production?** See the [Deployment Guide](/docs/deployment) and [Architecture Overview](/docs/architecture/system-overview.md).
