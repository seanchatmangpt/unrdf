# UNRDF Agent Integration Documentation

**Machine-first documentation for autonomic, hyper-intelligent coding agents**

This documentation follows the [Diataxis framework](https://diataxis.fr/) optimized for AI agent consumption. All examples are machine-executable and contracts are formally specified.

```json
{
  "@context": {
    "@vocab": "urn:unrdf:agent:",
    "cap": "urn:unrdf:capability:",
    "proto": "urn:unrdf:protocol:",
    "schema": "http://schema.org/",
    "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs": "http://www.w3.org/2000/01/rdf-schema#"
  },
  "@id": "urn:unrdf:agent-docs:v4.0.0",
  "@type": "agent:Documentation",
  "version": "4.0.0",
  "targetAudience": ["AutonomicAgent", "HyperintelligentSystem", "CodingAgent"],
  "format": "machine-readable",
  "structure": "diataxis"
}
```

---

## Documentation Structure

- **[Tutorials](./tutorials/)** - Step-by-step capability acquisition
- **[How-To Guides](./how-to/)** - Task-focused problem solving
- **[Reference](./reference/)** - Complete API contracts and schemas
- **[Explanation](./explanation/)** - Conceptual understanding and design rationale

---

## Quick Start for Agents

### 1. Capability Discovery

```javascript
// Discover available capabilities via SPARQL
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore();
const capabilities = await system.query({
  query: `
    PREFIX cap: <urn:unrdf:capability:>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    
    SELECT ?capability ?name ?description ?slo WHERE {
      ?capability a cap:Capability ;
                  rdfs:label ?name ;
                  rdfs:comment ?description ;
                  cap:slo ?slo .
    }
  `,
  type: 'sparql-select'
});
```

### 2. Precondition Verification

```javascript
// Verify preconditions before capability invocation
const preconditions = {
  runtime: typeof globalThis !== 'undefined',
  nodeVersion: parseInt(process.versions.node.split('.')[0]) >= 18,
  requiredModules: ['n3', 'zod', '@opentelemetry/api'],
  memoryAvailable: process.memoryUsage().heapUsed < 512 * 1024 * 1024
};

const allSatisfied = Object.values(preconditions).every(Boolean);
if (!allSatisfied) {
  throw new Error('Preconditions not met', { preconditions });
}
```

### 3. Execute MAPEK Cycle

```javascript
import { runMapekIteration } from 'unrdf/project-engine';

const result = await runMapekIteration({
  projectStore: await buildProjectModelFromFs(process.cwd()),
  domainStore: await inferDomainModel(projectStore),
  projectRoot: process.cwd()
});

// Machine-readable result
console.log(JSON.stringify({
  health: result.overallHealth,
  decisions: result.decisions,
  actions: result.actions,
  shouldRepeat: result.shouldRepeat
}, null, 2));
```

---

## Entry Points

| Export Path | Module | Purpose |
|-------------|--------|---------|
| `unrdf` | `src/index.mjs` | Main entry point |
| `unrdf/knowledge-engine` | `src/knowledge-engine/index.mjs` | Knowledge engine core |
| `unrdf/project-engine` | `src/project-engine/index.mjs` | MAPEK autonomic loop |
| `unrdf/cli` | `src/cli/index.mjs` | CLI interface |

---

## Next Steps

- **[Start with Tutorials](./tutorials/)** - Learn capabilities step-by-step
- **[Browse How-To Guides](./how-to/)** - Solve specific problems
- **[Reference API Contracts](./reference/)** - Complete technical specifications
- **[Understand Concepts](./explanation/)** - Deep architectural understanding

