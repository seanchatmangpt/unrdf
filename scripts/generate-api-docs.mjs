#!/usr/bin/env node
/**
 * Generate API Reference Documentation from Capability Maps
 *
 * Outputs:
 * - docs/capability-map/API-REFERENCE.md (master API doc)
 * - docs/capability-map/reference/*.md (individual package API pages)
 * - docs/capability-map/QUICK-REFERENCE.md (cheat sheet)
 * - docs/capability-map/openapi-schema.json (OpenAPI 3.0 schema)
 */

import { readFileSync, writeFileSync, readdirSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT = join(__dirname, '..');

// Load capability map JSON
const capabilityMapPath = join(ROOT, 'exploration', 'capability-map.json');
const capabilityMap = JSON.parse(readFileSync(capabilityMapPath, 'utf-8'));

// Categories mapping for organization
const CATEGORIES = {
  'RDF & Storage': ['@unrdf/core', '@unrdf/oxigraph', '@unrdf/caching', '@unrdf/collab', '@unrdf/graph-analytics', '@unrdf/semantic-search', '@unrdf/rdf-graphql'],
  'Governance & Policy': ['@unrdf/hooks', '@unrdf/validation', '@unrdf/v6-core', '@unrdf/engine-gateway', '@unrdf/decision-fabric'],
  'Temporal & Events': ['@unrdf/kgc-4d', '@unrdf/kgc-substrate', '@unrdf/kgc-claude', '@unrdf/kgc-probe', '@unrdf/kgc-runtime', '@unrdf/blockchain'],
  'Streaming & Distribution': ['@unrdf/streaming', '@unrdf/federation', '@unrdf/consensus', '@unrdf/yawl-kafka', '@unrdf/yawl-queue'],
  'Workflow & Orchestration': ['@unrdf/yawl', '@unrdf/yawl-api', '@unrdf/yawl-ai', '@unrdf/yawl-durable', '@unrdf/yawl-langchain', '@unrdf/yawl-observability', '@unrdf/yawl-realtime', '@unrdf/yawl-viz'],
  'AI & ML': ['@unrdf/knowledge-engine', '@unrdf/ml-inference', '@unrdf/ml-versioning', '@unrdf/react', '@unrdf/semantic-search'],
  'Infrastructure & Tools': ['@unrdf/cli', '@unrdf/kgc-cli', '@unrdf/kgc-docs', '@unrdf/kgn', '@unrdf/diataxis-kit', '@unrdf/observability', '@unrdf/atomvm', '@unrdf/serverless', '@unrdf/test-utils']
};

// Extract API info from capability map markdown files
function parseCapabilityMapMd(filePath) {
  try {
    const content = readFileSync(filePath, 'utf-8');
    const apis = [];

    // Extract capability atoms from tables
    const atomRegex = /\|\s*`([^`]+)`\s*\|\s*(\w+)\s*\|\s*\[([^\]]+)\]\(([^)]+)\)\s*\|/g;
    let match;

    while ((match = atomRegex.exec(content)) !== null) {
      const [_, signature, type, evidence, evidencePath] = match;
      apis.push({
        signature,
        type,
        evidence,
        evidencePath
      });
    }

    return apis;
  } catch (e) {
    return [];
  }
}

// Read existing capability map docs
const capMapDocs = {
  '@unrdf/core': parseCapabilityMapMd(join(ROOT, 'docs/capability-map/core.md')),
  '@unrdf/oxigraph': parseCapabilityMapMd(join(ROOT, 'docs/capability-map/oxigraph.md'))
};

// Generate OpenAPI 3.0 schema
function generateOpenAPISchema() {
  const schema = {
    openapi: '3.0.0',
    info: {
      title: 'UNRDF API Reference',
      version: '6.0.0',
      description: 'Complete API reference for all UNRDF packages - RDF processing, SPARQL execution, workflow orchestration, and knowledge graph management',
      contact: {
        name: 'UNRDF Team',
        url: 'https://github.com/seanchatmangpt/unrdf'
      },
      license: {
        name: 'MIT',
        url: 'https://opensource.org/licenses/MIT'
      }
    },
    servers: [
      {
        url: 'https://api.unrdf.io',
        description: 'Production API server'
      },
      {
        url: 'http://localhost:3000',
        description: 'Local development server'
      }
    ],
    tags: Object.keys(CATEGORIES).map(category => ({
      name: category,
      description: `${category} packages and APIs`
    })),
    paths: {},
    components: {
      schemas: {},
      securitySchemes: {
        bearerAuth: {
          type: 'http',
          scheme: 'bearer',
          bearerFormat: 'JWT'
        },
        apiKey: {
          type: 'apiKey',
          in: 'header',
          name: 'X-API-Key'
        }
      }
    }
  };

  // Add package schemas and paths
  for (const pkg of capabilityMap.packages) {
    const category = Object.entries(CATEGORIES).find(([_, pkgs]) =>
      pkgs.includes(pkg.name)
    )?.[0] || 'Infrastructure & Tools';

    // Add schema for package
    const schemaName = pkg.name.replace('@unrdf/', '').replace(/-/g, '_');
    schema.components.schemas[schemaName] = {
      type: 'object',
      description: pkg.description,
      properties: {
        name: { type: 'string', example: pkg.name },
        version: { type: 'string', example: pkg.version },
        description: { type: 'string', example: pkg.description }
      }
    };

    // Add basic info endpoint for each package
    const pathName = `/packages/${pkg.name.replace('@unrdf/', '')}`;
    schema.paths[pathName] = {
      get: {
        tags: [category],
        summary: `Get ${pkg.name} package information`,
        description: pkg.description,
        operationId: `get${schemaName}`,
        responses: {
          '200': {
            description: 'Successful response',
            content: {
              'application/json': {
                schema: {
                  $ref: `#/components/schemas/${schemaName}`
                }
              }
            }
          },
          '404': {
            description: 'Package not found'
          }
        }
      }
    };
  }

  return schema;
}

// Generate master API reference
function generateMasterAPIReference() {
  let md = `# UNRDF API Reference

**Version**: 6.0.0
**Last Updated**: ${new Date().toISOString().split('T')[0]}
**Total Packages**: ${capabilityMap.totalPackages}

---

## Overview

Complete API reference for all UNRDF packages. APIs are organized by use case and package category for easy navigation.

**Quick Links**:
- [OpenAPI Schema](./openapi-schema.json) - Machine-readable API specification
- [Quick Reference](./QUICK-REFERENCE.md) - One-page cheat sheet
- [Individual Package APIs](./reference/) - Detailed package documentation

---

## Package Categories

`;

  // Generate category sections
  for (const [category, packageNames] of Object.entries(CATEGORIES)) {
    md += `\n### ${category}\n\n`;

    const packages = capabilityMap.packages.filter(p => packageNames.includes(p.name));

    for (const pkg of packages) {
      const shortName = pkg.name.replace('@unrdf/', '');
      const hasCapMap = capMapDocs[pkg.name] && capMapDocs[pkg.name].length > 0;
      const status = hasCapMap ? '✅' : '⏳';

      md += `#### ${status} [${pkg.name}](./reference/${shortName}.md)\n\n`;
      md += `${pkg.description}\n\n`;
      md += `**Version**: ${pkg.version} | **Maturity**: ${pkg.maturity}\n\n`;

      if (hasCapMap) {
        md += `**Key APIs**:\n`;
        const apis = capMapDocs[pkg.name].slice(0, 5); // Top 5 APIs
        for (const api of apis) {
          md += `- \`${api.signature}\` - ${api.type}\n`;
        }
        md += `\n[View Full API →](./reference/${shortName}.md)\n\n`;
      } else {
        md += `**Exports**: ${pkg.exports.slice(0, 3).join(', ')}${pkg.exports.length > 3 ? '...' : ''}\n\n`;
        md += `*Full API documentation coming soon*\n\n`;
      }

      md += `---\n\n`;
    }
  }

  // Add usage examples section
  md += `\n## Common Usage Patterns\n\n`;
  md += `### Pattern 1: Create and Query RDF Store\n\n`;
  md += `\`\`\`javascript
import { createStore, namedNode, literal } from '@unrdf/oxigraph';
import { executeSelect } from '@unrdf/core';

const store = createStore();
await store.insert(/* quads */);
const results = await executeSelect(store, 'SELECT ?s WHERE { ?s ?p ?o }');
\`\`\`

`;

  md += `### Pattern 2: Add Validation Hooks\n\n`;
  md += `\`\`\`javascript
import { createStore } from '@unrdf/oxigraph';
import { defineHook, executeHook } from '@unrdf/hooks';

const store = createStore();
const hook = defineHook({
  trigger: 'before:insert',
  validate: (quad) => /* validation logic */
});

if (await executeHook(hook, quad)) {
  await store.insert(quad);
}
\`\`\`

`;

  md += `### Pattern 3: Workflow Orchestration\n\n`;
  md += `\`\`\`javascript
import { createWorkflow, executeWorkflow } from '@unrdf/yawl';
import { createStore } from '@unrdf/kgc-4d';

const workflow = createWorkflow({
  tasks: [/* task definitions */]
});

const store = createStore();
await executeWorkflow(workflow, store);
\`\`\`

`;

  md += `---

## API Stability Levels

| Maturity | Description | Breaking Changes |
|----------|-------------|------------------|
| **mature** | Production-ready, stable API | Rare, with deprecation notices |
| **stable** | API finalized, minor changes possible | Only in major versions |
| **documented** | API defined but may evolve | Possible in minor versions |

---

## Support & Resources

- **Documentation**: [UNRDF Docs](../README.md)
- **GitHub**: [seanchatmangpt/unrdf](https://github.com/seanchatmangpt/unrdf)
- **Issues**: [GitHub Issues](https://github.com/seanchatmangpt/unrdf/issues)
- **Discussions**: [GitHub Discussions](https://github.com/seanchatmangpt/unrdf/discussions)

---

**Generated**: ${new Date().toISOString()}
**Source**: Capability maps + package metadata
`;

  return md;
}

// Generate quick reference cheat sheet
function generateQuickReference() {
  let md = `# UNRDF Quick Reference

**One-page cheat sheet** for common UNRDF operations across all packages.

---

## Core RDF Operations

### Create Store
\`\`\`javascript
import { createStore } from '@unrdf/oxigraph';
const store = createStore();
\`\`\`

### Add Data
\`\`\`javascript
import { namedNode, literal } from '@unrdf/oxigraph';
store.insert(store.dataFactory.quad(
  namedNode('http://example.org/s'),
  namedNode('http://example.org/p'),
  literal('object')
));
\`\`\`

### Query Data
\`\`\`javascript
import { executeSelect } from '@unrdf/core';
const results = await executeSelect(store, \`
  SELECT ?s ?p ?o WHERE { ?s ?p ?o }
\`);
\`\`\`

---

## SPARQL Query Types

| Type | Purpose | Returns | Example |
|------|---------|---------|---------|
| **SELECT** | Retrieve bindings | Array of objects | \`SELECT ?name WHERE { ?person foaf:name ?name }\` |
| **ASK** | Boolean test | true/false | \`ASK { ?s rdf:type foaf:Person }\` |
| **CONSTRUCT** | Build new graph | Quads | \`CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\` |
| **DESCRIBE** | Get resource description | Quads | \`DESCRIBE <http://example.org/resource>\` |

---

## Validation & Hooks

### Define Validation Hook
\`\`\`javascript
import { defineHook } from '@unrdf/hooks';
const hook = defineHook({
  trigger: 'before:insert',
  validate: (quad) => quad.object.value.length < 1000,
  onFailure: (quad) => console.error('Validation failed', quad)
});
\`\`\`

### Execute Hook
\`\`\`javascript
import { executeHook } from '@unrdf/hooks';
if (await executeHook(hook, quad)) {
  await store.insert(quad);
}
\`\`\`

---

## Workflow Orchestration

### Create Workflow
\`\`\`javascript
import { createWorkflow } from '@unrdf/yawl';
const workflow = createWorkflow({
  id: 'my-workflow',
  tasks: [
    { id: 'task1', execute: async () => { /* ... */ } },
    { id: 'task2', execute: async () => { /* ... */ } }
  ]
});
\`\`\`

### Execute Workflow
\`\`\`javascript
import { executeWorkflow } from '@unrdf/yawl';
await executeWorkflow(workflow, { /* context */ });
\`\`\`

---

## Temporal Events (KGC-4D)

### Create Temporal Store
\`\`\`javascript
import { createTemporalStore } from '@unrdf/kgc-4d';
const store = createTemporalStore();
\`\`\`

### Record Event
\`\`\`javascript
import { recordEvent } from '@unrdf/kgc-4d';
await recordEvent(store, {
  type: 'DataInserted',
  quad: /* quad */,
  timestamp: Date.now()
});
\`\`\`

### Time Travel Query
\`\`\`javascript
import { queryAtTime } from '@unrdf/kgc-4d';
const historicalData = await queryAtTime(store, timestamp, query);
\`\`\`

---

## Common Namespaces

\`\`\`javascript
import { RDF, RDFS, OWL, DCTERMS } from '@unrdf/core';

RDF.type          // http://www.w3.org/1999/02/22-rdf-syntax-ns#type
RDFS.label        // http://www.w3.org/2000/01/rdf-schema#label
OWL.sameAs        // http://www.w3.org/2002/07/owl#sameAs
DCTERMS.created   // http://purl.org/dc/terms/created
\`\`\`

---

## Package Categories Cheat Sheet

| Category | Key Packages | Primary Use Cases |
|----------|-------------|-------------------|
| **RDF & Storage** | core, oxigraph, caching | Store and query RDF data |
| **Governance** | hooks, validation | Policy enforcement, validation |
| **Temporal** | kgc-4d, blockchain | Event sourcing, time-travel |
| **Workflow** | yawl, yawl-* | Orchestration, execution |
| **AI & ML** | knowledge-engine, ml-inference | Reasoning, embeddings |
| **Infrastructure** | cli, observability | Tooling, monitoring |

---

## Performance Tips

1. **Batch Operations**: Insert multiple quads at once for better throughput
2. **Use Indexes**: Query with specific predicates to leverage indexes
3. **Cache Results**: Use \`@unrdf/caching\` for frequently accessed queries
4. **Stream Large Data**: Use streaming parsers for files >10MB
5. **Enable OTEL**: Monitor performance with OpenTelemetry spans

---

## Error Handling

\`\`\`javascript
import { ValidationError } from '@unrdf/core';

try {
  await store.insert(quad);
} catch (error) {
  if (error instanceof ValidationError) {
    console.error('Invalid RDF:', error.message);
  } else {
    console.error('Unexpected error:', error);
  }
}
\`\`\`

---

## API Maturity Legend

- ✅ **mature** - Production-ready, stable
- ⚡ **stable** - API finalized, minor changes possible
- 📝 **documented** - API defined, may evolve

---

**Last Updated**: ${new Date().toISOString().split('T')[0]}
**Full API Reference**: [API-REFERENCE.md](./API-REFERENCE.md)
`;

  return md;
}

// Generate individual package API pages
function generatePackageAPIPage(pkg) {
  const shortName = pkg.name.replace('@unrdf/', '');
  const hasCapMap = capMapDocs[pkg.name] && capMapDocs[pkg.name].length > 0;

  let md = `# ${pkg.name} API Reference

**Version**: ${pkg.version}
**Maturity**: ${pkg.maturity}
**Main Export**: ${pkg.mainExport}

---

## Description

${pkg.description}

---

## Installation

\`\`\`bash
pnpm add ${pkg.name}
\`\`\`

---

## Exports

`;

  if (hasCapMap) {
    md += `### API Functions\n\n`;
    md += `| Signature | Type | Evidence |\n`;
    md += `|-----------|------|----------|\n`;

    for (const api of capMapDocs[pkg.name]) {
      md += `| \`${api.signature}\` | ${api.type} | [${api.evidence}](${api.evidencePath}) |\n`;
    }
    md += `\n`;
  } else {
    md += `**Module Exports**:\n`;
    for (const exp of pkg.exports.slice(0, 10)) {
      md += `- \`${exp}\`\n`;
    }
    if (pkg.exports.length > 10) {
      md += `- *...and ${pkg.exports.length - 10} more*\n`;
    }
    md += `\n`;
  }

  md += `---

## Dependencies

`;
  if (pkg.dependencies && pkg.dependencies.length > 0) {
    for (const dep of pkg.dependencies) {
      md += `- \`${dep}\`\n`;
    }
  } else {
    md += `*No external dependencies*\n`;
  }

  md += `
---

## Keywords

${pkg.keywords.map(k => `\`${k}\``).join(' · ')}

---

## Maturity Signals

| Signal | Status |
|--------|--------|
| Has Tests | ${pkg.signals.hasTests ? '✅ Yes' : '❌ No'} |
| Has Examples | ${pkg.signals.hasExamples ? '✅ Yes' : '❌ No'} |
| Has README | ${pkg.signals.hasReadme ? '✅ Yes' : '❌ No'} |
| Has ChangeLog | ${pkg.signals.hasChangeLog ? '✅ Yes' : '❌ No'} |

---

## Package Role

${pkg.role.map(r => capabilityMap.role_descriptions[r] || r).join(' · ')}

---

## Resources

- **Source**: [\`packages/${shortName}\`](../../packages/${shortName})
- **Tests**: [\`packages/${shortName}/test\`](../../packages/${shortName}/test)
${pkg.examples ? `- **Examples**: [\`packages/${shortName}/examples\`](../../packages/${shortName}/examples)\n` : ''}
- **Full Capability Map**: ${hasCapMap ? `[View →](../${shortName}.md)` : '*Coming soon*'}

---

**Last Updated**: ${new Date().toISOString().split('T')[0]}
**Generated from**: capability-map.json
`;

  return md;
}

// Main execution
console.log('🚀 Generating API Reference Documentation...\n');

// 1. Generate OpenAPI Schema
console.log('📝 Generating OpenAPI 3.0 schema...');
const openAPISchema = generateOpenAPISchema();
const openAPIPath = join(ROOT, 'docs/capability-map/openapi-schema.json');
writeFileSync(openAPIPath, JSON.stringify(openAPISchema, null, 2));
console.log(`   ✅ Created: ${openAPIPath}`);

// 2. Generate Master API Reference
console.log('📝 Generating master API reference...');
const masterAPIRef = generateMasterAPIReference();
const masterAPIPath = join(ROOT, 'docs/capability-map/API-REFERENCE.md');
writeFileSync(masterAPIPath, masterAPIRef);
console.log(`   ✅ Created: ${masterAPIPath}`);

// 3. Generate Quick Reference
console.log('📝 Generating quick reference cheat sheet...');
const quickRef = generateQuickReference();
const quickRefPath = join(ROOT, 'docs/capability-map/QUICK-REFERENCE.md');
writeFileSync(quickRefPath, quickRef);
console.log(`   ✅ Created: ${quickRefPath}`);

// 4. Generate Individual Package API Pages
console.log('📝 Generating individual package API pages...');
let packageCount = 0;
for (const pkg of capabilityMap.packages) {
  const shortName = pkg.name.replace('@unrdf/', '');
  const packageAPIPage = generatePackageAPIPage(pkg);
  const packageAPIPath = join(ROOT, 'docs/capability-map/reference', `${shortName}.md`);
  writeFileSync(packageAPIPath, packageAPIPage);
  packageCount++;
}
console.log(`   ✅ Created ${packageCount} package API pages`);

// Summary
console.log('\n📊 Generation Summary:');
console.log(`   - OpenAPI Schema: 1 file`);
console.log(`   - Master API Reference: 1 file`);
console.log(`   - Quick Reference: 1 file`);
console.log(`   - Individual Package APIs: ${packageCount} files`);
console.log(`   - Total API Docs: ${3 + packageCount} files`);
console.log('\n✅ API Reference Documentation Complete!\n');
