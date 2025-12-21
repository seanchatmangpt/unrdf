---
description: Generate documentation from UNRDF RDF data and source code, outputting Markdown and HTML
---

# Doc Generator

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Transform UNRDF RDF ontologies, specifications, and source code into user-facing documentation in Markdown and HTML formats.

## Documentation Sources

| Source            | Output           | Format   |
| ----------------- | ---------------- | -------- |
| Ontology (\*.ttl) | API reference    | Markdown |
| SHACL shapes      | Validation rules | Markdown |
| JSDoc comments    | API docs         | HTML     |
| Spec files        | User guides      | Markdown |

## Execution Steps

### 1. Generate Ontology Documentation

```bash
# Extract ontology metadata
node -e "
import { createStore } from '@unrdf/oxigraph';
import { readFileSync } from 'fs';

const store = createStore();
store.load(readFileSync('packages/core/src/ontologies/maturity.ttl', 'utf8'), {format: 'text/turtle'});

const query = \`
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
SELECT ?class ?label ?comment WHERE {
  ?class a owl:Class .
  OPTIONAL { ?class rdfs:label ?label }
  OPTIONAL { ?class rdfs:comment ?comment }
}
\`;

for (const row of store.query(query)) {
  console.log('##', row.get('label')?.value || row.get('class').value);
  if (row.get('comment')) console.log(row.get('comment').value);
  console.log();
}
"
```

### 2. Generate API Documentation from JSDoc

```bash
# Use typedoc or similar
pnpm add -D typedoc
pnpm typedoc --entryPoints packages/core/src/index.mjs --out docs/api

# Or use jsdoc directly
pnpm jsdoc packages/core/src/**/*.mjs -d docs/api
```

### 3. Transform RDF to Markdown

```javascript
// rdf-to-md.mjs
import { createStore } from '@unrdf/oxigraph';

export function ontologyToMarkdown(turtleContent) {
  const store = createStore();
  store.load(turtleContent, { format: 'text/turtle' });

  let md = '# Ontology Reference\n\n';

  // Classes
  md += '## Classes\n\n';
  const classes = store.query(`
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?class ?label ?comment WHERE {
      ?class a owl:Class .
      OPTIONAL { ?class rdfs:label ?label }
      OPTIONAL { ?class rdfs:comment ?comment }
    }
  `);

  for (const row of classes) {
    const name = row.get('label')?.value || row.get('class').value.split('#').pop();
    md += `### ${name}\n\n`;
    if (row.get('comment')) {
      md += `${row.get('comment').value}\n\n`;
    }
  }

  // Properties
  md += '## Properties\n\n';
  const props = store.query(`
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?prop ?label ?domain ?range WHERE {
      { ?prop a owl:ObjectProperty } UNION { ?prop a owl:DatatypeProperty }
      OPTIONAL { ?prop rdfs:label ?label }
      OPTIONAL { ?prop rdfs:domain ?domain }
      OPTIONAL { ?prop rdfs:range ?range }
    }
  `);

  md += '| Property | Domain | Range |\n';
  md += '|----------|--------|-------|\n';
  for (const row of props) {
    const name = row.get('label')?.value || row.get('prop').value.split('#').pop();
    const domain = row.get('domain')?.value.split('#').pop() || '-';
    const range = row.get('range')?.value.split('#').pop() || '-';
    md += `| ${name} | ${domain} | ${range} |\n`;
  }

  return md;
}
```

### 4. Generate Maturity Report Documentation

```bash
# Generate maturity matrix docs
node packages/cli/dist/index.mjs maturity report --format md -o docs/maturity-matrix.md

# Generate synergy guide
node packages/cli/dist/index.mjs maturity synergy --all --format md -o docs/synergy-guide.md
```

## Documentation Templates

### Package README Template

```markdown
# @unrdf/[package-name]

[One-line description from package.json]

## Installation

\`\`\`bash
pnpm add @unrdf/[package-name]
\`\`\`

## Quick Start

\`\`\`javascript
import { [mainExport] } from '@unrdf/[package-name]';

// Example usage
\`\`\`

## API Reference

### [Function/Class Name]

[JSDoc description]

**Parameters:**

- `param1` (type) - Description

**Returns:** type - Description

**Example:**
\`\`\`javascript
// Example code
\`\`\`

## Maturity

| Metric        | Value    |
| ------------- | -------- |
| Level         | L[X]     |
| Coverage      | X%       |
| API Stability | [status] |

## Related Packages

- [@unrdf/related](../related/) - Description
```

### Ontology Documentation Template

```markdown
# [Ontology Name] Ontology

**Namespace:** `[prefix]: <[uri]>`
**Version:** [version]

## Overview

[Description from owl:Ontology]

## Classes

### [ClassName]

**URI:** `[full-uri]`
**Superclass:** [superclass]

[rdfs:comment]

**Properties:**

- [property1] - [description]
- [property2] - [description]

## Properties

| Property | Type   | Domain   | Range   | Description |
| -------- | ------ | -------- | ------- | ----------- |
| [name]   | Object | [domain] | [range] | [comment]   |

## Instances

### [InstanceName]

**Type:** [class]

| Property | Value   |
| -------- | ------- |
| [prop]   | [value] |
```

## Output Format

```markdown
## Doc Generation Report

**Date**: [timestamp]
**Source**: [files processed]

### Generated Files

| File                    | Type     | Size | Status |
| ----------------------- | -------- | ---- | ------ |
| docs/maturity-matrix.md | Markdown | XKB  | ✅     |
| docs/api/index.html     | HTML     | XKB  | ✅     |

### Ontology Coverage

- Classes documented: X/Y
- Properties documented: X/Y
- Instances documented: X/Y

### API Coverage

- Functions: X exported, Y documented
- Types: X defined, Y documented

### Issues Found

1. [WARNING] Missing JSDoc for function X
2. [WARNING] No rdfs:comment on class Y

### Next Steps

1. Add missing documentation
2. Review generated output
3. Publish to docs site
```

## Integration Commands

```bash
# Full documentation build
pnpm run docs:build

# Serve locally
pnpm run docs:serve

# Deploy to GitHub Pages
pnpm run docs:deploy
```

End Command ---
