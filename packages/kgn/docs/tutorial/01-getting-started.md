# Getting Started with @unrdf/kgn

Welcome! This tutorial will teach you how to use @unrdf/kgn to generate code and documents using templates with RDF data.

## What You'll Learn

By the end of this tutorial, you'll be able to:
- Parse templates with YAML frontmatter
- Use RDF filters to work with semantic data
- Generate code from RDF entity definitions
- Create deterministic, reproducible output

## Prerequisites

- Node.js 18+ installed
- Basic knowledge of templates (Nunjucks/Jinja2 style)
- Understanding of RDF concepts (URIs, prefixes, triples) is helpful but not required

## Installation

```bash
# Using pnpm (recommended)
pnpm add @unrdf/kgn

# Using npm
npm install @unrdf/kgn

# Using yarn
yarn add @unrdf/kgn
```

## Your First Template

Let's create a simple template that generates a person profile:

### Step 1: Create a Template File

Create a file called `person.njk`:

```nunjucks
---
name: "Person Profile Generator"
prefixes:
  foaf: "http://xmlns.com/foaf/0.1/"
  ex: "http://example.org/"
person:
  uri: "ex:john-doe"
  type: "foaf:Person"
  name: "John Doe"
  email: "john@example.org"
  homepage: "https://johndoe.com"
---
# {{ person.name }}'s Profile

## RDF Representation

```turtle
{{ person.uri | expand(prefixes) }}
  a {{ person.type | expand(prefixes) }} ;
  foaf:name {{ person.name | rdfLiteral }} ;
  foaf:mbox <mailto:{{ person.email }}> ;
  foaf:homepage <{{ person.homepage }}> .
```

## JSON-LD Representation

```json
{
  "@context": {
    "foaf": "{{ prefixes.foaf }}",
    "ex": "{{ prefixes.ex }}"
  },
  "@id": "{{ person.uri }}",
  "@type": "{{ person.type }}",
  "foaf:name": "{{ person.name }}",
  "foaf:mbox": "mailto:{{ person.email }}",
  "foaf:homepage": "{{ person.homepage }}"
}
```
```

### Step 2: Render the Template

Create a file called `render-person.mjs`:

```javascript
import { FrontmatterParser } from '@unrdf/kgn';
import { expand, rdfLiteral } from '@unrdf/kgn/filters';
import nunjucks from 'nunjucks';
import { readFileSync } from 'fs';

// Set up nunjucks with RDF filters
const env = new nunjucks.Environment(null, { autoescape: false });
env.addFilter('expand', expand);
env.addFilter('rdfLiteral', rdfLiteral);

// Parse frontmatter
const parser = new FrontmatterParser();
const template = readFileSync('person.njk', 'utf-8');
const parsed = parser.parse(template);

// Render template
const output = env.renderString(parsed.content, parsed.frontmatter);

console.log(output);
```

### Step 3: Run It

```bash
node render-person.mjs
```

You should see output like this:

```markdown
# John Doe's Profile

## RDF Representation

```turtle
http://example.org/john-doe
  a http://xmlns.com/foaf/0.1/Person ;
  foaf:name "John Doe" ;
  foaf:mbox <mailto:john@example.org> ;
  foaf:homepage <https://johndoe.com> .
```

## JSON-LD Representation

```json
{
  "@context": {
    "foaf": "http://xmlns.com/foaf/0.1/",
    "ex": "http://example.org/"
  },
  "@id": "ex:john-doe",
  "@type": "foaf:Person",
  "foaf:name": "John Doe",
  "foaf:mbox": "mailto:john@example.org",
  "foaf:homepage": "https://johndoe.com"
}
```
```

## Understanding What Happened

Let's break down what happened:

### 1. Frontmatter Parsing

The `FrontmatterParser` extracted the YAML frontmatter:

```javascript
{
  name: "Person Profile Generator",
  prefixes: { /* RDF prefixes */ },
  person: { /* Person data */ }
}
```

### 2. Template Variables

The frontmatter data became available as template variables:
- `{{ person.name }}` → "John Doe"
- `{{ prefixes.foaf }}` → "http://xmlns.com/foaf/0.1/"

### 3. RDF Filters

Two filters transformed the data:

**`expand` filter**: Expands CURIEs to full URIs
```
person.uri | expand(prefixes)
"ex:john-doe" → "http://example.org/john-doe"
```

**`rdfLiteral` filter**: Wraps values in RDF literal syntax
```
person.name | rdfLiteral
"John Doe" → "\"John Doe\""
```

## Next Steps

Congratulations! You've created your first RDF-powered template.

Continue to:
- [Tutorial 2: Working with Multiple Entities](./02-multiple-entities.md)
- [Tutorial 3: Using All RDF Filters](./03-rdf-filters.md)
- [How-To: Generate TypeScript from RDF](../how-to/generate-typescript.md)

## Troubleshooting

**Error: "Cannot find package '@unrdf/kgn'"**
- Make sure you're in a workspace that has @unrdf/kgn installed
- Run `pnpm install` in the workspace root

**Template not rendering correctly**
- Check that frontmatter starts and ends with `---`
- Verify YAML syntax (indentation matters!)
- Ensure filter names match exactly: `expand`, `rdfLiteral`

**RDF output looks wrong**
- Verify prefix definitions in frontmatter
- Check that CURIEs use correct format: `prefix:localName`
- Make sure you're passing `prefixes` to the `expand` filter
