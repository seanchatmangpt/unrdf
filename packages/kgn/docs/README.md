# @unrdf/kgn Documentation

Welcome to the @unrdf/kgn documentation! This package provides deterministic template generation with RDF/semantic web support.

## Documentation Structure

This documentation follows the [Diataxis](https://diataxis.fr/) framework, organized into four categories:

### 📚 [Tutorials](./tutorial/) - **Learning-Oriented**

Step-by-step lessons for getting started:

- [Getting Started](./tutorial/01-getting-started.md) - Your first RDF template
- [Working with Multiple Entities](./tutorial/02-multiple-entities.md) - Generate TypeScript from RDF vocabulary

**Start here if you're new to @unrdf/kgn.**

### 🛠️ [How-To Guides](./how-to/) - **Problem-Oriented**

Practical guides for specific tasks:

- [Generate TypeScript from RDF](./how-to/generate-typescript.md) - Create TypeScript interfaces from RDF schemas

**Use these when you have a specific goal.**

### 📖 [Reference](./reference/) - **Information-Oriented**

Technical documentation and API details:

- [Filter API Reference](./reference/filters.md) - Complete filter documentation

**Use these to look up specific details.**

### 💡 [Explanation](./explanation/) - **Understanding-Oriented**

Conceptual discussions and design philosophy:

- [Why RDF Templates?](./explanation/why-rdf-templates.md) - Design philosophy and use cases

**Read these to understand the "why" behind the design.**

## Quick Start

```bash
# Install
pnpm add @unrdf/kgn

# Create a template with RDF frontmatter
cat > person.njk <<'EOF'
---
prefixes:
  foaf: "http://xmlns.com/foaf/0.1/"
person:
  uri: "ex:john"
  name: "John Doe"
---
{{ person.uri | expand(prefixes) }} foaf:name {{ person.name | rdfLiteral }} .
EOF

# Render it
node -e "
import { FrontmatterParser } from '@unrdf/kgn';
import { expand, rdfLiteral } from '@unrdf/kgn/filters';
import nunjucks from 'nunjucks';
import { readFileSync } from 'fs';

const env = new nunjucks.Environment(null, { autoescape: false });
env.addFilter('expand', expand);
env.addFilter('rdfLiteral', rdfLiteral);

const parser = new FrontmatterParser();
const template = readFileSync('person.njk', 'utf-8');
const parsed = parser.parse(template);
console.log(env.renderString(parsed.content, parsed.frontmatter));
"
```

## Key Features

### ✅ RDF-Aware Templates

Work with RDF data natively:
- CURIE expansion (`ex:Person` → `http://example.org/Person`)
- RDF literal formatting (`"John"@en`, `"42"^^xsd:integer`)
- Turtle/N3 escaping
- SPARQL query integration

### ✅ Deterministic Output

Same input → same output (always):
- No timestamps, UUIDs, or randomness
- Reproducible builds
- Meaningful git diffs
- Testable output

### ✅ Frontmatter-Driven

YAML frontmatter as schema:
- Human-readable
- Version-control friendly
- Nested structures
- Easy to edit

### ✅ Multiple Output Formats

One schema, many outputs:
- TypeScript interfaces
- Zod validators
- JSON Schema
- JSON-LD contexts
- SPARQL queries
- Documentation

## Architecture Overview

```
┌─────────────────────┐
│  RDF Vocabulary     │  ← YAML frontmatter
│  (source of truth)  │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  @unrdf/kgn         │  ← Template engine + RDF filters
│  Template Engine    │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Generated Code     │  ← TypeScript, JSON-LD, etc.
│  (deterministic)    │
└─────────────────────┘
```

## Core Concepts

### RDF Frontmatter

Define your RDF schema in YAML:

```yaml
---
prefixes:
  ex: "http://example.org/"
  foaf: "http://xmlns.com/foaf/0.1/"
entities:
  - uri: "ex:Person1"
    type: "foaf:Person"
    properties:
      name: "John Doe"
      email: "john@example.org"
---
```

### RDF Filters

Transform RDF concepts in templates:

- `expand` - CURIE → full URI
- `contract` - URI → CURIE
- `rdfLiteral` - Create RDF literals
- `rdfResource` - Wrap URI in `<>`
- `turtleEscape` - Escape for Turtle
- `sparqlVar` - Create SPARQL variable

### Deterministic Rendering

Every render produces identical output:

```javascript
// Run 100 times → same result every time
for (let i = 0; i < 100; i++) {
  const output = render(template, data);
  assert(output === expectedOutput);
}
```

## Examples

### Generate TypeScript from RDF

```nunjucks
---
classes:
  - uri: "ex:Person"
    properties:
      - {name: "name", type: "string"}
      - {name: "age", type: "number"}
---
export interface {{ classes[0].uri | contract }} {
{% for prop in classes[0].properties %}
  {{ prop.name }}: {{ prop.type }};
{% endfor %}
}
```

Output:
```typescript
export interface Person {
  name: string;
  age: number;
}
```

### Generate Turtle from Data

```nunjucks
---
prefixes:
  foaf: "http://xmlns.com/foaf/0.1/"
people:
  - {uri: "ex:john", name: "John Doe"}
  - {uri: "ex:jane", name: "Jane Smith"}
---
{% for person in people %}
{{ person.uri | expand(prefixes) | rdfResource }}
  a foaf:Person ;
  foaf:name {{ person.name | rdfLiteral }} .
{% endfor %}
```

## API Exports

```javascript
// Main exports
import {
  TemplateEngine,
  FrontmatterParser,
  DeterministicRenderer,
  renderTemplate,
  lintTemplate,
  validateTemplate
} from '@unrdf/kgn';

// RDF filters
import {
  expand,
  contract,
  rdfLiteral,
  rdfResource,
  turtleEscape,
  sparqlVar,
  rdfList,
  blankNode,
  rdfDatatype
} from '@unrdf/kgn/filters';

// Sub-packages
import { TemplateEngine } from '@unrdf/kgn/engine';
import { createCustomFilters } from '@unrdf/kgn/filters';
import { DeterministicRenderer } from '@unrdf/kgn/renderer';
import { TemplateLinter } from '@unrdf/kgn/linter';
```

## Contributing

Found a bug? Have a feature request?

- **Issues**: [github.com/seanchatmangpt/unrdf/issues](https://github.com/seanchatmangpt/unrdf/issues)
- **Pull Requests**: [github.com/seanchatmangpt/unrdf/pulls](https://github.com/seanchatmangpt/unrdf/pulls)

## License

MIT © UNRDF Project

## See Also

- [@unrdf/core](../../core/) - Core RDF functionality
- [@unrdf/oxigraph](../../oxigraph/) - RDF store
- [Nunjucks Documentation](https://mozilla.github.io/nunjucks/) - Template syntax
- [Diataxis](https://diataxis.fr/) - Documentation framework

---

**Need help?** Start with the [Getting Started Tutorial](./tutorial/01-getting-started.md).
