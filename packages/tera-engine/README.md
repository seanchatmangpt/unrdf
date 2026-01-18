# @unrdf/tera-engine

> Tera-compatible template engine for UNRDF with RDF/Turtle and TOML integration

**Version**: 1.0.0
**License**: MIT

## Overview

`@unrdf/tera-engine` is a pure JavaScript template engine inspired by [Tera](https://tera.netlify.app/) (Rust) with first-class support for RDF knowledge graphs and TOML configuration files. It integrates seamlessly with the UNRDF ecosystem.

## Features

- **Tera-compatible syntax** - Variables, filters, for loops, if/else statements
- **RDF/Turtle support** - Load and query RDF data directly in templates
- **TOML integration** - Load configuration files as template context
- **Custom RDF filters** - Extract URIs, namespaces, format triples, and more
- **Knowledge Engine integration** - Works with `@unrdf/knowledge-engine`
- **Zod validation** - Runtime type safety for all inputs
- **Pure ESM** - Modern JavaScript with full ESM support

## Installation

```bash
pnpm add @unrdf/tera-engine
```

## Quick Start

### Basic Template Rendering

```javascript
import { quickRender } from '@unrdf/tera-engine';

const output = await quickRender({
  template: 'Hello {{ name | upper }}!',
  context: { name: 'world' }
});
// Output: "Hello WORLD!"
```

### With TOML Configuration

```javascript
import { createRdfTeraEngine } from '@unrdf/tera-engine';

const engine = createRdfTeraEngine();

const output = await engine.render(
  'App: {{ app.name }} v{{ app.version }}',
  {
    toml: {
      content: `
        [app]
        name = "MyApp"
        version = "1.0.0"
      `
    }
  }
);
// Output: "App: MyApp v1.0.0"
```

### With RDF/Turtle Data

```javascript
const output = await engine.render(
  'Found {{ rdf.triples | length }} triples',
  {
    rdf: {
      content: `
        @prefix ex: <http://example.org/> .
        ex:Alice ex:knows ex:Bob .
        ex:Bob ex:knows ex:Charlie .
      `,
      format: 'turtle'
    }
  }
);
// Output: "Found 2 triples"
```

### Combined TOML + RDF

```javascript
const template = `
# {{ project.name }}

Total triples: {{ rdf.triples | length }}

## People
{% for subject in rdf.subjects() %}
- {{ subject | localname }}
{% endfor %}
`;

const output = await engine.render(template, {
  toml: {
    content: `
      [project]
      name = "Knowledge Graph Project"
    `
  },
  rdf: {
    content: `
      @prefix ex: <http://example.org/> .
      ex:Alice ex:knows ex:Bob .
      ex:Charlie ex:knows ex:Alice .
    `,
    format: 'turtle'
  }
});
```

## Template Syntax

### Variables

```
{{ variable }}
{{ user.name }}
{{ items.0 }}
```

### Filters

```
{{ name | upper }}
{{ text | truncate(50) }}
{{ items | join(", ") }}
```

Chain multiple filters:
```
{{ name | upper | truncate(10) }}
```

### For Loops

```
{% for item in items %}
  {{ item }}
{% endfor %}
```

Loop variables:
```
{% for item in items %}
  {{ loop.index }}: {{ item }}
  {% if loop.first %}FIRST{% endif %}
  {% if loop.last %}LAST{% endif %}
{% endfor %}
```

### If Statements

```
{% if condition %}
  true branch
{% else %}
  false branch
{% endif %}
```

### Comments

```
{# This is a comment #}
{#
  Multiline
  comment
#}
```

## Standard Filters

| Filter | Description | Example |
|--------|-------------|---------|
| `upper` | Uppercase | `{{ "hello" \| upper }}` → HELLO |
| `lower` | Lowercase | `{{ "HELLO" \| lower }}` → hello |
| `capitalize` | Capitalize first letter | `{{ "hello" \| capitalize }}` → Hello |
| `truncate(n)` | Truncate to length | `{{ text \| truncate(10) }}` |
| `join(sep)` | Join array | `{{ items \| join(", ") }}` |
| `length` | Get length | `{{ items \| length }}` |
| `first` | First element | `{{ items \| first }}` |
| `last` | Last element | `{{ items \| last }}` |
| `reverse` | Reverse array/string | `{{ items \| reverse }}` |
| `replace(from, to)` | Replace substring | `{{ text \| replace("a", "b") }}` |

## RDF Filters

| Filter | Description | Example |
|--------|-------------|---------|
| `localname` | Extract local name from URI | `{{ uri \| localname }}` |
| `namespace` | Extract namespace from URI | `{{ uri \| namespace }}` |
| `prefixedName(prefixes)` | Convert to prefixed name | `{{ uri \| prefixedName(prefixes) }}` |
| `ntriples` | Format as N-Triples | `{{ term \| ntriples }}` |
| `turtle(prefixes)` | Format as Turtle | `{{ term \| turtle(prefixes) }}` |
| `subjects` | Extract unique subjects | `{{ triples \| subjects }}` |
| `predicates` | Extract unique predicates | `{{ triples \| predicates }}` |
| `objects` | Extract unique objects | `{{ triples \| objects }}` |
| `filterByPredicate(uri)` | Filter triples by predicate | `{{ triples \| filterByPredicate(uri) }}` |

## API Reference

### `createRdfTeraEngine(config)`

Creates a fully-configured engine with RDF support.

```javascript
const engine = createRdfTeraEngine({
  includeRdfFilters: true,  // Include RDF filters (default: true)
  customFilters: {          // Additional custom filters
    myFilter: (value) => ...
  },
  options: {                // Default template options
    autoescape: true,
    strictVariables: false
  }
});
```

### `engine.render(template, sources, options)`

Renders a template with merged context.

```javascript
const output = await engine.render(
  'Hello {{ name }}!',
  {
    context: { name: 'World' },
    toml: { path: 'config.toml' },
    rdf: { path: 'data.ttl', format: 'turtle' }
  },
  { autoescape: true }
);
```

### `engine.renderFile(templatePath, sources, options)`

Renders a template file.

```javascript
const output = await engine.renderFile(
  'templates/page.html',
  { toml: { path: 'config.toml' } }
);
```

### `quickRender(options)`

Quick render helper.

```javascript
const output = await quickRender({
  template: 'Hello {{ name }}!',
  context: { name: 'World' },
  toml: { path: 'config.toml' },
  rdf: { content: '...', format: 'turtle' },
  options: { autoescape: true }
});
```

### `createTeraEngine(config)`

Creates a basic engine without RDF features.

```javascript
const engine = createTeraEngine({
  filters: { upper: s => s.toUpperCase() },
  options: { autoescape: true }
});

const output = engine.render('{{ name }}', { name: 'test' });
```

## Loaders

### `loadToml(options)`

Loads TOML data.

```javascript
import { loadToml } from '@unrdf/tera-engine/loaders';

const data = await loadToml({ path: 'config.toml' });
const data2 = await loadToml({ content: 'key = "value"' });
```

### `loadRdf(options)`

Loads RDF data.

```javascript
import { loadRdf } from '@unrdf/tera-engine/loaders';

const { store, triples } = await loadRdf({
  path: 'data.ttl',
  format: 'turtle',
  baseIRI: 'http://example.org/'
});
```

### `loadRdfContext(options)`

Loads RDF with helper methods.

```javascript
const context = await loadRdfContext({ path: 'data.ttl' });

context.triples       // Array of triples
context.subjects()    // Unique subjects
context.predicates()  // Unique predicates
context.objects()     // Unique objects
context.query(sparql) // Execute SPARQL query
```

## Integration with Knowledge Engine

```javascript
import { renderWithKnowledge } from '@unrdf/tera-engine/api';
import { KnowledgeHookManager } from '@unrdf/knowledge-engine';

const km = new KnowledgeHookManager();
const output = await renderWithKnowledge(
  km,
  'Hooks: {{ hooks | length }}',
  { additional: 'context' }
);
```

## Examples

### Generate Documentation from RDF

```javascript
const template = `
# {{ project.name }}

## Classes
{% for subject in rdf.triples | filterByPredicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type") %}
- {{ subject.subject.value | localname }}
{% endfor %}
`;

const output = await engine.render(template, {
  toml: { content: '[project]\nname = "My Ontology"' },
  rdf: { path: 'ontology.ttl', format: 'turtle' }
});
```

### Configuration-Driven Reports

```javascript
const template = `
# {{ report.title }}

Date: {{ report.date }}
Author: {{ report.author }}

## Statistics
- Total entities: {{ rdf.subjects() | length }}
- Total relations: {{ rdf.predicates() | length }}
`;

const output = await engine.render(template, {
  toml: { path: 'report-config.toml' },
  rdf: { path: 'knowledge-graph.ttl' }
});
```

## Template Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `autoescape` | boolean | `true` | Auto-escape HTML |
| `strictVariables` | boolean | `false` | Throw on undefined variables |
| `trimBlocks` | boolean | `false` | Trim newlines after blocks |
| `lstripBlocks` | boolean | `false` | Strip leading whitespace |

## Error Handling

All loaders and render functions throw descriptive errors:

```javascript
try {
  await engine.render(template, sources);
} catch (error) {
  console.error('Render failed:', error.message);
}
```

Zod validation errors include detailed field information.

## Performance

- Template parsing: ~0.1ms per template
- TOML parsing: ~1ms for typical config files
- RDF loading: ~10ms for 1000 triples (Oxigraph)
- Filter application: <0.01ms per filter

## Development

```bash
# Install dependencies
pnpm install

# Run tests
pnpm test

# Watch mode
pnpm test:watch

# Lint
pnpm lint

# Format
pnpm format
```

## License

MIT

## Contributing

Contributions welcome! Please follow UNRDF contribution guidelines.

## Related Packages

- `@unrdf/core` - Core RDF operations
- `@unrdf/oxigraph` - SPARQL engine
- `@unrdf/knowledge-engine` - Rule engine and inference
- `@unrdf/hooks` - Policy framework

## Support

- GitHub Issues: https://github.com/unrdf/unrdf/issues
- Documentation: https://github.com/unrdf/unrdf#readme
