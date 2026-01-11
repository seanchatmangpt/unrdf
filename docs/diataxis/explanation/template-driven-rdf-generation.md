# Template-Driven RDF Generation

Understanding the principles and benefits of template-based RDF generation.

## Introduction

Template-driven RDF generation is an approach where RDF data is produced from declarative templates rather than imperative code. This document explores why this approach is effective, how it works, and when to use it.

## The Problem with Traditional RDF Generation

### Imperative Approach

Traditional RDF generation uses imperative code:

```javascript
// ❌ Imperative: Step-by-step instructions
function generatePersonRDF(person) {
  let rdf = '';
  rdf += `<http://example.org/${person.id}>\n`;
  rdf += `  a foaf:Person ;\n`;
  rdf += `  foaf:name "${escapeLiteral(person.name)}" ;\n`;

  if (person.email) {
    rdf += `  foaf:mbox <mailto:${person.email}> ;\n`;
  }

  if (person.friends && person.friends.length > 0) {
    for (let i = 0; i < person.friends.length; i++) {
      rdf += `  foaf:knows <http://example.org/${person.friends[i]}> ;\n`;
    }
  }

  rdf = rdf.replace(/;\n$/, ' .\n');
  return rdf;
}
```

**Problems:**
1. **Verbose**: Lots of boilerplate code
2. **Error-Prone**: Easy to forget escaping or syntax
3. **Hard to Read**: Logic mixed with formatting
4. **Difficult to Maintain**: Changes require code edits
5. **Not Inspectable**: Can't see output structure without running

### Declarative Approach

Template-driven generation is declarative:

```nunjucks
{# ✅ Declarative: What, not how #}
ex:{{ person.id }}
  a foaf:Person ;
  foaf:name {{ person.name | rdfLiteral }} ;
  {% if person.email %}
  foaf:mbox {{ person.email | rdfResource }} ;
  {% endif %}
  {% for friend in person.friends %}
  foaf:knows ex:{{ friend }} ;
  {% endfor %}
  .
```

**Benefits:**
1. **Concise**: Minimal boilerplate
2. **Safe**: Filters ensure correctness
3. **Readable**: Output structure is visible
4. **Maintainable**: Edit template, not code
5. **Inspectable**: Template shows exact output format

## Core Principles

### Principle 1: Separation of Concerns

**Data** and **Presentation** are separate:

```javascript
// Data (business logic)
const person = {
  id: 'alice',
  name: 'Alice Smith',
  email: 'alice@example.org',
  friends: ['bob', 'charlie']
};

// Presentation (template)
// person.njk
ex:{{ person.id }}
  a foaf:Person ;
  foaf:name {{ person.name | rdfLiteral }} .
```

**Why It Matters:**
- Data can come from any source (DB, API, file)
- Template can be reused for different data
- Changes to one don't affect the other

### Principle 2: Declarative Over Imperative

**Imperative:** "How to build RDF"
```javascript
let rdf = '';
rdf += '...\n';
rdf += '...\n';
```

**Declarative:** "What RDF looks like"
```nunjucks
ex:{{ id }}
  a {{ type }} ;
  rdfs:label {{ label | rdfLiteral }} .
```

**Why Declarative Wins:**
- Easier to understand intent
- Less room for bugs
- Self-documenting
- Optimizable by engine

### Principle 3: Composition

Build complex templates from simple parts:

```nunjucks
{# Base template: base.njk #}
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

{% block content %}{% endblock %}

{# Specific template: person.njk #}
{% extends "base.njk" %}

{% block content %}
ex:{{ person.id }}
  a foaf:Person ;
  foaf:name {{ person.name | rdfLiteral }} .
{% endblock %}
```

**Why Composition:**
- DRY: Define prefixes once
- Consistency: All templates use same base
- Maintainability: Update base → all templates updated

### Principle 4: Type Safety Through Filters

Filters ensure type correctness:

```nunjucks
{# Filters prevent syntax errors #}
{{ string | rdfLiteral }}      <!-- "string" -->
{{ uri | rdfResource }}         <!-- <uri> -->
{{ number | rdfDatatype('integer') }} <!-- "42"^^xsd:integer -->
```

**Without Filters (Error-Prone):**
```nunjucks
{# ❌ Easy to forget escaping #}
foaf:name "{{ name }}" .  <!-- Breaks if name has quotes -->

{# ❌ Easy to forget angle brackets #}
foaf:homepage {{ url }} .  <!-- Invalid if not bracketed -->
```

**With Filters (Type-Safe):**
```nunjucks
{# ✅ Automatic escaping #}
foaf:name {{ name | rdfLiteral }} .

{# ✅ Automatic bracketing #}
foaf:homepage {{ url | rdfResource }} .
```

## Template Patterns

### Pattern 1: Conditional Properties

Handle optional properties elegantly:

```nunjucks
ex:{{ person.id }}
  a foaf:Person ;
  foaf:name {{ person.name | rdfLiteral }} ;
  {% if person.email %}
  foaf:mbox {{ person.email | rdfResource }} ;
  {% endif %}
  {% if person.homepage %}
  foaf:homepage {{ person.homepage | rdfResource }} ;
  {% endif %}
  .
```

**Alternative: Default Values**
```nunjucks
foaf:status {{ person.status | default('active') | rdfLiteral }} ;
```

### Pattern 2: Collections

Loop over arrays to generate multiple triples:

```nunjucks
{# Multiple values for same property #}
ex:{{ article.id }}
  a schema:Article ;
  {% for tag in article.tags %}
  schema:keywords {{ tag | rdfLiteral }} ;
  {% endfor %}
  .

{# Multiple relationships #}
ex:{{ person.id }}
  a foaf:Person ;
  {% for friend in person.friends %}
  foaf:knows ex:{{ friend }} ;
  {% endfor %}
  .
```

### Pattern 3: Nested Resources

Generate complex structures:

```nunjucks
ex:{{ person.id }}
  a foaf:Person ;
  foaf:name {{ person.name | rdfLiteral }} ;
  vcard:hasAddress [
    a vcard:Address ;
    vcard:street-address {{ person.address.street | rdfLiteral }} ;
    vcard:locality {{ person.address.city | rdfLiteral }} ;
    vcard:postal-code {{ person.address.zip | rdfLiteral }}
  ] .
```

### Pattern 4: Macros

Create reusable template functions:

```nunjucks
{# Define macro #}
{% macro personTriple(p) %}
ex:{{ p.id }}
  a foaf:Person ;
  foaf:name {{ p.name | rdfLiteral }} ;
  {% if p.email %}
  foaf:mbox {{ p.email | rdfResource }} ;
  {% endif %}
  .
{% endmacro %}

{# Use macro #}
{% for person in people %}
{{ personTriple(person) }}
{% endfor %}
```

### Pattern 5: Parameterization

Make templates configurable:

```nunjucks
{# Parameterized ontology #}
@prefix {{ prefix }}: <{{ namespace }}> .

{{ prefix }}:{{ className }}
  a owl:Class ;
  rdfs:label {{ label | rdfLiteral(language) }} ;
  rdfs:comment {{ comment | rdfLiteral(language) }} .
```

**Usage:**
```javascript
render('class.njk', {
  prefix: 'ex',
  namespace: 'http://example.org/',
  className: 'Person',
  label: 'Person',
  comment: 'A human being',
  language: 'en'
});
```

## When to Use Templates

### Use Templates When:

1. **Output Structure is Known**
   - Generating RDF from schema
   - Consistent data transformations
   - Repeatable workflows

2. **Multiple Similar Outputs**
   - Many entities with same structure
   - Variations of a pattern
   - Bulk data conversion

3. **Maintainability Matters**
   - Long-term maintenance expected
   - Multiple developers
   - Documentation is important

4. **Type Safety Desired**
   - Prevent syntax errors
   - Validate at generation time
   - Ensure consistency

### Don't Use Templates When:

1. **Dynamic, Unknown Structure**
   - Runtime-determined structure
   - Highly variable data
   - Exploratory data analysis

2. **Complex Logic Required**
   - Intricate business rules
   - Multiple data sources
   - Complex transformations

3. **Performance Critical**
   - Millions of entities per second
   - Sub-millisecond generation
   - Tight memory constraints

4. **Simple One-Off Tasks**
   - Single-use conversion
   - Quick prototyping
   - Temporary scripts

## Template vs. Code

### Comparison

| Aspect | Template | Code |
|--------|----------|------|
| **Readability** | ✅ High | ⚠️ Medium |
| **Maintainability** | ✅ High | ⚠️ Medium |
| **Type Safety** | ✅ Filter-based | ❌ Manual |
| **Performance** | ⚠️ Medium | ✅ High |
| **Flexibility** | ⚠️ Medium | ✅ High |
| **Learning Curve** | ✅ Low | ⚠️ Medium |
| **Debugging** | ⚠️ Template errors | ✅ Standard tools |
| **Testability** | ✅ Input → Output | ✅ Unit tests |

### Hybrid Approach

Combine both for optimal results:

```javascript
// Complex logic in code
function prepareData(rawData) {
  return {
    id: generateId(rawData),
    name: normalizeName(rawData.name),
    relationships: computeRelationships(rawData),
    // ... complex transformations
  };
}

// Simple presentation in template
const rdf = await renderTemplate('entity.njk', prepareData(rawData));
```

**Best of Both:**
- Code handles complexity
- Template handles formatting
- Clear separation of concerns

## Deterministic Templates

### Why Determinism Matters

**Reproducible Builds:**
- Same input → same output
- Essential for testing
- Git-friendly (no spurious diffs)

**Content-Addressable:**
- Hash-based caching
- Deduplication
- Verification

**Debugging:**
- Eliminate non-deterministic bugs
- Reliable reproduction of issues

### Achieving Determinism

**1. Static Timestamps**
```nunjucks
{# ❌ Non-deterministic #}
Generated: {{ now() }}

{# ✅ Deterministic #}
Generated: {{ staticBuildTime }}
```

**2. Sorted Collections**
```nunjucks
{# ❌ Non-deterministic (Map order) #}
{% for key, value in map %}
  ...
{% endfor %}

{# ✅ Deterministic (Sorted) #}
{% for key in map | sort %}
  {{ key }}: {{ map[key] }}
{% endfor %}
```

**3. Deterministic IDs**
```nunjucks
{# ❌ Non-deterministic (UUID) #}
_:{{ uuid() }}

{# ✅ Deterministic (Content hash) #}
_:{{ content | hash | slice(0, 8) }}
```

**4. No Random Values**
```nunjucks
{# ❌ Non-deterministic #}
ex:sample{{ random(1000) }}

{# ✅ Deterministic #}
ex:{{ name | hash | slice(0, 4) }}
```

## Testing Templates

### Input → Output Testing

```javascript
import { renderTemplate } from '@unrdf/kgn';
import { describe, it, expect } from 'vitest';

describe('Person Template', () => {
  it('should generate valid RDF for person', async () => {
    const input = {
      id: 'alice',
      name: 'Alice Smith',
      email: 'alice@example.org'
    };

    const result = await renderTemplate('person.njk', input);

    expect(result).toContain('ex:alice');
    expect(result).toContain('foaf:Person');
    expect(result).toContain('"Alice Smith"');
    expect(result).toContain('mailto:alice@example.org');
  });

  it('should handle missing optional fields', async () => {
    const input = {
      id: 'bob',
      name: 'Bob Jones'
      // No email
    };

    const result = await renderTemplate('person.njk', input);

    expect(result).toContain('ex:bob');
    expect(result).not.toContain('foaf:mbox');
  });
});
```

### Validation Testing

```javascript
it('should produce valid Turtle syntax', async () => {
  const result = await renderTemplate('person.njk', input);

  // Parse to verify syntax
  const parser = new Parser({ format: 'text/turtle' });
  const quads = parser.parse(result);

  expect(quads.length).toBeGreaterThan(0);
});
```

### Snapshot Testing

```javascript
it('should match snapshot', async () => {
  const result = await renderTemplate('person.njk', input);
  expect(result).toMatchSnapshot();
});
```

## Performance Optimization

### 1. Template Caching

```javascript
// ✅ Cache compiled templates
const engine = new TemplateEngine({
  templatesDir: './templates',
  cache: true  // Compile once, reuse many times
});

// First render: compiles template
await engine.render('person.njk', data1);

// Subsequent renders: uses cache
await engine.render('person.njk', data2); // Faster!
```

### 2. Batch Rendering

```javascript
// ❌ Slow: Render one at a time
for (const person of people) {
  const rdf = await renderTemplate('person.njk', person);
  console.log(rdf);
}

// ✅ Fast: Batch in template
const rdf = await renderTemplate('people.njk', { people });
```

**Template:**
```nunjucks
{% for person in people %}
ex:{{ person.id }}
  a foaf:Person ;
  foaf:name {{ person.name | rdfLiteral }} .
{% endfor %}
```

### 3. Streaming

```javascript
// For very large datasets
import { Readable } from 'stream';

const dataStream = Readable.from(people);
dataStream
  .pipe(templateRenderer('person.njk'))
  .pipe(outputStream);
```

## Best Practices

### 1. Keep Templates Simple

```nunjucks
{# ✅ Good: Simple, focused template #}
ex:{{ id }}
  a foaf:Person ;
  foaf:name {{ name | rdfLiteral }} .

{# ❌ Bad: Complex logic in template #}
ex:{{ id }}
  a foaf:Person ;
  {% if name %}
    {% if name.length > 10 %}
      foaf:name {{ name | slice(0, 10) | rdfLiteral }} ;
      ex:fullName {{ name | rdfLiteral }} ;
    {% else %}
      foaf:name {{ name | rdfLiteral }} ;
    {% endif %}
  {% endif %}
  .
```

**Instead:** Move logic to data preparation

### 2. Use Meaningful Variable Names

```nunjucks
{# ✅ Good: Clear names #}
{% for person in team.members %}
  {{ person.name }}
{% endfor %}

{# ❌ Bad: Vague names #}
{% for x in y %}
  {{ x.n }}
{% endfor %}
```

### 3. Document Template Variables

```nunjucks
{#
  Template: person.njk
  Variables:
    - id (string): Person identifier
    - name (string): Full name
    - email (string, optional): Email address
    - friends (Array<string>, optional): Friend IDs
#}

ex:{{ id }}
  a foaf:Person ;
  ...
```

### 4. Version Templates

Use git to track template changes:

```bash
git log templates/person.njk
# See history of changes

git diff v1.0.0 templates/person.njk
# Compare versions
```

## Conclusion

Template-driven RDF generation provides:

- **Clarity**: Declarative output specification
- **Safety**: Type-safe filters prevent errors
- **Maintainability**: Easy to understand and modify
- **Reusability**: One template, many uses
- **Testability**: Input → output testing

Use templates when structure is known and maintainability matters. Combine with code for complex logic. Always strive for determinism and simplicity.

## See Also

- [RDF-KGN Architecture](./rdf-kgn-architecture.md)
- [Performance Optimization Strategies](./performance-optimization-strategies.md)
- [Generate RDF from Templates How-To](../how-to/generate-rdf-from-templates.md)
