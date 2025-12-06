# How to Generate TypeScript from RDF Data

This guide shows you how to generate TypeScript code from RDF vocabulary definitions.

## Problem

You have an RDF vocabulary and want to generate TypeScript interfaces that match your RDF schema.

## Solution

Use @unrdf/kgn templates with RDF filters to transform RDF classes into TypeScript interfaces.

## Steps

### 1. Define Your RDF Vocabulary in Frontmatter

```yaml
---
prefixes:
  ex: "http://example.org/vocab#"
  xsd: "http://www.w3.org/2001/XMLSchema#"

classes:
  - uri: "ex:Book"
    properties:
      - name: "title"
        uri: "ex:title"
        type: "xsd:string"
        required: true
      - name: "author"
        uri: "ex:author"
        type: "ex:Person"
        required: true
      - name: "published"
        uri: "ex:published"
        type: "xsd:date"
        required: false
---
```

### 2. Create the Template

```nunjucks
{% for cls in classes %}
export interface {{ cls.uri | contract(prefixes) | replace(':', '') }} {
  '@id': string;
  '@type': '{{ cls.uri }}';
{% for prop in cls.properties %}
  {{ prop.name }}{{ '?' if not prop.required }}: {{ prop.type | toTypeScript }};
{% endfor %}
}
{% endfor %}
```

### 3. Add Custom TypeScript Type Mapping Filter

```javascript
env.addFilter('toTypeScript', function(rdfType) {
  const typeMap = {
    'xsd:string': 'string',
    'xsd:integer': 'number',
    'xsd:decimal': 'number',
    'xsd:boolean': 'boolean',
    'xsd:date': 'Date',
    'xsd:dateTime': 'Date',
  };

  // If it's an XSD type, map it
  if (typeMap[rdfType]) {
    return typeMap[rdfType];
  }

  // If it's a custom class, use the class name
  return rdfType.split(':')[1] || rdfType;
});
```

### 4. Render

```javascript
import { FrontmatterParser } from '@unrdf/kgn';
import { contract } from '@unrdf/kgn/filters';
import nunjucks from 'nunjucks';

const env = new nunjucks.Environment(null, { autoescape: false });
env.addFilter('contract', contract);
env.addFilter('toTypeScript', /* filter from step 3 */);

const parser = new FrontmatterParser();
const parsed = parser.parse(template);
const output = env.renderString(parsed.content, parsed.frontmatter);
```

## Result

```typescript
export interface Book {
  '@id': string;
  '@type': 'ex:Book';
  title: string;
  author: Person;
  published?: Date;
}
```

## Variations

### Generate Zod Schemas

```nunjucks
import { z } from 'zod';

{% for cls in classes %}
export const {{ cls.uri | contract(prefixes) | replace(':', '') }}Schema = z.object({
  '@id': z.string().url(),
  '@type': z.literal('{{ cls.uri }}'),
{% for prop in cls.properties %}
  {{ prop.name }}: {{ prop.type | toZod }}{{ '.optional()' if not prop.required }},
{% endfor %}
});
{% endfor %}
```

### Generate JSON Schema

```nunjucks
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "definitions": {
{% for cls in classes %}
    "{{ cls.uri | contract(prefixes) }}": {
      "type": "object",
      "properties": {
        "@id": { "type": "string", "format": "uri" },
        "@type": { "const": "{{ cls.uri }}" }
{% for prop in cls.properties %}
        ,"{{ prop.name }}": {{ prop.type | toJsonSchema }}
{% endfor %}
      },
      "required": [{{ classes[0].properties | selectattr('required') | map(attribute='name') | map('rdfLiteral') | join(', ') }}]
    }{{ ',' if not loop.last }}
{% endfor %}
  }
}
```

## Best Practices

1. **Use contract filter** to convert URIs to CURIEs for cleaner names
2. **Create type mapping filters** for common XSD types
3. **Validate required fields** to ensure type safety
4. **Generate both types and validators** (TypeScript + Zod)
5. **Add @generated comments** to indicate machine-generated code

## See Also

- [How to Generate SHACL Shapes](./generate-shacl-shapes.md)
- [Reference: RDF Filters](../reference/filters.md)
- [Explanation: Type Mapping Strategies](../explanation/type-mapping.md)
