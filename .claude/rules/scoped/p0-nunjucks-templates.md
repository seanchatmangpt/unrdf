# Scoped Rule: Nunjucks Template Patterns

**Scope**: All `.njk` template files

## Filter Conventions

### Case Filters

Use `{{ value|camelCase }}`, not `{{ value|camel_case }}`:

```nunjucks
{# Correct #}
{{ entity.name|camelCase }}
{{ entity.name|pascalCase }}

{# Wrong - inconsistent naming #}
{{ entity.name|camel_case }}
```

### Type Conversion Filters

For Zod/JSDoc type mapping:

```nunjucks
{# Correct - use zodType/jsdocType #}
{{ property.xsdType|zodType }}
{{ property.xsdType|jsdocType }}

{# Wrong - manual mapping #}
{% if property.xsdType == 'xsd:string' %}z.string(){% endif %}
```

## Template Structure

### Frontmatter with Auto-Quoting

Values containing `{{ }}` are auto-quoted. Write cleaner templates:

```yaml
{# Correct - auto-quoted during processing #}
---
to: {{ output_dir }}/generated/{{ entity.name }}.mjs
mode: overwrite
---

{# Also valid - manually quoted (backward compatible) #}
---
to: "{{ output_dir }}/generated/{{ entity.name }}.mjs"
mode: overwrite
---
```

### Block Balance

Every `{% for %}` has `{% endfor %}`, every `{% if %}` has `{% endif %}`:

```nunjucks
{# Correct #}
{% for entity in entities %}
  {% if entity.active %}
    {{ entity.name }}
  {% endif %}
{% endfor %}

{# Wrong - missing endfor #}
{% for entity in entities %}
  {% if entity.active %}
    {{ entity.name }}
  {% endif %}
{# Missing {% endfor %} #}
```

## Template Discovery

Templates live in category directories under `packages/cli/templates/sync/`:

```
packages/cli/templates/sync/
├── openapi/     # OpenAPI spec templates
├── zod/         # Zod schema templates
└── types/       # JSDoc type definitions
```

## Testing

Templates are tested in `packages/cli/test/sync/templates.test.mjs`:

- All 11 templates must exist
- Frontmatter must have `to` field with `{{` template syntax
- Body must be non-empty
- Nunjucks blocks must be balanced
