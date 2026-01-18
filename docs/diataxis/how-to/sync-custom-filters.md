# How to Create Custom Template Filters for Code Generation

**Goal:** Add custom Nunjucks filters for project-specific transformations
**Time:** 10-15 minutes
**Difficulty:** Intermediate

## Problem

The built-in template filters handle common cases, but your project needs domain-specific transformations like pluralizing entity names, mapping RDF types to database columns, or generating REST API patterns.

## Solution

Create a custom filters module and register it with the template engine before rendering.

## Prerequisites

- `@unrdf/cli` package installed
- Familiarity with the `unrdf sync` command

## Step 1: Identify the Transformation Needed

Define the input/output contract clearly before writing a filter.

| Filter | Input | Output | Use Case |
|--------|-------|--------|----------|
| `pluralize` | `"Person"` | `"People"` | Collection names |
| `sqlType` | `"xsd:integer"` | `"INTEGER"` | Database columns |
| `httpMethod` | `"create"` | `"POST"` | REST endpoints |
| `routePath` | `"UserProfile"` | `"/user-profiles"` | URL patterns |

## Step 2: Create a Filters Module

Create `templates/filters.mjs`:

```javascript
/**
 * @file Custom Template Filters
 * @module templates/filters
 */

/** Pluralize an English word */
export function pluralize(word) {
  if (!word) return '';
  const str = String(word);
  const lower = str.toLowerCase();

  const irregulars = { person: 'people', child: 'children', datum: 'data', index: 'indices' };
  if (irregulars[lower]) {
    return str[0] === str[0].toUpperCase()
      ? irregulars[lower].charAt(0).toUpperCase() + irregulars[lower].slice(1)
      : irregulars[lower];
  }

  if (/[sxz]$|[^aeiou]ch$|sh$/i.test(str)) return str + 'es';
  if (/[^aeiou]y$/i.test(str)) return str.slice(0, -1) + 'ies';
  return str + 's';
}

/** Map RDF/XSD types to SQL column types */
export function sqlType(xsdType) {
  const normalized = String(xsdType || 'string')
    .replace(/^xsd:|^http:\/\/www\.w3\.org\/2001\/XMLSchema#/, '');

  const mapping = {
    string: 'TEXT', integer: 'INTEGER', int: 'INTEGER', long: 'BIGINT',
    decimal: 'DECIMAL(18,6)', float: 'REAL', double: 'DOUBLE PRECISION',
    boolean: 'BOOLEAN', date: 'DATE', dateTime: 'TIMESTAMP', anyURI: 'TEXT',
  };
  return mapping[normalized] || 'TEXT';
}

/** Map CRUD operations to HTTP methods */
export function httpMethod(operation) {
  const mapping = {
    create: 'POST', read: 'GET', get: 'GET', update: 'PUT',
    patch: 'PATCH', delete: 'DELETE', list: 'GET', search: 'GET',
  };
  return mapping[String(operation || '').toLowerCase()] || 'GET';
}

/** Convert entity name to URL route path */
export function routePath(entityName) {
  if (!entityName) return '/';
  const kebab = String(entityName)
    .replace(/([A-Z])/g, '-$1').toLowerCase().replace(/^-/, '');
  return '/' + pluralize(kebab);
}

export const customFilters = { pluralize, sqlType, httpMethod, routePath };
export default customFilters;
```

## Step 3: Register Filters in Sync Configuration

Create `sync.config.mjs`:

```javascript
import { customFilters } from './templates/filters.mjs';

export default {
  rdfSource: './ontology.ttl',
  templatesDir: './templates',
  outputDir: './generated',
  filters: customFilters,
};
```

## Step 4: Use Filters in Templates

Create `templates/entity.njk`:

```nunjucks
---
to: "{{ entityName | kebabCase }}.mjs"
---
import { z } from 'zod';

export const {{ entityName }}Schema = z.object({
{% for prop in properties %}
  {{ prop.name | camelCase }}: {{ prop.type | zodType }},
{% endfor %}
});

export const COLLECTION_NAME = '{{ entityName | pluralize }}';
export const API_ENDPOINT = '{{ entityName | routePath }}';
```

Create `templates/migration.njk`:

```nunjucks
---
to: "migrations/{{ now | date('YYYYMMDD') }}_create_{{ entityName | snakeCase }}.sql"
---
CREATE TABLE IF NOT EXISTS {{ entityName | snakeCase | pluralize }} (
  id SERIAL PRIMARY KEY,
{% for prop in properties %}
  {{ prop.name | snakeCase }} {{ prop.type | sqlType }}{% if not loop.last %},{% endif %}

{% endfor %}
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

Create `templates/routes.njk`:

```nunjucks
---
to: "routes/{{ entityName | kebabCase }}.mjs"
---
import { Router } from 'express';

const router = Router();

// {{ 'list' | httpMethod }} {{ entityName | routePath }}
router.get('/', async (req, res) => { /* List {{ entityName | pluralize }} */ });

// {{ 'create' | httpMethod }} {{ entityName | routePath }}
router.post('/', async (req, res) => { /* Create {{ entityName }} */ });

// {{ 'read' | httpMethod }} {{ entityName | routePath }}/:id
router.get('/:id', async (req, res) => { /* Get {{ entityName }} */ });

// {{ 'update' | httpMethod }} {{ entityName | routePath }}/:id
router.put('/:id', async (req, res) => { /* Update {{ entityName }} */ });

// {{ 'delete' | httpMethod }} {{ entityName | routePath }}/:id
router.delete('/:id', async (req, res) => { /* Delete {{ entityName }} */ });

export default router;
```

## Step 5: Test the Output

Run sync in dry-run mode:

```bash
timeout 10s unrdf sync --dry-run --verbose
```

Verify generated content:

```bash
timeout 10s unrdf sync && cat generated/person.mjs | head -15
```

**Expected output:**

```javascript
import { z } from 'zod';

export const PersonSchema = z.object({
  name: z.string(),
  age: z.number().int(),
});

export const COLLECTION_NAME = 'People';
export const API_ENDPOINT = '/people';
```

## Troubleshooting

| Problem | Symptom | Solution |
|---------|---------|----------|
| Filter not found | `Error: filter "pluralize" not found` | Verify export and import in `sync.config.mjs` |
| Returns undefined | Empty output | Add null check: `if (!word) return '';` |
| Wrong SQL type | All columns are TEXT | Strip XSD prefix before lookup |

## Related Guides

- [Generate RDF from Templates](./generate-rdf-from-templates.md)
- [RDF Filters Reference](../reference/rdf-filters-reference.md)
