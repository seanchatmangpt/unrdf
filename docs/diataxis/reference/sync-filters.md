# Template Filter Reference

Complete reference for template filters in the `unrdf sync` command.

## Overview

Template filters are Nunjucks filters used during RDF API code generation. They transform SPARQL query results into generated code by providing case conversion, RDF manipulation, type mapping, and data transformation capabilities.

## Installation

Filters are automatically available in sync templates. For programmatic use:

```javascript
import { createNunjucksEnvironment } from '@unrdf/cli/src/cli/commands/sync/template-renderer.mjs';

const env = createNunjucksEnvironment('./templates');
```

---

## Case Conversion Filters

### `camelCase`

Convert string to camelCase.

**Signature:**
```javascript
camelCase(str)
```

**Parameters:**
- `str` (string) - Input string with spaces, hyphens, or underscores

**Returns:** `string` - camelCase string with first letter lowercase

**Template Usage:**
```nunjucks
{{ "user name" | camelCase }}
{# userName #}

{{ "User-Profile" | camelCase }}
{# userProfile #}

{{ "get_all_items" | camelCase }}
{# getAllItems #}

{% for prop in properties %}
  {{ prop.name | camelCase }}: {{ prop.type | zodType }},
{% endfor %}
```

**Example Output:**
| Input | Output |
|-------|--------|
| `"user name"` | `userName` |
| `"User-Profile"` | `userProfile` |
| `"get_all_items"` | `getAllItems` |
| `"HTTPRequest"` | `httpRequest` |

---

### `pascalCase`

Convert string to PascalCase.

**Signature:**
```javascript
pascalCase(str)
```

**Parameters:**
- `str` (string) - Input string with spaces, hyphens, or underscores

**Returns:** `string` - PascalCase string with first letter uppercase

**Template Usage:**
```nunjucks
{{ "user name" | pascalCase }}
{# UserName #}

{{ "order-item" | pascalCase }}
{# OrderItem #}

export class {{ className | pascalCase }} {
  // ...
}
```

**Example Output:**
| Input | Output |
|-------|--------|
| `"user name"` | `UserName` |
| `"order-item"` | `OrderItem` |
| `"create_new_user"` | `CreateNewUser` |
| `"api"` | `Api` |

---

### `snakeCase`

Convert string to snake_case.

**Signature:**
```javascript
snakeCase(str)
```

**Parameters:**
- `str` (string) - Input string (camelCase, PascalCase, or mixed)

**Returns:** `string` - snake_case string with lowercase letters

**Template Usage:**
```nunjucks
{{ "UserName" | snakeCase }}
{# user_name #}

{{ "getOrderItems" | snakeCase }}
{# get_order_items #}

const {{ varName | snakeCase }} = getValue();
```

**Example Output:**
| Input | Output |
|-------|--------|
| `"UserName"` | `user_name` |
| `"getOrderItems"` | `get_order_items` |
| `"HTTPRequest"` | `h_t_t_p_request` |
| `"simpleTest"` | `simple_test` |

---

### `kebabCase`

Convert string to kebab-case.

**Signature:**
```javascript
kebabCase(str)
```

**Parameters:**
- `str` (string) - Input string (camelCase, PascalCase, or mixed)

**Returns:** `string` - kebab-case string with lowercase letters

**Template Usage:**
```nunjucks
{{ "UserName" | kebabCase }}
{# user-name #}

{{ "OrderItemList" | kebabCase }}
{# order-item-list #}

<div class="{{ componentName | kebabCase }}-wrapper">
```

**Example Output:**
| Input | Output |
|-------|--------|
| `"UserName"` | `user-name` |
| `"OrderItemList"` | `order-item-list` |
| `"getAPIData"` | `get-a-p-i-data` |
| `"simpleComponent"` | `simple-component` |

---

## RDF Filters

### `localName`

Extract local name from URI.

**Signature:**
```javascript
localName(uri)
```

**Parameters:**
- `uri` (string) - Full URI or prefixed name

**Returns:** `string` - Local name portion after last `#` or `/`

**Template Usage:**
```nunjucks
{{ "http://xmlns.com/foaf/0.1/Person" | localName }}
{# Person #}

{{ "http://example.org/ontology#Customer" | localName }}
{# Customer #}

{% for class in classes %}
export interface {{ class.uri | localName }} {
  // ...
}
{% endfor %}
```

**Example Output:**
| Input | Output |
|-------|--------|
| `"http://xmlns.com/foaf/0.1/Person"` | `Person` |
| `"http://example.org/ontology#Customer"` | `Customer` |
| `"https://schema.org/PostalAddress"` | `PostalAddress` |
| `"urn:isbn:0451450523"` | `0451450523` |

---

### `namespace`

Extract namespace from URI.

**Signature:**
```javascript
namespace(uri)
```

**Parameters:**
- `uri` (string) - Full URI

**Returns:** `string` - Namespace portion up to and including last `#` or `/`

**Template Usage:**
```nunjucks
{{ "http://xmlns.com/foaf/0.1/Person" | namespace }}
{# http://xmlns.com/foaf/0.1/ #}

{{ "http://example.org/ontology#Customer" | namespace }}
{# http://example.org/ontology# #}

{% set ns = classUri | namespace %}
@prefix ex: <{{ ns }}> .
```

**Example Output:**
| Input | Output |
|-------|--------|
| `"http://xmlns.com/foaf/0.1/Person"` | `http://xmlns.com/foaf/0.1/` |
| `"http://example.org/ontology#Customer"` | `http://example.org/ontology#` |
| `"https://schema.org/PostalAddress"` | `https://schema.org/` |

---

### `expand`

Expand prefixed name to full URI.

**Signature:**
```javascript
expand(prefixedName, prefixes = DEFAULT_PREFIXES)
```

**Parameters:**
- `prefixedName` (string) - CURIE in format `prefix:localName`
- `prefixes` (Object) - Optional custom prefix mappings

**Returns:** `string` - Expanded full URI, or original if no matching prefix

**Default Prefixes:**
- `rdf` - `http://www.w3.org/1999/02/22-rdf-syntax-ns#`
- `rdfs` - `http://www.w3.org/2000/01/rdf-schema#`
- `owl` - `http://www.w3.org/2002/07/owl#`
- `xsd` - `http://www.w3.org/2001/XMLSchema#`
- `foaf` - `http://xmlns.com/foaf/0.1/`
- `schema` - `https://schema.org/`

**Template Usage:**
```nunjucks
{{ "foaf:Person" | expand }}
{# http://xmlns.com/foaf/0.1/Person #}

{{ "schema:name" | expand }}
{# https://schema.org/name #}

{{ "xsd:string" | expand }}
{# http://www.w3.org/2001/XMLSchema#string #}

{% set customPrefixes = { myapp: "http://example.org/myapp/" } %}
{{ "myapp:User" | expand(customPrefixes) }}
{# http://example.org/myapp/User #}
```

**Example Output:**
| Input | Output |
|-------|--------|
| `"foaf:Person"` | `http://xmlns.com/foaf/0.1/Person` |
| `"schema:name"` | `https://schema.org/name` |
| `"xsd:integer"` | `http://www.w3.org/2001/XMLSchema#integer` |
| `"unknown:Thing"` | `unknown:Thing` (no prefix found) |

---

## Type Conversion Filters

### `zodType`

Convert XSD datatype to Zod schema.

**Signature:**
```javascript
zodType(xsdType)
```

**Parameters:**
- `xsdType` (string) - XSD datatype (with or without `xsd:` prefix)

**Returns:** `string` - Zod schema expression

**Type Mappings:**
| XSD Type | Zod Schema |
|----------|------------|
| `string` | `z.string()` |
| `integer` | `z.number().int()` |
| `int` | `z.number().int()` |
| `float` | `z.number()` |
| `boolean` | `z.boolean()` |
| `date` | `z.string().date()` |
| `anyURI` | `z.string().url()` |
| (default) | `z.string()` |

**Template Usage:**
```nunjucks
{{ "xsd:string" | zodType }}
{# z.string() #}

{{ "xsd:integer" | zodType }}
{# z.number().int() #}

{{ "http://www.w3.org/2001/XMLSchema#boolean" | zodType }}
{# z.boolean() #}

const {{ propName | camelCase }}Schema = {{ propType | zodType }};
```

**Example Output:**
| Input | Output |
|-------|--------|
| `"xsd:string"` | `z.string()` |
| `"xsd:integer"` | `z.number().int()` |
| `"xsd:boolean"` | `z.boolean()` |
| `"xsd:date"` | `z.string().date()` |
| `"xsd:anyURI"` | `z.string().url()` |

---

### `jsdocType`

Convert XSD datatype to JSDoc type annotation.

**Signature:**
```javascript
jsdocType(xsdType)
```

**Parameters:**
- `xsdType` (string) - XSD datatype (with or without `xsd:` prefix)

**Returns:** `string` - JSDoc type string

**Type Mappings:**
| XSD Type | JSDoc Type |
|----------|------------|
| `string` | `string` |
| `integer` | `number` |
| `int` | `number` |
| `float` | `number` |
| `boolean` | `boolean` |
| (default) | `string` |

**Template Usage:**
```nunjucks
{{ "xsd:string" | jsdocType }}
{# string #}

{{ "xsd:integer" | jsdocType }}
{# number #}

/**
 * @param {{ "{" }}{{ propType | jsdocType }}{{ "}" }} {{ propName | camelCase }}
 */
```

**Example Output:**
| Input | Output |
|-------|--------|
| `"xsd:string"` | `string` |
| `"xsd:integer"` | `number` |
| `"xsd:boolean"` | `boolean` |
| `"xsd:float"` | `number` |
| `"xsd:dateTime"` | `string` |

---

## Data Manipulation Filters

### `groupBy`

Group array of objects by a key.

**Signature:**
```javascript
groupBy(array, key)
```

**Parameters:**
- `array` (Array) - Array of objects to group
- `key` (string) - Property name to group by (with or without `?` prefix)

**Returns:** `Object` - Object with keys as group values and arrays as values

**Template Usage:**
```nunjucks
{% set grouped = results | groupBy('type') %}
{% for typeName, items in grouped %}
// Type: {{ typeName }}
{% for item in items %}
  {{ item.name }}
{% endfor %}
{% endfor %}
```

**Example:**
```javascript
// Input
[
  { type: 'Person', name: 'Alice' },
  { type: 'Person', name: 'Bob' },
  { type: 'Organization', name: 'Acme' }
]

// Output (groupBy 'type')
{
  'Person': [
    { type: 'Person', name: 'Alice' },
    { type: 'Person', name: 'Bob' }
  ],
  'Organization': [
    { type: 'Organization', name: 'Acme' }
  ]
}
```

---

### `distinctValues`

Extract unique values for a key from array.

**Signature:**
```javascript
distinctValues(array, key)
```

**Parameters:**
- `array` (Array) - Array of objects
- `key` (string) - Property name to extract (with or without `?` prefix)

**Returns:** `Array` - Array of unique values (nulls filtered out)

**Template Usage:**
```nunjucks
{% set types = results | distinctValues('type') %}
{% for type in types %}
import { {{ type | pascalCase }} } from './{{ type | kebabCase }}';
{% endfor %}
```

**Example:**
```javascript
// Input
[
  { type: 'Person', name: 'Alice' },
  { type: 'Person', name: 'Bob' },
  { type: 'Organization', name: 'Acme' }
]

// Output (distinctValues 'type')
['Person', 'Organization']
```

---

### `sortBy`

Sort array of objects by a key.

**Signature:**
```javascript
sortBy(array, key, direction = 'asc')
```

**Parameters:**
- `array` (Array) - Array of objects to sort
- `key` (string) - Property name to sort by (with or without `?` prefix)
- `direction` (string) - Sort direction: `'asc'` (default) or `'desc'`

**Returns:** `Array` - Sorted array (new array, original unchanged)

**Template Usage:**
```nunjucks
{% set sorted = results | sortBy('name') %}
{% for item in sorted %}
  {{ item.name }}
{% endfor %}

{% set descending = results | sortBy('priority', 'desc') %}
```

**Example:**
```javascript
// Input
[
  { name: 'Charlie', priority: 1 },
  { name: 'Alice', priority: 3 },
  { name: 'Bob', priority: 2 }
]

// Output (sortBy 'name')
[
  { name: 'Alice', priority: 3 },
  { name: 'Bob', priority: 2 },
  { name: 'Charlie', priority: 1 }
]

// Output (sortBy 'priority', 'desc')
[
  { name: 'Alice', priority: 3 },
  { name: 'Bob', priority: 2 },
  { name: 'Charlie', priority: 1 }
]
```

---

### `keys`

Get object keys as array.

**Signature:**
```javascript
keys(obj)
```

**Parameters:**
- `obj` (Object) - Input object

**Returns:** `Array` - Array of keys

**Template Usage:**
```nunjucks
{% set propertyNames = entity | keys %}
{% for name in propertyNames %}
  {{ name }}: any;
{% endfor %}
```

**Example:**
```javascript
// Input
{ name: 'Alice', age: 30, email: 'alice@example.org' }

// Output
['name', 'age', 'email']
```

---

### `values`

Get object values as array.

**Signature:**
```javascript
values(obj)
```

**Parameters:**
- `obj` (Object) - Input object

**Returns:** `Array` - Array of values

**Template Usage:**
```nunjucks
{% set allValues = config | values %}
const defaults = [{{ allValues | join(', ') }}];
```

**Example:**
```javascript
// Input
{ name: 'Alice', age: 30, active: true }

// Output
['Alice', 30, true]
```

---

### `items`

Get object entries as array of [key, value] pairs.

**Signature:**
```javascript
items(obj)
```

**Parameters:**
- `obj` (Object) - Input object

**Returns:** `Array` - Array of [key, value] pairs

**Template Usage:**
```nunjucks
{% for key, value in entity | items %}
  {{ key }}: {{ value | quote }},
{% endfor %}
```

**Example:**
```javascript
// Input
{ name: 'Alice', age: 30 }

// Output
[['name', 'Alice'], ['age', 30]]
```

---

## String Filters

### `indent`

Add indentation to each line of a string.

**Signature:**
```javascript
indent(str, spaces = 2)
```

**Parameters:**
- `str` (string) - Input string (may contain newlines)
- `spaces` (number) - Number of spaces to indent (default: 2)

**Returns:** `string` - Indented string

**Template Usage:**
```nunjucks
{{ multilineCode | indent }}
{# Each line indented by 2 spaces #}

{{ nestedCode | indent(4) }}
{# Each line indented by 4 spaces #}

class {{ className }} {
{{ classBody | indent(2) }}
}
```

**Example:**
```javascript
// Input
`line1
line2
line3`

// Output (indent 2)
`  line1
  line2
  line3`

// Output (indent 4)
`    line1
    line2
    line3`
```

---

### `quote`

Wrap string in quotes with escaping.

**Signature:**
```javascript
quote(str, quoteChar = '"')
```

**Parameters:**
- `str` (string) - Input string
- `quoteChar` (string) - Quote character (default: `"`)

**Returns:** `string` - Quoted string with internal quotes escaped

**Template Usage:**
```nunjucks
{{ variableName | quote }}
{# "variableName" #}

{{ "it's a test" | quote("'") }}
{# 'it\'s a test' #}

const message = {{ userMessage | quote }};
```

**Example:**
```javascript
// Input: hello
// Output: "hello"

// Input: say "hello"
// Output: "say \"hello\""

// Input: it's (with quote char ')
// Output: 'it\'s'
```

---

### `date`

Format date using pattern.

**Signature:**
```javascript
date(dateValue, format = 'YYYY-MM-DD')
```

**Parameters:**
- `dateValue` (Date|string) - Date object or string (uses current date if invalid)
- `format` (string) - Format pattern with tokens:
  - `YYYY` - 4-digit year
  - `MM` - 2-digit month (01-12)
  - `DD` - 2-digit day (01-31)
  - `HH` - 2-digit hour (00-23)
  - `mm` - 2-digit minute (00-59)
  - `ss` - 2-digit second (00-59)

**Returns:** `string` - Formatted date string

**Template Usage:**
```nunjucks
{{ now | date }}
{# 2024-01-15 #}

{{ now | date('YYYY-MM-DD HH:mm:ss') }}
{# 2024-01-15 14:30:45 #}

{{ createdAt | date('MM/DD/YYYY') }}
{# 01/15/2024 #}

// Generated on {{ now | date('YYYY-MM-DD') }}
```

**Example:**
```javascript
// Input: new Date('2024-06-15T14:30:45')

// Output (default): "2024-06-15"
// Output ('YYYY-MM-DD HH:mm:ss'): "2024-06-15 14:30:45"
// Output ('MM/DD/YYYY'): "06/15/2024"
// Output ('YYYY'): "2024"
```

---

## Filter Combinations

### Common Patterns

#### Pattern 1: Generate TypeScript Interface from SPARQL Results

```nunjucks
{% set classes = results | groupBy('class') %}
{% for className, properties in classes %}
export interface {{ className | localName | pascalCase }} {
{% for prop in properties | sortBy('name') %}
  {{ prop.name | localName | camelCase }}: {{ prop.type | jsdocType }};
{% endfor %}
}
{% endfor %}
```

#### Pattern 2: Generate Zod Schema

```nunjucks
{% set entities = results | distinctValues('entity') %}
{% for entity in entities %}
export const {{ entity | localName | pascalCase }}Schema = z.object({
{% set props = results | groupBy('entity') %}
{% for prop in props[entity] %}
  {{ prop.name | localName | camelCase }}: {{ prop.type | zodType }},
{% endfor %}
});
{% endfor %}
```

#### Pattern 3: Generate API Client Methods

```nunjucks
{% for operation in results | sortBy('name') %}
/**
 * {{ operation.description | default('No description') }}
 * @param {Object} params - Request parameters
{% for param in operation.params %}
 * @param {{ "{" }}{{ param.type | jsdocType }}{{ "}" }} params.{{ param.name | camelCase }}
{% endfor %}
 */
async {{ operation.name | camelCase }}(params) {
  return this.request('{{ operation.method }}', '{{ operation.path }}', params);
}
{% endfor %}
```

#### Pattern 4: Generate File Header with Metadata

```nunjucks
/**
 * @file {{ filename | pascalCase }}
 * @description Auto-generated from RDF ontology
 * @generated {{ now | date('YYYY-MM-DD HH:mm:ss') }}
 * @source {{ sourceUri | localName }}
 */

import { z } from 'zod';

{% for entity in entities | sortBy('name') %}
{{ entity | indent(0) }}
{% endfor %}
```

---

## Error Handling

### Null/Undefined Values

All filters handle null/undefined gracefully:

```javascript
camelCase(null)       // ''
pascalCase(undefined) // ''
localName(null)       // ''
zodType(null)         // 'z.string()'
groupBy(null, 'key')  // {}
keys(null)            // []
```

### Empty Arrays

Data manipulation filters return empty results for empty arrays:

```javascript
groupBy([], 'key')        // {}
distinctValues([], 'key') // []
sortBy([], 'key')         // []
```

### Missing Keys

SPARQL results may use `?` prefix for variable names. Filters check both forms:

```javascript
// Both work identically
groupBy(results, 'name')   // Checks 'name' and '?name'
groupBy(results, '?name')  // Checks '?name' and 'name'
```

---

## See Also

- [Sync Command Reference](./cli-commands.md) - CLI sync command documentation
- [RDF Filters Reference](./rdf-filters-reference.md) - Additional RDF template filters
- [RDF-KGN API](./rdf-kgn-api.md) - Template engine API
- [Generate Code from RDF How-To](../how-to/generate-code-from-rdf.md) - Step-by-step guide

---

## Source Reference

Filters are implemented in:
```
packages/cli/src/cli/commands/sync/template-renderer.mjs
```

Function: `createNunjucksEnvironment()` (lines 62-104)
