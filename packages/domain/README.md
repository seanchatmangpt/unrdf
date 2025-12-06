# @unrdf/domain

![Version](https://img.shields.io/badge/version-5.0.0--beta.1-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)


Domain models, type definitions, and shared schemas for UNRDF.

**Status:** Internal use only (private package)

## Purpose

This package provides shared type definitions and domain models used across UNRDF:

- Zod schemas for input validation
- TypeScript type definitions (for JSDoc)
- Constant definitions
- Shared interfaces

## Installation

This is a private package. Internal use only:

```bash
pnpm --filter @unrdf/domain install
```

## Usage

### Using Domain Types

```javascript
import {
  StoreConfigSchema,
  QueryResultSchema,
  ShapeSchema
} from '@unrdf/domain';

// Validate input against schema
const config = StoreConfigSchema.parse({
  backend: 'memory',
  persistence: false
});
```

### Type Definitions

```javascript
// Type annotations via JSDoc
import { StoreConfigSchema } from '@unrdf/domain';

/**
 * Create a knowledge substrate
 * @param {StoreConfigSchema} config - Store configuration
 * @returns {Promise<KnowledgeSubstrate>}
 */
export async function createKnowledgeSubstrateCore(config) {
  // Implementation
}
```

## Contents

```
src/
├── schemas/
│   ├── store.mjs         # Store configuration schemas
│   ├── query.mjs         # Query and result schemas
│   ├── shapes.mjs        # SHACL shape schemas
│   └── hooks.mjs         # Knowledge hook schemas
├── constants/
│   ├── namespaces.mjs    # RDF namespaces
│   ├── prefixes.mjs      # Common prefixes
│   └── limits.mjs        # System limits
└── index.mjs             # Main exports
```

## Schema Examples

### Store Schema

```javascript
import { StoreConfigSchema } from '@unrdf/domain';
import { z } from 'zod';

const schema = StoreConfigSchema.extend({
  backend: z.enum(['memory', 'oxigraph', 'remote'])
});

const config = schema.parse({
  backend: 'oxigraph',
  persistence: true
});
```

### Query Result Schema

```javascript
import { QueryResultSchema } from '@unrdf/domain';

const results = QueryResultSchema.array().parse([
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
]);
```

## Constants

```javascript
import { RDF, RDFS, OWL, FOAF } from '@unrdf/domain';

// Standard RDF namespaces
const type = RDF.type;
const label = RDFS.label;
const Class = OWL.Class;
const name = FOAF.name;
```

## Adding New Domain Types

1. **Create schema file** in `src/schemas/`
2. **Export from index.mjs**
3. **Add tests** in `test/`
4. **Update documentation**

Example:

```javascript
// src/schemas/custom.mjs
import { z } from 'zod';

export const CustomSchema = z.object({
  id: z.string().uuid(),
  data: z.record(z.string(), z.any())
});

// src/index.mjs
export { CustomSchema } from './schemas/custom.mjs';
```

## See Also

- [WORKSPACE-STRUCTURE.md](../../docs/WORKSPACE-STRUCTURE.md) - Type organization
- [PACKAGE-DEVELOPMENT.md](../../docs/PACKAGE-DEVELOPMENT.md) - Adding types
- Individual package `src/types/` for usage
