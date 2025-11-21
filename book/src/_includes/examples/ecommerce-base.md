# E-commerce Product Example (Shared)

This base example is used across multiple chapters. Each chapter extends it for specific use cases.

## Base Schema

```typescript
import { z } from 'zod';

export const ProductSchema = z.object({
  id: z.string().url(),
  name: z.string().min(1).max(200),
  price: z.number().positive(),
  inStock: z.boolean(),
  category: z.enum(['electronics', 'clothing', 'food', 'books']),
  tags: z.array(z.string()).default([]),
  ratings: z.object({
    average: z.number().min(0).max(5),
    count: z.number().int().nonnegative()
  }).optional()
});

export type Product = z.infer<typeof ProductSchema>;
```

## Base RDF Data

```turtle
@prefix schema: <http://schema.org/> .
@prefix ex: <http://example.org/> .

ex:product1
  a schema:Product ;
  schema:name "Wireless Mouse" ;
  schema:price 29.99 ;
  schema:inStock true ;
  schema:category "electronics" ;
  schema:tag "wireless", "computer" .
```

## Usage Across Chapters

- **Reactive KG**: Live price updates with `useKnowledgeHook()`
- **Type-Safe SPARQL**: Product search with validated results
- **Hooks Mastery**: Order validation and processing

See each chapter for specific implementations.
