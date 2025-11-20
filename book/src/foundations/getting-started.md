# Getting Started

Let's build your first UNRDF + Next.js application in 15 minutes.

## Quick Setup

### 1. Create Next.js Project

```bash
npx create-next-app@latest unrdf-demo --typescript --app --use-pnpm
cd unrdf-demo
```

### 2. Install UNRDF

```bash
pnpm add unrdf
```

### 3. Create Knowledge Engine

```typescript
// lib/knowledge-engine.ts
import { createKnowledgeEngine } from 'unrdf';

let engineInstance: ReturnType<typeof createKnowledgeEngine> | null = null;

export async function getKnowledgeEngine() {
  if (!engineInstance) {
    engineInstance = await createKnowledgeEngine({
      enableObservability: true,
      enableBrowserStorage: true
    });
  }
  return engineInstance;
}
```

### 4. Your First Server Component

```typescript
// app/page.tsx
import { getKnowledgeEngine } from '@/lib/knowledge-engine';
import { namedNode, literal } from '@rdfjs/data-model';

export default async function Home() {
  const engine = await getKnowledgeEngine();
  
  // Insert some data
  await engine.insert([
    {
      subject: namedNode('http://example.org/alice'),
      predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
      object: literal('Alice'),
      graph: namedNode('http://example.org/graph1')
    }
  ]);
  
  // Query it
  const results = await engine.query(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name WHERE {
      ?person foaf:name ?name .
    }
  `);
  
  return (
    <main>
      <h1>UNRDF + Next.js Demo</h1>
      <ul>
        {results.map((row, i) => (
          <li key={i}>{row.name.value}</li>
        ))}
      </ul>
    </main>
  );
}
```

### 5. Run It

```bash
pnpm dev
```

Visit http://localhost:3000 — you just queried a knowledge graph from a Server Component!

## What Just Happened?

1. **Server-Side Knowledge Graph:** Engine runs on Next.js server
2. **Type-Safe Queries:** SPARQL results are properly typed
3. **Automatic Caching:** Query results cached for performance
4. **Built-in Observability:** OpenTelemetry spans created automatically

## Next Steps

- **[Add Real-time Updates](./nextjs-integration.md)** with Knowledge Hooks
- **[Enable Client Storage](./first-kg.md)** with IndexedDB
- **[Add Validation](../enterprise/auth.md)** with Policy Packs

**→ [Next: Next.js Integration Patterns](./nextjs-integration.md)**
