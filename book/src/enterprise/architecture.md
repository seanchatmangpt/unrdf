# Enterprise Architecture

Building production-grade Next.js applications with UNRDF requires thoughtful architecture. This chapter covers battle-tested patterns from real enterprise deployments.

## Reference Architecture

```
┌──────────────────────────────────────────────────────────┐
│                     CLIENT (Browser)                     │
├──────────────────────────────────────────────────────────┤
│  React Components (RSC + Client Components)             │
│  ├─ Server Components: Data fetching + rendering        │
│  ├─ Client Components: Interactivity + real-time       │
│  └─ Knowledge Hooks: useKnowledgeHook()                │
├──────────────────────────────────────────────────────────┤
│  IndexedDB Store (Offline-first cache)                  │
│  └─ Syncs with server via Knowledge Hooks              │
└──────────────────────────────────────────────────────────┘
                            ↓ HTTP/WebSocket
┌──────────────────────────────────────────────────────────┐
│              EDGE RUNTIME (Vercel Edge)                  │
├──────────────────────────────────────────────────────────┤
│  API Routes (/api/knowledge/*)                          │
│  ├─ SPARQL query endpoint                               │
│  ├─ Mutation endpoint (insert/delete)                   │
│  ├─ Semantic search endpoint                            │
│  └─ Real-time subscription endpoint                     │
├──────────────────────────────────────────────────────────┤
│  Knowledge Engine (UNRDF)                               │
│  ├─ Transaction Manager (ACID)                          │
│  ├─ Query Optimizer                                     │
│  ├─ Policy Pack Validator                               │
│  └─ Change Feed Publisher                               │
└──────────────────────────────────────────────────────────┘
                            ↓
┌──────────────────────────────────────────────────────────┐
│              NODE.JS RUNTIME (Server/Serverless)         │
├──────────────────────────────────────────────────────────┤
│  Knowledge Hooks Executor                                │
│  ├─ Pre-transaction hooks                               │
│  ├─ Post-transaction hooks                              │
│  ├─ Effect sandbox (isolated-vm)                        │
│  └─ Observability (OpenTelemetry)                       │
├──────────────────────────────────────────────────────────┤
│  Distributed Features (Optional)                         │
│  ├─ Federation Manager                                   │
│  ├─ Consensus Protocol (Byzantine FTB)                  │
│  └─ Multi-region replication                            │
└──────────────────────────────────────────────────────────┘
                            ↓
┌──────────────────────────────────────────────────────────┐
│                    PERSISTENT STORAGE                     │
├──────────────────────────────────────────────────────────┤
│  Primary Store (Postgres/Supabase)                      │
│  ├─ RDF quads table                                     │
│  ├─ Indexes (S, P, O, G)                               │
│  └─ Full-text search (pg_trgm)                         │
├──────────────────────────────────────────────────────────┤
│  Vector Store (pgvector/Pinecone)                       │
│  └─ Embeddings for semantic search                      │
├──────────────────────────────────────────────────────────┤
│  Audit Trail (Lockchain)                                │
│  └─ Cryptographic provenance                            │
└──────────────────────────────────────────────────────────┘
```

## Core Patterns

### Pattern 1: Server Component Data Fetching

**Use Case:** Initial page load with SEO-optimized content

```typescript
// app/products/[id]/page.tsx
import { createKnowledgeEngine } from 'unrdf';
import { Suspense } from 'react';

export default async function ProductPage({ params }: { params: { id: string } }) {
  const engine = await createKnowledgeEngine();
  
  // Fetched on server, streamed to client
  const product = await engine.queryTyped({
    query: `
      PREFIX schema: <http://schema.org/>
      SELECT ?name ?description ?price WHERE {
        <${params.id}> 
          schema:name ?name ;
          schema:description ?description ;
          schema:price ?price .
      }
    `,
    schema: ProductSchema
  });

  return (
    <div>
      <h1>{product.name}</h1>
      <p>{product.description}</p>
      
      {/* Real-time components wrapped in Suspense */}
      <Suspense fallback={<PriceSkeleton />}>
        <LivePrice productId={params.id} initialPrice={product.price} />
      </Suspense>
    </div>
  );
}
```

**Benefits:**
✅ SEO-friendly (fully rendered HTML)
✅ Fast initial load (server-side rendering)
✅ Progressive enhancement (real-time updates add later)

### Pattern 2: API Route for Mutations

**Use Case:** Client-triggered data changes with ACID guarantees

```typescript
// app/api/products/route.ts
import { createKnowledgeEngine } from 'unrdf';
import { namedNode, literal } from '@rdfjs/data-model';
import { NextResponse } from 'next/server';

export async function POST(request: Request) {
  const engine = await createKnowledgeEngine();
  const { name, price, description } = await request.json();
  
  // ACID transaction automatically
  const productId = `http://example.org/product/${Date.now()}`;
  
  await engine.insert([
    {
      subject: namedNode(productId),
      predicate: namedNode('http://schema.org/name'),
      object: literal(name),
      graph: namedNode('http://example.org/products')
    },
    {
      subject: namedNode(productId),
      predicate: namedNode('http://schema.org/price'),
      object: literal(price),
      graph: namedNode('http://example.org/products')
    }
  ]);
  
  return NextResponse.json({ productId });
}
```

**Benefits:**
✅ ACID transactions built-in
✅ Automatic validation via Policy Packs
✅ OpenTelemetry spans for monitoring
✅ Change feed notifications to all subscribers

### Pattern 3: Real-time Client Component

**Use Case:** Live data updates without polling

```typescript
// components/LivePrice.tsx
'use client';
import { useKnowledgeHook } from 'unrdf/react';

export function LivePrice({ productId, initialPrice }: Props) {
  const { data, loading, error } = useKnowledgeHook({
    hookId: 'product-price-updates',
    filter: { productId },
    fallback: initialPrice
  });
  
  if (loading) return <PriceSkeleton />;
  if (error) return <PriceError error={error} />;
  
  return (
    <div className="price">
      ${data.price}
      <span className="badge">LIVE</span>
    </div>
  );
}
```

**Benefits:**
✅ Real-time updates (no polling)
✅ Optimistic UI with rollback
✅ Automatic reconnection
✅ Type-safe data

### Pattern 4: Edge Function for Global Performance

**Use Case:** <50ms query latency worldwide

```typescript
// app/api/search/route.ts
export const runtime = 'edge'; // Deploy to 300+ locations

import { createKnowledgeEngine } from 'unrdf';

export async function GET(request: Request) {
  const { searchParams } = new URL(request.url);
  const query = searchParams.get('q');
  
  const engine = await createKnowledgeEngine({
    enableEdgeCache: true, // Multi-region caching
    enableVectorSearch: true
  });
  
  // Semantic search with vector embeddings
  const results = await engine.semanticSearch({
    query,
    limit: 10,
    minSimilarity: 0.7
  });
  
  return Response.json(results);
}
```

**Benefits:**
✅ <50ms global latency
✅ Automatic geo-routing
✅ Built-in caching
✅ No cold starts

## Enterprise Considerations

### Multi-tenancy

```typescript
// Tenant isolation via graph namespacing
const tenantGraph = namedNode(`http://example.org/tenant/${tenantId}`);

await engine.insert(quads, { graph: tenantGraph });

// Queries automatically scoped to tenant
const results = await engine.query(sparql, { 
  defaultGraph: tenantGraph 
});
```

### Authentication

```typescript
// Integration with NextAuth.js
import { getServerSession } from 'next-auth';

export default async function handler(req, res) {
  const session = await getServerSession(req, res, authOptions);
  
  if (!session) {
    return res.status(401).json({ error: 'Unauthorized' });
  }
  
  // Apply row-level security via Policy Packs
  const engine = await createKnowledgeEngine({
    policyPack: await getUserPolicyPack(session.user.id)
  });
  
  // Queries respect user permissions automatically
  const results = await engine.query(sparql);
  res.json(results);
}
```

### Observability

```typescript
// Automatic OpenTelemetry integration
const engine = await createKnowledgeEngine({
  observability: {
    serviceName: 'knowledge-api',
    exporters: ['jaeger', 'prometheus'],
    sampleRate: 0.1 // 10% sampling in production
  }
});

// Every query creates spans automatically
await engine.query(sparql); // Traced end-to-end
```

## Performance Optimization

### Query Caching

```typescript
// Intelligent caching with automatic invalidation
const engine = await createKnowledgeEngine({
  cache: {
    enabled: true,
    ttl: 3600, // 1 hour default
    maxSize: '500MB',
    strategy: 'intelligent' // Invalidates on related changes
  }
});
```

### Connection Pooling

```typescript
// Singleton pattern for connection reuse
let engineInstance: KnowledgeEngine | null = null;

export async function getEngine() {
  if (!engineInstance) {
    engineInstance = await createKnowledgeEngine({
      poolSize: 10, // Connection pool
      idleTimeout: 30000
    });
  }
  return engineInstance;
}
```

## Next Steps

- **[State Management Patterns](./state-management.md)**
- **[API Route Design](./api-routes.md)**
- **[Authentication Deep Dive](./auth.md)**

---

> **🎯 Enterprise Pattern:** Always use Server Components for initial data fetching, Client Components only for interactivity and real-time updates.
