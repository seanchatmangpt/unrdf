# Getting Started

Get up and running with UNRDF in under 5 minutes.

## Installation

```bash
pnpm add @unrdf/core
```

## Quick Start

### 1. Create an RDF Store

```javascript
import { createStore } from '@unrdf/core';

const store = createStore();
```

### 2. Add Data

```javascript
import { dataFactory } from '@unrdf/core';

const { quad, namedNode, literal } = dataFactory;

store.add(quad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
));
```

### 3. Query Data

```javascript
const results = store.match(
  null,
  namedNode('http://xmlns.com/foaf/0.1/name'),
  null
);

for (const quad of results) {
  console.log(quad.subject.value, quad.object.value);
}
```

## Next Steps

- [API Reference](/docs/reference/overview) - Explore the full API
- [Tutorials](/docs/guides/tutorials/basic-usage) - Learn advanced patterns
- [KGC-4D Concepts](/docs/explanation/kgc-4d/overview) - Understanding the architecture

## Package-Specific Installation

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

<Tabs>
  <TabItem value="core" label="Core" default>

```bash
pnpm add @unrdf/core
```

Basic RDF store and query functionality.

  </TabItem>
  <TabItem value="hooks" label="React Hooks">

```bash
pnpm add @unrdf/hooks
```

40+ React hooks for RDF integration.

  </TabItem>
  <TabItem value="browser" label="Browser">

```bash
pnpm add @unrdf/browser
```

Browser-optimized RDF with IndexedDB.

  </TabItem>
  <TabItem value="kgc-4d" label="KGC-4D">

```bash
pnpm add @unrdf/kgc-4d
```

Advanced knowledge graph construction.

  </TabItem>
</Tabs>
