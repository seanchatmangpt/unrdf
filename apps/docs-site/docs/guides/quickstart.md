# Quick Start

A rapid 2-minute tutorial to get UNRDF running.

```bash
pnpm add @unrdf/core
```

```javascript
import { createStore, dataFactory } from '@unrdf/core';

const store = createStore();
const { quad, namedNode, literal } = dataFactory;

// Add data
store.add(quad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
));

// Query
console.log([...store.match()]);
```

Done! See [Getting Started](/docs/guides/getting-started) for more details.
