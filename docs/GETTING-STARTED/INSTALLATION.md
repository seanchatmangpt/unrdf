# Installation Guide

Complete setup instructions for UNRDF.

## Quick Installation

### NPM

```bash
npm install @unrdf/core
```

### PNPM (Recommended)

```bash
pnpm add @unrdf/core
```

### Yarn

```bash
yarn add @unrdf/core
```

---

## Requirements

- **Node.js:** 18.0.0 or higher
- **NPM:** 8.0.0 or higher
- **PNPM:** 7.0.0 or higher (optional, but recommended)
- **Module Type:** ESM (ES Modules)

### Verify Installation

```bash
node --version  # Should be 18.0.0 or higher
npm --version   # Should be 8.0.0 or higher
```

---

## Browser Installation

### From CDN

```html
<script type="module">
  import { createKnowledgeSubstrateCore } from 'https://cdn.jsdelivr.net/npm/@unrdf/browser@latest';
  const core = await createKnowledgeSubstrateCore();
</script>
```

### NPM (for bundlers)

```bash
npm install @unrdf/browser
```

Then import:

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/browser';
```

---

## Full Stack Installation

Install all packages for maximum features:

```bash
npm install \
  @unrdf/core \
  @unrdf/oxigraph \
  @unrdf/hooks \
  @unrdf/streaming \
  @unrdf/federation \
  @unrdf/knowledge-engine \
  @unrdf/browser \
  @unrdf/composables
```

---

## CLI Installation

### Global Installation

```bash
npm install -g @unrdf/cli
# or
pnpm add -g @unrdf/cli
```

### Local Installation

```bash
npm install --save-dev @unrdf/cli
# or
pnpm add -D @unrdf/cli
```

Then use with `npx`:

```bash
npx @unrdf/cli query data.ttl
```

---

## Framework Integration

### React

```bash
npm install @unrdf/composables @unrdf/core
```

Usage in components:

```jsx
import { useQuery } from '@unrdf/composables/react';

function MyComponent() {
  const { data, loading } = useQuery('SELECT * WHERE { ?s ?p ?o }');
  return loading ? <div>Loading...</div> : <div>{data}</div>;
}
```

### Vue

```bash
npm install @unrdf/composables @unrdf/core
```

Usage in components:

```vue
<script setup>
import { useQuery } from '@unrdf/composables/vue';

const { data, loading } = useQuery('SELECT * WHERE { ?s ?p ?o }');
</script>

<template>
  <div>{{ loading ? 'Loading...' : data }}</div>
</template>
```

### Express.js

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';
import express from 'express';

const app = express();
const core = await createKnowledgeSubstrateCore();

app.get('/query', async (req, res) => {
  const store = core.parseRdf(req.query.data);
  const results = await core.query(store, req.query.sparql);
  res.json(results);
});

app.listen(3000);
```

---

## Persistent Storage Setup

### Using Oxigraph (SQLite)

```bash
npm install @unrdf/oxigraph
```

Usage:

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore({
  backend: 'oxigraph',
  oxigraphOptions: {
    path: './data.db'  // SQLite file location
  }
});
```

---

## Docker Setup

### Dockerfile

```dockerfile
FROM node:18-alpine

WORKDIR /app

# Copy package files
COPY package.json pnpm-lock.yaml ./

# Install dependencies
RUN npm install -g pnpm && pnpm install --frozen-lockfile

# Copy source
COPY . .

# Run application
CMD ["node", "src/index.mjs"]
```

### Build and Run

```bash
docker build -t unrdf-app .
docker run -p 3000:3000 unrdf-app
```

---

## Monorepo Development Setup

If you're developing UNRDF itself:

```bash
# Clone repository
git clone https://github.com/unrdf/unrdf.git
cd unrdf

# Install dependencies (pnpm required)
pnpm install

# Build all packages
pnpm run build

# Run tests
pnpm test

# Watch mode for development
pnpm run dev
```

---

## Environment Configuration

### Environment Variables

Create `.env` file:

```bash
# Optional: SPARQL endpoint
UNRDF_SPARQL_ENDPOINT=https://example.org/sparql

# Optional: Custom backend
UNRDF_BACKEND=oxigraph

# Optional: Database path
UNRDF_DB_PATH=./data.db

# Optional: Enable telemetry
UNRDF_TELEMETRY_ENABLED=true
```

### TypeScript Configuration

If using TypeScript, ensure `tsconfig.json`:

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ESNext",
    "moduleResolution": "node",
    "allowSyntheticDefaultImports": true,
    "esModuleInterop": true,
    "strict": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true
  }
}
```

---

## Troubleshooting

### "Module not found" Error

**Problem:** `Cannot find module '@unrdf/core'`

**Solution:**
```bash
# Clear cache and reinstall
npm cache clean --force
npm install
```

### "ESM only" Error

**Problem:** Error about CommonJS

**Solution:** Ensure your `package.json` has:
```json
{
  "type": "module"
}
```

### Node.js Version Error

**Problem:** `Unsupported Node.js version`

**Solution:**
```bash
# Update Node.js to 18+
nvm install 18
nvm use 18
```

### Permission Error

**Problem:** `EACCES: permission denied`

**Solution:**
```bash
# Fix npm permissions
mkdir ~/.npm-global
npm config set prefix '~/.npm-global'
export PATH=~/.npm-global/bin:$PATH
```

---

## Verification

Test your installation:

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();
const store = core.parseRdf(`
  @prefix ex: <http://example.org/> .
  ex:test ex:hello ex:world .
`);

const results = await core.query(store, `
  SELECT ?subject WHERE { ?subject ?p ?o }
`);

console.log('✓ UNRDF installed correctly');
console.log('Results:', results);
```

Run with:

```bash
node verify.mjs
```

Expected output:

```
✓ UNRDF installed correctly
Results: [ { subject: 'http://example.org/test' } ]
```

---

## Next Steps

1. Read [QUICK-START.md](QUICK-START.md) for your first program
2. Check [../START-HERE.md](../START-HERE.md) for orientation
3. Explore [../PACKAGES.md](../PACKAGES.md) for all available packages
4. See [../EXAMPLES.md](../EXAMPLES.md) for code examples

---

**Questions?** → [../START-HERE.md](../START-HERE.md)
