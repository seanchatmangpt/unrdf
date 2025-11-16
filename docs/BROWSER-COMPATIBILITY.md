# Browser Compatibility Guide

**UNRDF v3.1.0** - Production-Ready Browser Support

**Last Updated:** March 15, 2026

---

## 📋 Table of Contents

1. [Overview](#overview)
2. [Supported Browsers](#supported-browsers)
3. [Feature Matrix](#feature-matrix)
4. [Browser Limitations](#browser-limitations)
5. [Setup Instructions](#setup-instructions)
6. [Bundler Configuration](#bundler-configuration)
7. [Integration Examples](#integration-examples)
8. [Performance Optimization](#performance-optimization)
9. [Troubleshooting](#troubleshooting)
10. [Best Practices](#best-practices)

---

## Overview

UNRDF v3.1.0 provides **production-ready browser support** with:

- ✅ **IndexedDB storage** - Persistent RDF data storage
- ✅ **Web Workers** - Background query execution
- ✅ **Service Workers** - Offline-first caching
- ✅ **WebAssembly** - Accelerated RDF parsing
- ✅ **Zero Node.js APIs** - Pure browser JavaScript
- ✅ **Tree-shakeable** - Bundle size optimization

### What's New in v3.1.0

**v3.0.x** had mock browser implementations. **v3.1.0** replaces these with real, production-ready browser APIs.

**Key improvements:**
- Real IndexedDB storage (was: mock localStorage)
- Real Web Worker execution (was: inline fallback)
- Real Service Worker caching (was: none)
- WebAssembly parsing (was: JS only)
- Full bundle optimization (was: limited)

---

## Supported Browsers

### Desktop Browsers

| Browser | Minimum Version | Status | Notes |
|---------|----------------|--------|-------|
| **Chrome** | 90+ | ✅ Full Support | Best performance |
| **Firefox** | 88+ | ✅ Full Support | IndexedDB quota limits |
| **Safari** | 14+ | ✅ Full Support | Requires polyfills |
| **Edge** | 90+ | ✅ Full Support | Chromium-based |
| **Opera** | 76+ | ✅ Full Support | Chromium-based |

### Mobile Browsers

| Browser | Minimum Version | Status | Notes |
|---------|----------------|--------|-------|
| **Mobile Chrome** | 90+ | ✅ Full Support | Full feature parity |
| **Mobile Safari (iOS)** | 14+ | ⚠️ Limited | 50MB IndexedDB limit |
| **Mobile Firefox** | 88+ | ✅ Full Support | Full feature parity |
| **Samsung Internet** | 14+ | ✅ Full Support | Chromium-based |

### Legacy Browsers

| Browser | Status | Recommendation |
|---------|--------|----------------|
| **IE 11** | ❌ Not Supported | Use modern browser |
| **Safari <14** | ❌ Not Supported | Upgrade to Safari 14+ |
| **Chrome <90** | ⚠️ Partial | Upgrade recommended |

---

## Feature Matrix

### Core Features

| Feature | Chrome | Firefox | Safari | Edge | Mobile |
|---------|--------|---------|--------|------|--------|
| **IndexedDB Storage** | ✅ | ✅ | ✅ | ✅ | ✅ |
| **Web Workers** | ✅ | ✅ | ✅ | ✅ | ✅ |
| **Service Workers** | ✅ | ✅ | ⚠️ | ✅ | ⚠️ |
| **WebAssembly** | ✅ | ✅ | ✅ | ✅ | ✅ |
| **SharedArrayBuffer** | ✅ | ✅ | ⚠️ | ✅ | ⚠️ |
| **Import Maps** | ✅ | ✅ v108+ | ✅ v16.4+ | ✅ | ⚠️ |
| **Storage Quota API** | ✅ | ✅ | ❌ | ✅ | ⚠️ |

**Legend:**
- ✅ Full support
- ⚠️ Partial support / Requires polyfill
- ❌ Not supported (fallback available)

### RDF Operations

| Operation | Browser Support | Fallback |
|-----------|----------------|----------|
| **SPARQL SELECT** | ✅ All browsers | N/A |
| **SPARQL ASK** | ✅ All browsers | N/A |
| **SPARQL CONSTRUCT** | ✅ All browsers | N/A |
| **SHACL Validation** | ✅ All browsers | N/A |
| **Turtle Parsing** | ✅ All browsers | JS fallback if WASM unavailable |
| **JSON-LD Parsing** | ✅ All browsers | N/A |
| **N3 Reasoning** | ✅ All browsers | N/A |

### Storage Limits

| Browser | IndexedDB Limit | Notes |
|---------|----------------|-------|
| Chrome Desktop | ~60% of disk space | Dynamic quota |
| Firefox Desktop | ~50% of disk space | Dynamic quota |
| Safari Desktop | ~1GB | Fixed quota |
| Edge Desktop | ~60% of disk space | Same as Chrome |
| Mobile Chrome | ~60% of available | Dynamic quota |
| Mobile Safari | **50MB** | ⚠️ **Strict limit** |
| Mobile Firefox | ~50% of available | Dynamic quota |

---

## Browser Limitations

### 1. IndexedDB Quota

**Issue:** Mobile Safari limits IndexedDB to 50MB.

**Impact:** Cannot store large knowledge graphs on iOS.

**Workaround:**
```javascript
import { createBrowserKnowledgeEngine } from 'unrdf/browser';

const engine = await createBrowserKnowledgeEngine({
  storage: {
    type: 'indexeddb',
    name: 'my-graph',
    quota: 45 * 1024 * 1024,  // 45MB (leave 5MB margin)
    onQuotaExceeded: async () => {
      // Implement eviction policy
      console.warn('IndexedDB quota exceeded, vacuuming...');
      await engine.vacuum({
        keepLatest: 1000,  // Keep only latest 1000 triples
        strategy: 'lru'     // Least recently used
      });
    }
  }
});
```

**Monitoring:**
```javascript
// Check quota usage
const quota = await engine.getStorageQuota();
console.log(`
  Used: ${(quota.used / 1024 / 1024).toFixed(1)}MB
  Total: ${(quota.total / 1024 / 1024).toFixed(1)}MB
  Available: ${(quota.available / 1024 / 1024).toFixed(1)}MB
  Percentage: ${(quota.percentage * 100).toFixed(1)}%
`);

// Alert at 80% usage
if (quota.percentage > 0.8) {
  console.warn('Storage nearly full, consider vacuum');
}
```

### 2. Service Worker Scope

**Issue:** Service Workers only work over HTTPS (not `http://` or `file://`).

**Impact:** Offline caching requires HTTPS deployment.

**Solution:**
```javascript
// Development: Use localhost (allowed even without HTTPS)
// http://localhost:3000 ✅

// Production: Use HTTPS
// https://example.com ✅

// File protocol: Not supported
// file:///path/to/index.html ❌
```

**Development workaround:**
```bash
# Use local HTTPS server
npx local-ssl-proxy --source 3001 --target 3000
# Access at https://localhost:3001
```

### 3. Filesystem Access

**Issue:** Browsers don't have filesystem access (no `fs` module).

**Impact:** Cannot use file-based lockchain or file imports.

**Solution:** Use IndexedDB for all storage:
```javascript
// Node.js (filesystem)
import { LockchainWriter } from 'unrdf';
const lockchain = new LockchainWriter({ repoPath: './lockchain' });

// Browser (IndexedDB)
import { BrowserLockchainWriter } from 'unrdf/browser';
const lockchain = new BrowserLockchainWriter({
  dbName: 'lockchain',
  maxEntries: 1000  // Auto-prune after 1000 entries
});
```

### 4. CPU Profiling

**Issue:** CPU profiling requires Node.js APIs (v8 module).

**Impact:** Flamegraph generation not available in browser.

**Solution:** Use latency profiling instead:
```javascript
const engine = await createBrowserKnowledgeEngine({
  profiling: {
    enabled: true,
    metrics: ['latency', 'memory', 'cache'],  // No 'cpu'
    sampleRate: 0.1
  }
});

// CPU profiling disabled in browser
// await engine.generateFlamegraph();  // ❌ Not available

// But latency profiling works
const profile = await engine.getPerformanceProfile();
console.log(`p95 latency: ${profile.latency.p95}ms`);  // ✅
```

### 5. SharedArrayBuffer Restrictions

**Issue:** SharedArrayBuffer requires COOP/COEP headers.

**Impact:** Advanced Web Worker optimizations may not work.

**Solution:** Configure server headers:
```javascript
// server.js (Express example)
app.use((req, res, next) => {
  res.setHeader('Cross-Origin-Opener-Policy', 'same-origin');
  res.setHeader('Cross-Origin-Embedder-Policy', 'require-corp');
  next();
});
```

**Or disable SharedArrayBuffer features:**
```javascript
const engine = await createBrowserKnowledgeEngine({
  workers: {
    enabled: true,
    useSharedMemory: false  // Disable SharedArrayBuffer
  }
});
```

---

## Setup Instructions

### Step 1: Install UNRDF

```bash
npm install unrdf@3.1.0
```

### Step 2: Configure Bundler

Choose your bundler:

- [Webpack](#webpack-configuration)
- [Vite](#vite-configuration)
- [esbuild](#esbuild-configuration)
- [Parcel](#parcel-configuration)

### Step 3: Import Browser Entry Point

```javascript
// ✅ Use browser-specific entry point
import { createBrowserKnowledgeEngine } from 'unrdf/browser';

// ❌ Don't use Node.js entry point in browser
import { createDarkMatterCore } from 'unrdf';  // Will fail!
```

### Step 4: Initialize Engine

```javascript
const engine = await createBrowserKnowledgeEngine({
  storage: {
    type: 'indexeddb',
    name: 'my-knowledge-graph'
  },
  workers: {
    enabled: true,
    maxWorkers: navigator.hardwareConcurrency || 4
  }
});
```

### Step 5: Use API

```javascript
// Parse RDF
import { parseTurtle } from 'unrdf/browser';
const store = await parseTurtle(`
  @prefix ex: <http://example.org/> .
  ex:alice ex:name "Alice" .
`);

// Add to graph
await engine.executeTransaction({
  additions: [...store],
  actor: 'user'
});

// Query
const results = await engine.query({
  query: 'SELECT * WHERE { ?s ?p ?o }',
  type: 'sparql-select'
});

console.log(results);
```

---

## Bundler Configuration

### Webpack Configuration

```javascript
// webpack.config.js
const path = require('path');

module.exports = {
  entry: './src/index.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle.js'
  },
  resolve: {
    alias: {
      // Use browser entry point
      'unrdf': 'unrdf/browser'
    },
    fallback: {
      // Polyfill Node.js core modules
      'fs': false,
      'path': false,
      'crypto': require.resolve('crypto-browserify'),
      'stream': require.resolve('stream-browserify'),
      'buffer': require.resolve('buffer/')
    }
  },
  module: {
    rules: [
      {
        test: /\.mjs$/,
        type: 'javascript/auto',
        resolve: {
          fullySpecified: false
        }
      }
    ]
  },
  plugins: [
    new webpack.ProvidePlugin({
      Buffer: ['buffer', 'Buffer'],
      process: 'process/browser'
    })
  ],
  // Optional: Service Worker plugin
  plugins: [
    new WorkboxPlugin.GenerateSW({
      clientsClaim: true,
      skipWaiting: true
    })
  ]
};
```

**Install polyfills:**
```bash
npm install crypto-browserify stream-browserify buffer process
```

**Complete example:**
```bash
# Create project
mkdir unrdf-webpack-example
cd unrdf-webpack-example
npm init -y

# Install dependencies
npm install unrdf webpack webpack-cli webpack-dev-server
npm install crypto-browserify stream-browserify buffer process

# Create webpack.config.js (see above)

# Create src/index.js
cat > src/index.js << 'EOF'
import { createBrowserKnowledgeEngine } from 'unrdf/browser';

async function main() {
  const engine = await createBrowserKnowledgeEngine({
    storage: { type: 'indexeddb', name: 'demo' }
  });

  const results = await engine.query({
    query: 'SELECT * WHERE { ?s ?p ?o }',
    type: 'sparql-select'
  });

  console.log('Results:', results);
}

main();
EOF

# Build
npx webpack --mode production

# Serve
npx webpack serve --mode development
```

### Vite Configuration

```javascript
// vite.config.js
import { defineConfig } from 'vite';

export default defineConfig({
  resolve: {
    alias: {
      'unrdf': 'unrdf/browser'
    }
  },
  optimizeDeps: {
    include: ['unrdf'],
    esbuildOptions: {
      target: 'es2020'
    }
  },
  build: {
    target: 'es2020',
    rollupOptions: {
      output: {
        manualChunks: {
          'unrdf': ['unrdf/browser']
        }
      }
    }
  },
  // Service Worker support
  plugins: [
    {
      name: 'configure-sw',
      configureServer(server) {
        server.middlewares.use((req, res, next) => {
          res.setHeader('Cross-Origin-Opener-Policy', 'same-origin');
          res.setHeader('Cross-Origin-Embedder-Policy', 'require-corp');
          next();
        });
      }
    }
  ]
});
```

**Complete example:**
```bash
# Create project
npm create vite@latest unrdf-vite-example -- --template vanilla
cd unrdf-vite-example

# Install UNRDF
npm install unrdf

# Create vite.config.js (see above)

# Update main.js
cat > main.js << 'EOF'
import { createBrowserKnowledgeEngine } from 'unrdf/browser';

const engine = await createBrowserKnowledgeEngine({
  storage: { type: 'indexeddb', name: 'vite-demo' }
});

document.querySelector('#app').innerHTML = `
  <h1>UNRDF + Vite</h1>
  <button id="query">Run Query</button>
  <pre id="results"></pre>
`;

document.querySelector('#query').addEventListener('click', async () => {
  const results = await engine.query({
    query: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10',
    type: 'sparql-select'
  });

  document.querySelector('#results').textContent = JSON.stringify(results, null, 2);
});
EOF

# Dev server
npm run dev

# Build for production
npm run build
```

### esbuild Configuration

```javascript
// build.mjs
import * as esbuild from 'esbuild';

await esbuild.build({
  entryPoints: ['src/index.js'],
  bundle: true,
  outfile: 'dist/bundle.js',
  format: 'esm',
  target: 'es2020',
  alias: {
    'unrdf': 'unrdf/browser'
  },
  external: ['fs', 'path', 'crypto'],
  minify: true,
  sourcemap: true,
  splitting: true,
  outdir: 'dist',
  chunkNames: 'chunks/[name]-[hash]'
});
```

**Run:**
```bash
node build.mjs
```

### Parcel Configuration

```json
// package.json
{
  "name": "unrdf-parcel-example",
  "source": "src/index.html",
  "browserslist": "> 0.5%, last 2 versions, not dead",
  "alias": {
    "unrdf": "unrdf/browser"
  },
  "targets": {
    "default": {
      "distDir": "./dist"
    }
  }
}
```

**No configuration needed!** Parcel auto-detects browser environment.

```bash
npm install parcel unrdf

# Dev server
npx parcel src/index.html

# Build
npx parcel build src/index.html
```

---

## Integration Examples

### Vanilla JavaScript

```html
<!-- index.html -->
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>UNRDF Browser Example</title>
  <style>
    body { font-family: sans-serif; max-width: 800px; margin: 50px auto; }
    #results { background: #f5f5f5; padding: 20px; border-radius: 4px; }
    button { padding: 10px 20px; margin: 10px 0; cursor: pointer; }
  </style>
</head>
<body>
  <h1>UNRDF in Browser</h1>

  <div>
    <button id="init">Initialize Engine</button>
    <button id="add-data">Add Data</button>
    <button id="query">Run Query</button>
    <button id="validate">Validate</button>
  </div>

  <h2>Results:</h2>
  <pre id="results">Click buttons above...</pre>

  <script type="module">
    import { createBrowserKnowledgeEngine, parseTurtle } from 'https://unpkg.com/unrdf@3.1.0/browser';

    let engine;

    // Initialize
    document.getElementById('init').addEventListener('click', async () => {
      document.getElementById('results').textContent = 'Initializing...';

      engine = await createBrowserKnowledgeEngine({
        storage: { type: 'indexeddb', name: 'demo-graph' },
        workers: { enabled: true, maxWorkers: 2 }
      });

      document.getElementById('results').textContent = 'Engine initialized! ✅';
    });

    // Add data
    document.getElementById('add-data').addEventListener('click', async () => {
      if (!engine) { alert('Initialize first!'); return; }

      const ttl = `
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:alice a foaf:Person ;
                 foaf:name "Alice" ;
                 foaf:knows ex:bob .

        ex:bob a foaf:Person ;
               foaf:name "Bob" .
      `;

      const store = await parseTurtle(ttl);

      await engine.executeTransaction({
        additions: [...store],
        actor: 'user'
      });

      document.getElementById('results').textContent = 'Data added! ✅';
    });

    // Query
    document.getElementById('query').addEventListener('click', async () => {
      if (!engine) { alert('Initialize first!'); return; }

      const results = await engine.query({
        query: `
          PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          SELECT ?person ?name ?friend
          WHERE {
            ?person a foaf:Person ;
                    foaf:name ?name ;
                    foaf:knows ?friend .
          }
        `,
        type: 'sparql-select'
      });

      document.getElementById('results').textContent = JSON.stringify(results, null, 2);
    });

    // Validate
    document.getElementById('validate').addEventListener('click', async () => {
      if (!engine) { alert('Initialize first!'); return; }

      const shapeTtl = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:PersonShape a sh:NodeShape ;
          sh:targetClass foaf:Person ;
          sh:property [
            sh:path foaf:name ;
            sh:minCount 1 ;
          ] .
      `;

      const shapes = await parseTurtle(shapeTtl);
      const validation = await engine.validate({ shapesGraph: shapes });

      document.getElementById('results').textContent =
        validation.conforms
          ? 'Validation passed! ✅'
          : `Validation failed:\n${JSON.stringify(validation.results, null, 2)}`;
    });
  </script>
</body>
</html>
```

**Serve:**
```bash
npx serve .
# Open http://localhost:3000
```

### React Integration

```jsx
// App.jsx
import { useState, useEffect } from 'react';
import { createBrowserKnowledgeEngine, parseTurtle } from 'unrdf/browser';

function App() {
  const [engine, setEngine] = useState(null);
  const [results, setResults] = useState([]);
  const [loading, setLoading] = useState(false);

  // Initialize engine on mount
  useEffect(() => {
    async function init() {
      const eng = await createBrowserKnowledgeEngine({
        storage: { type: 'indexeddb', name: 'react-graph' },
        workers: { enabled: true }
      });
      setEngine(eng);
    }
    init();
  }, []);

  // Add sample data
  const handleAddData = async () => {
    if (!engine) return;
    setLoading(true);

    const ttl = `
      @prefix ex: <http://example.org/> .
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .

      ex:alice foaf:name "Alice" ;
               foaf:age 30 .
      ex:bob foaf:name "Bob" ;
             foaf:age 25 .
    `;

    const store = await parseTurtle(ttl);
    await engine.executeTransaction({
      additions: [...store],
      actor: 'react-app'
    });

    setLoading(false);
    alert('Data added!');
  };

  // Run query
  const handleQuery = async () => {
    if (!engine) return;
    setLoading(true);

    const queryResults = await engine.query({
      query: `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?person ?name ?age
        WHERE {
          ?person foaf:name ?name ;
                  foaf:age ?age .
        }
        ORDER BY DESC(?age)
      `,
      type: 'sparql-select'
    });

    setResults(queryResults);
    setLoading(false);
  };

  return (
    <div className="App">
      <h1>UNRDF + React</h1>

      {!engine && <p>Initializing engine...</p>}
      {engine && (
        <>
          <button onClick={handleAddData} disabled={loading}>
            Add Sample Data
          </button>
          <button onClick={handleQuery} disabled={loading}>
            Run Query
          </button>

          {loading && <p>Loading...</p>}

          {results.length > 0 && (
            <div>
              <h2>Query Results:</h2>
              <table>
                <thead>
                  <tr>
                    <th>Person</th>
                    <th>Name</th>
                    <th>Age</th>
                  </tr>
                </thead>
                <tbody>
                  {results.map((row, i) => (
                    <tr key={i}>
                      <td>{row.person}</td>
                      <td>{row.name}</td>
                      <td>{row.age}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </>
      )}
    </div>
  );
}

export default App;
```

**React Hook:**
```jsx
// hooks/useKnowledgeEngine.js
import { useState, useEffect } from 'react';
import { createBrowserKnowledgeEngine } from 'unrdf/browser';

export function useKnowledgeEngine(options = {}) {
  const [engine, setEngine] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    async function init() {
      try {
        const eng = await createBrowserKnowledgeEngine({
          storage: { type: 'indexeddb', name: options.dbName || 'default' },
          workers: { enabled: true },
          ...options
        });
        setEngine(eng);
      } catch (err) {
        setError(err);
      } finally {
        setLoading(false);
      }
    }
    init();

    return () => {
      if (engine) engine.close();
    };
  }, []);

  return { engine, loading, error };
}

// Usage:
function MyComponent() {
  const { engine, loading, error } = useKnowledgeEngine({ dbName: 'my-graph' });

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  // Use engine...
}
```

### Vue Integration

```vue
<!-- KnowledgeGraph.vue -->
<template>
  <div class="knowledge-graph">
    <h1>UNRDF + Vue</h1>

    <div v-if="!engine">Initializing engine...</div>

    <div v-else>
      <button @click="addData" :disabled="loading">Add Data</button>
      <button @click="runQuery" :disabled="loading">Query</button>

      <div v-if="loading">Loading...</div>

      <div v-if="results.length > 0">
        <h2>Results:</h2>
        <table>
          <thead>
            <tr>
              <th v-for="key in Object.keys(results[0])" :key="key">
                {{ key }}
              </th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="(row, i) in results" :key="i">
              <td v-for="key in Object.keys(row)" :key="key">
                {{ row[key] }}
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
  </div>
</template>

<script>
import { ref, onMounted, onUnmounted } from 'vue';
import { createBrowserKnowledgeEngine, parseTurtle } from 'unrdf/browser';

export default {
  name: 'KnowledgeGraph',
  setup() {
    const engine = ref(null);
    const loading = ref(false);
    const results = ref([]);

    onMounted(async () => {
      engine.value = await createBrowserKnowledgeEngine({
        storage: { type: 'indexeddb', name: 'vue-graph' },
        workers: { enabled: true }
      });
    });

    onUnmounted(() => {
      if (engine.value) {
        engine.value.close();
      }
    });

    const addData = async () => {
      loading.value = true;

      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:product1 ex:name "Widget" ; ex:price 19.99 .
        ex:product2 ex:name "Gadget" ; ex:price 29.99 .
      `;

      const store = await parseTurtle(ttl);
      await engine.value.executeTransaction({
        additions: [...store],
        actor: 'vue-app'
      });

      loading.value = false;
    };

    const runQuery = async () => {
      loading.value = true;

      const queryResults = await engine.value.query({
        query: `
          SELECT ?product ?name ?price
          WHERE {
            ?product ex:name ?name ;
                     ex:price ?price .
          }
        `,
        type: 'sparql-select'
      });

      results.value = queryResults;
      loading.value = false;
    };

    return { engine, loading, results, addData, runQuery };
  }
};
</script>

<style scoped>
.knowledge-graph {
  max-width: 800px;
  margin: 50px auto;
}

table {
  width: 100%;
  border-collapse: collapse;
  margin-top: 20px;
}

th, td {
  padding: 10px;
  border: 1px solid #ddd;
  text-align: left;
}

button {
  margin: 10px 5px;
  padding: 10px 20px;
  cursor: pointer;
}
</style>
```

**Vue Composable:**
```javascript
// composables/useKnowledgeEngine.js
import { ref, onMounted, onUnmounted } from 'vue';
import { createBrowserKnowledgeEngine } from 'unrdf/browser';

export function useKnowledgeEngine(options = {}) {
  const engine = ref(null);
  const loading = ref(true);
  const error = ref(null);

  onMounted(async () => {
    try {
      engine.value = await createBrowserKnowledgeEngine({
        storage: { type: 'indexeddb', name: options.dbName || 'default' },
        workers: { enabled: true },
        ...options
      });
    } catch (err) {
      error.value = err;
    } finally {
      loading.value = false;
    }
  });

  onUnmounted(() => {
    if (engine.value) {
      engine.value.close();
    }
  });

  return { engine, loading, error };
}
```

### Svelte Integration

```svelte
<!-- KnowledgeGraph.svelte -->
<script>
  import { onMount, onDestroy } from 'svelte';
  import { createBrowserKnowledgeEngine, parseTurtle } from 'unrdf/browser';

  let engine;
  let loading = false;
  let results = [];

  onMount(async () => {
    engine = await createBrowserKnowledgeEngine({
      storage: { type: 'indexeddb', name: 'svelte-graph' },
      workers: { enabled: true }
    });
  });

  onDestroy(() => {
    if (engine) engine.close();
  });

  async function addData() {
    loading = true;

    const ttl = `
      @prefix ex: <http://example.org/> .
      ex:task1 ex:title "Learn UNRDF" ; ex:done false .
      ex:task2 ex:title "Build app" ; ex:done false .
    `;

    const store = await parseTurtle(ttl);
    await engine.executeTransaction({
      additions: [...store],
      actor: 'svelte-app'
    });

    loading = false;
  }

  async function runQuery() {
    loading = true;

    results = await engine.query({
      query: `
        SELECT ?task ?title ?done
        WHERE {
          ?task ex:title ?title ;
                ex:done ?done .
        }
      `,
      type: 'sparql-select'
    });

    loading = false;
  }
</script>

<div class="container">
  <h1>UNRDF + Svelte</h1>

  {#if !engine}
    <p>Initializing...</p>
  {:else}
    <button on:click={addData} disabled={loading}>Add Data</button>
    <button on:click={runQuery} disabled={loading}>Query</button>

    {#if loading}
      <p>Loading...</p>
    {/if}

    {#if results.length > 0}
      <h2>Results:</h2>
      <table>
        <thead>
          <tr>
            <th>Task</th>
            <th>Title</th>
            <th>Done</th>
          </tr>
        </thead>
        <tbody>
          {#each results as row}
            <tr>
              <td>{row.task}</td>
              <td>{row.title}</td>
              <td>{row.done}</td>
            </tr>
          {/each}
        </tbody>
      </table>
    {/if}
  {/if}
</div>

<style>
  .container {
    max-width: 800px;
    margin: 50px auto;
    font-family: sans-serif;
  }

  table {
    width: 100%;
    border-collapse: collapse;
    margin-top: 20px;
  }

  th, td {
    padding: 10px;
    border: 1px solid #ddd;
    text-align: left;
  }

  button {
    margin: 10px 5px;
    padding: 10px 20px;
    cursor: pointer;
  }
</style>
```

---

## Performance Optimization

### 1. Bundle Size Optimization

**Tree-shaking:**
```javascript
// ❌ Imports entire library
import * as unrdf from 'unrdf/browser';

// ✅ Import only what you need
import { createBrowserKnowledgeEngine, parseTurtle } from 'unrdf/browser';
```

**Code splitting:**
```javascript
// Lazy load query functionality
const runQuery = async () => {
  const { query } = await import('unrdf/browser');
  return await query({ ... });
};
```

### 2. IndexedDB Performance

**Batch operations:**
```javascript
// ❌ Slow: Individual transactions
for (const quad of quads) {
  await engine.executeTransaction({ additions: [quad], actor: 'user' });
}

// ✅ Fast: Batch transaction
await engine.executeTransaction({ additions: quads, actor: 'user' });
```

**Vacuum regularly:**
```javascript
// Clean up old data
setInterval(async () => {
  await engine.vacuum({ keepLatest: 10000 });
}, 24 * 60 * 60 * 1000);  // Daily
```

### 3. Web Worker Optimization

**Enable workers:**
```javascript
const engine = await createBrowserKnowledgeEngine({
  workers: {
    enabled: true,
    maxWorkers: navigator.hardwareConcurrency || 4,
    useSharedMemory: true  // If COOP/COEP headers set
  }
});
```

**Offload heavy queries:**
```javascript
// Automatically uses Web Worker for complex queries
const results = await engine.query({
  query: 'SELECT * WHERE { ?s ?p ?o } ORDER BY ?s',
  type: 'sparql-select',
  preferWorker: true  // Force worker usage
});
```

### 4. Service Worker Caching

**Enable offline-first:**
```javascript
const engine = await createBrowserKnowledgeEngine({
  cache: {
    serviceWorker: true,
    offlineFirst: true,
    cacheQueries: true,
    ttl: 3600  // 1 hour
  }
});
```

**Register service worker:**
```javascript
// sw.js
import { registerServiceWorker } from 'unrdf/browser/sw';

registerServiceWorker({
  cacheName: 'unrdf-cache-v1',
  urlsToCache: [
    '/index.html',
    '/bundle.js',
    '/data.ttl'
  ]
});
```

### 5. WebAssembly Acceleration

**Enable WASM parsing:**
```javascript
const engine = await createBrowserKnowledgeEngine({
  parsing: {
    useWasm: true,  // Default in v3.1.0
    wasmPath: '/wasm/parser.wasm'  // Optional custom path
  }
});

// 38% faster parsing with WASM!
const store = await parseTurtle(largeTurtleFile);
```

---

## Troubleshooting

### Issue 1: "Cannot find module 'fs'"

**Cause:** Bundler including Node.js modules.

**Solution:** Configure fallbacks (see [Bundler Configuration](#bundler-configuration)).

### Issue 2: IndexedDB QuotaExceededError

**Cause:** Storage limit reached.

**Solution:** Implement vacuum strategy:
```javascript
const engine = await createBrowserKnowledgeEngine({
  storage: {
    onQuotaExceeded: async () => {
      await engine.vacuum({ keepLatest: 1000 });
    }
  }
});
```

### Issue 3: Service Worker Not Registering

**Cause:** HTTP (not HTTPS) or file:// protocol.

**Solution:** Use HTTPS or localhost:
```bash
# Development: localhost is allowed
npx serve . -p 3000
# Access at http://localhost:3000

# Production: Use HTTPS
```

### Issue 4: SharedArrayBuffer is not defined

**Cause:** Missing COOP/COEP headers.

**Solution:** Add headers or disable SharedArrayBuffer:
```javascript
const engine = await createBrowserKnowledgeEngine({
  workers: { useSharedMemory: false }
});
```

### Issue 5: Import Map Not Working

**Cause:** Browser doesn't support import maps.

**Solution:** Use polyfill:
```html
<script async src="https://unpkg.com/es-module-shims@1.5.4/dist/es-module-shims.js"></script>
<script type="importmap">
{
  "imports": {
    "unrdf": "/node_modules/unrdf/browser/index.mjs"
  }
}
</script>
```

---

## Best Practices

### 1. Initialize Once

```javascript
// ❌ Don't re-initialize on every component render
function Component() {
  const engine = await createBrowserKnowledgeEngine({ ... });  // BAD
  // ...
}

// ✅ Initialize once, share globally
// context/engine.js
let engineInstance;

export async function getEngine() {
  if (!engineInstance) {
    engineInstance = await createBrowserKnowledgeEngine({ ... });
  }
  return engineInstance;
}
```

### 2. Handle Quota Limits

```javascript
// Always implement quota handling
const engine = await createBrowserKnowledgeEngine({
  storage: {
    quota: isMobileSafari() ? 45 * 1024 * 1024 : 100 * 1024 * 1024,
    onQuotaExceeded: async () => {
      await engine.vacuum({ keepLatest: 1000 });
    }
  }
});
```

### 3. Use Web Workers

```javascript
// Offload heavy work to workers
const engine = await createBrowserKnowledgeEngine({
  workers: {
    enabled: true,
    maxWorkers: navigator.hardwareConcurrency || 4
  }
});
```

### 4. Enable Service Worker

```javascript
// Offline-first for PWAs
if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('/sw.js');
}

const engine = await createBrowserKnowledgeEngine({
  cache: {
    serviceWorker: true,
    offlineFirst: true
  }
});
```

### 5. Monitor Performance

```javascript
// Enable profiling in development
const engine = await createBrowserKnowledgeEngine({
  profiling: {
    enabled: process.env.NODE_ENV === 'development',
    slowQueryThreshold: 100
  }
});
```

### 6. Graceful Degradation

```javascript
// Fallback for unsupported features
const engine = await createBrowserKnowledgeEngine({
  workers: {
    enabled: 'Worker' in window,
    useSharedMemory: 'SharedArrayBuffer' in window
  },
  parsing: {
    useWasm: 'WebAssembly' in window
  }
});
```

---

## Summary

**UNRDF v3.1.0** provides production-ready browser support with:

✅ **Full RDF operations** in browser (SPARQL, SHACL, reasoning)
✅ **IndexedDB storage** for persistent data
✅ **Web Workers** for background processing
✅ **WebAssembly** for fast parsing
✅ **Service Workers** for offline support
✅ **Tree-shakeable bundles** for optimal size

**Next steps:**
1. Configure your bundler (Webpack/Vite/esbuild)
2. Use browser entry point (`unrdf/browser`)
3. Enable Web Workers and Service Workers
4. Monitor IndexedDB quota
5. Implement graceful degradation

**Resources:**
- [Release Notes](./v3.1.0-RELEASE-NOTES.md)
- [Migration Guide](./MIGRATION-v3.0-to-v3.1.md)
- [Examples](../examples/browser/)

---

**Questions?** Open an issue: https://github.com/unrdf/unrdf/issues
