# UNRDF Runtime Compatibility Analysis

Agent 8 audit results: **undefined** packages analyzed

## Summary

| Category                    | Count                                                                                                                                                                                                                                                                                                                                                                                            | Percentage |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ---------- |
| Node-only                   | @unrdf/atomvm,@unrdf/caching,@unrdf/cli,@unrdf/collab,@unrdf/consensus,@unrdf/core,@unrdf/federation,@unrdf/hooks,@unrdf/kgc-4d,@unrdf/kgc-claude,@unrdf/kgc-substrate,@unrdf/kgn,@unrdf/knowledge-engine,@unrdf/ml-inference,@unrdf/ml-versioning,@unrdf/observability,@unrdf/project-engine,@unrdf/serverless,@unrdf/streaming,@unrdf/yawl,@unrdf/yawl-api,@unrdf/yawl-kafka,@unrdf/yawl-queue | NaN%       |
| Dual-runtime (Browser-safe) | @unrdf/blockchain,@unrdf/diataxis-kit,@unrdf/engine-gateway,@unrdf/graph-analytics,@unrdf/oxigraph,@unrdf/rdf-graphql,@unrdf/semantic-search,@unrdf/yawl-ai,@unrdf/yawl-durable,@unrdf/yawl-langchain,@unrdf/yawl-realtime                                                                                                                                                                       | NaN%       |
| Browser-only                | @unrdf/composables,@unrdf/dark-matter,@unrdf/fusion,@unrdf/kgc-cli,@unrdf/yawl-observability,@unrdf/yawl-viz                                                                                                                                                                                                                                                                                     | NaN%       |

## Node-only Packages

These packages use Node.js-specific APIs and cannot run in browsers without polyfills or abstraction layers:

```
- @unrdf/atomvm
- @unrdf/caching
- @unrdf/cli
- @unrdf/collab
- @unrdf/consensus
- @unrdf/core
- @unrdf/federation
- @unrdf/hooks
- @unrdf/kgc-4d
- @unrdf/kgc-claude
- @unrdf/kgc-substrate
- @unrdf/kgn
- @unrdf/knowledge-engine
- @unrdf/ml-inference
- @unrdf/ml-versioning
- @unrdf/observability
- @unrdf/project-engine
- @unrdf/serverless
- @unrdf/streaming
- @unrdf/yawl
- @unrdf/yawl-api
- @unrdf/yawl-kafka
- @unrdf/yawl-queue
```

## Dual-runtime Packages (Browser-safe)

These packages have NO Node-specific dependencies and can safely run in both Node.js and browser contexts:

```
- @unrdf/blockchain
- @unrdf/diataxis-kit
- @unrdf/engine-gateway
- @unrdf/graph-analytics
- @unrdf/oxigraph
- @unrdf/rdf-graphql
- @unrdf/semantic-search
- @unrdf/yawl-ai
- @unrdf/yawl-durable
- @unrdf/yawl-langchain
- @unrdf/yawl-realtime
```

## Compatibility Matrix

| Package            | Classification | Node Imports                                                                          | Node Patterns                                                     | Node Deps   | Files |
| ------------------ | -------------- | ------------------------------------------------------------------------------------- | ----------------------------------------------------------------- | ----------- | ----- |
| atomvm             | node-only      | fs, path, module, child_process                                                       | **dirname, **filename                                             | -           | 2     |
| blockchain         | dual-runtime   | -                                                                                     | process.env, Buffer                                               | -           | 3     |
| caching            | node-only      | -                                                                                     | -                                                                 | ioredis     | 0     |
| cli                | node-only      | fs, path, process                                                                     | process.cwd(), Buffer                                             | -           | 10    |
| collab             | node-only      | -                                                                                     | -                                                                 | ws          | 0     |
| composables        | browser-only   | -                                                                                     | global, \_\_dirname, process.env                                  | -           | 3     |
| consensus          | node-only      | events, crypto                                                                        | -                                                                 | ws          | 4     |
| core               | node-only      | child_process, fs, path, crypto                                                       | process.env, global, Buffer, process.cwd(), \_\_dirname           | -           | 16    |
| dark-matter        | browser-only   | -                                                                                     | -                                                                 | -           | 0     |
| diataxis-kit       | dual-runtime   | -                                                                                     | process.env                                                       | -           | 2     |
| engine-gateway     | dual-runtime   | -                                                                                     | -                                                                 | -           | 0     |
| federation         | node-only      | events, crypto                                                                        | -                                                                 | -           | 3     |
| fusion             | browser-only   | -                                                                                     | process.env, **dirname, **filename, global                        | -           | 5     |
| graph-analytics    | dual-runtime   | -                                                                                     | -                                                                 | -           | 0     |
| hooks              | node-only      | worker_threads, path, crypto, fs                                                      | process.cwd(), **dirname, **filename, process.env, global, Buffer | -           | 17    |
| kgc-4d             | node-only      | fs, path, crypto                                                                      | Buffer, process.env                                               | -           | 6     |
| kgc-claude         | node-only      | fs, path                                                                              | process.cwd()                                                     | -           | 1     |
| kgc-cli            | browser-only   | -                                                                                     | Buffer                                                            | -           | 1     |
| kgc-substrate      | node-only      | crypto, fs, path                                                                      | Buffer                                                            | -           | 3     |
| kgn                | node-only      | crypto, fs, path, worker_threads, stream, perf_hooks, os, child_process, util, events | Buffer, **dirname, **filename, process.cwd(), process.env, global | fs-extra    | 27    |
| knowledge-engine   | node-only      | worker_threads, path, crypto, fs, assert, events                                      | process.env, Buffer, process.cwd(), global, **dirname, **filename | -           | 27    |
| ml-inference       | node-only      | fs                                                                                    | -                                                                 | -           | 1     |
| ml-versioning      | node-only      | crypto                                                                                | -                                                                 | -           | 1     |
| observability      | node-only      | -                                                                                     | -                                                                 | express     | 0     |
| oxigraph           | dual-runtime   | -                                                                                     | -                                                                 | -           | 0     |
| project-engine     | node-only      | fs, path, crypto                                                                      | -                                                                 | -           | 11    |
| rdf-graphql        | dual-runtime   | -                                                                                     | -                                                                 | -           | 0     |
| semantic-search    | dual-runtime   | -                                                                                     | -                                                                 | -           | 0     |
| serverless         | node-only      | -                                                                                     | global, process.env                                               | aws-cdk-lib | 3     |
| streaming          | node-only      | events, crypto                                                                        | -                                                                 | ws          | 2     |
| yawl               | node-only      | crypto                                                                                | -                                                                 | -           | 8     |
| yawl-ai            | dual-runtime   | -                                                                                     | -                                                                 | -           | 0     |
| yawl-api           | node-only      | -                                                                                     | -                                                                 | -           | 0     |
| yawl-durable       | dual-runtime   | -                                                                                     | -                                                                 | -           | 0     |
| yawl-kafka         | node-only      | -                                                                                     | process.env, Buffer                                               | kafkajs     | 2     |
| yawl-langchain     | dual-runtime   | -                                                                                     | -                                                                 | -           | 0     |
| yawl-observability | browser-only   | -                                                                                     | -                                                                 | -           | 0     |
| yawl-queue         | node-only      | -                                                                                     | -                                                                 | ioredis     | 0     |
| yawl-realtime      | dual-runtime   | -                                                                                     | -                                                                 | -           | 0     |
| yawl-viz           | browser-only   | -                                                                                     | -                                                                 | -           | 0     |

## Key Findings

### Node-specific Modules Detected

- `fs` - File system (Node-only)
- `path` - Path utilities (Node-only)
- `os` - OS utilities (Node-only)
- `process` - Process management (Node-only)
- `crypto` - Native crypto (Node-only in some usages)
- `stream` - Streaming API (Node-only)
- `buffer` - Buffer API (Node-only)
- `worker_threads` - Threading (Node-only)
- `child_process` - Process spawning (Node-only)

### Common Patterns

1. **File I/O operations** - Packages that read/write files are Node-only
2. **Process spawning** - Packages that fork/spawn processes are Node-only
3. **System integration** - Packages using `os`, `path` are Node-only
4. **Worker threads** - Packages using `worker_threads` are Node-only
5. **In-memory operations** - Pure RDF operations (no I/O) are browser-safe

## Browser Deployment Guide

### For Dual-runtime Packages

These can be directly used in browsers:

```javascript
// Browser
import { createStore } from '@unrdf/core/rdf'; // Safe!
const store = createStore();
```

### For Node-only Packages

To use in browser contexts:

1. **Fetch instead of fs.read()**

   ```javascript
   // Instead of: const data = fs.readFileSync('file.ttl');
   const response = await fetch('file.ttl');
   const data = await response.text();
   ```

2. **Abstract file operations**

   ```javascript
   interface FileProvider {
     read(path: string): Promise<string>;
     write(path: string, data: string): Promise<void>;
   }

   // Node implementation
   class NodeFileProvider implements FileProvider {
     async read(path) { return fs.readFileSync(path, 'utf-8'); }
   }

   // Browser implementation
   class FetchFileProvider implements FileProvider {
     async read(path) { return (await fetch(path)).text(); }
   }
   ```

3. **Use streaming abstractions**
   - Stream package requires Node `stream` module
   - Use fetch API for browser streaming instead

## Cross-runtime Safe Module Pattern

To create a module that works in both Node and browser:

1. Import ONLY dual-runtime packages
2. Avoid all imports of: `fs`, `path`, `os`, `process`, `stream`
3. Abstract file operations behind interfaces
4. Use conditional imports for platform-specific code

Example:

```javascript
// src/rdf-processor.mjs - Works in Node AND browser
import { createStore, createQuad } from '@unrdf/core/rdf';

export class RDFProcessor {
  constructor(fileProvider) {
    this.fileProvider = fileProvider;
    this.store = createStore();
  }

  async processRDF(path) {
    const data = await this.fileProvider.read(path);
    const quads = this.parse(data);
    quads.forEach(q => this.store.add(q));
    return this.store;
  }
}
```

## Generated

2025-12-27T03:06:44.714Z
