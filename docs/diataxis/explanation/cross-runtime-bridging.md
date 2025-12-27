# Explanation 03: Cross-Runtime Bridging

**Objective:** Understand UNRDF's strategy for running the same RDF code in Node.js, browsers, and other JavaScript runtimes.

**Audience:** Architects and library developers

**Estimated Reading Time:** 40 minutes

---

## Introduction

UNRDF is designed as a **universal RDF library** that runs in Node.js, browsers, Deno, Bun, and potentially WebAssembly environments. This article explains the architectural decisions, trade-offs, and implementation strategies for cross-runtime compatibility.

---

## The Cross-Runtime Challenge

**[Placeholder - Content to be filled]**

### Runtime Differences

| Feature | Node.js | Browser | Deno | Bun |
|---------|---------|---------|------|-----|
| File System | ✅ Full | ❌ None | ✅ Full | ✅ Full |
| Network | ✅ Full | ⚠️ CORS | ✅ Full | ✅ Full |
| Modules | ESM/CJS | ESM | ESM | ESM |
| Native Addons | ✅ Yes | ❌ No | ⚠️ Limited | ⚠️ Limited |
| WebAssembly | ✅ Yes | ✅ Yes | ✅ Yes | ✅ Yes |
| Memory | Large | Limited | Large | Large |

### Problems to Solve

1. **API Compatibility**
   - Different APIs for same operations
   - Missing features in some runtimes
   - Performance characteristics vary

2. **Module Resolution**
   - Different module systems
   - Import path differences
   - Bundler complications

3. **Dependencies**
   - Native modules (Node-only)
   - Browser-only APIs
   - Runtime-specific optimizations

**Evidence:** Analysis at `/home/user/unrdf/docs/architecture/runtime-analysis.md`

---

## UNRDF's Bridge Architecture

**[Placeholder - Bridge architecture explanation]**

### Core Strategy: Adapter Pattern

```
┌─────────────────────────────────────────────────────┐
│              UNRDF Core (Universal)                 │
│  ┌────────────────────────────────────────────┐    │
│  │  Pure JavaScript Implementation            │    │
│  │  (No runtime-specific APIs)               │    │
│  └────────────────────────────────────────────┘    │
│                      │                              │
│         ┌────────────┼────────────┐                │
│         ▼            ▼            ▼                │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐          │
│  │  Node.js │ │ Browser  │ │  Deno    │          │
│  │ Adapter  │ │ Adapter  │ │ Adapter  │          │
│  └──────────┘ └──────────┘ └──────────┘          │
└─────────────────────────────────────────────────────┘
```

**Evidence:** Adapter pattern at `/home/user/unrdf/packages/core/src/adapters/`

---

### Adapter Implementation

**[Placeholder - Adapter code examples]**

```javascript
// Node.js adapter
export const storage = {
  async read(path) {
    return fs.promises.readFile(path, 'utf-8');
  },
  async write(path, data) {
    return fs.promises.writeFile(path, data);
  }
};

// Browser adapter
export const storage = {
  async read(path) {
    const response = await fetch(path);
    return response.text();
  },
  async write(path, data) {
    // Use localStorage or IndexedDB
    localStorage.setItem(path, data);
  }
};
```

**Evidence:** Implementations at `/home/user/unrdf/packages/*/src/adapters/`

---

## Browser-Specific Challenges

**[Placeholder - Browser challenges]**

### Challenge 1: No File System

**Solution:** Virtual file system using IndexedDB

**Evidence:** Virtual FS at `/home/user/unrdf/packages/browser/src/virtual-fs.mjs`

---

### Challenge 2: CORS Restrictions

**Solution:** Proxy patterns and service workers

**Evidence:** CORS handling at `/home/user/unrdf/packages/browser/src/cors-handling.mjs`

---

### Challenge 3: Memory Limits

**Solution:** Streaming and chunking

**Evidence:** Streaming at `/home/user/unrdf/packages/streaming/src/browser-stream.mjs`

---

## WebAssembly Bridge

**[Placeholder - WASM bridge explanation]**

### Using Oxigraph (Rust) via WASM

**Evidence:** WASM integration at `/home/user/unrdf/packages/oxigraph/src/wasm-bridge.mjs`

---

## Performance Optimization

**[Placeholder - Performance strategies]**

### Per-Runtime Optimization

1. **Node.js**
   - Native modules where beneficial
   - File system caching
   - Worker threads for parallel operations

2. **Browser**
   - Web Workers
   - IndexedDB for large datasets
   - Lazy loading

3. **Shared**
   - WASM for compute-intensive operations
   - Streaming for large datasets
   - Caching strategies

**Evidence:** Optimizations at `/home/user/unrdf/packages/*/src/optimizations/`

---

## Testing Strategy

**[Placeholder - Cross-runtime testing]**

### Test Matrix

**Evidence:** Test configuration at `/home/user/unrdf/vitest.config.js`

---

## Build Process

**[Placeholder - Build pipeline]**

### Universal Bundle Generation

**Evidence:** Build scripts at `/home/user/unrdf/scripts/build-universal.sh`

---

## Trade-offs

**[Placeholder - Trade-off analysis]**

### Advantages of Universal Approach

- ✅ Single codebase
- ✅ Shared APIs
- ✅ Easier maintenance
- ✅ Knowledge reuse

### Disadvantages

- ❌ Lowest-common-denominator features
- ❌ Adapter complexity
- ❌ Testing complexity
- ❌ Performance overhead in some cases

**Evidence:** Trade-offs at `/home/user/unrdf/docs/architecture/universal-tradeoffs.md`

---

## Real-World Usage

**[Placeholder - Usage examples]**

### Example 1: Next.js Full-Stack App

**Evidence:** Example at `/home/user/unrdf/examples/nextjs-fullstack/`

---

### Example 2: Browser-Only Demo

**Evidence:** Example at `/home/user/unrdf/examples/browser-demo/`

---

### Example 3: Deno CLI Tool

**Evidence:** Example at `/home/user/unrdf/examples/deno-cli/`

---

## Future: Other Runtimes

**[Placeholder - Future runtime support]**

- React Native
- Electron
- CloudFlare Workers
- WebContainers

**Evidence:** Roadmap at `/home/user/unrdf/docs/ROADMAP.md`

---

## Conclusion

**[Placeholder - Summary]**

UNRDF achieves cross-runtime compatibility through:
- Adapter pattern for runtime-specific APIs
- Pure JavaScript core
- WebAssembly for performance
- Comprehensive testing across runtimes

This enables true "write once, run anywhere" for RDF applications.

---

## Related Reading

- **[Tutorial 02: Parse RDF in Browser](../tutorials/02-parse-rdf-in-browser.md)** - Browser usage
- **[Explanation 04: Performance Tradeoffs](./performance-tradeoffs.md)** - Performance considerations
- **[Reference: RDF Format Notes](../reference/rdf-format-notes.md)** - Format support

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
