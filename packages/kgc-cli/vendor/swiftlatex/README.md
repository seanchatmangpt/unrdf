# SwiftLaTeX WASM Engine - Integration Status

## Executive Summary

**Status**: ⚠️ **Partial Implementation - Node.js Adapter Needed**

- ✅ WASM binaries identified and download script created
- ✅ API contract defined and documented
- ✅ Error handling and graceful degradation implemented
- ⚠️ Node.js adapter requires completion (browser→Node.js)
- ⚠️ XMLHttpRequest polyfill or fetch replacement needed

## What Works

1. **Binary Discovery**: SwiftLaTeX WASM binaries located in GitHub releases
2. **Download Script**: `scripts/vendor-tex-engine.mjs` downloads and extracts binaries
3. **Module Structure**: Clean API in `src/lib/latex/engine/`
4. **Error Messages**: Clear guidance when engine unavailable

## What Needs Work

### Critical Path: Node.js Adaptation

SwiftLaTeX's `swiftlatex.js` is designed for **Web Workers** in browsers:

```javascript
// Browser code (current):
self.onmessage = function(ev) {
  let data = ev['data'];
  if (data.cmd === 'compilelatex') {
    compileLaTeXRoutine();
  }
};
```

**Required changes for Node.js**:

1. **Remove Web Worker dependency**
   - Replace `self.onmessage` / `self.postMessage` with direct function calls
   - Export Module as standard ES module

2. **Replace XMLHttpRequest**
   - SwiftLaTeX downloads TeX packages from `https://texlive.swiftlatex.com/`
   - Current: Uses `XMLHttpRequest` (browser-only)
   - Needed: Use `node:https` or `fetch` (Node.js 18+)

3. **Emscripten Module Integration**
   - Load WASM binary synchronously or async
   - Initialize Emscripten FS with our VFS
   - Call `Module.compileLaTeX()` directly

## Installation

### Step 1: Download WASM Binaries

```bash
cd packages/kgc-cli
node scripts/vendor-tex-engine.mjs
```

This downloads SwiftLaTeX v15022022 (~16MB) to `vendor/swiftlatex/`:
- `swiftlatex.wasm` (2.1 MB) - TeX engine compiled to WebAssembly
- `swiftlatex.js` (99 KB) - Emscripten glue code

### Step 2: Verify Installation

```bash
ls -lh vendor/swiftlatex/
# Should show:
# - swiftlatex.wasm (2.1M)
# - swiftlatex.js (99K)
# - MANIFEST.json
```

## API Usage (Once Adapter Complete)

```javascript
import { loadEngine, runEngine } from './src/lib/latex/engine/index.mjs';

// Load engine
const engine = await loadEngine({ engine: 'pdftex', verbose: true });

// Prepare VFS
const encoder = new TextEncoder();
const vfs = new Map([
  ['main.tex', encoder.encode(`
    \\documentclass{article}
    \\begin{document}
    Hello World!
    \\end{document}
  `)]
]);

// Compile
const result = await runEngine(engine, {
  vfs,
  entry: 'main.tex',
  passes: 2
});

if (result.code === 0) {
  console.log('Success! PDF size:', result.pdf.length);
  // result.pdf is Uint8Array
} else {
  console.error('Errors:', result.errors);
  console.error('Log:', result.log);
}
```

## Alternative Approaches

If completing the SwiftLaTeX adapter proves too complex, consider:

### Option A: latexjs (Emscripten-based)

```bash
npm install latexjs
```

- ✅ Designed for Node.js
- ✅ Downloads TeX Live packages on-demand
- ✅ Active project
- ❌ Slower than SwiftLaTeX
- ❌ Larger package downloads

**Resources**:
- GitHub: https://github.com/latexjs/latexjs
- Similar to SwiftLaTeX but Node.js-first

### Option B: System LaTeX (Shell Out)

```javascript
import { spawn } from 'node:child_process';

function compilePdf(texPath) {
  return new Promise((resolve, reject) => {
    const proc = spawn('pdflatex', ['-interaction=nonstopmode', texPath]);
    // ... handle output
  });
}
```

- ✅ Fastest (native code)
- ✅ Complete TeX Live features
- ❌ Requires LaTeX installation
- ❌ Not portable

### Option C: pdf-lib (JavaScript PDF generation)

```bash
npm install pdf-lib
```

- ✅ Pure JavaScript
- ✅ No dependencies
- ❌ NOT LaTeX - must generate PDF programmatically
- ❌ No math typesetting

## Technical Deep Dive

### SwiftLaTeX Architecture

```
┌─────────────────────────────────────────┐
│         Browser (original design)       │
├─────────────────────────────────────────┤
│ PdfTeXEngine.js (TypeScript)            │
│   └─> new Worker('swiftlatexpdftex.js')│ ← Web Worker
│         └─> postMessage({ cmd: ... })   │
│         └─> onmessage handler           │
├─────────────────────────────────────────┤
│ swiftlatex.js (Emscripten glue)         │
│   ├─> self.onmessage                    │ ← Worker context
│   ├─> XMLHttpRequest (package fetch)    │ ← Browser API
│   └─> Module._compileLaTeX()            │ ← WASM call
├─────────────────────────────────────────┤
│ swiftlatex.wasm (TeX engine)            │
│   └─> C/C++ code compiled to WASM      │
└─────────────────────────────────────────┘
```

**Node.js adaptation needed**:

```
┌─────────────────────────────────────────┐
│         Node.js (target design)          │
├─────────────────────────────────────────┤
│ engine/load.mjs (our wrapper)           │
│   └─> import swiftlatex.js              │ ← ES module
│         └─> Module.loadEngine()         │
│         └─> Module.compileLaTeX()       │ ← Direct call
├─────────────────────────────────────────┤
│ swiftlatex-node.js (adapted)            │
│   ├─> export { Module }                 │ ← Remove Worker
│   ├─> https.get() (package fetch)       │ ← Replace XHR
│   └─> Module._compileLaTeX()            │ ← WASM call
├─────────────────────────────────────────┤
│ swiftlatex.wasm (TeX engine)            │
│   └─> Same binary (no changes needed)   │
└─────────────────────────────────────────┘
```

### Key Code Changes Needed

**1. Remove Worker Context** (swiftlatex.js):

```javascript
// OLD (browser):
self.onmessage = function(ev) { ... };

// NEW (Node.js):
export async function compileLaTeX(options) {
  const { files, mainfile } = options;
  // ... populate FS
  const status = Module._compileLaTeX(0);
  return { status, pdf: ... };
}
```

**2. Replace XMLHttpRequest**:

```javascript
// OLD (browser):
let xhr = new XMLHttpRequest();
xhr.open('GET', remote_url, false);
xhr.send();

// NEW (Node.js):
import https from 'node:https';
const response = await fetch(remote_url); // Or https.get()
const buffer = await response.arrayBuffer();
```

**3. Module Export**:

```javascript
// OLD (Emscripten default):
var Module = { ... };

// NEW (ES module):
export async function createModule(options) {
  return new Promise((resolve) => {
    const Module = {
      ...options,
      onRuntimeInitialized() {
        resolve(Module);
      }
    };
    // ... Emscripten init code
  });
}
```

## Estimated Effort

| Task | Complexity | Effort | Priority |
|------|------------|--------|----------|
| Remove Web Worker API | Medium | 2-4 hours | P0 |
| Replace XMLHttpRequest | Low | 1-2 hours | P0 |
| Test with minimal doc | Medium | 2-3 hours | P0 |
| Handle edge cases | High | 4-8 hours | P1 |
| Full integration tests | Medium | 3-5 hours | P1 |

**Total**: ~12-22 hours for full implementation

## Current Status Summary

### Completed (Agent 3)

- [x] Research available TeX WASM engines
- [x] Identify SwiftLaTeX as best option
- [x] Create download script for binaries
- [x] Define API contract in `src/lib/latex/engine/`
- [x] Document challenges and next steps
- [x] Provide clear error messages

### Remaining Work

- [ ] Adapt swiftlatex.js for Node.js
  - [ ] Remove Web Worker dependency
  - [ ] Replace XMLHttpRequest
  - [ ] Export as ES module
- [ ] Test minimal LaTeX compilation
- [ ] Handle package downloads
- [ ] Integration with KGC pipeline

## References

- SwiftLaTeX GitHub: https://github.com/SwiftLaTeX/SwiftLaTeX
- Emscripten Docs: https://emscripten.org/docs/
- latexjs (alternative): https://github.com/latexjs/latexjs
- TeX Live: https://www.tug.org/texlive/

---

**Last Updated**: 2025-12-27
**Agent**: Agent 3 (WASM Engine Runner)
**Next Agent**: Agent 4 (Dependency Resolver) or Agent 10 (Orchestrator)
