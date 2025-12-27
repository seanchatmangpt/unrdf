# WASM TeX Engine Internals

Understanding SwiftLaTeX, Emscripten, and how TeX runs in WebAssembly.

## What is SwiftLaTeX?

**SwiftLaTeX** is a complete TeX distribution compiled to WebAssembly, enabling LaTeX compilation in JavaScript environments (browsers, Node.js, Deno).

**Origin**: Port of XeTeX and PDFLaTeX using Emscripten compiler toolchain.

**Key insight**: TeX is a deterministic computation (input → output), perfect for WASM.

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│  JavaScript Runtime (Node.js / Browser)                  │
│                                                           │
│  ┌──────────────────────────────────────────────┐        │
│  │  swiftlatex-engine.mjs                        │        │
│  │  (Our wrapper code)                           │        │
│  └───────────────┬──────────────────────────────┘        │
│                  │                                        │
│                  ▼                                        │
│  ┌──────────────────────────────────────────────┐        │
│  │  SwiftLaTeX Glue Code (JavaScript)            │        │
│  │  - MEMFS (virtual filesystem)                 │        │
│  │  - Memory management                          │        │
│  │  - API bindings                               │        │
│  └───────────────┬──────────────────────────────┘        │
│                  │                                        │
│                  ▼                                        │
│  ┌──────────────────────────────────────────────┐        │
│  │  WASM Module (xetex.wasm or pdftex.wasm)     │        │
│  │  - TeX engine core (~15 MB)                   │        │
│  │  - kpathsea (file search)                    │        │
│  │  - Font rendering                             │        │
│  │  - PDF generation                             │        │
│  └──────────────────────────────────────────────┘        │
└─────────────────────────────────────────────────────────┘
```

---

## WASM Binary Details

### File Structure

```
vendor/swiftlatex/
├── xetex.wasm          # XeTeX engine (15.2 MB)
├── xetex.js            # Glue code (JavaScript bindings)
├── xetex.data          # Preloaded data (fonts, hyphenation) (28 MB)
├── pdftex.wasm         # PDFLaTeX engine (12.4 MB)
├── pdftex.js           # Glue code
└── pdftex.data         # Preloaded data (22 MB)
```

### Binary Size Breakdown

| Component | XeTeX | PDFLaTeX | Purpose |
|-----------|-------|----------|---------|
| Engine core | 8 MB | 6 MB | TeX logic, parsing, expansion |
| kpathsea | 2 MB | 2 MB | File search algorithm |
| Font rendering | 3 MB | 2 MB | Font metrics, rasterization |
| PDF generation | 2 MB | 2 MB | PDF primitives, compression |
| **Total WASM** | **15 MB** | **12 MB** | |
| Data files (.data) | 28 MB | 22 MB | Fonts, hyphenation patterns |
| **Total download** | **43 MB** | **34 MB** | |

### Loading Time

| Network Speed | XeTeX Load Time | PDFLaTeX Load Time |
|---------------|-----------------|-------------------|
| Fast (100 Mbps) | ~3s | ~2.5s |
| Moderate (10 Mbps) | ~30s | ~25s |
| Slow (1 Mbps) | ~5min | ~4min |

**Optimization**: Engines are cached after first load (browser: IndexedDB, Node.js: memory).

---

## Emscripten Compilation

### What is Emscripten?

**Emscripten** is a compiler toolchain that converts C/C++ code to WebAssembly.

**Compilation flow**:
```
XeTeX C source code
    │
    ├─ clang (LLVM frontend)
    │    → LLVM IR (intermediate representation)
    │
    ├─ LLVM optimizer
    │    → Optimized LLVM IR
    │
    └─ Emscripten backend
         → xetex.wasm (WASM binary)
         → xetex.js (glue code)
         → xetex.data (preloaded filesystem)
```

### Compilation Flags (SwiftLaTeX Build)

```bash
emcc xetex.c \
  -O3 \                          # Optimization level 3 (aggressive)
  -s WASM=1 \                    # Target WebAssembly
  -s ALLOW_MEMORY_GROWTH=1 \     # Dynamic memory allocation
  -s MODULARIZE=1 \              # Export as ES module
  -s EXPORT_NAME='XeTeX' \       # Module name
  -s EXPORTED_FUNCTIONS='[...]' \# API surface
  -s FILESYSTEM=1 \              # Enable MEMFS
  -s FORCE_FILESYSTEM=1 \        # Required for I/O
  --preload-file fonts/ \        # Bundle fonts in .data
  --preload-file hyphen/ \       # Bundle hyphenation patterns
  -o xetex.js
```

**Key flags explained**:
- `-O3`: Aggressive optimization (size + speed)
- `ALLOW_MEMORY_GROWTH`: Heap expands as needed (dynamic documents)
- `MODULARIZE`: Plays nice with ES modules
- `FILESYSTEM=1`: Enables virtual filesystem (MEMFS)
- `--preload-file`: Bundles files into `.data` blob

---

## MEMFS (Memory Filesystem)

### What is MEMFS?

Emscripten's in-memory filesystem implementation. Provides POSIX-like file API (`fopen`, `fread`, `fwrite`) that operates on RAM instead of disk.

**Structure**:
```javascript
// Internal to WASM module
MEMFS = {
  '/': {
    type: 'dir',
    children: {
      'work': {
        type: 'dir',
        children: {
          'main.tex': { type: 'file', contents: Uint8Array([...]) },
          'intro.tex': { type: 'file', contents: Uint8Array([...]) }
        }
      },
      'texmf': {
        type: 'dir',
        children: {
          'tex': { /* ... */ }
        }
      }
    }
  }
}
```

### API (JavaScript)

```javascript
// Create directory
Module.FS.mkdirTree('/work/chapters');

// Write file
Module.FS.writeFile('/work/main.tex', new Uint8Array([...]));

// Read file
const content = Module.FS.readFile('/work/main.tex');

// Check existence
const exists = Module.FS.analyzePath('/work/main.tex').exists;

// List directory
const files = Module.FS.readdir('/work');
```

### Our VFS → MEMFS Integration

```javascript
// swiftlatex-engine.mjs
async function populateMemFS(engine, vfs) {
  for (const [vfsPath, content] of vfs.entries()) {
    // Ensure parent directory exists
    const dir = dirname(vfsPath);
    if (!engine.FS.analyzePath(dir).exists) {
      engine.FS.mkdirTree(dir);
    }

    // Write file to MEMFS
    engine.FS.writeFile(`/${vfsPath}`, content);
  }
}
```

**Example**:
```javascript
// Our VFS
const vfs = new Map([
  ['work/main.tex', Uint8Array([92, 100, 111, ...])],  // \doc...
  ['texmf/tex/latex/tikz/tikz.sty', Uint8Array([...])]
]);

// Populates MEMFS as:
// /work/main.tex
// /texmf/tex/latex/tikz/tikz.sty
```

---

## Compilation Process

### Step-by-Step Execution

```javascript
// 1. Load WASM module
const wasmBinary = await readFile('vendor/swiftlatex/xetex.wasm');
const XeTeX = await loadModule(wasmBinary);

// 2. Populate MEMFS
await populateMemFS(XeTeX, vfs);

// 3. Set entry point
XeTeX.setTexContent('main.tex');

// 4. Run compilation (pass 1)
const exitCode1 = await XeTeX.compileLaTeX();
// exitCode: 0 = success, 1 = error

// 5. Run compilation (pass 2, for cross-refs)
const exitCode2 = await XeTeX.compileLaTeX();

// 6. Extract PDF
const pdfBytes = XeTeX.FS.readFile('/work/main.pdf');

// 7. Extract log
const logText = new TextDecoder().decode(
  XeTeX.FS.readFile('/work/main.log')
);
```

### Multi-Pass Compilation

**Why multiple passes?**

LaTeX generates auxiliary files (`.aux`, `.toc`) that are read on subsequent passes:

**Pass 1**:
```latex
\section{Introduction}
\label{sec:intro}

See Section~\ref{sec:results}.  % ← Reference doesn't exist yet
```

Generates `main.aux`:
```latex
\newlabel{sec:intro}{{1}{1}}
\newlabel{sec:results}{{2}{3}}
```

Output: `See Section ??` (unknown reference)

**Pass 2**:
```latex
% Now reads main.aux with label definitions
See Section~\ref{sec:results}.  % ← Reference resolves to "2"
```

Output: `See Section 2` (correct!)

**Pass 3** (rarely needed):
- Bibliography (BibTeX)
- Complex cross-references (hyperref)
- Table of contents in multi-part documents

---

## Memory Management

### Heap Size

Emscripten allocates a **growable heap**:

**Initial**: 16 MB
**Maximum**: ~2 GB (Node.js default)
**Growth**: Doubles when needed (16 → 32 → 64 → ...)

**Typical usage**:
| Document Type | Heap Usage |
|---------------|------------|
| Minimal article | ~50 MB |
| Thesis (50 pages) | ~100 MB |
| Book (200 pages) | ~200 MB |
| Large book with graphics | ~500 MB |

### Out-of-Memory Handling

```javascript
try {
  const result = await compileWithSwiftLatex({ engine, vfs, ... });
} catch (error) {
  if (error.message.includes('Out of memory')) {
    console.error('LaTeX compilation ran out of memory');
    console.error('Try:');
    console.error('  - Reducing document size');
    console.error('  - Splitting into multiple PDFs');
    console.error('  - Increasing Node.js heap: NODE_OPTIONS=--max-old-space-size=4096');
  }
  throw error;
}
```

**Increase heap**:
```bash
export NODE_OPTIONS="--max-old-space-size=4096"  # 4 GB
kgc latex build --input huge-book.tex
```

---

## Font Handling

### Bundled Fonts

SwiftLaTeX includes a minimal font set:

**XeTeX**:
- Computer Modern (TeX default)
- Latin Modern (OpenType version)
- DejaVu Sans/Serif/Mono
- Liberation Sans/Serif/Mono

**PDFLaTeX**:
- Computer Modern (Type1)
- Latin Modern (Type1)

**Total size**: ~15 MB (bundled in `.data` file)

### Custom Fonts

**Option 1**: Add to VFS

```javascript
// Load custom font
const fontBytes = await readFile('MyFont.ttf');

// Add to VFS
vfs.set('texmf/fonts/truetype/myfont/MyFont.ttf', new Uint8Array(fontBytes));

// Use in LaTeX
```

```latex
\documentclass{article}
\usepackage{fontspec}
\setmainfont{MyFont}  % XeTeX finds it in VFS
```

**Option 2**: Use system fonts (NOT recommended)
- WASM can't access system fonts (sandboxed)
- Must manually copy fonts to VFS

---

## Debugging Engine Failures

### Enable Verbose Mode

```javascript
const result = await compileWithSwiftLatex({
  engine: 'xetex',
  vfs,
  entry: 'main.tex',
  verbose: true  // ← Prints engine stdout/stderr
});
```

**Output**:
```
[XeTeX] This is XeTeX, Version 3.14159265-2.6-0.999992
[XeTeX] entering extended mode
[XeTeX] (./main.tex
[XeTeX] LaTeX2e <2021-11-15>
[XeTeX] ...
```

### Inspect MEMFS State

```javascript
// After engine initialization
console.log('MEMFS root:', engine.FS.readdir('/'));

// Check specific file
const exists = engine.FS.analyzePath('/work/main.tex').exists;
console.log('main.tex exists:', exists);

// Read file
if (exists) {
  const content = engine.FS.readFile('/work/main.tex', { encoding: 'utf8' });
  console.log('Content:', content.slice(0, 100));
}
```

### Examine Artifacts

```javascript
// After compilation, check generated files
const artifactFiles = ['.aux', '.log', '.toc', '.out'];
const artifacts = new Map();

for (const ext of artifactFiles) {
  const path = `/work/main${ext}`;
  if (engine.FS.analyzePath(path).exists) {
    artifacts.set(ext, engine.FS.readFile(path));
  }
}

console.log('Generated artifacts:', Array.from(artifacts.keys()));
```

---

## Performance Optimization

### WASM Caching

**Problem**: Loading 15 MB WASM binary is slow (~3s on fast connection)

**Solution**: Cache compiled WASM module

```javascript
// Browser: IndexedDB
const cachedModule = await idb.get('xetex-wasm-v1');
if (cachedModule) {
  return WebAssembly.instantiate(cachedModule);
}

// Node.js: In-memory (singleton)
let _cachedXeTeX = null;
async function getXeTeX() {
  if (!_cachedXeTeX) {
    _cachedXeTeX = await loadXeTeX();
  }
  return _cachedXeTeX;
}
```

**Result**: Second build is ~100x faster (no network I/O)

### Minimize VFS Size

**Problem**: Large VFS (500 MB graphics) slows population

**Solution**: Lazy-load graphics

```javascript
// Don't load all images upfront
// Instead, proxy image loading
engine.FS.mount(PROXYFS, {
  root: '/work/figures',
  loader: async (path) => {
    return await fetch(`/images/${path}`).then(r => r.arrayBuffer());
  }
}, '/work/figures');
```

### Parallel Compilation

**Future**: Multiple WASM instances for parallel chapters

```javascript
// Compile chapters in parallel
const chapters = ['intro', 'methods', 'results'];
const pdfs = await Promise.all(
  chapters.map(chapter =>
    compileWithSwiftLatex({
      engine: 'xetex',
      vfs: getChapterVFS(chapter),
      entry: `${chapter}.tex`
    })
  )
);
```

**Current limitation**: WASM modules are heavy (15 MB each), so parallelism is memory-intensive.

---

## Engine Comparison

| Feature | XeTeX | PDFLaTeX | LuaLaTeX (future) |
|---------|-------|----------|-------------------|
| **Unicode** | ✓ Full | ✗ Limited | ✓ Full |
| **Fonts** | OpenType, TrueType | Type1 only | OpenType, TrueType |
| **Speed** | Moderate (~3s) | Fast (~2s) | Slow (~5s) |
| **WASM size** | 15 MB | 12 MB | ~18 MB |
| **Memory** | ~100 MB | ~80 MB | ~150 MB |
| **Best for** | Modern docs, Unicode | Legacy docs, speed | Scripting, advanced |

**Recommendation**:
- Default: XeTeX (best balance)
- Speed-critical: PDFLaTeX (if no Unicode needed)
- Future: LuaLaTeX (when implemented)

---

## Limitations

### No System Calls

WASM can't call OS system calls directly:

**Blocked**:
- `fork()`, `exec()` (can't spawn processes)
- `socket()` (no network from WASM)
- `open("/etc/passwd")` (no arbitrary file access)

**Workaround**: Everything goes through JavaScript glue code

### No Shell Scripts

Some LaTeX packages rely on shell scripts (e.g., `gnuplot`, `pstricks` with `ps2pdf`):

```latex
\usepackage{pstricks}  % Requires external ps2pdf
```

**Error**:
```
! Package pstricks Error: Cannot run ps2pdf (shell escape disabled)
```

**Solution**: Use pure-LaTeX alternatives (e.g., TikZ instead of pstricks)

### No BibTeX (Yet)

**Status**: Not implemented in SwiftLaTeX

**Workaround**: Pre-generate `.bbl` file locally, include in VFS

```bash
# On local machine with TeX installed
pdflatex main.tex
bibtex main
# → Generates main.bbl

# Include main.bbl in VFS
vfs.set('work/main.bbl', await readFile('main.bbl'));
```

---

## Summary

The WASM engine provides:

- ✓ **Zero system dependencies**: No TeX installation required
- ✓ **Portability**: Runs anywhere JavaScript runs
- ✓ **Sandboxing**: Isolated filesystem (MEMFS)
- ✓ **Determinism**: Same input → same output
- ✓ **Performance**: Near-native speed (~3s for typical doc)

**Trade-offs**:
- ✗ Large binary (15-43 MB download)
- ✗ Memory intensive (~100-200 MB)
- ✗ No system calls (limited package support)
- ✗ No BibTeX (yet)

**Next steps**:
- Learn about [Caching strategy](./caching.md)
- Understand [VFS design](./vfs.md)
- Read [Architecture overview](./architecture.md)
