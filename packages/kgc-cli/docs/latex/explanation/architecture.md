# Architecture: How the Pipeline Works

Understanding the LaTeX→PDF compilation pipeline design and implementation.

## Overview

The pipeline implements a **pure JavaScript LaTeX→PDF compiler** with zero system dependencies. It achieves this through:

1. **WebAssembly TeX Engine**: SwiftLaTeX (XeTeX/PDFLaTeX) compiled to WASM
2. **Virtual File System**: In-memory representation of project files
3. **Automatic Dependency Resolution**: Fetch missing packages from CTAN
4. **Deterministic Builds**: Lockfile pins package versions
5. **Agent-Based Architecture**: Modular components with clear contracts

## Agent-Based Design

The pipeline is organized into specialized agents (following the 10-agent swarm pattern):

```
┌────────────────────────────────────────────────────────────┐
│ Agent 10: Compilation Pipeline Orchestrator                │
│ (compile.mjs)                                               │
└────────────────────────────────────────────────────────────┘
              │
              │  Orchestrates
              │
    ┌─────────┼─────────┬─────────┬─────────┬─────────┐
    │         │         │         │         │         │
    ▼         ▼         ▼         ▼         ▼         ▼
┌────────┐┌────────┐┌────────┐┌────────┐┌────────┐┌────────┐
│Agent 2 ││Agent 3 ││Agent 4 ││Agent 5 ││Agent 6 ││Agent 7 │
│VFS     ││Engine  ││CTAN    ││Lockfile││Diagnos-││CLI     │
│Collect ││Runner  ││Resolver││Manager ││tics    ││Extensn │
└────────┘└────────┘└────────┘└────────┘└────────┘└────────┘
```

### Agent Responsibilities

**Agent 2: VFS Collector** (`project-files.mjs`)
- Scan project directory
- Collect `.tex`, `.sty`, `.cls`, images, etc.
- Build sorted `Map<string, Uint8Array>`
- Pure function (deterministic)

**Agent 3: Engine Runner** (`swiftlatex-engine.mjs`)
- Load WASM binary (XeTeX or PDFLaTeX)
- Populate VFS from Map
- Execute compilation (1-5 passes)
- Extract PDF and artifacts
- Parse error logs

**Agent 4: CTAN Resolver** (`ctan-resolver.mjs`)
- Detect missing packages from log
- Fetch from CTAN mirrors
- Cache with content-hash filenames
- Return `Map<vfsPath, Uint8Array>`

**Agent 5: Lockfile Manager** (`latex-lock.mjs`)
- Load/save `latex.lock.json`
- Record resolved inputs with SHA-256 hashes
- Validate cache integrity
- Merge lockfiles (collaboration)

**Agent 6: Diagnostics** (`diagnostics.mjs`)
- Parse LaTeX error logs
- Extract missing file names
- Write diagnostic artifacts
- Provide structured errors

**Agent 7: CLI Extension** (`latex.mjs`)
- Integrate with `kgc` CLI
- Parse arguments (Zod schemas)
- Call Agent 10 orchestrator
- Format output (JSON envelope)

**Agent 10: Orchestrator** (`compile.mjs`)
- **This document's focus**
- Coordinates all agents
- Implements retry logic (missing inputs)
- Manages compilation cycles
- Enforces invariants (lockfile consistency)

---

## Data Flow

### Inputs

```javascript
compileLatexToPdf({
  inputTexPath: '/absolute/path/to/main.tex',
  projectDir: '/absolute/path/to/project',
  engine: 'xetex',
  cacheDir: '.latex-cache',  // Optional
  passes: 2                   // Optional
})
```

### Internal Transformations

```
┌─────────────────────────────────────────────────────────────┐
│ 1. INPUT VALIDATION                                          │
│    - File exists                                             │
│    - Directory exists                                        │
│    - Cache creatable                                         │
└─────────────────────────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. VFS INITIALIZATION (Agent 2)                              │
│    collectProjectFiles(projectDir)                           │
│    → Map<'main.tex', Uint8Array>                             │
│      Map<'chapters/intro.tex', Uint8Array>                   │
│      Map<'figures/logo.png', Uint8Array>                     │
└─────────────────────────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. LOCKFILE LOADING (Agent 5)                                │
│    loadLatexLock('.latex-cache/latex.lock.json')             │
│    → Lockfile object OR null (first build)                   │
│    If null: createLatexLock(engine)                          │
└─────────────────────────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│ 4. COMPILATION CYCLE (up to 2 iterations)                    │
│                                                               │
│    ┌──────────────────────────────────────────┐              │
│    │ 4a. Execute Compilation (Agent 3)        │              │
│    │     compileWithSwiftLatex({              │              │
│    │       engine, vfs, entry, passes         │              │
│    │     })                                    │              │
│    │     → { ok, pdf?, log, missingInputs? }  │              │
│    └──────────────────────────────────────────┘              │
│                       │                                       │
│              ┌────────┴────────┐                              │
│              │                 │                              │
│        ok=true          ok=false                              │
│              │                 │                              │
│              ▼                 ▼                              │
│    ┌──────────────┐   ┌──────────────────────┐              │
│    │ SUCCESS      │   │ 4b. Parse Errors      │              │
│    │ Save lockfile│   │     (Agent 6)         │              │
│    │ Return PDF   │   │     Extract missing   │              │
│    └──────────────┘   │     inputs from log   │              │
│                       └──────────────────────┘              │
│                                 │                              │
│                       ┌─────────┴─────────┐                  │
│                       │                   │                  │
│              Missing inputs      No missing inputs            │
│                       │                   │                  │
│                       ▼                   ▼                  │
│            ┌────────────────────┐  ┌────────────┐            │
│            │ 4c. Resolve (Agent 4│  │ Give Up    │            │
│            │     resolveMissing  │  │ Write log  │            │
│            │     Inputs({        │  │ Throw error│            │
│            │       missing,      │  └────────────┘            │
│            │       cacheDir      │                            │
│            │     })              │                            │
│            │     → Map<vfs, data>│                            │
│            └────────────────────┘                            │
│                       │                                       │
│                       ▼                                       │
│            ┌────────────────────┐                            │
│            │ 4d. Augment VFS    │                            │
│            │     Update lockfile│                            │
│            │     Retry cycle    │                            │
│            └────────────────────┘                            │
│                       │                                       │
│                       └──────────┘ (back to 4a)               │
└─────────────────────────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│ 5. OUTPUT                                                     │
│    - Success: Uint8Array (PDF bytes)                         │
│    - Failure: LatexCompileError                              │
└─────────────────────────────────────────────────────────────┘
```

---

## Compilation Cycle Deep Dive

### Why Multiple Cycles?

LaTeX often requires files that don't exist until after compilation:

**Cycle 1**: Discovers missing packages
```latex
\usepackage{algorithm2e}  % Not in VFS
```

Compilation fails with:
```
! LaTeX Error: File `algorithm2e.sty' not found.
```

**Between Cycles**: Resolver fetches `algorithm2e.sty` from CTAN

**Cycle 2**: Compilation succeeds with complete VFS

### Cycle Limit

**MAX_COMPILATION_CYCLES = 2**

**Why not more?**
- Most documents resolve in 2 cycles
- Prevents infinite loops (circular dependencies)
- Forces explicit error handling

**Edge case**: If cycle 2 discovers NEW missing inputs:
- Pipeline gives up (reports all missing inputs)
- User must manually add missing files or debug

---

## VFS (Virtual File System)

### Structure

The VFS is a `Map<string, Uint8Array>` with specific path conventions:

```javascript
Map {
  // Project files (relative paths)
  'work/main.tex' => Uint8Array([...]),
  'work/chapters/intro.tex' => Uint8Array([...]),
  'work/figures/logo.png' => Uint8Array([...]),

  // Resolved CTAN packages (texmf paths)
  'texmf/tex/latex/algorithm2e/algorithm2e.sty' => Uint8Array([...]),
  'texmf/tex/latex/tikz/tikz.sty' => Uint8Array([...]),

  // Engine binaries and data
  'xetex.wasm' => Uint8Array([...]),
  'xetex.data' => Uint8Array([...])
}
```

### Path Conventions

| File Type | VFS Path | Example |
|-----------|----------|---------|
| Project `.tex` | `work/{relative-path}` | `work/main.tex` |
| Project images | `work/{relative-path}` | `work/figures/logo.png` |
| CTAN `.sty` | `texmf/tex/latex/{pkg}/{file}` | `texmf/tex/latex/tikz/tikz.sty` |
| CTAN `.cls` | `texmf/tex/latex/{pkg}/{file}` | `texmf/tex/latex/beamer/beamer.cls` |
| CTAN `.bib` | `texmf/bibtex/bib/{pkg}/{file}` | `texmf/bibtex/bib/biblatex/biblatex.bib` |

### Why VFS?

**Benefits**:
1. **Isolation**: No filesystem pollution
2. **Determinism**: Sorted keys ensure stable ordering
3. **Testability**: Mock files easily
4. **Portability**: Works in browser, Node, Deno
5. **Security**: Sandboxed (no arbitrary file access)

---

## WASM Engine Integration

### Engine Loading

```javascript
// Agent 3: swiftlatex-engine.mjs

// Load WASM binary
const wasmPath = resolve(__dirname, '../vendor/swiftlatex/xetex.wasm');
const wasmBinary = await readFile(wasmPath);

// Initialize engine (platform-specific glue code)
const engine = await initializeXeTeX(wasmBinary);
```

### Compilation Execution

```javascript
// Populate VFS
for (const [path, content] of vfs.entries()) {
  engine.writeMemFSFile(path, content);
}

// Set main entry
engine.setTexContent('main.tex');

// Run compilation (multiple passes)
for (let pass = 1; pass <= passes; pass++) {
  const exitCode = await engine.compileLaTeX();
  if (exitCode !== 0 && pass < passes) {
    // Non-final pass failure is expected (cross-refs)
    continue;
  }
}

// Extract outputs
const pdfBytes = engine.getFileContent('main.pdf');
const logText = engine.getFileContent('main.log');
```

### Engine Capabilities

| Engine | Unicode | Fonts | Speed | Use Case |
|--------|---------|-------|-------|----------|
| XeTeX | ✓ Full | OpenType, TrueType | Moderate | Modern documents, Unicode |
| PDFLaTeX | ✗ Limited | Type1 | Fast | Legacy documents, ASCII |
| LuaLaTeX | ✓ Full | OpenType, TrueType | Slow | Scripting, advanced typography |

---

## Dependency Resolution Strategy

### Detection

After compilation fails, Agent 6 parses the log:

```javascript
// diagnostics.mjs
const missingInputs = parseMissingInputsFromLog(logText);
// => ['algorithm2e.sty', 'tikz.sty']
```

**Patterns matched**:
- `! LaTeX Error: File 'X' not found.`
- `! I can't find file 'X'`
- `(X.sty not found)`

### Resolution

Agent 4 attempts CTAN fetch:

```javascript
// ctan-resolver.mjs
const resolved = await resolveMissingInputs({
  missingInputs: ['algorithm2e.sty'],
  cacheDir: '.latex-cache'
});

// Returns Map {
//   'texmf/tex/latex/algorithm2e/algorithm2e.sty' => Uint8Array([...])
// }
```

**CTAN URL construction**:
```
https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty
https://mirrors.ctan.org/macros/latex/required/algorithm2e/algorithm2e.sty
https://mirrors.ctan.org/macros/latex/base/algorithm2e.sty
```

Tries all three; uses first successful response.

### Caching

Resolved packages are cached with **content-hash filenames**:

```
.latex-cache/ctan/
├── index.json
│   {
│     "algorithm2e.sty": {
│       "hash": "a1b2c3d4...",
│       "path": "files/a1b2c3d4...sty",
│       "url": "https://mirrors.ctan.org/...",
│       "timestamp": 1703686200000
│     }
│   }
└── files/
    └── a1b2c3d4e5f6789abcdef0123456789abcdef0123456789abcdef0123456789.sty
```

**Content addressing benefits**:
- Deduplication (same content = same file)
- Integrity verification (hash mismatch = corruption)
- Immutable (hash never changes for given content)

---

## Lockfile Integration

### Recording Dependencies

After successful resolution:

```javascript
// Agent 5: latex-lock.mjs
recordResolvedInput(lockfile, {
  inputName: 'algorithm2e.sty',
  hash: computeHash(content),
  sourceUrl: 'https://mirrors.ctan.org/...',
  cachedPath: 'texmf/tex/latex/algorithm2e/algorithm2e.sty'
});
```

### Verification on Subsequent Builds

```javascript
// Load lockfile
const lockfile = await loadLatexLock(lockPath);

// Verify cache matches
for (const [inputName, entry] of Object.entries(lockfile.resolvedInputs)) {
  const cachedContent = await readCachedFile(entry.cachedPath);
  const actualHash = computeHash(cachedContent);

  if (actualHash !== entry.hash) {
    throw new Error(`Cache corruption: ${inputName}`);
  }
}
```

**Guarantees**:
- Same lockfile → same packages (bit-identical)
- Cross-machine reproducibility
- Offline builds (cache + lockfile)

---

## Error Handling Philosophy

### Fail-Fast Validation

```javascript
// Before any work, validate inputs
await validateInputs({
  inputTexPath,
  projectDir,
  cacheDir
});
// Throws immediately if files missing
```

### Structured Errors

```javascript
class LatexCompileError extends Error {
  constructor(message, { logFilePath, missingInputs, ... }) {
    super(message);
    this.logFilePath = logFilePath;
    this.missingInputs = missingInputs;
  }
}
```

**Benefits**:
- Programmatic error inspection
- JSON serialization (`toJSON()`)
- Clear debugging path (log file location)

### Diagnostic Artifacts

```
.latex-cache/runs/
└── 20251227_103045_xetex.log    # Full compilation log
```

**Preservation**: Logs kept indefinitely for debugging

---

## Performance Characteristics

### Compilation Time

| Document Type | Size | Passes | Time (cold) | Time (warm) |
|---------------|------|--------|-------------|-------------|
| Minimal article | 1 page | 2 | ~2s | ~0.5s |
| Thesis (no packages) | 50 pages | 2 | ~5s | ~2s |
| Thesis (10 packages) | 50 pages | 2 | ~15s | ~3s |
| Book (100+ pages) | 200 pages | 3 | ~30s | ~10s |

**Cold**: First build (engine init + package fetch)
**Warm**: Subsequent builds (cache hits)

### Memory Usage

| Component | RAM |
|-----------|-----|
| Node.js baseline | ~50 MB |
| WASM engine | ~100 MB |
| VFS (typical thesis) | ~10 MB |
| PDF output buffer | ~2 MB |
| **Total** | **~160 MB** |

**Scaling**: +1 MB per 10 MB of project files

---

## Determinism Guarantees

### Sources of Non-Determinism (Addressed)

| Source | Problem | Solution |
|--------|---------|----------|
| File ordering | Filesystem iteration order varies | Sorted VFS (alphabetical keys) |
| Package versions | CTAN updates packages | Lockfile with SHA-256 hashes |
| Timestamps | PDF metadata includes build time | User controls via LaTeX (or strip metadata) |
| Random seeds | Some packages use randomness | (User responsibility) |

### Verification

```bash
# Build 1
kgc latex build --input main.tex --output build1.pdf
sha256sum build1.pdf > hash1.txt

# Build 2 (same machine, same time)
kgc latex build --input main.tex --output build2.pdf
sha256sum build2.pdf > hash2.txt

# Compare
diff hash1.txt hash2.txt
# (no output = identical hashes)
```

---

## Extensibility Points

### Custom VFS Provider

```javascript
// Instead of collectProjectFiles, provide custom VFS
const customVfs = new Map([
  ['work/main.tex', Buffer.from('\\documentclass{article}...')],
  ['work/logo.png', await fetch('https://example.com/logo.png').then(r => r.arrayBuffer())]
]);

const pdf = await compileWithSwiftLatex({ engine: 'xetex', vfs: customVfs, ... });
```

### Custom Resolver

```javascript
// Replace CTAN resolver with private package repository
async function customResolver({ missingInputs }) {
  const resolved = new Map();
  for (const input of missingInputs) {
    const content = await fetchFromPrivateRepo(input);
    resolved.set(`work/${input}`, content);
  }
  return resolved;
}
```

### Custom Lockfile Location

```javascript
// Use project-specific lockfile path
const lockfile = await loadLatexLock('/custom/path/latex.lock.json');
```

---

## Comparison with Traditional TeX

| Aspect | Traditional TeX | This Pipeline |
|--------|-----------------|---------------|
| **Installation** | tlmgr, apt, brew | `pnpm add @unrdf/kgc-cli` |
| **Dependencies** | ~2GB TeX distribution | ~50MB WASM binaries |
| **Package management** | tlmgr update | Automatic CTAN fetch + lockfile |
| **Reproducibility** | Manual pinning | Built-in lockfile |
| **Offline support** | Requires full TeX Live | Cache + lockfile |
| **Sandboxing** | Full filesystem access | VFS isolation |
| **Portability** | Platform-specific | Pure JavaScript (Node, browser) |

---

## Future Architecture

### Planned Improvements

1. **LuaTeX support**: Add LuaLaTeX engine (scriptable)
2. **Parallel compilation**: Multi-threaded engine execution
3. **Incremental builds**: Cache intermediate `.aux` files
4. **Custom fonts**: VFS font injection
5. **Bibliography support**: BibTeX/Biber integration

### Non-Goals

- **Full TeX Live compatibility**: Only support common packages
- **GUI**: CLI and API only (GUI = separate package)
- **Online editor**: This is a build tool, not an editor

---

## Summary

The architecture prioritizes:

- **Modularity**: Agent-based design with clear contracts
- **Determinism**: Sorted VFS, lockfile, content addressing
- **Usability**: Automatic dependency resolution
- **Portability**: Pure JavaScript, WASM engines
- **Transparency**: Diagnostic logs, structured errors

**Next steps**:
- Understand [VFS design](./vfs.md)
- Learn about [WASM engine internals](./engine.md)
- Read [Caching strategy](./caching.md)
