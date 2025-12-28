# Pure JS LaTeX→PDF: Environment Constraints & Architecture

## Context

This document defines constraints and architecture for compiling LaTeX to PDF using **pure JavaScript/WebAssembly** within the Claude Code web environment. See [claude-code-vm-environment.md](./claude-code-vm-environment.md) for full environment details.

---

## Environment Summary

| Factor | Value | Impact |
|--------|-------|--------|
| Runtime | gVisor container on GCE | No shell-out to native TeX |
| Node.js | v22.21.1 | Full WASM support, ESM native |
| Network | Egress proxy with JWT auth | Vendor dependencies or allowlist |
| CPU | 16 cores (AVX-512) | Parallel compilation viable |
| RAM | ~21 GB | Large documents feasible |
| Disk | 30 GB available | Sufficient for CTAN vendoring |

---

## Hard Constraints

### MUST NOT

1. **Shell out to `pdflatex`, `xelatex`, `lualatex`** - Not installed
2. **Assume unrestricted network access** - Proxy may restrict domains
3. **Depend on system fonts** - Container has minimal fonts
4. **Use `/tmp` for large artifacts** - tmpfs may be size-limited
5. **Assume persistent storage** - Session-scoped container

### MUST

1. **Vendor all TeX/WASM binaries** - In repo or allowlisted registry
2. **Use virtual filesystem** - memfs or similar for TeX inputs
3. **Bundle required fonts** - Include in repo or WASM payload
4. **Handle CTAN packages offline** - Pre-bundle or lazy-load via proxy
5. **Return PDF as Buffer** - For downstream processing

---

## Architectural Options

### Option 1: SwiftLaTeX WASM (Recommended)

```javascript
// Architecture: Pre-compiled pdflatex.wasm + bundled CTAN
import { compile } from '@unrdf/swiftlatex-wasm';

const pdf = await compile({
  mainFile: 'main.tex',
  files: {
    'main.tex': texSource,
    'chapter1.tex': chapter1Source,
    // ...
  },
  packages: ['amsmath', 'graphicx', 'hyperref'],
});
```

**Pros**:
- Full pdflatex compatibility
- Active maintenance
- Known-good WASM compilation

**Cons**:
- WASM binary size (~30MB)
- Package management complexity

### Option 2: Texlive.js

```javascript
import { createTexlive } from 'texlive.js';

const texlive = await createTexlive({
  wasmUrl: './texlive.wasm',
  filesystemUrl: './texlive-fs.data',
});

const pdf = await texlive.compile('main.tex');
```

**Pros**:
- Full TexLive distribution in WASM
- Better package coverage

**Cons**:
- Very large (100MB+)
- Longer startup time

### Option 3: Tectonic WASM

```javascript
import { tectonic } from '@aspect/tectonic-wasm';

const pdf = await tectonic.compile(texSource, {
  fetchBundle: true, // Uses network for missing packages
});
```

**Pros**:
- Modern XeTeX-based engine
- Automatic package fetching

**Cons**:
- Requires network for package bundles
- May conflict with egress restrictions

---

## Recommended Stack

```
┌─────────────────────────────────────────────────────────────┐
│  @unrdf/latex-pipeline                                      │
├─────────────────────────────────────────────────────────────┤
│  Layer 1: Document Preparation                              │
│  ├── Zod schema validation for LaTeX structure             │
│  ├── Template interpolation (Mustache/Handlebars)           │
│  └── File virtualization (memfs)                            │
├─────────────────────────────────────────────────────────────┤
│  Layer 2: Compilation Engine                                │
│  ├── WASM TeX engine (swiftlatex-wasm)                     │
│  ├── Vendored CTAN packages (amsmath, graphicx, etc.)       │
│  └── Bundled fonts (Latin Modern, etc.)                     │
├─────────────────────────────────────────────────────────────┤
│  Layer 3: Parallel Execution                                │
│  ├── Worker pool (16 workers for 16 cores)                  │
│  ├── Chapter-level parallelism                              │
│  └── Progress reporting via EventEmitter                    │
├─────────────────────────────────────────────────────────────┤
│  Layer 4: Output Processing                                 │
│  ├── PDF as Uint8Array/Buffer                               │
│  ├── Optional: pdf-lib for post-processing                  │
│  └── Merkle hash for verification                           │
└─────────────────────────────────────────────────────────────┘
```

---

## Package Vendoring Strategy

### Core Packages (Must Vendor)

```
packages/
├── base/           # article.cls, report.cls, book.cls
├── amsmath/        # Math typesetting
├── graphicx/       # Image inclusion
├── hyperref/       # PDF hyperlinks
├── geometry/       # Page layout
├── fancyhdr/       # Headers/footers
├── booktabs/       # Professional tables
├── listings/       # Code listings
├── biblatex/       # Bibliography
└── fonts/
    └── lm/         # Latin Modern fonts
```

### Lazy-Load Strategy (Optional)

```javascript
const packageLoader = {
  async load(pkgName) {
    // Try vendored first
    const vendored = await this.tryVendored(pkgName);
    if (vendored) return vendored;

    // Fallback to CTAN mirror (if allowed by proxy)
    return this.fetchFromCTAN(pkgName);
  },

  tryVendored(pkg) {
    const path = `./packages/${pkg}`;
    return fs.existsSync(path) ? fs.readFileSync(path) : null;
  },

  async fetchFromCTAN(pkg) {
    // May be blocked by egress proxy
    try {
      return await fetch(`https://ctan.org/pkg/${pkg}`);
    } catch {
      throw new Error(`Package ${pkg} not vendored and CTAN blocked`);
    }
  },
};
```

---

## Parallel Compilation Architecture

Given 16 available cores, exploit chapter-level parallelism:

```javascript
import { Worker } from 'worker_threads';

async function compileThesis(chapters) {
  const POOL_SIZE = Math.min(chapters.length, 16);
  const workerPool = Array.from({ length: POOL_SIZE }, () =>
    new Worker('./latex-worker.mjs')
  );

  const results = await Promise.all(
    chapters.map((chapter, i) =>
      compileWithWorker(workerPool[i % POOL_SIZE], chapter)
    )
  );

  // Merge chapter PDFs
  return mergePdfs(results);
}
```

---

## Error Handling

### TeX Compilation Errors

```javascript
try {
  const pdf = await compile(source);
} catch (error) {
  if (error.type === 'TeX') {
    console.error('TeX error:', error.message);
    console.error('Line:', error.line);
    console.error('File:', error.file);
    // Provide actionable feedback
  } else if (error.type === 'PackageMissing') {
    console.error(`Missing package: ${error.package}`);
    console.error('Add to vendored packages or allowlist CTAN');
  }
}
```

### Network Errors (Egress Blocked)

```javascript
try {
  await fetchPackage('exotic-package');
} catch (error) {
  if (error.code === 'ECONNREFUSED' || error.code === 'ETIMEDOUT') {
    throw new Error(
      'Network blocked by egress proxy. ' +
      'Vendor the package or configure allowed_hosts.'
    );
  }
}
```

---

## Testing Strategy

### Unit Tests (No Network Required)

```javascript
describe('LaTeX compilation', () => {
  it('compiles minimal document', async () => {
    const tex = `\\documentclass{article}
\\begin{document}
Hello, World!
\\end{document}`;

    const pdf = await compile(tex);
    expect(pdf).toBeInstanceOf(Uint8Array);
    expect(pdf.slice(0, 5)).toEqual(new Uint8Array([0x25, 0x50, 0x44, 0x46, 0x2d])); // %PDF-
  });
});
```

### Integration Tests (Network Required)

```javascript
describe('Package fetching', () => {
  it('fetches from CTAN when allowed', async () => {
    // Only run if CTAN is allowlisted
    if (!process.env.CTAN_ALLOWED) {
      return;
    }

    const pdf = await compile(`\\usepackage{tikz}`);
    expect(pdf).toBeDefined();
  });
});
```

---

## Performance Targets

| Metric | Target | Rationale |
|--------|--------|-----------|
| Cold start | < 5s | WASM initialization |
| Single page | < 1s | Simple document |
| Full thesis (10 chapters) | < 30s | Parallel compilation |
| Memory per worker | < 500MB | 16 workers × 500MB = 8GB headroom |

---

## Related Documents

- [claude-code-vm-environment.md](./claude-code-vm-environment.md) - Full environment fingerprinting
- [bb80-20-methodology.md](./bb80-20-methodology.md) - Development methodology

---

*Document generated: 2025-12-27*
*For: Pure JS LaTeX→PDF in Claude Code Web*
