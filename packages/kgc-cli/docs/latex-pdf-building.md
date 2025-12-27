# LaTeX → PDF Building (Pure JavaScript)

> **Pure JavaScript LaTeX compilation using SwiftLaTeX WASM engines - zero system dependencies**

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [CLI Commands](#cli-commands)
- [Cache & Lockfile Behavior](#cache--lockfile-behavior)
- [Compilation Pipeline](#compilation-pipeline)
- [Troubleshooting Guide](#troubleshooting-guide)
- [Advanced Configuration](#advanced-configuration)
- [API Reference](#api-reference)

---

## Overview

The KGC CLI includes a complete LaTeX → PDF compilation system that runs **entirely in Node.js** using WebAssembly. No system LaTeX installation (`texlive`, `miktex`, etc.) is required.

### Key Features

| Feature | Description |
|---------|-------------|
| **Zero System Dependencies** | WASM binaries vendored at `packages/kgc-cli/vendor/swiftlatex/` |
| **Automatic Resolution** | Missing packages (`.sty`, `.cls`) are auto-fetched and cached |
| **Deterministic Builds** | Lockfile (`latex.lock.json`) ensures same inputs → same output |
| **Multi-Engine** | Switch between `pdftex` and `xetex` via CLI flag |
| **Offline Support** | Cached dependencies enable offline compilation |
| **Hermetic VFS** | Virtual file system isolates compilation from host filesystem |

### Supported Engines

- **`pdftex`** (default): Standard LaTeX → PDF engine, ASCII/Latin-1 focused
- **`xetex`**: Unicode-aware engine for multilingual documents, OpenType fonts

### Node.js Requirements

- **Minimum**: Node.js 18.0.0 (WASM support required)
- **Recommended**: Node.js 20+ (improved WASM performance)

Check your version:
```bash
node --version  # Should output v18.0.0 or higher
```

---

## Architecture

### Component Overview

```
┌─────────────────────────────────────────────────────────────┐
│                        KGC CLI                               │
│  kgc latex build --input main.tex --output thesis.pdf       │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            v
┌─────────────────────────────────────────────────────────────┐
│              Compilation Pipeline (compile.mjs)              │
│  • Validates inputs                                          │
│  • Orchestrates VFS, engine, resolver, lockfile             │
│  • Manages multi-pass compilation                           │
└───────┬─────────────────────────────────────────────────────┘
        │
        ├──> [VFS Collection] (project-files.mjs)
        │    Collects .tex, .sty, .cls, images → Map<path, bytes>
        │
        ├──> [SwiftLaTeX Engine] (swiftlatex-engine.mjs)
        │    Runs pdftex.wasm or xetex.wasm in Node.js
        │
        ├──> [Diagnostics] (diagnostics.mjs)
        │    Parses logs, extracts missing files, writes run logs
        │
        ├──> [Resolver] (resolver.mjs)
        │    Fetches missing packages from CTAN, caches them
        │
        └──> [Lockfile Manager] (latex-lock.mjs)
             Tracks resolved dependencies, ensures determinism
```

### Virtual File System (VFS)

The compiler uses an in-memory VFS (`Map<string, Uint8Array>`) to:
- **Isolate** compilation from host filesystem (no writes outside cache)
- **Collect** all project files (`.tex`, `.bib`, `.sty`, images)
- **Inject** resolved packages transparently

Example VFS structure:
```
Map {
  'work/main.tex'          => Uint8Array(1234),   // Entry point
  'work/chapters/ch1.tex'  => Uint8Array(5678),   // \input{chapters/ch1}
  'work/logo.png'          => Uint8Array(91011),  // \includegraphics{logo.png}
  'resolved/amsmath.sty'   => Uint8Array(121314)  // Auto-fetched package
}
```

---

## CLI Commands

### `kgc latex build`

Compile a LaTeX project to PDF.

#### Basic Usage

```bash
kgc latex build --input <path-to-main.tex> --output <path-to-output.pdf>
```

#### Options

| Flag | Type | Default | Description |
|------|------|---------|-------------|
| `--input` | `string` | **(required)** | Path to main `.tex` file |
| `--output` | `string` | **(required)** | Path to output PDF file |
| `--engine` | `pdftex\|xetex` | `pdftex` | LaTeX engine to use |
| `--cache` | `string` | `.kgc/cache/latex` | Cache directory for lockfile and logs |
| `--passes` | `number` | `2` | Number of compilation passes (for ToC, cross-refs) |
| `--json` | `boolean` | `false` | Output machine-readable JSON envelope |

#### Examples

```bash
# Basic compilation
kgc latex build --input thesis/main.tex --output dist/thesis.pdf

# Use XeTeX for Unicode support
kgc latex build --input thesis/main.tex --engine xetex --output thesis-xetex.pdf

# Custom cache location
kgc latex build --input main.tex --cache /tmp/latex-cache --output main.pdf

# Single-pass compilation (faster, but may miss cross-refs)
kgc latex build --input draft.tex --passes 1 --output draft.pdf

# JSON output for CI/CD
kgc latex build --input main.tex --output main.pdf --json
```

#### JSON Output Format

**Success**:
```json
{
  "ok": true,
  "data": {
    "pdfPath": "/absolute/path/to/output.pdf",
    "engine": "pdftex",
    "passes": 2,
    "cacheDir": ".kgc/cache/latex",
    "lockfilePath": ".kgc/cache/latex/latex.lock.json"
  },
  "meta": {
    "timestamp": "2025-12-27T14:30:22.123Z",
    "source": "@unrdf/kgc-cli"
  }
}
```

**Error**:
```json
{
  "ok": false,
  "code": "LATEX_COMPILE_ERROR",
  "message": "LaTeX compilation failed after 2 cycles",
  "details": {
    "engine": "pdftex",
    "inputTexPath": "/path/to/main.tex",
    "logFilePath": ".kgc/cache/latex/runs/20251227_143022_pdftex.log",
    "missingInputs": ["mystyle.sty", "logo.pdf"]
  },
  "hint": "Check log file for detailed error messages",
  "meta": {
    "timestamp": "2025-12-27T14:30:22.123Z"
  }
}
```

---

## Cache & Lockfile Behavior

### Cache Directory Structure

Default location: `.kgc/cache/latex/` (configurable via `--cache` flag)

```
.kgc/cache/latex/
├── latex.lock.json              # Dependency lockfile
├── runs/                        # Timestamped compilation logs
│   ├── 20251227_143022_pdftex.log
│   └── 20251227_150311_xetex.log
└── resolved/                    # Cached LaTeX packages
    ├── amsmath.sty
    ├── article.cls
    └── graphicx.sty
```

### Lockfile Format (`latex.lock.json`)

The lockfile records resolved dependencies for **deterministic builds**:

```json
{
  "version": "1.0.0",
  "engine": "pdftex",
  "resolvedInputs": {
    "amsmath.sty": {
      "hash": "a1b2c3d4e5f6...",
      "sourceUrl": "https://ctan.org/tex-archive/macros/latex/required/amsmath/amsmath.sty",
      "cachedPath": "/absolute/path/.kgc/cache/latex/resolved/amsmath.sty",
      "resolvedAt": "2025-12-27T14:30:22.123Z"
    }
  },
  "createdAt": "2025-12-27T14:00:00.000Z",
  "updatedAt": "2025-12-27T14:30:22.123Z"
}
```

### Cache Behavior

| Scenario | Behavior |
|----------|----------|
| **First build** | Fetches missing packages, creates lockfile |
| **Subsequent builds** | Uses cached packages, validates hashes |
| **Hash mismatch** | Invalidates cache entry, re-fetches package |
| **Engine switch** | Warns and rebuilds (lockfile tied to engine) |
| **Offline mode** | Uses cache only, fails if missing packages not cached |

### Lockfile Validation

Before reusing cached files, the compiler:
1. Checks `resolvedInputs[file].hash` matches current cache content (SHA-256)
2. If mismatch → invalidates cache, re-fetches
3. If match → reuses cached file (no network call)

---

## Compilation Pipeline

### Pipeline Stages

1. **Validation**
   - Verify input `.tex` file exists
   - Ensure cache directory is writable
   - Validate engine choice (`pdftex` or `xetex`)

2. **VFS Initialization**
   - Collect all project files (`.tex`, `.sty`, `.cls`, images)
   - Normalize paths for LaTeX consumption (`work/main.tex`)

3. **Lockfile Loading**
   - Load `latex.lock.json` if exists
   - Create empty lockfile if first build

4. **Compilation Cycles** (max 2 cycles)
   - **Pass 1**: Run LaTeX engine (e.g., `pdftex`)
   - **Pass 2**: Re-run for cross-references, ToC, bibliography
   - If errors → parse log for missing inputs
   - If missing inputs found → resolve and augment VFS → retry

5. **Success Handling**
   - Write PDF to output path
   - Update lockfile with compile metadata
   - Return PDF bytes

6. **Failure Handling**
   - Write diagnostic log to `${cacheDir}/runs/`
   - Throw `LatexCompileError` with log path
   - User can inspect log for detailed errors

### Multi-Pass Compilation

LaTeX often requires **multiple passes** to resolve:
- Cross-references (`\ref`, `\pageref`)
- Table of contents (`\tableofcontents`)
- Bibliography (`\bibliography`)
- Index (`\makeindex`)

**Default**: 2 passes (sufficient for most documents)

**Customize**:
```bash
# Single pass (faster, but may show "??" for refs)
kgc latex build --input main.tex --passes 1 --output main.pdf

# Three passes (for complex documents with index + bibliography)
kgc latex build --input thesis.tex --passes 3 --output thesis.pdf
```

---

## Troubleshooting Guide

### Missing Input Files

**Symptom**: Error message like:
```
! LaTeX Error: File 'mystyle.sty' not found.
```

**Root Cause**: LaTeX package not in project directory or cache.

**Solution**:
1. **Check logs** to identify missing file:
   ```bash
   cat .kgc/cache/latex/runs/<latest>.log | grep "not found"
   ```

2. **If auto-resolution failed**, manually add to project:
   ```bash
   # Download package
   wget https://ctan.org/path/to/mystyle.sty -O mystyle.sty

   # Re-run compilation
   kgc latex build --input main.tex --output main.pdf
   ```

3. **If package should be auto-resolved**, check resolver configuration (see [Resolver API](#resolver-api))

### Offline Compilation

**Symptom**: Compilation fails with network errors when offline.

**Root Cause**: Resolver cannot fetch missing packages.

**Solution**:
1. **First build online** to populate cache:
   ```bash
   kgc latex build --input thesis.tex --output thesis.pdf  # Fetches all dependencies
   ```

2. **Subsequent builds work offline**:
   ```bash
   # No network required - uses lockfile + cache
   kgc latex build --input thesis.tex --output thesis.pdf
   ```

### Engine Switch Issues

**Symptom**: Compilation errors or warnings after switching from `pdftex` to `xetex`.

**Root Cause**: Lockfile is tied to specific engine. Some packages behave differently across engines.

**Solution**:
1. **Use separate cache directories**:
   ```bash
   kgc latex build --input main.tex --engine pdftex --cache .cache-pdftex --output main-pdftex.pdf
   kgc latex build --input main.tex --engine xetex --cache .cache-xetex --output main-xetex.pdf
   ```

2. **Or delete lockfile** when switching:
   ```bash
   rm .kgc/cache/latex/latex.lock.json
   kgc latex build --input main.tex --engine xetex --output main.pdf  # Rebuilds lockfile
   ```

### Invalid PDF Output

**Symptom**: PDF generated but contains errors, missing content, or corrupted.

**Root Cause**: LaTeX compilation succeeded but with warnings/errors.

**Solution**:
1. **Check log file** for warnings:
   ```bash
   cat .kgc/cache/latex/runs/<latest>.log
   ```

2. **Look for**:
   - `Overfull \hbox` (text overflows margin)
   - `Underfull \hbox` (text doesn't fill line)
   - `Missing character` (font doesn't have glyph)
   - `Undefined control sequence` (typo in command)

3. **Fix LaTeX source** and re-compile

### Performance Issues

**Symptom**: Compilation takes >30 seconds for small document.

**Root Cause**: Too many passes, large images, or complex TikZ diagrams.

**Solution**:
1. **Reduce passes** for drafts:
   ```bash
   kgc latex build --input main.tex --passes 1 --output draft.pdf  # Faster
   ```

2. **Optimize images**:
   - Use compressed PNG/JPEG instead of raw formats
   - Convert large PDFs to lower resolution

3. **Profile compilation**:
   - Check log for slow package loads
   - Consider splitting large documents into chapters

---

## Advanced Configuration

### Custom Resolver

To fetch packages from non-CTAN sources, implement a custom resolver:

**Location**: `packages/kgc-cli/src/lib/latex/resolver.mjs`

**Interface**:
```javascript
/**
 * Resolve missing LaTeX packages
 * @param {string[]} missingInputs - Array of missing filenames
 * @param {string} cacheDir - Cache directory path
 * @returns {Promise<Map<string, Uint8Array>>} Resolved files (path -> bytes)
 */
export async function resolveMissingInputs(missingInputs, cacheDir) {
  const resolved = new Map();

  for (const filename of missingInputs) {
    // Custom resolution logic here
    const bytes = await fetchFromCustomSource(filename);
    resolved.set(`resolved/${filename}`, bytes);
  }

  return resolved;
}
```

### Custom VFS Provider

To include non-filesystem sources (databases, APIs, etc.) in VFS:

**Location**: `packages/kgc-cli/src/lib/latex/project-files.mjs`

**Interface**:
```javascript
/**
 * Collect project files into VFS
 * @param {string} projectDir - Project directory path
 * @returns {Promise<Map<string, Uint8Array>>} VFS map
 */
export async function collectProjectFiles(projectDir) {
  const vfs = new Map();

  // Standard filesystem scan
  await scanDirectory(projectDir, vfs);

  // Custom sources (example: fetch from database)
  const dbFiles = await fetchProjectFilesFromDB(projectDir);
  for (const [path, bytes] of dbFiles) {
    vfs.set(path, bytes);
  }

  return vfs;
}
```

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `KGC_LATEX_CACHE_DIR` | `.kgc/cache/latex` | Default cache location |
| `KGC_LATEX_ENGINE` | `pdftex` | Default LaTeX engine |
| `KGC_LATEX_MAX_CYCLES` | `2` | Max compilation retry cycles |

---

## API Reference

### Lockfile API

**Module**: `packages/kgc-cli/src/lib/latex/latex-lock.mjs`

#### `loadLatexLock(lockPath: string): Promise<Object|null>`

Load lockfile from disk. Returns `null` if missing or invalid.

#### `saveLatexLock(lockPath: string, lockObj: Object): Promise<void>`

Save lockfile to disk atomically with sorted keys for stable diffs.

#### `createLatexLock(engine: string): Object`

Create new empty lockfile for specified engine.

#### `recordResolvedInput(lockObj, entry): void`

Record a resolved input in lockfile (mutates in-place).

**Example**:
```javascript
import { createLatexLock, recordResolvedInput, saveLatexLock } from './latex-lock.mjs';

const lock = createLatexLock('pdftex');
recordResolvedInput(lock, {
  inputName: 'amsmath.sty',
  hash: 'a1b2c3...',
  sourceUrl: 'https://ctan.org/...',
  cachedPath: '/path/to/cache/amsmath.sty'
});
await saveLatexLock('.kgc/cache/latex/latex.lock.json', lock);
```

### Diagnostics API

**Module**: `packages/kgc-cli/src/lib/latex/diagnostics.mjs`

#### `parseMissingInputsFromLog(logText: string): string[]`

Extract missing filenames from LaTeX log output.

**Returns**: Array of unique missing files (sorted).

**Example**:
```javascript
import { parseMissingInputsFromLog } from './diagnostics.mjs';

const log = `
! LaTeX Error: File 'mystyle.sty' not found.
! I can't find file 'logo.pdf'.
`;

const missing = parseMissingInputsFromLog(log);
// => ['logo.pdf', 'mystyle.sty']
```

#### `writeLatexRunLog(options): Promise<string>`

Write compilation log to cache with metadata header.

**Returns**: Absolute path to written log file.

**Example**:
```javascript
import { writeLatexRunLog } from './diagnostics.mjs';

const logPath = await writeLatexRunLog({
  cacheDir: '.kgc/cache/latex',
  engine: 'pdftex',
  inputTexPath: '/path/to/main.tex',
  logText: rawLogOutput
});

console.log(`Log saved to: ${logPath}`);
// => Log saved to: .kgc/cache/latex/runs/20251227_143022_pdftex.log
```

### Compilation API

**Module**: `packages/kgc-cli/src/lib/latex/compile.mjs`

#### `compileLatexToPdf(params): Promise<Uint8Array>`

**Main entry point** for LaTeX → PDF compilation.

**Parameters**:
```javascript
{
  inputTexPath: string,   // Absolute path to main .tex file
  projectDir: string,     // Absolute path to project directory
  engine?: string,        // LaTeX engine (default: 'pdftex')
  cacheDir?: string,      // Cache directory (default: projectDir/.latex-cache)
  passes?: number         // Compilation passes (default: 2)
}
```

**Returns**: `Promise<Uint8Array>` - PDF file bytes

**Throws**: `LatexCompileError` on failure (includes log path)

**Example**:
```javascript
import { compileLatexToPdf } from './compile.mjs';
import { writeFile } from 'node:fs/promises';

try {
  const pdfBytes = await compileLatexToPdf({
    inputTexPath: '/absolute/path/to/thesis/main.tex',
    projectDir: '/absolute/path/to/thesis',
    engine: 'xetex',
    passes: 2
  });

  await writeFile('output.pdf', pdfBytes);
  console.log('✅ PDF generated successfully');
} catch (error) {
  console.error('❌ Compilation failed:', error.message);
  console.error('Log file:', error.logFilePath);
}
```

---

## See Also

- [SwiftLaTeX Project](https://www.swiftlatex.com/)
- [WASM Binary Licenses](../vendor/swiftlatex/LICENSES/)
- [KGC CLI Architecture](../README.md#architecture)
- [Extension Registry](../README.md#manifest--discovery)

---

## License

MIT - See [LICENSE](../LICENSE) for details.

SwiftLaTeX WASM binaries are licensed separately - see [vendor/swiftlatex/LICENSES/](../vendor/swiftlatex/LICENSES/) for details.
