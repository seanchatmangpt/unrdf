# JavaScript API Reference

Programmatic usage of the LaTeX→PDF compilation pipeline.

## Installation

```bash
pnpm add @unrdf/kgc-cli
```

## Imports

```javascript
// Main compilation pipeline
import { compileLatexToPdf } from '@unrdf/kgc-cli/src/lib/latex/compile.mjs';

// Individual components (advanced)
import { collectProjectFiles } from '@unrdf/kgc-cli/src/lib/latex/project-files.mjs';
import { compileWithSwiftLatex } from '@unrdf/kgc-cli/src/lib/latex/swiftlatex-engine.mjs';
import { resolveMissingInputs } from '@unrdf/kgc-cli/src/lib/latex/ctan-resolver.mjs';
import { loadLatexLock, saveLatexLock } from '@unrdf/kgc-cli/src/lib/latex/latex-lock.mjs';
import { LatexCompileError } from '@unrdf/kgc-cli/src/lib/latex/diagnostics.mjs';
```

---

## High-Level API

### `compileLatexToPdf(options)`

Main entry point for compiling LaTeX to PDF.

**Type Signature**:
```typescript
async function compileLatexToPdf(options: {
  inputTexPath: string;
  projectDir: string;
  engine?: 'xetex' | 'pdftex' | 'luatex';
  cacheDir?: string;
  passes?: number;
}): Promise<Uint8Array>
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `inputTexPath` | `string` | Yes | - | Absolute path to main `.tex` file |
| `projectDir` | `string` | Yes | - | Absolute path to project root |
| `engine` | `'xetex' \| 'pdftex' \| 'luatex'` | No | `'xetex'` | LaTeX engine to use |
| `cacheDir` | `string` | No | `${projectDir}/.latex-cache` | Cache directory path |
| `passes` | `number` | No | `2` | Number of compilation passes (1-5) |

**Returns**: `Promise<Uint8Array>`
- Resolves with PDF file bytes on success
- Rejects with `LatexCompileError` on failure

**Throws**:
- `LatexCompileError` - Compilation failed (syntax error, missing file, etc.)
- `Error` - Invalid arguments or I/O errors

**Example**:

```javascript
import { compileLatexToPdf } from '@unrdf/kgc-cli/src/lib/latex/compile.mjs';
import { writeFile } from 'node:fs/promises';

async function main() {
  try {
    const pdfBytes = await compileLatexToPdf({
      inputTexPath: '/home/user/thesis/main.tex',
      projectDir: '/home/user/thesis',
      engine: 'xetex',
      passes: 2
    });

    await writeFile('output.pdf', pdfBytes);
    console.log('✓ PDF generated:', pdfBytes.length, 'bytes');
  } catch (error) {
    if (error instanceof LatexCompileError) {
      console.error('LaTeX compilation failed:');
      console.error('  Log:', error.logFilePath);
      console.error('  Missing:', error.missingInputs);
    } else {
      console.error('Error:', error.message);
    }
    process.exit(1);
  }
}

main();
```

**Pipeline Flow**:

```
compileLatexToPdf()
  ├─ validateInputs()                # Check files exist
  ├─ collectProjectFiles()           # Build VFS
  ├─ loadLatexLock() or create       # Load lockfile
  └─ runCompilationPipeline()
      ├─ compileWithSwiftLatex()     # Run TeX engine
      ├─ parseMissingInputsFromLog() # Detect missing files
      ├─ resolveMissingInputs()      # Fetch from CTAN
      ├─ augmentVfsWithResolvedPackages()
      └─ retry up to MAX_CYCLES
          ├─ Success → saveLatexLock() → return PDF
          └─ Failure → writeDiagnosticLog() → throw
```

---

## VFS Management API

### `collectProjectFiles(projectDir, options?)`

Collect all LaTeX-related files from a directory into a virtual file system.

**Type Signature**:
```typescript
async function collectProjectFiles(
  projectDir: string,
  options?: {
    exclude?: string[];
    include?: string[];
  }
): Promise<Map<string, Uint8Array>>
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `projectDir` | `string` | Yes | - | Absolute path to scan |
| `options.exclude` | `string[]` | No | `['node_modules', '.git', ...]` | Directories to skip |
| `options.include` | `string[]` | No | `['.tex', '.sty', '.cls', ...]` | File extensions to include |

**Returns**: `Promise<Map<string, Uint8Array>>`
- Map keys: Relative file paths (e.g., `main.tex`, `chapters/intro.tex`)
- Map values: File contents as `Uint8Array`
- Map is **sorted by keys** (deterministic)

**Example**:

```javascript
import { collectProjectFiles } from '@unrdf/kgc-cli/src/lib/latex/project-files.mjs';

const vfs = await collectProjectFiles('/path/to/thesis');

// Inspect VFS contents
console.log('Files in VFS:');
for (const [path, content] of vfs.entries()) {
  console.log(`  ${path} (${content.length} bytes)`);
}

// Output:
// Files in VFS:
//   main.tex (1234 bytes)
//   chapters/intro.tex (5678 bytes)
//   figures/logo.png (9876 bytes)
```

**Default Inclusions**:
- Source: `.tex`, `.sty`, `.cls`, `.bib`, `.bst`
- Graphics: `.pdf`, `.png`, `.jpg`, `.jpeg`, `.eps`
- Data: `.csv`, `.dat`, `.txt`
- Config: `.cfg`, `.def`, `.fd`

**Default Exclusions**:
- Build artifacts: `dist/`, `build/`, `out/`
- Dependencies: `node_modules/`
- VCS: `.git/`, `.svn/`
- Cache: `.latex-cache/`

---

## Engine API

### `compileWithSwiftLatex(options)`

Low-level interface to the SwiftLaTeX WASM engine.

**Type Signature**:
```typescript
async function compileWithSwiftLatex(options: {
  engine: 'xetex' | 'pdftex';
  vfs: Map<string, Uint8Array>;
  entry: string;
  cacheDir?: string;
  passes?: number;
  verbose?: boolean;
}): Promise<CompileResult>
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `engine` | `'xetex' \| 'pdftex'` | Yes | - | TeX engine |
| `vfs` | `Map<string, Uint8Array>` | Yes | - | Virtual file system |
| `entry` | `string` | Yes | - | Filename of main `.tex` (just filename, not path) |
| `cacheDir` | `string` | No | `'work'` | VFS working directory |
| `passes` | `number` | No | `2` | Number of passes |
| `verbose` | `boolean` | No | `false` | Verbose logging |

**Returns**: `Promise<CompileResult>`

```typescript
type CompileResult = {
  ok: boolean;                          // Compilation succeeded
  pdf?: Uint8Array;                     // PDF bytes (if ok: true)
  log: string;                          // LaTeX log output
  artifacts?: Map<string, Uint8Array>;  // .aux, .toc, etc.
  missingInputs?: string[];             // Missing files
  error?: string;                       // Error message (if ok: false)
}
```

**Example**:

```javascript
import { compileWithSwiftLatex } from '@unrdf/kgc-cli/src/lib/latex/swiftlatex-engine.mjs';
import { collectProjectFiles } from '@unrdf/kgc-cli/src/lib/latex/project-files.mjs';

// Collect files into VFS
const vfs = await collectProjectFiles('/path/to/project');

// Run engine
const result = await compileWithSwiftLatex({
  engine: 'xetex',
  vfs,
  entry: 'main.tex',
  passes: 2,
  verbose: true
});

if (result.ok) {
  console.log('✓ Compilation succeeded');
  console.log('  PDF size:', result.pdf.length, 'bytes');
  console.log('  Artifacts:', result.artifacts?.size || 0);
} else {
  console.log('✗ Compilation failed');
  console.log('  Error:', result.error);
  console.log('  Missing:', result.missingInputs);
  console.log('  Log excerpt:', result.log.slice(0, 500));
}
```

---

## Resolver API

### `resolveMissingInputs(options)`

Fetch missing LaTeX packages from CTAN.

**Type Signature**:
```typescript
async function resolveMissingInputs(options: {
  missingInputs: string[];
  cacheDir: string;
  ctanMirror?: string;
}): Promise<Map<string, Uint8Array>>
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `missingInputs` | `string[]` | Yes | - | Array of missing filenames (e.g., `['tikz.sty']`) |
| `cacheDir` | `string` | Yes | - | Cache directory path |
| `ctanMirror` | `string` | No | `'https://mirrors.ctan.org'` | CTAN mirror URL |

**Returns**: `Promise<Map<string, Uint8Array>>`
- Map keys: VFS paths (e.g., `texmf/tex/latex/tikz/tikz.sty`)
- Map values: Package contents

**Throws**:
- `Error` - If package not found or network error

**Example**:

```javascript
import { resolveMissingInputs } from '@unrdf/kgc-cli/src/lib/latex/ctan-resolver.mjs';

try {
  const resolved = await resolveMissingInputs({
    missingInputs: ['algorithm2e.sty', 'tikz.sty'],
    cacheDir: '.latex-cache'
  });

  console.log('Resolved packages:');
  for (const [vfsPath, content] of resolved.entries()) {
    console.log(`  ${vfsPath} (${content.length} bytes)`);
  }
} catch (error) {
  console.error('Failed to resolve:', error.message);
}
```

### `augmentVfsWithResolvedPackages(vfs, resolvedMap)`

Merge resolved packages into existing VFS.

**Type Signature**:
```typescript
function augmentVfsWithResolvedPackages(
  vfs: Record<string, Uint8Array>,
  resolvedMap: Map<string, Uint8Array>
): Record<string, Uint8Array>
```

**Parameters**:

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `vfs` | `Record<string, Uint8Array>` | Yes | Existing VFS object |
| `resolvedMap` | `Map<string, Uint8Array>` | Yes | Resolved packages from `resolveMissingInputs` |

**Returns**: `Record<string, Uint8Array>`
- New VFS object with merged files (does NOT mutate input)

**Example**:

```javascript
const vfs = { 'work/main.tex': new Uint8Array(...) };

const resolved = await resolveMissingInputs({
  missingInputs: ['tikz.sty'],
  cacheDir: '.latex-cache'
});

const augmentedVfs = augmentVfsWithResolvedPackages(vfs, resolved);

// augmentedVfs now has:
// - work/main.tex (original)
// - texmf/tex/latex/tikz/tikz.sty (resolved)
```

---

## Lockfile API

### `loadLatexLock(lockPath)`

Load lockfile from disk.

**Type Signature**:
```typescript
async function loadLatexLock(
  lockPath: string
): Promise<LatexLockfile | null>
```

**Returns**: `Promise<LatexLockfile | null>`
- `null` if lockfile doesn't exist or is invalid
- Lockfile object if valid

**Example**:

```javascript
import { loadLatexLock } from '@unrdf/kgc-cli/src/lib/latex/latex-lock.mjs';

const lockfile = await loadLatexLock('.latex-cache/latex.lock.json');

if (lockfile) {
  console.log('Lockfile version:', lockfile.version);
  console.log('Engine:', lockfile.engine);
  console.log('Resolved packages:', Object.keys(lockfile.resolvedInputs).length);
} else {
  console.log('No lockfile found (first build)');
}
```

### `saveLatexLock(lockPath, lockObj)`

Save lockfile to disk.

**Type Signature**:
```typescript
async function saveLatexLock(
  lockPath: string,
  lockObj: LatexLockfile
): Promise<void>
```

**Throws**: `Error` if lockfile invalid or write fails

**Example**:

```javascript
import { saveLatexLock, createLatexLock } from '@unrdf/kgc-cli/src/lib/latex/latex-lock.mjs';

const lockfile = createLatexLock('xetex');

// Add resolved input
lockfile.resolvedInputs['tikz.sty'] = {
  hash: 'a1b2c3d4...',
  cachedPath: 'texmf/tex/latex/tikz/tikz.sty',
  resolvedAt: new Date().toISOString()
};

await saveLatexLock('.latex-cache/latex.lock.json', lockfile);
```

### `createLatexLock(engine)`

Create a new empty lockfile.

**Type Signature**:
```typescript
function createLatexLock(
  engine: 'xetex' | 'pdftex' | 'luatex'
): LatexLockfile
```

**Returns**: Fresh lockfile object

**Example**:

```javascript
import { createLatexLock } from '@unrdf/kgc-cli/src/lib/latex/latex-lock.mjs';

const lockfile = createLatexLock('xetex');

console.log(lockfile);
// {
//   version: '1.0.0',
//   engine: 'xetex',
//   resolvedInputs: {},
//   createdAt: '2025-12-27T10:00:00.000Z',
//   updatedAt: '2025-12-27T10:00:00.000Z'
// }
```

### `recordResolvedInput(lockObj, entry)`

Record a resolved package in the lockfile.

**Type Signature**:
```typescript
function recordResolvedInput(
  lockObj: LatexLockfile,
  entry: {
    inputName: string;
    hash: string;
    sourceUrl?: string;
    cachedPath: string;
  }
): void
```

**Mutates**: `lockObj` in-place (caller must call `saveLatexLock` afterward)

**Example**:

```javascript
import { createLatexLock, recordResolvedInput } from '@unrdf/kgc-cli/src/lib/latex/latex-lock.mjs';

const lockfile = createLatexLock('xetex');

recordResolvedInput(lockfile, {
  inputName: 'tikz.sty',
  hash: 'a1b2c3d4e5f6...',
  sourceUrl: 'https://mirrors.ctan.org/graphics/pgf/base/tikz.sty',
  cachedPath: 'texmf/tex/latex/tikz/tikz.sty'
});

console.log(lockfile.resolvedInputs['tikz.sty']);
// {
//   hash: 'a1b2c3d4e5f6...',
//   sourceUrl: 'https://mirrors.ctan.org/graphics/pgf/base/tikz.sty',
//   cachedPath: 'texmf/tex/latex/tikz/tikz.sty',
//   resolvedAt: '2025-12-27T10:05:30.123Z'
// }
```

---

## Error Handling API

### `LatexCompileError`

Custom error class for compilation failures.

**Type Signature**:
```typescript
class LatexCompileError extends Error {
  constructor(
    message: string,
    context: {
      engine?: string;
      inputTexPath?: string;
      logFilePath?: string;
      missingInputs?: string[];
      exitCode?: string;
    }
  );

  engine: string;
  inputTexPath: string;
  logFilePath: string;
  missingInputs: string[];
  exitCode?: string;

  toJSON(): object;
}
```

**Properties**:

| Property | Type | Description |
|----------|------|-------------|
| `message` | `string` | Human-readable error message |
| `engine` | `string` | LaTeX engine used |
| `inputTexPath` | `string` | Input `.tex` file path |
| `logFilePath` | `string` | Path to diagnostic log |
| `missingInputs` | `string[]` | Missing files detected |
| `exitCode` | `string?` | Process exit code |

**Example**:

```javascript
import { compileLatexToPdf } from '@unrdf/kgc-cli/src/lib/latex/compile.mjs';
import { LatexCompileError } from '@unrdf/kgc-cli/src/lib/latex/diagnostics.mjs';

try {
  await compileLatexToPdf({
    inputTexPath: '/path/to/broken.tex',
    projectDir: '/path/to/project'
  });
} catch (error) {
  if (error instanceof LatexCompileError) {
    console.error('Compilation failed:');
    console.error('  Engine:', error.engine);
    console.error('  Input:', error.inputTexPath);
    console.error('  Log:', error.logFilePath);
    console.error('  Missing files:', error.missingInputs);

    // Serialize to JSON
    console.error(JSON.stringify(error.toJSON(), null, 2));
  }
}
```

---

## Type Definitions

### `LatexLockfile`

```typescript
type LatexLockfile = {
  version: '1.0.0';
  engine: 'xetex' | 'pdftex' | 'luatex';
  resolvedInputs: Record<string, ResolvedInput>;
  createdAt: string;  // ISO timestamp
  updatedAt: string;  // ISO timestamp
}

type ResolvedInput = {
  hash: string;           // SHA-256 content hash
  cachedPath: string;     // VFS path
  sourceUrl?: string;     // Original URL
  resolvedAt: string;     // ISO timestamp
}
```

---

## Complete Example

Full pipeline integration:

```javascript
import { promises as fs } from 'node:fs';
import { join } from 'node:path';
import {
  compileLatexToPdf,
  LatexCompileError
} from '@unrdf/kgc-cli/src/lib/latex/compile.mjs';

async function buildThesis() {
  const projectRoot = join(process.cwd(), 'thesis');
  const mainTex = join(projectRoot, 'main.tex');
  const outputPdf = join(process.cwd(), 'dist', 'thesis.pdf');

  console.log('Building thesis...');

  try {
    // Compile with XeLaTeX, 3 passes for complex references
    const pdfBytes = await compileLatexToPdf({
      inputTexPath: mainTex,
      projectDir: projectRoot,
      engine: 'xetex',
      passes: 3,
      cacheDir: join(projectRoot, '.cache')
    });

    // Ensure output directory exists
    await fs.mkdir(join(process.cwd(), 'dist'), { recursive: true });

    // Write PDF
    await fs.writeFile(outputPdf, pdfBytes);

    console.log(`✓ Success! PDF written to ${outputPdf}`);
    console.log(`  Size: ${(pdfBytes.length / 1024).toFixed(2)} KB`);

  } catch (error) {
    if (error instanceof LatexCompileError) {
      console.error('✗ LaTeX compilation failed');
      console.error(`  Log file: ${error.logFilePath}`);

      if (error.missingInputs.length > 0) {
        console.error('  Missing files:');
        error.missingInputs.forEach(file => {
          console.error(`    - ${file}`);
        });
      }

      // Read log excerpt
      const log = await fs.readFile(error.logFilePath, 'utf-8');
      const errorLines = log.split('\n').filter(line => line.startsWith('!'));
      console.error('  LaTeX errors:');
      errorLines.forEach(line => console.error(`    ${line}`));

      process.exit(1);
    } else {
      console.error('✗ Unexpected error:', error.message);
      process.exit(1);
    }
  }
}

buildThesis();
```

---

## See Also

- [CLI Reference](./cli.md) - Command-line interface
- [Lockfile Schema](./lockfile-schema.md) - Lockfile format details
- [Architecture Explanation](../explanation/architecture.md) - How it all works
- [How-To Guides](../how-to/) - Practical examples
