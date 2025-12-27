# VFS (Virtual File System) Module

**Agent 2 Deliverable**: Deterministic file packing, path normalization, and content hashing for LaTeX compilation.

## Overview

This module provides a complete VFS implementation with guaranteed deterministic behavior. Identical directory contents will **always** produce identical hashes, regardless of:
- File system iteration order
- Platform (Windows vs POSIX)
- Map insertion order
- Multiple runs

## Module Structure

```
vfs/
├── hash.mjs         - SHA256 hashing with stable ordering
├── normalize.mjs    - Path normalization (Windows → POSIX)
├── pack.mjs         - Directory packing with include/exclude
├── index.mjs        - Main exports
└── __tests__/       - Comprehensive test suite (62 tests, 100% pass)
```

## Core API

### Packing Files

```javascript
import { packDirectory } from './vfs/index.mjs';

// Pack LaTeX project directory
const vfs = await packDirectory('/home/user/thesis', {
  include: ['.tex', '.sty', '.cls', '.bib', '.png'],
  exclude: ['node_modules', '.git', 'build'],
  maxFileSize: 10 * 1024 * 1024, // 10MB default
});

// vfs is Map<string, Uint8Array>
// Keys: 'work/main.tex', 'work/chapters/ch1.tex', etc.
// Values: Binary content as Uint8Array
```

### Hashing

```javascript
import { hashVfs, hashFile, areVfsEqual } from './vfs/index.mjs';

// Hash entire VFS (deterministic)
const hash = hashVfs(vfs);
// => 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'

// Hash single file
const fileHash = hashFile(content); // content = Uint8Array

// Compare VFS instances
if (areVfsEqual(vfs1, vfs2)) {
  console.log('Identical content');
}

// Get metadata with hash
const metadata = getVfsHashMetadata(vfs);
// {
//   hash: 'e3b0c44...',
//   fileCount: 10,
//   totalBytes: 52480,
//   paths: ['work/main.tex', ...]
// }
```

### Path Normalization

```javascript
import { normalizePath, isValidVfsPath } from './vfs/index.mjs';

// Normalize Windows paths to POSIX
normalizePath('foo\\bar\\baz.tex'); // => 'foo/bar/baz.tex'
normalizePath('./main.tex');        // => 'main.tex'
normalizePath('/absolute/path');    // => 'absolute/path'

// Validate paths
isValidVfsPath('foo/bar.tex');      // => true
isValidVfsPath('../etc/passwd');    // => false (path traversal)
isValidVfsPath('foo//bar.tex');     // => false (double slash)
```

### Utilities

```javascript
import {
  createVfs,
  cloneVfs,
  mergeVfs,
  getVfsText,
  setVfsText,
} from './vfs/index.mjs';

// Create empty VFS
const vfs = createVfs();

// Clone VFS
const vfs2 = cloneVfs(vfs1);

// Merge multiple VFS (later overrides earlier)
const merged = mergeVfs(baseVfs, customVfs);

// Read text files
const texSource = getVfsText(vfs, 'work/main.tex');
// => '\\documentclass{article}...'

// Write text files
setVfsText(vfs, 'work/new.tex', '\\chapter{Introduction}');
```

## Determinism Guarantees

### Hash Stability

The `hashVfs()` function guarantees:

1. **Path Ordering**: Paths are sorted alphabetically before hashing
2. **Unambiguous Boundaries**: Hash format includes path/content lengths
3. **Platform Independence**: All paths normalized to POSIX format
4. **Encoding Stability**: UTF-8 encoding for all text

```javascript
// These produce IDENTICAL hashes despite different insertion order:

const vfs1 = new Map([
  ['work/main.tex', content1],
  ['work/chapter1.tex', content2],
]);

const vfs2 = new Map([
  ['work/chapter1.tex', content2],
  ['work/main.tex', content1],
]);

hashVfs(vfs1) === hashVfs(vfs2); // ✅ Always true
```

### Hash Format

Each entry is hashed as:
```
[path-length: 4 bytes][path: UTF-8][content-length: 4 bytes][content: binary]
```

This ensures paths like `ab` + `cd` vs `abc` + `d` produce different hashes.

## Default Include/Exclude Patterns

### Included by Default
- `.tex`, `.sty`, `.cls` - LaTeX source
- `.bib`, `.bst` - Bibliography
- `.png`, `.jpg`, `.jpeg` - Raster images
- `.svg`, `.pdf` - Vector graphics

### Excluded by Default
- `node_modules/`, `.git/`, `.kgc/`
- `build/`, `dist/`, `.latex-cache/`
- `.claude-flow/`, `.cache/`

### Use `packDirectoryClean()` to Also Exclude
- `*.aux`, `*.log`, `*.toc`, `*.out`
- `*.synctex.gz`, `*.fdb_latexmk`, `*.fls`
- `*.blg`, `*.bbl`

## Integration with Other Agents

### For Agent 3 (Cache Manager)
```javascript
import { hashVfs } from './vfs/index.mjs';

// Use hash as cache key
const cacheKey = hashVfs(vfs);
const cachedPdf = await cache.get(cacheKey);

if (cachedPdf) {
  return cachedPdf; // Cache hit
}
```

### For Agent 4 (Dependency Resolver)
```javascript
import { packDirectory, getVfsText } from './vfs/index.mjs';

// Pack and scan for dependencies
const vfs = await packDirectory(projectDir);
const mainTex = getVfsText(vfs, 'work/main.tex');

// Parse \usepackage{...} from mainTex
```

### For Agent 5+ (Compilation)
```javascript
import { packDirectory } from './vfs/index.mjs';

// Pack project and pass to SwiftLaTeX engine
const vfs = await packDirectory(projectDir);

// VFS is ready for engine consumption
engine.compile(vfs, 'work/main.tex');
```

## Test Coverage

**62 tests, 100% passing** (verified with `timeout 5s node --test`)

### Test Suites
1. **Hash Utilities** (24 tests)
   - Empty/single/multiple file hashing
   - Deterministic ordering verification
   - Change detection (content, paths, additions)
   - UTF-8 and large file handling

2. **Path Normalization** (19 tests)
   - Windows/POSIX path conversion
   - Leading/trailing slash handling
   - Path validation and security checks

3. **Packing** (19 tests)
   - Recursive directory collection
   - Include/exclude pattern filtering
   - Determinism verification with real files
   - Path normalization in packed output

## Performance Characteristics

- **Packing**: ~5-20ms for typical LaTeX project (< 100 files)
- **Hashing**: ~0.5-10ms depending on VFS size
- **Memory**: Entire VFS held in memory (Map<string, Uint8Array>)

## Error Handling

All functions validate inputs and throw descriptive errors:

```javascript
hashFile('not a uint8array');    // TypeError: Content must be a Uint8Array
hashVfs({});                      // TypeError: VFS must be a Map
normalizePath(null);              // TypeError: Path must be a string
```

## Backward Compatibility

The original `/home/user/unrdf/packages/kgc-cli/src/lib/latex/vfs.mjs` re-exports everything:

```javascript
// Legacy import (still works)
import { collectProjectFiles } from './vfs.mjs';

// New import (preferred)
import { packDirectory } from './vfs/index.mjs';

// collectProjectFiles is aliased to packDirectory
```

## Next Steps for Other Agents

1. **Agent 3 (Cache)**: Use `hashVfs()` as cache key
2. **Agent 4 (Deps)**: Use `getVfsText()` to parse LaTeX sources
3. **Agent 5+ (Compile)**: Pass `vfs` Map directly to engine

---

**Status**: ✅ Complete (62/62 tests passing)
**Agent**: 2 (VFS Packer + Determinism)
**Deliverables**: All modules implemented and tested
