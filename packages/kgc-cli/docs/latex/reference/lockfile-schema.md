# Lockfile Schema Reference

Complete specification for `latex.lock.json` format.

## Purpose

The lockfile ensures **deterministic, reproducible builds** by:
- Pinning exact package versions via content hash
- Recording all external dependencies
- Enabling offline builds (when combined with cache)
- Facilitating cross-machine collaboration

## File Location

```
project/
├── .latex-cache/
│   └── latex.lock.json    ← Lockfile
└── main.tex
```

**Convention**: Lockfile lives in cache directory to colocate with resolved packages.

**Git**: Commit the lockfile, NOT the cache directory.

```gitignore
# .gitignore
.latex-cache/ctan/
.latex-cache/runs/

# Keep lockfile (reproducibility)
!.latex-cache/latex.lock.json
```

---

## Schema Version 1.0.0

### Top-Level Object

```json
{
  "version": "1.0.0",
  "engine": "xetex",
  "resolvedInputs": { /* ... */ },
  "createdAt": "2025-12-27T10:00:00.000Z",
  "updatedAt": "2025-12-27T10:05:30.456Z"
}
```

### Fields

#### `version` (string, required)

**Description**: Lockfile schema version (semver).

**Type**: `"1.0.0"` (literal)

**Purpose**: Future-proofing for schema evolution.

**Example**:
```json
{
  "version": "1.0.0"
}
```

**Validation**:
- Must be exactly `"1.0.0"` in current implementation
- Future versions may support `"1.1.0"`, `"2.0.0"`, etc.

---

#### `engine` (string, required)

**Description**: LaTeX engine used for compilation.

**Type**: `"xetex" | "pdftex" | "luatex"`

**Purpose**: Ensures lockfile matches compilation engine (different engines may need different packages).

**Example**:
```json
{
  "engine": "xetex"
}
```

**Validation**:
- Must be one of: `"xetex"`, `"pdftex"`, `"luatex"`
- Case-sensitive
- If you switch engines, lockfile is invalidated (must rebuild)

**Engine-specific behavior**:
- XeTeX: Better Unicode, requires Unicode-compatible packages
- PDFLaTeX: Faster, limited Unicode support
- LuaLaTeX: Scriptable, full Unicode

---

#### `resolvedInputs` (object, required)

**Description**: Map of resolved package dependencies.

**Type**: `Record<string, ResolvedInput>`

**Purpose**: Pin exact versions of all external packages.

**Example**:
```json
{
  "resolvedInputs": {
    "algorithm2e.sty": {
      "hash": "a1b2c3d4e5f6789abcdef0123456789abcdef0123456789abcdef0123456789",
      "cachedPath": "texmf/tex/latex/algorithm2e/algorithm2e.sty",
      "sourceUrl": "https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty",
      "resolvedAt": "2025-12-27T10:05:30.123Z"
    },
    "tikz.sty": {
      "hash": "f6e5d4c3b2a1098765432109876543210987654321098765432109876543210",
      "cachedPath": "texmf/tex/latex/tikz/tikz.sty",
      "sourceUrl": "https://mirrors.ctan.org/graphics/pgf/base/tikz.sty",
      "resolvedAt": "2025-12-27T10:05:31.456Z"
    }
  }
}
```

**Key**: Package filename (e.g., `"algorithm2e.sty"`, `"tikz.sty"`)
- Case-sensitive
- Must match filename in LaTeX `\usepackage{...}` or `\input{...}`

**Value**: `ResolvedInput` object (see below)

---

### `ResolvedInput` Object

```json
{
  "hash": "a1b2c3d4...",
  "cachedPath": "texmf/tex/latex/algorithm2e/algorithm2e.sty",
  "sourceUrl": "https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty",
  "resolvedAt": "2025-12-27T10:05:30.123Z"
}
```

#### Fields

**`hash` (string, required)**
- **Description**: SHA-256 hash of file content (hex-encoded, 64 characters)
- **Purpose**: Content-addressable integrity verification
- **Example**: `"a1b2c3d4e5f6789abcdef0123456789abcdef0123456789abcdef0123456789"`
- **Validation**: Must be 64-character hex string

**`cachedPath` (string, required)**
- **Description**: Virtual file system path where package is stored
- **Purpose**: Locate package in VFS during compilation
- **Example**: `"texmf/tex/latex/algorithm2e/algorithm2e.sty"`
- **Convention**:
  - `.sty` files: `texmf/tex/latex/{package}/{file}`
  - `.cls` files: `texmf/tex/latex/{package}/{file}`
  - `.bib` files: `texmf/bibtex/bib/{package}/{file}`
  - `.bst` files: `texmf/bibtex/bst/{package}/{file}`

**`sourceUrl` (string, optional)**
- **Description**: Original URL where package was fetched from
- **Purpose**: Provenance tracking, re-fetching if cache lost
- **Example**: `"https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty"`
- **Validation**: Must be valid URL (if present)

**`resolvedAt` (string, required)**
- **Description**: ISO 8601 timestamp when package was resolved
- **Purpose**: Debugging, audit trails
- **Example**: `"2025-12-27T10:05:30.123Z"`
- **Validation**: Must be valid ISO 8601 datetime with timezone

---

#### `createdAt` (string, required)

**Description**: ISO 8601 timestamp when lockfile was first created.

**Type**: ISO 8601 datetime string

**Purpose**: Audit trail, debugging.

**Example**:
```json
{
  "createdAt": "2025-12-27T10:00:00.000Z"
}
```

**Behavior**:
- Set once on initial lockfile creation
- Never updated (even when adding new packages)

---

#### `updatedAt` (string, required)

**Description**: ISO 8601 timestamp of last modification.

**Type**: ISO 8601 datetime string

**Purpose**: Track when lockfile was last changed.

**Example**:
```json
{
  "updatedAt": "2025-12-27T10:05:30.456Z"
}
```

**Behavior**:
- Updated every time lockfile is saved
- Triggered by: adding packages, changing engine, manual edits

---

## Complete Example

```json
{
  "version": "1.0.0",
  "engine": "xetex",
  "resolvedInputs": {
    "algorithm2e.sty": {
      "hash": "a1b2c3d4e5f6789abcdef0123456789abcdef0123456789abcdef0123456789",
      "cachedPath": "texmf/tex/latex/algorithm2e/algorithm2e.sty",
      "sourceUrl": "https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty",
      "resolvedAt": "2025-12-27T10:05:30.123Z"
    },
    "tikz.sty": {
      "hash": "f6e5d4c3b2a1098765432109876543210987654321098765432109876543210",
      "cachedPath": "texmf/tex/latex/tikz/tikz.sty",
      "sourceUrl": "https://mirrors.ctan.org/graphics/pgf/base/tikz.sty",
      "resolvedAt": "2025-12-27T10:05:31.456Z"
    },
    "pgfcore.sty": {
      "hash": "0987654321fedcba0987654321fedcba0987654321fedcba0987654321fedcba",
      "cachedPath": "texmf/tex/latex/pgf/pgfcore.sty",
      "sourceUrl": "https://mirrors.ctan.org/graphics/pgf/base/pgfcore.sty",
      "resolvedAt": "2025-12-27T10:05:32.789Z"
    }
  },
  "createdAt": "2025-12-27T10:00:00.000Z",
  "updatedAt": "2025-12-27T10:05:32.789Z"
}
```

---

## Formatting and Determinism

### JSON Formatting

The lockfile uses **stable, sorted JSON** for deterministic diffs:

**Rules**:
1. **Sorted keys**: Object keys in alphabetical order
2. **2-space indentation**: Consistent formatting
3. **Trailing newline**: POSIX-compliant text file

**Example** (before sorting):
```json
{"updatedAt": "...", "version": "1.0.0", "engine": "xetex"}
```

**Example** (after sorting):
```json
{
  "createdAt": "...",
  "engine": "xetex",
  "resolvedInputs": {},
  "updatedAt": "...",
  "version": "1.0.0"
}
```

### Why Stable Formatting?

**Git diffs**: Adding a new package shows minimal diff:

```diff
   "resolvedInputs": {
     "algorithm2e.sty": { /* ... */ },
+    "tikz.sty": {
+      "hash": "f6e5d4c3b2a1...",
+      "cachedPath": "texmf/tex/latex/tikz/tikz.sty",
+      "sourceUrl": "https://mirrors.ctan.org/graphics/pgf/base/tikz.sty",
+      "resolvedAt": "2025-12-27T10:05:31.456Z"
+    }
   },
-  "updatedAt": "2025-12-27T10:05:30.456Z"
+  "updatedAt": "2025-12-27T10:05:31.456Z"
 }
```

**Merge conflicts**: Easier to resolve (keys in predictable order).

---

## Validation Schema (Zod)

Programmatic validation:

```javascript
import { z } from 'zod';

const ResolvedInputSchema = z.object({
  hash: z.string().length(64).regex(/^[a-f0-9]{64}$/),
  sourceUrl: z.string().url().optional(),
  cachedPath: z.string().min(1),
  resolvedAt: z.string().datetime()
});

const LatexLockSchema = z.object({
  version: z.literal('1.0.0'),
  engine: z.enum(['xetex', 'pdftex', 'luatex']),
  resolvedInputs: z.record(z.string(), ResolvedInputSchema),
  createdAt: z.string().datetime(),
  updatedAt: z.string().datetime()
});

// Usage
const lockfile = JSON.parse(await fs.readFile('latex.lock.json', 'utf-8'));
const validated = LatexLockSchema.parse(lockfile);
```

---

## Lockfile Lifecycle

### Creation

```bash
# First build creates lockfile
kgc latex build --input main.tex
```

Creates:
```json
{
  "version": "1.0.0",
  "engine": "xetex",
  "resolvedInputs": {},
  "createdAt": "2025-12-27T10:00:00.000Z",
  "updatedAt": "2025-12-27T10:00:00.000Z"
}
```

### Population

```latex
% main.tex
\usepackage{algorithm2e}  % Not in cache
```

After build:
```json
{
  "version": "1.0.0",
  "engine": "xetex",
  "resolvedInputs": {
    "algorithm2e.sty": {
      "hash": "a1b2c3d4...",
      "cachedPath": "texmf/tex/latex/algorithm2e/algorithm2e.sty",
      "sourceUrl": "https://mirrors.ctan.org/...",
      "resolvedAt": "2025-12-27T10:05:30.123Z"
    }
  },
  "createdAt": "2025-12-27T10:00:00.000Z",
  "updatedAt": "2025-12-27T10:05:30.456Z"
}
```

### Updates

Adding new package:
```latex
\usepackage{tikz}  % New package
```

Lockfile grows:
```json
{
  "resolvedInputs": {
    "algorithm2e.sty": { /* existing */ },
    "tikz.sty": { /* new */ }
  },
  "updatedAt": "2025-12-27T10:10:00.000Z"  // Updated
}
```

### Integrity Verification

On subsequent builds:
1. Load lockfile
2. Check cache has all `resolvedInputs`
3. Verify file hashes match
4. Use cached files (no network fetch)

If hash mismatch:
```
⚠ Cache hash mismatch for algorithm2e.sty
  Expected: a1b2c3d4...
  Got: 0000aaaa...
  Re-fetching from CTAN...
```

---

## Advanced Usage

### Lockfile Merging

When two developers add different packages:

**Alice's lockfile**:
```json
{
  "resolvedInputs": {
    "algorithm2e.sty": { /* ... */ }
  }
}
```

**Bob's lockfile**:
```json
{
  "resolvedInputs": {
    "tikz.sty": { /* ... */ }
  }
}
```

**Merge conflict resolution**:
1. Accept either version
2. Rebuild: `kgc latex build --input main.tex`
3. Pipeline merges both packages automatically

**Result**:
```json
{
  "resolvedInputs": {
    "algorithm2e.sty": { /* ... */ },
    "tikz.sty": { /* ... */ }
  }
}
```

### Manual Editing

**Allowed**:
- Remove obsolete packages (after removing from `.tex`)
- Update `engine` (if intentionally switching)

**Not recommended**:
- Changing `hash` (breaks integrity verification)
- Changing `cachedPath` (breaks VFS lookup)

**Example**: Remove unused package:
```json
{
  "resolvedInputs": {
    "algorithm2e.sty": { /* ... */ },
    // Removed "tikz.sty" (no longer used in .tex)
  }
}
```

Rebuild verifies lockfile matches actual usage.

---

## Troubleshooting

### Invalid Lockfile

**Error**:
```
⚠ Invalid lockfile at .latex-cache/latex.lock.json
  Expected version: "1.0.0"
  Got: "0.9.0"
```

**Solution**: Delete lockfile, rebuild:
```bash
rm .latex-cache/latex.lock.json
kgc latex build --input main.tex
```

### Missing `resolvedInputs`

**Error**:
```
✗ Missing input: tikz.sty
  Lockfile has no entry for tikz.sty
```

**Diagnosis**: Lockfile is stale (package added to `.tex` but not resolved).

**Solution**: Rebuild (auto-resolves):
```bash
kgc latex build --input main.tex
```

### Hash Mismatch

**Error**:
```
⚠ Cache hash mismatch for algorithm2e.sty
```

**Cause**: Cached file was modified or corrupted.

**Solution**: Clear cache (keeps lockfile, re-downloads):
```bash
rm -rf .latex-cache/ctan/
kgc latex build --input main.tex
```

---

## See Also

- [Deterministic Builds How-To](../how-to/deterministic-builds.md) - Using lockfiles
- [Caching Explanation](../explanation/caching.md) - How cache + lockfile work together
- [API Reference](./api.md) - Programmatic lockfile management
