# LaTeX CTAN Resolver

**Agent 4 (Package Resolver)** - Optional CTAN fetch/cache system for missing LaTeX inputs.

## Overview

This module provides deterministic package resolution for LaTeX compilation. When the engine runner (Agent 3) detects missing inputs (`.sty`, `.cls`, `.bib`, `.bst`), this resolver:

1. Checks local cache (content-hash indexed)
2. Fetches from CTAN mirror if not cached
3. Returns VFS-compatible `Map<string, Uint8Array>`
4. Maintains deterministic cache with `index.json`

**Key Properties**:
- ✅ Pure functions (no OTEL in business logic)
- ✅ Deterministic caching (content-addressed)
- ✅ Offline-safe (clear error messages)
- ✅ Self-healing (corrupted cache rebuilds automatically)

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Agent 10 (Synthesis Editor)                                 │
│   ├─ Runs LaTeX compilation                                 │
│   └─ Detects missing inputs from log                        │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ Agent 3 (Engine Runner)                                      │
│   ├─ Parses missing file list                               │
│   └─ Calls resolveMissingInputs()                           │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ Agent 4 (Package Resolver) ← THIS MODULE                    │
│   ├─ Load cache index.json                                  │
│   ├─ Check each input:                                      │
│   │   ├─ If cached: validate hash, return content           │
│   │   └─ If not: fetch from CTAN, cache, return             │
│   └─ Return Map<vfsPath, Uint8Array>                        │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ augmentVfsWithResolvedPackages()                            │
│   └─ Merges resolved files into VFS                         │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ Agent 3 (Engine Runner)                                      │
│   └─ Re-runs LaTeX with complete VFS                        │
└─────────────────────────────────────────────────────────────┘
```

## Cache Structure

```
${cacheDir}/ctan/
├── index.json                  # Mapping: inputName -> cache metadata
└── files/
    ├── a1b2c3d4e5f6...sty     # Content-hash filenames
    ├── f6e5d4c3b2a1...cls
    └── 123456789abc...bst
```

## API Reference

### resolveMissingInputs(options)

Main resolver function called by Agent 3.

**Parameters**:
- `options.missingInputs`: `string[]` - Missing filenames
- `options.cacheDir`: `string` - Cache directory path
- `options.ctanMirror`: `string` (optional) - CTAN mirror URL

**Returns**: `Promise<Map<string, Uint8Array>>`

**Example**:
```javascript
const resolved = await resolveMissingInputs({
  missingInputs: ['algorithm2e.sty', 'tikz.sty'],
  cacheDir: '/home/user/.cache/kgc-latex'
});
```

### augmentVfsWithResolvedPackages(vfs, resolvedMap)

Merges resolved packages into VFS.

**Returns**: New VFS with merged files (does not mutate input)

## Files Delivered

| File | LOC | Purpose |
|------|-----|---------|
| `ctan-resolver.mjs` | 585 | Main resolver |
| `ctan-resolver.test.mjs` | 220 | Unit tests |
| `README.md` | This file | Documentation |

## Build Timestamp

Generated: 2025-12-27 (Agent 4 delivery)
