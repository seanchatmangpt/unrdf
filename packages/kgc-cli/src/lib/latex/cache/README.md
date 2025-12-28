# Cache Module

**Purpose**: Compilation cache management and CTAN package caching.

## Contents

Future files:
- `manager.mjs` - Cache CRUD operations
- `manifest.mjs` - Cache manifest (cache/manifest.json) management
- `cleanup.mjs` - LRU cleanup and size management
- `integrity.mjs` - Cache integrity verification (hash checking)

## Cache Structure

```
.latex-cache/
├── manifest.json           # Cache metadata
├── packages/              # CTAN packages
│   ├── amsmath.sty
│   ├── geometry.sty
│   └── ...
├── artifacts/             # Compilation artifacts (.aux, .toc)
│   └── [hash]/
│       ├── main.aux
│       └── main.log
└── latex.lock.json        # Dependency lockfile
```

## Cache Key

Cache keys are 16-character SHA-256 hash prefixes based on:
- Engine (xetex/pdftex)
- Compiler version
- Input file path

See `/home/user/unrdf/packages/kgc-cli/src/lib/latex/compile.mjs` `generateCacheKey()` function.

## Schemas

Cache schemas defined in `/home/user/unrdf/packages/kgc-cli/src/lib/latex/schemas.mjs`:
- `CacheEntrySchema`
- `CacheManifestSchema`
