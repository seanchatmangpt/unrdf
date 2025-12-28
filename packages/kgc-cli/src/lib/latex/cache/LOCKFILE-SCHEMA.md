# LaTeX Lockfile Schema

## Overview

The LaTeX lockfile (`latex.lock.json`) provides deterministic, reproducible builds by recording exact versions and hashes of all resolved packages, fonts, and assets.

## Schema Version: 1.0.0

### Lockfile Structure

```json
{
  "version": "1.0.0",
  "engine": "xetex",
  "entries": {
    "hyperref.sty": {
      "name": "hyperref.sty",
      "hash": "a3c9f8e7d1b2c3a4e5f6g7h8i9j0k1l2m3n4o5p6q7r8s9t0u1v2w3x4y5z6a7b8",
      "sourceUrl": "https://mirrors.ctan.org/macros/latex/contrib/hyperref/hyperref.sty",
      "cachedPath": "packages/hyperref/hyperref.sty",
      "fetchedAt": 1703721600000,
      "size": 98765
    },
    "beamer.cls": {
      "name": "beamer.cls",
      "hash": "b4d0f9e8d2c3b4a5f6e7g8h9i0j1k2l3m4n5o6p7q8r9s0t1u2v3w4x5y6z7a8b9",
      "sourceUrl": "https://mirrors.ctan.org/macros/latex/contrib/beamer/base/beamer.cls",
      "cachedPath": "packages/beamer/beamer.cls",
      "fetchedAt": 1703721650000,
      "size": 45678
    }
  },
  "createdAt": 1703721600000,
  "updatedAt": 1703721650000
}
```

## Field Descriptions

### Top Level

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `version` | `string` | Yes | Lockfile schema version (currently `"1.0.0"`) |
| `engine` | `string` | Yes | LaTeX engine used (`"xetex"`, `"pdftex"`, or `"luatex"`) |
| `entries` | `object` | Yes | Map of entry name to lock entry |
| `createdAt` | `number` | Yes | Unix timestamp (ms) when lockfile was first created |
| `updatedAt` | `number` | Yes | Unix timestamp (ms) when lockfile was last modified |

### Lock Entry

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | `string` | Yes | Package or file name (e.g., `"hyperref.sty"`) |
| `hash` | `string` | Yes | SHA-256 hash of file content (64 hex chars) |
| `sourceUrl` | `string` | No | Original source URL (CTAN, GitHub, etc.) |
| `cachedPath` | `string` | Yes | Relative path in cache directory |
| `fetchedAt` | `number` | Yes | Unix timestamp (ms) when file was fetched |
| `size` | `number` | No | File size in bytes |

## Hash Computation

Hashes are computed using SHA-256 over the **exact file content**:

```javascript
import { createHash } from 'node:crypto';

function hashContent(content) {
  const hash = createHash('sha256');
  hash.update(content);
  return hash.digest('hex');
}
```

## Cache Path Convention

Cache paths follow this structure:

```
.latex-cache/
  packages/
    <package-name>/
      <file-name>
  fonts/
    <font-family>/
      <font-file>
```

Examples:
- `packages/hyperref/hyperref.sty`
- `packages/beamer/beamer.cls`
- `fonts/texgyre/texgyrepagella-regular.otf`

## Engine Compatibility

Lockfiles are **engine-specific**. Changing the engine requires regenerating the lockfile, as different engines may require different package versions or dependencies.

| Engine | Description | Use Case |
|--------|-------------|----------|
| `xetex` | Modern Unicode support | Recommended for most documents |
| `pdftex` | Classic LaTeX | Legacy documents |
| `luatex` | Lua scripting support | Advanced typesetting |

## Validation Rules

1. **Hash must be valid SHA-256**: 64 hexadecimal characters
2. **Timestamps must be positive integers**: Unix milliseconds
3. **Engine must be one of**: `xetex`, `pdftex`, `luatex`
4. **Cached path must be relative**: No leading `/`
5. **Source URL must be valid** (if present): Valid HTTP(S) URL

## Deterministic Output

The lockfile is formatted with:
- **Sorted object keys** (alphabetically)
- **2-space indentation**
- **Trailing newline**

This ensures stable git diffs when entries are added or updated.

## Usage Examples

### Create New Lockfile

```javascript
import { createLockfile } from './lockfile.mjs';

const lockfile = createLockfile('xetex');
// { version: '1.0.0', engine: 'xetex', entries: {}, ... }
```

### Add Entry

```javascript
import { addEntry, createLockEntry } from './lockfile.mjs';

const entry = createLockEntry({
  name: 'hyperref.sty',
  content: packageContent,  // Uint8Array
  cachedPath: 'packages/hyperref/hyperref.sty',
  sourceUrl: 'https://mirrors.ctan.org/...'
});

addEntry(lockfile, entry);
```

### Verify Entry

```javascript
import { verifyEntry } from './lockfile.mjs';

const isValid = verifyEntry(lockfile, 'hyperref.sty', actualContent);
// true if hash matches
```

### Verify All Entries

```javascript
import { verifyAllEntries } from './lockfile.mjs';

const result = await verifyAllEntries(lockfile, '.latex-cache');
// { valid: ['hyperref.sty'], invalid: [], missing: [] }
```

### Save Lockfile

```javascript
import { saveLockfile } from './lockfile.mjs';

await saveLockfile('/path/to/latex.lock.json', lockfile);
// Atomically writes with sorted keys
```

## Migration from v0.x

If migrating from an earlier lockfile format:

1. Old lockfiles will fail validation (return `null` on load)
2. Create new lockfile with `createLockfile()`
3. Re-resolve all packages to populate entries
4. Save with `saveLockfile()`

## Bundle Export

Lockfiles can be exported to self-contained bundles:

```javascript
import { exportBundle } from './bundle.mjs';

const manifest = await exportBundle(
  lockfile,
  '.latex-cache',
  './bundle-output'
);

// Creates:
//   bundle-output/
//     bundle.manifest.json
//     packages/...
//     fonts/...
```

## Security Considerations

1. **Hash verification**: Always verify hashes before using cached files
2. **Source URLs**: Treat as untrusted (validate HTTPS, check certificates)
3. **Cache integrity**: Regularly verify cache with `verifyAllEntries()`
4. **Lockfile signing**: Consider signing lockfiles in production (external tool)

## Error Handling

### Invalid Lockfile
- **Load returns `null`**: Treat as missing, create new lockfile
- **Never throws on load**: Fail-safe design

### Hash Mismatch
- **Cache invalidated**: Re-fetch package
- **Logged to console**: Warning message with expected/actual hashes

### Missing Files
- **Verify reports as missing**: Re-populate cache from bundle or network

## Performance

- **Load/save**: ~1-5ms for 100 entries
- **Verify entry**: ~5-10ms per file (disk I/O + SHA-256)
- **Verify all**: Parallel verification (~50ms for 100 files)

## References

- [Zod Schema Validation](https://github.com/colinhacks/zod)
- [Node.js crypto module](https://nodejs.org/api/crypto.html)
- [CTAN Package Repository](https://ctan.org/)
