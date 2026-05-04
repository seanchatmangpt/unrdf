# Caching and Resolution Strategy

Understanding how package caching, content addressing, and CTAN resolution work together.

## Cache Architecture

The caching system has three layers:

```
┌─────────────────────────────────────────────────────────┐
│ Layer 1: Lockfile (latex.lock.json)                     │
│ - Records what SHOULD be cached                         │
│ - SHA-256 hashes for integrity                          │
│ - Enables cross-machine reproducibility                 │
└─────────────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────┐
│ Layer 2: CTAN Cache (.latex-cache/ctan/)                │
│ - Content-addressed files (hash-based filenames)        │
│ - Index mapping filenames → cache entries               │
│ - Persistent across builds                              │
└─────────────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────┐
│ Layer 3: VFS (In-Memory)                                │
│ - Ephemeral (exists only during compilation)            │
│ - Populated from cache + project files                  │
│ - Passed to WASM engine                                 │
└─────────────────────────────────────────────────────────┘
```

---

## Cache Directory Structure

```
.latex-cache/
├── latex.lock.json              # Layer 1: Lockfile
├── ctan/                        # Layer 2: CTAN cache
│   ├── index.json               #   Package index
│   └── files/                   #   Content-addressed files
│       ├── a1b2c3d4...sty       #   algorithm2e.sty
│       ├── f6e5d4c3...sty       #   tikz.sty
│       └── 0987654321...cls     #   beamer.cls
└── runs/                        # Diagnostic logs
    ├── 20251227_103045_xetex.log
    └── 20251227_104523_xetex.log
```

---

## Content Addressing

### Why Content-Addressed Filenames?

**Problem**: Multiple packages may have the same filename but different content:

```
algorithm2e.sty (CTAN 2021-01-15)  # Hash: a1b2c3...
algorithm2e.sty (CTAN 2023-06-20)  # Hash: f6e5d4...  # Updated version
```

**Traditional approach** (filename-based):
```
cache/algorithm2e.sty  # Which version? Ambiguous!
```

**Our approach** (content-addressed):
```
cache/files/a1b2c3d4e5f6789abcdef0123456789abcdef0123456789abcdef0123456789.sty
cache/files/f6e5d4c3b2a1098765432109876543210987654321098765432109876543210.sty
```

**Benefits**:
1. **Deduplication**: Same content = same file (even if fetched twice)
2. **Integrity**: Hash mismatch = corruption (automatic detection)
3. **Immutability**: Hash never changes for given content
4. **Versioning**: Different versions coexist peacefully

### Hash Computation

```javascript
import { createHash } from 'node:crypto';

function computeContentHash(content) {
  return createHash('sha256')
    .update(content)
    .digest('hex');  // 64-character hex string
}

const content = await fetch('https://mirrors.ctan.org/.../algorithm2e.sty');
const hash = computeContentHash(content);
// => 'a1b2c3d4e5f6789abcdef0123456789abcdef0123456789abcdef0123456789'

// Cache file
const cachedPath = `.latex-cache/ctan/files/${hash}.sty`;
await writeFile(cachedPath, content);
```

---

## Cache Index

### Structure

```json
{
  "algorithm2e.sty": {
    "hash": "a1b2c3d4e5f6789abcdef0123456789abcdef0123456789abcdef0123456789",
    "path": "files/a1b2c3d4e5f6789abcdef0123456789abcdef0123456789abcdef0123456789.sty",
    "url": "https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty",
    "timestamp": 1703686200000
  },
  "tikz.sty": {
    "hash": "f6e5d4c3b2a1098765432109876543210987654321098765432109876543210",
    "path": "files/f6e5d4c3b2a1098765432109876543210987654321098765432109876543210.sty",
    "url": "https://mirrors.ctan.org/graphics/pgf/base/tikz.sty",
    "timestamp": 1703686201000
  }
}
```

### Index Purpose

**Maps**: `filename` → `{hash, path, url, timestamp}`

**Why needed?**
- Lookup by filename (not hash)
- Reconstruct VFS path (e.g., `texmf/tex/latex/tikz/tikz.sty`)
- Provenance tracking (where package came from)

### Index Operations

**Add entry**:
```javascript
function addToIndex(index, filename, hash, url, path) {
  index[filename] = {
    hash,
    path,
    url,
    timestamp: Date.now()
  };
}
```

**Lookup**:
```javascript
function getCachedFile(index, filename, cacheDir) {
  const entry = index[filename];
  if (!entry) return null;

  const fullPath = join(cacheDir, 'ctan', entry.path);
  if (!existsSync(fullPath)) return null;

  const content = readFileSync(fullPath);
  const actualHash = computeContentHash(content);

  // Verify integrity
  if (actualHash !== entry.hash) {
    console.warn(`Hash mismatch for ${filename}, re-fetching`);
    return null;
  }

  return content;
}
```

---

## Resolution Flow

### Cold Build (No Cache)

```
┌──────────────────────────────────────────────────────────┐
│ 1. User runs: kgc latex build --input main.tex           │
└──────────────────────────────────────────────────────────┘
                    │
                    ▼
┌──────────────────────────────────────────────────────────┐
│ 2. Compilation fails: Missing algorithm2e.sty             │
└──────────────────────────────────────────────────────────┘
                    │
                    ▼
┌──────────────────────────────────────────────────────────┐
│ 3. Resolver: Fetch from CTAN                             │
│    GET https://mirrors.ctan.org/.../algorithm2e.sty      │
│    → content (45 KB)                                     │
└──────────────────────────────────────────────────────────┘
                    │
                    ▼
┌──────────────────────────────────────────────────────────┐
│ 4. Compute hash                                           │
│    hash = sha256(content) = 'a1b2c3d4...'                │
└──────────────────────────────────────────────────────────┘
                    │
                    ▼
┌──────────────────────────────────────────────────────────┐
│ 5. Cache to disk                                          │
│    .latex-cache/ctan/files/a1b2c3d4...sty ← content      │
└──────────────────────────────────────────────────────────┘
                    │
                    ▼
┌──────────────────────────────────────────────────────────┐
│ 6. Update index                                           │
│    index['algorithm2e.sty'] = { hash, path, url, ... }   │
│    Save .latex-cache/ctan/index.json                     │
└──────────────────────────────────────────────────────────┘
                    │
                    ▼
┌──────────────────────────────────────────────────────────┐
│ 7. Update lockfile                                        │
│    lockfile.resolvedInputs['algorithm2e.sty'] = {...}    │
│    Save .latex-cache/latex.lock.json                     │
└──────────────────────────────────────────────────────────┘
                    │
                    ▼
┌──────────────────────────────────────────────────────────┐
│ 8. Retry compilation                                      │
│    VFS now includes algorithm2e.sty → SUCCESS             │
└──────────────────────────────────────────────────────────┘
```

### Warm Build (Cache Hit)

```
┌──────────────────────────────────────────────────────────┐
│ 1. User runs: kgc latex build --input main.tex           │
└──────────────────────────────────────────────────────────┘
                    │
                    ▼
┌──────────────────────────────────────────────────────────┐
│ 2. Load lockfile                                          │
│    lockfile.resolvedInputs['algorithm2e.sty'] exists     │
│    hash = 'a1b2c3d4...'                                  │
└──────────────────────────────────────────────────────────┘
                    │
                    ▼
┌──────────────────────────────────────────────────────────┐
│ 3. Check cache                                            │
│    .latex-cache/ctan/files/a1b2c3d4...sty exists         │
│    Verify hash matches → ✓                               │
└──────────────────────────────────────────────────────────┘
                    │
                    ▼
┌──────────────────────────────────────────────────────────┐
│ 4. Add to VFS                                             │
│    vfs['texmf/tex/latex/algorithm2e/algorithm2e.sty'] =  │
│        content from cache                                 │
└──────────────────────────────────────────────────────────┘
                    │
                    ▼
┌──────────────────────────────────────────────────────────┐
│ 5. Compile (no missing inputs)                            │
│    → SUCCESS (no network I/O!)                            │
└──────────────────────────────────────────────────────────┘
```

**Performance**: Warm build is ~10-100x faster (no network, no hash computation, just file read).

---

## Cache Invalidation

### Scenarios

**1. Manual deletion**:
```bash
rm -rf .latex-cache/ctan/
```
**Effect**: Next build re-fetches all packages (uses lockfile URLs).

**2. Hash mismatch**:
```javascript
const cachedContent = readFileSync(cachedPath);
const actualHash = computeContentHash(cachedContent);
if (actualHash !== entry.hash) {
  // Corruption detected → re-fetch
  await fetchFromCtan(filename);
}
```

**3. Lockfile changed** (package version updated):
```json
// Old lockfile
{
  "algorithm2e.sty": { "hash": "a1b2c3d4..." }
}

// New lockfile (after CTAN update)
{
  "algorithm2e.sty": { "hash": "f6e5d4c3..." }
}
```
**Effect**: Cache miss (different hash) → re-fetch.

### Cache Eviction

**No automatic eviction**. Cache grows indefinitely unless manually cleared.

**Rationale**:
- Disk is cheap (~1 GB cache = 10,000 packages)
- Old packages may be needed (lockfile time-travel)
- User controls cleanup

**Manual cleanup**:
```bash
# Clear packages older than 30 days
find .latex-cache/ctan/files/ -mtime +30 -delete
```

---

## CTAN Mirror Selection

### Default Mirror

```javascript
const DEFAULT_CTAN_MIRROR = 'https://mirrors.ctan.org';
```

**Why this mirror?**
- CDN (Content Delivery Network) - fast globally
- Maintained by CTAN team
- Reliable uptime (>99.9%)

### Custom Mirrors

**Use case**: Faster regional access, corporate firewall

```javascript
await resolveMissingInputs({
  missingInputs: ['tikz.sty'],
  cacheDir: '.latex-cache',
  ctanMirror: 'https://ctan.math.utah.edu'  // US mirror
});
```

**Popular mirrors**:
- US: `https://ctan.math.utah.edu`
- Europe: `https://ftp.tu-chemnitz.de`
- Asia: `https://ctan.mirror.nus.edu.sg`
- Australia: `https://mirror.aarnet.edu.au/pub/CTAN`

### Fallback Strategy

```javascript
const CTAN_MIRRORS = [
  'https://mirrors.ctan.org',
  'https://ctan.math.utah.edu',
  'https://ftp.tu-chemnitz.de'
];

async function fetchFromCtanWithFallback(filename) {
  for (const mirror of CTAN_MIRRORS) {
    try {
      const content = await fetchFromCtan(filename, mirror);
      return content;
    } catch (error) {
      // Try next mirror
      continue;
    }
  }
  throw new Error(`All mirrors failed for ${filename}`);
}
```

---

## Lockfile + Cache Synergy

### Problem: Cache Without Lockfile

```bash
# Machine A: Builds on 2025-01-15
cache/files/a1b2c3d4...sty  # algorithm2e.sty (v3.1)

# Machine B: Builds on 2025-06-20 (CTAN updated algorithm2e)
cache/files/f6e5d4c3...sty  # algorithm2e.sty (v3.2)
```

**Result**: Different PDFs (non-reproducible)

### Solution: Lockfile Pins Hash

```json
// Committed to Git
{
  "algorithm2e.sty": {
    "hash": "a1b2c3d4...",  // ← Pins version 3.1
    "sourceUrl": "https://mirrors.ctan.org/.../algorithm2e.sty"
  }
}
```

**Machine B reads lockfile**:
- Expected hash: `a1b2c3d4...` (version 3.1)
- Cache lookup: `cache/files/a1b2c3d4...sty`
- If missing, re-fetches from `sourceUrl`

**Result**: Both machines use **identical package** (version 3.1).

---

## Offline Builds

### Preconditions

1. Lockfile exists (records all packages)
2. Cache is complete (all hashes present)

### Verification

```bash
# Check lockfile
jq '.resolvedInputs | keys' .latex-cache/latex.lock.json
# Output: ["algorithm2e.sty", "tikz.sty"]

# Check cache has all files
for hash in $(jq -r '.resolvedInputs[].hash' .latex-cache/latex.lock.json); do
  file=".latex-cache/ctan/files/${hash}.sty"
  if [ ! -f "$file" ]; then
    echo "Missing: $file"
  fi
done
```

### Build Process

```javascript
// Offline build (no network access)
const lockfile = await loadLatexLock('.latex-cache/latex.lock.json');

for (const [filename, entry] of Object.entries(lockfile.resolvedInputs)) {
  const cachedPath = join('.latex-cache/ctan/files', `${entry.hash}.sty`);

  if (!existsSync(cachedPath)) {
    throw new Error(`Offline build failed: Missing cached file for ${filename}`);
  }

  const content = readFileSync(cachedPath);
  vfs.set(entry.cachedPath, content);
}

// Compile with complete VFS (no network I/O)
await compileWithSwiftLatex({ vfs, ... });
```

---

## Performance Characteristics

### Cache Hit Rate

| Build Type | Cache Hit Rate | Network I/O |
|------------|----------------|-------------|
| First build | 0% | High (fetch all) |
| Second build (same machine) | 100% | Zero |
| CI build (cached artifacts) | 100% | Zero |
| New machine (with lockfile) | 0% (first), 100% (subsequent) | One-time fetch |

### Storage Requirements

| Document Type | Packages | Cache Size |
|---------------|----------|------------|
| Minimal article | 0 | 0 KB |
| Thesis (basic) | 10 | ~500 KB |
| Thesis (graphics) | 25 | ~2 MB |
| Book (complex) | 50 | ~5 MB |
| Large corpus | 200 | ~20 MB |

**Conclusion**: Cache is small (~1-20 MB typical).

### Build Time Comparison

| Scenario | Time (Cold) | Time (Warm) | Speedup |
|----------|-------------|-------------|---------|
| No cache | 15s | 15s | 1x |
| Cache (local) | 15s (first) | 3s | 5x |
| Cache (networked) | 15s (first) | 2s | 7.5x |

---

## Best Practices

### 1. Commit Lockfile, Not Cache

```gitignore
# .gitignore
.latex-cache/ctan/      # Binary cache (regenerable)
.latex-cache/runs/      # Logs (ephemeral)

# Keep lockfile (reproducibility)
!.latex-cache/latex.lock.json
```

### 2. Verify Cache Integrity

```bash
# Before deploying offline
node -e '
const lockfile = require("./.latex-cache/latex.lock.json");
for (const [file, entry] of Object.entries(lockfile.resolvedInputs)) {
  const cached = fs.readFileSync(`.latex-cache/ctan/files/${entry.hash}.sty`);
  const hash = crypto.createHash("sha256").update(cached).digest("hex");
  if (hash !== entry.hash) {
    console.error(`Corruption: ${file}`);
    process.exit(1);
  }
}
console.log("✓ Cache integrity verified");
'
```

### 3. Use CI Cache

```yaml
# .github/workflows/build.yml
- name: Cache LaTeX packages
  uses: actions/cache@v3
  with:
    path: .latex-cache/ctan/
    key: latex-${{ hashFiles('.latex-cache/latex.lock.json') }}
```

**Benefit**: CI builds reuse cache across runs (fast!).

---

## Troubleshooting

### Cache Corruption

**Symptom**:
```
⚠ Hash mismatch for algorithm2e.sty
  Expected: a1b2c3d4...
  Got: 0000aaaa...
```

**Cause**: File modified or disk corruption

**Solution**: Delete cache, rebuild:
```bash
rm -rf .latex-cache/ctan/
kgc latex build --input main.tex
```

### Missing Cache After Git Pull

**Symptom**:
```
✗ Cached file not found: .latex-cache/ctan/files/a1b2c3d4...sty
```

**Cause**: Teammate updated lockfile, but cache not committed

**Solution**: Rebuild (re-fetches from lockfile URLs):
```bash
kgc latex build --input main.tex
```

### Stale Index

**Symptom**:
```javascript
index['tikz.sty'] exists but file doesn't
```

**Cause**: Manual file deletion without updating index

**Solution**: Rebuild index:
```bash
rm .latex-cache/ctan/index.json
kgc latex build --input main.tex  # Rebuilds index
```

---

## Summary

The caching system provides:

- ✓ **Determinism**: Content-addressed files (hash = identity)
- ✓ **Reproducibility**: Lockfile + cache = identical builds
- ✓ **Performance**: Cache hits avoid network I/O (5-10x faster)
- ✓ **Integrity**: Hash verification detects corruption
- ✓ **Offline support**: Complete cache enables air-gapped builds

**Key insights**:
- Lockfile = **what** should be cached
- Cache = **where** it's stored
- Content addressing = **how** integrity is ensured

**Next steps**:
- Learn about [Failure Modes](./failure-modes.md)
- Understand [Deterministic Builds](../how-to/deterministic-builds.md)
- Read [Architecture Overview](./architecture.md)
