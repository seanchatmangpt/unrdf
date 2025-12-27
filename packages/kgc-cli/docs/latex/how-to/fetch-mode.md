# How-To: Use Fetch-on-Miss Mode

**Goal**: Configure the LaTeX pipeline to automatically fetch missing packages from CTAN during compilation.

**Use cases**:
- Rapid prototyping (try new packages without manual installation)
- Exploring LaTeX packages (automatic discovery)
- Minimal setup (no pre-configuration needed)
- Development environments (internet access available)

**Time**: ~5 minutes

## What is Fetch-on-Miss?

**Fetch-on-miss** is the default mode where:
1. Compilation starts with your local files + cache
2. If a package is missing, the pipeline detects it from the LaTeX log
3. The package is fetched from CTAN automatically
4. Compilation retries with the new package
5. The package is cached for future builds

This mode provides the **best developer experience** at the cost of network dependency.

## How It Works

```
┌─────────────────────────────────────────────────────────┐
│  Compilation Cycle (Max 2 cycles)                       │
└─────────────────────────────────────────────────────────┘
                      │
                      ▼
            ┌──────────────────┐
            │ Try Compilation  │
            └──────────────────┘
                      │
          ┌───────────┴───────────┐
          │                       │
          ▼                       ▼
    ┌─────────┐           ┌──────────────┐
    │ Success │           │ Parse Errors │
    │ → PDF   │           │ for Missing  │
    └─────────┘           └──────────────┘
                                  │
                          ┌───────┴────────┐
                          │                │
                          ▼                ▼
                   ┌──────────┐      ┌─────────┐
                   │ Fetch    │      │ Give Up │
                   │ from     │      │ → Error │
                   │ CTAN     │      └─────────┘
                   └──────────┘
                          │
                          ▼
                   ┌──────────────┐
                   │ Retry Compile│
                   └──────────────┘
```

## Enabling Fetch-on-Miss

**Good news**: It's **enabled by default**! No configuration needed.

```bash
kgc latex build --input main.tex --output output.pdf
```

The pipeline automatically:
- Attempts compilation
- Detects missing packages
- Fetches them from CTAN
- Retries (up to 2 cycles)

## Example: Adding a New Package

Let's see fetch-on-miss in action.

### Step 1: Create a Document with an Unfamiliar Package

```latex
\documentclass{article}
\usepackage{lipsum}  % Dummy text generator - probably not cached

\begin{document}

\section{Introduction}

\lipsum[1-3]  % Generate 3 paragraphs of dummy text

\end{document}
```

Save as `lipsum-test.tex`.

### Step 2: Compile

```bash
kgc latex build --input lipsum-test.tex --output lipsum-test.pdf
```

**Output**:

```
✓ Collecting project files...
✓ Running compilation (pass 1/2)...
⚠ Missing input detected: lipsum.sty
✓ Resolving missing inputs...
  → Trying: https://mirrors.ctan.org/macros/latex/contrib/lipsum/lipsum.sty
  ✓ Downloaded lipsum.sty (12.3 KB)
  ✓ Cached as: .latex-cache/ctan/files/a1b2c3d4...sty
✓ Updating lockfile...
✓ Running compilation (pass 2/2)...
✓ PDF generated: lipsum-test.pdf (15.4 KB)
```

### Step 3: Verify Caching

Compile again:

```bash
kgc latex build --input lipsum-test.tex --output lipsum-test2.pdf
```

**Output**:

```
✓ Collecting project files...
✓ Using cached packages (1 package, 0 fetches)
✓ Running compilation (pass 1/2)...
✓ Running compilation (pass 2/2)...
✓ PDF generated: lipsum-test2.pdf (15.4 KB)
```

Notice: **0 fetches** - the package was served from cache.

## Lockfile Integration

Fetch-on-miss automatically updates your lockfile.

Before fetching:

```json
{
  "version": "1.0.0",
  "engine": "xetex",
  "resolvedInputs": {},
  "createdAt": "2025-12-27T10:00:00.000Z",
  "updatedAt": "2025-12-27T10:00:00.000Z"
}
```

After fetching `lipsum.sty`:

```json
{
  "version": "1.0.0",
  "engine": "xetex",
  "resolvedInputs": {
    "lipsum.sty": {
      "hash": "a1b2c3d4e5f6789...",
      "cachedPath": "texmf/tex/latex/lipsum/lipsum.sty",
      "resolvedAt": "2025-12-27T10:05:30.123Z",
      "sourceUrl": "https://mirrors.ctan.org/macros/latex/contrib/lipsum/lipsum.sty"
    }
  },
  "createdAt": "2025-12-27T10:00:00.000Z",
  "updatedAt": "2025-12-27T10:05:30.456Z"
}
```

The lockfile now contains:
- **hash**: Content hash for integrity verification
- **cachedPath**: VFS path where package is stored
- **resolvedAt**: Timestamp of resolution
- **sourceUrl**: Original CTAN URL

## Configuration Options

While fetch-on-miss is automatic, you can configure it:

### Custom CTAN Mirror

Use a specific mirror (e.g., for faster regional access):

```javascript
import { resolveMissingInputs } from '@unrdf/kgc-cli/src/lib/latex/ctan-resolver.mjs';

const resolved = await resolveMissingInputs({
  missingInputs: ['algorithm2e.sty'],
  cacheDir: '.latex-cache',
  ctanMirror: 'https://ctan.math.utah.edu'  // US mirror
});
```

Available mirrors:
- `https://mirrors.ctan.org` (default, CDN)
- `https://ctan.math.utah.edu` (US)
- `https://ftp.tu-chemnitz.de` (Germany)
- `https://ctan.mirrors.hoobly.com` (UK)

### Disable Automatic Fetching

To **prevent** automatic fetching (fail fast if packages missing):

```javascript
// Not yet implemented in CLI - programmatic use only
import { compileLatexToPdf } from '@unrdf/kgc-cli/src/lib/latex/compile.mjs';

// Manually control resolution
const vfs = await initializeVFS(projectDir);

// Compile WITHOUT auto-resolution
const result = await compileWithSwiftLatex({
  engine: 'xetex',
  vfs,
  entry: 'main.tex',
  cacheDir: '.latex-cache'
});

if (!result.ok && result.missingInputs) {
  console.error('Missing packages:', result.missingInputs);
  process.exit(1);
}
```

**Use case**: Enforce that all packages are pre-cached (e.g., in production CI).

## Network Requirements

### Bandwidth

Typical package sizes:
- Small packages (`.sty` files): 5-50 KB
- Medium packages (with docs): 50-500 KB
- Large packages (TikZ, pgf): 1-5 MB

Average thesis with 10-20 packages: **~500 KB** total download.

### Latency

Fetch times (observed):
- Fast (< 1s): US/EU with good connection
- Moderate (1-3s): Global CDN, moderate connection
- Slow (> 3s): Regional mirrors, poor connection

**Tip**: Use a nearby CTAN mirror to reduce latency.

### Offline Fallback

If network is unavailable, the pipeline:
1. Attempts compilation with cached packages only
2. If missing packages needed, fails with clear error:

```
✗ Missing input: algorithm2e.sty
✗ Network unreachable - cannot fetch from CTAN
✗ Possible solutions:
  1. Connect to the internet
  2. Use an offline bundle (see docs/latex/how-to/offline-bundle.md)
  3. Manually add the package to your project
```

## Error Handling

### Package Not Found on CTAN

```
✗ Failed to fetch 'mypackage.sty' from CTAN
  Tried URLs:
    - https://mirrors.ctan.org/macros/latex/contrib/mypackage/mypackage.sty
    - https://mirrors.ctan.org/macros/latex/required/mypackage/mypackage.sty
    - https://mirrors.ctan.org/macros/latex/base/mypackage.sty

  Possible reasons:
    - Package name misspelled (check \usepackage{...})
    - Package not available on CTAN (use local file instead)
    - Package renamed or obsolete

  Solutions:
    1. Check package name in LaTeX log
    2. Search CTAN: https://ctan.org/search?q=mypackage
    3. Use custom package (see docs/latex/how-to/custom-packages.md)
```

### Network Timeout

```
✗ Failed to fetch 'tikz.sty' from CTAN
  Error: Network timeout after 30s

  Solutions:
    1. Check internet connection
    2. Try a different CTAN mirror
    3. Increase timeout (programmatic API only)
```

### Corrupted Download

If a package downloads but is corrupted:

```
✗ Package hash mismatch: tikz.sty
  Expected: a1b2c3d4e5f6...
  Got: f6e5d4c3b2a1...

  The downloaded file may be corrupted. Retrying...
```

The pipeline automatically retries once.

## Best Practices

### Development Workflow

```bash
# 1. Write LaTeX with any packages
vim thesis/chapter1.tex

# 2. Compile (packages auto-fetch)
kgc latex build --input thesis/main.tex

# 3. Commit lockfile for reproducibility
git add .latex-cache/latex.lock.json
git commit -m "Add lockfile for chapter 1"
```

### Committing the Lockfile

**DO commit**: `latex.lock.json`
- Ensures reproducible builds across team
- Documents exact package versions (via hash)
- Enables offline builds after initial fetch

**DON'T commit**: `.latex-cache/ctan/`
- Binary files (large, not suitable for Git)
- Can be regenerated from lockfile

Example `.gitignore`:

```gitignore
# LaTeX cache (regenerable from lockfile)
.latex-cache/ctan/
.latex-cache/runs/

# Keep lockfile (reproducibility)
!.latex-cache/latex.lock.json
```

### CI/CD with Fetch-on-Miss

GitHub Actions example:

```yaml
name: Build Thesis

on: [push]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'

      - name: Install dependencies
        run: pnpm install

      - name: Cache LaTeX packages
        uses: actions/cache@v3
        with:
          path: .latex-cache/ctan/
          # Cache key includes lockfile hash
          key: latex-ctan-${{ hashFiles('.latex-cache/latex.lock.json') }}

      - name: Build PDF
        run: kgc latex build --input thesis/main.tex --output thesis.pdf
        # First run: fetches packages, populates cache
        # Subsequent runs: uses cached packages (fast!)

      - name: Upload PDF
        uses: actions/upload-artifact@v3
        with:
          name: thesis
          path: thesis.pdf
```

## Performance Characteristics

| Scenario | First Build | Subsequent Builds |
|----------|-------------|-------------------|
| No packages | ~2s | ~2s |
| 5 small packages | ~5s (fetching) | ~2s (cached) |
| 20 packages (thesis) | ~15s (fetching) | ~3s (cached) |
| 50+ packages (book) | ~30s (fetching) | ~5s (cached) |

**Takeaway**: First build is slower (network I/O), subsequent builds are fast (cache hits).

## Comparison with Other Modes

| Mode | Network | Speed | Use Case |
|------|---------|-------|----------|
| **Fetch-on-miss** (default) | Required (first build) | Moderate→Fast | Development |
| [Offline bundle](./offline-bundle.md) | Not required | Fast | Production, CI, air-gapped |
| [Custom packages](./custom-packages.md) | Not required | Fast | Private/proprietary packages |

## Summary

Fetch-on-miss mode provides:

- ✓ Zero configuration (works out of the box)
- ✓ Automatic package discovery
- ✓ Lockfile for reproducibility
- ✓ Cache for performance
- ✓ Clear error messages

**Tradeoffs**:
- ✗ Requires internet (first build)
- ✗ Slower initial compilation
- ✗ Network errors can block builds

**Next steps**:
- Set up [Offline Bundle](./offline-bundle.md) for production
- Learn about [Deterministic Builds](./deterministic-builds.md)
- Understand [Caching Strategy](../explanation/caching.md)
