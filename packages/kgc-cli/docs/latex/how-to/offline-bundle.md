# How-To: Create an Offline Package Bundle

**Goal**: Set up the LaTeX pipeline to work without internet access by pre-caching all required packages.

**Use cases**:
- Air-gapped environments (secure networks)
- CI/CD pipelines (deterministic, no external deps)
- Traveling / unreliable internet
- Corporate firewalls blocking CTAN

**Time**: ~10 minutes

## Prerequisites

- Internet access (initially, to download packages)
- Completed [First PDF Tutorial](../tutorials/first-pdf.md)
- Your LaTeX project files

## Strategy Overview

The offline bundle strategy:
1. Compile your document once **with internet access**
2. All missing packages are fetched and cached
3. The cache is recorded in `latex.lock.json`
4. Copy the cache directory to offline machine
5. Subsequent builds use only cached packages

## Step 1: Compile with Full Dependency Resolution

On a machine **with internet access**, compile your document:

```bash
kgc latex build --input thesis/main.tex --output dist/thesis.pdf
```

Watch the output carefully. You'll see packages being fetched:

```
✓ Collecting project files...
✓ Running compilation (pass 1/2)...
⚠ Missing inputs: algorithm2e.sty, tikz.sty, pgfcore.sty, ...
✓ Fetching algorithm2e.sty from CTAN...
✓ Fetching tikz.sty from CTAN...
✓ Fetching pgfcore.sty from CTAN...
✓ Cached 12 packages (245 KB)
✓ Running compilation (pass 2/2)...
✓ PDF generated successfully
```

**Key point**: The first compilation populates your cache.

## Step 2: Verify the Cache

Check your cache directory:

```bash
ls -lh .latex-cache/
```

You should see:

```
.latex-cache/
├── ctan/
│   ├── index.json                # Package index
│   └── files/
│       ├── a1b2c3d4e5f6...sty    # Content-hash filenames
│       ├── f6e5d4c3b2a1...sty
│       └── ...
├── latex.lock.json               # Lockfile
└── runs/
    └── 20251227_143000_xetex.log # Last run log
```

Inspect the package index:

```bash
cat .latex-cache/ctan/index.json
```

Output:

```json
{
  "algorithm2e.sty": {
    "hash": "a1b2c3d4e5f6...",
    "path": "files/a1b2c3d4e5f6...sty",
    "timestamp": 1703686200000,
    "url": "https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty"
  },
  "tikz.sty": {
    "hash": "f6e5d4c3b2a1...",
    "path": "files/f6e5d4c3b2a1...sty",
    "timestamp": 1703686201000,
    "url": "https://mirrors.ctan.org/graphics/pgf/base/tikz.sty"
  }
}
```

Check the lockfile:

```bash
cat .latex-cache/latex.lock.json
```

## Step 3: Test Offline Compilation

Simulate offline mode by disconnecting from the internet (or blocking CTAN):

```bash
# Option 1: Disconnect network
# Option 2: Add CTAN to /etc/hosts (for testing)
echo "127.0.0.1 mirrors.ctan.org" | sudo tee -a /etc/hosts

# Try compiling again
kgc latex build --input thesis/main.tex --output dist/thesis-offline.pdf
```

**Expected output**:

```
✓ Collecting project files...
✓ Using cached packages (12 packages, 0 fetches)
✓ Running compilation (pass 1/2)...
✓ Running compilation (pass 2/2)...
✓ PDF generated successfully
```

Notice: **0 fetches** - all packages came from cache!

## Step 4: Package the Bundle

Create a portable bundle for transfer:

```bash
# Archive the cache
tar -czf latex-bundle.tar.gz .latex-cache/

# Check the size
ls -lh latex-bundle.tar.gz
```

Expected output:

```
-rw-r--r-- 1 user user 180K Dec 27 14:35 latex-bundle.tar.gz
```

## Step 5: Deploy to Offline Machine

Transfer the bundle to your offline machine:

```bash
# Copy via USB, SCP, or other secure transfer
scp latex-bundle.tar.gz offline-machine:/path/to/project/

# On the offline machine:
cd /path/to/project
tar -xzf latex-bundle.tar.gz

# Verify cache exists
ls .latex-cache/
```

## Step 6: Compile Offline

On the offline machine:

```bash
kgc latex build --input thesis/main.tex --output thesis.pdf
```

**Success criteria**:
- ✓ No network requests
- ✓ PDF generated
- ✓ Build time < 5 seconds

## Advanced: Pre-Cache Common Packages

If you want to pre-cache common packages **before** writing your document:

### Create a "Kitchen Sink" Document

Create `preload.tex`:

```latex
\documentclass{article}

% Commonly used packages
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{graphicx}
\usepackage{hyperref}
\usepackage{tikz}
\usepackage{algorithm2e}
\usepackage{listings}
\usepackage{booktabs}
\usepackage{multirow}
\usepackage{subcaption}
\usepackage{xcolor}
\usepackage{geometry}

\begin{document}
Preloading cache...
\end{document}
```

Compile it:

```bash
kgc latex build --input preload.tex --output /dev/null
```

All listed packages are now cached for future use.

## Validation Checklist

Before deploying offline:

- [ ] Compile your document **at least once** with internet
- [ ] Check `.latex-cache/ctan/index.json` has all packages
- [ ] Check `latex.lock.json` has all `resolvedInputs`
- [ ] Test compilation with network disabled
- [ ] Archive includes both `ctan/` and `latex.lock.json`
- [ ] Archive is compressed (`.tar.gz` or `.zip`)

## Troubleshooting

### Error: "Failed to fetch X.sty from CTAN"

**On offline machine**:

This means the package wasn't in your cache. Solutions:

1. **Re-compile on online machine** to fetch missing package
2. **Manually download** the `.sty` file and add it to your project (see [Custom Packages](./custom-packages.md))

### Error: "Cache hash mismatch"

```
⚠ Cache hash mismatch for algorithm2e.sty, re-fetching
```

This means:
- The cached file was corrupted during transfer
- Someone manually edited the cached file

**Solution**: Delete the cache and rebuild:

```bash
rm -rf .latex-cache/ctan/
kgc latex build --input thesis/main.tex
```

### Compilation works online but fails offline

**Diagnosis**: Check the lockfile:

```bash
jq '.resolvedInputs | keys' .latex-cache/latex.lock.json
```

If a package is missing, it wasn't fetched during the initial online build.

**Solution**:
1. Clear cache
2. Re-compile online with verbose mode (see logs)
3. Verify all packages are cached

## Cache Maintenance

### Check cache size

```bash
du -sh .latex-cache/ctan/
```

Typical sizes:
- Minimal document: ~50 KB
- Thesis with graphics: ~500 KB
- Large book with TikZ: ~2 MB

### Clear specific package

```bash
# Remove a specific package (will be re-fetched next build)
rm .latex-cache/ctan/files/<hash>.sty
```

Edit `index.json` to remove the entry:

```bash
jq 'del(.["algorithm2e.sty"])' .latex-cache/ctan/index.json > index.tmp.json
mv index.tmp.json .latex-cache/ctan/index.json
```

### Clear entire cache

```bash
rm -rf .latex-cache/ctan/
```

Next build will re-fetch all packages.

## CI/CD Integration

For reproducible CI builds:

```yaml
# .github/workflows/build.yml
name: Build LaTeX

on: [push]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      # Restore cache from previous builds
      - name: Cache LaTeX packages
        uses: actions/cache@v3
        with:
          path: .latex-cache/
          key: latex-${{ hashFiles('thesis/**/*.tex') }}

      # Build PDF (uses cache if available)
      - name: Compile LaTeX
        run: |
          pnpm install
          kgc latex build --input thesis/main.tex --output dist/thesis.pdf

      # Upload PDF artifact
      - name: Upload PDF
        uses: actions/upload-artifact@v3
        with:
          name: thesis-pdf
          path: dist/thesis.pdf
```

The cache is automatically restored, making builds deterministic and fast.

## Summary

You've created a fully offline LaTeX compilation bundle:

- ✓ All packages pre-cached
- ✓ Lockfile ensures reproducibility
- ✓ No network access required
- ✓ Portable bundle (~100-500 KB)
- ✓ CI/CD ready

**Next steps**:
- Learn about [Deterministic Builds](./deterministic-builds.md)
- Understand [Caching Strategy](../explanation/caching.md)
- Set up [Custom Packages](./custom-packages.md)
