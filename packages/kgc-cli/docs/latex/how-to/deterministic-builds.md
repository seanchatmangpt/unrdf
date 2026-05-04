# How-To: Achieve Deterministic Builds

**Goal**: Configure the LaTeX pipeline to produce **byte-identical PDFs** across different machines and at different times.

**Use cases**:
- Reproducible research (scientific papers)
- Compliance / regulatory requirements
- CI/CD verification (detect unintended changes)
- Team collaboration (consistent output)

**Time**: ~10 minutes

## What is Determinism?

**Deterministic build**: Given the same input files, the build system produces **bit-for-bit identical output** every time.

**Why it matters**:
- **Trust**: Verify that PDF matches source code
- **Collaboration**: Team members get identical PDFs from same LaTeX source
- **Debugging**: Changes in PDF are only from intentional source edits
- **Compliance**: Prove documents weren't tampered with

## Challenge: LaTeX is Non-Deterministic by Default

LaTeX includes several sources of non-determinism:

| Source | Why Non-Deterministic | Example |
|--------|----------------------|---------|
| Timestamps | `\today`, creation date in PDF metadata | "Compiled on Dec 27, 2025" |
| Package versions | Fetched from CTAN at different times | TikZ v3.1.8 vs v3.1.9 |
| File ordering | Depends on filesystem iteration order | Multi-file `\input` order |
| Random seeds | Some packages use randomness | Random diagram layouts |

## Our Strategy for Determinism

The pipeline provides determinism through:

1. **Lockfile** (`latex.lock.json`) - Pins package versions by content hash
2. **Sorted VFS** - Files always loaded in alphabetical order
3. **Content addressing** - Packages cached by SHA-256 hash
4. **Stable metadata** - Control PDF metadata (timestamps, etc.)

Let's implement each.

## Step 1: Generate a Lockfile

First build creates the lockfile:

```bash
kgc latex build --input thesis/main.tex --output dist/thesis.pdf
```

This creates `.latex-cache/latex.lock.json`:

```json
{
  "version": "1.0.0",
  "engine": "xetex",
  "resolvedInputs": {
    "algorithm2e.sty": {
      "hash": "a1b2c3d4e5f6789abcdef...",
      "cachedPath": "texmf/tex/latex/algorithm2e/algorithm2e.sty",
      "resolvedAt": "2025-12-27T10:00:00.000Z",
      "sourceUrl": "https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty"
    }
  },
  "createdAt": "2025-12-27T10:00:00.000Z",
  "updatedAt": "2025-12-27T10:00:00.000Z"
}
```

**Key insight**: The `hash` field ensures we always use the **exact same version** of each package.

## Step 2: Commit the Lockfile

Add the lockfile to version control:

```bash
git add .latex-cache/latex.lock.json
git commit -m "Add LaTeX lockfile for deterministic builds"
```

**Don't commit** the cache directory (binary files):

```bash
# .gitignore
.latex-cache/ctan/
.latex-cache/runs/
```

## Step 3: Verify Determinism

Build the PDF twice and compare:

```bash
# First build
kgc latex build --input thesis/main.tex --output build1.pdf

# Second build (immediately after)
kgc latex build --input thesis/main.tex --output build2.pdf

# Compare byte-for-byte
diff build1.pdf build2.pdf
```

**Expected result**:

```
# Files differ (because of PDF metadata timestamps)
```

Wait - they differ? That's because **PDF metadata includes timestamps by default**.

## Step 4: Remove Non-Deterministic Metadata

### Option A: Strip PDF Metadata (Recommended)

Use `qpdf` or similar tool to strip metadata:

```bash
# Install qpdf
brew install qpdf  # macOS
apt-get install qpdf  # Linux

# Compile
kgc latex build --input thesis/main.tex --output thesis-raw.pdf

# Strip metadata
qpdf --deterministic-id --stream-data=uncompress thesis-raw.pdf thesis-deterministic.pdf
```

Now compare:

```bash
kgc latex build --input thesis/main.tex --output build1.pdf
qpdf --deterministic-id build1.pdf build1-clean.pdf

kgc latex build --input thesis/main.tex --output build2.pdf
qpdf --deterministic-id build2.pdf build2-clean.pdf

diff build1-clean.pdf build2-clean.pdf
echo $?
# Output: 0 (files are identical!)
```

### Option B: Pin Metadata in LaTeX

Add this to your LaTeX preamble:

```latex
\documentclass{article}

% Reproducible builds: fix date and metadata
\usepackage{hyperref}
\hypersetup{
  pdfcreator={KGC LaTeX Pipeline v1.0},
  pdfproducer={XeTeX + SwiftLaTeX},
  pdfcreationdate={D:20251227120000Z00'00'},  % Fixed timestamp
  pdfmoddate={D:20251227120000Z00'00'}        % Fixed timestamp
}

% Optionally fix \today
\day=27
\month=12
\year=2025

\begin{document}
% Your content
\end{document}
```

Rebuild and verify:

```bash
kgc latex build --input thesis/main.tex --output build1.pdf
kgc latex build --input thesis/main.tex --output build2.pdf
diff build1.pdf build2.pdf
# Output: (no output - files identical!)
```

## Step 5: Test Cross-Machine Reproducibility

### On Machine A:

```bash
# Build and hash
kgc latex build --input thesis/main.tex --output thesis.pdf
sha256sum thesis.pdf > thesis.pdf.sha256

# Output:
# a1b2c3d4e5f6789... thesis.pdf
```

Commit and push:

```bash
git add thesis.pdf.sha256 .latex-cache/latex.lock.json
git commit -m "Add reproducible build hash"
git push
```

### On Machine B:

```bash
# Clone repo
git clone https://github.com/yourteam/thesis.git
cd thesis

# Build (lockfile ensures same packages)
kgc latex build --input thesis/main.tex --output thesis.pdf

# Verify hash matches
sha256sum -c thesis.pdf.sha256

# Output:
# thesis.pdf: OK ✓
```

**Success!** Machine B produced bit-identical PDF.

## Step 6: Enforce Determinism in CI

GitHub Actions example:

```yaml
name: Verify Deterministic Build

on: [push, pull_request]

jobs:
  verify:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'

      - name: Install dependencies
        run: pnpm install

      - name: Restore LaTeX cache
        uses: actions/cache@v3
        with:
          path: .latex-cache/ctan/
          key: latex-${{ hashFiles('.latex-cache/latex.lock.json') }}

      - name: Build PDF (first)
        run: kgc latex build --input thesis/main.tex --output build1.pdf

      - name: Build PDF (second)
        run: kgc latex build --input thesis/main.tex --output build2.pdf

      - name: Strip metadata
        run: |
          qpdf --deterministic-id build1.pdf build1-clean.pdf
          qpdf --deterministic-id build2.pdf build2-clean.pdf

      - name: Verify bit-identical builds
        run: |
          diff build1-clean.pdf build2-clean.pdf
          if [ $? -ne 0 ]; then
            echo "ERROR: Builds are not deterministic!"
            exit 1
          fi
          echo "✓ Builds are deterministic"

      - name: Verify hash (if committed)
        run: sha256sum -c thesis.pdf.sha256 || echo "Warning: Hash file not found"
```

This CI job:
1. Builds PDF twice
2. Strips metadata
3. Verifies bit-identical output
4. Optionally checks committed hash

## Troubleshooting Determinism

### Different Hashes Across Machines

**Diagnosis**:

```bash
# On both machines:
sha256sum thesis.pdf
```

If different, check:

1. **Lockfile version**: Ensure same commit
   ```bash
   git log -1 .latex-cache/latex.lock.json
   ```

2. **Package cache**: Verify package hashes
   ```bash
   jq '.resolvedInputs | to_entries[] | "\(.key): \(.value.hash)"' .latex-cache/latex.lock.json
   ```

3. **Source files**: Ensure same LaTeX source
   ```bash
   git status
   ```

4. **Metadata**: Check if metadata differs
   ```bash
   pdfinfo build1.pdf > info1.txt
   pdfinfo build2.pdf > info2.txt
   diff info1.txt info2.txt
   ```

### Lockfile Merge Conflicts

When multiple team members add packages:

```bash
# Alice adds algorithm2e.sty
# Bob adds tikz.sty
# Merge conflict in latex.lock.json
```

**Resolution**: Just rebuild

```bash
# 1. Accept either version of lockfile
git checkout --theirs .latex-cache/latex.lock.json

# 2. Rebuild (will add missing packages)
kgc latex build --input thesis/main.tex

# 3. Commit merged lockfile
git add .latex-cache/latex.lock.json
git commit -m "Merge lockfiles: algorithm2e + tikz"
```

The pipeline automatically resolves conflicts by merging package sets.

### Cache Corruption

If cache gets corrupted:

```bash
# Delete cache (keep lockfile!)
rm -rf .latex-cache/ctan/

# Rebuild (re-downloads from lockfile URLs)
kgc latex build --input thesis/main.tex
```

The lockfile ensures we fetch the **exact same versions** (verified by hash).

## Advanced: Hermetic Builds

For **maximum** determinism (e.g., regulatory compliance):

### 1. Vendor All Dependencies

Instead of fetching from CTAN, commit packages to repo:

```bash
# After first build, copy cache to repo
cp -r .latex-cache/ctan/ vendor/latex-packages/

# Add to Git
git add vendor/latex-packages/
git commit -m "Vendor LaTeX packages for hermetic builds"
```

### 2. Use Local Cache Only

```javascript
// Programmatic API: disable network fetching
import { compileLatexToPdf } from '@unrdf/kgc-cli/src/lib/latex/compile.mjs';

// Pre-populate VFS with vendored packages
const vfs = await loadVendoredPackages('vendor/latex-packages/');

// Compile with no network access
const pdf = await compileLatexToPdf({
  inputTexPath: '/path/to/main.tex',
  projectDir: '/path/to/project',
  // ... options
});
```

### 3. Freeze TeX Engine Version

The WebAssembly engine is vendored, so it's already deterministic. But to be explicit:

```json
// package.json
{
  "dependencies": {
    "@unrdf/kgc-cli": "5.0.1"  // Pin exact version
  }
}
```

## Verification Checklist

Your build is deterministic if:

- [ ] Same lockfile → same package versions (verify hashes)
- [ ] Same source files → same PDF output (bit-identical)
- [ ] Different machines → same hash (cross-machine test)
- [ ] Different times → same hash (rebuild after 1 week)
- [ ] CI builds → all identical (automated verification)

## Performance

Deterministic builds have **zero performance overhead**:

- Lockfile: ~1KB (minimal I/O)
- Hash verification: ~10ms per package
- Sorted VFS: Negligible (microseconds)

**Benefit**: Faster cache hits (packages verified by hash, not re-downloaded)

## Comparison with Traditional TeX

| Aspect | Traditional TeX | KGC Pipeline |
|--------|----------------|--------------|
| Package versions | `tlmgr update` (variable) | Lockfile (fixed) |
| File ordering | Filesystem-dependent | Sorted VFS (stable) |
| Metadata | Timestamp on every build | Configurable / strippable |
| Cross-machine | Not guaranteed | Lockfile ensures |
| Verification | Manual (`diff`) | Automated (CI) |

## Summary

You've achieved deterministic LaTeX builds:

- ✓ Lockfile pins package versions by content hash
- ✓ Sorted VFS ensures stable file ordering
- ✓ Metadata control removes timestamps
- ✓ Cross-machine reproducibility verified
- ✓ CI automation ensures ongoing determinism

**Next steps**:
- Set up [Offline Bundles](./offline-bundle.md) for air-gapped builds
- Learn about [Caching Strategy](../explanation/caching.md)
- Understand [Lockfile Schema](../reference/lockfile-schema.md)
- Read [Architecture Explanation](../explanation/architecture.md)
