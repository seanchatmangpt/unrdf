# LaTeX Features - Installation & Setup Guide

> **Status**: ⚠️ EXPERIMENTAL (v6.0.0-rc.3) - Not production-ready

This guide covers installation, configuration, and troubleshooting for the LaTeX compilation features in `@unrdf/kgc-cli`.

## Quick Facts

| Aspect | Status |
|--------|--------|
| Core KGC-CLI | ✅ Production Ready |
| LaTeX Features | ⚠️ Experimental |
| Test Pass Rate | 4/15 (26.7%) |
| Multi-file Support | ❌ Known Issues |
| Recommended Use | Evaluation, Simple Documents |

## Installation Steps

### 1. Prerequisites

Ensure you have:
- **Node.js** ≥18.0.0
- **unzip** command-line tool (required for WASM setup)
- **3-5 minutes** of time

Check your versions:
```bash
node --version      # Should be v18.0.0 or higher
which unzip         # Should return path to unzip command
```

### 2. Install KGC-CLI Package

```bash
# Option A: Add to existing project
pnpm add @unrdf/kgc-cli

# Option B: Use in workspace
pnpm -C packages/kgc-cli install
```

### 3. Download & Setup WASM Binaries

**This is REQUIRED before using LaTeX features.**

```bash
# Navigate to kgc-cli package
cd packages/kgc-cli

# Run vendor script
node scripts/vendor-tex-engine.mjs
```

**What this does**:
- Downloads SwiftLaTeX WASM binaries (~15-20 MB)
- Extracts and validates files
- Creates `vendor/swiftlatex/` directory
- Generates `MANIFEST.json` for version tracking

**Expected Output**:
```
SwiftLaTeX WASM Engine Vendor Script
=====================================

Downloading SwiftLaTeX WASM binaries...

Downloading: https://github.com/SwiftLaTeX/SwiftLaTeX/archive/refs/tags/v15022022.zip
✓ Downloaded to: .tmp/swiftlatex-v15022022.zip

Extracting archive...
✓ Extracted

Copying WASM binaries to vendor directory...
  ✓ Copied: swiftlatex.wasm
  ✓ Copied: swiftlatex.js

✓ Installation complete!

Files installed:
  - swiftlatex.wasm (19.45 MB)
  - swiftlatex.js (0.52 MB)
```

### 4. Verify Installation

```bash
# Run diagnostic command
node src/cli.mjs latex diagnose

# Expected output shows:
# - WASM binaries status
# - Available packages
# - Engine version
# - Cache directory
```

### 5. Test with Example Document

```bash
# Create simple LaTeX document
cat > /tmp/hello.tex << 'EOF'
\documentclass{article}
\usepackage[utf8]{inputenc}
\title{Hello UNRDF}
\author{Claude}
\date{\today}
\begin{document}
\maketitle
\section{Introduction}
This is a test document compiled by SwiftLaTeX via UNRDF.
\end{document}
EOF

# Compile it
node src/cli.mjs latex build \
  --input /tmp/hello.tex \
  --output /tmp/hello.pdf

# Verify output
ls -lh /tmp/hello.pdf
file /tmp/hello.pdf
```

## Known Limitations

### What Works
✅ Simple single-file LaTeX documents
✅ Basic document classes (`article`, `report`, `book`)
✅ Common packages (`inputenc`, `babel`, `graphicx`)
✅ Cross-references and bibliography (with repeated compilation)

### What Doesn't Work (v6.0.0-rc.3)
❌ Multi-file projects (`.tex` includes, relative paths)
❌ Some advanced packages (TikZ, pgfplots, complex graphics)
❌ System font references (`\usefont` with system fonts)
❌ External tool calls (gnuplot, Asymptote)
❌ Complex error handling (errors can be cryptic)

### Error Examples

**Error: "File not found"**
```
Cause: Referenced file (.sty, .cls) not available in CTAN
Fix: Check package name spelling; some packages have different names in CTAN
```

**Error: "VFS initialization failed"**
```
Cause: WASM binaries not found or corrupted
Fix: Re-run: node scripts/vendor-tex-engine.mjs
```

**Error: "Compilation timeout"**
```
Cause: Complex document taking too long
Fix: Simplify document or break into smaller parts
```

## Troubleshooting

### Issue: "unzip command not found"

```bash
# Install unzip
# Ubuntu/Debian
sudo apt-get install unzip

# macOS
brew install unzip

# Or download from https://gnuzip.org
```

### Issue: "Module not found: swiftlatex"

The WASM binaries haven't been set up yet:

```bash
# Reinstall WASM binaries
cd packages/kgc-cli
node scripts/vendor-tex-engine.mjs

# Verify files exist
ls -la vendor/swiftlatex/swiftlatex.wasm
```

### Issue: "Cache directory read-only"

Check permissions:

```bash
# Check cache directory
ls -la vendor/swiftlatex/

# Fix permissions if needed
chmod -R u+w vendor/swiftlatex/
```

### Issue: LaTeX tests skip silently

This is expected behavior when WASM binaries are not installed. To see test results:

```bash
# Run tests explicitly
pnpm test -- latex-build.test.mjs

# Should show "40 tests skipped" if binaries not found
# Or "4 passed, 11 failed" if binaries are installed
```

## Without LaTeX Setup

**Good news**: You don't need LaTeX features to use KGC-CLI!

All other features work normally:
- ✅ Registry and extension system
- ✅ Command resolution
- ✅ JSON envelope output
- ✅ All other extension packages

LaTeX will simply not be available:

```bash
# This still works
kgc --help
kgc snapshot --help

# This gracefully fails
kgc latex build --input test.tex --output test.pdf
# Error: LaTeX extension not available. Run: node scripts/vendor-tex-engine.mjs
```

## Production Deployment

### For Production Use

**NOT RECOMMENDED** for v6.0.0-rc.3. Instead:

```bash
# Option 1: Use external TeX Live/MiKTeX
apt-get install texlive-full
pdflatex thesis/main.tex

# Option 2: Use online service
# Overleaf: https://www.overleaf.com
# ShareLaTeX: https://www.sharelatex.com

# Option 3: Wait for v6.0.0 stable (production-ready LaTeX support)
```

### For CI/CD Pipelines

If you want to test LaTeX features in CI/CD:

```yaml
# GitHub Actions example
- name: Setup LaTeX Features
  run: |
    cd packages/kgc-cli
    node scripts/vendor-tex-engine.mjs

- name: Run LaTeX Tests
  run: |
    pnpm test -- latex-build.test.mjs
```

## Configuration Files

### SwiftLaTeX Manifest

Location: `packages/kgc-cli/vendor/swiftlatex/MANIFEST.json`

Example:
```json
{
  "version": "v15022022",
  "source": "https://github.com/SwiftLaTeX/SwiftLaTeX/archive/refs/tags/v15022022.zip",
  "installedAt": "2026-01-19T10:30:00.000Z",
  "files": ["swiftlatex.wasm", "swiftlatex.js"]
}
```

### Cache Directory

Location: `vendor/swiftlatex/cache/`

- Stores downloaded CTAN packages
- Can be safely deleted to clear cache
- Recreated on first compilation

### Lockfile

Location: `latex.lock.json` (created in project directory)

Tracks all packages used for reproducible builds. See [Lockfile Schema](./reference/lockfile-schema.md) for details.

## Updating WASM Binaries

To update to a newer SwiftLaTeX release:

```bash
# Remove old binaries
rm -rf packages/kgc-cli/vendor/swiftlatex/

# Re-run vendor script
cd packages/kgc-cli
node scripts/vendor-tex-engine.mjs
```

## Uninstalling LaTeX Features

To remove LaTeX support:

```bash
# Remove WASM binaries
rm -rf packages/kgc-cli/vendor/swiftlatex/

# Remove cache
rm -rf packages/kgc-cli/vendor/swiftlatex/cache/

# LaTeX commands will no longer be available
kgc latex build --input test.tex
# Error: LaTeX extension not available
```

## Getting Help

### Where to Find Help

- **Documentation**: See [LaTeX Docs](./README.md)
- **Architecture**: [Pipeline Architecture](./explanation/architecture.md)
- **Common Errors**: [Failure Modes](./explanation/failure-modes.md)
- **GitHub Issues**: https://github.com/unrdf/unrdf/issues?label=latex
- **Discussions**: https://github.com/unrdf/unrdf/discussions

### Reporting Issues

When reporting issues, include:

```bash
# 1. Diagnostic output
node packages/kgc-cli/src/cli.mjs latex diagnose > latex-diag.txt

# 2. Test output
pnpm -C packages/kgc-cli test -- latex-build.test.mjs > latex-tests.txt

# 3. Your LaTeX document
# Simplified version that reproduces the issue
```

## Next Steps

- **New to LaTeX?** → Read [Your First PDF](./tutorials/first-pdf.md)
- **Want reproducible builds?** → [Deterministic Builds Guide](./how-to/deterministic-builds.md)
- **Need offline support?** → [Offline Bundle Guide](./how-to/offline-bundle.md)
- **Curious about architecture?** → [Pipeline Architecture](./explanation/architecture.md)

## Timeline

| Version | Status | LaTeX Support |
|---------|--------|---------------|
| v6.0.0-rc.3 (current) | 🧪 Experimental | Simple single-file documents |
| v6.0.0 (planned) | ✅ Production Ready | Full multi-file support |

---

**Last Updated**: January 19, 2026
**Status**: Experimental v6.0.0-rc.3
**Maintained By**: UNRDF Contributors
