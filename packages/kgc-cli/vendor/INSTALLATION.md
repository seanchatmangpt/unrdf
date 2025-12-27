# Vendor Assets Installation Guide

## Overview

This guide explains how to install production WASM binaries for SwiftLaTeX engines.

## Current Status

**Placeholder Mode**: The repository contains placeholder WASM files that will fail at runtime with clear error messages indicating missing binaries.

## Why Placeholders?

1. **Size**: Production WASM binaries are 25-45 MB combined (too large for git)
2. **Determinism**: Vendored binaries ensure reproducible builds
3. **Offline**: No install-time downloads required once vendored
4. **Security**: Manual verification of binary integrity required

## Installation Steps

### 1. Download SwiftLaTeX Binaries

```bash
# Clone SwiftLaTeX repository
git clone https://github.com/SwiftLaTeX/SwiftLaTeX.git
cd SwiftLaTeX

# Or download pre-built binaries from releases
wget https://github.com/SwiftLaTeX/SwiftLaTeX/releases/download/vX.Y.Z/xetex.wasm
wget https://github.com/SwiftLaTeX/SwiftLaTeX/releases/download/vX.Y.Z/pdftex.wasm
```

### 2. Verify Binary Integrity

```bash
# Download checksums
wget https://github.com/SwiftLaTeX/SwiftLaTeX/releases/download/vX.Y.Z/SHA256SUMS

# Verify
sha256sum -c SHA256SUMS
```

### 3. Copy to Vendor Directory

```bash
# From your download location
cp xetex.wasm /path/to/unrdf/packages/kgc-cli/vendor/swiftlatex/
cp pdftex.wasm /path/to/unrdf/packages/kgc-cli/vendor/swiftlatex/

# Verify placement
ls -lh packages/kgc-cli/vendor/swiftlatex/*.wasm
```

### 4. Verify Installation

```bash
# Check file sizes (should be ~15-25 MB each)
wc -c packages/kgc-cli/vendor/swiftlatex/*.wasm

# Placeholders are <1 KB, real binaries are 10-25 MB
# If still <1 KB, you're still using placeholders

# Test with loader (if implemented)
node -e "import('./packages/kgc-cli/src/lib/swiftlatex-loader.mjs').then(m => console.log(m))"
```

## Alternative: Build from Source

### Prerequisites
- Emscripten SDK (emsdk)
- TeX Live source distribution
- 4+ GB RAM for compilation

### Build Steps

```bash
# Install Emscripten
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
./emsdk install latest
./emsdk activate latest
source ./emsdk_env.sh

# Clone SwiftLaTeX
git clone https://github.com/SwiftLaTeX/SwiftLaTeX.git
cd SwiftLaTeX

# Follow build instructions in SwiftLaTeX repository
# (typically involves make commands for each engine)

# Copy built binaries
cp build/xetex.wasm /path/to/unrdf/packages/kgc-cli/vendor/swiftlatex/
cp build/pdftex.wasm /path/to/unrdf/packages/kgc-cli/vendor/swiftlatex/
```

## CI/CD Considerations

### Option 1: Git LFS
```bash
# Track WASM files with Git LFS
git lfs track "*.wasm"
git lfs install
```

### Option 2: Artifact Storage
```yaml
# .github/workflows/build.yml
- name: Download SwiftLaTeX binaries
  run: |
    curl -L -o xetex.wasm ${{ secrets.WASM_ARTIFACT_URL }}/xetex.wasm
    curl -L -o pdftex.wasm ${{ secrets.WASM_ARTIFACT_URL }}/pdftex.wasm
    mv *.wasm packages/kgc-cli/vendor/swiftlatex/
```

### Option 3: Package Separately
```bash
# Create @unrdf/kgc-cli-wasm package
# Install as peer dependency
npm install @unrdf/kgc-cli-wasm
```

## Validation

After installation, verify with:

```bash
# File size check
du -h packages/kgc-cli/vendor/swiftlatex/*.wasm

# Expected output:
# 15M   xetex.wasm
# 12M   pdftex.wasm

# File type check
file packages/kgc-cli/vendor/swiftlatex/*.wasm

# Expected output:
# xetex.wasm: WebAssembly (wasm) binary module version 0x1 (mvp)
# pdftex.wasm: WebAssembly (wasm) binary module version 0x1 (mvp)
```

## Runtime Error Messages

If placeholders are still in place, expect:

```
Error: SwiftLaTeX WASM binary not found: vendor/swiftlatex/xetex.wasm
Expected size: ~15-25 MB, found: <1 KB
This is a placeholder file. See vendor/INSTALLATION.md
```

## Troubleshooting

### Binary Too Small
**Problem**: File is <1 KB
**Cause**: Still using placeholder
**Fix**: Follow installation steps above

### Binary Won't Load
**Problem**: Runtime error loading WASM
**Cause**: Corrupted download or wrong version
**Fix**: Re-download and verify checksums

### Import Errors
**Problem**: `Cannot find module vendor/swiftlatex/xetex.wasm`
**Cause**: File not in expected location
**Fix**: Ensure path is `packages/kgc-cli/vendor/swiftlatex/*.wasm`

## Security Notes

1. **Only download from official sources**
2. **Always verify SHA256 checksums**
3. **Review SwiftLaTeX security advisories**
4. **Scan binaries with malware detection if paranoid**
5. **Pin specific versions in production**

## License Compliance

See `swiftlatex/LICENSES/` for:
- SwiftLaTeX Apache 2.0 license
- pdfTeX GPL-2.0 license requirements
- Component license details

## Support

- SwiftLaTeX issues: https://github.com/SwiftLaTeX/SwiftLaTeX/issues
- This project: See main repository issues
- TeX User Group: https://www.tug.org/
