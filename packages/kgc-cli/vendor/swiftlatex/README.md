# SwiftLaTeX WASM Binaries

## Overview

This directory contains WebAssembly binaries for SwiftLaTeX LaTeX engines.

## Files

- **xetex.wasm**: XeTeX engine compiled to WebAssembly
- **pdftex.wasm**: pdfTeX engine compiled to WebAssembly

## Origin

SwiftLaTeX is an open-source WebAssembly-based LaTeX compiler that runs in browsers and Node.js environments.

**Source**: https://github.com/SwiftLaTeX/SwiftLaTeX

## Current Status

**PLACEHOLDER FILES**: The current WASM files are placeholders that return error messages indicating missing binaries.

To use SwiftLaTeX functionality:

1. Download production WASM binaries from SwiftLaTeX releases
2. Replace placeholder files in this directory
3. Verify integrity with checksums from upstream

## Integration

The loader module (`src/lib/swiftlatex-loader.mjs`) expects these files at:
```
vendor/swiftlatex/xetex.wasm
vendor/swiftlatex/pdftex.wasm
```

Paths are resolved using `import.meta.url` for ES module compatibility.

## License

See `LICENSES/` directory for:
- SwiftLaTeX project license
- WASM binary compilation licenses

## Security

**Binary Verification Required**:
- Always verify checksums against official releases
- Never execute untrusted WASM binaries
- Review SwiftLaTeX security advisories before updates

## Size Considerations

Production WASM binaries are typically:
- xetex.wasm: ~15-25 MB
- pdftex.wasm: ~10-20 MB

These files are vendored (not downloaded at install time) to ensure:
- Deterministic builds
- Offline compilation capability
- Version lock with package releases
