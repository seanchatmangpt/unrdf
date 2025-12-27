# WASM Binary Licenses

## Binary Components

The SwiftLaTeX WASM binaries (`xetex.wasm`, `pdftex.wasm`) are compiled from multiple upstream projects with different licenses.

## Component Licenses

### XeTeX Engine
- **License**: MIT License / X11 License
- **Copyright**: SIL International, Jonathan Kew, Khaled Hosny, et al.
- **Source**: https://github.com/TeX-Live/xetex
- **Status**: Permissive, commercial use allowed

### pdfTeX Engine
- **License**: GNU General Public License v2 (GPL-2.0)
- **Copyright**: The pdfTeX Team, Hàn Thế Thành, et al.
- **Source**: https://www.tug.org/applications/pdftex/
- **Status**: Copyleft license, derivative works must be GPL

### TeX Live Libraries
- **Licenses**: Various (BSD, MIT, Public Domain)
- **Components**: kpathsea, libpng, zlib, freetype, fontconfig
- **Source**: https://www.tug.org/texlive/
- **Status**: Mostly permissive

### Emscripten Runtime
- **License**: MIT License / University of Illinois Open Source License
- **Copyright**: Emscripten contributors
- **Source**: https://emscripten.org/
- **Status**: Permissive, included in WASM output

## License Implications

### GPL Components (pdfTeX)

**Important**: `pdftex.wasm` contains GPL-licensed code.

**Compliance Requirements**:
1. Provide source code or written offer for source
2. Derivative works must be GPL-compatible
3. Cannot add additional restrictions

**Our Usage**:
- We redistribute binary without modification
- No linking (WASM runs in isolated sandbox)
- MIT license (our project) is GPL-compatible for this use case
- Source available at upstream repositories

### Permissive Components (XeTeX, Libraries)

**MIT/BSD/X11 Licensed**:
- Commercial use allowed
- Modification allowed
- Redistribution allowed with attribution
- No warranty

## Compliance Checklist

For vendoring these binaries:

- [x] Include all upstream license texts
- [x] Preserve copyright notices
- [x] Document license of each component
- [ ] Provide source code links for GPL components
- [ ] Include build instructions (if we modify binaries)
- [x] State "AS IS" warranty disclaimer

## Source Code Availability

As required by GPL (for pdfTeX components):

**Source Repositories**:
- SwiftLaTeX: https://github.com/SwiftLaTeX/SwiftLaTeX
- pdfTeX: https://www.tug.org/applications/pdftex/
- XeTeX: https://github.com/TeX-Live/xetex
- TeX Live: https://www.tug.org/texlive/

**Build Instructions**: See SwiftLaTeX repository for WASM compilation process.

## License Compatibility Matrix

| Component | License | Compatible with MIT? | Redistribution OK? |
|-----------|---------|----------------------|--------------------|
| SwiftLaTeX | Apache 2.0 | Yes | Yes |
| XeTeX | MIT/X11 | Yes | Yes |
| pdfTeX | GPL-2.0 | Yes (runtime) | Yes (with source) |
| TeX Live libs | BSD/MIT | Yes | Yes |
| Emscripten | MIT/UIUC | Yes | Yes |

## Warranty Disclaimer

ALL COMPONENTS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.

See individual licenses for full warranty disclaimers.

## Updates

When updating WASM binaries:
1. Check upstream licenses for changes
2. Update this document if licenses change
3. Verify GPL compliance for pdfTeX
4. Test binary integrity and functionality

## Contact

For licensing questions:
- SwiftLaTeX: See GitHub repository issues
- TeX User Group: https://www.tug.org/
- Our project: See main repository LICENSE
