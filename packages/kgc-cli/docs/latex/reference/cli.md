# CLI Reference

Complete command-line interface documentation for the LaTeX compilation pipeline.

## Synopsis

```bash
kgc latex <command> [options]
```

## Commands

### `build`

Compile a LaTeX document to PDF.

```bash
kgc latex build --input <file> [options]
```

#### Required Arguments

**`--input <path>`** (alias: `-i`)
- Path to main `.tex` file
- Can be relative or absolute
- Must exist on filesystem

Example:
```bash
kgc latex build --input thesis/main.tex
kgc latex build -i paper.tex
```

#### Optional Arguments

**`--output <path>`** (alias: `-o`)
- Output PDF file path
- Default: `dist/thesis.pdf`
- Parent directory created if needed

Example:
```bash
kgc latex build --input main.tex --output build/paper.pdf
kgc latex build -i main.tex -o out.pdf
```

**`--engine <name>`** (alias: `-e`)
- LaTeX engine to use
- Options: `pdflatex`, `xelatex`, `lualatex`
- Default: `pdflatex`

Engine comparison:
| Engine | Speed | Unicode | Fonts | Graphics |
|--------|-------|---------|-------|----------|
| `pdflatex` | Fast | Limited | Type1 | PDF, PNG, JPG |
| `xelatex` | Moderate | Full | OpenType, TrueType | PDF, PNG, JPG |
| `lualatex` | Slow | Full | OpenType, TrueType | PDF, PNG, JPG, SVG |

Example:
```bash
kgc latex build --input thesis.tex --engine xelatex
kgc latex build -i main.tex -e pdflatex
```

**`--passes <number>`** (alias: `-p`)
- Number of compilation passes
- Range: 1-5
- Default: 2

Why multiple passes:
- Pass 1: Generate `.aux` files, identify missing references
- Pass 2: Resolve cross-references, table of contents
- Pass 3+: Resolve complex multi-pass dependencies (rare)

Example:
```bash
kgc latex build --input book.tex --passes 3
kgc latex build -i main.tex -p 1  # Quick draft
```

**`--cacheDir <path>`** (alias: `-c`)
- Cache directory for packages and logs
- Default: `.latex-cache` (relative to project root)
- Contains: `ctan/` (packages), `latex.lock.json` (lockfile), `runs/` (logs)

Example:
```bash
kgc latex build --input main.tex --cacheDir /tmp/latex-cache
kgc latex build -i main.tex -c .cache
```

**`--projectRoot <path>`** (alias: `-r`)
- Project root directory (for multi-file projects)
- Default: Directory containing `--input` file
- All files under this directory are included in VFS

Example:
```bash
kgc latex build --input thesis/chapters/intro.tex --projectRoot thesis/
```

**`--verbose`** (alias: `-v`)
- Enable verbose logging
- Shows: VFS collection, engine output, package resolution
- Default: `false`

Example:
```bash
kgc latex build --input main.tex --verbose
```

**`--help`** (alias: `-h`)
- Show help message
- Lists all options and examples

Example:
```bash
kgc latex build --help
```

#### Exit Codes

| Code | Meaning | Description |
|------|---------|-------------|
| `0` | Success | PDF generated successfully |
| `1` | Compilation error | LaTeX syntax error, missing file, etc. |
| `2` | Invalid arguments | Missing `--input`, invalid `--engine`, etc. |
| `3` | File not found | Input file or project directory doesn't exist |
| `4` | Network error | Failed to fetch package from CTAN (if online mode) |
| `5` | Cache error | Corrupt cache, disk full, permissions issue |

#### Examples

**Basic compilation**:
```bash
kgc latex build --input main.tex
```

**Custom output path**:
```bash
kgc latex build --input thesis/main.tex --output dist/thesis-$(date +%Y%m%d).pdf
```

**Use XeLaTeX for Unicode**:
```bash
kgc latex build --input chinese.tex --engine xelatex
```

**Fast single-pass draft**:
```bash
kgc latex build --input draft.tex --passes 1 --output quick.pdf
```

**Full multi-file project**:
```bash
kgc latex build \
  --input book/main.tex \
  --projectRoot book/ \
  --output build/book.pdf \
  --engine xelatex \
  --passes 3 \
  --verbose
```

---

### `validate`

Validate LaTeX document structure without generating PDF (dry-run).

```bash
kgc latex validate --input <file>
```

**Status**: Not yet implemented (placeholder).

When implemented, will:
- Parse LaTeX document
- Check syntax errors
- Verify referenced files exist
- Check for missing packages
- Report warnings and errors

---

### `clean`

Clear LaTeX compilation cache.

```bash
kgc latex clean [options]
```

**Status**: Not yet implemented (placeholder).

#### Planned Options

**`--cacheDir <path>`**
- Cache directory to clean
- Default: `.latex-cache`

**`--packages`**
- Clear CTAN package cache only
- Preserves lockfile and logs

**`--logs`**
- Clear compilation logs only
- Preserves packages and lockfile

**`--all`**
- Clear everything (packages, logs, lockfile)

When implemented, examples:
```bash
# Clear all cache
kgc latex clean --all

# Clear packages only (keep lockfile)
kgc latex clean --packages

# Clear specific cache directory
kgc latex clean --cacheDir /tmp/latex-cache
```

---

## Global Options

These options work with all commands.

**`--json`**
- Output results as JSON
- Useful for scripting and automation

Example:
```bash
kgc latex build --input main.tex --json
```

Output (success):
```json
{
  "ok": true,
  "data": {
    "output": "/path/to/output.pdf",
    "size": 8192,
    "message": "Successfully compiled to output.pdf (8192 bytes)"
  },
  "meta": {
    "source": "@unrdf/latex",
    "timestamp": "2025-12-27T10:30:45.123Z"
  }
}
```

Output (error):
```json
{
  "ok": false,
  "code": "COMPILATION_FAILED",
  "message": "LaTeX compilation failed after 2 cycles",
  "details": {
    "logFilePath": ".latex-cache/runs/20251227_103045_xetex.log",
    "cycles": 2
  },
  "meta": {
    "timestamp": "2025-12-27T10:30:45.123Z"
  }
}
```

**`--no-color`**
- Disable colored output
- Useful for CI/CD logs

Example:
```bash
kgc latex build --input main.tex --no-color
```

---

## Environment Variables

**`CTAN_MIRROR`**
- Override default CTAN mirror
- Default: `https://mirrors.ctan.org`

Example:
```bash
export CTAN_MIRROR=https://ctan.math.utah.edu
kgc latex build --input main.tex
```

**`LATEX_CACHE_DIR`**
- Override default cache directory
- Default: `.latex-cache`

Example:
```bash
export LATEX_CACHE_DIR=/tmp/my-latex-cache
kgc latex build --input main.tex
```

**`NODE_OPTIONS`**
- Increase Node.js memory limit (for large documents)
- Default: System default (~2GB)

Example:
```bash
export NODE_OPTIONS="--max-old-space-size=4096"
kgc latex build --input huge-book.tex
```

---

## Configuration File

**Status**: Not yet implemented.

Planned: `.latexrc.json` in project root.

```json
{
  "engine": "xelatex",
  "passes": 2,
  "cacheDir": ".cache/latex",
  "ctanMirror": "https://mirrors.ctan.org",
  "verbose": false
}
```

When implemented, CLI flags override config file values.

---

## Scripting Examples

### Bash Script

```bash
#!/bin/bash
# build-all-chapters.sh

set -e

CHAPTERS=(intro methods results conclusion)

for chapter in "${CHAPTERS[@]}"; do
  echo "Building chapter: $chapter"
  kgc latex build \
    --input "chapters/${chapter}.tex" \
    --output "build/${chapter}.pdf" \
    --engine xelatex
done

echo "All chapters built successfully"
```

### Makefile

```makefile
# Makefile for LaTeX compilation

.PHONY: all clean

CHAPTERS := intro methods results conclusion
PDFS := $(addprefix build/,$(addsuffix .pdf,$(CHAPTERS)))

all: build/thesis.pdf

build/%.pdf: chapters/%.tex
	@mkdir -p build
	kgc latex build --input $< --output $@

build/thesis.pdf: thesis/main.tex $(PDFS)
	kgc latex build \
		--input thesis/main.tex \
		--output build/thesis.pdf \
		--engine xelatex \
		--passes 3

clean:
	rm -rf build/
	kgc latex clean --all
```

### npm script

```json
{
  "name": "my-thesis",
  "scripts": {
    "build": "kgc latex build --input thesis/main.tex --output dist/thesis.pdf",
    "build:draft": "kgc latex build --input thesis/main.tex --output dist/draft.pdf --passes 1",
    "build:final": "kgc latex build --input thesis/main.tex --output dist/thesis-final.pdf --engine xelatex --passes 3",
    "clean": "rm -rf dist/ .latex-cache/runs/",
    "watch": "nodemon --watch thesis/ --ext tex --exec 'npm run build:draft'"
  }
}
```

### CI/CD (GitHub Actions)

```yaml
name: Build LaTeX

on:
  push:
    paths:
      - 'thesis/**/*.tex'
      - '.latex-cache/latex.lock.json'

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
          key: latex-${{ hashFiles('.latex-cache/latex.lock.json') }}

      - name: Build PDF
        run: kgc latex build --input thesis/main.tex --output thesis.pdf --json | tee result.json

      - name: Check build status
        run: |
          if ! jq -e '.ok == true' result.json > /dev/null; then
            echo "Build failed"
            jq '.message' result.json
            exit 1
          fi

      - name: Upload PDF
        uses: actions/upload-artifact@v3
        with:
          name: thesis
          path: thesis.pdf
```

---

## Troubleshooting

### Command not found: `kgc`

**Solution**: Install globally or use via `npx`:

```bash
# Install globally
pnpm add -g @unrdf/kgc-cli

# Or use npx
npx @unrdf/kgc-cli latex build --input main.tex
```

### Error: "Input file not found"

**Solution**: Verify file path and permissions:

```bash
ls -la main.tex
kgc latex build --input "$(pwd)/main.tex"
```

### Error: "Permission denied" (cache directory)

**Solution**: Check cache directory permissions:

```bash
ls -ld .latex-cache/
chmod 755 .latex-cache/
```

---

## See Also

- [JavaScript API Reference](./api.md) - Programmatic usage
- [Lockfile Schema](./lockfile-schema.md) - `latex.lock.json` format
- [How-To Guides](../how-to/) - Practical examples
- [Failure Modes](../explanation/failure-modes.md) - Common errors

---

## Changelog

**v5.0.1** (2025-12-27)
- Initial implementation of `build` command
- Support for `pdflatex`, `xelatex`, `lualatex`
- Automatic CTAN package resolution
- Lockfile generation

**Planned**
- `validate` command (dry-run checking)
- `clean` command (cache management)
- Configuration file support (`.latexrc.json`)
