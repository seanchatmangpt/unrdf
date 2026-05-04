# Common Errors and Debugging

Understanding failure modes, error messages, and debugging strategies for the LaTeX→PDF pipeline.

## Error Classification

Errors fall into five categories:

| Category | Source | Examples |
|----------|--------|----------|
| **Input Validation** | File not found, invalid args | `Input file not found: main.tex` |
| **LaTeX Syntax** | TeX compilation errors | `! Undefined control sequence.` |
| **Missing Dependencies** | Package/file not available | `File 'tikz.sty' not found` |
| **Network Errors** | CTAN fetch failures | `Failed to fetch from CTAN: ETIMEDOUT` |
| **System Errors** | Out of memory, disk full | `Out of memory` (WASM), `ENOSPC` (disk) |

---

## Input Validation Errors

### Error: Input file not found

```
✗ Error: Input file not found: /path/to/main.tex
```

**Cause**: File doesn't exist or path is wrong

**Debug**:
```bash
# Verify file exists
ls -la /path/to/main.tex

# Check path is absolute
realpath main.tex

# Try absolute path explicitly
kgc latex build --input "$(pwd)/main.tex"
```

**Solution**: Provide correct absolute path

---

### Error: Project directory not found

```
✗ Error: Project directory not found: /path/to/project
```

**Cause**: `--projectRoot` points to non-existent directory

**Debug**:
```bash
# Check directory exists
ls -ld /path/to/project

# If relative, convert to absolute
realpath project/
```

**Solution**: Use absolute path or omit `--projectRoot` (defaults to input file's directory)

---

### Error: Invalid engine

```
✗ Error: Invalid engine: latx
  Valid engines: pdflatex, xelatex, lualatex
```

**Cause**: Typo in `--engine` argument

**Solution**: Use correct engine name (case-sensitive)

---

## LaTeX Syntax Errors

### Error: Undefined control sequence

```
! Undefined control sequence.
l.15 \mycustomcommand
                     {foo}
```

**Cause**: Used command that doesn't exist

**Debug**:
```latex
% Check if package is loaded
\usepackage{mypackage}  % Defines \mycustomcommand

% Or define command yourself
\newcommand{\mycustomcommand}[1]{#1}
```

**Solution**: Load required package or define command

---

### Error: Missing $ inserted

```
! Missing $ inserted.
<inserted text>
                $
l.20 The quadratic formula is x = -b +- sqrt(b^2 - 4ac) / 2a
```

**Cause**: Math symbols outside math mode

**Debug**:
```latex
% Wrong: Math symbols in text mode
x = -b +- sqrt(b^2 - 4ac) / 2a

% Correct: Wrap in math mode
$x = -b \pm \sqrt{b^2 - 4ac} / 2a$

% Or display mode
\[
x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}
\]
```

**Solution**: Use `$...$` for inline math, `\[...\]` for display math

---

### Error: Environment undefined

```
! LaTeX Error: Environment theorem undefined.

\begin{theorem}
```

**Cause**: Environment not defined by loaded packages

**Debug**:
```latex
% Check if package is loaded
\usepackage{amsthm}  % Defines theorem environment

% Or define environment yourself
\newtheorem{theorem}{Theorem}
```

**Solution**: Load package defining environment or define it manually

---

## Missing Dependency Errors

### Error: File not found (Package)

```
! LaTeX Error: File `algorithm2e.sty' not found.
```

**Expected behavior**: Pipeline should auto-fetch from CTAN

**Debug**:
```bash
# Check if CTAN resolution is working
kgc latex build --input main.tex --verbose

# Look for:
# ⚠ Missing input detected: algorithm2e.sty
# ✓ Fetching algorithm2e.sty from CTAN...
```

**If auto-fetch failed**:
```bash
# Check internet connection
ping mirrors.ctan.org

# Check package exists on CTAN
curl -I https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty
```

**Manual workaround**:
```bash
# Download manually
curl -o algorithm2e.sty https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty

# Place in project
mv algorithm2e.sty thesis/style/

# Rebuild
kgc latex build --input thesis/main.tex
```

---

### Error: File not found (Input)

```
! LaTeX Error: File `chapters/intro.tex' not found.
```

**Cause**: Referenced file doesn't exist in VFS

**Debug**:
```bash
# Check file exists in project
find thesis/ -name "intro.tex"

# Check LaTeX path matches
grep "input{" thesis/main.tex
# \input{chapters/intro.tex}  # Path must match project structure
```

**Solution**: Ensure file exists and path is correct

---

### Error: Graphics file not found

```
! Package graphics Error: File `figures/logo.png' not found.
```

**Cause**: Image file not in VFS

**Debug**:
```bash
# Check file exists
ls -la figures/logo.png

# Check file extension is included
# VFS collector includes: .pdf, .png, .jpg, .jpeg
# NOT included: .eps (requires conversion)
```

**Solution for EPS**:
```bash
# Convert EPS to PDF
epstopdf figures/diagram.eps  # Creates diagram.pdf

# Use PDF in LaTeX
\includegraphics{figures/diagram.pdf}  # Not .eps
```

---

## Network Errors

### Error: Failed to fetch from CTAN

```
✗ Failed to fetch 'tikz.sty' from CTAN
  Tried URLs:
    - https://mirrors.ctan.org/graphics/pgf/base/tikz.sty
    - https://mirrors.ctan.org/macros/latex/contrib/tikz/tikz.sty

  Possible reasons:
    - Network offline
    - Package name misspelled
    - File not on CTAN
```

**Debug**:
```bash
# Check internet connection
ping mirrors.ctan.org

# Try fetching manually
curl -I https://mirrors.ctan.org/graphics/pgf/base/tikz.sty

# Check if package exists
# Search: https://ctan.org/search?q=tikz
```

**Solutions**:

**1. Network offline** → Use offline bundle:
```bash
# See: docs/latex/how-to/offline-bundle.md
```

**2. Package misspelled** → Fix LaTeX:
```latex
% Wrong
\usepackage{tkz}

% Correct
\usepackage{tikz}
```

**3. Package not on CTAN** → Add manually:
```bash
# Download from package website
# Place in project/style/
# Pipeline will find it via VFS
```

---

### Error: CTAN timeout

```
✗ Failed to fetch 'algorithm2e.sty' from CTAN
  Error: Network timeout after 30s
```

**Cause**: Slow network or CTAN mirror down

**Debug**:
```bash
# Test mirror speed
time curl -o /dev/null https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty

# Try different mirror
export CTAN_MIRROR=https://ctan.math.utah.edu
kgc latex build --input main.tex
```

**Solution**: Use faster mirror or offline bundle

---

## System Errors

### Error: Out of memory (WASM)

```
✗ Compilation failed
  Error: Abort(LinkError: WebAssembly.instantiate(): Out of memory)
```

**Cause**: Document too large for WASM heap

**Debug**:
```bash
# Check document size
find thesis/ -name "*.tex" -exec wc -l {} \; | awk '{sum+=$1} END {print sum " lines"}'

# Check VFS size
du -sh .latex-cache/
```

**Solutions**:

**1. Increase Node.js heap**:
```bash
export NODE_OPTIONS="--max-old-space-size=4096"  # 4 GB
kgc latex build --input main.tex
```

**2. Split document**:
```latex
% Instead of one huge file
% Split into smaller PDFs and combine later

% chapter1.tex (compile separately)
\documentclass{article}
\begin{document}
Chapter 1 content...
\end{document}

% Combine with pdfunite or similar
```

**3. Reduce graphics size**:
```bash
# Compress images
mogrify -resize 50% figures/*.png
optipng figures/*.png
```

---

### Error: Disk full

```
✗ Error: ENOSPC: no space left on device, write '.latex-cache/ctan/files/...'
```

**Cause**: Disk full (cache can't write)

**Debug**:
```bash
# Check disk usage
df -h .latex-cache/

# Check cache size
du -sh .latex-cache/ctan/
```

**Solution**: Clear cache or increase disk space:
```bash
# Clear old runs
rm -rf .latex-cache/runs/

# Clear CTAN cache (will re-download)
rm -rf .latex-cache/ctan/

# Or move cache to different disk
kgc latex build --input main.tex --cacheDir /mnt/large-disk/latex-cache
```

---

### Error: Permission denied

```
✗ Error: EACCES: permission denied, mkdir '.latex-cache'
```

**Cause**: No write permission in project directory

**Debug**:
```bash
# Check permissions
ls -ld .

# Check if directory is read-only
touch test.txt && rm test.txt || echo "Read-only!"
```

**Solution**: Use writable cache directory:
```bash
kgc latex build --input main.tex --cacheDir /tmp/latex-cache
```

---

## Debugging Strategies

### 1. Enable Verbose Mode

```bash
kgc latex build --input main.tex --verbose
```

**Output includes**:
- VFS file collection
- Engine stdout/stderr
- Package resolution attempts
- Compilation passes

---

### 2. Examine Diagnostic Log

```bash
# Find latest log
ls -lt .latex-cache/runs/*.log | head -1

# Read errors
grep "^!" .latex-cache/runs/20251227_103045_xetex.log

# Read warnings
grep "Warning" .latex-cache/runs/20251227_103045_xetex.log
```

---

### 3. Simplify Document

```latex
% Start with minimal document
\documentclass{article}
\begin{document}
Hello, world!
\end{document}

% Gradually add back content
% \input{preamble}  % Add one by one
% \input{chapters/intro}
```

**Binary search debugging**:
1. Remove half of document
2. Recompile
3. If works: error is in removed half
4. If fails: error is in remaining half
5. Repeat

---

### 4. Check LaTeX Locally (If Available)

```bash
# If you have TeX installed locally
pdflatex main.tex

# Compare with pipeline
kgc latex build --input main.tex

# If pdflatex works but pipeline doesn't:
# → Likely VFS/package issue
```

---

### 5. Inspect VFS Contents

```javascript
// Add to swiftlatex-engine.mjs (temporary)
console.log('VFS Contents:');
for (const [path, content] of vfs.entries()) {
  console.log(`  ${path} (${content.length} bytes)`);
}
```

**Check for**:
- Missing files
- Wrong paths (e.g., `work/intro.tex` vs `work/chapters/intro.tex`)
- Empty files (0 bytes)

---

## Error Message Reference

### LatexCompileError Structure

```javascript
class LatexCompileError extends Error {
  message: string;            // Human-readable error
  engine: string;             // 'xetex' | 'pdftex'
  inputTexPath: string;       // Input file path
  logFilePath: string;        // Diagnostic log path
  missingInputs: string[];    // Missing files detected
  exitCode?: string;          // Process exit code
}
```

**Example**:
```javascript
try {
  await compileLatexToPdf({ ... });
} catch (error) {
  if (error instanceof LatexCompileError) {
    console.error('Compilation failed:');
    console.error('  Engine:', error.engine);
    console.error('  Input:', error.inputTexPath);
    console.error('  Log:', error.logFilePath);
    console.error('  Missing files:', error.missingInputs);

    // Read log excerpt
    const log = await fs.readFile(error.logFilePath, 'utf-8');
    const errors = log.split('\n').filter(line => line.startsWith('!'));
    console.error('  LaTeX errors:');
    errors.forEach(line => console.error(`    ${line}`));
  }
}
```

---

## Common Pitfalls

### 1. Relative Paths in LaTeX

```latex
% Relative to LaTeX's working directory, not filesystem
% VFS working directory is /work/

% Wrong (assumes /chapters/ root directory)
\input{chapters/intro}

% Correct (relative to /work/)
\input{chapters/intro}  % → /work/chapters/intro.tex
```

### 2. Case-Sensitive Filenames

```latex
% Linux: case-sensitive
\input{Chapter1}  % Looks for Chapter1.tex (not chapter1.tex)

% Best practice: use lowercase
\input{chapter1}  % chapter1.tex
```

### 3. Missing File Extensions

```latex
% LaTeX tries multiple extensions
\input{intro}  % Tries intro.tex, intro.ltx

% But \includegraphics needs explicit extension
\includegraphics{logo}  % ✗ Won't find logo.png
\includegraphics{logo.png}  # ✓ Correct
```

### 4. Forgetting Multi-Pass Compilation

```latex
% First pass: cross-refs unknown
See Section~\ref{sec:intro}.  % Prints "??"

% Need --passes 2 for references to resolve
```

```bash
kgc latex build --input main.tex --passes 2
```

### 5. Package Version Conflicts

```latex
% Package loaded twice with different options
\usepackage[utf8]{inputenc}
\usepackage[latin1]{inputenc}  % ✗ Conflict!

% Solution: load once with all options
\usepackage[utf8]{inputenc}
```

---

## Getting Help

### 1. Read the Diagnostic Log

```bash
cat .latex-cache/runs/20251227_103045_xetex.log
```

**Look for**:
- Lines starting with `!` (errors)
- `Warning:` (warnings)
- `File not found:` (missing dependencies)

### 2. Search Documentation

- [Tutorial](../tutorials/first-pdf.md) - Basic usage
- [How-To Guides](../how-to/) - Specific tasks
- [Reference](../reference/) - API and CLI details

### 3. Check GitHub Issues

Search for similar errors:
- https://github.com/unrdf/unrdf/issues

### 4. Create Minimal Reproducible Example

```latex
% minimal.tex
\documentclass{article}
\begin{document}
% Minimal code that reproduces error
\end{document}
```

---

## Summary

**Most common errors**:
1. **File not found** (80%) → Check paths, VFS contents
2. **LaTeX syntax** (15%) → Read diagnostic log, fix LaTeX
3. **Network errors** (4%) → Use offline bundle, check connection
4. **System errors** (1%) → Increase heap, free disk space

**Debugging workflow**:
1. **Read error message** (structured LatexCompileError)
2. **Check diagnostic log** (`.latex-cache/runs/*.log`)
3. **Simplify document** (binary search for culprit)
4. **Enable verbose mode** (detailed pipeline output)
5. **Seek help** (docs, issues, minimal example)

**Next steps**:
- Learn about [Dependencies and Licenses](./dependencies.md)
- Understand [Caching Strategy](./caching.md)
- Read [Architecture Overview](./architecture.md)
