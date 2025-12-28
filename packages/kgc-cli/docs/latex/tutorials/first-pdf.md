# Tutorial: Your First PDF

**Learning goal**: Compile a LaTeX document to PDF in 5 minutes using the pure JavaScript pipeline.

**What you'll learn**:
- How to use the `kgc latex build` command
- What happens during compilation
- How to verify your PDF output
- Basic troubleshooting

**Prerequisites**:
- Node.js ≥18.0.0 installed
- `@unrdf/kgc-cli` package installed
- Basic familiarity with command line

## Step 1: Create a Simple LaTeX Document

Create a new directory for your first LaTeX project:

```bash
mkdir my-first-latex
cd my-first-latex
```

Create a file named `main.tex` with this content:

```latex
\documentclass{article}

\title{My First PDF}
\author{Your Name}
\date{\today}

\begin{document}

\maketitle

\section{Introduction}

This is my first LaTeX document compiled using the pure JavaScript pipeline!

\subsection{What I'm Learning}

I'm learning how to:
\begin{itemize}
  \item Compile LaTeX without installing TeX
  \item Use the kgc CLI tool
  \item Generate professional PDFs
\end{itemize}

\section{Mathematics}

The quadratic formula is:
\[
x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}
\]

\section{Conclusion}

This was easy! No TeX installation required.

\end{document}
```

Save the file.

## Step 2: Compile to PDF

Run the compilation command:

```bash
kgc latex build --input main.tex --output my-first.pdf
```

**What you should see**:

```
✓ Collecting project files...
✓ Initializing TeX engine...
✓ Running compilation (pass 1/2)...
✓ Running compilation (pass 2/2)...
✓ PDF generated: my-first.pdf (8.2 KB)
```

**What just happened**:
1. The pipeline collected `main.tex` into a virtual file system
2. A WebAssembly TeX engine (XeTeX) was initialized
3. Two compilation passes ran to resolve cross-references
4. A PDF was written to `my-first.pdf`

## Step 3: Verify Your PDF

Check that the PDF was created:

```bash
ls -lh my-first.pdf
```

Expected output:

```
-rw-r--r-- 1 user user 8.2K Dec 27 10:30 my-first.pdf
```

Open the PDF in your viewer:

```bash
# macOS
open my-first.pdf

# Linux
xdg-open my-first.pdf

# Windows
start my-first.pdf
```

You should see a professionally formatted document with:
- Title, author, and date
- Section headers
- Bullet list
- Mathematical equation
- Proper margins and typography

## Step 4: Explore the Cache

The compilation created a cache directory:

```bash
ls -la .latex-cache/
```

You should see:

```
.latex-cache/
├── latex.lock.json          # Lockfile for reproducible builds
└── runs/
    └── 20251227_103045_xetex.log  # Compilation log
```

**Try this**: Look at the lockfile:

```bash
cat .latex-cache/latex.lock.json
```

Output:

```json
{
  "createdAt": "2025-12-27T10:30:45.123Z",
  "engine": "xetex",
  "resolvedInputs": {},
  "updatedAt": "2025-12-27T10:30:45.456Z",
  "version": "1.0.0"
}
```

The lockfile tracks all resolved dependencies. Since our document used only built-in packages, `resolvedInputs` is empty.

## Step 5: Try Different Engines

The pipeline supports two engines:

### XeTeX (default)
Better Unicode support, modern fonts:

```bash
kgc latex build --input main.tex --engine xelatex --output xelatex-output.pdf
```

### PDFLaTeX
Faster for simple documents:

```bash
kgc latex build --input main.tex --engine pdflatex --output pdflatex-output.pdf
```

Compare the file sizes:

```bash
ls -lh *.pdf
```

## Step 6: Add a Missing Package

Let's see automatic dependency resolution in action. Edit `main.tex` to add a new package:

```latex
\documentclass{article}
\usepackage{algorithm2e}  % ← Add this line

\title{My First PDF}
% ... rest of the file unchanged
```

Add an algorithm before `\end{document}`:

```latex
\section{Algorithms}

\begin{algorithm}[H]
  \SetAlgoLined
  \KwData{An integer $n$}
  \KwResult{The factorial $n!$}
  $result \gets 1$\;
  \For{$i \gets 2$ \KwTo $n$}{
    $result \gets result \times i$\;
  }
  \Return{result}\;
  \caption{Factorial Computation}
\end{algorithm}
\end{document}
```

Compile again:

```bash
kgc latex build --input main.tex --output with-algorithm.pdf
```

**What you should see**:

```
✓ Collecting project files...
✓ Initializing TeX engine...
✓ Running compilation (pass 1/2)...
⚠ Missing input detected: algorithm2e.sty
✓ Fetching algorithm2e.sty from CTAN...
✓ Cached: algorithm2e.sty (45.2 KB)
✓ Running compilation (pass 2/2)...
✓ PDF generated: with-algorithm.pdf (12.4 KB)
```

Check the lockfile again:

```bash
cat .latex-cache/latex.lock.json
```

Now you'll see:

```json
{
  "createdAt": "2025-12-27T10:30:45.123Z",
  "engine": "xetex",
  "resolvedInputs": {
    "algorithm2e.sty": {
      "cachedPath": "texmf/tex/latex/algorithm2e/algorithm2e.sty",
      "hash": "a1b2c3d4e5f6...",
      "resolvedAt": "2025-12-27T10:35:12.789Z",
      "sourceUrl": "https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty"
    }
  },
  "updatedAt": "2025-12-27T10:35:12.789Z",
  "version": "1.0.0"
}
```

The package was automatically:
1. Detected as missing from the compilation log
2. Fetched from CTAN
3. Cached locally with a content hash
4. Recorded in the lockfile
5. Used in a retry compilation

## Troubleshooting

### Error: "Input file not found"

```bash
kgc latex build --input main.tex
# Error: Input file not found: main.tex
```

**Solution**: Ensure you're in the correct directory and the file exists:

```bash
pwd
ls main.tex
```

### Error: "LaTeX compilation failed"

Check the diagnostic log:

```bash
cat .latex-cache/runs/*.log
```

Look for lines starting with `!` (LaTeX errors).

### Error: "Failed to fetch from CTAN"

This usually means:
1. You're offline (see [Offline Bundle Guide](../how-to/offline-bundle.md))
2. Package name is misspelled
3. Package doesn't exist on CTAN

## What You Learned

- ✓ How to compile LaTeX to PDF without TeX installed
- ✓ The `kgc latex build` command syntax
- ✓ How the cache and lockfile work
- ✓ Automatic dependency resolution from CTAN
- ✓ Basic troubleshooting

## Next Steps

- **Add more content**: Try multi-file projects with `\input{chapter1.tex}`
- **Deterministic builds**: Read [Deterministic Builds Guide](../how-to/deterministic-builds.md)
- **Offline mode**: Set up an [Offline Bundle](../how-to/offline-bundle.md)
- **Understand the pipeline**: Read [Architecture Explanation](../explanation/architecture.md)
- **Use the API**: Check the [JavaScript API Reference](../reference/api.md)

## Summary

You've successfully compiled your first LaTeX document using a pure JavaScript pipeline! The system:
- Runs entirely in Node.js (no external TeX required)
- Automatically fetches missing packages
- Creates lockfiles for reproducibility
- Provides detailed diagnostic logs

**Time to completion**: ~5 minutes ✓
