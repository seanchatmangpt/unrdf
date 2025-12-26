# UNRDF Thesis Suite - Build and Publication Guide

**Date**: December 25, 2025
**Version**: 1.0.0
**Status**: Pre-Submission

---

## Overview

This directory contains the complete publication roadmap for the UNRDF thesis suite, consisting of 4 academic works ready for arXiv and conference submission.

### Thesis Inventory

| # | Title | Format | Location | Pages |
|---|-------|--------|----------|-------|
| 1 | KGC Field-Theoretic Knowledge Representation | mdBook | `/books/kgc-thesis/` | 25 HTML |
| 2 | Knowledge Hooks PhD - mu(O) Calculus | LaTeX | `/packages/hooks/docs/thesis/` | ~25 |
| 3 | KGC 4D Blue Ocean Strategy | LaTeX | `/packages/kgc-4d/docs/4d-blue-ocean/` | 27 |
| 4 | Big Bang 80/20 Methodology | LaTeX | `/packages/kgc-4d/docs/explanation/` | ~35 |

---

## Prerequisites

### System Requirements

```bash
# TeX Live (2024 or later)
sudo apt-get install texlive-full

# Or on macOS
brew install --cask mactex

# mdBook for Thesis 1
cargo install mdbook

# Verification
pdflatex --version  # TeX Live 2024+
mdbook --version    # 0.4.35+
```

### Required LaTeX Packages

All theses use these core packages (included in texlive-full):
- `amsmath`, `amssymb`, `amsthm` - Mathematics
- `graphicx`, `tikz`, `pgfplots` - Graphics
- `hyperref`, `cleveref` - Cross-references
- `natbib` - Bibliography
- `booktabs`, `longtable` - Tables
- `listings`, `algorithm` - Code

---

## Build Instructions

### Thesis 1: KGC Field-Theoretic Knowledge Representation

```bash
# Navigate to thesis directory
cd /home/user/unrdf/books/kgc-thesis

# Build HTML version
mdbook build

# Output location
ls -la book/index.html

# Serve locally for preview
mdbook serve --open
# Opens http://localhost:3000
```

**Expected Output**:
- 25 HTML pages in `book/` directory
- Build time: <2 seconds
- Index size: ~36KB

**Converting to PDF (for arXiv)**:
```bash
# Install wkhtmltopdf for HTML-to-PDF
sudo apt-get install wkhtmltopdf

# Generate PDF from HTML
wkhtmltopdf --enable-local-file-access \
    book/index.html \
    kgc-thesis.pdf

# Or use pandoc for direct MD-to-LaTeX
pandoc -s src/*.md \
    -o kgc-thesis.tex \
    --template=article \
    --bibliography=refs.bib
```

---

### Thesis 2: Knowledge Hooks PhD

```bash
# Navigate to thesis directory
cd /home/user/unrdf/packages/hooks/docs/thesis

# First compile (generates .aux)
pdflatex knowledge-hooks-phd-thesis.tex

# Process bibliography
bibtex knowledge-hooks-phd-thesis

# Second compile (resolves references)
pdflatex knowledge-hooks-phd-thesis.tex

# Third compile (finalizes TOC, etc.)
pdflatex knowledge-hooks-phd-thesis.tex

# Output location
ls -la knowledge-hooks-phd-thesis.pdf
```

**Expected Output**:
- PDF: ~400KB
- Pages: ~25
- Build time: ~10 seconds

**One-liner Build**:
```bash
latexmk -pdf knowledge-hooks-phd-thesis.tex
```

---

### Thesis 3: KGC 4D Blue Ocean Strategy

```bash
# Navigate to thesis directory
cd /home/user/unrdf/packages/kgc-4d/docs/4d-blue-ocean

# Standard LaTeX build
pdflatex thesis.tex
pdflatex thesis.tex  # Run twice for references

# Output location
ls -la thesis.pdf
```

**Expected Output**:
- PDF: ~210KB (verified)
- Pages: 27 (verified)
- Build time: ~8 seconds

**Verification**:
```bash
# Check page count
pdfinfo thesis.pdf | grep Pages
# Expected: Pages: 27

# Check file size
ls -lh thesis.pdf
# Expected: ~210K
```

---

### Thesis 4: Big Bang 80/20 Methodology

```bash
# Navigate to thesis directory
cd /home/user/unrdf/packages/kgc-4d/docs/explanation

# Standard LaTeX build
pdflatex thesis-bigbang-80-20.tex
bibtex thesis-bigbang-80-20
pdflatex thesis-bigbang-80-20.tex
pdflatex thesis-bigbang-80-20.tex

# Output location
ls -la thesis-bigbang-80-20.pdf
```

**Expected Output**:
- PDF: ~500KB
- Pages: ~35
- Build time: ~12 seconds

---

## Build All Theses (Script)

Create this script at `/home/user/unrdf/scripts/build-all-theses.sh`:

```bash
#!/bin/bash
set -e

REPO_ROOT="/home/user/unrdf"

echo "=== Building UNRDF Thesis Suite ==="

# Thesis 1: mdBook
echo "[1/4] Building KGC Field-Theoretic thesis..."
cd "$REPO_ROOT/books/kgc-thesis"
mdbook build
echo "  -> Output: book/index.html"

# Thesis 2: LaTeX
echo "[2/4] Building Knowledge Hooks PhD thesis..."
cd "$REPO_ROOT/packages/hooks/docs/thesis"
latexmk -pdf -interaction=nonstopmode knowledge-hooks-phd-thesis.tex
echo "  -> Output: knowledge-hooks-phd-thesis.pdf"

# Thesis 3: LaTeX
echo "[3/4] Building KGC 4D Blue Ocean thesis..."
cd "$REPO_ROOT/packages/kgc-4d/docs/4d-blue-ocean"
latexmk -pdf -interaction=nonstopmode thesis.tex
echo "  -> Output: thesis.pdf"

# Thesis 4: LaTeX
echo "[4/4] Building Big Bang 80/20 thesis..."
cd "$REPO_ROOT/packages/kgc-4d/docs/explanation"
latexmk -pdf -interaction=nonstopmode thesis-bigbang-80-20.tex
echo "  -> Output: thesis-bigbang-80-20.pdf"

echo ""
echo "=== Build Complete ==="
echo "All 4 theses built successfully."
```

**Run**:
```bash
chmod +x /home/user/unrdf/scripts/build-all-theses.sh
/home/user/unrdf/scripts/build-all-theses.sh
```

---

## arXiv Submission Packages

### Creating arXiv-Ready Archives

```bash
# Thesis 2 example
cd /home/user/unrdf/packages/hooks/docs/thesis

# Create submission archive
tar -czvf arxiv-knowledge-hooks.tar.gz \
    knowledge-hooks-phd-thesis.tex \
    knowledge-hooks-phd-thesis.bbl \
    figures/*.pdf \
    figures/*.png

# Verify archive
tar -tzvf arxiv-knowledge-hooks.tar.gz
```

### PDF/A Compliance (arXiv Requirement)

```bash
# Add to LaTeX preamble
# \usepackage[a-1b]{pdfx}

# Or post-process with ghostscript
gs -dPDFA -dBATCH -dNOPAUSE \
   -sColorConversionStrategy=UseDeviceIndependentColor \
   -sDEVICE=pdfwrite \
   -dPDFACompatibilityPolicy=1 \
   -sOutputFile=thesis-pdfa.pdf \
   thesis.pdf
```

---

## Format Conversion

### mdBook to LaTeX (Thesis 1)

```bash
cd /home/user/unrdf/books/kgc-thesis/src

# Combine all markdown files
cat *.md > combined.md

# Convert to LaTeX
pandoc combined.md \
    -o ../kgc-thesis.tex \
    --template=default \
    --pdf-engine=pdflatex

# Manual cleanup required for:
# - Mathematical notation (MathJax -> LaTeX)
# - Cross-references
# - Bibliography
```

### LaTeX to ACM Format

```bash
# Replace document class
sed -i 's/\\documentclass\[.*\]{report}/\\documentclass[sigconf,anonymous]{acmart}/' thesis.tex

# Add ACM-specific commands
# (manual editing required)
```

### LaTeX to IEEE Format

```bash
# Replace document class
sed -i 's/\\documentclass\[.*\]{report}/\\documentclass[conference]{IEEEtran}/' thesis.tex

# Convert bibliography
# natbib -> cite package
```

---

## Verification Checklist

Run these commands before submission:

```bash
# 1. Check for LaTeX errors
pdflatex thesis.tex 2>&1 | grep -E "(Error|Warning)"
# Expected: 0 errors, minimal warnings

# 2. Check for missing references
grep "undefined" thesis.log
# Expected: no results

# 3. Check for overfull boxes
grep "Overfull" thesis.log | wc -l
# Expected: <5 minor overfulls

# 4. Validate PDF
pdfinfo thesis.pdf
# Check Producer, Pages, PDF version

# 5. Check page count
pdfinfo thesis.pdf | grep Pages
# Verify against venue limit

# 6. Spell check
aspell -c thesis.tex
```

---

## Troubleshooting

### Common Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| Missing .sty file | Package not installed | `tlmgr install <package>` |
| Bibliography empty | bibtex not run | Run `bibtex thesis` |
| Figures not found | Wrong path | Use absolute paths |
| PDF too large | High-res images | Compress with `gs` |
| Font not found | Missing font | Install font package |

### LaTeX Emergency Debugging

```bash
# Clean auxiliary files
rm -f *.aux *.log *.bbl *.blg *.toc *.lof *.lot *.out

# Rebuild from scratch
latexmk -pdf -f thesis.tex

# Generate detailed log
pdflatex -interaction=nonstopmode thesis.tex > build.log 2>&1
```

---

## Directory Structure

```
/home/user/unrdf/
├── books/
│   └── kgc-thesis/                    # Thesis 1
│       ├── book.toml                  # mdBook config
│       ├── src/                       # Markdown sources
│       │   ├── SUMMARY.md             # Table of contents
│       │   ├── 01-abstract.md
│       │   ├── chapter-01/
│       │   └── ...
│       └── book/                      # Generated HTML
│
├── packages/
│   ├── hooks/docs/thesis/             # Thesis 2
│   │   ├── knowledge-hooks-phd-thesis.tex
│   │   └── knowledge-hooks-phd-thesis.pdf
│   │
│   └── kgc-4d/docs/
│       ├── 4d-blue-ocean/             # Thesis 3
│       │   ├── thesis.tex
│       │   └── thesis.pdf
│       │
│       └── explanation/               # Thesis 4
│           └── thesis-bigbang-80-20.tex
│
└── docs/thesis-publication/           # This directory
    ├── README-FINAL.md                # This file
    ├── FINAL-INTEGRATION-CHECKLIST.md
    ├── SUBMISSION-GUIDE.md
    ├── PUBLICATION-TIMELINE.md
    └── CONFERENCE-TARGETING.md
```

---

## Quick Reference

### Build Commands Summary

```bash
# Thesis 1 (mdBook)
cd /home/user/unrdf/books/kgc-thesis && mdbook build

# Thesis 2 (LaTeX)
cd /home/user/unrdf/packages/hooks/docs/thesis && latexmk -pdf knowledge-hooks-phd-thesis.tex

# Thesis 3 (LaTeX)
cd /home/user/unrdf/packages/kgc-4d/docs/4d-blue-ocean && latexmk -pdf thesis.tex

# Thesis 4 (LaTeX)
cd /home/user/unrdf/packages/kgc-4d/docs/explanation && latexmk -pdf thesis-bigbang-80-20.tex
```

### Output Locations

| Thesis | PDF/HTML Location |
|--------|-------------------|
| 1 | `/home/user/unrdf/books/kgc-thesis/book/index.html` |
| 2 | `/home/user/unrdf/packages/hooks/docs/thesis/knowledge-hooks-phd-thesis.pdf` |
| 3 | `/home/user/unrdf/packages/kgc-4d/docs/4d-blue-ocean/thesis.pdf` |
| 4 | `/home/user/unrdf/packages/kgc-4d/docs/explanation/thesis-bigbang-80-20.pdf` |

---

## Support Documents

| Document | Purpose |
|----------|---------|
| `FINAL-INTEGRATION-CHECKLIST.md` | Track completion status |
| `SUBMISSION-GUIDE.md` | Venue format requirements |
| `PUBLICATION-TIMELINE.md` | 10-week schedule |
| `CONFERENCE-TARGETING.md` | Paper-to-venue matching |

---

## Next Steps

1. **Immediate**: Run build script to verify all theses compile
2. **Week 1-2**: Complete integration checklist items
3. **Week 3-4**: Submit for external review
4. **Week 5-8**: Incorporate feedback and polish
5. **Week 9**: Submit to arXiv
6. **Week 10+**: Submit to conferences

---

**Contact**: unrdf@research.org
**Repository**: https://github.com/gitvan/unrdf
