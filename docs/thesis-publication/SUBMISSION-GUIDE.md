# Submission Guide - Format Requirements by Venue

**Date**: December 25, 2025
**Purpose**: Detailed format requirements for each target publication venue

---

## arXiv Submission Requirements

### General arXiv Requirements (All Theses)

| Requirement | Specification | Notes |
|-------------|---------------|-------|
| Format | LaTeX (preferred) or PDF | LaTeX enables better accessibility |
| PDF Version | PDF/A-1b or PDF 1.5 | Use `\usepackage[a-1b]{pdfx}` |
| File Size | <50MB compressed | Large figures may need compression |
| Page Limit | None (soft limit ~50 pages) | Longer papers may face scrutiny |
| Figures | EPS, PDF, PNG, or JPEG | Vector preferred for diagrams |
| Bibliography | BibTeX (.bib) | Include .bbl file |
| License | CC BY 4.0 recommended | arXiv default license acceptable |

### arXiv Categories by Thesis

| Thesis | Primary Category | Cross-list |
|--------|------------------|------------|
| KGC Field Theory | cs.DB | cs.AI, cs.LO |
| Knowledge Hooks PhD | cs.PL | cs.AI, cs.SE |
| KGC 4D Blue Ocean | cs.DB | cs.SE, cs.DC |
| Big Bang 80/20 | cs.SE | cs.PL |

### arXiv Submission Checklist

```bash
# 1. Create submission archive
tar -czvf submission.tar.gz \
    thesis.tex \
    *.bib \
    *.bbl \
    figures/ \
    preamble.sty

# 2. Verify LaTeX compiles
pdflatex thesis.tex && bibtex thesis && pdflatex thesis.tex

# 3. Submit via https://arxiv.org/submit
```

---

## ACM Format Requirements

### Applicable Conferences
- **SIGMOD** (databases) - Thesis 3 (KGC 4D)
- **SIGSOFT/FSE** (software engineering) - Thesis 4 (Big Bang 80/20)
- **PLDI** (programming languages) - Thesis 2 (Knowledge Hooks)

### ACM Format Specifications

| Requirement | Specification |
|-------------|---------------|
| Template | `acmart` class |
| Format | `sigconf` or `sigplan` |
| Columns | Two-column (sigconf) |
| Page Limit | 12 pages + references (SIGMOD), 10 pages (FSE) |
| Font | Libertine or Times New Roman |
| Anonymous | Yes for initial submission |

### ACM LaTeX Template

```latex
\documentclass[sigconf,anonymous,review]{acmart}
\usepackage{booktabs}

% ACM specific
\copyrightyear{2026}
\acmYear{2026}
\setcopyright{acmlicensed}
\acmConference[SIGMOD '26]{International Conference on Management of Data}{June 2026}{Santiago, Chile}
\acmBooktitle{Proceedings of SIGMOD '26}
\acmDOI{10.1145/nnnnnnn.nnnnnnn}
\acmISBN{978-1-4503-XXXX-X/XX/XX}

\begin{document}
\title{KGC 4D: A Blue Ocean Strategy in Temporal Data Management}
% ... content
\end{document}
```

### ACM Submission Deadlines (2026)

| Conference | Submission | Notification | Camera-Ready |
|------------|------------|--------------|--------------|
| SIGMOD '26 | Nov 2025 | Feb 2026 | Mar 2026 |
| FSE '26 | Mar 2026 | Jun 2026 | Jul 2026 |
| PLDI '26 | Nov 2025 | Feb 2026 | Mar 2026 |

---

## IEEE Format Requirements

### Applicable Conferences
- **ICSE** (software engineering) - Thesis 4 (Big Bang 80/20)
- **IEEE S&P** (security) - Cryptographic receipts (from Thesis 1)
- **ICDCS** (distributed computing) - Thesis 3 (4D temporal)

### IEEE Format Specifications

| Requirement | Specification |
|-------------|---------------|
| Template | `IEEEtran` class |
| Format | `conference` option |
| Columns | Two-column |
| Page Limit | 10-12 pages (varies by conference) |
| Font | Times New Roman, 10pt |
| Anonymous | Conference-dependent |

### IEEE LaTeX Template

```latex
\documentclass[conference]{IEEEtran}
\usepackage{cite}
\usepackage{amsmath,amssymb,amsfonts}
\usepackage{algorithmic}
\usepackage{graphicx}
\usepackage{textcomp}
\usepackage{xcolor}

\begin{document}
\title{Big Bang 80/20: Pareto-Optimized Single-Pass Development}
\author{
\IEEEauthorblockN{Author Name}
\IEEEauthorblockA{Institution\\
email@institution.edu}
}
\maketitle
% ... content
\end{document}
```

### IEEE Submission Deadlines (2026)

| Conference | Submission | Notification | Camera-Ready |
|------------|------------|--------------|--------------|
| ICSE '26 | Sep 2025 | Dec 2025 | Jan 2026 |
| IEEE S&P '26 | Apr 2025 | Aug 2025 | Oct 2025 |
| ICDCS '26 | Jan 2026 | Apr 2026 | May 2026 |

---

## Springer LNCS Format Requirements

### Applicable Venues
- **BPM** (Business Process Management) - Thesis 2 (SPARQL control flow)
- **ICWS** (Web Services) - Thesis 3 (temporal events)
- **CAiSE** (Information Systems) - All theses

### LNCS Format Specifications

| Requirement | Specification |
|-------------|---------------|
| Template | `llncs` class |
| Columns | Single-column |
| Page Limit | 15-20 pages |
| Font | Times/Computer Modern, 10pt |
| Figures | In text, not at end |
| References | Numbered style |

### LNCS LaTeX Template

```latex
\documentclass[runningheads]{llncs}
\usepackage{graphicx}
\usepackage{amsmath}

\begin{document}
\title{The $\mu(O)$ Calculus with Hyperdimensional Semantics}
\author{Author Name\inst{1}}
\institute{Institution \email{author@inst.edu}}
\maketitle

\begin{abstract}
This paper presents...
\end{abstract}

\keywords{Knowledge Calculus \and Information Theory \and Semantic Spaces}
% ... content
\end{document}
```

### Springer Submission Deadlines (2026)

| Conference | Submission | Notification | Camera-Ready |
|------------|------------|--------------|--------------|
| BPM '26 | Mar 2026 | May 2026 | Jun 2026 |
| ICWS '26 | Mar 2026 | May 2026 | Jun 2026 |
| CAiSE '26 | Nov 2025 | Feb 2026 | Mar 2026 |

---

## VLDB Format Requirements

### Applicable Theses
- Thesis 1 (KGC Field Theory)
- Thesis 3 (KGC 4D Blue Ocean)

### VLDB Format Specifications

| Requirement | Specification |
|-------------|---------------|
| Template | `vldb` style |
| Columns | Two-column |
| Page Limit | 12 pages + unlimited references |
| Font | Times, 9pt body |
| Anonymous | Double-blind |
| Reproducibility | Encouraged (badges available) |

### VLDB LaTeX Template

```latex
\documentclass{vldb}
\usepackage{graphicx}
\usepackage{balance}

\begin{document}
\title{Field-Theoretic Knowledge Representation with Cryptographic Receipts}

\numberofauthors{1}
\author{
\alignauthor Anonymous
}

\maketitle
\begin{abstract}
% 150 words max
\end{abstract}

\section{Introduction}
% ... content
\end{document}
```

### VLDB Submission Schedule (2026)

| Round | Submission | Notification |
|-------|------------|--------------|
| Round 1 | Feb 1, 2026 | Apr 2026 |
| Round 2 | Jun 1, 2026 | Aug 2026 |
| Round 3 | Oct 1, 2026 | Dec 2026 |

---

## Format Conversion Commands

### mdBook to LaTeX (Thesis 1)

```bash
# Using pandoc for conversion
pandoc -s books/kgc-thesis/src/*.md \
    -o kgc-thesis.tex \
    --template=ieee \
    --bibliography=refs.bib

# Manual cleanup required for mathematical notation
```

### LaTeX to PDF/A (arXiv compliance)

```bash
# Add to preamble
\usepackage[a-1b]{pdfx}

# Or use ghostscript post-processing
gs -dPDFA -dBATCH -dNOPAUSE -sColorConversionStrategy=UseDeviceIndependentColor \
   -sDEVICE=pdfwrite -dPDFACompatibilityPolicy=1 \
   -sOutputFile=thesis-pdfa.pdf thesis.pdf
```

### Single-Column to Two-Column

For converting between LNCS (single) and ACM/IEEE (two-column):
1. Resize figures (max width 3.5in for two-column)
2. Reflow tables
3. Adjust algorithm floats
4. Check page limits

---

## Venue-Specific Author Guidelines URLs

| Venue | Author Guidelines |
|-------|-------------------|
| arXiv | https://arxiv.org/help/submit |
| SIGMOD | https://sigmod.org/call-for-contributions/ |
| ICSE | https://conf.researchr.org/track/icse-2026/ |
| IEEE S&P | https://sp2026.ieee-security.org/ |
| VLDB | https://vldb.org/submit |
| BPM | https://bpm-conference.org/ |
| FSE | https://conf.researchr.org/home/fse-2026 |

---

## Summary: Thesis-to-Venue Format Mapping

| Thesis | Primary Venue | Format | Page Limit |
|--------|---------------|--------|------------|
| KGC Field Theory | arXiv + VLDB | VLDB | 12 |
| Knowledge Hooks PhD | arXiv + BPM | LNCS | 15 |
| KGC 4D Blue Ocean | arXiv + SIGMOD | ACM sigconf | 12 |
| Big Bang 80/20 | arXiv + ICSE | IEEE | 10 |
