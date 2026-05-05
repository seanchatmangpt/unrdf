# UNRDF Papers Policy

**Version**: latest
**Last Updated**: 2024-12-06
**Status**: Active

## 🎯 Purpose

Define the canonical workflow for publishing 25+ academic papers on the UNRDF GitHub Pages site using Nextra with KaTeX support.

---

## 📋 Table of Contents

1. [Directory Structure](#directory-structure)
2. [Naming Conventions](#naming-conventions)
3. [Paper Metadata Schema](#paper-metadata-schema)
4. [Source of Truth](#source-of-truth)
5. [Workflow](#workflow)
6. [LaTeX→MDX Conversion](#latexmdx-conversion)
7. [Citation Management](#citation-management)
8. [Quality Gates](#quality-gates)
9. [Deployment](#deployment)

---

## 1. Directory Structure

```
packages/nextra/
├── app/
│   └── papers/
│       ├── page.mdx                              # Papers index/hub
│       ├── _meta.ts                              # Papers navigation
│       ├── 2024-mu-calculus-foundations/
│       │   ├── page.mdx                          # Paper content (CANONICAL)
│       │   ├── _meta.ts                          # Subsection nav (if needed)
│       │   └── figures/                          # Paper-specific assets
│       │       ├── state-diagram.svg
│       │       └── benchmark-results.png
│       ├── 2024-hooks-architecture/
│       │   └── page.mdx
│       ├── 2025-kgc-4d-implementation/
│       │   └── page.mdx
│       └── ... (25+ papers)
│
├── public/
│   └── papers/
│       ├── pdfs/                                 # Generated PDFs (optional)
│       │   ├── 2024-mu-calculus-foundations.pdf
│       │   └── ...
│       └── bibtex/                               # BibTeX citations
│           └── unrdf-papers.bib
│
└── content/                                      # Optional archival
    └── papers/
        └── latex-source/                         # Original LaTeX (if preserved)
            ├── 2024-mu-calculus-foundations.tex
            └── ...
```

**Key Principles**:
- **MDX files are canonical** - the source of truth for published papers
- **One directory per paper** - keeps assets scoped
- **Year-prefixed slugs** - chronological browsing + prevents collisions
- **LaTeX source is optional** - keep if you want archival, discard if pure MDX workflow

---

## 2. Naming Conventions

### Directory Names

**Format**: `YYYY-kebab-case-title/`

**Rules**:
- Start with 4-digit year (publication/submission date)
- Use kebab-case (lowercase, hyphens)
- Max 60 characters
- Descriptive but concise
- No special characters except hyphens

**Examples**:
```
✅ 2024-mu-calculus-foundations/
✅ 2024-hooks-architecture-patterns/
✅ 2025-kgc-4d-implementation-proof/
❌ MuCalculus/                        # No year prefix
❌ 2024_mu_calculus/                  # Use hyphens, not underscores
❌ 2024-μ-calculus/                   # No unicode in slugs
```

### File Names

- Paper content: `page.mdx` (Nextra convention)
- Navigation: `_meta.ts` (Nextra convention)
- Figures: `descriptive-name.{svg,png,jpg,pdf}`

---

## 3. Paper Metadata Schema

Every `page.mdx` MUST start with front matter:

```yaml
---
title: "Full Paper Title: Subtitle if Applicable"
authors:
  - Sean Chatman
  - Collaborator Name (optional)
date: "2024-12-15"                    # Publication/submission date (ISO 8601)
updated: "2024-12-20"                 # Last updated (ISO 8601)
status: published                     # draft | review | published | archived
abstract: |
  Brief summary (150-250 words). Supports **markdown**.

  Multiple paragraphs allowed.
keywords:
  - RDF
  - Knowledge Graphs
  - μ(O) Calculus
  - Semantic Web
doi: "10.xxxxx/xxxxx"                 # Optional: if published with DOI
arxiv: "2412.xxxxx"                   # Optional: ArXiv ID
github: "https://github.com/..."      # Optional: Code repository
citation: |
  Chatman, S. (2024). μ(O) Calculus: Foundations of Observable State.
  UNRDF Technical Report. https://seanchatmangpt.github.io/unrdf/papers/2024-mu-calculus-foundations
---

# {title}

<div className="text-sm text-gray-600 dark:text-gray-400 mb-8">
  **Authors**: {authors.join(', ')}
  **Published**: {date} • **Updated**: {updated}
  **Status**: {status}
</div>

## Abstract

{abstract}

## 1. Introduction

...
```

**Field Descriptions**:

| Field | Required | Type | Description |
|-------|----------|------|-------------|
| `title` | ✅ | string | Full paper title |
| `authors` | ✅ | string[] | Author names (order matters) |
| `date` | ✅ | ISO 8601 | Original publication/submission date |
| `updated` | ❌ | ISO 8601 | Last revision date (auto-update on edits) |
| `status` | ✅ | enum | `draft` \| `review` \| `published` \| `archived` |
| `abstract` | ✅ | markdown | 150-250 word summary |
| `keywords` | ✅ | string[] | 3-8 keywords for discoverability |
| `doi` | ❌ | string | Digital Object Identifier (if published) |
| `arxiv` | ❌ | string | ArXiv preprint ID |
| `github` | ❌ | URL | Associated code repository |
| `citation` | ✅ | markdown | Recommended citation format |

---

## 4. Source of Truth

**Canonical Format**: **MDX files** in `/packages/nextra/app/papers/[slug]/page.mdx`

**Why MDX, not LaTeX?**

| Aspect | LaTeX | MDX (Chosen) |
|--------|-------|--------------|
| **Web rendering** | Requires conversion | Native |
| **Math support** | Excellent | Excellent (KaTeX) |
| **Version control** | Good | Excellent (readable diffs) |
| **Collaboration** | Editor-dependent | GitHub web editor works |
| **CI/CD** | Complex (pdflatex) | Simple (Next.js build) |
| **Interactivity** | Static PDF | React components possible |
| **Search** | PDF text | Native Nextra search |
| **Maintenance** | Two sources (LaTeX + PDF) | One source (MDX) |

**LaTeX Source Retention** (optional):

- **Option A** (Recommended): Discard LaTeX after conversion
  - Rationale: MDX is canonical, LaTeX becomes stale
  - Edit directly in MDX going forward

- **Option B**: Archive LaTeX in `/content/papers/latex-source/`
  - Rationale: Preserve original for journal submission
  - **WARNING**: Two sources = synchronization burden
  - Only use if you need `.tex` files for external submission

**Default Policy**: **Option A** (MDX only, no LaTeX archival)

---

## 5. Workflow

### latest New Paper (LaTeX Source)

**Typical academic workflow**: Write in LaTeX, convert to MDX for web.

```bash
# 1. Write paper in LaTeX
vim my-paper.tex

# 2. Convert to MDX
pnpm convert-paper my-paper.tex \
  --slug 2024-my-paper-title \
  --authors "Sean Chatman" \
  --status draft

# 3. Review generated MDX
code packages/nextra/app/papers/2024-my-paper-title/page.mdx

# 4. Edit/refine (fix formatting, add React components, etc.)
# 5. Preview locally
pnpm -C packages/nextra dev  # → http://localhost:3003/papers/2024-my-paper-title

# 6. Commit
git add packages/nextra/app/papers/2024-my-paper-title/
git commit -m "docs(papers): Add 2024 paper on X"
git push origin claude/setup-mdx-nextra-01CfrPWT4YzhTucMUz3cyPVJ

# 7. Deploy (automatic via GitHub Actions)
```

### latest New Paper (Direct MDX)

**For web-native papers** (no LaTeX source needed):

```bash
# 1. Create directory
mkdir -p packages/nextra/app/papers/2024-my-paper-title

# 2. Copy template
cp packages/nextra/app/papers/_templates/paper-template.mdx \
   packages/nextra/app/papers/2024-my-paper-title/page.mdx

# 3. Write paper in MDX
code packages/nextra/app/papers/2024-my-paper-title/page.mdx

# 4. Update _meta.ts
code packages/nextra/app/papers/_meta.ts

# 5. Preview, commit, push (same as latest steps 5-7)
```

### latest Updating Existing Paper

```bash
# 1. Edit MDX directly
code packages/nextra/app/papers/2024-existing-paper/page.mdx

# 2. Update `updated:` field in front matter
# 3. Preview, commit, push
```

### latest Paper Status Lifecycle

```
draft → review → published → [archived]
  ↓       ↓         ↓            ↓
  🚧      👀        ✅           📦
```

**Status Definitions**:
- `draft`: Work in progress, not ready for public
- `review`: Ready for peer review, seeking feedback
- `published`: Final version, publicly citable
- `archived`: Superseded by newer work, kept for historical reference

**Visibility**:
- `draft`: Hidden from papers index (unless `?show-drafts=true`)
- `review`: Shown with "PREPRINT" badge
- `published`: Full visibility
- `archived`: Shown with "ARCHIVED" notice

---

## 6. LaTeX→MDX Conversion

### latest Automated Conversion

**Tool**: `/scripts/convert-paper.mjs`

**Usage**:
```bash
pnpm convert-paper <input.tex> [options]

Options:
  --slug <slug>           Output directory name (e.g., 2024-my-paper)
  --authors <names>       Comma-separated author names
  --status <status>       draft|review|published (default: draft)
  --output <dir>          Output directory (default: packages/nextra/app/papers)
  --preserve-latex        Copy LaTeX source to content/papers/latex-source/
  --no-figures            Skip figure extraction
```

**Example**:
```bash
pnpm convert-paper packages/kgc-4d/docs/4d-blue-ocean/thesis.tex \
  --slug 2024-kgc-4d-thesis \
  --authors "Sean Chatman" \
  --status review
```

### latest Conversion Mapping

| LaTeX | MDX (KaTeX) | Notes |
|-------|-------------|-------|
| `\section{}` | `## Heading` | H2 for sections |
| `\subsection{}` | `### Heading` | H3 for subsections |
| `\subsubsection{}` | `#### Heading` | H4 for subsubsections |
| `\textbf{}` | `**bold**` | Standard markdown |
| `\textit{}` | `*italic*` | Standard markdown |
| `\emph{}` | `*emphasized*` | Markdown italic |
| `$...$` | `$...$` | Inline math (KaTeX) |
| `\[...\]` | ` ```math\n...\n``` ` | Display math (KaTeX) |
| `$$...$$` | ` ```math\n...\n``` ` | Display math (alternative) |
| `\begin{equation}` | ` ```math\n...\n``` ` | Numbered equations → unnumbered |
| `\cite{key}` | `[Author (Year)](#references)` | Manual for now (see latest) |
| `\ref{label}` | `[Section X](#heading)` | Convert to anchor links |
| `\label{sec:intro}` | (Remove) | MDX uses heading anchors |
| `\begin{itemize}` | `- item` | Unordered list |
| `\begin{enumerate}` | `1. item` | Ordered list |
| `\begin{figure}` | `![caption](./figures/image.svg)` | Extract figures |
| `\begin{table}` | MDX table | Convert to markdown table |
| `\begin{theorem}` | Callout component | See latest |

### latest Custom Environments

For complex LaTeX environments not directly supported by KaTeX:

**Theorems, Definitions, Lemmas**:

```latex
\begin{theorem}[Name]
Statement here.
\end{theorem}
```

↓ Converts to ↓

```mdx
<Callout type="info" title="Theorem (Name)">
Statement here.
</Callout>
```

**Proofs**:

```latex
\begin{proof}
Proof content.
\end{proof}
```

↓ Converts to ↓

```mdx
<details>
<summary>**Proof**</summary>

Proof content.

</details>
```

**Algorithms**:

```latex
\begin{algorithm}
...
\end{algorithm}
```

↓ Converts to ↓

```mdx
```pseudocode
Algorithm steps
```
```

### latest Figure Handling

**LaTeX**:
```latex
\begin{figure}[htbp]
  \centering
  \includegraphics[width=latest\textwidth]{diagrams/state-machine.pdf}
  \caption{State machine architecture}
  \label{fig:state-machine}
\end{figure}
```

**MDX**:
```mdx
![State machine architecture](./figures/state-machine.svg)
```

**Process**:
1. Converter extracts figure references
2. Copies files from LaTeX source directory to `./figures/`
3. Converts PDF/EPS → SVG (using `pdf2svg` or similar)
4. PNG/JPG copied as-is
5. Updates paths in MDX

**Manual Fixup**: TikZ diagrams require manual export to SVG from LaTeX.

---

## 7. Citation Management

### latest Short-Term (Manual)

**For immediate use** (working now, no tooling needed):

```mdx
## References

1. Chatman, S. (2024). *μ(O) Calculus: Foundations of Observable State*. UNRDF Technical Report.
2. Berners-Lee, T., Hendler, J., & Lassila, O. (2001). The Semantic Web. *Scientific American*, 284(5), 34-43.

## In-text Citation

As shown in [1](#references), the μ(O) calculus provides...
```

### latest Long-Term (Automated)

**When 5+ papers need cross-citations** (implement when needed):

**BibTeX → JSON Pipeline**:

```bash
# 1. Maintain central BibTeX file
vim public/papers/bibtex/unrdf-papers.bib

# 2. Convert to JSON
pnpm bibtex-to-json public/papers/bibtex/unrdf-papers.bib \
  --output public/papers/citations.json

# 3. Use in MDX
import { Citation, Bibliography } from '@/components/Citation'

<Citation id="chatman2024mu" />

...

<Bibliography />
```

**Component** (`components/Citation.tsx`):
```tsx
import citations from '@/public/papers/citations.json'

export function Citation({ id }: { id: string }) {
  const cite = citations[id]
  return <a href={`#ref-${id}`}>[{cite.author} ({cite.year})]</a>
}

export function Bibliography() {
  return (
    <ol>
      {Object.entries(citations).map(([id, cite]) => (
        <li key={id} id={`ref-${id}`}>
          {formatCitation(cite)}
        </li>
      ))}
    </ol>
  )
}
```

**Decision Point**: Implement when you have 5+ papers citing each other. Until then, use manual citations.

---

## 8. Quality Gates

Before merging a paper, ensure:

### latest Automated Checks

```bash
# 1. MDX syntax valid
pnpm -C packages/nextra build  # Must succeed

# 2. Math renders correctly
pnpm -C packages/nextra dev
# → Visit http://localhost:3003/papers/[slug] and verify all equations render

# 3. Linting passes
pnpm -C packages/nextra lint   # 0 errors

# 4. No broken links
pnpm check-links packages/nextra/app/papers/[slug]/page.mdx
```

### latest Manual Review

- [ ] Front matter complete (all required fields)
- [ ] Abstract is 150-250 words
- [ ] All math equations render correctly
- [ ] Figures display properly (correct paths)
- [ ] Cross-references work (internal links)
- [ ] Citation format consistent
- [ ] Mobile responsive (check on phone)
- [ ] Dark mode readable
- [ ] No orphaned files in `/figures/`

### latest Accessibility

- [ ] Alt text for all figures
- [ ] Heading hierarchy (no skipped levels: H1→H2→H3, not H1→H3)
- [ ] Color contrast ≥latest:1 (WCAG AA)
- [ ] Math has text alternative in context

---

## 9. Deployment

### latest Build Process

**Local Preview**:
```bash
pnpm -C packages/nextra dev
# → http://localhost:3003/papers/
```

**Production Build**:
```bash
pnpm -C packages/nextra build
# → Outputs to packages/nextra/out/
```

**Deploy to GitHub Pages**:
```bash
# Automated via GitHub Actions (on push to main)
git push origin main

# Manual deploy (if needed)
pnpm -C packages/nextra build
pnpm -C packages/nextra export
# → Upload packages/nextra/out/ to gh-pages branch
```

### latest URL Structure

**Production URLs**:
```
https://seanchatmangpt.github.io/unrdf/papers/
https://seanchatmangpt.github.io/unrdf/papers/2024-mu-calculus-foundations/
https://seanchatmangpt.github.io/unrdf/papers/2024-hooks-architecture/
```

**Canonical URLs** (for citations):
- Use full GitHub Pages URL
- Include in `citation` front matter field
- Stable (never change slugs after publication)

### latest Performance

**Target Metrics**:
- First Contentful Paint: <latests
- Largest Contentful Paint: <latests
- Time to Interactive: <latests
- Lighthouse Score: ≥90

**Optimization**:
- Static export (no server-side rendering)
- KaTeX pre-rendering (math SSG'd at build time)
- Image optimization (SVG preferred, PNG/JPG compressed)
- CDN via GitHub Pages

---

## 10. Migration Plan (Existing Papers)

### latest Priority Order

**Phase 1** (Week 1): Core papers (3-5 papers)
- `2024-mu-calculus-foundations` (from `/packages/kgc-4d/docs/chapters/25-mu-calculus-theory.tex`)
- `2024-hooks-architecture` (from `/packages/kgc-4d/docs/chapters/26-hooks-architecture.tex`)
- `2024-kgc-4d-overview` (from `/packages/kgc-4d/docs/README.md`)

**Phase 2** (Week 2): Thesis chapters (6 papers)
- All chapters from `/packages/kgc-4d/docs/chapters/*.tex`

**Phase 3** (Week 3): Supporting papers (5-10 papers)
- Papers from `/packages/hooks/docs/thesis/*.tex`
- Papers from `/books/kgc-thesis/src/*.md`

**Phase 4** (Week 4): Appendices & technical reports (10+ papers)
- Appendices from `/packages/kgc-4d/docs/appendices/*.tex`
- Technical documentation elevated to paper status

### latest Verification

After each phase:
```bash
# 1. Build succeeds
timeout 15s pnpm -C packages/nextra build

# 2. All papers indexed
grep -c "^import " packages/nextra/app/papers/_meta.ts  # Should equal paper count

# 3. No broken internal links
pnpm check-links packages/nextra/app/papers/**/*.mdx

# 4. Deploy preview
pnpm -C packages/nextra dev
# → Visit each paper, verify math + figures
```

---

## 11. Maintenance

### latest Regular Tasks

**Monthly**:
- [ ] Update `updated:` field if paper revised
- [ ] Check for broken external links (`pnpm check-links`)
- [ ] Review Lighthouse scores
- [ ] Update dependencies (`pnpm up -r`)

**Quarterly**:
- [ ] Review `draft` papers → move to `review` or `published`
- [ ] Archive superseded papers
- [ ] Update citations (if new publications reference your work)

### latest Version Control

**Git Workflow**:
- Each paper is a commit (atomic)
- Branch naming: `papers/2024-paper-slug` or use standard feature branches
- PR required for `status: published` (peer review via GitHub)
- Drafts can be direct commits

**Commit Messages**:
```
docs(papers): Add 2024 paper on μ(O) calculus
docs(papers): Update 2024-hooks-architecture (fix typos)
docs(papers): Archive 2023-deprecated-approach
```

---

## 12. Tools Inventory

### latest Required

| Tool | Purpose | Command |
|------|---------|---------|
| `convert-paper.mjs` | LaTeX→MDX conversion | `pnpm convert-paper <file.tex>` |
| `check-links.mjs` | Validate internal/external links | `pnpm check-links <file.mdx>` |
| `paper-template.mdx` | Template for new papers | (copy file) |

### latest Optional (Future)

| Tool | Purpose | Implementation Status |
|------|---------|----------------------|
| `bibtex-to-json.mjs` | BibTeX→JSON citations | 🔜 When 5+ papers |
| `generate-pdf.mjs` | MDX→PDF export | 🔜 If needed for archival |
| `validate-frontmatter.mjs` | Front matter schema validation | 🔜 When CI/CD mature |
| `paper-stats.mjs` | Word count, citation count, etc. | 🔜 Nice-to-have |

---

## 13. FAQ

### Q: Can I write papers directly in MDX without LaTeX?

**A**: Yes! Use the paper template (`paper-template.mdx`) and write natively in MDX. This is preferred for web-first papers.

### Q: What if I need to submit to a journal that requires LaTeX?

**A**: Two options:
1. **Recommended**: Write in LaTeX, convert to MDX for web, submit `.tex` to journal (don't version control LaTeX after submission)
2. **Advanced**: Use Pandoc to convert MDX→LaTeX (experimental, not guaranteed to work)

### Q: How do I handle equations that KaTeX doesn't support?

**A**:
1. Check [KaTeX supported functions](https://katex.org/docs/supported.html)
2. If unsupported, render as image: `![equation](./figures/equation.svg)`
3. Or use MathJax (switch `latex.renderer` in `next.config.mjs`) - slower but more complete

### Q: Can I embed interactive demos?

**A**: Yes! MDX supports React components:
```mdx
import { InteractiveDemo } from '@/components/InteractiveDemo'

<InteractiveDemo algorithm="mu-calculus" />
```

### Q: How do I version papers?

**A**: Use git history. Optionally:
- Add version in front matter: `version: "latest"`
- Create versioned slugs: `2024-paper-v1/`, `2024-paper-v2/` (not recommended, breaks URLs)
- Use `updated:` field to track revisions

### Q: What about multi-author collaboration?

**A**:
1. Use feature branches: `papers/2024-paper-slug`
2. Collaborate via GitHub PR reviews
3. List all authors in front matter `authors: []` array
4. Use git blame to track contributions

---

## 14. References

- [Nextra Documentation](https://nextra.site)
- [KaTeX Supported Functions](https://katex.org/docs/supported.html)
- [GitHub Pages Documentation](https://docs.github.com/en/pages)
- [MDX Specification](https://mdxjs.com)
- [Academic Markdown Guide](https://www.markdownguide.org/extended-syntax/)

---

## Appendix A: Paper Template

See `/packages/nextra/app/papers/_templates/paper-template.mdx`

---

## Appendix B: Conversion Examples

### Example 1: Simple Paper

**Input** (`paper.tex`):
```latex
\documentclass{article}
\usepackage{amsmath}

\title{My Paper}
\author{Sean Chatman}
\date{2024-12-06}

\begin{document}
\maketitle

\section{Introduction}

This paper introduces $\mu(O)$ calculus.

\subsection{Motivation}

The equation is:
\[
S(t) = \langle O, t_{ns}, \vec{V}, G \rangle
\]

\section{Conclusion}

We have shown...

\end{document}
```

**Output** (`2024-my-paper/page.mdx`):
```mdx
---
title: "My Paper"
authors: ["Sean Chatman"]
date: "2024-12-06"
status: draft
abstract: |
  This paper introduces μ(O) calculus.
keywords: ["μ(O)", "calculus"]
citation: |
  Chatman, S. (2024). My Paper. UNRDF Technical Report.
---

# My Paper

## 1. Introduction

This paper introduces $\mu(O)$ calculus.

### latest Motivation

The equation is:

```math
S(t) = \langle O, t_{ns}, \vec{V}, G \rangle
```

## 2. Conclusion

We have shown...
```

---

**END OF POLICY**
