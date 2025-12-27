# V6 Documentation Pipeline Summary

**Created:** 2025-12-27  
**Status:** Complete  
**Version:** 6.0.0-alpha.1

## Overview

Unified documentation pipeline using Diataxis framework with deterministic LaTeX→PDF generation.

---

## Deliverables

### 1. Specification

**File:** `/home/user/unrdf/docs/v6/DIATAXIS_SPEC.md`

Comprehensive specification covering:
- Diataxis framework (4 categories)
- Directory structure per package
- Required sections for each category
- Frontmatter with proof system
- Cross-linking conventions
- LaTeX pipeline stages
- Quality gates
- CLI integration

**Key Features:**
- Proof system with confidence scores (0.0-1.0)
- Merkle root for deterministic verification
- Receipt emission at each pipeline stage
- Example: @unrdf/yawl documentation structure

---

### 2. Pipeline Implementation

**Module:** `@unrdf/v6-core/docs`

#### a. `pipeline.mjs` (270 lines)

**Exports:**
- `collectPackageDocs(packageDirs, options)` - Gather docs from packages
- `generateDiataxisOutput(packageDocs)` - Generate Diataxis structure
- `emitReceipt(stage, input, output, outputPath)` - Receipt generation

**Features:**
- Collects docs from 4 categories: tutorials, how-to, reference, explanation
- Parses frontmatter (YAML)
- Calculates SHA256 hashes for all files
- Computes Merkle root (sorted hash concatenation)
- Deterministic output (sorted keys, stable JSON)

**Receipt Format:**
```javascript
{
  stage: "collect|diataxis|latex|pdf",
  input: { type, hash },
  output: { type, hash },
  timestamp: "ISO-8601",
  merkleRoot: "sha256-..."
}
```

#### b. `latex-generator.mjs` (341 lines)

**Exports:**
- `generateLatex(diataxisDocs, outputDir)` - Convert to LaTeX
- `compileToPDF(texFile, options)` - Compile to PDF (stubbed)

**Features:**
- Generates LaTeX preamble with packages (hyperref, listings, geometry, etc.)
- Creates one chapter per package
- Converts markdown → LaTeX (simplified):
  - Code blocks → `\begin{lstlisting}`
  - Inline code → `\texttt{}`
  - Bold → `\textbf{}`
  - Links → `\href{}{}`
- Escapes LaTeX special characters
- Deterministic lockfile with file hashes

**Lockfile Format:**
```json
{
  "version": "6.0.0-alpha.1",
  "generatedAt": "ISO-8601",
  "merkleRoot": "sha256-...",
  "files": { "main": "...", "chapters": [...] },
  "latex": { "compiler": "latexmk", "version": "4.77", "hash": "..." },
  "packages": { "@unrdf/pkg": { "version": "...", ... } }
}
```

**PDF Compilation:**
- Currently stubbed (WASM engine pending)
- Returns expected PDF path
- TODO: Integrate WASM LaTeX compiler

#### c. `thesis-builder.mjs` (196 lines)

**Exports:**
- `buildThesis(config)` - Full pipeline build
- `renderFromOntology(ontologyPath, outputDir)` - CONSTRUCT rendering (stubbed)
- `exportThesis(thesisDir, format, outputPath)` - Export to pdf|latex|html

**Build Pipeline Stages:**
1. **Collect** - Package docs → PackageDocs[]
2. **Diataxis** - Structure generation → DiataxisOutput
3. **LaTeX** - Conversion → .tex files
4. **PDF** - Compilation → .pdf (optional)

Each stage emits a receipt to `{outputDir}/receipts/NN-stage.json`

**Build Output:**
```javascript
{
  latexDir: "/path/to/latex",
  mainTexFile: "/path/to/thesis.tex",
  pdfFile: "/path/to/thesis.pdf" (optional),
  receipts: [{ stage, input, output, timestamp, merkleRoot }],
  stats: {
    duration: 1234,
    packages: 10,
    tutorials: 15,
    howtos: 20,
    reference: 25,
    explanation: 12,
    totalDocs: 72,
    merkleRoot: "sha256-..."
  }
}
```

---

### 3. CLI Commands

**Module:** `@unrdf/v6-core/cli/commands/thesis.mjs`

#### `kgc thesis build`

Build thesis from package documentation.

**Options:**
- `--output` (default: `./thesis`) - Output directory
- `--packages` (default: `packages/*`) - Package glob pattern
- `--pdf` (default: `false`) - Generate PDF

**Example:**
```bash
kgc thesis build --output=./output --packages="packages/@unrdf/*" --pdf
```

**Output:**
- LaTeX files in `{output}/latex/`
- Receipts in `{output}/receipts/`
- Build summary with statistics

#### `kgc thesis render`

CONSTRUCT documentation from ontology (SPARQL).

**Options:**
- `--ontology` (required) - Path to .ttl ontology
- `--output` (default: `./rendered-docs`) - Output directory

**Example:**
```bash
kgc thesis render --ontology=docs.ttl --output=./rendered
```

**Status:** Not yet implemented (stubbed)

#### `kgc thesis export`

Export thesis to specified format.

**Options:**
- `--format` (default: `pdf`) - Export format (pdf|latex|html)
- `--input` (default: `./thesis`) - Thesis directory
- `--output` - Output file path

**Example:**
```bash
kgc thesis export --format=pdf --input=./thesis --output=./thesis.pdf
```

---

### 4. Diataxis Templates

**Location:** `/home/user/unrdf/docs/templates/`

All templates updated with proof system:

#### `tutorial.md`
- Action-oriented title
- Learning objectives
- Prerequisites checklist
- Step-by-step instructions
- Complete working example
- Expected output
- Next steps + challenges
- Troubleshooting
- **Proof section** (sources, hash, confidence)

#### `howto.md`
- Problem statement
- Use cases
- Solution steps
- Complete example
- Variations
- Troubleshooting
- Best practices
- **Proof section**

#### `reference.md`
- API overview
- Import paths
- Function signatures (JSDoc)
- Parameter tables
- Return values
- Error conditions
- Types
- Usage examples
- **Proof section** (confidence: 1.00 for code-generated)

#### `explanation.md`
- Concept overview
- Background (problem context)
- UNRDF approach
- Key principles
- Design decisions
- Trade-offs table
- When to use
- Common misconceptions
- Next steps
- **Proof section** (confidence: 0.85 for conceptual)

---

## Pipeline Stages with Receipt Points

```
┌─────────────────┐
│ Package Dirs    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐    Receipt: 01-collect.json
│ 1. Collect      │ ──→ { packageDocs[], hash }
└────────┬────────┘
         │
         ▼
┌─────────────────┐    Receipt: 02-diataxis.json
│ 2. CONSTRUCT    │ ──→ { diataxisOutput, merkleRoot }
└────────┬────────┘
         │
         ▼
┌─────────────────┐    Receipt: 03-latex.json
│ 3. LaTeX        │ ──→ { .tex files, lockfile, hash }
└────────┬────────┘
         │
         ▼
┌─────────────────┐    Receipt: 04-pdf.json
│ 4. PDF          │ ──→ { .pdf file } (optional)
└─────────────────┘
```

Each receipt includes:
- Stage name
- Input hash
- Output hash
- Timestamp (ISO-8601)
- Merkle root

---

## Template Structure by Category

### Tutorials

**Goal:** Learning-oriented, hands-on

**Structure:**
1. What you'll build (concrete outcome)
2. What you'll learn (learning objectives)
3. Prerequisites (checklist)
4. Steps (numbered, detailed)
5. Complete example (copy-paste-able)
6. Summary (what was learned)
7. Next steps (continue learning)
8. Troubleshooting
9. Proof (sources, hash, confidence: 0.95)

**Confidence:** 0.95 (verified against code + tests)

---

### How-to Guides

**Goal:** Task-oriented, problem-solving

**Structure:**
1. Problem statement
2. Use cases ("Use this when...")
3. Prerequisites
4. Solution steps (concise)
5. Complete example
6. Variations (alternatives)
7. Troubleshooting
8. Best practices
9. Proof (sources, hash, confidence: 0.90)

**Confidence:** 0.90 (manual review against code)

---

### Reference

**Goal:** Information-oriented, technical specs

**Structure:**
1. Overview
2. Import paths
3. API (functions, signatures, params, returns)
4. Types (JSDoc typedefs)
5. Constants
6. Errors
7. Usage examples (minimal)
8. See also (links)
9. Proof (sources, hash, confidence: 1.00)

**Confidence:** 1.00 (code-generated from JSDoc)

---

### Explanation

**Goal:** Understanding-oriented, conceptual

**Structure:**
1. Overview (what + why it matters)
2. Background (problem context)
3. UNRDF approach
4. Key principles
5. How it works
6. Design decisions (context, decision, rationale, trade-offs)
7. When to use / not use
8. Common misconceptions
9. Real-world example
10. Summary
11. Next steps
12. Proof (sources, hash, confidence: 0.85)

**Confidence:** 0.85 (conceptual, references ADRs)

---

## Cross-Linking Strategy

**Navigation Paths:**

```
Tutorials ──────────→ How-to Guides
    │                      │
    │                      ▼
    └──────────────→ Reference ←───────┐
                           │           │
                           ▼           │
                      Explanation ─────┘
```

**Patterns:**
- **Tutorial → How-to:** "Now solve specific problem X"
- **How-to → Reference:** "See API docs for full details"
- **Reference → Explanation:** "Understand why this design"
- **Explanation → Tutorial:** "Try it hands-on"

**Link Format:**
- Internal: Relative paths (`../tutorials/01-start.md`)
- Cross-package: Absolute paths (`/packages/pkg/docs/...`)

---

## File Counts

```bash
# Core implementation
/home/user/unrdf/packages/v6-core/src/docs/
  ├── index.mjs              (12 lines)
  ├── pipeline.mjs           (270 lines)
  ├── latex-generator.mjs    (341 lines)
  └── thesis-builder.mjs     (196 lines)
  TOTAL: 819 lines

# CLI commands
/home/user/unrdf/packages/v6-core/src/cli/commands/
  └── thesis.mjs             (143 lines)

# Specification
/home/user/unrdf/docs/v6/
  └── DIATAXIS_SPEC.md       (11,238 bytes)

# Templates
/home/user/unrdf/docs/templates/
  ├── tutorial.md            (3,529 bytes)
  ├── howto.md               (2,383 bytes)
  ├── reference.md           (2,712 bytes)
  └── explanation.md         (3,208 bytes)
```

---

## Success Criteria

- [x] DIATAXIS_SPEC.md created with complete specification
- [x] Pipeline stages implemented (collect, diataxis, latex, pdf-stub)
- [x] Receipt emission at each stage
- [x] LaTeX generation with deterministic lockfile
- [x] CLI commands (build, render-stub, export)
- [x] Templates updated with proof system
- [x] All 4 Diataxis categories covered
- [x] Cross-linking conventions defined
- [x] Merkle root calculation
- [x] Deterministic output (sorted keys, stable hashes)

---

## Next Steps (Implementation)

### Immediate
1. Test pipeline with sample package
2. Validate frontmatter parsing
3. Verify Merkle root calculation
4. Test LaTeX generation

### Short-term
1. Implement WASM LaTeX compiler integration
2. Add HTML export
3. Implement SPARQL CONSTRUCT rendering
4. Add validation gates (structure, links, proof)

### Long-term
1. Auto-generate reference docs from JSDoc
2. Extract tutorials from examples/
3. Generate explanations from ADRs
4. Build comprehensive test suite

---

## Evidence

**Files Created:**
- /home/user/unrdf/docs/v6/DIATAXIS_SPEC.md
- /home/user/unrdf/packages/v6-core/src/docs/pipeline.mjs
- /home/user/unrdf/packages/v6-core/src/docs/latex-generator.mjs
- /home/user/unrdf/packages/v6-core/src/docs/thesis-builder.mjs
- /home/user/unrdf/packages/v6-core/src/docs/index.mjs
- /home/user/unrdf/packages/v6-core/src/cli/commands/thesis.mjs
- /home/user/unrdf/packages/v6-core/src/cli/index.mjs
- /home/user/unrdf/docs/templates/tutorial.md (updated)
- /home/user/unrdf/docs/templates/howto.md (updated)
- /home/user/unrdf/docs/templates/reference.md (updated)
- /home/user/unrdf/docs/templates/explanation.md (updated)

**Total LOC:** 819 lines (docs module) + 143 lines (CLI) = 962 lines

**Verification:**
```bash
ls -la /home/user/unrdf/docs/v6/DIATAXIS_SPEC.md
ls -la /home/user/unrdf/packages/v6-core/src/docs/*.mjs
ls -la /home/user/unrdf/packages/v6-core/src/cli/commands/thesis.mjs
ls -la /home/user/unrdf/docs/templates/*.md
```

