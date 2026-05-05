# UNRDF Diataxis Documentation Plan

## Executive Summary

This document outlines the strategy for implementing **full Diataxis documentation** across all 17 UNRDF packages. Diataxis is a four-part framework that organizes documentation into distinct types: Tutorials, How-To Guides, Reference, and Explanation.

**Current State:** Root-level docs exist with some Diataxis structure, but 17 packages have inconsistent, incomplete documentation.

**Goal:** Every package has complete, well-organized documentation following Diataxis principles by Q2 2025.

**Impact:**
- 🎯 Users find answers faster
- 👥 Contributors understand code better
- 🔄 Maintainability improves
- 📈 Adoption increases

---

## What is Diataxis?

Diataxis is a documentation framework by Daniele Procida that organizes docs into four quadrants:

```
                    THEORY ←→ PRACTICE
                       ↑
                       |
         TUTORIALS    |    HOW-TO GUIDES
      (Learning)      |    (Problem-solving)
  ────────────────────┼────────────────────
  (Understanding)     |     (Information)
     EXPLANATION      |      REFERENCE
                       |
                       ↓
```

| Type | Purpose | Audience | Question Asked |
|------|---------|----------|-----------------|
| **Tutorials** | Learning by doing | Newcomers | "How do I get started?" |
| **How-To Guides** | Solving specific problems | Working developers | "How do I do X?" |
| **Reference** | Complete information | Active users | "What does this do?" |
| **Explanation** | Understanding concepts | Curious learners | "Why does it work this way?" |

**Key Principle:** Each type serves a different need. Don't mix them (e.g., API reference shouldn't teach concepts).

---

## Current State Assessment

### 📊 Coverage by Package

| Package | README | docs/ | API.md | GUIDE.md | Status |
|---------|:------:|:-----:|:------:|:--------:|--------|
| @unrdf/core | ✅ | ✅ | 📄 (latestK) | 📄 | Most developed |
| @unrdf/hooks | ✅ | ✅ | 📄 | 📄 | Strong (PhD thesis) |
| @unrdf/browser | ✅ | ✅ | 📋 TODO | 📋 TODO | Needs content |
| @unrdf/cli | ✅ | ✅ | 📋 TODO | 📋 TODO | Needs content |
| @unrdf/streaming | ✅ | ✅ | 📋 TODO | 📋 TODO | Needs content |
| @unrdf/federation | ✅ | ✅ | 📋 TODO | 📋 TODO | Needs content |
| @unrdf/knowledge-engine | ✅ | ✅ | 📋 TODO | 📋 TODO | Needs content |
| @unrdf/composables | ✅ | ✅ | 📋 TODO | 📋 TODO | Needs content |
| @unrdf/dark-matter | ✅ | ✅ | 📋 TODO | 📋 TODO | Needs content |
| @unrdf/project-engine | ✅ | ✅ | 📋 TODO | 📋 TODO | Needs content |
| @unrdf/oxigraph | ✅ | ❌ | — | — | Needs docs/ |
| @unrdf/react | ✅ | ❌ | — | — | Needs docs/ |
| @unrdf/engine-gateway | ✅ | ❌ | — | — | Needs docs/ |
| @unrdf/domain | ✅ | ❌ | — | — | Private (minimal OK) |
| @unrdf/test-utils | ✅ | ❌ | — | — | Private (minimal OK) |
| @unrdf/validation | ✅ | ❌ | — | — | Private (minimal OK) |

**Summary:**
- 100% have README ✅
- 53% have docs/ directory
- 8/9 with docs/ have incomplete API.md and GUIDE.md files
- No package has structured Diataxis layout

### 🎯 Current Gaps

1. **No tutorials** at package level (only root /examples/)
2. **Stub API files** need real content from JSDoc
3. **No task-oriented guides** per package
4. **No troubleshooting** sections
5. **Private packages** (domain, test-utils, validation) are undocumented
6. **Inconsistent structure** across packages
7. **No cross-package workflows** documented
8. **Examples disconnected** from docs

---

## Target Diataxis Structure

### Per-Package Documentation Directory

```
packages/{name}/docs/
│
├── 📋 INDEX.md                         # Navigation & overview
│
├── 📚 TUTORIALS/                       # Getting started
│   ├── 01-getting-started.md           # "Your first experience"
│   ├── 02-basic-workflow.md            # "Common workflow"
│   ├── 03-advanced-patterns.md         # "Advanced usage"
│   └── README.md                       # Tutorials overview
│
├── 🔧 HOW-TO/                          # Task-oriented
│   ├── common-task-1.md                # "How to do X"
│   ├── common-task-2.md                # "How to do Y"
│   ├── troubleshooting.md              # "Why doesn't X work?"
│   ├── performance-optimization.md     # "How to speed up..."
│   └── README.md                       # How-to overview
│
├── 📖 REFERENCE/                       # Complete information
│   ├── API.md                          # Full API reference
│   ├── TYPES.md                        # Type definitions
│   ├── CONFIGURATION.md                # Config options
│   ├── ERRORS.md                       # Error codes
│   ├── MIGRATION.md                    # Version migration
│   └── README.md                       # Reference overview
│
├── 💡 EXPLANATION/                     # Understanding
│   ├── architecture.md                 # How it works
│   ├── design-decisions.md             # Why this design?
│   ├── concepts.md                     # Key concepts
│   ├── performance.md                  # Performance details
│   └── README.md                       # Explanation overview
│
└── 📄 CONTRIBUTING.md                  # Contributor guide

```

### Root-Level Documentation Map

```
docs/
│
├── 📋 DIATAXIS-MAP.md                  # THIS: Overview of all docs
│
├── TUTORIALS/                          # Cross-package learning paths
│   ├── getting-started.md              # UNRDF 101
│   ├── first-rdf-app.md                # Build a simple app
│   ├── knowledge-hooks.md              # Learn hooks in depth
│   └── federated-queries.md            # Multi-store queries
│
├── HOW-TO/                             # Task-oriented (root level)
│   ├── performance-optimization.md     # Cross-package perf guide
│   ├── debugging.md                    # Debug across packages
│   ├── deployment.md                   # Deploy UNRDF apps
│   └── troubleshooting.md              # Common issues
│
├── REFERENCE/                          # Cross-package reference
│   ├── API-INDEX.md                    # Links to all APIs
│   ├── CLI-REFERENCE.md                # CLI command reference
│   ├── SPARQL-REFERENCE.md             # SPARQL query guide
│   └── SHACL-REFERENCE.md              # SHACL validation guide
│
├── EXPLANATION/                        # Concepts
│   ├── architecture-overview.md        # System design
│   ├── data-flow.md                    # How data flows
│   ├── monorepo-structure.md           # Why this structure?
│   └── comparison-with-alternatives.md # vs GraphDB, Virtuoso, etc
│
├── PACKAGE-GUIDES/                     # Package discovery
│   ├── package-index.md                # All packages explained
│   ├── choosing-packages.md            # Which packages for your task?
│   └── dependency-graph.md             # Package relationships
│
├── GETTING-STARTED/ (existing)         # Keep as-is
├── guides/ (existing)                  # Convert to HOW-TO/
└── explanation/ (existing)             # Merge into EXPLANATION/
```

---

## Implementation Strategy

### Phase 1: Foundation (Weeks 1-2)

**Goal:** Create structure and templates

#### Task latest: Create Diataxis Templates
- [ ] Create `docs/DIATAXIS-GUIDE.md` - How to write each type
- [ ] Create `docs/_templates/` with boilerplate:
  - `TUTORIALS.md` template
  - `HOW-TO.md` template
  - `REFERENCE.md` template
  - `EXPLANATION.md` template
- [ ] Create package-level `INDEX.md` template
- [ ] Create `docs/DIATAXIS-MAP.md` (navigation hub)

#### Task latest: Update Root-Level Structure
- [ ] Rename `guides/` → `HOW-TO/` (or copy)
- [ ] Rename `explanation/` → `EXPLANATION/` (or merge)
- [ ] Create `TUTORIALS/` root directory
- [ ] Create `REFERENCE/` root directory
- [ ] Create `PACKAGE-GUIDES/` root directory

#### Task latest: Create Package Structure Template
- [ ] Create shell script: `scripts/create-package-docs.sh`
  - Creates standard directory structure
  - Copies INDEX.md template
  - Creates placeholder files
- [ ] Run for all 17 packages

### Phase 2: Core Packages (Weeks 3-5)

**Goal:** Complete documentation for 3 most-used packages

#### Priority 1: @unrdf/core

**Tutorials:**
- [ ] 01-getting-started.md - First SPARQL query
- [ ] 02-storing-data.md - Parse & store RDF
- [ ] 03-querying-patterns.md - Common SPARQL patterns
- [ ] 04-validation.md - SHACL validation tutorial

**How-To:**
- [ ] how-to-optimize-queries.md
- [ ] how-to-work-with-formats.md
- [ ] troubleshooting-sparql.md
- [ ] performance-tuning.md

**Reference:**
- [ ] API.md - Extract from code + current docs
- [ ] TYPES.md - All data types
- [ ] CONFIGURATION.md - createKnowledgeSubstrateCore options
- [ ] ERRORS.md - Common errors
- [ ] MIGRATION.md - Version migration guide

**Explanation:**
- [ ] architecture.md - How core works
- [ ] design-decisions.md - Why this design?
- [ ] sparql-execution.md - Query execution model
- [ ] performance-internals.md - Performance details

#### Priority 2: @unrdf/hooks

**Tutorials:**
- [ ] 01-first-hook.md
- [ ] 02-hook-patterns.md
- [ ] 03-autonomous-behaviors.md

**How-To:**
- [ ] how-to-debug-hooks.md
- [ ] how-to-compose-hooks.md
- [ ] how-to-handle-errors.md
- [ ] performance-optimization.md

**Reference:**
- [ ] API.md - Hook API
- [ ] SCHEMA.md - Hook configuration schema
- [ ] TRIGGERS.md - All trigger types

**Explanation:**
- [ ] how-hooks-work.md - Hook lifecycle
- [ ] architecture.md - Hook system design
- [ ] use-cases.md - Real-world examples

#### Priority 3: @unrdf/oxigraph

**Tutorials:**
- [ ] 01-persistent-storage.md
- [ ] 02-rust-integration.md

**How-To:**
- [ ] how-to-migrate-data.md
- [ ] how-to-backup.md
- [ ] performance-tuning.md

**Reference:**
- [ ] API.md
- [ ] INSTALLATION.md - Oxigraph setup

**Explanation:**
- [ ] why-oxigraph.md - Why Rust backend?
- [ ] architecture.md

### Phase 3: Extended Features (Weeks 6-8)

Complete documentation for 6 packages:
- @unrdf/streaming
- @unrdf/federation
- @unrdf/knowledge-engine
- @unrdf/browser
- @unrdf/cli
- @unrdf/react

**Template per package:**
- 3 tutorials (getting started, workflow, advanced)
- 4 how-to guides (common tasks, troubleshooting)
- 4 reference files (API, types, config, errors)
- 3 explanation files (architecture, design, concepts)

### Phase 4: Supporting Packages (Weeks 9-10)

Complete documentation for remaining public packages:
- @unrdf/composables
- @unrdf/dark-matter
- @unrdf/project-engine
- @unrdf/engine-gateway

### Phase 5: Private Packages (Week 11)

Minimal Diataxis for internal packages:
- @unrdf/domain - Reference + Explanation
- @unrdf/test-utils - How-To + Reference
- @unrdf/validation - How-To + Reference

### Phase 6: Root-Level & Cross-Package (Week 12)

- [ ] Create cross-package tutorials:
  - "Build a blog with UNRDF" (core + hooks + browser)
  - "Federated search across databases" (federation + streaming)
  - "Knowledge reasoning engine" (knowledge-engine + hooks)

- [ ] Create navigation:
  - DIATAXIS-MAP.md linking all docs
  - Package index with Diataxis sections for each
  - "Which package should I use?" guide

- [ ] Update main README to point to Diataxis structure

---

## Content Guidelines

### Tutorial Writing

**Tone:** Friendly, encouraging, hands-on

**Structure:**
1. **Why this matters** (1 sentence)
2. **What you'll build** (1-2 sentences)
3. **Prerequisites** (list)
4. **Step-by-step instructions** (numbered)
5. **Verify it works** (how to test)
6. **What's next** (next tutorial to read)

**Example template:**
```markdown
# Tutorial: Your First SPARQL Query

This tutorial will teach you how to query RDF data.

## What You'll Learn
- Parse RDF data
- Write a SPARQL query
- Read results

## Prerequisites
- Node.js 18+
- npm or pnpm

## Step 1: Create a file

```bash
npm init
npm install @unrdf/core
```

[rest of tutorial...]
```

### How-To Writing

**Tone:** Direct, practical, problem-focused

**Structure:**
1. **Problem statement** (what problem?)
2. **Prerequisites** (what you need)
3. **Solution** (code + explanation)
4. **Variations** (alternative approaches)
5. **See also** (related guides)

**Example:**
```markdown
# How to Optimize SPARQL Queries

## Problem
Your queries are slow. You want to speed them up.

## Solution

### 1. Use indexes
[code example]

### 2. Simplify patterns
[code example]

## See Also
- [Performance Tuning Guide]
- [SPARQL Execution]
```

### Reference Writing

**Tone:** Formal, complete, precise

**Structure:**
1. **Overview** (what is this?)
2. **Quick example** (minimal working example)
3. **Detailed sections** (all options/methods)
4. **Parameters** (types, defaults)
5. **Examples** (common use cases)
6. **See also** (related references)

### Explanation Writing

**Tone:** Educational, conceptual, exploratory

**Structure:**
1. **Big picture** (context)
2. **How it works** (mechanism)
3. **Why this way** (design rationale)
4. **Real-world analogy** (comparison)
5. **When to use** (applicability)

---

## Automation & Tools

### JSDoc → Reference Extraction

Create `scripts/generate-api-reference.js`:

```javascript
// Parse JSDoc comments from src/ files
// Generate API.md with:
// - All exported functions
// - Parameters & return types
// - Code examples from @example tags
// - Links to source files

// Usage: pnpm run generate-api-ref --package @unrdf/core
```

### Template Generation

Create `scripts/init-package-docs.sh`:

```bash
#!/bin/bash
# Usage: ./scripts/init-package-docs.sh @unrdf/core

PACKAGE=$1
mkdir -p packages/${PACKAGE#@unrdf/}/docs/{TUTORIALS,HOW-TO,REFERENCE,EXPLANATION}

# Copy templates
cp docs/_templates/INDEX.md packages/${PACKAGE#@unrdf/}/docs/
cp docs/_templates/*.md packages/${PACKAGE#@unrdf/}/docs/*/

echo "✅ Initialized docs for $PACKAGE"
echo "📍 Next: Fill in content in packages/${PACKAGE#@unrdf/}/docs/"
```

### Diataxis Validation

Create `scripts/validate-diataxis.js`:

```javascript
// Checks each package for:
// - Has all 4 directories (TUTORIALS, HOW-TO, REFERENCE, EXPLANATION)
// - Each has README.md
// - Has minimum content (not just stubs)
// - Links are valid
// - Tone matches Diataxis guidelines

// Usage: pnpm run validate:diataxis
```

---

## Measurement & Success Criteria

### Coverage Metrics

- [ ] 100% of packages have Diataxis structure (4 directories)
- [ ] 80%+ of packages have >3 files in each section
- [ ] 0 "TODO" placeholders in documentation
- [ ] All exported functions have reference documentation
- [ ] All packages have at least 1 tutorial

### Quality Metrics

- [ ] All internal links are valid
- [ ] All code examples run without errors
- [ ] Readability score 60+ (Flesch-Kincaid)
- [ ] Examples cover 80% of common use cases
- [ ] Screenshots/diagrams for complex concepts

### User Metrics (post-launch)

- [ ] Documentation bounce rate < 15%
- [ ] Average time on docs > 2 minutes
- [ ] Search success rate > 70%
- [ ] Issue reports mentioning "docs unclear" drop 50%

---

## Ownership & Timeline

### Roles

| Role | Responsibility | Timeline |
|------|-----------------|----------|
| **Documentation Lead** | Oversee entire initiative | Weeks 1-12 |
| **Template Creator** | Build templates & automation | Weeks 1-2 |
| **Core/Hooks Writer** | Write @unrdf/core & hooks docs | Weeks 3-5 |
| **Feature Writers** | Write extended feature docs | Weeks 6-10 |
| **Technical Reviewer** | Verify accuracy & examples | Ongoing |
| **Editor** | Polish tone & consistency | Weeks 6-12 |

### Suggested Owners

- **Core (@unrdf/core):** Architecture team lead
- **Hooks (@unrdf/hooks):** Hooks maintainer
- **Streaming (@unrdf/streaming):** Performance expert
- **Federation (@unrdf/federation):** Distribution specialist
- **Browser (@unrdf/browser):** Frontend expert
- **React (@unrdf/react):** React expert
- **CLI (@unrdf/cli):** CLI maintainer
- etc.

### Milestones

| Milestone | Target | Status |
|-----------|--------|--------|
| **Foundation Ready** | End of Week 2 | 🔄 In Progress |
| **Core 3 Complete** | End of Week 5 | 📋 Planned |
| **Extended Features Done** | End of Week 8 | 📋 Planned |
| **All Public Packages Done** | End of Week 10 | 📋 Planned |
| **Root-Level Integration** | End of Week 12 | 📋 Planned |
| **Soft Launch** | End of Week 13 | 📋 Planned |
| **Feedback & Polish** | Weeks 14-16 | 📋 Planned |

---

## Integration with Existing Docs

### Keep vs Migrate vs Retire

| Directory | Status | Action |
|-----------|--------|--------|
| `/docs/GETTING-STARTED/` | Active | Migrate to TUTORIALS/ |
| `/docs/guides/` | Active | Merge into HOW-TO/ |
| `/docs/how-to/` | Active | Merge into HOW-TO/ |
| `/docs/explanation/` | Active | Keep + enhance |
| `/docs/audit/` | Reference | Keep as-is |
| `/docs/benchmarks/` | Reference | Keep as-is |
| `/docs/API-REFERENCE.md` | Old | Deprecate (move to REFERENCE/) |
| `packages/*/docs/API.md` | Stub | Populate with real content |
| `packages/*/docs/GUIDE.md` | Stub | Expand to full HOW-TO section |

### Navigation Strategy

**Main entry point:** New `docs/DIATAXIS-MAP.md`

```markdown
# UNRDF Documentation Map

## Quick Links
- **New to UNRDF?** → [Tutorials](./TUTORIALS/)
- **Building something?** → [How-To Guides](./HOW-TO/)
- **Looking up API?** → [Reference](./REFERENCE/)
- **Learning concepts?** → [Explanation](./EXPLANATION/)

## By Package
[Table of all packages with their Diataxis docs]

## By Task
[Index of tutorials and how-tos by use case]
```

---

## Risk Mitigation

### Potential Challenges

| Risk | Mitigation |
|------|------------|
| **High effort, low ROI** | Start with high-impact packages (core, hooks) |
| **Content becomes stale** | Automated validation + contributor guidelines |
| **Inconsistent tone** | Style guide + peer review process |
| **Duplication** | Cross-reference, don't duplicate |
| **Too much content** | Use Diataxis as filter (one per type) |
| **Missing examples** | Generate from tests/examples/ |

---

## Success Story

After full Diataxis implementation, users will:

✅ **New users:** "I got started in 10 minutes with the tutorial"

✅ **Active users:** "I found exactly how to do X with the how-to guide"

✅ **Developers:** "The API reference is complete and clear"

✅ **Contributors:** "The explanation helped me understand why it's designed this way"

✅ **Search:** "Google knows about each page and I found my answer"

---

## Next Steps

1. **Approve this plan** → Get buy-in from team
2. **Create templates** → Phase 1
3. **Staff the effort** → Assign package owners
4. **Set up automation** → JSDoc extraction, validation scripts
5. **Write core docs** → Phase 2
6. **Iterate & expand** → Phases 3-6
7. **Measure impact** → Track metrics, gather feedback

---

## Appendix: Diataxis Cheat Sheet

### When to Write What Type

**Tutorial:** User is new and wants to **learn by doing**
- Goal: Get them "unstuck" quickly
- Success: They complete a working example
- Question: "How do I get started?"

**How-To:** User has a **specific problem to solve**
- Goal: Give them the solution directly
- Success: They solve their problem faster
- Question: "How do I do X?"

**Reference:** User needs **complete information**
- Goal: Be comprehensive and precise
- Success: They find exactly what they need
- Question: "What does this do?"

**Explanation:** User wants to **understand concepts**
- Goal: Increase understanding, not solve a problem
- Success: They understand *why* things work this way
- Question: "Why does it work like this?"

### Common Mistakes to Avoid

❌ **Tutorial that explains concepts** → Split into tutorial + explanation
❌ **Reference trying to teach** → Move pedagogy to explanation/tutorial
❌ **How-to that's a mini-tutorial** → Keep it solution-focused
❌ **Explanation with step-by-step code** → Use a tutorial instead

---

**Document Status:** 📋 **PLAN - READY FOR IMPLEMENTATION**

**Last Updated:** 2025-12-05

**Owner:** Documentation Lead (TBD)
