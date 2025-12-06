# UNRDF Documentation - Diátaxis 360 Matrix

## Overview

The UNRDF documentation follows the [Diátaxis framework](https://diataxis.fr/), organizing content into four distinct quadrants based on user needs and learning stages.

```
                    PRACTICAL ←→ THEORETICAL

LEARNING    │  TUTORIALS        │  EXPLANATION     │
ORIENTED    │  Learning by doing│  Understanding   │
            │  Step-by-step     │  Concepts        │
            ├─────────────────────────────────────┤
WORK        │  HOW-TO GUIDES   │  REFERENCE       │
ORIENTED    │  Problem solving │  Information     │
            │  Task recipes    │  Technical specs │
```

---

## 1. TUTORIALS 📚 (Learning-Oriented)

**Purpose**: Help beginners learn by doing through step-by-step lessons.
**Audience**: New users, first-time learners
**Style**: "Follow along", confidence-building, safe exploration

### Current Content

| Tutorial | Path | Status | Description |
|----------|------|--------|-------------|
| **Getting Started** | `/guides/getting-started/` | ✅ Live | Install UNRDF and create first RDF graph |
| **Quick Start** | `/guides/quick-start/` | ✅ Live | 5-minute introduction to core features |

### Missing (Priority)

| Tutorial | Proposed Path | Description |
|----------|---------------|-------------|
| **Your First Knowledge Graph** | `/tutorials/first-knowledge-graph/` | Build a person-company-product graph step-by-step |
| **Using Hooks** | `/tutorials/using-hooks/` | Add reactive hooks to your graph |
| **Streaming Data** | `/tutorials/streaming-rdf/` | Process SPARQL results as streams |
| **Building a Chatbot** | `/tutorials/chatbot-with-kg/` | Combine LLM + knowledge graph |

---

## 2. HOW-TO GUIDES 🛠️ (Task-Oriented)

**Purpose**: Solve specific problems users face in real work.
**Audience**: Practitioners with basic knowledge
**Style**: "Here's how to...", assumes context, goal-focused

### Current Content

| Guide | Path | Status | Description |
|-------|------|--------|-------------|
| **Installation** | `/guides/installation/` | ✅ Live | Install from npm, build from source |

### Missing (Priority)

| Guide | Proposed Path | Description |
|-------|---------------|-------------|
| **Query RDF Data** | `/how-to/query-rdf/` | SPARQL queries, filters, aggregation |
| **Import Data** | `/how-to/import-data/` | Load from Turtle, JSON-LD, CSV |
| **Export Results** | `/how-to/export-results/` | Export to various formats |
| **Performance Tuning** | `/how-to/optimize-performance/` | Indexing, caching, benchmarking |
| **Deploy to Production** | `/how-to/deploy-production/` | Docker, scaling, monitoring |
| **Integrate with AI** | `/how-to/integrate-llm/` | OpenAI, Claude, embeddings |
| **Handle Large Datasets** | `/how-to/large-datasets/` | Chunking, streaming, pagination |
| **Migrate from N3** | `/how-to/migrate-from-n3/` | Convert N3.js code to UNRDF |

---

## 3. REFERENCE 📖 (Information-Oriented)

**Purpose**: Provide accurate, complete technical information.
**Audience**: Developers needing to look up specific details
**Style**: Dry, precise, comprehensive, searchable

### Current Content

| Reference | Path | Status | Description |
|-----------|------|--------|-------------|
| **Core API** | `/api/core/` | ✅ Live | Store, DataFactory, Quad operations |
| **Streaming API** | `/api/streaming/` | ✅ Live | Stream utilities, pipeline operations |
| **Knowledge Engine** | `/api/knowledge-engine/` | ✅ Live | Query engine, reasoning |
| **Hooks API** | `/api/hooks/` | ✅ Live | Event hooks, reactive patterns |
| **Oxigraph API** | `/api/oxigraph/` | ✅ Live | Oxigraph store integration |

### Missing (Priority)

| Reference | Proposed Path | Description |
|-----------|---------------|-------------|
| **Configuration** | `/reference/configuration/` | All config options, environment variables |
| **CLI Commands** | `/reference/cli/` | All command-line tools and flags |
| **Error Codes** | `/reference/errors/` | Complete error catalog with solutions |
| **Data Formats** | `/reference/formats/` | Supported RDF formats (Turtle, JSON-LD, etc.) |
| **SPARQL Support** | `/reference/sparql/` | Supported SPARQL features, extensions |
| **Type Definitions** | `/reference/types/` | TypeScript type reference |

---

## 4. EXPLANATION 💡 (Understanding-Oriented)

**Purpose**: Deepen understanding of concepts, design choices, theory.
**Audience**: Users wanting to understand "why" and "how it works"
**Style**: Discursive, reflective, connects ideas

### Current Content

| Topic | Path | Status | Description |
|-------|------|--------|-------------|
| **KGC-4D Overview** | `/concepts/kgc-4d/` | ✅ Live | 4-dimensional knowledge graph construction |
| **KGC-4D Mathematics** | `/concepts/kgc-4d/mathematics/` | ✅ Live | Mathematical foundations |
| **Forensic UX** | `/concepts/kgc-4d/forensic-ux/` | ✅ Live | UX patterns and debugging |
| **RDF Fundamentals** | `/concepts/rdf-fundamentals/` | ✅ Live | What is RDF, why use it |
| **Architecture** | `/concepts/architecture/` | ✅ Live | System design, components |
| **Research Papers** | `/papers/*` | ✅ Live | 6 academic papers (1,538 lines) |

### Missing (Priority)

| Topic | Proposed Path | Description |
|-------|---------------|-------------|
| **Why UNRDF?** | `/explanation/why-unrdf/` | Design philosophy, trade-offs vs alternatives |
| **Event Sourcing** | `/explanation/event-sourcing/` | Why immutable events, benefits |
| **Hyperdimensional Computing** | `/explanation/hyperdimensional/` | HDC theory, applications to RDF |
| **μ(O) Calculus Explained** | `/explanation/mu-calculus/` | Non-technical introduction to μ(O) |
| **Performance Model** | `/explanation/performance/` | Why UNRDF is fast (40% faster, 60% less memory) |
| **Design Decisions** | `/explanation/design-decisions/` | Architectural decision records (ADRs) |

---

## 5. SUPPLEMENTARY 🎯 (Cross-Cutting)

### Playground

| Tool | Path | Status | Type |
|------|------|--------|------|
| **μ(O) Dashboard** | `/playground/` | ✅ Live | Interactive demo |

**Classification**: Hybrid (Tutorial + How-To hybrid)
**Purpose**: Hands-on exploration without setup

### Papers

| Section | Path | Status | Type |
|---------|------|--------|------|
| **Research Papers Index** | `/papers/` | ✅ Live | Explanation |
| **6 KGC-4D Papers** | `/papers/2025-*` | ✅ Live | Explanation |

**Classification**: Explanation (deep theory)
**Audience**: Researchers, academic users

---

## Implementation Plan

### Phase 1: Reorganization (Week 1)

1. **Create new top-level structure**:
   ```
   /tutorials/          (new)
   /how-to/            (new)
   /reference/         (rename from /api/)
   /explanation/       (rename from /concepts/)
   /playground/        (keep)
   /papers/            (keep under /explanation/)
   ```

2. **Move existing content**:
   - `/guides/getting-started/` → `/tutorials/getting-started/`
   - `/guides/quick-start/` → `/tutorials/quick-start/`
   - `/guides/installation/` → `/how-to/installation/`
   - `/api/*` → `/reference/api/*`
   - `/concepts/*` → `/explanation/*`

3. **Update navigation** (`_meta.ts`):
   ```typescript
   export default {
     index: 'Home',
     tutorials: 'Tutorials 📚',
     'how-to': 'How-To Guides 🛠️',
     reference: 'Reference 📖',
     explanation: 'Explanation 💡',
     playground: 'Playground 🎯',
   }
   ```

### Phase 2: Fill Gaps (Week 2-3)

Priority order based on user needs:

1. **How-To Guides** (most requested):
   - Query RDF Data
   - Import/Export Data
   - Performance Tuning
   - Deploy to Production

2. **Tutorials** (onboarding):
   - Your First Knowledge Graph
   - Using Hooks
   - Streaming Data

3. **Reference** (completeness):
   - Configuration Reference
   - CLI Reference
   - Error Codes

4. **Explanation** (depth):
   - Why UNRDF?
   - Performance Model
   - Design Decisions

### Phase 3: Cross-Linking (Week 4)

Add navigation between quadrants:

```mdx
<Callout type="info">
**Related**:
- 📚 Tutorial: [Getting Started](/tutorials/getting-started)
- 🛠️ How-To: [Query RDF Data](/how-to/query-rdf)
- 📖 Reference: [Core API](/reference/api/core)
- 💡 Explanation: [RDF Fundamentals](/explanation/rdf-fundamentals)
</Callout>
```

---

## Success Metrics

| Metric | Current | Target | How to Measure |
|--------|---------|--------|----------------|
| **Tutorials** | 2 | 4 | New user completion rate |
| **How-To Guides** | 1 | 8 | Task success rate |
| **Reference Pages** | 5 | 11 | API coverage % |
| **Explanation Topics** | 6 | 12 | Conceptual understanding surveys |
| **Cross-Links** | ~5% | 80% | Pages with ≥2 cross-quadrant links |
| **User Satisfaction** | - | 4.5/5 | Feedback scores |

---

## Maintenance

### Quarterly Review

- Check if content is in correct quadrant
- Identify gaps based on user questions
- Update cross-links as content evolves
- Archive outdated content

### Content Guidelines

**For each new page, ask**:
1. Is the user learning or working? → Tutorials vs How-To/Reference/Explanation
2. Is it practical or theoretical? → Tutorials/How-To vs Reference/Explanation

**If both**:
- Split into multiple pages (one per quadrant)
- Example: "Using Hooks" tutorial + "Hook Patterns" explanation + "Hooks API" reference

---

## References

- [Diátaxis Framework](https://diataxis.fr/)
- [Write the Docs: Diátaxis](https://www.writethedocs.org/videos/eu/2017/the-four-kinds-of-documentation-and-why-you-need-to-understand-what-they-are-daniele-procida/)
- [Good Docs Project](https://www.thegooddocsproject.dev/)
