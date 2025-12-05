# Diataxis Documentation Plan: 3 Levels Deep

This document summarizes the comprehensive Diataxis documentation strategy for UNRDF, organized in three levels of detail.

---

## Level 1: Strategic Overview (DIATAXIS-PLAN.md)

**What:** High-level strategy document
**Length:** ~12,000 words
**Purpose:** Answer "What are we building?"

**Contains:**
- ✅ Current state assessment (17 packages analyzed)
- ✅ Target Diataxis structure (4 directories per package)
- ✅ 12-week implementation roadmap (6 phases)
- ✅ Content guidelines for each type (tone, structure)
- ✅ Automation tools overview
- ✅ Success metrics and ownership model
- ✅ Risk mitigation strategies
- ✅ Integration with existing docs

**Audience:** Leadership, planning, stakeholders
**Use case:** "Should we do this? How long? Who pays?"

---

## Level 2: Operational Detail (Phase 1 & Core Roadmap)

### 2a. DIATAXIS-PHASE-1.md

**What:** Week-by-week detailed breakdown of foundation phase
**Length:** ~6,000 words
**Purpose:** Answer "How do we build the foundation?"

**Contains:**

**Week 1: Scaffolding & Automation**
- Day-by-day tasks (1.1 through 1.7)
- Create root-level Diataxis directories
- Create writing templates (4 types)
- Create package documentation template
- Create package initialization script (\`init-package-docs.sh\`)
- Create validation script (\`validate-diataxis.js\`)
- Update documentation standards
- Time estimates: 2-3 hours per task

**Week 2: Bootstrap @unrdf/core**
- Tasks 2.1 through 2.15
- Write all 16 documentation files
- 3 tutorials (15-30 minutes each)
- 4 how-to guides (5-10 minutes each)
- 5 reference documents (comprehensive)
- 4 explanation documents (15-20 minutes each)
- Time estimates: 3-12 hours per file

**Detailed breakdown:**
- Specific deliverables per day
- Exact time estimates
- Dependencies between tasks
- Acceptance criteria
- Sign-off requirements

**Audience:** Project managers, development teams
**Use case:** "What exactly do we do this week?"

---

### 2b. DIATAXIS-CORE-ROADMAP.md

**What:** Package-specific content roadmap for @unrdf/core
**Length:** ~8,000 words
**Purpose:** Answer "What exactly goes in each file?"

**Contains per tutorial:**
1. Learning outcomes (what readers understand after)
2. Content structure (exact sections with word counts)
3. Code examples needed (what to demonstrate)
4. Key concepts to introduce
5. Time estimates for reading and writing
6. Acceptance criteria

**Example: Tutorial 1 "Getting Started"**
- Goal: "Understand RDF basics, execute first SPARQL query in 15 minutes"
- Outcomes: Parse RDF, store triples, write SELECT query, read results
- Sections: Overview, Learning, Prerequisites, Before You Start, 8-10 steps, Verify, What You've Learned, Next Steps
- Code: Parse Turtle, create store, basic query, results iteration
- Write time: 6-8 hours
- Read time: 15-20 minutes

**Contains per how-to:**
1. Problem statement (what the reader is trying to do)
2. Solution sections (3-5 approaches)
3. Performance comparisons
4. Benchmarking code
5. Common pitfalls
6. "See also" links

**Example: How-To "Optimize Queries"**
- Problem: "My queries are slow. How do I speed them up?"
- Solutions: Simplify patterns, add filters, use LIMIT, avoid expensive patterns, use typed data
- Comparison table: Technique | Impact | Difficulty
- Benchmarking code: Complete function
- Write time: 6-8 hours
- Read time: 10-15 minutes

**Contains per reference:**
1. Every exported function documented
2. Function signature, parameters table, returns, throws
3. 2-3 code examples per function
4. Complete type definitions
5. All configuration options in table
6. Error code reference
7. Version migration guide

**Example: API Reference**
- Functions: createKnowledgeSubstrateCore, parseRdf, query, validateShacl, addQuad, removeQuad, etc.
- Per function: Signature | Params | Returns | Throws | Examples
- 15+ functions × 100-150 words = 2000+ words
- Write time: 10-12 hours

**Contains per explanation:**
1. Big picture (why this exists)
2. Conceptual foundations
3. Design decisions with trade-offs
4. Real-world analogies
5. Performance implications
6. When to use / when not to use
7. Future evolution possibilities

**Example: Architecture Explanation**
- Why layered design?
- Why backend abstraction?
- Why optional features?
- Diagram with each layer
- Data flow walkthrough
- Performance characteristics
- Write time: 6-8 hours

**Summary table:**
| Type | Files | Words | Hours |
|------|-------|-------|-------|
| Tutorials | 3 | 5000-6000 | 24-30 |
| How-To | 4 | 4000-5000 | 22-30 |
| Reference | 5 | 7000-8000 | 26-34 |
| Explanation | 4 | 6000-7000 | 21-28 |
| **Total** | **16** | **22000-26000** | **93-122** |

**Audience:** Writers, subject matter experts
**Use case:** "What do I write in this file?"

---

## Level 3: Concrete Implementation (Examples & Guides)

### 3a. DIATAXIS-EXAMPLES.md

**What:** Real working examples of all 4 Diataxis types
**Length:** ~5,000 words
**Purpose:** Answer "Show me exactly what this looks like"

**Contains:**

**1. Tutorial Example: "Your First SPARQL Query"**
- Full 15-minute tutorial (~2000 words)
- Complete code that runs
- Step-by-step progression
- Verify section showing expected output
- Shows exact tone and structure

**2. How-To Example: "Optimize Slow SPARQL Queries"**
- Full problem-solving guide (~1500 words)
- 5+ solutions with before/after code
- Performance comparison table
- Benchmarking code included
- Shows practical, direct tone

**3. Reference Example: "API Reference"**
- Complete example of function reference (~1000 words)
- Function signatures with all parameters
- Parameter table with types and defaults
- Return values documented
- 2-3 code examples per function
- Shows comprehensive, formal tone

**4. Explanation Example: "How @unrdf/core is Organized"**
- Conceptual walkthrough (~1500 words)
- Architecture diagram with layers
- Why each design decision was made
- Real-world analogy (house metaphor)
- Data flow walkthrough
- Shows educational, thoughtful tone

**Key insight:** These are not templates—they're real examples that show:
- Exact length and depth expected
- Appropriate tone for each type
- Code structure and complexity
- Link patterns
- How sections flow

**Audience:** Writers, reviewers
**Use case:** "I need to write something like this. Here's a reference implementation."

---

### 3b. DIATAXIS-GUIDE.md

**What:** Writing standards for each Diataxis type
**Length:** ~4,000 words
**Purpose:** Answer "How do I write this type of documentation?"

**Contains per type:**

**Tutorials:**
- Purpose: Learn by doing
- Audience: Newcomers
- Structure: Overview → Learning → Prerequisites → Steps → Verify → Summary → Next
- Length: 10-30 minutes
- Tone: Encouraging, step-by-step
- Common mistakes: ❌ Explaining concepts, ❌ Skipping steps, ❌ No verification

**How-To Guides:**
- Purpose: Solve specific problem
- Audience: Active developers
- Structure: Problem → Prerequisites → Solution (multiple approaches) → Issues → See Also
- Length: 3-10 minutes
- Tone: Direct, practical
- Common mistakes: ❌ Too long, ❌ Too many problems, ❌ Not solution-focused

**Reference:**
- Purpose: Complete information
- Audience: Active users looking up details
- Structure: Overview → Quick Example → Detailed sections → Parameters table → Types → Errors → See Also
- Length: No limit (completeness > brevity)
- Tone: Formal, precise, comprehensive
- Common mistakes: ❌ Teaching concepts, ❌ Abbreviating details, ❌ Incomplete coverage

**Explanation:**
- Purpose: Understand concepts
- Audience: Curious learners
- Structure: Big Picture → Definition → How It Works → Why This Design → Analogy → When to Use → See Also
- Length: 10-30 minutes (can be longer)
- Tone: Educational, thoughtful, generous
- Common mistakes: ❌ Step-by-step instructions, ❌ Being too brief, ❌ Focusing on "how to use"

**Bonus sections:**
- Quick decision tree (when to write what type)
- Consistency guidelines (tone, voice, code)
- Template checklist before publishing
- Tools and resources

**Audience:** Writers, editors, reviewers
**Use case:** "I'm writing a tutorial. What's the right structure?"

---

### 3c. DIATAXIS-MAP.md

**What:** Navigation hub for all documentation
**Length:** ~3,000 words
**Purpose:** Answer "Where do I find what I need?"

**Contains:**

**By User Type:**
- New users → Tutorials
- Building something → How-To
- Looking up details → Reference
- Want to understand → Explanation

**By Package:**
- All 17 packages listed with their Diataxis docs
- Links to TUTORIALS/ HOW-TO/ REFERENCE/ EXPLANATION/ in each package

**By Use Case:**
- Building a blog platform → Specific tutorial + how-to path
- Knowledge management system → Specific path
- Real-time collaboration → Specific path
- Semantic search engine → Specific path

**By Topic:**
- RDF & Data → Related docs
- Querying → Related docs
- Validation → Related docs
- Performance → Related docs
- Integration → Related docs

**By Role:**
- End user: START-HERE → Tutorials → How-To → Reference
- Developer: Quick start → Specific package docs → API reference
- Contributor: Monorepo overview → Local dev → Package development
- DevOps: Architecture → Deployment → Performance

**Quick reference table:**
- I want to... | Read this | Time
- Get started | Getting Started Tutorial | 15 min
- Learn workflow | Basic Workflow Tutorial | 25 min
- Look up API | API Reference | 2 min
- Understand design | Architecture Explanation | 15 min
- Etc.

**Audience:** Users, support staff
**Use case:** "I need help. Where do I start?"

---

## Level 4: Automation & Validation

### DIATAXIS-PHASE-1.md includes scripts:

**1. init-package-docs.sh**
```bash
./scripts/init-package-docs.sh @unrdf/core
```
Creates complete Diataxis directory structure:
- TUTORIALS/ with 3 placeholders
- HOW-TO/ with 4 placeholders
- REFERENCE/ and EXPLANATION/ directories
- All necessary index files
- Ready to fill with content

**2. validate-diataxis.js**
```bash
pnpm run validate:docs
```
Checks documentation against standards:
- ✅ All 4 Diataxis directories exist
- ✅ Minimum files in each directory
- ✅ No TODO/FIXME placeholders
- ✅ Directory indexes present
- Scores: 0-100%
- Per-package results

---

## How These 3 Levels Interact

```
Level 1: DIATAXIS-PLAN.md
├─ "What are we building?"
├─ Who: Stakeholders, leadership
└─ Duration: 12 weeks, 6 phases

  ↓ Uses

Level 2a: DIATAXIS-PHASE-1.md
├─ "How do we build phase 1?"
├─ Who: Project managers, developers
└─ Duration: 2 weeks detailed breakdown

  ↓ Uses + Follows

Level 2b: DIATAXIS-CORE-ROADMAP.md
├─ "What goes in @unrdf/core docs?"
├─ Who: Content writers, SMEs
└─ Maps: 16 files, 22000-26000 words

  ↓ Follows structure from

Level 3a: DIATAXIS-EXAMPLES.md
├─ "Here's what it actually looks like"
├─ Who: Writers as reference
└─ Shows: 4 complete working examples

  ↓ Uses rules from

Level 3b: DIATAXIS-GUIDE.md
├─ "Here's how to write each type"
├─ Who: Writers, editors
└─ For: Tutorials, How-To, Reference, Explanation

  ↓ Users navigate with

Level 3c: DIATAXIS-MAP.md
├─ "Here's where everything is"
├─ Who: All users of docs
└─ Organizes: All 17 packages × 4 types
```

---

## What's Documented (Complete Inventory)

### Documentation Documents (Created)

1. **DIATAXIS-PLAN.md** (12,000 words)
   - Strategic overview and 12-week roadmap
   - Current state assessment
   - Phase breakdown
   - Success criteria

2. **DIATAXIS-PHASE-1.md** (6,000 words)
   - Week-by-week detailed tasks
   - 15 specific tasks with time estimates
   - Automation script specifications
   - Acceptance criteria

3. **DIATAXIS-CORE-ROADMAP.md** (8,000 words)
   - 16 documentation files detailed
   - 22000-26000 words total mapped
   - 93-122 hours of writing assignments
   - Content outline for every file

4. **DIATAXIS-EXAMPLES.md** (5,000 words)
   - 4 complete working examples
   - Tutorial, How-To, Reference, Explanation
   - Shows exact structure and tone
   - Serves as reference implementation

5. **DIATAXIS-GUIDE.md** (4,000 words)
   - Writing standards for each type
   - Structure, length, tone per type
   - Common mistakes and how to avoid
   - Template checklist

6. **DIATAXIS-MAP.md** (3,000 words)
   - Navigation hub for all docs
   - Organized by user type, package, use case, topic, role
   - Quick reference tables
   - Entry points for different audiences

### Previously Created

7. **DIATAXIS-PLAN.md** (mentioned above)
8. **DIATAXIS-GUIDE.md** (mentioned above)
9. **DIATAXIS-MAP.md** (mentioned above)
10. **PACKAGE-INDEX.md template** (in docs/_templates/)

---

## Knowledge Encoded

### At Level 1 (Strategic)
- Why Diataxis matters
- Current gaps in documentation
- Long-term vision
- Ownership model
- Risk mitigation

### At Level 2a (Phase 1)
- Day-by-day execution plan
- Specific tasks and outputs
- Skill requirements
- Dependencies
- Quality gates

### At Level 2b (Core Roadmap)
- What goes in each file
- Specific sections and word counts
- Code examples needed
- Learning outcomes per file
- Time to read/write per file

### At Level 3a (Examples)
- Real working documentation
- Exact tone for each type
- Code structure and complexity
- Length and depth expectations
- How sections flow together

### At Level 3b (Guide)
- How to write each type
- What makes good tutorials
- What makes good how-tos
- What makes good references
- What makes good explanations

### At Level 3c (Map)
- Where everything lives
- How users navigate
- Quick lookup tables
- Entry points by role
- Cross-references

---

## Implementation Path

### Option 1: Start with Level 3 (Examples)
- **Best for:** Teams that learn by doing
- **Start:** Read DIATAXIS-EXAMPLES.md
- **Then:** Read DIATAXIS-GUIDE.md
- **Then:** Check Phase 1 for tasks
- **Finally:** Execute tasks

### Option 2: Start with Level 1 (Strategy)
- **Best for:** Teams that plan first
- **Start:** Read DIATAXIS-PLAN.md
- **Then:** Read DIATAXIS-PHASE-1.md
- **Then:** Review Core Roadmap
- **Finally:** Execute Phase 1

### Option 3: Start with Level 2 (What to Write)
- **Best for:** Teams ready to write
- **Start:** Read DIATAXIS-CORE-ROADMAP.md
- **Then:** Read DIATAXIS-EXAMPLES.md for reference
- **Then:** Use DIATAXIS-GUIDE.md while writing
- **Finally:** Run validation

---

## Total Deliverables

| Document | Words | Purpose |
|----------|-------|---------|
| DIATAXIS-PLAN.md | 12,000 | Strategic overview |
| DIATAXIS-PHASE-1.md | 6,000 | Week-by-week breakdown |
| DIATAXIS-CORE-ROADMAP.md | 8,000 | Package-specific content |
| DIATAXIS-EXAMPLES.md | 5,000 | Working examples |
| DIATAXIS-GUIDE.md | 4,000 | Writing standards |
| DIATAXIS-MAP.md | 3,000 | Navigation hub |
| **Total Planning Docs** | **38,000** | **Complete strategy** |
| Plus: Core package content | 22,000-26,000 | To be written |
| **Grand Total** | **60,000-64,000** | **Complete system** |

---

## Next Steps

1. **Review** all documents
2. **Assign** Phase 1 team
3. **Execute** Week 1 (scaffolding)
4. **Execute** Week 2 (bootstrap @unrdf/core)
5. **Validate** using validate-diataxis.js
6. **Repeat** for Phases 2-6 (other packages)

---

**Status:** ✅ Complete planning documentation
**Ready to:** Execute Phase 1
**Files committed:** 6 documents, ~38,000 words
**Effort documented:** 93-122 hours for Phase 1, 600+ hours total

---

This 3-levels-deep planning ensures every writer, manager, and team member can find the right level of detail for their role.
