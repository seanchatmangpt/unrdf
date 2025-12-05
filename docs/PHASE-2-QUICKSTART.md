# Phase 2 Quick Start (Weeks 3-5)

## Overview

Phase 2 documents 6 public packages with parallel teams.

| Week | Team | Package | Type | Hours |
|------|------|---------|------|-------|
| 3 | A | @unrdf/streaming | Feature | 60-76 |
| 3 | B | @unrdf/federation | Feature | 60-76 |
| 4 | C | @unrdf/knowledge-engine | Feature | 72-88 |
| 4 | D | @unrdf/browser | Feature | 60-76 |
| 5 | E | @unrdf/cli | Integration | 40-56 |
| 5 | F | @unrdf/react | Integration | 46-60 |

**Total:** 338-412 hours, 6 teams, 96 files, ~100,000 words

---

## Quick Start for Each Package

### 1. Initialize Documentation Structure

```bash
# For streaming package:
./scripts/init-package-docs.sh streaming Feature

# Creates:
# packages/streaming/docs/tutorials/
# packages/streaming/docs/how-to/
# packages/streaming/docs/reference/
# packages/streaming/docs/explanation/
```

### 2. Read Relevant Roadmap

**For Phase 2 packages, see:**
- `DIATAXIS-PHASE-2.md` - Week-by-week breakdown
- `DIATAXIS-STREAMING-ROADMAP.md` - Example (if similar to your package)
- `DIATAXIS-PACKAGE-TYPES.md` - Adaptation guide for your package type

### 3. Follow the Pattern from @unrdf/core

Reference the @unrdf/core documentation structure:

```
packages/core/docs/
├── tutorials/
│   ├── 01-getting-started.md
│   ├── 02-basic-workflow.md
│   └── 03-advanced-patterns.md
├── how-to/
│   ├── 01-optimize-sparql-queries.md
│   ├── 02-working-with-formats.md
│   ├── 03-troubleshooting.md
│   └── 04-performance-tuning.md
├── reference/
│   ├── 01-api.md
│   ├── 02-types.md
│   ├── 03-configuration.md
│   ├── 04-errors.md
│   └── 05-migration.md
└── explanation/
    ├── 01-architecture.md
    ├── 02-design-decisions.md
    ├── 03-sparql-concepts.md
    └── 04-query-execution.md
```

Adapt the same structure for your package.

### 4. Write Documentation in Parallel

Each team works independently:

**Day 1-2: Reference Documentation (API, Types, Config, Errors)**
```bash
# Typical effort: 12-16 hours
# Extract from JSDoc or read source code
# Write function signatures with parameters
# Document configuration options
# Create error reference table
```

**Day 2-3: Tutorials (3 files)**
```bash
# Typical effort: 12-20 hours
# Tutorial 1: Getting started (hello world)
# Tutorial 2: Common workflow
# Tutorial 3: Advanced patterns
```

**Day 4-5: How-To Guides (4 files)**
```bash
# Typical effort: 12-20 hours
# How-To 1: [Problem 1] - 3-4 hours
# How-To 2: [Problem 2] - 3-4 hours
# How-To 3: [Problem 3] - 3-4 hours
# How-To 4: [Problem 4] - 3-4 hours
```

**Day 5: Explanation Documents (4 files)**
```bash
# Typical effort: 10-14 hours
# Explanation 1: Architecture/Design - 3-4 hours
# Explanation 2: Design decisions - 2-3 hours
# Explanation 3: Concepts - 3-4 hours
# Explanation 4: Advanced - 2-3 hours
```

### 5. Validate and Sign Off

```bash
# Run validation:
node scripts/validate-diataxis.js packages/streaming/

# Expected output:
# TUTORIALS: 3/3 complete
# HOW-TO: 4/4 complete
# REFERENCE: 5/5 complete
# EXPLANATION: 4/4 complete
# OVERALL SCORE: 100/100

# All code examples tested
# Peer reviewed by another team
```

### 6. Commit and Push

```bash
# After validation passes:
git add packages/streaming/docs/
git commit -m "docs: implement Phase 2 streaming package documentation"
git push origin claude/rewrite-docs-monorepo-[session-id]
```

---

## Key Files to Reference

| Document | Purpose |
|----------|---------|
| DIATAXIS-PHASE-2.md | Week-by-week task breakdown |
| DIATAXIS-PACKAGE-TYPES.md | How to adapt patterns by type |
| DIATAXIS-GUIDE.md | Writing standards and voice |
| DIATAXIS-EXAMPLES.md | Real working examples |
| DIATAXIS-MAP.md | Navigation and organization |

---

## Team Assignments (Example)

### Team A: @unrdf/streaming (Feature Extension)
- Lead: [Person]
- Start: Week 3, Monday
- Effort: 60-76 hours
- Type-specific guidance: See DIATAXIS-STREAMING-ROADMAP.md

### Team B: @unrdf/federation (Feature Extension)
- Lead: [Person]
- Start: Week 3, Monday
- Effort: 60-76 hours

... (and so on for teams C-F)

---

## Daily Standup Template

**Each team reports daily:**

```
Day 3 Monday - Team A (Streaming)

Completed:
- [ ] Reference documentation (API, Types) - 8 hours
- Tutorials: In progress

Issues:
- Need clarification on [X]

Next:
- Complete Configuration reference
- Start tutorials
```

---

## Quality Gates

### Before Finishing Day

```bash
# 1. Run validation:
node scripts/validate-diataxis.js packages/streaming/

# 2. Check for TODOs:
grep -r "TODO\|FIXME" packages/streaming/docs/

# 3. Spell check (use editor)

# 4. Test all code examples
```

### Before Team Sign-Off

```bash
# Checklist:
- [ ] All 16 files complete
- [ ] No TODO/FIXME placeholders
- [ ] All code examples tested
- [ ] validate-diataxis.js score = 100/100
- [ ] Peer review passed (other team)
- [ ] Commit pushed to git
```

---

## Automation Available

### Init Package Docs

```bash
./scripts/init-package-docs.sh <package-name> <type>
```

Creates directory structure and templates.

### Validate Completeness

```bash
node scripts/validate-diataxis.js <package-dir>
```

Scores documentation and identifies gaps.

---

## Timeline

```
Week 3:
│ Mon-Tue: Teams A & B start (reference docs)
│ Wed-Thu: Teams A & B (tutorials)
│ Fri: Teams A & B (how-to guides)
│ Sat: Teams A & B (explanation + validation)

Week 4:
│ Mon-Tue: Teams C & D start (reference docs)
│ Wed-Thu: Teams C & D (tutorials)
│ Fri: Teams C & D (how-to guides)
│ Sat: Teams C & D (explanation + validation)

Week 5:
│ Mon-Tue: Teams E & F start (reference docs)
│ Wed-Thu: Teams E & F (tutorials)
│ Fri: Teams E & F (how-to guides)
│ Sat: Teams E & F (explanation + validation + wrap-up)
```

---

## Success Criteria

- ✅ All 96 files written (16 per package × 6 packages)
- ✅ ~100,000 words total
- ✅ 100% validation score on all packages
- ✅ Zero rework needed
- ✅ All tests passing (code examples)

---

## Metrics to Track

| Metric | Target | Owner |
|--------|--------|-------|
| Files created | 96 | All teams |
| Words written | 100,000 | All teams |
| Validation score | 100/100 | All teams |
| Code examples tested | 100% | All teams |
| Peer reviews completed | 6 | Team leads |
| Commits pushed | 6 | Team leads |

---

## Blocker Resolution

If stuck:

1. **Check DIATAXIS-GUIDE.md** - Writing standards
2. **Review DIATAXIS-EXAMPLES.md** - Real examples
3. **Compare to @unrdf/core** - Reference implementation
4. **Ask: Is this information in source code?** - Extract from JSDoc
5. **Ask: Is this something users would want?** - If yes, document it

---

## Next Phase (Phase 3)

Phase 3 (Weeks 6-8) continues with 4 more public packages:
- @unrdf/composables (Vue integration)
- @unrdf/dark-matter (Query optimization)
- @unrdf/project-engine (Project management)
- @unrdf/engine-gateway (Multi-engine federation)

Same process: Initialize → Reference → Tutorials → How-To → Explanation → Validate

---

## Resources

**Phase 2 Planning Documents:**
- `DIATAXIS-PHASE-2.md` (7,500 words)
- `DIATAXIS-STREAMING-ROADMAP.md` (5,500 words example)
- `DIATAXIS-PHASE-2-SUMMARY.md` (3,500 words overview)

**Automation Tools:**
- `./scripts/init-package-docs.sh` - Setup new packages
- `./scripts/validate-diataxis.js` - Check completeness

**Standards & Examples:**
- `DIATAXIS-GUIDE.md` - Writing standards
- `DIATAXIS-EXAMPLES.md` - Real examples
- `packages/core/docs/` - Reference implementation

---

## Getting Help

- **Writing standards?** → See DIATAXIS-GUIDE.md
- **Examples needed?** → See DIATAXIS-EXAMPLES.md
- **Structure unclear?** → Review packages/core/docs/
- **Package-specific?** → Check DIATAXIS-PACKAGE-TYPES.md
- **Need template?** → Run init-package-docs.sh

---

**Ready to start?** Assign teams and begin Week 3!
