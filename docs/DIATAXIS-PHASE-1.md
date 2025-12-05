# Phase 1: Foundation (Detailed Week-by-Week Breakdown)

**Duration:** 2 weeks
**Goal:** Create structure and templates so packages 2-6 can implement docs in parallel
**Effort:** ~80-100 hours (can be done by 1-2 people)

---

## Week 1: Scaffolding & Automation

### Day 1-2: Create Base Structure

#### Task 1.1: Create Root-Level Diataxis Directories
```bash
mkdir -p docs/TUTORIALS
mkdir -p docs/HOW-TO
mkdir -p docs/REFERENCE
mkdir -p docs/EXPLANATION
mkdir -p docs/PACKAGE-GUIDES
mkdir -p docs/_templates
mkdir -p docs/_scripts
```

**Subtasks:**
- [ ] Create `.gitkeep` files in each directory to preserve structure
- [ ] Create `docs/TUTORIALS/README.md` - Index of all tutorials
- [ ] Create `docs/HOW-TO/README.md` - Index of all how-to guides
- [ ] Create `docs/REFERENCE/README.md` - Index of all references
- [ ] Create `docs/EXPLANATION/README.md` - Index of all explanations

**Time estimate:** 2-3 hours

#### Task 1.2: Create Root-Level Diataxis Navigation
**Files to create/update:**
- [ ] `docs/DIATAXIS-MAP.md` ← Already created in previous work
- [ ] `docs/START-HERE.md` → Add link to DIATAXIS-MAP
- [ ] `README.md` → Add "Documentation by Type" section
- [ ] `CONTRIBUTING.md` → Add "Documentation Standards" section

**Content to write:**
```markdown
# Documentation Standards

All documentation must follow Diataxis:
- **Tutorials:** Learning by doing (see DIATAXIS-GUIDE.md)
- **How-To:** Problem solving (see DIATAXIS-GUIDE.md)
- **Reference:** Complete information (see DIATAXIS-GUIDE.md)
- **Explanation:** Understanding (see DIATAXIS-GUIDE.md)

Mixing types makes docs confusing.
```

**Time estimate:** 3-4 hours

### Day 2-3: Create Templates

#### Task 1.3: Create Diataxis Writing Templates

Create in `docs/_templates/`:

**TUTORIAL.template.md** (800 words, shows structure)
```markdown
# Tutorial: [Concrete Task]

In this tutorial, you'll [specific skill] by [practical approach].

## What You'll Learn
- Skill 1
- Skill 2
- Skill 3

## What You'll Build
[Describe end result]

## Prerequisites
[List requirements]

## Before You Start
[Setup steps]

## Step 1: [Clear Title]
[Explanation]
\`\`\`javascript
// Code
\`\`\`
[Why this matters]

[Continue steps...]

## Verify It Works
[How to test]

## What You've Learned
[Summary checkmarks]

## Next Steps
[Links to related docs]
```

**HOW-TO.template.md** (500 words, shows structure)
```markdown
# How To: [Specific Problem]

## Problem
[Clearly state it]

## Prerequisites
[What reader needs to know]

## Solution

### Approach 1: [Recommended]
[Code + explanation]

### Approach 2: [Alternative]
[When to use this]

## Common Issues
- Issue 1: Solution
- Issue 2: Solution

## See Also
[Links]
```

**REFERENCE.template.md** (1000+ words, shows structure)
```markdown
# Reference: [Topic]

## Overview
[What is this?]

## Quick Example
\`\`\`
[Minimal example]
\`\`\`

## [Function Name]()

### Signature
\`\`\`
function signature
\`\`\`

### Parameters
[Table or list]

### Returns
[Description]

### Examples
[Code examples]

### Throws
[Error types]

[Continue for each function...]

## Type Reference
[All types]

## Error Reference
[Table of errors]

## See Also
[Links]
```

**EXPLANATION.template.md** (800 words, shows structure)
```markdown
# Explanation: [Concept]

## The Big Picture
[Context]

## What Is [Concept]?
[Definition]

## How It Works

### Mechanism 1
[Details]

### Mechanism 2
[Details]

## Why This Design?

### Decision 1
[Trade-offs]

### Decision 2
[Rationale]

## Real-World Analogy
[Comparison]

## When (and When Not) to Use This
[Applicability]

## Performance Implications
[Relevant metrics]

## See Also
[Links]
```

**Time estimate:** 4-5 hours

#### Task 1.4: Create Package Documentation Template

**`docs/_templates/PACKAGE-INDEX.md`** ← Already created in previous work

**Create:** `docs/_templates/PACKAGE-STRUCTURE.txt`
```
packages/NEW-PACKAGE/docs/
├── INDEX.md                          (Use PACKAGE-INDEX.md template)
├── CONTRIBUTING.md                   (Contributor guidelines)
├── TUTORIALS/
│   ├── README.md                     (Overview of tutorials)
│   ├── 01-getting-started.md         (First experience, 15-20 min)
│   ├── 02-basic-workflow.md          (Common patterns, 20-25 min)
│   ├── 03-advanced-patterns.md       (Next level, 25-30 min)
│   └── .gitkeep
├── HOW-TO/
│   ├── README.md                     (Overview of guides)
│   ├── common-task-1.md              (Problem-specific)
│   ├── common-task-2.md
│   ├── troubleshooting.md            (Common issues & fixes)
│   ├── performance-tuning.md         (Speed optimization)
│   └── .gitkeep
├── REFERENCE/
│   ├── README.md                     (Overview of reference)
│   ├── API.md                        (All functions, types, options)
│   ├── TYPES.md                      (Type definitions, schemas)
│   ├── CONFIGURATION.md              (All config options)
│   ├── ERRORS.md                     (Error codes & meanings)
│   └── .gitkeep
└── EXPLANATION/
    ├── README.md                     (Overview of explanations)
    ├── architecture.md               (How it's organized)
    ├── design-decisions.md           (Why this approach?)
    ├── concepts.md                   (Key ideas)
    ├── performance.md                (Performance details)
    └── .gitkeep
```

**Time estimate:** 1-2 hours

### Day 3-4: Create Automation

#### Task 1.5: Create Package Initialization Script

**File:** `scripts/init-package-docs.sh`

```bash
#!/bin/bash
# Usage: ./scripts/init-package-docs.sh @unrdf/core
# Initializes complete Diataxis structure for a package

PACKAGE_NAME=$1
PACKAGE_DIR="packages/${PACKAGE_NAME#@unrdf/}"

if [ -z "$PACKAGE_NAME" ]; then
  echo "Usage: init-package-docs.sh <@unrdf/package-name>"
  exit 1
fi

if [ ! -d "$PACKAGE_DIR" ]; then
  echo "Error: Package directory not found: $PACKAGE_DIR"
  exit 1
fi

# Create structure
mkdir -p "$PACKAGE_DIR/docs"/{TUTORIALS,HOW-TO,REFERENCE,EXPLANATION}

# Copy templates
cp docs/_templates/PACKAGE-INDEX.md "$PACKAGE_DIR/docs/INDEX.md"

# Create directory README files
for dir in TUTORIALS HOW-TO REFERENCE EXPLANATION; do
  cat > "$PACKAGE_DIR/docs/$dir/README.md" <<EOF
# $dir

[To be completed]

See [docs/DIATAXIS-GUIDE.md](../../docs/DIATAXIS-GUIDE.md) for writing guidelines.
EOF
  touch "$PACKAGE_DIR/docs/$dir/.gitkeep"
done

# Create stub content files
for i in {01..03}; do
  touch "$PACKAGE_DIR/docs/TUTORIALS/$i-title.md"
done

for i in {01..04}; do
  touch "$PACKAGE_DIR/docs/HOW-TO/$i-title.md"
done

# Add .gitkeep to ensure Git preserves empty directories
touch "$PACKAGE_DIR/docs/.gitkeep"

echo "✅ Initialized docs structure for $PACKAGE_NAME"
echo "📍 Next: Fill in content in $PACKAGE_DIR/docs/"
echo ""
echo "Quick start:"
echo "  vim $PACKAGE_DIR/docs/INDEX.md"
echo "  vim $PACKAGE_DIR/docs/TUTORIALS/01-title.md"
```

**Make it executable:**
```bash
chmod +x scripts/init-package-docs.sh
```

**Test it:**
```bash
./scripts/init-package-docs.sh @unrdf/core
ls -R packages/core/docs/
```

**Time estimate:** 2-3 hours

#### Task 1.6: Create Validation Script

**File:** `scripts/validate-diataxis.js`

```javascript
#!/usr/bin/env node

/**
 * Validate documentation against Diataxis standards
 * Usage: node scripts/validate-diataxis.js [package]
 */

import { readdirSync, statSync, readFileSync } from 'fs';
import { join } from 'path';

const REQUIRED_DIRS = ['TUTORIALS', 'HOW-TO', 'REFERENCE', 'EXPLANATION'];
const MIN_FILES = { TUTORIALS: 1, 'HOW-TO': 1, REFERENCE: 1, EXPLANATION: 1 };

function validatePackage(packagePath) {
  const issues = [];
  const score = { total: 0, passed: 0 };

  // Check Diataxis directories exist
  for (const dir of REQUIRED_DIRS) {
    score.total += 20;
    const dirPath = join(packagePath, 'docs', dir);
    try {
      const stat = statSync(dirPath);
      if (stat.isDirectory()) {
        score.passed += 20;
      } else {
        issues.push(`❌ ${dir} exists but is not a directory`);
      }
    } catch {
      issues.push(`❌ Missing directory: docs/${dir}`);
    }
  }

  // Check file count in each directory
  for (const dir of REQUIRED_DIRS) {
    score.total += 20;
    const dirPath = join(packagePath, 'docs', dir);
    try {
      const files = readdirSync(dirPath).filter(f => f.endsWith('.md'));
      if (files.length >= MIN_FILES[dir]) {
        score.passed += 20;
      } else {
        issues.push(
          `⚠️  ${dir} has only ${files.length} files (need >= ${MIN_FILES[dir]})`
        );
      }
    } catch {
      issues.push(`❌ Cannot read ${dir} directory`);
    }
  }

  // Check for README.md files
  score.total += 20;
  const hasMainREADME =
    statSync(join(packagePath, 'docs', 'README.md')).isFile() ||
    statSync(join(packagePath, 'docs', 'INDEX.md')).isFile();
  if (hasMainREADME) {
    score.passed += 20;
  } else {
    issues.push('⚠️  Missing docs/README.md or docs/INDEX.md');
  }

  // Check for TODO placeholders (should be none in complete docs)
  score.total += 20;
  const allFiles = getAllMarkdownFiles(join(packagePath, 'docs'));
  const withTODO = allFiles.filter(f => {
    const content = readFileSync(f, 'utf-8');
    return /TODO|FIXME|XXX|PLACEHOLDER/i.test(content);
  });

  if (withTODO.length === 0) {
    score.passed += 20;
  } else {
    issues.push(
      `⚠️  ${withTODO.length} files have TODO/FIXME placeholders`
    );
  }

  return { issues, score };
}

function getAllMarkdownFiles(dir) {
  const files = [];
  try {
    for (const file of readdirSync(dir)) {
      const path = join(dir, file);
      const stat = statSync(path);
      if (stat.isDirectory()) {
        files.push(...getAllMarkdownFiles(path));
      } else if (file.endsWith('.md')) {
        files.push(path);
      }
    }
  } catch {
    // Ignore unreadable directories
  }
  return files;
}

// Main
const packageArg = process.argv[2];
const packages = packageArg
  ? [packageArg]
  : readdirSync('packages').filter(f => {
      const stat = statSync(join('packages', f));
      return stat.isDirectory();
    });

console.log('🔍 Validating Diataxis documentation...\n');

const results = {};
for (const pkg of packages) {
  const pkgPath = join('packages', pkg);
  const { issues, score } = validatePackage(pkgPath);
  results[pkg] = { issues, score };

  const percent = Math.round((score.passed / score.total) * 100);
  const status = percent === 100 ? '✅' : percent >= 80 ? '⚠️ ' : '❌';

  console.log(`${status} @unrdf/${pkg}: ${percent}% (${score.passed}/${score.total})`);

  if (issues.length > 0) {
    for (const issue of issues) {
      console.log(`   ${issue}`);
    }
  }
}

// Summary
const avgScore =
  Object.values(results).reduce((sum, r) => sum + (r.score.passed / r.score.total), 0) /
  packages.length;

console.log('\n📊 Summary:');
console.log(`Average: ${Math.round(avgScore * 100)}%`);
console.log(`Packages: ${packages.length}`);
console.log(`Complete: ${Object.values(results).filter(r => (r.score.passed / r.score.total) === 1).length}`);

process.exit(avgScore === 1 ? 0 : 1);
```

**Make it executable:**
```bash
chmod +x scripts/validate-diataxis.js
```

**Add to package.json:**
```json
{
  "scripts": {
    "validate:docs": "node scripts/validate-diataxis.js",
    "validate:docs:core": "node scripts/validate-diataxis.js core"
  }
}
```

**Time estimate:** 3-4 hours

### Day 4-5: Documentation for the Docs

#### Task 1.7: Update DIATAXIS-GUIDE.md with Tools

Add section at the end:

```markdown
## Automation & Tools

### Initializing a Package's Documentation

Use the provided script to create the standard structure:

\`\`\`bash
./scripts/init-package-docs.sh @unrdf/core
\`\`\`

This creates:
- \`docs/TUTORIALS/\` with 3 placeholder files
- \`docs/HOW-TO/\` with 4 placeholder files
- \`docs/REFERENCE/\` and \`docs/EXPLANATION/\` directories
- All necessary index files

### Validating Documentation

Check your docs against Diataxis standards:

\`\`\`bash
# Check all packages
pnpm run validate:docs

# Check one package
pnpm run validate:docs:core
\`\`\`

Validation checks:
- ✅ All 4 Diataxis directories exist
- ✅ Minimum files in each (1+)
- ✅ No TODO/FIXME placeholders
- ✅ Directory index files present

Validation score:
- 100% = Complete
- 80%+ = Mostly done
- <80% = Needs work

### JSDoc to API Reference

To generate API.md from code comments:

\`\`\`bash
node scripts/generate-api-ref.js --package @unrdf/core
\`\`\`

This parses all JSDoc comments in \`src/\` and generates:
- Function signatures
- Parameter documentation
- Return type documentation
- Error types
- Code examples from @example tags

See [scripts/generate-api-ref.js](../scripts/generate-api-ref.js) for options.
```

**Time estimate:** 2-3 hours

---

## Week 2: Core Package Documentation (Bootstrap)

**Goal:** Complete @unrdf/core docs to serve as blueprint for other packages

### Day 6-7: @unrdf/core TUTORIALS

#### Task 2.1: Write Core Tutorial 1 - Getting Started

**File:** `packages/core/docs/TUTORIALS/01-getting-started.md`

**Based on:** DIATAXIS-EXAMPLES.md (already provided)

**Checklist:**
- [ ] Covers parsing RDF (Turtle format)
- [ ] Shows creating a store
- [ ] Demonstrates basic SPARQL query
- [ ] Has working code examples
- [ ] Includes "verify it works" section
- [ ] 15-20 minute read time
- [ ] Links to next tutorial

**Acceptance criteria:**
- Run through code examples, all work ✅
- Readability score 60+ (Flesch-Kincaid)
- No undefined terms (or explained first)
- Clear progression from simple to slightly complex

**Time estimate:** 6-8 hours

#### Task 2.2: Write Core Tutorial 2 - Common Patterns

**File:** `packages/core/docs/TUTORIALS/02-basic-workflow.md`

**Content outline:**
1. Parsing multiple RDF formats (Turtle, JSON-LD, N-Triples)
2. Writing queries with filters and joins
3. Handling results (iterate, convert to JSON/CSV)
4. Updating data dynamically

**Code examples:**
```javascript
// Pattern 1: Multiple formats
const ttl = core.parseRdf(data, 'turtle');
const jsonld = core.parseRdf(data, 'jsonld');
const ntriples = core.parseRdf(data, 'ntriples');

// Pattern 2: Filtering
SELECT ?name WHERE {
  ?person foaf:name ?name ;
          foaf:age ?age .
  FILTER (?age > 18)
}

// Pattern 3: Joins
SELECT ?personName ?friendName WHERE {
  ?person foaf:name ?personName ;
          foaf:knows ?friend .
  ?friend foaf:name ?friendName .
}

// Pattern 4: Update
store.addQuad(...);
store.removeQuad(...);
```

**Acceptance criteria:**
- Covers 4+ common patterns
- Each has working example
- Real-world use case for each
- ~25 minute read time

**Time estimate:** 8-10 hours

#### Task 2.3: Write Core Tutorial 3 - Advanced Patterns

**File:** `packages/core/docs/TUTORIALS/03-advanced-patterns.md`

**Content outline:**
1. Optional patterns (\`OPTIONAL\`)
2. Union queries (\`UNION\`)
3. Subqueries
4. Aggregate functions (\`COUNT\`, \`GROUP_BY\`)
5. String functions and regex

**Example patterns:**
```sparql
# OPTIONAL - find people even without email
SELECT ?name ?email WHERE {
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:email ?email . }
}

# UNION - people or organizations
SELECT ?name WHERE {
  { ?thing a foaf:Person ; foaf:name ?name . }
  UNION
  { ?thing a foaf:Organization ; foaf:name ?name . }
}

# GROUP_BY and COUNT - find prolific authors
SELECT ?author (COUNT(?article) as ?count) WHERE {
  ?article dct:creator ?author .
}
GROUP BY ?author
ORDER BY DESC(?count)
```

**Acceptance criteria:**
- Covers 5+ advanced patterns
- Progressive difficulty
- Real-world examples
- ~30 minute read time

**Time estimate:** 10-12 hours

### Day 7-8: @unrdf/core HOW-TO GUIDES

#### Task 2.4: Write Core How-To 1 - Optimize Queries

**File:** `packages/core/docs/HOW-TO/optimize-sparql-queries.md`

**Based on:** DIATAXIS-EXAMPLES.md (already provided)

**Key sections:**
1. Quick diagnosis (timing queries)
2. Simplify patterns
3. Add filters strategically
4. Use LIMIT for pagination
5. Avoid expensive patterns
6. Use typed data
7. Add indexes (when needed)

**Acceptance criteria:**
- Shows 6+ optimization techniques
- Each has before/after example
- Performance impact documented
- Includes benchmarking code

**Time estimate:** 6-8 hours

#### Task 2.5: Write Core How-To 2 - Working with Formats

**File:** `packages/core/docs/HOW-TO/working-with-formats.md`

**Content:**
1. Parse different formats
2. Convert between formats
3. Handle format errors
4. Custom format support
5. Stream parsing (large files)

**Code examples:**
```javascript
// Parse Turtle
const ttl = core.parseRdf(ttlString, 'turtle');

// Parse JSON-LD with context
const jsonld = core.parseRdf(jsonldObj, 'jsonld', {
  context: { ... }
});

// Convert to different format
const csvResults = results.toCSV();
const xmlResults = results.toXML();

// Stream parsing large files
const stream = fs.createReadStream('big-file.ttl');
for await (const quad of core.parseStream(stream)) {
  store.addQuad(quad);
}
```

**Time estimate:** 5-7 hours

#### Task 2.6: Write Core How-To 3 - Troubleshooting

**File:** `packages/core/docs/HOW-TO/troubleshooting.md`

**Content:**
Common problems and solutions:
- Query returns no results
- Getting parse errors
- Type mismatches
- Performance issues
- Memory leaks
- Concurrency issues

**Format:**
| Problem | Cause | Solution |
|---------|-------|----------|
| [Issue] | [Why] | [Fix] |

**Time estimate:** 4-6 hours

#### Task 2.7: Write Core How-To 4 - Performance Tuning

**File:** `packages/core/docs/HOW-TO/performance-tuning.md`

**Content:**
1. Profiling queries
2. Identifying bottlenecks
3. Using indexes effectively
4. Memory optimization
5. Batch operations
6. Concurrency patterns

**Time estimate:** 6-8 hours

### Day 8-9: @unrdf/core REFERENCE

#### Task 2.8: Generate & Write Core API Reference

**File:** `packages/core/docs/REFERENCE/API.md`

**Based on:** DIATAXIS-EXAMPLES.md (already provided)

**Content:**
1. Main functions (createKnowledgeSubstrateCore, parseRdf, query, etc.)
2. Store interface
3. Result types
4. Options objects
5. Error types

**Process:**
1. Use JSDoc extraction script to get function signatures
2. Add detailed parameter descriptions
3. Add code examples for each function
4. Add error documentation
5. Create return type reference

**Acceptance criteria:**
- All public functions documented
- All options described
- Code examples for each function
- Error types listed

**Time estimate:** 10-12 hours

#### Task 2.9: Write Core Types Reference

**File:** `packages/core/docs/REFERENCE/TYPES.md`

**Content:**
- Store interface
- QueryResults, Binding
- RDFTerm types (NamedNode, Literal, BlankNode, Variable)
- Quad interface
- All schema types

**Time estimate:** 4-6 hours

#### Task 2.10: Write Core Configuration Reference

**File:** `packages/core/docs/REFERENCE/CONFIGURATION.md`

**Content:**
- All createKnowledgeSubstrateCore options
- All query options
- All parse options
- All Store methods and options

**Table format:**
| Option | Type | Default | Description |
|--------|------|---------|-------------|

**Time estimate:** 3-4 hours

#### Task 2.11: Write Core Error Reference

**File:** `packages/core/docs/REFERENCE/ERRORS.md`

**Content:**
- Error code reference
- When each error occurs
- How to fix each error
- Code examples for each

**Table format:**
| Error | When | Solution |
|-------|------|----------|

**Time estimate:** 3-4 hours

### Day 9-10: @unrdf/core EXPLANATION

#### Task 2.12: Write Core Architecture Explanation

**File:** `packages/core/docs/EXPLANATION/architecture.md`

**Based on:** DIATAXIS-EXAMPLES.md (already provided)

**Content:**
1. Layered architecture
2. Backend abstraction
3. Optional features
4. Data flow
5. Performance implications

**Time estimate:** 6-8 hours

#### Task 2.13: Write Core Design Decisions

**File:** `packages/core/docs/EXPLANATION/design-decisions.md`

**Content:**
1. Why layered design?
2. Why backend abstraction?
3. Why optional features?
4. Why this API?
5. Comparison with alternatives

**Time estimate:** 6-8 hours

#### Task 2.14: Write Core SPARQL Concepts

**File:** `packages/core/docs/EXPLANATION/sparql-concepts.md`

**Content:**
1. What is SPARQL?
2. Graph patterns
3. Query structure
4. Triple-based thinking
5. vs SQL

**Time estimate:** 5-7 hours

#### Task 2.15: Write Core Performance Details

**File:** `packages/core/docs/EXPLANATION/performance.md`

**Content:**
1. Query execution model
2. Memory usage patterns
3. Bottlenecks and trade-offs
4. Scalability limits
5. Optimization approaches

**Time estimate:** 5-7 hours

---

## Week 2 Summary & Handoff

### Deliverables
- ✅ Diataxis structure (4 directory types)
- ✅ Templates for each type
- ✅ @unrdf/core complete (all 4 types)
- ✅ Automation scripts (init, validate, API gen)
- ✅ DIATAXIS-MAP navigation
- ✅ Guidelines for future packages

### What Phase 2 Needs
- Copy structure from core to 6 other packages
- Follow core as example
- Use templates as starting point
- Run validation to ensure completeness

### Success Criteria
- [ ] All 4 Diataxis types have examples
- [ ] Scripts automated and working
- [ ] @unrdf/core validation: 100%
- [ ] Documentation is deployable
- [ ] Future teams have clear blueprint

---

## Time Summary

| Task | Est. Hours | Owner |
|------|-----------|-------|
| Week 1: Scaffolding | 25-30 | Dev Lead |
| Week 1: Automation | 8-10 | Automation Engineer |
| Week 2: @unrdf/core | 80-100 | Core Team (2-3 people) |
| **Total** | **110-140** | **1-2 FTE** |

**Can be compressed:** Yes, to 1 week if full team dedicates time
**Can be extended:** Yes, quality improves with more time per package

---

## Dependencies & Blockers

**No blockers:** This phase is independent of code changes

**Outputs:** All artifacts are purely documentation

**Inputs needed:**
- Access to code source files (for JSDoc extraction)
- Understanding of package functionality
- Examples from test files

---

## Acceptance & Sign-Off

Phase 1 is complete when:
1. ✅ All 4 Diataxis types have real examples
2. ✅ @unrdf/core docs score 100% validation
3. ✅ Scripts are tested and documented
4. ✅ Guidelines are clear for Phase 2
5. ✅ All files committed and pushed

**Sign-off:** Review all files, run validation, approve PRs
