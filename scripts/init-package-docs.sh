#!/bin/bash
# init-package-docs.sh - Initialize documentation structure for new packages
#
# Usage: ./init-package-docs.sh <package-name> [package-type]
# Example: ./init-package-docs.sh streaming Feature
#

set -e

PACKAGE_NAME=$1
PACKAGE_TYPE=${2:-Feature}  # Default: Feature

if [ -z "$PACKAGE_NAME" ]; then
  echo "Usage: $0 <package-name> [package-type]"
  echo ""
  echo "Examples:"
  echo "  $0 federation Feature"
  echo "  $0 composables Integration"
  echo "  $0 common Foundation"
  exit 1
fi

PACKAGE_DIR="packages/$PACKAGE_NAME"

if [ ! -d "$PACKAGE_DIR" ]; then
  echo "Error: Package directory not found: $PACKAGE_DIR"
  exit 1
fi

DOCS_DIR="$PACKAGE_DIR/docs"

echo "Initializing documentation for: $PACKAGE_NAME (Type: $PACKAGE_TYPE)"

# Create directory structure
mkdir -p "$DOCS_DIR/tutorials"
mkdir -p "$DOCS_DIR/how-to"
mkdir -p "$DOCS_DIR/reference"
mkdir -p "$DOCS_DIR/explanation"

# Create README
cat > "$DOCS_DIR/README.md" << 'EOF'
# @unrdf/$PACKAGE_NAME Documentation

Complete Diataxis documentation for @unrdf/$PACKAGE_NAME.

## Structure

- **tutorials/** - Learn by doing (3 files)
- **how-to/** - Solve specific problems (4 files)
- **reference/** - Complete API reference (5 files)
- **explanation/** - Understand concepts (4 files)

## Getting Started

1. Start with tutorials/01-getting-started.md
2. Explore how-to guides for your use case
3. Reference API docs while coding
4. Read explanations to understand design

## Status

- [ ] All 3 tutorials complete
- [ ] All 4 how-to guides complete
- [ ] All 5 reference docs complete
- [ ] All 4 explanation docs complete
- [ ] 100% validation score
- [ ] Peer review passed

EOF

# Create tutorial templates
for i in 1 2 3; do
  cat > "$DOCS_DIR/tutorials/0$i-template.md" << 'EOF'
# [Tutorial Title]

**Time estimate:** X-Y hours
**Difficulty:** [Beginner|Intermediate|Advanced]
**Prerequisites:** [Previous tutorials]

---

## What You'll Do

In this tutorial, you'll:
1. [Goal 1]
2. [Goal 2]
3. [Goal 3]

---

## [Section 1]

[Content]

## [Section 2]

[Content]

---

## Summary

You've learned:
- ✅ [Learning outcome 1]
- ✅ [Learning outcome 2]

---

## Next Steps

[References to other documentation]
EOF

  echo "Created: $DOCS_DIR/tutorials/0$i-template.md"
done

# Create how-to templates
for i in 1 2 3 4; do
  cat > "$DOCS_DIR/how-to/0$i-template.md" << 'EOF'
# How To: [Problem Statement]

**Time estimate:** X-Y hours
**Difficulty:** [Beginner|Intermediate|Advanced]
**Context:** [When you'd use this]

---

## Problem

[Describe the problem clearly]

---

## Solution

[Provide step-by-step solution]

---

## Real-World Example

[Show practical usage]

---

## Summary

Key techniques:
1. [Technique 1]
2. [Technique 2]

---

## Next Reading

[References to other documentation]
EOF

  echo "Created: $DOCS_DIR/how-to/0$i-template.md"
done

# Create reference templates
for i in 1 2 3 4 5; do
  cat > "$DOCS_DIR/reference/0$i-template.md" << 'EOF'
# [Reference Title]: @unrdf/$PACKAGE_NAME

[Brief description]

---

## [Section 1]

[Content with tables, code, parameters]

---

## Next Reading

[References to other documentation]
EOF

  echo "Created: $DOCS_DIR/reference/0$i-template.md"
done

# Create explanation templates
for i in 1 2 3 4; do
  cat > "$DOCS_DIR/explanation/0$i-template.md" << 'EOF'
# [Concept]: @unrdf/$PACKAGE_NAME

Understanding [concept].

---

## Big Picture

[Overview and context]

---

## Definition

[Formal definition]

---

## How It Works

[Implementation details]

---

## Why This Approach

[Design rationale]

---

## When to Use

[Practical guidance]

---

## Next Reading

[References to other documentation]
EOF

  echo "Created: $DOCS_DIR/explanation/0$i-template.md"
done

# Create index file
cat > "$DOCS_DIR/INDEX.md" << 'EOF'
# Documentation Index: @unrdf/$PACKAGE_NAME

## Quick Navigation

### By Learning Style

**I want to learn by doing:** Start with [tutorials/01-getting-started.md](tutorials/01-getting-started.md)

**I have a specific problem:** Browse [how-to/](how-to/) guides

**I'm looking for details:** Check [reference/](reference/) documentation

**I want to understand why:** Read [explanation/](explanation/) documents

### By File

#### Tutorials
- [tutorials/01-getting-started.md](tutorials/01-getting-started.md)
- [tutorials/02-basic-workflow.md](tutorials/02-basic-workflow.md)
- [tutorials/03-advanced-patterns.md](tutorials/03-advanced-patterns.md)

#### How-To Guides
- [how-to/01-...md](how-to/01-...md)
- [how-to/02-...md](how-to/02-...md)
- [how-to/03-...md](how-to/03-...md)
- [how-to/04-...md](how-to/04-...md)

#### Reference
- [reference/01-api.md](reference/01-api.md)
- [reference/02-types.md](reference/02-types.md)
- [reference/03-configuration.md](reference/03-configuration.md)
- [reference/04-errors.md](reference/04-errors.md)
- [reference/05-migration.md](reference/05-migration.md)

#### Explanation
- [explanation/01-architecture.md](explanation/01-architecture.md)
- [explanation/02-design-decisions.md](explanation/02-design-decisions.md)
- [explanation/03-concepts.md](explanation/03-concepts.md)
- [explanation/04-advanced.md](explanation/04-advanced.md)

---

## Getting Started

1. Read [tutorials/01-getting-started.md](tutorials/01-getting-started.md) (30 min)
2. Try the examples yourself
3. Explore [how-to/](how-to/) for your use case
4. Refer to [reference/](reference/) while coding

---

## Completion Checklist

- [ ] All 3 tutorials written and tested
- [ ] All 4 how-to guides written
- [ ] All 5 reference docs complete
- [ ] All 4 explanation docs complete
- [ ] Code examples tested
- [ ] Peer reviewed
- [ ] 100% validation score
EOF

echo ""
echo "✓ Documentation structure initialized for: $PACKAGE_NAME"
echo ""
echo "Next steps:"
echo "1. Edit tutorials/ - Start with 01-getting-started.md"
echo "2. Fill in how-to/ guides"
echo "3. Create reference/ docs"
echo "4. Write explanation/ docs"
echo "5. Run: node validate-diataxis.js $PACKAGE_DIR"
echo ""
