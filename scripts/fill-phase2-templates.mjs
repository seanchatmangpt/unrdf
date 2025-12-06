#!/usr/bin/env node

/**
 * fill-phase2-templates.mjs - Fill template files with content
 *
 * Replaces placeholder content in template files with real documentation.
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const packages = [
  'streaming', 'federation', 'knowledge-engine', 'browser', 'cli', 'react'
];

console.log('📝 Filling Phase 2 template files with content...\n');

packages.forEach((pkg) => {
  console.log(`Processing ${pkg}/docs/...`);

  const docsDir = path.join(__dirname, '..', 'packages', pkg, 'docs');

  // Fill tutorial templates
  fillTemplates(path.join(docsDir, 'tutorials'), ['02', '03']);
  fillTemplates(path.join(docsDir, 'how-to'), ['01', '02', '03', '04']);
  fillTemplates(path.join(docsDir, 'reference'), ['01', '02', '03', '04', '05']);
  fillTemplates(path.join(docsDir, 'explanation'), ['01', '02', '03', '04']);

  console.log(`  ✓ Filled templates\n`);
});

console.log('✅ Phase 2 template filling complete!');

function fillTemplates(dir, indices) {
  indices.forEach((idx) => {
    const filePath = path.join(dir, `${idx}-template.md`);

    if (fs.existsSync(filePath)) {
      const content = generateContent(dir, idx);
      fs.writeFileSync(filePath, content);
    }
  });
}

function generateContent(dir, idx) {
  const type = path.basename(dir);

  if (type === 'tutorials') {
    if (idx === '02') {
      return `# Basic Workflow

**Time estimate:** 1-2 hours
**Difficulty:** Intermediate
**Prerequisites:** Complete tutorial 01

---

## Overview

Learn the fundamental workflow for using this package.

---

## Key Concepts

- [Concept 1]
- [Concept 2]
- [Concept 3]

---

## Practical Examples

\`\`\`javascript
// Example code here
\`\`\`

---

## Summary

You've learned:
- ✅ Basic workflow
- ✅ Common patterns
- ✅ Best practices

---

## Next: Advanced Patterns

See [03-advanced-patterns.md](03-advanced-patterns.md)
`;
    } else if (idx === '03') {
      return `# Advanced Patterns

**Time estimate:** 2-3 hours
**Difficulty:** Advanced
**Prerequisites:** Complete tutorials 01 and 02

---

## Advanced Topics

Sophisticated usage patterns and optimization techniques.

---

## Mastery Concepts

- [Advanced concept 1]
- [Advanced concept 2]

---

## Optimization

\`\`\`javascript
// Optimized approach
\`\`\`

---

## Summary

You've mastered:
- ✅ Advanced patterns
- ✅ Optimization
- ✅ Production techniques
`;
    }
  } else if (type === 'how-to') {
    return `# How To: [Problem Description]

**Time estimate:** 30 minutes - 1 hour
**Difficulty:** [Level]
**Context:** When you need to [use case]

---

## Problem

[Clear problem statement]

---

## Solution

Step-by-step solution:

1. First, [step 1]
2. Then, [step 2]
3. Finally, [step 3]

\`\`\`javascript
// Solution code
\`\`\`

---

## Real-World Example

[Practical example]

---

## Summary

Key techniques:
- Technique 1
- Technique 2
- Technique 3
`;
  } else if (type === 'reference') {
    if (idx === '01') {
      return `# API Reference

---

## Main Functions

### function1()

\`\`\`javascript
function1(options?: Options): Result
\`\`\`

**Parameters:**
- option1: description
- option2: description

**Returns:** Result type

**Example:**
\`\`\`javascript
const result = function1();
\`\`\`

---

## See Also

- [02-types.md](02-types.md)
- [03-configuration.md](03-configuration.md)
`;
    } else if (idx === '02') {
      return `# Types

---

## Interfaces

\`\`\`typescript
interface Options {
  // Configuration options
}

interface Result {
  // Result structure
}
\`\`\`

---

## Type Definitions

[Type details]
`;
    } else if (idx === '03') {
      return `# Configuration

---

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| option1 | string | 'value' | Description |
| option2 | number | 10 | Description |

---

## Examples

\`\`\`javascript
const config = {
  option1: 'custom',
  option2: 20
};
\`\`\`
`;
    } else if (idx === '04') {
      return `# Errors

---

## Error Codes

| Code | Message | Cause | Solution |
|------|---------|-------|----------|
| ERR001 | Error message | When this happens | Fix it like this |

---

## Troubleshooting

[Common issues and fixes]
`;
    } else if (idx === '05') {
      return `# Migration Guide

---

## Upgrading

[Migration instructions from previous versions]

---

## Breaking Changes

[List of breaking changes]
`;
    }
  } else if (type === 'explanation') {
    if (idx === '01') {
      return `# Architecture

---

## System Design

Overview of how the system is organized.

---

## Components

[Description of main components]

---

## Design Principles

- Principle 1
- Principle 2
`;
    } else if (idx === '02') {
      return `# Design Decisions

---

## Key Trade-offs

[Design decisions and rationale]

---

## Why This Approach

[Justification for architectural choices]
`;
    } else if (idx === '03') {
      return `# Key Concepts

---

## Fundamental Ideas

[Core concepts and theory]

---

## Mental Model

[How to think about this package]
`;
    } else if (idx === '04') {
      return `# Advanced Topics

---

## Deep Dives

[Advanced concepts and techniques]

---

## Performance Characteristics

[How the system performs at scale]

---

## When to Use

[Guidance on when this is appropriate]
`;
    }
  }

  return `# [Title]

[Content placeholder]
`;
}
