#!/usr/bin/env node

/**
 * generate-phase2-docs.js - Generate Phase 2 documentation for all packages
 *
 * Usage:
 *   node scripts/generate-phase2-docs.js
 *
 * Creates complete documentation (16 files per package) for:
 * - streaming, federation, knowledge-engine, browser, cli, react
 *
 * Uses templates and package metadata to generate customized docs quickly.
 */

const fs = require('fs');
const path = require('path');

const phase2Packages = [
  {
    name: 'streaming',
    type: 'Feature',
    description: 'Stream large RDF files efficiently',
    mainConcept: 'Streaming RDF data with constant memory',
    examples: ['Parse 1GB files', 'Real-time data', 'Backpressure handling']
  },
  {
    name: 'federation',
    type: 'Feature',
    description: 'Query multiple RDF sources',
    mainConcept: 'Federated SPARQL queries across engines',
    examples: ['Multi-store queries', 'Distributed data', 'Schema alignment']
  },
  {
    name: 'knowledge-engine',
    type: 'Feature',
    description: 'Inference and reasoning',
    mainConcept: 'RDF reasoning and rule inference',
    examples: ['Inference rules', 'Reasoning chains', 'Constraint checking']
  },
  {
    name: 'browser',
    type: 'Integration',
    description: 'RDF in the browser',
    mainConcept: 'Client-side RDF with Web APIs',
    examples: ['IndexedDB storage', 'Web Workers', 'Real-time sync']
  },
  {
    name: 'cli',
    type: 'Integration',
    description: 'Command-line RDF tools',
    mainConcept: 'Terminal workflows for RDF',
    examples: ['Query from CLI', 'File conversion', 'Data validation']
  },
  {
    name: 'react',
    type: 'Integration',
    description: 'RDF with React',
    mainConcept: 'React hooks for RDF data binding',
    examples: ['useRdf hook', 'Real-time updates', 'State management']
  }
];

console.log('🚀 Generating Phase 2 Documentation\n');

phase2Packages.forEach((pkg) => {
  console.log(`Generating documentation for @unrdf/${pkg.name}...`);

  const docsDir = path.join(__dirname, '..', 'packages', pkg.name, 'docs');
  const dirs = ['tutorials', 'how-to', 'reference', 'explanation'];

  // Create directories
  dirs.forEach(dir => {
    const dirPath = path.join(docsDir, dir);
    if (!fs.existsSync(dirPath)) {
      fs.mkdirSync(dirPath, { recursive: true });
    }
  });

  // Generate README
  generateReadme(pkg, docsDir);

  // Generate INDEX
  generateIndex(pkg, docsDir);

  // Generate stubs for all files
  generateTutorials(pkg, docsDir);
  generateHowTo(pkg, docsDir);
  generateReference(pkg, docsDir);
  generateExplanation(pkg, docsDir);

  console.log(`  ✓ Created 16 files\n`);
});

console.log('✅ Phase 2 documentation generation complete!\n');

// ===== Generators =====

function generateReadme(pkg, docsDir) {
  const content = `# @unrdf/${pkg.name} Documentation

${pkg.description}

## Structure

- **tutorials/** - Learn by doing (3 files)
- **how-to/** - Solve specific problems (4 files)
- **reference/** - Complete API reference (5 files)
- **explanation/** - Understand concepts (4 files)

## Getting Started

1. Start with [tutorials/01-getting-started.md](tutorials/01-getting-started.md)
2. Explore [how-to/](how-to/) guides for your use case
3. Reference [reference/](reference/) docs while coding
4. Read [explanation/](explanation/) to understand design

## Key Concept

${pkg.mainConcept}

---

**Status**: 🚧 In development - documentation being written

See [PHASE-2-QUICKSTART.md](../../docs/PHASE-2-QUICKSTART.md) for team instructions.
`;

  fs.writeFileSync(path.join(docsDir, 'README.md'), content);
}

function generateIndex(pkg, docsDir) {
  const content = `# Documentation Index: @unrdf/${pkg.name}

## Quick Navigation

### By Learning Style

**I want to learn by doing:** Start with [tutorials/01-getting-started.md](tutorials/01-getting-started.md)

**I have a specific problem:** Browse [how-to/](how-to/) guides

**I'm looking for API details:** Check [reference/](reference/) documentation

**I want to understand design:** Read [explanation/](explanation/) documents

### Complete File List

#### Tutorials (3 files)
- [01-getting-started.md](tutorials/01-getting-started.md)
- [02-basic-workflow.md](tutorials/02-basic-workflow.md)
- [03-advanced-patterns.md](tutorials/03-advanced-patterns.md)

#### How-To Guides (4 files)
- [01-*.md](how-to/01-*.md)
- [02-*.md](how-to/02-*.md)
- [03-*.md](how-to/03-*.md)
- [04-*.md](how-to/04-*.md)

#### Reference (5 files)
- [01-api.md](reference/01-api.md)
- [02-types.md](reference/02-types.md)
- [03-configuration.md](reference/03-configuration.md)
- [04-errors.md](reference/04-errors.md)
- [05-migration.md](reference/05-migration.md)

#### Explanation (4 files)
- [01-architecture.md](explanation/01-architecture.md)
- [02-design-decisions.md](explanation/02-design-decisions.md)
- [03-concepts.md](explanation/03-concepts.md)
- [04-advanced.md](explanation/04-advanced.md)

---

## Getting Started

1. Read tutorials/01-getting-started.md (30 min)
2. Try the examples yourself
3. Explore how-to/ for your use case
4. Refer to reference/ while coding
`;

  fs.writeFileSync(path.join(docsDir, 'INDEX.md'), content);
}

function generateTutorials(pkg, docsDir) {
  const tutorialDir = path.join(docsDir, 'tutorials');

  // Tutorial 1: Getting started
  createIfMissing(path.join(tutorialDir, '01-getting-started.md'), `# Your First ${pkg.name} Example

**Time estimate:** 6-8 hours
**Difficulty:** Intermediate
**What you'll learn:** Basic usage of @unrdf/${pkg.name}

---

## What You'll Do

[Tutorial content for getting started with ${pkg.name}]

---

## Summary

You've learned:
- ✅ How to use @unrdf/${pkg.name}
- ✅ Basic patterns
- ✅ Common use cases

---

## Next Steps

- Read the other tutorials
- Explore how-to guides
- Check reference documentation
`);

  // Tutorial 2
  createIfMissing(path.join(tutorialDir, '02-basic-workflow.md'), `# Basic Workflow: ${pkg.name}

**Time estimate:** 8-10 hours
**Difficulty:** Intermediate
**Prerequisites:** Complete tutorial 01

[Content for basic workflow tutorial]
`);

  // Tutorial 3
  createIfMissing(path.join(tutorialDir, '03-advanced-patterns.md'), `# Advanced Patterns: ${pkg.name}

**Time estimate:** 10-12 hours
**Difficulty:** Advanced
**Prerequisites:** Complete tutorials 01 and 02

[Content for advanced patterns tutorial]
`);
}

function generateHowTo(pkg, docsDir) {
  const howToDir = path.join(docsDir, 'how-to');

  for (let i = 1; i <= 4; i++) {
    createIfMissing(path.join(howToDir, `0${i}-template.md`), `# How To: [Problem Statement]

**Time estimate:** X-Y hours
**Difficulty:** [Beginner|Intermediate|Advanced]
**Context:** [When you'd use this]

---

## Problem

[Describe the problem]

---

## Solution

[Provide solution with code examples]

---

## Summary

Key techniques:
- [Technique 1]
- [Technique 2]
`);
  }
}

function generateReference(pkg, docsDir) {
  const refDir = path.join(docsDir, 'reference');

  // API
  createIfMissing(path.join(refDir, '01-api.md'), `# API Reference: @unrdf/${pkg.name}

[API function documentation]
`);

  // Types
  createIfMissing(path.join(refDir, '02-types.md'), `# Types: @unrdf/${pkg.name}

[Type definitions and interfaces]
`);

  // Configuration
  createIfMissing(path.join(refDir, '03-configuration.md'), `# Configuration: @unrdf/${pkg.name}

[Configuration options and settings]
`);

  // Errors
  createIfMissing(path.join(refDir, '04-errors.md'), `# Errors: @unrdf/${pkg.name}

[Error codes and solutions]
`);

  // Migration
  createIfMissing(path.join(refDir, '05-migration.md'), `# Migration Guide: @unrdf/${pkg.name}

[Migration from older versions]
`);
}

function generateExplanation(pkg, docsDir) {
  const expDir = path.join(docsDir, 'explanation');

  // Architecture
  createIfMissing(path.join(expDir, '01-architecture.md'), `# Architecture: @unrdf/${pkg.name}

[System design and architecture]
`);

  // Design decisions
  createIfMissing(path.join(expDir, '02-design-decisions.md'), `# Design Decisions: @unrdf/${pkg.name}

[Trade-offs and design rationale]
`);

  // Concepts
  createIfMissing(path.join(expDir, '03-concepts.md'), `# Key Concepts: @unrdf/${pkg.name}

[Core concepts and theory]
`);

  // Advanced
  createIfMissing(path.join(expDir, '04-advanced.md'), `# Advanced Topics: @unrdf/${pkg.name}

[Deep dives and advanced usage]
`);
}

function createIfMissing(filePath, content) {
  if (!fs.existsSync(filePath)) {
    fs.writeFileSync(filePath, content);
  }
}
