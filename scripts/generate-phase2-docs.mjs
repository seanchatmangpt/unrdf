#!/usr/bin/env node

/**
 * generate-phase2-docs.mjs - Generate Phase 2 documentation for all packages
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const phase2Packages = [
  {
    name: 'streaming',
    type: 'Feature',
    description: 'Stream large RDF files efficiently',
    mainConcept: 'Streaming RDF data with constant memory'
  },
  {
    name: 'federation',
    type: 'Feature',
    description: 'Query multiple RDF sources',
    mainConcept: 'Federated SPARQL queries across engines'
  },
  {
    name: 'knowledge-engine',
    type: 'Feature',
    description: 'Inference and reasoning',
    mainConcept: 'RDF reasoning and rule inference'
  },
  {
    name: 'browser',
    type: 'Integration',
    description: 'RDF in the browser',
    mainConcept: 'Client-side RDF with Web APIs'
  },
  {
    name: 'cli',
    type: 'Integration',
    description: 'Command-line RDF tools',
    mainConcept: 'Terminal workflows for RDF'
  },
  {
    name: 'react',
    type: 'Integration',
    description: 'RDF with React',
    mainConcept: 'React hooks for RDF data binding'
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
  const readmeContent = `# @unrdf/${pkg.name} Documentation

${pkg.description}

## Quick Links

- **Getting Started:** [tutorials/01-getting-started.md](tutorials/01-getting-started.md)
- **API Reference:** [reference/01-api.md](reference/01-api.md)
- **How-To Guides:** [how-to/](how-to/)
- **Explanation:** [explanation/](explanation/)

## Key Concept

**${pkg.mainConcept}**

---

**Status**: 🚧 Documentation in progress (Phase 2)

Learn more: [PHASE-2-QUICKSTART.md](../../docs/PHASE-2-QUICKSTART.md)
`;
  fs.writeFileSync(path.join(docsDir, 'README.md'), readmeContent);

  // Generate tutorials
  const tutorialDir = path.join(docsDir, 'tutorials');
  for (let i = 1; i <= 3; i++) {
    const filePath = path.join(tutorialDir, `0${i}-template.md`);
    if (!fs.existsSync(filePath)) {
      fs.writeFileSync(filePath, `# Tutorial ${i}: [Title]

**Time:** X hours | **Difficulty:** [Level]

[Content placeholder]
`);
    }
  }

  // Generate how-to guides
  const howToDir = path.join(docsDir, 'how-to');
  for (let i = 1; i <= 4; i++) {
    const filePath = path.join(howToDir, `0${i}-template.md`);
    if (!fs.existsSync(filePath)) {
      fs.writeFileSync(filePath, `# How To: [Problem]

[Content placeholder]
`);
    }
  }

  // Generate reference docs
  const refDir = path.join(docsDir, 'reference');
  const refFiles = ['01-api.md', '02-types.md', '03-configuration.md', '04-errors.md', '05-migration.md'];
  refFiles.forEach((file) => {
    const filePath = path.join(refDir, file);
    if (!fs.existsSync(filePath)) {
      fs.writeFileSync(filePath, `# ${file.slice(3, -3)}: @unrdf/${pkg.name}

[Content placeholder]
`);
    }
  });

  // Generate explanation docs
  const expDir = path.join(docsDir, 'explanation');
  const expFiles = ['01-architecture.md', '02-design-decisions.md', '03-concepts.md', '04-advanced.md'];
  expFiles.forEach((file) => {
    const filePath = path.join(expDir, file);
    if (!fs.existsSync(filePath)) {
      fs.writeFileSync(filePath, `# ${file.slice(3, -3)}: @unrdf/${pkg.name}

[Content placeholder]
`);
    }
  });

  console.log(`  ✓ Created 16 files`);
});

console.log('\n✅ Phase 2 documentation scaffolding complete!\n');
console.log('Next steps:');
console.log('1. Fill in tutorial and how-to content');
console.log('2. Complete reference documentation from JSDoc');
console.log('3. Write explanation documents');
console.log('4. Run: node scripts/validate-diataxis.js packages/<package>/');
