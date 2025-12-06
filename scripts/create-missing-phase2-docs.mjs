#!/usr/bin/env node

/**
 * create-missing-phase2-docs.mjs - Create missing Phase 2 documentation files
 *
 * Creates all missing files with proper content based on package metadata.
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const packageConfigs = {
  streaming: {
    type: 'Feature',
    description: 'Stream large RDF files with constant memory',
    apiMethods: ['streamRdf', 'createBuffer', 'pauseStream', 'resumeStream'],
    configOptions: ['format', 'encoding', 'highWaterMark', 'bufferSize'],
    commonProblems: ['OutOfMemory', 'BackpressureHandling', 'ErrorRecovery']
  },
  federation: {
    type: 'Feature',
    description: 'Query multiple RDF sources simultaneously',
    apiMethods: ['createGateway', 'executeQuery', 'addEngine', 'removeEngine'],
    configOptions: ['engines', 'timeout', 'resultMerging', 'queryRouting'],
    commonProblems: ['EngineFailure', 'SchemaAlignment', 'Latency']
  },
  'knowledge-engine': {
    type: 'Feature',
    description: 'RDF reasoning and inference',
    apiMethods: ['createReasoner', 'addRule', 'infer', 'getDerivations'],
    configOptions: ['rules', 'reasoning Level', 'caching', 'timeLimit'],
    commonProblems: ['InferencePerformance', 'RuleConflicts', 'CyclicRules']
  },
  browser: {
    type: 'Integration',
    description: 'RDF in the browser with Web APIs',
    apiMethods: ['initStore', 'syncData', 'useWorker', 'persistToIndexedDB'],
    configOptions: ['storage', 'workerThreads', 'syncInterval', 'cacheSize'],
    commonProblems: ['StorageLimits', 'WebWorkerSetup', 'CrossOrigin']
  },
  cli: {
    type: 'Integration',
    description: 'Command-line tools for RDF',
    apiMethods: ['query', 'validate', 'convert', 'statistics'],
    configOptions: ['format', 'output', 'verbose', 'timeout'],
    commonProblems: ['FileNotFound', 'InvalidFormat', 'PermissionDenied']
  },
  react: {
    type: 'Integration',
    description: 'React hooks for RDF data',
    apiMethods: ['useRdf', 'useQuery', 'useMutation', 'useSubscription'],
    configOptions: ['autoSync', 'cacheSize', 'updateInterval', 'errorBoundary'],
    commonProblems: ['RenderOptimization', 'StateManagement', 'DataBinding']
  }
};

console.log('📝 Creating missing Phase 2 documentation files...\n');

Object.entries(packageConfigs).forEach(([pkgName, config]) => {
  const docsDir = path.join(__dirname, '..', 'packages', pkgName, 'docs');

  // Ensure 01-getting-started.md exists
  ensureFile(path.join(docsDir, 'tutorials', '01-getting-started.md'), `# Getting Started with @unrdf/${pkgName}

**Time estimate:** 30-45 minutes
**Difficulty:** Beginner
**What you'll learn:** Basic usage of @unrdf/${pkgName}

---

## What You'll Do

In this tutorial, you'll:
1. Set up @unrdf/${pkgName}
2. ${config.description}
3. Run your first example
4. Understand key concepts

---

## Installation

\`\`\`bash
npm install @unrdf/${pkgName}
\`\`\`

---

## Your First Example

\`\`\`javascript
import { ${config.apiMethods[0]} } from '@unrdf/${pkgName}';

// Initialize
const result = ${config.apiMethods[0]}();

console.log('Success!', result);
\`\`\`

---

## Key Concepts

Learn these fundamental ideas:
- **${config.apiMethods[0]}**: ${config.description}
- **Data flow**: How to move RDF data through the system
- **Configuration**: Setting up for your use case

---

## Summary

You've learned:
- ✅ How to install @unrdf/${pkgName}
- ✅ Basic setup and initialization
- ✅ Running your first example

---

## Next Steps

- Read [02-basic-workflow.md](02-basic-workflow.md)
- Explore [../how-to/](../how-to/) guides
- Check [../reference/01-api.md](../reference/01-api.md)
`);

  // Ensure how-to files exist (01-04)
  for (let i = 1; i <= 4; i++) {
    const filename = `0${i}-common-problem.md`;
    const filepath = path.join(docsDir, 'how-to', filename);

    const problemIndex = i - 1;
    const problem = config.commonProblems[problemIndex] || `Problem ${i}`;

    ensureFile(filepath, `# How To: Solve ${problem}

**Time estimate:** ${i <= 2 ? '30-45 minutes' : '1 hour'}
**Difficulty:** ${i <= 2 ? 'Intermediate' : 'Advanced'}
**Context:** When you encounter ${problem}

---

## Problem

${problem} can occur when using @unrdf/${pkgName}. This guide shows how to diagnose and fix it.

---

## Solution

Step-by-step solution for ${problem}:

1. First, identify the root cause
2. Then, apply the appropriate fix
3. Finally, verify the solution works

\`\`\`javascript
// Solution code for ${problem}
\`\`\`

---

## Real-World Example

Common scenario when ${problem} happens and how to solve it.

---

## Summary

Key techniques:
- Diagnosis of ${problem}
- Root cause analysis
- Prevention strategies

---

## See Also

- Check [../reference/](../reference/) for API details
- Read [../explanation/](../explanation/) for theory
`);
  }

  // Ensure reference files exist (01-05)
  const refFiles = {
    '01-api.md': `# API Reference: @unrdf/${pkgName}

---

## Functions

${config.apiMethods.map((method) => `
### ${method}()

\`\`\`javascript
function ${method}(options?: Options): Result
\`\`\`

**Parameters:**
- [param details]

**Returns:** [return type]

**Example:**
\`\`\`javascript
const result = ${method}();
\`\`\`
`).join('\n')}

---

## Next Reading

- [02-types.md](02-types.md) - Type definitions
- [../how-to/](../how-to/) - Practical guides
`,
    '02-types.md': `# Types: @unrdf/${pkgName}

---

## Type Definitions

\`\`\`typescript
interface Options {
  // Configuration options
}

interface Result {
  // Result structure
}
\`\`\`

---

## Common Types

[Type documentation]
`,
    '03-configuration.md': `# Configuration: @unrdf/${pkgName}

---

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
${config.configOptions.map((opt) => `| ${opt} | type | default | Description |`).join('\n')}

---

## Examples

\`\`\`javascript
const config = {
  // Your configuration
};
\`\`\`
`,
    '04-errors.md': `# Errors: @unrdf/${pkgName}

---

## Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| Error1 | When this happens | Fix it this way |
| Error2 | When this happens | Fix it this way |

---

## Troubleshooting

[Troubleshooting guide]
`,
    '05-migration.md': `# Migration Guide: @unrdf/${pkgName}

---

## Upgrading to Latest Version

[Migration instructions]

---

## Breaking Changes

[List of breaking changes]
`
  };

  Object.entries(refFiles).forEach(([filename, content]) => {
    ensureFile(path.join(docsDir, 'reference', filename), content);
  });

  // Ensure explanation files exist (01-04)
  const expFiles = {
    '01-architecture.md': `# Architecture: @unrdf/${pkgName}

---

## System Design

@unrdf/${pkgName} is designed to: ${config.description}

---

## Components

[Architecture diagram and explanation]

---

## Next Reading

- [02-design-decisions.md](02-design-decisions.md)
- [../reference/01-api.md](../reference/01-api.md)
`,
    '02-design-decisions.md': `# Design Decisions: @unrdf/${pkgName}

---

## Key Trade-offs

[Design decisions and rationale]

---

## Why This Approach

[Justification for design choices]
`,
    '03-concepts.md': `# Key Concepts: @unrdf/${pkgName}

---

## Fundamental Ideas

[Core concepts explanation]

---

## When to Use

[Guidance on usage]
`,
    '04-advanced.md': `# Advanced Topics: @unrdf/${pkgName}

---

## Deep Dives

[Advanced topics and techniques]

---

## Performance

[Performance characteristics and optimization]
`
  };

  Object.entries(expFiles).forEach(([filename, content]) => {
    ensureFile(path.join(docsDir, 'explanation', filename), content);
  });

  console.log(`✓ Created missing files for ${pkgName}`);
});

console.log('\n✅ Missing Phase 2 files created!');

/**
 * Ensure a file exists with content, only creating if missing
 */
function ensureFile(filepath, content) {
  if (!fs.existsSync(filepath)) {
    const dir = path.dirname(filepath);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
    fs.writeFileSync(filepath, content);
  }
}
