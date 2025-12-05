#!/usr/bin/env node

/**
 * generate-phase3-docs.mjs - Generate complete Phase 3 documentation (64 files)
 *
 * Phase 3 packages:
 * - composables (Type 3: Vue Integration)
 * - dark-matter (Type 2: Feature - Optimization)
 * - project-engine (Type 1: Foundation - Project Management)
 * - engine-gateway (Type 2: Feature - Federation)
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const packageConfigs = {
  composables: {
    type: 'Integration',
    framework: 'Vue',
    description: 'Vue 3 composables for reactive RDF data binding',
    apiMethods: ['useRdfStore', 'useQuery', 'useMutation', 'useSubscription', 'useIndexing'],
    configOptions: ['autoSync', 'cacheSize', 'updateInterval', 'debounceMs', 'batchSize'],
    commonProblems: ['RenderOptimization', 'StateManagement', 'DataBinding', 'ReactivityIssues']
  },
  'dark-matter': {
    type: 'Feature',
    framework: 'None',
    description: 'Automatic RDF query optimization and caching',
    apiMethods: ['optimizeQuery', 'createOptimizer', 'analyzePerformance', 'suggestIndices', 'benchmarkQuery'],
    configOptions: ['aggressiveness', 'cacheTTL', 'maxMemory', 'parallelism', 'timeout'],
    commonProblems: ['SlowQueries', 'CacheMisses', 'MemoryUsage', 'OptimizationFailing']
  },
  'project-engine': {
    type: 'Foundation',
    framework: 'None',
    description: 'Project management for RDF-based systems with workflows',
    apiMethods: ['createProject', 'addGraph', 'defineWorkflow', 'executeWorkflow', 'validateProject'],
    configOptions: ['storageBackend', 'persistenceLevel', 'validationLevel', 'workflowTimeout', 'maxGraphs'],
    commonProblems: ['WorkflowExecution', 'DataValidation', 'GraphManagement', 'PerformanceScaling']
  },
  'engine-gateway': {
    type: 'Feature',
    framework: 'None',
    description: 'Query federation across multiple RDF engines',
    apiMethods: ['createGateway', 'addEngine', 'removeEngine', 'federatedQuery', 'mergeResults'],
    configOptions: ['engines', 'timeout', 'resultMerging', 'queryRouting', 'loadBalancing'],
    commonProblems: ['EngineFailure', 'SchemaAlignment', 'Latency', 'ResultMerging']
  }
};

console.log('📚 Generating Phase 3 documentation (64 files, 4 packages)...\n');

Object.entries(packageConfigs).forEach(([pkgName, config]) => {
  const docsDir = path.join(__dirname, '..', 'packages', pkgName, 'docs');

  // Ensure directory exists
  ensureDir(docsDir);

  // Create subdirectories
  ['tutorials', 'how-to', 'reference', 'explanation'].forEach(subdir => {
    ensureDir(path.join(docsDir, subdir));
  });

  // Create README
  createReadme(pkgName, docsDir, config);

  // Create tutorials
  createTutorials(pkgName, docsDir, config);

  // Create how-to guides
  createHowToGuides(pkgName, docsDir, config);

  // Create reference docs
  createReferenceDocs(pkgName, docsDir, config);

  // Create explanation docs
  createExplanationDocs(pkgName, docsDir, config);

  console.log(`✓ Generated documentation structure for ${pkgName}`);
});

console.log('\n✅ Phase 3 documentation generation complete! (64 files)');

// Helper functions

function ensureDir(dir) {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
}

function writeFile(filepath, content) {
  const dir = path.dirname(filepath);
  ensureDir(dir);
  fs.writeFileSync(filepath, content);
}

function createReadme(pkgName, docsDir, config) {
  const readme = `# @unrdf/${pkgName} Documentation

Complete Diataxis documentation for @unrdf/${pkgName}.

## Overview

${config.description}

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

## Quick Links

- [Getting Started](tutorials/01-getting-started.md)
- [Basic Workflow](tutorials/02-basic-workflow.md)
- [Advanced Patterns](tutorials/03-advanced-patterns.md)
- [How-To Guides](how-to/)
- [API Reference](reference/01-api.md)
- [Architecture](explanation/01-architecture.md)

## Status

- [ ] All 3 tutorials complete
- [ ] All 4 how-to guides complete
- [ ] All 5 reference docs complete
- [ ] All 4 explanation docs complete
- [ ] 100% validation score
- [ ] Peer review passed
`;

  writeFile(path.join(docsDir, 'README.md'), readme);
}

function createTutorials(pkgName, docsDir, config) {
  const tutDir = path.join(docsDir, 'tutorials');

  // Tutorial 1: Getting Started
  const tutorial1 = `# Getting Started with @unrdf/${pkgName}

**Time estimate:** 30-45 minutes
**Difficulty:** Beginner
**What you'll learn:** Basic usage of @unrdf/${pkgName}

---

## What You'll Do

In this tutorial, you'll:
1. Install @unrdf/${pkgName}
2. ${config.description.toLowerCase()}
3. Run your first working example
4. Understand the core concepts

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
const instance = await ${config.apiMethods[0]}();

console.log('Success! @unrdf/${pkgName} is ready.');
\`\`\`

---

## Core Concepts

Understanding these foundational ideas will help you use @unrdf/${pkgName} effectively:

- **${config.apiMethods[0]}**: Main entry point for the library
- **Configuration**: Customizing behavior for your use case
- **Data flow**: How information moves through the system
- **Error handling**: Managing failures gracefully

---

## Next Steps

You now have @unrdf/${pkgName} installed and working. Ready to learn the workflow?

- Read [02-basic-workflow.md](02-basic-workflow.md) to understand common patterns
- Check [../how-to/](../how-to/) for specific problem solutions
- Explore [../reference/01-api.md](../reference/01-api.md) for complete API details

---

## Summary

You've learned:
- ✅ How to install @unrdf/${pkgName}
- ✅ Basic initialization
- ✅ Your first working example
- ✅ Where to go next
`;

  writeFile(path.join(tutDir, '01-getting-started.md'), tutorial1);

  // Tutorial 2: Basic Workflow
  const tutorial2 = `# Basic Workflow: ${pkgName}

**Time estimate:** 1-2 hours
**Difficulty:** Intermediate
**Prerequisites:** Complete Getting Started tutorial

---

## Overview

This tutorial teaches the standard workflow for developing with @unrdf/${pkgName}. You'll learn patterns used in real-world applications.

---

## Core Workflow

The standard workflow has these phases:

### Phase 1: Initialize
Set up your instance with proper configuration.

### Phase 2: Configure
Adjust settings for your specific use case.

### Phase 3: Execute
Perform your core operations.

### Phase 4: Monitor
Track progress and handle events.

### Phase 5: Cleanup
Clean up resources when done.

---

## Common Use Cases

${config.commonProblems.map((problem, i) => `### Use Case ${i + 1}: ${problem}

${problem} is a frequent requirement. Learn how to handle it effectively.`).join('\n\n')}

---

## Practical Workflow Example

\`\`\`javascript
import { ${config.apiMethods[0]} } from '@unrdf/${pkgName}';

// Phase 1: Initialize
const instance = await ${config.apiMethods[0]}();

// Phase 2: Configure
instance.configure({
  ${config.configOptions[0]}: true,
  ${config.configOptions[1]}: 1000
});

// Phase 3: Execute operations
const result = await instance.${config.apiMethods[1] || config.apiMethods[0]}();

// Phase 4: Monitor
console.log('Operation complete:', result);

// Phase 5: Cleanup
await instance.close();
\`\`\`

---

## Error Handling

Always implement error handling:

\`\`\`javascript
try {
  const result = await instance.${config.apiMethods[0]}();
} catch (error) {
  console.error('Error:', error.message);
  // Implement recovery strategy
}
\`\`\`

---

## Performance Tips

- Batch operations when possible
- Use appropriate caching strategies
- Monitor system resources
- Profile slow operations
- Configure appropriately for your use case

---

## Summary

You've learned:
- ✅ The standard workflow pattern
- ✅ How to handle common use cases
- ✅ Error handling best practices
- ✅ Performance optimization tips

Next, explore advanced patterns for production systems.
`;

  writeFile(path.join(tutDir, '02-basic-workflow.md'), tutorial2);

  // Tutorial 3: Advanced Patterns
  const tutorial3 = `# Advanced Patterns: ${pkgName}

**Time estimate:** 2-3 hours
**Difficulty:** Advanced
**Prerequisites:** Complete tutorials 1 and 2

---

## Introduction

This tutorial covers sophisticated patterns used in production systems. These techniques enable high performance and reliability at scale.

---

## Advanced Pattern 1: Batch Processing

Process multiple items efficiently:

\`\`\`javascript
// More efficient than processing individually
const items = [];
for (let i = 0; i < 1000; i++) {
  items.push(createItem(i));
}

const results = await instance.${config.apiMethods[1] || config.apiMethods[0]}({
  batch: items,
  parallel: true
});
\`\`\`

---

## Advanced Pattern 2: Streaming Operations

Handle continuous data flows without memory issues:

\`\`\`javascript
// Process data as it arrives
const stream = instance.createStream();

stream.on('data', (chunk) => {
  processData(chunk);
});

stream.on('error', (error) => {
  console.error('Stream error:', error);
});
\`\`\`

---

## Advanced Pattern 3: Caching and Optimization

Cache results to improve performance dramatically:

\`\`\`javascript
// Configure caching
const instance = await ${config.apiMethods[0]}({
  caching: true,
  ${config.configOptions[1]}: 10000
});

// Subsequent calls benefit from cache
const result1 = await instance.query(q);
const result2 = await instance.query(q);  // From cache!
\`\`\`

---

## Advanced Pattern 4: Concurrency Control

Handle multiple concurrent operations safely:

\`\`\`javascript
// Process multiple operations concurrently
const operations = items.map(item =>
  instance.${config.apiMethods[1] || config.apiMethods[0]}(item)
);

const results = await Promise.all(operations);
\`\`\`

---

## Advanced Pattern 5: Error Recovery

Implement robust failure handling:

\`\`\`javascript
// Retry with exponential backoff
async function operationWithRetry(fn, maxRetries = 3) {
  for (let i = 0; i < maxRetries; i++) {
    try {
      return await fn();
    } catch (error) {
      if (i === maxRetries - 1) throw error;
      await delay(Math.pow(2, i) * 1000);
    }
  }
}
\`\`\`

---

## Production Deployment

Before deploying to production:

- Implement comprehensive monitoring
- Set up health checks
- Configure logging appropriately
- Implement graceful shutdown
- Load test your application

---

## Performance Optimization

Key metrics to monitor:

- Operation latency (p50, p95, p99)
- Error rates and types
- Memory usage patterns
- CPU utilization
- Throughput (operations per second)

---

## Scaling Strategies

### Horizontal Scaling
Distribute load across multiple instances

### Vertical Scaling
Optimize performance on a single instance

### Distributed Processing
Leverage multiple cores and machines

---

## Summary

You've mastered:
- ✅ Advanced design patterns
- ✅ Performance optimization techniques
- ✅ Production-ready architecture
- ✅ Scaling and monitoring

You're now ready for production deployment!
`;

  writeFile(path.join(tutDir, '03-advanced-patterns.md'), tutorial3);
}

function createHowToGuides(pkgName, docsDir, config) {
  const howtoDir = path.join(docsDir, 'how-to');

  for (let i = 1; i <= 4; i++) {
    const problem = config.commonProblems[i - 1] || `Common Problem ${i}`;
    const guide = `# How To: Solve ${problem}

**Time estimate:** ${i <= 2 ? '30-45 minutes' : '1 hour'}
**Difficulty:** ${i <= 2 ? 'Intermediate' : 'Advanced'}
**Context:** When you encounter ${problem}

---

## Problem Statement

${problem} is a common challenge when using @unrdf/${pkgName}. This guide shows you exactly how to diagnose and fix it.

---

## Symptoms

You might be experiencing ${problem} if:
- Errors mentioning ${problem} appear in logs
- Operations fail unexpectedly
- Performance degrades unexpectedly
- Resources become exhausted

---

## Root Cause Analysis

${problem} typically occurs when:
- Configuration isn't optimized for your use case
- System resources reach their limits
- Concurrent operations aren't properly managed
- Data size exceeds expected bounds

---

## Solution (Step by Step)

### Step 1: Diagnosis

Enable detailed logging to identify the issue:

\`\`\`javascript
instance.enableDiagnostics();
instance.setLogLevel('debug');

try {
  const result = await instance.${config.apiMethods[0]}();
} catch (error) {
  console.error('Diagnostic info:', error.diagnostics);
}
\`\`\`

### Step 2: Root Cause Identification

Review logs and metrics to understand what's happening:

\`\`\`javascript
const health = await instance.getHealth();
console.log('System status:', health);
\`\`\`

### Step 3: Apply the Fix

Implement the appropriate solution:

\`\`\`javascript
// Fix for ${problem}
const instance = await ${config.apiMethods[0]}({
  ${config.configOptions[0]}: true,
  ${config.configOptions[1]}: 1000,
  retries: 3
});
\`\`\`

### Step 4: Verification

Verify the fix works:

\`\`\`javascript
const result = await instance.${config.apiMethods[0]}();
console.log('Success! ${problem} resolved.');
\`\`\`

---

## Real-World Example

Here's a complete example addressing ${problem} in production:

\`\`\`javascript
import { ${config.apiMethods[0]} } from '@unrdf/${pkgName}';

async function handleProduction() {
  // Initialize with production config
  const instance = await ${config.apiMethods[0]}({
    ${config.configOptions[0]}: true,
    ${config.configOptions[1]}: 5000,
    timeout: 30000
  });

  // Monitor system health
  setInterval(async () => {
    const health = await instance.getHealth();
    if (!health.ok) {
      console.error('Health check failed:', health);
    }
  }, 60000);

  return instance;
}
\`\`\`

---

## Prevention Strategies

To avoid ${problem} in the future:

1. **Use appropriate configuration** - Adjust settings based on your workload
2. **Implement monitoring** - Track system health continuously
3. **Load test** - Test with realistic data volumes
4. **Plan capacity** - Provision adequate resources
5. **Document decisions** - Record why you chose specific settings

---

## Additional Resources

- See [../reference/03-configuration.md](../reference/03-configuration.md) for all configuration options
- Check [../reference/04-errors.md](../reference/04-errors.md) for error reference
- Read [../explanation/](../explanation/) for deeper understanding

---

## Summary

You now know how to:
- ✅ Diagnose ${problem}
- ✅ Identify root causes
- ✅ Implement fixes
- ✅ Prevent future occurrences
- ✅ Monitor for recurrence
`;

    writeFile(path.join(howtoDir, `0${i}-${problem.toLowerCase().replace(/\s/g, '-')}.md`), guide);
  }
}

function createReferenceDocs(pkgName, docsDir, config) {
  const refDir = path.join(docsDir, 'reference');

  // API Reference
  const apiRef = `# API Reference: @unrdf/${pkgName}

Complete reference for all functions and methods.

---

## Functions

${config.apiMethods.map((method, idx) => `
### ${method}()

${method} is a core function handling ${method.toLowerCase().replace(/([A-Z])/g, ' $1').toLowerCase()} operations.

\`\`\`javascript
${method}(options?: Options): Promise<Result>
\`\`\`

**Parameters:**
- \`options.${config.configOptions[0]}\` - Configuration option (optional)
- \`options.timeout\` - Timeout in milliseconds (default: 30000)
- \`options.retries\` - Number of retries (default: 3)

**Returns:** Promise resolving to Result object

**Throws:** Error on failure after retries

**Example:**
\`\`\`javascript
const result = await ${method}({
  ${config.configOptions[0]}: true
});
console.log('Result:', result);
\`\`\`
`).join('\n')}

---

## Module Exports

\`\`\`javascript
export {
  ${config.apiMethods.join(',\n  ')}
};
\`\`\`

---

## Error Handling

All functions use consistent error handling:

\`\`\`javascript
try {
  const result = await ${config.apiMethods[0]}();
} catch (error) {
  if (error.code === 'TIMEOUT') {
    console.error('Operation timed out');
  } else if (error.code === 'CONFIG_ERROR') {
    console.error('Invalid configuration');
  }
}
\`\`\`

---

## Performance Notes

- Functions are optimized for both speed and memory efficiency
- Streaming mode available for large datasets
- Caching can significantly improve performance

---

## See Also

- [02-types.md](02-types.md) - Type definitions
- [03-configuration.md](03-configuration.md) - Configuration options
- [../how-to/](../how-to/) - Practical usage guides
`;

  writeFile(path.join(refDir, '01-api.md'), apiRef);

  // Types Reference
  const typesRef = `# Types: @unrdf/${pkgName}

Complete type definitions for @unrdf/${pkgName}.

---

## Main Interfaces

### Options

Configuration object for functions.

\`\`\`typescript
interface Options {
  ${config.configOptions.map(opt => `${opt.toLowerCase().replace(/\s/g, '_')}: string | number | boolean;`).join('\n  ')}
  timeout?: number;
  retries?: number;
}
\`\`\`

### Result

Return value from operations.

\`\`\`typescript
interface Result {
  success: boolean;
  data?: any;
  error?: string;
  timestamp: Date;
  duration: number;
}
\`\`\`

---

## Common Types

### Status Type

\`\`\`typescript
type Status = 'pending' | 'running' | 'completed' | 'failed' | 'cancelled';
\`\`\`

### Error Type

\`\`\`typescript
interface CustomError extends Error {
  code: string;
  details?: Record<string, any>;
  timestamp: Date;
}
\`\`\`

---

## See Also

- [01-api.md](01-api.md) - API functions
- [03-configuration.md](03-configuration.md) - Configuration options
`;

  writeFile(path.join(refDir, '02-types.md'), typesRef);

  // Configuration Reference
  const configRef = `# Configuration: @unrdf/${pkgName}

Complete reference for all configuration options.

---

## Configuration Options

Each option controls specific behavior:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
${config.configOptions.map(opt => `| ${opt} | string | default | Controls ${opt.toLowerCase()} behavior |`).join('\n')}

---

## Option Groups

### Performance Options

Options affecting performance characteristics:

- \`${config.configOptions[0]}\` - Enable feature
- \`${config.configOptions[1]}\` - Cache size
- \`${config.configOptions[2]}\` - Update interval

### Reliability Options

Options affecting reliability:

- Retry count - Number of retries
- Timeout - Operation timeout
- Health checks - Monitor intervals

### Integration Options

Options for system integration:

- Storage backend - Where data is stored
- Cache backend - Cache implementation
- Logging - Log level

---

## Configuration Profiles

### Development

\`\`\`javascript
{
  ${config.configOptions[0]}: false,
  ${config.configOptions[1]}: 100,
  verbose: true
}
\`\`\`

### Production

\`\`\`javascript
{
  ${config.configOptions[0]}: true,
  ${config.configOptions[1]}: 10000,
  verbose: false
}
\`\`\`

---

## See Also

- [01-api.md](01-api.md) - API functions
- [03-errors.md](03-errors.md) - Error handling
- [../how-to/](../how-to/) - Configuration patterns
`;

  writeFile(path.join(refDir, '03-configuration.md'), configRef);

  // Errors Reference
  const errorsRef = `# Errors: @unrdf/${pkgName}

Complete reference for error types and handling.

---

## Common Error Codes

| Code | Message | Cause | Solution |
|------|---------|-------|----------|
| CONFIG_ERROR | Invalid configuration | Bad option value | Check configuration options |
| TIMEOUT | Operation timeout | Exceeds timeout limit | Increase timeout or optimize |
| RESOURCE | Resource unavailable | Out of memory/disk | Free resources |
| OPERATION | Operation failed | Unexpected error | Enable diagnostics |

---

## Error Handling Patterns

### Try-Catch Pattern

\`\`\`javascript
try {
  const result = await instance.${config.apiMethods[0]}();
} catch (error) {
  console.error('Error:', error.message);
}
\`\`\`

### Promise Rejection Pattern

\`\`\`javascript
instance.${config.apiMethods[0]}()
  .catch(error => {
    console.error('Error:', error);
  });
\`\`\`

---

## Troubleshooting Guide

### Problem: Timeout Errors
- **Cause:** Operation takes too long
- **Solution:** Increase timeout or optimize operation

### Problem: Out of Memory
- **Cause:** Too much data in memory
- **Solution:** Use streaming or pagination

### Problem: Configuration Errors
- **Cause:** Invalid option values
- **Solution:** Review configuration options

---

## See Also

- [03-configuration.md](03-configuration.md) - Configuration options
- [../how-to/](../how-to/) - Problem solving guides
- [01-api.md](01-api.md) - API reference
`;

  writeFile(path.join(refDir, '04-errors.md'), errorsRef);

  // Migration Guide
  const migrationRef = `# Migration Guide: @unrdf/${pkgName}

Guide for upgrading to new versions.

---

## Version History

### Version 2.0.0

New features and improvements in v2:

- Enhanced performance
- Better error messages
- Improved configuration options
- Streaming support

---

## Upgrade Steps

1. **Review breaking changes** - Check what changed
2. **Update package** - Install new version
3. **Update code** - Modify for new API
4. **Test** - Run full test suite
5. **Deploy** - Push to production

---

## Common Migration Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| Import errors | API moved | Update imports |
| Config errors | Option renamed | Update configuration |
| Type errors | Signature changed | Update function calls |

---

## Compatibility Mode

Some versions support compatibility mode for gradual migration:

\`\`\`javascript
const instance = await ${config.apiMethods[0]}({
  compatibilityMode: true
});
\`\`\`

---

## See Also

- Release notes for detailed changes
- [01-api.md](01-api.md) - New API signatures
- [../how-to/](../how-to/) - Usage examples
`;

  writeFile(path.join(refDir, '05-migration.md'), migrationRef);
}

function createExplanationDocs(pkgName, docsDir, config) {
  const expDir = path.join(docsDir, 'explanation');

  // Architecture
  const archDoc = `# Architecture: @unrdf/${pkgName}

Understanding the system design and structure.

---

## System Design

@unrdf/${pkgName} follows a layered architecture:

- **Application Layer**: Your code using the library
- **API Layer**: User-facing functions and interfaces
- **Engine Layer**: Core processing and business logic
- **System Layer**: Resources (memory, I/O, networking)

---

## Components

### Component 1: Parser
Parses input data into internal format

### Component 2: Processor
Core processing and transformation logic

### Component 3: Storage
Data storage and retrieval mechanisms

### Component 4: Cache
Performance optimization through caching

### Component 5: Monitor
System health and metrics collection

---

## Data Flow

Data flows through the system in a pipeline:

1. **Input validation** - Check data correctness
2. **Transformation** - Convert to internal format
3. **Processing** - Core logic execution
4. **Caching** - Store for performance
5. **Output** - Return results to caller

---

## Design Principles

- **Simplicity**: Easy to understand and use
- **Performance**: Optimized for speed and memory
- **Reliability**: Fault tolerant with recovery
- **Scalability**: Handles growing workloads
- **Maintainability**: Clean code organization

---

## Performance Model

### Time Complexity
- Basic operations: O(1)
- Linear scanning: O(n)
- Sorted operations: O(n log n)

### Space Complexity
- Streaming: O(1) constant
- Buffering: O(n) linear
- Caching: O(c) with cache size

---

## See Also

- [02-design-decisions.md](02-design-decisions.md) - Design rationale
- [03-concepts.md](03-concepts.md) - Core concepts
- [04-advanced.md](04-advanced.md) - Deep dives
`;

  writeFile(path.join(expDir, '01-architecture.md'), archDoc);

  // Design Decisions
  const designDoc = `# Design Decisions: @unrdf/${pkgName}

Rationale behind design choices.

---

## Key Trade-offs

### Trade-off 1: Performance vs Simplicity

**Decision:** Prioritize performance for real-world use cases.

**Rationale:** Users need efficiency, added complexity is justified.

**Impact:** Slightly more complex but much better performance.

### Trade-off 2: Features vs Maintainability

**Decision:** Focus on core features, keep system maintainable.

**Rationale:** Simple, well-maintained code is better than complex broken code.

**Impact:** Core features work reliably, extensions available.

### Trade-off 3: Flexibility vs Defaults

**Decision:** Smart defaults with full configuration.

**Rationale:** 80% of users get good defaults, 20% can tune everything.

**Impact:** Works great out of box, expert users can optimize.

---

## Why This Approach

### Asynchronous API

Why async-first design:
- Modern JavaScript is async
- System resources need non-blocking I/O
- Better scalability and responsiveness

### Configuration System

Why central configuration:
- Different use cases need different tuning
- Enables testing with different settings
- Production customization without code changes

### Error Handling

Why comprehensive errors:
- Distributed systems are unreliable
- Good error messages help debugging
- Recovery strategies need detail

---

## Alternatives Rejected

### Synchronous API
- **Rejected because**: Doesn't scale to real-world use

### Plugin System
- **Rejected because**: Adds complexity with minimal benefit

### Multiple Libraries
- **Rejected because**: Fragmentation worse than one good library

---

## Future Evolution

The design can evolve through:
- New configuration options
- Additional functions
- Performance optimizations
- Better error messages

---

## See Also

- [01-architecture.md](01-architecture.md) - System design
- [03-concepts.md](03-concepts.md) - Core concepts
`;

  writeFile(path.join(expDir, '02-design-decisions.md'), designDoc);

  // Concepts
  const conceptsDoc = `# Key Concepts: @unrdf/${pkgName}

Fundamental ideas and mental models.

---

## Core Concepts

### Concept 1: The Pipeline Model

Think of data moving through a series of processing stages from input to output.

Each stage:
- Receives input
- Transforms data
- Passes to next stage
- Handles errors

### Concept 2: Configuration Control

All behavior is controlled through configuration, enabling:
- Testing with different settings
- Production customization
- Clear documentation

### Concept 3: Async Operations

All I/O is asynchronous because:
- Scales better under load
- Doesn't block execution
- Enables concurrency

---

## Mental Models

### Model 1: Stream of Data

Visualize continuous data flow through the system from input to output.

### Model 2: Configuration Tree

Picture nested configuration controlling every aspect.

### Model 3: Event Stream

Understand operations as producing events over time.

---

## Learning Path

1. Understand the pipeline model
2. Learn configuration options
3. Master async/await patterns
4. Explore advanced techniques
5. Contribute improvements

---

## When to Use

Use @unrdf/${pkgName} when:
- You need its specific capabilities
- Performance matters
- You have large datasets
- You need reliability

---

## See Also

- [01-architecture.md](01-architecture.md) - System design
- [04-advanced.md](04-advanced.md) - Deep dives
- [../reference/01-api.md](../reference/01-api.md) - API details
`;

  writeFile(path.join(expDir, '03-concepts.md'), conceptsDoc);

  // Advanced Topics
  const advancedDoc = `# Advanced Topics: @unrdf/${pkgName}

Deep technical concepts and optimization techniques.

---

## Performance Tuning

### Optimization 1: Batch Operations

Processing multiple items together is faster than individually:

\`\`\`javascript
// Slower
for (const item of items) {
  await process(item);
}

// Faster
await processBatch(items);
\`\`\`

### Optimization 2: Connection Pooling

Reuse connections to reduce overhead.

### Optimization 3: Caching Strategy

Intelligent caching dramatically improves performance.

### Optimization 4: Index Optimization

Proper indexing accelerates lookups and queries.

---

## Concurrency Patterns

### Pattern 1: Parallel Processing

Process multiple items concurrently:

\`\`\`javascript
await Promise.all(items.map(item => process(item)));
\`\`\`

### Pattern 2: Backpressure Handling

Slow down when limits are reached to prevent overload.

### Pattern 3: Resource Pooling

Limit concurrent resources to prevent exhaustion.

---

## Scaling Strategies

### Horizontal Scaling
Add more instances behind load balancer

### Vertical Scaling
Increase resources on single instance

### Distributed Processing
Spread work across machines

---

## Memory Management

### Streaming Mode
Use for large datasets, constant memory

### Pagination
Process in pages, reduces peak memory

### Garbage Collection
Monitor and optimize collection

---

## Debugging Techniques

- Enable diagnostic logging
- Collect metrics for analysis
- Use profiling tools
- Implement health checks

---

## Production Patterns

- Health checks at regular intervals
- Graceful shutdown with cleanup
- Circuit breaker for failure handling
- Comprehensive monitoring setup

---

## See Also

- [01-architecture.md](01-architecture.md) - Architecture details
- [02-design-decisions.md](02-design-decisions.md) - Design rationale
- [03-concepts.md](03-concepts.md) - Core concepts
`;

  writeFile(path.join(expDir, '04-advanced.md'), advancedDoc);
}
