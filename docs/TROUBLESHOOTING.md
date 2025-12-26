# UNRDF Troubleshooting Guide

Common issues and their solutions. If you don't find your issue here, check [GitHub Issues](https://github.com/unrdf/unrdf/issues) or ask in [Discussions](https://github.com/unrdf/unrdf/discussions).

## Table of Contents

- [Installation Issues](#installation-issues)
- [Build Issues](#build-issues)
- [Test Issues](#test-issues)
- [Runtime Issues](#runtime-issues)
- [Development Workflow Issues](#development-workflow-issues)
- [Performance Issues](#performance-issues)

---

## Installation Issues

### "pnpm: command not found"

**Symptom:**
```bash
$ pnpm install
pnpm: command not found
```

**Cause:** pnpm is not installed globally.

**Solution:**
```bash
npm install -g pnpm
pnpm --version  # Verify installation
```

**Alternative (using npm):**
```bash
npm install -g @pnpm/exe
```

---

### "Cannot find module '@unrdf/core'"

**Symptom:**
```
Error: Cannot find module '@unrdf/core'
```

**Cause:** Dependencies not installed or packages not built.

**Solution:**
```bash
# Reinstall dependencies
pnpm install

# Build all packages
pnpm run build

# Verify installation
pnpm list --depth 0 | grep @unrdf
```

---

### Node Version Too Old

**Symptom:**
```
Error: The engine "node" is incompatible with this module.
Expected version ">=18.0.0". Got "16.x.x"
```

**Cause:** UNRDF requires Node.js 18 or higher.

**Solution:**

Using nvm:
```bash
nvm install 18
nvm use 18
node --version  # Should show v18.x.x or higher
```

Or download from [nodejs.org](https://nodejs.org).

---

### Peer Dependency Warnings

**Symptom:**
```
WARN  Issues with peer dependencies found
```

**Cause:** Version conflicts in dependencies.

**Solution:**
```bash
# Usually safe to ignore warnings
# If builds fail, try:
pnpm install --force

# Or clean and reinstall:
rm -rf node_modules pnpm-lock.yaml
pnpm install
```

---

## Build Issues

### Build Fails with "Permission denied"

**Symptom:**
```
EACCES: permission denied, mkdir '/home/user/unrdf/packages/core/dist'
```

**Cause:** Incorrect file permissions.

**Solution:**
```bash
# Fix ownership
sudo chown -R $(whoami) .

# Retry build
pnpm run build
```

---

### "Cannot find module './dist/index.mjs'"

**Symptom:**
```
Error: Cannot find module './dist/index.mjs'
```

**Cause:** Package not built yet.

**Solution:**
```bash
# Build all packages
pnpm run build

# Or build specific package
pnpm --filter @unrdf/core run build
```

---

### Build Hangs or Takes Forever

**Symptom:** Build process seems stuck.

**Cause:** Dependency resolution issue or network problem.

**Solution:**
```bash
# Cancel build (Ctrl+C)

# Clear pnpm cache
pnpm store prune

# Reinstall
rm -rf node_modules pnpm-lock.yaml
pnpm install
pnpm run build
```

---

### TypeScript Errors in Build

**Symptom:**
```
error TS2307: Cannot find module '@unrdf/core'
```

**Cause:** TypeScript definitions not generated.

**Solution:**
```bash
# Build dependencies first
pnpm --filter @unrdf/core run build
pnpm --filter @unrdf/hooks run build

# Then build package with errors
pnpm --filter @unrdf/YOUR-PACKAGE run build
```

---

## Test Issues

### Tests Fail After Fresh Install

**Symptom:**
```
FAIL packages/core/test/parse.test.mjs
```

**Cause:** Known test failures or environment issues.

**Solution:**
```bash
# Check if it's a known issue
cat TEST-RESULTS.md  # If exists

# Try rebuilding
pnpm run build
pnpm test

# Run specific test to diagnose
pnpm --filter @unrdf/core test -- parse.test.mjs
```

---

### "Timeout of 5000ms exceeded"

**Symptom:**
```
Error: Timeout of 5000ms exceeded
```

**Cause:** Test taking too long (network, large dataset, etc.).

**Solution:**

In test file, increase timeout:
```javascript
import { describe, it, expect } from 'vitest';

describe('slow test', () => {
  it('processes large dataset', async () => {
    // Increase timeout to 30 seconds
    vi.setConfig({ testTimeout: 30000 });

    // ... test code
  });
});
```

Or run with timeout flag:
```bash
pnpm test -- --testTimeout=30000
```

---

### Tests Pass Locally but Fail in CI

**Symptom:** Tests pass on your machine but fail in GitHub Actions.

**Cause:** Environment differences (file paths, timing, resources).

**Solution:**
- Check CI logs for specific error
- Ensure tests don't depend on local file paths
- Make tests deterministic (avoid race conditions)
- Add debugging output to CI logs

---

## Runtime Issues

### "Invalid or incomplete RDF data"

**Symptom:**
```
Error: Invalid or incomplete RDF data
```

**Cause:** Malformed Turtle/N-Triples syntax.

**Solution:**

Validate your RDF:
```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();

try {
  const store = core.parseRdf(`
    @prefix ex: <http://example.org/> .
    ex:Alice ex:knows ex:Bob .
  `);
  console.log('Valid RDF');
} catch (error) {
  console.error('Invalid RDF:', error.message);
  // Check syntax - missing period, quotes, etc.
}
```

Common syntax errors:
- Missing period (`.`) at end of statement
- Missing prefix declaration
- Unescaped quotes in literals
- Invalid URIs

---

### "SPARQL query failed"

**Symptom:**
```
Error: SPARQL query failed: Syntax error at line X
```

**Cause:** Invalid SPARQL syntax.

**Solution:**

Test query step by step:
```javascript
// Start simple
const query1 = 'SELECT * WHERE { ?s ?p ?o }';

// Add complexity gradually
const query2 = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name
  }
`;

// Check results
const results = await core.query(store, query1);
console.log(results);
```

Use a SPARQL validator: https://sparql.org/query-validator.html

---

### Memory Issues with Large Graphs

**Symptom:**
```
FATAL ERROR: Reached heap limit Allocation failed - JavaScript heap out of memory
```

**Cause:** Loading too much data into memory at once.

**Solution:**

Use streaming:
```javascript
import { createReadStream } from 'fs';

// Instead of loading entire file
// const data = await readFile('huge.ttl', 'utf-8');

// Stream in chunks
const stream = createReadStream('huge.ttl');
let buffer = '';

for await (const chunk of stream) {
  buffer += chunk;

  if (buffer.length > 100000) {
    // Process chunk
    const store = core.parseRdf(buffer);
    // ... process store
    buffer = '';
  }
}
```

Or increase Node.js memory:
```bash
node --max-old-space-size=8192 your-script.mjs
```

---

### "Store is not defined"

**Symptom:**
```
ReferenceError: store is not defined
```

**Cause:** Trying to use store before creation.

**Solution:**

Ensure proper initialization:
```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

// Create core first
const core = await createKnowledgeSubstrateCore();

// Then create store
const store = core.parseRdf('...');

// Now you can use store
const results = await core.query(store, '...');
```

---

## Development Workflow Issues

### "Can't push to GitHub"

**Symptom:**
```
ERROR: Permission to unrdf/unrdf.git denied
```

**Cause:** Trying to push to upstream instead of your fork.

**Solution:**
```bash
# Check remotes
git remote -v

# Should see:
# origin    git@github.com:YOUR-USERNAME/unrdf.git
# upstream  https://github.com/unrdf/unrdf.git

# Push to YOUR fork
git push origin feat/your-branch

# NOT to upstream!
```

---

### Merge Conflicts

**Symptom:**
```
CONFLICT (content): Merge conflict in packages/core/src/parse.mjs
```

**Solution:**
```bash
# 1. Update main branch
git checkout main
git pull upstream main

# 2. Rebase your feature branch
git checkout feat/your-feature
git rebase main

# 3. Resolve conflicts in your editor
# Look for <<<<<<< HEAD markers

# 4. After fixing conflicts
git add .
git rebase --continue

# 5. Force push (only to YOUR fork!)
git push origin feat/your-feature --force
```

---

### "Detached HEAD state"

**Symptom:**
```
You are in 'detached HEAD' state.
```

**Cause:** Checked out a specific commit instead of a branch.

**Solution:**
```bash
# Create a branch from current position
git checkout -b fix/my-fix

# Or switch to an existing branch
git checkout main
```

---

### Pre-commit Hook Failures

**Symptom:**
```
pre-commit hook failed
```

**Cause:** Lint or test failures.

**Solution:**
```bash
# Fix linting issues
pnpm run lint:fix

# Run tests
pnpm test

# If you need to commit anyway (not recommended)
git commit --no-verify
```

---

## Performance Issues

### Slow SPARQL Queries

**Symptom:** Queries take many seconds to execute.

**Cause:** Inefficient query pattern or large dataset.

**Solution:**

Optimize query:
```javascript
// Bad - Cartesian product
const slow = `
  SELECT * WHERE {
    ?s1 ?p1 ?o1 .
    ?s2 ?p2 ?o2 .
  }
`;

// Good - Specific pattern
const fast = `
  SELECT ?name WHERE {
    ?person foaf:name ?name .
    FILTER (CONTAINS(?name, "Alice"))
  }
`;
```

Use indexes or persistent storage:
```javascript
import { createStore } from '@unrdf/oxigraph';

// Persistent store with indexes
const store = await createStore({
  type: 'persistent',
  path: './data/store.db'
});
```

---

### High Memory Usage

**Symptom:** Application uses excessive RAM.

**Cause:** Large in-memory stores or memory leaks.

**Solution:**

Monitor memory:
```javascript
console.log('Memory:', process.memoryUsage());

// Before operation
const before = process.memoryUsage().heapUsed;

// ... operation

// After operation
const after = process.memoryUsage().heapUsed;
console.log('Memory used:', (after - before) / 1024 / 1024, 'MB');
```

Use streaming for large datasets (see above).

Clear stores when done:
```javascript
store.clear();
store = null;
```

---

### Slow Package Installation

**Symptom:** `pnpm install` takes very long.

**Cause:** Network issues or large node_modules.

**Solution:**
```bash
# Use fast mirror
pnpm config set registry https://registry.npmmirror.com

# Or clean cache and retry
pnpm store prune
pnpm install --force
```

---

## Getting More Help

### Check Existing Resources

1. **Search Issues**: https://github.com/unrdf/unrdf/issues
2. **Read Docs**: Start with [docs/START-HERE.md](START-HERE.md)
3. **Check Examples**: Look in `examples/` directory
4. **Review Tests**: Tests show correct usage patterns

### Ask for Help

1. **GitHub Discussions**: https://github.com/unrdf/unrdf/discussions
   - Best for questions and general help
   - Tag with relevant labels

2. **Open an Issue**: https://github.com/unrdf/unrdf/issues/new
   - For bugs or feature requests
   - Include:
     - Error message (full stack trace)
     - Minimal reproduction code
     - Environment (Node version, OS, package versions)
     - What you've tried

### Provide Good Bug Reports

Include:
```markdown
## Description
Brief description of the issue

## Environment
- Node.js version: 18.19.0
- UNRDF version: 5.0.1
- OS: Ubuntu 22.04
- Package manager: pnpm 8.15.0

## Steps to Reproduce
1. Step 1
2. Step 2
3. Step 3

## Expected Behavior
What should happen

## Actual Behavior
What actually happens

## Code Sample
```javascript
// Minimal code that reproduces the issue
```

## Error Output
```
Full error message and stack trace
```

## What I've Tried
- Tried X, didn't work
- Tried Y, didn't work
```

---

**Still stuck?** Don't hesitate to ask in [Discussions](https://github.com/unrdf/unrdf/discussions). We're here to help!
