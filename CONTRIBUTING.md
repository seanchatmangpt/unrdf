# Contributing to UNRDF

Thank you for considering contributing to UNRDF! This guide will help you get started with development, testing, and submitting contributions.

## Table of Contents

- [Development Setup](#development-setup)
- [Git Workflow](#git-workflow)
- [Coding Standards](#coding-standards)
- [Quality Gates](#quality-gates)
- [Testing](#testing)
- [Documentation](#documentation)
- [Release Process](#release-process)
- [Getting Help](#getting-help)

---

## Development Setup

### Prerequisites

- **Node.js:** 18.0.0 or higher
- **pnpm:** 7.0.0 or higher (recommended package manager)
- **Git:** Latest stable version

### Clone and Install

```bash
# Clone the repository
git clone https://github.com/unrdf/unrdf.git
cd unrdf

# Install all dependencies (monorepo + all packages)
pnpm install

# Build all packages
pnpm build

# Run tests to verify setup
pnpm test
```

### Monorepo Structure

UNRDF uses a **pnpm workspace** monorepo with 17 packages:

```
unrdf/
├── packages/
│   ├── core/              # RDF storage, SPARQL, SHACL
│   ├── hooks/             # Knowledge Hooks framework
│   ├── oxigraph/          # Rust-based triple store backend
│   ├── streaming/         # Large graph streaming
│   ├── federation/        # Distributed query execution
│   ├── cli/               # Command-line tools
│   ├── browser/           # Browser runtime support
│   └── ...                # 10 more packages
├── docs/                  # Documentation
├── examples/              # Example projects
└── pnpm-workspace.yaml    # Workspace configuration
```

See [docs/MONOREPO-QUICK-REFERENCE.md](docs/MONOREPO-QUICK-REFERENCE.md) for detailed package information.

### Local Development Workflow

```bash
# Run tests in watch mode (auto-rerun on changes)
pnpm test:watch

# Lint code
pnpm lint

# Format code
pnpm format

# Build specific package
pnpm -C packages/core build

# Test specific package
pnpm -C packages/core test

# Run all quality checks (recommended before committing)
pnpm lint && pnpm test:fast
```

### IDE Setup

**VSCode (Recommended):**
- Install ESLint extension
- Install Prettier extension
- Workspace settings are committed in `.vscode/settings.json`

**WebStorm/IntelliJ:**
- Enable ESLint in Preferences → Languages & Frameworks → JavaScript → Code Quality Tools
- Enable Prettier in Preferences → Languages & Frameworks → JavaScript → Prettier

---

## Git Workflow

UNRDF follows **trunk-based development** with merge-only workflow (no rebases).

### Branch Naming

```bash
# Feature branches
git checkout -b feature/add-sparql-federation

# Bug fixes
git checkout -b fix/memory-leak-in-streaming

# Documentation
git checkout -b docs/update-api-reference

# Chores (refactoring, deps, etc.)
git checkout -b chore/upgrade-dependencies
```

### Commit Messages

We use **Conventional Commits** format:

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `test`: Test additions or changes
- `refactor`: Code refactoring (no behavior change)
- `perf`: Performance improvements
- `chore`: Maintenance tasks (deps, config, etc.)

**Examples:**

```bash
# Good commits
git commit -m "feat(core): add SPARQL 1.1 federated query support"
git commit -m "fix(hooks): prevent memory leak in event listeners"
git commit -m "docs(api): update createKnowledgeSubstrateCore examples"

# Bad commits (avoid these)
git commit -m "update code"
git commit -m "fix bug"
git commit -m "WIP"
```

### Merge Workflow

1. **Create feature branch:**
   ```bash
   git checkout -b feature/your-feature
   ```

2. **Make changes and commit:**
   ```bash
   git add .
   git commit -m "feat(package): add your feature"
   ```

3. **Keep branch updated (merge, NOT rebase):**
   ```bash
   git checkout main
   git pull origin main
   git checkout feature/your-feature
   git merge main  # ✅ ALWAYS merge, NEVER rebase
   ```

4. **Push and create Pull Request:**
   ```bash
   git push -u origin feature/your-feature
   # Create PR on GitHub
   ```

5. **After PR approval, merge to main:**
   - Use "Merge commit" (default)
   - **DO NOT use "Rebase and merge"** (violates trunk-based workflow)

---

## Coding Standards

### JavaScript/MJS Style

- **ES Modules only** (`.mjs` file extension)
- **JSDoc type annotations** (NO TypeScript in source)
- **Zod validation** for runtime type checking
- **Pure functions** preferred (avoid side effects)

**Example:**

```javascript
/**
 * Parse RDF Turtle format into a quad store.
 * @param {string} turtleData - RDF Turtle string
 * @param {object} [options] - Parse options
 * @returns {Promise<import('n3').Store>} Quad store
 */
export async function parseTurtle(turtleData, options = {}) {
  // Implementation
}
```

### RDF/Triple Store Rules (CRITICAL)

**MANDATORY patterns:**

```javascript
// ✅ CORRECT: Use @unrdf/oxigraph
import { createStore } from '@unrdf/oxigraph';
const store = createStore();

// ✅ CORRECT: Import dataFactory from @unrdf/oxigraph
import { dataFactory } from '@unrdf/oxigraph';

// ❌ WRONG: NEVER use N3 directly in application code
import { Store } from 'n3';  // ❌ FORBIDDEN

// ✅ EXCEPTION: Only in justified modules (n3-justified-only.mjs, minimal-n3-integration.mjs)
// These modules centralize N3 usage for streaming
```

**Verification:**

```bash
# Check for forbidden N3 imports (must return 0 results outside justified modules)
grep -r "from 'n3'" packages/*/src --exclude-dir=justified --exclude-dir=minimal

# Run import validation
pnpm lint
```

### Code Quality Requirements

- **Line limit:** <500 lines per file
- **Function complexity:** Keep cyclomatic complexity <10
- **Comments:** JSDoc on all exported functions
- **Naming:** Clear, descriptive names (no abbreviations)

---

## Quality Gates

**ALL of these must pass before merging:**

### 1. Linting (ESLint)

```bash
pnpm lint
# Must show: ✓ 0 errors, 0 warnings
```

Rules enforced:
- No unused variables
- No console.log in production code (use debug logger)
- Proper JSDoc formatting
- Import/export order

### 2. Formatting (Prettier)

```bash
pnpm format:check
# Must show: All files formatted correctly
```

Auto-fix:
```bash
pnpm format
```

### 3. Tests

```bash
pnpm test
# Must show: ✓ ALL tests passing
```

**Requirements:**
- **80%+ code coverage** (minimum)
- **100% test pass rate** (0 failures, 0 flakes)
- Tests for all new features
- Tests for all bug fixes

### 4. Build

```bash
pnpm build
# Must show: Build succeeded with 0 errors
```

### 5. Pre-commit Hook

Automatically runs on `git commit`:

```bash
# Runs automatically:
pnpm lint && pnpm test:fast
```

If this fails, commit is rejected. Fix issues before committing.

---

## Testing

### Test Structure

```javascript
// packages/core/test/sparql-executor.test.mjs
import { describe, it, expect } from 'vitest';
import { query } from '../src/sparql/executor.mjs';

describe('SPARQL Executor', () => {
  it('executes SELECT queries', async () => {
    const store = createTestStore();
    const results = await query(store, 'SELECT * WHERE { ?s ?p ?o }');
    expect(results.length).toBeGreaterThan(0);
  });

  it('handles empty results', async () => {
    const store = createTestStore();
    const results = await query(store, 'SELECT * WHERE { ?s <missing> ?o }');
    expect(results).toEqual([]);
  });

  it('throws on invalid SPARQL', async () => {
    await expect(() => query(store, 'INVALID')).rejects.toThrow();
  });
});
```

### Running Tests

```bash
# All tests
pnpm test

# Specific package
pnpm -C packages/core test

# Watch mode (auto-rerun on file changes)
pnpm test:watch

# Coverage report
pnpm -C packages/core test:coverage
# Opens coverage/index.html in browser
```

### Test Best Practices

- **Test behavior, not implementation**
- **Use descriptive test names** (read like documentation)
- **Arrange-Act-Assert pattern:**
  ```javascript
  it('adds two numbers', () => {
    // Arrange
    const a = 2, b = 3;
    // Act
    const result = add(a, b);
    // Assert
    expect(result).toBe(5);
  });
  ```
- **Test edge cases:** empty inputs, null, undefined, large inputs
- **Mock external dependencies** (filesystem, network, etc.)

---

## Documentation

### Writing Documentation

**Location:**
- **User docs:** `docs/` directory (Markdown)
- **API docs:** JSDoc comments in source code
- **Examples:** `examples/` directory

**Format:**
- Use Markdown for all documentation
- Follow [Diátaxis framework](https://diataxis.fr/) (tutorials, how-to guides, explanation, reference)
- Include code examples for all features
- Link to related documentation

**Example JSDoc:**

```javascript
/**
 * Execute a SPARQL query against an RDF store.
 *
 * @param {import('n3').Store} store - The RDF quad store to query
 * @param {string} sparqlQuery - SPARQL 1.1 query string
 * @param {object} [options] - Query options
 * @param {string} [options.baseIRI] - Base IRI for relative URIs
 * @param {number} [options.timeout=30000] - Query timeout in milliseconds
 * @returns {Promise<Array<Map<string, RDFTerm>>>} Query results as bindings
 * @throws {SPARQLSyntaxError} If query has syntax errors
 * @throws {TimeoutError} If query exceeds timeout
 *
 * @example
 * const results = await query(store, `
 *   SELECT ?name WHERE {
 *     ?person foaf:name ?name .
 *   }
 * `);
 *
 * @see https://www.w3.org/TR/sparql11-query/ for SPARQL specification
 */
export async function query(store, sparqlQuery, options = {}) {
  // Implementation
}
```

### Updating Documentation

When adding features:
1. **Update API reference** (JSDoc in source)
2. **Add examples** to `examples/` directory
3. **Update user guides** in `docs/`
4. **Update CHANGELOG.md**

---

## Release Process

**For Maintainers Only**

### Versioning

UNRDF follows [Semantic Versioning](https://semver.org/):

- **MAJOR** (x.0.0): Breaking changes
- **MINOR** (0.x.0): New features (backward compatible)
- **PATCH** (0.0.x): Bug fixes (backward compatible)

### Release Checklist

1. **Update CHANGELOG.md:**
   ```markdown
   ## [5.1.0] - 2024-01-15

   ### Added
   - SPARQL 1.1 federated query support (#123)

   ### Fixed
   - Memory leak in streaming parser (#124)

   ### Changed
   - Improved query performance by 30% (#125)
   ```

2. **Bump version:**
   ```bash
   # Automatically updates all package.json versions
   pnpm version minor  # or major, patch
   ```

3. **Create git tag:**
   ```bash
   git tag v5.1.0
   git push --tags
   ```

4. **GitHub Actions auto-publishes to npm**

### Pre-release Versions

For beta/alpha releases:

```bash
pnpm version 5.1.0-beta.1
git tag v5.1.0-beta.1
git push --tags
```

---

## Getting Help

### Community

- **GitHub Discussions:** https://github.com/unrdf/unrdf/discussions
- **GitHub Issues:** https://github.com/unrdf/unrdf/issues
- **Discord:** [Coming soon]

### Documentation

- **[START-HERE.md](docs/START-HERE.md)** - Quick orientation
- **[MONOREPO-QUICK-REFERENCE.md](docs/MONOREPO-QUICK-REFERENCE.md)** - Package overview
- **[LOCAL-DEVELOPMENT.md](docs/LOCAL-DEVELOPMENT.md)** - Development environment
- **[ARCHITECTURE.md](docs/ARCHITECTURE.md)** - System design

### Asking Questions

**Before asking:**
1. Check existing [documentation](docs/)
2. Search [GitHub Issues](https://github.com/unrdf/unrdf/issues)
3. Search [GitHub Discussions](https://github.com/unrdf/unrdf/discussions)

**When asking:**
- Provide minimal reproducible example
- Include UNRDF version (`pnpm list @unrdf/core`)
- Include Node.js version (`node --version`)
- Include error messages and stack traces

---

## Code of Conduct

### Our Pledge

We are committed to providing a welcoming and inspiring community for all.

### Expected Behavior

- Use welcoming and inclusive language
- Be respectful of differing viewpoints and experiences
- Gracefully accept constructive criticism
- Focus on what is best for the community
- Show empathy towards other community members

### Unacceptable Behavior

- Trolling, insulting/derogatory comments, and personal or political attacks
- Public or private harassment
- Publishing others' private information without permission
- Other conduct which could reasonably be considered inappropriate

### Reporting

Report unacceptable behavior to: conduct@unrdf.dev

---

## License

By contributing to UNRDF, you agree that your contributions will be licensed under the MIT License.

---

## Recognition

Contributors are recognized in:
- **[CONTRIBUTORS.md](CONTRIBUTORS.md)** - All contributors
- **GitHub Insights** - Contribution statistics
- **Release notes** - Feature authors credited

Thank you for contributing to UNRDF! 🎉
