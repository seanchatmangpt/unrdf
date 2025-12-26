# Contributing to UNRDF

Welcome! We're excited you want to contribute to UNRDF. This guide will get you from zero to your first merged PR in under an hour.

## Quick Links

- **First time here?** → [docs/ONBOARDING.md](docs/ONBOARDING.md) - Step-by-step setup guide
- **Need examples?** → [docs/WALKTHROUGHS.md](docs/WALKTHROUGHS.md) - Text-based tutorials
- **Detailed guidelines** → [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md) - Complete contribution guide
- **Monorepo structure** → [docs/MONOREPO-QUICK-REFERENCE.md](docs/MONOREPO-QUICK-REFERENCE.md) - Package overview
- **Development setup** → [docs/LOCAL-DEVELOPMENT.md](docs/LOCAL-DEVELOPMENT.md) - Dev environment guide

## The 5-Minute Quick Start

### 1. Prerequisites

You need:
- **Node.js 18+** ([download](https://nodejs.org))
- **pnpm** (install with `npm install -g pnpm`)
- **Git**

### 2. Get the Code

```bash
git clone https://github.com/unrdf/unrdf.git
cd unrdf
pnpm install
```

### 3. Verify Everything Works

```bash
# Build all packages (takes ~2 minutes first time)
pnpm run build

# Run tests (should see mostly passing tests)
pnpm test
```

### 4. Make Your First Change

```bash
# Create a feature branch
git checkout -b feat/my-first-contribution

# Make changes to any file
# For example, fix a typo in README.md

# Run tests to verify nothing broke
pnpm test

# Commit your changes
git add .
git commit -m "docs: fix typo in README"

# Push and create a PR
git push -u origin feat/my-first-contribution
```

That's it! You're ready to contribute.

## Types of Contributions

### Documentation (EASIEST - Great First Issue)
- Fix typos or unclear explanations
- Add examples to existing docs
- Improve error messages
- Write tutorials

**Files to edit:** `docs/*.md`, `README.md`, JSDoc comments in `packages/*/src/*.mjs`

### Bug Fixes
- Fix failing tests
- Resolve GitHub issues
- Handle edge cases

**Process:**
1. Find an issue labeled `good first issue`
2. Comment saying you'll work on it
3. Create a PR with the fix

### New Features
- Add new RDF operations
- Implement SPARQL extensions
- Create new packages

**Process:**
1. Open an issue describing the feature
2. Wait for maintainer feedback
3. Implement with tests
4. Submit PR

## Code Standards

### File Format
- Use `.mjs` extension (ES modules)
- Use JSDoc for type hints (NO TypeScript in source)
- Validate runtime values with Zod schemas

### Example Pattern

```javascript
/**
 * Parse RDF data from Turtle format
 * @param {string} turtle - Turtle syntax RDF data
 * @param {Object} [options] - Parse options
 * @param {string} [options.baseIRI] - Base IRI for relative URIs
 * @returns {Promise<Store>} RDF store containing parsed triples
 *
 * @example
 * const store = await parseTurtle(`
 *   @prefix ex: <http://example.org/> .
 *   ex:Alice ex:knows ex:Bob .
 * `);
 */
export async function parseTurtle(turtle, options = {}) {
  // Zod validation
  const schema = z.string().min(1);
  schema.parse(turtle);

  // Implementation with error handling
  try {
    // ... implementation
  } catch (error) {
    throw new Error(`parseTurtle failed: ${error.message}`);
  }
}
```

## Testing

Every change needs tests. Add them in `packages/*/test/`:

```javascript
import { describe, it, expect } from 'vitest';
import { parseTurtle } from '../src/parse.mjs';

describe('parseTurtle', () => {
  it('should parse valid Turtle data', async () => {
    const turtle = `@prefix ex: <http://example.org/> . ex:Alice ex:knows ex:Bob .`;
    const store = await parseTurtle(turtle);
    expect(store.size).toBe(1);
  });

  it('should throw on invalid Turtle', async () => {
    await expect(parseTurtle('invalid turtle')).rejects.toThrow();
  });
});
```

Run tests:
```bash
# All tests
pnpm test

# Specific package
pnpm --filter @unrdf/core test

# Watch mode
pnpm --filter @unrdf/core test:watch
```

## Pull Request Checklist

Before submitting your PR, verify:

- [ ] Tests pass (`pnpm test`)
- [ ] Linting passes (`pnpm run lint`)
- [ ] Build succeeds (`pnpm run build`)
- [ ] Added tests for new code
- [ ] Updated relevant documentation
- [ ] Followed JSDoc style for all functions
- [ ] No TypeScript added to source code
- [ ] Commit messages follow convention (see below)

## Commit Message Convention

We use [Conventional Commits](https://www.conventionalcommits.org/):

```bash
# Format
<type>(<scope>): <description>

# Examples
feat(core): add SPARQL 1.1 property paths
fix(hooks): resolve hook execution race condition
docs(readme): update installation instructions
test(streaming): add tests for large graph processing
refactor(federation): simplify query distribution
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `test`: Tests only
- `refactor`: Code change that neither fixes nor adds features
- `perf`: Performance improvement
- `chore`: Maintenance tasks

## PR Template

When creating a PR, include:

```markdown
## Description
Brief summary of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Documentation update
- [ ] Refactoring

## Testing
- [ ] All existing tests pass
- [ ] Added tests for new functionality
- [ ] Tested manually (describe steps)

## Checklist
- [ ] JSDoc comments added/updated
- [ ] No TypeScript in source
- [ ] Follows existing code patterns
- [ ] Documentation updated
```

## Getting Help

Stuck? Here's where to go:

1. **Check existing docs** - Start with [docs/START-HERE.md](docs/START-HERE.md)
2. **Search issues** - Someone may have asked before
3. **Ask in Discussions** - https://github.com/unrdf/unrdf/discussions
4. **Open an issue** - Label it with `question`

## Development Philosophy

UNRDF follows specific principles (from [CLAUDE.md](CLAUDE.md)):

1. **80/20 Big Bang** - Implement the 20% that delivers 80% of value, do it right the first time
2. **Measure, Don't Assume** - Run commands, read output, prove claims with evidence
3. **Pattern Reuse** - Copy working patterns exactly, don't improve unless necessary
4. **OTEL is Truth** - Agent claims require validation via observability
5. **Pure Functions** - No observability code in business logic

## Common Tasks

### Add a New Package

```bash
# Copy existing package structure
cp -r packages/core packages/my-new-package

# Update package.json
# Add to pnpm-workspace.yaml
# Implement features
# Add tests
# Update docs
```

### Fix a Failing Test

```bash
# Find the failing test
pnpm test 2>&1 | grep -A 5 "FAIL"

# Run just that test
pnpm --filter <package-name> test -- <test-file>

# Fix the issue
# Verify fix
pnpm --filter <package-name> test

# Commit
git commit -m "fix(package): description of fix"
```

### Update Documentation

```bash
# Edit the docs
vim docs/ARCHITECTURE.md

# Verify links work
# Check spelling
# Commit
git commit -m "docs: improve architecture documentation"
```

## Architecture Quick Reference

```
unrdf/
├── packages/          # 20+ packages (the actual code)
│   ├── core/          # Main RDF library (start here)
│   ├── hooks/         # Knowledge Hooks framework
│   ├── streaming/     # Large graph processing
│   └── ...
├── docs/              # Documentation (150+ files)
├── examples/          # Usage examples
└── test/              # Cross-package integration tests
```

**Key packages to know:**
- `@unrdf/core` - RDF operations, SPARQL, SHACL (80% of usage)
- `@unrdf/hooks` - Autonomous behaviors triggered by data changes
- `@unrdf/streaming` - Handle graphs with millions of triples
- `@unrdf/cli` - Command-line tools

## Code of Conduct

- Be respectful and inclusive
- Provide constructive feedback
- Welcome newcomers
- Focus on what's best for the project

## License

By contributing, you agree your contributions will be licensed under the MIT License.

## Recognition

Contributors are recognized in:
- GitHub contributor stats
- Release notes (CHANGELOG.md)
- Special mentions for significant contributions

## Next Steps

1. **Complete setup** → [docs/ONBOARDING.md](docs/ONBOARDING.md)
2. **Learn by doing** → [docs/WALKTHROUGHS.md](docs/WALKTHROUGHS.md)
3. **Find your first issue** → [Good First Issues](https://github.com/unrdf/unrdf/labels/good%20first%20issue)
4. **Join discussions** → [GitHub Discussions](https://github.com/unrdf/unrdf/discussions)

**Questions?** Open an issue or ask in Discussions. We're here to help!

Thank you for contributing to UNRDF!
