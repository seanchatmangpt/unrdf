# UNRDF Onboarding Guide

**Welcome to UNRDF!** This guide will get you productive in 5 minutes.

## Quick Start (5 Minutes)

### Prerequisites

1. **Node.js >= 18.0.0**

   ```bash
   node --version  # Should show v18.x or higher
   ```

2. **Git**
   ```bash
   git --version
   ```

### Setup (One Command)

```bash
# Clone repository (if not already done)
git clone https://github.com/unrdf/unrdf.git
cd unrdf

# Run setup script
./scripts/dev-start.sh
```

**Expected output:**

```
✨ UNRDF Development Environment Ready!

Quick Start Commands:
  pnpm dev           # Start watch mode
  pnpm test:watch    # Start test watch
  pnpm fix           # Auto-fix issues
  pnpm validate      # Fast validation
```

**Done!** You're ready to develop.

## Your First Task (10 Minutes)

### 1. Explore the Codebase

```bash
# Interactive package explorer
./scripts/dev-package.sh

# Select "1" for core package
# Select "6" to show file structure
```

**Key directories:**

- `packages/core` - Core RDF functionality
- `packages/hooks` - React hooks
- `packages/federation` - Multi-store federation
- `packages/streaming` - Streaming operations

### 2. Run Tests

```bash
# Fast tests (5s)
pnpm test:fast

# All tests (15s)
pnpm test
```

**Expected output:**

```
✅ All tests passed (330/330)
```

### 3. Make a Change

```bash
# Start watch mode
pnpm dev

# Edit a file (example)
# packages/core/src/index.mjs

# Tests auto-run on save
```

### 4. Validate Your Changes

```bash
# Auto-fix any issues
./scripts/quick-fix.sh

# Pre-commit validation
./scripts/pre-commit.sh

# Should complete in <10s
```

## Understanding the Project

### Architecture Overview

UNRDF is a monorepo with 20+ packages:

**Core packages:**

- `@unrdf/core` - RDF store and operations
- `@unrdf/yawl` - YARRRML parser
- `@unrdf/oxigraph` - Oxigraph bindings

**Integration packages:**

- `@unrdf/hooks` - React integration
- `@unrdf/federation` - Multi-store support
- `@unrdf/streaming` - Streaming operations

**Advanced packages:**

- `@unrdf/knowledge-engine` - AI-powered knowledge graphs
- `@unrdf/browser` - Browser support
- `@unrdf/cli` - Command-line tools

### Technology Stack

- **Language:** JavaScript (ESM modules, .mjs extension)
- **Type System:** JSDoc (not TypeScript)
- **Validation:** Zod schemas
- **Testing:** Vitest
- **Linting:** ESLint (400+ rules)
- **Formatting:** Prettier
- **Package Manager:** pnpm (required)

### Code Style

**Key principles:**

1. **Pure functions** - No side effects in core logic
2. **JSDoc types** - 100% type coverage
3. **Zod validation** - Runtime validation at boundaries
4. **MJS extension** - ESM modules only
5. **No TypeScript** - JSDoc + Zod instead

**Example:**

```javascript
/**
 * Add two quads to the store
 * @param {Store} store - RDF store
 * @param {Quad} quad - Quad to add
 * @returns {Store} Updated store
 */
export function addQuad(store, quad) {
  const schema = z.object({
    subject: z.any(),
    predicate: z.any(),
    object: z.any(),
    graph: z.any(),
  });

  const validated = schema.parse(quad);
  return store.add(validated);
}
```

### RDF Specifics

**CRITICAL:** UNRDF uses Oxigraph, not N3:

```javascript
// ✅ CORRECT
import { createStore } from '@unrdf/oxigraph';
const store = createStore();

// ❌ WRONG
import { Store } from 'n3';
const store = new Store();
```

**Verification:**

```bash
# Should return 0 results
grep -r "from 'n3'" packages/*/src/
```

## Daily Workflows

### Development Loop

```bash
# 1. Start watch mode
pnpm dev

# 2. Edit files
# ... make changes ...

# 3. Tests auto-run
# Watch terminal for results

# 4. Before commit
./scripts/pre-commit.sh
```

### Testing Workflow

```bash
# Fast tests (during development)
pnpm test:fast

# Watch mode (TDD)
pnpm test:watch

# Debug failing test
./scripts/debug-test.sh packages/core/tests/core.test.mjs
```

### Quality Workflow

```bash
# Auto-fix everything
./scripts/quick-fix.sh

# Verify
./scripts/pre-commit.sh

# Full validation (before push)
pnpm validate:full
```

## Common Tasks

### Adding a New Feature

1. **Start development mode:**

   ```bash
   pnpm dev
   ```

2. **Create feature file:**

   ```bash
   # Example: packages/core/src/new-feature.mjs
   ```

3. **Write tests:**

   ```bash
   # Example: packages/core/tests/new-feature.test.mjs
   pnpm test:watch
   ```

4. **Validate:**

   ```bash
   ./scripts/pre-commit.sh
   ```

5. **Commit:**
   ```bash
   git add .
   git commit -m "feat: description"
   ```

### Fixing a Bug

1. **Reproduce with test:**

   ```bash
   ./scripts/debug-test.sh <test-file>
   ```

2. **Fix in watch mode:**

   ```bash
   pnpm dev
   ```

3. **Verify fix:**

   ```bash
   pnpm test
   ```

4. **Commit:**
   ```bash
   git add .
   git commit -m "fix: description"
   ```

### Running Package-Specific Commands

```bash
# Interactive selector
./scripts/dev-package.sh

# Or direct
cd packages/core
pnpm dev
pnpm test
pnpm build
```

## Troubleshooting

### Build Errors

```bash
# Clean and rebuild
pnpm clean
pnpm install
pnpm build
```

### Test Failures

```bash
# Auto-fix common issues
./scripts/quick-fix.sh

# Debug specific test
./scripts/debug-test.sh <test-file>
```

### Lint/Format Errors

```bash
# Auto-fix
pnpm fix

# Or
./scripts/quick-fix.sh
```

### Type Errors

```bash
# Check types
pnpm typecheck

# Fix JSDoc comments manually
```

### "Cannot find module" Errors

```bash
# Reinstall dependencies
pnpm install

# Build core packages
pnpm -r --filter @unrdf/core --filter @unrdf/yawl build
```

## Best Practices

### Git Workflow

```bash
# 1. Create feature branch
git checkout -b feat/my-feature

# 2. Make changes
# ... edit files ...

# 3. Validate
./scripts/pre-commit.sh

# 4. Commit
git add .
git commit -m "feat: my feature"

# 5. Full validation before push
pnpm validate:full

# 6. Push
git push origin feat/my-feature
```

### Commit Messages

Follow [Conventional Commits](https://www.conventionalcommits.org/):

- `feat:` - New feature
- `fix:` - Bug fix
- `refactor:` - Code refactoring
- `docs:` - Documentation
- `test:` - Tests
- `chore:` - Maintenance

**Examples:**

```bash
git commit -m "feat: add federation support"
git commit -m "fix: resolve memory leak in streaming"
git commit -m "refactor: simplify quad parsing"
```

### Code Review Checklist

Before submitting a PR:

- [ ] All tests pass (`pnpm test`)
- [ ] No lint errors (`pnpm lint`)
- [ ] Types correct (`pnpm typecheck`)
- [ ] Build succeeds (`pnpm build`)
- [ ] Pre-commit passes (`./scripts/pre-commit.sh`)
- [ ] Full validation passes (`pnpm validate:full`)

### Performance Targets

All workflows should be fast:

- Fast tests: <5s
- Pre-commit: <10s
- Full validation: <2min
- Package rebuild: <2s

**If slower, investigate root cause.**

## Important Files

### Configuration

- `CLAUDE.md` - Project guidelines (READ THIS!)
- `package.json` - Root dependencies and scripts
- `.eslintrc.*` - Linting rules
- `.prettierrc` - Formatting rules
- `pnpm-workspace.yaml` - Workspace configuration

### Documentation

- `README.md` - Project overview
- `docs/developer-workflow.md` - This guide's companion
- `docs/onboarding-guide.md` - This file
- `packages/*/README.md` - Package-specific docs

### Scripts

- `scripts/dev-start.sh` - Setup script
- `scripts/pre-commit.sh` - Pre-commit validation
- `scripts/quick-fix.sh` - Auto-fix issues
- `scripts/dev-package.sh` - Package selector
- `scripts/debug-test.sh` - Test debugger

## Getting Help

### Resources

1. **Documentation:**
   - Read `CLAUDE.md` (project rules)
   - Read `developer-workflow.md` (workflows)
   - Check package README files

2. **Code Examples:**

   ```bash
   # Find examples in tests
   find packages -name "*.test.mjs" | head -5
   ```

3. **Architecture:**
   ```bash
   # Analyze codebase
   ./scripts/architecture-analyzer.mjs
   ```

### Common Questions

**Q: Why .mjs extension?**
A: Pure ESM modules, no CommonJS compatibility needed.

**Q: Why JSDoc instead of TypeScript?**
A: Simpler tooling, same type safety, better runtime validation with Zod.

**Q: Why pnpm?**
A: Faster, more efficient, proper monorepo support.

**Q: Why Oxigraph instead of N3?**
A: Better performance, standards compliance, SPARQL support.

**Q: How do I debug tests?**
A: Use `./scripts/debug-test.sh <test-file>` for interactive debugging.

**Q: Tests are slow, why?**
A: Use `pnpm test:fast` during development (5s), full tests before commit (15s).

**Q: How do I fix lint errors?**
A: Run `./scripts/quick-fix.sh` to auto-fix 90% of issues.

## Next Steps

Now that you're set up:

1. **Explore packages:**

   ```bash
   ./scripts/dev-package.sh
   ```

2. **Read architecture docs:**
   - Check `packages/core/README.md`
   - Review `CLAUDE.md` guidelines

3. **Make your first change:**
   - Pick a small task
   - Follow the development workflow
   - Submit a PR

4. **Join the community:**
   - Read existing issues
   - Review open PRs
   - Ask questions

## Success Metrics

You're fully onboarded when you can:

- [ ] Run `./scripts/dev-start.sh` successfully
- [ ] Make a code change in watch mode
- [ ] Write and run tests
- [ ] Fix lint/format issues automatically
- [ ] Pass pre-commit validation (<10s)
- [ ] Submit a PR with all checks passing

**Target onboarding time: 5 minutes setup + 10 minutes first task = 15 minutes total**

Welcome to the team! 🎉
