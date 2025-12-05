# UNRDF Local Development Guide

This guide covers setting up your development environment, running tests, and building the UNRDF monorepo locally.

## Prerequisites

- **Node.js:** 18.19.0 or higher ([download](https://nodejs.org))
- **pnpm:** 8.15.0 or higher (required, not npm or yarn)
- **Git:** Any recent version

## Install pnpm (First Time Only)

If you don't have pnpm installed:

```bash
# Using npm (comes with Node.js)
npm install -g pnpm

# Or using Homebrew (macOS)
brew install pnpm

# Verify installation
pnpm --version  # Should be 8.15.0 or higher
```

**Why pnpm?** UNRDF uses pnpm's monorepo workspace feature. It's faster, more disk-efficient, and strict about dependencies.

---

## Initial Setup

### 1. Clone the Repository

```bash
git clone https://github.com/unrdf/unrdf.git
cd unrdf
```

### 2. Create and Switch to Your Branch

All development happens on feature branches:

```bash
# For this session, use the provided branch
git checkout claude/rewrite-docs-monorepo-011wca5kv4dmUw4KRSdFnSx4

# Or create a new feature branch
git checkout -b feat/your-feature-name
```

### 3. Install Dependencies

This installs dependencies for **all 17 packages** in the monorepo:

```bash
pnpm install
```

**What happens:** pnpm reads `pnpm-workspace.yaml` and installs dependencies for each package, creating a shared node_modules at the root.

**First time takes:** ~2-3 minutes depending on internet speed.

### 4. Verify Installation

```bash
# Should show all packages are linked
pnpm list --depth 0

# Should show 18+ packages
pnpm list | grep @unrdf
```

---

## Common Development Tasks

### Building the Monorepo

```bash
# Build all packages
pnpm run build

# Build a specific package (e.g., core)
pnpm --filter @unrdf/core run build

# Build with watch mode (rebuilds on file changes)
pnpm run build:watch
```

### Running Tests

```bash
# Run all tests across all packages
pnpm test

# Run tests for a specific package
pnpm --filter @unrdf/core test

# Run tests in watch mode
pnpm run test:watch

# Run tests with coverage
pnpm test -- --coverage

# Run tests matching a pattern
pnpm test -- --testNamePattern="SPARQL"
```

**Test framework:** Vitest (installed in each package)

### Linting

```bash
# Lint all packages
pnpm run lint

# Lint a specific package
pnpm --filter @unrdf/core run lint

# Fix linting issues (auto-correct)
pnpm run lint:fix
```

**Linter:** Ruff (400+ rules enforced)

### Type Checking

```bash
# Check types across all packages (if TypeScript enabled)
pnpm run type-check

# Check a specific package
pnpm --filter @unrdf/core run type-check
```

**Note:** UNRDF source code uses JSDoc, not TypeScript. Type definitions are generated from JSDoc.

---

## Working with Individual Packages

### Running Commands in One Package

Use `--filter` to target a specific package:

```bash
# Run any npm script in a package
pnpm --filter @unrdf/core run <script>

# Examples
pnpm --filter @unrdf/hooks run test
pnpm --filter @unrdf/streaming run build
pnpm --filter @unrdf/browser run lint
```

### Installing Dependencies in a Package

Add a dependency to a specific package:

```bash
# Add a dependency to @unrdf/core
pnpm --filter @unrdf/core add lodash

# Add a dev dependency
pnpm --filter @unrdf/core add -D @types/node

# Add a dependency that's shared across packages
pnpm add -w typescript  # -w = workspace (all packages)
```

### Working on a Package Locally

Each package has its own structure:

```
packages/core/
├── src/                  # Source files (JavaScript with JSDoc)
├── test/                 # Test files (Vitest)
├── dist/                 # Built output
├── package.json          # Package configuration
└── README.md             # Package documentation
```

**Typical workflow:**
1. Make changes in `src/`
2. Run `pnpm --filter @unrdf/core test` to verify
3. Run `pnpm --filter @unrdf/core run build` to build
4. Test integration with other packages

---

## Monorepo Scripts

These scripts run across all packages:

| Script | Purpose |
|--------|---------|
| `pnpm test` | Run all tests |
| `pnpm run build` | Build all packages |
| `pnpm run lint` | Lint all packages |
| `pnpm run lint:fix` | Fix lint issues |
| `pnpm run type-check` | Type check all packages |
| `pnpm run clean` | Clean all build artifacts |

---

## Debugging

### Node.js Inspector

Debug a test or script with Node's built-in debugger:

```bash
# Debug a test file
node --inspect-brk node_modules/.bin/vitest run packages/core/test/query.test.mjs

# Then open chrome://inspect in Chrome and click the test target
```

### Logging

Add debug output to your code:

```javascript
console.log('Debug:', value);  // Use console.log in development

// For structured logging
console.error('Error:', error);  // Errors to stderr
```

### Interactive REPL

Test UNRDF code interactively:

```bash
# Start Node REPL
node

# Inside the REPL
import('@unrdf/core').then(({ createKnowledgeSubstrateCore }) => {
  globalThis.core = createKnowledgeSubstrateCore();
});

await globalThis.core;
// Then use core in the REPL
```

---

## Project Structure for Contributors

Understanding the layout helps navigate quickly:

```
unrdf/
├── packages/                    # 17 monorepo packages
│   ├── core/                    # Essential - START HERE
│   ├── oxigraph/                # Persistent storage
│   ├── hooks/                   # Autonomous behaviors
│   ├── streaming/               # Large graphs
│   ├── federation/              # Distributed queries
│   ├── knowledge-engine/        # Inference
│   ├── browser/                 # Client-side RDF
│   ├── react/                   # React integration
│   ├── cli/                     # CLI tools
│   └── [7 more packages]
│
├── docs/                        # 100+ documentation files
│   ├── START-HERE.md            # Quick orientation
│   ├── MONOREPO-QUICK-REFERENCE.md  # Package matrix
│   ├── LOCAL-DEVELOPMENT.md     # This file
│   ├── WORKSPACE-STRUCTURE.md   # File layout
│   ├── ARCHITECTURE.md          # System design
│   ├── PACKAGES.md              # Detailed package docs
│   └── [many more...]
│
├── examples/                    # Example projects
│   ├── basic/                   # Simple SPARQL example
│   ├── react-app/               # React integration example
│   └── [more examples]
│
├── src/                         # Legacy source (gradually migrated)
├── test/                        # Test utilities
├── scripts/                     # Build & automation
├── root package.json            # Workspace configuration
├── pnpm-workspace.yaml          # pnpm workspace config
└── CLAUDE.md                    # Development guidelines
```

---

## Development Workflow

### Typical Day

1. **Start work:**
   ```bash
   git checkout -b feat/my-feature
   ```

2. **Make changes** in `packages/*/src/`

3. **Test locally:**
   ```bash
   pnpm --filter @unrdf/core test
   ```

4. **Lint and build:**
   ```bash
   pnpm run lint:fix
   pnpm run build
   ```

5. **Run full test suite:**
   ```bash
   pnpm test
   ```

6. **Commit:**
   ```bash
   git add .
   git commit -m "feat: add new feature"
   git push -u origin feat/my-feature
   ```

### Testing Across Packages

If your change affects multiple packages:

```bash
# Test the changed package
pnpm --filter @unrdf/core test

# Test packages that depend on it
pnpm --filter @unrdf/hooks test  # hooks depends on core
pnpm --filter @unrdf/streaming test

# Run full suite to be safe
pnpm test
```

---

## Troubleshooting

### Issue: "pnpm: command not found"

**Solution:** Install pnpm globally:
```bash
npm install -g pnpm
```

### Issue: Node version too old

**Solution:** Use Node 18.19.0 or higher:
```bash
node --version  # Check version
nvm install 18  # If using nvm
```

### Issue: "Cannot find module @unrdf/core" after installing

**Solution:** Rebuild the monorepo:
```bash
pnpm install
pnpm run build
```

### Issue: Tests are failing

**Solution:** Verify your environment:
```bash
# Clean and reinstall
rm -rf node_modules pnpm-lock.yaml
pnpm install

# Run tests
pnpm test
```

### Issue: Disk space issues

**Solution:** pnpm-store can grow large:
```bash
# Clean pnpm cache
pnpm store prune

# Remove build artifacts
pnpm run clean
```

---

## Performance Tips

### Speed Up Builds

```bash
# Build only changed packages
pnpm run build  # Only rebuilds what changed

# Skip specific packages
pnpm --filter '!@unrdf/browser' run build
```

### Speed Up Tests

```bash
# Run tests in parallel (default)
pnpm test  # Uses all available cores

# Run specific test file
pnpm --filter @unrdf/core run test -- query.test.mjs

# Run tests matching pattern
pnpm test -- --testNamePattern="SPARQL"
```

### Monorepo Insights

```bash
# See dependency graph
pnpm ls

# See what would be built
pnpm run build --dry-run

# List all packages
pnpm list --depth 0
```

---

## IDE Setup

### VS Code

**Recommended extensions:**
- ESLint
- Prettier Code Formatter
- Ruff (linter)
- Vitest (test runner)

**Settings (.vscode/settings.json):**
```json
{
  "editor.defaultFormatter": "esbenp.prettier-vscode",
  "editor.formatOnSave": true,
  "[javascript]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
  },
  "eslint.validate": ["javascript"],
  "ruff.run": "onType"
}
```

### WebStorm / IntelliJ

- No special configuration needed
- Built-in ESLint and test support
- Just open the root directory

---

## Git Workflow

### Feature Branch Workflow

```bash
# 1. Create feature branch
git checkout -b feat/add-sparql-optimization

# 2. Make commits
git add src/
git commit -m "feat: add query optimization"

# 3. Push to remote
git push -u origin feat/add-sparql-optimization

# 4. Create Pull Request on GitHub

# 5. After PR merged, clean up
git checkout main
git pull origin main
git branch -d feat/add-sparql-optimization
```

### Before Pushing

Always verify:
```bash
# Tests pass
pnpm test

# Linting passes
pnpm run lint

# Builds without errors
pnpm run build

# No uncommitted changes
git status
```

---

## Next Steps

- **Read:** [WORKSPACE-STRUCTURE.md](WORKSPACE-STRUCTURE.md) - File layout within packages
- **Build something:** [PACKAGE-DEVELOPMENT.md](PACKAGE-DEVELOPMENT.md) - Create a new package
- **Run tests:** See the [TESTING-STRATEGY.md](TESTING-STRATEGY.md) guide for comprehensive testing

---

## Quick Command Reference

```bash
# Setup
pnpm install              # Install all dependencies
pnpm run build            # Build all packages

# Development
pnpm test                 # Run all tests
pnpm run lint             # Lint all code
pnpm run lint:fix         # Fix linting issues

# Specific package
pnpm --filter @unrdf/core run test    # Test one package
pnpm --filter @unrdf/core run build   # Build one package

# Debugging
node --inspect-brk node_modules/.bin/vitest  # Debug tests
pnpm run type-check       # Check types

# Cleanup
pnpm run clean            # Remove build artifacts
pnpm store prune          # Clean pnpm cache
```

---

**Ready?** → [WORKSPACE-STRUCTURE.md](WORKSPACE-STRUCTURE.md) or start contributing!
