# UNRDF Monorepo Setup & Development Guide

## Quick Start

### Prerequisites
- Node.js 18+
- pnpm 8.0+

### Installation

```bash
# Clone repository
git clone https://github.com/unrdf/unrdf.git
cd unrdf

# Install dependencies (monorepo-aware)
pnpm install

# Run tests across all packages
pnpm test

# Build all packages
pnpm build
```

## Monorepo Organization

```
unrdf/
├── packages/
│   ├── core/              # @unrdf/core - RDF substrate (ESSENTIAL)
│   ├── hooks/             # @unrdf/hooks - Policy engine (ESSENTIAL)
│   ├── federation/        # @unrdf/federation - Peer queries (ESSENTIAL)
│   ├── streaming/         # @unrdf/streaming - Change feeds (ESSENTIAL)
│   ├── browser/           # @unrdf/browser - Browser SDK (ESSENTIAL)
│   ├── cli/               # @unrdf/cli - CLI tools (ESSENTIAL)
│   ├── knowledge-engine/  # @unrdf/knowledge-engine - Rules (OPTIONAL)
│   ├── dark-matter/       # @unrdf/dark-matter - Optimization (OPTIONAL)
│   ├── composables/       # @unrdf/composables - Vue 3 (OPTIONAL)
│   └── project-engine/    # @unrdf/project-engine - Dev only (DEV)
├── docs/                  # Documentation
├── examples/              # Examples and demos
├── test/                  # Shared test utilities
└── pnpm-workspace.yaml    # Workspace configuration
```

## Package Dependencies

```
@unrdf/core (no UNRDF deps)
  ↑
  ├── @unrdf/hooks
  ├── @unrdf/federation
  ├── @unrdf/streaming
  ├── @unrdf/browser
  ├── @unrdf/cli
  ├── @unrdf/knowledge-engine
  ├── @unrdf/dark-matter
  └── @unrdf/composables
```

## Development Workflow

### Working on a Specific Package

```bash
# Install dependencies (workspace-aware)
pnpm install

# Test specific package
pnpm -C packages/core test

# Or from package directory
cd packages/core
pnpm test

# Watch mode
pnpm test:watch
```

### Running Tests

```bash
# Test all packages
pnpm test

# Test specific package
pnpm test:core
pnpm test:hooks
pnpm test:federation
pnpm test:streaming
pnpm test:browser
pnpm test:cli
pnpm test:knowledge-engine

# Fast tests (subset)
pnpm test:fast

# Watch mode (all)
pnpm test:watch

# Coverage report
pnpm test:core -- --coverage
```

### Linting & Formatting

```bash
# Lint all packages
pnpm lint

# Fix lint errors
pnpm lint:fix

# Format code
pnpm format

# Check formatting
pnpm format:check
```

### Building

```bash
# Build all packages
pnpm build

# Build specific package
cd packages/core
pnpm build

# Output goes to dist/ in each package
```

## Adding a New Package

```bash
# Create package directory
mkdir packages/my-feature
cd packages/my-feature

# Initialize with template
npm init -y

# Create structure
mkdir src test

# Add @unrdf/core dependency (usually needed)
pnpm add @unrdf/core

# Create src/index.mjs
echo "export const feature = () => {}" > src/index.mjs

# Add to workspace (pnpm auto-detects)
# Test imports
pnpm -C packages/my-feature test
```

## Importing Between Packages

### From Workspace
```javascript
// packages/my-feature/src/index.mjs
import { createStore } from '@unrdf/core'
import { defineHook } from '@unrdf/hooks'

// pnpm workspace auto-links these
// No special setup needed
```

### From package.json
```json
{
  "dependencies": {
    "@unrdf/core": "workspace:*",
    "@unrdf/hooks": "workspace:*"
  }
}
```

The `workspace:*` protocol tells pnpm to use the local workspace version.

## Publishing Packages

### Single Package
```bash
cd packages/core
npm publish
```

### All Packages (Recommended Order)
```bash
# Substrate packages first (in parallel)
cd packages/core && npm publish &
cd packages/hooks && npm publish &
cd packages/federation && npm publish &
cd packages/streaming && npm publish &
cd packages/browser && npm publish &

# Then CLI
cd packages/cli && npm publish

# Then optional extensions
cd packages/knowledge-engine && npm publish &
cd packages/dark-matter && npm publish &
cd packages/composables && npm publish &
```

### Version Bumping

```bash
# Monorepo approach: each package independent
cd packages/core
npm version patch  # 5.0.0 → 5.0.1

cd packages/dark-matter
npm version minor  # 1.0.0 → 1.1.0
```

## IDE Setup

### VSCode
Create `.vscode/settings.json`:
```json
{
  "typescript.tsdk": "node_modules/typescript/lib",
  "typescript.enablePromptUseWorkspaceTsdk": true
}
```

### WebStorm
- File → Settings → Languages & Frameworks → TypeScript → TSLint
- Enable "Use config file"
- Select workspace root

## Troubleshooting

### Package Not Found
```bash
# Ensure workspace is installed
pnpm install

# Check workspace structure
pnpm list

# Verify package.json has dependency
ls packages/*/package.json
```

### Version Mismatch
```bash
# Check all versions
pnpm list @unrdf/core

# Sync versions
pnpm add @unrdf/core@workspace:* --recursive
```

### Build Errors
```bash
# Clean and rebuild
pnpm clean
pnpm install
pnpm build

# Check specific package
cd packages/core
pnpm build --verbose
```

### Import Resolution
```bash
# Clear node_modules
rm -rf node_modules pnpm-lock.yaml
pnpm install

# Verify workspace linking
pnpm list @unrdf/core
```

## CI/CD Integration

### GitHub Actions
```yaml
name: Test & Lint

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: pnpm/action-setup@v2
      - uses: actions/setup-node@v3
        with:
          node-version: 18
          cache: 'pnpm'
      - run: pnpm install
      - run: pnpm lint
      - run: pnpm test
      - run: pnpm build
```

### Running in CI
```bash
# Install
pnpm install

# Lint all
pnpm lint

# Test all
pnpm test

# Build all
pnpm build
```

## Performance Tips

### Selective Testing
```bash
# Test only changed packages
pnpm test core hooks
```

### Parallel Execution
```bash
# Run all tests in parallel
pnpm -r --parallel test
```

### Build Caching
```bash
# Reuse dist/ between builds
pnpm build
# dist/ preserved for next build
```

## Scripts Reference

### Root Level
```bash
pnpm test              # Test all packages
pnpm test:fast         # Fast test subset
pnpm test:watch        # Watch mode
pnpm test:core         # Test @unrdf/core only
pnpm lint              # Lint all
pnpm lint:fix          # Fix lint errors
pnpm format            # Format code
pnpm format:check      # Check formatting
pnpm build             # Build all packages
pnpm clean             # Clean all dist/
pnpm dev               # Dev mode (all parallel)
```

### Package Level
```bash
cd packages/core
pnpm test              # Test @unrdf/core
pnpm build             # Build @unrdf/core
pnpm lint              # Lint @unrdf/core
```

## Next Steps

1. **Read** [MONOREPO-STRUCTURE.md](./MONOREPO-STRUCTURE.md) for architecture overview
2. **Review** [MONOREPO-MIGRATION.md](./MONOREPO-MIGRATION.md) if upgrading from v4.x
3. **Check** [v5-voc-to-implementation.md](./v5-voc-to-implementation.md) for design rationale
4. **Start** with `@unrdf/core` for basic RDF operations
5. **Add** other packages as needed for your use case

## Support

- **Issues**: GitHub Issues
- **Discussions**: GitHub Discussions
- **Documentation**: [GitHub Wiki](https://github.com/unrdf/unrdf/wiki)

## Contributing

See [CONTRIBUTING.md](../CONTRIBUTING.md) for guidelines on:
- Creating new packages
- Code standards
- Testing requirements
- Commit conventions
