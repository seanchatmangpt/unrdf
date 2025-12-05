# UNRDF Developer Guide

> Comprehensive guide for UNRDF v3 development, testing, and contribution

## Table of Contents

1. [Getting Started](#getting-started)
2. [Development Setup](#development-setup)
3. [Project Structure](#project-structure)
4. [Development Workflow](#development-workflow)
5. [Testing Guide](#testing-guide)
6. [CLI Development](#cli-development)
7. [Knowledge Hooks Development](#knowledge-hooks-development)
8. [Policy Pack Development](#policy-pack-development)
9. [VS Code Extension](#vs-code-extension)
10. [Shell Completions](#shell-completions)
11. [Contributing](#contributing)

---

## Getting Started

### Prerequisites

- Node.js >= 18.0.0
- pnpm >= 7.0.0
- Git

### Quick Start

```bash
# Clone repository
git clone https://github.com/unrdf/unrdf.git
cd unrdf

# Install dependencies
pnpm install

# Run tests
pnpm test

# Start development
pnpm dev
```

---

## Development Setup

### Install Developer Tools

#### Shell Completions

```bash
# Install for your shell
cd completions
./install.sh

# Or manually:
# Bash
source completions/bash-completion.sh

# Zsh
cp completions/zsh-completion.zsh ~/.zsh/completions/_unrdf
# Add to ~/.zshrc:
# fpath=(~/.zsh/completions $fpath)
# autoload -Uz compinit && compinit

# Fish
cp completions/fish-completion.fish ~/.config/fish/completions/unrdf.fish
```

#### VS Code Extension

```bash
# Install from VSIX (when published)
code --install-extension unrdf-vscode-0.1.0.vsix

# Or symlink for development
ln -s /path/to/unrdf/vscode-extension ~/.vscode/extensions/unrdf-vscode
```

### Environment Configuration

Create `.env` file:

```env
# SPARQL Endpoint
SPARQL_ENDPOINT=http://localhost:3030/ds/sparql


# Observability
OTEL_EXPORTER_JAEGER_ENDPOINT=http://localhost:14268/api/traces
OTEL_EXPORTER_PROMETHEUS_PORT=9464

# Development
NODE_ENV=development
LOG_LEVEL=debug
```

---

## Project Structure

```
unrdf/
├── src/
│   ├── cli-v2/              # CLI v2 (noun-verb pattern)
│   │   ├── commands/        # Resource commands
│   │   │   ├── graph/       # Graph commands
│   │   │   ├── hook/        # Hook commands
│   │   │   ├── policy/      # Policy commands
│   │   │   ├── store/       # Store commands
│   │   │   ├── context/     # Context commands
│   │   │   └── repl.mjs     # Interactive REPL
│   │   ├── core/            # Core CLI infrastructure
│   │   │   ├── router.mjs
│   │   │   ├── context.mjs
│   │   │   ├── plugin-loader.mjs
│   │   │   └── completion.mjs
│   │   └── middleware/      # CLI middleware
│   ├── knowledge-engine/    # Knowledge Hooks engine
│   ├── policy-engine/       # Policy Pack engine
│   ├── knowledge-engine/         # Kubernetes gRPC knowledge-engine
│   └── observability/       # OTEL integration
├── test/                    # Vitest tests
│   ├── knowledge-engine/
│   ├── policy-engine/
│   ├── e2e/
│   └── dark-matter-80-20.test.mjs
├── completions/             # Shell completions
│   ├── bash-completion.sh
│   ├── zsh-completion.zsh
│   ├── fish-completion.fish
│   └── install.sh
├── vscode-extension/        # VS Code extension
│   ├── syntaxes/
│   ├── snippets/
│   └── extension.js
├── docs/                    # Documentation
├── examples/                # Example code
└── terraform/               # Infrastructure as code
```

---

## Development Workflow

### 1. Create Feature Branch

```bash
git checkout -b feature/your-feature-name
```

### 2. Write Tests First (TDD)

```javascript
// test/knowledge-engine/your-feature.test.mjs
import { describe, it, expect } from 'vitest';
import { YourFeature } from '../../src/knowledge-engine/your-feature.mjs';

describe('YourFeature', () => {
  it('should do something', () => {
    const feature = new YourFeature();
    expect(feature.doSomething()).toBe(expected);
  });
});
```

### 3. Implement Feature

```javascript
// src/knowledge-engine/your-feature.mjs
/**
 * @file Your Feature
 * @module knowledge-engine/your-feature
 */

export class YourFeature {
  doSomething() {
    // Implementation
  }
}
```

### 4. Run Tests

```bash
# Run all tests
pnpm test

# Run specific test file
pnpm vitest test/knowledge-engine/your-feature.test.mjs

# Watch mode
pnpm test:watch

# Coverage
pnpm test --coverage
```

### 5. Lint and Format

```bash
# Lint
pnpm lint

# Fix linting issues
pnpm lint:fix

# Format code
pnpm format

# Check formatting
pnpm format:check
```

### 6. Commit Changes

```bash
# Stage changes
git add .

# Commit with conventional commit message
git commit -m "feat: add your feature description"

# Commit types:
# feat: New feature
# fix: Bug fix
# docs: Documentation changes
# test: Test changes
# refactor: Code refactoring
# perf: Performance improvements
# chore: Build/tooling changes
```

---

## Testing Guide

### Test Structure

UNRDF uses Vitest with comprehensive test coverage:

```javascript
import { describe, it, expect, beforeEach, afterEach } from 'vitest';

describe('Feature Suite', () => {
  beforeEach(() => {
    // Setup before each test
  });

  afterEach(() => {
    // Cleanup after each test
  });

  describe('Subfeature', () => {
    it('should behave correctly', async () => {
      // Arrange
      const input = setupInput();

      // Act
      const result = await executeFeature(input);

      // Assert
      expect(result).toMatchObject({ ... });
    });
  });
});
```

### Test Categories

#### Unit Tests

```bash
# Run unit tests
pnpm vitest test/knowledge-engine/
pnpm vitest test/policy-engine/
```

#### Integration Tests

```bash
# Run integration tests
pnpm vitest test/e2e/
```

#### E2E Tests with Testcontainers

```bash
# Run E2E with Docker
pnpm test:e2e

# Run K8s tests
pnpm test:k8s
```

#### Dark Matter 80/20 Tests

```bash
# Run critical path tests
pnpm test:dark-matter
```

### Validation Protocol

**CRITICAL**: Always validate agent/AI-generated code:

```bash
# Step 1: Run tests
pnpm test

# Step 2: Check for failures
grep "FAIL\|Error" test-output.log

# Step 3: Validate OTEL metrics
grep "Error recorded" test-output.log

# Step 4: Only accept if ALL tests pass
```

---

## CLI Development

### Adding New Commands

#### 1. Create Command File

```javascript
// src/cli-v2/commands/resource/action.mjs
import { defineCommand } from 'citty';

export const actionCommand = defineCommand({
  meta: {
    name: 'action',
    description: 'Perform action on resource'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Resource name',
      required: true
    },
    format: {
      type: 'string',
      description: 'Output format',
      default: 'json'
    }
  },
  async run(ctx) {
    const { name, format } = ctx.args;
    // Implementation
  }
});
```

#### 2. Register Command

```javascript
// src/cli-v2/commands/resource/index.mjs
export { actionCommand } from './action.mjs';

// src/cli-v2/index.mjs - add to subCommands
subCommands: {
  resource: defineCommand({
    meta: { name: 'resource', description: '...' },
    subCommands: {
      action: actionCommand
    }
  })
}
```

#### 3. Update Shell Completions

```bash
# Add to completions/bash-completion.sh
local resource_verbs="list get create update delete action"

# Add to completions/zsh-completion.zsh
resource)
  _arguments '2:verb:(list get create update delete action)'
  ;;

# Add to completions/fish-completion.fish
complete -c unrdf -n "__fish_seen_subcommand_from resource" -a "action" -d "Perform action"
```

### Testing CLI Commands

```bash
# Manual testing
unrdf resource action test-name --format json

# Automated testing
pnpm vitest test/cli-v2/commands/resource/action.test.mjs
```

---

## Knowledge Hooks Development

### Creating Hooks

```javascript
import { defineHook } from 'unrdf';

export const myHook = defineHook({
  name: 'my-hook',
  description: 'Hook description',
  version: '1.0.0',

  triggers: {
    events: ['onCreate', 'onUpdate'],
    patterns: {
      subject: '?s',
      predicate: '<http://example.org/property>',
      object: '?o'
    }
  },

  async execute(context) {
    const { quad, store, metadata, auditTrail } = context;

    // Hook logic
    console.log('Hook executed:', quad);

    // Optional: Modify graph
    // await store.add(newQuad);

    // Optional: Record audit trail
    await auditTrail.record({
      timestamp: new Date().toISOString(),
      event: 'hook-executed',
      quad: quad
    });

    return {
      success: true,
      modified: false
    };
  }
});
```

### Testing Hooks

```javascript
import { describe, it, expect } from 'vitest';
import { myHook } from '../src/knowledge-engine/hooks/my-hook.mjs';

describe('My Hook', () => {
  it('should execute on matching pattern', async () => {
    const context = {
      quad: createTestQuad(),
      store: createMockStore(),
      metadata: {},
      auditTrail: createMockAuditTrail()
    };

    const result = await myHook.execute(context);

    expect(result.success).toBe(true);
  });
});
```

---

## Policy Pack Development

### Creating Policy Packs

```javascript
import { definePolicyPack } from 'unrdf';

export const myPolicyPack = definePolicyPack({
  name: 'my-policy-pack',
  description: 'Policy pack description',
  version: '1.0.0',

  rules: [
    {
      name: 'validation-rule',
      description: 'Validate data',
      severity: 'critical',
      enforcementLevel: 'mandatory',

      async validate(context) {
        const { quad, store } = context;

        // Validation logic
        const isValid = await performValidation(quad);

        return {
          valid: isValid,
          message: isValid ? 'Valid' : 'Invalid',
          remediate: !isValid
        };
      },

      async remediate(context) {
        // Remediation logic
        console.log('Remediating...');
      }
    }
  ]
});
```

### Testing Policy Packs

```javascript
import { describe, it, expect } from 'vitest';
import { myPolicyPack } from '../src/policy-engine/packs/my-policy.mjs';

describe('My Policy Pack', () => {
  it('should validate correctly', async () => {
    const context = {
      quad: createTestQuad(),
      store: createMockStore()
    };

    const result = await myPolicyPack.rules[0].validate(context);

    expect(result.valid).toBe(true);
  });
});
```

---

## VS Code Extension

### Using the Extension

1. **Syntax Highlighting**: Automatic for `.hook` and `.policy` files
2. **Code Snippets**: Type prefix and press Tab
   - `defhook` - Define hook
   - `sparqlhook` - SPARQL query hook
   - `defpolicy` - Define policy pack
3. **Commands**: Access via Command Palette (Cmd/Ctrl+Shift+P)
   - `UNRDF: Validate Hook`
   - `UNRDF: Evaluate Hook`
   - `UNRDF: Apply Policy Pack`

### Development

```bash
cd vscode-extension

# Install dependencies
npm install

# Package extension
npm run package

# Install locally
code --install-extension unrdf-vscode-0.1.0.vsix
```

---

## Shell Completions

### Testing Completions

```bash
# Bash
source completions/bash-completion.sh
unrdf <TAB>

# Zsh
autoload -Uz compinit && compinit
unrdf <TAB>

# Fish
fish_update_completions
unrdf <TAB>
```

### Adding New Completions

Edit the appropriate completion file and regenerate:

```bash
# Bash: completions/bash-completion.sh
# Zsh: completions/zsh-completion.zsh
# Fish: completions/fish-completion.fish

# Test changes
./completions/install.sh
```

---

## Contributing

### Pull Request Process

1. **Fork & Clone**
   ```bash
   git clone https://github.com/YOUR-USERNAME/unrdf.git
   cd unrdf
   ```

2. **Create Branch**
   ```bash
   git checkout -b feature/your-feature
   ```

3. **Develop & Test**
   ```bash
   # Write tests
   # Implement feature
   pnpm test
   pnpm lint
   ```

4. **Commit**
   ```bash
   git commit -m "feat: add your feature"
   ```

5. **Push & PR**
   ```bash
   git push origin feature/your-feature
   # Create PR on GitHub
   ```

### Code Review Checklist

- [ ] Tests pass (`pnpm test`)
- [ ] Linting passes (`pnpm lint`)
- [ ] Code formatted (`pnpm format`)
- [ ] Documentation updated
- [ ] Commit messages follow convention
- [ ] No breaking changes (or documented)
- [ ] Performance validated
- [ ] Security reviewed

### Release Process

```bash
# Patch release (bug fixes)
pnpm release:patch

# Minor release (new features)
pnpm release:minor

# Major release (breaking changes)
pnpm release:major
```

---

## Resources

- **Documentation**: https://github.com/unrdf/unrdf
- **Issues**: https://github.com/unrdf/unrdf/issues
- **Discussions**: https://github.com/unrdf/unrdf/discussions
- **Discord**: [Join our community](https://discord.gg/unrdf)

---

## FAQ

### Q: How do I run the REPL?

```bash
unrdf repl
# Or via CLI v2
npx unrdf repl
```

### Q: How do I debug hooks?

```bash
# Enable debug logging
NODE_ENV=development LOG_LEVEL=debug unrdf hook eval my-hook.mjs
```

### Q: How do I contribute a new hook?

1. Create hook in `src/knowledge-engine/hooks/`
2. Write tests in `test/knowledge-engine/`
3. Update documentation
4. Submit PR

### Q: Where are logs stored?

```bash
# Application logs
tail -f ~/.unrdf/logs/unrdf.log

# OTEL traces
# View in Jaeger UI: http://localhost:16686
```

---

**Happy developing!** 🚀

For questions, open an issue or join our Discord community.
