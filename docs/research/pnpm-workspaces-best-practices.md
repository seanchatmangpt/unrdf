# PNPM Workspaces Best Practices Research
## Unified Monorepo Tooling for UNRDF v5.0.1

**Research Date:** 2025-12-20
**Researcher:** Research Agent
**Project:** UNRDF v5.0.1 Monorepo (29 workspace packages)

---

## Executive Summary

This research document analyzes pnpm workspace best practices for the UNRDF monorepo, covering dependency management, version resolution, circular dependency detection, and shared script patterns. The UNRDF project currently uses **29 workspace packages** with **workspace:*** protocol for internal linking.

**Key Findings:**
- ✅ UNRDF correctly uses `workspace:*` protocol for 32+ internal dependencies
- ✅ Root-level `pnpm.overrides` enforces consistent versions (@opentelemetry/api, zod)
- ⚠️ No circular dependency detection tooling currently configured
- ⚠️ No dedicated `.npmrc` at root level (only in packages/docs)
- ⚠️ Mixed Zod versions detected (v3.24.1 in streaming, v4.1.13 in root)

---

## 1. Workspace-Level vs Package-Level Dependencies

### 1.1 Current UNRDF Structure

```yaml
# pnpm-workspace.yaml
packages:
  - 'packages/*'
  - 'packages/kgc-4d/playground'
  - 'packages/atomvm/playground'
  - 'packages/cli/examples/*'
  - 'packages/hooks/examples/*'
  - 'packages/federation/examples/*'
  - 'packages/streaming/examples/*'
  - 'playground/full-stack-example/apps/*'
  - 'apps/*'
```

**Total Packages:** 29 workspace packages identified

### 1.2 Best Practice: Dependency Placement

| Dependency Type | Placement | Example (UNRDF) |
|----------------|-----------|-----------------|
| **Build Tools** | Root `devDependencies` | `vitest`, `eslint`, `prettier`, `unbuild` |
| **Shared Dev Tools** | Root `devDependencies` | `@types/node`, `typescript`, `@vitest/coverage-v8` |
| **Monorepo Tooling** | Root `devDependencies` | `citty` (CLI framework), `glob` |
| **Common Runtime Deps** | Root `dependencies` + overrides | `zod` (v4.1.13) |
| **Package-Specific Deps** | Package `dependencies` | `n3`, `jsonld`, `rdf-ext`, `prom-client` |
| **Workspace Protocol** | Package `dependencies` | `"@unrdf/core": "workspace:*"` |
| **Peer Dependencies** | Package `peerDependencies` | `vue` in `@unrdf/composables` |

### 1.3 UNRDF Root Dependencies (Current State)

**Root `devDependencies` (13 packages):**
```json
{
  "@opentelemetry/api": "^1.9.0",
  "@opentelemetry/exporter-trace-otlp-http": "^0.208.0",
  "@opentelemetry/instrumentation": "^0.208.0",
  "@opentelemetry/resources": "^2.2.0",
  "@opentelemetry/sdk-node": "^0.208.0",
  "@opentelemetry/sdk-trace-base": "^2.2.0",
  "@opentelemetry/sdk-trace-node": "^2.2.0",
  "@opentelemetry/semantic-conventions": "^1.38.0",
  "@types/node": "^24.10.1",
  "@vitest/browser": "^4.0.15",
  "@vitest/coverage-v8": "^4.0.15",
  "@vitest/ui": "^4.0.15",
  "citty": "^0.1.6",
  "eslint": "^9.39.1",
  "eslint-config-prettier": "^10.1.8",
  "eslint-plugin-jsdoc": "^61.4.1",
  "glob": "^13.0.0",
  "globals": "^15.12.0",
  "jsdom": "^27.2.0",
  "playwright": "^1.57.0",
  "prettier": "^3.7.4",
  "typescript": "^5.9.3",
  "unbuild": "^3.6.1",
  "vitest": "^4.0.15"
}
```

**Root `dependencies` (1 package):**
```json
{
  "zod": "^4.1.13"
}
```

**Rationale:**
- ✅ OpenTelemetry packages hoisted to root (shared across federation, streaming, hooks)
- ✅ Build/test tools centralized (vitest, eslint, prettier, unbuild)
- ✅ Zod pinned in root for version consistency
- ⚠️ Consider moving `citty` to root since it's used in hooks, cli, streaming packages

---

## 2. Dependency Version Management & Resolution Order

### 2.1 PNPM Resolution Hierarchy

pnpm resolves dependencies in this order:

1. **Workspace packages** (`workspace:*` protocol)
2. **pnpm.overrides** (forced version resolution)
3. **Package-level dependencies** (direct declarations)
4. **Workspace root dependencies** (hoisted)
5. **Registry packages** (npm/pnpm registry)

### 2.2 UNRDF Override Strategy

**Current Overrides:**
```json
{
  "pnpm": {
    "overrides": {
      "@opentelemetry/api": "^1.7.0",
      "zod": "^4.1.13"
    }
  }
}
```

**Analysis:**
- ✅ `@opentelemetry/api` override ensures consistency across OTEL packages
- ⚠️ `zod` override to v4.1.13, but `@unrdf/streaming` declares v3.24.1 (potential conflict)

**Recommendation:**
```json
{
  "pnpm": {
    "overrides": {
      "@opentelemetry/api": "^1.9.0",  // Match root version
      "zod": "^4.1.13",                 // Enforce v4 across all packages
      "n3": "^1.26.0"                   // Consider adding N3 for RDF consistency
    }
  }
}
```

### 2.3 Workspace Protocol Best Practices

**UNRDF Usage (Correct Pattern):**
```json
{
  "dependencies": {
    "@unrdf/core": "workspace:*",
    "@unrdf/oxigraph": "workspace:*",
    "@unrdf/streaming": "workspace:*"
  }
}
```

**Benefits:**
- Forces resolution to local workspace packages (prevents accidental registry installs)
- Automatic version bumping during publish
- Faster installs (no network fetch)
- Guaranteed version synchronization

**Alternative Protocols:**
| Protocol | Use Case | Example |
|----------|----------|---------|
| `workspace:*` | **Recommended** - Always latest local version | `"@unrdf/core": "workspace:*"` |
| `workspace:^` | Semver range (allows ^5.0.0) | `"@unrdf/core": "workspace:^5.0.0"` |
| `workspace:~` | Patch-level range | `"@unrdf/core": "workspace:~5.0.0"` |

**UNRDF Status:** ✅ All 32 internal dependencies use `workspace:*` (correct)

### 2.4 Peer Dependencies Pattern

**Example: `@unrdf/composables`**
```json
{
  "dependencies": {
    "vue": "^3.5.25"
  },
  "peerDependencies": {
    "vue": "^3.0.0"
  }
}
```

**Best Practice:**
- Declare flexible range in `peerDependencies` (e.g., `^3.0.0`)
- Install specific version in `dependencies` for development
- Users must provide peer dependency in their apps

---

## 3. Circular Dependency Detection & Prevention

### 3.1 UNRDF Dependency Graph Analysis

**Workspace Dependency Map:**
```
@unrdf/cli           => core, federation, hooks, oxigraph, streaming
@unrdf/composables   => core, streaming
@unrdf/core          => oxigraph
@unrdf/dark-matter   => core
@unrdf/engine-gateway => core, oxigraph
@unrdf/federation    => core, hooks
@unrdf/hooks         => core, oxigraph
@unrdf/kgc-4d        => core, oxigraph
@unrdf/kgn           => core, test-utils
@unrdf/knowledge-engine => core, streaming
@unrdf/oxigraph      => core (POTENTIAL CYCLE!)
@unrdf/project-engine => core
@unrdf/streaming     => core, hooks, oxigraph
```

**Circular Dependency Risk:**
```
@unrdf/core => @unrdf/oxigraph
@unrdf/oxigraph => @unrdf/core  (CYCLE DETECTED!)
```

**Impact:**
- ⚠️ pnpm cannot guarantee topological build order
- ⚠️ May cause runtime initialization issues
- ⚠️ Breaks clean architecture boundaries

**Resolution Strategy:**
1. **Option A (Recommended):** Extract shared types to `@unrdf/types` package
   ```
   @unrdf/types (no dependencies)
   @unrdf/core => @unrdf/types, @unrdf/oxigraph
   @unrdf/oxigraph => @unrdf/types
   ```

2. **Option B:** Move Oxigraph to core as internal module
   ```
   @unrdf/core (includes oxigraph internally)
   ```

3. **Option C:** Use peerDependencies to break cycle
   ```json
   // @unrdf/oxigraph/package.json
   {
     "peerDependencies": {
       "@unrdf/core": "workspace:*"
     }
   }
   ```

### 3.2 Circular Dependency Detection Tools

| Tool | Weekly Downloads | GitHub Stars | Best For |
|------|-----------------|--------------|----------|
| **madge** | 1,484,758 | 9,873 | Visual graphs, CommonJS/ESM/AMD |
| **dependency-cruiser** | 788,616 | 6,236 | Custom rules, advanced validation |
| **dpdm** | 301,885 | 824 | TypeScript/JavaScript accuracy |

#### Recommended Tool: **dependency-cruiser**

**Installation:**
```bash
pnpm add -D -w dependency-cruiser
```

**Configuration (`.dependency-cruiser.cjs`):**
```javascript
module.exports = {
  forbidden: [
    {
      name: 'no-circular',
      severity: 'error',
      comment: 'Circular dependencies are not allowed',
      from: {},
      to: { circular: true }
    },
    {
      name: 'no-orphans',
      severity: 'warn',
      comment: 'Orphan modules should be cleaned up',
      from: { orphan: true },
      to: {}
    },
    {
      name: 'no-workspace-cycle',
      severity: 'error',
      comment: 'Workspace packages cannot have circular dependencies',
      from: { path: '^packages/[^/]+' },
      to: {
        path: '^packages/[^/]+',
        circular: true,
        pathNot: 'node_modules'
      }
    }
  ],
  options: {
    doNotFollow: {
      path: 'node_modules'
    },
    tsPreCompilationDeps: true,
    tsConfig: {
      fileName: './tsconfig.json'
    },
    enhancedResolveOptions: {
      exportsFields: ['exports'],
      conditionNames: ['import', 'require', 'node', 'default']
    }
  }
};
```

**Usage:**
```bash
# Check for circular dependencies
pnpm depcruise --config .dependency-cruiser.cjs packages

# Generate visual graph
pnpm depcruise --config .dependency-cruiser.cjs --output-type dot packages | dot -T svg > dependency-graph.svg

# Add to package.json scripts
"deps:check": "depcruise --config .dependency-cruiser.cjs packages",
"deps:graph": "depcruise --config .dependency-cruiser.cjs --output-type dot packages | dot -T svg > docs/dependency-graph.svg"
```

#### Alternative: **madge** (Simpler)

**Installation:**
```bash
pnpm add -D -w madge
```

**Usage:**
```bash
# Check workspace packages for cycles
pnpm madge --circular packages/*/src/index.mjs

# Generate visual graph
pnpm madge --image dependency-graph.svg packages/*/src/index.mjs

# Add to package.json
"deps:circular": "madge --circular --extensions mjs packages/*/src",
"deps:graph": "madge --image docs/dependency-graph.svg packages/*/src"
```

### 3.3 Prevention Strategies

**1. Architecture Rules (Layered Dependencies)**
```
Layer 1: @unrdf/types, @unrdf/test-utils (no dependencies)
Layer 2: @unrdf/oxigraph (depends on Layer 1 only)
Layer 3: @unrdf/core (depends on Layer 1-2)
Layer 4: @unrdf/hooks, @unrdf/streaming (depends on Layer 1-3)
Layer 5: @unrdf/federation, @unrdf/composables (depends on Layer 1-4)
Layer 6: @unrdf/cli, @unrdf/kgc-4d (depends on all layers)
```

**2. Pre-commit Hook**
```bash
#!/bin/bash
# .husky/pre-commit
pnpm deps:check || {
  echo "❌ Circular dependencies detected!"
  exit 1
}
```

**3. CI/CD Pipeline**
```yaml
# .github/workflows/ci.yml
- name: Check Circular Dependencies
  run: pnpm deps:check
```

---

## 4. Shared Scripts Across All Packages

### 4.1 Current UNRDF Root Scripts

**Root `package.json` Scripts:**
```json
{
  "scripts": {
    "test": "pnpm -r test",
    "test:fast": "pnpm -r test:fast",
    "test:watch": "pnpm -r test:watch",
    "test:coverage": "pnpm -r test -- --coverage",
    "test:core": "pnpm -C packages/core test",
    "test:hooks": "pnpm -C packages/hooks test",
    "test:federation": "pnpm -C packages/federation test",
    "test:streaming": "pnpm -C packages/streaming test",
    "test:browser": "pnpm -C packages/browser test",
    "test:cli": "pnpm -C packages/cli test",
    "test:knowledge-engine": "pnpm -C packages/knowledge-engine test",
    "lint": "pnpm -r lint",
    "lint:fix": "pnpm -r lint:fix",
    "format": "pnpm -r format",
    "format:check": "pnpm -r format:check",
    "build": "pnpm -r --filter './packages/*' --filter '!docs' --filter '!@unrdf/nextra-docs' build",
    "clean": "pnpm -r clean && rm -rf node_modules",
    "dev": "pnpm -r --parallel dev",
    "docs": "pnpm -C packages/core docs",
    "convert-paper": "node scripts/convert-paper.mjs",
    "precommit": "pnpm lint && pnpm test:fast"
  }
}
```

### 4.2 PNPM Script Flags Reference

| Flag | Purpose | Example |
|------|---------|---------|
| `-r` / `--recursive` | Run in all workspace packages | `pnpm -r test` |
| `-C <path>` | Run in specific package | `pnpm -C packages/core test` |
| `--filter <pattern>` | Filter packages by name/path | `pnpm --filter '@unrdf/*' build` |
| `--parallel` | Run scripts in parallel | `pnpm -r --parallel dev` |
| `--workspace-concurrency <n>` | Limit parallel tasks | `pnpm -r --workspace-concurrency 4 build` |
| `--if-present` | Skip if script doesn't exist | `pnpm -r --if-present build` |
| `--stream` | Stream output immediately | `pnpm -r --stream test` |
| `--aggregate-output` | Group output by package | `pnpm -r --aggregate-output test` |

### 4.3 Best Practice Script Patterns

#### Pattern 1: Parallel Execution (Fast Feedback)
```json
{
  "dev": "pnpm -r --parallel dev",
  "test:watch": "pnpm -r --parallel test:watch"
}
```

#### Pattern 2: Sequential Execution (Topological Order)
```json
{
  "build": "pnpm -r --workspace-concurrency 1 build"
}
```

#### Pattern 3: Filtered Execution
```json
{
  "build:packages": "pnpm -r --filter './packages/*' --filter '!@unrdf/docs' build",
  "test:core-packages": "pnpm --filter '@unrdf/core' --filter '@unrdf/hooks' test"
}
```

#### Pattern 4: Conditional Scripts
```json
{
  "build:changed": "pnpm --filter '...[origin/main]' build",
  "test:affected": "pnpm --filter '...[HEAD~1]' test"
}
```

### 4.4 Recommended Script Additions

**Add to Root `package.json`:**
```json
{
  "scripts": {
    // Existing scripts...

    // Dependency Management
    "deps:check": "depcruise --config .dependency-cruiser.cjs packages",
    "deps:graph": "depcruise --config .dependency-cruiser.cjs --output-type dot packages | dot -T svg > docs/dependency-graph.svg",
    "deps:update": "pnpm update --latest --recursive",
    "deps:outdated": "pnpm outdated --recursive",

    // Quality Gates
    "quality": "pnpm lint && pnpm format:check && pnpm deps:check && pnpm test",
    "ci": "pnpm quality && pnpm build",

    // Version Management
    "version:check": "pnpm -r exec -- node -e 'console.log(require(\"./package.json\").version)'",
    "version:bump": "pnpm -r exec -- npm version patch --no-git-tag-version",

    // Cleanup
    "clean:all": "pnpm -r clean && rm -rf node_modules **/node_modules",
    "clean:dist": "pnpm -r exec -- rm -rf dist",

    // Build Optimizations
    "build:core-first": "pnpm -C packages/core build && pnpm -r --filter '!@unrdf/core' build",
    "build:parallel": "pnpm -r --parallel --workspace-concurrency 4 build",

    // Selective Testing
    "test:unit": "pnpm -r --filter './packages/*' test -- --run",
    "test:integration": "pnpm -r --filter './packages/*' test -- --run --testNamePattern='integration'",
    "test:changed": "pnpm --filter '...[origin/main]' test"
  }
}
```

### 4.5 Package-Level Script Standardization

**Enforce Consistent Scripts Across Packages:**

All packages should implement:
```json
{
  "scripts": {
    "test": "vitest run --coverage",
    "test:fast": "vitest run --no-coverage",
    "test:watch": "vitest --coverage",
    "build": "unbuild || true",
    "lint": "eslint src/ test/ --max-warnings=0",
    "lint:fix": "eslint src/ test/ --fix",
    "format": "prettier --write src/ test/",
    "format:check": "prettier --check src/ test/",
    "clean": "rm -rf dist/ .nyc_output/ coverage/",
    "dev": "unbuild --watch"
  }
}
```

**Verification Script:**
```bash
#!/bin/bash
# scripts/verify-package-scripts.sh
REQUIRED_SCRIPTS="test test:fast build lint clean"

for pkg in packages/*/package.json; do
  echo "Checking $pkg..."
  for script in $REQUIRED_SCRIPTS; do
    if ! jq -e ".scripts[\"$script\"]" "$pkg" > /dev/null; then
      echo "❌ Missing script: $script in $pkg"
    fi
  done
done
```

---

## 5. Recommended pnpm-workspace.yaml Structure

### 5.1 Current Structure (Good)
```yaml
packages:
  - 'packages/*'
  - 'packages/kgc-4d/playground'
  - 'packages/atomvm/playground'
  - 'packages/cli/examples/*'
  - 'packages/hooks/examples/*'
  - 'packages/federation/examples/*'
  - 'packages/streaming/examples/*'
  - 'playground/full-stack-example/apps/*'
  - 'apps/*'
```

### 5.2 Enhanced Structure (Best Practice)
```yaml
packages:
  # Core Libraries (Layer 1-3)
  - 'packages/types'
  - 'packages/test-utils'
  - 'packages/oxigraph'
  - 'packages/core'

  # Extensions (Layer 4-5)
  - 'packages/hooks'
  - 'packages/streaming'
  - 'packages/federation'
  - 'packages/composables'

  # Applications (Layer 6)
  - 'packages/cli'
  - 'packages/kgc-4d'
  - 'packages/knowledge-engine'

  # Wildcard for new packages
  - 'packages/*'

  # Examples & Playgrounds
  - 'packages/*/playground'
  - 'packages/*/examples/*'
  - 'playground/*'
  - 'apps/*'

  # Documentation (excluded from builds)
  - '!packages/docs'
  - '!packages/nextra-docs'
```

**Rationale:**
- Explicit listing shows architecture layers
- Wildcard catches new packages
- Negation patterns exclude non-publishable packages

---

## 6. Root package.json Best Practices

### 6.1 Recommended Root Structure

```json
{
  "name": "unrdf-workspace",
  "version": "5.0.1",
  "private": true,
  "type": "module",
  "description": "UNRDF v5 Monorepo - RDF Knowledge Graph Substrate Platform",

  "engines": {
    "node": ">=18.0.0",
    "pnpm": ">=8.0.0"
  },

  "packageManager": "pnpm@8.15.0",

  "scripts": {
    // (See Section 4.4)
  },

  "devDependencies": {
    // Shared build tools
    "unbuild": "^3.6.1",
    "typescript": "^5.9.3",

    // Shared test tools
    "vitest": "^4.0.15",
    "@vitest/coverage-v8": "^4.0.15",
    "@vitest/ui": "^4.0.15",
    "@vitest/browser": "^4.0.15",

    // Shared linting
    "eslint": "^9.39.1",
    "eslint-config-prettier": "^10.1.8",
    "eslint-plugin-jsdoc": "^61.4.1",
    "prettier": "^3.7.4",

    // Monorepo tools
    "dependency-cruiser": "^16.0.0",
    "turbo": "^2.0.0",

    // Common types
    "@types/node": "^24.10.1"
  },

  "dependencies": {
    // Shared runtime dependencies (hoisted)
    "zod": "^4.1.13"
  },

  "pnpm": {
    "overrides": {
      "@opentelemetry/api": "^1.9.0",
      "zod": "^4.1.13",
      "n3": "^1.26.0"
    },
    "packageExtensions": {
      // Fix missing peer dependencies in third-party packages
    },
    "peerDependencyRules": {
      "allowedVersions": {
        "vue": "3"
      }
    }
  }
}
```

---

## 7. Dependency Deduplication Strategies

### 7.1 PNPM Auto-Deduplication

pnpm automatically deduplicates dependencies using:
- **Content-addressable storage** (single copy per version)
- **Hard links** (instant installs, minimal disk usage)
- **Strict node_modules** (prevents phantom dependencies)

**Results:**
- 60-80% disk usage reduction vs npm/yarn
- 3-5x faster installs
- Guaranteed reproducibility

### 7.2 Manual Deduplication Commands

```bash
# Find duplicate dependencies
pnpm list --depth Infinity | grep -A1 "dependencies:"

# Deduplicate lockfile
pnpm dedupe

# Update all to latest compatible versions
pnpm update --latest --recursive

# Check for outdated packages
pnpm outdated --recursive
```

### 7.3 Hoisting Strategy

**pnpm.hoisting Modes:**

| Mode | Behavior | Use Case |
|------|----------|----------|
| `shamefully-hoist=true` | Hoist all to root (npm/yarn compat) | Legacy apps |
| `public-hoist-pattern` | Hoist specific packages | Selective hoisting |
| **Default (strict)** | No hoisting (recommended) | Modern apps |

**UNRDF Current Config:**
- `packages/docs/.npmrc`: `shamefully-hoist=true` (for Nextra compatibility)

**Recommendation:**
```ini
# .npmrc (root)
# Default: strict mode (no hoisting)
shamefully-hoist=false

# Hoist specific packages only
public-hoist-pattern[]=*eslint*
public-hoist-pattern[]=*prettier*
```

---

## 8. UNRDF-Specific Recommendations

### 8.1 Immediate Actions (Priority 1)

1. **Fix Circular Dependency: `core ↔ oxigraph`**
   - Create `@unrdf/types` package
   - Extract shared TypeScript types
   - Break cycle by removing `core` dependency from `oxigraph`

2. **Install Circular Dependency Detection**
   ```bash
   pnpm add -D -w dependency-cruiser
   pnpm deps:check
   ```

3. **Fix Zod Version Inconsistency**
   ```bash
   # Update @unrdf/streaming/package.json
   "dependencies": {
     "zod": "^4.1.13"  // Match root override
   }
   ```

4. **Add Root `.npmrc`**
   ```ini
   shamefully-hoist=false
   public-hoist-pattern[]=*eslint*
   public-hoist-pattern[]=*prettier*
   auto-install-peers=true
   strict-peer-dependencies=false
   ```

### 8.2 Short-Term Improvements (Priority 2)

1. **Standardize Package Scripts**
   - Ensure all packages have `test`, `build`, `lint`, `clean`
   - Add `scripts/verify-package-scripts.sh`

2. **Add Quality Gate Scripts**
   ```json
   "quality": "pnpm lint && pnpm format:check && pnpm deps:check && pnpm test",
   "ci": "pnpm quality && pnpm build"
   ```

3. **Configure Pre-commit Hooks**
   ```bash
   pnpm add -D -w husky lint-staged
   npx husky install
   npx husky add .husky/pre-commit "pnpm deps:check && pnpm lint"
   ```

### 8.3 Long-Term Optimizations (Priority 3)

1. **Consider Turborepo Integration**
   ```bash
   pnpm add -D -w turbo
   ```
   - Caching for builds/tests
   - Parallel task execution
   - Remote caching (CI speedup)

2. **Implement Changesets for Versioning**
   ```bash
   pnpm add -D -w @changesets/cli
   pnpm changeset init
   ```

3. **Extract Common Patterns**
   - Move OTEL setup to `@unrdf/observability`
   - Create `@unrdf/test-utils` with shared test helpers

---

## 9. Tools for Circular Dependency Detection

### 9.1 Comparison Matrix

| Tool | Stars | Downloads/wk | Accuracy | Config | Graph | CI/CD |
|------|-------|--------------|----------|--------|-------|-------|
| **dependency-cruiser** | 6,236 | 788,616 | ★★★★★ | ★★★★★ | ★★★★★ | ★★★★★ |
| **madge** | 9,873 | 1,484,758 | ★★★★☆ | ★★★☆☆ | ★★★★★ | ★★★★☆ |
| **dpdm** | 824 | 301,885 | ★★★★★ | ★★★☆☆ | ★★★☆☆ | ★★★★☆ |

### 9.2 Recommended Toolchain

**For UNRDF Monorepo:**
```bash
# Install dependency-cruiser (most comprehensive)
pnpm add -D -w dependency-cruiser

# Install madge (for quick visual graphs)
pnpm add -D -w madge

# Configure scripts
pnpm pkg set scripts.deps:check="depcruise --config .dependency-cruiser.cjs packages"
pnpm pkg set scripts.deps:graph="madge --image docs/dependency-graph.svg packages/*/src"
pnpm pkg set scripts.deps:circular="madge --circular --extensions mjs packages/*/src"
```

### 9.3 CI/CD Integration

**GitHub Actions Workflow:**
```yaml
name: Dependency Check

on:
  pull_request:
    branches: [main]
  push:
    branches: [main]

jobs:
  check-deps:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
        with:
          version: 8.15.0

      - name: Install Dependencies
        run: pnpm install --frozen-lockfile

      - name: Check Circular Dependencies
        run: pnpm deps:check

      - name: Generate Dependency Graph
        run: pnpm deps:graph

      - name: Upload Graph Artifact
        uses: actions/upload-artifact@v4
        with:
          name: dependency-graph
          path: docs/dependency-graph.svg
```

---

## 10. References & Sources

### Official Documentation
- [PNPM Workspaces Documentation](https://pnpm.io/workspaces)
- [PNPM Settings (pnpm-workspace.yaml)](https://pnpm.io/next/settings)
- [Turborepo Managing Dependencies](https://turborepo.com/docs/crafting-your-repository/managing-dependencies)

### Best Practice Guides (2025)
- [Mastering pnpm Workspaces: Complete Guide to Monorepo Management](https://blog.glen-thomas.com/software%20engineering/2025/10/02/mastering-pnpm-workspaces-complete-guide-to-monorepo-management.html)
- [Complete Monorepo Guide: pnpm + Workspace + Changesets (2025)](https://jsdev.space/complete-monorepo-guide/)
- [Complete Monorepo Guide – pnpm Workspaces Changesets (2025)](https://peerlist.io/saxenashikhil/articles/complete-monorepo-guide--pnpm--workspaces--changesets-2025)
- [How to Bootstrap a Monorepo with PNPM: A Complete Guide](https://www.wisp.blog/blog/how-to-bootstrap-a-monorepo-with-pnpm-a-complete-guide)
- [Monorepo using pnpm workspaces - Medium](https://fazalerabbi.medium.com/monorepo-using-pnpm-workspaces-cb23ed332127)

### Tool Documentation
- [dependency-cruiser - npm](https://www.npmjs.com/package/dependency-cruiser)
- [madge - npm](https://www.npmjs.com/package/madge)
- [dpdm - npm](https://socket.dev/npm/package/dpdm)
- [GitHub - acrazing/dpdm: Detect circular dependencies](https://github.com/acrazing/dpdm)
- [dependency-cruiser vs dpdm vs madge - npm trends](https://npmtrends.com/dependency-cruiser-vs-dpdm-vs-madge)

### Advanced Topics
- [Identifying Circular Dependencies in JavaScript Modules](https://brianperry.dev/til/2021/identifying-circular-dependencies-in-javascript/)
- [Automate Circular Dependency Detection in your Node.js Project](https://sanyamaggarwal.medium.com/automate-circular-dependency-detection-in-your-node-js-project-394ed08f64bf)
- [Detecting circular dependencies in Javascript projects](https://sergiocarracedo.es/circular-dependencies/)
- [How to detect circular dependencies within a pnpm monorepo](https://github.com/orgs/pnpm/discussions/5544)
- [PNPM vs. Bun Install vs. Yarn Berry](https://betterstack.com/community/guides/scaling-nodejs/pnpm-vs-bun-install-vs-yarn/)
- [Dependency Management - DeepWiki](https://deepwiki.com/pnpm/pnpm/5.7-dependency-management)

---

## Appendix A: UNRDF Dependency Analysis

### A.1 Top 5 Most Depended Packages
```
1. @unrdf/core (11 dependents)
2. @unrdf/oxigraph (5 dependents)
3. @unrdf/streaming (3 dependents)
4. @unrdf/hooks (3 dependents)
5. @unrdf/federation (1 dependent)
```

### A.2 Leaf Packages (No Internal Dependencies)
```
- @unrdf/atomvm
- @unrdf/domain
- @unrdf/nextra-docs
- @unrdf/test-utils
- @unrdf/validation
- docs
```

### A.3 Critical Path (Build Order)
```
1. @unrdf/test-utils
2. @unrdf/oxigraph
3. @unrdf/core
4. @unrdf/hooks, @unrdf/streaming (parallel)
5. @unrdf/federation, @unrdf/composables, @unrdf/knowledge-engine (parallel)
6. @unrdf/cli (final)
```

---

## Appendix B: Script Template Library

### B.1 Root Scripts (Copy-Paste Ready)
```json
{
  "scripts": {
    "dev": "pnpm -r --parallel dev",
    "build": "pnpm -r --workspace-concurrency 4 build",
    "test": "pnpm -r test",
    "test:fast": "pnpm -r --parallel test:fast",
    "test:watch": "pnpm -r --parallel test:watch",
    "test:coverage": "pnpm -r test -- --coverage",
    "lint": "pnpm -r lint",
    "lint:fix": "pnpm -r lint:fix",
    "format": "pnpm -r format",
    "format:check": "pnpm -r format:check",
    "clean": "pnpm -r clean",
    "clean:all": "pnpm -r clean && rm -rf node_modules",
    "deps:check": "depcruise --config .dependency-cruiser.cjs packages",
    "deps:graph": "madge --image docs/dependency-graph.svg packages/*/src",
    "deps:circular": "madge --circular --extensions mjs packages/*/src",
    "deps:update": "pnpm update --latest --recursive",
    "deps:outdated": "pnpm outdated --recursive",
    "quality": "pnpm lint && pnpm format:check && pnpm deps:check && pnpm test",
    "ci": "pnpm quality && pnpm build",
    "precommit": "pnpm lint && pnpm test:fast"
  }
}
```

### B.2 Package Scripts (Standardized)
```json
{
  "scripts": {
    "test": "vitest run --coverage",
    "test:fast": "vitest run --no-coverage",
    "test:watch": "vitest --coverage",
    "build": "unbuild",
    "lint": "eslint src/ test/ --max-warnings=0",
    "lint:fix": "eslint src/ test/ --fix",
    "format": "prettier --write src/ test/",
    "format:check": "prettier --check src/ test/",
    "clean": "rm -rf dist/ .nyc_output/ coverage/",
    "dev": "unbuild --watch"
  }
}
```

---

**End of Research Document**

**Next Steps:**
1. Review findings with team
2. Implement Priority 1 actions (circular dependency fix, Zod version sync)
3. Install dependency-cruiser and configure CI checks
4. Standardize package scripts across monorepo
5. Add pre-commit hooks for quality gates

**Questions/Feedback:** Contact research agent or open issue in UNRDF repository.
