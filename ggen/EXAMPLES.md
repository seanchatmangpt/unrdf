# ggen Examples - Ontology-Driven Code Generation for UNRDF

This document provides practical examples of using ggen with the UNRDF ontology to generate code and documentation.

---

## Example 1: Generating Package Index Documentation

**Goal**: Automatically generate a master index of all UNRDF packages.

### Configuration (ggen.toml)

```toml
[project]
name = "unrdf"
version = "6.0.0-rc.1"
description = "RDF Knowledge Graph Substrate Platform"

[ontology]
source = "schemas/unrdf-packages.ttl"
format = "turtle"

[generation]
output_dir = "ggen/generated/"
rules = [
  { name = "packages_index", template = "packages-index.tera", output = "docs/packages-index.md" }
]
```

### Template (packages-index.tera)

```tera
# UNRDF Packages Index

> Auto-generated from ontology
> Last updated: {{ generation_date }}

## Essential Packages ({{ essential_count }})

{% for package in packages | filter(attribute="tier", value="essential") %}
- **{{ package.label }}** (`{{ package.name }}`) - {{ package.description }}
{% endfor %}

## Extended Packages ({{ extended_count }})

{% for package in packages | filter(attribute="tier", value="extended") %}
- **{{ package.label }}** (`{{ package.name }}`) - {{ package.description }}
{% endfor %}
```

### Running

```bash
ggen sync --rule packages_index
```

### Result

Generates `docs/packages-index.md` with current package information from the ontology.

**Benefits**:
- Single source of truth (ontology)
- Automatic updates when packages change
- Consistent documentation across the project

---

## Example 2: Generating TypeScript Type Definitions

**Goal**: Auto-generate TypeScript interfaces matching your RDF ontology.

### Configuration (ggen.toml)

```toml
[[generation.rules]]
name = "typescript_types"
description = "Generate TypeScript interfaces from ontology"
template = "ts-interfaces.tera"
output = "src/generated/unrdf-types.ts"
```

### Template (ts-interfaces.tera)

```typescript
/**
 * Auto-generated TypeScript interfaces from UNRDF ontology
 * Generated: {{ generation_date }}
 * DO NOT EDIT - Changes will be overwritten
 */

{% for package in packages %}
/**
 * {{ package.label }}
 */
export interface I{{ package.label | replace(pattern=" ", replace="") }} {
  name: "{{ package.name }}";
  version: "{{ package.version }}";
  tier: "{{ package.tier }}";
  testCoverage: {{ package.testCoverage }};
  capabilities: {{ package.capabilities | length }};
}
{% endfor %}

/**
 * UNRDF Platform Configuration
 */
export interface IUnrdfPlatform {
  packages: IPackage[];
  version: "{{ version }}";
  essential: IPackage[];
  extended: IPackage[];
  optional: IPackage[];
}
```

### Running

```bash
ggen sync --rule typescript_types
```

### Result

Generates `src/generated/unrdf-types.ts` with TypeScript interfaces for all packages.

**Benefits**:
- Type-safe code generation
- Sync TypeScript and RDF definitions
- Reduce manual type annotation

---

## Example 3: Per-Package Documentation

**Goal**: Generate API documentation for each UNRDF package.

### Configuration (ggen.toml)

```toml
[[generation.rules]]
name = "package_docs"
description = "Generate API documentation per package"
template = "package-documentation.tera"
output = "docs/packages/{{ name }}.md"
# This requires SPARQL query to iterate packages
```

### Template (package-documentation.tera)

```markdown
# {{ package.label }} API Reference

**Package**: `{{ package.name }}`
**Path**: `packages/{{ package.path | split(pat="/") | last }}`
**Version**: {{ package.version }}
**Tier**: {{ package.tier | capitalize }}
**Coverage**: {{ package.testCoverage }}%

## Overview

{{ package.description }}

## Installation

\`\`\`bash
npm install {{ package.name }}
\`\`\`

## Quick Start

\`\`\`javascript
import { {{ package.mainExport }} } from '{{ package.name }}';

const instance = {{ package.mainExport }}();
\`\`\`

## Features

{% for capability in package.capabilities %}
### {{ capability.label }}

{{ capability.description }}

{% endfor %}

## Dependencies

{% if package.dependencies | length > 0 %}
This package depends on:

{% for dep in package.dependencies %}
- \`{{ dep.name }}\` (v{{ dep.version }})
{% endfor %}
{% else %}
This package has no internal dependencies.
{% endif %}

## Testing

\`\`\`bash
npm test
npm test -- --coverage
\`\`\`

Current test coverage: **{{ package.testCoverage }}%**
```

### Running

```bash
ggen sync --rule package_docs --verbose true
```

### Result

Generates `docs/packages/@unrdf-core.md`, `docs/packages/@unrdf-oxigraph.md`, etc.

**Benefits**:
- Automatic API documentation
- Consistency across all packages
- Always up-to-date with ontology changes

---

## Example 4: Package Manifest Generation

**Goal**: Generate metadata manifests for each package (for registry or tooling).

### Configuration (ggen.toml)

```toml
[[generation.rules]]
name = "package_manifest"
description = "Generate package manifest JSON"
template = "package-manifest.tera"
output = "packages/{{ name }}/ggen-manifest.json"
```

### Template (package-manifest.tera)

```json
{
  "$schema": "https://unrdf.io/schemas/package-manifest.json",
  "generated": "{{ generation_date }}",
  "package": {
    "name": "{{ package.name }}",
    "version": "{{ package.version }}",
    "path": "packages/{{ package.path | split(pat="/") | last }}",
    "tier": "{{ package.tier }}"
  },
  "quality": {
    "testCoverage": {{ package.testCoverage }},
    "minNodeVersion": "{{ package.minNodeVersion }}"
  },
  "capabilities": [
    {% for cap in package.capabilities %}
    "{{ cap.id }}"{{ not loop.last ? "," : "" }}
    {% endfor %}
  ],
  "dependencies": {
    "internal": [
      {% for dep in package.dependencies %}
      "{{ dep.name }}"{{ not loop.last ? "," : "" }}
      {% endfor %}
    ]
  },
  "exports": {
    "main": "{{ package.mainExport }}"
  }
}
```

### Running

```bash
ggen sync --rule package_manifest
```

### Result

Generates manifests for each package with structured metadata.

**Benefits**:
- Machine-readable package information
- Integration with tools and registries
- Dependency tracking and analysis

---

## Example 5: Multi-Format Generation (Pipeline)

**Goal**: Generate multiple output formats from a single ontology in one sync operation.

### Configuration (ggen.toml)

```toml
[project]
name = "unrdf"
version = "6.0.0-rc.1"

[ontology]
source = "schemas/unrdf-packages.ttl"
format = "turtle"

[generation]
output_dir = "ggen/generated/"
rules = [
  { name = "docs_md", template = "package-documentation.tera", output = "docs/{name}.md" },
  { name = "types_ts", template = "ts-interfaces.tera", output = "src/types.ts" },
  { name = "manifest_json", template = "package-manifest.tera", output = "manifests/{name}.json" },
  { name = "index_html", template = "index.tera", output = "static/index.html" }
]
```

### Running

```bash
# Generate all formats
ggen sync

# Or specific rules
ggen sync --rule docs_md
ggen sync --rule types_ts

# Dry run to preview
ggen sync --dry-run true

# Watch for changes
ggen sync --watch true
```

### Result

All three formats generated from single ontology source.

**Benefits**:
- Single source of truth
- Consistent data across formats
- Reduced maintenance burden

---

## Example 6: Conditional Generation (Tiers)

**Goal**: Generate different artifacts based on package tier.

### Configuration (ggen.toml)

```toml
[[generation.rules]]
name = "essential_packages"
description = "Generate stubs for essential packages only"
template = "essential-stub.tera"
output = "stubs/essential-{{ name }}.ts"
# Only processes packages where tier == "essential"
```

### Template (essential-stub.tera)

```typescript
/**
 * Auto-generated stub for {{ package.label }}
 * This is an ESSENTIAL tier package
 *
 * Test Coverage: {{ package.testCoverage }}%
 * Node.js: >= {{ package.minNodeVersion }}
 */

export interface {{ package.label | replace(pattern=" ", replace="") }}Options {
  // Options for {{ package.label }}
}

export async function {{ package.mainExport }}(
  options?: {{ package.label | replace(pattern=" ", replace="") }}Options
): Promise<void> {
  // Implementation from @unrdf packages
}
```

### Running

```bash
ggen sync --rule essential_packages
```

### Result

Generates stubs only for essential tier packages.

**Benefits**:
- Tier-specific generation
- Graduated complexity based on importance
- Bootstrap new packages faster

---

## Example 7: Interactive Mode

**Goal**: Generate with user prompts and validation.

### Running

```bash
# Dry run first to preview
ggen sync --dry-run true --format json

# Validate without generating
ggen sync --validate-only true

# Audit trail of changes
ggen sync --audit true

# Force overwrite if conflicts
ggen sync --force true
```

---

## Workflow: Development to CI/CD

### Local Development

```bash
# Watch mode - regenerate on changes
ggen sync --watch true --verbose true
```

### Before Commit

```bash
# Validate all rules
ggen sync --validate-only true || exit 1

# Generate for commit
ggen sync

# Review and commit
git add ggen/generated/
git commit -m "chore: regenerate from ontology"
```

### CI/CD Pipeline

```bash
# Verify sync is up-to-date
ggen sync --dry-run true --format json > sync-check.json

# Fail if out-of-sync
if ! grep -q '"files_synced":0' sync-check.json; then
  echo "Generated files out of sync with ontology"
  exit 1
fi
```

---

## Best Practices

### 1. Single Source of Truth

Keep the RDF ontology as the source of truth. Update there first:

```bash
# Good
1. Edit schemas/unrdf-packages.ttl
2. Run ggen sync
3. Commit generated files

# Bad
1. Manually edit generated files
2. Forget to update ontology
3. Inconsistency between sources
```

### 2. Template Reusability

Create generic templates that work across multiple use cases:

```tera
# Good - works for any package
{% for package in packages %}
  {{ package.name }} - {{ package.description }}
{% endfor %}

# Bad - hardcoded for specific packages
@unrdf/core - RDF Graph Operations
@unrdf/oxigraph - Oxigraph engine
```

### 3. Validation Before Generation

Always validate before generating:

```bash
# Check configuration
ggen sync --validate-only true

# Preview changes
ggen sync --dry-run true

# Then generate
ggen sync
```

### 4. Version Control

Commit generated files and track changes:

```bash
git add ggen/generated/
git commit -m "chore: regenerate from ontology [skip ci]"
```

### 5. Documentation

Document any custom rules or templates:

```toml
[[generation.rules]]
name = "my_custom_rule"
description = "What this generates and why"
template = "my-template.tera"
output = "path/to/{{ variable }}.ext"
```

---

## Troubleshooting Examples

### Issue: No files generated

```bash
# Check if rules are defined
cat ggen.toml | grep -A5 "rules ="

# Validate configuration
ggen sync --validate-only true

# Try dry-run with verbose
ggen sync --dry-run true --verbose true
```

### Issue: Template errors

```bash
# Check template syntax
cat ggen/templates/my-template.tera

# Validate ontology data
# Ensure variables in template exist in ontology
```

### Issue: Out of sync during CI

```bash
# Regenerate locally
ggen sync

# Commit changes
git add ggen/generated/
git commit -m "chore: regenerate"

# Push to CI
```

---

## Next Steps

1. **Start with Example 1** - Generate package index documentation
2. **Add Example 2** - Generate TypeScript types
3. **Create custom templates** for your specific needs
4. **Integrate into CI/CD** for automatic generation
5. **Document your rules** in project README

---

**UNRDF v6.0.0-rc.1** - https://unrdf.io
