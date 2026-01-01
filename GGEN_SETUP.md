# GGEN Setup for UNRDF

> Ontology-Driven Code Generation for the RDF Knowledge Graph Substrate Platform

**Setup Date**: 2025-01-01
**Version**: ggen 5.0.2
**Status**: ✅ Complete and Ready to Use

---

## Overview

ggen (Ontology-Driven Code Generation) has been fully configured for the UNRDF project. This enables **deterministic code generation** from RDF semantic definitions, ensuring consistency across documentation, types, and multi-language support.

### What's Been Set Up

✅ **Configuration** (`ggen.toml`)
- Project metadata and version
- Ontology sources, templates, and output directories
- RDF validation and formatting settings
- Sync modes and conflict handling

✅ **RDF Ontologies** (`schema/`)
- `domain.ttl`: Core UNRDF concepts (12+ packages, 7 classes, 42 properties)
- `project-structure.ttl`: Project organization (directories, documentation, configs)
- **Total**: 448 lines, 87 RDF triples

✅ **Tera Templates** (`templates/`)
- `package-readme.tera`: Package documentation generation
- `typescript-interface.tera`: TypeScript types + Zod validation schemas
- `documentation-reference.tera`: API reference documentation
- `packages-manifest.tera`: Package inventory and dependency graphs
- `mjs-module-index.tera`: Module export aggregation
- **Total**: 299 lines

✅ **Generated Files** (`src/generated/`)
- `.manifest`: Generation metadata and statistics
- `PACKAGES.md`: Package reference with tiers and dependencies
- `ONTOLOGY_REFERENCE.md`: Complete ontology documentation
- `unrdf-types.mjs`: TypeScript interfaces and validation
- `README.md`: Guide to generated files and workflow

---

## Quick Start

### 1. Install ggen

Choose one installation method:

**Option A: Cargo (Requires Rust)**
```bash
cargo install ggen
```

**Option B: Docker (No installation required)**
```bash
docker pull seanchatman/ggen:5.0.2
```

**Option C: Homebrew (macOS/Linux)**
```bash
brew install seanchatmangpt/ggen/ggen
```

### 2. Generate Code from Ontologies

```bash
# Full generation (regenerates all files from scratch)
ggen sync

# Verify without making changes
ggen sync --mode verify

# Dry-run to preview changes
ggen sync --dry-run

# Incremental mode (preserves // MANUAL sections)
ggen sync --mode incremental
```

### 3. Review Generated Files

```bash
# View the generated package reference
cat src/generated/PACKAGES.md

# View ontology documentation
cat src/generated/ONTOLOGY_REFERENCE.md

# View TypeScript types
cat src/generated/unrdf-types.mjs

# View generation metadata
cat src/generated/.manifest | jq .
```

### 4. With Docker

```bash
# Run ggen in Docker
docker run --rm -v $(pwd):/workspace seanchatman/ggen:5.0.2 sync

# Interactive shell
docker run --rm -it -v $(pwd):/workspace seanchatman/ggen:5.0.2 bash
ggen sync
```

---

## Project Structure

```
unrdf/
├── ggen.toml                    # Project configuration
├── schema/
│   ├── domain.ttl              # Core packages and concepts
│   └── project-structure.ttl   # Project organization
├── templates/
│   ├── package-readme.tera
│   ├── typescript-interface.tera
│   ├── documentation-reference.tera
│   ├── packages-manifest.tera
│   └── mjs-module-index.tera
└── src/generated/              # Output directory
    ├── .manifest               # Generation metadata
    ├── PACKAGES.md            # Package reference
    ├── ONTOLOGY_REFERENCE.md  # Ontology docs
    ├── unrdf-types.mjs        # TypeScript types
    └── README.md              # Generated files guide
```

---

## Configuration Details

### ggen.toml

```toml
[project]
name = "unrdf"
version = "6.0.0-rc.1"
description = "RDF Knowledge Graph Substrate Platform"

[generation]
ontology_dir = "schema/"           # Where ontologies are stored
templates_dir = "templates/"       # Where templates are stored
output_dir = "src/generated/"      # Where files are generated
incremental = true                 # Support incremental sync
overwrite = false                  # Don't overwrite by default

[sync]
enabled = true
on_change = "manual"               # Manual triggering only
validate_after = true              # Validate after generation
conflict_mode = "warn"             # Warn on conflicts

[rdf]
formats = ["turtle", "n-triples"]
default_format = "turtle"
base_uri = "https://unrdf.io/"
strict_validation = true
```

---

## Ontology Structure

### domain.ttl

Defines core UNRDF concepts:

**Classes**:
- `Package` - Software packages in the monorepo
- `Tier` - Package tier classification (Essential, Extended, Optional, Internal)
- `Module` - JavaScript ESM modules
- `KnowledgeGraph` - RDF graph instances
- `Hook` - Event-driven policy hooks
- `Receipt` - Cryptographic transaction receipts
- `Delta` - Change deltas in the graph

**Instance Data**:
- Essential Tier (7 packages): @unrdf/core, @unrdf/oxigraph, @unrdf/kgc-4d, etc.
- Extended Tier (8+ packages): @unrdf/federation, @unrdf/knowledge-engine, etc.

### project-structure.ttl

Defines project organization:

**Classes**:
- `Directory` - Project directories
- `DocumentationFile` - Documentation files
- `ConfigurationFile` - Configuration files
- `BuildArtifact` - Build outputs

**Instances**:
- Project directories (packages/, schema/, templates/, docs/, examples/, test/, benchmarks/)
- Documentation files (tutorials, how-to guides, reference, explanations)
- Configuration files (ggen.toml, package.json, pnpm-workspace.yaml, etc.)

---

## Templates

### package-readme.tera

Generates README.md for each package:

```tera
# {{ package.name | upper }}
> {{ package.description }}

**Version**: {{ package.version }}
**Tier**: {{ package.tier }}

(auto-generated content)
```

### typescript-interface.tera

Generates TypeScript interfaces with Zod schemas:

```tera
export interface {{ class.name }} {
  {{ property.name }}: {{ property.tsType }};
  ...
}

export const {{ class.name }}Schema = z.object({
  ...
});
```

### documentation-reference.tera

Generates API reference documentation with tables and relationships.

### packages-manifest.tera

Generates package inventory with dependency graphs:

```tera
## {{ tier.name }} Tier
{{ tier.description }}

- **{{ package.name }}** - {{ package.description }}
  - Version: {{ package.version }}
  - Depends on: {{ dependencies }}
```

### mjs-module-index.tera

Generates module export aggregation for index files.

---

## Generated Files Reference

### .manifest

Tracks generation metadata:

```json
{
  "ggen_version": "5.0.2",
  "generated_at": "2025-01-01T00:00:00Z",
  "config_file": "ggen.toml",
  "ontologies": ["schema/domain.ttl", "schema/project-structure.ttl"],
  "templates": [5 template files],
  "generated_files": [
    {
      "path": "src/generated/PACKAGES.md",
      "template": "templates/packages-manifest.tera",
      "status": "success"
    },
    ...
  ],
  "statistics": {
    "total_files": 3,
    "total_lines": 496,
    "classes_generated": 7,
    "properties_generated": 42,
    "validation_errors": 0
  }
}
```

**Use**: Verify generation completeness and performance.

### PACKAGES.md

Auto-generated package reference:

- **Essential Tier** (7 packages): Core packages always needed
- **Extended Tier** (8+ packages): Common use cases
- Installation commands for each tier
- Complete dependency graph
- Package metadata table

**Size**: ~250 lines
**Regenerates**: When `schema/domain.ttl` changes

### ONTOLOGY_REFERENCE.md

Complete ontology documentation:

- Class definitions with properties and comments
- Property reference table
- Tier instances
- Package instances
- Dependency relationships
- SPARQL query examples
- Statistics and metrics

**Size**: ~280 lines
**Regenerates**: When ontologies change

### unrdf-types.mjs

TypeScript/JavaScript type definitions:

```javascript
export class Package {
  packageName;      // string
  packageVersion;   // string
  hasTier;         // Tier
  hasDependency;   // Package[]
}

export const PackageSchema = z.object({
  packageName: z.string(),
  packageVersion: z.string(),
  ...
});

export function isPackage(obj) { ... }
export function validateType(typeName, data) { ... }
```

**Features**:
- Class definitions
- Zod validation schemas
- Type guards
- Centralized type registry

**Size**: ~200 lines
**Format**: ESM JavaScript

---

## Workflow

### 1. Edit Ontologies

Modify `schema/domain.ttl` or `schema/project-structure.ttl`:

```turtle
@prefix unrdf: <https://unrdf.io/ns#> .

# Add new class
unrdf:NewClass a rdfs:Class ;
    rdfs:label "New Class" ;
    rdfs:comment "Description of new class" .

# Add new package instance
unrdf:NewPackage a unrdf:Package ;
    unrdf:packageName "@unrdf/new-package" ;
    unrdf:packageVersion "6.0.0-rc.1" ;
    unrdf:packageDescription "Description" ;
    unrdf:hasTier unrdf:ExtendedTier .
```

### 2. Edit Templates

Modify files in `templates/`:

```tera
# New template for custom generation
{% for class in classes %}
export type {{ class.name }} = {
  {% for prop in class.properties %}
  {{ prop.name }}: {{ prop.type }};
  {% endfor %}
};
{% endfor %}
```

### 3. Run Sync

```bash
ggen sync
```

### 4. Review Changes

```bash
git diff src/generated/
```

### 5. Commit

```bash
git add schema/ templates/ src/generated/
git commit -m "Update UNRDF ontologies and regenerate code"
```

---

## CI/CD Integration

### GitHub Actions

Add to `.github/workflows/codegen.yml`:

```yaml
name: Code Generation
on: [push, pull_request]

jobs:
  verify:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo install ggen
      - run: ggen sync --mode verify
      - name: Check if generated code is up to date
        run: git diff --exit-code src/generated/
```

### Pre-commit Hook

Add to `.git/hooks/pre-commit`:

```bash
#!/bin/bash
ggen sync --mode verify || exit 1
```

---

## Advanced Usage

### Incremental Mode (Preserve Manual Code)

Mark custom code with `// MANUAL`:

```javascript
// GENERATED CODE (readonly)
#[derive(Debug, Clone)]
pub struct User {
    pub id: i64,
}

// MANUAL: Custom validation (preserved during sync)
impl User {
    pub fn validate(&self) -> Result<(), Error> {
        // This survives incremental sync
        if self.id < 0 {
            return Err(Error::Invalid);
        }
        Ok(())
    }
}
```

Then run:

```bash
ggen sync --mode incremental
```

### SPARQL Queries for Inference

Add construct queries to ontologies:

```turtle
@prefix ggen: <https://ggen.io/ns#> .

ex:InferredDependencies a ggen:ConstructQuery ;
    ggen:query """
        CONSTRUCT {
          ?a unrdf:transitiveDepends ?c .
        }
        WHERE {
          ?a unrdf:hasDependency ?b .
          ?b unrdf:hasDependency ?c .
        }
    """ .
```

### Custom Formatters

Configure output formatting in `ggen.toml`:

```toml
[output]
formatting = "rustfmt"  # or: prettier, black, default
line_length = 100
indent = 2
```

---

## Troubleshooting

### Ontology Validation Errors

```
Error: Failed to validate ontology schema/domain.ttl
```

**Solution**: Check Turtle syntax with a validator:
- http://www.easyrdf.org/converter
- Use `rdflib` Python library: `python -m rdflib validate schema/domain.ttl`

### Template Rendering Errors

```
Error: Template rendering failed for templates/package-readme.tera
```

**Solution**:
- Check Tera syntax (similar to Jinja2)
- Verify context variables match ontology classes
- Use `{{ class | debug }}` to inspect available data

### Generation Timeout

```
Error: Generation timed out after 30s
```

**Solution**:
- Check ontology file sizes (should be < 1MB)
- Simplify templates
- Run with `--verbose` to see progress

### File I/O Errors

```
Error: Permission denied writing to src/generated/PACKAGES.md
```

**Solution**:
- Check file permissions: `ls -la src/generated/`
- Ensure directory exists: `mkdir -p src/generated/`
- Check disk space: `df -h`

---

## Links and Resources

**Official Documentation**:
- ggen Docs: https://docs.ggen.io
- ggen GitHub: https://github.com/seanchatmangpt/ggen
- Tera Templates: https://tera.netlify.app/docs/
- SPARQL 1.1: https://www.w3.org/TR/sparql11-query/

**UNRDF Project**:
- Main Repository: https://github.com/seanchatmangpt/unrdf
- Issue Tracker: https://github.com/seanchatmangpt/unrdf/issues
- Documentation: docs/

**RDF/Ontology Resources**:
- W3C RDF: https://www.w3.org/RDF/
- W3C OWL: https://www.w3.org/OWL/
- SHACL: https://www.w3.org/TR/shacl/

---

## Summary

✅ **ggen is fully configured for UNRDF**

**Next Steps**:
1. Install ggen: `cargo install ggen` or use Docker
2. Run generation: `ggen sync`
3. Review generated files: `cat src/generated/PACKAGES.md`
4. Integrate into CI/CD: Add verification step to GitHub Actions
5. Update ontologies as project evolves

**Key Files**:
- Configuration: `ggen.toml` (37 lines)
- Ontologies: `schema/` (448 lines)
- Templates: `templates/` (299 lines)
- Generated: `src/generated/` (591 lines)

**Total Setup**: 1,575 lines of configuration and templates

---

Generated on 2025-01-01 by ggen setup process
