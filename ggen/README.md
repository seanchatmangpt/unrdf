# UNRDF ggen Setup

**ggen** is an ontology-driven code generation tool that transforms RDF ontologies into typed code using SPARQL queries and Tera templates.

This directory contains the UNRDF ggen setup, including:
- **Configuration**: `ggen.toml` - Project configuration
- **Ontologies**: `schemas/unrdf-packages.ttl` - RDF description of UNRDF packages
- **Templates**: `ggen/templates/*.tera` - Tera templates for generating files
- **Generated Output**: `ggen/generated/` - Output files (auto-generated)

---

## Quick Start

### Installation

ggen is installed via Cargo. Verify installation:

```bash
ggen --version
# Output: ggen 5.0.2
```

If not installed, install it:

```bash
cargo install ggen-cli-lib
```

### First Sync

Run ggen sync to validate the configuration:

```bash
ggen sync --dry-run true
```

Expected output:
```
[DRY RUN] Would sync 0 files:
Inference rules: []
Generation rules: []
{"duration_ms":0,"files":[],"files_synced":0,"generation_rules_executed":0,"inference_rules_executed":0,"status":"success"}
```

---

## Configuration: ggen.toml

The `ggen.toml` file at the project root defines:

```toml
[project]
name = "unrdf"
version = "6.0.0-rc.1"
description = "RDF Knowledge Graph Substrate Platform"

[ontology]
source = "schemas/unrdf-packages.ttl"  # RDF ontology file
format = "turtle"                      # RDF format

[generation]
output_dir = "ggen/generated/"         # Output directory
rules = []                             # Generation rules (see below)
```

### Required Sections

1. **[project]** - Project metadata
   - `name`: Project identifier
   - `version`: Semantic version
   - `description`: Project description

2. **[ontology]** - RDF ontology source
   - `source`: Path to RDF file (turtle, n-triples, etc.)
   - `format`: RDF serialization format

3. **[generation]** - Code generation settings
   - `output_dir`: Directory for generated files
   - `rules`: Array of generation rules (optional)

---

## RDF Ontology: schemas/unrdf-packages.ttl

Defines the UNRDF platform structure using OWL vocabulary:

### Core Classes

- **Package** - Represents an npm package
  - `packageName` - npm package name (e.g., @unrdf/core)
  - `packagePath` - Relative path in monorepo
  - `version` - Semantic version
  - `description` - Short description
  - `tier` - Package tier (essential, extended, optional)
  - `testCoverage` - Test coverage percentage
  - `minNodeVersion` - Minimum required Node.js version
  - `mainExport` - Primary export function or class
  - `hasCapability` - Capabilities provided by package
  - `dependsOn` - Dependencies on other packages

- **Capability** - Features provided by packages
  - `label` - Human-readable name
  - `comment` - Description

- **PackageTier** - Classification of packages
  - `tierLevel` - Numeric level (1=essential, 2=extended, 3=optional)

### Example Package Definition

```turtle
unrdf:CorePackage a unrdf:Package ;
    unrdf:packageName "@unrdf/core" ;
    unrdf:packagePath "packages/core" ;
    unrdf:version "6.0.0-rc.1" ;
    unrdf:description "RDF Graph Operations, SPARQL, Foundational Substrate" ;
    unrdf:tier unrdf:EssentialTier ;
    unrdf:mainExport "createKnowledgeSubstrateCore" ;
    unrdf:testCoverage 95.0 ;
    unrdf:hasCapability unrdf:SPARQLQuerying ;
    unrdf:hasCapability unrdf:TripleStore ;
    rdfs:label "UNRDF Core" ;
    rdfs:comment "Foundation layer providing RDF operations" .
```

---

## Tera Templates: ggen/templates/

Templates define how to generate code and documentation from ontology data.

### Available Templates

1. **package-readme.tera** - Generate README for each package
   - Variables: `package` (Package object)
   - Output: `README.md` files per package

2. **ts-interfaces.tera** - Generate TypeScript interfaces
   - Variables: `packages` (array), `generation_date`
   - Output: `types.ts` with TypeScript interfaces

3. **package-documentation.tera** - Generate API documentation
   - Variables: `package`, `generation_date`, `version`
   - Output: `[package-name].md` API reference

4. **package-manifest.tera** - Generate package manifest JSON
   - Variables: `package`, `generation_date`
   - Output: `manifest.json` with package metadata

5. **packages-index.tera** - Generate packages index
   - Variables: `packages`, `generation_date`, `version`
   - Output: `packages-index.md` master documentation

### Template Variables

Templates have access to:

- **package** (object)
  - `name` - Package name
  - `path` - Package path
  - `version` - Version
  - `description` - Description
  - `tier` - Tier (essential, extended, optional)
  - `label` - Display label
  - `testCoverage` - Coverage percentage
  - `mainExport` - Main export name
  - `capabilities` - Array of capabilities
  - `dependencies` - Array of dependent packages
  - `externalDependencies` - Array of external package names

- **packages** (array) - All packages from ontology

- **generation_date** - Timestamp of generation

- **version** - Project version (from ggen.toml)

### Template Syntax

Tera is a Jinja2-like template engine. Common patterns:

```tera
# Loop over items
{% for package in packages %}
  {{ package.name }}
{% endfor %}

# Conditionals
{% if package.tier == "essential" %}
  This is essential
{% endif %}

# Filters
{{ package.name | upper }}

# Loops with last check
{% for item in items %}
  {{ item }}{{ not loop.last ? ", " : "" }}
{% endfor %}
```

---

## Examples

### Generate Package Documentation

Create a rule in `ggen.toml`:

```toml
[[generation.rules]]
name = "api_docs"
query = "SELECT ?p WHERE { ?p a <https://unrdf.io/ns/Package> }"
template = "package-documentation.tera"
output = "docs/api/{name}.md"
```

Run:
```bash
ggen sync
```

### Generate TypeScript Interfaces

```toml
[[generation.rules]]
name = "ts_types"
template = "ts-interfaces.tera"
output = "src/types.generated.ts"
```

---

## Commands

### Validate Configuration (Dry Run)

```bash
ggen sync --dry-run true
```

Preview changes without writing files.

### Run Code Generation

```bash
ggen sync
```

Generate code and documentation.

### Watch Mode (Development)

```bash
ggen sync --watch true
```

Regenerate when ontology or templates change.

### Verbose Output

```bash
ggen sync --verbose true
```

Show detailed generation process.

### Specific Rule

```bash
ggen sync --rule package_docs
```

Execute only specific generation rule.

---

## Adding New Packages to the Ontology

To add a new UNRDF package to the ontology:

1. **Edit** `schemas/unrdf-packages.ttl`

2. **Add package instance**:

```turtle
unrdf:MyNewPackage a unrdf:Package ;
    unrdf:packageName "@unrdf/my-new-package" ;
    unrdf:packagePath "packages/my-new-package" ;
    unrdf:version "6.0.0-rc.1" ;
    unrdf:description "What this package does" ;
    unrdf:tier unrdf:ExtendedTier ;
    unrdf:mainExport "createMyPackage" ;
    unrdf:testCoverage 85.0 ;
    unrdf:minNodeVersion "18.0.0" ;
    unrdf:hasCapability unrdf:SomeCapability ;
    unrdf:dependsOn unrdf:CorePackage ;
    rdfs:label "My New Package" ;
    rdfs:comment "Detailed description" .
```

3. **Create/update templates** if needed

4. **Run sync**:

```bash
ggen sync
```

---

## Project Structure

```
/home/user/unrdf/
├── ggen.toml                          # Main configuration
├── schemas/
│   └── unrdf-packages.ttl             # RDF ontology
├── ggen/
│   ├── templates/                     # Tera templates
│   │   ├── package-readme.tera
│   │   ├── ts-interfaces.tera
│   │   ├── package-documentation.tera
│   │   ├── package-manifest.tera
│   │   └── packages-index.tera
│   ├── generated/                     # Generated files (auto)
│   └── README.md                      # This file
└── ...
```

---

## Troubleshooting

### ggen not found

Install ggen:
```bash
cargo install ggen-cli-lib
```

Verify:
```bash
which ggen
ggen --version
```

### Manifest parse error

Check `ggen.toml` syntax:
- All required sections present: `[project]`, `[ontology]`, `[generation]`
- TOML syntax valid (valid brackets, commas)
- File paths exist and are readable

### No files generated

Check:
1. Generation rules are defined in `ggen.toml`
2. SPARQL queries return results
3. Templates exist and are readable
4. Output directory is writable

---

## Next Steps

1. **Define generation rules** in `ggen.toml` to enable code generation
2. **Add more capabilities** to the ontology for your use case
3. **Create custom templates** for your specific needs
4. **Integrate with CI/CD** to generate code on every commit

---

## Resources

- **ggen Documentation**: https://docs.ggen.io
- **ggen GitHub**: https://github.com/seanchatmangpt/ggen
- **Tera Template Engine**: https://tera.netlify.app/
- **RDF/Turtle Format**: https://www.w3.org/TR/turtle/
- **SPARQL 1.1 Query**: https://www.w3.org/TR/sparql11-query/

---

**UNRDF v6.0.0-rc.1** - https://unrdf.io
