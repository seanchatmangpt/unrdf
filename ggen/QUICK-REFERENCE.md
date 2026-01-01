# ggen Quick Reference

Fast reference for ggen commands, configuration, and operations.

## Installation

```bash
# Via Cargo (recommended)
cargo install ggen-cli-lib

# Verify
ggen --version  # Should show: ggen 5.0.2
```

## Commands

### Basic Sync

```bash
# Generate code
ggen sync

# Preview changes (no files written)
ggen sync --dry-run true

# Validate without generating
ggen sync --validate-only true

# Specific rule only
ggen sync --rule <rule_name>

# Verbose output
ggen sync --verbose true

# Watch mode (regenerate on changes)
ggen sync --watch true

# Force overwrite conflicts
ggen sync --force true

# Track changes
ggen sync --audit true
```

## ggen.toml Structure

### Minimal Configuration

```toml
[project]
name = "my-project"
version = "1.0.0"
description = "My project"

[ontology]
source = "schemas/ontology.ttl"
format = "turtle"

[generation]
output_dir = "generated/"
rules = []
```

### With Generation Rules

```toml
[generation]
output_dir = "generated/"
rules = [
  {
    name = "my_rule",
    template = "template.tera",
    output = "output/{name}.md"
  }
]
```

## Project Layout

```
my-project/
├── ggen.toml                 # Configuration
├── schemas/
│   └── ontology.ttl          # RDF ontology (Turtle)
├── ggen/
│   ├── templates/            # Tera templates (.tera files)
│   ├── generated/            # Generated files (auto)
│   └── README.md             # Documentation
└── src/
    └── generated/            # Generated source code
```

## Tera Template Syntax

### Variables

```tera
{{ variable }}              # Output variable
{{ variable | upper }}      # Filter: uppercase
{{ variable | default(val) }} # Default value
```

### Loops

```tera
{% for item in items %}
  {{ item.name }}
{% endfor %}

{% for item in items %}
  {{ item }}{{ not loop.last ? "," : "" }}  # Add comma except last
{% endfor %}
```

### Conditionals

```tera
{% if condition %}
  Yes
{% else %}
  No
{% endif %}

{% if variable == "value" %}
  Exact match
{% endif %}
```

### Filters

```tera
{{ "hello" | upper }}           # HELLO
{{ 3.14 | round }}              # 3
{{ array | length }}            # Array length
{{ array | first }}             # First element
{{ array | last }}              # Last element
{{ string | replace(pat, repl) }} # Replace pattern
```

## RDF Ontology Quick Reference

### Class Definition

```turtle
@prefix ex: <https://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:MyClass a rdfs:Class ;
    rdfs:label "My Class" ;
    rdfs:comment "Description of my class" .
```

### Property Definition

```turtle
ex:myProperty a rdf:Property ;
    rdfs:domain ex:MyClass ;
    rdfs:range rdfs:Literal ;
    rdfs:label "my property" ;
    rdfs:comment "Description" .
```

### Instance (Data)

```turtle
ex:instance1 a ex:MyClass ;
    ex:myProperty "value" ;
    rdfs:label "Instance 1" .
```

## Workflow

### Adding to Ontology

1. Edit `schemas/ontology.ttl`
2. Add class, property, or instance
3. Run `ggen sync --dry-run true`
4. Review changes
5. Run `ggen sync`

### Adding Template

1. Create `ggen/templates/my-template.tera`
2. Add to generation rules in `ggen.toml`
3. Run `ggen sync --dry-run true`
4. Run `ggen sync`

### Generate Specific File

```bash
ggen sync --rule my_rule_name
```

### Watch for Changes

```bash
ggen sync --watch true
```

Auto-regenerates when templates or ontology changes.

## Debugging

### Check Configuration

```bash
cat ggen.toml

# Validate structure
ggen sync --validate-only true
```

### Preview Output

```bash
# See what would be generated
ggen sync --dry-run true

# With verbose output
ggen sync --dry-run true --verbose true

# JSON format (for parsing)
ggen sync --dry-run true --format json
```

### Check Template

```bash
cat ggen/templates/my-template.tera

# Test with ontology data
ggen sync --rule my_rule --verbose true
```

### Validate Ontology

```bash
# Load and validate RDF
# Check for syntax errors
# Verify namespaces

cat schemas/ontology.ttl | head -20
```

## Common Patterns

### Template with Defaults

```tera
{{ variable | default("N/A") }}
```

### Conditional Output

```tera
{% if package.tier == "essential" %}
  This is essential
{% endif %}
```

### Loop with Index

```tera
{% for item in items %}
  {{ loop.index }}: {{ item.name }}
{% endfor %}
```

### Filter Array

```tera
{% for item in items | filter(attribute="type", value="active") %}
  {{ item.name }}
{% endfor %}
```

### Join Array

```tera
{{ packages | map(attribute="name") | join(", ") }}
```

## UNRDF Specific

### UNRDF Ontology Location

```
schemas/unrdf-packages.ttl
```

### Core Classes

- `Package` - npm package
- `Capability` - feature provided
- `PackageTier` - classification

### Core Properties

- `packageName` - @unrdf/...
- `packagePath` - packages/...
- `version` - semantic version
- `tier` - essential/extended/optional
- `mainExport` - function name
- `testCoverage` - percentage

### UNRDF Templates

```
ggen/templates/
├── package-readme.tera
├── ts-interfaces.tera
├── package-documentation.tera
├── package-manifest.tera
└── packages-index.tera
```

### Generate for UNRDF

```bash
# Dry-run
ggen sync --dry-run true

# Generate all
ggen sync

# Specific rule
ggen sync --rule packages_index

# Watch
ggen sync --watch true
```

## Helpful Commands

```bash
# Show ggen help
ggen sync --help

# Check installed version
ggen --version

# Find ggen location
which ggen

# Show generated files
ls -la ggen/generated/

# Check file sizes
du -h ggen/generated/*

# Compare with git
git diff ggen/generated/
```

## Resources

- **ggen**: https://github.com/seanchatmangpt/ggen
- **Tera**: https://tera.netlify.app/
- **RDF**: https://www.w3.org/RDF/
- **Turtle**: https://www.w3.org/TR/turtle/
- **SPARQL**: https://www.w3.org/TR/sparql11-query/

## File Locations

```
/home/user/unrdf/
├── ggen.toml                    # Configuration
├── schemas/
│   ├── kgc-markdown.mjs        # Existing schema
│   └── unrdf-packages.ttl       # Package ontology
├── ggen/
│   ├── README.md               # Full documentation
│   ├── EXAMPLES.md             # Practical examples
│   ├── QUICK-REFERENCE.md      # This file
│   ├── templates/
│   │   ├── package-readme.tera
│   │   ├── ts-interfaces.tera
│   │   ├── package-documentation.tera
│   │   ├── package-manifest.tera
│   │   └── packages-index.tera
│   └── generated/              # Output (auto-generated)
```

---

**Quick Links**: [README](./README.md) | [Examples](./EXAMPLES.md) | [ggen Docs](https://docs.ggen.io)
