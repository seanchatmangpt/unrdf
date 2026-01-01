# ggen Examples and Extensions

> Practical examples for extending UNRDF ontologies and templates

---

## Example 1: Adding a New Package to the Domain Ontology

### Scenario
You're adding a new package `@unrdf/ai-agents` (Extended tier) that depends on `@unrdf/knowledge-engine`.

### Step 1: Edit `schema/domain.ttl`

Add the new package instance at the end of the file:

```turtle
# AI Agents Package (Extended Tier)

unrdf:AIAgentsPackage a unrdf:Package ;
    unrdf:packageName "@unrdf/ai-agents" ;
    unrdf:packageVersion "6.0.0-rc.1" ;
    unrdf:packageDescription "AI agent orchestration and swarm coordination framework" ;
    unrdf:hasTier unrdf:ExtendedTier ;
    unrdf:hasDependency unrdf:CorePackage ;
    unrdf:hasDependency unrdf:KnowledgeEnginePackage ;
    unrdf:hasDependency unrdf:StreamingPackage .
```

### Step 2: Add to appropriate collections (optional)

You can also add it to a collection for easy querying:

```turtle
unrdf:ExtendedTierPackages a rdf:Bag ;
    rdf:_1 unrdf:CorePackage ;
    rdf:_2 unrdf:OxigraphPackage ;
    ...
    rdf:_9 unrdf:AIAgentsPackage .
```

### Step 3: Run ggen sync

```bash
ggen sync
```

### Step 4: Verify in generated files

Check that the package appears:

```bash
# Should list @unrdf/ai-agents in Extended tier
grep -A 5 "@unrdf/ai-agents" src/generated/PACKAGES.md

# Should include in dependency graph
grep -A 3 "ai-agents" src/generated/PACKAGES.md

# Should be in TypeScript types
grep "AIAgents" src/generated/unrdf-types.mjs
```

### Step 5: Commit

```bash
git add schema/domain.ttl src/generated/
git commit -m "feat(ontology): Add @unrdf/ai-agents package to domain ontology"
```

---

## Example 2: Creating a Custom Template

### Scenario
Generate Python dataclass definitions from the RDF ontology.

### Step 1: Create template

Create `templates/python-dataclass.tera`:

```python
{% for class in classes %}
"""
{{ class.label }}

{{ class.comment }}
"""

from dataclasses import dataclass, field
from typing import Optional, List

@dataclass
class {{ class.name | pascalcase }}:
    """{{ class.label }}"""
    {% for property in class.properties %}

    {{ property.name }}: {{ property.pythonType }}
    """{{ property.comment }}"""
    {% endfor %}

    def to_dict(self):
        """Convert to dictionary"""
        return {
            {% for property in class.properties %}
            "{{ property.name }}": self.{{ property.name }},
            {% endfor %}
        }

    @classmethod
    def from_dict(cls, data):
        """Create from dictionary"""
        return cls(
            {% for property in class.properties %}
            {{ property.name }}=data.get("{{ property.name }}"),
            {% endfor %}
        )

{% endfor %}
```

### Step 2: Add to ggen.toml

The template is automatically picked up since it's in `templates/`, but you can reference it explicitly in scripts or documentation.

### Step 3: Use in generation

Create a generation configuration that uses this template:

```bash
# If ggen supports per-template generation:
ggen sync --template templates/python-dataclass.tera --output generated/models.py
```

Or manually use Tera rendering:

```bash
tera templates/python-dataclass.tera schema/domain.ttl > generated/models.py
```

---

## Example 3: Adding SPARQL Inference Rules

### Scenario
Automatically infer transitive dependencies (if A depends on B, and B depends on C, then A transitively depends on C).

### Step 1: Add CONSTRUCT query to ontology

Add to `schema/domain.ttl`:

```turtle
# Inference Rules

unrdf:TransitiveDependencies a unrdf:InferenceRule ;
    rdfs:label "Transitive Dependencies" ;
    rdfs:comment "Infer transitive package dependencies" ;
    unrdf:query """
        PREFIX unrdf: <https://unrdf.io/ns#>

        CONSTRUCT {
            ?package1 unrdf:transitiveDependency ?package3 .
        }
        WHERE {
            ?package1 unrdf:hasDependency ?package2 .
            ?package2 unrdf:hasDependency ?package3 .
        }
    """ .

unrdf:PackageReliance a unrdf:InferenceRule ;
    rdfs:label "Package Reliance" ;
    rdfs:comment "Packages that rely on a package (inverse)" ;
    unrdf:query """
        PREFIX unrdf: <https://unrdf.io/ns#>

        CONSTRUCT {
            ?dependent unrdf:reliesOn ?package .
        }
        WHERE {
            ?package unrdf:hasDependency ?dependent .
        }
    """ .
```

### Step 2: Create template to visualize dependencies

Create `templates/dependency-graph.tera`:

```mermaid
graph TD
{% for package in packages %}
    {{ package.name | replace("@unrdf/", "") }}["{{ package.name }}<br/>{{ package.version }}"]
    {% for dep in package.dependencies %}
    {{ package.name | replace("@unrdf/", "") }} --> {{ dep | replace("@unrdf/", "") }}
    {% endfor %}
{% endfor %}

classDef essential fill:#90EE90,stroke:#228B22,stroke-width:2px
classDef extended fill:#87CEEB,stroke:#4682B4,stroke-width:2px
classDef optional fill:#FFB6C1,stroke:#FF69B4,stroke-width:2px

{% for package in packages %}
{% if package.tier == "Essential" %}
class {{ package.name | replace("@unrdf/", "") }} essential
{% elif package.tier == "Extended" %}
class {{ package.name | replace("@unrdf/", "") }} extended
{% else %}
class {{ package.name | replace("@unrdf/", "") }} optional
{% endif %}
{% endfor %}
```

### Step 3: Generate dependency diagram

```bash
ggen sync
# Then render the diagram with a Mermaid processor
```

---

## Example 4: Extending with Custom Metadata

### Scenario
Track which packages have been released vs experimental, and include in documentation.

### Step 1: Extend the Package class

Add new properties to `schema/domain.ttl`:

```turtle
unrdf:releaseStatus a rdf:Property ;
    rdfs:label "release status" ;
    rdfs:domain unrdf:Package ;
    rdfs:range xsd:string ;
    rdfs:comment "Release status: stable, beta, alpha, experimental" ;
    rdfs:isDefinedBy <https://unrdf.io/> .

unrdf:lastReleaseDate a rdf:Property ;
    rdfs:label "last release date" ;
    rdfs:domain unrdf:Package ;
    rdfs:range xsd:date ;
    rdfs:comment "Date of last release" ;
    rdfs:isDefinedBy <https://unrdf.io/> .

unrdf:maintainer a rdf:Property ;
    rdfs:label "maintainer" ;
    rdfs:domain unrdf:Package ;
    rdfs:range xsd:string ;
    rdfs:comment "GitHub username of package maintainer" ;
    rdfs:isDefinedBy <https://unrdf.io/> .
```

### Step 2: Update package instances

```turtle
unrdf:CorePackage a unrdf:Package ;
    unrdf:packageName "@unrdf/core" ;
    unrdf:packageVersion "6.0.0-rc.1" ;
    unrdf:packageDescription "RDF Graph Operations, SPARQL, Foundational Substrate" ;
    unrdf:hasTier unrdf:EssentialTier ;
    unrdf:releaseStatus "stable" ;
    unrdf:lastReleaseDate "2025-01-01"^^xsd:date ;
    unrdf:maintainer "seanchatmangpt" .
```

### Step 3: Update templates to use new properties

Update `templates/package-readme.tera`:

```tera
# {{ package.name | upper }}

**Status**: {{ package.releaseStatus }}
**Last Release**: {{ package.lastReleaseDate }}
**Maintainer**: [@{{ package.maintainer }}](https://github.com/{{ package.maintainer }})

> {{ package.description }}
```

Update `templates/packages-manifest.tera`:

```tera
| Package Name | Version | Status | Maintainer |
|---|---|---|---|
{% for package in packages %}
| `{{ package.name }}` | `{{ package.version }}` | {{ package.releaseStatus }} | @{{ package.maintainer }} |
{% endfor %}
```

### Step 4: Regenerate

```bash
ggen sync
```

---

## Example 5: Conditional Generation

### Scenario
Only generate documentation for packages marked as "stable", skip experimental ones.

### Template with conditional logic

Create `templates/stable-packages-only.tera`:

```tera
# Stable UNRDF Packages

Generated: {{ timestamp }}

{% set stable = packages | filter(attribute="status", value="stable") %}

## Available Packages

{% for package in stable %}
### {{ package.name }}

{{ package.description }}

**Version**: {{ package.version }}
**Tier**: {{ package.tier }}
**Maintainer**: {{ package.maintainer }}

{% endfor %}

## Statistics

- Total Stable Packages: {{ stable | length }}
- Experimental Packages: {{ packages | filter(attribute="status", value="experimental") | length }}

---

Generated by ggen
```

### Generate only stable package docs

```bash
# This would require custom ggen CLI extension or manual filtering
ggen sync --template templates/stable-packages-only.tera
```

---

## Example 6: Integration with CI/CD

### GitHub Actions Workflow

Create `.github/workflows/codegen-check.yml`:

```yaml
name: Code Generation Check
on:
  pull_request:
    paths:
      - 'schema/**'
      - 'templates/**'
      - 'ggen.toml'

jobs:
  verify:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install ggen
        run: |
          curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
          cargo install ggen

      - name: Verify ontology
        run: |
          ggen sync --mode verify

      - name: Check for uncommitted changes
        run: |
          git diff --exit-code src/generated/ || {
            echo "Generated files are out of date. Run 'ggen sync' and commit the changes."
            exit 1
          }

      - name: Generate and test
        run: |
          ggen sync
          npm test
```

---

## Example 7: SPARQL Query Examples

### Find all packages in Extended tier with their dependencies

```sparql
PREFIX unrdf: <https://unrdf.io/ns#>

SELECT ?package ?name ?dependency ?depName
WHERE {
    ?tier unrdf:tierName "Extended" .
    ?package unrdf:hasTier ?tier ;
             unrdf:packageName ?name ;
             unrdf:hasDependency ?dependency .
    ?dependency unrdf:packageName ?depName .
}
ORDER BY ?name ?depName
```

### Find packages with no dependencies

```sparql
PREFIX unrdf: <https://unrdf.io/ns#>

SELECT ?package ?name
WHERE {
    ?package unrdf:packageName ?name .
    FILTER NOT EXISTS {
        ?package unrdf:hasDependency ?dep .
    }
}
ORDER BY ?name
```

### Find dependency chains (A -> B -> C)

```sparql
PREFIX unrdf: <https://unrdf.io/ns#>

SELECT ?pkgA ?nameA ?pkgB ?nameB ?pkgC ?nameC
WHERE {
    ?pkgA unrdf:packageName ?nameA ;
          unrdf:hasDependency ?pkgB .
    ?pkgB unrdf:packageName ?nameB ;
          unrdf:hasDependency ?pkgC .
    ?pkgC unrdf:packageName ?nameC .
}
ORDER BY ?nameA ?nameB ?nameC
```

### Count packages by tier

```sparql
PREFIX unrdf: <https://unrdf.io/ns#>

SELECT ?tierName (COUNT(?package) as ?count)
WHERE {
    ?tier unrdf:tierName ?tierName .
    ?package unrdf:hasTier ?tier .
}
GROUP BY ?tierName
ORDER BY ?tierName
```

---

## Tips and Best Practices

### 1. Incremental Development
- Make small changes to ontologies
- Run `ggen sync` frequently
- Review changes before committing

### 2. Template Organization
- One template per output type (README, types, docs, etc.)
- Use descriptive names: `package-readme.tera`, `typescript-interface.tera`
- Keep templates under 100 lines each

### 3. Ontology Maintenance
- Use consistent naming (PascalCase for classes, camelCase for properties)
- Add comprehensive rdfs:comment and rdfs:label
- Keep instances organized by type/tier

### 4. Version Control
- Commit ontology and template changes together
- Always commit generated files (they're part of the contract)
- Use meaningful commit messages with `feat(ggen):` or `docs(ggen):`

### 5. Documentation
- Keep GGEN_SETUP.md updated with new features
- Document custom templates in comments
- Include SPARQL query examples for complex ontologies

### 6. Testing
- Validate generated TypeScript with `npm test`
- Check generated documentation renders properly
- Verify dependency graphs are complete and correct

---

## Resources

- [ggen Documentation](https://docs.ggen.io)
- [Tera Template Syntax](https://tera.netlify.app/docs/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [OWL 2 Web Ontology Language](https://www.w3.org/TR/owl2-overview/)

---

**Last Updated**: 2025-01-01
**Examples Version**: 1.0
