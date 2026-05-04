# Getting Started with @unrdf/cli

Welcome to UNRDF! This guide will help you get up and running with the UNRDF Command Line Interface (CLI) for graph operations, code generation, and knowledge management.

## Installation

You can install the UNRDF CLI globally or add it to your project.

### Global Installation
```bash
npm install -g @unrdf/cli
# or
pnpm add -g @unrdf/cli
```

### Local Project Installation
```bash
pnpm add @unrdf/cli
```

### Run without Installation
```bash
npx @unrdf/cli --help
```

---

## Your First Template (5-Minute Quickstart)

Let's generate some code from RDF data using the `template` command.

### 1. Create a minimal RDF file
Create `data.ttl` with some basic information:
```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:Alice a foaf:Person ;
    foaf:name "Alice Smith" ;
    foaf:mbox <mailto:alice@example.org> .
```

### 2. Create a minimal template
Create `greet.njk` with Hygen-style frontmatter and Nunjucks body:
```njk
---
to: greeting.txt
---
Hello, {{ results[0].name }}!
We found your email: {{ results[0].mbox }}
```

### 3. Run the template generation
```bash
unrdf template generate data.ttl --template greet.njk --sparql "SELECT ?name ?mbox WHERE { ?s foaf:name ?name; foaf:mbox ?mbox }"
```

### 4. View the output
```bash
cat src/greeting.txt
# Output:
# Hello, Alice Smith!
# We found your email: mailto:alice@example.org
```

---

## Core Concepts

### What is RDF?
RDF (Resource Description Framework) is a standard for data interchange on the Web. It represents data as "triples": **Subject** -> **Predicate** -> **Object**.
*   **Subject**: The thing you're describing (e.g., Alice).
*   **Predicate**: The property (e.g., name).
*   **Object**: The value (e.g., "Alice Smith").

### What is SPARQL?
SPARQL is the query language for RDF. It allows you to select specific data from your graph to pass into templates.

### What is Nunjucks?
Nunjucks is a powerful templating engine for JavaScript. UNRDF uses it to render your code based on the data extracted via SPARQL.

---

## Template Anatomy

A UNRDF template consists of two parts:

1.  **Frontmatter (YAML)**: Configuration for the generator (output path, injection rules, skip conditions).
2.  **Body (Nunjucks)**: The actual template content.

Example:
```njk
---
to: src/models/{{ results[0].name | pascalCase }}.ts
description: Auto-generated model
---
export class {{ results[0].name | pascalCase }} {
    // Generated for {{ results[0].name }}
}
```

---

## Common Patterns

### Single File Generation
The default mode. One template generates one output file.

### Batch Generation
Generate one file for every instance of a class found in your RDF data.
```bash
unrdf template generate data.ttl --template service.njk --batch --class-uri "http://xmlns.com/foaf/0.1/Person"
```

### Conditional Skip
Skip generating a file if a condition is met or if a pattern already exists in the file.
```yaml
---
to: config.json
unless_exists: true
---
{ "default": "config" }
```

---

## Troubleshooting

*   **"SPARQL query returned no results"**: Check your prefixes and property URIs. Use `unrdf query` to debug your SPARQL independently.
*   **"Invalid YAML in template"**: Ensure your frontmatter is valid YAML. If values contain template variables like `{{ }}`, wrap them in quotes: `to: "{{ name }}.js"`.
*   **"Template not rendering correctly"**: Check for typos in variable names. By default, query results are available in the `results` array.

## Next Steps
*   Read the [Template Command Reference](template-command.md) for full details on directives and filters.
*   Explore the `unrdf sync` command for managing complex project-wide generation in the [Sync Command Guide](sync-command.md).
*   Check the `examples/` directory in the repository for complete workflows.
