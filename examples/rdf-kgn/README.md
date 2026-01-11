# RDF-KGN Integration Examples

Production-ready examples demonstrating RDF knowledge graph generation using the UNRDF KGN template system.

## Quick Start

```bash
# Run any example
node examples/rdf-kgn/01-minimal-rdf-template.mjs
```

## Examples Overview

### 01-minimal-rdf-template.mjs
**Simplest RDF template example**

- **Time**: 5-10 minutes
- **Difficulty**: Beginner
- **Topics**: Basic RDF generation, Template rendering

Demonstrates the absolute minimum code needed to generate RDF from a template. Perfect starting point for understanding the RDF-KGN integration.

**What you'll learn**:
- Creating an RDF template engine
- Rendering simple Turtle templates
- Basic variable substitution

**Run it**:
```bash
node examples/rdf-kgn/01-minimal-rdf-template.mjs
```

---

### 02-ontology-generation.mjs
**Build OWL ontologies from templates**

- **Time**: 15-20 minutes
- **Difficulty**: Intermediate
- **Topics**: OWL ontologies, Class hierarchies, Property definitions

Shows how to generate complete OWL ontologies using templates, including classes, properties, and axioms. Demonstrates querying the generated ontology.

**What you'll learn**:
- Defining ontology structure in data
- Generating OWL classes and properties
- Querying ontology metadata with SPARQL

**Run it**:
```bash
node examples/rdf-kgn/02-ontology-generation.mjs
```

---

### 03-sparql-builder-demo.mjs
**Dynamic SPARQL query generation**

- **Time**: 15-20 minutes
- **Difficulty**: Intermediate
- **Topics**: SPARQL queries, Query builders, Data filtering

Demonstrates building complex SPARQL queries from structured data using templates. Includes SELECT queries with filters, ordering, and pagination.

**What you'll learn**:
- Generating SPARQL SELECT queries
- Adding FILTER, ORDER BY, LIMIT clauses
- Executing dynamic queries against RDF stores

**Run it**:
```bash
node examples/rdf-kgn/03-sparql-builder-demo.mjs
```

---

### 04-shacl-validation-workflow.mjs
**Complete SHACL validation workflow**

- **Time**: 20-25 minutes
- **Difficulty**: Advanced
- **Topics**: SHACL shapes, Data validation, Constraint checking

Shows how to generate SHACL shapes from templates and use them to validate RDF data. Includes detailed violation reporting.

**What you'll learn**:
- Generating SHACL shapes from structured definitions
- Validating RDF data against shapes
- Understanding validation violations
- Implementing data quality workflows

**Run it**:
```bash
node examples/rdf-kgn/04-shacl-validation-workflow.mjs
```

---

### 05-rdf-visualization.mjs
**Graph visualization in multiple formats**

- **Time**: 15-20 minutes
- **Difficulty**: Intermediate
- **Topics**: Graph visualization, DOT format, Mermaid diagrams

Demonstrates generating graph visualizations from RDF data in multiple formats (GraphViz DOT, Mermaid, JSON).

**What you'll learn**:
- Extracting graph structure from RDF
- Generating GraphViz DOT diagrams
- Creating Mermaid graph syntax
- Exporting JSON for D3.js/Cytoscape.js

**Run it**:
```bash
node examples/rdf-kgn/05-rdf-visualization.mjs
```

---

### 06-knowledge-graph-pipeline.mjs
**End-to-end knowledge graph pipeline**

- **Time**: 25-30 minutes
- **Difficulty**: Advanced
- **Topics**: Complete workflows, Integration, Production patterns

Comprehensive example tying everything together: ontology definition, shape generation, data creation, validation, and querying.

**What you'll learn**:
- Building complete knowledge graph pipelines
- Integrating multiple stages
- Production-ready workflow patterns
- Analytics and reporting

**Run it**:
```bash
node examples/rdf-kgn/06-knowledge-graph-pipeline.mjs
```

---

## Sample Data

The `data/` directory contains reusable sample files:

- **sample-ontology.ttl** - Example domain ontology (Person, Organization, Document)
- **sample-data.ttl** - Instance data conforming to the ontology
- **sample-shapes.ttl** - SHACL validation shapes
- **template-person.njk** - Template for generating Person instances
- **template-organization.njk** - Template for generating Organization instances

## Learning Paths

### Path 1: Beginner (30-40 minutes)
1. 01-minimal-rdf-template.mjs
2. 02-ontology-generation.mjs
3. 05-rdf-visualization.mjs

**Goal**: Understand basic RDF generation and visualization

### Path 2: Intermediate (50-60 minutes)
1. 01-minimal-rdf-template.mjs
2. 03-sparql-builder-demo.mjs
3. 04-shacl-validation-workflow.mjs

**Goal**: Master querying and validation workflows

### Path 3: Advanced (60-75 minutes)
1. Complete all examples in order (01-06)

**Goal**: Build production-ready knowledge graph systems

### Path 4: Production Focus (30 minutes)
1. 06-knowledge-graph-pipeline.mjs (study thoroughly)

**Goal**: Understand end-to-end production patterns

## Prerequisites

### Knowledge
- Basic understanding of RDF triples (subject-predicate-object)
- Familiarity with Turtle syntax (helpful but not required)
- Basic SPARQL knowledge (for examples 03, 06)

### Technical
- Node.js 18+
- UNRDF packages installed (`@unrdf/core`, `@unrdf/kgn`)

## Common Patterns

### Pattern 1: Template Engine Setup
```javascript
import { RdfTemplateEngine } from '@unrdf/kgn/rdf';
import { COMMON_PREFIXES } from '@unrdf/core';

const engine = new RdfTemplateEngine({
  prefixes: COMMON_PREFIXES,
  enableStore: true,
});
```

### Pattern 2: Rendering Templates
```javascript
const result = engine.render(template, data);
```

### Pattern 3: Loading into Store
```javascript
const store = createStore();
await store.load(rdfString, { format: 'turtle' });
```

### Pattern 4: Querying
```javascript
const results = await executeQuery(store, sparqlQuery);
```

### Pattern 5: Validation
```javascript
const validation = await validateWithShacl(dataStore, shapesStore);
```

## Troubleshooting

### Import Errors
If you see module import errors:
```bash
# Ensure you're in the project root
cd /home/user/unrdf

# Run from project root
node examples/rdf-kgn/01-minimal-rdf-template.mjs
```

### Template Rendering Issues
- Check that variables in templates match data keys exactly
- Ensure proper Nunjucks syntax ({{ variable }}, {% for %}, etc.)
- Validate Turtle syntax in generated output

### Validation Errors
- Review SHACL shape constraints carefully
- Check that datatypes match (xsd:string, xsd:integer, etc.)
- Verify cardinality constraints (minCount, maxCount)

## Next Steps

After completing these examples:

1. **Explore the KGN package**: `/home/user/unrdf/packages/kgn/`
2. **Review RDF templates**: `/home/user/unrdf/packages/kgn/src/templates/rdf/`
3. **Study integration tests**: `/home/user/unrdf/packages/yawl/test/integrations/`
4. **Build your own**: Apply patterns to your domain

## Additional Resources

- [KGN Package Documentation](../../packages/kgn/README.md)
- [UNRDF Core Documentation](../../packages/core/README.md)
- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [SPARQL 1.1 Specification](https://www.w3.org/TR/sparql11-query/)
- [SHACL Specification](https://www.w3.org/TR/shacl/)

## License

MIT - See LICENSE file in repository root
