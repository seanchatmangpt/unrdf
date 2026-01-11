# RDF-KGN Examples Guide

Comprehensive learning guide for mastering RDF knowledge graph generation using UNRDF KGN templates.

## Overview

This guide provides structured learning paths for different skill levels and use cases. All examples are production-ready and demonstrate real-world patterns.

## Table of Contents

- [Example Catalog](#example-catalog)
- [Learning Paths](#learning-paths)
- [Difficulty Levels](#difficulty-levels)
- [Time Estimates](#time-estimates)
- [Prerequisites](#prerequisites)
- [Learning Objectives](#learning-objectives)
- [Best Practices](#best-practices)

## Example Catalog

### Example 01: Minimal RDF Template
**File**: `01-minimal-rdf-template.mjs`

**Description**: The simplest possible RDF template example showing basic triple generation.

**Key Concepts**:
- RDF template engine initialization
- Variable substitution in Turtle templates
- Basic RDF triple structure

**Lines of Code**: ~50

**Real-World Use Case**: Generating person profiles, product catalogs, simple data exports

**Success Criteria**:
- Can create RDF template engine
- Can render Turtle templates with data
- Understands basic triple patterns

---

### Example 02: Ontology Generation
**File**: `02-ontology-generation.mjs`

**Description**: Generate complete OWL ontologies from structured data definitions.

**Key Concepts**:
- OWL class hierarchies
- Property definitions (DatatypeProperty, ObjectProperty)
- Functional properties
- Ontology metadata
- SPARQL ontology queries

**Lines of Code**: ~220

**Real-World Use Case**: Domain modeling, schema generation, ontology engineering

**Success Criteria**:
- Can define ontology structure in JavaScript
- Can generate OWL classes and properties
- Can query ontology metadata
- Understands class hierarchies

---

### Example 03: SPARQL Builder Demo
**File**: `03-sparql-builder-demo.mjs`

**Description**: Build complex SPARQL queries dynamically using templates.

**Key Concepts**:
- SPARQL SELECT query structure
- WHERE clause patterns
- FILTER conditions
- ORDER BY and LIMIT
- Query parameterization

**Lines of Code**: ~280

**Real-World Use Case**: Search interfaces, analytics dashboards, data exploration tools

**Success Criteria**:
- Can generate SPARQL queries from data
- Can add filters and ordering
- Can execute queries and process results
- Understands query optimization

---

### Example 04: SHACL Validation Workflow
**File**: `04-shacl-validation-workflow.mjs`

**Description**: Complete data validation workflow using SHACL shapes.

**Key Concepts**:
- SHACL shape generation
- Constraint types (minCount, pattern, datatype)
- Validation execution
- Violation reporting
- Data quality workflows

**Lines of Code**: ~270

**Real-World Use Case**: Data quality assurance, ETL validation, API input validation

**Success Criteria**:
- Can generate SHACL shapes from definitions
- Can validate RDF data
- Can interpret validation results
- Understands constraint types

---

### Example 05: RDF Visualization
**File**: `05-rdf-visualization.mjs`

**Description**: Generate graph visualizations in multiple formats (DOT, Mermaid, JSON).

**Key Concepts**:
- Graph structure extraction
- GraphViz DOT format
- Mermaid diagram syntax
- JSON graph export
- Visualization best practices

**Lines of Code**: ~260

**Real-World Use Case**: Documentation, graph exploration, knowledge graph presentations

**Success Criteria**:
- Can extract graph structure from RDF
- Can generate multiple visualization formats
- Can customize visual appearance
- Understands graph layouts

---

### Example 06: Knowledge Graph Pipeline
**File**: `06-knowledge-graph-pipeline.mjs`

**Description**: End-to-end pipeline integrating ontology, validation, and querying.

**Key Concepts**:
- Pipeline orchestration
- Multi-stage workflows
- Error handling
- Analytics and reporting
- Production patterns

**Lines of Code**: ~290

**Real-World Use Case**: Complete knowledge graph systems, ETL pipelines, data integration

**Success Criteria**:
- Can build complete pipelines
- Can integrate multiple stages
- Can handle errors gracefully
- Understands production workflows

---

## Learning Paths

### Path A: Quick Start (30 minutes)
**Goal**: Get productive quickly with basic RDF generation

**Steps**:
1. Read README.md (5 min)
2. Run Example 01 (5 min)
3. Modify Example 01 with your data (10 min)
4. Run Example 02 (10 min)

**Outcome**: Can generate RDF triples and simple ontologies

---

### Path B: Data Validation Focus (60 minutes)
**Goal**: Master data quality and validation workflows

**Steps**:
1. Run Example 01 (10 min)
2. Study Example 04 in depth (20 min)
3. Modify Example 04 with custom shapes (20 min)
4. Run Example 06 to see validation in context (10 min)

**Outcome**: Can implement production-ready validation workflows

---

### Path C: Query Engineering (60 minutes)
**Goal**: Build sophisticated query interfaces

**Steps**:
1. Run Example 01 (10 min)
2. Study Example 03 in depth (20 min)
3. Build custom query templates (20 min)
4. Study query optimization in Example 06 (10 min)

**Outcome**: Can build dynamic SPARQL query builders

---

### Path D: Complete Mastery (90 minutes)
**Goal**: Master all aspects of RDF-KGN integration

**Steps**:
1. Run all examples in order (30 min)
2. Study source code of each example (30 min)
3. Build a mini-project combining techniques (30 min)

**Outcome**: Production-ready knowledge graph development skills

---

### Path E: Production Implementation (45 minutes)
**Goal**: Focus on production-ready patterns only

**Steps**:
1. Study Example 06 in depth (20 min)
2. Review error handling patterns (10 min)
3. Study pipeline orchestration (10 min)
4. Plan your production architecture (5 min)

**Outcome**: Can architect production knowledge graph systems

---

## Difficulty Levels

### Beginner
**Examples**: 01

**Prerequisites**:
- Basic JavaScript knowledge
- Understanding of objects and arrays
- Awareness of RDF concepts (helpful but not required)

**Time Commitment**: 15-20 minutes

**Success Indicators**:
- Can run examples successfully
- Can modify template data
- Can read generated RDF

---

### Intermediate
**Examples**: 02, 03, 05

**Prerequisites**:
- Completed beginner examples
- Understanding of RDF triples
- Basic SPARQL knowledge
- Familiarity with ontologies (for 02)

**Time Commitment**: 45-60 minutes

**Success Indicators**:
- Can create custom templates
- Can query RDF data
- Can generate visualizations

---

### Advanced
**Examples**: 04, 06

**Prerequisites**:
- Completed intermediate examples
- SHACL knowledge (for 04)
- Understanding of data workflows
- Production development experience

**Time Commitment**: 45-60 minutes

**Success Indicators**:
- Can build complete pipelines
- Can implement validation workflows
- Can handle errors and edge cases

---

## Time Estimates

### By Example
| Example | Read | Run | Experiment | Total |
|---------|------|-----|------------|-------|
| 01 | 5 min | 2 min | 3-8 min | 10-15 min |
| 02 | 8 min | 3 min | 4-9 min | 15-20 min |
| 03 | 8 min | 3 min | 4-9 min | 15-20 min |
| 04 | 10 min | 4 min | 6-11 min | 20-25 min |
| 05 | 8 min | 3 min | 4-9 min | 15-20 min |
| 06 | 12 min | 5 min | 8-13 min | 25-30 min |

### By Activity
- **First Run All**: 20-25 minutes
- **Study All Code**: 50-60 minutes
- **Modify All Examples**: 40-50 minutes
- **Build Custom Project**: 60-120 minutes

---

## Prerequisites

### Knowledge Prerequisites

#### Beginner Level
- JavaScript ES6+ syntax
- Basic async/await
- Object and array manipulation

#### Intermediate Level
- RDF concepts (triples, graphs)
- Turtle syntax basics
- SPARQL SELECT queries
- Template engines (helpful)

#### Advanced Level
- OWL ontologies
- SHACL shapes
- Data validation patterns
- Pipeline orchestration

### Technical Prerequisites

#### Required
- Node.js 18+ installed
- UNRDF workspace set up
- Terminal/command line familiarity

#### Recommended
- Code editor with JavaScript support
- Git for version control
- Understanding of semantic web concepts

---

## Learning Objectives

### After Example 01
**You will be able to**:
- Create RDF template engine instances
- Render Turtle templates with data
- Generate simple RDF triples
- Understand variable substitution

**Assessment**: Can you generate RDF for 5 people with different properties?

---

### After Example 02
**You will be able to**:
- Define ontology structure in JavaScript
- Generate OWL classes and properties
- Create class hierarchies
- Query ontology metadata
- Understand functional properties

**Assessment**: Can you create an ontology for your domain (e.g., e-commerce, healthcare)?

---

### After Example 03
**You will be able to**:
- Build SPARQL SELECT queries dynamically
- Add FILTER, ORDER BY, LIMIT clauses
- Execute queries against RDF stores
- Process query results
- Understand query optimization

**Assessment**: Can you build a query interface for searching data?

---

### After Example 04
**You will be able to**:
- Generate SHACL shapes from definitions
- Define validation constraints
- Validate RDF data
- Interpret validation results
- Implement data quality workflows

**Assessment**: Can you create validation rules for your data?

---

### After Example 05
**You will be able to**:
- Extract graph structure from RDF
- Generate GraphViz DOT diagrams
- Create Mermaid visualizations
- Export JSON for visualization libraries
- Customize visual appearance

**Assessment**: Can you visualize your knowledge graph in multiple formats?

---

### After Example 06
**You will be able to**:
- Build complete knowledge graph pipelines
- Integrate ontology, validation, and queries
- Handle errors gracefully
- Generate analytics and reports
- Implement production patterns

**Assessment**: Can you build a complete KG system for your use case?

---

## Best Practices

### Template Design
1. **Keep templates simple**: Complex logic belongs in code, not templates
2. **Use meaningful variable names**: `{{ personName }}` not `{{ x }}`
3. **Add comments**: Document template structure and expected data
4. **Validate data before rendering**: Use Zod schemas for input validation

### Error Handling
1. **Validate inputs**: Check data before template rendering
2. **Handle missing data**: Use default values or optional blocks
3. **Provide clear errors**: Include context in error messages
4. **Log failures**: Track what went wrong and why

### Performance
1. **Reuse engine instances**: Create once, render many times
2. **Batch operations**: Load data in bulk, not one triple at a time
3. **Use appropriate stores**: Oxigraph for large datasets
4. **Cache results**: Don't regenerate static content

### Code Quality
1. **Follow patterns**: Use examples as templates
2. **Add JSDoc**: Document functions and parameters
3. **Handle async properly**: Always await promises
4. **Test edge cases**: Empty data, invalid input, missing fields

### Production Readiness
1. **Validate at boundaries**: Shape validation for external data
2. **Monitor performance**: Track query and validation times
3. **Handle scale**: Test with realistic data volumes
4. **Plan for errors**: Implement retry and fallback strategies

---

## Exercises

### Exercise 1: Custom Domain Model
**Difficulty**: Beginner
**Time**: 20 minutes

Create RDF templates for your domain:
1. Choose a domain (library, restaurant, inventory)
2. Define 3-5 classes
3. Create templates for each class
4. Generate sample data

---

### Exercise 2: Validation Rules
**Difficulty**: Intermediate
**Time**: 30 minutes

Build custom SHACL shapes:
1. Take your domain from Exercise 1
2. Define validation rules for each class
3. Create test data (valid and invalid)
4. Run validation and fix errors

---

### Exercise 3: Query Interface
**Difficulty**: Intermediate
**Time**: 30 minutes

Build a query builder:
1. Create SPARQL templates for common queries
2. Add support for filters and pagination
3. Test with different data sets
4. Optimize query performance

---

### Exercise 4: Complete Pipeline
**Difficulty**: Advanced
**Time**: 60 minutes

Build end-to-end workflow:
1. Define ontology for your domain
2. Generate SHACL shapes
3. Create and validate data
4. Query and generate report
5. Add error handling

---

## Common Pitfalls

### 1. Template Syntax Errors
**Problem**: Nunjucks syntax mistakes
**Solution**: Use simple templates first, validate output

### 2. Data Type Mismatches
**Problem**: Strings where numbers expected
**Solution**: Use Zod schemas for input validation

### 3. Missing Prefixes
**Problem**: Undefined namespace prefixes
**Solution**: Always define prefixes, use COMMON_PREFIXES

### 4. Invalid SPARQL
**Problem**: Generated queries fail
**Solution**: Test queries manually first, validate syntax

### 5. Performance Issues
**Problem**: Slow with large datasets
**Solution**: Use Oxigraph, batch operations, optimize queries

---

## Next Steps

After completing these examples:

1. **Build real project**: Apply to your domain
2. **Study advanced topics**: Inference, reasoning, federation
3. **Contribute**: Share your templates and patterns
4. **Explore integrations**: Connect to databases, APIs, services
5. **Optimize**: Profile and improve performance

---

## Support

**Questions?** Check:
- [UNRDF Documentation](../../docs/)
- [KGN Package](../../packages/kgn/)
- [GitHub Issues](https://github.com/unrdf/unrdf/issues)

**Found a bug?** Please report with:
- Example that fails
- Expected vs actual output
- Error messages
- Environment details

---

## Glossary

**RDF**: Resource Description Framework - Standard for knowledge graphs
**Triple**: Subject-Predicate-Object statement
**Ontology**: Formal domain model with classes and properties
**SPARQL**: Query language for RDF data
**SHACL**: Shapes Constraint Language - Validation for RDF
**Turtle**: Text syntax for RDF
**KGN**: Knowledge Graph Notation - UNRDF template system
**Template**: Reusable pattern for generating RDF
**Shape**: SHACL validation constraint definition

---

## Changelog

**v1.0.0** (2026-01-11)
- Initial release
- 6 production-ready examples
- Complete learning paths
- Sample data and templates

---

## License

MIT - See repository LICENSE file
