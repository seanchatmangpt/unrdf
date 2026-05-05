# Monorepo Universe Model

The **Monorepo Universe** is the unified RDF-based knowledge graph that represents the entire UNRDF monorepo structure, dependencies, and metadata. It treats all 42 packages as partitions within a single observable universe (O_t).

## Overview

The universe model provides:

1. **Complete Package Representation**: Every package.json is converted to RDF quads
2. **Dependency Graph as RDF**: All workspace dependencies are queryable via SPARQL
3. **Partition Architecture**: 10 logical partitions with ordering and access rules
4. **Query Library**: Pre-built SPARQL queries for common monorepo questions
5. **Validation**: Partition dependency rules and circular dependency detection

## Architecture

```
                          +-------------------+
                          |  MonorepoUniverse |
                          |    (O_t)          |
                          +-------------------+
                                   |
          +------------------------+------------------------+
          |                        |                        |
   +------+------+          +------+------+          +------+------+
   | O_foundational|         | O_knowledge |         | O_workflow  |
   | (order: 0)  |          | (order: 1)  |          | (order: 2)  |
   +-------------+          +-------------+          +-------------+
   | core        |          | hooks       |          | yawl        |
   | oxigraph    |          | knowledge-  |          | yawl-ai     |
   | caching     |          |   engine    |          | yawl-api    |
   +-------------+          | kgc-4d      |          | ...7 more   |
                            +-------------+          +-------------+
```

## Partition Definitions

The monorepo is organized into 10 partition groups:

### O_foundational (Order: 0, Read-Only)

**Foundational packages - immutable, version-locked**

- `@unrdf/core` - Core RDF operations and SPARQL execution
- `@unrdf/oxigraph` - Oxigraph graph database wrapper
- `@unrdf/caching` - Multi-layer caching infrastructure

These packages form the bedrock of the system. Changes require careful versioning and are subject to strict review.

### O_knowledge (Order: 1)

**Knowledge packages - stable API, additive extensions**

- `@unrdf/hooks` - Policy definition and execution framework
- `@unrdf/knowledge-engine` - Rule engine and inference
- `@unrdf/kgc-4d` - 4D time-travel knowledge graph
- `@unrdf/kgn` - Knowledge Graph Network

### O_workflow (Order: 2)

**YAWL workflow packages - workflow orchestration**

- `@unrdf/yawl` - Core YAWL engine
- `@unrdf/yawl-ai` - AI integration for workflows
- `@unrdf/yawl-api` - REST/GraphQL API layer
- `@unrdf/yawl-durable` - Durable execution
- `@unrdf/yawl-kafka` - Kafka integration
- `@unrdf/yawl-langchain` - LangChain integration
- `@unrdf/yawl-queue` - Queue management
- `@unrdf/yawl-realtime` - Real-time updates
- `@unrdf/yawl-viz` - Workflow visualization

### O_ml (Order: 3)

**Machine learning packages - emerging, high churn**

- `@unrdf/ml-inference` - ML model inference
- `@unrdf/ml-versioning` - ML model versioning

### O_infrastructure (Order: 4)

**Infrastructure packages - deployment-specific**

- `@unrdf/serverless` - Serverless deployment
- `@unrdf/collab` - Collaborative editing (CRDT)
- `@unrdf/streaming` - Stream processing
- `@unrdf/federation` - Federated queries
- `@unrdf/consensus` - Distributed consensus
- `@unrdf/blockchain` - Blockchain integration
- `@unrdf/engine-gateway` - API gateway

### O_observability (Order: 5)

**Observability packages - tracing and metrics**

- `@unrdf/observability` - Core observability
- `@unrdf/yawl-observability` - YAWL-specific observability

### O_documentation (Order: 6)

**Documentation packages - documentation projections**

- `docs` - Documentation site (Nuxt)
- `@unrdf/nextra-docs` - Nextra documentation
- `@unrdf/diataxis-kit` - Documentation framework

### O_integration (Order: 7)

**Integration and testing packages**

- `@unrdf/integration-tests` - Cross-package tests
- `@unrdf/test-utils` - Testing utilities
- `@unrdf/validation` - Schema validation

### O_emerging (Order: 8)

**Experimental packages - bounded exploration**

- `@unrdf/dark-matter` - Experimental query optimization
- `@unrdf/atomvm` - AtomVM integration

### O_platform (Order: 9)

**Platform packages - UI, CLI, and developer tooling**

- `@unrdf/cli` - Command-line interface
- `@unrdf/composables` - Vue composables
- `@unrdf/domain` - Domain modeling
- `@unrdf/graph-analytics` - Graph analytics
- `@unrdf/project-engine` - Project scaffolding
- `@unrdf/rdf-graphql` - RDF to GraphQL bridge
- `@unrdf/semantic-search` - Semantic search

## RDF Schema

### Namespaces

```turtle
@prefix pkg: <http://unrdf.org/package#> .
@prefix dep: <http://unrdf.org/dependency#> .
@prefix mod: <http://unrdf.org/module#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
```

### Classes

| Class | Description |
|-------|-------------|
| `pkg:Package` | Represents an npm package |
| `pkg:Module` | Represents a JavaScript module |
| `pkg:Function` | Represents an exported function |
| `pkg:Type` | Represents a TypeScript/JSDoc type |
| `pkg:Test` | Represents a test file |
| `pkg:Documentation` | Represents documentation |
| `pkg:Partition` | Represents a package partition group |
| `pkg:Export` | Represents a package export entry |
| `pkg:Script` | Represents an npm script |
| `dep:Dependency` | Represents a dependency relationship |
| `dep:WorkspaceDependency` | Represents an internal monorepo dependency |
| `dep:ExternalDependency` | Represents an external npm dependency |

### Properties

| Property | Domain | Range | Description |
|----------|--------|-------|-------------|
| `pkg:name` | Package | xsd:string | Package name |
| `pkg:version` | Package | xsd:string | Package version |
| `pkg:description` | Package | xsd:string | Package description |
| `pkg:partition` | Package | Partition | Partition assignment |
| `dep:dependsOn` | Package | Package | Dependency relationship |
| `mod:exports` | Package | Export | Package export |
| `pkg:keyword` | Package | xsd:string | Package keyword |
| `pkg:hasScript` | Package | Script | Script definition |

## Dependency Rules

Partitions can only depend on partitions with equal or lower order numbers:

```
O_foundational -> [O_foundational]
O_knowledge    -> [O_foundational, O_knowledge]
O_workflow     -> [O_foundational, O_knowledge, O_workflow]
O_ml           -> [O_foundational, O_knowledge, O_ml]
O_integration  -> [ALL except O_emerging]
```

This ensures a clean dependency graph without cycles between partition groups.

## Usage

### Loading the Universe

```javascript
import { createMonorepoUniverse } from './src/universe/monorepo-universe.mjs';

// Load the universe from the monorepo root
const universe = await createMonorepoUniverse('/path/to/unrdf');

// Get summary
console.log(universe.getSummary());
// { packages: 43, quads: 2500+, partitions: 10, ... }
```

### Querying with SPARQL

```javascript
import { QueryTemplates } from './src/universe/monorepo-queries.mjs';

// Find all packages
const packages = universe.query(QueryTemplates.ALL_PACKAGES);

// Find packages that depend on @unrdf/core
const dependents = universe.query(
  QueryTemplates.DEPENDENTS_OF('@unrdf/core')
);

// Find transitive dependencies
const transitive = universe.query(
  QueryTemplates.TRANSITIVE_DEPENDENCIES('@unrdf/yawl')
);
```

### Dependency Analysis

```javascript
// Get dependency graph
const graph = universe.getDependencyGraph();
// Map<string, Set<string>>

// Detect circular dependencies
const cycles = universe.detectCircularDependencies();
// Array<Array<string>>

// Validate partition rules
const validation = universe.validatePartitionDependencies();
// { valid: boolean, violations: [...] }
```

### Projections (mu)

The universe supports projections to different formats:

```javascript
// D3-compatible dependency visualization
const d3Graph = universe.projectToDependencyGraph();
// { nodes: [...], links: [...] }

// Coverage matrix
const coverage = universe.projectToCoverageMatrix();
// [{ package, hasTests, hasCoverage, ... }, ...]

// API surface matrix
const apis = universe.projectToApiSurface();
// [{ package, exports, sideEffects, ... }, ...]
```

## Query Examples

### Find Packages in a Partition

```sparql
PREFIX pkg: <http://unrdf.org/package#>

SELECT ?name ?version WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  ?pkg pkg:partition <http://unrdf.org/package#partition/O_foundational> .
  OPTIONAL { ?pkg pkg:version ?version }
}
```

### Find Cross-Partition Dependencies

```sparql
PREFIX pkg: <http://unrdf.org/package#>
PREFIX dep: <http://unrdf.org/dependency#>

SELECT ?srcName ?srcPartition ?depName ?depPartition WHERE {
  ?src a pkg:Package .
  ?src pkg:name ?srcName .
  ?src pkg:partition ?srcPart .
  ?srcPart pkg:name ?srcPartition .

  ?src dep:dependsOn ?dep .
  ?dep a pkg:Package .
  ?dep pkg:name ?depName .
  ?dep pkg:partition ?depPart .
  ?depPart pkg:name ?depPartition .

  FILTER(?srcPartition != ?depPartition)
}
```

### Find Transitive Dependencies (Property Path)

```sparql
PREFIX pkg: <http://unrdf.org/package#>
PREFIX dep: <http://unrdf.org/dependency#>

SELECT DISTINCT ?depName WHERE {
  <http://unrdf.org/package#package/unrdf-yawl> dep:dependsOn+ ?dep .
  ?dep pkg:name ?depName .
}
```

### Analyze Impact of a Change

```sparql
PREFIX pkg: <http://unrdf.org/package#>
PREFIX dep: <http://unrdf.org/dependency#>

SELECT DISTINCT ?affectedName WHERE {
  ?affected dep:dependsOn+ <http://unrdf.org/package#package/unrdf-core> .
  ?affected pkg:name ?affectedName .
}
ORDER BY ?affectedName
```

## Validation

### Circular Dependency Detection

The universe automatically detects circular dependencies:

```javascript
const cycles = universe.detectCircularDependencies();
if (cycles.length > 0) {
  console.error('Circular dependencies detected:', cycles);
}
```

### Partition Dependency Validation

Validates that all dependencies respect partition ordering:

```javascript
const { valid, violations } = universe.validatePartitionDependencies();
if (!valid) {
  for (const v of violations) {
    console.error(
      `${v.from} (${v.fromPartition}) cannot depend on ` +
      `${v.to} (${v.toPartition})`
    );
  }
}
```

### Workspace Dependency Validation

Ensures all workspace:* dependencies exist:

```javascript
import { validateWorkspaceDependencies } from './src/universe/package-rdf-model.mjs';

const packages = universe.getAllPackages();
const { valid, missing } = validateWorkspaceDependencies(packages);
if (!valid) {
  console.error('Missing workspace dependencies:', missing);
}
```

## Integration with Universe System

The monorepo universe integrates with the existing UNRDF universe system:

```javascript
import { Universe } from './src/universe/universe.mjs';
import { MonorepoUniverse } from './src/universe/monorepo-universe.mjs';

// Standard universe for RDF ontologies
const ontologyUniverse = Universe.createStandard();

// Monorepo universe for package management
const monorepoUniverse = await createMonorepoUniverse('/path/to/unrdf');

// Both provide SPARQL query capabilities
const ontologyResults = ontologyUniverse.query('SELECT * WHERE { ?s ?p ?o }');
const packageResults = monorepoUniverse.query('SELECT * WHERE { ?s ?p ?o }');
```

## Best Practices

1. **Load Once, Query Many**: Load the universe once at startup and reuse for queries
2. **Use Query Templates**: Prefer pre-built queries from `QueryTemplates` for common operations
3. **Validate Before CI**: Run partition validation in CI to catch dependency violations early
4. **Monitor Cycles**: Regularly check for circular dependencies
5. **Keep Partitions Clean**: Respect partition boundaries in new package development

## File Structure

```
src/universe/
  package-rdf-model.mjs    # RDF schema and parsers
  monorepo-universe.mjs    # Universe orchestrator
  monorepo-queries.mjs     # SPARQL query library
  universe.mjs             # Base universe class
  partition.mjs            # Partition definitions
  rdf-utils.mjs            # RDF utility functions
  registry.mjs             # Ontology registry
```

## API Reference

See the JSDoc comments in each file for detailed API documentation:

- `MonorepoUniverse` - Main universe class
- `QueryTemplates` - Pre-built SPARQL queries
- `parsePackageToQuads()` - Convert package.json to RDF
- `createPackageStore()` - Create RDF store from packages
- `validateWorkspaceDependencies()` - Validate workspace deps

## Performance

The universe model is optimized for:

- **Fast Loading**: All packages loaded in parallel
- **Efficient Queries**: Oxigraph-backed SPARQL engine
- **Lazy Computation**: Dependency graphs computed on demand
- **Minimal Memory**: Stores only necessary metadata

Typical performance for 43 packages:

- Load time: ~50-100ms
- Query time: ~1-5ms per query
- Memory: ~5-10MB for full universe

## Future Enhancements

Planned improvements:

1. **Live Watching**: Watch package.json files for changes
2. **Incremental Updates**: Update only changed packages
3. **Visualization Export**: Export to Mermaid, Graphviz formats
4. **Coverage Integration**: Import test coverage data
5. **API Diff Detection**: Detect breaking API changes
