# Basic Inference Example

This example demonstrates basic RDFS inference capabilities of the UNRDF Knowledge Engine.

## Features

- **Domain Inference**: Infer subject types from `rdfs:domain` constraints
- **Range Inference**: Infer object types from `rdfs:range` constraints
- **Property Hierarchies**: Infer relationships from `rdfs:subPropertyOf` chains
- **Statistics Tracking**: Monitor inference execution and results

## Running the Example

```bash
# From this directory
npm start

# Run tests
npm test
```

## What It Does

1. **Defines RDFS Schema**:
   - `ex:worksFor` has domain `ex:Person` and range `ex:Organization`
   - `ex:manages` is a subproperty of `ex:worksFor`

2. **Adds Instance Data**:
   - `ex:Alice ex:manages ex:AcmeCorp`
   - `ex:Bob ex:worksFor ex:TechCo`

3. **Runs Inference**:
   - Infers `ex:Alice rdf:type ex:Person` (from domain)
   - Infers `ex:AcmeCorp rdf:type ex:Organization` (from range)
   - Infers `ex:Alice ex:worksFor ex:AcmeCorp` (from subPropertyOf)
   - Infers `ex:Bob rdf:type ex:Person` (from domain)
   - Infers `ex:TechCo rdf:type ex:Organization` (from range)

## Key Concepts

### Domain Constraints

When a property has an `rdfs:domain`, any subject using that property is inferred to be of that type:

```turtle
ex:worksFor rdfs:domain ex:Person .
ex:Alice ex:worksFor ex:AcmeCorp .
# Infers: ex:Alice rdf:type ex:Person
```

### Range Constraints

When a property has an `rdfs:range`, any object of that property is inferred to be of that type:

```turtle
ex:worksFor rdfs:range ex:Organization .
ex:Bob ex:worksFor ex:TechCo .
# Infers: ex:TechCo rdf:type ex:Organization
```

### Property Hierarchies

Subproperties inherit the semantics of their parent properties:

```turtle
ex:manages rdfs:subPropertyOf ex:worksFor .
ex:Alice ex:manages ex:AcmeCorp .
# Infers: ex:Alice ex:worksFor ex:AcmeCorp
```

## Testing

The test suite validates:
- Domain constraint inference
- Range constraint inference
- SubPropertyOf inference
- Combined rule application
- Inference statistics

All tests use `@vitest-environment node` to ensure N3 Store compatibility.

## Integration with UNRDF

This example uses:
- `@unrdf/core` for RDF data structures
- `@unrdf/knowledge-engine` for inference rules
- Standard RDFS vocabulary for semantic constraints

The Knowledge Engine integrates seamlessly with other UNRDF packages for querying, federation, and streaming.
