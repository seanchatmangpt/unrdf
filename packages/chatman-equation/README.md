# @unrdf/chatman-equation

> 3T Methodology: TOML + Tera + Turtle = Configuration-Driven Knowledge Graphs

## Overview

The **Chatman Equation** is a systematic methodology for creating knowledge graphs through configuration-driven development. It combines three powerful technologies:

1. **TOML** - Configuration format for metadata and variables
2. **Tera** - Template engine for generating RDF from configurations
3. **Turtle** - RDF serialization format for knowledge representation

### The Equation

```
TOML (Config) + Tera (Templates) + Turtle (RDF) = Validated Knowledge Graph
```

## Features

- **Declarative Configuration**: Define knowledge graphs using TOML
- **Template-Based Generation**: Use Tera templates to generate RDF
- **Validated Output**: Ensure well-formed Turtle RDF
- **Cryptographic Receipts**: KGC 4D receipts for provenance
- **80/20 Efficiency**: Focused components deliver maximum value

## Installation

```bash
pnpm add @unrdf/chatman-equation
```

## Quick Start

```javascript
import { getMethodologyInfo, validateStack } from '@unrdf/chatman-equation';

// Get methodology information
const info = getMethodologyInfo();
console.log(info);

// Validate the 3T stack is available
const isValid = await validateStack();
console.log('Stack valid:', isValid);
```

## Validation

The package includes comprehensive validation for all 3T components:

```bash
# Validate all components
pnpm validate

# Validate individual components
pnpm validate:toml    # TOML configurations
pnpm validate:tera    # Tera templates
pnpm validate:turtle  # Turtle RDF files
```

## Directory Structure

```
chatman-equation/
├── src/              # Main implementation
├── validation/       # Validation scripts
│   ├── validate-toml.mjs
│   ├── validate-tera.mjs
│   ├── validate-turtle.mjs
│   └── validate-integration.mjs
├── examples/         # Example files
│   ├── toml/        # Example TOML configs
│   ├── tera/        # Example templates
│   └── turtle/      # Example RDF
└── schemas/          # Zod validation schemas
```

## Validation Features

### TOML Validation
- Parse all TOML files
- Validate against Zod schemas
- Check required fields and types

### Tera Validation
- Syntax checking for templates
- Balance tag validation
- Test rendering with sample data

### Turtle Validation
- Parse RDF with N3
- Validate RDF semantics
- Count triples and check integrity

### Integration Validation
- End-to-end validation
- KGC receipt generation
- Deployment manifest creation
- 80/20 Pareto analysis

## 80/20 Principle

The 3T methodology follows the Pareto principle:
- **20% of components** (config, templates, ontology) deliver **80% of value**
- Focused design maximizes efficiency
- Validated output ensures quality

## KGC Receipts

All validation results include cryptographic receipts:
- Package creation receipt
- Documentation generation receipt
- Ontology publication receipt
- Integration completion receipt

Receipts are stored in `receipts/validation-receipts.json`.

## Deployment Manifest

After validation, a TOML deployment manifest is generated:

```toml
[deployment]
name = "chatman-equation-3t"
version = "1.0.0"
timestamp = "2026-01-18T00:00:00Z"
components = ["toml-configs", "tera-templates", "turtle-ontologies"]

[validation]
toml_files = 1
tera_templates = 1
turtle_files = 2
all_valid = true

[receipts]
package_creation = "receipt-package-..."
documentation = "receipt-documentation-..."
ontology = "receipt-ontology-..."
integration = "receipt-integration-..."
```

## API Reference

### Core Functions

#### `getMethodologyInfo()`
Returns metadata about the 3T methodology.

**Returns**: `Object` - Methodology information

#### `validateStack()`
Validates that all required dependencies are available.

**Returns**: `Promise<boolean>` - True if stack is valid

### Validation Functions

#### `validateAllTOML()`
Validates all TOML configuration files.

**Returns**: `Promise<{success: boolean, results: Array}>``

#### `validateAllTera()`
Validates all Tera template files.

**Returns**: `Promise<{success: boolean, results: Array}>`

#### `validateAllTurtle()`
Validates all Turtle RDF files.

**Returns**: `Promise<{success: boolean, results: Array}>`

#### `validateIntegration()`
Runs end-to-end validation and generates receipts.

**Returns**: `Promise<{success: boolean, results: Object, receipts: Object}>`

## Examples

See the `examples/` directory for complete examples of:
- TOML configurations (`examples/toml/`)
- Tera templates (`examples/tera/`)
- Turtle ontologies (`examples/turtle/`)

## Contributing

Contributions are welcome! Please ensure:
1. All validation tests pass
2. New components include validation
3. Documentation is updated
4. KGC receipts are generated

## License

MIT

## Credits

Created as part of the UNRDF project by the UNRDF team.

**The Chatman Equation**: Making knowledge graphs accessible through configuration.
